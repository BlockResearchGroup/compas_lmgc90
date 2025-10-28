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

  !!****h* LMGC90.CHIC/DISPLAY_3D
  !! NAME
  !!  module wrap_DISPLAY_3D
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/DISPLAY_3D
  !!****

  USE CHIC
  USE DISPLAY_3D,ONLY: &
       init_display_3D           , &
       get_write_gmv_display_3D  , &
       desactive_ascii_gmv_write , &
       set_nb_copies             , &
       set_shift_copy            , &
       active_GMV_DISPLAY3D      , &
       write_out_GMV
!  USE vtk_DISPLAY_3D,ONLY: &
!       init_vtk_display_3D, &
!       write_out_vtk_display_3D, &
!       get_write_vtk_display_3D, &
!       activate_option_vtk_DISPLAY_3D,&
!       set_nb_copies_vtk_display_3D, &
!       set_shift_copy_vtk_display_3D

CONTAINS

!!!--------------------------------------------------------------------
  SUBROUTINE chic_command_DISPLAY_3D

    IMPLICIT NONE

    INTEGER :: STEP
    integer :: I4
    real(kind=8),dimension(3) :: R8_3
    LOGICAL :: write_gmv

    logical,save :: with_vtk=.false. 

!    IF (INDEX(CHZZZ,'DISPLAY WITH VTK')==1) THEN
!       CALL LOGCHIC('DISPLAY_3D')
!       with_vtk=.true.
!       IETZZZ = 1
!    endif

    IF (INDEX(CHZZZ,'INIT POST')==1) THEN
       CALL LOGCHIC('DISPLAY_3D')
!       if (with_vtk) then
!         call init_vtk_display_3D(KHOZZZ)
!       else
         CALL init_display_3D(KHOZZZ)
!       endif
       IETZZZ = 1
    END IF

    IF (INDEX(CHZZZ,'WRITE OUTPUT GMV')==1) THEN
       !!****e* POST2D/WRITE OUTPUT GMV
       !! NAME
       !!  WRITE OUTPUT GMV
       !! PURPOSE
       !!  write gmv file
       !! USES
       !!  LMGC90.CORE/POST3D/write_GMV_display3D
       !!****
       write_gmv = get_write_gmv_display_3D()
!       if (with_vtk) then
!         IF(write_gmv) CALL write_out_vtk_display_3D
!       else
         IF(write_gmv) CALL write_out_GMV
!       endif     
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'BINARY GMV WRITE')==1) THEN
       !!****e* DISPLAY_3D/BINARY GMV WRITE
       !! NAME
       !!  BINARY GMV WRITE
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DISPLAY_3D/desactive_ascii_gmv_write
       !!****
       CALL LOGCHIC('DISPLAY_3D')
!       if (with_vtk) then
!         call activate_option_vtk_DISPLAY_3D('BIN__')
!       else
         CALL desactive_ascii_gmv_write
!       endif
       IETZZZ = 1
       RETURN
    END IF
    
    IF (INDEX(CHZZZ,'NB COPIES')==1) THEN
       !!****e* DISPLAY_3D/NB COPIES
       !! NAME
       !!  NB COPIES
       !! PURPOSE
       !!  set the number of copies of the sample
       !! USES
       !!  LMGC90.CORE/DISPLAY_3D/set_nb_copies
       !!****
       CALL LOGCHIC('DISPLAY_3D')
       READ(CHLZZZ(NZZZ+1),*) i4

!       if (with_vtk) then
!         call set_nb_copies_vtk_display_3D(i4)
!       else
         CALL set_nb_copies(i4)
!       endif 
       ! print *,'nb copies set to : ',i4

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SHIFT COPY')==1) THEN
       !!****e* DISPLAY_3D/SHIFT COPY
       !! NAME
       !!  SHIFT COPY
       !! PURPOSE
       !!  set the shift of a given copy
       !! USES
       !!  LMGC90.CORE/DISPLAY_3D/set_shift_copy
       !!****
       CALL LOGCHIC('DISPLAY_3D')
       READ(CHLZZZ(NZZZ+1),*) i4,r8_3

!       if (with_vtk) then
!         call set_shift_copy_vtk_display_3D(i4,r8_3)
!       else
         CALL set_shift_copy(i4,r8_3)
!       endif    
       ! print *,'shift copy set to : ',i4,r8_3

       IETZZZ = 1
!       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY TACTOR')==1) THEN
       !!****e* DISPLAY_3D/DISPLAY TACTOR
       !! NAME
       !!   DISPLAY TACTOR
       !! PURPOSE
       !!  Specifies that you want to compute and display the contactor shape 
       !! USES
       !!   LMGC90.CORE/DISPLAY_3D/active_GMV
       !!****
       CALL LOGCHIC('DISPLAY_3D')
!       if (with_vtk) then   
!         call activate_option_vtk_DISPLAY_3D('TCTRS')
!       else
         CALL active_GMV_DISPLAY3D('TCTRS')
!       endif
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY INTERACTION')==1) THEN
       !!****e* DISPLAY_3D/DISPLAY INTERACTION
       !! NAME
       !!   DISPLAY INTER
       !! PURPOSE
       !!  Specifies that you want to compute and display the contactor shape 
       !! USES
       !!   LMGC90.CORE/DISPLAY_3D/active_GMV
       !!****
       CALL LOGCHIC('DISPLAY_3D')
!       if (with_vtk) then   
!         call activate_option_vtk_DISPLAY_3D('INTER')
!       endif
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY NEW WAY')==1) THEN
       !!****e* DISPLAY_3D/DISPLAY INTERACTION
       !! NAME
       !!   DISPLAY INTER
       !! PURPOSE
       !!  Specifies that you want to compute and display the contactor shape 
       !! USES
       !!   LMGC90.CORE/DISPLAY_3D/active_GMV
       !!****
       CALL LOGCHIC('DISPLAY_3D')
!       if (with_vtk) then   
!         call activate_option_vtk_DISPLAY_3D('NEW__')
!       endif
       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_DISPLAY_3D
!!!--------------------------------------------------------------------

END MODULE wrap_DISPLAY_3D
