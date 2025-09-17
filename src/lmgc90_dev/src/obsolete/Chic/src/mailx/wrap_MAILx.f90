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
MODULE wrap_MAILx                                       

  !!****h* LMGC90.CHIC/MAILX
  !! NAME
  !!  module wrap_MAILx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/MAILX
  !!****

  USE CHIC
  USE overall
  USE MAILx,ONLY:&
       read_in_bodies_MAILx, &
       write_out_bodies_MAILx, &
       write_xxx_gpv_MAILx, &
       add_dof2bodies_MAILx, &
       get_write_GPV_actif_MAILx,&
       set_nb_MAILx, &
       set_nb_bulks_MAILx, &
       set_nodes_of_bulk_MAILx, &
       set_model_and_behav_of_bulk_MAILx, &
       set_nb_nodes_MAILx, &
       set_cooref_nodes_MAILx, &
       build_working_arrays_MAILx, &   
       set_nb_tacts_MAILx, &
       set_tact_MAILx


CONTAINS
!!!----------------------------------------------------
  SUBROUTINE chic_command_MAILx

    IMPLICIT NONE
    
    LOGICAL :: write_GPV_actif=.FALSE.

    IF (INDEX(CHZZZ,'READ BODIES')==1) THEN
       !!****e* MAILX/READ BODIES
       !! NAME
       !!  READ BODIES
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MAILX/read_in_bodies_MAILx
       !!****
       CALL LOGCHIC('MAILx')
       CALL read_in_bodies_MAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'WRITE BODIES')==1) THEN
       !!****e* MAILX/WRITE BODIES
       !! NAME
       !!  WRITE BODIES
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MAILX/write_out_bodies_MAILx
       !!****
       CALL LOGCHIC('MAILx')
       CALL write_out_bodies_MAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST GPV')==1) THEN
       CALL LOGCHIC('MAILx')
       !!****e* MAILX/WRITE LAST GPV
       !! NAME
       !!  WRITE LAST GPV
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MAILX/write_xxx_gpv_MAILx
       !!****
       CALL write_xxx_gpv_MAILx(2)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT GPV')==1) THEN
       CALL LOGCHIC('MAILx')
       !!****e* MAILX/WRITE OUT GPV
       !! NAME
       !!  WRITE OUT GPV
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MAILX/write_xxx_gpv_MAILx
       !!****
       write_GPV_actif = get_write_GPV_actif_MAILx()
       IF (write_GPV_actif) CALL write_xxx_gpv_MAILx(1)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT GPV')==1) THEN
       CALL LOGCHIC('MAILx')
       !!****e* MAILX/DISPLAY OUT GPV
       !! NAME
       !!  DISPLAY OUT GPV
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MAILX/write_xxx_gpv_MAILx
       !!****
       CALL write_xxx_gpv_MAILx(6)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'MAKE BODIES+DOF')==1) THEN
       !!****e* MAILX/MAKE BODIES+DOF
       !! NAME
       !!  MAKE BODIES+DOF
       !! USES
       !!  LMGC90.CORE/MAILX/add_dof2bodies_MAILx
       !!****
       CALL LOGCHIC('MAILx')
       CALL add_dof2bodies_MAILx
       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_MAILx
!!!----------------------------------------------------

  include '../../more_src/mailx/wrap_MAILx.inc'

END MODULE wrap_MAILx
