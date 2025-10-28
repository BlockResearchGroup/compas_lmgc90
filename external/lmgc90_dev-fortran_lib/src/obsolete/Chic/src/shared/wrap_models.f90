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
MODULE wrap_MODELS

  !!****h* LMGC90.CHIC/MODELS
  !! NAME
  !!  module wrap_models
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/MODELS
  !!****

  USE CHIC
  USE models
  USE  ExternalModels

  PRIVATE

  PUBLIC chic_command_models,more_chic_command_models

CONTAINS
!!!------------------------------------------------------------------------
 SUBROUTINE chic_command_models

   IMPLICIT NONE
  
   IF (INDEX(CHZZZ,'READ MODELS')==1) THEN
       !!****e* MODELS/READ MODELS
       !! NAME
       !!  READ MODELS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MODELS/read_models
       !!****
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A6)')' @ ',CHZZZ,'models'
      CALL read_models
      IETZZZ = 1
      RETURN      
   END IF

   IF (INDEX(CHZZZ,'WRITE MODELS')==1) THEN
       !!****e* MODELS/WRITE MODELS
       !! NAME
       !!  WRITE MODELS
       !! PURPOSE
       !!
       !! USES
       !! LMGC90.CORE/MODELS/write_models
       !!****
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A6)')' @ ',CHZZZ,'models'
      CALL write_models
      IETZZZ = 1
      RETURN      
   END IF

   IF (INDEX(CHZZZ,'INIT MODELS')==1) THEN
       !!****e* MODELS/INIT MODELS
       !! NAME
       !!  INIT MODELS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MODELS/init_models
       !!****
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A6)')' @ ',CHZZZ,'models'
      CALL init_models
      IETZZZ = 1
      RETURN      
   END IF

   IF (INDEX(CHZZZ,'INIT EXTERNAL MODELS')==1) THEN
       !!****e* MODELS/INIT EXTERNA MODELS
       !! NAME
       !!  INIT EXTERNA MODELS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MODELS/init_models
       !!****
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A6)')' @ ',CHZZZ,'models'
      CALL init_external_models
      IETZZZ = 1
      RETURN      
   END IF

   IF (INDEX(CHZZZ,'STORE PROPERTIES')==1) THEN
       !!****e* MODELS/STORE PROPERTIES
       !! NAME
       !!  STORE PROPERTIES
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MODELS/store_ppset
       !!****
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A6)')' @ ',CHZZZ,'models'
      CALL store_ppset
      IETZZZ = 1
      RETURN      
   END IF


   IF (INDEX(CHZZZ,'CHECK PROPERTIES')==1) THEN
       !!****e* MODELS/CHECK PROPERTIES
       !! NAME
       !!  CHECK PROPERTIES
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MODELS/store_ppset
       !!****
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A6)')' @ ',CHZZZ,'models'
      CALL check_external_ppset
      IETZZZ = 1
      RETURN      
   END IF

 END SUBROUTINE chic_command_models

 include '../../more_src/shared/inc_models.f90'

END MODULE wrap_MODELS
