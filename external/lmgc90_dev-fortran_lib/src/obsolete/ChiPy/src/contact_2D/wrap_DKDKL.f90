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
MODULE wrap_DKDKL

  !!****h* LMGC90.CHIPY/DKDKL
  !! NAME
  !!  module wrap_DKDKL
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKDKL
  !!****

  USE DKDKL,ONLY: &
       get_nb_DKDKL, &
       get_nb_recup_DKDKL, &
       stock_rloc_DKDKL, &
       recup_rloc_DKDKL, &
       smooth_computation_DKDKL, &
       compute_box_DKDKL, &
       read_ini_Vloc_Rloc_DKDKL, &
       write_xxx_Vloc_Rloc_DKDKL, &
       coor_prediction_DKDKL, &
       creation_tab_visu_DKDKL, &
       compute_contact_DKDKL, &
       display_prox_tactors_DKDKL, &
       RUN_DKDKL, &
       CHECK_DKDKL, &
       get_write_Vloc_Rloc_DKDKL

CONTAINS
  
!!!-------------------------------------------------------

!!    CHECK = CHECK_DKDKL()
!!    IF (.NOT.CHECK) RETURN

  SUBROUTINE SelectProxTactors
    IMPLICIT NONE
    
    INTEGER      :: nb_DKDKL
    LOGICAL      :: RUN
       !!****e* DKDKL/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and DISKL tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKDKL/coor_prediction_DKDKL
       !!  LMGC90.CORE/DKDKL/creation_tab_visu_DKDKL
       !!  LMGC90.CORE/DKDKL/compute_contact_DKDKL
       !!****

       if( .not. check_DKDKL() ) then
         return
       end if

       RUN=.FALSE.

       CALL coor_prediction_DKDKL
       
       RUN = RUN_DKDKL()

       IF (RUN) THEN
          CALL creation_tab_visu_DKDKL
          nb_DKDKL = get_nb_DKDKL()
          WRITE(*,'(1X,I10,A20)') nb_DKDKL,' DKDKL roughly found'       
       END IF

       CALL compute_contact_DKDKL
       nb_DKDKL = get_nb_DKDKL()
       WRITE(*,'(1X,I10,A12)') nb_DKDKL,' DKDKL found'       

       RETURN      
  END SUBROUTINE

!!!--------------------------------------------------

  SUBROUTINE StockRloc
    IMPLICIT NONE

       !!****e* DKDKL/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKDKL/stock_rloc_DKDKL
       !!****

       CALL stock_rloc_DKDKL 

       RETURN
  END SUBROUTINE

  SUBROUTINE RecupRloc
    IMPLICIT NONE
    INTEGER      :: nb_DKDKL

       !!****e* DKDKL/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDKL/recup_rloc_DKDKL
       !!****

       CALL recup_rloc_DKDKL

       nb_DKDKL = get_nb_recup_DKDKL()
       WRITE(*,'(1X,I10,A12)') nb_DKDKL,' recup DKDKL'
       RETURN 
  END SUBROUTINE

  SUBROUTINE SmoothForceComputation
    IMPLICIT NONE
       !!****e* DKDKL/SmoothForceComputation
       !! NAME 
       !!  SmoothForceComputation
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDKL/smooth_computation_DKDKL
       !!****
       CALL smooth_computation_DKDKL
       RETURN 
  END SUBROUTINE

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc
    IMPLICIT NONE
       !!****e* DKDKL/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/write_xxx_Vloc_Rloc_DKDKL
       !!****

       CALL write_xxx_Vloc_Rloc_DKDKL(2)

       RETURN 
  END SUBROUTINE
   
  SUBROUTINE WriteOutVlocRloc
    IMPLICIT NONE
    LOGICAL      :: write_Vloc_Rloc
       !!****e* DKDKL/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/write_xxx_Vloc_Rloc_DKDKL
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDKL()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKDKL(1)
       END IF
       RETURN 
  END SUBROUTINE
    
  SUBROUTINE DisplayOutVlocRloc
    IMPLICIT NONE
       !!****e* DKDKL/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/write_xxx_Vloc_Rloc_DKDKL
       !!****

       CALL write_xxx_Vloc_Rloc_DKDKL(6)

       RETURN 
  END SUBROUTINE

  SUBROUTINE DisplayProcTactors
    IMPLICIT NONE
       !!****e* DKDKL/DisplayProcTactors
       !! NAME
       !!  DisplayProcTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKDKL/display_prox_tactors_DKDKL
       !!****

       CALL display_prox_tactors_DKDKL

       RETURN 
  END SUBROUTINE

  SUBROUTINE ComputeBox
    IMPLICIT NONE
       !!****e* DKDKL/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKDKL/compute_box_DKDKL
       !!****

       CALL compute_box_DKDKL

       RETURN 
  END SUBROUTINE

  SUBROUTINE ReadIniVlocRloc
       !!****e* DKDKL/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKDKL/read_ini_Vloc_Rloc_DKDKL
       !!****

       CALL read_ini_Vloc_Rloc_DKDKL

       RETURN 
  END SUBROUTINE

END MODULE wrap_DKDKL
