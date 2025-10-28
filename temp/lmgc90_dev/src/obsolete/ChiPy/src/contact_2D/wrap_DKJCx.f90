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
MODULE wrap_DKJCx

  !!****h* LMGC90.CHIPY/DKJCx
  !! NAME
  !!  module wrap_DKJCx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKJCx
  !!****

  USE DKJCx,ONLY: &
       get_nb_DKJCx, &
       get_nb_recup_DKJCx, &
       stock_rloc_DKJCx, &
       recup_rloc_DKJCx, &
       smooth_computation_DKJCx, &
       compute_box_DKJCx, &
       read_ini_Vloc_Rloc_DKJCx, &
       write_xxx_Vloc_Rloc_DKJCx, &
       display_prox_tactors_DKJCx, &
       coor_prediction_DKJCx, &
       creation_tab_visu_DKJCx, &
       compute_contact_DKJCx, &
       RUN_DKJCx, &
       CHECK_DKJCx, &
       get_write_Vloc_Rloc_DKJCx
  
CONTAINS
  
!!    CHECK = CHECK_DKJCx()
!!    IF (.NOT.CHECK) RETURN

  SUBROUTINE SelectProxTactors
    IMPLICIT NONE
    INTEGER      :: nb_DKJCx
    LOGICAL      :: RUN=.FALSE.

       !!****e* DKJCx/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and JONCx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKJCx/coor_prediction_DKJCx
       !!  LMGC90.CORE/DKJCx/creation_tab_visu_DKJCx
       !!  LMGC90.CORE/DKJCx/compute_contact_DKJCx
       !!****

       if( .not. check_DKJCx() ) then
         return
       end if

             
       CALL coor_prediction_DKJCx
       
       RUN = RUN_DKJCx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKJCx
          nb_DKJCx = get_nb_DKJCx()
          WRITE(*,'(1X,I10,A20)') nb_DKJCx,' DKJCx roughly found'       
       END IF

       CALL compute_contact_DKJCx
       nb_DKJCx = get_nb_DKJCx()
       WRITE(*,'(1X,I10,A12)') nb_DKJCx,' DKJCx found'       

       RETURN      
  END SUBROUTINE

!!!--------------------------------------------------

  SUBROUTINE StockRloc
    IMPLICIT NONE
       !!****e* DKJCx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKJCx/stock_rloc_DKJCx
       !!****

       CALL stock_rloc_DKJCx 

       RETURN
  END SUBROUTINE

  SUBROUTINE RecupRloc
    IMPLICIT NONE
    INTEGER      :: nb_DKJCx
       !!****e* DKJCx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKJCx/recup_rloc_DKJCx
       !!****
       CALL recup_rloc_DKJCx
       nb_DKJCx = get_nb_recup_DKJCx()
       WRITE(*,'(1X,I10,A12)') nb_DKJCx,' recup DKJCx'

       RETURN 
  END SUBROUTINE

  SUBROUTINE SmoothForceComputation
    IMPLICIT NONE
       !!****e* DKJCx/SmoothForceComputation
       !! NAME 
       !!  SmoothForceComputation
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKJCx/smooth_computation_DKJCx
       !!****

       CALL smooth_computation_DKJCx

       RETURN 
  END SUBROUTINE

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc
    IMPLICIT NONE
       !!****e* DKJCx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKJCx contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/write_xxx_Vloc_Rloc_DKJCx
       !!****

       CALL write_xxx_Vloc_Rloc_DKJCx(2)

       RETURN 
  END SUBROUTINE
   
  SUBROUTINE WriteOutVlocRloc
    IMPLICIT NONE
    LOGICAL      :: write_Vloc_Rloc

       !!****e* DKJCx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKJCx contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/write_xxx_Vloc_Rloc_DKJCx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKJCx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKJCx(1)
       END IF
       RETURN 
  END SUBROUTINE
    
  SUBROUTINE DisplayOutVlocRloc
    IMPLICIT NONE
       !!****e* DKJCx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKJCx contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/write_xxx_Vloc_Rloc_DKJCx
       !!****

       CALL write_xxx_Vloc_Rloc_DKJCx(6)

       RETURN 
  END SUBROUTINE

  SUBROUTINE DisplayProcTactors
    IMPLICIT NONE
       !!****e* DKJCx/DisplayProcTactors
       !! NAME
       !!  DisplayProcTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKJCx/display_prox_tactors_DKJCx
       !!****

       CALL display_prox_tactors_DKJCx

       RETURN 
  END SUBROUTINE

  SUBROUTINE ComputeBox
    IMPLICIT NONE
       !!****e* DKJCx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKJCx/compute_box_DKJCx
       !!****

       CALL compute_box_DKJCx

       RETURN 
  END SUBROUTINE

  SUBROUTINE ReadIniVlocRloc
    IMPLICIT NONE
       !!****e* DKJCx/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKJCx/read_ini_Vloc_Rloc_DKJCx
       !!****

       CALL read_ini_Vloc_Rloc_DKJCx

       RETURN 
  END SUBROUTINE
  
END MODULE wrap_DKJCx
