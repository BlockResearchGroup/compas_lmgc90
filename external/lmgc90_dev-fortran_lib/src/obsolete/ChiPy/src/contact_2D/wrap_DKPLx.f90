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
MODULE wrap_DKPLx

  !!****h* LMGC90.CHIPY/DKPLx
  !! NAME
  !!  module wrap_DKPLx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKPLx
  !!****

  USE DKPLx,ONLY: &
       get_nb_DKPLx, &
       get_nb_recup_DKPLx, &
       stock_rloc_DKPLx, &
       recup_rloc_DKPLx, &
       compute_box_DKPLx, &
       read_ini_Vloc_Rloc_DKPLx, &
       write_xxx_Vloc_Rloc_DKPLx, &
       coor_prediction_DKPLx, &
       creation_tab_visu_DKPLx, &
       compute_contact_DKPLx, &
       display_prox_tactors_DKPLx, &
       RUN_DKPLx, &
       CHECK_DKPLx, &
       get_write_Vloc_Rloc_DKPLx
  
CONTAINS
  
!!    CHECK = CHECK_DKPLx()
!!    IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER      :: nb_DKPLx
      LOGICAL      :: RUN

       !!****e* DKPLx/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and POLYG tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKPLx/coor_prediction_DKPLx
       !!  LMGC90.CORE/DKPLx/creation_tab_visu_DKPLx
       !!  LMGC90.CORE/DKPLx/compute_contact_DKPLx
       !!****

       if( .not. check_DKPLx() ) then
         return
       end if

       CALL coor_prediction_DKPLx
       
       RUN = RUN_DKPLx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKPLx
          nb_DKPLx = get_nb_DKPLx()
          WRITE(*,'(1X,I10,A20)') nb_DKPLx,' DKPLx roughly found'       
       END IF

       CALL compute_contact_DKPLx
       nb_DKPLx = get_nb_DKPLx()
       WRITE(*,'(1X,I10,A12)') nb_DKPLx,' DKPLx found'       

       RETURN      
    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* DKPLx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKPLx/stock_rloc_DKPLx
       !!****

       CALL stock_rloc_DKPLx 

       RETURN
    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER :: nb_DKPLx

       !!****e* DKPLx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKPLx/recup_rloc_DKPLx
       !!****

       CALL recup_rloc_DKPLx

       nb_DKPLx = get_nb_recup_DKPLx()
       WRITE(*,'(1X,I10,A12)') nb_DKPLx,' recup DKPLx'

       RETURN
    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE

       !!****e* DKPLx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKPLx contacts
       !! USES
       !!  LMGC90.CORE/DKPLx/write_xxx_Vloc_Rloc_DKPLx
       !!****

       CALL write_xxx_Vloc_Rloc_DKPLx(2)

       RETURN 
    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc

       !!****e* DKPLx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKPLx contacts
       !! USES
       !!  LMGC90.CORE/DKPLx/write_xxx_Vloc_Rloc_DKPLx
       !!****

       write_Vloc_Rloc = get_write_Vloc_Rloc_DKPLx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKPLx(1)
       END IF

       RETURN 
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE

       !!****e* DKPLx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKPLx contacts
       !! USES
       !!  LMGC90.CORE/DKPLx/write_xxx_Vloc_Rloc_DKPLx
       !!****

       CALL write_xxx_Vloc_Rloc_DKPLx(6)

       RETURN 
    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* DKPLx/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKPLx/display_prox_tactors_DKPLx
       !!****

       CALL display_prox_tactors_DKPLx

       RETURN 
    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* DKPLx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKPLx/compute_box_DKPLx
       !!****

       CALL compute_box_DKPLx

       RETURN 
    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* DKPLx/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKPLx/read_ini_Vloc_Rloc_DKPLx
       !!****

       CALL read_ini_Vloc_Rloc_DKPLx

       RETURN 
    END SUBROUTINE
  
END MODULE wrap_DKPLx
