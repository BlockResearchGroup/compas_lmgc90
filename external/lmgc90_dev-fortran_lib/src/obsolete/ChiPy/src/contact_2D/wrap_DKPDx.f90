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
MODULE wrap_DKPDx

  !!****h* LMGC90.CHIPY/DKPDx
  !! NAME
  !!  module wrap_DKPDx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKPDx
  !!****

  USE DKPDx,ONLY: &
       get_nb_DKPDx, &
       get_nb_recup_DKPDx, &
       stock_rloc_DKPDx, &
       recup_rloc_DKPDx, &
       compute_box_DKPDx, &
       read_ini_Vloc_Rloc_DKPDx, &
       write_xxx_Vloc_Rloc_DKPDx, &
       coor_prediction_DKPDx, &
       creation_tab_visu_DKPDx, &
       compute_contact_DKPDx, &
       display_prox_tactors_DKPDx, &
       RUN_DKPDx, &
       CHECK_DKPDx, &
       get_write_Vloc_Rloc_DKPDx
  
CONTAINS
  
!!    CHECK = CHECK_DKPDx()
!!    IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER :: nb_DKPDx
      LOGICAL :: RUN

       !!****e* DKPDx/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and xPSID tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKPDx/coor_prediction_DKPDx
       !!  LMGC90.CORE/DKPDx/creation_tab_visu_DKPDx
       !!  LMGC90.CORE/DKPDx/compute_contact_DKPDx
       !!****

       if( .not. check_DKPDx() ) then
         return
       end if

       CALL coor_prediction_DKPDx

       RUN = RUN_DKPDx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKPDx
          nb_DKPDx = get_nb_DKPDx()
          WRITE(*,'(1X,I10,A20)') nb_DKPDx,' DKPDx roughly found'       
       END IF

       CALL compute_contact_DKPDx
       nb_DKPDx = get_nb_DKPDx()
       WRITE(*,'(1X,I10,A12)') nb_DKPDx,' DKPDx found'       

       RETURN      
    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* DKPDx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKPDx/stock_rloc_DKPDx
       !!****

       CALL stock_rloc_DKPDx 

       RETURN
    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER :: nb_DKPDx

       !!****e* DKPDx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKPDx/recup_rloc_DKPDx
       !!****

       CALL recup_rloc_DKPDx
       nb_DKPDx = get_nb_recup_DKPDx()
       WRITE(*,'(1X,I10,A12)') nb_DKPDx,' recup DKPDx'

       RETURN 
    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE

       !!****e* DKPDx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKPDx contacts
       !! USES
       !!  LMGC90.CORE/DKPDx/write_xxx_Vloc_Rloc_DKPDx
       !!****

       CALL write_xxx_Vloc_Rloc_DKPDx(2)

       RETURN 
    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL ::  write_Vloc_Rloc
       !!****e* DKPDx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKPDx contacts
       !! USES
       !!  LMGC90.CORE/DKPDx/write_xxx_Vloc_Rloc_DKPDx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKPDx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKPDx(1)
       END IF
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* DKPDx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKPDx contacts
       !! USES
       !!  LMGC90.CORE/DKPDx/write_xxx_Vloc_Rloc_DKPDx
       !!****

       CALL write_xxx_Vloc_Rloc_DKPDx(6)

       RETURN 
    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* DKPDx/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKPDx/display_prox_tactors_DKPDx
       !!****

       CALL display_prox_tactors_DKPDx

       RETURN 
    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* DKPDx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKPDx/compute_box_DKPDx
       !!****

       CALL compute_box_DKPDx

       RETURN 
    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* DKPDx/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKPDx/read_ini_Vloc_Rloc_DKPDx
       !!****

       CALL read_ini_Vloc_Rloc_DKPDx

       RETURN 
    END SUBROUTINE

END MODULE wrap_DKPDx
