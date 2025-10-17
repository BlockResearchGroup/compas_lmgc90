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
MODULE wrap_DKDPx

  !!****h* LMGC90.CHIPY/DKDPx
  !! NAME
  !!  module wrap_DKDPx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKDPx
  !!****

  USE DKDPx,ONLY: &
       get_nb_DKDPx, &
       get_nb_recup_DKDPx, &
       stock_rloc_DKDPx, &
       recup_rloc_DKDPx, &
       compute_box_DKDPx, &
       read_ini_Vloc_Rloc_DKDPx, &
       write_xxx_Vloc_Rloc_DKDPx, &
       coor_prediction_DKDPx, &
       creation_tab_visu_DKDPx, &
       compute_contact_DKDPx, &
       display_prox_tactors_DKDPx, &
       RUN_DKDPx, &
       CHECK_DKDPx, &
       get_write_Vloc_Rloc_DKDPx

CONTAINS
  
!!    CHECK = CHECK_DKDPx()
!!    IF (.NOT.CHECK) RETURN

  SUBROUTINE SelectProxTactors

    IMPLICIT NONE
    LOGICAL      :: RUN=.FALSE.    
    INTEGER      :: nb_DKDPx

       !!****e* DKDPx/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and DISPx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKDPx/coor_prediction_DKDPx
       !!  LMGC90.CORE/DKDPx/creation_tab_visu_DKDPx
       !!  LMGC90.CORE/DKDPx/compute_contact_DKDPx
       !!****

       if( .not. check_DKDPx() ) then
         return
       end if

       CALL coor_prediction_DKDPx
       
       RUN = RUN_DKDPx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKDPx

          nb_DKDPx = get_nb_DKDPx()
          WRITE(*,'(1X,I10,A20)') nb_DKDPx,' DKDPx roughly found'       
       END IF

       CALL compute_contact_DKDPx
       nb_DKDPx = get_nb_DKDPx()
       WRITE(*,'(1X,I10,A12)') nb_DKDPx,' DKDPx found'       

       RETURN      
  END SUBROUTINE

!!!--------------------------------------------------

  SUBROUTINE StockRloc
    IMPLICIT NONE
       !!****e* DKDPx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKDPx/stock_rloc_DKDPx
       !!****

       CALL stock_rloc_DKDPx 

       RETURN
  END SUBROUTINE

  SUBROUTINE RecupRloc
    IMPLICIT NONE
    INTEGER      :: nb_DKDPx
       !!****e* DKDPx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKDPx/recup_rloc_DKDPx
       !!****

       CALL recup_rloc_DKDPx

       nb_DKDPx = get_nb_recup_DKDPx()
       WRITE(*,'(1X,I10,A12)') nb_DKDPx,' recup DKDPx'

       RETURN 
  END SUBROUTINE

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc
    IMPLICIT NONE
       !!****e* DKDPx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDPx contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/write_xxx_Vloc_Rloc_DKDPx
       !!****

       CALL write_xxx_Vloc_Rloc_DKDPx(2)

       RETURN 
  END SUBROUTINE
   
  SUBROUTINE WriteOutVlocRloc
    IMPLICIT NONE
    LOGICAL      :: write_Vloc_Rloc
       !!****e* DKDPx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDPx contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/write_xxx_Vloc_Rloc_DKDPx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDPx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKDPx(1)
       END IF

       RETURN 
  END SUBROUTINE
    
  SUBROUTINE DisplayOutVlocRloc
    IMPLICIT NONE
       !!****e* DKDPx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDPx contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/write_xxx_Vloc_Rloc_DKDPx
       !!****
    CALL write_xxx_Vloc_Rloc_DKDPx(6)
    RETURN 
  END SUBROUTINE

  SUBROUTINE DisplayProxTactors
    IMPLICIT NONE
       !!****e* DKDPx/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKDPx/display_prox_tactors_DKDPx
       !!****
       CALL display_prox_tactors_DKDPx
       RETURN 
  END SUBROUTINE

  SUBROUTINE ComputeBox
       !!****e* DKDPx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKDPx/compute_box_DKDPx
       !!****

       CALL compute_box_DKDPx

       RETURN 
  END SUBROUTINE

  SUBROUTINE ReadIniVlocRloc
       !!****e* DKDPx/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKDPx/read_ini_Vloc_Rloc_DKDPx
       !!****
  
       CALL read_ini_Vloc_Rloc_DKDPx
  
       RETURN 
  END SUBROUTINE

   
END MODULE wrap_DKDPx
