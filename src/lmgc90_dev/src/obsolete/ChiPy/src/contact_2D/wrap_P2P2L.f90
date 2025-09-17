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
MODULE wrap_P2P2L

  !!****h* LMGC90.CHIPY/P2P2L
  !! NAME
  !!  module wrap_P2P2L
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/P2P2L
  !!****

  USE P2P2L,ONLY: &
       get_nb_P2P2L, &
       stock_rloc_P2P2L, &
       recup_rloc_P2P2L, &
       compute_box_P2P2L, &
       read_ini_Vloc_Rloc_P2P2L, &
       write_xxx_Vloc_Rloc_P2P2L, &
       coor_prediction_P2P2L, &
       compute_contact_P2P2L, &
       display_prox_tactors_P2P2L, &
       RUN_P2P2L, &
       CHECK_P2P2L, &
       get_write_Vloc_Rloc_P2P2L
  
CONTAINS
  
!!    CHECK = CHECK_P2P2L()
!!    IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER      :: nb_P2P2L

       !!****e* P2P2L/SelectProxTactor
       !! NAME 
       !!  SelectProxTactor
       !! PURPOSE
       !!  contact detection between PT2DL tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/P2P2L/coor_prediction_P2P2L
       !!  LMGC90.CORE/P2P2L/compute_contact_P2P2L
       !!****
       if( .not. check_P2P2L() ) then
         return
       end if

       CALL coor_prediction_P2P2L
       
       CALL compute_contact_P2P2L
       nb_P2P2L = get_nb_P2P2L()
       WRITE(*,'(1X,I10,A12)') nb_P2P2L,' P2P2L found'       

       RETURN      
    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* P2P2L/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/P2P2L/stock_rloc_P2P2L
       !!****

       CALL stock_rloc_P2P2L 

       RETURN
    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
       !!****e* P2P2L/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/P2P2L/recup_rloc_P2P2L
       !!****

       CALL recup_rloc_P2P2L

    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* P2P2L/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/write_xxx_Vloc_Rloc_P2P2L
       !!****

       CALL write_xxx_Vloc_Rloc_P2P2L(2)
 
    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc

       !!****e* P2P2L/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/write_xxx_Vloc_Rloc_P2P2L
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_P2P2L()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_P2P2L(1)
       END IF

       RETURN 
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* P2P2L/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/write_xxx_Vloc_Rloc_P2P2L
       !!****

       CALL write_xxx_Vloc_Rloc_P2P2L(6)

    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* P2P2L/DisplayProxTactors
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/P2P2L/display_prox_tactors_P2P2L
       !!****

       CALL display_prox_tactors_P2P2L

       RETURN 
    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* P2P2L/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/P2P2L/compute_box_P2P2L
       !!****

       CALL compute_box_P2P2L

       RETURN 
    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* P2P2L/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/P2P2L/read_ini_Vloc_Rloc_P2P2L
       !!****

       CALL read_ini_Vloc_Rloc_P2P2L

    END SUBROUTINE

  
END MODULE wrap_P2P2L
