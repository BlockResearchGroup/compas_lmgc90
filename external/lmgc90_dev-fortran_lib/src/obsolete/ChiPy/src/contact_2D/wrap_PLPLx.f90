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
MODULE wrap_PLPLx

  !!****h* LMGC90.CHIPY/PLPLx
  !! NAME
  !!  module wrap_PLPLx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/PLPLx
  !!****

  USE PLPLx,ONLY: &
       get_nb_PLPLx, &
       get_nb_recup_PLPLx, &
       stock_rloc_PLPLx, &
       recup_rloc_PLPLx, &
       compute_box_PLPLx, &
       read_ini_Vloc_Rloc_PLPLx, &
       write_xxx_Vloc_Rloc_PLPLx, &
       coor_prediction_PLPLx, &
       creation_tab_visu_PLPLx, &
       compute_contact_PLPLx, &
       display_prox_tactors_PLPLx, &
       RUN_PLPLx, &
       CHECK_PLPLx, &
       get_write_Vloc_Rloc_PLPLx, &
       set_periodic_data_PLPLx
  
CONTAINS
  
!!    CHECK = CHECK_PLPLx()
!!   IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER      :: nb_PLPLx
      LOGICAL      :: RUN
       !!****e* PLPLx/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between POLYG tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PLPLx/coor_prediction_PLPLx
       !!  LMGC90.CORE/PLPLx/creation_tab_visu_PLPLx
       !!  LMGC90.CORE/PLPLx/compute_contact_PLPLx
       !!****
          
       if( .not. check_PLPLx() ) then
         return
       end if

       CALL coor_prediction_PLPLx
       
       RUN = RUN_PLPLx()

       IF (RUN) THEN
          CALL creation_tab_visu_PLPLx
          nb_PLPLx = get_nb_PLPLx()
          WRITE(*,'(1X,I10,A20)') nb_PLPLx,' PLPLx roughly found'       
       END IF

       CALL compute_contact_PLPLx
       nb_PLPLx = get_nb_PLPLx()
       WRITE(*,'(1X,I10,A12)') nb_PLPLx,' PLPLx found'       

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* PLPLx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PLPLx/stock_rloc_PLPLx
       !!****

       CALL stock_rloc_PLPLx 

    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER :: nb_PLPLx
       !!****e* PLPLx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PLPLx/recup_rloc_PLPLx
       !!****

       CALL recup_rloc_PLPLx
       nb_PLPLx = get_nb_recup_PLPLx()
       WRITE(*,'(1X,I10,A12)') nb_PLPLx,' recup PLPLx'

    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* PLPLx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PLPLx contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/write_xxx_Vloc_Rloc_PLPLx
       !!****

       CALL write_xxx_Vloc_Rloc_PLPLx(2)

    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL write_Vloc_Rloc
       !!****e* PLPLx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PLPLx contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/write_xxx_Vloc_Rloc_PLPLx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PLPLx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_PLPLx(1)
       END IF
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* PLPLx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PLPLx contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/write_xxx_Vloc_Rloc_PLPLx
       !!****

       CALL write_xxx_Vloc_Rloc_PLPLx(6)

    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* PLPLx/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PLPLx/display_prox_tactors_PLPLx
       !!****

       CALL display_prox_tactors_PLPLx

    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* PLPLx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/PLPLx/compute_box_PLPLx
       !!****

       CALL compute_box_PLPLx

    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE

        CALL read_ini_Vloc_Rloc_PLPLx

    END SUBROUTINE

!!!--------------------------------------------------
    SUBROUTINE SetPeriodicCondition(rvalue1)
      IMPLICIT NONE
      real(kind=8) :: rvalue1
      logical      :: periodic

       PERIODIC=.TRUE.
       CALL set_periodic_data_PLPLx(rvalue1,PERIODIC)

    END SUBROUTINE


END MODULE wrap_PLPLx
