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
MODULE wrap_PLALp

  !!****h* LMGC90.CHIPY/PLALp
  !! NAME
  !!  module wrap_PLALp
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/PLALp
  !!****

  USE PLALp,ONLY: &
       get_nb_PLALp, &
       get_nb_recup_PLALp, &
       stock_rloc_PLALp, &
       recup_rloc_PLALp, &
       compute_box_PLALp, &
       read_ini_Vloc_Rloc_PLALp, &
       write_xxx_Vloc_Rloc_PLALp, &
       coor_prediction_PLALp, &
       creation_tab_visu_PLALp, &
       compute_contact_PLALp, &
       display_prox_tactors_PLALp, &
       RUN_PLALp, &
       CHECK_PLALp, &
       get_write_Vloc_Rloc_PLALp

CONTAINS
  
!!    CHECK = CHECK_PLALp()
!!    IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER      :: nb_PLALp
      LOGICAL      :: RUN

       !!****e* PLALp/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and POLYG tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PLALp/coor_prediction_PLALp
       !!  LMGC90.CORE/PLALp/creation_tab_visu_PLALp
       !!  LMGC90.CORE/PLALp/compute_contact_PLALp
       !!****

       if( .not. check_PLALp() ) then
         return
       end if

       CALL coor_prediction_PLALp

       RUN = RUN_PLALp()

       IF (RUN) THEN
         CALL creation_tab_visu_PLALp
         nb_PLALp = get_nb_PLALp()
         WRITE(*,'(1X,I10,A20)') nb_PLALp,' PLALp roughly found'       
       END IF

       CALL compute_contact_PLALp
       nb_PLALp = get_nb_PLALp()
       WRITE(*,'(1X,I10,A12)') nb_PLALp,' PLALp found'       

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* PLALp/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PLALp/stock_rloc_PLALp
       !!****

       CALL stock_rloc_PLALp 

    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER :: nb_PLALp

       !!****e* PLALp/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PLALp/recup_rloc_PLALp
       !!****
       CALL recup_rloc_PLALp
       nb_PLALp = get_nb_recup_PLALp()
       WRITE(*,'(1X,I10,A12)') nb_PLALp,' recup PLALp'
    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* PLALp/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts
       !! USES
       !!  LMGC90.CORE/PLALp/write_xxx_Vloc_Rloc_PLALp
       !!****

       CALL write_xxx_Vloc_Rloc_PLALp(2)

    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc
       !!****e* PLALp/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts
       !! USES
       !!  LMGC90.CORE/PLALp/write_xxx_Vloc_Rloc_PLALp
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PLALp()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_PLALp(1)
       END IF
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* PLALp/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts
       !! USES
       !!  LMGC90.CORE/PLALp/write_xxx_Vloc_Rloc_PLALp
       !!****

       CALL write_xxx_Vloc_Rloc_PLALp(6)

    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE

       !!****e* PLALp/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PLALp/display_prox_tactors_PLALp
       !!****

       CALL display_prox_tactors_PLALp

    END SUBROUTINE
       
    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* PLALp/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/PLALp/compute_box_PLALp
       !!****

       CALL compute_box_PLALp

    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* PLALp/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PLALp/read_ini_Vloc_Rloc_PLALp
       !!****

       CALL read_ini_Vloc_Rloc_PLALp

    END SUBROUTINE
  
END MODULE wrap_PLALp
