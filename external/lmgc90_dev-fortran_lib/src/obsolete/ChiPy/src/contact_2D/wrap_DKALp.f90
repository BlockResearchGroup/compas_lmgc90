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
MODULE wrap_DKALp

  !!****h* LMGC90.CHIPY/DKALp
  !! NAME
  !!  module wrap_DKALp
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKALp
  !!****

  USE DKALp,ONLY: &
       get_nb_DKALp, &
       get_nb_recup_DKALp, &
       stock_rloc_DKALp, &
       recup_rloc_DKALp, &
       compute_box_DKALp, &
       read_ini_Vloc_Rloc_DKALp, &
       write_xxx_Vloc_Rloc_DKALp, &
       coor_prediction_DKALp, &
       creation_tab_visu_DKALp, &
       compute_contact_DKALp, &
       display_prox_tactors_DKALp, &
       RUN_DKALp, &
       CHECK_DKALp, &
       get_write_Vloc_Rloc_DKALp

CONTAINS
  
!!    CHECK = CHECK_DKALp()
!!    IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      LOGICAL      :: RUN
      INTEGER      :: nb_DKALp
       !!****e* DKALp/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx and ALpxx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKALp/coor_prediction_DKALp
       !!  LMGC90.CORE/DKALp/creation_tab_visu_DKALp
       !!  LMGC90.CORE/DKALp/compute_contact_DKALp
       !!****
       if( .not. check_DKALp() ) then
         return
       end if

       RUN=.FALSE.

       CALL coor_prediction_DKALp
       
       RUN = RUN_DKALp()

       IF (RUN) THEN
          CALL creation_tab_visu_DKALp
          nb_DKALp = get_nb_DKALp()
          WRITE(*,'(1X,I10,A20)') nb_DKALp,' DKALp roughly found'       
       END IF

       CALL compute_contact_DKALp
       nb_DKALp = get_nb_DKALp()
       WRITE(*,'(1X,I10,A12)') nb_DKALp,' DKALp found'       

       RETURN      
    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* DKALp/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/DKALp/stock_rloc_DKALp
       !!****

       CALL stock_rloc_DKALp 

       RETURN
    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER      :: nb_DKALp

       !!****e* DKALp/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/DKALp/recup_rloc_DKALp
       !!****

       CALL recup_rloc_DKALp

       nb_DKALp = get_nb_recup_DKALp()
       WRITE(*,'(1X,I10,A12)') nb_DKALp,' recup DKALp'

       RETURN 
    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* DKALp/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts
       !! USES
       !!  LMGC90.CORE/DKALp/write_xxx_Vloc_Rloc_DKALp
       !!****

       CALL write_xxx_Vloc_Rloc_DKALp(2)

       RETURN 
    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc

       !!****e* DKALp/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts
       !! USES
       !!  LMGC90.CORE/DKALp/write_xxx_Vloc_Rloc_DKALp
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKALp()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKALp(1)
       END IF

       RETURN 
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* DKALp/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts
       !! USES
       !!  LMGC90.CORE/DKALp/write_xxx_Vloc_Rloc_DKALp
       !!****

       CALL write_xxx_Vloc_Rloc_DKALp(6)

       RETURN 
    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* DKALp/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/DKALp/display_prox_tactors_DKALp
       !!****

       CALL display_prox_tactors_DKALp

       RETURN 
    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* DKALp/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/DKALp/compute_box_DKALp
       !!****

       CALL compute_box_DKALp

       RETURN 
    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* DKALp/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/DKALp/read_ini_Vloc_Rloc_DKALp
       !!****

       CALL read_ini_Vloc_Rloc_DKALp

       RETURN 
    END SUBROUTINE

END MODULE wrap_DKALp
