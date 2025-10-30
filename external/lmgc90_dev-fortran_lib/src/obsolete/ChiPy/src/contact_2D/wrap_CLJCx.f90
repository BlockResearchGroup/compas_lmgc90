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
MODULE wrap_CLJCx

  !!****h* LMGC90.CHIPY/CLJCx
  !! NAME
  !!  module wrap_CLJCx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/CLJCx
  !!****

  USE CLJCx,ONLY: &
       get_nb_CLJCx, &
       get_nb_recup_CLJCx, &
       stock_rloc_CLJCx, &
       recup_rloc_CLJCx, &
       compute_box_CLJCx, &
       read_ini_Vloc_Rloc_CLJCx, &
       write_xxx_Vloc_Rloc_CLJCx, &
       display_prox_tactors_CLJCx, &
       coor_prediction_CLJCx, &
       creation_tab_visu_CLJCx, &
       compute_contact_CLJCx, &
       RUN_CLJCx, &
       CHECK_CLJCx, &
       get_write_Vloc_Rloc_CLJCx
  
CONTAINS
  
!!    CHECK = CHECK_CLJCx()
!!    IF (.NOT.CHECK) RETURN
    
  SUBROUTINE SelectProxTactors
    IMPLICIT NONE
    INTEGER      :: nb_CLJCx
    LOGICAL      :: RUN

    !!****e* CLJCx/SelectProxTactors
    !! NAME 
    !!  SelectProxTactors
    !! PURPOSE
    !!  contact detection between CLxxx and JONCx tactors.
    !!  first recup coordinate prediction, then proceed to a box selection to found rough
    !!  contact list and finally compute the final contact list
    !! USES
    !!  LMGC90.CORE/CLJCx/coor_prediction_CLJCx
    !!  LMGC90.CORE/CLJCx/creation_tab_visu_CLJCx
    !!  LMGC90.CORE/CLJCx/compute_contact_CLJCx
    !!****
       if( .not. check_CLJCx() ) then
         return
       end if

       RUN=.FALSE.

       CALL coor_prediction_CLJCx

       RUN = RUN_CLJCx()

       IF (RUN) THEN
          CALL creation_tab_visu_CLJCx
          nb_CLJCx = get_nb_CLJCx()
          WRITE(*,'(1X,I10,A20)') nb_CLJCx,' CLJCx roughly found'       
       END IF

       CALL compute_contact_CLJCx

       nb_CLJCx = get_nb_CLJCx()
       WRITE(*,'(1X,I10,A12)') nb_CLJCx,' CLJCx found'       

       RETURN      
    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* CLJCx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CLJCx/stock_rloc_CLJCx
       !!****
       CALL stock_rloc_CLJCx 
       RETURN
    END SUBROUTINE

    SUBROUTINE RecupRloc
       IMPLICIT NONE
       INTEGER      :: nb_CLJCx
       !!****e* CLJCx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CLJCx/recup_rloc_CLJCx
       !!****
       CALL recup_rloc_CLJCx
       nb_CLJCx = get_nb_recup_CLJCx()
       WRITE(*,'(1X,I10,A12)') nb_CLJCx,' recup CLJCx'
       RETURN 
    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* CLJCx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CLJCx contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/write_xxx_Vloc_Rloc_CLJCx
       !!****

       CALL write_xxx_Vloc_Rloc_CLJCx(2)

       RETURN 
    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      logical :: write_Vloc_Rloc
       !!****e* CLJCx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CLJCx contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/write_xxx_Vloc_Rloc_CLJCx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CLJCx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_CLJCx(1)
       END IF
       RETURN 
    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* CLJCx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CLJCx contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/write_xxx_Vloc_Rloc_CLJCx
       !!****

       CALL write_xxx_Vloc_Rloc_CLJCx(6)

       RETURN 
    END SUBROUTINE

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* CLJCx/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CLJCx/display_prox_tactors_CLJCx
       !!****

       CALL display_prox_tactors_CLJCx

       RETURN 
    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* CLJCx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CLJCx/compute_box_CLJCx
       !!****

       CALL compute_box_CLJCx

       RETURN 
    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* CLJCx/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLJCx/read_ini_Vloc_Rloc_CLJCx
       !!****
       CALL read_ini_Vloc_Rloc_CLJCx

       RETURN 
    END SUBROUTINE

END MODULE wrap_CLJCx
