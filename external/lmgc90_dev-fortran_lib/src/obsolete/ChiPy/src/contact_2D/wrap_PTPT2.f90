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
MODULE wrap_PTPT2

  !!****h* LMGC90.CHIPY/PTPT2
  !! NAME
  !!  module wrap_PTPT2
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/PTPT2
  !!****

  USE PTPT2,ONLY: &
       get_nb_PTPT2, &
       get_nb_recup_PTPT2, &
       stock_rloc_PTPT2, &
       recup_rloc_PTPT2, &
       compute_box_PTPT2, &
       read_ini_Vloc_Rloc_PTPT2, &
       write_xxx_Vloc_Rloc_PTPT2, &
       coor_prediction_PTPT2, &
       compute_contact_PTPT2, &
       display_prox_tactors_PTPT2, &
       RUN_PTPT2, &
       CHECK_PTPT2, &
       get_write_Vloc_Rloc_PTPT2
  
CONTAINS
  
!!    CHECK = CHECK_PTPT2()
!!    IF (.NOT.CHECK) RETURN

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER      :: nb_PTPT2

       !!****e* PTPT2/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between PT2Dx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PTPT2/coor_prediction_PTPT2
       !!  LMGC90.CORE/PTPT2/compute_contact_PTPT2
       !!****
       
       if( .not. check_PTPT2() ) then
         return
       end if

       CALL coor_prediction_PTPT2
       
       CALL compute_contact_PTPT2
       nb_PTPT2 = get_nb_PTPT2()
       WRITE(*,'(1X,I10,A12)') nb_PTPT2,' PTPT2 found'       

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* PTPT2/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PTPT2/stock_rloc_PTPT2
       !!****

       CALL stock_rloc_PTPT2 

    END SUBROUTINE

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER :: nb_PTPT2

       !!****e* PTPT2/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PTPT2/recup_rloc_PTPT2
       !!****

       CALL recup_rloc_PTPT2
       nb_PTPT2 = get_nb_recup_PTPT2()
       WRITE(*,'(1X,I10,A12)') nb_PTPT2,' recup PTPT2'

    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* PTPT2/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PTPT2 contacts
       !! USES
       !!  LMGC90.CORE/PTPT2/write_xxx_Vloc_Rloc_PTPT2
       !!****

       CALL write_xxx_Vloc_Rloc_PTPT2(2)

    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc
       !!****e* PTPT2/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PTPT2 contacts
       !! USES
       !!  LMGC90.CORE/PTPT2/write_xxx_Vloc_Rloc_PTPT2
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PTPT2()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_PTPT2(1)
       END IF

    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* PTPT2/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PTPT2 contacts
       !! USES
       !!  LMGC90.CORE/PTPT2/write_xxx_Vloc_Rloc_PTPT2
       !!****

       CALL write_xxx_Vloc_Rloc_PTPT2(6)

    END SUBROUTINE

    SUBROUTINE DisplayProcTactors
      IMPLICIT NONE
       !!****e* PTPT2/DisplayProcTactors
       !! NAME
       !!  DisplayProcTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PTPT2/display_prox_tactors_PTPT2
       !!****

       CALL display_prox_tactors_PTPT2

    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* PTPT2/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/PTPT2/compute_box_PTPT2
       !!****

       CALL compute_box_PTPT2

    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* PTPT2/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PTPT2/read_ini_Vloc_Rloc_PTPT2
       !!****

       CALL read_ini_Vloc_Rloc_PTPT2

    END SUBROUTINE

END MODULE wrap_PTPT2
