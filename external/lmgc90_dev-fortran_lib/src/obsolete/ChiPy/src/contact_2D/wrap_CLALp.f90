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
MODULE wrap_CLALp

  !!****h* LMGC90.CHIPY/CLALp
  !! NAME
  !!  module wrap_CLALp
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/CLALp
  !!****


  USE CLALp,ONLY: &
       get_nb_CLALp, &
       get_nb_recup_CLALp, &
       stock_rloc_CLALp, &
       recup_rloc_CLALp, &
       compute_box_CLALp, &
       read_ini_Vloc_Rloc_CLALp, &
       write_xxx_Vloc_Rloc_CLALp, &
       display_prox_tactors_CLALp, &
       coor_prediction_CLALp, &
       creation_tab_visu_CLALp, &
       compute_contact_CLALp, &
       set_halo_CLALp, &
       update_wear_CLALp, &
       RUN_CLALp, &
       CHECK_CLALp, &
       get_write_Vloc_Rloc_CLALp

!       display_contact_energy_CLALp, &
!       display_contact_internal_CLALp, &
  
CONTAINS
  
!!!-------------------------------------------------------

    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      INTEGER :: nb_CLALp
      LOGICAL :: RUN

      RUN=.FALSE.

       !!****e* CLALp/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between CLxxx and ALpxx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CLALp/coor_prediction_CLALp
       !!  LMGC90.CORE/CLALp/creation_tab_visu_CLALp
       !!  LMGC90.CORE/CLALp/compute_contact_CLALp
       !!****
       
       if( .not. check_CLALp() ) then
         return
       end if

       CALL coor_prediction_CLALp

       RUN = RUN_CLALp()

       IF (RUN) THEN
          CALL creation_tab_visu_CLALp
          nb_CLALp = get_nb_CLALp()
          WRITE(*,'(1X,I10,A20)') nb_CLALp,' CLALp roughly found'       
       END IF

       CALL compute_contact_CLALp

       nb_CLALp = get_nb_CLALp()
       WRITE(*,'(1X,I10,A12)') nb_CLALp,' CLALp found'       

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* CLALp/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  CLALp/stock_rloc_CLALp
       !!****

       CALL stock_rloc_CLALp 

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE RecupRloc
      IMPLICIT NONE
      INTEGER :: nb_CLALp
       !!****e* CLALp/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CLALp/recup_rloc_CLALp
       !!****

       CALL recup_rloc_CLALp
       nb_CLALp = get_nb_recup_CLALp()
       WRITE(*,'(1X,I10,A12)') nb_CLALp,' recup CLALp'       
    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE UpdateWear
      IMPLICIT NONE
       !!****e* CLALp/UpdateWear
       !! NAME
       !!  UpdateWear
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLALp/update_wear_CLALp
       !!****

       CALL update_wear_CLALp

    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* CLALp/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CLALp contacts
       !! USES
       !!   LMGC90.CORE/CLALp/write_xxx_Vloc_Rloc_CLALp
       !!****

       CALL write_xxx_Vloc_Rloc_CLALp(2)

    END SUBROUTINE
   
!!!----------------------------------------------------

    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      logical :: write_Vloc_Rloc

       !!****e* CLALp/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CLALp contacts
       !! USES
       !!  LMGC90.CORE/CLALp/write_xxx_Vloc_Rloc_CLALp
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CLALp()
       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_CLALp(1)

    END SUBROUTINE
    
!!!----------------------------------------------------

    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* CLALp/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CLALp contacts
       !! USES
       !!  LMGC90.CORE/CLALp/write_xxx_Vloc_Rloc_CLALp
       !!****

       CALL write_xxx_Vloc_Rloc_CLALp(6)

    END SUBROUTINE

!!!----------------------------------------------------

    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* CLALp/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CLALp/display_prox_tactors_CLALp
       !!****

       CALL display_prox_tactors_CLALp

    END SUBROUTINE

!!!----------------------------------------------------

!!$    SUBROUTINE DisplayContactEnergy
!!$      IMPLICIT NONE
!!$       !!****e* CLALp/DisplayContactEnergy
!!$       !! NAME
!!$       !!  DisplayContactEnergy
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/CLALp/display_contact_energy_CLALp
!!$       !!****
!!$
!!$       CALL display_contact_energy_CLALp
!!$
!!$    END SUBROUTINE
!!$
!!$    SUBROUTINE DisplayContactInternal
!!$      IMPLICIT NONE
!!$       !!****e* CLALp/DisplayContactInternal
!!$       !! NAME
!!$       !!  DisplayContactInternal
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/CLALp/display_contact_internal_CLALp
!!$       !!****
!!$
!!$       CALL display_contact_internal_CLALp
!!$
!!$    END SUBROUTINE

    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* CLALp/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CLALp/compute_box_CLALp
       !!****

       CALL compute_box_CLALp

    END SUBROUTINE

    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* CLALp/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLALp/read_ini_Vloc_Rloc_CLALp
       !!****

       CALL read_ini_Vloc_Rloc_CLALp

    END SUBROUTINE

    SUBROUTINE SetHalo(rvalue1)
      IMPLICIT NONE
      real(kind=8) :: rvalue1
       !!****e* CLALp/SetHalo
       !! NAME
       !!  SetHalo
       !! SYNOPSIS
       !!  SetHalo
       !!  [halo]
       !! INPUTS
       !!  [halo] : 
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLALp/set_halo_CLALp
       !!****

       CALL set_halo_CLALp(rvalue1)

    END SUBROUTINE

END MODULE wrap_CLALp
