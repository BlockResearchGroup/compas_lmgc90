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

  !!****h* LMGC90.CHIC/CLALp
  !! NAME
  !!  module wrap_CLALp
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/CLALp
  !!****

  USE CHIC
  USE CLALp,ONLY: &
       get_nb_CLALp, &
       get_nb_rough_CLALp, &
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
!!$       set_halo_CLALp, &
       update_wear_CLALp, &
       RUN_CLALp, &
       CHECK_CLALp, &
       get_write_Vloc_Rloc_CLALp, &
       set_nonsymmetric_detection_CLALp
!       ,display_contact_energy_CLALp, &
!       display_contact_internal_CLALp
  
CONTAINS
  
!!!-------------------------------------------------------
  SUBROUTINE chic_command_CLALp 
    
    IMPLICIT NONE
    
    INTEGER      :: nb_CLALp,nb_rough_CLALp
    REAL(kind=8) :: periode  = 0.D0, halo=0.D0
    LOGICAL      :: PERIODIC = .FALSE.,RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    CHARACTER(len=35) :: cout

    CHECK = CHECK_CLALp()

    IF (.NOT.CHECK) RETURN
    
    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .OR. &
         INDEX(CHZZZ,'SELECT THETA PROX TACTORS')==1) THEN
       !!****e* CLALp/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between CLxxx and ALpxx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CLALp/coor_prediction_CLALp
       !!  LMGC90.CORE/CLALp/creation_tab_visu_CLALp
       !!  LMGC90.CORE/CLALp/compute_contact_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       
       CALL coor_prediction_CLALp

       RUN = RUN_CLALp()

       IF(RUN)THEN
          CALL creation_tab_visu_CLALp
          IF(KHOZZZ == 1)THEN
             nb_rough_CLALp = get_nb_rough_CLALp()
             WRITE(cout,'(1X,I10,A20)') nb_rough_CLALp,' CLALp roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF

       CALL compute_contact_CLALp

       IF(KHOZZZ == 1)THEN
          nb_CLALp = get_nb_CLALp()
          WRITE(cout,'(1X,I10,A12)') nb_CLALp,' CLALp found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* CLALp/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CLALp/stock_rloc_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       CALL stock_rloc_CLALp 
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* CLALp/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CLALp/recup_rloc_CLALp
       !!****
       CALL LOGCHIC('CLALp') 
       CALL recup_rloc_CLALp
       IF(KHOZZZ == 1)THEN
          nb_CLALp = get_nb_recup_CLALp()
          WRITE(cout,'(1X,I10,A12)') nb_CLALp,' recup CLALp'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'UPDATE WEAR')==1) THEN
       !!****e* CLALp/UPDATE WEAR
       !! NAME
       !!  UPDATE WEAR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLALp/update_wear_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       CALL update_wear_CLALp
       IETZZZ = 1
       RETURN
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* CLALp/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CORE.CLALp contacts
       !! USES
       !!  LMGC90.CORE/CLALp/write_xxx_Vloc_Rloc_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       CALL write_xxx_Vloc_Rloc_CLALp(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* CLALp/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CORE.CLALp contacts
       !! USES
       !!  LMGC90.CORE/CLALp/write_xxx_Vloc_Rloc_CLALp
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CLALp()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('CLALp') 
          CALL write_xxx_Vloc_Rloc_CLALp(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* CLALp/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CORE.CLALp contacts
       !! USES
       !!  LMGC90.CORE/CLALp/write_xxx_Vloc_Rloc_CLALp
       !!****
       CALL LOGCHIC('CLALp') 
       CALL write_xxx_Vloc_Rloc_CLALp(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* CLALp/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CLALp/display_prox_tactors_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       CALL display_prox_tactors_CLALp
       IETZZZ = 1
       RETURN 
    END IF

!    IF (INDEX(CHZZZ,'DISPLAY CONTACTS ENERGY')==1) THEN
!       !!****e* CLALp/DISPLAY CONTACTS ENERGY
!       !! NAME
!       !!  DISPLAY CONTACTS ENERGY
!       !! PURPOSE
!       !!  
!       !! USES
!       !!  LMGC90.CORE/CLALp/display_contact_energy_CLALp
!       !!****
!       CALL LOGCHIC('CLALp')
!       CALL display_contact_energy_CLALp
!       IETZZZ = 1
!       RETURN      
!    END IF

!    IF (INDEX(CHZZZ,'DISPLAY CONTACT INTERNAL')==1) THEN
!       !!****e* CLALp/DISPLAY CONTACT INTERNAL
!       !! NAME
!       !!  DISPLAY CONTACT INTERNAL
!       !! PURPOSE
!       !!  
!       !! USES
!       !!  LMGC90.CORE/CLALp/display_contact_internal_CLALp
!       !!****
!       CALL LOGCHIC('CLALp')
!       CALL display_contact_internal_CLALp
!       IETZZZ = 1
!       RETURN      
!    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* CLALp/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CLALp/compute_box_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       CALL compute_box_CLALp
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* CLALp/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLALp/read_ini_Vloc_Rloc_CLALp
       !!****
       CALL LOGCHIC('CLALp') 
       CALL read_ini_Vloc_Rloc_CLALp
       IETZZZ = 1
       RETURN 
    END IF

!!$    IF (INDEX(CHZZZ,'SET HALO')==1) THEN
!!$       !!****e* CLALp/SET HALO
!!$       !! NAME
!!$       !!  SET HALO
!!$       !! SYNOPSIS
!!$       !!  SET HALO
!!$       !!  [halo]
!!$       !! INPUTS
!!$       !!  [halo] : 
!!$       !! PURPOSE
!!$       !!  
!!$       !! USES
!!$       !!  LMGC90.CORE/CLALp/set_halo_CLALp
!!$       !!****
!!$       CALL LOGCHIC('CLALp')
!!$       READ(CHLZZZ(NZZZ+1),*) halo
!!$       CALL set_halo_CLALp(halo)
!!$       IETZZZ = 1
!!$       RETURN      
!!$    ENDIF
                     
    IF (INDEX(CHZZZ,'SET NONSYMMETRIC DETECTION')==1) THEN
       !!****e* CLALp/SET NONSYMMETRIC DETECTION
       !! NAME
       !!  SET NONSYMMETRIC DETECTION
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CLALp/set_nonsymmetric_detection_CLALp
       !!****
       CALL LOGCHIC('CLALp')
       CALL set_nonsymmetric_detection_CLALp()
       IETZZZ = 1
       RETURN      
    ENDIF

  END SUBROUTINE chic_command_CLALp
!!!-------------------------------------------------------

  include '../../more_src/contact_2D/wrap_CLALp.inc' 

  
END MODULE wrap_CLALp
