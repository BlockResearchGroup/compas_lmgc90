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
MODULE wrap_CDCDx

  !!****h* LMGC90.CHIC/CDCDx
  !! NAME
  !!  module wrap_CDCDx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/CDCDx
  !!****

  USE CHIC

  USE CDCDx,ONLY:&
       coor_prediction_CDCDx,&
       CHECK_CDCDx,&
       RUN_CDCDx, &
       get_write_Vloc_Rloc_CDCDx, &
       read_ini_Vloc_Rloc_CDCDx,&
       write_xxx_Vloc_Rloc_CDCDx,&
       stock_rloc_CDCDx, &
       recup_rloc_CDCDx, &
       smooth_computation_CDCDx, &
       compute_box_CDCDx, &
       creation_tab_visu_CDCDx, &
       compute_contact_CDCDx, &
       display_prox_tactors_CDCDx,&
       get_nb_CDCDx,&
       set_xperiodic_data_CDCDx,&     ! CE QUI SUIT EST EXPERIMENTAL ET NE MARCHE PAS ENCORE !!!!!
       set_yperiodic_data_CDCDx,&
       Set_NbInteractionByContact,&
       Set_ContactRadius,&
       fd_compute_contact_CDCDx
CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_CDCDx

    IMPLICIT NONE

    INTEGER      :: nb_CDCDx
    REAL(kind=8) :: xperiode  = 0.D0,yperiode  = 0.D0
    LOGICAL      :: RUN=.FALSE.,CHECK=.TRUE.,XPERIODIC = .FALSE.,YPERIODIC = .FALSE.
    LOGICAL      :: write_Vloc_Rloc

!fd experimental
    integer      :: nbic = 0
    real(kind=8) :: cr = 0.d0

!!! internal values for tactor selection

    INTEGER,PARAMETER :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , i_real_tactor = 4

    CHARACTER(len=35) :: cout

    CHECK = CHECK_CDCDx()
    
    IF (.NOT.CHECK) RETURN
 
    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1) THEN
       !!****e* CDCDx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between SPHER and SPHER tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CDCDx/coor_prediction_CDCDx
       !!  LMGC90.CORE/CDCDx/creation_tab_visu_CDCDx
       !!  LMGC90.CORE/CDCDx/compute_contact_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')

       CALL coor_prediction_CDCDx

       RUN = RUN_CDCDx()

       IF (RUN) THEN
          CALL creation_tab_visu_CDCDx
          IF(KHOZZZ == 1)THEN
             nb_CDCDx = get_nb_CDCDx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_CDCDx,' CDCDx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_CDCDx
       IF (KHOZZZ == 1) THEN
          nb_CDCDx = get_nb_CDCDx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CDCDx,' CDCDx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* CDCDx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/CDCDx/stock_rloc_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')
       CALL stock_rloc_CDCDx 
       IF (KHOZZZ == 1)THEN
         nb_CDCDx = get_nb_CDCDx(i_verlet_tactor)
                                             !1234567890123
         WRITE(cout,'(1X,I10,A13)') nb_CDCDx,' CDCDx stored'       
         CALL LOGMESCHIC(cout)
       END IF


       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* CDCDx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/CDCDx/recup_rloc_CDCDx
       !!****
       CALL LOGCHIC('CDCDx') 
       CALL recup_rloc_CDCDx
       IF (KHOZZZ == 1) THEN
          nb_CDCDx = get_nb_CDCDx(i_recup_tactor)
                                              !1234567890123456
          WRITE(cout,'(1X,I10,A16)') nb_CDCDx,' CDCDx recovered'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SMOOTH FORCE COMPUTATION')==1) THEN
       !!****e* CDCDx/SMOOTH FORCE COMPUTATION
       !! NAME 
       !!  SMOOTH FORCE COMPUTATION
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CDCDx/smooth_computation_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')
       CALL smooth_computation_CDCDx
       IETZZZ = 1
       RETURN 
    END IF
!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* CDCDx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CDCDx contacts
       !! USES
       !!  LMGC90.CORE/CDCDx/write_xxx_Vloc_Rloc_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')
       CALL write_xxx_Vloc_Rloc_CDCDx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* CDCDx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CDCDx contacts
       !! USES
       !!  LMGC90.CORE/CDCDx/write_xxx_Vloc_Rloc_CDCDx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_CDCDx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('CDCDx') 
          CALL write_xxx_Vloc_Rloc_CDCDx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* CDCDx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CDCDx contacts
       !! USES
       !!  LMGC90.CORE/CDCDx/write_xxx_Vloc_Rloc_CDCDx
       !!****
       CALL LOGCHIC('CDCDx') 
       CALL write_xxx_Vloc_Rloc_CDCDx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* CDCDx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/CDCDx/display_prox_tactors_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')
       CALL display_prox_tactors_CDCDx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* CDCDx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/CDCDx/compute_box_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')
       CALL compute_box_CDCDx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* CDCDx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/CDCDx/read_ini_Vloc_Rloc_CDCDx
       !!****
       CALL LOGCHIC('CDCDx') 
       CALL read_ini_Vloc_Rloc_CDCDx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'XPERIODIC CONDITION')==1) THEN
       !!****e* CDCDx/XPERIODIC CONDITION
       !! NAME
       !!  XPERIODIC CONDITION
       !! SYNOPSIS
       !!  XPERIODIC CONDITION
       !!  [xperiode]
       !! INPUTS
       !!  [xperiode] : periode of periodic condition
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       !!****
       CALL LOGCHIC('CDCDx')
       READ(CHLZZZ(NZZZ+1),*) xperiode
       XPERIODIC=.TRUE.
       CALL set_xperiodic_data_CDCDx(xperiode,XPERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'YPERIODIC CONDITION')==1) THEN
       !!****e* CDCDx/YPERIODIC CONDITION
       !! NAME
       !!  YPERIODIC CONDITION
       !! SYNOPSIS
       !!  YPERIODIC CONDITION
       !!  [yperiode]
       !! INPUTS
       !!  [yperiode] : periode of periodic condition
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       !!****
       CALL LOGCHIC('CDCDx')
       READ(CHLZZZ(NZZZ+1),*) yperiode
       YPERIODIC=.TRUE.
       CALL set_yperiodic_data_CDCDx(yperiode,YPERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'NB INTER BY CONTACT')==1) THEN
       !!****e* CDCDx/NB INTER BY CONTACT
       !! NAME
       !!  NB INTER BY CONTACT
       !! SYNOPSIS
       !!  NB INTER BY CONTACT
       !!  [nb]
       !! INPUTS
       !!  [nb] : number of interactions by contact
       !! PURPOSE
       !!  define the number of interaction by contact (experimental)
       !!****
       CALL LOGCHIC('CDCDx')
       READ(CHLZZZ(NZZZ+1),*) nbic

       CALL set_NbInteractionByContact(nbic)

       IETZZZ = 1

       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CONTACT RADIUS')==1) THEN
       !!****e* CDCDx/CONTACT RADIUS
       !! NAME
       !!  CONTACT RADIUS
       !! SYNOPSIS
       !!  CONTACT RADIUS
       !!  [Rd]
       !! INPUTS
       !!  [Rd] : radius of the contact zone
       !! PURPOSE
       !!  define the contact radius (experimental)
       !!****
       CALL LOGCHIC('CDCDx')
       READ(CHLZZZ(NZZZ+1),*) cr

       CALL set_ContactRadius(cr)

       IETZZZ = 1

       RETURN      
    END IF

    IF (INDEX(CHZZZ,'FD SELECT PROX TACTORS')==1) THEN

       !!****e* CDCDx/FD SELECT PROX TACTORS
       !! NAME 
       !!  FD SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between SPHER and SPHER tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/CDCDx/coor_prediction_CDCDx
       !!  LMGC90.CORE/CDCDx/creation_tab_visu_CDCDx
       !!  LMGC90.CORE/CDCDx/fd_compute_contact_CDCDx
       !!****
       CALL LOGCHIC('CDCDx')

       CALL coor_prediction_CDCDx

       RUN = RUN_CDCDx()

       IF (RUN) THEN
          CALL creation_tab_visu_CDCDx
          IF(KHOZZZ == 1)THEN
             nb_CDCDx = get_nb_CDCDx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_CDCDx,' CDCDx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL fd_compute_contact_CDCDx
       IF (KHOZZZ == 1) THEN
          nb_CDCDx = get_nb_CDCDx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_CDCDx,' CDCDx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      

    END IF




  END SUBROUTINE chic_command_CDCDx
!!!---------------------------------------------------------------------



END MODULE wrap_CDCDx
