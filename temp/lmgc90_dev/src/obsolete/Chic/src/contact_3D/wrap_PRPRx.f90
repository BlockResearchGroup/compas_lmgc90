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
MODULE wrap_PRPRx

  !!****h* LMGC90.CHIC/PRPRx
  !! NAME
  !!  module wrap_PRPRx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/PRPRx
  !!****

  USE CHIC
  USE PRPRx,ONLY:&
       coor_prediction_PRPRx,&
       CHECK_PRPRx,&
       RUN_PRPRx, &
       get_write_Vloc_Rloc_PRPRx, &
       read_ini_Vloc_Rloc_PRPRx,&
       write_xxx_Vloc_Rloc_PRPRx,&
       stock_rloc_PRPRx, &
       recup_rloc_PRPRx, &
       compute_box_PRPRx, &
       creation_tab_visu_PRPRx, &
       compute_contact_PRPRx, &
       display_prox_tactors_PRPRx,&
       get_nb_PRPRx, &
       set_cundall_iteration_PRPRx, &
       set_shrink_polyr_faces_PRPRx, &
       set_size_factor_polyr_PRPRx, &
       compute_explicit_contact_PRPRx, &
       creation_tab_visu_to_file_PRPRx, &
       creation_tab_visu_from_file_PRPRx, &
       wcp_compute_contact_PRPRx,&
       set_xperiodic_data_PRPRx, &
       set_yperiodic_data_PRPRx, &
       set_f2f_tol_PRPRx, &
       nc_compute_contact_PRPRx       
  

CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE chic_command_PRPRx

    IMPLICIT NONE

    INTEGER      :: iter=0,nb_PRPRx
    REAL(kind=8) :: periode  = 0.D0,SHRINK=0.5,sfactor
    LOGICAL      :: RUN=.FALSE.,CHECK=.TRUE.
    LOGICAL      :: write_Vloc_Rloc

    INTEGER :: RESTART=0
    LOGICAL :: is_first_time = .TRUE.
    LOGICAL :: save_PRPRx_to_file=.FALSE.,load_PRPRx_from_file=.FALSE.
    CHARACTER(len=2) ::prefix
    CHARACTER(len=35) :: cout

!!! internal values for tactor selection

    INTEGER,PARAMETER :: i_recup_tactor = 0 , i_verlet_tactor = 1 , i_rough_tactor = 2 , i_real_tactor = 4

    LOGICAL      :: XPERIODIC = .FALSE.,YPERIODIC = .FALSE.
    REAL(kind=8) :: xperiode  = 0.D0,yperiode  = 0.D0,tol

    REAL(kind=8) :: gdist

    CHECK = CHECK_PRPRx()
    
    IF (.NOT.CHECK) RETURN

    IF ( INDEX(CHZZZ,'SELECT PROX TACTORS')==1) THEN
       !!****e* PRPRx/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between POLYR tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PRPRx/coor_prediction_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_PRPRx
       !!  LMGC90.CORE/PRPRx/compute_contact_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')

       CALL coor_prediction_PRPRx

       RUN = RUN_PRPRx()
       
       IF (RUN) THEN
          CALL creation_tab_visu_PRPRx
          IF(KHOZZZ == 1)THEN
             nb_PRPRx = get_nb_PRPRx(i_rough_tactor)
             WRITE(cout,'(1X,I10,A20)') nb_PRPRx,' PRPRx roughly found'       
             CALL LOGMESCHIC(cout)
          END IF
       END IF
       CALL compute_contact_PRPRx
       IF (KHOZZZ == 1) THEN
          nb_PRPRx = get_nb_PRPRx(i_real_tactor)
          WRITE(cout,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'WCP SELECT PROX TACTORS')==1) THEN
       !!****e* PRPRx/WCP SELECT PROX TACTORS
       !! NAME 
       !!  WCP SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between POLYHEDRA tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/PRPRx/coor_prediction_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_PRPRx
       !!  LMGC90.CORE/PRPRx/compute_contact_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_to_file_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_from_file_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       
       CALL coor_prediction_PRPRx

       RUN = RUN_PRPRx()

       IF (RUN.OR.RESTART.EQ.0) THEN
          RESTART = 1 
          IF (save_PRPRx_to_file) THEN
             CALL creation_tab_visu_to_file_PRPRx
          ELSE
             IF (load_PRPRx_from_file) THEN
                CALL creation_tab_visu_from_file_PRPRx
             ELSE                
                CALL creation_tab_visu_PRPRx
             END IF
          END IF
       END IF
       CALL wcp_compute_contact_PRPRx
       IETZZZ = 1
       RETURN
    ENDIF

    IF (INDEX(CHZZZ,'NC SELECT PROX TACTORS')==1) THEN
       !!****e* PRPRx/NC SELECT PROX TACTORS
       !! NAME 
       !!  NC SELECT PROX TACTORS
       !! PURPOSE
       !!  contact detection between non convex POLYHEDRA tactors.
       !! USES
       !!  LMGC90.CORE/PRPRx/coor_prediction_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_PRPRx
       !!  LMGC90.CORE/PRPRx/compute_contact_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_to_file_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_from_file_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       READ(chlzzz(nzzz+1),*) gdist       

       CALL coor_prediction_PRPRx

       RUN = RUN_PRPRx()

       IF (RUN.OR.RESTART.EQ.0) THEN
          RESTART = 1 
          IF (save_PRPRx_to_file) THEN
             CALL creation_tab_visu_to_file_PRPRx
          ELSE
             IF (load_PRPRx_from_file) THEN
                CALL creation_tab_visu_from_file_PRPRx
             ELSE                
                CALL creation_tab_visu_PRPRx
             END IF
          END IF
       END IF
       CALL nc_compute_contact_PRPRx(gdist)
       IETZZZ = 1
       RETURN
    ENDIF

    IF (INDEX(CHZZZ,'EXPLICIT PROX TACTORS')==1) THEN
       !!****e* PRPRx/EXPLICIT PROX TACTORS
       !! NAME 
       !!  EXPLICIT PROX TACTORS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/update_position_PRPRx
       !!  LMGC90.CORE/PRPRx/creation_tab_visu_PRPRx
       !!  LMGC90.CORE/PRPRx/compute_explicit_contact_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       READ(chlzzz(nzzz+1),*) prefix
       
       CALL coor_prediction_PRPRx
       
       IF (is_first_time) THEN 
          CALL creation_tab_visu_PRPRx
          is_first_time = .FALSE.
       END IF
       CALL compute_explicit_contact_PRPRx(prefix)
       IETZZZ = 1
       RETURN
    END IF
    
!!!--------------------------------------------------

    IF (INDEX(CHZZZ,'STOCK Rloc')==1) THEN
       !!****e* PRPRx/STOCK Rloc
       !! NAME 
       !!  STOCK Rloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!  LMGC90.CORE/PRPRx/stock_rloc_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       CALL stock_rloc_PRPRx 
       IF (KHOZZZ == 1)THEN
         nb_PRPRx = get_nb_PRPRx(i_verlet_tactor)
                                             !1234567890123
         WRITE(cout,'(1X,I10,A13)') nb_PRPRx,' PRPRx stored'       
         CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'RECUP Rloc')==1) THEN
       !!****e* PRPRx/RECUP Rloc
       !! NAME 
       !!  RECUP Rloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!  LMGC90.CORE/PRPRx/recup_rloc_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       CALL recup_rloc_PRPRx
       IF (KHOZZZ == 1) THEN
          nb_PRPRx = get_nb_PRPRx(i_recup_tactor)
                                              !1234567890123456
          WRITE(cout,'(1X,I10,A16)') nb_PRPRx,' PRPRx recovered'
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN 
    END IF

!!!----------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN     
       !!****e* PRPRx/WRITE LAST Vloc Rloc
       !! NAME
       !!  WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PRPRx contacts
       !! USES
       !!  LMGC90.CORE/PRPRx/write_xxx_Vloc_Rloc_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       CALL write_xxx_Vloc_Rloc_PRPRx(2)
       IETZZZ=1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* PRPRx/WRITE OUT Vloc Rloc
       !! NAME
       !!  WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PRPRx contacts
       !! USES
       !!  LMGC90.CORE/PRPRx/write_xxx_Vloc_Rloc_PRPRx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_PRPRx()
       IF (write_Vloc_Rloc) THEN
          CALL LOGCHIC('PRPRx') 
          CALL write_xxx_Vloc_Rloc_PRPRx(1)
       END IF
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* PRPRx/DISPLAY OUT Vloc Rloc
       !! NAME
       !!  DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PRPRx contacts
       !! USES
       !!  LMGC90.CORE/PRPRx/write_xxx_Vloc_Rloc_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       CALL write_xxx_Vloc_Rloc_PRPRx(6)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* PRPRx/DISPLAY PROX TACTORS'
       !! NAME
       !!  DISPLAY PROX TACTORS'
       !! PURPOSE
       !!  display contacts
       !! USES
       !!  LMGC90.CORE/PRPRx/display_prox_tactors_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       CALL display_prox_tactors_PRPRx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* PRPRx/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!  LMGC90.CORE/PRPRx/compute_box_PRPRx
       !!****
       CALL LOGCHIC('PRPRx')
       CALL compute_box_PRPRx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* PRPRx/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/read_ini_Vloc_Rloc_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       CALL read_ini_Vloc_Rloc_PRPRx
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'CUNDALL ITERATION')==1) THEN
       !!****e* PRPRx/CUNDALL ITERATION
       !! NAME
       !!  CUNDALL ITERATION
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/set_cundall_iteration_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       READ(chlzzz(nzzz+1),*) iter
       CALL set_cundall_iteration_PRPRx(iter)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'F2F TOLERANCE')==1) THEN
       !!****e* PRPRx/F2F TOLERANCE
       !! NAME
       !!  F2F
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/set_f2f_tol_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       READ(chlzzz(nzzz+1),*) tol
       CALL set_f2f_tol_PRPRx(tol)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SHRINK POLYR FACES')==1) THEN
       !!****e* PRPRx/SHRINK POLYR FACES
       !! NAME
       !!  SHRINK POLYR FACES
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/set_shrink_polyr_faces_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       READ(CHLZZZ(NZZZ+1),*) SHRINK
       IF (SHRINK.LT.0.D0 .OR. SHRINK .GT. 1.D0) THEN
          CALL LOGMESCHIC('You should specify a shrink factor in [0,1]')
          STOP
       END IF
       CALL set_shrink_polyr_faces_PRPRx(SHRINK)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'LOW SIZE ARRAY POLYR')==1) THEN
       !!****e* PRPRx/LOW SIZE ARRAY POLYR
       !! NAME
       !!  LOW SIZE ARRAY POLYR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/set_size_factor_polyr_PRPRx
       !!****
       CALL LOGCHIC('PRPRx') 
       READ(CHLZZZ(NZZZ+1),*) sfactor
       CALL set_size_factor_polyr_PRPRx(sfactor)
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'SAVE PROX TACTORS TO FILE')==1) THEN
       !!****e* PRPRx/SAVE PROX TACTORS TO FILE
       !! NAME
       !!  SAVE PROX TACTORS TO FILE
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/save_PRPRx_to_file
       !!****
       CALL LOGCHIC('PRPRx') 
       save_PRPRx_to_file=.TRUE.
       IETZZZ = 1
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'LOAD PROX TACTORS FROM FILE')==1) THEN
       !!****e* PRPRx/LOAD PROX TACTORS FROM FILE
       !! NAME
       !!  LOAD PROX TACTORS FROM FILE
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/PRPRx/load_PRPRx_from_file
       !!****
       CALL LOGCHIC('PRPRx') 
       load_PRPRx_from_file=.TRUE.
       IETZZZ = 1
       RETURN 
    END IF


    IF (INDEX(CHZZZ,'XPERIODIC CONDITION')==1) THEN
       !!****e* PRPRx/XPERIODIC CONDITION
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
       CALL LOGCHIC('PRPRx')
       READ(CHLZZZ(NZZZ+1),*) xperiode
       XPERIODIC=.TRUE.
       CALL set_xperiodic_data_PRPRx(xperiode,XPERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'YPERIODIC CONDITION')==1) THEN
       !!****e* PRPRx/YPERIODIC CONDITION
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
       CALL LOGCHIC('PRPRx')
       READ(CHLZZZ(NZZZ+1),*) yperiode
       YPERIODIC=.TRUE.
       CALL set_yperiodic_data_PRPRx(yperiode,YPERIODIC)
       !IETZZZ = 1
       RETURN      
    END IF


  END SUBROUTINE chic_command_PRPRx
!!!---------------------------------------------------------------------

END MODULE wrap_PRPRx
