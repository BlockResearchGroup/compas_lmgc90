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
MODULE wrap_RBDY2

  !!****h* LMGC90.CHIC/RBDY2
  !! NAME
  !!  module WRAP_RBDY2
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/RBDY2
  !!****

  USE CHIC

  use overall, only: get_time, get_time_step

  USE RBDY2, only: &
       increment_RBDY2, &
       comp_dof_RBDY2, &
       update_dof_RBDY2, &
       comp_free_vlocy_RBDY2, &
       comp_Fext_RBDY2, &
       comp_Fint_RBDY2, &
       check_equilibrium_state_RBDY2, &
       ghost2invisible_RBDY2, &
       check_source_point_RBDY2, &
       out_of_bounds_RBDY2, &
       fatal_damping_RBDY2, &
       partial_damping_RBDY2, &
       init_construct_wall, &
       construct_wall, &
       read_in_bodies_RBDY2, &
       update_existing_entities_RBDY2, &
       read_in_dof_RBDY2, &
       binary_read_in_dof_RBDY2, &
       read_in_driven_dof_RBDY2, &
       read_behaviours_RBDY2, &
       write_out_bodies_RBDY2, &
       write_out_cleared_bodies_RBDY2, &
       write_xxx_dof_RBDY2, &
       binary_write_last_dof_RBDY2, &
       binary_write_out_dof_RBDY2, &
       write_xxx_Rnod_RBDY2, &
       write_out_driven_dof_RBDY2, &
       comp_mass_RBDY2, &
       set_periodic_data_RBDY2, &
       resize_RBDY2, &
       nullify_X_dof_RBDY2, &
       nullify_V_dof_RBDY2, &
       init_source_point_RBDY2, &
       set_init_boundary_RBDY2, &
       set_data_equilibrium_RBDY2, &
       add_dof2bodies_RBDY2, &
       get_nb_RBDY2, &
       get_write_Rnod_RBDY2, &
       get_write_DOF_RBDY2, &
       read_mp_behaviours_rbdy2, &
       update_WS_RBDY2, &
       compute_window_forces, &
       compute_window_velocity, &
       increment_window_velocity, &
       set_init_cnstrt_window, &
       biaxial_loading,&
       Oedemetric_test,&
       Biaxial_def_walls,&
       shear_def_walls, &
       set_dilatation_increment, &
       update_dilatation, &
       init_free_boundary_RBDY2

  PRIVATE

  PUBLIC chic_command_RBDY2,more_chic_command_RBDY2

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE chic_command_RBDY2

    IMPLICIT NONE

    INTEGER :: ifrom,ito,lc,IGOZZZ
    INTEGER :: nfich,first_RBDY2,nb_RBDY2
    INTEGER :: ii,l_ii,iv

    REAL(kind=8)     :: homo = 1.D0
    REAL(kind=8)     :: periode  = 0.D0
    LOGICAL,SAVE     :: PERIODIC = .FALSE.,check_convergence,WINDOWS=.FALSE.
    LOGICAL          :: write_Rnod,write_DOF
    REAL(kind=8)     :: radius,Xshift,Yshift,limit,tol
    REAL(kind=8)     :: xi,yi,x0,y0,xf,yf,nx,ny,Vmax,disper
    CHARACTER(len=5) :: checktype,behav
    REAL(kind=8)     :: Xmin,Xmax,cv

    ! gestion des membranes et autres boites
    ! les numeros des corps (1:down/2:right/3:up/4:left) formant la boite
    integer                                 :: num_down,num_right,num_up,num_left 
    integer                                 :: inb_loads=0,nb_loads=0
    real(kind=8),dimension(:),allocatable   :: loads  
    real(kind=8)                            :: Finitial,Ffinal,DeltaT
    real(kind=8)                            :: sigma
    real(kind=8)                            :: thickness,lenght
    character(len=5)                        :: membrana_color_Right,membrana_color_Left,mcolor




    IF (INDEX(CHZZZ,'INCREMENT STEP')==1) THEN
       !!****e* RBDY2/INCREMENT STEP
       !! NAME
       !!   INCREMENT STEP
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/RBDY2/increment_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL increment_RBDY2
       IF (WINDOWS) CALL increment_window_velocity
       IETZZZ = 1
       RETURN      
    END IF

    IF ( INDEX(CHZZZ,'MD INCREMENT STEP')==1.OR.&
         INDEX(CHZZZ,'DEM INCREMENT STEP')==1) THEN
       CALL LOGCHIC('THIS CHIC COMMAND HAVE BEEN REPLACED')
       CALL LOGCHIC('YOU MUST DEFINE YOUR TIME INTEGRATOR')
       STOP
    END IF

    IF (INDEX(CHZZZ,'COMPUTE DOF')==1) THEN
       !!****e* RBDY2/COMPUTE DOF
       !! NAME
       !!  COMPUTE DOF
       !! PURPOSE
       !!  correction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/RBDY2/comp_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL comp_dof_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE DOF')==1) THEN
       !!****e* RBDY2/UPDATE DOF
       !! NAME
       !!  UPDATE DOF
       !! PURPOSE
       !!  save d.o.f. of the end of the time step to d.o.f. of the begining 
       !!  of the next one
       !! USES
       !!  LMGC90.CORE/RBDY2/update_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL update_dof_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE WSvsT')==1) THEN
       !!****e* RBDY2/UPDATE WSvsT
       !! NAME
       !!  UPDATE WSvsT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY2/update_WS_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL update_WS_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INCREMENT WSvsT')==1) THEN
       !!****e* RBDY2/INCREMENT WSvsT
       !! NAME
       !!  INCREMENT WSvsT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY2/update_WS_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL update_WS_RBDY2
       IETZZZ = 1
       RETURN      
    END IF


    IF ( (INDEX(CHZZZ,'COMPUTE FREE VELOCITY')==1) .OR. &
         (INDEX(CHZZZ,'COMPUTE NL FREE VELOCITY')==1))THEN
       !!****e* RBDY2/COMPUTE FREE VELOCITY
       !! NAME
       !!  COMPUTE FREE VELOCITY
       !!  COMPUTE NL FREE VELOCITY
       !! PURPOSE
       !!  compute free velocity with external forces only
       !! USES
       !!  LMGC90.CORE/RBDY2/comp_free_vlocy_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL comp_free_vlocy_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE Fext')==1) THEN
       !!****e* RBDY2/COMPUTE Fext
       !! NAME
       !!  COMPUTE Fext
       !! PURPOSE
       !!  compute external forces
       !! USES
       !!  LMGC90.CORE/RBDY2/comp_Fext_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL comp_Fext_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF ( (INDEX(CHZZZ,'COMPUTE Fint')==1) .OR. &
         (INDEX(CHZZZ,'COMPUTE TTFint')==1)) THEN

       CALL LOGCHIC('RBDY2')
       
       print*,'OBSOLETE KEYWORD USE COMPUTE BULK INSTEED'
       STOP

    ENDIF

    IF ( INDEX(CHZZZ,'COMPUTE NL BULK')==1.OR. &
         INDEX(CHZZZ,'COMPUTE BULK')==1) THEN
       !!****e* RBDY2/COMPUTE BULK
       !! NAME
       !!  COMPUTE BULK
       !!  COMPUTE NL BULK
       !! PURPOSE
       !!  compute internal forces
       !! USES
       !!  LMGC90.CORE/RBDY2/comp_Fint_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL comp_Fint_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CHECK EQUILIBRIUM STATE')==1) THEN
       !!****e* RBDY2/CHECK EQUILIBRIUM STATE
       !! NAME
       !!   CHECK EQUILIBRIUM STATE 
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0
       !! USES
       !!  LMGC90.CORE/RBDY2/check_equilibrium_state_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       IFLZZZ(NZZZ,1)=1
       check_convergence = .FALSE.
       CALL check_equilibrium_state_RBDY2(check_convergence)
       IF (check_convergence) IFLZZZ(NZZZ,1) = 0
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'GHOST TO INVISIBLE')==1) THEN
       !!****e* RBDY2/GHOST TO INVISIBLE
       !! NAME
       !!  GHOST TO INVISBLE
       !! PURPOSE
       !!  turn off the flag visible when body behav is equal to ghost
       !! USES
       !!  LMGC90.CORE/RBDY2/ghost2invisible_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL ghost2invisible_RBDY2 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CHECK SOURCE POINT')==1) THEN
       !!****e* RBDY2/CHECK SOURCE POINT
       !! NAME
       !!  CHECK SOURCE POINT
       !! PURPOSE
       !!  check if one more particle must be add on the system
       !! USES
       !! LMGC90.CORE/RBDY2/check_source_point_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL check_source_point_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CHECK OUT OF BOUNDS')==1) THEN
       !!****e* RBDY2/CHECK OUT OF BOUNDS
       !! NAME
       !!  CHECH OUT OF BOUNDS
       !! PURPOSE
       !!  check if body go out the defined bounds
       !! USES
       !!  LMGC90.CORE/RBDY2/out_of_bounds_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       PRINT *,'OBSOLETE KEYWORD: SHOULD BE REMOVED' 
!       CALL out_of_bounds_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'FATAL DAMPING')==1) THEN
       !!****e* RBDY2/FATAL DAMPING
       !! NAME
       !!  FATAL DAMPING
       !! PURPOSE
       !!  Nullify body velocities
       !! USES
       !!  LMGC90.CORE/RBDY2/fatal_damping_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       iv = 1
       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
          ii   = INDEX(CHZZZ,'STEP')
          l_ii = LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii .NE. 0) READ(CHZZZ(ii+4:30),*) iv
       END IF
       CALL fatal_damping_RBDY2(iv)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'PARTIAL DAMPING')==1) THEN
       !!****e* RBDY2/PARTIAL DAMPING
       !! NAME
       !!   PARTIAL DAMPING
       !! SYNOPSIS
       !!  PARTIAL DAMPING
       !!  [Vmax]
       !! INPUTS
       !!  [Vmax] : maximal velocity
       !! PURPOSE
       !!  reduce body velocity greater than Vmax to the Vmax value
       !! USES
       !!  LMGC90.CORE/RBDY2/partial_damping_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) Vmax
       iv = 1
       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
          ii  = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii .NE. 0) READ(CHZZZ(ii+4:30),*) iv
       END IF
       CALL partial_damping_RBDY2(iv,Vmax)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CONSTRUCT WALL')==1) THEN
       !!****e* RBDY2/CONSTRUCT WALL
       !! NAME
       !!  CONSTRUCT WALL
       !! PURPOSE
       !!  build a wall of rigid bodies
       !! USES
       !!  LMGC90.CORE/RBDY2/construct_wall
       !!****
       CALL LOGCHIC('RBDY2')
       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
          ii = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) READ(CHZZZ(ii+4:30),*) iv
       ELSE
          iv=1
       END IF
       CALL construct_wall(iv)
       IETZZZ = 1
       RETURN      
    END IF

!!! WRITING COMMAND ------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST DOF')==1) THEN     
       !!****e* RBDY2/WRITE LAST DOF
       !! NAME
       !!   WRITE LAST DOF
       !! PURPOSE
       !!  write ascii DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_RBDY2 = get_nb_RBDY2()
          ifrom = 1
          ito   = nb_RBDY2
          CALL write_xxx_dof_RBDY2(2,ifrom,ito)          
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'BINARY WRITE LAST DOF')==1) THEN     
       !!****e* RBDY2/BINARY WRITE LAST DOF
       !! NAME
       !!   BINARY WRITE LAST DOF
       !! PURPOSE
       !!  write binary DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY2/binary_write_last_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL binary_write_last_dof_RBDY2
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'BINARY WRITE OUT DOF')==1) THEN     
       !!****e* RBDY2/BINARY WRITE OUT DOF
       !! NAME
       !!  BINARY WRITE OUT DOF
       !! PURPOSE
       !!  write binary DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY2/binary_write_out_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       write_DOF = get_write_DOF_RBDY2()
       IF (write_DOF) CALL binary_write_out_dof_RBDY2
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT DOF')==1) THEN     
       !!****e* RBDY2/WRITE OUT DOF
       !! NAME
       !!   WRITE OUT DOF
       !! PURPOSE
       !!   write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       write_DOF = get_write_DOF_RBDY2()
       IF (write_DOF) THEN
          nb_RBDY2 = get_nb_RBDY2()
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
             ifrom = 1
             ito   = nb_RBDY2
             CALL write_xxx_dof_RBDY2(1,ifrom,ito)
          ELSE
             IGOZZZ = 0
             DO
                IGOZZZ = IGOZZZ + 1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY2') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                      ifrom = max0(ifrom,1)
                      ito   = min0(ito,nb_RBDY2)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_dof_RBDY2(1,ifrom,ito)
                   END IF
                ELSE
                   EXIT
                END IF
                CYCLE
             END DO
          END IF
       END IF
       IETZZZ = 1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT DOF')==1) THEN
       !!****e* RBDY2/DISPLAY OUT DOF
       !! NAME
       !!   DISPLAY OUT DOF
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       nb_RBDY2 = get_nb_RBDY2()
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          ifrom=1  ; ito=nb_RBDY2
          CALL write_xxx_dof_RBDY2(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY2') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   ifrom=max0(ifrom,1) ; ito=min0(ito,nb_RBDY2)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_dof_RBDY2(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST Rnod')==1) THEN     
       !!****e* RBDY2/WRITE LAST Rnod
       !! NAME
       !!   WRITE LAST Rnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_Rnod_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_RBDY2 = get_nb_RBDY2()
          ifrom = 1  
          ito   = nb_RBDY2
          CALL write_xxx_Rnod_RBDY2(2,ifrom,ito)
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT Rnod')==1) THEN
       !!****e* RBDY2/WRITE OUT Rnod
       !! NAME
       !!   WRITE OUT Rnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_Rnod_RBDY2
       !!****
       write_Rnod = get_write_Rnod_RBDY2()
       IF (write_Rnod) THEN
          CALL LOGCHIC('RBDY2')
          nb_RBDY2 = get_nb_RBDY2()
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
             ifrom = 1
             ito   = nb_RBDY2
             CALL write_xxx_Rnod_RBDY2(1,ifrom,ito)
          ELSE
             IGOZZZ=0
             DO
                IGOZZZ=IGOZZZ+1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY2') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                      ifrom = max0(ifrom,1)
                      ito   = min0(ito,nb_RBDY2)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_Rnod_RBDY2(1,ifrom,ito)
                   END IF
                ELSE
                   EXIT
                END IF
                CYCLE
             END DO
          END IF
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT Rnod')==1) THEN
       !!****e* RBDY2/DISPLAY OUT Rnod
       !! NAME
       !!   DISPLAY OUT Rnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_Rnod_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       nb_RBDY2 = get_nb_RBDY2()
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          ifrom = 1  
          ito   = nb_RBDY2
          CALL write_xxx_Rnod_RBDY2(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY2') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   ifrom = max0(ifrom,1) 
                   ito   = min0(ito,nb_RBDY2)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_Rnod_RBDY2(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE BODIES')==1) THEN
       !!****e* RBDY2/WRITE BODIES
       !! NAME
       !!   WRITE BODIES
       !! PURPOSE
       !!  write BODIES.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_out_bodies_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')

       CALL write_out_bodies_RBDY2

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CLEARED WRITE BODIES')==1) THEN
       !!****e* RBDY2/CLEARED WRITE BODIES
       !! NAME
       !!   CLEARED WRITE BODIES
       !! PURPOSE
       !!  ...
       !! USES
       !!  LMGC90.CORE/RBDY2/write_out_cleared_bodies_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')

       CALL write_out_cleared_bodies_RBDY2

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'WRITE DRIVEN DOF')==1) THEN
       !!****e* RBDY2/WRITE DRIVEN DOF
       !! NAME
       !!   WRITE DRIVEN DOF
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_out_driven_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')

       CALL write_out_driven_dof_RBDY2

       IETZZZ = 1
       RETURN      
    END IF

!!! READING COMMAND ----------------------------------------------------

    IF (INDEX(CHZZZ,'READ BODIES')==1) THEN
       !!****e* RBDY2/READ BODIES
       !! NAME
       !!   READ BODIES
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable in RBDY2
       !!  Adds the number of found bodies to entity
       !! USES
       !!  LMGC90.CORE/RBDY2/read_in_bodies_RBDY2
       !!  LMGC90.CORE/RBDY2/update_existing_entities_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')

       CALL read_in_bodies_RBDY2
       CALL update_existing_entities_RBDY2

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ INI DOF')==1) THEN
       !!****e* RBDY2/READ INI DOF
       !! NAME
       !!   READ INI DOF
       !! PURPOSE
       !!  read DOF.INI file
       !! USES
       !!  LMGC90.CORE/RBDY2/read_in_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')

       CALL read_in_dof_RBDY2

       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ DRIVEN DOF')==1) THEN
       !!****e* RBDY2/READ DRIVEN DOF
       !! NAME
       !!   READ DRIVEN DOF
       !! PURPOSE
       !!  read DRV_DOF.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY2/read_in_driven_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')

       CALL read_in_driven_dof_RBDY2

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'BINARY READ INI DOF')==1) THEN
       !!****e* RBDY2/BINARY READ INI DOF
       !! NAME
       !!   BINARY READ INI DOF
       !! PURPOSE
       !!   read binary file DOF.INI
       !! USES
       !!  LMGC90.CORE/RBDY2/binary_read_in_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL binary_read_in_dof_RBDY2
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ BEHAVIOURS')==1) THEN
       !!****e* RBDY2/READ BEHAVIOURS
       !! NAME
       !!   READ BEHAVIOURS
       !! PURPOSE
       !!  read BULK_BEHAV.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY2/read_behaviours_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL read_behaviours_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ MP BEHAVIOURS')==1) THEN
       !!****e* RBDY2/READ MP BEHAVIOURS
       !! NAME
       !!   READ MP BEHAVIOURS
       !! PURPOSE
       !!  read extra physical behaviour in BULK_BEHAV.DAT file.
       !!  Must be used for THERMO_RIGID ELECTRO_RIGID and 
       !!  THERMO_ELECTRO_RIGID behaviour
       !! USES
       !!  LMGC90.CORE/RBDY2/read_mp_behaviours_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) disper
       CALL read_mp_behaviours_RBDY2(disper)
       RETURN      
    END IF

!!!---------------------------------------------------------------- 

    IF (INDEX(CHZZZ,'COMPUTE MASS')==1) THEN
       !!****e* RBDY2/COMPUTE MASS
       !! NAME
       !!   COMPUTE MASS
       !! PURPOSE
       !!  compute mass and inertia of bodies
       !! USES
       !!  LMGC90.CORE/RBDY2/comp_mass_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL comp_mass_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'PERIODIC CONDITION')==1) THEN
       !!****e* RBDY2/PERIODIC CONDITION
       !! NAME
       !!  PERIODIC CONDITION
       !! SYNOPSIS
       !!  PERIODIC CONDITION
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/RBDY2/set_periodic_data_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) periode
       CALL set_periodic_data_RBDY2(periode)
       !IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SIZE BODIES')==1) THEN
       !!****e* RBDY2/SIZE BODIES
       !! NAME
       !!  SIZE BODIES
       !! SYNOPSIS
       !!  SIZE BODIES
       !!  [homo]
       !! INPUTS
       !!  [homo] : homothety factor
       !! PURPOSE
       !!  resize body radius of a  [homo] factor
       !! USES
       !!  LMGC90.CORE/RBDY2/resize_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) homo
       WRITE(*,'(A10,D12.5)')'  @ homo =',homo
       CALL resize_RBDY2(homo)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INI X DOF NULL')==1) THEN
       !!****e* RBDY2/INI X DOF NULL
       !! NAME
       !!  INI X DOF NULL
       !! PURPOSE
       !!  nullify X degree of freedom
       !! USES
       !!  LMGC90.CORE/RBDY2/nullify_X_dof_RBDY2
       !!****

       CALL LOGCHIC('RBDY2')
       CALL nullify_X_dof_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INI V DOF NULL')==1) THEN
       !!****e* RBDY2/INI V DOF NULL
       !! NAME
       !!  INI V DOF NULL
       !! PURPOSE
       !!  nullify V degree of freedom 
       !! USES
       !!  LMGC90.CORE/RBDY2/nullify_V_dof_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL nullify_V_dof_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT SOURCE POINT')==1) THEN
       !!****e* RBDY2/INIT SOURCE POINT
       !! NAME
       !!  INIT SOURCE POINT
       !! SYNOPSIS
       !!  INIT SOURCE POINT
       !!  [first_RBDY2]
       !!  [radius]
       !!  [Xshift] [Yshift]
       !! INPUTS
       !!  [first_RBDY2] : number of first invisble body
       !!  [radius]      : source point area radius
       !!  [Xshift]      : X coordinate
       !!  [Yshift]      : Y coordinate
       !! PURPOSE
       !!  create an assembly by source point deposit
       !! USES
       !!  LMGC90.CORE/RBDY2/init_source_point_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) first_RBDY2
       READ(CHLZZZ(NZZZ+2),*) radius
       READ(CHLZZZ(NZZZ+3),*) Xshift,Yshift
       CALL init_source_point_RBDY2(first_RBDY2,radius,Xshift,Yshift)
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (      (INDEX(CHZZZ,'INIT INF BOUNDARY')==1)  &
         .OR. (INDEX(CHZZZ,'INIT SUP BOUNDARY')==1)  &
         .OR. (INDEX(CHZZZ,'INIT LEFT BOUNDARY')==1) &
         .OR. (INDEX(CHZZZ,'INIT RIGHT BOUNDARY')==1)) THEN
       CALL LOGCHIC('RBDY2')
       CALL LOGCHIC('OBSOLETE KEYWORD: SHOULD BE REMOVED')
       CALL LOGCHIC('USE XMIN, XMAX, YMIN OR YMAX INSTEAD OF')
       CALL LOGCHIC('INF, SUP, LEFT OR RIGHT')
       IETZZZ = 1
       STOP
    END IF
    
    IF (INDEX(CHZZZ,'INIT YMIN BOUNDARY')==1) THEN
       !!****e* RBDY2/INIT INF BOUNDARY
       !! NAME
       !!  INIT INF BOUNDARY
       !! SYNOPSIS
       !!  INIT INF BOUNDARY
       !!  [limit_inf]
       !! INPUTS
       !!  limit_inf : inferior boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY2/set_init_boundary_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY2(1,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT YMAX BOUNDARY')==1) THEN
       !!****e* RBDY2/INIT SUP BOUNDARY
       !! NAME
       !!  INIT SUP BOUNDARY
       !! SYNOPSIS
       !!  INIT SUP BOUNDARY
       !!  [limit]
       !! INPUTS
       !!  limit : inferior boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY2/set_init_boundary_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY2(2,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT XMIN BOUNDARY')==1) THEN
       !!****e* RBDY2/INIT LEFT BOUNDARY
       !! NAME
       !!  INIT LEFT BOUNDARY
       !! SYNOPSIS
       !!  INIT LEFT BOUNDARY
       !!  [limit]
       !! INPUTS
       !!  limit : left boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY2/set_init_boundary_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY2(3,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT XMAX BOUNDARY')==1) THEN
       !!****e* RBDY2/INIT RIGHT BOUNDARY
       !! NAME
       !!  INIT RIGHT BOUNDARY
       !! SYNOPSIS
       !!  INIT RIGHT BOUNDARY
       !!  [limit]
       !! INPUTS
       !!  limit : right boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY2/set_init_boundary_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY2(4,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'EQUILIBRIUM NORM')==1) THEN
       !!****e* RBDY2/EQUILIBRIUM NORM
       !! NAME
       !!   EQUILIBRIUM NORM 
       !! SYNOPSIS
       !!  EQUILIBRIUM NORM
       !!  [checktype] [tol]
       !! INPUTS
       !!  [checktype] : norm check type
       !!  [tol]       : norm tolerance
       !! PURPOSE
       !!  Initialisation of data for the  equilibrium state
       !!  check.
       !!  You must precise the type of check test [checktype]
       !!  - Qvlcy : quadratic norm of velocy
       !!  - Maxm  : maximum   norm of velocy
       !! USES
       !!  LMGC90.CORE/RBDY2/set_data_equilibrium_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),'(A5)') checktype
!        IF ( checktype .NE. 'Qvlcy' .AND. checktype .NE. 'Mvlcy') THEN
!           WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ)
!           CALL LOGMESCHIC(' @ WARNING, NEW from 03.12.16.')
!           CALL LOGMESCHIC(' @ You must precise the type of check test,')
!           CALL LOGMESCHIC(' @  - Qvlcy : quadratic norm of velocy,')
!           CALL LOGMESCHIC(' @  - Maxm  : maximum   norm of velocy.')
!           STOP
!        END IF
       READ(CHLZZZ(NZZZ+1)(7:30),*) tol
       CALL set_data_equilibrium_RBDY2(checktype,tol)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'MAKE BODIES+DOF')==1) THEN
       !!****e* RBDY2/MAKE BODIES+DOF
       !! NAME
       !!  MAKE BODIES+DOF
       !! PURPOSE
       !!  create a new BODIES.OUT file as a combination
       !!  of the last one and of the last DOF.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY2/add_dof2bodies_RBDY2
       !!****
       CALL LOGCHIC('RBDY2')
       CALL add_dof2bodies_RBDY2
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'GENERATE R2M')==1) THEN
       !!****e* CHIC.RBDY2/GENERATE R2M
       !! NAME
       !!  GENERATE R2M
       !! PURPOSE
       !!  ...
       !! TODO
       !!  check this routine
       !!****
       CALL LOGCHIC('RBDY2')
       CALL LOGMESCHIC('r2m activation')
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT CONSTRUCT WALL')==1) THEN
       !!****e* RBDY2/INIT CONSTRUCT WALL
       !! NAME
       !!  INIT CONSTRUCT WALL
       !! SYNOPSIS
       !!  INIT CONSTRUCT WALL
       !!  [X0] [XF] [NX]
       !!  [Y0] [YF] [NY]
       !! INPUTS
       !!  [X0] : First X coordinate
       !!  [XF] : Last  X coordinate
       !!  [NX] : X-axis step
       !!  [Y0] : First Y coordinate
       !!  [YF] : Last  Y coordinate
       !!  [NY] : Y-axis step
       !! PURPOSE
       !!  Initialisation of data for the simulation of wall 
       !!  construction
       !! USES
       !!  LMGC90.CORE/RBDY2/init_construct_wall
       !!  LMGC90.CORE/RBDY2/construct_wall
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) x0,xf,nx
       READ(CHLZZZ(NZZZ+2),*) y0,yf,ny
       IF((nx.EQ.0.D0).OR.(ny.EQ.0.D0))THEN
          CALL LOGMESCHIC(' @ WARNING : WRONG INCREMENT')
          STOP
       END IF
       xi = x0
       yi = y0
       CALL init_construct_wall(xi,yi,x0,y0,xf,yf,nx,ny)
       CALL construct_wall(1)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT CONSTRAINT WINDOW')==1) THEN
       !!****e* RBDY2/CONSTRAINED WINDOW
       !! NAME
       !!  CONSTRAINED WINDOW
       !! SYNOPSIS
       !!  CONSTRAINED WINDOW
       !!  [Xmin]
       !!  [Xmax]
       !!  [CV]
       !! INPUTS
       !!  Xmin : right boundary value
       !!  Xmax :  left boundary value
       !!  CV   : constraint value
       !! PURPOSE
       !!  contrain particle located in the window
       !! USES
       !!  LMGC90.CORE/RBDY2/
       !!****
       CALL LOGCHIC('RBDY2')
       READ(CHLZZZ(NZZZ+1),*) Xmin
       READ(CHLZZZ(NZZZ+2),*) Xmax
       READ(CHLZZZ(NZZZ+3),*) cv
       READ(CHLZZZ(NZZZ+4),*) behav
       WINDOWS = .TRUE.
       CALL set_init_cnstrt_window(Xmin,Xmax,CV,behav)
       IETZZZ = 1
       RETURN      
    END IF

!     IF (INDEX(CHZZZ,'COMPUTE WINDOW FORCES')==1) THEN
!        !!****e* RBDY2/CONSTRAINED WINDOW
!        !! NAME
!        !!  
!        !! SYNOPSIS
!        !!  
!        !! INPUTS
!        !!  
!        !! PURPOSE
!        !!  
!        !! USES
!        !!  LMGC90.CORE/RBDY2/
!        !!****
!        CALL LOGCHIC('RBDY2')
!        CALL compute_window_forces
!        IETZZZ = 1
!        RETURN     
!     END IF

!     IF (INDEX(CHZZZ,'COMPUTE WINDOW VELOCITY')==1) THEN
!        !!****e* RBDY2/CONSTRAINED WINDOW
!        !! NAME
!        !!  
!        !! SYNOPSIS
!        !!  
!        !! INPUTS
!        !!  
!        !! PURPOSE
!        !!  
!        !! USES
!        !!  LMGC90.CORE/RBDY2/
!        !!****
!        CALL LOGCHIC('RBDY2')
!        CALL compute_window_velocity
!        IETZZZ = 1
!        RETURN     
!     END IF

!     IF (INDEX(CHZZZ,'INCREMENT WINDOW VELOCITY')==1) THEN
!        !!****e* RBDY2/CONSTRAINED WINDOW
!        !! NAME
!        !!  
!        !! SYNOPSIS
!        !!  
!        !! INPUTS
!        !!  
!        !! PURPOSE
!        !!  
!        !! USES
!        !!  LMGC90.CORE/RBDY2/
!        !!****
!        CALL LOGCHIC('RBDY2')
!        CALL increment_window_velocity
!        IETZZZ = 1
!        RETURN     
!     END IF


  IF (INDEX(CHZZZ,'BIAXIAL LOADING')==1) THEN

    !!****e* CHIC_cmd/BIAXIAL_LOADING
    !! NAME
    !!   BIAXE LOADING
    !! SYNOPSIS
    !!  BIAXE LOADING
    !!  num_down num_right num_up num_left   
    !!  nb_loaded_body
    !!    loaded_body applied_pressure   <- loaded body == 1|2|3|4
    !!    ...
    !! FUNCTION
    !!  guess !
    !!****

    CALL LOGCHIC('RBDY2')
    READ(CHLZZZ(NZZZ+1),*) num_down,num_right,num_up,num_left
    READ(CHLZZZ(NZZZ+2),*) nb_loads
    if (allocated(loads)) deallocate(loads)
    allocate(loads(4))
    loads=0.d0
    DO inb_loads=1,nb_loads
      READ(CHLZZZ(NZZZ+2+inb_loads),*) ii,loads(ii)
    ENDDO
  
    call biaxial_loading(num_down,loads(1),num_right,loads(2),num_up,loads(3),num_left,loads(4))
        
    IETZZZ = 1
    RETURN      
  END IF

  IF (INDEX(CHZZZ,'OEDOMETRIC TEST')==1) THEN

    !!****e* CHIC_cmd/OEDOMETRIC TEST
    !! NAME
    !!   OEDOMETRIC TEST
    !! SYNOPSIS
    !!  OEDOMETRIC TEST
    !!  num_up,num_right,num_left
    !!  Finitial,Ffinal
    !!  DeltaT
    !!   ...
    !! FUNCTION
    !!  guess !
    !!****

    CALL LOGCHIC('RBDY2')
    READ(CHLZZZ(NZZZ+1),*) num_up,num_right,num_left
    READ(CHLZZZ(NZZZ+2),*) Finitial,Ffinal
    READ(CHLZZZ(NZZZ+3),*) DeltaT
  
    call Oedemetric_test(num_up,num_right,num_left,Finitial,Ffinal,DeltaT)
        
    IETZZZ = 1
    RETURN      
  END IF


  IF (INDEX(CHZZZ,'BIAXIAL DEF WALLS')==1) THEN

    !!****e* CHIC_cmd/BIAXIAL DEF WALLS
    !! NAME
    !!   BIAXIAL DEF WALLS
    !! SYNOPSIS
    !!  BIAXIAL DEF WALLS
    !!  num_up,num_down
    !!  membrane_thickness
    !!  pressure
    !!  membranecolor_right,membranecolor_left
    !! FUNCTION
    !!  guess !
    !!****

    CALL LOGCHIC('RBDY2')
    READ(CHLZZZ(NZZZ+1),*) num_up,num_down
    READ(CHLZZZ(NZZZ+2),*) thickness
    READ(CHLZZZ(NZZZ+3),*) sigma
    READ(CHLZZZ(NZZZ+4),*) membrana_color_Right,membrana_color_Left
  
    call Biaxial_def_walls(num_up,num_down,thickness,sigma, &
                           membrana_color_Right,membrana_color_Left)
        
    IETZZZ = 1
    RETURN      
  END IF

!!!mr experimental
  IF (INDEX(CHZZZ,'SHEAR DEF WALLS')==1) THEN
    !!****e* CHIC_cmd/SHEAR DEF WALLS
    !! NAME
    !!   SHEAR DEF WALLS
    !! SYNOPSIS
    !!  thickness
    !!  sigma
    !!  MCOLOR
    !! FUNCTION
    !!  guess !
    !!****

    CALL LOGCHIC('RBDY2')
    READ(CHLZZZ(NZZZ+1),*) thickness
    READ(CHLZZZ(NZZZ+1),*) lenght
    READ(CHLZZZ(NZZZ+2),*) sigma
    READ(CHLZZZ(NZZZ+3),*) MCOLOR
  
    call shear_def_walls(thickness,lenght,sigma,MCOLOR)
        
    IETZZZ = 1
    RETURN      
  END IF

  IF (INDEX(CHZZZ,'INIT FREE BOUNDARY')==1) THEN
     !!****e* CHIC_cmd/INIT FREE BOUNDARY
     !! NAME
     !!   INIT FREE BOUNDARY
     !! SYNOPSIS
     !!  XMIN
     !!  XMAX
     !!  DX
     !! FUNCTION
     !!  guess !
     !!****

    CALL LOGCHIC('RBDY2')
    READ(CHLZZZ(NZZZ+1),*) XMIN
    READ(CHLZZZ(NZZZ+2),*) XMAX
    READ(CHLZZZ(NZZZ+3),*) RADIUS
  
    CALL init_free_boundary_RBDY2(XMIN,XMAX,RADIUS)
        
    !IETZZZ = 1
    RETURN      
  END IF

  IF (INDEX(CHZZZ,'UPDATE THERMAL STRAIN')==1) THEN
   !!****e* RBDY2/UPDATE_THERMAL_STRAIN
   !! NAME
   !!  UPDATE THERMAL STRAIN
   !!****
    CALL LOGCHIC('RBDY2')
    CALL update_dilatation
    IETZZZ = 1
    RETURN      
  END IF

  END SUBROUTINE chic_command_RBDY2
!!!------------------------------------------------------------

  include '../../more_src/rigid_2D/wrap_RBDY2.inc'

END MODULE wrap_RBDY2
