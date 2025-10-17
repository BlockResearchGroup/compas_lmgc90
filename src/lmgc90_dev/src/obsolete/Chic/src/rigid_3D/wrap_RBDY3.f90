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
MODULE wrap_RBDY3

  !!****h* LMGC90.CHIC/RBDY3
  !! NAME
  !!  module wrap_RBDY3
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/RBDY3
  !!****

  USE CHIC

  USE RBDY3,ONLY:&
!!! CALLED BY CHIC_COMMAND
       get_nb_RBDY3, &
       increment_RBDY3, &
       check_source_point_RBDY3, &
       out_of_bounds_RBDY3, &
       fatal_damping_RBDY3, &
       comp_Fext_RBDY3, &
       comp_Fint_RBDY3, &
       comp_free_vlocy_RBDY3, &
       comp_dof_RBDY3, &
       update_dof_RBDY3, &
       write_xxx_dof_RBDY3, &
       binary_write_last_dof_RBDY3, &
       binary_write_out_dof_RBDY3, &
       write_xxx_Rnod_RBDY3, &
       write_out_bodies_RBDY3, &
       write_out_driven_dof_RBDY3, &
       read_in_bodies_RBDY3, &
       update_existing_entities_RBDY3, &
       read_in_dof_RBDY3, &
       read_in_driven_dof_RBDY3, &
       binary_read_in_dof_RBDY3, &
       read_behaviours_RBDY3, &
       comp_mass_RBDY3, &
       set_new_rotation_scheme_RBDY3, &
       init_source_point_RBDY3, &
       init4fd_source_point_RBDY3, &
       set_init_boundary_RBDY3, &
       set_xperiodic_data_RBDY3, &
       set_yperiodic_data_RBDY3, &
       get_write_DOF_RBDY3, &
       get_write_Rnod_RBDY3, &
       read_mp_behaviours_RBDY3, &
!!! CALLED BY MORE_CHIC_COMMAND
       read_in_comp_bodies_RBDY3, &
       update_WS_rbdy3, &
       without_rotation_of_RBDY3, &
       init_progressive_activation_RBDY3, &
       do_progressive_activation_RBDY3, &
       set_skip_invisible_RBDY3, &
       set_keep_ini_dof_order_RBDY3, &
       membrane_RBDY3, &
       init_free_boundary_RBDY3,&
       triaxial_loading

CONTAINS

!!!-------------------------------------------------------------------------

  SUBROUTINE chic_command_RBDY3

    IMPLICIT NONE
    
    INTEGER :: nb_RBDY3,first_RBDY3
    INTEGER :: ifrom,ito
    INTEGER :: ii,l_ii,iv,IGOZZZ
    
    LOGICAL,SAVE :: XPERIODIC = .FALSE.,YPERIODIC = .FALSE.
    LOGICAL      :: write_Rnod,write_DOF
    REAL(kind=8) :: radius,Xshift,Yshift,Zshift,limit
    REAL(kind=8) :: xperiode,yperiode,disper

    REAL(KIND=8) :: zini,dz,XMIN,XMAX,YMIN,YMAX
    integer      :: pa_step

    logical,save                                    :: is_first_time=.true., is_up_actif=.true.
    integer,save                                    :: nb_grain,up_grain
    real(kind=8),save                               :: ep, sigma_min, sigma_max
    real(kind=8)                                    :: DT_ini,DT_load,DT_relax
    real(kind=8),dimension(2)                       :: centre
    integer                                         :: nfich,i_up_actif

    ! gestion tri-tri
    integer,save        :: num_down,num_right,num_up,num_left,num_av,num_der
    integer,save        :: nb_loads=0
    !les numeros des corps (1:down/2:right/3:up/4:left/5:avant/6:derriere) formant la boite                        
    real(kind=8),dimension(:,:),allocatable,save   :: loads  
    integer             :: i_load=0




    nb_RBDY3 = get_nb_RBDY3()

    IF (nb_RBDY3.EQ.0) RETURN
    
    IF (INDEX(CHZZZ,'INCREMENT STEP')==1) THEN
       !!****e* RBDY3/INCREMENT STEP
       !! NAME
       !!   INCREMENT STEP
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/RBDY3/increment_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL increment_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CHECK SOURCE POINT')==1) THEN
       !!****e* RBDY3/CHECK SOURCE POINT
       !! NAME
       !!  CHECK SOURCE POINT
       !! PURPOSE
       !!  check if one more particle must be add on the system
       !! USES
       !! LMGC90.CORE/RBDY3/check_source_point_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL check_source_point_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CHECK OUT OF BOUNDS')==1) THEN
       !!****e* RBDY3/CHECK OUT OF BOUNDS
       !! NAME
       !!  CHECH OUT OF BOUNDS
       !! PURPOSE
       !!  check if body go out the defined bounds
       !! USES
       !!  LMGC90.CORE/RBDY3/out_of_bounds_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       PRINT *,'OBSOLETE KEYWORD: SHOULD BE REMOVED' 
       !CALL out_of_bounds_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'FATAL DAMPING')==1) THEN
       !!****e* RBDY3/FATAL DAMPING
       !! NAME
       !!  FATAL DAMPING
       !! PURPOSE
       !!  Nullify body velocities
       !! USES
       !!  LMGC90.CORE/RBDY3/fatal_damping_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       iv = 1
       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
          ii = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) READ(CHZZZ(ii+4:30),*) iv
       END IF
       CALL fatal_damping_RBDY3(iv)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(chzzz,'COMPUTE Fext')==1) THEN
       !!****e* RBDY3/COMPUTE Fext
       !! NAME
       !!  COMPUTE Fext
       !! PURPOSE
       !!  compute external forces
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_Fext_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL comp_Fext_RBDY3
       IETZZZ = 1
       RETURN      
    END IF
    
    IF ( INDEX(CHZZZ,'COMPUTE Fint')==1 .OR. &
         INDEX(CHZZZ,'COMPUTE TTFint')==1 ) then

       CALL LOGCHIC('RBDY3')
       print*,'OBSOLETE KEYWORD USE COMPUTE BULK INSTEED'
       STOP

    ENDIF

    IF ( INDEX(CHZZZ,'COMPUTE NL BULK')==1 .OR. &
         INDEX(CHZZZ,'COMPUTE BULK')==1   ) THEN

       !!****e* RBDY3/COMPUTE Fint
       !! NAME
       !!  COMPUTE Fint
       !! PURPOSE
       !!  compute internal forces
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_Fint_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL comp_Fint_RBDY3
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'COMPUTE FREE VELOCITY')==1) THEN
       !!****e* RBDY3/COMPUTE FREE VELOCITY
       !! NAME
       !!  COMPUTE FREE VELOCITY
       !! PURPOSE
       !!  compute free velocity with external forces only
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_free_vlocy_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL comp_free_vlocy_RBDY3
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'COMPUTE DOF')==1) THEN
       !!****e* RBDY3/COMPUTE DOF
       !! NAME
       !!  COMPUTE DOF
       !! PURPOSE
       !!  correction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_dof_RBDY3
       !!  LMGC90.CORE/RBDY3/ckeck_periodic_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL comp_dof_RBDY3
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'UPDATE DOF')==1) THEN
       !!****e* RBDY3/UPDATE DOF
       !! NAME
       !!  UPDATE DOF
       !! PURPOSE
       !!  save d.o.f. of the end of the time step to d.o.f. of the begining 
       !!  of the next one
       !! USES
       !!  LMGC90.CORE/RBDY3/update_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL update_dof_RBDY3
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'UPDATE GAMMAvsT')==1) THEN
       !!****e* RBDY3/UPDATE GAMMAvsT
       !! NAME
       !!  UPDATE GAMMAvsT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY3/update_gamma_rbdy3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL update_WS_rbdy3
       IETZZZ = 1
       RETURN      
    END IF

    
!!! WRITING COMMAND ------------------------------------------------
    
    IF (INDEX(CHZZZ,'WRITE LAST DOF')==1) THEN     
       !!****e* RBDY3/WRITE LAST DOF
       !! NAME
       !!   WRITE LAST DOF
       !! PURPOSE
       !!  write ascii DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_RBDY3 = get_nb_RBDY3()
          ifrom = 1
          ito   = nb_RBDY3
          CALL write_xxx_dof_RBDY3(2,ifrom,ito)          
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'BINARY WRITE LAST DOF')==1) THEN     
       !!****e* RBDY3/BINARY WRITE LAST DOF
       !! NAME
       !!   BINARY WRITE LAST DOF
       !! PURPOSE
       !!  write binary DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY3/binary_write_last_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL binary_write_last_dof_RBDY3
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'BINARY WRITE OUT DOF')==1) THEN     
       !!****e* RBDY3/BINARY WRITE OUT DOF
       !! NAME
       !!  BINARY WRITE OUT DOF
       !! PURPOSE
       !!  write binary DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY3/binary_write_out_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) CALL binary_write_out_dof_RBDY3
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT DOF')==1) THEN     
       !!****e* RBDY3/WRITE OUT DOF
       !! NAME
       !!   WRITE OUT DOF
       !! PURPOSE
       !!   write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) THEN
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
             nb_RBDY3 = get_nb_RBDY3()
             ifrom = 1
             ito   = nb_RBDY3
             CALL write_xxx_dof_RBDY3(1,ifrom,ito)
          ELSE
             IGOZZZ = 0
             DO
                IGOZZZ = IGOZZZ + 1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY3') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                      ifrom = max0(ifrom,1)
                      ito   = min0(ito,nb_RBDY3)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_dof_RBDY3(1,ifrom,ito)
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
       !!****e* RBDY3/DISPLAY OUT DOF
       !! NAME
       !!   DISPLAY OUT DOF
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_RBDY3 = get_nb_RBDY3()
          ifrom=1  ; ito=nb_RBDY3
          CALL write_xxx_dof_RBDY3(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY3') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   ifrom=max0(ifrom,1) ; ito=min0(ito,nb_RBDY3)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_dof_RBDY3(6,ifrom,ito)
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
       !!****e* RBDY3/WRITE LAST Rnod
       !! NAME
       !!   WRITE LAST Rnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_Rnod_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_RBDY3 = get_nb_RBDY3()
          ifrom = 1  
          ito   = nb_RBDY3
          CALL write_xxx_Rnod_RBDY3(2,ifrom,ito)
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT Rnod')==1) THEN
       !!****e* RBDY3/WRITE OUT Rnod
       !! NAME
       !!   WRITE OUT Rnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_Rnod_RBDY3
       !!****
       write_Rnod = get_write_Rnod_RBDY3()
       IF (write_Rnod) THEN
          CALL LOGCHIC('RBDY3')
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
             nb_RBDY3 = get_nb_RBDY3()
             ifrom = 1
             ito   = nb_RBDY3
             CALL write_xxx_Rnod_RBDY3(1,ifrom,ito)
          ELSE
             IGOZZZ=0
             DO
                IGOZZZ=IGOZZZ+1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY3') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                      ifrom = max0(ifrom,1)
                      ito   = min0(ito,nb_RBDY3)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_Rnod_RBDY3(1,ifrom,ito)
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
       !!****e* RBDY3/DISPLAY OUT Rnod
       !! NAME
       !!   DISPLAY OUT Rnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_Rnod_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_RBDY3 = get_nb_RBDY3()
          ifrom = 1  
          ito   = nb_RBDY3
          CALL write_xxx_Rnod_RBDY3(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'RBDY3') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   ifrom = max0(ifrom,1) 
                   ito   = min0(ito,nb_RBDY3)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_Rnod_RBDY3(6,ifrom,ito)
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
       !!****e* RBDY3/WRITE BODIES
       !! NAME
       !!   WRITE BODIES
       !! PURPOSE
       !!  write BODIES.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_out_bodies_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')

       CALL write_out_bodies_RBDY3(1)

       IETZZZ = 1
       RETURN      
    END IF
    IF (INDEX(CHZZZ,'WRITE DRIVEN DOF')==1) THEN
       !!****e* RBDY3/WRITE DRIVEN DOF
       !! NAME
       !!   WRITE DRIVEN DOF
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_out_driven_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')

       CALL write_out_driven_dof_RBDY3

       IETZZZ = 1
       RETURN      
    END IF

!!! READING COMMAND ----------------------------------------------------

    IF (INDEX(CHZZZ,'READ BODIES')==1) THEN
       !!****e* RBDY3/READ BODIES
       !! NAME
       !!   READ BODIES
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable in RBDY3
       !!  Adds the number of found bodies to entity
       !! USES
       !!  LMGC90.CORE/RBDY3/read_in_bodies_RBDY3
       !!  LMGC90.CORE/RBDY3/update_existing_entities_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')

       CALL read_in_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ INI DOF')==1) THEN
       !!****e* RBDY3/READ INI DOF
       !! NAME
       !!   READ INI DOF
       !! PURPOSE
       !!  read DOF.INI file
       !! USES
       !!  LMGC90.CORE/RBDY3/read_in_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')

       CALL read_in_dof_RBDY3

       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ DRIVEN DOF')==1) THEN
       !!****e* RBDY3/READ DRIVEN DOF
       !! NAME
       !!   READ DRIVEN DOF
       !! PURPOSE
       !!  read DRV_DOF.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY3/read_in_driven_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')

       CALL read_in_driven_dof_RBDY3

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'BINARY READ INI DOF')==1) THEN
       !!****e* RBDY3/BINARY READ INI DOF
       !! NAME
       !!   BINARY READ INI DOF
       !! PURPOSE
       !!   read binary file DOF.INI
       !! USES
       !!  LMGC90.CORE/RBDY3/binary_read_in_dof_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL binary_read_in_dof_RBDY3
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'READ BEHAVIOURS')==1) THEN
       !!****e* RBDY3/READ BEHAVIOURS
       !! NAME
       !!   READ BEHAVIOURS
       !! PURPOSE
       !!  read BULK_BEHAV.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY3/read_behaviours_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL read_behaviours_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ MP BEHAVIOURS')==1) THEN
       !!****e* RBDY3/READ MP BEHAVIOURS
       !! NAME
       !!   READ MP BEHAVIOURS
       !! PURPOSE
       !!  read extra physical behaviour in BULK_BEHAV.DAT file.
       !!  Must be used for THERMO_RIGID ELECTRO_RIGID and 
       !!  THERMO_ELECTRO_RIGID behaviour
       !! USES
       !!  LMGC90.CORE/RBDY3/read_mp_behaviours_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) disper
       CALL read_mp_behaviours_RBDY3(disper)
       RETURN      
    END IF

!!!---------------------------------------------------------------- 

    IF (INDEX(CHZZZ,'COMPUTE MASS')==1) THEN
       !!****e* RBDY3/COMPUTE MASS
       !! NAME
       !!   COMPUTE MASS
       !! PURPOSE
       !!  compute mass and inertia of bodies
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_mass_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL comp_mass_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'NEW ROTATION SCHEME')==1) THEN
       !!****e* RBDY3/NEW ROTATION SCHEME
       !! NAME
       !!  NEW ROTATION SCHEME
       !! PURPOSE
       !!  active new rotation scheme FLAG
       !! USES
       !!  LMGC90.CORE/RBDY3/set_new_rotation_scheme_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL set_new_rotation_scheme_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT SOURCE POINT')==1) THEN
       !!****e* RBDY3/INIT SOURCE POINT
       !! NAME
       !!  INIT SOURCE POINT
       !! SYNOPSIS
       !!  INIT SOURCE POINT
       !!  [first_RBDY3]
       !!  [radius]
       !!  [Xshift] [Yshift] [Zshift]
       !! INPUTS
       !!  [first_RBDY3] : number of first invisble body
       !!  [radius]      : source point area radius
       !!  [Xshift]      : X coordinate
       !!  [Yshift]      : Y coordinate
       !!  [Zshift]      : Z coordinate
       !! PURPOSE
       !!  create an assembly by source point deposit
       !! USES
       !!  LMGC90.CORE/RBDY3/init_source_point_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) first_RBDY3
       READ(CHLZZZ(NZZZ+2),*) radius
       READ(CHLZZZ(NZZZ+3),*) Xshift,Yshift,Zshift
       CALL init_source_point_RBDY3(first_RBDY3,radius,Xshift,Yshift,Zshift)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'FD INIT SOURCE POINT')==1) THEN
       !!****e* RBDY3/FD INIT SOURCE POINT
       !! NAME
       !!  FD INIT SOURCE POINT
       !! SYNOPSIS
       !!  FD INIT SOURCE POINT
       !!  [first_RBDY3]
       !!  [radius]
       !!  [Xshift] [Yshift] [Zshift]
       !! INPUTS
       !!  [first_RBDY3] : number of first invisble body
       !!  [radius]      : source point area radius
       !!  [Xshift]      : X coordinate
       !!  [Yshift]      : Y coordinate
       !!  [Zshift]      : Z coordinate
       !! PURPOSE
       !!  create an assembly by source point deposit
       !! USES
       !!  LMGC90.CORE/RBDY3/init4fd_source_point_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) first_RBDY3
       READ(CHLZZZ(NZZZ+2),*) radius
       READ(CHLZZZ(NZZZ+3),*) Xshift,Yshift,Zshift
       CALL init4fd_source_point_RBDY3(first_RBDY3,radius,Xshift,Yshift,Zshift)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT XMIN BOUNDARY')==1) THEN
       !!****e* RBDY3/INIT XMIN BOUNDARY
       !! NAME
       !!  INIT XMIN BOUNDARY
       !! SYNOPSIS
       !!  INIT XMIN BOUNDARY
       !!  [limit_inf]
       !! INPUTS
       !!  limit_inf : inferior boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY3(1,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT XMAX BOUNDARY')==1) THEN
       !!****e* RBDY3/INIT XMAX BOUNDARY
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
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY3(2,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT YMIN BOUNDARY')==1) THEN
       !!****e* RBDY3/INIT LEFT BOUNDARY
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
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY3(3,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT YMAX BOUNDARY')==1) THEN
       !!****e* RBDY3/INIT YMAX BOUNDARY
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
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY3(4,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT ZMIN BOUNDARY')==1) THEN
       !!****e* RBDY3/INIT ZMIN BOUNDARY
       !! NAME
       !!  INIT LEFT BOUNDARY
       !! SYNOPSIS
       !!  INIT LEFT BOUNDARY
       !!  [limit]
       !! INPUTS
       !!  limit : back boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY3(5,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT ZMAX BOUNDARY')==1) THEN
       !!****e* RBDY3/INIT ZMAX BOUNDARY
       !! NAME
       !!  INIT FRONT BOUNDARY
       !! SYNOPSIS
       !!  INIT FRONT BOUNDARY
       !!  [limit]
       !! INPUTS
       !!  limit : front boundary value 
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       !! USES
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) limit
       CALL set_init_boundary_RBDY3(6,limit)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'XPERIODIC CONDITION')==1) THEN
       !!****e* RBDY3/XPERIODIC CONDITION
       !! NAME
       !!  XPERIODIC CONDITION
       !! SYNOPSIS
       !!  XPERIODIC CONDITION
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/RBDY3/set_xperiodic_data_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) xperiode
       CALL set_xperiodic_data_RBDY3(xperiode)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'YPERIODIC CONDITION')==1) THEN
       !!****e* RBDY3/YPERIODIC CONDITION
       !! NAME
       !!  YPERIODIC CONDITION
       !! SYNOPSIS
       !!  YPERIODIC CONDITION
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The Y variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/RBDY3/set_yperiodic_data_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) yperiode
       CALL set_yperiodic_data_RBDY3(yperiode)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'FIX BODY ROTATION')==1) THEN
       !!****e* RBDY3/FIX BODY ROTATION
       !! NAME
       !!  FIX BODY ROTATION
       !! PURPOSE
       !!  kill rotation effect for RBDY3
       !! USES
       !!  LMGC90.CORE/RBDY3/without_rotation_of_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL without_rotation_of_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT PROGRESSIVE ACTIVATION')==1) THEN
       !!****e* RBDY3/INIT PROGRESSIVE ACTIVATION
       !! NAME
       !!  INIT PROGRESSIVE ACTIVATION
       !! SYNOPSIS
       !!  INIT PROGRESSIVE ACTIVATION
       !!  [zini] [dz]
       !! PURPOSE
       !!  [zini] initial altitude
       !!  [dz] increment of altitude
       !! USES
       !!  LMGC90.CORE/RBDY3/init_progressive_activation
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) zini,dz
       CALL init_progressive_activation_RBDY3(zini,dz)
       RETURN      
    END IF
  IF (INDEX(CHZZZ,'DO PROGRESSIVE ACTIVATION')==1) THEN
       !!****e* RBDY3/DO PROGRESSIVE ACTIVATION
       !! NAME
       !!  DO PROGRESSIVE ACTIVATION
       !! SYNOPSIS
       !!  DO PROGRESSIVE ACTIVATION
       !!  step
       !! PURPOSE
       !!  [step] occurence of ativation 
       !! USES
       !!  LMGC90.CORE/RBDY3/do_progressive_activation
       !!****
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) pa_step
       CALL do_progressive_activation_RBDY3(pa_step)
       RETURN      
    END IF
    IF (INDEX(CHZZZ,'SKIP INVISIBLE')==1) THEN

       CALL LOGCHIC('RBDY3')
       CALL set_skip_invisible_RBDY3
    END IF

    IF (INDEX(CHZZZ,'KEEP INI DOF ORDER')==1) THEN

       CALL LOGCHIC('RBDY3')
       CALL set_keep_ini_dof_order_RBDY3 
    END IF


    IF (INDEX(CHZZZ,'MEMBRANE')==1) THEN
      CALL LOGCHIC('RBDY3')

      nfich=999
      if (is_first_time) then

        open(unit=nfich,STATUS='REPLACE',file='radius.txt')

        READ(CHLZZZ(NZZZ+1),*) sigma_min,sigma_max
        READ(CHLZZZ(NZZZ+2),*) DT_ini,DT_load,DT_relax
        Read(CHLZZZ(NZZZ+3),*) centre,ep
        READ(CHLZZZ(NZZZ+4),*) nb_grain 

        READ(CHLZZZ(NZZZ+5),*) up_grain
        READ(CHLZZZ(NZZZ+6),*) i_up_actif

        if (i_up_actif == 0) is_up_actif = .false.
        is_first_time=.false.

      else
        nfich=999
        open(unit=nfich,STATUS='OLD',POSITION='APPEND',file='radius.txt')

      endif

      call membrane_RBDY3(sigma_min,sigma_max,DT_ini,DT_load,DT_relax,centre,ep, &
                          nb_grain,up_grain,is_first_time,is_up_actif,nfich)



      close(nfich)

      IETZZZ = 1
      RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE WSvsT')==1) THEN
       !!****e* RBDY3/UPDATE WSvsT
       !! NAME
       !!  UPDATE WSvsT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY3/update_WS_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL update_WS_RBDY3
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INCREMENT WSvsT')==1) THEN
       !!****e* RBDY3/INCREMENT WSvsT
       !! NAME
       !!  INCREMENT WSvsT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY3/update_WS_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       CALL update_WS_RBDY3
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
       !!  YMIN
       !!  YMAX
       !!  DX
       !! FUNCTION
       !!  guess !
       !!****
       
       CALL LOGCHIC('RBDY3')
       READ(CHLZZZ(NZZZ+1),*) XMIN
       READ(CHLZZZ(NZZZ+2),*) XMAX
       READ(CHLZZZ(NZZZ+3),*) YMIN
       READ(CHLZZZ(NZZZ+4),*) YMAX
       READ(CHLZZZ(NZZZ+5),*) RADIUS
       
       CALL init_free_boundary_RBDY3(XMIN,XMAX,YMIN,YMAX,RADIUS)
       
       !IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'TRIAXIAL LOADING')==1) THEN
      CALL LOGCHIC('RBDY3')

      if (is_first_time) then

        READ(CHLZZZ(NZZZ+1),*) num_down,num_right
        READ(CHLZZZ(NZZZ+2),*) num_up,num_left
        READ(CHLZZZ(NZZZ+3),*) num_av,num_der
        READ(CHLZZZ(NZZZ+4),*) nb_loads
        if (allocated(loads)) deallocate(loads)
        allocate(loads(2,nb_loads))
        DO i_load=1,nb_loads
          READ(CHLZZZ(NZZZ+4+i_load),*) loads(1,i_load),loads(2,i_load) 
        ENDDO
        is_first_time=.false.
      endif

      call triaxial_loading(num_down,num_right,num_up,num_left,num_av,num_der, &
                            nb_loads,loads)

      IETZZZ = 1
      RETURN      
    END IF


  END SUBROUTINE chic_command_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE more_chic_command_RBDY3
    
    IMPLICIT NONE
    
    IF (INDEX(CHZZZ,'COLLECT BODIES.OUT')==1) THEN
       !  some more CHIC command to be used in preprocessing
       CALL LOGCHIC('RBDY3')
       CALL read_in_bodies_RBDY3(2)
       IETZZZ = 1
       RETURN      
    END IF
 
    IF (INDEX(CHZZZ,'APPEND TO BODIES.OUT')==1) THEN
       !  some more CHIC command to be used in preprocessing
       CALL LOGCHIC('RBDY3')
       CALL write_out_bodies_RBDY3(1)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REBUILD BODIES.DAT')==1) THEN
       !  some more CHIC command to be used in preprocessing
       CALL LOGCHIC('RBDY3')
       CALL write_out_bodies_RBDY3(2)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ COMPRESSED BODIES')==1) THEN
       !!****e* RBDY3/READ COMPRESSED BODIES
       !! NAME
       !!   READ COMPRESSED BODIES
       !! PURPOSE
       !!  read BODIES.DAT file without any comment
       !!  Initializes existing_entities variable in RBDY3
       !!  Adds the number of found bodies to entity
       !! USES
       !!  LMGC90.CORE/RBDY3/read_in_compress_bodies_RBDY3
       !!  LMGC90.CORE/RBDY3/update_existing_entities_RBDY3
       !!****
       CALL LOGCHIC('RBDY3')
       
       CALL read_in_comp_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3
       
       IETZZZ = 1
       RETURN      
    END IF



  END SUBROUTINE more_chic_command_RBDY3
!!!------------------------------------------------------------------------
END MODULE wrap_RBDY3
