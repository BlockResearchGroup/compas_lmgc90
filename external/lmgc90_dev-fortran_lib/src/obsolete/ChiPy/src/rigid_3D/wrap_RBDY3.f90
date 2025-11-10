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

  !!****h* LMGC90.CHiPy/RBDY3
  !! NAME
  !!  module wrap_RBDY3
  !! USES
  !!  LMGC90.CORE/RBDY3
  !!****

  USE RBDY3,ONLY:&
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
       set_keep_ini_dof_order_RBDY3

CONTAINS

!!!-------------------------------------------------------------------------

    Subroutine IncrementStep
       !!****e* RBDY3/INCREMENT STEP
       !! NAME
       !!   INCREMENT STEP
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/RBDY3/increment_RBDY3
       !!****
       ! CALL LOGCHIC('RBDY3')

       implicit none
       CALL increment_RBDY3
     END Subroutine IncrementStep

    Subroutine CheckSourcePoint
       !!****e* RBDY3/CHECK SOURCE POINT
       !! NAME
       !!  CHECK SOURCE POINT
       !! PURPOSE
       !!  check if one more particle must be add on the system
       !! USES
       !! LMGC90.CORE/RBDY3/check_source_point_RBDY3
       !!****
       implicit none
       !CALL LOGCHIC('RBDY3')
       CALL check_source_point_RBDY3
    END Subroutine CheckSourcePoint

    Subroutine CheckOutOfBounds
       !!****e* RBDY3/CHECK OUT OF BOUNDS
       !! NAME
       !!  CHECH OUT OF BOUNDS
       !! PURPOSE
       !!  check if body go out the defined bounds
       !! USES
       !!  LMGC90.CORE/RBDY3/out_of_bounds_RBDY3
       !!****
       implicit none
       !CALL LOGCHIC('RBDY3')
       CALL out_of_bounds_RBDY3
    END Subroutine CheckOutOfBounds

    Subroutine FatalDamping(freq)
       !!****e* RBDY3/FATAL DAMPING
       !! NAME
       !!  FATAL DAMPING
       !! PURPOSE
       !!  Nullify body velocities
       !! USES
       !!  LMGC90.CORE/RBDY3/fatal_damping_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       integer :: freq

       CALL fatal_damping_RBDY3(freq)

    END Subroutine FatalDamping

    Subroutine ComputeFext
       !!****e* RBDY3/COMPUTE Fext
       !! NAME
       !!  COMPUTE Fext
       !! PURPOSE
       !!  compute external forces
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_Fext_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL comp_Fext_RBDY3
    END Subroutine ComputeFext


    Subroutine ComputeBulk    
       !'COMPUTE NL BULK' 'COMPUTE BULK

       !!****e* RBDY3/COMPUTE Fint
       !! NAME
       !!  COMPUTE Fint
       !! PURPOSE
       !!  compute internal forces
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_Fint_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL comp_Fint_RBDY3
    END Subroutine ComputeBulk    
    
    Subroutine ComputeFreeVelocity
       !!****e* RBDY3/COMPUTE FREE VELOCITY
       !! NAME
       !!  COMPUTE FREE VELOCITY
       !! PURPOSE
       !!  compute free velocity with external forces only
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_free_vlocy_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL comp_free_vlocy_RBDY3
    END Subroutine ComputeFreeVelocity
    
    Subroutine ComputeDof
       !!****e* RBDY3/COMPUTE DOF
       !! NAME
       !!  COMPUTE DOF
       !! PURPOSE
       !!  correction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_dof_RBDY3
       !!  LMGC90.CORE/RBDY3/ckeck_periodic_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL comp_dof_RBDY3
    End Subroutine    

    Subroutine UpdateDof
       !!****e* RBDY3/UPDATE DOF
       !! NAME
       !!  UPDATE DOF
       !! PURPOSE
       !!  save d.o.f. of the end of the time step to d.o.f. of the begining 
       !!  of the next one
       !! USES
       !!  LMGC90.CORE/RBDY3/update_dof_RBDY3
       !!****
       ! CALL LOGCHIC('RBDY3')
       implicit none
         CALL update_dof_RBDY3
    End Subroutine    

    
!!! WRITING COMMAND ------------------------------------------------
    
    Subroutine WriteLastDOF     
       !!****e* RBDY3/WRITE LAST DOF
       !! NAME
       !!   WRITE LAST DOF
       !! PURPOSE
       !!  write ascii DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_dof_RBDY3
       !!****
       ! CALL LOGCHIC('RBDY3')
       implicit none
       INTEGER :: ifrom,ito

       ifrom = 1
       ito = get_nb_RBDY3()

       CALL write_xxx_dof_RBDY3(2,ifrom,ito)          
    END Subroutine

    Subroutine BinaryWriteLastDOF     
       !!****e* RBDY3/BINARY WRITE LAST DOF
       !! NAME
       !!   BINARY WRITE LAST DOF
       !! PURPOSE
       !!  write binary DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY3/binary_write_last_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL binary_write_last_dof_RBDY3
    END Subroutine

    Subroutine BinaryWriteOutDOF     
       !!****e* RBDY3/BINARY WRITE OUT DOF
       !! NAME
       !!  BINARY WRITE OUT DOF
       !! PURPOSE
       !!  write binary DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY3/binary_write_out_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       logical :: write_DOF
       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) CALL binary_write_out_dof_RBDY3

    END Subroutine

    Subroutine WriteOutDOF(ifrom,ito)     
       !!****e* RBDY3/WRITE OUT DOF
       !! NAME
       !!   WRITE OUT DOF
       !! PURPOSE
       !!   write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY3
       logical :: write_DOF

       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) THEN
         nb_RBDY3 = get_nb_RBDY3()
         ivalue1 = max0(ifrom,1)
         ivalue2 = min0(ito,nb_RBDY3)
         CALL write_xxx_dof_RBDY3(1,ivalue1,ivalue2)
       END IF

    END Subroutine

    Subroutine DisplayOutDOF(ifrom,ito)
       !!****e* RBDY3/DISPLAY OUT DOF
       !! NAME
       !!   DISPLAY OUT DOF
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_dof_RBDY3
       !!****
       implicit none
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY3
       logical :: write_DOF

       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) THEN
         nb_RBDY3 = get_nb_RBDY3()
         ivalue1 = max0(ifrom,1)
         ivalue2 = min0(ito,nb_RBDY3)
         CALL write_xxx_dof_RBDY3(6,ivalue1,ivalue2)
       END IF

    END Subroutine

    Subroutine WriteLastRnod
       !!****e* RBDY3/WRITE LAST Rnod
       !! NAME
       !!   WRITE LAST Rnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_Rnod_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       IMPLICIT NONE
       INTEGER :: ifrom,ito,nb_RBDY3

       nb_RBDY3 = get_nb_RBDY3()
       ifrom = 1  
       ito   = nb_RBDY3
       CALL write_xxx_Rnod_RBDY3(2,ifrom,ito)
    END Subroutine

    Subroutine WriteOutRnod(ifrom,ito)
       !!****e* RBDY3/WRITE OUT Rnod
       !! NAME
       !!   WRITE OUT Rnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_Rnod_RBDY3
       !!****
       implicit none
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY3
       logical :: write_Rnod
       write_Rnod = get_write_Rnod_RBDY3()
       IF (write_Rnod) THEN
         nb_RBDY3 = get_nb_RBDY3()
         ivalue1 = max0(ifrom,1)
         ivalue2 = min0(ito,nb_RBDY3)
         CALL write_xxx_Rnod_RBDY3(1,ivalue1,ivalue2)
       END IF
    END Subroutine

    Subroutine DisplayOutRnod(ifrom,ito)
       !!****e* RBDY3/DISPLAY OUT Rnod
       !! NAME
       !!   DISPLAY OUT Rnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/RBDY3/write_xxx_Rnod_RBDY3
       !!****
       implicit none
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY3
       logical :: write_Rnod
       write_Rnod = get_write_Rnod_RBDY3()
       IF (write_Rnod) THEN
         nb_RBDY3 = get_nb_RBDY3()
         ivalue1 = max0(ifrom,1)
         ivalue2 = min0(ito,nb_RBDY3)
         CALL write_xxx_Rnod_RBDY3(6,ivalue1,ivalue2)
       END IF
       RETURN 
    END Subroutine

    Subroutine WriteBodies
       !!****e* RBDY3/WRITE BODIES
       !! NAME
       !!   WRITE BODIES
       !! PURPOSE
       !!  write BODIES.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_out_bodies_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none   
       CALL write_out_bodies_RBDY3(1)
    End Subroutine

    Subroutine WriteDrivenDOF
       !!****e* RBDY3/WRITE DRIVEN DOF
       !! NAME
       !!   WRITE DRIVEN DOF
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY3/write_out_driven_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL write_out_driven_dof_RBDY3
    End Subroutine

!!! READING COMMAND ----------------------------------------------------

    Subroutine ReadBodies
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
       !CALL LOGCHIC('RBDY3')

       implicit none 
       CALL read_in_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3

    End Subroutine

    Subroutine ReadIniDof
       !!****e* RBDY3/READ INI DOF
       !! NAME
       !!   READ INI DOF
       !! PURPOSE
       !!  read DOF.INI file
       !! USES
       !!  LMGC90.CORE/RBDY3/read_in_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')

       implicit none
       CALL read_in_dof_RBDY3

    END Subroutine

    Subroutine ReadDrivenDOF
       !!****e* RBDY3/READ DRIVEN DOF
       !! NAME
       !!   READ DRIVEN DOF
       !! PURPOSE
       !!  read DRV_DOF.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY3/read_in_driven_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL read_in_driven_dof_RBDY3
    End Subroutine

    Subroutine BinaryReadIniDOF
       !!****e* RBDY3/BINARY READ INI DOF
       !! NAME
       !!   BINARY READ INI DOF
       !! PURPOSE
       !!   read binary file DOF.INI
       !! USES
       !!  LMGC90.CORE/RBDY3/binary_read_in_dof_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL binary_read_in_dof_RBDY3
       RETURN 
    END Subroutine

    Subroutine ReadBehaviours
       !!****e* RBDY3/READ BEHAVIOURS
       !! NAME
       !!   READ BEHAVIOURS
       !! PURPOSE
       !!  read BULK_BEHAV.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY3/read_behaviours_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       CALL read_behaviours_RBDY3
    End Subroutine

    Subroutine ReadMPBehaviours(disper)
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
       !CALL LOGCHIC('RBDY3')
       implicit none
       real(kind=8) :: disper
       CALL read_mp_behaviours_RBDY3(disper)

    END Subroutine

!!!---------------------------------------------------------------- 

    Subroutine ComputeMass
       !!****e* RBDY3/COMPUTE MASS
       !! NAME
       !!   COMPUTE MASS
       !! PURPOSE
       !!  compute mass and inertia of bodies
       !! USES
       !!  LMGC90.CORE/RBDY3/comp_mass_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL comp_mass_RBDY3
    End Subroutine

    Subroutine NewRotationScheme
       !!****e* RBDY3/NEW ROTATION SCHEME
       !! NAME
       !!  NEW ROTATION SCHEME
       !! PURPOSE
       !!  active new rotation scheme FLAG
       !! USES
       !!  LMGC90.CORE/RBDY3/set_new_rotation_scheme_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       CALL set_new_rotation_scheme_RBDY3
    End Subroutine

    Subroutine SetSourcePoint(first_RBDY3,radius,Xshift,Yshift,Zshift)
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
       !CALL LOGCHIC('RBDY3')
       implicit none
       integer      :: first_RBDY3
       REAL(kind=8) :: radius,Xshift,Yshift,Zshift
       CALL init_source_point_RBDY3(first_RBDY3,radius,Xshift,Yshift,Zshift)

    End Subroutine

    Subroutine SetSourcePointwithini(first_RBDY3,radius,Xshift,Yshift,Zshift)
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
       !CALL LOGCHIC('RBDY3')
       implicit none
       integer      :: first_RBDY3
       REAL(kind=8) :: radius,Xshift,Yshift,Zshift
       CALL init4fd_source_point_RBDY3(first_RBDY3,radius,Xshift,Yshift,Zshift)
    End Subroutine

    Subroutine SetZminBoundary(Zmin)
       !!****e* RBDY3/INIT INF BOUNDARY
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
       !!  LMGC90.CORE/RBDY3/set_init_boundary_RBDY3
       !!****
       !CALL LOGCHIC('RBDY3')
       implicit none
       REAL(kind=8) :: Zmin

       CALL set_init_boundary_RBDY3(1,Zmin)
    End Subroutine

    Subroutine SetZmaxBoundary(Zmax)
       !!****e* RBDY3/INIT SUP BOUNDARY
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
       !CALL LOGCHIC('RBDY3')
       implicit none
       REAL(kind=8) :: Zmax

       CALL set_init_boundary_RBDY3(2,Zmax)
    End Subroutine

    Subroutine SetYminBoundary(Ymin)
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
       implicit none
       REAL(kind=8) :: Ymin


       CALL set_init_boundary_RBDY3(3,Ymin)
    End Subroutine

    Subroutine SetYmaxBoundary(Ymax)
       !!****e* RBDY3/INIT RIGHT BOUNDARY
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
       implicit none
       REAL(kind=8) :: Ymax

       CALL set_init_boundary_RBDY3(4,Ymax)
    End Subroutine

    Subroutine SetXminBoundary(Xmin)
       !!****e* RBDY3/INIT BACK BOUNDARY
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
       implicit none
       REAL(kind=8) :: Xmin

       CALL set_init_boundary_RBDY3(5,Xmin)
    End Subroutine

    Subroutine SetXmaxBoundary(Xmax)
       !!****e* RBDY3/INIT FRONT BOUNDARY
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
       implicit none
       REAL(kind=8) :: Xmax

       CALL set_init_boundary_RBDY3(6,Xmax)
    End Subroutine

    Subroutine SetXperiode(xperiode)
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
       implicit none
       REAL(kind=8) :: Xperiode
       CALL set_xperiodic_data_RBDY3(xperiode)

    END Subroutine

    Subroutine SetYperiode(Yperiode)
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
       implicit none
       REAL(kind=8) :: yperiode

       CALL set_yperiodic_data_RBDY3(yperiode)

    END Subroutine

    Subroutine AvoidBodyRotation
       !!****e* RBDY3/FIX BODY ROTATION
       !! NAME
       !!  FIX BODY ROTATION
       !! PURPOSE
       !!  kill rotation effect for RBDY3
       !! USES
       !!  LMGC90.CORE/RBDY3/without_rotation_of_RBDY3
       !!****
       implicit none
       CALL without_rotation_of_RBDY3
    End Subroutine


    Subroutine InitializeProgressiveActivation(zini,dz)
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
       implicit none
       real(kind=8) :: zini,dz
       CALL init_progressive_activation_RBDY3(zini,dz)

    END Subroutine

    Subroutine ApplyProgressiveActivation(freq)
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
       implicit none
       integer :: freq
       CALL do_progressive_activation_RBDY3(freq)
       RETURN      
    END Subroutine

    Subroutine SkipInvisible

       !fd if a body is invisible it won't be written in bodies.out and dof.out

       implicit none
       CALL set_skip_invisible_RBDY3

    END Subroutine

    Subroutine KeepIniDofOrder

       !fd numbering information as they are read (similar to bodies.dat)

       CALL set_keep_ini_dof_order_RBDY3 

    END Subroutine

!fd je ne sais pas ce que c'est 
    Subroutine UpdateGAMMAvsT
       !!****e* RBDY3/UPDATE GAMMAvsT
       !! NAME
       !!  UPDATE GAMMAvsT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY3/update_gamma_rbdy3
       !!****
       ! CALL LOGCHIC('RBDY3')
       implicit none
       CALL update_WS_rbdy3
    End Subroutine


!    Subroutine COLLECT BODIES.OUT
!       !  some more CHIC command to be used in preprocessing
!       CALL LOGCHIC('RBDY3')
!       CALL read_in_bodies_RBDY3(2)
!    End Subroutine 

!    Subroutine APPEND TO BODIES.OUT
!       !  some more CHIC command to be used in preprocessing
!       CALL LOGCHIC('RBDY3')
!       CALL write_out_bodies_RBDY3(1)
!   End Subroutine

!    SubroutineREBUILD BODIES.DAT
!       !  some more CHIC command to be used in preprocessing
!       CALL LOGCHIC('RBDY3')
!       CALL write_out_bodies_RBDY3(2)
!      End Subroutine

!     Subroutine MEMBRANE
!        CALL LOGCHIC('RBDY2')
!        IF (is_first_time) THEN
!           nfich=get_io_unit()
!           OPEN(unit=nfich,STATUS='REPLACE',file='radius.txt')
          
!           READ(CHLZZZ(NZZZ+1),*) sigma_min,sigma_max
!           READ(CHLZZZ(NZZZ+2),*) DT_ini,DT_load,DT_relax
!           READ(CHLZZZ(NZZZ+3),*) centre,ep
!           READ(CHLZZZ(NZZZ+4),*) nb_grain 
          
!           READ(CHLZZZ(NZZZ+5),*) up_grain
!           !     READ(CHLZZZ(NZZZ+6),*) up_sigma_min,up_sigma_max
!           READ(CHLZZZ(NZZZ+6),*) i_up_actif
          
!           IF (i_up_actif == 0) is_up_actif = .FALSE.
          
!           T0=TPSbegin
!           T1=T0+DT_ini
!           T2=T1+DT_load
!           T3=T2+DT_relax
          
!           is_first_time=.FALSE.
          
!           CALL membrane(sigma_min,sigma_max,up_sigma_min,up_sigma_max,T1,T2,T3,centre,ep,nb_grain,up_grain,.TRUE.,is_up_actif,nfich)
!        ELSE
!           nfich=get_io_unit()
!           OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file='radius.txt')
          
!           CALL membrane(sigma_min,sigma_max,up_sigma_min,up_sigma_max,T1,T2,T3,centre,ep,nb_grain,up_grain,.FALSE.,is_up_actif,nfich)
!        ENDIF

!        CLOSE(nfich)
       
!        IETZZZ = 1
!        RETURN      
!     END IF
    
!    Subroutine READ COMPRESSED BODIES
!       !!****e* RBDY3/READ COMPRESSED BODIES
!       !! NAME
!       !!   READ COMPRESSED BODIES
!       !! PURPOSE
!       !!  read BODIES.DAT file without any comment
!       !!  Initializes existing_entities variable in RBDY3
!       !!  Adds the number of found bodies to entity
!       !! USES
!       !!  LMGC90.CORE/RBDY3/read_in_compress_bodies_RBDY3
!       !!  LMGC90.CORE/RBDY3/update_existing_entities_RBDY3
!       !!****
!       CALL LOGCHIC('RBDY3')
!       
!       CALL read_in_comp_bodies_RBDY3(1)
!       CALL update_existing_entities_RBDY3
!       
!    End Subroutine




!!!------------------------------------------------------------------------
END MODULE wrap_RBDY3
