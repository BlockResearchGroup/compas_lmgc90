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

  !!****h* LMGC90.CHIPY/RBDY2
  !! NAME
  !!  module wrap_RBDY2
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/RBDY2
  !!****


  use utilities 

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
       check_periodic_RBDY2 => check_periodic, & ! am: on rend visible la fonction qui vérifie que la condition périodique est vérifiée
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
       get_coor, & ! am: on rend visible la fonction qui donne les coordonnées à la fin du pas de temps
!!$       read_extra_behaviours_rbdy2,&
       put_invmass_RBDY2,put_precon_W_RBDY2, &
       put_vector_RBDY2,get_vector_RBDY2, &
       get_area, & ! <- am: debut des fonctions supplementaires
       check_partial_equilibrium_state_RBDY2, &
       compute_partial_equilibrium_state_RBDY2, & ! <- am: debut des fonctions supplementaires
       is_periodic_RBDY2, &
       set_visible, &
       put_coor, &
       put_coor_begin, &
       get_coor_begin, &
       set_invisible, &
       comp_vlocy_4all_RBDY2

  PUBLIC

CONTAINS

    SUBROUTINE PutInvmass(ivalue1,rvect,ivalue4)
      IMPLICIT NONE
      integer,intent(in) :: ivalue1,ivalue4
      real(kind=8),intent(in):: rvect(ivalue4)
!f2py optional, depend(rvect) :: ivalue4=len(rvect)

       !!****e* RBDY2/PutInvmass
       !! NAME
       !!  PutInvmass
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/RBDY2
       !!****
       ! ibdyty,inodty,jdof,nbdof,vaux

       CALL put_invmass_RBDY2(ivalue1,rvect,ivalue4)

    END SUBROUTINE

    SUBROUTINE PutPreconW(ivalue1,ivalue2,ivalue3,rvect,ivalue4)
      IMPLICIT NONE
      integer,intent(in) :: ivalue1,ivalue2,ivalue3,ivalue4
      real(kind=8),intent(in):: rvect(ivalue4)
      real(kind=8) :: InvMass
!f2py optional, depend(rvect) :: ivalue4=len(rvect)

       !!****e* RBDY2/PutPreconW
       !! NAME
       !!  PutPreconW
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/RBDY2/PutPreconW
       !!****
       ! ibdyty,inodty,jdof,vaux,nbdof

       InvMass = rvect(ivalue3)

       CALL put_precon_W_RBDY2(ivalue1,ivalue3,InvMass)

    END SUBROUTINE

    SUBROUTINE PutVector(cvalue1,ivalue1,rvect,ivalue2)
      IMPLICIT NONE
      character(len=5),intent(in) :: cvalue1
      integer,intent(in):: ivalue1,ivalue2

      real(kind=8),intent(in) :: rvect(ivalue2)
!f2py optional, depend(rvect) :: ivalue2=len(rvect)

      character(len=128) :: cout


       !!****e* RBDY2/PutVector
       !! NAME
       !!  PutVector
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/RBDY2/PutVector
       !!****
       !ibdyty,nbdof,id_vect,vect

       write(cout,*) cvalue1,ivalue1,ivalue2,size(rvect)
       call logmes(cout)

       CALL put_vector_RBDY2(cvalue1,ivalue1,rvect,ivalue2)


    END SUBROUTINE

    SUBROUTINE GetVector(cvalue1,ivalue1,rvect,ivalue2)
      IMPLICIT NONE
      character(len=5),intent(in) :: cvalue1
      integer,intent(in):: ivalue1,ivalue2

      real(kind=8),intent(out) :: rvect(ivalue2)
!f2py optional, depend(rvect) :: ivalue2=len(rvect)

      character(len=128) :: cout


       !!****e* RBDY2/GetVector
       !! NAME
       !!  GetVector
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/RBDY2/GetVector
       !!****
       !ibdyty,nbdof,id_vect,vect
       write(cout,*) cvalue1,ivalue1,ivalue2 
       call logmes(cout)
       CALL get_vector_RBDY2(cvalue1,ivalue1,rvect,ivalue2)

    END SUBROUTINE
!!!---------------------------------------------------------------------

    SUBROUTINE IncrementStep

       !!****e* RBDY2/incrementstep
       !! NAME
       !!  incrementstep
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/RBDY2/construct_wall
       !!****


       IMPLICIT NONE

       CALL increment_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ComputeDof

       IMPLICIT NONE

       CALL comp_dof_RBDY2

       !fd maintenant c'est fait dans la routine compute dof
       !  
       ! am:
       ! si on impose une condition périodique sur l'axe (0x)
       !if (is_periodic_RBDY2()) then
       !
       !   ! on corrige les positions des grains qui la viole
       !   call check_periodic_RBDY2
       !
       !end if

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE UpdateDof

       IMPLICIT NONE
       CALL update_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ComputeFreeVelocity
       IMPLICIT NONE

       CALL comp_free_vlocy_RBDY2

    END SUBROUTINE

    SUBROUTINE ComputeNlFreeVelocity
       IMPLICIT NONE

       CALL comp_free_vlocy_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ComputeFext
       IMPLICIT NONE

       CALL comp_Fext_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ComputeFint
       IMPLICIT NONE

       CALL comp_Fint_RBDY2

    END SUBROUTINE

    SUBROUTINE ComputeTTFint
       IMPLICIT NONE

       CALL comp_Fint_RBDY2

    END SUBROUTINE

    SUBROUTINE ComputeNLBulk
       IMPLICIT NONE

       CALL comp_Fint_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE CheckEquilibriumState(check_convergence) 
       IMPLICIT NONE
       logical ::  check_convergence

       check_convergence = .FALSE.
       CALL check_equilibrium_state_RBDY2(check_convergence)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE GhostToInvisible
       IMPLICIT NONE
       CALL ghost2invisible_RBDY2 

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE CheckSourcePoint
       IMPLICIT NONE
       CALL check_source_point_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE CheckOutOfBounds
       IMPLICIT NONE
       CALL out_of_bounds_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE FatalDamping(entier)
       !!****e* RBDY2/FatalDamping
       !! NAME
       !!  FatalDamping
       !! PURPOSE
       !!  Nullify body velocities
       !! USES
       !!  LMGC90.CORE/RBDY2/fatal_damping_RBDY2
       !!****
       IMPLICIT NONE
       INTEGER :: entier


       CALL fatal_damping_RBDY2(entier)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE PartialDamping(entier,reel)
       !!****e* RBDY2/PartialDamping
       !! NAME
       !!  PartialDamping
       !! SYNOPSIS
       !!  PartialDamping
       !!  [Vmax]
       !! INPUTS
       !!  [Vmax] : maximal velocity
       !! PURPOSE
       !!  reduce body velocity greater than Vmax to the Vmax value
       !! USES
       !!  LMGC90.CORE/RBDY2/partial_damping_RBDY2
       !!****
       IMPLICIT NONE
       INTEGER :: entier
       REAL(KIND=8) :: reel


       CALL partial_damping_RBDY2(entier,reel)

    END SUBROUTINE


!!! WRITING COMMAND ------------------------------------------------

    SUBROUTINE WriteLastDof
       !!****e* RBDY2/WriteLastDof
       !! NAME
       !!   WriteLastDof
       !! PURPOSE
       !!  write ascii DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_dof_RBDY2
       !!****
       IMPLICIT NONE
       INTEGER :: ifrom,ito

          ito = get_nb_RBDY2()
          ifrom = 1

          CALL write_xxx_dof_RBDY2(2,ifrom,ito)          

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE BinaryWriteLastDof
       !!****e* RBDY2/BinaryWriteLastDof
       !! NAME
       !!   BinaryWriteLastDof
       !! PURPOSE
       !!  write binary DOF.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY2/binary_write_last_dof_RBDY2
       !!****
       IMPLICIT NONE

       CALL binary_write_last_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE BinaryWriteOutDof
       !!****e* RBDY2/BinaryWriteOutDof
       !! NAME
       !!  BinaryWriteOutDof
       !! PURPOSE
       !!  write binary DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY2/binary_write_out_dof_RBDY2
       !!****
       IMPLICIT NONE
       logical :: write_DOF

       write_DOF = get_write_DOF_RBDY2()
       IF (write_DOF) CALL binary_write_out_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE WriteOutDof(ivalue1,ivalue2)
       IMPLICIT NONE
       !INTEGER, optional :: ivalue1,ivalue2 ! am: argument optionnel: on considère les corps dont les indices
                                            ! apppartiennent à [ivalue1, ivalue2]
       integer, optional, intent(in) :: ivalue1, ivalue2
       INTEGER :: ifrom,ito,nb_RBDY2
       logical :: write_DOF

       !!****e* RBDY2/WriteOutDof
       !! NAME
       !!   WriteOutDof
       !! PURPOSE
       !!   write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_dof_RBDY2
       !!****

       write_DOF = get_write_DOF_RBDY2()
       IF (write_DOF) THEN
         nb_RBDY2 = get_nb_RBDY2()
         
         ! am: par défaut on utilise tous les corps
         ifrom = 1
         ito = nb_RBDY2

         ! am: si on donne une borne, ou les deux, pour l'intervalle 
         !     d'indices de corps à utiliser on corrige ses bornes
         ! ATTENTION: f2py gère mal les arguments optionnels!
         !            si, par exemple on ne passe pas ivalue1, il est 
         !            tout de même présent mais nul!
         if (present(ivalue1)) then
            ifrom = max0(ivalue1,1)
         end if
         if (present(ivalue2)) then
            if (ivalue2 > ivalue1) then
               ito   = min0(ivalue2,nb_RBDY2)
            end if
         end if

         CALL write_xxx_dof_RBDY2(1,ifrom,ito)

       END IF

    END SUBROUTINE
!----------------------------------------------------

    SUBROUTINE DisplayOutDof(ivalue1,ivalue2)
       IMPLICIT NONE
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY2

       !!****e* RBDY2/DisplayOutDof
       !! NAME
       !!   DisplayOutDof
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_dof_RBDY2
       !!****

       nb_RBDY2 = get_nb_RBDY2()
       ifrom = max0(ivalue1,1)
       ito   = min0(ivalue2,nb_RBDY2)

       CALL write_xxx_dof_RBDY2(6,ifrom,ito)
 
    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE WriteLastRnod
       IMPLICIT NONE

       INTEGER :: ifrom,ito,nb_RBDY2

       !!****e* RBDY2/WriteLastRnod
       !! NAME
       !!   WriteLastRnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_Rnod_RBDY2
       !!****

        nb_RBDY2 = get_nb_RBDY2()
        ifrom = 1  
        ito   = nb_RBDY2
        CALL write_xxx_Rnod_RBDY2(2,ifrom,ito)

    END SUBROUTINE

!----------------------------------------------------

    Subroutine WriteOutRnod(ivalue1,ivalue2)
       IMPLICIT NONE
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY2
       logical :: write_Rnod

       !!****e* RBDY2/WriteOutRnod
       !! NAME
       !!   WriteOutRnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_Rnod_RBDY2
       !!****

       write_Rnod = get_write_Rnod_RBDY2()
       IF (write_Rnod) THEN
         nb_RBDY2 = get_nb_RBDY2()
         ifrom = max0(ivalue1,1)
         ito   = min0(ivalue2,nb_RBDY2)

         CALL write_xxx_Rnod_RBDY2(1,ifrom,ito)

       END IF

    END SUBROUTINE

!----------------------------------------------------

    Subroutine DisplayOutRnod(ivalue1,ivalue2)
       IMPLICIT NONE
       INTEGER :: ivalue1,ivalue2
       INTEGER :: ifrom,ito,nb_RBDY2


       !!****e* RBDY2/DisplayOutRnod
       !! NAME
       !!   DisplayOutRnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/RBDY2/write_xxx_Rnod_RBDY2
       !!****

       nb_RBDY2 = get_nb_RBDY2()

       ifrom = max0(ivalue1,1)
       ito   = min0(ivalue2,nb_RBDY2)

       CALL write_xxx_Rnod_RBDY2(6,ifrom,ito)

    END SUBROUTINE

!----------------------------------------------------

    Subroutine WriteBodies
       IMPLICIT NONE

       !!****e* RBDY2/WriteBodies
       !! NAME
       !!   WriteBodies
       !! PURPOSE
       !!  write BODIES.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_out_bodies_RBDY2
       !!****

       CALL write_out_bodies_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ClearedWriteBodies
       IMPLICIT NONE

       !!****e* RBDY2/ClearedWriteBodies
       !! NAME
       !!   ClearedWriteBodies
       !! PURPOSE
       !!  ...
       !! USES
       !!  LMGC90.CORE/RBDY2/write_out_cleared_bodies_RBDY2
       !!****

       CALL write_out_cleared_bodies_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE WriteDrivenDof
       IMPLICIT NONE
       !!****e* RBDY2/WriteDrivenDof
       !! NAME
       !!   WriteDrivenDof
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY2/write_out_driven_dof_RBDY2
       !!****

       CALL write_out_driven_dof_RBDY2

    END SUBROUTINE

!!! READING COMMAND ----------------------------------------------------

    SUBROUTINE ReadBodies
       IMPLICIT NONE
       !!****e* RBDY2/ReadBodies
       !! NAME
       !!   ReadBodies
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable in RBDY2
       !!  Adds the number of found bodies to entity
       !! USES
       !!  LMGC90.CORE/RBDY2/read_in_bodies_RBDY2
       !!  LMGC90.CORE/RBDY2/update_existing_entities_RBDY2
       !!****

       CALL read_in_bodies_RBDY2
       CALL update_existing_entities_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ReadIniDof
       IMPLICIT NONE
       !!****e* RBDY2/ReadIniDof
       !! NAME
       !!   ReadIniDof
       !! PURPOSE
       !!  read DOF.INI file
       !! USES
       !!  LMGC90.CORE/RBDY2/read_in_dof_RBDY2
       !!****

       CALL read_in_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ReadDrivenDof
       IMPLICIT NONE
       !!****e* RBDY2/ReadDrivenDof
       !! NAME
       !!   ReadDrivenDof
       !! PURPOSE
       !!  read DRV_DOF.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY2/read_in_driven_dof_RBDY2
       !!****
       CALL read_in_driven_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE BinaryReadIniDof
       IMPLICIT NONE
       !!****e* RBDY2/BinaryReadIniDof
       !! NAME
       !!   BinaryReadIniDof
       !! PURPOSE
       !!   read binary file DOF.INI
       !! USES
       !!  LMGC90.CORE/RBDY2/binary_read_in_dof_RBDY2
       !!****

       CALL binary_read_in_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE LoadBehaviours
       IMPLICIT NONE
       !!****e* RBDY2/LoadBehaviours
       !! NAME
       !!   LoadBehaviours
       !! PURPOSE
       !!  read BULK_BEHAV.DAT file
       !! USES
       !!  LMGC90.CORE/RBDY2/read_behaviours_RBDY2
       !!****

       CALL read_behaviours_RBDY2

    END SUBROUTINE

!----------------------------------------------------

!!$    SUBROUTINE ReadExtraBehaviours(disper)
!!$       IMPLICIT NONE
!!$       REAL(kind=8) :: disper
!!$       !!****e* RBDY2/ReadExtraBehaviours
!!$       !! NAME
!!$       !!   ReadExtraBehaviours
!!$       !! PURPOSE
!!$       !!  read extra physical behaviour in BULK_BEHAV.DAT file.
!!$       !!  Must be used for THERMO_RIGID ELECTRO_RIGID and 
!!$       !!  THERMO_ELECTRO_RIGID behaviour
!!$       !! USES
!!$       !!  LMGC90.CORE/RBDY2/read_extra_behaviours_RBDY2
!!$       !!****
!!$
!!$
!!$       CALL read_extra_behaviours_RBDY2(disper)
!!$
!!$    END SUBROUTINE

!!!---------------------------------------------------------------- 

    Subroutine ComputeMass
       IMPLICIT NONE
       !!****e* RBDY2/ComputeMass
       !! NAME
       !!  ComputeMass
       !! PURPOSE
       !!  compute mass and inertia of bodies
       !! USES
       !!  LMGC90.CORE/RBDY2/comp_mass_RBDY2
       !!****

       CALL comp_mass_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    Subroutine SetPeriodicCondition(periode)
       IMPLICIT NONE
       real(kind=8) :: periode

       !!****e* RBDY2/SetPeriodicCondition
       !! NAME
       !!   PeriodicCondition
       !! SYNOPSIS
       !!   PeriodicCondition
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!   LMGC90.CORE/RBDY2/set_periodic_data_RBDY2
       !!****

       CALL set_periodic_data_RBDY2(periode)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE ResizeBodies(homo)
       IMPLICIT NONE
       real(kind=8) :: homo

       !!****e* RBDY2/ResizeBodies
       !! NAME
       !!  sizeBodies
       !! SYNOPSIS
       !!  sizeBodies
       !!  [homo]
       !! INPUTS
       !!  [homo] : homothety factor
       !! PURPOSE
       !!  resize body radius of a  [homo] factor
       !! USES
       !!  LMGC90.CORE/RBDY2/resize_RBDY2
       !!****

       WRITE(*,'(A10,D12.5)')'  @ homo =',homo
       CALL resize_RBDY2(homo)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE NullifyDisplacements
       IMPLICIT NONE
       !!****e* RBDY2/NullifyDisplacements
       !! NAME
       !!  IniXDofNull
       !! PURPOSE
       !!  nullify X degree of freedom
       !! USES
       !!  LMGC90.CORE/RBDY2/nullify_X_dof_RBDY2
       !!****


       CALL nullify_X_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE NullifyVelocities
       IMPLICIT NONE
       !!****e* RBDY2/NullifyVelocities
       !! NAME
       !!  IniXDofNull
       !! PURPOSE
       !!  nullify V degree of freedom 
       !! USES
       !!  LMGC90.CORE/RBDY2/nullify_V_dof_RBDY2
       !!****

       CALL nullify_V_dof_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE SetSourcePoint(ivalue1,rvalue1,rvalue2,rvalue3)
       IMPLICIT NONE
       INTEGER      :: ivalue1
       REAL(KIND=8) :: rvalue1,rvalue2,rvalue3 

       !!****e* RBDY2/SetSourcePoint
       !! NAME
       !!  INITSourcePoint
       !! SYNOPSIS
       !!  INITSourcePoint
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

       CALL init_source_point_RBDY2(ivalue1,rvalue1,rvalue2,rvalue3)

    END SUBROUTINE
    
!----------------------------------------------------

    SUBROUTINE SetLowerBoundary(rvalue1)
       IMPLICIT NONE
       REAL(KIND=8) :: rvalue1


       !!****e* RBDY2/SetLowerBoundary
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


       CALL set_init_boundary_RBDY2(1,rvalue1)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE SetUpperBoundary(rvalue1)
       IMPLICIT NONE
       REAL(KIND=8) :: rvalue1

       !!****e* RBDY2/SetUpperBoundary
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
       CALL set_init_boundary_RBDY2(2,rvalue1)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE SetLeftBoundary(rvalue1)
       IMPLICIT NONE
       REAL(KIND=8) :: rvalue1

       !!****e* RBDY2/SetLeftBoundary
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

       CALL set_init_boundary_RBDY2(3,rvalue1)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE SetRightBoundary(rvalue1)
       IMPLICIT NONE
       REAL(KIND=8) :: rvalue1

       !!****e* RBDY2/SetRightBoundary
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

       CALL set_init_boundary_RBDY2(4,rvalue1)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE CheckEquilibrium(cvalue1,rvalue1)
       IMPLICIT NONE
       CHARACTER(LEN=5) :: cvalue1
       REAL(KIND=8) :: rvalue1


       !!****e* RBDY2/CheckEquilibrium
       !! NAME
       !!   EquilibriumNorm
       !! SYNOPSIS
       !!   EquilibriumNorm
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

       CALL set_data_equilibrium_RBDY2(cvalue1,rvalue1)

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE AddDof2InBodies
       IMPLICIT NONE
       !!****e* RBDY2/AddDof2InBodies
       !! NAME
       !!  MakeBodies+Dof
       !! PURPOSE
       !!  create a new BODIES.OUT file as a combination
       !!  of the last one and of the last DOF.OUT file
       !! USES
       !!  LMGC90.CORE/RBDY2/add_dof2bodies_RBDY2
       !!****

       CALL add_dof2bodies_RBDY2

    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE Rigid2Mailx
       IMPLICIT NONE
       !!****e* RBDY2/Rigid2Mailx
       !! NAME
       !!  Rigid2Mailx
       !! PURPOSE
       !!  ...
       !! TODO
       !!  check this routine
       !!****
 
    END SUBROUTINE

!----------------------------------------------------

    SUBROUTINE InitWall(rvalue1,rvalue2,rvalue3,rvalue4,rvalue5,rvalue6)
       IMPLICIT NONE

       REAL(KIND=8) :: rvalue1,rvalue2,rvalue3,rvalue4,rvalue5,rvalue6

       !!****e* RBDY2/InitWall
       !! NAME
       !!  InitConstructWall
       !! SYNOPSIS
       !!  InitConstructWall 
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

       CALL init_construct_wall(rvalue1,rvalue2,rvalue1,rvalue2,rvalue3,rvalue4,rvalue5,rvalue6)
       CALL construct_wall(1)

    END SUBROUTINE
!----------------------------------------------------

    SUBROUTINE BuildWall(entier)
       !!****e* RBDY2/BuildWall
       !! NAME
       !!  ConstructWall
       !! PURPOSE
       !!  build a wall of rigid bodies
       !! USES
       !!  LMGC90.CORE/RBDY2/construct_wall
       !!****
       IMPLICIT NONE
       INTEGER :: entier

       CALL construct_wall(entier)

    END SUBROUTINE

!!!-am----- début des définitions de fonctions supplémentaires...------

    ! fonction qui renvoie le nombre de RBDY2
    function GetNbRBDY2()

       implicit none

       ! valeur de retour
       integer :: GetNbRBDY2 ! nombre de rigides 2D

       GetNbRBDY2 = get_nb_RBDY2()

    end function GetNbRBDY2

    ! fonction qui renvoie les coordonnées au début du pas de temps
    ! précondition:
    !   - ibdyty: numéro du corps
    !   - itacty: numéro du contacteur
    ! postcondition:
    !   - renvoie les coordonnées (translation suivant les directions (0x) et (0y), rotation autour de l'axe (0z))
    !     du contacteur itacty, attaché au corps ibdyty
    function GetCoor(ibdyty,itacty)

       implicit none

       ! variables d'entrée
       integer, intent(in) :: ibdyty ! numéro du corps
       integer, intent(in) :: itacty ! numéro du contacteur
       
       ! valeur de retour
       real(kind=8), dimension(3) :: GetCoor
       
       GetCoor = get_coor(ibdyty,itacty)

    end function GetCoor

    ! fonction qui récupère l'aire (volume en 2D) d'un corps
    function GetArea(ibdyty)
      implicit none
      integer, intent(in) :: ibdyty ! numéro du corps dont on veut récupérer l'aire
      real(kind=8)        :: GetArea ! aire du corps numéro ibdyty

      !!****e* DISKx/GetArea
      !! NAME
      !!   wrap_diskx.getarea
      !! PURPOSE
      !!  return area (2D volume equivalent) of a body
      !! USES
      !!  LMGC90.CORE/RBDY2/get_area
      !!****
      GetArea = get_area(ibdyty)
    end function GetArea

    ! fonction qui teste si une partie de l'achantillon est en équilibre
    ! les corps ayant leur abscisse comprise entre 0 et abs_max
    ! elle sert dans le cas des silosn pour décider si on active la 
    ! recherche d'arches
    ! précondition:
    !    - abs_min: abscisse minimale définissant les corps pris en compte
    !    - abs_max: abscisse maximale définissant les corps pris en compte
    ! postcondition:
    !    - info: vaut "vrai" ssi l'équilibre a été atteint
    subroutine CheckPartialEquilibriumState(info, abs_min, abs_max)

       IMPLICIT NONE 

       ! variable d'entrée
       real(kind=8), intent(in) :: abs_min, abs_max
          ! le calcul n'est effectué que pour les corps dont l'abscisse
          ! est entre abs_min et abs_max
       
       ! variable de sortie
       LOGICAL, intent(out) :: info
 
       call check_partial_equilibrium_state_RBDY2(info, abs_min, abs_max)
       
    end subroutine CheckPartialEquilibriumState 

    ! fonction qui calcule les grandeurs necessaires pour tester si on a atteint l'etat d'equilibre :
    !   * Qnorm : 1/N*sum(<|v_i|>), ou |v_i| est la norme euclidienne du vecteur vitesse v_i
    !   * Mnorm : sup(<|v_i|>) 
    ! pour les corps dont l'ordonnee est situe entre alt_min et alt_max
    subroutine ComputePartialEquilibriumState(abs_min, abs_max, Qnorm, Mnorm)

       IMPLICIT NONE 

       ! variable d'entrée
       real(kind=8), intent(in) :: abs_min, abs_max
          ! le calcul n'est effectué que pour les corps dont l'abscisse
          ! est entre abs_min et abs_max
       
       ! variables de sortie
       real(kind=8), intent(out) :: Qnorm, Mnorm
          !   * Qnorm : 1/N*sum(<|v_i|>), ou |v_i| est la norme euclidienne du vecteur vitesse v_i
          !   * Mnorm : sup(<|v_i|>)
 
       call compute_partial_equilibrium_state_RBDY2(abs_min, abs_max, Qnorm, Mnorm)
       
    end subroutine ComputePartialEquilibriumState 

    ! fonction qui permet de modifier les coordonnées, à la fin du pas de temps, d'un corps
    ! précondition:
    !    - ibdyty: numéro du corps auquel on veut imposer de nouvelles coordonnées
    !    - new_coor: nouvelles coordonnées du corps
    !    - itacty (en OPTION): pour agir sur un certain conacteur attaché au corps (am: est-ce utile?)
    ! postcondition:
    !    - les coordonnées à la fin du pas de temps du corps ibdyty sont celles stockées dans
    !      new_coor 
    subroutine PutCoor(ibdyty, new_coor)

       implicit none

       ! variables d'entrée:
       integer, intent(in) :: ibdyty ! numéro du corps on veut imposer de nouvelles coordonnées
       real(kind=8), dimension(3) :: new_coor ! nouvelles coordonnées du corps
   
       call put_coor(ibdyty, new_coor)

    end subroutine PutCoor

    ! fonction qui permet de modifier les coordonnées, au début du pas de temps, d'un corps
    ! précondition:
    !    - ibdyty: numéro du corps auquel on veut imposer de nouvelles coordonnées
    !    - new_coor: nouvelles coordonnées du corps
    ! postcondition:
    !    - les coordonnées à la fin du pas de temps du corps ibdyty sont celles stockées dans
    !      new_coor 
    subroutine PutCoorBegin(ibdyty, new_coor)

       implicit none

       ! variables d'entrée:
       integer, intent(in) :: ibdyty ! numéro du corps on veut imposer de nouvelles coordonnées
       real(kind=8), dimension(3) :: new_coor ! nouvelles coordonnées du corps
   
       call put_coor_begin(ibdyty, new_coor)

    end subroutine PutCoorBegin

    ! fonction qui permet de récupérer les coordonnées au début du pas de temps d'un corps
    ! précondition:
    !    - ibdyty: numéro du corps dont on veut imposer de nouvelles coordonnées
    !    - itacty (en OPTION): pour agir sur un certain conacteur attaché au corps (am: est-ce utile?)
    ! postcondition:
    !    - le vecteur retourné contient les coordonnées du corps au début du pas de temps
    function GetCoorBegin(ibdyty,itacty)

       IMPLICIT NONE

       ! variables d'entrée:
       integer, intent(in) :: ibdyty ! numéro du corps on veut imposer de nouvelles coordonnées
       integer, intent(in) :: itacty ! pour agir sur un certain conacteur attaché au corps (au cas où...)

       ! variable de retour
       REAL(kind=8),DIMENSION(3) :: GetCoorBegin ! coordonnées au début du pas de temps

       GetCoorBegin = get_coor_begin(ibdyty,itacty)

    end function GetCoorBegin

!!!-am------ fin des definitions de fonctions supplementaires...-------

    SUBROUTINE SetInvisible(list_bdy,nb_bdy)
      IMPLICIT NONE
      integer,intent(in) :: nb_bdy
      integer,intent(in) :: list_bdy(nb_bdy)

!f2py optional, depend(list_bdy) :: nb_bdy=len(list_bdy)

       CALL set_invisible(nb_bdy,list_bdy)


    END SUBROUTINE

    subroutine SetVisible(list_bdy,nb_bdy)

       implicit none
      integer,intent(in) :: nb_bdy
      integer,intent(in) :: list_bdy(nb_bdy)

!f2py optional, depend(list_bdy) :: nb_bdy=len(list_bdy)
       
       ! variables d'entrée
       integer :: ibdyty ! numéro du corps dont on veut modifier la visibilité    
       logical :: visible ! vaut: - "vrai", si on veut que le corps devienne visible
                        !       - "faux", si on veut que le corps devienne invisible
     
       call set_visible(nb_bdy,list_bdy)

    end subroutine

!----------------------------------------------------

    SUBROUTINE compvlocy4all

       IMPLICIT NONE

       CALL comp_vlocy_4all_RBDY2

    END SUBROUTINE

!----------------------------------------------------


END MODULE wrap_RBDY2
