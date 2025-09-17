!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
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
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================
MODULE wrap_RBDY3

  USE ISO_C_BINDING

  use overall, only: faterr

  USE RBDY3,ONLY:&
       get_nb_RBDY3, &
       increment_RBDY3, &
       update_WS_RBDY3, &
       set_vlocy_drvdof_RBDY3, &
       fatal_damping_RBDY3, &
       comp_Fext_RBDY3, &
       comp_Fint_RBDY3, &
       comp_free_vlocy_RBDY3, &
       comp_dof_RBDY3, &
       update_dof_RBDY3, &
       write_xxx_dof_RBDY3, &
       write_xxx_Rnod_RBDY3, &
       write_out_bodies_RBDY3, &
       write_out_driven_dof_RBDY3, &
       read_in_bodies_RBDY3, &
       update_existing_entities_RBDY3, &
       read_in_dof_RBDY3, &
       read_in_driven_dof_RBDY3, &
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
       init_free_boundary_RBDY3, &
       set_visible, &
       get_visible, &
       comp_coor_4all_RBDY3, &
       get_density, &
       get_V, &
       get_coor, &
       get_ptr_mass, &
       get_mass, &
       put_V, &
       get_vector_RBDY3, &
       get_ptr_vector_RBDY3, &
       put_vector_RBDY3, &
       comp_glob_inert_RBDY3, &
       triaxial_loading, &
       get_drv_vlocy_RBDY3, comp_drv_vlocy_RBDY3, &
       get_matrix_RBDY3, &
       put_matrix_RBDY3, &
       get_all_rdata_RBDY3, &
       get_behav, & 
       get_Nb_tacty, &
       get_tacID, &
       get_color, &
       set_color_RBDY3 , &
       write_out_one_RBDY3, &
       write_out_dof_one_RBDY3, &
       load_thread_network_RBDY3, &
       set_invisible_small_objects, &
       compute_configurationTT_RBDY3, &
       switch_vlocy_driven_dof, &
       partial_damping_RBDY3  , &
       get_volume             , &
       renum_visible_RBDY3    , & !pta
       clean_memory_RBDY3, &
       get_bulk_behav_number_RBDY3, &
       get_thermal_value, &
       set_data_equilibrium_RBDY3, &
       check_equilibrium_state_RBDY3, &
       get_dofstatus_RBDY3

CONTAINS

!!!-------------------------------------------------------------------------

    SUBROUTINE IncrementStep() bind(c, name='RBDY3_IncrementStep')
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method
       IMPLICIT NONE

       CALL increment_RBDY3

     END SUBROUTINE IncrementStep

    SUBROUTINE IncrementWSvsT() bind(c, name='RBDY3_IncrementWSvsT')
       IMPLICIT NONE

       CALL update_WS_RBDY3

     END SUBROUTINE IncrementWSvsT

    SUBROUTINE SetVlocyDrivenDof(ibdyty, idrvdof, value) bind(c, name='RBDY3_SetVlocyDrivenDof')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty
      INTEGER(c_int), INTENT(in), value :: idrvdof
      REAL(c_double), INTENT(in), value :: value

      CALL set_vlocy_drvdof_RBDY3(ibdyty, idrvdof, value)

    END SUBROUTINE

    subroutine FatalDamping(list_ids, length) bind(c, name='RBDY3_FatalDamping')
      use timer
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] fatal damp  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_RBDY3()
          call fatal_damping_RBDY3(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call fatal_damping_RBDY3(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine FatalDamping

    subroutine ComputeFext() bind(c, name='RBDY3_ComputeFext')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] comp Fext   ')
      call start_itimer(timer_id)

      call comp_Fext_RBDY3

      call stop_itimer(timer_id)

    end subroutine ComputeFext

    subroutine ComputeBulk() bind(c, name='RBDY3_ComputeBulk')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] comp bulk   ')
      call start_itimer(timer_id)

      call comp_Fint_RBDY3

      call stop_itimer(timer_id)

    end subroutine ComputeBulk    
    
    subroutine ComputeFreeVelocity() bind(c, name='RBDY3_ComputeFreeVelocity')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] comp Free V ')
      call start_itimer(timer_id)

      call comp_free_vlocy_RBDY3

      call stop_itimer(timer_id)

    end subroutine ComputeFreeVelocity
    
    subroutine ComputeDof() bind(c, name='RBDY3_ComputeDof')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] comp dof    ')
      call start_itimer(timer_id)

      call comp_dof_RBDY3

      call stop_itimer(timer_id)

    end subroutine    

    subroutine UpdateDof() bind(c, name='RBDY3_UpdateDof')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] update dof  ')
      call start_itimer(timer_id)

      call update_dof_RBDY3

      call stop_itimer(timer_id)

    end subroutine    

    subroutine ComputeContactDetectionConfiguration() bind(c, name='RBDY3_ComputeContactDetectionConfiguration')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] comp coorTT ')
      call start_itimer(timer_id)

      call compute_configurationTT_RBDY3

      call stop_itimer(timer_id)

    end subroutine ComputeContactDetectionConfiguration

    
!!! WRITING COMMAND ------------------------------------------------
    
    SUBROUTINE WriteLastDof() bind(c, name='RBDY3_WriteLastDof')
       !! PURPOSE
       !!  write ascii DOF.LAST file
       IMPLICIT NONE
       INTEGER :: ifrom,ito

       ifrom = 1
       ito = get_nb_RBDY3()

       CALL write_xxx_dof_RBDY3(2,ifrom,ito)          

    END SUBROUTINE

    SUBROUTINE WriteOutDof(ifrom,ito) bind(c, name='RBDY3_WriteOutDof')
       !! PURPOSE
       !!   write ascii DOF.OUT file. Can be activate only each N step
       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN), VALUE :: ifrom,ito
       INTEGER :: ivalue1,ivalue2,nb_RBDY3
       LOGICAL :: write_DOF

       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) THEN
         nb_RBDY3 = get_nb_RBDY3()
         
         ivalue1 = 1
         ivalue2 = nb_RBDY3

         ivalue1 = max0(ifrom,1)
         if (ito > ifrom) then
            ivalue2 = min0(ito,nb_RBDY3)
         end if
         CALL write_xxx_dof_RBDY3(1,ivalue1,ivalue2)
       END IF

    END SUBROUTINE

    SUBROUTINE DisplayOutDof() bind(c, name='RBDY3_DisplayOutDof')
       !! PURPOSE
       !!  display body degrees of freedom
       IMPLICIT NONE
       INTEGER :: ivalue1,ivalue2
       LOGICAL :: write_DOF

       write_DOF = get_write_DOF_RBDY3()
       IF (write_DOF) THEN
         ivalue1 = 1
         ivalue2 = get_nb_RBDY3()
         CALL write_xxx_dof_RBDY3(6,ivalue1,ivalue2)
       END IF

    END SUBROUTINE

    SUBROUTINE WriteLastRnod() bind(c, name='RBDY3_WriteLastRnod')
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       IMPLICIT NONE
       INTEGER :: ifrom,ito,nb_RBDY3

       nb_RBDY3 = get_nb_RBDY3()
       ifrom = 1  
       ito   = nb_RBDY3
       CALL write_xxx_Rnod_RBDY3(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutRnod() bind(c, name='RBDY3_WriteOutRnod')
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       IMPLICIT NONE
       INTEGER :: ivalue1,ivalue2
       LOGICAL :: write_Rnod

       write_Rnod = get_write_Rnod_RBDY3()
       IF (write_Rnod) THEN
         ivalue1 = 1
         ivalue2 = get_nb_RBDY3()
         CALL write_xxx_Rnod_RBDY3(1,ivalue1,ivalue2)
       END IF

    END SUBROUTINE

    SUBROUTINE DisplayOutRnod() bind(c, name='RBDY3_DisplayOutRnod')
       !! PURPOSE
       !!  display body forces.
       IMPLICIT NONE
       INTEGER :: ivalue1,ivalue2
       LOGICAL :: write_Rnod

       write_Rnod = get_write_Rnod_RBDY3()
       IF (write_Rnod) THEN
         ivalue1 = 1
         ivalue2 = get_nb_RBDY3()
         CALL write_xxx_Rnod_RBDY3(6,ivalue1,ivalue2)
       END IF

    END SUBROUTINE

    SUBROUTINE WriteBodies() bind(c, name='RBDY3_WriteBodies')
       !! PURPOSE
       !!  write BODIES.OUT file
       IMPLICIT NONE   

       CALL write_out_bodies_RBDY3(1)

    END SUBROUTINE

    SUBROUTINE WriteDrivenDof() bind(c, name='RBDY3_WriteDrivenDof')
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       IMPLICIT NONE

       CALL write_out_driven_dof_RBDY3

    END SUBROUTINE

!!! READING COMMAND ----------------------------------------------------

    SUBROUTINE ReadBodies() bind(c, name='RBDY3_ReadBodies')
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable in RBDY3
       !!  Adds the number of found bodies to entity
       IMPLICIT NONE 

       CALL read_in_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3

    END SUBROUTINE

    SUBROUTINE ReadCompressedBodies() bind(c, name='RBDY3_ReadCompressedBodies')
       !! PURPOSE
       !!  read BODIES.DAT file without any comment
       !!  Initializes existing_entities variable in RBDY3
       !!  Adds the number of found bodies to entity
       IMPLICIT NONE
       
       CALL read_in_comp_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3
       
    END SUBROUTINE

    subroutine ReadIniDof(step) bind(c, name='RBDY3_ReadIniDof')
      implicit none
      integer(c_int), intent(in), value :: step

      call read_in_dof_RBDY3(step)

    end subroutine

    SUBROUTINE ReadDrivenDof() bind(c, name='RBDY3_ReadDrivenDof')
       !! PURPOSE
       !!  read DRV_DOF.DAT file

       IMPLICIT NONE
       CALL read_in_driven_dof_RBDY3
    END SUBROUTINE

    SUBROUTINE LoadBehaviours() bind(c, name='RBDY3_LoadBehaviours')
       !! PURPOSE
       !!  read BULK_BEHAV.DAT file
       IMPLICIT NONE

       CALL read_behaviours_RBDY3

    END SUBROUTINE

    SUBROUTINE MP_LoadBehaviours(disper) bind(c, name='RBDY3_LoadMpBehaviours')
       !! PURPOSE
       !!  read extra physical behaviour in BULK_BEHAV.DAT file.
       !!  Must be used for THERMO_RIGID ELECTRO_RIGID and 
       !!  THERMO_ELECTRO_RIGID behaviour
       IMPLICIT NONE
       REAL(c_double), INTENT(in), value :: disper

       CALL read_mp_behaviours_RBDY3(disper)

    END SUBROUTINE

!!!---------------------------------------------------------------- 

    subroutine ComputeMass() bind(c, name='RBDY3_ComputeMass')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_RBDY3() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY3] comp mass   ')
      call start_itimer(timer_id)

      call comp_mass_RBDY3

      call stop_itimer(timer_id)

    end subroutine

    SUBROUTINE NewRotationScheme() bind(c, name='RBDY3_NewRotationScheme')
       !! PURPOSE
       !!  active new rotation scheme FLAG
       IMPLICIT NONE

       CALL set_new_rotation_scheme_RBDY3

    END SUBROUTINE

    SUBROUTINE SetSourcePoint(first_RBDY3,radius,Xshift,Yshift,Zshift) bind(c, name='RBDY3_SetSourcePoint')
       !! PURPOSE
       !!  create an assembly by source point deposit
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: first_RBDY3
       REAL(c_double), INTENT(in), value :: radius,Xshift,Yshift,Zshift

       CALL init_source_point_RBDY3(first_RBDY3,radius,Xshift,Yshift,Zshift)

    END SUBROUTINE

    SUBROUTINE SetSourcePointWithIni(first_RBDY3,radius,Xshift,Yshift,Zshift) bind(c, name='RBDY3_SetSourcePointWithIni')
       !! PURPOSE
       !!  create an assembly by source point deposit
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: first_RBDY3
       REAL(c_double), INTENT(in), value :: radius,Xshift,Yshift,Zshift

       CALL init4fd_source_point_RBDY3(first_RBDY3,radius,Xshift,Yshift,Zshift)

    END SUBROUTINE

    SUBROUTINE SetZminBoundary(Zmin) bind(c, name='RBDY3_SetZminBoundary')
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Zmin

       CALL set_init_boundary_RBDY3(5,Zmin)

    END SUBROUTINE

    SUBROUTINE SetZmaxBoundary(Zmax) bind(c, name='RBDY3_SetZmaxBoundary')
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Zmax

       CALL set_init_boundary_RBDY3(6,Zmax)

    END SUBROUTINE

    SUBROUTINE SetYminBoundary(Ymin) bind(c, name='RBDY3_SetYminBoundary')
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Ymin

       CALL set_init_boundary_RBDY3(3,Ymin)

    END SUBROUTINE

    SUBROUTINE SetYmaxBoundary(Ymax) bind(c, name='RBDY3_SetYmaxBoundary')
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Ymax

       CALL set_init_boundary_RBDY3(4,Ymax)

    END SUBROUTINE

    SUBROUTINE SetXminBoundary(Xmin) bind(c, name='RBDY3_SetXminBoundary')
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Xmin

       CALL set_init_boundary_RBDY3(1,Xmin)

    END SUBROUTINE

    SUBROUTINE SetXmaxBoundary(Xmax) bind(c, name='RBDY3_SetXmaxBoundary')
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Xmax

       CALL set_init_boundary_RBDY3(2,Xmax)

    END SUBROUTINE

    SUBROUTINE SetXperiode(xperiode) bind(c, name='RBDY3_SetXPeriodicCondition')
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: Xperiode

       CALL set_xperiodic_data_RBDY3(xperiode)

    END SUBROUTINE

    SUBROUTINE SetYperiode(Yperiode) bind(c, name='RBDY3_SetYPeriodicCondition')
       !! PURPOSE
       !!  [periode] periode of simulation system. The Y variable reaches
       !!  between 0 and [periode]
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: yperiode

       CALL set_yperiodic_data_RBDY3(yperiode)

    END SUBROUTINE

    SUBROUTINE AvoidBodyRotation() bind(c, name='RBDY3_AvoidBodyRotation')
       !! PURPOSE
       !!  kill rotation effect for RBDY3
       IMPLICIT NONE

       CALL without_rotation_of_RBDY3

    END SUBROUTINE


    SUBROUTINE InitializeProgressiveActivation(zini,dz) bind(c, name='RBDY3_InitializeProgressiveActivation')
       !! PURPOSE
       !!  [zini] initial altitude
       !!  [dz] increment of altitude
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE ::  zini,dz

       CALL init_progressive_activation_RBDY3(zini,dz)

    END SUBROUTINE

    SUBROUTINE ApplyProgressiveActivation(freq) bind(c, name='RBDY3_ApplyProgressiveActivation')
       !! PURPOSE
       !!  [step] occurence of ativation 
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: freq

       CALL do_progressive_activation_RBDY3(freq)

    END SUBROUTINE

    SUBROUTINE InitFreeBoundary(xmin,xmax,ymin,ymax,radius) bind(c, name='RBDY3_InitFreeBoundary')
       IMPLICIT NONE
       REAL(c_double), INTENT(in), value :: xmin, xmax, ymin, ymax, radius

       CALL init_free_boundary_RBDY3(xmin,xmax,ymin,ymax,radius)

    END SUBROUTINE

    !fd if a body is invisible it won't be written in bodies.out and dof.out
    SUBROUTINE SkipInvisible() bind(c, name='RBDY3_SkipInvisible')
       IMPLICIT NONE
       CALL set_skip_invisible_RBDY3

    END SUBROUTINE

    !fd numbering information as they are read (similar to bodies.dat)
    SUBROUTINE KeepIniDofOrder() bind(c, name='RBDY3_KeepIniDofOrder')
       IMPLICIT NONE

       CALL set_keep_ini_dof_order_RBDY3 

    END SUBROUTINE

    SUBROUTINE SetVisible(ibdyty) bind(c, name='RBDY3_SetVisible')
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: ibdyty

       CALL set_visible(ibdyty,.TRUE.)

    END SUBROUTINE

    SUBROUTINE SetInvisible(ibdyty) bind(c, name='RBDY3_SetInvisible')
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: ibdyty

       CALL set_visible(ibdyty,.FALSE.)

    END SUBROUTINE

    SUBROUTINE SetInvisibleSmallObjects(val) bind(c, name='RBDY3_SetInvisibleSmallObjects')
       IMPLICIT NONE
       REAL(c_double), INTENT(in), value :: val

       CALL set_invisible_small_objects(val)

    END SUBROUTINE

    FUNCTION IsVisible(ibdyty) bind(c, name='RBDY3_IsVisible')
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: ibdyty
       INTEGER(c_int) :: IsVisible

       IF ( get_visible(ibdyty) ) THEN
         IsVisible = 1
       ELSE
         IsVisible = 0
       END IF

    END FUNCTION


    !fd je ne sais pas ce que c'est 
    SUBROUTINE UpdateGAMMAvsT() bind(c, name='RBDY3_UpdateGAMMAvsT')
       IMPLICIT NONE

       CALL update_WS_rbdy3

    END SUBROUTINE


    SUBROUTINE CompCoor() bind(C, name='RBDY3_CompCoor')
       !! PURPOSE
       !!  Compute the positions of rigid bodies
       IMPLICIT NONE

       CALL comp_coor_4all_RBDY3

    END SUBROUTINE

    FUNCTION GetBodyDensity(ibdyty) bind(C, name='RBDY3_GetBodyDensity')
       !! PURPOSE
       !!  Get the density of a body
      IMPLICIT NONE
      INTEGER(C_INT), value :: ibdyty
      REAL(C_DOUBLE)        :: GetBodyDensity

      CALL get_density(ibdyty, GetBodyDensity)

    END FUNCTION
      
    SUBROUTINE GetBodyInertia(ivalue1, rvect, ivalue2) bind(c, name='RBDY3_GetBodyInertia')
       !! PURPOSE
       !!  Get inertia of a body
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ivalue1
      INTEGER(c_int)                    :: ivalue2
      type(c_ptr)                       :: rvect
      !
      REAL(kind=8), dimension(:), pointer :: mass, ptr_mass

      ivalue2 = 3
      allocate(mass(ivalue2))

      ptr_mass => get_ptr_mass(ivalue1)
      mass = ptr_mass(4:6)

      rvect = c_loc(mass(1))

    END SUBROUTINE

    subroutine GetAllInertia(rmat,ivalue1,ivalue2) bind(c, name='RBDY3_GetAllInertia')
      implicit none
      integer(c_int) :: ivalue1
      integer(c_int) :: ivalue2
      type(c_ptr)    :: rmat
      !
      real(c_double), dimension(:)  , pointer :: mass
      real(c_double), dimension(:,:), pointer :: inertia
      integer :: i

      ivalue1 = 3
      ivalue2 = get_nb_RBDY3()

      allocate(inertia(ivalue1,ivalue2))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue2
        mass => get_ptr_mass(i)
        inertia(:,i) = mass(4:6)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rmat = c_loc(inertia(1,1))

    end subroutine


    SUBROUTINE CollectBodiesDotOUT() bind(c, name='RBDY3_CollectBodiesDotOUT')
       IMPLICIT NONE
       !  some more CHIC command to be used in preprocessing

       CALL read_in_bodies_RBDY3(2)

    END SUBROUTINE 

    SUBROUTINE AppendToBodiesDotOUT() bind(c, name='RBDY3_AppendToBodiesDotOUT')
       IMPLICIT NONE
       !  some more CHIC command to be used in preprocessing

       CALL write_out_bodies_RBDY3(1)

   END SUBROUTINE

   SUBROUTINE RebuildBodiesDotDAT() bind(c, name='RBDY3_RebuildBodiesDotDAT')
       IMPLICIT NONE
       !  some more CHIC command to be used in preprocessing

       CALL write_out_bodies_RBDY3(2)

   END SUBROUTINE

    
    SUBROUTINE PutBodyVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='RBDY3_PutBodyVector')
      IMPLICIT NONE
      CHARACTER(c_char), DIMENSION(5),INTENT(in) :: cvalue1_c
      INTEGER(c_int),INTENT(in), value           :: ivalue1,ivalue2
      REAL(c_double),INTENT(in) :: rvect(ivalue2)
      !
      CHARACTER(len=5) :: cvalue1
      INTEGER :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      CALL put_vector_RBDY3(cvalue1,ivalue1,rvect,ivalue2)

    END SUBROUTINE

    subroutine PutAllBodyVector(cvalue1_c,rmat,ivalue1,ivalue2) bind(c, name='RBDY3_PutAllBodyVector')
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int), intent(in), value          :: ivalue1,ivalue2
      type(c_ptr)   , intent(in), value          :: rmat
      !
      character(len=5) :: cvalue1
      real(c_double), dimension(:,:), pointer :: matrix
      integer :: i

      cvalue1 = ''
      do i = 1, 5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      ! sanity check
      if( ivalue1 /= get_nb_RBDY3() ) then
        call faterr('RBDY3::PutAllBodyVector', 'Input matrix not size of RBDY3_GetNb')
      end if

      call c_f_pointer(cptr=rmat, fptr=matrix, shape=(/ivalue2,ivalue1/))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue1
        call put_vector_RBDY3(cvalue1,i,matrix(:,i),ivalue2)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

    end subroutine

    SUBROUTINE GetBodyVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='RBDY3_GetBodyVector')
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int),intent(in), value :: ivalue1
      integer(c_int)                   :: ivalue2
      type(c_ptr)                      :: rvect
      !
      real(c_double), dimension(:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      ivalue2 = 6
      allocate(vector(ivalue2))

      CALL get_vector_RBDY3(cvalue1,ivalue1,vector,ivalue2)

      rvect = c_loc(vector(1))

    END SUBROUTINE

    subroutine GetAllBodyVector(cvalue1_c,rmat,ivalue1,ivalue2) bind(c, name='RBDY3_GetAllBodyVector')
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int) :: ivalue1
      integer(c_int) :: ivalue2
      type(c_ptr)    :: rmat
      !
      real(c_double), dimension(:,:), pointer :: matrix
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i = 1, 5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      ivalue1 = 6
      ivalue2 = get_nb_RBDY3()

      allocate(matrix(ivalue1,ivalue2))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue2
        call get_vector_RBDY3(cvalue1,i,matrix(:,i),ivalue1)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rmat = c_loc(matrix(1,1))

    end subroutine

    SUBROUTINE GetPtrBodyVector(cvalue1_c,ivalue1,outvect,ivalue2) bind(c, name='RBDY3_GetPtrBodyVector')
      IMPLICIT NONE
      CHARACTER(c_char), DIMENSION(5),INTENT(in) :: cvalue1_c
      INTEGER(c_int),INTENT(in), value :: ivalue1
      TYPE(c_ptr) :: outvect
      INTEGER(c_int),INTENT(out) :: ivalue2
      !
      REAL(c_double), DIMENSION(:), POINTER :: rvect
      CHARACTER(len=5) :: cvalue1
      INTEGER :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      rvect => get_ptr_vector_RBDY3(cvalue1,ivalue1)
      ivalue2 = SIZE(rvect)
      outvect = c_loc(rvect(1))

    END SUBROUTINE

    SUBROUTINE PutBodyMatrix(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='RBDY3_PutBodyMatrix')
      IMPLICIT NONE
      CHARACTER(c_char), DIMENSION(5),INTENT(in) :: cvalue1_c
      INTEGER(c_int),INTENT(in), value           :: ivalue1,ivalue2,ivalue3
      TYPE(c_ptr), value :: rvect
      !
      REAL(kind=8), DIMENSION(:,:), POINTER :: matrix
      CHARACTER(len=5) :: cvalue1
      INTEGER(kind=4)  :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      CALL c_f_pointer(cptr=rvect, fptr=matrix, shape=(/ivalue2,ivalue3/))

      CALL put_matrix_RBDY3(cvalue1,ivalue1,matrix,max(ivalue2,ivalue3))

    END SUBROUTINE

    SUBROUTINE GetBodyMatrix(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='RBDY3_GetBodyMatrix')
      IMPLICIT NONE
      CHARACTER(c_char), DIMENSION(5),INTENT(in) :: cvalue1_c
      INTEGER(c_int),INTENT(in), value :: ivalue1
      TYPE(c_ptr)        :: rvect
      INTEGER(c_int) :: ivalue2, ivalue3
      !
      REAL(kind=8), DIMENSION(:,:), POINTER :: matrix
      CHARACTER(len=5) :: cvalue1
      INTEGER(kind=4)  :: i

      cvalue1 = ''
      DO i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      END DO

      matrix => null()

      ! c'est moche mais pour ne pas changer le comportement de get_matrix
      ! je n'ai d'autres choix que de mettre en dur la taille de la matrice
      allocate(matrix(3,3))
      ivalue2 = 3
      ivalue3 = 3
      CALL get_matrix_RBDY3(cvalue1,ivalue1,matrix,3)

      rvect = c_loc(matrix(1,1))

    END SUBROUTINE

    subroutine GetAllRData(ptr,dim1,dim2) bind(c, name='RBDY3_GetAllRData')
      implicit none
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2
      !
      integer :: nb_fields, nb_bodies
      real(kind=8), dimension(:,:), pointer :: rdata

      rdata => null()

      nb_bodies = get_nb_RBDY3()
      !           coor, vlocy, spin, fext, reac, frame
      nb_fields = 3  +  3   +  3  +  6   + 6  +  9

      allocate(rdata(nb_fields,nb_bodies))
      call get_all_rdata_RBDY3(rdata, nb_bodies, nb_fields)

      ptr  = c_loc(rdata(1,1))
      dim1 = nb_fields
      dim2 = nb_bodies

    end subroutine

    FUNCTION getNbRBDY3() bind(C, name='RBDY3_GetNbRBDY3')
      IMPLICIT NONE
      INTEGER(C_INT) :: getNbRBDY3

      getNBRBDY3 = get_nb_RBDY3()

    END FUNCTION

    FUNCTION getMass(ibdyty) bind(C, name='RBDY3_GetMass')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(in), value :: ibdyty
      REAL(C_DOUBLE) :: getMass

      getMass = get_mass(ibdyty)

    END FUNCTION

    subroutine GetAllMass(rvect,ivalue) bind(c, name='RBDY3_GetAllMass')
      implicit none
      type(c_ptr)    :: rvect
      integer(c_int) :: ivalue
      !
      real(c_double), dimension(:), pointer :: vector
      integer :: ibdyty

      ivalue = get_nb_RBDY3()
      allocate(vector(ivalue))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(ibdyty)
      !$OMP DO SCHEDULE(RUNTIME)
      do ibdyty = 1, ivalue
        vector(ibdyty) = get_mass(ibdyty)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rvect = c_loc(vector(1))

    end subroutine

!!! vt for peligriff ----------------------------------------

    SUBROUTINE GetPtrMass(ibdyty, mass_ptr) bind(c, name='RBDY3_GetPtrMass')
      INTEGER(c_int), INTENT(in), value :: ibdyty
      TYPE(c_ptr)                       :: mass_ptr

      REAL(c_double), DIMENSION(:), POINTER :: ptr
      ptr => get_ptr_mass(ibdyty)

      mass_ptr = c_loc(ptr(1))

    END SUBROUTINE

    SUBROUTINE GetV(ibdyty, vel) bind(C, name='RBDY3_GetVelocity')
      IMPLICIT NONE
      INTEGER :: i
      INTEGER(C_INT), INTENT(in), value :: ibdyty
      REAL(C_DOUBLE), DIMENSION(6)      :: vel
      REAL(kind=8) :: eps

      eps=EPSILON(1.d0)

      vel = get_V(ibdyty)
      
      !write(*,*) vel(1),vel(2),vel(3),vel(4),vel(5),vel(6)
      DO i=1,6 
        IF (ABS(vel(i))<eps) vel(i)=eps
      ENDDO

    END SUBROUTINE

    !fd same than other one 
    
    ! ! Give the global inertia matrix
    ! SUBROUTINE CompGlobInerRBDY3(ibdyty,GlobInert) bind(c, name='RBDY3_CompGlobInerRBDY3')
    !   IMPLICIT NONE 
      
    !   INTEGER(c_int), INTENT(in), value :: ibdyty
    !   REAL(C_DOUBLE), DIMENSION(6), INTENT(inout)    :: GlobInert
    !   !
    !   REAL(c_double), DIMENSION(3,3):: GlobalInertia

    !   CALL comp_glob_inert_RBDY3(ibdyty,GlobalInertia)
      
    !   !Voigt Notation for Vector GlobInert (I11, I22, I33, I32, I13, I12)
    !   GlobInert(1)=GlobalInertia(1,1)
    !   GlobInert(2)=GlobalInertia(2,2)
    !   GlobInert(3)=GlobalInertia(3,3)
    !   GlobInert(4)=GlobalInertia(2,3)
    !   GlobInert(5)=GlobalInertia(1,3)
    !   GlobInert(6)=GlobalInertia(1,2)

    ! END SUBROUTINE CompGlobInerRBDY3


    ! Give the global inertia matrix
    subroutine GetGlobInerRBDY3(ibdyty,mat,dim1,dim2) bind(c, name='RBDY3_GetGlobInertia')
      implicit none 
      integer(c_int), intent(in), value :: ibdyty
      type(c_ptr)    :: mat
      integer(c_int) :: dim1, dim2
      !
      real(c_double), dimension(:,:), pointer:: GlobalInertia

      allocate(GlobalInertia(3,3))
      dim1 = 3
      dim2 = 3

      call comp_glob_inert_RBDY3(ibdyty,GlobalInertia)
      
      mat = c_loc(GlobalInertia(1,1))

    END SUBROUTINE GetGlobInerRBDY3

    
!!!------------------------------------------------------------------------

    SUBROUTINE TriaxialLoading(num_down,num_right,num_up,num_left,num_front,num_rear,nb_loads,loads_c,length) &
                bind(c, name='RBDY3_TriaxialLoading')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: num_down,num_right,num_up,num_left,num_front,num_rear
      INTEGER(c_int), INTENT(in), value :: nb_loads,length
      TYPE(c_ptr), INTENT(in), value    :: loads_c
      !
      REAL(kind=8), DIMENSION(:,:), POINTER :: loads

      CALL c_f_pointer(cptr=loads_c, fptr=loads, shape=(/2,nb_loads/))
      
      CALL triaxial_loading(num_down,num_right,num_up,num_left,num_front,num_rear,nb_loads,loads)
        
    END SUBROUTINE

!!!------------------------------------------------------------------------

    FUNCTION getNbContactor(ibdyty) bind(C, name='RBDY3_GetNbContactor')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty
      INTEGER(C_INT) :: getNbContactor

      getNbContactor = get_nb_tacty(ibdyty)

    END FUNCTION

!!!------------------------------------------------------------------------

    SUBROUTINE GetContactorType(ibdyty, itacty, c5) bind(C, name='RBDY3_GetContactorType')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty, itacty
      TYPE(c_ptr) :: c5
      !
      INTEGER(kind=4)  :: i
      CHARACTER(len=5), POINTER :: contactor_type

      ALLOCATE(contactor_type)
      contactor_type = get_tacID(ibdyty,itacty)

      c5 = c_loc(contactor_type(1:1))

    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE GetContactorColor(ibdyty, itacty, c5) bind(C, name='RBDY3_GetContactorColor')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty, itacty
      TYPE(c_ptr) :: c5
      !
      INTEGER(kind=4)  :: i
      CHARACTER(len=5), POINTER :: contactor_color

      ALLOCATE(contactor_color)
      contactor_color = get_color(ibdyty,itacty)

      c5 = c_loc(contactor_color(1:1))

    END SUBROUTINE GetContactorColor

!!!------------------------------------------------------------------------
    
    SUBROUTINE SetContactorColor(ibdyty, itacty, cvalue1_c) bind(C, name='RBDY3_SetContactorColor')
      IMPLICIT NONE
      CHARACTER(c_char), DIMENSION(5),INTENT(in) :: cvalue1_c      
      INTEGER(c_int), INTENT(in), value :: ibdyty, itacty
      TYPE(c_ptr) :: c5
      !
      INTEGER(kind=4)  :: i
      CHARACTER(len=5) :: color

      color = ''
      DO i=1,5
         color = color(1:i-1) // cvalue1_c(i)
      END DO

      call set_color_RBDY3(ibdyty,itacty,color)
  
    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE GetBehavior(ibdyty, c5) bind(C, name='RBDY3_GetBehavior')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty
      TYPE(c_ptr) :: c5
      !
      INTEGER(kind=4)  :: i
      CHARACTER(len=5), POINTER :: behavior

      ALLOCATE(behavior)
      behavior = get_behav(ibdyty)

      c5 = c_loc(behavior(1:1))

    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE getDrvVlocy(ibdyty, i4_vector, i4_size, r8_vector, r8_size) bind(c, name='RBDY3_getDrvVlocy')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty
      INTEGER(c_int), INTENT(out) :: i4_size, r8_size
      TYPE(c_ptr) :: i4_vector, r8_vector
      !
      INTEGER(kind=4), DIMENSION(:), POINTER :: i4_target
      REAL(kind=8)   , DIMENSION(:), POINTER :: r8_target

      i4_target => NULL()
      r8_target => NULL()

      CALL get_drv_vlocy_RBDY3(ibdyty, i4_target, r8_target)

      IF( ASSOCIATED(i4_target) ) THEN
        i4_size = SIZE(i4_target)
        r8_size = SIZE(r8_target)

        i4_vector = c_loc(i4_target(1))
        r8_vector = c_loc(r8_target(1))
      ELSE
        i4_size = 0
        r8_size = 0

        i4_vector = c_null_ptr
        r8_vector = c_null_ptr
      END IF
      
    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE compDrvVlocy(ibdyty, vector_in, length) bind(c, name='RBDY3_computeDrvVlocy')
      IMPLICIT NONE
      INTEGER(c_int), INTENT(in), value :: ibdyty
      INTEGER(c_int), INTENT(in), value :: length
      TYPE(c_ptr), value :: vector_in
      !
      REAL(kind=8), DIMENSION(:), POINTER :: values

      CALL c_f_pointer(cptr=vector_in, fptr=values, shape=(/length/))
      CALL comp_drv_vlocy_RBDY3(ibdyty, values)
    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE WriteOutOneBody(ibdyty,new_ibdyty) bind(c, name='RBDY3_WriteOutOneBody')
       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN), VALUE :: ibdyty,new_ibdyty

       CALL write_out_one_RBDY3(ibdyty,new_ibdyty)

    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE WriteOutDofOneBody(ibdyty,new_ibdyty) bind(c, name='RBDY3_WriteOutDofOneBody')

       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN), VALUE :: ibdyty,new_ibdyty

       CALL write_out_dof_one_RBDY3(ibdyty,new_ibdyty)

    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE LoadThreadNetwork() bind(c, name='RBDY3_LoadThreadNetwork')

      IMPLICIT NONE
      
      CALL load_thread_network_RBDY3()

    END SUBROUTINE LoadThreadNetwork

!!!------------------------------------------------------------------------

    SUBROUTINE SetVisibleVlocyDrivenDof(ibdyty,iccdof) bind(c, name='RBDY3_SetVisibleVlocyDrivenDof')
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: ibdyty,iccdof

       CALL switch_vlocy_driven_dof(ibdyty,iccdof,1)

    END SUBROUTINE

!!!------------------------------------------------------------------------

    SUBROUTINE SetInvisibleVlocyDrivenDof(ibdyty,iccdof) bind(c, name='RBDY3_SetInvisibleVlocyDrivenDof')
       IMPLICIT NONE
       INTEGER(c_int), INTENT(in), value :: ibdyty,iccdof

       CALL switch_vlocy_driven_dof(ibdyty,iccdof,0)

    END SUBROUTINE


    SUBROUTINE PartialDamping(entier,reel) bind(c, name='RBDY3_PartialDamping')
       !! PURPOSE
       !!  reduce body velocity greater than Vmax to the Vmax value
       implicit none
       integer(c_int), intent(in), value :: entier
       real(c_double), intent(in), value :: reel

       call partial_damping_RBDY3(entier,reel)

    END SUBROUTINE

    subroutine getVolume(ibdyty, volume) bind(c, name='RBDY3_GetVolume')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      real(c_double) :: volume
      !
      volume = get_volume(ibdyty)

    end subroutine

    subroutine GetAllVolume(rvec,ivalue) bind(c, name='RBDY3_GetAllVolume')
      implicit none
      integer(c_int) :: ivalue
      type(c_ptr)    :: rvec
      !
      real(c_double), dimension(:), pointer :: volume
      integer :: i

      ivalue = get_nb_RBDY3()

      allocate(volume(ivalue))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue
        volume(i) = get_volume(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rvec = c_loc(volume(1))

    end subroutine

    !pta
    SUBROUTINE RenumVisibleRBDY3() bind(c, name='RBDY3_RenumVisibleBodies')
       IMPLICIT NONE
       CALL renum_visible_RBDY3

    END SUBROUTINE

   function GetBulkBehavNumberRBDY3(ibdyty) bind(c, name='RBDY3_GetBulkBehavNumber')
     implicit none
     integer(c_int), intent(in), value :: ibdyty ! numéro du RBDY3 considere
     integer(c_int) ::  GetBulkBehavNumberRBDY3

     !mr: pas tres propre car on considere iblmty=1
     GetBulkBehavNumberRBDY3 = get_bulk_behav_number_RBDY3(ibdyty,1) 

   end function GetBulkBehavNumberRBDY3

    subroutine CleanMemory() bind(c, name='RBDY3_CleanMemory')
      implicit none

      call clean_memory_RBDY3

    end subroutine

    function GetThermalValueRBDY3(ibdyty,itacty) bind(c, name='RBDY3_GetThermalValue')
     implicit none
     integer(c_int),intent(in), value :: ibdyty,itacty
     real(c_double) :: GetThermalValueRBDY3

     GetThermalValueRBDY3 = get_thermal_value(ibdyty,itacty)

    end function GetThermalValueRBDY3

   !----------------------------------------------------

    subroutine SetEquilibriumNorm(cvalue1_c,rvalue1) bind(c, name='RBDY3_SetEquilibriumNorm')
       implicit none
       character(C_CHAR), dimension(5)   :: cvalue1_c
       real(c_double), intent(in), value :: rvalue1
       !! PURPOSE
       !!  Initialisation of data for the  equilibrium state
       !!  check.
       !!  You must precise the type of check test [checktype]
       !!  - Qvlcy : quadratic norm of velocy
       !!  - Mvlcy : maximum   norm of velocy
       character(len=5) :: cvalue1
       integer :: i

       cvalue1 = ''
       do i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
       end do

       call set_data_equilibrium_RBDY3(cvalue1,rvalue1)

    end subroutine SetEquilibriumNorm

    function CheckEquilibriumState() bind(c, name='RBDY3_CheckEquilibriumState')
       implicit none
       logical(c_bool) ::  CheckEquilibriumState
       logical         ::  check_convergence

       check_convergence = .false.
       call check_equilibrium_state_RBDY3(check_convergence)
       CheckEquilibriumState = check_convergence

    end function CheckEquilibriumState
    
    function GetDofStatus(ivalue1) bind(c, name='RBDY3_GetDofStatus')
      IMPLICIT NONE
      integer(c_int),value  :: ivalue1
      integer(c_int)        :: GetDofStatus
      !

      CALL get_dofstatus_RBDY3(ivalue1,GetDofStatus)

    END function
      
END MODULE wrap_RBDY3
