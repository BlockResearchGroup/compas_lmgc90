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
module wrap_RBDY2

  use ISO_C_BINDING

  use utilities 

  use RBDY2, only: &
       increment_RBDY2, &
       set_vlocy_drvdof_RBDY2, &
       update_WSvsT_RBDY2, &
       update_WSvsTime_RBDY2, &
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
       read_in_bodies_RBDY2, &
       update_existing_entities_RBDY2, &
       read_in_dof_RBDY2, &
       read_in_driven_dof_RBDY2, &
       read_behaviours_RBDY2, &
       init_mp_behaviours_RBDY2, &
       write_out_bodies_RBDY2, &
       write_out_cleared_bodies_RBDY2, &
       write_xxx_dof_RBDY2, &
       write_xxx_Rnod_RBDY2, &
       write_out_driven_dof_RBDY2, &
       comp_mass_RBDY2, &
       set_periodic_data_RBDY2, &
       check_periodic_RBDY2 => check_periodic, &
       resize_RBDY2, &
       nullify_X_dof_RBDY2, &
       nullify_V_dof_RBDY2, &
       init_source_point_RBDY2, &
       set_init_boundary_RBDY2, &
       set_data_equilibrium_RBDY2, &
       add_dof2bodies_RBDY2, &
       update_dilatation, &
       init_free_boundary_RBDY2, &
       get_nb_RBDY2, &
       get_nb_tacty, &
       get_color   , &
       set_color_RBDY2, &
       get_write_Rnod_RBDY2, &
       get_write_DOF_RBDY2, &
       get_coor, & 
       put_invmass_RBDY2,put_precon_W_RBDY2, &
       put_vector_RBDY2,get_vector_RBDY2, &
       get_ptr_vector_RBDY2, &
       get_area, &
       check_partial_equilibrium_state_RBDY2, &
       is_periodic_RBDY2, &
       put_coor, &
       put_coor_begin, &
       get_coor_begin, &
       set_invisible, &
       get_visible, &
       get_mass, &
       get_ptr_mass, & ! <- rm: fonctions pour binding dans siconos
       get_ptr_fint, &
       get_ptr_fext, &
       get_ptr_vbeg, &
       get_tacID, &
       comp_coor_4all_RBDY2, & ! <- rm: fonctions pour binding dans peligriff
       get_density, &
       get_V, &
       Biaxial_def_walls, biaxial_loading, &
       get_drv_vlocy_RBDY2, comp_drv_vlocy_RBDY2, &
       set_visibility, &
       compute_partial_equilibrium_state_RBDY2, &
       get_bulk_behav_ID_RBDY2, &
       set_surface_sectors, &
       get_stress_RBDY2, &
       set_skip_invisible_RBDY2, &
       modify_body, &
       initialize_stress, &
       get_bulk_behav_number_RBDY2, &
       clean_memory_RBDY2, &
       get_thermal_value, &
       get_electric_potentiel, &
       get_electric_current, &
       initialize_WS_sectors, &   !vhn
       get_periode, &
       switch_vlocy_driven_dof, &
       get_betai, &
       get_average_WS

  public

contains

    subroutine PutBodyInvMass(ivalue1,rvect,ivalue4) bind(c, name='RBDY2_PutBodyInvMass')
      implicit none
      integer(c_int),intent(in), value :: ivalue1,ivalue4
      real(c_double),intent(in)        :: rvect(ivalue4)

       !! PURPOSE
       !! Set the inv_mass of a given body
       call put_invmass_RBDY2(ivalue1,rvect,ivalue4)

    end subroutine

    subroutine PutBodyPreconW(ivalue1,ivalue3,rvect,ivalue4) bind(c, name='RBDY2_PutBodyPreconW')
      implicit none
      integer(c_int),intent(in), value :: ivalue1,ivalue3,ivalue4
      real(c_double),intent(in):: rvect(ivalue4)
      real(kind=8) :: InvMass
       !! PURPOSE
       !!
       InvMass = rvect(ivalue3)
       call put_precon_W_RBDY2(ivalue1,ivalue3,InvMass)

    end subroutine

    subroutine PutBodyVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='RBDY2_PutBodyVector')
      !! PURPOSE
      !!
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int),intent(in), value           :: ivalue1,ivalue2

      real(c_double),intent(in) :: rvect(ivalue2)

      character(len=128) :: cout
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      call put_vector_RBDY2(cvalue1,ivalue1,rvect,ivalue2)

    end subroutine

    subroutine PutAllBodyVector(cvalue1_c,rmat,ivalue1,ivalue2) bind(c, name='RBDY2_PutAllBodyVector')
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
      if( ivalue1 /= get_nb_RBDY2() ) then
        call faterr('RBDY2::PutAllBodyVector', 'Input matrix not size of RBDY2_GetNb')
      end if

      call c_f_pointer(cptr=rmat, fptr=matrix, shape=(/ivalue2,ivalue1/))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue1
        call put_vector_RBDY2(cvalue1,i,matrix(:,i),ivalue2)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

    end subroutine

    subroutine GetBodyVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='RBDY2_GetBodyVector')
      !! PURPOSE
      !!
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int),intent(in), value :: ivalue1
      integer(c_int)                   :: ivalue2
      type(c_ptr)                      :: rvect
      !
      real(c_double), dimension(:), pointer :: vector
      character(len=128) :: cout
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      ivalue2 = 3
      allocate(vector(ivalue2))
      call get_vector_RBDY2(cvalue1,ivalue1,vector,ivalue2)

      rvect = c_loc(vector(1))

    end subroutine

    subroutine GetAllBodyVector(cvalue1_c,rmat,ivalue1,ivalue2) bind(c, name='RBDY2_GetAllBodyVector')
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

      ivalue1 = 3
      ivalue2 = get_nb_RBDY2()

      allocate(matrix(ivalue1,ivalue2))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue2
        call get_vector_RBDY2(cvalue1,i,matrix(:,i),ivalue1)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rmat = c_loc(matrix(1,1))

    end subroutine

    subroutine GetPtrBodyVector(cvalue1_c,ivalue1,outvect,ivalue2) bind(c, name='RBDY2_GetPtrBodyVector')
      !! PURPOSE
      !!
      implicit none
      character(c_char), dimension(5),intent(in) :: cvalue1_c
      integer(c_int),intent(in), value :: ivalue1
      type(c_ptr) :: outvect
      integer(c_int),intent(out) :: ivalue2
      !
      real(c_double), dimension(:), pointer :: rvect
      character(len=128) :: cout
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      rvect => get_ptr_vector_RBDY2(cvalue1,ivalue1)
      ivalue2 = size(rvect)
      outvect = c_loc(rvect(1))

    end subroutine

    function GetBodyInertia(ivalue1) bind(c, name='RBDY2_GetBodyInertia')
      !! PURPOSE
      !!
      implicit none
      integer(c_int), intent(in), value :: ivalue1
      real(c_double)                    :: GetBodyInertia
      !
      real(c_double), dimension(:), pointer :: mass

      mass => get_ptr_mass(ivalue1)

      GetBodyInertia = mass(3)

    end function

    subroutine GetAllInertia(rvec,ivalue) bind(c, name='RBDY2_GetAllInertia')
      implicit none
      integer(c_int) :: ivalue
      type(c_ptr)    :: rvec
      !
      real(c_double), dimension(:), pointer :: mass
      real(c_double), dimension(:), pointer :: inertia
      integer :: i

      ivalue = get_nb_RBDY2()

      allocate(inertia(ivalue))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, ivalue
        mass => get_ptr_mass(i)
        inertia(i) = mass(3)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rvec = c_loc(inertia(1))

    end subroutine

!!!---------------------------------------------------------------------

    subroutine IncrementStep() bind(c, name='RBDY2_IncrementStep')
       implicit none

       call increment_RBDY2

    end subroutine

    subroutine SetVlocyDrivenDof(ibdyty, idrvdof, value) bind(c, name='RBDY2_SetVlocyDrivenDof')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: idrvdof
      real(c_double), intent(in), value :: value

      call set_vlocy_drvdof_RBDY2(ibdyty, idrvdof, value)

    end subroutine

!----------------------------------------------------

    subroutine ComputeDof() bind(c, name='RBDY2_ComputeDof')
       use timer
       implicit none
       integer(kind=4), save :: timer_id = 0

       if( get_nb_RBDY2() < 1 ) return
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] comp dof    ')
       call start_itimer(timer_id)

       call comp_dof_RBDY2

       ! am:
       ! si on impose une condition periodique sur l'axe (0x)
       if (is_periodic_RBDY2()) then

          ! on corrige les positions des grains qui la viole
          call check_periodic_RBDY2

       end if

       call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    subroutine UpdateDof() bind(c, name='RBDY2_UpdateDof')
       use timer
       implicit none
       integer(kind=4), save :: timer_id = 0

       if( get_nb_RBDY2() < 1 ) return
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] update dof  ')
       call start_itimer(timer_id)

       call update_dof_RBDY2

       call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    subroutine ComputeFreeVelocity() bind(c, name='RBDY2_ComputeFreeVelocity')
       use timer
       implicit none
       integer(kind=4), save :: timer_id = 0

       if( get_nb_RBDY2() < 1 ) return
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] comp Free V ')
       call start_itimer(timer_id)

       call comp_free_vlocy_RBDY2

       call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    subroutine ComputeFext() bind(c, name='RBDY2_ComputeFext')
       use timer
       implicit none
       integer(kind=4), save :: timer_id = 0

       if( get_nb_RBDY2() < 1 ) return
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] comp Fext   ')
       call start_itimer(timer_id)

       call comp_Fext_RBDY2

       call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    subroutine ComputeBulk() bind(c, name='RBDY2_ComputeBulk')
       use timer
       implicit none
       integer(kind=4), save :: timer_id = 0

       if( get_nb_RBDY2() < 1 ) return
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] comp bulk   ')
       call start_itimer(timer_id)

       call comp_Fint_RBDY2

       call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    function CheckEquilibriumState() bind(c, name='RBDY2_CheckEquilibriumState')
       implicit none
       logical(c_bool) ::  CheckEquilibriumState
       logical         ::  check_convergence

       check_convergence = .false.
       call check_equilibrium_state_RBDY2(check_convergence)
       CheckEquilibriumState = check_convergence

    end function

!----------------------------------------------------

    subroutine GhostToInvisible() bind(c, name='RBDY2_GhostToInvisible')
       implicit none

       call ghost2invisible_RBDY2 

    end subroutine

!----------------------------------------------------

    subroutine CheckSourcePoint() bind(c, name='RBDY2_CheckSourcePoint')
       implicit none

       call check_source_point_RBDY2

    end subroutine

!----------------------------------------------------

    subroutine FatalDamping(list_ids, length) bind(c, name='RBDY2_FatalDamping')
      use timer
      implicit none
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_RBDY2() < 1 ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] fatal damp  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_RBDY2()
          call fatal_damping_RBDY2(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call fatal_damping_RBDY2(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    subroutine PartialDamping(entier,reel) bind(c, name='RBDY2_PartialDamping')
       !! PURPOSE
       !!  reduce body velocity greater than Vmax to the Vmax value
       implicit none
       integer(c_int), intent(in), value :: entier
       real(c_double), intent(in), value :: reel

       call partial_damping_RBDY2(entier,reel)

    end subroutine


!!! WRITING COMMAND ------------------------------------------------

    subroutine WriteLastDof() bind(c, name='RBDY2_WriteLastDof')
       !! PURPOSE
       !!  write ascii DOF.LAST file
       implicit none
       integer(c_int) :: ifrom,ito

          ito = get_nb_RBDY2()
          ifrom = 1

          call write_xxx_dof_RBDY2(2,ifrom,ito)          

    end subroutine

!----------------------------------------------------

    subroutine WriteOutDof(ivalue1,ivalue2) bind(c, name='RBDY2_WriteOutDof')
       implicit none
       integer(c_int), intent(in), value :: ivalue1, ivalue2
       integer :: ifrom,ito,nb_RBDY2
       logical :: write_DOF
       !! PURPOSE
       !!   write ascii DOF.OUT file. Can be activate only each N step

       write_DOF = get_write_DOF_RBDY2()
       if (write_DOF) then
         nb_RBDY2 = get_nb_RBDY2()
         
         ifrom = 1
         ito = nb_RBDY2

         ifrom = max0(ivalue1,1)
         if (ivalue2 > ivalue1) then
            ito   = min0(ivalue2,nb_RBDY2)
         end if

         call write_xxx_dof_RBDY2(1,ifrom,ito)

       end if

    end subroutine
!----------------------------------------------------

    subroutine DisplayOutDof() bind(c, name='RBDY2_DisplayOutDof')
       implicit none
       integer :: ifrom,ito
       !! PURPOSE
       !!  display body degrees of freedom

       ifrom = 1
       ito   = get_nb_RBDY2()

       call write_xxx_dof_RBDY2(6,ifrom,ito)
 
    end subroutine

!----------------------------------------------------

    subroutine WriteLastRnod() bind(c, name='RBDY2_WriteLastRnod')
       implicit none

       integer :: ifrom,ito,nb_RBDY2
       !! PURPOSE
       !!  write ascii Rnod.LAST file

        nb_RBDY2 = get_nb_RBDY2()
        ifrom = 1  
        ito   = nb_RBDY2
        call write_xxx_Rnod_RBDY2(2,ifrom,ito)

    end subroutine

!----------------------------------------------------

    subroutine WriteOutRnod() bind(c, name='RBDY2_WriteOutRnod')
       implicit none
       integer :: ifrom,ito
       logical :: write_Rnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.

       write_Rnod = get_write_Rnod_RBDY2()
       if (write_Rnod) then
         ifrom = 1
         ito   = get_nb_RBDY2()

         call write_xxx_Rnod_RBDY2(1,ifrom,ito)

       end if

    end subroutine

!----------------------------------------------------

    subroutine DisplayOutRnod() bind(c, name='RBDY2_DisplayOutRnod')
       implicit none
       integer :: ifrom,ito,nb_RBDY2
       !! PURPOSE
       !!  display body forces.

       ifrom = 1
       ito   = get_nb_RBDY2()

       call write_xxx_Rnod_RBDY2(6,ifrom,ito)

    end subroutine

!----------------------------------------------------

    subroutine WriteBodies() bind(c, name='RBDY2_WriteBodies')
       implicit none
       !! PURPOSE
       !!  write BODIES.OUT file

       call write_out_bodies_RBDY2

    end subroutine

!----------------------------------------------------

    subroutine ClearedWriteBodies() bind(c, name='RBDY2_ClearedWriteBodies')
       implicit none
       !! PURPOSE
       !!  ...

       call write_out_cleared_bodies_RBDY2

    end subroutine

    subroutine WriteDrivenDof() bind(c, name='RBDY2_WriteDrivenDof')
       implicit none
       !! PURPOSE
       !!  write DRV_DOF.OUT file

       call write_out_driven_dof_RBDY2

    end subroutine

!!! READING COMMAND ----------------------------------------------------

    subroutine ReadBodies() bind(c, name='RBDY2_ReadBodies')
       implicit none
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable in RBDY2
       !!  Adds the number of found bodies to entity

       call read_in_bodies_RBDY2
       call update_existing_entities_RBDY2

    end subroutine

!----------------------------------------------------

    subroutine ReadIniDof(step) bind(c, name='RBDY2_ReadIniDof')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_dof_RBDY2(step)

    end subroutine

!----------------------------------------------------

    subroutine ReadDrivenDof() bind(c, name='RBDY2_ReadDrivenDof')
       implicit none
       !! PURPOSE
       !!  read DRV_DOF.DAT file

       call read_in_driven_dof_RBDY2

    end subroutine

!----------------------------------------------------

    subroutine LoadBehaviours() bind(c, name='RBDY2_LoadBehaviours')
       implicit none
       !! PURPOSE
       !!  read BULK_BEHAV.DAT file

       call read_behaviours_RBDY2

    end subroutine

    !>  load extra physical behaviour read in BULK_BEHAV.DAT file.
    !>  Must be used with THERMO_RIGID ELECTRO_RIGID and THERMO_ELECTRO_RIGID behaviours
    subroutine MP_LoadBehaviours(disper,cvalue1_c) bind(c, name='RBDY2_MP_LoadBehaviours')
       implicit none
       character(C_CHAR), dimension(5)   :: cvalue1_c
       real(c_double), intent(in), value :: disper
       character(len=5) :: cvalue1
       integer :: i

       cvalue1 = ''
       do i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
       end do
       !cvalue1: therm, elect, sener
       call init_mp_behaviours_RBDY2(disper,cvalue1)

    end subroutine

    subroutine UpdateWSvsT() bind(c, name='RBDY2_UpdateWSvsT')
      implicit none

      call update_WSvsT_RBDY2

    end subroutine UpdateWSvsT

    subroutine UpdateWSvsTime() bind(c, name='RBDY2_UpdateWSvsTime')
      implicit none

      call update_WSvsTime_RBDY2

    end subroutine UpdateWSvsTime

    subroutine ComputeMass() bind(c, name='RBDY2_ComputeMass')
       use timer
       implicit none
       integer(kind=4), save :: timer_id = 0

       if( get_nb_RBDY2() < 1 ) return
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[RBDY2] comp mass   ')
       call start_itimer(timer_id)

       call comp_mass_RBDY2

       call stop_itimer(timer_id)

    end subroutine

!----------------------------------------------------

    subroutine SetPeriodicCondition(periode) bind(c, name='RBDY2_SetPeriodicCondition')
       implicit none
       real(c_double), intent(in), value :: periode
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]

       call set_periodic_data_RBDY2(periode)

    end subroutine

!----------------------------------------------------

    subroutine ResizeBodies(homo) bind(c, name='RBDY2_ResizeBodies')
       implicit none
       real(c_double), intent(in), value :: homo
       !! PURPOSE
       !!  resize body radius of a  [homo] factor

       call resize_RBDY2(homo)

    end subroutine

!----------------------------------------------------

    subroutine NullifyDisplacements() bind(c, name='RBDY2_NullifyDisplacements')
       implicit none
       !! PURPOSE
       !!  nullify X degree of freedom

       call nullify_X_dof_RBDY2

    end subroutine

!----------------------------------------------------

    subroutine NullifyVelocities() bind(c, name='RBDY2_NullifyVelocities')
       implicit none
       !! PURPOSE
       !!  nullify V degree of freedom 

       call nullify_V_dof_RBDY2

    end subroutine

!----------------------------------------------------

    subroutine SetSourcePoint(ivalue1,rvalue1,rvalue2,rvalue3) bind(c, name='RBDY2_SetSourcePoint')
       implicit none
       integer(c_int), intent(in), value :: ivalue1
       real(c_double), intent(in), value :: rvalue1,rvalue2,rvalue3 
       !! PURPOSE
       !!  create an assembly by source point deposit

       call init_source_point_RBDY2(ivalue1,rvalue1,rvalue2,rvalue3)

    end subroutine
    
!----------------------------------------------------

    subroutine SetYminBoundary(rvalue1) bind(c, name='RBDY2_SetYminBoundary')
       implicit none
       real(c_double), intent(in), value :: rvalue1
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS


       call set_init_boundary_RBDY2(1,rvalue1)

    end subroutine

!----------------------------------------------------

    subroutine SetYmaxBoundary(rvalue1) bind(c, name='RBDY2_SetYmaxBoundary')
       implicit none
       real(c_double), intent(in), value :: rvalue1
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS

       call set_init_boundary_RBDY2(2,rvalue1)

    end subroutine

!----------------------------------------------------

    subroutine SetXminBoundary(rvalue1) bind(c, name='RBDY2_SetXminBoundary')
       implicit none
       real(c_double), intent(in), value :: rvalue1
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS

       call set_init_boundary_RBDY2(3,rvalue1)

    end subroutine

!----------------------------------------------------

    subroutine SetXmaxBoundary(rvalue1) bind(c, name='RBDY2_SetXmaxBoundary')
       implicit none
       real(c_double), intent(in), value :: rvalue1
       !! PURPOSE
       !!  define the boundary of command CHECK_OUT_OF_BOUNDS

       call set_init_boundary_RBDY2(4,rvalue1)

    end subroutine

!----------------------------------------------------

    subroutine SetEquilibriumNorm(cvalue1_c,rvalue1) bind(c, name='RBDY2_SetEquilibriumNorm')
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

       call set_data_equilibrium_RBDY2(cvalue1,rvalue1)

    end subroutine

!----------------------------------------------------

    subroutine AddDof2InBodies() bind(c, name='RBDY2_AddDof2InBodies')
       implicit none
       !! PURPOSE
       !!  create a new BODIES.OUT file as a combination
       !!  of the last one and of the last DOF.OUT file

       call add_dof2bodies_RBDY2

    end subroutine

    subroutine InitFreeBoundary(xmin,xmax,radius) bind(c, name='RBDY2_InitFreeBoundary')
       implicit none
       real(c_double), intent(in), value :: xmin, xmax, radius

       call init_free_boundary_RBDY2(xmin,xmax,radius)

    end subroutine

    subroutine MembraneBiaxialLoading(num_down,num_up,thickness,sigma) bind(c, name='RBDY2_MembraneBiaxialLoading')
       implicit none
       real(c_double), intent(in), value :: thickness,sigma
       integer(c_int), intent(in), value :: num_down,num_up   
 
       call Biaxial_def_walls(num_up,num_down,thickness,sigma,'RIGHT','LEFTx')
 
    end subroutine

    subroutine BiaxialLoading(num_down,f_down,num_right,f_right,num_up,f_up,num_left,f_left) bind(c, name='RBDY2_BiaxialLoading')
       implicit none
       integer(c_int), intent(in), value :: num_down,num_right,num_up,num_left
       real(c_double), intent(in), value :: f_down,f_right,f_up,f_left
  
       call biaxial_loading(num_down,f_down,num_right,f_right,num_up,f_up,num_left,f_left)
        
    end subroutine

!!$ TODO
!!$    subroutine SetThermalStrain(ibdy,alpha,dT)
!!$
!!$       ! petite routine calculer pour dilater 
!!$       ! lineaire sur un maine rectangulaire 
!!$       ! l espacement des noeuds est suppose regulier
!!$
!!$       implicit none
!!$       integer      :: ibdy
!!$       real(kind=8) :: alpha,dT(4),dTm
!!$       real(kind=8) :: inc_dilat(4)
!!$
!!$       dTm = 0.25*(dT(1)+dT(2)+dT(3)+dT(4))
!!$       
!!$       inc_dilat = alpha * dT
!!$
!!$       call set_dilatation_increment(ibdy,dTm,inc_dilat)
!!$
!!$    end subroutine

    subroutine UpdateThermalStrain() bind(c, name='RBDY2_UpdateThermalStrain')
       implicit none

       call update_dilatation

    end subroutine

    ! fonction qui renvoie le nombre de RBDY2
    function GetNbRBDY2() bind(c, name='RBDY2_GetNbRBDY2')
       implicit none
       ! valeur de retour
       integer(c_int) :: GetNbRBDY2 ! nombre de rigides 2D

       GetNbRBDY2 = get_nb_RBDY2()

    end function GetNbRBDY2


    ! fonction qui recupere l'aire (volume en 2D) d'un corps
    function GetBodyArea(ibdyty) bind(c, name='RBDY2_GetBodyArea')
      implicit none
      integer(c_int), intent(in), value :: ibdyty      ! numero du corps dont on veut recuperer l'aire
      real(c_double)                    :: GetBodyArea ! aire du corps numero ibdyty
      !! PURPOSE
      !!  return area (2D volume equivalent) of a given body

      GetBodyArea = get_area(ibdyty)

    end function GetBodyArea

    ! fonction qui recupere l'aire (volume en 2D) de tout les corps
    subroutine GetAllArea(rvect, ivalue) bind(c, name='RBDY2_GetAllArea')
      implicit none
      type(c_ptr)    :: rvect
      integer(c_int) :: ivalue
      !
      real(c_double), dimension(:), pointer :: vector
      integer :: ibdyty

      ivalue = get_nb_RBDY2()
      allocate(vector(ivalue))

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(ibdyty)
      !$OMP DO SCHEDULE(RUNTIME)
      do ibdyty = 1, ivalue
        vector(ibdyty) = get_area(ibdyty)
      end do
      !$OMP END DO
      !$OMP END PARALLEL

      rvect = c_loc(vector(1))

    end subroutine GetAllArea

    ! fonction qui calcule les grandeurs necessaires pour tester si on a atteint l'etat d'equilibre :
    !   * Qnorm : 1/N*sum(<|v_i|>), ou |v_i| est la norme euclidienne du vecteur vitesse v_i
    !   * Mnorm : sup(<|v_i|>) 
    ! pour les corps dont l'ordonnee est situe entre alt_min et alt_max et rnvoie les resultats sous la forme d'un vecteur de deux doubles
    !   norms = (/ Qnorm, Mnorm /)
    subroutine ComputePartialEquilibriumState(alt_min, alt_max, norms) bind(c, name='RBDY2_ComputePartialEquilibriumState')

       implicit none

       ! variables d'entree
       real(c_double), intent(in), value :: alt_min, alt_max ! le calcul n'est effectue 
          ! que pour les corps dont l'ordonnee est entre alt_min et alt_max

       ! variable de sortie
       real(c_double), intent(out), dimension(2) :: norms ! vecteur destiner a recevoir les deux
          ! normes calculees

       ! variables locales
       real(kind=8) :: Qnorm, Mnorm ! resp. norme quadratique et du maximum
          ! des vitesses des corps, dont l'ordonnee est entre alt_min et alt_max

       ! on clacule les normes
       call compute_partial_equilibrium_state_RBDY2(alt_min, alt_max, Qnorm, Mnorm)

       ! on les stocke dans le tabelau a renvoyer
       norms = (/ Qnorm, Mnorm /)

    end subroutine ComputePartialEquilibriumState 

    ! fonction qui teste si une partie de l'echantillon est en equilibre
    ! les corps ayant leur abscisse comprise entre 0 et abs_max
    ! elle sert dans le cas des silos pour decider si on active la 
    ! recherche d'arches
    ! precondition:
    !    - alt_min: altitude (ordonnee) minimale definissant les corps pris en compte
    !    - alt_max: altitude (ordonnee) maximale definissant les corps pris en compte
    ! postcondition:
    !    - info: vaut "vrai" ssi l'equilibre a ete atteint
    function CheckPartialEquilibriumState(alt_min, alt_max) bind(c, name='RBDY2_CheckPartialEquilibriumState')
       implicit none 
       ! variable d'entree
       real(c_double), intent(in), value :: alt_min, alt_max
          ! le calcul n'est effectue que pour les corps dont l'ordonnee
          ! est entre alt_min et alt_max
       ! variable de sortie
       logical(C_BOOL) :: CheckPartialEquilibriumState
       logical :: info
       
       call check_partial_equilibrium_state_RBDY2(info, alt_min, alt_max)
       CheckPartialEquilibriumState = info
       
    end function CheckPartialEquilibriumState 

    !-- gestion visibilite

    subroutine SetBodiesInvisible(list_bdy,nb_bdy) bind(c, name='RBDY2_SetBodiesInvisible')
      implicit none
      integer(c_int),intent(in), value :: nb_bdy
      integer(c_int),intent(in)        :: list_bdy(nb_bdy)

      call set_invisible(nb_bdy,list_bdy)

    end subroutine

    function IsVisible(ibdyty) bind(c, name='RBDY2_IsVisible')
       implicit none
       integer(c_int), intent(in), value :: ibdyty
       integer(c_int) :: IsVisible

       if (get_visible(ibdyty)) then
         IsVisible = 1
       else
         IsVisible = 0
       end if

    end function

    subroutine setVisible(ibdyty) bind(c, name='RBDY2_SetVisible')

       implicit none

       integer(c_int), intent(in), value :: ibdyty ! indice du coprs dans la liste des RBDY2

       call set_visibility(ibdyty, .TRUE.)

    end subroutine

    subroutine setInvisible(ibdyty) bind(c, name='RBDY2_SetInvisible')

       implicit none

       integer(c_int), intent(in), value :: ibdyty ! indice du coprs dans la liste des RBDY2

       call set_visibility(ibdyty, .FALSE.)

    end subroutine

    subroutine SetVisibleVlocyDrivenDof( ibdyty, iccdof ) &
        bind( c, name='RBDY2_SetVisibleVlocyDrivenDof' )

      implicit none
      integer( c_int ), intent( in ), value :: ibdyty
      integer( c_int ), intent( in ), value :: iccdof

      call switch_vlocy_driven_dof( ibdyty, iccdof, 1 )

    end subroutine SetVisibleVlocyDrivenDof

    subroutine SetInvisibleVlocyDrivenDof( ibdyty, iccdof ) &
        bind( c, name='RBDY2_SetInvisibleVlocyDrivenDof' )

      implicit none
      integer( c_int ), intent( in ), value :: ibdyty
      integer( c_int ), intent( in ), value :: iccdof

      call switch_vlocy_driven_dof( ibdyty, iccdof, 0 )

    end subroutine SetInvisibleVlocyDrivenDof

    !----

    function GetBodyMass(ibdyty) bind(c, name='RBDY2_GetBodyMass')
      !! PURPOSE
      !!  Get the mass of a given body
      integer(c_int), intent(in), value :: ibdyty
      real(c_double) :: GetBodyMass

      GetBodyMass = get_mass(ibdyty)

    end function

    subroutine GetAllMass(rvect,ivalue) bind(c, name='RBDY2_GetAllMass')
      implicit none
      type(c_ptr)    :: rvect
      integer(c_int) :: ivalue
      !
      real(c_double), dimension(:), pointer :: vector
      integer :: ibdyty

      ivalue = get_nb_RBDY2()
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

    subroutine CompCoor() bind(C, name='RBDY2_CompCoor')
       !! PURPOSE
       !!  Compute the positions of rigid bodies in container

       call comp_coor_4all_RBDY2

    end subroutine

    function GetBodyDensity(ibdyty) bind(C, name='RBDY2_GetBodyDensity')
      !! PURPOSE
      !!  Get the density of a given body
      implicit none
      integer(C_INT), value :: ibdyty
      real(C_DOUBLE)        :: GetBodyDensity

      call get_density(ibdyty, GetBodyDensity)
    end function 
      
    function getNbContactor(ibdyty) bind(C, name='RBDY2_GetNbContactor')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int) :: getNbContactor

      getNbContactor = get_nb_tacty(ibdyty)

    end function

!!!------------------------------------------------------------------------

    subroutine GetContactorType(ibdyty, c5) bind(C, name='RBDY2_GetContactorType')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      type(c_ptr) :: c5
      !
      integer(kind=4)  :: i
      character(len=5), pointer :: contactor_type

      allocate(contactor_type)
      contactor_type = get_tacID(ibdyty,1)

      c5 = c_loc(contactor_type(1:1))

    end subroutine

!!!------------------------------------------------------------------------

    subroutine GetContactorColor(ibdyty, itacty, c5) bind(C, name='RBDY2_GetContactorColor')
      implicit none
      integer(c_int), intent(in), value :: ibdyty, itacty
      type(c_ptr) :: c5
      !
      integer(kind=4)  :: i
      character(len=5), pointer :: contactor_color

      ALLOCATE(contactor_color)
      contactor_color = get_color(ibdyty,itacty)

      c5 = c_loc(contactor_color(1:1))

    end subroutine
!!!------------------------------------------------------------------------
    
    SUBROUTINE SetContactorColor(ibdyty, itacty, cvalue1_c) bind(C, name='RBDY2_SetContactorColor')
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

      call set_color_RBDY2(ibdyty,itacty,color)
  
    END SUBROUTINE

!!!------------------------------------------------------------------------

    subroutine getDrvVlocy(ibdyty, i4_vector, i4_size, r8_vector, r8_size) bind(c, name='RBDY2_getDrvVlocy')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(out) :: i4_size, r8_size
      type(c_ptr) :: i4_vector, r8_vector
      !
      integer(kind=4), dimension(:), pointer :: i4_target
      real(kind=8)   , dimension(:), pointer :: r8_target

      i4_target => null()
      r8_target => null()

      call get_drv_vlocy_RBDY2(ibdyty, i4_target, r8_target)

      if( associated(i4_target) ) then
        i4_size = size(i4_target)
        r8_size = size(r8_target)

        i4_vector = c_loc(i4_target(1))
        r8_vector = c_loc(r8_target(1))
      else
        i4_size = 0
        r8_size = 0

        i4_vector = c_null_ptr
        r8_vector = c_null_ptr
      end if
      
    end subroutine

    subroutine compDrvVlocy(ibdyty, vector_in, length) bind(c, name='RBDY2_computeDrvVlocy')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: length
      type(c_ptr), value :: vector_in
      !
      real(kind=8), dimension(:), pointer :: values

      call c_f_pointer(cptr=vector_in, fptr=values, shape=(/length/))
      call comp_drv_vlocy_RBDY2(ibdyty, values)
    end subroutine

!!! vt for peligriff ----------------------------------------

    subroutine GetPtrMass(ibdyty, mass_ptr) bind(c, name='RBDY2_GetPtrMass')
      integer(c_int), intent(in), value :: ibdyty
      type(c_ptr)                       :: mass_ptr

      real(c_double), dimension(:), pointer :: ptr
      ptr => get_ptr_mass(ibdyty)

      mass_ptr = c_loc(ptr(1))

    end subroutine

    subroutine GetV(ibdyty, vel) bind(C, name='RBDY2_GetVelocity')
      implicit none
      integer :: i
      integer(C_INT), intent(in), value :: ibdyty
      real(C_DOUBLE), dimension(3)      :: vel
      real(kind=8) :: eps

      eps=epsilon(1.d0)

      vel = get_V(ibdyty)
      
      !write(*,*) vel(1),vel(2),vel(3),vel(4),vel(5),vel(6)
      do i=1,3 
        if (abs(vel(i))<eps) vel(i)=eps
      enddo

    end subroutine

    subroutine GetBulkBehavID(ibdyty, iblmty, blmID) bind(c, name='RBDY2_GetBulkBehavID')
       implicit none
       !! PURPOSE
       !!  return the ID of a given bulk of a given body

       ! variable d'entree
       integer(c_int), intent(in), value :: ibdyty ! numéro du RBDY2 considere
       integer(c_int), intent(in), value :: iblmty ! numéro du bulk considere

       ! variable de sortie
       type(c_ptr) :: blmID                        ! pointeur C vers l'ID du materiau considere

       ! variables locales
       character(c_char), dimension(5), target :: tmp     ! l'ID du materiau considere, stocke comme une chaine C
       character(len=5)                        :: GetID_f ! l'ID du materiau considere, stocke comme une chaine Fortran
 
       ! on recupere l'ID du materiau du bulk considere
       GetID_f = get_bulk_behav_ID_RBDY2(ibdyty, iblmty)
       ! on le converti en une chaine C
       tmp(:) = transfer( GetID_f, 'a', size(tmp) )
       ! on recupere un pointeur vers la chaine stockant l'ID du materiau
       blmID = c_loc(tmp(1))

   end subroutine GetBulkBehavID
 
   function GetBulkBehavNumberRBDY2(ibdyty) bind(c, name='RBDY2_GetBulkBehavNumber')
     implicit none
     integer(c_int), intent(in), value :: ibdyty ! numéro du RBDY2 considere
     integer(c_int) ::  GetBulkBehavNumberRBDY2

     !mr: pas tres propre car on considere iblmty=1
     GetBulkBehavNumberRBDY2 = get_bulk_behav_number_RBDY2(ibdyty,1) 

   end function GetBulkBehavNumberRBDY2

   subroutine SetSurfaceSectors(nbsect) bind(c, name='RBDY2_SetSurfaceSectors')
     implicit none
     integer(c_int),intent(in), value :: nbsect

     call set_surface_sectors(nbsect)

   end subroutine SetSurfaceSectors

   subroutine GetStressRBDY2(ibdyty,mat,dim1,dim2) bind(c, name='RBDY2_GetStress')
     implicit none
     type(c_ptr) :: mat
     integer(c_int), intent(in),value        :: ibdyty
     integer(c_int), intent(out)             :: dim1,dim2
     real(c_double), dimension(:,:), pointer :: sigma

     dim1 = 2
     dim2 = 2
     allocate(sigma(dim1,dim2))
     mat = c_loc(sigma(1,1))

     call get_stress_RBDY2(ibdyty,sigma)
     
   end subroutine GetStressRBDY2

   subroutine InitializeStressRBDY2() bind(c, name='RBDY2_InitializeStresses')
     implicit none

     call initialize_stress()

   end subroutine InitializeStressRBDY2

!vhn
   subroutine InitializeWSRBDY2(rvalue1) bind(c, name='RBDY2_InitializeWS')
     implicit none
     real(c_double), intent(in), value :: rvalue1
     
     call initialize_WS_sectors(rvalue1)

   end subroutine InitializeWSRBDY2

   subroutine ModifyBody(ibdyty,itacty,rvect,length) bind(c, name='RBDY2_ModifyBody')
      !! PURPOSE
      !!
      implicit none
      integer(c_int),intent(in), value           :: ibdyty,itacty,length

      real(c_double),intent(in) :: rvect(length)

      call modify_body(ibdyty,itacty,rvect)

    end subroutine

   subroutine SkipInvisible() bind(c, name='RBDY2_SkipInvisible')
     implicit none

     call set_skip_invisible_RBDY2()

   end subroutine

   subroutine CleanMemory() bind(c, name='RBDY2_CleanMemory')
     implicit none

     call clean_memory_RBDY2()

   end subroutine

   function GetThermalValueRBDY2(ibdyty,itacty) bind(c, name='RBDY2_GetThermalValue')
     implicit none
     integer(c_int),intent(in), value :: ibdyty,itacty
     real(c_double) :: GetThermalValueRBDY2

     GetThermalValueRBDY2 = get_thermal_value(ibdyty,itacty)

   end function GetThermalValueRBDY2

   function GetElectricalPotentialRBDY2(ibdyty) bind(c, name='RBDY2_GetElectricalPotential')
     implicit none
     integer(c_int),intent(in), value :: ibdyty
     real(c_double) :: GetElectricalPotentialRBDY2

     GetElectricalPotentialRBDY2 = get_electric_potentiel(ibdyty)

   end function  GetElectricalPotentialRBDY2

   function GetElectricalCurrentRBDY2(ibdyty) bind(c, name='RBDY2_GetElectricalCurrent')
     implicit none
     integer(c_int),intent(in), value :: ibdyty
     real(c_double) :: GetElectricalCurrentRBDY2

     GetElectricalCurrentRBDY2 = get_electric_current(ibdyty)

   end function  GetElectricalCurrentRBDY2

   function GetBetaiRBDY2(ibdyty,itacty) bind(c, name='RBDY2_GetBetai')
     implicit none
     integer(c_int),intent(in), value :: ibdyty,itacty
     real(c_double) :: GetBetaiRBDY2

     GetBetaiRBDY2 = get_betai(ibdyty,itacty)

   end function  GetBetaiRBDY2

   function GetPeriodeRBDY2(ibdyty) bind(c, name='RBDY2_GetPeriode')
     implicit none
     integer(c_int),intent(in), value :: ibdyty
     real(c_double) :: GetPeriodeRBDY2

     GetPeriodeRBDY2 = get_periode(ibdyty)

   end function  GetPeriodeRBDY2

   function GetAverageSurfaceEnergy(ibdyty,itacty) bind(c, name='RBDY2_GetAverageSurfaceEnergy')
     implicit none
     integer(c_int),intent(in), value :: ibdyty,itacty
     real(c_double) :: GetAverageSurfaceEnergy

     GetAverageSurfaceEnergy = get_average_WS(ibdyty,itacty)

   end function GetAverageSurfaceEnergy
   
end module wrap_RBDY2
