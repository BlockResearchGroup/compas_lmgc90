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
module wrap_multiMAILx                                       

  use ISO_C_BINDING

  use multiMAILx, only: is_new                              , &
                        get_nb_multiMAILx                   , &
                        get_nb_nodes_multiMAILx             , &
                        get_nb_elements_multiMAILx          , &
                        get_visibility_multiMAILx           , &
                        get_vector_multiMAILx               , &
                        set_vector_multiMAILx               , &
                        read_in_driven_dof_multiMAILx       , &
                        write_out_driven_dof_multiMAILx     , &
                        read_in_gpv_multiMAILx              , &
                        read_in_dof_multiMAILx              , &
                        write_xxx_dof_multiMAILx            , &
                        load_behaviours_multiMAILx          , &
                        load_models_multiMAILx              , &
                        update_existing_entities_multiMAILx , &
                        push_ppset_multiMAILx               , &
                        increment_multiMAILx                , &
                        compute_mass_multiMAILx             , &
                        compute_bulk_multiMAILx             , &
                        compute_flux_multiMAILx             , &
                        compute_damping_multiMAILx          , &
                        compute_Fext_multiMAILx             , &
                        compute_flourxces_multiMAILx        , &
                        assemb_KT_multiMAILx                , &
                        assemb_RHS_multiMAILx               , &
                        compute_free_state_multiMAILx       , &
                        compute_dof_multiMAILx              , &
                        compute_residue_norm_multiMAILx     , &
                        update_bulk_multiMAILx              , &
                        update_dof_multiMAILx               , &
                        apply_drvdof_KT_multiMAILx          , &
                        get_field_rank                      , &
                        set_field_bynode, set_field_byelem  , &
                        get_vfield_rank                     , &
                        set_vfield_bynode, set_vfield_byelem, &
                        get_connectivities_multiMAILx       , &
                        get_coor_multiMAILx                 , &
                        get_all_multiMAILx                  , &
                        get_elements_volume                 , &
                        get_elements_neighbor               , &
                        get_ptr_elements_energy             , &
                        get_ptr_elements_jacobian           , &
                        get_ptr_elements_visibility         , &
                        get_ptr_boundary_elements           , &
                        compute_elements_energy             , &
                        compute_elements_jacobian           , &
                        get_deformation_energy              , &
                        set_Matrix_Storage_multiMAILx       , &
                        clean_memory_multiMAILx             , &
                        set_Matrix_Storage_multiMAILx       , &
                        set_Matrix_Shape_multiMAILx         , &
                        set_without_renum_multiMAILx

                         

  use utilities, only: faterr, logmes

CONTAINS
!----------------------------------------------------

    SUBROUTINE WithoutRenumbering() bind(c, name='multiMAILx_WithoutRenumbering')
      IMPLICIT NONE

       CALL set_without_renum_multiMAILx()

    END SUBROUTINE

    SUBROUTINE BandStorage() bind(c, name='multiMAILx_BandStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_multiMAILx('band____')

    END SUBROUTINE

    SUBROUTINE SparseStorage() bind(c, name='multiMAILx_SparseStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_multiMAILx('sparse__')

    END SUBROUTINE

    SUBROUTINE ExplodedStorage() bind(c, name='multiMAILx_ExplodedStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_multiMAILx('exploded')

    END SUBROUTINE


    SUBROUTINE DiagonalStorage() bind(c, name='multiMAILx_DiagonalStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_multiMAILx('diagonal')

    END SUBROUTINE

    SUBROUTINE SkylineStorage() bind(c, name='multiMAILx_SkylineStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_multiMAILx('skyline_')

    END SUBROUTINE

    SUBROUTINE FullStorage() bind(c, name='multiMAILx_FullStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_multiMAILx('full____')

    END SUBROUTINE

    SUBROUTINE SymmetricShape() bind(c, name='multiMAILx_SymmetricShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_multiMAILx('sym_____')

    END SUBROUTINE

    SUBROUTINE UnspecifiedShape() bind(c, name='multiMAILx_UnspecifiedShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_multiMAILx('std_____')

    END SUBROUTINE


  subroutine UsePicardScheme() bind(c, name='multiMAILx_UsePicardScheme')
    implicit none

    is_new = .true.

  end subroutine

  subroutine UseNewtonScheme() bind(c, name='multiMAILx_UseNewtonScheme')
    implicit none

    is_new = .false.

  end subroutine

!-----------------------------------------------------
!  accessors
!-----------------------------------------------------

  function GetNbMultiMAILx() bind(c, name='multiMAILx_GetNb')
     implicit none
     integer(c_int) :: GetNbMultiMAILx

     GetNbMultiMAILx = get_nb_multiMAILx()

  end function GetNbMultiMAILx

  function GetNbNodes(ibdyty) bind(c, name='multiMAILx_GetNbNodes')
     implicit none
     integer(c_int),value :: ibdyty
     integer(c_int) :: GetNbNodes

     GetNbNodes = get_nb_nodes_multiMAILx(ibdyty)

  end function GetNbNodes

  function GetNbElements(ibdyty) bind(c, name='multiMAILx_GetNbElements')
     implicit none
     integer(c_int),value :: ibdyty
     integer(c_int) :: GetNbElements

     GetNbElements = get_nb_elements_multiMAILx(ibdyty)

  end function GetNbElements

  function IsVisibleMultiMAILx(i_bdyty) bind(c, name='multiMAILx_IsVisible')
     implicit none
     integer(c_int), intent(in), value :: i_bdyty
     integer(c_int) :: IsVisibleMultiMAILx

     if(get_visibility_multiMAILx(i_bdyty)) then
       IsVisibleMultiMAILx = 1
     else
       IsVisibleMultiMAILx = 0
     end if

  end function

  subroutine GetVector(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='multiMAILx_GetBodyVector')
    use overall, only: nbDIME
    IMPLICIT NONE
    character(c_char),dimension(5),intent(in) :: cvalue1_c
    integer(c_int), intent(in), value         :: ivalue1
    integer(c_int)                            :: ivalue2, ivalue3
    type(c_ptr)                               :: rvect
    !
    real(kind=8), dimension(:,:), pointer :: vector
    character(len=5) :: cvalue1
    integer :: i

    cvalue1 = ''
    do i=1,5
      cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do

    ! \todo: there is probably a better way to do so...
    if( cvalue1(1:1)=='P' ) then
      ivalue2 = 1
    else
      ivalue2 = nbDIME
    end if
    ivalue3 = get_nb_nodes_multiMAILx(ivalue1)
    allocate(vector(ivalue2,ivalue3))
    call get_vector_multiMAILx(cvalue1,ivalue1,vector,ivalue2,ivalue3)

    rvect = c_loc(vector(1,1))

  end subroutine

  subroutine SetVector(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='multiMAILx_PutBodyVector')
    implicit none
    character(c_char), intent(in), dimension(5) :: cvalue1_c
    integer(c_int)   , intent(in), value        :: ivalue1, ivalue2, ivalue3
    type(c_ptr), value :: rvect
    !
    real(kind=8), dimension(:,:), pointer :: matrix
    character(len=5) :: cvalue1
    integer :: i

    cvalue1 = ''
    do i=1,5
      cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do

    !\WARNING: sizes are inverted to insure consistency with get and indices switch due to C
    call c_f_pointer(cptr=rvect, fptr=matrix, shape=(/ivalue3,ivalue2/))
    call set_vector_multiMAILx(cvalue1,ivalue1,matrix,ivalue3,ivalue2)

  end subroutine

!-----------------------------------------------------
!  reading, writting and loading
!-----------------------------------------------------

  subroutine ReadDrivenDof() bind(c, name='multiMAILx_ReadDrivenDof')
    implicit none

    call read_in_driven_dof_multiMAILx

  end subroutine
  
  subroutine WriteDrivenDof() bind(c, name='multiMAILx_WriteDrivenDof')
    implicit none

    call write_out_driven_dof_multiMAILx

  end subroutine

  subroutine ReadIniGPV(step) bind(c, name='multiMAILx_ReadIniGPV')
     implicit none
     integer(c_int), intent(in), value :: step

     call read_in_gpv_multiMAILx(step)

  end subroutine

  subroutine ReadIniDof(step) bind(c, name='multiMAILx_ReadIniDof')
     implicit none
     integer(c_int), intent(in), value :: step

     call read_in_dof_multiMAILx(step)

  end subroutine

  subroutine WriteLastDof(list_ids, length) bind(c, name='multiMAILx_WriteLastDof')
    implicit none
    type(c_ptr)   , intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    if( length == 0 ) then  
      do i = 1, get_nb_multiMAILx()
        call write_xxx_dof_multiMAILx(2,i)
      end do
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      do i = 1, length
        call write_xxx_dof_multiMAILx(2,list(i))
      end do
    end if

  end subroutine

  subroutine WriteOutDof(list_ids, length) bind(c, name='multiMAILx_WriteOutDof')
    implicit none
    type(c_ptr)   , intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    if( length == 0 ) then  
      do i = 1, get_nb_multiMAILx()
        call write_xxx_dof_multiMAILx(1,i)
      end do
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      do i = 1, length
        call write_xxx_dof_multiMAILx(1,list(i))
      end do
    end if

  end subroutine

  subroutine LoadBehaviours() bind(c, name='multiMAILx_LoadBehaviours')
    implicit none

     call load_behaviours_multiMAILx 

  end subroutine


  subroutine LoadModels() bind(c, name='multiMAILx_LoadModels')
    implicit none

    call load_models_multiMAILx
    call update_existing_entities_multiMAILx
     
  end subroutine

  subroutine PushProperties() bind(c, name='multiMAILx_PushProperties')
    implicit none

     call push_ppset_multiMAILx

  end subroutine

!-----------------------------------------------------
!  computation
!-----------------------------------------------------

  subroutine IncrementStep() bind(c, name='multiMAILx_IncrementStep')
    implicit none

     call increment_multiMAILx 

  end subroutine

  subroutine ComputeMass(list_ids, length) bind(c, name='multiMAILx_ComputeMass')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] comp mass   ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_mass_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_mass_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine ComputeBulk(list_ids, length) bind(c, name='multiMAILx_ComputeBulk')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] comp bulk   ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_bulk_multiMAILx(i)
        call compute_damping_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_bulk_multiMAILx(list(i))
        call compute_damping_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine ComputeFext(list_ids, length) bind(c, name='multiMAILx_ComputeFext')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] comp Fext   ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_Fext_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_Fext_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine AssembKT(list_ids, length) bind(c, name='multiMAILx_AssembKT')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] assemb KT   ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call assemb_KT_multiMAILx(i)
        call apply_drvdof_KT_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call assemb_KT_multiMAILx(list(i))
        call apply_drvdof_KT_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine AssembRHS(list_ids, length) bind(c, name='multiMAILx_AssembRHS')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] assemb RHS  ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call assemb_RHS_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call assemb_RHS_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  function ComputeResidueNorm(list_ids, length) bind(c, name='multiMAILx_ComputeResidueNorm')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    real(c_double) :: ComputeResidueNorm
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list
    !
    integer(c_int)  :: ibdy_Dofs, ibdy_Res
    real(c_double)  :: norm_res,norm_Dofs,tmp
    !
    character(len=70) :: cout
 
    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] compResNorm ')
    call start_itimer(timer_id)
 
    ibdy_Dofs = -1
    ibdy_Res  = -1

    ComputeResidueNorm = 0.d0
    tmp                = 0.d0
 
    ibdy_Dofs = 0
    ibdy_Res  = 0

    if( length == 0 ) then  
 
      do i = 1, get_nb_multiMAILx()
        call compute_residue_norm_multiMAILx(norm_res,norm_Dofs,i)
        if( norm_Dofs > tmp ) then
          ibdy_Dofs = i
          tmp    = norm_Dofs
        end if
        if( norm_res > ComputeResidueNorm ) then
          ibdy_Res = i
          ComputeResidueNorm = norm_res
        end if
      end do
 
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      do i = 1, length
        call compute_residue_norm_multiMAILx(norm_res,tmp,list(i))
        if( norm_Dofs > tmp ) then
          tmp    = norm_Dofs
          ibdy_Dofs = list(i)
        end if
        if( norm_res > ComputeResidueNorm ) then
          ibdy_Res=list(i)
          ComputeResidueNorm = norm_res
        end if
      end do
    end if

    call logmes(' ')   
    write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxRes/MaxFint = ',ComputeResidueNorm,'body : ',ibdy_Res  
    call logmes(cout)
    write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxDDofs /MaxDofs    = ',tmp ,'body : ',ibdy_Dofs 
    call logmes(cout)
 
    call stop_itimer(timer_id)
 
    if (is_new) ComputeResidueNorm = tmp


  end function

  subroutine ComputeFreeState(list_ids, length) bind(c, name='multiMAILx_ComputeFreeState')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] comp Free S ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_free_state_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_free_state_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine ComputeDof(list_ids, length) bind(c, name='multiMAILx_ComputeDof')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] comp dof    ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_dof_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_dof_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine ComputeField(list_ids, length) bind(c, name='multiMAILx_ComputeField')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] comp field  ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_flux_multiMAILx(i)
        call compute_flourxces_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_flux_multiMAILX(list(i))
        call compute_flourxces_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine UpdateBulk(list_ids, length) bind(c, name='multiMAILx_UpdateBulk')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] update bulk ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call compute_flux_multiMAILx(i)
        call update_bulk_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call compute_flux_multiMAILx(list(i))
        call update_bulk_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  subroutine UpdateDof(list_ids, length) bind(c, name='multiMAILx_UpdateDof')
    use timer
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), save :: timer_id = 0
    integer(kind=4), dimension(:), pointer :: list

    if( get_nb_multiMAILx() < 1 ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[multM] update dof  ')
    call start_itimer(timer_id)

    if( length == 0 ) then  
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, get_nb_multiMAILx()
        call update_dof_multiMAILx(i)
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    else
      call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i)
      !$OMP DO SCHEDULE(RUNTIME)
      do i = 1, length
        call update_dof_multiMAILx(list(i))
      end do
      !$OMP END DO
      !$OMP END PARALLEL
    end if

    call stop_itimer(timer_id)

  end subroutine

  function getScalarFieldRank(ibdyty, iblmty, name) bind(c, name='multiMAILx_GetScalarFieldRank')
    implicit none
    integer(c_int), intent(in), value :: ibdyty
    integer(c_int), intent(in), value :: iblmty
    character(c_char), dimension(*)   :: name
    integer(c_int) :: getScalarFieldRank
    !
    integer(kind=4)   :: i
    character(len=30) :: f_name

    f_name = ''
    i = 1
    do while( name(i) /= c_null_char .and. i <= 30 )
      f_name = f_name(1:i-1) // name(i)
      i = i+1
    end do

    getScalarFieldRank = get_field_rank(ibdyty, iblmty, f_name)

  end function

  subroutine SetScalarFieldByNode(IdBody,f_rank,f,f_size) bind(c, name='multiMAILx_SetScalarFieldByNode')
    implicit none
    integer(c_int), intent(in), value :: idbody,f_rank,f_size
    real(c_double), intent(in), dimension(f_size) :: f
   
    call set_field_bynode(IdBody,f_rank,f_size,f)

  end subroutine

  subroutine SetScalarFieldByElem(ibdyty,f_rank,f,f_size) bind(c, name='multiMAILx_SetScalarFieldByElement')
    implicit none
    integer(c_int), intent(in), value :: ibdyty, f_rank, f_size
    real(c_double), intent(in), dimension(f_size) :: f

    call set_field_byelem(ibdyty,f_rank,f_size,f)

  end subroutine

  function getVectorFieldRank(ibdyty, iblmty, name) bind(c, name='multiMAILx_GetVectorFieldRank')
    implicit none
    integer(c_int), intent(in), value :: ibdyty
    integer(c_int), intent(in), value :: iblmty
    character(c_char), dimension(*)   :: name
    integer(c_int) :: getVectorFieldRank
    !
    integer(kind=4)   :: i
    character(len=30) :: f_name

    f_name = ''
    i = 1
    do while( name(i) /= c_null_char .and. i <= 30 )
      f_name = f_name(1:i-1) // name(i)
      i = i+1
    end do

    getVectorFieldRank = get_vfield_rank(ibdyty, iblmty, f_name)

  end function

  subroutine SetVectorFieldByNode(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='multiMAILx_SetVectorFieldByNode')
    implicit none
    integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
    real(c_double), intent(in), dimension(f_size2,f_size1) :: f

    call set_vfield_bynode(ibdyty,f_rank,f,f_size2,f_size1)

  end subroutine

  subroutine SetVectorFieldByElem(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='multiMAILx_SetVectorFieldByElement')
    implicit none
    integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
    real(c_double), intent(in), dimension(f_size2,f_size1) :: f

    call set_vfield_byelem(ibdyty,f_rank,f,f_size2,f_size1)

  end subroutine

  subroutine GetConnectivityMultiMAILx(idBody, ptr, dim1) BIND(c, name='multiMAILx_GetConnectivity')
    implicit none
    integer(c_int), intent(in), value :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1
    !
    integer(kind=4), dimension(:), pointer :: all

    all => get_connectivities_multiMAILx(idBody)

    if( associated(all) ) then
      ptr  = c_loc(all(1))
      dim1 = size(all)
    else
      ptr  = c_null_ptr
      dim1 = 0
    end if

  end subroutine GetConnectivityMultiMAILx

  subroutine GetCoorMultiMAILx(idBody, ptr, dim1, dim2) BIND(c, name='multiMAILx_GetCoor')
    implicit none
    integer(c_int), intent(in), value :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1, dim2
    !
    real(kind=8), dimension(:,:), pointer :: all

    all => get_coor_multiMAILx(idBody)

    if( associated(all) ) then
      ptr  = c_loc(all(1,1))
      dim1 = size(all,1)
      dim2 = size(all,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine GetCoorMultiMAILx

  subroutine GetAllMultiMAILx(idBody,ptr, dim1, dim2) BIND(c, name='multiMAILx_GetAll')
    implicit none
    integer(c_int), intent(in), value :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1, dim2
    !
    real(kind=8), dimension(:,:), pointer :: all

    all => get_all_multiMAILx(idBody)

    if( associated(all) ) then
      ptr  = c_loc(all(1,1))
      dim1 = size(all,1)
      dim2 = size(all,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine GetAllMultiMAILx

  subroutine GetElementsVolume(idBody, ptr, length) bind(c, name='multiMAILx_GetElementsVolume')
    implicit none
    integer(c_int), intent(in), value :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    real(kind=8), dimension(:), pointer :: Volumes       

    Volumes => get_elements_volume(idBody)

    if( associated(Volumes) ) then
      ptr  = c_loc(Volumes(1))
      length = size(Volumes)
    else
      ptr  = c_null_ptr
      length = 0
    end if

  end subroutine

  subroutine GetElementsNeighbor(idBody, tol, maxnb, ptr, dim1, dim2) bind(c, name='multiMAILx_GetElementsNeighbor')
    implicit none
    integer(c_int), intent(in), value :: IdBody, maxnb
    real(c_double), intent(in), value :: tol                   
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1, dim2
    !
    integer(c_int), dimension(:,:), pointer :: Neighbors       

    Neighbors => get_elements_neighbor(idBody,tol,maxnb) 

    if( associated(Neighbors) ) then
      ptr  = c_loc(Neighbors(1,1))
      dim1 = size(Neighbors,1)
      dim2 = size(Neighbors,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine GetPtrElementsEnergy(idBody, ptr, length) bind(c, name='multiMAILx_GetPtrElementsEnergy')
    implicit none
    integer(c_int), intent(in), VALUE :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    real(c_double), dimension(:), pointer :: Energies       

    Energies => get_ptr_elements_energy(idBody)

    if( associated(Energies) ) then
      ptr  = c_loc(Energies(1))
      length = size(Energies)
    else
      ptr  = c_null_ptr
      length = 0
    end if

  end subroutine

  subroutine ComputeElementsEnergy(idBody) bind(c, name='multiMAILx_ComputeElementsEnergy')
    implicit none
    integer(c_int), intent(in), value :: IdBody

    call compute_elements_energy(idBody)

  end subroutine

  subroutine GetPtrElementsJacobian(idBody, ptr, length) bind(c, name='multiMAILx_GetPtrElementsJacobian')
    implicit none
    integer(c_int), intent(in), value :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    real(kind=8), dimension(:), pointer :: Jacobians       

    Jacobians => get_ptr_elements_jacobian(idBody)

    if( associated(Jacobians) ) then
      ptr  = c_loc(Jacobians(1))
      length = size(Jacobians)
    else
      ptr  = c_null_ptr
      length = 0
    end if

  end subroutine

  subroutine ComputeElementsJacobian(idBody) bind(c, name='multiMAILx_ComputeElementsJacobian')
    implicit none
    integer(c_int), intent(in), value :: IdBody

    call compute_elements_jacobian(idBody)

  end subroutine

  subroutine GetPtrElementsVisibility(idBody, ptr, dim1) bind(c, name='multiMAILx_GetPtrElementsVisibility')
    implicit none
    integer(c_int), intent(in), value :: IdBody
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1
    !
    integer(kind=4), dimension(:), pointer :: all

    all => get_ptr_elements_visibility(idBody)

    if( associated(all) ) then
      ptr  = c_loc(all(1))
      dim1 = size(all)
    else
      ptr  = c_null_ptr
      dim1 = 0
    end if

  end subroutine 

  function GetDeformationEnergy(ivalue1,rvect,ivalue2,ivalue3) bind(c, name='multiMAILx_GetDeformationEnergy')
    implicit none
    integer(c_int), intent(in), value :: ivalue1,ivalue2,ivalue3
    real(c_double), intent(in)        :: rvect(ivalue3,ivalue2)
    !
    real(c_double) :: GetDeformationEnergy

    GetDeformationEnergy = get_deformation_energy(ivalue1,rvect)

  end function

  subroutine GetPtrBoundaryElements(idBody, ptr, dim1) BIND(c, name='multiMAILx_GetPtrBoundaryElements')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody

      type(c_ptr)    :: ptr
      integer(c_int) :: dim1

      INTEGER(C_INT),dimension(:),pointer  :: Elements       

      Elements => Get_Ptr_Boundary_Elements(idBody) 

      if( associated(Elements) ) then
        ptr  = c_loc(Elements(1))
        dim1 = size(Elements)
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine

  subroutine CleanMemory() bind(c, name='multiMAILx_CleanMemory')
    implicit none

    call clean_memory_multiMAILx

  end subroutine

end module wrap_multiMAILx
