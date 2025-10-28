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
MODULE wrap_therMAILx

  USE ISO_C_BINDING

  USE therMAILx,ONLY: get_nb_therMAILx, &
       compute_conductivity_therMAILx, &
       compute_capacity_therMAILx, &
       compute_ttFint_therMAILx, &
       compute_Fext_therMAILx, &
       assemb_KT_therMAILx, &
       assemb_RHS_therMAILx, &
       trial_assemb_KT_therMAILx, &
       trial_assemb_RHS_therMAILx, &
       increment_therMAILx, &
       comp_dof_therMAILx, &
       update_dof_therMAILx, &
       update_bulk_therMAILx, &
       compute_residue_norm_therMAILx, &
       read_in_driven_dof_therMAILx, &
       read_models_therMAILx, &
       read_behaviours_therMAILx, &
       write_xxx_dof_therMAILx, &
       read_in_dof_therMAILx, &
       write_out_driven_dof_therMAILx, &
       read_in_gpv_therMAILx, &
       CHECK_therMAILx, &
       get_write_DOF_therMAILx, &
       get_write_Rnod_therMAILx, &
       get_vector_therMAILx, &
       put_vector_therMAILx, &
       get_field_rank, &
       set_field_bynode, set_field_byelem, &
       get_vfield_rank, &
       set_vfield_bynode, set_vfield_byelem, &
       get_nb_nodes_therMAILx, &
       get_nb_elements_therMAILx, &
       add_source_therMAILx, &
       push_ppset_therMAILx, &
       compute_convection_therMAILx, &
       add_field_divergence_therMAILx, &
       get_NodalGrad_therMAILx ,&
       get_NodalFlux_therMAILx ,&
       apply_drvdof_KT_therMAILx, &
       initialize_elementary_flux_therMAILx, &
       get_coor_therMAILx, &
       get_connectivity_therMAILx, &
       get_All_therMAILx, &
       get_gp_coor_therMAILx , &
       get_gp_field_therMAILx, &
       clean_memory_therMAILx, &
       set_Matrix_Storage_therMAILx, &
       set_Matrix_Shape_therMAILx, &
       set_without_renum_therMAILx, &
       check_properties_therMAILx, &
       get_nb_gp_by_elem_therMAILx, &
       get_nb_gp_therMAILx


CONTAINS

!!!----------------------------------------------------
    

    SUBROUTINE IncrementStep() bind(c, name='therMAILx_IncrementStep')
      IMPLICIT NONE

       CALL increment_therMAILx

    END SUBROUTINE

    subroutine ComputeConductivity(list_ids, length) bind(c, name='therMAILx_ComputeConductivity')
      use timer
      implicit none
      
      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp condu  ')
      call start_itimer(timer_id)

      if( length == 0 ) then  
        do i = 1, get_nb_therMAILx()
          call compute_conductivity_therMAILx(i,0)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_conductivity_therMAILx(list(i),0)
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeCapacity(list_ids, length) bind(c, name='therMAILx_ComputeCapacity')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp capa   ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          call compute_capacity_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_capacity_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeInternalFlux(list_ids, length) bind(c, name='therMAILx_ComputeInternalFlux')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp Fint   ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL compute_ttFint_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_ttFint_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeExternalFlux(list_ids, length) bind(c, name='therMAILx_ComputeExternalFlux')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp Fext   ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL compute_Fext_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call compute_Fext_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    !!! construction of the iteration matrix and the corresponding right hand side vector

    subroutine AssembThermKT(list_ids, length) bind(c, name='therMAILx_AssembThermKT')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] assemb KT   ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL assemb_KT_therMAILx(i)
          CALL apply_drvdof_KT_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          CALL assemb_KT_therMAILx(list(i))
          CALL apply_drvdof_KT_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine AssembThermRHS(list_ids, length) bind(c, name='therMAILx_AssembThermRHS')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] assemb RHS  ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL assemb_RHS_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call assemb_RHS_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeThermDof(list_ids, length) bind(c, name='therMAILx_ComputeThermDof')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp dof    ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL comp_dof_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call comp_dof_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine


    subroutine UpdateThermDof(list_ids, length) bind(c, name='therMAILx_UpdateThermDof')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] update dof  ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL update_dof_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call update_dof_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeThermFields(list_ids, length) bind(c, name='therMAILx_ComputeThermFields')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp field  ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL compute_conductivity_therMAILx(i,1)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          CALL compute_conductivity_therMAILx(list(i),1)
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    subroutine UpdateThermBulk(list_ids, length) bind(c, name='therMAILx_UpdateThermBulk')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] update bulk ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL update_bulk_therMAILx(i)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          call update_bulk_therMAILx(list(i))
        end do
      end if

      call stop_itimer(timer_id)

    end subroutine

    function ComputeResidueNorm(list_ids, length) bind(c, name='therMAILx_ComputeResidueNorm')
      use timer
      implicit none

      type(c_ptr),    intent(in), value :: list_ids
      integer(c_int), intent(in), value :: length
      !
      real(C_double) :: ComputeResidueNorm
      !
      integer(kind=4), save :: timer_id = 0
      integer(kind=4) :: i
      integer(kind=4), dimension(:), pointer :: list

      real(C_double) :: norm_res,norm_T

      ComputeResidueNorm = 0.d0

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] compResNorm ')
      call start_itimer(timer_id)

      if( length == 0 ) then
        do i = 1, get_nb_therMAILx()
          CALL compute_residue_norm_therMAILx(norm_res,norm_T,i) 
          ComputeResidueNorm = max(ComputeResidueNorm,norm_res)
        end do
      else
        call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
        do i = 1, length
          CALL compute_residue_norm_therMAILx(norm_res,norm_T,list(i)) 
          ComputeResidueNorm = max(ComputeResidueNorm,norm_res)
        end do
      end if

      call stop_itimer(timer_id)

    end function

!!!--------------------------------------------------------------------

    SUBROUTINE ReadDrivenDof() bind(c, name='therMAILx_ReadDrivenDof')
      IMPLICIT NONE

       CALL read_in_driven_dof_therMAILx

    END SUBROUTINE

    SUBROUTINE WriteDrivenDof() bind(c, name='therMAILx_WriteDrivenDof')
      IMPLICIT NONE

       CALL write_out_driven_dof_therMAILx

    END SUBROUTINE

    SUBROUTINE LoadModels() bind(c, name='therMAILx_LoadModels')
      IMPLICIT NONE

       CALL read_models_therMAILx

    END SUBROUTINE

    SUBROUTINE LoadBehaviours() bind(c, name='therMAILx_LoadBehaviours')

       CALL read_behaviours_therMAILx 

    END SUBROUTINE

    subroutine ReadIniDof(step) bind(c, name='therMAILx_ReadIniDof')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_dof_therMAILx(step)

    end subroutine

    subroutine ReadIniGPV(step) bind(c, name='therMAILx_ReadIniGPV')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_gpv_therMAILx(step)

    end subroutine

!!!--------------------------------------------------------------------

    SUBROUTINE WriteLastDof() bind(c, name='therMAILx_WriteLastDof')
      IMPLICIT NONE
      INTEGER :: ifrom,ito

      ifrom = 1  
      ito   = get_nb_therMAILx()
      CALL write_xxx_dof_therMAILx(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutDof() bind(c, name='therMAILx_WriteOutDof')
      IMPLICIT NONE
      INTEGER :: ifrom,ito
      LOGICAL :: write_DOF

      !! PURPOSE
      !!  write ascii DOF.OUT file. Can be activate only each N step

      write_DOF = get_write_DOF_therMAILx()
      IF (write_DOF) THEN
        ifrom = 1  
        ito   = get_nb_therMAILx()
        CALL write_xxx_dof_therMAILx(1,ifrom,ito)
      END IF

    END SUBROUTINE

    SUBROUTINE DisplayOutDof() bind(c, name='therMAILx_DisplayOutDof')
      IMPLICIT NONE
      INTEGER :: ifrom,ito

      !! PURPOSE
      !!  display body degrees of freedom
       
      ifrom = 1  
      ito   = get_nb_therMAILx()
      CALL write_xxx_dof_therMAILx(6,ifrom,ito)

    END SUBROUTINE

    subroutine PutBodyVector(cvalue1_c, ivalue1, mat_in, dim1, dim2) bind(c, name='therMAILx_PutBodyVector')
      implicit none
      character(c_char), intent(in), dimension(5) :: cvalue1_c
      integer(c_int)   , intent(in), value        :: ivalue1, dim1, dim2
      type(c_ptr)                  , value        :: mat_in
      !
      real(kind=8), dimension(:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      call c_f_pointer(cptr=mat_in, fptr=vector, shape=(/dim1*dim2/))
      call put_vector_therMAILx(cvalue1, ivalue1, vector, dim1*dim2)

    end subroutine

    SUBROUTINE GetBodyVector(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='therMAILx_GetBodyVector')
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

      if( cvalue1 == 'Coor0' ) then
        ivalue2 = nbDIME
      else
        ivalue2 = 1
      end if

      ivalue3 = get_nb_nodes_therMAILx(ivalue1)
      allocate(vector(ivalue2,ivalue3))
      CALL get_vector_therMAILx(cvalue1,ivalue1,vector,ivalue2*ivalue3)

      rvect = c_loc(vector(1,1))

    END SUBROUTINE

    function GetNbTherMAILx() bind(c, name='therMAILx_GetNbTherMAILx')

       implicit none

       ! valeur de retour
       integer(c_int) :: GetNbTherMAILx ! nombre mailles pour la thermique

       GetNbTherMAILx = get_nb_therMAILx()

    end function GetNbTherMAILx
    
    function getScalarFieldRank(ibdyty, iblmty, name) bind(c, name='therMAILx_GetScalarFieldRank')
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

    subroutine SetScalarFieldByNode(ibdyty,f_rank,f,f_size) bind(c, name='therMAILx_SetScalarFieldByNode')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size
      real(c_double), intent(in), dimension(f_size) :: f

       call set_field_bynode(ibdyty,f_rank,f_size,f)

    end subroutine

    subroutine SetScalarFieldByElem(ibdyty,f_rank,f,f_size) bind(c, name='therMAILx_SetScalarFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty, f_rank, f_size
      real(c_double), intent(in), dimension(f_size) :: f

      call set_field_byelem(ibdyty,f_rank,f_size,f)

    end subroutine

    function getVectorFieldRank(ibdyty, iblmty, name) bind(c, name='therMAILx_GetVectorFieldRank')
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

    subroutine SetVectorFieldByNode(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='therMAILx_SetVectorFieldByNode')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_vfield_bynode(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    subroutine SetVectorFieldByElem(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='therMAILx_SetVectorFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_vfield_byelem(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    !fd: fonction qui renvoie le nombre de noeuds d'un therMAILx
    function GetNbNodes(ibdyty) bind(c, name='therMAILx_GetNbNodes')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbNodes ! nombre de noeuds d un therMAILx 

       GetNbNodes = get_nb_nodes_therMAILx(ibdyty)

    end function GetNbNodes


    !fd: fonction qui renvoie le nombre de noeuds d'un therMAILx
    function GetNbElements(ibdyty) bind(c, name='therMAILx_GetNbElements')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbElements ! nombre de noeuds d un therMAILx 

       GetNbElements = get_nb_elements_therMAILx(ibdyty)

    end function GetNbElements

    !fd: fonction qui renvoie le nombre de ddl d'un therMAILx
    function GetNbDofs(ibdyty) bind(c, name='therMAILx_GetNbDofs')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbDofs ! nombre de ddl d un therMAILx 

       GetNbDofs = 1*get_nb_nodes_therMAILx(ibdyty)

    end function GetNbDofs
    
    !--------------------------------------------------------------------------------
    ! DA : Ajout du wrap pour source locale
    SUBROUTINE AddSource(ivalue1, ivalue2) bind(c, name='therMAILx_AddSource')
      IMPLICIT NONE
      integer(c_int),intent(in), value           :: ivalue1, ivalue2

      CALL add_source_therMAILx(ivalue1, ivalue2)


    END SUBROUTINE

    SUBROUTINE AddFieldDivergence(ivalue1, ivalue2) bind(c, name='therMAILx_AddNodalFieldDivergence')
      IMPLICIT NONE
      integer(c_int),intent(in), value           :: ivalue1, ivalue2

      CALL add_field_divergence_therMAILx(ivalue1, ivalue2)


    END SUBROUTINE
    !--------------------------------------------------------------------------------
    !DA : Ajout du wrap pour utiliser ppset dans thermailx
    SUBROUTINE PushProperties() bind(c, name='therMAILx_PushProperties')
      IMPLICIT NONE

       CALL push_ppset_therMAILx

    END SUBROUTINE
    !--------------------------------------------------------------------------------
    !DA : Ajout du wrap pour utiliser des termes de convection
    subroutine ComputeConvection() bind(c, name='therMAILx_ComputeConvection')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_therMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[therM] comp convec ')
      call start_itimer(timer_id)

      call compute_convection_therMAILx 

      call stop_itimer(timer_id)

    end subroutine
    !--------------------------------------------------------------------------------
    !DA : Ajout du wrap pour utiliser les stockage de matrice

    SUBROUTINE WithoutRenumbering() bind(c, name='therMAILx_WithoutRenumbering')
      IMPLICIT NONE

       CALL set_without_renum_therMAILx()

    END SUBROUTINE

    SUBROUTINE BandStorage() bind(c, name='therMAILx_BandStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_therMAILx('band____')

    END SUBROUTINE

    SUBROUTINE SparseStorage() bind(c, name='therMAILx_SparseStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_therMAILx('sparse__')

    END SUBROUTINE

    SUBROUTINE ExplodedStorage() bind(c, name='therMAILx_ExplodedStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_therMAILx('exploded')

    END SUBROUTINE


    SUBROUTINE DiagonalStorage() bind(c, name='therMAILx_DiagonalStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_therMAILx('diagonal')

    END SUBROUTINE

    SUBROUTINE SkylineStorage() bind(c, name='therMAILx_SkylineStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_therMAILx('skyline_')

    END SUBROUTINE

    SUBROUTINE FullStorage() bind(c, name='therMAILx_FullStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_therMAILx('full____')

    END SUBROUTINE

    SUBROUTINE SymmetricShape() bind(c, name='therMAILx_SymmetricShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_therMAILx('sym_____')

    END SUBROUTINE

    SUBROUTINE UnspecifiedShape() bind(c, name='therMAILx_UnspecifiedShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_therMAILx('std_____')

    END SUBROUTINE




    SUBROUTINE GetGrad(ivalue1,strain,ivalue2,ivalue3) bind(c, name='therMAILx_GetGrad')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2,ivalue3
      type(c_ptr)                       :: strain
      !
      real(c_double), dimension(:,:), pointer :: E

      ivalue2 = 3
      ivalue3 = get_nb_nodes_therMAILx(ivalue1)
      allocate(E(ivalue2,ivalue3))
      CALL get_NodalGrad_therMAILx(ivalue1,E)

      strain = c_loc(E(1,1))

    END SUBROUTINE
    
    SUBROUTINE GetFlux(ivalue1,strain,ivalue2,ivalue3) bind(c, name='therMAILx_GetFlux')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2,ivalue3
      type(c_ptr)                       :: strain
      !
      real(c_double), dimension(:,:), pointer :: E

      ivalue2 = 3
      ivalue3 = get_nb_nodes_therMAILx(ivalue1)
      allocate(E(ivalue2,ivalue3))
      CALL get_NodalFlux_therMAILx(ivalue1,E)

      strain = c_loc(E(1,1))

    END SUBROUTINE

    SUBROUTINE InitializeElementaryFlux() bind(c, name='therMAILx_InitializeElementaryFlux')
      IMPLICIT NONE

      call initialize_elementary_flux_therMAILx()

    END SUBROUTINE 

    subroutine GetCoorTherMAILx(idBody, ptr, dim1, dim2) BIND(c, name='therMAILx_GetCoor')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_coor_therMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetCoorTherMAILx

    subroutine GetConnectivityTherMAILx(idBody, ptr, dim1) BIND(c, name='therMAILx_GetConnectivity')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1

      integer(kind=4), dimension(:), pointer :: all

      all => get_connectivity_therMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1))
        dim1 = size(all)
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine GetConnectivityTherMAILx

    subroutine GetAllTherMAILx(idBody,ptr, dim1, dim2) BIND(c, name='therMAILx_GetAll')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_All_therMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetAllTherMAILx

    subroutine GetGpCoor(idBody, ptr, dim1, dim2) BIND(c, name='therMAILx_GetGpCoor')
      implicit none
      integer(c_int), intent(IN), value :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer  :: gp_coor

      gp_coor => get_gp_coor_therMAILx(idBody)

      if( associated(gp_coor) ) then
        ptr  = c_loc(gp_coor(1,1))
        dim1 = size(gp_coor,1)
        dim2 = size(gp_coor,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine

    subroutine GetGpField(idBody, idEle, idGp, idField, ptr, length) BIND(c, name='therMAILx_GetGpField')
      implicit none
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,idEle,idGp,idField
      type(c_ptr)    :: ptr
      integer(c_int) :: length

      real(kind=8), dimension(:), pointer :: field

      allocate(field(3))
      field = get_gp_field_therMAILx(idBody,idEle,idGp,idField)

      ptr  = c_loc(field(1))
      length = size(field)

    end subroutine GetGpField

    ! TODO: a quoi ca sert ?
    !!! construction of the iteration matrix and the corresponding right hand side vector

    SUBROUTINE TrialAssembThermKT() bind(c, name='therMAILx_TrialAssembThermKT')
      IMPLICIT NONE

       CALL trial_assemb_KT_therMAILx

    END SUBROUTINE

    SUBROUTINE TrialAssembThermRHS() bind(c, name='therMAILx_TrialAssembThermRHS')
      IMPLICIT NONE

       CALL trial_assemb_RHS_therMAILx

    END SUBROUTINE

    !!!!
    subroutine CleanMemory() bind(c, name='therMAILx_CleanMemory')
      implicit none

      call clean_memory_therMAILx

    end subroutine

    subroutine CheckProperties() bind(c, name='therMAILx_CheckProperties')
      implicit none
      !

      if( get_nb_therMAILx() < 1 ) return

      call check_properties_therMAILx()
       
    end subroutine CheckProperties

    subroutine GetNbGpByElem(elems, esize, ssize, nbgps, nsize) bind(c, name='therMAILx_GetNbGpByElem')
      implicit none

      type(c_ptr) :: elems, nbgps
      integer(c_int) :: esize, ssize, nsize
      !
      character(len=5), dimension(:), pointer :: names
      integer(c_int)  , dimension(:), pointer :: n_GPs

      ssize = 5

      call get_nb_gp_by_elem_therMAILx(names, n_Gps, esize)

      nsize = esize
      elems = c_loc( names(1) )
      nbgps = c_loc( n_GPs(1) )
    end subroutine

    ! number of Gauss Point of an element of a therMAILx
    function GetNbGp(ibdyty, iblmty) bind(c, name='therMAILx_GetNbGp')
       implicit none
       integer(c_int),value :: ibdyty, iblmty
       ! valeur de retour
       integer(c_int) :: GetNbGp

       GetNbGp = get_nb_gp_therMAILx(ibdyty, iblmty)

    end function GetNbGp
    
      
END MODULE wrap_therMAILx
