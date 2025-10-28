!===========================================================================
!
! Copyright 2000-2022 CNRS-UM.
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
module soe

  use parameters

  use a_system

  use utilities, only : logmes, &
                        faterr
  implicit none

  public

  type T_system 
    ! 
    !> number of RHS to store
    integer(kind=4) :: nb_RHS = 0
    !> number of LHS to store
    integer(kind=4) :: nb_LHS = 0

    !> number of dofs of the system
    integer(kind=4) :: nb_dofs = 0

    !> a G_system
    type(G_system) :: g_sys
    !> the Right Hand Sides of the system
    real(kind=8), dimension(:,:), allocatable :: RHS 
    !> Solutions of the system
    real(kind=8), dimension(:,:), allocatable :: X   
  end type

  !> number of systems
  integer(kind=4)                           :: nb_systems 
  !> all the systems of equations in the simulation
  type(T_system), dimension(:), allocatable :: all_systems 

  !> storage type to use for the systems of the soe
  integer(kind=4) :: matrix_storage = p_dense_system

  public initialize           , &
         initialize_soe       , &
         change_system_shape  , &
         set_nb_RHS           , &
         set_nb_LHS           , &
         set_sol_value        , &
         reset_rhs            , &
         reset_sol            , &
         assemble_rhs         , &
         set_elementary_lhs   , &
         set_system_drvdofs   , &
         erase_system_drvdofs , &
         set_system_rhs       , &
         apply_driven_dof_rhs , &
         set_system_drvvalues , &
         solve                , &
         add_sol_contributions, &
         add_rhs_contributions, &
         add_to_rhs           , &
         get_sol              , &
         clean_module

  contains

  !> \brief Initialize the module
  !> Allocate all_systems array
  subroutine initialize(nb_systems)
    implicit none
    !> [in] number of systems
    integer(kind=4), intent(in) :: nb_systems 
    !
    integer(kind=4)   :: i_system, errare
    character(len=80) :: cout
    character(len=15) :: IAM
    !      123456789012345
    IAM = 'soe::initialize'

    if( nb_systems == 0 ) then
      write(cout,'(A,1x,A)') IAM, 'No system...'
      call logmes(cout)
      return
    end if

    if( allocated(all_systems) ) deallocate(all_systems)

    allocate( all_systems(nb_systems), stat=errare )
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating all_systems'
      call faterr(IAM,cout)
    end if

    ! default values : to keep ?
    do i_system = 1, nb_systems
      all_systems(i_system)%nb_RHS = 3 ! RFree, Reac, Raux
      all_systems(i_system)%nb_LHS = 2 ! VFree, VAux
    end do

  end subroutine


  !> \brief Initialize the module
  !> Initialize all_systems array
  subroutine initialize_soe(i_system, ccdof, connectivities, i_storage, with_renum)
    implicit none
    !> [in] id of the system to densely initialize
    integer(kind=4),               intent(in) :: i_system
    !> [in] map between the nodes and the dofs
    integer(kind=4), dimension(:), intent(in) :: ccdof
    !> [in] connectivity of elements
    integer(kind=4), dimension(:), intent(in) :: connectivities
    !> [in] storage type of the matrix
    integer(kind=4),               intent(in) :: i_storage
    !> [in] if renumerotation is to be done
    logical        ,               intent(in) :: with_renum
    !
    integer(kind=4)   :: nb_dofs, nb_lhs, nb_rhs

    nb_dofs = ccdof(size(ccdof))
    all_systems(i_system)%nb_dofs = nb_dofs

    call initialize_system(all_systems(i_system)%g_sys, matrix_storage, ccdof, connectivities, i_storage)

    ! \todo test avant/apres allocation a faire
    nb_rhs = all_systems(i_system)%nb_RHS
    allocate( all_systems(i_system)%RHS(nb_dofs,nb_rhs) )
    all_systems(i_system)%RHS(1:nb_dofs,1:nb_rhs) = 0.D0

    nb_lhs = all_systems(i_system)%nb_LHS
    allocate( all_systems(i_system)%X(nb_dofs,nb_lhs) )
    all_systems(i_system)%X(1:nb_dofs,1:nb_lhs) = 0.D0

  end subroutine

  !> \brief Change storage type of a system
  subroutine change_system_shape(i_system, i_shape)
    implicit none
    !> [in] system index
    integer(kind=4), intent(in) :: i_system
    !> [in] new storage type id
    integer(kind=4), intent(in) :: i_shape

    call change_shape(all_systems(i_system)%g_sys, i_shape)

  end subroutine

  !> \brief Set the number of RHS to store
  subroutine set_nb_RHS(i_system, nb_rhs)
    implicit none
    !> [in] system index
    integer(kind=4), intent(in) :: i_system
    !> [in] number of rhs
    integer(kind=4), intent(in) :: nb_rhs

    all_systems(i_system)%nb_RHS = nb_rhs

  end subroutine

  !> \brief Set the number of LHS to store
  subroutine set_nb_LHS(i_system, nb_lhs)
    implicit none
    !> [in] system index
    integer(kind=4), intent(in) :: i_system
    !> [in] number of lhs
    integer(kind=4), intent(in) :: nb_lhs

    all_systems(i_system)%nb_LHS = nb_lhs

  end subroutine

  !> \brief Initialize value of a X
  subroutine set_sol_value(i_system, i_X, values)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4)           , intent(in) :: i_system
    !> [in] index of X to set
    integer(kind=4)           , intent(in) :: i_X
    !> [in] new values of X field
    real(kind=8), dimension(:), intent(in) :: values

    ! check on sizes ?
    all_systems(i_system)%X(1:all_systems(i_system)%nb_dofs,i_X) = values(1:all_systems(i_system)%nb_dofs)
  end subroutine

  !> \brief Reset a right hand side of a system of equations
  subroutine reset_rhs(i_system, i_rhs)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4), intent(in) :: i_system
    !> [in] index of rhs to reset
    integer(kind=4), intent(in) :: i_rhs

    all_systems(i_system)%RHS(1:all_systems(i_system)%nb_dofs,i_rhs) = 0.d0
  end subroutine

  !> \brief Reset a solution of a system of equations
  subroutine reset_sol(i_system, i_sol)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4), intent(in) :: i_system
    !> [in] index of solution to reset
    integer(kind=4), intent(in) :: i_sol

    all_systems(i_system)%X(1:all_systems(i_system)%nb_dofs,i_sol) = 0.d0
  end subroutine

  !> \brief Assemble the right hand side of systems of equations
  subroutine assemble_rhs(i_system, i_rhs, vector, edof2gdof)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4), intent(in) :: i_system
    !> [in] index of rhs in which to assemble
    integer(kind=4), intent(in) :: i_rhs
    !> [in] elementary vector to assemble
    real(kind=8)   , dimension(:), intent(in) :: vector
    !> [in] map of dof for assembling
    integer(kind=4), dimension(:), intent(in) :: edof2gdof

    call  assemble_elementary_vector(all_systems(i_system)%RHS(:,i_rhs), vector, edof2gdof)
    !print *,'------------------------'
    !print *,' i_system : ', i_system
    !print *,' rhs      : '
    !print *, all_systems(i_system)%RHS(:,i_rhs)
    !print *,'------------------------'

  end subroutine

  !> \brief Set an elementary left hand side of a system of equations
  subroutine set_elementary_lhs(i_system, i_element, matrix)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4)             , intent(in) :: i_system
    !> [in] index of elementary matrix to set
    integer(kind=4)             , intent(in) :: i_element
    !> [in] new values of the elementary matrix
    real(kind=8), dimension(:,:), intent(in) :: matrix

    call set_elementary_matrix(all_systems(i_system)%g_sys, i_element, matrix)

  end subroutine

  !> \brief set the driven dofs of a system
  subroutine set_system_drvdofs(i_system, drvdofs)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4),               intent(in) :: i_system
    !> [in] indices of driven dofs
    integer(kind=4), dimension(:), intent(in) :: drvdofs

    call set_drvdofs(all_systems(i_system)%g_sys,drvdofs)

  end subroutine

  !> \brief erase the driven dofs of a system
  subroutine erase_system_drvdofs(i_system)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4), intent(in) :: i_system

    call erase_drvdofs(all_systems(i_system)%g_sys)

  end subroutine

  !> \brief Set the right hand side of the system before solving
  subroutine set_system_rhs(i_system, i_rhs)
    implicit none
    !> [in] system index
    integer(kind=4), intent(in) :: i_system
    !> [in] index of RHS vector to use on the right hand side
    integer(kind=4), intent(in) :: i_rhs
    !

    call set_vector(all_systems(i_system)%g_sys,all_systems(i_system)%RHS(:,i_rhs))

  end subroutine

  !> \brief Generic function to apply driven dof
  subroutine apply_driven_dof_rhs(i_system, i_X, i_rhs, driv, drivdof)
    implicit none
    !> [in] system index
    integer(kind=4)              , intent(in)    :: i_system
    !> [in] index of X vector to use to compute the driven terms
    integer(kind=4)              , intent(in)    :: i_X
    !> [in] index of RHS vector to use on the right hand side
    integer(kind=4)              , intent(in)    :: i_rhs
    !> [in] value of driven dof to apply to rigth hand side
    real(kind=8)   , dimension(:), intent(in)    :: driv
    !> [in] index of driven dofs to apply to rigth hand side
    integer(kind=4), dimension(:), intent(in)    :: drivdof
    !
    integer(kind=4) :: nb_dofs, i_drvdof
    real(kind=8), dimension(:), allocatable :: tmp

    allocate(tmp(size(drivdof)))      
    do i_drvdof = 1, size(drivdof)
      tmp(i_drvdof) = driv(i_drvdof) - all_systems(i_system)%X(drivdof(i_drvdof),i_X) 
      all_systems(i_system)%RHS(drivdof(i_drvdof),i_rhs) = 0.d0
    end do

    call set_vector(all_systems(i_system)%g_sys,all_systems(i_system)%RHS(:,i_rhs))
    call set_drvvalues(all_systems(i_system)%g_sys,tmp)

    deallocate(tmp)

  end subroutine

  !> \brief Generic function to set the values of driven dof of a system
  subroutine set_system_drvvalues(i_system, driv)
    implicit none
    !> [in] system index
    integer(kind=4)              , intent(in)    :: i_system
    !> [in] value of driven dof to apply to rigth hand side
    real(kind=8)   , dimension(:), intent(in)    :: driv

    ! \todo: check size of driv with drvdofs or done within the g_sys ?
    call set_drvvalues(all_systems(i_system)%g_sys,driv)

  end subroutine

  !> \brief Generic solve function for one system
  subroutine solve(i_system, i_X, i_RHS)
    implicit none
    !> [in] system index
    integer(kind=4), intent(in) :: i_system
    !> [in] index of X vector in which to save the solution
    integer(kind=4), intent(in) :: i_X
    !> [in] index of RHS vector to use on the right hand side
    integer(kind=4), intent(in) :: i_RHS
    !
    integer(kind=4) :: nb_dofs, info

    nb_dofs = all_systems(i_system)%nb_dofs

    ! deja fait par apply_driven_... 
    !all_systems(i_system)%X(1:nb_dofs,i_X) = all_systems(i_system)%RHS(1:nb_dofs,i_RHS)

    ! \todo : check on info ?
    call solve_system( all_systems(i_system)%g_sys, all_systems(i_system)%X(1:nb_dofs,i_X), info)

    !print *,'------------------'
    !print *,' i_system : ', i_system
    !print *,' lhs      : '
    !print *, all_systems(i_system)%lhs%sym_band%V(:,:)
    !print *,' rhs      : ', i_rhs
    !print *, all_systems(i_system)%rhs(1:nb_dofs,i_rhs)
    !print *,' solution : ', i_X
    !print *, all_systems(i_system)%X(1:nb_dofs,i_X)
    !print *,'------------------'

  end subroutine

  !> \brief Return solve degrees of freedom to the model_handler to finish computation
  subroutine add_sol_contributions(i_system, dofs, i_list)
    implicit none
    !> [in] system index
    integer(kind=4),               intent(in)    :: i_system
    !> [in] the contributions computed by soe
    real(kind=8)   , dimension(:), intent(inout) :: dofs
    !> [in] list of solution vectors to get from soe
    integer(kind=4), dimension(:), intent(in)    :: i_list
    !
    integer(kind=4) :: nb_dofs, i

    nb_dofs = all_systems(i_system)%nb_dofs
    !print *,'------------------'
    !print *,' i_system : ', i_system
    !print *,' dof_ini  : '
    !print *, dofs(1:nb_dofs)
    !do i = 1, size(i_list)
    !  print *,' i_X      : ', i_list(i)
    !  print *, all_systems(i_system)%X(1:nb_dofs,i_list(i))
    !end do
    !print *,'------------------'
    dofs(1:nb_dofs) = dofs(1:nb_dofs) + sum(all_systems(i_system)%X(:,i_list), dim=2) 
  end subroutine

  !> \brief Return rhs contribution of freedom to the model_handler to compute residue norm
  subroutine add_rhs_contributions(i_system, residue, i_list)
    implicit none
    !> [in] system index
    integer(kind=4),               intent(in)    :: i_system
    !> [in] the residue stored in soe
    real(kind=8)   , dimension(:), intent(inout) :: residue
    !> [in] list of solution vectors to get from soe
    integer(kind=4), dimension(:), intent(in)    :: i_list
    !
    integer(kind=4) :: nb_dofs, i

    nb_dofs = all_systems(i_system)%nb_dofs
    residue(1:nb_dofs) = residue(1:nb_dofs) + sum(all_systems(i_system)%RHS(:,i_list), dim=2) 
  end subroutine

  !> \brief Add to a right hand side of a system of equations
  subroutine add_to_rhs(i_system, i_rhs, values)
    implicit none
    !> [in] index of system on which to work
    integer(kind=4), intent(in) :: i_system
    !> [in] index of rhs to reset
    integer(kind=4), intent(in) :: i_rhs
    !> [in] values to add to RHS field
    real(kind=8), dimension(:), intent(in) :: values
    !
    integer(kind=4) :: nb_dofs

    ! check on sizes ?
    nb_dofs = all_systems(i_system)%nb_dofs
    all_systems(i_system)%RHS(1:nb_dofs,i_rhs) = all_systems(i_system)%RHS(1:nb_dofs,i_rhs) + values(1:nb_dofs)

  end subroutine

  !> \brief Get the value of a solution
  subroutine get_sol(i_system, i_sol, values)
    implicit none
    !> [in] index of system
    integer(kind=4), intent(in) :: i_system
    !> [in] index of solution to get
    integer(kind=4), intent(in) :: i_sol
    !> [out] solution
    real(kind=8), dimension(:), intent(out):: values
    !
    integer(kind=4) :: nb_dofs

    ! check on sizes ?
    nb_dofs = all_systems(i_system)%nb_dofs
    values(1:nb_dofs) = all_systems(i_system)%X(1:nb_dofs,i_sol)

  end subroutine

  !> \brief Free memory allocated within the module
  subroutine clean_module()
    implicit none
    integer(kind=4) :: i

    nb_systems = 0

    if( allocated(all_systems) ) then
      do i = 1, size(all_systems)
        if( allocated(all_systems(i)%RHS) ) deallocate(all_systems(i)%RHS)
        if( allocated(all_systems(i)%X) )   deallocate(all_systems(i)%X)
        call erase_system(all_systems(i)%g_sys)
      end do

      deallocate(all_systems)
    end if

  end subroutine

  !> \brief Set the matrix storage type for all systems
  subroutine set_matrix_storage(type)
  implicit none
  character(len=8), intent(in) :: type

  matrix_storage = get_storage_id_from_type(type)

  end subroutine set_matrix_storage

end module soe
