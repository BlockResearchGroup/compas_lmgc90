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

!> manages linear systems: assembling, solving, etc
!> delegate solving to external routines 
module a_system

  use parameters

  use utilities , only : G_i_list, &
                         logmes  , &
                         faterr  , &
                         T_r8_vector, &
                         T_r8_matrix, &
                         bubble_sort

  use a_matrix

  use renum

  use SparseLinearAlgebra, only : G_sparse_matrix, &
                                  sparse_declare , &
                                  sparse_build   , &
                                  sparse_solve   , &
                                  sparse_erase   , &
                                  sparse_storage_available

  private 

  !> generic system which hide the technical aspects
  type, public :: G_system

    private

    ! specify how the system is stored :
    ! diagonal => lmgc90 (trivial)
    ! sparse   => MUMPS
    ! band     => LAPACK
    ! skyline  => lmgc90 (solveur FK)
    ! full     => LAPACK
    ! exploded => lmgc90 (gradient conjugue)

    integer(kind=4) :: i_storage = -99

    ! is it symmetric: 0 yes 1 no  -- convention C

    integer(kind=4) :: i_shape

    ! common part

    ! to manage assembling if any modification of elementary matrices or drvdof
    logical                                   :: is_matrix_modified
    ! to manage assembling if any modification of elementary vectors
    logical                                   :: is_vector_modified
    ! to manage the use of an external vector to add to rhs one
    logical                                   :: is_ext_vector_used

    ! number of elements
    integer(kind=4)                              :: nb_elements
    ! the order of each elementary matrices (aka nbdof_loc)
    integer(kind=4)  , dimension(:), allocatable :: orders
    ! map between elementary and global dofs
    type(G_i_list)   , dimension(:), allocatable :: edof2gdof
    ! elementary matrices
    type(T_r8_matrix), dimension(:), allocatable :: matrices_loc
    ! elementary vectors 
    type(T_r8_vector), dimension(:), allocatable :: vectors_loc

    ! number of degrees of freedom 
    integer(kind=4)                     :: nb_dof

    ! assembled vector of unknown
    real(kind=8), dimension(:), pointer :: vector => null()
    ! assembled vector of unknown
    real(kind=8), dimension(:), pointer :: ext_vector => null()

    ! drv dof
    integer(kind=4)                            :: nb_drvdof
    integer(kind=4), dimension(:), allocatable :: drvdofs
    real(kind=8)   , dimension(:), allocatable :: drvvalues


    ! specific part 

    ! Elementary system
    ! nothing
    
    ! Dense system
    ! a G_matrix
    type(G_matrix) :: matrix

    ! Sparse system

    !> list of adjacent dofs of a dof
    integer(kind=4), dimension(:,:), allocatable :: adj_dof
    !> correspondance map of adj_dof
    integer(kind=4), dimension(:)  , allocatable :: cc_adj_dof
    !> list of i indices
    integer(kind=4), dimension(:), pointer :: i_indices
    !> list of j indices
    integer(kind=4), dimension(:), pointer :: j_indices
    !> list of value
    real(kind=8)   , dimension(:), pointer :: val
    !> max value on the diagonal, necessary to apply boundary conditions
    real(kind=8) :: max_diag
    !> a G_sparse matrix
    type(G_sparse_matrix) :: sparse_matrix


  end type G_system

  !> Type to store a linked list where each element is a connectivity
  type, public :: T_link_connec
    !> next link
    type(T_link_connec), pointer :: n => null()
    !> connectivity of the element used by the solver
    integer(kind=4), dimension(:), pointer :: connec => null()
    !> a stored numbering of the connectivity (may be different from connec)
    integer(kind=4), dimension(:), pointer :: connec_ori => null()
  end type

  ! todo: pouvoir declarer de l'exterieur

  ! renumbering
  logical         :: with_renum=.TRUE.

  ! conjugate gradient parameters
  real(kind=8)    :: CG_tol    = 1d-14
  integer(kind=4) :: CG_min_it = 20

  !> scaling factor dedicated to driven dof
  real(kind=8) :: penal=1.e6


  ! from SparseLinearAlgebra
  public sparse_storage_available

  public initialize_system           , &
         change_shape                , &
         erase_system                , &

         set_elementary_matrix       , &
         erase_elementary_matrix     , &
         add_to_elementary_matrix    , &
         get_elementary_matrix       , &
         display_elementary_matrix   , &

         set_elementary_vector       , &
         erase_elementary_vector     , &
         add_to_elementary_vector    , &
         assemble_elementary_vector  , &
         set_vector                  , &
         get_vector                  , &
         erase_ext_vector            , &
         add_to_ext_vector           , &
         multiply_system             , &
         solve_system                , &
         build_system                , &
         set_drvdofs, set_drvvalues  , &
         erase_drvdofs         

  ! rm * : getter moisi pour global solver
  public get_nb_non_zero, &
         get_i_indices  , &
         get_j_indices  , &
         get_val

  private compute_perms                      , & 
          dense_assemble_elementary_matrices , &
          sparse_assemble_elementary_matrices, &
          assemble_elementary_vectors        , &
          apply_vector_drvdofs  

  contains

  !> \brief Initialize a G_system
  subroutine initialize_system(system, i_storage, i_shape, ccdof, connectivities, nb_max_adj)
    implicit none
    !> [in,out] system to initialize
    type(G_system) ,               intent(inout) :: system
    !> [in] type of system (dense, sparse...)
    integer(kind=4),               intent(in)    :: i_storage
    !> [in] shape type
    integer(kind=4),               intent(in)    :: i_shape
    !> [in] map between nodes and dofs
    integer(kind=4), dimension(:), intent(in)    :: ccdof
    !> [in] elements connectivity in linked list (first element is a dummy one storing the total number of elements)
    type(T_link_connec), pointer                 :: connectivities
    !> [in] maximum adjactent dofs of a dof (sparse system only)
    integer(kind=4),               intent(in)    :: nb_max_adj
    !
    integer(kind=4)                            :: nb_elements, nb_dof, nb_dof2, errare, info
    integer(kind=4)                            :: i_element, i_connec, i_node, i_ccdof, i_dof, idx
    integer(kind=4)                            :: i, j, glob_i, glob_j, nb_adj, nb_no_zero
    integer(kind=4), dimension(:), allocatable :: perm, inv_perm, edof2gdof
    character(len=80)                          :: cout
    character(len=27)                          :: IAM

    integer(kind=4),dimension(:),allocatable   :: itmp
    integer(kind=4)                            ::  id0

    type(T_link_connec), pointer :: connec
    !
          !123456789012345678901234567
    IAM = 'a_system::initialize_system'

    ! rm : to be a little bit more readable, each field of a system are allocated/initialized
    !      one after the other. Thus several loop on the elements are made.
    !      For efficiency's sake, everything could be made within the same loop on the elements.


    !print*,i_storage,i_shape
    !print*,ccdof
    !print*,connectivities 

 
    system%i_storage = i_storage
    system%i_shape   = i_shape

    if (i_storage /= i_diagonal .and. &
        i_storage /= i_sparse   .and. &        
        i_storage /= i_band     .and. &        
        i_storage /= i_skyline  .and. &        
        i_storage /= i_full     .and. &        
        i_storage /= i_exploded         ) then

      write(cout,'(A,1x,A)') 'unknow matrix storage type', get_matrix_storage_name_from_id(i_storage)
      call faterr(IAM,cout)
    end if

    if (i_shape /= i_sym .and. i_shape /= i_std) then
      write(cout,'(A,1x,A)') 'unknow matrix shape type', get_matrix_shape_name_from_id(i_shape)
      call faterr(IAM,cout)
    endif

    nb_elements = connectivities%connec(1)
    system%nb_elements = nb_elements

    system%is_matrix_modified = .true.
    system%is_vector_modified = .false.
    system%is_ext_vector_used = .false.

    ! ---------- orders ---------- 
    allocate(system%orders(nb_elements), stat=errare)
    if( errare /= 0 ) then
      write (cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating orders array of G_system'
      call faterr(IAM,cout)
    end if

    !rm connectivite: linked list
    ! First element : in field connec, the number of elements
    ! Then in each element field connec store the connectivity of the element
    connec => connectivities%n
    do i_element = 1, nb_elements
      nb_dof = 0
      do i_connec = 1, size(connec%connec)
        i_node = connec%connec(i_connec)
        nb_dof = nb_dof + ccdof(i_node+1) - ccdof(i_node)
      end do
      system%orders(i_element) = nb_dof
      connec => connec%n
    end do

    ! ---------- matrices_loc ---------- 
    allocate(system%matrices_loc(nb_elements), stat=errare)
    if( errare /= 0 ) then
      write (cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating matrices_loc array of G_system'
      call faterr(IAM,cout)
    end if

    do i_element = 1, nb_elements

      nb_dof = system%orders(i_element)
      if( system%i_storage == i_diagonal ) then
        nb_dof2 = 1
      else
        nb_dof2 = nb_dof
      end if

      allocate(system%matrices_loc(i_element)%rdata(nb_dof,nb_dof2), stat=errare)
      if( errare /= 0 ) then
        write (cout,'(A,I0,A,I0,A)') 'Error ', errare, ' while allocating index', i_element, &
                                     ' of matrices_loc of G_system'
        call faterr(IAM,cout)
      end if
      system%matrices_loc(i_element)%rdata(1:nb_dof,1:nb_dof2) = 0.d0

    end do

    ! ---------- vectors_loc ---------- 

    allocate(system%vectors_loc(nb_elements), stat=errare)
    if( errare /= 0 ) then
      write (cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating vectors_loc array of G_system'
      call faterr(IAM,cout)
    end if

    do i_element = 1, nb_elements

      nb_dof = system%orders(i_element)

      allocate(system%vectors_loc(i_element)%rdata(nb_dof), stat=errare)
      if( errare /= 0 ) then
        write (cout,'(A,I0,A,I0,A)') 'Error ', errare, ' while allocating index', i_element, &
                                     ' of vectors_loc of G_system'
        call faterr(IAM,cout)
      end if
      system%vectors_loc(i_element)%rdata(1:nb_dof) = 0.d0

    end do

    ! ---- edof2gdof ----
    allocate(system%edof2gdof(nb_elements), stat=errare)
    if( errare /= 0 ) then
      write (cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating edof2gdof array of G_system'
      call faterr(IAM,cout)
    end if

    connec => connectivities%n
    do i_element = 1, nb_elements

      nb_dof = system%orders(i_element)

      allocate(system%edof2gdof(i_element)%G_i(nb_dof), stat=errare)
      if( errare /= 0 ) then
        write (cout,'(A,I0,A,I0,A)') 'Error ', errare, ' while allocating index ', i_element, &
                                     ' of edof2gdof array of G_system'
        call faterr(IAM,cout)
      end if

      i_ccdof = 0
      do i_connec = 1, size(connec%connec)
        i_node = connec%connec(i_connec)
        do i_dof = 1, ccdof(i_node+1) - ccdof(i_node)
          i_ccdof = i_ccdof + 1
          system%edof2gdof(i_element)%G_i(i_ccdof) = ccdof(i_node) + i_dof
        end do
      end do

      connec => connec%n

    end do

    system%nb_dof = ccdof(size(ccdof))

    system%nb_drvdof = 0


    ! --------- vector -------

    allocate( system%vector(system%nb_dof) )


    ! ------ pointer ---------

    nullify(system%i_indices,system%j_indices,system%val)

    ! --------- matrix -------

    select case(system%i_storage)

    ! call to a_MATRIX
    case(i_diagonal, i_band, i_skyline, i_full)

      ! allocating dense part of the system


      nb_dof = system%nb_dof
      allocate( perm(nb_dof) )
      allocate( inv_perm(nb_dof) )
      !> compute
      call compute_perms(with_renum, perm, inv_perm, ccdof, connectivities)

      call G_declare(system%matrix, get_dense_shape_type_from_id(system%i_storage,system%i_shape), &
                     nb_dof, with_renum, perm, inv_perm                 )

      deallocate( perm )
      deallocate( inv_perm )

      do i_element = 1, nb_elements
        call G_settle(system%matrix,system%edof2gdof(i_element)%G_i)
      end do

      call G_build(system%matrix)


    ! call to MUMPS
    case(i_sparse) 

      nb_dof = system%nb_dof
      allocate(system%adj_dof(nb_dof,nb_max_adj), system%cc_adj_dof(nb_dof+1))
      system%adj_dof    = 0
      system%cc_adj_dof = 0

      ! for all degrees of freedom counting and storing connected ones
      do i_element = 1, connectivities%connec(1)
        do i = 1, size(system%edof2gdof(i_element)%G_i)
          glob_i = system%edof2gdof(i_element)%G_i(i)
          do j = 1, size(system%edof2gdof(i_element)%G_i)
            glob_j = system%edof2gdof(i_element)%G_i(j)

            ! si symetrique on garde la partie sup
            if (system%i_shape == i_sym .and. glob_j < glob_i) cycle 

            ! si le ddl est deja compte dans les adj
            if( any(system%adj_dof(glob_i,:) == glob_j) ) cycle

            ! on rajoute un adjacent, not cumulated count yet
            system%cc_adj_dof(glob_i+1) = system%cc_adj_dof(glob_i+1) + 1

            !? \todo (rm) this test should now be useless, keep it temporarly for debug purpose
            if ( system%cc_adj_dof(glob_i+1) > nb_max_adj) call FATERR(IAM,'nb adj max reached')

            system%adj_dof(glob_i,system%cc_adj_dof(glob_i+1)) = glob_j

          end do
        end do
      end do

      ! cumulating cc_adj_dof map values
      do i = 2, nb_dof+1
        system%cc_adj_dof(i) = system%cc_adj_dof(i) + system%cc_adj_dof(i-1)
      end do


      ! sorting adj_dof lines du plus grand au plus petit

      allocate(itmp(nb_max_adj))

      do i = 1, nb_dof
        call bubble_sort( system%adj_dof(i,1:nb_max_adj), nb_max_adj )

        itmp=0
        !print*,'--' 
        !print*,system%adj_dof(i,:)    
        id0 = minloc(system%adj_dof(i,1:nb_max_adj),dim=1)
        !print*,'--' 
        !print*,i,id0
        if (system%adj_dof(i,id0) == 0) then
          !print*,id0-1
          do j=1,id0-1
            itmp(j) = system%adj_dof(i,id0-j)    
          enddo
          !print*,'--'  
          !print*,itmp(:)

          system%adj_dof(i,1:id0-1) = itmp(1:id0-1)     
        else
          do j=1,nb_max_adj
            itmp(j) = system%adj_dof(i,1 + nb_max_adj - j)    
          enddo
          system%adj_dof(i,1:nb_max_adj) = itmp(1:nb_max_adj)
        endif
      end do

      deallocate(itmp)

      ! building i_indices, j_indices and val arrays
      nb_no_zero = system%cc_adj_dof(nb_dof+1)
      allocate( system%i_indices(nb_no_zero), system%j_indices(nb_no_zero), system%val(nb_no_zero) )

      idx = 0
      do i = 1, nb_dof
        nb_adj = system%cc_adj_dof(i+1) - system%cc_adj_dof(i)
        do j = 1, nb_adj
          idx = idx + 1
          system%i_indices(idx) = i
          system%j_indices(idx) = system%adj_dof(i,j)
        end do
      end do

      if (system%i_shape == i_sym) then
        call sparse_declare(system%sparse_matrix,system%nb_dof,system%cc_adj_dof(system%nb_dof+1), &
                            system%i_indices, system%j_indices, .TRUE. , info)
      else
        call sparse_declare(system%sparse_matrix,system%nb_dof,system%cc_adj_dof(system%nb_dof+1), &
                            system%i_indices, system%j_indices, .FALSE. , info)
      endif 

      if( info /= 0 ) then
        write(cout,'(A,I0)') 'Could not create sparse system, error: ', info
        call faterr(IAM,cout)
      end if
    ! local -> move it 
    case(i_exploded)

    case default

    end select    

  end subroutine

  !> \brief Change shape of a G_system
  subroutine change_shape(system, i_shape)
    implicit none
    !> [in,out] system to change
    type(G_system) , intent(inout) :: system    
    !> [in] new storage type
    integer(kind=4), intent(in)    :: i_shape 

    !
    integer(kind=4) :: nb_dofs   !number of dofs in the system
    integer(kind=4) :: i, i_element
    integer(kind=4), dimension(:), allocatable :: perm, inv_perm
    logical         :: with_renum

    character(len=22)                          :: IAM

          !1234567890123456789012
    IAM = 'a_system::change_shape'

    system%i_shape=i_shape

    select case(system%i_storage)
    case(i_diagonal, i_band, i_skyline, i_full)

      nb_dofs = system%nb_dof

      allocate( perm(nb_dofs) )
      allocate( inv_perm(nb_dofs) )

      if( associated(system%matrix%perm) ) then 
        with_renum = system%matrix%with_perm
        perm(1:nb_dofs)     = system%matrix%perm(1:nb_dofs)
        inv_perm(1:nb_dofs) = system%matrix%inv_perm(1:nb_dofs)
      else  
        with_renum=.false.
        perm     = (/ (i,i=1,nb_dofs) /)
        inv_perm = (/ (i,i=1,nb_dofs) /)
      end if

      call G_free(system%matrix)

      call G_declare(system%matrix, get_dense_shape_type_from_id(system%i_storage, system%i_shape), &
                   nb_dofs, with_renum, perm, inv_perm                )

      deallocate( perm )
      deallocate( inv_perm )

      do i_element = 1, system%nb_elements
        call G_settle(system%matrix,system%edof2gdof(i_element)%G_i)
      end do

      call G_build(system%matrix)

      system%is_matrix_modified = .true.


    case(i_sparse) 

      ! nothing to do

    case(i_exploded)

      ! nothing to do

    case default

      call faterr(IAM,'so strange')

    end select    

  end subroutine

  !> \brief Free memory allocated within a system
  subroutine erase_system(system)
    implicit none
    !> [in,out] system to erase
    type(G_system) :: system 
    !
    integer(kind=4) :: i
    !
    character(len=22) :: IAM
    !      1234567890123456789012
    IAM = 'a_system::erase_system'

    ! la partie commune

    if( allocated(system%orders) ) deallocate(system%orders)

    if( allocated(system%matrices_loc) ) then
      do i = 1,system%nb_elements 
        if( allocated(system%matrices_loc(i)%rdata) ) deallocate(system%matrices_loc(i)%rdata)
      end do
      deallocate(system%matrices_loc)
    end if

    if( allocated(system%vectors_loc) ) then
      do i = 1,system%nb_elements
        if( allocated(system%vectors_loc(i)%rdata) ) deallocate(system%vectors_loc(i)%rdata)
      end do
      deallocate(system%vectors_loc)
    end if

    if( allocated(system%edof2gdof) ) then
      do i = 1, system%nb_elements
        if( associated(system%edof2gdof(i)%G_i) ) then
          deallocate(system%edof2gdof(i)%G_i)
          nullify(system%edof2gdof(i)%G_i)
        end if
      end do
      deallocate(system%edof2gdof)
    end if

    if( associated(system%vector)     ) deallocate(system%vector)
    if( associated(system%ext_vector) ) deallocate(system%ext_vector)

    system%nb_elements = 0

    system%is_matrix_modified = .true.

    call erase_drvdofs(system)

    ! la partie stockage dependente

    select case(system%i_storage)

    case(i_diagonal, i_band, i_skyline, i_full)

      call G_free(system%matrix)

    case(i_sparse) 


      if (allocated(system%adj_dof)) deallocate(system%adj_dof)
      if (allocated(system%cc_adj_dof)) deallocate(system%cc_adj_dof)
      if (associated(system%i_indices)) deallocate(system%i_indices)
      if (associated(system%j_indices)) deallocate(system%j_indices)
      if (associated(system%val)) deallocate(system%val)

      call sparse_erase(system%sparse_matrix)

    case(i_exploded, -99)

      ! nothing to do

    case default

      call logmes("[WARNING:"//IAM//"] Undefined system",.true.)

    end select    

  end subroutine

  !> \brief Set the values of an elementary matrix
  subroutine set_elementary_matrix(system, i_element, matrix)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system    
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element 
    !> [in] the new values of the matrix
    real(kind=8), dimension(:,:), intent(in)    :: matrix    
    !
    integer(kind=4)   :: nb_dofs, nb_dofs2
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'a_system::set_elementary_matrix'

    ! checks
    nb_dofs = system%orders(i_element)
    if ( any(shape(matrix) /= shape(system%matrices_loc(i_element)%rdata)) ) then
      write(cout,'(A,2(I0,1x),A,2(I0,1x))') 'input elementary matrix of wrong shape, should be ', &
                                            shape(system%matrices_loc(i_element)%rdata), ' and is ', shape(matrix)
      call faterr(IAM,cout)
    end if
    nb_dofs2 = size(matrix,2)

    system%matrices_loc(i_element)%rdata(1:nb_dofs,1:nb_dofs2) = matrix(1:nb_dofs,1:nb_dofs2)

    system%is_matrix_modified = .true.

  end subroutine

  !> \brief Set the values of an elementary matrix to 0
  subroutine erase_elementary_matrix(system, i_element)
    implicit none 
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system    
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element 
    !
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'a_system::erase_elementary_matrix'

    system%matrices_loc(i_element)%rdata = 0.d0

    system%is_matrix_modified = .true.

  end subroutine

  !> \brief Set the values of an elementary matrix
  subroutine add_to_elementary_matrix(system, i_element, matrix, coeff)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system    
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element 
    !> [in] the new values of the matrix
    real(kind=8), dimension(:,:), intent(in)    :: matrix    
    !> [in] a coeff 
    real(kind=8), optional                      :: coeff     
    !
    integer(kind=4)   :: nb_dofs, i_dof
    character(len=80) :: cout
    character(len=31) :: IAM

    !      1234567890123456789012345678901234
    IAM = 'a_system::add_to_elementary_matrix'

    ! checks
    nb_dofs = system%orders(i_element)
    if ( any(shape(matrix) /= nb_dofs) ) then
      write(cout,'(A,2(I0,1x),A,2(I0,1x))') 'input elementary matrix of wrong shape, should be ', &
                                            (/nb_dofs,nb_dofs/), ' and is ', shape(matrix)
      call faterr(IAM,cout)
    end if

    if (present(coeff)) then
      if( system%i_storage == i_diagonal ) then
        do i_dof = 1, nb_dofs
          system%matrices_loc(i_element)%rdata(i_dof,1) = system%matrices_loc(i_element)%rdata(i_dof,1) + &
                                                          coeff*matrix(i_dof,i_dof)
        end do
      else
        system%matrices_loc(i_element)%rdata = system%matrices_loc(i_element)%rdata + coeff*matrix
      end if
    else
      if( system%i_storage == i_diagonal ) then
        do i_dof = 1, nb_dofs
          system%matrices_loc(i_element)%rdata(i_dof,1) = system%matrices_loc(i_element)%rdata(i_dof,1) + &
                                                          matrix(i_dof,i_dof)
        end do
      else
        system%matrices_loc(i_element)%rdata = system%matrices_loc(i_element)%rdata + matrix
      end if
    endif

    system%is_matrix_modified = .true.

  end subroutine

  !> \brief Get the values of an elementary matrix
  subroutine get_elementary_matrix(system, i_element, matrix)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element
    !> [out] the values of the matrix
    real(kind=8), dimension(:,:), intent(out)   :: matrix
    !
    integer(kind=4)   :: nb_dofs, nb_dofs2
    character(len=80) :: cout
    !                                      1234567890123456789012345678901
    character(len=31), parameter :: IAM = 'a_system::get_elementary_matrix'

    ! checks
    nb_dofs = system%orders(i_element)
    if ( any(shape(matrix) /= shape(system%matrices_loc(i_element)%rdata)) ) then
      write(cout,'(A,2(I0,1x),A,2(I0,1x))') 'input elementary matrix of wrong shape, should be ', &
                                            shape(system%matrices_loc(i_element)%rdata), ' and is ', shape(matrix)
      call faterr(IAM,cout)
    end if
    nb_dofs2 = size(matrix,2)

    matrix(1:nb_dofs,1:nb_dofs2) = system%matrices_loc(i_element)%rdata(1:nb_dofs,1:nb_dofs2)

  end subroutine

  !> \brief Display the values of an elementary matrix
  subroutine display_elementary_matrix(system, i_element)
    implicit none 
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system    
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element 
    !
    integer(kind=4)   :: nb_dofs
    character(len=20) :: fmt
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'a_system::display_elementary_matrix'
    fmt = ' ' 

    nb_dofs = system%orders(i_element)
    write(fmt,'(A,I0,A)') "(",nb_dofs,"(1x,D12.5))" 

    !print*,trim(fmt)

    write(*,trim(fmt)) system%matrices_loc(i_element)%rdata

  end subroutine

  !> \brief Set the values of an elementary vector
  subroutine set_elementary_vector(system, i_element, vector)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system    
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element 
    !> [in] the new values of the matrix
    real(kind=8), dimension(:)  , intent(in)    :: vector    
    !
    integer(kind=4)   :: nb_dofs
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'a_system::set_elementary_vector'

    ! checks
    nb_dofs = system%orders(i_element)
    if ( any(shape(vector) /= shape(system%vectors_loc(i_element)%rdata)) )  then
      write(cout,'(A,2(I0,1x),A,2(I0,1x))') 'input elementary vector of wrong shape, should be ', &
                            shape(system%vectors_loc(i_element)%rdata), ' and is ', shape(vector)
      call faterr(IAM,cout)
    end if

    system%vectors_loc(i_element)%rdata(1:nb_dofs) = vector(1:nb_dofs)

    system%is_vector_modified = .true.

  end subroutine

  !> \brief Set the values of an elementary vector to 0
  subroutine erase_elementary_vector(system, i_element)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element
    !
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'a_system::erase_elementary_vector'

    system%vectors_loc(i_element)%rdata = 0.d0

    system%is_vector_modified = .true.

  end subroutine

  !> \brief Add values to an elementary vector
  subroutine add_to_elementary_vector(system, i_element, vector, coeff)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system
    !> [in] element number
    integer(kind=4)             , intent(in)    :: i_element
    !> [in] the new values of the matrix
    real(kind=8), dimension(:)  , intent(in)    :: vector
    !> [in] a coeff
    real(kind=8), optional                      :: coeff
    !
    integer(kind=4)   :: nb_dofs
    character(len=80) :: cout
                                          ! 123456789012345678901234567890
    character(len=31), parameter :: IAM = 'a_system::set_elementary_vector'

    ! checks
    nb_dofs = system%orders(i_element)
    if ( any(shape(vector) /= shape(system%vectors_loc(i_element)%rdata)) )  then
      write(cout,'(A,2(I0,1x),A,2(I0,1x))') 'input elementary vector of wrong shape, should be ', &
                            shape(system%vectors_loc(i_element)%rdata), ' and is ', shape(vector)
      call faterr(IAM,cout)
    end if

    system%vectors_loc(i_element)%rdata(1:nb_dofs) = system%vectors_loc(i_element)%rdata(1:nb_dofs) + coeff*vector(1:nb_dofs)

    system%is_vector_modified = .true.

  end subroutine


  !> \brief Assemble an elementary vector 
  !> it designed to short circuit the system mechanism
  subroutine assemble_elementary_vector(vector, evector, edof2gdof)
    implicit none
    !> [in,out] vector on which to work
    real(kind=8), dimension(:)  , intent(inout) :: vector      
    !> [in] the elementary vector
    real(kind=8), dimension(:)  , intent(in)    :: evector   
    !> [in] map of dof for assembling
    integer(kind=4), dimension(:), intent(in)   :: edof2gdof 
    !
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'a_system::assemble_elementary_vector'

    call assemb_vector(vector,evector,edof2gdof)

  end subroutine

  !> \brief Set the values of the rhs vector
  subroutine set_vector(system, vector)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system    
    !> [in] the new values of the rhs vector
    real(kind=8), dimension(:)  , intent(in)    :: vector    
    !
    character(len=80) :: cout
    character(len=20) :: IAM
    !      12345678901234567890
    IAM = 'a_system::set_vector'

    if ( any(shape(vector) /= shape(system%vector)) ) then
       write(cout,'(A,I0,1x,A,I0)') 'input vector of wrong shape, should be ', &
                                        shape(system%vector), ' and is ', shape(vector)
       call faterr(IAM,cout)
    end if

    system%vector(1:system%nb_dof) = vector(1:system%nb_dof)
    system%is_vector_modified = .false.

  end subroutine

  !> \brief Get the values of the rhs vector
  subroutine get_vector(system, vector)
    implicit none
    !> [in] system on which to work
    type(G_system)              , intent(in)  :: system    
    !> [out] the rhs vector
    real(kind=8), dimension(:)  , intent(out) :: vector 
    !
    character(len=80) :: cout
    character(len=20) :: IAM
    !      12345678901234567890
    IAM = 'a_system::get_vector'

    if ( any(shape(vector) /= shape(system%vector)) ) then
       write(cout,'(A,I0,1x,A,I0,1x)') 'input vector of wrong shape, should be ', &
                                            shape(system%vector), ' and is ', shape(vector)
       call faterr(IAM,cout)
    end if

    vector(1:system%nb_dof) = system%vector(1:system%nb_dof)

  end subroutine

  !> \brief Set external RHS contributions to 0
  !> Allocate the ext_vector if needed
  subroutine erase_ext_vector(system)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system

    if( .not. associated(system%ext_vector) ) then
      allocate(system%ext_vector(system%nb_dof))
    end if

    system%ext_vector(1:system%nb_dof) = 0.d0
    system%is_ext_vector_used = .false.

  end subroutine

  !> \brief Add values of external RHS contributions on a section
  subroutine add_to_ext_vector(system, idx, vector)
    implicit none
    !> [in,out] system on which to work
    type(G_system)              , intent(inout) :: system
    !> [in] index from which to add vector in external RHS
    integer(kind=4)             , intent(in)    :: idx
    !> [in] the values to add to the rhs vector
    real(kind=8), dimension(:)  , intent(in)    :: vector
    !
    character(len=128) :: cout
    !                                      123456789012345678901234567
    character(len=27), parameter :: IAM = 'a_system::add_to_ext_vector'

    if ( idx+size(vector)-1 > system%nb_dof ) then
       write(cout,'(A,2(1x,I0),A,I0)') 'starting index and input vector size incoherent, are', &
                                    idx, size(vector), ' and should not exceed ', system%nb_dof
       call faterr(IAM,cout)
    end if

    system%ext_vector(idx:idx+size(vector)-1) = system%ext_vector(idx:idx+size(vector)-1) + vector(:)
    system%is_ext_vector_used = .true.

  end subroutine

  !> \brief declare drvdof to a system
  subroutine set_drvdofs(system,drvdofs)
    implicit none
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system  
    !> [in] list of primal driven dof
    integer(kind=4), dimension(:), intent(in)    :: drvdofs 
    !
    character(len=21) :: IAM
    !      123456789012345678901
    IAM = 'a_system::set_drvdofs'

    !print*, drvdofs

    if (allocated(system%drvdofs)) deallocate(system%drvdofs)
    if (allocated(system%drvvalues)) deallocate(system%drvvalues)
    
    system%nb_drvdof = size(drvdofs)
    allocate(system%drvdofs(system%nb_drvdof))
    allocate(system%drvvalues(system%nb_drvdof))

    system%drvdofs = drvdofs
    system%drvvalues = 0.d0

    system%is_matrix_modified = .true.

  end subroutine

  subroutine set_drvvalues(system,drvvalues)
    implicit none
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system    
    !> [in] values of primal driven dof
    real(kind=8)   , dimension(:), intent(in)    :: drvvalues 
    !
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'a_system::set_drvvalues'

    if (.not. allocated(system%drvvalues)) call faterr(IAM,'drvdofs not declared')
    
    system%drvvalues = drvvalues

  end subroutine

  subroutine erase_drvdofs(system)
    implicit none
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system  
    !
    character(len=21) :: IAM
    !      12345678901234567890123
    IAM = 'a_system::erase_drvdofs'

    if (allocated(system%drvdofs)) deallocate(system%drvdofs)
    if (allocated(system%drvvalues)) deallocate(system%drvvalues)
    
    system%nb_drvdof = 0

    system%is_matrix_modified = .true.

  end subroutine


  !> \brief Compute matrix vector product using matrix and vector of a system
  !> system%vector = system%matrix * system%vector
  subroutine multiply_system(system, with_bc)
    implicit none
    !> [in,out] system to use
    type(G_system)                       , intent(inout) :: system
    !> [in] (optional) is the matrix with boundary condition to be used
    logical, optional :: with_bc
    !
    real(kind=8), dimension(system%nb_dof) :: tmp    
    !
    character(len=25) :: IAM
    !      1234567890123456789012345
    IAM = 'a_system::multiply_system'

    ! check on size ? Or done in G_product_vector
    select case(system%i_storage)

    case(i_diagonal, i_band, i_skyline, i_full)

      if (system%is_matrix_modified) call dense_assemble_elementary_matrices(system)
      if (system%is_vector_modified) call assemble_elementary_vectors(system)

      call G_product_vector(system%matrix, system%vector)

    case(i_sparse, i_exploded)

      call exploded_matrix_vector_product(system,tmp,system%vector)
      system%vector = tmp

    case default

      call faterr(IAM,'so strange')
         
    end select
  end subroutine

  !> \brief Solve the linear system using assembled input vector for right hand side
  subroutine solve_system(system, sol, info)
    implicit none
    !> [in,out] system to use
    type(G_system)                       , intent(inout) :: system 
    !> [in,out] solution
    real(kind=8),dimension(system%nb_dof), intent(inout) :: sol    
    !> [out] info on resolution
    integer(kind=4)                      , intent(out)   :: info   

    select case(system%i_storage)
    case(i_diagonal, i_band, i_skyline, i_full)
      if (system%is_matrix_modified) call dense_assemble_elementary_matrices(system)
      if (system%is_vector_modified) call assemble_elementary_vectors(system)

      if (system%nb_drvdof /= 0) call apply_vector_drvdofs(system)

      call G_solve_linear_system(system%matrix, system%vector, info)

      sol = system%vector

    case(i_sparse)

      if (system%is_matrix_modified) call sparse_assemble_elementary_matrices(system)
      if (system%is_vector_modified) call assemble_elementary_vectors(system)
      if (system%is_ext_vector_used) then
        system%vector(1:system%nb_dof) = system%vector(1:system%nb_dof) + system%ext_vector(1:system%nb_dof)
        system%is_ext_vector_used = .false.
      end if

      if (system%nb_drvdof /= 0) call apply_vector_drvdofs(system)

      call sparse_solve(system%sparse_matrix, system%vector, info)

      sol = system%vector

    case(i_exploded)

      call solve_exploded_system(system , sol, info)

    case default
       
    end select
    
  end subroutine

!> \brief Build the linear system using assembled input vector for right hand side
  subroutine build_system(system)
    implicit none
    !> [in,out] system to use
    type(G_system)                       , intent(inout) :: system 

    select case(system%i_storage)
    case(i_diagonal, i_band, i_skyline, i_full)
      if (system%is_matrix_modified) call dense_assemble_elementary_matrices(system)
      if (system%is_vector_modified) call assemble_elementary_vectors(system)

      if (system%nb_drvdof /= 0) call apply_vector_drvdofs(system)

    case(i_sparse)

      if (system%is_matrix_modified) call sparse_assemble_elementary_matrices(system)
      if (system%is_vector_modified) call assemble_elementary_vectors(system)
      if (system%is_ext_vector_used) then
        system%vector(1:system%nb_dof) = system%vector(1:system%nb_dof) + system%ext_vector(1:system%nb_dof)
        system%is_ext_vector_used = .false.
      end if

      if (system%nb_drvdof /= 0) call apply_vector_drvdofs(system)

    case(i_exploded)


    case default
       
    end select
    
  end subroutine


  !> \brief Compute permutations for nodes renumbering
  subroutine compute_perms(with_renum, perm, inv_perm, ccdof, connectivities)
    implicit none
    !> [in] is renumbering to be used
    logical        , intent(in) :: with_renum 
    !> [out] nodes permutation (the array must be allocated beforehand)
    integer(kind=4), dimension(:), intent(out) :: perm     
    !> [out] reversed node permutation (the array must be allocated beforehand)
    integer(kind=4), dimension(:), intent(out) :: inv_perm 
    !> [in] map between the nodes and the dofs
    integer(kind=4), dimension(:), intent(in)  :: ccdof    
    !> [in] elements connectivity
    type(T_link_connec), pointer               :: connectivities
    !
    integer(kind=4) :: nb_dofs, nb_nodes, nb_elements, i_element, i_node, p_i_node
    integer(kind=4) :: iccdof, idof, nb_dof_node, idx
    integer(kind=4), dimension(:), allocatable :: tmp_perm, tmp_inv_perm
    type(T_link_connec), pointer               :: connec
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'a_system::compute_perms'

    nb_nodes = size(ccdof)-1
    nb_dofs  = ccdof(nb_nodes+1)

    if (with_renum) then

       nb_elements = connectivities%connec(1)

       allocate(tmp_perm(nb_nodes),tmp_inv_perm(nb_nodes))

       call initialize_rcm_renum(nb_nodes,nb_elements)
       connec => connectivities%n
       idx = 2
       do i_element = 1, nb_elements
         call settle_rcm_renum(i_element, size(connec%connec), connec%connec)
         connec => connec%n
       end do
       !fd  renum par algo rcm
       ! attention au sens
       ! new_num = inv_perm(old_num)
       ! old_num = perm(new_num)
       call get_rcm_renum(tmp_perm,tmp_inv_perm)

       !print*,'renum body:',ibdyty

       !fd construction des maps entre ddl

       ! construction de la map ddl_permute --> ddl_reel
       ! ddl_reel = perm(ddl_permute)

       ! indice du ddl permute
       iccdof = 0
       !on parcourt les noeuds permutes
       do p_i_node = 1, nb_nodes
         i_node = tmp_perm(p_i_node)
         nb_dof_node = ccdof(i_node+1) - ccdof(i_node)
         perm(iccdof+1:iccdof+nb_dof_node) = (/ (idof, idof = ccdof(i_node)+1, ccdof(i_node+1)) /)
         iccdof = iccdof + nb_dof_node
       end do

       ! construction de la map ddl_reel --> ddl_permute
       ! ddl_permute = inv_perm(ddl_reel)
       !on parcourt les ddls permutes
       do iccdof = 1, nb_dofs
         inv_perm(perm(iccdof)) = iccdof
       end do

       deallocate(tmp_perm,tmp_inv_perm)

    else
      perm = (/ (idof, idof = 1, nb_dofs) /)
      inv_perm = perm
    endif

  end subroutine

  !> \brief Assemble elementary matrices of a dense system
  subroutine dense_assemble_elementary_matrices(system)
    implicit none
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system      
    !
    integer(kind=4)   :: i_element, i_dof
    character(len=80) :: cout
    character(len=42) :: IAM
    !      123456789012345678901234567890123456789012
    IAM = 'a_system::dense_assemble_elementary_matrix'

    if( system%i_storage /= i_diagonal .and. &
        system%i_storage /= i_band     .and. &
        system%i_storage /= i_skyline  .and. &
        system%i_storage /= i_full              ) then
      write(cout, '(A,A)') 'cannot assemble a system of type ', get_matrix_storage_name_from_id(system%i_storage)
      call faterr(IAM,cout)
    end if

    call G_zero(system%matrix)
    do i_element = 1, system%nb_elements
      call G_assemb(system%matrix, system%matrices_loc(i_element)%rdata, system%edof2gdof(i_element)%G_i)
    end do
    call G_store(system%matrix)

    do i_dof = 1, system%nb_drvdof
      call G_apply_drvdof(system%matrix, system%drvdofs(i_dof))
    end do

    system%is_matrix_modified = .false.

  end subroutine

  !> \brief Assemble elementary matrices of a sparse system
  subroutine sparse_assemble_elementary_matrices(system)
    implicit none
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system      
    !
    integer(kind=4)   :: i_element, i, j, glob_i, glob_j, idx, info
    character(len=80) :: cout
    character(len=43) :: IAM
    !      1234567890123456789012345678901234567890123
    IAM = 'a_system::sparse_assemble_elementary_matrix'

    if( system%i_storage /= i_sparse ) then
      write(cout, '(A,A)') 'cannot assemble a system of type ', get_matrix_storage_name_from_id(system%i_storage)
      call faterr(IAM,cout)
    end if

    system%max_diag = 0.

    system%val = 0.d0

    do i_element = 1, system%nb_elements

      !print *,'assembling elementary matrix : ', i_element
      !print *,system%matrices_loc(i_element)%rdata

      do i = 1, size(system%edof2gdof(i_element)%G_i)
        glob_i = system%edof2gdof(i_element)%G_i(i)

        do j = 1, size(system%edof2gdof(i_element)%G_i)
          glob_j = system%edof2gdof(i_element)%G_i(j)

          ! cas symetrique
          if (system%i_shape == i_sym .and. glob_j < glob_i) cycle 

          ! leaving 0 on line/columns with driven dof
          !if( system%nb_drvdof > 0 ) then
          !  if( any(system%drvdofs==glob_j) ) cycle
          !end if

          !idx = minloc( system%adj_dof(glob_i,1:nb_max_adj), dim=1, mask=(system%adj_dof(glob_i,1:nb_max_adj)>=glob_j) )
          idx = minloc( system%adj_dof(glob_i,:), dim=1, mask=(system%adj_dof(glob_i,:)==glob_j) )

          idx = system%cc_adj_dof(glob_i) + idx

          system%val(idx) = system%val(idx) + system%matrices_loc(i_element)%rdata(i,j)

          if (glob_j == glob_i .and. system%val(idx) > system%max_diag) system%max_diag = system%val(idx)

        end do
      end do
    end do

    !print *, 'i_indices : '
    !print *, system%i_indices 
    !print *, 'j_indices : '
    !print *, system%j_indices 
    !print *, 'val : '
    !print *, system%val

    ! setting penal on diagonal where there are driven dof
    do i = 1, system%nb_drvdof
      glob_i = system%drvdofs(i)
      idx = minloc( system%adj_dof(glob_i,:), dim=1, mask=(system%adj_dof(glob_i,:)>=glob_i) )
      idx = idx + system%cc_adj_dof(glob_i)
      system%val(idx) = system%max_diag*penal
    end do

    !print *, 'val after driven dof: '
    !print *, system%val

    call sparse_build(system%sparse_matrix, system%val, info)    
    if( info /= 0 ) then
      write(cout,'(A,I0)') 'Could not build sparse system, error: ', info
      call faterr(IAM,cout)
    end if
    system%is_matrix_modified = .false.

  end subroutine

  !> \brief Assemble elementary vectors already loaded in the system
  subroutine assemble_elementary_vectors(system)
    implicit none
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system      
    !
    integer(kind=4)   :: i_element
    character(len=80) :: cout
    character(len=37) :: IAM
    !      1234567890123456789012345678901234567
    IAM = 'a_system::assemble_elementary_vectors'

    system%vector = 0.d0
    do i_element = 1, system%nb_elements
      call assemb_vector(system%vector,system%vectors_loc(i_element)%rdata,system%edof2gdof(i_element)%G_i)
    enddo

    system%is_vector_modified = .false.

  end subroutine

  !> \brief apply drvdof on rhs
  subroutine apply_vector_drvdofs(system)
    implicit none 
    !> [in,out] system on which to work
    type(G_system)               , intent(inout) :: system      
    !
    real(kind=8), dimension(:), allocatable :: tmp
    integer(kind=4)   :: i_drvdof
    character(len=30) :: IAM
    !      123456789012345678901234567890
    IAM = 'a_system::apply_vector_drvdofs'

    if (system%nb_drvdof == 0) return

    select case(system%i_storage)
    case(i_diagonal, i_band, i_skyline, i_full)

      allocate(tmp(system%nb_dof))

      ! Attention drvvalues contient la Xdriv - X (if X is the field to solve) on driven lines

      tmp = 0.d0
      do i_drvdof = 1, system%nb_drvdof
        tmp(system%drvdofs(i_drvdof)) = system%drvvalues(i_drvdof)
      end do

      call G_product_vector(system%matrix, tmp)

      system%vector = system%vector - tmp

      ! setting X to drvvalues on driven lines
      do i_drvdof = 1, system%nb_drvdof
        system%vector(system%drvdofs(i_drvdof)) = system%drvvalues(i_drvdof)
      end do

    case(i_sparse)
      ! setting X to drvvalues on driven lines
      do i_drvdof = 1, system%nb_drvdof
        system%vector(system%drvdofs(i_drvdof)) = system%max_diag*penal*system%drvvalues(i_drvdof)
      end do

    case(i_exploded) 

      ! nothing to do 

    case default 

      call faterr(IAM,'so strange')

    end select

  end subroutine


  !> matrix vector function for an element by element storage
  subroutine exploded_matrix_vector_product(system,x,vec)
    implicit none

    type(G_system)           ,intent(in) :: system
     real(kind=8),dimension(:),intent(out):: vec
    real(kind=8),dimension(:),intent(in) :: x
    !***
    integer :: i,j,i_element

    vec = 0.d0
    do i_element = 1, system%nb_elements   
      do i = 1,system%orders(i_element)
        do j = 1,system%orders(i_element)
          vec(system%edof2gdof(i_element)%G_i(i)) = vec(system%edof2gdof(i_element)%G_i(i)) + &
            system%matrices_loc(i_element)%rdata(i,j)*x(system%edof2gdof(i_element)%G_i(j))
        enddo
      enddo
    enddo  
  end subroutine

  !> Gradient conjugue preconditionne
  subroutine solve_exploded_system(system,sol,info)
    implicit none
    !> exploded system to solve
    type(G_system)                       , intent(inout) :: system
    !> rhs on input, solution on output
    real(kind=8),dimension(system%nb_dof), intent(inout) :: sol
    !> info on resolution (it)
    integer(kind=4)                      , intent(out)   :: info
    !*** 
    real(kind=8), dimension(:), allocatable :: vec
    !> calcul du residu
    real(kind=8), dimension(:), allocatable :: residu   
    !> direction de descente
    real(kind=8), dimension(:), allocatable :: descente 
    !> preconditionneur
    real(kind=8), dimension(:), allocatable :: C        
    real(kind=8)                            :: scal1     
    !> pas de descente  
    real(kind=8)                            :: pas      
    !> parametre d'orthogonalite des directions de descente
    real(kind=8)                            :: ortho    
    !> residu au carre 
    real(kind=8)                            :: rcarre   
    real(kind=8)                            :: ref

    integer(kind=4) :: i_element,idof,i_drvdof,it


    ! pas de solution
    info = 1 

    allocate(residu(system%nb_dof))
    allocate(descente(system%nb_dof))
    allocate(vec(system%nb_dof))
    allocate(C(system%nb_dof))

    ! preconditionneur
    ! diagonale de la matrice 
    C=0.d0
    do i_element = 1,system%nb_elements
       do idof = 1,system%orders(i_element)
         C(system%edof2gdof(i_element)%G_i(idof)) = C(system%edof2gdof(i_element)%G_i(idof)) + &
                                                system%matrices_loc(i_element)%rdata(idof,idof)
       end do
    end do
   
    ! matrice de preconditionnement inverse de diag
    do idof = 1,system%nb_dof
      C(idof) = 1.d0/C(idof)
    end do 


    ! C-norme du rhs
    ref=0.d0
    do idof = 1,system%nb_dof
      ref = ref + C(idof)*system%vector(idof)*system%vector(idof)
    end do

    if (ref < CG_tol ) then
      !print*,'C-norme de rhs is small: ',ref 
      ref = 1.d0
    endif 

    ! initialisation de la solution a 0
    ! a revoir si on donne une valeur initiale
    sol=0.d0 

    ! setting X to drvvalues on driven lines
    do i_drvdof = 1, system%nb_drvdof
      sol(system%drvdofs(i_drvdof)) = system%drvvalues(i_drvdof)
    end do
   
   call exploded_matrix_vector_product(system,sol,vec)

   ! initialisation du residu
   residu(:) = vec(:) - system%vector(:) 
   ! initialisation de la direction de descente
   do idof = 1,system%nb_dof
    descente(idof) = -C(idof)*residu(idof) 
   end do

    do i_drvdof = 1, system%nb_drvdof
      descente(system%drvdofs(i_drvdof)) = 0.d0
      residu(system%drvdofs(i_drvdof)) = 0.d0
    end do

    ! C-norme du residu
    rcarre=0.d0
    do idof = 1,system%nb_dof
      rcarre = rcarre + C(idof)*residu(idof)*residu(idof)
    end do

   !fd si jamais le second membre est nulle on arrete.
   !fs ca arrive si pb sans contact 
   if (rcarre == 0.d0) then
     info = 0
     return
   endif

   ! iterations du gc
   ! au max le nombre de dof
   do it=1,system%nb_dof

     scal1 = rcarre

     !print*,'descente:'
     !write(*,'(2(1x,D12.5))') descente

     call exploded_matrix_vector_product(system,descente,vec)

     ! pas optimal
     pas = - dot_product(residu,descente)/dot_product(descente,vec)  

     !print*,'pas: ',pas

     ! solution
     sol(:) = sol(:) + pas*descente(:)

     !print*,'sol:'
     !write(*,'(2(1x,D12.5))') sol

     residu(:) = residu(:) + pas*vec(:)

     do i_drvdof = 1, system%nb_drvdof
       residu(system%drvdofs(i_drvdof)) = 0.d0
     end do

     ! C-norme du residu
     rcarre=0.d0
     do idof = 1,system%nb_dof
       rcarre = rcarre + C(idof)*residu(idof)*residu(idof)
     end do

     !print*,'iter: ',it,' C-norme: ',rcarre

     ! test de convergence de la methode
     if (it > CG_min_it .and. rcarre .LE. CG_tol) exit

     ! orthogonalite des directions de descente
     ortho = rcarre / scal1
   
     ! direction de descente
     do idof = 1,system%nb_dof     
        descente(idof) = -C(idof)*residu(idof) + ortho*descente(idof)
     enddo   

   end do

   print*,'iter: ',it,' C-norme: ',rcarre

   info = -it 
   ! si DV
   if (it > system%nb_dof) info=1

   deallocate(vec)
   deallocate(residu)
   deallocate(descente)
   deallocate(C)

  end subroutine

  !> \brief Get the number of non zero terms in a sparse system
  function get_nb_non_zero(system)
    implicit none
    !> [in] system
    type(G_system), intent(in)  :: system         
    !> [return] number of non zero terms in sparse system
    integer(kind=4) :: get_nb_non_zero
    !
    character(len=80) :: cout
    character(len=25) :: IAM
    !      1234567890123456789012345
    IAM = 'a_system::get_nb_non_zero'

    if( system%i_storage /= i_sparse ) then
      write(cout, '(A,A)') 'cannot use this function on system of type ', get_matrix_storage_name_from_id(system%i_storage)
      call faterr(IAM,cout)
    end if

    get_nb_non_zero = system%cc_adj_dof(system%nb_dof+1)

  end function get_nb_non_zero

  !> \brief Get the i indices of a sparse system
  subroutine get_i_indices(system, i_list)
    implicit none
    !> [in] system
    type(G_system), intent(in)  :: system         
    !> [out] list of i_indices
    integer(kind=4), dimension(:), intent(out) :: i_list
    !
    character(len=80) :: cout
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'a_system::get_i_indices'

    if( system%i_storage /= i_sparse ) then
      write(cout, '(A,A)') 'cannot use this function on system of type ', get_matrix_storage_name_from_id(system%i_storage)
      call faterr(IAM,cout)
    end if

    if( system%cc_adj_dof(system%nb_dof+1) /= size(i_list) ) then
      write(cout, '(A,1x,I0,1x,I0)') 'input array of wrong size :', size(i_list), system%cc_adj_dof(system%nb_dof+1)
      call faterr(IAM,cout)
    end if

    i_list = system%i_indices

  end subroutine get_i_indices

  !> \brief Get the j indices of a sparse system
  subroutine get_j_indices(system, j_list)
    implicit none
    !> [in] system
    type(G_system), intent(in)  :: system         
    !> [out] list of j_indices
    integer(kind=4), dimension(:), intent(out) :: j_list
    !
    character(len=80) :: cout
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'a_system::get_j_indices'

    if( system%i_storage /= i_sparse ) then
      write(cout, '(A,A)') 'cannot use this function on system of type ', get_matrix_storage_name_from_id(system%i_storage)
      call faterr(IAM,cout)
    end if

    if( system%cc_adj_dof(system%nb_dof+1) /= size(j_list) ) then
      write(cout, '(A,1x,I0,1x,I0)') 'input array of wrong size :', size(j_list), system%cc_adj_dof(system%nb_dof+1)
      call faterr(IAM,cout)
    end if

    j_list = system%j_indices

  end subroutine get_j_indices

  !> \brief Get the val array of a sparse system
  subroutine get_val(system, val)
    implicit none
    !> [in] system
    type(G_system), intent(in)  :: system         
    !> [out] list of values
    real(kind=8), dimension(:), intent(out) :: val
    !
    character(len=80) :: cout
    character(len=17) :: IAM
    !      12345678901234567
    IAM = 'a_system::get_val'

    if( system%i_storage /= i_sparse ) then
      write(cout, '(A,A)') 'cannot use this function on system of type ', get_matrix_storage_name_from_id(system%i_storage)
      call faterr(IAM,cout)
    end if

    if( system%cc_adj_dof(system%nb_dof+1) /= size(val) ) then
      write(cout, '(A,1x,I0,1x,I0)') 'input array of wrong size :', size(val), system%cc_adj_dof(system%nb_dof+1)
      call faterr(IAM,cout)
    end if

    val = system%val

  end subroutine get_val

  function get_dense_shape_type_from_id(i_storage, i_shape)
    implicit none 
    integer(kind=4)  :: i_storage, i_shape

    character(len=8) :: get_dense_shape_type_from_id 

    if (i_storage == i_full .and. i_shape == i_sym) then

       get_dense_shape_type_from_id = 'sym_full'

    else if (i_storage == i_band .and. i_shape == i_sym) then

       get_dense_shape_type_from_id = 'sym_band'

    else if (i_storage == i_band .and. i_shape == i_std) then

       get_dense_shape_type_from_id = 'std_band'

    else if (i_storage == i_skyline .and. i_shape == i_sym) then

       get_dense_shape_type_from_id = 'sym_sky_'

    else if (i_storage == i_diagonal) then

       get_dense_shape_type_from_id = 'diagonal'

    else

       get_dense_shape_type_from_id = 'unknown'

    endif

  end function 

end module a_system
! 
