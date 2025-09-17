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

!> This module manage the database of the state variables (dof,fields,etc)
module state

  use paranoid_checks

  use overall
  use utilities

  use parameters

  implicit none

  private

  !> Definition of a state
  type, public :: T_State

    !> id of the modelHandle holding this state
    integer(kind=4) :: model_handle_id

    integer(kind=4) :: nb_dofs
    !> number of degrees of freedom
    integer(kind=4) :: nb_nodes
    !> number of nodes
    integer(kind=4) :: nb_elements
    !> number of elements

    !> coordinates of the nodes (nbDIME, nb_nodes, 3) (rm: like cooref, coor, coorTT)
    real(kind=8),  dimension(:,:,:), allocatable :: nodes

    !> Nodal fields necessary to solve the physical problem the number of fields
    !> is defined by the integrator depending on the model (X,V for NSDS mechanical system,
    !> X,V,A for a SDS mechanical syste, T for a thermal system...)
    !> Acces to data through nf_map
    real(kind=8), dimension(:), pointer :: nodal_fields => null()

    !> Two entries map to nodal_fields which gives back an index in nodal_fields array
    !> from a node index (first entry) and a nodal field index (second entry).
    !> It must be combined with nf_map_shift's data to get the real index
    integer(kind=4), dimension(:,:), allocatable :: nf_map

    !> Two entries map which gives for a nodal field index (first entry)
    !> and a time depth (second entry) the shift to add to nf_map value
    !> to get the index of the chosen nodal field in nodal_fields array
    type(G_i_list), dimension(:), allocatable :: nf_map_shift

    !>TODO: rassembler les deux maps precedentes en 
    !>      un type qui contient shift(time depth) + map(node)
    !>      un tableau d'objet de ce type indexe sur le nodal field index  

    !> Elementary contributions to the right hand side (Fint, Fext...)
    !> Access to data through erc_map
    real(kind=8), dimension(:),   allocatable :: el_rhs_contribs

    !> Map to acces el_rhs_contribs which gives back the beginning index
    !> in el_rhs_contribs array for an element index 
    integer(kind=4),   dimension(:), allocatable :: erc_map

    !> Two entries map which gives for an elementary rhs contribution index
    !> (first entry) and a time depth (second entry) the shift to add to
    !> erc_map value to get the index of the chosen elementary rhs contribution
    type(G_i_list), dimension(:), allocatable :: erc_map_shift

    !> All the internal fields defined by the model, acces to data through ef_map and ef_names
    !> First entry is: number of fields * number of quadrature points
    !> Second entry is: a storage depths (usually of 2)
    real(kind=8), dimension(:,:), allocatable :: elementary_fields 

    !> Array of maps to access elementary_fields for a given element index
    !> It gives access to a 2 entries map which gives back the beginning index
    !> in elementary_fields array for an field index (first entry) and a quadrature point index(second entry)
    type(T_i4_matrix), dimension(:), allocatable :: ef_map


    !> List the name of the elementary fields for a given element index
    type(T_string_array), dimension(:), allocatable :: ef_names

    !> for each node, the list of elements it belongs to
    type(G_i_list), dimension(:), allocatable :: node2elements

  end type T_State

  public get_new_state, &
         delete_state , &
         set_node     , &
         initialize   , &
         update       , &
         get_el_rhs_contrib_indices_boundary        , &
         get_element_el_rhs_contrib_indices_boundary, &
         get_nodal_field_index                      , &
         get_nodal_field_indices_boundary           , &
         get_node_nodal_field_indices_boundary

  contains

  !> \brief Create a new state
  function get_new_state(id, nb_nodes, nb_elements)
    implicit none
    !> [in] id of the modelHandle owning this state
    integer(kind=4), intent(in) :: id
    !> [in] number of nodes in the state
    integer(kind=4), intent(in) :: nb_nodes
    !> [in] number of elements in the state
    integer(kind=4), intent(in) :: nb_elements
    !> [return] a pointer on a new state
    type(T_state),   pointer    :: get_new_state
    !
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'state::create_new_state'

    ! new state has to be null...
    get_new_state => null()

    allocate(get_new_state)
    get_new_state%model_handle_id = id
    get_new_state%nb_nodes        = nb_nodes
    get_new_state%nb_elements     = nb_elements
 
    ! since get_new_state has just been allocated, its nodes field
    ! should not be allocated... paranoid check ?
    allocate(get_new_state%nodes(nbDIME,nb_nodes,3), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating nodes'
      call faterr(IAM,cout)
    end if

  end function

  !> \brief Delete a state
  subroutine delete_state(state)
    implicit none
    !> [in,out] a pointer on a state to free
    type(T_state), pointer :: state
    !
    integer(kind=4) :: i

    if( associated(state) ) then
      if( allocated(state%nodes)            ) deallocate(state%nodes)
      if( associated(state%nodal_fields)    ) then
        deallocate(state%nodal_fields)
        nullify(state%nodal_fields)
      end if
      if( allocated(state%el_rhs_contribs)  ) deallocate(state%el_rhs_contribs)
      if( allocated(state%elementary_fields)) deallocate(state%elementary_fields) 

      if( allocated(state%nf_map) ) deallocate(state%nf_map)
      if( allocated(state%nf_map_shift) ) then
        do i = 1, size(state%nf_map_shift)
          if( associated(state%nf_map_shift(i)%G_i) ) deallocate(state%nf_map_shift(i)%G_i)
        end do
        deallocate(state%nf_map_shift)
      end if

      if( allocated(state%erc_map) ) deallocate(state%erc_map)
      if( allocated(state%erc_map_shift) ) then
        do i = 1, size(state%erc_map_shift)
          if( associated(state%erc_map_shift(i)%G_i) ) deallocate(state%erc_map_shift(i)%G_i)
        end do
        deallocate(state%erc_map_shift)
      end if

      if( allocated(state%ef_map) ) then
        do i = 1, size(state%ef_map)
          if( allocated(state%ef_map(i)%idata) ) deallocate(state%ef_map(i)%idata)
        end do
        deallocate(state%ef_map)
      end if

      if( allocated(state%ef_names) ) then
        do i = 1, size(state%ef_names)
          if( allocated(state%ef_names(i)%sdata) ) deallocate(state%ef_names(i)%sdata)
        end do
        deallocate(state%ef_names)
      end if

      if( allocated(state%node2elements) ) then
        do i = 1, size(state%node2elements)
          if( associated(state%node2elements(i)%G_i) ) deallocate(state%node2elements(i)%G_i)
        end do
        deallocate(state%node2elements)
      end if

      deallocate(state)
      nullify(state)
    end if
 
  end subroutine

  !> \brief Set a node of a state
  subroutine set_node(state, i_node, coor)
    implicit none
    !> [in,out] state to set node
    type(T_state),              intent(inout) :: state
    !> [in] index of the node
    integer(kind=4),            intent(in)    :: i_node
    !> [in] coordinates of the node
    real(kind=8), dimension(:), intent(in)    :: coor
    !
    character(len=15) :: IAM
    !      123456789012345
    IAM = 'state::set_node'

    ! paranoid check of nodes array ?

    call paranoid_check_r8_size(IAM, state%nodes(:,i_node,1), nbDIME)
    call paranoid_check_r8_size(IAM, coor, nbDIME)

    state%nodes(1:nbDIME,i_node,1) = coor(1:nbDIME)

  end subroutine

  !> \brief Initialize a state
  !> Allocation of the fields arrays and their respective maps
  subroutine initialize(state, nf_td, nb_nf, erc_td, nb_erc, ef_td, nb_ef, ef_size)
    implicit none
    !> [in] state to initialize
    type(T_state),                   intent(inout) :: state
    !> [in] time depths of the nodal_fields
    integer(kind=4), dimension(:),   intent(in)    :: nf_td
    !> [in] number of nodal field
    integer(kind=4),                 intent(in)    :: nb_nf
    !> [in] time depths of the el_rhs_contribs
    integer(kind=4), dimension(:),   intent(in)    :: erc_td
    !> [in] number of elementary rhs contributions
    integer(kind=4),                 intent(in)    :: nb_erc
    !> [in] storage depth of the elementary_fields
    integer(kind=4), dimension(:),   intent(in)    :: ef_td
    !> [in] number of elementary fields (1 or 0)
    integer(kind=4),                 intent(in)    :: nb_ef
    !> [in] size of an elementary field
    integer(kind=4),                 intent(in)    :: ef_size
    !
    integer(kind=4)   :: nf_size, erc_size, errare
    integer(kind=4)   :: ref, nb_el, nb_no, i, i_td
    character(len=80) :: cout
    character(len=17) :: IAM
    !      12345678901234567
    IAM = 'state::initialize'
 
    ! helpfull reminder : 
    ! - nf stands for nodal_field (like X,V,T...)
    ! - erc stands for el_rhs_contribs (like Fext, Fint...)
    ! - ef stands for elementary_fields (all elementary fields internal, external, bulk...)

    nb_no = state%nb_nodes+1
    nb_el = state%nb_elements+1

    ! rm: really really (really?) paranoid:
    call paranoid_check_i4_size(IAM, nf_td , nb_nf)
    call paranoid_check_i4_size(IAM, erc_td, nb_erc)

    ! size of arrays computation
    nf_size  = dot_product(state%nf_map(nb_no,1:nb_nf), nf_td(1:nb_nf))
    erc_size = state%erc_map(state%nb_elements+1) * sum(erc_td(1:nb_erc))

    ! allocation of arrays nodal_fields, el_rhs_contribs and elementary_fields
    if( associated(state%nodal_fields) ) then
      call faterr(IAM, 'nodal_field already associated')
    end if
    allocate( state%nodal_fields(nf_size), stat=errare )
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating nodal_fields'
      call faterr(IAM,cout)
    end if
    state%nodal_fields(1:nf_size) = 0.D0

    if( allocated(state%el_rhs_contribs) ) then
      call faterr(IAM, 'el_rhs_contribs already allocated')
    end if
    allocate( state%el_rhs_contribs(erc_size), stat=errare )
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating el_rhs_contribs'
      call faterr(IAM,cout)
    end if
    state%el_rhs_contribs(1:erc_size) = 0.D0

    if( nb_ef > 0 ) then
      if( allocated(state%elementary_fields) ) then
        call faterr(IAM, 'elementary_field already allocated')
      end if
      allocate( state%elementary_fields(ef_size,ef_td(1)), stat=errare )
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating elementary_fields'
        call faterr(IAM,cout)
      end if
      state%elementary_fields(1:ef_size,1:ef_td(1)) = 0.D0
    end if


    ! nf_map_shift and erc_map_shif allocation and filling
    ! since there is no time dependency the map themselves
    ! have already been allocated and filled by the model
    ! handler before the call to this function

    if( allocated(state%nf_map_shift) ) then
      call faterr(IAM, 'nf_map_shift already allocated')
    end if
    allocate( state%nf_map_shift(nb_nf), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating nf_map_shift'
      call faterr(IAM,cout)
    end if

    ref = 0
    do i = 1, nb_nf
      allocate(state%nf_map_shift(i)%G_i(nf_td(i)))
      do i_td = 1, nf_td(i)
        state%nf_map_shift(i)%G_i(i_td) = ref
        ref = ref + state%nf_map(nb_no,i)
      end do
    end do


    if( allocated(state%erc_map_shift) ) then
      call faterr(IAM, 'erc_map_shift already allocated')
    end if
    allocate( state%erc_map_shift(nb_erc), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating erc_map_shift'
      call faterr(IAM,cout)
    end if

    ref = 0
    do i = 1, nb_erc
      allocate(state%erc_map_shift(i)%G_i(erc_td(i)))
      do i_td = 1, erc_td(i)
        state%erc_map_shift(i)%G_i(i_td) = ref
        ref = ref + state%erc_map(nb_el)
      end do
    end do

  end subroutine

  !> \brief Update a state
  !> Shift on time depths of every vectors
  subroutine update(state, nf_td, nb_nf, erc_td, nb_erc, ef_td, nb_ef)
    implicit none
    !> [in] state to update
    type(T_state),                   intent(inout) :: state
    !> [in] time depths of the nodal_fields
    integer(kind=4), dimension(:),   intent(in)    :: nf_td
    !> [in] number of nodal field
    integer(kind=4),                 intent(in)    :: nb_nf
    !> [in] time depths of the el_rhs_contribs
    integer(kind=4), dimension(:),   intent(in)    :: erc_td
    !> [in] number of elementary rhs contributions
    integer(kind=4),                 intent(in)    :: nb_erc
    !> [in] storage depth of the elementary_fields
    integer(kind=4), dimension(:),   intent(in)    :: ef_td
    !> [in] number of elementary fields (1 or 0)
    integer(kind=4),                 intent(in)    :: nb_ef
    !
    integer(kind=4)   :: i_nf, i_erc, i_ef, i_begin, i_end, nf_dof, erc_dof
    character(len=13) :: IAM
    !      1234567890123
    IAM = 'state::update'

    ! maybe use of eoshift instead of cshift to initialize new value to 0

    call paranoid_check_i4_size(IAM, nf_td , nb_nf)
    call paranoid_check_i4_size(IAM, erc_td, nb_erc)

    do i_nf = 1, nb_nf
      if( nf_td(i_nf) == 1 ) cycle
      i_begin = state%nf_map(1,i_nf)                + state%nf_map_shift(i_nf)%G_i(1) + 1
      i_end   = state%nf_map(state%nb_nodes+1,i_nf) + state%nf_map_shift(i_nf)%G_i(nf_td(i_nf))
      nf_dof  = state%nf_map(state%nb_nodes+1,i_nf)

      state%nodal_fields(i_begin:i_end) = cshift(state%nodal_fields(i_begin:i_end), shift=-nf_dof)
    end do

    do i_erc = 1, nb_erc
      if( erc_td(i_erc) == 1 ) cycle
      i_begin = state%erc_map(1)                   + state%erc_map_shift(i_erc)%G_i(1) + 1
      i_end   = state%erc_map(state%nb_elements+1) + state%erc_map_shift(i_erc)%G_i(erc_td(i_erc))
      erc_dof = state%erc_map(state%nb_elements+1)

      state%el_rhs_contribs(i_begin:i_end) = eoshift(state%el_rhs_contribs(i_begin:i_end), shift=-erc_dof)
    end do

    if( allocated(state%elementary_fields) ) then
      state%elementary_fields = cshift(state%elementary_fields, shift=-1, dim=2)
    end if

  end subroutine

  !> \brief Get the boundary indices of an a given elementary rhs contribution at a given time depths
  subroutine get_el_rhs_contrib_indices_boundary(i_begin, i_end, state, i_erc, i_time_depth)
    implicit none
    !> [in] state
    type(T_state),   intent(in)  :: state
    !> [in] index of the rhs contribution to get
    integer(kind=4), intent(in)  :: i_erc
    !> [in] time depth index (1 current, 2 previous...)
    integer(kind=4), intent(in)  :: i_time_depth
    !> [out] beginning index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_begin
    !> [out] ending index of the desired part of the elementary rhs contribution
    integer(kind=4), intent(out) :: i_end

    ! checks ?

    i_begin = state%erc_map(1)                   + state%erc_map_shift(i_erc)%G_i(i_time_depth) + 1
    i_end   = state%erc_map(state%nb_elements+1) + state%erc_map_shift(i_erc)%G_i(i_time_depth)

  end subroutine

  !> \brief Get the boundary indices of an element of a given elementary rhs contribution at a given time depths
  subroutine get_element_el_rhs_contrib_indices_boundary(i_begin, i_end, state, i_erc, i_element, i_time_depth)
    implicit none
    !> [in] state
    type(T_state),   intent(in)  :: state
    !> [in] index of the rhs contribution to get
    integer(kind=4), intent(in)  :: i_erc
    !> [in] index of the element to get
    integer(kind=4), intent(in)  :: i_element
    !> [in] time depth index (1 current, 2 previous...)
    integer(kind=4), intent(in)  :: i_time_depth
    !> [out] beginning index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_begin
    !> [out] ending index of the desired part of the elementary rhs contribution
    integer(kind=4), intent(out) :: i_end

    ! checks ?

    i_begin = state%erc_map(i_element)   + state%erc_map_shift(i_erc)%G_i(i_time_depth) + 1
    i_end   = state%erc_map(i_element+1) + state%erc_map_shift(i_erc)%G_i(i_time_depth)

  end subroutine

  !> \brief Get the beginning index of a given nodal field at a given time depths
  subroutine get_nodal_field_index(i_begin, state, i_nf, i_time_depth)
    implicit none
    !> [in] state
    type(T_state),   intent(in)  :: state
    !> [in] index of the nodal field to get
    integer(kind=4), intent(in)  :: i_nf
    !> [in] time depth index (1 current, 2 previous...)
    integer(kind=4), intent(in)  :: i_time_depth
    !> [out] beginning index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_begin

    ! checks ?

    i_begin = state%nf_map(1,i_nf) + state%nf_map_shift(i_nf)%G_i(i_time_depth)

  end subroutine

  !> \brief Get the boundary indices of a given nodal field at a given time depths
  subroutine get_nodal_field_indices_boundary(i_begin, i_end, state, i_nf, i_time_depth)
    implicit none
    !> [in] state
    type(T_state),   intent(in)  :: state
    !> [in] index of the nodal field to get
    integer(kind=4), intent(in)  :: i_nf
    !> [in] time depth index (1 current, 2 previous...)
    integer(kind=4), intent(in)  :: i_time_depth
    !> [out] beginning index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_begin
    !> [out] ending index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_end

    ! checks ?

    i_begin = state%nf_map(1,i_nf)                + state%nf_map_shift(i_nf)%G_i(i_time_depth) + 1
    i_end   = state%nf_map(state%nb_nodes+1,i_nf) + state%nf_map_shift(i_nf)%G_i(i_time_depth)

  end subroutine

  !> \brief Get the boundary indices of a given node of a nodal field at a given time depths
  subroutine get_node_nodal_field_indices_boundary(i_begin, i_end, state, i_nf, i_node, i_time_depth)
    implicit none
    !> [in] state
    type(T_state),   intent(in)  :: state
    !> [in] index of the nodal field to get
    integer(kind=4), intent(in)  :: i_nf
    !> [in] index of the node of the nodal field to get
    integer(kind=4), intent(in)  :: i_node
    !> [in] time depth index (1 current, 2 previous...)
    integer(kind=4), intent(in)  :: i_time_depth
    !> [out] beginning index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_begin
    !> [out] ending index of the desired part of the nodal field
    integer(kind=4), intent(out) :: i_end

    ! checks ?

    i_begin = state%nf_map(i_node,i_nf)   + state%nf_map_shift(i_nf)%G_i(i_time_depth) + 1
    i_end   = state%nf_map(i_node+1,i_nf) + state%nf_map_shift(i_nf)%G_i(i_time_depth)

  end subroutine

end module state
