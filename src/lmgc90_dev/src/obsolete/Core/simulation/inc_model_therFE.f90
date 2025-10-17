
!> \brief Set some data of a bulk of a meca finite element
subroutine set_bulk_of_therFE(model, blmID, i_model, i_blmID, ndofs_by_element)
  use a_therEF, only : get_nb_in_therEF, &
                       get_N_DOF_therEF
  implicit none
  !> [in] model name
  character(len=5), intent(in)  :: model
  !> [in] element type name
  character(len=5), intent(in)  :: blmID
  !> [out] model id
  integer(kind=4),  intent(out) :: i_model
  !> [out] element type id
  integer(kind=4),  intent(out) :: i_blmID
  !> [out] number of dofs in the element
  integer(kind=4),  intent(out) :: ndofs_by_element

  i_model = get_model_nb(model)
  i_blmID = get_nb_in_therEF(blmID)
  ! \todo : revoir ce mecanisme a cause des elements avec un nombre de ddl variable par noeud
  ndofs_by_element = get_N_DOF_therEF(modelz(i_model)%ID)

end subroutine

!> \brief Set the map between a node and a nodal field index of a mechanical finite element
subroutine set_cc_nodal_field_therFE(id, nb_nodes, cc_nodal_field)
  use a_therEF, only : get_N_DOF_of_NODE_therEF
  implicit none
  !> [in] index of the modelization
  integer(kind=4), intent(in) :: id
  !> [in] number of nodes in the modelization
  integer(kind=4), intent(in) :: nb_nodes
  !> [in,out] map of indices between nodes and nodal field
  integer(kind=4), dimension(:), intent(inout) :: cc_nodal_field
  !
  integer(kind=4) :: i_element, i_node, n_dof, i_connec, i_model

  do i_element = 1, all_modelizations(id)%nb_elements

    i_model = all_modelizations(id)%elements(i_element)%i_model

    ! \todo : FD -> a reprendre, il faudrait que un noeud sache a quel noeud de l'element il appartient
    !               en lien avec la grouille 'todo' de l'initialization
    do i_connec = 1, all_modelizations(id)%elements(i_element)%connec_size
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i_connec)
      n_dof  = get_N_DOF_of_NODE_therEF(modelz(i_model)%ID, i_node)
      cc_nodal_field(i_node+1) = max( cc_nodal_field(i_node+1), n_dof )
    end do

  end do

  do i_node = 1, nb_nodes
    cc_nodal_field(i_node+1) = cc_nodal_field(i_node+1) + cc_nodal_field(i_node)
  end do

end subroutine

!> \brief Set the ef_names and ef_map of a mechanical finite element
subroutine set_cc_elementary_fields_therFE(id, ef_names, ef_map, ef_size)
  use a_therEF, only : get_N_GP_therEF
  implicit none
  !> [in] index of the modelization
  integer(kind=4)                   , intent(in)    :: id
  !> [in,out] map between quadrature point and field index to fields
  type(T_i4_matrix),    dimension(:), intent(inout) :: ef_map
  !> [in,out] list of name of fields
  type(T_string_array), dimension(:), intent(inout) :: ef_names
  !> [out] total size of the elementary field array
  integer(kind=4)                   , intent(out)   :: ef_size
  !
  integer(kind=4) :: nb_elements, i_element, i_model, i_behav
  integer(kind=4) :: nb_dof, nb_ef, nb_bf, nbf, nb_gp, i_gp, i_f
  integer(kind=4) :: total_nbf, nb_external, nb_internal, errare
  character(len=80) :: cout
  character(len=49) :: IAM
  !    1234567890123456789012345678901234567890123456789
  IAM='inc_model_therFE::set_cc_elementary_fields_therFE'

  nb_elements = all_modelizations(id)%nb_elements

  do i_element = 1, nb_elements

    i_model = all_modelizations(id)%elements(i_element)%i_model
    i_behav = all_modelizations(id)%elements(i_element)%i_behav

    nb_dof = 0
    nb_external = modelz(i_model)%nb_external_variables
    nb_internal = modelz(i_model)%nb_internal_variables
    nb_ef = get_external_field_nb( i_model )
    nb_bf = get_bulk_field_nb( i_behav )

    nbf = 3 ! grad, flux, internal
    nb_gp = get_N_GP_therEF( modelz(i_model)%ID )
    total_nbf = nbf + nb_ef + nb_bf

    if( allocated(ef_names(i_element)%sdata) ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'sdata field of ef_names in element', i_element, &
                                              'of modelization number', id, 'already allocated'
      call faterr(IAM,cout)
    end if

    allocate( ef_names(i_element)%sdata(total_nbf), stat=errare )
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'Error ', errare, &
                                                    ' while allocating sdata field of ef_names in element', &
                                                    i_element, 'of modelization number', id
      call faterr(IAM,cout)
    end if

    ef_names(i_element)%sdata(1) = 'grad'
    nb_dof = nb_dof + nb_external
    ef_names(i_element)%sdata(2) = 'flux'
    nb_dof = nb_dof + nb_external

    ! this is done even if nb_internal is 0
    ef_names(i_element)%sdata(3) = 'internal'
    nb_dof = nb_dof + nb_internal

    do i_f = 1, nb_ef
      ef_names(i_element)%sdata(nbf+i_f) = get_external_field_name(i_model,i_f)
      nb_dof = nb_dof + 1 ! \todo assumes fields are scalar ; if fields may be vector or tensor... to modify ! 
    end do

    do i_f = 1,nb_bf
      ef_names(i_element)%sdata(nbf+nb_ef+i_f) = get_bulk_field_name(i_behav,i_f)
      nb_dof = nb_dof + 1 ! \todo assumes fields are scalar ; if fields may be vector or tensor... to modify !
    end do

    ! filling the map between the quadrature points index and field index of the element
    if( allocated(ef_map(i_element)%idata) ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'idata field of ef_map in element', i_element, &
                                              'of modelization number', id, 'already allocated'
      call faterr(IAM,cout)
    end if

    allocate( ef_map(i_element)%idata(total_nbf+1,nb_gp), stat=errare )
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'Error ', errare, ' while allocating idata field of ef_map in element', &
                                                    i_element, 'of modelization number', id
      call faterr(IAM,cout)
    end if

    !ef_map(i_element)%idata(1:total_nbf+1,1:nb_gp) = 0
    if( i_element == 1 ) then
      ef_map(i_element)%idata(1,1) = 0
    else
      ef_map(i_element)%idata(1,1) = ef_map(i_element-1)%idata(total_nbf+1,nb_gp)
    end if

    do i_gp = 1, nb_gp
      ! strain/flux
      if (i_gp > 1) ef_map(i_element)%idata(1,i_gp) = ef_map(i_element)%idata(total_nbf+1,i_gp-1)
      ! stress/grad
      ef_map(i_element)%idata(2,i_gp) = ef_map(i_element)%idata(1,i_gp) + nb_external
      ! internal
      ef_map(i_element)%idata(3,i_gp) = ef_map(i_element)%idata(2,i_gp) + nb_external
      ! premier external+bulk field ou total_nbf+1 si pas de external+bulk field
      ef_map(i_element)%idata(4,i_gp) = ef_map(i_element)%idata(3,i_gp) + nb_internal

      do i_f = 1, nb_ef+nb_bf
        ! \todo assumes fields are scalar ; if fields may be vector or tensor... to modify !
        ef_map(i_element)%idata(4+i_f,i_gp) = ef_map(i_element)%idata(3+i_f,i_gp) + 1
      end do
    end do
  end do

  i_model = all_modelizations(id)%elements(nb_elements)%i_model
  i_behav = all_modelizations(id)%elements(nb_elements)%i_behav

  nb_ef = get_external_field_nb( i_model )
  nb_bf = get_bulk_field_nb( i_behav )
  nbf = 3
  total_nbf = nbf + nb_ef + nb_bf

  nb_gp = get_N_GP_therEF( modelz(i_model)%ID )

  ef_size = ef_map(nb_elements)%idata(total_nbf+1,nb_gp)

end subroutine

!> \brief Computation of elementary capacity matrices for a thermal finite element
subroutine comp_capacity_ther_FE(id, i_mat, dt, nodes, temp, temp_map, fields, ef_map, ef_names, fint, fint_map)
  use a_therEF, only : compute_elementary_capacity2
  implicit none
  !> [in] id of the modelization
  integer(kind=4),                      intent(in)  :: id
  !> [in] index of mass matrix in M_elem
  integer(kind=4),                      intent(in)  :: i_mat
  !> [in] time differential
  real(kind=8),                         intent(in)  :: dt
  !> [in] the position of the nodes
  real(kind=8),         dimension(:,:), intent(in)  :: nodes
  !> [in] temperature of the nodes
  real(kind=8),         dimension(:)  , intent(in)  :: temp
  !> [in] map to acces temp
  integer(kind=4),      dimension(:)  , intent(in)  :: temp_map
  !> [in] fields values at quadrature points
  real(kind=8),         dimension(:)  , intent(in)  :: fields
  !> [in] map to acces quadrature point fields
  type(T_i4_matrix),    dimension(:)  , intent(in)  :: ef_map
  !> [in] map to get quadrature point fields' name
  type(T_string_array), dimension(:)  , intent(in)  :: ef_names
  !> [out] vector in which to store internal fluxes
  real(kind=8),         dimension(:)  , intent(out) :: fint
  !> [in] map to acces internal fluxes vector by element
  integer(kind=4),      dimension(:)  , intent(in)  :: fint_map
  !
  real(kind=8), dimension(:)  , allocatable :: DT_ele
  real(kind=8), dimension(:,:), allocatable :: X
  integer(kind=4) :: i_node, i_element, elem_dofs, nb_nodes, idof, i1, i2, i

  do i_element = 1, all_modelizations(id)%nb_elements

    elem_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element
    nb_nodes  = all_modelizations(id)%elements(i_element)%connec_size
    idof      = elem_dofs / nb_nodes

    allocate(X(nbDIME,nb_nodes),DT_ele(elem_dofs))

    do i = 1, nb_nodes
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i)
      DT_ele((i-1)*idof+1:i*idof) = temp(temp_map(i_node)-temp_map(1)+1:temp_map(i_node+1)-temp_map(1))
    end do
    X = nodes(1:nbDIME,all_modelizations(id)%elements(i_element)%connectivity)

    i1 = fint_map(i_element) + 1
    i2 = fint_map(i_element+1)

    call compute_elementary_capacity2( all_modelizations(id)%elements(i_element)%i_blmID                                     , &
                                       all_modelizations(id)%elements(i_element)%ppsnb                                       , &
                                       dt, X, DT_ele, fields, ef_map(i_element)%idata, ef_names(i_element)%sdata, fint(i1:i2), &
                                       all_modelizations(id)%elements(i_element)%M_elem(1:elem_dofs,1:elem_dofs,i_mat)  )

    deallocate(X, DT_ele)

  end do

end subroutine

!> \brief Computation of elementary conductivity matrices for a thermal finite element
subroutine comp_conductivity_ther_FE(id, i_mat, dt, nodes, temp, temp_map, fields, ef_map, ef_names, fint, fint_map)
  use a_therEF, only : compute_elementary_conductivity2
  implicit none
  !> [in] id of the modelization
  integer(kind=4),                      intent(in)  :: id
  !> [in] index of mass matrix in M_elem
  integer(kind=4),                      intent(in)  :: i_mat
  !> [in] time differential
  real(kind=8),                         intent(in)  :: dt
  !> [in] the position of the nodes
  real(kind=8),         dimension(:,:), intent(in)  :: nodes
  !> [in] temperature of the nodes
  real(kind=8),         dimension(:)  , intent(in)  :: temp
  !> [in] map to acces temp
  integer(kind=4),      dimension(:)  , intent(in)  :: temp_map
  !> [in] fields values at quadrature points
  real(kind=8),         dimension(:)  , intent(in)  :: fields
  !> [in] map to acces quadrature point fields
  type(T_i4_matrix),    dimension(:)  , intent(in)  :: ef_map
  !> [in] map to get quadrature point fields' name
  type(T_string_array), dimension(:)  , intent(in)  :: ef_names
  !> [out] vector in which to store internal fluxes
  real(kind=8),         dimension(:)  , intent(out) :: fint
  !> [in] map to acces internal fluxes vector by element
  integer(kind=4),      dimension(:)  , intent(in)  :: fint_map
  !
  real(kind=8), dimension(:)  , allocatable :: T_ele
  real(kind=8), dimension(:,:), allocatable :: X
  integer(kind=4) :: i_node, i_element, elem_dofs, nb_nodes, idof, i1, i2, i

  do i_element = 1, all_modelizations(id)%nb_elements

    elem_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element
    nb_nodes  = all_modelizations(id)%elements(i_element)%connec_size
    idof      = elem_dofs / nb_nodes

    allocate(X(nbDIME,nb_nodes),T_ele(elem_dofs))

    do i = 1, nb_nodes
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i)
      T_ele((i-1)*idof+1:i*idof) = temp(temp_map(i_node)-temp_map(1)+1:temp_map(i_node+1)-temp_map(1))
    end do
    X = nodes(1:nbDIME,all_modelizations(id)%elements(i_element)%connectivity)

    i1 = fint_map(i_element) + 1
    i2 = fint_map(i_element+1)

    call compute_elementary_conductivity2( all_modelizations(id)%elements(i_element)%i_blmID                              , &
                                           all_modelizations(id)%elements(i_element)%ppsnb, dt, X, T_ele, fields          , &
                                           ef_map(i_element)%idata, ef_names(i_element)%sdata, fint(i1:i2)                , &
                                           all_modelizations(id)%elements(i_element)%M_elem(1:elem_dofs,1:elem_dofs,i_mat)  )

    deallocate(X,T_ele)
  end do

end subroutine

!> \brief Compute elementary stiffness and internal forces of a mechanical finite element
subroutine comp_bulks_therFE(id, dt, nodes, old_fields, new_fields, ef_map, ef_names, fint, fint_map)
  use a_therEF, only : compute_elementary_ttfint2
  implicit none
  !> [in] id of the modelization
  integer(kind=4),                      intent(in)    :: id
  !> [in] time differential
  real(kind=8),                         intent(in)    :: dt
  !> [in] reference coordinates of the nodes
  real(kind=8),         dimension(:,:), intent(in)    :: nodes
  !> [in] fields values at quadrature points at beginning of time step
  real(kind=8),         dimension(:)  , intent(inout) :: old_fields
  !> [in] fields values at quadrature points at current time step
  real(kind=8),         dimension(:)  , intent(inout) :: new_fields
  !> [in] map to acces quadrature point fields
  type(T_i4_matrix),    dimension(:)  , intent(in)    :: ef_map
  !> [in] map to get quadrature point fields' name
  type(T_string_array), dimension(:)  , intent(in)    :: ef_names
  !> [out] vector in which to store internal forces
  real(kind=8),         dimension(:)  , intent(out)   :: fint
  !> [in] map to acces internal forces vector by element
  integer(kind=4),      dimension(:)  , intent(in)    :: fint_map
  !
  integer(kind=4) :: i_element, i1, i2

  do i_element = 1, all_modelizations(id)%nb_elements

    i1 = fint_map(i_element) + 1
    i2 = fint_map(i_element+1)

    call compute_elementary_ttfint2(all_modelizations(id)%elements(i_element)%i_blmID                     , &
                                    all_modelizations(id)%elements(i_element)%ppsnb, dt                   , &
                                    nodes(1:nbDIME,all_modelizations(id)%elements(i_element)%connectivity), &
                                    old_fields,new_fields,ef_map(i_element)%idata                         , &
                                    ef_names(i_element)%sdata,fint(i1:i2)                                   )

  end do

end subroutine

