
!> \brief Set some data of a bulk of a meca finite element
subroutine set_bulk_of_mecaFE(model, blmID, i_model, i_blmID, ndofs_by_element)
  use a_mecaEF, only : get_nb_in_mecaEF, &
                       get_N_DOF_mecaEF
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
  i_blmID = get_nb_in_mecaEF(blmID)
  ! \todo : revoir ce mecanisme a cause des elements avec un nombre de ddl variable par noeud
  ndofs_by_element = get_N_DOF_mecaEF(modelz(i_model)%ID)

end subroutine

!> \brief Set the map between a node and a nodal field index of a mechanical finite element
subroutine set_cc_nodal_field_mecaFE(id, nb_nodes, cc_nodal_field)
  use a_mecaEF, only : get_N_DOF_of_NODE_mecaEF
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
      n_dof  = get_N_DOF_of_NODE_mecaEF(modelz(i_model)%ID, i_node)
      cc_nodal_field(i_node+1) = max( cc_nodal_field(i_node+1), n_dof )
    end do

  end do

  do i_node = 1, nb_nodes
    cc_nodal_field(i_node+1) = cc_nodal_field(i_node+1) + cc_nodal_field(i_node)
  end do

end subroutine

!> \brief Set the ef_names and ef_map of a mechanical finite element
subroutine set_cc_elementary_fields_mecaFE(id, ef_names, ef_map, ef_size)
  use a_mecaEF, only : get_N_GP_mecaEF
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
  IAM='inc_model_mecaFE::set_cc_elementary_fields_mecaFE'

  nb_elements = all_modelizations(id)%nb_elements

  do i_element = 1, nb_elements

    i_model = all_modelizations(id)%elements(i_element)%i_model
    i_behav = all_modelizations(id)%elements(i_element)%i_behav

    nb_dof = 0
    nb_external = modelz(i_model)%nb_external_variables
    nb_internal = modelz(i_model)%nb_internal_variables
    nb_ef = get_external_field_nb( i_model )
    nb_bf = get_bulk_field_nb( i_behav )

    nbf = 3 ! stress, strain, internal
    nb_gp = get_N_GP_mecaEF( modelz(i_model)%ID )
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

    ef_names(i_element)%sdata(1) = 'strain'
    nb_dof = nb_dof + nb_external
    ef_names(i_element)%sdata(2) = 'stress'
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

  nb_gp = get_N_GP_mecaEF( modelz(i_model)%ID )

  ef_size = ef_map(nb_elements)%idata(total_nbf+1,nb_gp)

end subroutine

!> \brief Computation of elementary mass matrices for a mechanical finite element
subroutine comp_mass_meca_FE(id, i_mat, nodes)
  use a_mecaEF, only : compute_elementary_mass
  implicit none
  !> [in] id of the modelization
  integer(kind=4),              intent(in) :: id
  !> [in] index of mass matrix in M_elem
  integer(kind=4),              intent(in) :: i_mat
  !> [in] the position of the nodes
  real(kind=8), dimension(:,:), intent(in) :: nodes
  !
  integer(kind=4) :: i_element, elem_dofs

  do i_element = 1, all_modelizations(id)%nb_elements

    elem_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element

    call compute_elementary_mass( all_modelizations(id)%elements(i_element)%i_blmID                     , &
                                  all_modelizations(id)%elements(i_element)%ppsnb                       , &
                                  nodes(1:nbDIME,all_modelizations(id)%elements(i_element)%connectivity), &
                                  all_modelizations(id)%elements(i_element)%M_elem(1:elem_dofs,1:elem_dofs,i_mat) )

  end do

end subroutine

!> \brief Compute elementary external forces for a mechanical finite element
subroutine comp_Fext_mecaFE(id, i_mat, fext, fext_map)
  implicit none
  !> [in] id of the modelization
  integer(kind=4),               intent(in)  :: id
  !> [in] index of the elementary matrice to use for the computation
  integer(kind=4),               intent(in)  :: i_mat
  !> [out] vector in which to store external forces
  real(kind=8),    dimension(:), intent(out) :: fext
  !> [in] map to access external forces vector
  integer(kind=4), dimension(:), intent(in)  :: fext_map
  !
  real(kind=8), dimension(nbDIME) :: gravy_vect
  real(kind=8), dimension(:), allocatable :: tmp
  integer(kind=4)   :: i, i_element, i_node, i_dof, i1, i2
  integer(kind=4)   :: nb_nodes, nb_dofs_by_nodes

  gravy_vect(1) = grav1
  gravy_vect(2) = grav2
  if( nbDIME == 3 ) gravy_vect(3) = grav3

  do i_element = 1, all_modelizations(id)%nb_elements

    i_dof = all_modelizations(id)%elements(i_element)%ndofs_by_element

    allocate(tmp(i_dof))

    nb_nodes         = all_modelizations(id)%elements(i_element)%connec_size
    nb_dofs_by_nodes = i_dof / nb_nodes

    i1 = fext_map(i_element) + 1
    i2 = fext_map(i_element+1)

    do i_node = 1, nb_nodes
      i = (i_node-1) * nb_dofs_by_nodes
      tmp(i+1:i+nbDIME) = gravy_vect(1:nbDIME)
    end do

    fext(i1:i2) = matmul( all_modelizations(id)%elements(i_element)%M_elem(1:i_dof,1:i_dof,i_mat), tmp(1:i_dof) )

    deallocate(tmp)

  end do

end subroutine

!> \brief Compute elementary stiffness and internal forces of a mechanical finite element
subroutine comp_bulks_mecaFE(id, i_mat, dt, coor, disp, disp_map, fields, ef_map, ef_names, fint, fint_map)
  use a_mecaEF, only : compute_elementary_bulk2
  implicit none
  !> [in] id of the modelization
  integer(kind=4),                      intent(in)    :: id
  !> [in] index of the elementary matrice to store stiffness 
  integer(kind=4),                      intent(in)    :: i_mat
  !> [in] time differential
  real(kind=8),                         intent(in)    :: dt
  !> [in] reference coordinates of the nodes
  real(kind=8),         dimension(:,:), intent(in)    :: coor
  !> [in] displacement of the nodes
  real(kind=8),         dimension(:)  , intent(in)    :: disp
  !> [in] map to acces disp
  integer(kind=4),      dimension(:)  , intent(in)    :: disp_map
  !> [in] fields values at quadrature points
  real(kind=8),         dimension(:)  , intent(inout) :: fields
  !> [in] map to acces quadrature point fields
  type(T_i4_matrix),    dimension(:)  , intent(in)    :: ef_map
  !> [in] map to get quadrature point fields' name
  type(T_string_array), dimension(:)  , intent(in)    :: ef_names
  !> [out] vector in which to store internal forces
  real(kind=8),         dimension(:)  , intent(out)   :: fint
  !> [in] map to acces internal forces vector by element
  integer(kind=4),      dimension(:)  , intent(in)    :: fint_map
  !
  real(kind=8), dimension(:,:), allocatable :: X
  real(kind=8), dimension(:)  , allocatable :: U
  integer(kind=4)   :: i, i_element, i_node, idof, elem_dofs, nb_nodes, i1, i2

  do i_element = 1, all_modelizations(id)%nb_elements

    ! allocate local stiffness matrix to pass to a_mecaEF function
    elem_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element

    nb_nodes = all_modelizations(id)%elements(i_element)%connec_size
    idof = elem_dofs / nb_nodes
    allocate(X(idof, nb_nodes))
    allocate(U(elem_dofs))

    ! get back reference coordinates and displacements of the nodes of the element
    do i = 1, nb_nodes
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i)
      X(1:idof,i) = coor(1:idof,i_node)
      U((i-1)*idof+1:i*idof) = disp(disp_map(i_node)-disp_map(1)+1:disp_map(i_node+1)-disp_map(1))
    end do

    i1 = fint_map(i_element) + 1
    i2 = fint_map(i_element+1)

    call compute_elementary_bulk2(all_modelizations(id)%elements(i_element)%i_blmID, &
                                  all_modelizations(id)%elements(i_element)%ppsnb  , &
                                  dt,X,U,fields,ef_map(i_element)%idata            , &
                                  ef_names(i_element)%sdata,fint(i1:i2)            , &
                                  all_modelizations(id)%elements(i_element)%M_elem(1:elem_dofs,1:elem_dofs,i_mat) )

    deallocate(X)
    deallocate(U)

    !print *,'------------------------------'
    !print *,'id : ', id
    !print *,'ie : ', i_element
    !print *,'fi : '
    !print *, fint(i1:i2)
    !print *,'K  : '
    !call display_G_elementary_matrix(all_modelizations(id)%elements(i_element)%M_elem(i_mat))
    !print *,'------------------------------'

  end do

end subroutine

!> \brief Compute elementary fields
subroutine comp_fields_mecaFE(id, dt, coor, disp, disp_map, fields_old, fields_new, ef_map, ef_names)
  use a_mecaEF, only : compute_elementary_fields2
  implicit none
  !> [in] id of the modelization
  integer(kind=4),                      intent(in)  :: id
  !> [in] time differential
  real(kind=8),                         intent(in)  :: dt
  !> [in] reference coordinates of the nodes
  real(kind=8),         dimension(:,:), intent(in)  :: coor
  !> [in] displacement of the nodes
  real(kind=8),         dimension(:)  , intent(in)  :: disp
  !> [in] map to acces disp
  integer(kind=4),      dimension(:)  , intent(in)  :: disp_map
  !> [in] old fields values at quadrature points
  real(kind=8),         dimension(:)  , intent(in)  :: fields_old
  !> [in] new fields values at quadrature points
  real(kind=8),         dimension(:)  , intent(out) :: fields_new
  !> [in] map to acces quadrature point fields
  type(T_i4_matrix),    dimension(:)  , intent(in)  :: ef_map
  !> [in] map to get quadrature point fields' name
  type(T_string_array), dimension(:)  , intent(in)  :: ef_names
  !
  real(kind=8), dimension(:,:), allocatable :: X, K
  real(kind=8), dimension(:)  , allocatable :: U
  integer(kind=4)   :: i, i_element, i_node, idof, elem_dofs, nb_nodes

  do i_element = 1, all_modelizations(id)%nb_elements

    elem_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element
    nb_nodes  = all_modelizations(id)%elements(i_element)%connec_size
    idof      = elem_dofs / nb_nodes
    allocate(X(idof, nb_nodes))
    allocate(U(elem_dofs))

    ! get back reference coordinates and displacements of the nodes of the element
    do i = 1, nb_nodes
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i)
      X(1:idof,i) = coor(1:idof,i_node)
      U((i-1)*idof+1:i*idof) = disp(disp_map(i_node)-disp_map(1)+1:disp_map(i_node+1)-disp_map(1))
    end do

    call compute_elementary_fields2(all_modelizations(id)%elements(i_element)%i_blmID   , &
                                    all_modelizations(id)%elements(i_element)%ppsnb     , &
                                    dt,X,U,fields_old,fields_new,ef_map(i_element)%idata, &
                                    ef_names(i_element)%sdata)
    deallocate(X)
    deallocate(U)

    !print *,'------------------------------'
    !print *,'id : ', id
    !print *,'ie : ', i_element
    !print *,'fo : '
    !print *, fields_old
    !print *,'fn : '
    !print *, fields_new
    !print *,'------------------------------'

  end do

end subroutine

!> \brief Get interpolation of gauss points fields on nodes
!> \todo : a check on the field name is done here AND in mod_a_mecaEF_iso
!>         every particular treatment on the field may better be done here
!>         and only the interpolation in EF module
subroutine get_elementary_fields_on_nodes_mecaFE(id, nb_nodes, ef_name, fields, ef_map, ef_names, node2elements, ef_out)
  use a_mecaEF, only : gpv2node2
  implicit none
  !> [in] id of the modelization
  integer(kind=4),                      intent(in) :: id
  !> [in] number of nodes in the modelization
  integer(kind=4),                      intent(in) :: nb_nodes
  !> [in] name of the elementary field to get
  character(len=30),                    intent(in) :: ef_name
  !> [in] fields values at quadrature points
  real(kind=8),         dimension(:)  , intent(in) :: fields
  !> [in] map to acces quadrature point fields
  type(T_i4_matrix),    dimension(:)  , intent(in) :: ef_map
  !> [in] map to get quadrature point fields' name
  type(T_string_array), dimension(:)  , intent(in) :: ef_names
  !> [in] the list of elements a node belongs to
  type(G_i_list),       dimension(:)  , intent(in) :: node2elements
  !> [in,out] fields values interpolated on nodes
  real(kind=8),         dimension(:,:), pointer    :: ef_out
  !
  logical :: name_found
  integer(kind=4) :: i, i_element, i_ef, i_node, i_connec, field_size, connec_size
  real(kind=8), dimension(:,:), allocatable :: tmp

  if( trim(ef_name) == 'strain' ) then
    if( nbDIME == 2 ) then
      allocate( ef_out(4,nb_nodes) )
      field_size = 4
    else
      allocate( ef_out(6,nb_nodes) )
      field_size = 6
    end if
  else if( trim(ef_name) == 'stress' ) then
    if( nbDIME == 2 ) then
      allocate( ef_out(5,nb_nodes) )
      field_size = 5
    else
      allocate( ef_out(7,nb_nodes) )
      field_size = 7
    end if
  else
    ! only scalar fields for now
    allocate( ef_out(1,nb_nodes) )
    field_size = 1
  end if

  ef_out(1:field_size,1:nb_nodes) = 0.d0

  do i_element = 1, all_modelizations(id)%nb_elements

    ! look for field index from name
    ! \todo: sort if name or field index is to be used... because check this
    !        for each element of each body is going to be expensive.
    name_found = .false.
    i_ef = 0
    do i = 1, size(ef_names(i_element)%sdata)
      if( ef_names(i_element)%sdata(i) == ef_name ) then
        i_ef = i
        name_found = .true.
        exit
      end if
    end do
    if (.not. name_found) cycle

    connec_size = all_modelizations(id)%elements(i_element)%connec_size
    allocate(tmp(field_size,connec_size))

    call gpv2node2(all_modelizations(id)%elements(i_element)%i_blmID, &
                   all_modelizations(id)%elements(i_element)%i_model, &
                   i_ef, fields, ef_map(i_element)%idata            , &
                   ef_names(i_element)%sdata, tmp, nbDIME             )

    do i_connec = 1, connec_size
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i_connec)
      ef_out(1:field_size,i_node) = ef_out(1:field_size,i_node) + tmp(1:field_size,i_connec)
    end do

    deallocate(tmp)
   
  end do

  do i_node = 1, nb_nodes
    ef_out(1:field_size,i_node) = ef_out(1:field_size,i_node) / size(node2elements(i_node)%G_i)
  end do

end subroutine

