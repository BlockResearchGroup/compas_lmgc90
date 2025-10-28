!==========================================================================
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
module global_thermal_solver

  use utilities, only : faterr

  use therMAILx, only : get_nb_therMAILx         , &
                        get_nb_nodes_therMAILx   , &
                        get_nb_elements_therMAILx, &
                        get_lhs_loc_therMAILx    , &
                        get_rhs_loc_therMAILx    , &
                        get_ptr_Tbeg_therMAILx   , &
                        get_ptr_T_therMAILx      , &
                        get_ptr_Tdriv_therMAILx  , &
                        get_ptr_Fext_therMAILx   , &
                        get_ptr_drvdofs_therMAILx, &
                        get_ptr_ccdof_therMAILx  , &
                        get_ptr_connec_therMAILx , &
                        get_nb_max_dofs_adj_therMAILx

  use overall, only : H, THETA_T, &
                      max_internal_tact, &
                      i_real_tactor

  use CLALp, only : get_nb_CLALp        , &
                    get_iannodes_CLALp  , &
                    get_icdnodes_CLALp  , &
                    ! get_internal_CLALp  , &
                    get_rel_pos_CLALp   , &
                    get_nb_max_adj_CLALp, &
                    get_tact_lawnb_CLALp, &
                    get_diffusion_lhs_CLALp

  use inter_meca_handler_2D, only : get_icdbdy, get_ianbdy, get_internal

  use a_system, only : T_link_connec             , &
                       g_system                  , &
                       initialize_system         , &
                       set_elementary_matrix     , &
                       erase_elementary_matrix   , &
                       add_to_elementary_matrix  , &
                       get_elementary_matrix     , &
                       erase_elementary_vector   , &
                       set_elementary_vector     , &
                       erase_ext_vector          , &
                       add_to_ext_vector         , &
                       set_drvdofs               , &
                       set_drvvalues             , &
                       solve_system              , &
                       erase_system

  use parameters, only : i_sparse, i_sym, i_MP3_CZM_THER, &
                         get_inter_law_name_from_id, &
                         i_clalp

  use tact_behaviour, only : tact_behav

  implicit none

  private

  !> Verbose flag for debug intent
  logical :: verbose = .false.

  !> Map from body number to global node number
  integer(kind=4), dimension(:), pointer :: cc_node => null()
  !> Map from global node number to global dof number
  integer(kind=4), dimension(:), pointer :: cc_dof  => null()

  !> root of linked list of global connectivities
  type(T_link_connec), pointer :: g_connec_root => null()
  !> leaf of linked list of global connectivities
  type(T_link_connec), pointer :: g_connec_last => null()
  !> element of global connectivities linked list corresponding to last mesh element
  type(T_link_connec), pointer :: g_last_melem => null()
  !> element of global connectivities linked list corresponding to first eliminated node
  type(T_link_connec), pointer :: g_last_inter => null()

  !> Global system
  type(G_system) :: global_system

  !> Global solution array
  real(kind=8), dimension(:), pointer :: global_sol  => null()

  integer(kind=4), dimension(:), pointer :: global_drvdofs   => null()
  real(kind=8)   , dimension(:), pointer :: global_drvvalues => null()

  !> Maximum number of adjacent to a dof
  integer(kind=4) :: nb_max_dofs_adj

  !> Map of facing nodes to prepare elimination
  integer(kind=4), dimension(:), pointer :: n2n_map  => null()

  !> Node elimination map
  integer(kind=4), dimension(:), pointer :: nod_elim => null()

  !> Dof elimination map (for driven dof)
  integer(kind=4), dimension(:), pointer :: dof_elim => null()


  public initialize        , &
         prep_global_system, &
         assemble_lhs      , &
         assemble_rhs      , &
         apply_drvdofs     , &
         solve             , &
         clean_memory

contains

  !> \brief Initialization of global system
  subroutine initialize()
    implicit none
    !
    type(T_link_connec), pointer :: new_link
    integer(kind=4) :: nb_ther, ibdyty, iblmty, i_beg, i_end, nb_elem
    integer(kind=4) :: nb_drvdofs
    integer(kind=4), dimension(:), pointer :: loc, drvdofs

    nb_ther = get_nb_therMAILx()

    if( nb_ther <= 0 ) return

    ! generating some fixed maps

    if( associated(cc_node) ) then
      deallocate(cc_node)
      nullify(cc_node)
    end if

    if( associated(cc_dof) ) then
      deallocate(cc_dof)
      nullify(cc_dof)
    end if


    ! first nodes index map
    allocate(cc_node(nb_ther+1))
    cc_node(1) = 0
    do ibdyty = 1, nb_ther
      cc_node(ibdyty+1) = cc_node(ibdyty) + get_nb_nodes_therMAILx(ibdyty)
    end do 

    ! then dofs index map
    allocate(cc_dof(cc_node(nb_ther+1)+1))
    cc_dof(1) = 0
    do ibdyty = 1, nb_ther
      i_beg = cc_node(ibdyty)+1 +1 !shift to keep the first 0
      i_end = cc_node(ibdyty+1) +1
      loc => get_ptr_ccdof_therMAILx(ibdyty)
      cc_dof(i_beg:i_end) = loc(2:i_end-i_beg+2) !do not store the first 0 of the local map
      cc_dof(i_beg:i_end) = cc_dof(i_beg:i_end) + cc_dof(i_beg-1) !cumulating in global map
    end do 


    ! creating root of the linked list to store total number of elements
    allocate( g_connec_root )
    allocate( g_connec_root%connec(4) )
    g_connec_root%connec(:) = 0
    g_connec_last => g_connec_root

    ! first sizing for therMAILx
    do ibdyty = 1, nb_ther
      nb_elem =  get_nb_elements_therMAILx(ibdyty)
      do iblmty = 1, nb_elem
        loc => get_ptr_connec_therMAILx(ibdyty,iblmty)
        allocate( new_link )
        allocate( new_link%connec_ori(size(loc)) )
        allocate( new_link%connec    (size(loc)) )
        new_link%connec_ori = cc_node(ibdyty) + loc
        new_link%connec     = new_link%connec_ori
        g_connec_last%n => new_link
        g_connec_last   => new_link
        g_connec_root%connec(1) = g_connec_root%connec(1) + 1
        g_connec_root%connec(2) = g_connec_root%connec(2) + 1
      end do
    end do
    g_last_melem => g_connec_last

    ! sizing global system maps
    ! maximum number of adjacent of a dof is oversized
    nb_max_dofs_adj = 0
    do ibdyty = 1, nb_ther
      nb_max_dofs_adj = max( nb_max_dofs_adj, get_nb_max_dofs_adj_therMAILx(ibdyty) )
    end do

    nb_drvdofs = 0
    do ibdyty = 1, nb_ther
      drvdofs => get_ptr_drvdofs_therMAILx(ibdyty)
      if(.not. associated(drvdofs)) cycle
      nb_drvdofs = nb_drvdofs + size(drvdofs)
    end do
    allocate(global_drvdofs(nb_drvdofs))
    allocate(global_drvvalues(nb_drvdofs))

    if( associated(global_sol) ) then
      deallocate(global_sol)
      nullify(global_sol)
    end if
    allocate(global_sol(cc_dof(cc_node(nb_ther+1)+1))) 

    ! allocate map for elimination process
    if( associated(n2n_map) ) deallocate(n2n_map)
    if( associated(nod_elim)) deallocate(nod_elim)
    if( associated(dof_elim)) deallocate(dof_elim)

    allocate(n2n_map(cc_node(nb_ther+1)))
    allocate(nod_elim(cc_node(nb_ther+1)))
    allocate(dof_elim(cc_dof(cc_node(nb_ther+1)+1)))

  end subroutine

  !> \brief Do the elimination in n2n_map
  !> Add the fact that icd and ian nodes are facing each other.
  !> In the n2n_map, the higher node index stores the fact that
  !> it is facing another one of lower index.
  subroutine node_elimination(icd,ian)
    implicit none
    !> Candidat global node number
    integer(kind=4), intent(in) :: icd
    !> Antagonist global node number
    integer(kind=4), intent(in) :: ian
    !
    integer(kind=4) :: elim, node, e1, e2

    ! node to eliminate
    elim = max(icd,ian)
    ! reference node
    node = min(icd,ian)

    ! we would like:
    ! n2n_map(elim) = node
    ! but we must be careful to not
    ! loose already store information

    ! if the reference node is itself eliminated
    ! we look for the real reference node
    do while( n2n_map(node) /= node )
      node = n2n_map(node)
    end do

    ! check if node to eliminate already store an elimination
    ! and rebuild link in this case
    if( n2n_map(elim) /= elim .and. n2n_map(elim) /= node ) then
      e1 = min(node, n2n_map(elim))
      e2 = max(node, n2n_map(elim))
      do while( n2n_map(e2) > e1 .and. n2n_map(e2) /= e2 )
        e2 = n2n_map(e2)
      end do
      n2n_map(e2) = e1
    end if

    n2n_map(elim) = node

  end subroutine

  !> Initialisation of the g_system
  !> \todo : regler cette histoire du nb_max_dofs_adj
  subroutine prep_global_system()
    implicit none
    type(T_link_connec), pointer :: new_link
    integer(kind=4) :: i_inter, nb_inter, i_node, i_map, i_dof, icd, ian, lawnb
    logical, save :: is_init = .false.
    real(kind=8) :: apab, cpcd
    real(kind=8), dimension(max_internal_tact) :: internals
    integer(kind=4), save :: old_nb_beta1 = huge(i_inter)
    integer(kind=4)       :: nb_beta1
    !                                      12345678901234567890123456789012345678901
    character(len=41), parameter :: IAM = 'global_thermal_solver::prep_global_system'

    nb_inter = get_nb_CLALp(i_real_tactor)

    if( is_init .and. nb_inter /= g_connec_root%connec(3) ) then
      call faterr(IAM,"number of contact changed... algo not thought to work in this case")
    end if

    if( .not. is_init ) then
      ! if first time, update linked list interaction part
      ! WARNING: in fact it should be 'if first time OR network interaction changed'

      ! filling inter connec part
      g_connec_root%connec(1) = g_connec_root%connec(2)
      g_connec_root%connec(3) = 0

      do i_inter = 1, nb_inter

        ! check if interaction uses a cohesive law
        lawnb = get_tact_lawnb_CLALp(i_inter)
        if( tact_behav(lawnb)%ilaw /= i_MP3_CZM_THER ) cycle

        ! tricky... no lazy test with or/and... thus the second part cannot be tested
        ! with the first one
        if(.not. associated(g_connec_last%n) .or. .not. associated(g_last_inter) ) then
          allocate( new_link )
          allocate( new_link%connec(4) )
          allocate( new_link%connec_ori(4) )
        else if( associated(g_connec_last,g_last_inter%n)) then
          allocate( new_link )
          allocate( new_link%connec(4) )
          allocate( new_link%connec_ori(4) )
        !if(associated(g_connec_last%n) .and. .not. associated(g_connec_last,g_last_inter%n)) then
        else 
          new_link => g_connec_last%n
        end if

        new_link%connec_ori(1:2) = get_iannodes_CLALp(i_inter) + cc_node( get_ianbdy(i_clalp, i_inter) )
        new_link%connec_ori(3:4) = get_icdnodes_CLALp(i_inter) + cc_node( get_icdbdy(i_clalp, i_inter) )
        new_link%connec(1:4)     = new_link%connec_ori(1:4)

        g_connec_last%n => new_link
        g_connec_last   => new_link
        g_connec_root%connec(1) = g_connec_root%connec(1) + 1
        g_connec_root%connec(3) = g_connec_root%connec(3) + 1

      end do

      g_last_inter => g_connec_last

      ! remove unused leftover interaction and eliminated node links
      do while ( associated(g_connec_last%n) )

        new_link => g_connec_last%n
        g_connec_last%n => new_link%n

        if( associated(new_link%connec_ori) ) deallocate( new_link%connec_ori )
        deallocate( new_link%connec )
        deallocate( new_link )

      end do

    end if


    nb_beta1 = 0
    do i_inter = 1, nb_inter
      ! call get_internal_CLALp(i_inter,internals)
      call get_internal(i_clalp,i_inter,internals)       
      if( internals(4) >= 1.d0 ) nb_beta1 = nb_beta1 + 1
    end do

    ! (re)do elimination map
    if( nb_beta1 < old_nb_beta1 ) then
      if( verbose ) then
        print *, "[VERBOSE] [",IAM,"]"
        print *,'UPDATING ELIMINATION MAP : ', old_nb_beta1, nb_beta1
        print *, "[/VERBOSE] [",IAM,"]"
      end if

      ! init n2n_map
      do i_node = 1, size(n2n_map)
        n2n_map(i_node) = i_node
      end do

      new_link => g_last_melem%n

      do i_inter = 1, nb_inter

        ! getting linked list element corresponding to interaction
        if( .not. associated(new_link)) then
          call faterr(IAM, 'inconsistency between linked list and interaction network')
        end if

        ! check if interaction uses a cohesive law
        lawnb = get_tact_lawnb_CLALp(i_inter)
        if( tact_behav(lawnb)%ilaw /= i_MP3_CZM_THER ) cycle

        ! check if there is an elimination
        ! call get_internal_CLALp(i_inter,internals)
        call get_internal(i_clalp,i_inter,internals)

        ! sane interface: relies on facing elements
        if( internals(4) >= 1.d0 ) then

          call get_rel_pos_CLALp(i_inter, apab, cpcd)

          ! numnoda of CL is always facing numnodb of AL
          ! and numnodb of CL is always facing numnoda of AL

          if( apab <= 0.5d0 ) then
            icd = new_link%connec_ori(3)
            ian = new_link%connec_ori(2)
            call node_elimination(icd,ian)
          end if

          if( apab >= 0.5d0 ) then
            icd = new_link%connec_ori(4)
            ian = new_link%connec_ori(1)
            call node_elimination(icd,ian)
          end if

        end if

        new_link => new_link%n

      end do

      ! rm : generate elimination map
      do i_node = 1, size(n2n_map)

        i_map = i_node
        do while( n2n_map(i_map) /= i_map )
          i_map = n2n_map(i_map)
        end do

        nod_elim(i_node) = i_map
        do i_dof = 1, cc_dof(i_node+1)-cc_dof(i_node)
          dof_elim(cc_dof(i_node)+i_dof) = cc_dof(i_map)+i_dof
        end do

      end do

      if( verbose ) then
        print *, "[VERBOSE] [",IAM,"]"
        print *, 'n2n_map : '
        print *, n2n_map
        print *, 'nod_elim : '
        print *, nod_elim
        print *, 'dof_elim : '
        print *, dof_elim
        print *, "[/VERBOSE] [",IAM,"]"
      end if

      ! rm : eliminate nodes in connectivity
      new_link => g_connec_root%n
      do while( associated(new_link) .and. .not. associated(new_link,g_last_inter%n) )
        new_link%connec(:) = nod_elim( new_link%connec_ori )
        new_link => new_link%n
      end do

      ! adding element corresponding to a single eliminated node
      g_connec_root%connec(1) = g_connec_root%connec(2) + g_connec_root%connec(3)
      g_connec_root%connec(4) = 0
      g_connec_last => g_last_inter
      do i_node = 1, size(nod_elim)
        if( nod_elim(i_node) /= i_node ) then
          if( associated(g_connec_last%n) ) then
            new_link => g_connec_last%n
          else
            allocate(new_link)
            allocate(new_link%connec(1))
          end if
          new_link%connec(1) = i_node
          g_connec_last%n => new_link
          g_connec_last   => new_link
          g_connec_root%connec(1) = g_connec_root%connec(1) + 1
          g_connec_root%connec(4) = g_connec_root%connec(4) + 1
        end if
      end do

      ! remove leftover eliminated node link
      do while( associated(g_connec_last%n) )
        new_link => g_connec_last%n
        g_connec_last%n => new_link%n
        if( associated(new_link%connec_ori) ) deallocate( new_link%connec_ori )
        deallocate( new_link%connec )
        deallocate( new_link )
      end do

      if( is_init ) call erase_system(global_system)

      ! maximum number of adjacents dof is increased depending on the contact network
      call initialize_system(global_system,i_sparse,i_sym,cc_dof,g_connec_root,nb_max_dofs_adj*(get_nb_max_adj_CLALp()+2)*5)

    end if

    old_nb_beta1 = nb_beta1
    is_init = .true.

  end subroutine

  !> \brief Compute elementary left hand side matrices of the global system
  subroutine assemble_lhs() 
    implicit none 
    !
    type(T_link_connec), pointer :: link
    integer(kind=4) :: nb_ther, ibdyty, iblmty, i_inter, g_iblmty, nde, lawnb
    real(kind=8)    :: lhs_loc(20,20)         ! oversized arrays
    real(kind=8)    :: lhs_con(4,4), t_con(4) ! work only because CLALp support only
    real(kind=8), dimension(max_internal_tact) :: internals

    real(kind=8), parameter :: one(1,1) = reshape((/1.d0/),shape=(/1,1/))

    nb_ther = get_nb_therMAILx()

    if( nb_ther <= 0 ) return

    g_iblmty = 0
    do ibdyty = 1, nb_ther

      do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
        g_iblmty = g_iblmty + 1
        call get_lhs_loc_therMAILx(ibdyty,iblmty,lhs_loc,nde)
        call set_elementary_matrix(global_system,g_iblmty,lhs_loc(1:nde,1:nde))
      end do

    end do

    if( get_nb_CLALp(i_real_tactor) < 1 ) return

    link => g_last_melem
    do i_inter = 1, get_nb_CLALp(i_real_tactor)

      ! check if interaction uses a cohesive law
      lawnb = get_tact_lawnb_CLALp(i_inter)
      if( tact_behav(lawnb)%ilaw /= i_MP3_CZM_THER ) cycle

      g_iblmty = g_iblmty + 1
      link => link%n

      call erase_elementary_matrix(global_system,g_iblmty)

      ! skip interaction if there already is a dof elimination
      ! call get_internal_CLALp(i_inter,internals)
      call get_internal(i_clalp,i_inter,internals)      
      if( internals(4) >= 1.d0 ) cycle

      call get_diffusion_lhs_CLALp(i_inter, lhs_con)
      call add_to_elementary_matrix(global_system,g_iblmty,lhs_con,H*THETA_T)

    end do

    ! eliminated nodes
    link => g_last_inter%n
    do while( associated(link) )
      g_iblmty = g_iblmty + 1
      call set_elementary_matrix(global_system,g_iblmty,one)
      link => link%n
    end do

  end subroutine

  !> \brief Compute the elementary righ hand side vector of the global system
  subroutine assemble_rhs()
    implicit none
    !
    type(T_link_connec), pointer :: link
    integer(kind=4) :: nb_ther, ibdyty, iblmty, g_iblmty, i_inter, nde, lawnb, ibeg, iend
    real(kind=8)    :: rhs_loc(20)            ! oversized array
    real(kind=8)    :: lhs_con(4,4), t_con(4) ! work only because CLALp support only
    real(kind=8), dimension(:), pointer :: T, Tbeg, Fext

    nb_ther = get_nb_therMAILx()

    if( nb_ther <= 0 ) return

    g_iblmty = 0
    do ibdyty = 1, nb_ther

      do iblmty = 1, get_nb_elements_therMAILx(ibdyty)

        g_iblmty = g_iblmty + 1

        call get_rhs_loc_therMAILx(ibdyty,iblmty,rhs_loc,nde)
        call set_elementary_vector(global_system,g_iblmty,rhs_loc(1:nde))

      end do
    end do

    call erase_ext_vector(global_system)

    do ibdyty = 1, nb_ther
       Tbeg  => get_ptr_Tbeg_therMAILx(ibdyty)
       T     => get_ptr_T_therMAILx(ibdyty)
       ibeg  =  cc_dof(cc_node(ibdyty)+1)+1
       iend  =  cc_dof(cc_node(ibdyty+1 )+1)
       global_sol(ibeg:iend)  = THETA_t*T(:) + (1.d0-THETA_t)*Tbeg(:)
       ! add external flux
       Fext  => get_ptr_Fext_therMAILx(ibdyty)
       call add_to_ext_vector(global_system,cc_dof(cc_node(ibdyty)+1)+1, Fext)
    end do

    link => g_last_melem
    do i_inter = 1, get_nb_CLALp(i_real_tactor)

      ! check if interaction uses a cohesive law
      lawnb = get_tact_lawnb_CLALp(i_inter)
      if( tact_behav(lawnb)%ilaw /= i_MP3_CZM_THER ) cycle

      g_iblmty = g_iblmty + 1
      link => link%n

      call erase_elementary_vector(global_system,g_iblmty)

      t_con(:) = global_sol(link%connec)
      call get_elementary_matrix(global_system,g_iblmty,lhs_con)
      call set_elementary_vector(global_system,g_iblmty,-matmul(lhs_con,t_con)/THETA_t)

    end do

    ! eliminated nodes
    link => g_last_inter%n
    do while( associated(link) )
      g_iblmty = g_iblmty + 1
      call erase_elementary_vector(global_system,g_iblmty)
      link => link%n
    end do

  end subroutine

  !> \brief Get driven dofs of therMAILx to apply them to the global system
  subroutine apply_drvdofs()
    implicit none
    integer(kind=4) :: nb_ther, ibdyty, idrvdof, dof_shift, ivd
    integer(kind=4), dimension(:), pointer :: drvdofs
    real(kind=8), dimension(:), pointer :: T, Tdriv
    
    nb_ther = get_nb_therMAILx()
   
    ! applying driven dof
    idrvdof = 0
    do ibdyty = 1, nb_ther

      Tdriv   => get_ptr_Tdriv_therMAILx(ibdyty)
      T       => get_ptr_T_therMAILx(ibdyty)
      drvdofs => get_ptr_drvdofs_therMAILx(ibdyty)

      if( .not. associated(drvdofs) ) cycle

      dof_shift = cc_dof( cc_node(ibdyty) + 1)
      do ivd = 1, size(drvdofs)
        global_drvvalues(idrvdof+ivd) = Tdriv(ivd) - T(drvdofs(ivd))
      end do
      global_drvdofs( idrvdof+1:idrvdof+size(drvdofs) ) = dof_elim(drvdofs(:) + dof_shift)
      idrvdof = idrvdof + size(drvdofs)
    end do

    call set_drvdofs(global_system,global_drvdofs)
    call set_drvvalues(global_system,global_drvvalues)
  
  end subroutine

  !> \brief Solve global thermal solver
  subroutine solve()
    implicit none
    integer(kind=4) :: nb_ther, ibdyty, i_node, info, ibeg, iend, ibeg_elim, iend_elim
    real(kind=8), dimension(:), pointer :: T
    character(len=28) :: IAM
    character(len=80) :: cout
    !      1234567890123456789012345678
    IAM = 'global_thermal_solver::solve'

    nb_ther = get_nb_therMAILx()

    call solve_system(global_system, global_sol, info)
    if( info /= 0 ) then
      write(cout,'(A,I0)') 'Could not solve sparse system, error: ', info
      call faterr(IAM,cout)
    end if

    do i_node = 1, size(nod_elim)
      if( nod_elim(i_node) /= i_node ) then
        ibeg      = cc_dof(i_node)+1
        iend      = cc_dof(i_node+1)
        ibeg_elim = cc_dof(nod_elim(i_node))+1
        iend_elim = cc_dof(nod_elim(i_node)+1)
        global_sol(ibeg:iend) = global_sol(ibeg_elim:iend_elim)
      end if
    end do

    ! scatter solution in therMAILx
    do ibdyty = 1, nb_ther
       T     => get_ptr_T_therMAILx(ibdyty)
       T(:) = T(:) + global_sol(cc_dof(cc_node(ibdyty)+1)+1 : cc_dof(cc_node(ibdyty+1)+1) )
    end do

  end subroutine 

  !> \brief Free memory allocated within the module
  subroutine clean_memory()
    implicit none
    type(T_link_connec), pointer :: dangling_link

    if( associated(cc_node) ) then
      deallocate(cc_node)
      nullify(cc_node)
    end if

    if( associated(cc_dof) ) then
      deallocate(cc_dof)
      nullify(cc_dof)
    end if

    
    if (associated(g_connec_root)) then

      do while ( associated(g_connec_root) )

        dangling_link => g_connec_root
        g_connec_root => dangling_link%n

        deallocate( dangling_link%connec )
        if(associated(dangling_link%connec_ori)) deallocate( dangling_link%connec_ori )
        deallocate( dangling_link )

      end do

    end if  

    g_connec_root => null()
    g_last_melem => null()
    g_last_inter => null()
    g_connec_last => null()

    if( associated(global_drvvalues) ) then
      deallocate(global_drvvalues)
      nullify(global_drvvalues)
    end if
 
    if( associated(global_drvdofs) ) then
      deallocate(global_drvdofs)
      nullify(global_drvdofs)
    end if

    if( associated(global_sol) ) then
      deallocate(global_sol)
      nullify(global_sol)
    end if

    if( associated(n2n_map) ) then
      deallocate(n2n_map)
      nullify(n2n_map)
    end if

    if( associated(nod_elim)) then
      deallocate(nod_elim)
      nullify(nod_elim)
    end if

    if( associated(dof_elim)) then
      deallocate(dof_elim)
      nullify(dof_elim)
    end if

    call erase_system(global_system)

  end subroutine

end module
