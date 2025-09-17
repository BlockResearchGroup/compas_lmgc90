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
module multiMAILx

  !!****h* LMGC90.CORE/multiMAILX
  !! NAME
  !!  module multiMAILx
  !! PURPOSE
  !!  modelling of meca and multi-phasique deformable system through finite elements
  !!****

  use overall, only: logmes, &
                     faterr
  !use utilities

  use parameters
  !use timer
  !
  use algebra, only: length2, &
                     length3

  !USE DiscreteGeometry
  !USE RigidKinematic
  !
  use bulk_behaviour
  use models
  !
  use a_DOF
  !
  use a_system, only : T_link_connec             , &
                       g_system                  , &
                       initialize_system         , &
                       get_nb_non_zero           , &
                       erase_elementary_matrix   , &
                       add_to_elementary_matrix  , &
                       set_vector                , &
                       assemble_elementary_vector, &
                       erase_drvdofs             , &
                       set_drvdofs               , &
                       set_drvvalues             , &
                       solve_system              , &
                       erase_system              , &
                       sparse_storage_available
  !
  !USE a_EF

  use a_multiEF
  
  use MAILx

  use MAILx_type, only : T_multiMAILx
  
  implicit none
  
  private

  !> list of multiMAILx bodies
  type( T_multiMAILx ), dimension( : ), allocatable, private, target :: bdyty
  
  !> mapping between local  to global bdyty number
  integer(kind=4), dimension(:), allocatable, private :: bdyty2M_bdyty
  
  !> reverse mapping between global 2 local bdyty,blmty,nodty numbering
  type(T_MAILx_2_localMAILx), dimension(:), allocatable, public :: M2multi
  
  !> number of multiMAILx bodies
  integer(kind=4) :: nb_multiMAILx = 0
  !> to keep track of existing entities
  integer(kind=4) :: nb_existing_entities

  ! rm :for check_equilibrium functions... later !
  !! parametres permettant de stopper un calcul si la vitesse est stabilisee.
  !!
  !REAL(kind=8)      :: eqs_tol
  !INTEGER           :: eqs_ichecktype
  !INTEGER,PARAMETER :: iQvlcy = 1 , iMvlcy = 2
  
  !> matrices storage type used in G_system
  ! i_diagonal, i_sparse, i_band, i_skyline, i_full
  integer(kind=4) :: Matrix_storage = -99
  !> matrices shape type use in G_system
  ! i_sym , i_std
  integer(kind=4) :: Matrix_shape = i_std
  ! pour les matrices denses 
  logical :: with_renum = .TRUE.
  
  !> define the strategy new/re-use of ppset
  !> try to reduce computation time introduced when using a new ppset by gp with matlib  
  logical :: use_existing_ppset = .true.

  !> is schema picard used
  logical, public :: is_new = .TRUE.



  
  !=============== methodes ===================================!
  
  public get_nb_multiMAILx                    , &
         get_nb_nodes_multiMAILx              , &
         get_nb_elements_multiMAILx           , &
         get_visibility_multiMAILx            , &
         read_in_driven_dof_multiMAILx        , &
         write_out_driven_dof_multiMAILx      , &
         read_in_dof_multiMAILx               , &
         write_xxx_dof_multiMAILx             , &
         read_in_gpv_multiMAILx               , &
         load_behaviours_multiMAILx           , &
         load_models_multiMAILx               , &
         update_existing_entities_multiMAILx  , &
         push_ppset_multiMAILx                , &
         increment_multiMAILx                 , &
         compute_mass_multiMAILx              , &
         compute_bulk_multiMAILx              , &
         compute_damping_multiMAILx           , &
         compute_Fext_multiMAILx              , &
         compute_flux_multiMAILx              , &
         assemb_KT_multiMAILx                 , &
         assemb_RHS_multiMAILx                , &
         apply_drvdof_KT_multiMAILx           , &
         compute_free_state_multiMAILx        , &
         compute_dof_multiMAILx               , &
         compute_flourxces_multiMAILx         , &
         update_bulk_multiMAILx               , &
         update_dof_multiMAILx                , &
         compute_residue_norm_multiMAILx      , &
         get_vector_multiMAILx                , &
         set_vector_multiMAILx                , &
         !check_equilibrium_state_multiMAILx, &
         set_Matrix_Storage_multiMAILx        , &
         set_Matrix_Shape_multiMAILx          , &
         set_without_renum_multiMAIlx         , &
         get_field_rank                       , &
         set_field_bynode, set_field_byelem   , &
         get_vfield_rank                      , &
         set_vfield_bynode, set_vfield_byelem , &
         !set_vlocy_drvdof_multiMAILx, &
         !nullify_poro_driven_dof, &
         !nullify_reac_multiMAILx,&
         !nullify_vlocy_multiMAILx,&
         !comp_vlocy_bynode_multiMAILx,&
         !comp_vlocy_multiMAILx,&
         !get_entity_multiMAILx,&
         !get_coorTT_nodty_multiMAILx,&
         !get_cooref_nodty_multiMAILx,&
         !set_visible_multiMAILx,&
         !set_precon_node_multiMAILx,&
         !compute_configurationTT_multiMAILx,&
         !compute_precon_vaux_bynode_multiMAILx,&
         !compute_precon_vaux_multiMAILx, &
         !set_precon_body_multiMAILx, &
         !compute_precon_W_multiMAILx ,&
         get_connectivities_multiMAILx, &
         get_coor_multiMAILx          , &
         get_all_multiMAILx           , &
         get_elements_volume          , &
         !<Erosion>
         get_elements_neighbor        , &
         get_ptr_elements_energy      , &
         get_ptr_elements_jacobian    , &
         get_ptr_elements_visibility  , &
         get_ptr_boundary_elements    , &
         compute_elements_energy      , &
         compute_elements_jacobian    , &
         get_deformation_energy       , &
         !</Erosion>
         clean_memory_multiMAILx

  private read_driven_dof , &
          write_driven_dof, &
          read_in_dof     , &
          write_out_dof   , &
          get_physic_values
          !read_in_gpv     , &

  !rm: accessor for hdf5
  public get_nb_gp_multiMAILx, &
         get_field_multiMAILx, &
         set_field_multiMAILx

  public get_bdyty_multiMAILx
  
contains 

!------------------------------------------------------------------------

  subroutine get_bdyty_multiMAILx( arg_bdyty )

    implicit none

    type( T_multiMAILx ), dimension( : ), pointer :: arg_bdyty

    arg_bdyty => bdyty

  end subroutine get_bdyty_multiMAILx

!------------------------------------------------------------------------

  !> \brief Get the number of multiMAILx bodies
  function get_nb_multiMAILx()
    implicit none
    !> the number of multiMAILx bodies
    integer(kind=4) :: get_nb_multiMAILx

    get_nb_multiMAILx = nb_multiMAILx
  end function

  !> \brief Get the number of nodes of a multiMAILx body
  function get_nb_nodes_multiMAILx(i_bdyty)
    implicit none
    !> id of multiMAILx body
    integer(kind=4) :: i_bdyty
    !> the number of nodes of the multiMAILx body
    integer(kind=4) :: get_nb_nodes_multiMAILx

    get_nb_nodes_multiMAILx = bdyty(i_bdyty)%nb_nodes
  end function

  !> \brief Get the number of elements of a multiMAILx body
  function get_nb_elements_multiMAILx(i_bdyty)
    implicit none
    !> id of multiMAILx body
    integer(kind=4) :: i_bdyty
    !> the number of elements of the multiMAILx body
    integer(kind=4) :: get_nb_elements_multiMAILx

    get_nb_elements_multiMAILx = size(bdyty(i_bdyty)%blmty)
  end function

  !> \brief Get the visibility of a multiMAILx
  logical function get_visibility_multiMAILx(i_bdyty)
    implicit none
    !> id of the multiMAILx body
    integer(kind=4), intent(in) :: i_bdyty

    get_visibility_multiMAILx = bdyty(i_bdyty)%visible
    
  end function get_visibility_multiMAILx

!------------------------------------------------------------------------  
! read and write routine
!------------------------------------------------------------------------ 

  !> \brief Read DRV_DOF.DAT file in DATBOX directory
  subroutine read_in_driven_dof_multiMAILx
    implicit none

    G_nfich = get_io_unit()
    open(unit=G_nfich,file=trim(location(in_driven_dof(:))))
    call read_driven_dof
    close(G_nfich)

  end subroutine read_in_driven_dof_multiMAILx

  !> \brief Write contribution to DRV_DOF.OUT file in OUTBOX directory
  subroutine write_out_driven_dof_multiMAILx
    implicit none
    integer(kind=4) :: nfich

    nfich = get_io_unit()
    open(unit=nfich,status='OLD',position='APPEND',file=trim(location(out_driven_dof(:))))
    call write_driven_dof(nfich)
    close(nfich)

  end subroutine write_out_driven_dof_multiMAILx

  !> \brief Read a GPV file to initialize database
  subroutine read_in_gpv_multiMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_gpv(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_gpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_gpv(:))))
    end if

    !call read_in_gpv
    close(G_nfich)

  end subroutine read_in_gpv_multiMAILx

  !> \brief Read a DOF file to initialize database
  subroutine read_in_dof_multiMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0 ) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
    else if(step > 1) then
      open(unit=G_nfich,file=trim(location(out_dof(:))))
    else
      open(unit=G_nfich,file=trim(location(last_dof(:))))
    end if

    call read_in_dof
    close(G_nfich)
    
  end subroutine read_in_dof_multiMAILx

  !> \brief Write dofs to DOF.OUT file for a given body
  !> \todo: 10/02/2014 rm just add the which parameters
  !>        which is a shitty id since this function write just one body
  !>        the select case should be done in the wrap file !
  subroutine write_xxx_dof_multiMAILx(which, i_bdyty)
    implicit none
    !> index of type of file where to write (LAST or OUT)
    integer(kind=4), intent(in) :: which
    !> id of multiMAILx to write dofs of
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: nfich,lc 

    nfich = get_io_unit()
    
    select case(which)
    case(1)
      lc = len_trim(out_dof)
      open(unit=nfich, status='OLD', position='APPEND', file=trim(location(out_dof(1:lc)))) 
      call write_out_dof(nfich,i_bdyty)
      close(nfich)
    case(2)
      lc = len_trim(last_dof)
      open(unit=nfich, status='OLD', position='APPEND', file=trim(location(last_dof(1:lc)))) 
      call write_out_dof(nfich,i_bdyty)
      close(nfich)
    case(6)
      call write_out_dof(nfich,i_bdyty)
    end select

  end subroutine write_xxx_dof_multiMAILx

  !> \brief Read driven dofs
  subroutine read_driven_dof
    implicit none
    integer(kind=4) :: itest, i_primal, i_dual, max_dof
    integer :: iM_bdyty, iM_nodty, i_bdyty, i_nodty, i_dof
    character(len=5)   :: chnod
    character(len=103) :: cout
    character(len=27)  :: IAM
    !      123456789012345678901234567
    IAM = 'multiMAILx::read_driven_dof'
  
    if (nb_multiMAILx == 0) return

    ! initialisation
    do i_bdyty = 1, size(bdyty)
      nullify(bdyty(i_bdyty)%drvdofs, &
              bdyty(i_bdyty)%drvvalues)

      bdyty(i_bdyty)%nb_primal_driven_dofs = 0
      bdyty(i_bdyty)%nb_dual_driven_dofs   = 0
    end do

    ! first reading: sizing array vlocy_driven_dof  
    do    
      if( .not. read_G_clin() ) exit
      if( G_clin(2:6) /= 'bdyty' ) cycle ! fishing for the keyword 'bdyty'

      i_primal = 0
      i_dual   = 0

      if( .not. read_G_clin() ) then
         call faterr(iam,'Problem reading bdyty')
      end if
      itest = itest_bdyty_MAILx(G_clin)  
      if (itest .ne. ifound) cycle

      !    we keep the body number   
      read(G_clin(9:13),'(I5)') iM_bdyty    
      if (iM_bdyty <= 0 .or. iM_bdyty > size(M_bdyty)) then
         write(cout,'(A,1x,I0,1x,A)') 'body number', iM_bdyty, 'does not belong to collection'
         call faterr(IAM,cout)
      end if

      i_bdyty = M2multi(iM_bdyty)%bdyty
      !if it's a body without MULTI behaviour
      if (i_bdyty == 0) cycle     

      do
        if ( .not. read_G_clin()) exit
        if (G_clin(2:6) == '$$$$$') exit
        if (G_clin(2:6) /= 'model') cycle ! fishing for the keyword 'model' 
        if ( .not. read_G_clin()) then
          call faterr(IAM,'Problem reading model')
        end if

        if (G_clin(2:6) /= 'MULTI') cycle ! fishing the MULTI part 

        do
          if ( .not. read_G_clin()) exit
          if (G_clin(2:6) == '     ') cycle
          if (G_clin(2:6) == 'model' .or. G_clin(2:6) == '$$$$$') exit 
          
          if (G_clin(2:6) /= 'nodty') then ! fishing for the keyword 'nodty' 
            call faterr(IAM,'keyword nodty expected')
          end if

          do
            if( .not. read_G_clin()) exit
            if(G_clin(2:6) == '     ') cycle
            if (.not. is_a_nodty(G_clin(2:6))) then
              call faterr(IAM,'Problem reading nodty')
            end if

            chnod = G_clin(2:6)
            read(G_clin(9:13),'(I5)') iM_nodty    

            if (iM_nodty <= 0 .or. iM_nodty > size(M_bdyty(iM_bdyty)%nodty)) then 
              write(cout,'(A12,I5,A25,I5)') 'node number ',iM_nodty,' does not belong to body ',iM_bdyty
              call faterr(IAM,cout)
            end if
            
            i_nodty = M2multi(iM_bdyty)%nodty(iM_nodty)
            
            !rm : remove this check because drvdofs bigger than possible may have been written
            !if ( nbdof_a_nodty(chnod) > &
            !     nbdof_a_nodty(get_nodID(bdyty(i_bdyty)%nodty(i_nodty)))) then

            !  write(cout,'(A,A,A)') 'nodty ',chnod,' incompatible with the one belonging to the body '
            !  call faterr(IAM,cout)
            !end if

            max_dof = maxval(bdyty(i_bdyty)%ccsize(:,i_nodty))

            do
              if( .not. read_G_clin()) exit
              if (G_clin(2:6) == '     ') cycle
              if (G_clin(2:6) /= 'dofty') then  ! fishing for the keyword 'dofty'
                call faterr(IAM,'keyword dofty expected')
              end if
              !
              do
                if( .not. read_G_clin()) exit
                select case(G_clin(2:6))
                case('prim_') 

                  i_primal = i_primal+1
                  read(G_clin( 9: 13),'(I5)') i_dof

                  ! DRV_DOF.DAT may contain more driven dofs than possible
                  if( i_dof > max_dof ) then
                    i_primal = i_primal-1
                    cycle
                  end if

                  if( i_dof<=0 .or. i_dof>nbdof_a_nodty(bdyty(i_bdyty)%nodty(i_nodty)) ) then
                    write(cout,'(A,I0,A,I0)') 'dof number ', i_dof, ' does not belong to bdyty ', i_bdyty
                    call faterr(IAM,cout)
                  end if
                   
                case('dual_') 
                  i_dual = i_dual+1
                  read(G_clin( 9: 13),'(I5)') i_dof

                  ! DRV_DOF.DAT may contain more driven dofs than possible
                  if( i_dof > max_dof ) then
                    i_dual = i_dual-1
                    cycle
                  end if

                  if( i_dof<=0 .or. i_dof>nbdof_a_nodty(bdyty(i_bdyty)%nodty(i_nodty)) ) then
                    write(cout,'(A,I0,A,I0)') 'dof number ', i_dof, ' does not belong to bdyty ', i_bdyty
                    call faterr(IAM,cout)
                  end if

                case('     ') 
                   
                case default
                  exit
                end select

              end do
              exit
            end do
            exit
          end do
          backspace(G_nfich)
        end do
        backspace(G_nfich)
      end do
      backspace(G_nfich)  
       
      bdyty(i_bdyty)%nb_primal_driven_dofs = i_primal

      allocate(bdyty(i_bdyty)%primal_drvdofs(i_primal))
         
      allocate(bdyty(i_bdyty)%drvdofs(i_primal),bdyty(i_bdyty)%drvvalues(i_primal))

      if (i_primal == 0) then
        write (cout,'(A,I0,A)') 'Warning: multiMAILx ', i_bdyty, ' without primal driven dof'
        call logmes(cout)
      else
        !!bdyty(i_bdyty)%Vdriv(:)    = 0.d0
        !!bdyty(i_bdyty)%VdrivBeg(:) = 0.d0
        !!bdyty(i_bdyty)%Xdriv(:)    = 0.d0
        bdyty(i_bdyty)%drvdofs(:)   = 0
        bdyty(i_bdyty)%drvvalues(:) = 0.d0
      end if
         
      bdyty(i_bdyty)%nb_dual_driven_dofs = i_dual
         
      allocate(bdyty(i_bdyty)%dual_drvdofs(i_dual))

      !   ALLOCATE(bdyty(ibdyty)%Fdriv(ifd),bdyty(ibdyty)%FdrivBeg(ifd),stat=errare)
      !   IF (errare/=0) THEN
      !      CALL FATERR(IAM,'error allocating Fdriv')
      !   END IF

      if (i_dual == 0) then
        write (cout,'(A,I0,A)') 'Warning: multiMAILx ', i_bdyty, ' without dual driven dof'
        call logmes(cout)
      !else
      !     bdyty(ibdyty)%FdrivBeg(:)=0.d0
      !     bdyty(ibdyty)%Fdriv(:)   =0.d0
      end if

    end do

    ! second reading: filling in data
    rewind(G_nfich)

    do    
      if( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'bdyty') cycle ! fishing for the keyword 'bdyty' 

      i_primal = 0
      i_dual   = 0

      if( .not. read_G_clin()) then
        call faterr(IAM,'Problem reading bdyty')
      endif
      itest = itest_bdyty_MAILx(G_clin)                      
      if (itest /= ifound) cycle

      ! we keep the body number   
      read(G_clin(9:13),'(I5)') iM_bdyty    
      i_bdyty = M2multi(iM_bdyty)%bdyty
       
      do
        if ( .not. read_G_clin()) exit
        if (G_clin(2:6) == '$$$$$') exit
        if (G_clin(2:6) /= 'model') cycle ! fishing for the keyword 'model' 
        if ( .not. read_G_clin()) then
          call faterr(IAM,'Problem reading model')
        end if

        if (G_clin(2:6) /= 'MULTI') cycle ! fishing the MULTI part 

        do
          if ( .not. read_G_clin()) exit
          if (G_clin(2:6) == '     ') cycle
          if (G_clin(2:6) == 'model' .or. G_clin(2:6) == '$$$$$') exit 

          if (G_clin(2:6) /= 'nodty') then ! fishing for the keyword 'nodty' 
            call faterr(IAM,'keyword nodty expected')
          end if

          do
            if( .not. read_G_clin()) exit
            if(G_clin(2:6) == '     ') cycle
            if (.not. is_a_nodty(G_clin(2:6))) then
              call faterr(IAM,'Problem reading nodty')
            end if

            chnod = G_clin(2:6)
            read(G_clin(8:13),'(I6)') iM_nodty    
            i_nodty = M2multi(iM_bdyty)%nodty(iM_nodty)

            max_dof = maxval(bdyty(i_bdyty)%ccsize(:,i_nodty))

            do
              if( .not. read_G_clin()) exit
              if (G_clin(2:6) == '     ') cycle
              if (G_clin(2:6) /= 'dofty') then ! fishing for the keyword 'dofty'
                call faterr(IAM,'keyword dofty expected')
              end if
              !
              do
                if( .not. read_G_clin()) exit
                select case(G_clin(2:6))
                case('prim_') 

                  ! DRV_DOF.DAT may contain more driven dofs than possible
                  read(G_clin( 9: 13),'(I5)') i_dof
                  if( i_dof > max_dof ) cycle

                  i_primal = i_primal+1
                   
                  nullify(bdyty(i_bdyty)%primal_drvdofs(i_primal)%time_evolution%x, &
                          bdyty(i_bdyty)%primal_drvdofs(i_primal)%time_evolution%fx)
                  
                  call read_a_driven_dof(chnod,i_nodty,G_clin,bdyty(i_bdyty)%primal_drvdofs(i_primal))
                   
                case('dual_') 

                  ! DRV_DOF.DAT may contain more driven dofs than possible
                  read(G_clin( 9: 13),'(I5)') i_dof
                  if( i_dof > max_dof ) cycle

                  i_dual = i_dual+1
                  
                  nullify(bdyty(i_bdyty)%dual_drvdofs(i_dual)%time_evolution%x, &
                          bdyty(i_bdyty)%dual_drvdofs(i_dual)%time_evolution%fx)
                  
                  call read_a_driven_dof(chnod,i_nodty,G_clin,bdyty(i_bdyty)%dual_drvdofs(i_dual))
                  
                case('     ')
                   
                case default
                  exit
                end select
              end do
                   
              exit
            end do
            exit
          end do
          backspace(G_nfich)
        end do
        backspace(G_nfich)
      end do
      backspace(G_nfich)  
    end do
  
  end subroutine read_driven_dof

  !> \brief Write driven dofs of multiMAILx
  subroutine write_driven_dof(nfich)
    implicit none
    !> file unit in which to write
    integer(kind=4) :: nfich
    !
    logical :: to_write
    integer(kind=4) :: i_primal, i_dual
    integer(kind=4) :: iM_bdyty, iM_nodty, i_bdyty, i_nodty, i_dof

    if (nb_multiMAILx== 0) return

    do i_bdyty = 1, size(bdyty)

      if (       bdyty(i_bdyty)%nb_primal_driven_dofs == 0 &
           .and. bdyty(i_bdyty)%nb_dual_driven_dofs   == 0 ) cycle

      ! the ibdyty body has some driven dof
      write(nfich,'(A6)') '$bdyty'
      write(nfich,101)    'MAILx',bdyty2M_bdyty(i_bdyty)
      write(nfich,'(A6)') '$model'
      write(nfich,'(A6)') ' MULTI'

      do i_nodty = 1, size(bdyty(i_bdyty)%nodty)
        to_write = .false.
        do i_primal = 1, bdyty(i_bdyty)%nb_primal_driven_dofs
          if (is_a_driven_dof_of_node(i_nodty,bdyty(i_bdyty)%primal_drvdofs(i_primal))) then
            to_write = .true.
            exit
          end if
        end do
        do i_dual = 1, bdyty(i_bdyty)%nb_dual_driven_dofs
          if (is_a_driven_dof_of_node(i_nodty,bdyty(i_bdyty)%dual_drvdofs(i_dual))) THEN
            to_write = .true.
            exit
          end if
        end do

        if (to_write) then
          ! the ibdyty body with inodty node has driven dof
          write(nfich,'(A6)') '$nodty'
          iM_bdyty = bdyty2M_bdyty(i_bdyty)
          iM_nodty = bdyty(i_bdyty)%nodty2M_nodty(i_nodty)
          write(nfich,101) get_nodNAME(M_bdyty(iM_bdyty)%nodty(iM_nodty)),iM_nodty
          call write_a_driven_dof(nfich)

          do i_primal = 1, bdyty(i_bdyty)%nb_primal_driven_dofs
            if (is_a_driven_dof_of_node(i_nodty,bdyty(i_bdyty)%primal_drvdofs(i_primal))) THEN
              call write_a_driven_dof(nfich,'prim_',bdyty(i_bdyty)%primal_drvdofs(i_primal))
            end if
          end do
          do i_dual = 1, bdyty(i_bdyty)%nb_dual_driven_dofs
            if (is_a_driven_dof_of_node(i_nodty,bdyty(i_bdyty)%dual_drvdofs(i_dual))) THEN
              call write_a_driven_dof(nfich,'dual_',bdyty(i_bdyty)%dual_drvdofs(i_dual))
            end if
          end do
        end if
      end do
      write(nfich,'(a6)')'$$$$$$'
      write(nfich,'(a6)')'      '
    end do
    
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    write(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
    close(nfich) 
      
101 format(1X,A5,2X,I5)    
 
  end subroutine write_driven_dof

  !> \brief Read content of DOF.INI file to initialize values of Dofs or displacements
  subroutine read_in_dof
    implicit none
    !
    integer(kind=4)    :: i_bdyty, i_nodty, iccdof, nbdof, itest
    integer(kind=4)    :: iM_bdyty, iM_nodty, iM_ccdof, max_dof
    character(len=5)   :: chnod
    character(len=23)  :: IAM
    character(len=103) :: cout

          !12345678901234567890123
    IAM = 'multiMAILx::read_in_dof'

    if (nb_multiMAILx == 0) return
    
    ! Initialize everything to 0.
    do i_bdyty = 1, nb_multiMAILx
      bdyty(i_bdyty)%Dofs = 0.d0
      bdyty(i_bdyty)%X    = 0.d0
    end do
   
    do
      if( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'bdyty') cycle ! fishing for the keyword 'bdyty'
      if( .not. read_G_clin()) exit
      itest = itest_bdyty_MAILx(G_clin)                      
      if (itest == ifound) then
        read(G_clin(9:13),'(I5)') i_bdyty
        if (i_bdyty <= 0 .or. i_bdyty > nb_multiMAILx ) then
           write(cout,'(A12,I5,A60)') 'body number ',i_bdyty,' does not belong to collection'
           call LOGMES('Error '//IAM//': '//cout)
        end if
      else
        cycle
      end if

      ! fd recherche du model ...
      do
        if ( .not. read_G_clin()) exit
        if (G_clin(2:6) == '$$$$$') exit
        if (G_clin(2:6) /= 'model') cycle   ! fishing for the keyword 'model' 
        if ( .not. read_G_clin()) then
           call faterr(IAM,'Problem reading model')
        end if

        if (G_clin(2:6) /= 'MULTI') cycle   ! fishing the MECAx part 

        do
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'nodty') cycle ! fishing for the keyword 'nodty' 
          do
            if( .not. read_G_clin()) exit
            itest = itest_nodty_MAILx(G_clin,i_bdyty)
            if (itest == isskip) cycle
            if (itest == inomor) exit                      
            if (itest == ifound) then
              read(G_clin(8:13),'(I6)') i_nodty
              if (i_nodty < 0 .or. i_nodty > size(bdyty(i_bdyty)%nodty)) then 
                write(cout,'(A12,I0,A25,I0,A29)') 'node number ',i_nodty,' does not belong to body ',i_bdyty
                call faterr(IAM,cout)
              end if

              !reading displacement
              if (get_node_id_from_name(G_clin(2:6)) > nbDIME) then
                write(cout,'(A5,I5,A21,I5)') G_clin(2:6),i_nodty,' too many data for X ', i_bdyty
                call faterr(IAM,cout)
              end if
              call G_read_a_nodty(bdyty(i_bdyty)%X(:,i_nodty,2),G_clin(2:6))

              if( .not. read_G_clin()) exit 

              chnod = G_clin(2:6)
              max_dof = maxval(bdyty(i_bdyty)%ccsize(:,i_nodty))

              ! tres tres beurk...
              if( max_dof < get_node_id_from_name(chnod) ) then
                write(chnod,'(A2,I1,A2)') 'NO',max_dof,'xx'
              end if

              nbdof   = nbdof_a_nodty(bdyty(i_bdyty)%nodty(i_nodty))
              iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)

              call G_read_a_nodty(bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbdof,2),chnod)

              !! normalement ca ne devrait pas etre utile 
              bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbdof,1) = bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbdof,2)
              bdyty(i_bdyty)%X(:,i_nodty,1) = bdyty(i_bdyty)%X(:,i_nodty,2)

              cycle
            end if
          end do !les valeurs aux nodty
          exit       
         end do ! $nodty 
         exit
       end do ! $model
     end do ! $bdyty
   
    ! actualisation des cordonnees du maillage

    do i_bdyty = 1, nb_multiMAILx

      iM_bdyty = bdyty2M_bdyty(i_bdyty)

      do i_nodty = 1, size(bdyty(i_bdyty)%nodty)
         iM_nodty = bdyty(i_bdyty)%nodty2M_nodty(i_nodty)
         iM_ccdof = M_bdyty(iM_bdyty)%ccdof(iM_nodty)

         M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) + &
                                                              bdyty(i_bdyty)%X(:,i_nodty,1)
      end do

    end do

  end subroutine read_in_dof

  !> \brief Write DOF of a multiMAILX in file 
  subroutine write_out_dof(nfich,i_bdyty)
    implicit none
    !> unit in which to write
    integer(kind=4), intent(in) :: nfich
    !> unit in which to write
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: i_nodty, iccdof, nb_dof, lc
    !real(kind=8) :: x(6)
    character(len=5) :: chnod
   
    if ( i_bdyty < 1 .or. i_bdyty > nb_multiMAILX ) return

    write(chnod, '(A2,I1,A2)') 'NO',nbDIME,'xx'

    write(nfich,'(A6)') '$bdyty'
    write(nfich,101)'MAILx',i_bdyty
    write(nfich,'(A6)') '$model'
    write(nfich,'(A6)') ' MULTI'
    write(nfich,'(A6)') '$nodty'

    do i_nodty = 1, size(bdyty(i_bdyty)%nodty) 
       
       nb_dof = nbdof_a_nodty(bdyty(i_bdyty)%nodty(i_nodty))
       iccdof = bdyty(i_bdyty)%ccdof(i_nodty)
       
       call write_a_nodty(chnod,i_nodty, bdyty(i_bdyty)%X(:,i_nodty,1), &
                          'X  ',nfich)

       call write_a_nodty(get_nodNAME(bdyty(i_bdyty)%nodty(i_nodty)),i_nodty  , &
                          bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1), &
                          'Z  ',nfich)
       
               
       
    end do
    write(nfich,'(A6)')'$$$$$$'
    write(nfich,'(A6)')'      '
    !!                     123456789012345678901234567890123456789012345678901234567890123456789012
    !WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
101 FORMAT(1X,A5,2X,I5)            
    
  end subroutine write_out_dof
!!!------------------------------------------------------------------------
  !> \brief update number of existing entities
  !> must be called after load_models
  subroutine update_existing_entities_multiMAILx
    implicit none

    nb_existing_entities = get_nb_ENTITY()
    call add_nb_ENTITY(nb_multiMAILx)

  end subroutine update_existing_entities_multiMAILx

  !> \brief material initialization
  subroutine load_behaviours_multiMAILx
    implicit none
    !
    integer(kind=4) :: ibdyty,iblmty,ibehav,imodel
    integer(kind=4) :: iM_bdyty,iM_blmty,iM_behav,iM_model

    character(len=27)  :: IAM
    character(len=103) :: cout

    !      123456789012345678901234567
    IAM = 'multiMAILx::load_behaviours'

    if (nb_multiMAILx == 0) return

    do ibdyty = 1, size(bdyty)
      iM_bdyty = bdyty2M_bdyty(ibdyty)
      do iblmty = 1, size(bdyty(ibdyty)%blmty)
        iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
        
        imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb

        ! finding material number from model number stored

        do iM_model = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
          if (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) exit
        end do

        if (iM_model > size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)) then
           !                    123456789012345678901234567890123456789
           write(cout,'(A39)') 'unable to recover the global model rank'
           call FATERR(IAM,cout)
        end if

        bdyty(ibdyty)%blmty(iblmty)%lawnb = get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
          
        if (bdyty(ibdyty)%blmty(iblmty)%lawnb == 0) then
          write(cout,'(A,I5,A,I5,A)') 'multiMAILX ',ibdyty,' element ',iblmty,' without behaviour !?'
          call LOGMES('check BODIES.DAT in DATBOX')
          call LOGMES('check BEHAVIOURS.DAT in DATBOX')
          call FATERR(IAM,cout)
        end if
      end do
    end do
    
  end subroutine load_behaviours_multiMAILx

  !> \brief Model initialization
  !> must be called after load_behaviours
  subroutine load_models_multiMAILx
    implicit none
    !
    integer(kind=4) :: nb_MAILx
    integer(kind=4) :: iM_bdyty, iM_blmty, iM_model, iM_nodty
    integer(kind=4) :: ibdyty, iblmty, blmnb, imodel, inodty
    integer(kind=4) :: i, itest, idof, iG_i, itempo, iccdof

    integer(kind=4) :: grad_size, internal_size, nb_gp

    integer(kind=4), dimension(:), allocatable :: edof2gdof, grad_sizes

    type(T_link_connec),             pointer :: connectivities, tmp
    integer(kind=4), dimension(:,:), pointer :: edge2vertices

    !fd 22/04/08 external fields
    integer(kind=4)   :: IF, nbf, nb_ef, nb_bf
    character(len=30) :: f_name
    character(len=30), dimension(:), allocatable :: field_names

    integer(kind=4) ::  max_nod2el, max_dofs_adj, max_conn

    character(len=23)  :: IAM
    character(len=103) :: cout
           !12345678901234567890123
    IAM = 'multiMAILx::load_models'


  !  integer(kind=4) :: i,p_inodty,bw,nbdof_meca
  !  !INTEGER :: bw

  !  integer,dimension(:),allocatable :: i4_vector
    
    connectivities => null()
    
    ! 0 initialisations
    call init_multiEF

    nb_MAILx = get_nb_MAILx()

    allocate(M2multi(nb_MAILx))
  
    do iM_bdyty = 1, nb_MAILx
       M2multi(iM_bdyty)%bdyty = 0
       nullify(M2multi(iM_bdyty)%nodty)
       nullify(M2multi(iM_bdyty)%blmty)
    end do

    ! choose a default value for matrix_storage if not set
    if( Matrix_storage < 0 ) then
      if( nbDIME == 3 ) then
        if( sparse_storage_available ) then
          Matrix_storage = i_sparse
        else
          Matrix_storage = i_band
        end if
      else
        Matrix_storage = i_band
      end if
    end if

    ! first reading: sizing array of models  
    itest = 0
    
    if( .not. allocated(modelz) .or. size(modelz) < 1 ) then
        call faterr(IAM,'please call ReadModels before trying to LoadModels')
    end if

    do imodel = 1, size(modelz)
      if (modelz(imodel)%mdlty == 'MULTI') itest = itest + 1 
    end do
    
    write(cout,'(I0,1x,A)') itest,'MULTI model(s) declared'
    call LOGMES(cout)
    
    ! then constructing the multi EF database 
    
    ! first scaning of the M_MAILx and models 
    ! database determining the size of bdyty
    
    if( .not. allocated(M_bdyty) ) then
        call faterr(IAM,'please call ReadBodies before trying to LoadModels')
    end if

    ibdyty=0
 
    do iM_bdyty = 1, size(M_bdyty)
      itest = 0
      do iM_blmty = 1, size(M_bdyty(iM_bdyty)%blmty)
        do iM_model = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
          do imodel = 1, size(modelz)
            if (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) then
              if (modelz(imodel)%mdlty == 'MULTI') itest = 1
            end if
          end do
          if (itest == 1) exit
        end do
        if (itest == 1) exit
      end do
      if (itest == 1) ibdyty = ibdyty + 1 
    end do
    
    allocate(bdyty(ibdyty))

    nb_multiMAILx = ibdyty

    write(cout,'(I0,1x,A)') nb_multiMAILx,'MULTI BODIES found'
    call LOGMES(cout)


    if (ibdyty == 0) then
      call LOGMES('no multiMAILx')
      call LOGMES('if any check BODIES.DAT or MODELS.DAT')
    else
      allocate(bdyty2M_bdyty(ibdyty))
    end if
 
    if (ibdyty == 0) return

    ! second scaning of the M_MAILx and models database 
    ! filling the correspondance table bdyty2M_bdyty and
    !  M2multi(iM_bdyty)%bdyty
    ! sizing bdyty(ibdyty)%blmty, bdyty(ibdyty)%blmty2M_blmty  

    ibdyty = 0

    do iM_bdyty = 1, size(M_bdyty)
      itest  = 0
      iblmty = 0

      do iM_blmty = 1, size(M_bdyty(iM_bdyty)%blmty)
        do iM_model = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
           do imodel = 1, size(modelz)
             if (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) then
               if (modelz(imodel)%mdlty == 'MULTI') then 
                 if (itest == 0) then
                   ibdyty = ibdyty + 1
                   bdyty2M_bdyty(ibdyty)   = iM_bdyty              
                   M2multi(iM_bdyty)%bdyty = ibdyty
                   itest=1
                 end if
                 iblmty = iblmty + 1
               end if
             end if
           end do
        end do
      end do
      if (itest /= 0) then
         allocate(bdyty(ibdyty)%blmty(iblmty))
         allocate(bdyty(ibdyty)%blmty2M_blmty(iblmty))

         allocate(M2multi(iM_bdyty)%blmty(size(M_bdyty(iM_bdyty)%blmty)))
         allocate(M2multi(iM_bdyty)%nodty(size(M_bdyty(iM_bdyty)%nodty)))

         M2multi(iM_bdyty)%blmty = 0
         M2multi(iM_bdyty)%nodty = 0

         !erosion
         allocate(bdyty(ibdyty)%eviz(iblmty))
         allocate(bdyty(ibdyty)%el_energy(iblmty))
         allocate(bdyty(ibdyty)%el_jacobian(iblmty))

         bdyty(ibdyty)%eviz        = 1
         bdyty(ibdyty)%el_energy   = 0.d0
         bdyty(ibdyty)%el_jacobian = 0.d0

      end if
    end do
    
    ! third scaning of the MAILx database: 
    ! filling the components of bdyty

    if( get_nb_bulk_behav() < 1 ) then
        call faterr(IAM,'Please call ReadBehaviours before LoadModels')
    end if

    do ibdyty = 1, size(bdyty)

      iM_bdyty = bdyty2M_bdyty(ibdyty)

      iblmty = 0
      do iM_blmty = 1, size(M_bdyty(iM_bdyty)%blmty)
        do iM_model = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
          do imodel = 1, size(modelz)
            if (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) then
              if (modelz(imodel)%mdlty == 'MULTI') then
                iblmty = iblmty + 1

                if( get_eleop_value(imodel,'isext') == 'yes__' ) then
                  call faterr(IAM,'Cannot use external models with MULTI model')
                end if

                blmnb =  get_nb_in_multiEF(modelz(imodel)%ID)
                bdyty(ibdyty)%blmty(iblmty)%blmnb = blmnb
                bdyty(ibdyty)%blmty(iblmty)%mdlnb = imodel

                bdyty(ibdyty)%blmty(iblmty)%lawnb = &
                get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))

                bdyty(ibdyty)%blmty2M_blmty(iblmty) = iM_blmty
                M2multi(iM_bdyty)%blmty(iM_blmty)   = iblmty 
                
                ! building connectivity table
                inodty = size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                allocate(bdyty(ibdyty)%blmty(iblmty)%NODES(inodty))
                bdyty(ibdyty)%blmty(iblmty)%NODES = 0

                !*! building maps between local and global dofs
                !*! building maps for elementary matrices and vectors
                
                idof = get_N_DOF_multiEF(modelz(imodel)%ID)
                !
                allocate(bdyty(ibdyty)%blmty(iblmty)%edof2gdof(idof))
                bdyty(ibdyty)%blmty(iblmty)%edof2gdof = 0
                
                allocate(bdyty(ibdyty)%blmty(iblmty)%mass(idof,idof))
                allocate(bdyty(ibdyty)%blmty(iblmty)%stiffness(idof,idof))
                allocate(bdyty(ibdyty)%blmty(iblmty)%damping(idof,idof))

                bdyty(ibdyty)%blmty(iblmty)%mass      = 0.d0
                bdyty(ibdyty)%blmty(iblmty)%stiffness = 0.d0
                bdyty(ibdyty)%blmty(iblmty)%damping   = 0.d0
                
                
                allocate(bdyty(ibdyty)%blmty(iblmty)%Fext(idof,2))
                allocate(bdyty(ibdyty)%blmty(iblmty)%Fint(idof,2))
                bdyty(ibdyty)%blmty(iblmty)%Fext = 0.d0
                bdyty(ibdyty)%blmty(iblmty)%Fint = 0.d0
                !
                allocate(bdyty(ibdyty)%blmty(iblmty)%RHSloc(idof))
                bdyty(ibdyty)%blmty(iblmty)%RHSloc = 0.d0


                ! building arrays holding Gauss points values

                grad_size     = modelz(imodel)%nb_external_variables
                internal_size = modelz(imodel)%nb_internal_variables

                nb_ef = get_external_field_nb(imodel) 
                nb_bf = get_bulk_field_nb(bdyty(ibdyty)%blmty(iblmty)%lawnb)
                nbf = nb_ef + nb_bf

                allocate(grad_sizes(nb_nf))
                do if = 1, nb_nf
                  grad_sizes(if) = get_grad_size_multiEF(blmnb,if)
                end do
                !paranoid check : about size of grad/flux arrays
                if( sum(grad_sizes) /= grad_size ) call faterr(IAM,'total grad size different of sum of each grad sizes')

                ! find the maximum number of Gauss points used to store the fields
                nb_gp = 0
                do if = 1, nb_eo ! nb_eo is a variable from a_multiEF_iso
                  nb_gp = max(nb_gp, get_N_GP_multiEF(blmnb, if))
                end do

                if (nbf /= 0) then
                  allocate(field_names(nbf))
                  do IF = 1, nb_ef
                    field_names(IF) = get_external_field_name(imodel,IF)
                  end do                      
                  do IF = 1, nb_bf
                     field_names(nb_ef+IF) = get_bulk_field_name(imodel,IF)
                  end do                      

                  call init_multigpv_MAILx(iM_bdyty,iM_blmty, nb_gp, grad_sizes, internal_size,&
                                           nbf, field_names)

                  deallocate(field_names)

                else

                  call init_multigpv_MAILx(iM_bdyty,iM_blmty, nb_gp, grad_sizes, internal_size)
                end if
!
                ! getting rank of the field to use for each operator and storing the map
                bdyty(ibdyty)%blmty(iblmty)%eo2fr = 0
                do if = 1, nb_eo
                  select case(if)
                  case( p_mass_s )
                    f_name = 'Ms'
                  case( p_mass_wf )
                    f_name = 'Mw'
                  case( p_mass_nwf )
                    f_name = 'Mn'
                  case( p_pc2disp_cpl )
                    f_name = 'Csw'
                  case( p_pn2disp_cpl )
                    f_name = 'Csn'
                  case( p_disp2pc_cpl )
                    f_name = 'Cws'
                  case( p_disp2pn_cpl )
                    f_name = 'Cns'
                  case( p_pc2pn_cpl )
                    f_name = 'Cnw'
                  case( p_pn2pc_cpl )
                    f_name = 'Cwn'
                  case( p_wf_compy )
                    f_name = 'Pww'
                  case( p_nwf_compy )
                    f_name = 'Pnn'
                  case( p_wf_permy )
                    f_name = 'Hww'
                  case( p_wf_permy_cpl)
                    f_name = 'Hwn'
                  case( p_nwf_permy )
                    f_name = 'Hnn'
                  case default
                    cycle
                  end select

                  nb_ef = get_multi_field_rank_MAILx(iM_bdyty,iM_blmty,f_name)
                  if( nb_ef == 0 ) call faterr(IAM, f_name//' external field not present')

                  bdyty(ibdyty)%blmty(iblmty)%eo2fr(if) = nb_ef
                end do
                deallocate(grad_sizes)
              end if
            end if
          end do
        end do
      end do

      if (iblmty == 0) call FATERR(IAM,'no blmty')

    end do

    ! perform some temporary computations
    ! we look for the nodes owning a multiMAILx
    ! 
    ! first we scan the MAILx database and we count the 
    ! nodes owning a MULTI element 
    !
    ! second we fill the bdyty ... database and we determine
    ! the nodty of a node which is the highest one
    !
    do ibdyty = 1, size(bdyty)

      iM_bdyty = bdyty2M_bdyty(ibdyty)
      inodty=0

      do iM_nodty = 1, size(M_bdyty(iM_bdyty)%nodty) 
        do iG_i = 1, size(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
          iM_blmty = M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)
          if (M2multi(iM_bdyty)%blmty(iM_blmty) /= 0) then
            inodty = inodty + 1
            exit
          end if
        end do
      end do
      if (inodty /= 0) then
        allocate(bdyty(ibdyty)%nodty(inodty))
        allocate(bdyty(ibdyty)%nodty2M_nodty(inodty))

        bdyty(ibdyty)%nb_nodes = inodty

        do inodty = 1, size(bdyty(ibdyty)%nodty)
          call new_nodty(bdyty(ibdyty)%nodty(inodty),'     ')
          bdyty(ibdyty)%nodty2M_nodty(inodty) = 0
        end do
      else
        call FATERR(IAM,'error computing size of bdyty%nodty')
      end if

      inodty=0
      do iM_nodty = 1, size(M_bdyty(iM_bdyty)%nodty) 
        itest = 0
        do iG_i = 1, size(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
          iM_blmty = M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)      
          if (M2multi(iM_bdyty)%blmty(iM_blmty) /= 0) then
            if (itest == 0) then
              inodty = inodty + 1
              bdyty(ibdyty)%nodty2M_nodty(inodty) = iM_nodty
              M2multi(iM_bdyty)%nodty(iM_nodty) = inodty
              itest=1
            end if
            ! fishing for element type...
            iblmty = M2multi(iM_bdyty)%blmty(iM_blmty)
            blmnb  = bdyty(ibdyty)%blmty(iblmty)%blmnb
            do i = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
              ! I really am on the node
              if (M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(i) ==  inodty) then
                ! fishing for nodes type of the element...
                itempo = get_N_DOF_of_NODE_multiEF(blmnb, i)
                if( (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) < itempo) ) &
                  call new_nodty(bdyty(ibdyty)%nodty(inodty),get_node_name_from_id(itempo))
              end if
            end do
          end if
        end do
      end do
    end do

    do iM_bdyty = 1, size(M2multi)
      if (M2multi(iM_bdyty)%bdyty /=0) then
        do iM_blmty = 1, size(M2multi(iM_bdyty)%blmty)
          if (M2multi(iM_bdyty)%blmty(iM_blmty) /=0) then
             ibdyty = M2multi(iM_bdyty)%bdyty
             iblmty = M2multi(iM_bdyty)%blmty(iM_blmty)
             do inodty = 1, size(bdyty(ibdyty)%blmty(iblmty)%NODES)
               iM_nodty = M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)
               bdyty(ibdyty)%blmty(iblmty)%NODES(inodty) = M2multi(iM_bdyty)%nodty(iM_nodty)
             end do
          end if
        end do
      end if
    end do


    do ibdyty = 1, size(bdyty)

      iccdof = 0

      do inodty = 1, size(bdyty(ibdyty)%nodty)
        iccdof = iccdof + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
      end do

      bdyty(ibdyty)%nbdof = iccdof
      if (iccdof /= 0) then
        allocate(bdyty(ibdyty)%nodnb(iccdof))
        allocate(bdyty(ibdyty)%dofnb(iccdof))

        allocate(bdyty(ibdyty)%Dofs(iccdof,2))
        allocate(bdyty(ibdyty)%DofsLast(iccdof))

        allocate(bdyty(ibdyty)%X(nbDIME,bdyty(ibdyty)%nb_nodes,2))

        allocate(bdyty(ibdyty)%DofsFree(iccdof))
        allocate(bdyty(ibdyty)%DofsAux(iccdof))

        allocate(bdyty(ibdyty)%Fext(iccdof))
        allocate(bdyty(ibdyty)%Fint(iccdof))
        allocate(bdyty(ibdyty)%Fdmp(iccdof))

        allocate(bdyty(ibdyty)%residu(iccdof))
        allocate(bdyty(ibdyty)%Fdyn(iccdof))

        allocate(bdyty(ibdyty)%Ireac(iccdof))
        allocate(bdyty(ibdyty)%Iaux(iccdof))

        !rm : \todo later
        !! Gestion des vitesses Methode ALE
        !ALLOCATE(bdyty(ibdyty)%V_ALE_begin(iccdof),stat=errare)
        !ALLOCATE(bdyty(ibdyty)%V_ALE(iccdof),stat=errare)
        !ALLOCATE(bdyty(ibdyty)%Mask_ALE(bdyty(ibdyty)%nbdof),stat=errare)
        !ALLOCATE(bdyty(ibdyty)%Mask_No_ALE(bdyty(ibdyty)%nbdof),stat=errare)
        
        !ALLOCATE(bdyty(ibdyty)%periodicnode(iccdof),stat=errare)

        bdyty(ibdyty)%nodnb = 0
        bdyty(ibdyty)%dofnb = 0
        
        bdyty(ibdyty)%Dofs      = 0.d0
        bdyty(ibdyty)%Dofslast  = 0.d0

        bdyty(ibdyty)%X         = 0.d0

        bdyty(ibdyty)%DofsFree  = 0.d0
        bdyty(ibdyty)%DofsAux   = 0.d0

        bdyty(ibdyty)%Fext      = 0.d0 
        bdyty(ibdyty)%Fint      = 0.d0
        bdyty(ibdyty)%Fdmp      = 0.d0

        bdyty(ibdyty)%residu    = 0.d0
        bdyty(ibdyty)%Fdyn      = 0.d0

        bdyty(ibdyty)%Ireac     = 0.d0
        bdyty(ibdyty)%Iaux      = 0.d0

        !rm : \todo later
        !! Gestion des vitesses Methode ALE
        !bdyty(ibdyty)%V_ALE_begin=0.d0
        !bdyty(ibdyty)%V_ALE      =0.d0
        !bdyty(ibdyty)%Mask_ALE   =0
        !bdyty(ibdyty)%Mask_No_ALE=1
          
        !bdyty(ibdyty)%is_precon=.FALSE.

        bdyty(ibdyty)%visible     = .true.
        !bdyty(ibdyty)%is_periodic = .FALSE.

        !bdyty(ibdyty)%periodicnode = 0
          
        allocate(bdyty(ibdyty)%coorTT(nbdime,size(bdyty(ibdyty)%nodty)))
        bdyty(ibdyty)%coorTT  = 0.d0

        allocate(bdyty(ibdyty)%RHS(iccdof))
        bdyty(ibdyty)%RHS       = 0.d0
 
        allocate(bdyty(ibdyty)%flying_nodes(size(bdyty(ibdyty)%nodty)))
        bdyty(ibdyty)%flying_nodes = 0

      else 

        nullify(bdyty(ibdyty)%nodnb)
        nullify(bdyty(ibdyty)%dofnb)

        nullify(bdyty(ibdyty)%Dofs)
        nullify(bdyty(ibdyty)%DofsLast)

        nullify(bdyty(ibdyty)%X)

        nullify(bdyty(ibdyty)%DofsFree)
        nullify(bdyty(ibdyty)%DofsAux)

        nullify(bdyty(ibdyty)%Fext)
        nullify(bdyty(ibdyty)%Fint)
        nullify(bdyty(ibdyty)%Fdmp)

        nullify(bdyty(ibdyty)%residu)
        nullify(bdyty(ibdyty)%Fdyn)

        nullify(bdyty(ibdyty)%Ireac)
        nullify(bdyty(ibdyty)%Iaux)

        !NULLIFY(bdyty(ibdyty)%V_ALE_begin)
        !NULLIFY(bdyty(ibdyty)%V_ALE)
        !NULLIFY(bdyty(ibdyty)%Mask_ALE)
        !NULLIFY(bdyty(ibdyty)%Mask_No_ALE)
        !
        !NULLIFY(bdyty(ibdyty)%periodicnode)

        !bdyty(ibdyty)%is_precon=.FALSE.          

        bdyty(ibdyty)%visible     = .true.
        !bdyty(ibdyty)%is_periodic = .FALSE.

        nullify(bdyty(ibdyty)%coorTT)

        nullify(bdyty(ibdyty)%RHS)
          
        write(cout,'(A,1x,I0,1x,A)') 'Warning: body', ibdyty, 'without DOF'
        call LOGMES(cout)
      end if

      !array node -> first global ddl
      if (size(bdyty(ibdyty)%nodty) /= 0) then
        allocate(bdyty(ibdyty)%ccdof(size(bdyty(ibdyty)%nodty)+1))
        allocate(bdyty(ibdyty)%ccsize(nb_nf,size(bdyty(ibdyty)%nodty)))
        bdyty(ibdyty)%ccsize = 0
      else 
        nullify(bdyty(ibdyty)%ccdof)
        nullify(bdyty(ibdyty)%ccsize)
        write(cout,'(A,1x,I0,1x,A)') 'Warning MAILx', ibdyty, 'without node'
        call LOGMES(cout)
      end if
    end do

    !second: filling ordering arrays, and element local 2 global dof correspondance
    do ibdyty = 1, size(bdyty)

      iccdof = 0
      do inodty = 1, size(bdyty(ibdyty)%nodty)
        bdyty(ibdyty)%ccdof(inodty) = iccdof ! mapping
        do idof = 1, nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          iccdof = iccdof + 1
          bdyty(ibdyty)%nodnb(iccdof) = inodty ! reverse mapping
          bdyty(ibdyty)%dofnb(iccdof) = idof   ! reverse mapping
        end do
        !\todo rm: this ccsize filling is really dirty... to improve !
        bdyty(ibdyty)%ccsize(p_disp,inodty) = nbDIME ! disp always present and first
        if( nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME ) then
          bdyty(ibdyty)%ccsize(p_pc,inodty) = nbDIME + 1
          bdyty(ibdyty)%ccsize(p_pn,inodty) = nbDIME + 2
        end if
      end do
      
      bdyty(ibdyty)%ccdof(size(bdyty(ibdyty)%nodty)+1) = iccdof
      
      max_conn = 0
      do iblmty = 1, size(bdyty(ibdyty)%blmty)
        iccdof = 0
        max_conn = max(max_conn, size(bdyty(ibdyty)%blmty(iblmty)%NODES))
        do itempo = 1, size(bdyty(ibdyty)%blmty(iblmty)%NODES)
          inodty = bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
          do idof = 1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
            iccdof = iccdof + 1
            bdyty(ibdyty)%blmty(iblmty)%edof2gdof(iccdof) = bdyty(ibdyty)%ccdof(inodty) + idof
          end do
        end do
      end do

      ! connectivity     
      connectivities => get_ll_connectivity_multiMAILx(ibdyty)

      max_nod2el = get_max_nod2el(bdyty2M_bdyty(ibdyty))
      max_dofs_adj = max_nod2el * max_conn * (nbDIME+2)

      call initialize_system(bdyty(ibdyty)%g_sys,Matrix_storage,Matrix_shape,bdyty(ibdyty)%ccdof,connectivities,max_dofs_adj)

      do while( associated(connectivities) )
        tmp => connectivities%n
        deallocate(connectivities%connec)
        deallocate(connectivities)
        connectivities => tmp
      end do

      !!! Right hand side
      !!allocate(bdyty(ibdyty)%RHS(bdyty(ibdyty)%nbdof))
      !!bdyty(ibdyty)%RHS = 0.d0

      iM_bdyty = bdyty2M_bdyty(ibdyty)

      !creating mask to compute edge values of a dof if the dof is not computed by the finite elment
      allocate(bdyty(ibdyty)%mask(2,bdyty(ibdyty)%nb_nodes))
      
      do inodty = 1, bdyty(ibdyty)%nb_nodes
      
        !print *,'Etude du noeud : ',inodty

        ! looking for the first found edge2vertices map of elements node i_nodty belongs to
        iM_nodty = bdyty(ibdyty)%nodty2M_nodty(inodty)
        do iG_i = 1, size(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
          iM_blmty = M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)
          iblmty  = M2multi(iM_bdyty)%blmty(iM_blmty)
          edge2vertices => get_ptr_edge2vertices_multiEF(bdyty(ibdyty)%blmty(iblmty)%blmnb)
          if( associated(edge2vertices) ) exit
        end do

        ! filling mask depending on edge2vertices 0ness

        ! looking for node in element local numbering
        do i = 1, size(bdyty(ibdyty)%blmty(iblmty)%NODES)
          if (bdyty(ibdyty)%blmty(iblmty)%NODES(i) == inodty) exit
        end do

        if( any(edge2vertices(:,i)==0) ) then
          bdyty(ibdyty)%mask(1,inodty) = inodty
          bdyty(ibdyty)%mask(2,inodty) = 0
        else
          bdyty(ibdyty)%mask(1,inodty) = bdyty(ibdyty)%blmty(iblmty)%NODES(edge2vertices(1,i))
          bdyty(ibdyty)%mask(2,inodty) = bdyty(ibdyty)%blmty(iblmty)%NODES(edge2vertices(2,i))
        end if

      end do

    end do
   
    ! optional display
    if (itchache) then
      write(*,'(A,1x,I0)') 'number of multiMAILx bodies:', size(bdyty)
      do ibdyty = 1, size(bdyty)
        write(*,'(A)') '==========================================='
        write(*,'(A,1x,I0)') 'Body:', ibdyty
        write(*,'(A,1x,I0)') 'Match in MAILx database:', bdyty2M_bdyty(ibdyty)
        write(*,'(A,1x,I0)') 'PARANOIAC TEST local->global->local', M2multi(bdyty2M_bdyty(ibdyty))%bdyty
        write(*,'(A)') '**nodty************'
        do inodty = 1, size(bdyty(ibdyty)%nodty)
          write(*,'(A,1x,I0)') 'Node:', inodty
          write(*,'(A,1x,A)')  'Node type:', get_nodNAME(bdyty(ibdyty)%nodty(inodty))
          write(*,'(A,1x,I0)') 'Match in MAILx database:', bdyty(ibdyty)%nodty2M_nodty(inodty)
          write(*,'(A,1x,I0,1x,A,1x,I0)') 'dofs starting at',bdyty(ibdyty)%ccdof(inodty), &
                                          'nombre:', nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          write(*,'(A,1x,I0)') 'PARANOIAC TEST local->global->local', &
                               M2multi(bdyty2M_bdyty(ibdyty))%nodty(bdyty(ibdyty)%nodty2M_nodty(inodty))
        end do
        write(*,'(A)') '**blmty************'
        do iblmty = 1, size(bdyty(ibdyty)%blmty)
          write(*,'(A,1x,I0)') 'Element:', iblmty
          write(*,*) 'Connectivity:', bdyty(ibdyty)%blmty(iblmty)%NODES(:)
          write(*,'(A,1x,I0)') 'EF number in list of multiEF:', bdyty(ibdyty)%blmty(iblmty)%blmnb
          imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
          write(*,'(A,1x,A,1x,A,1x,I0)') 'model ID:', modelz(imodel)%ID,'| model number:',imodel 
          write(*,'(A,1x,I0)') 'Match in MAILx database:', bdyty(ibdyty)%blmty2M_blmty(iblmty)
        end do
      end do
      
      write(*,'(A)') 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

    end if

  end subroutine load_models_multiMAILx

  !----------------------------------------------------------------
  !> \brief get the connectivies of every elements of a multiMAILx
  function get_connectivities_multiMAILx(ibdyty)
    implicit none
    !> multiMAILx index
    integer(kind=4), intent(in) :: ibdyty
    !> array holding number of element and for each element size of connectivity and connectivity
    integer(kind=4), dimension(:), pointer :: get_connectivities_multiMAILx
    ! ***
    integer :: sz,iblmty,inode

    get_connectivities_multiMAILx => null()

    ! counting
    sz = 1
    do iblmty = 1, size(bdyty(ibdyty)%blmty)
      sz = sz + size(bdyty(ibdyty)%blmty(iblmty)%NODES) + 1
    end do

    ! allocating
    allocate(get_connectivities_multiMAILx(sz)) 

    ! filling
    sz = 1
    get_connectivities_multiMAILx(sz) = size(bdyty(ibdyty)%blmty)
    do iblmty = 1, size(bdyty(ibdyty)%blmty)
      sz = sz + 1
      get_connectivities_multiMAILx(sz) = size(bdyty(ibdyty)%blmty(iblmty)%NODES)
      do inode=1,size(bdyty(ibdyty)%blmty(iblmty)%NODES)
        sz = sz + 1
        get_connectivities_multiMAILx(sz) = bdyty(ibdyty)%blmty(iblmty)%NODES(inode)
      end do
    end do

  end function

  !> \brief Get the connectivity of all elements in a linked list
  function get_ll_connectivity_multiMAILx(ibdyty)
    implicit none
    !> multiMAILx index
    integer(kind=4), intent(in)  :: ibdyty
    !> Root of the linked list, first cell holds the number of elements
    type(T_link_connec), pointer :: get_ll_connectivity_multiMAILx
    !
    integer(kind=4) :: nb_elem, iblmty
    type(T_link_connec), pointer :: last, new

    allocate( get_ll_connectivity_multiMAILx )
    allocate( get_ll_connectivity_multiMAILx%connec(1) )

    nb_elem = size(bdyty(ibdyty)%blmty)

    get_ll_connectivity_multiMAILx%connec(1) = nb_elem

    last => get_ll_connectivity_multiMAILx

    do iblmty = 1, nb_elem
      allocate( new )
      allocate( new%connec(size(bdyty(ibdyty)%blmty(iblmty)%NODES)) )
      new%connec = bdyty(ibdyty)%blmty(iblmty)%NODES
      last%n => new
      last   => new
    end do

  end function

  !> \brief get back property set rank from the model and material number
  subroutine push_ppset_multiMAILx
    implicit none
    !
    integer(kind=4) :: i_bdyty,i_blmty,i_behav,i_model
    integer(kind=4) :: iM_bdyty,iM_blmty,iM_behav
    integer(kind=4) :: i_gp, nb_gp, i_p

    character(len=22)  :: IAM
    character(len=103) :: cout

    !      1234567890123456789012
    IAM = 'multiMAILx::push_ppset'
  
    if (nb_multiMAILx == 0) return
  
    do i_bdyty = 1, size(bdyty)
      do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
  
        i_model = bdyty(i_bdyty)%blmty(i_blmty)%mdlnb
        i_behav = bdyty(i_bdyty)%blmty(i_blmty)%lawnb
  
        !!! rm : sort this story of ppset allocation !
        !!!      just mimic current form of ppset in meca/poro
        !!nb_gp = 1
        !!do i_p = 1, nb_physics
        !!  nb_gp = max(nb_gp, get_N_GP_multiEF(modelz(i_model)%ID, physics_id(i_p)))
        !!end do

        !!allocate(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb(nb_gp))
  
        !!do i_gp = 1,nb_gp
        bdyty(i_bdyty)%blmty(i_blmty)%ppsnb = get_ppset_nb(use_existing_ppset,i_model,i_behav)
        !!end do
      end do
    end do  
    
  end subroutine push_ppset_multiMAILx

  !----------------------------------------------------------------------!
  ! Setter and getter
  !----------------------------------------------------------------------!
  !> \brief Get values of a vector of a body
  subroutine get_vector_multiMAILx(id_vect, i_bdyty, vect, s1, s2)
    implicit none
    !> data type to get
    character(len=5), intent(in) :: id_vect
    !> body id
    integer(kind=4), intent(in) :: i_bdyty
    !> first dimension size of vect
    integer(kind=4), intent(in) :: s1
    !> second dimension size of vect
    integer(kind=4), intent(in) :: s2
    !> vector to get
    real(kind=8), dimension(s1,s2), intent(out) :: vect
    !
    integer(kind=4)   :: i_nodty, iccdof, iM_bdyty, iM_nodty, iM_ccdof
    character(len=22) :: IAM
    character(len=80) :: cout
    !      1234567890123456789012
    IAM = 'multiMAILx::get_vector'

    select case(id_vect)
    case('Coor0')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Coor0. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      iM_bdyty = bdyty2M_bdyty(i_bdyty)
      do i_nodty = 1, bdyty(i_bdyty)%nb_nodes
        iccdof   = bdyty(i_bdyty)%ccdof(i_nodty)
        iM_nodty = bdyty(i_bdyty)%nodty2M_nodty(i_nodty)
        iM_ccdof = M_bdyty(iM_bdyty)%ccdof(iM_nodty)

        vect(:,i_nodty) = M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME)
      end do

    case('Xbeg_')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Xbeg_. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      vect = bdyty(i_bdyty)%X(:,:,2)
    case('X____')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for X____. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      vect = bdyty(i_bdyty)%X(:,:,1)
    case('Vbeg_')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Vbeg_. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Dofs(:,2), vect)
    case('V____')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for V____. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Dofs(:,1), vect)
    case('Pcbeg')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Pcbeg. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Dofs(:,2), vect)
    case('Pc___')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Pc___. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Dofs(:,1), vect)
    case('Pnbeg')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Pnbeg. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pn, bdyty(i_bdyty)%Dofs(:,2), vect)
    case('Pn___')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for Pn___. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pn, bdyty(i_bdyty)%Dofs(:,1), vect)
    case('U_Fex')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for U_Fex. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Fext, vect)
    case('PcFex')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PcFex. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Fext, vect)
    case('PnFex')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PnFex. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pn, bdyty(i_bdyty)%Fext, vect)
    case('U_Fin')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for U_Fin. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Fint, vect)
    case('PcFin')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PcFin. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Fint, vect)
    case('PnFin')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PnFin. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pn, bdyty(i_bdyty)%Fint, vect)
    case('U_Fdp')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for U_Fdp. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Fdmp, vect)
    case('PcFdp')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PcFdp. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Fdmp, vect)
    case('PnFdp')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PnFdp. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pn, bdyty(i_bdyty)%Fdmp, vect)
    case('U_Fdy')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for U_Fdy. Is:',s1, s2, &
                        'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Fdyn, vect)
    case('PcFdy')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PcFdy. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Fdyn, vect)
    case('PnFdy')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,100) 'wrong size for PnFdy. Is:',s1, s2, &
                        'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call get_physic_values(i_bdyty, p_pn, bdyty(i_bdyty)%Fdyn, vect)
    case default
      write(cout,'(A,1x,A)') 'Unknown vector id to get: ', id_vect
      call faterr(IAM,cout)
    end select

100 format(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)

  end subroutine

  !> \brief Get the vector of dofs
  !> Assume that vect is of the right size...
  subroutine get_physic_values(i_bdyty, i_nf, big_vect, vect)
    implicit none
    !> body id
    integer(kind=4), intent(in) :: i_bdyty
    !> dof type to get
    integer(kind=4), intent(in) :: i_nf
    !> vector to get from
    real(kind=8), dimension(:)  , intent(in)  :: big_vect
    !> vector to get
    real(kind=8), dimension(:,:), intent(out) :: vect
    !
    integer(kind=4) :: i, nf_size, iccdof
    integer(kind=4), dimension(nb_nf) :: shift

    ! \todo: better than that somewhere else
    shift(1) = 1
    shift(2) = nbDIME+1
    shift(3) = nbDIME+2

    do i = 1, bdyty(i_bdyty)%nb_nodes

      nf_size = bdyty(i_bdyty)%ccsize(i_nf,i)

      if ( nf_size > 0 ) then
        ! nodes with dofs
        iccdof = bdyty(i_bdyty)%ccdof(i)
        vect(:,i) = big_vect(iccdof+shift(i_nf):iccdof+nf_size)
      else
        ! nodes without dofs
        iccdof  = bdyty(i_bdyty)%ccdof(bdyty(i_bdyty)%mask(1,i))
        nf_size = bdyty(i_bdyty)%ccsize(i_nf,bdyty(i_bdyty)%mask(1,i))
        vect(:,i) = big_vect(iccdof+shift(i_nf):iccdof+nf_size)

        iccdof  = bdyty(i_bdyty)%ccdof(bdyty(i_bdyty)%mask(2,i))
        nf_size = bdyty(i_bdyty)%ccsize(i_nf,bdyty(i_bdyty)%mask(2,i))
        vect(:,i) = vect(:,i) + big_vect(iccdof+shift(i_nf):iccdof+nf_size)

        vect(:,i) = vect(:,i) * 0.5
      end if
    end do

  end subroutine
!--------------------------------------------------------------------------
  subroutine Set_Matrix_Storage_multiMAILx(type)
    implicit none
    character(len=8) :: type
    character(len=30) :: IAM
  
        !123456789012345678901234567890
    IAM='multiMAILx::set_matrix_storage'

    Matrix_storage=get_matrix_storage_id_from_name(type)

    if (Matrix_storage == -99) call faterr(IAM,'unknown storage type '//type)

  end subroutine set_Matrix_Storage_multiMAILx
!--------------------------------------------------------------------------
  subroutine Set_Matrix_Shape_multiMAILx(type)
    implicit none
    character(len=8) :: type
    character(len=28) :: IAM

        !1234567890123456789012345678
    IAM='multiMAILx::set_matrix_shape'

    Matrix_shape=get_matrix_shape_id_from_name(type)

    if (Matrix_storage == -99) call faterr(IAM,'unknown shape type '//type)

  end subroutine set_Matrix_Shape_multiMAILx
!--------------------------------------------------------------------------
  !> \brief Set values of a vector of a body
  subroutine set_vector_multiMAILx(id_vect, i_bdyty, vect, s1, s2)
    implicit none
    !> data type to set
    character(len=5), intent(in) :: id_vect
    !> body id
    integer(kind=4), intent(in) :: i_bdyty
    !> first dimension size of vect
    integer(kind=4), intent(in) :: s1
    !> second dimension size of vect
    integer(kind=4), intent(in) :: s2
    !> vector to set
    real(kind=8), dimension(s1,s2), intent(in) :: vect
    !
    character(len=22) :: IAM
    character(len=80) :: cout
    !      1234567890123456789012
    IAM = 'multiMAILx::set_vector'

    select case(id_vect)
    case('Xbeg_')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for Xbeg_. Is:',s1, s2, &
                                                       'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      bdyty(i_bdyty)%X(:,:,2) = vect
    case('X____')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for X____. Is:',s1, s2, &
                                                       'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      bdyty(i_bdyty)%X(:,:,1) = vect
    case('Vbeg_')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for Vbeg_. Is:',s1, s2, &
                                                       'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call set_dofs_multiMAILx(i_bdyty, p_disp, 2, vect)
    case('V____')
      if( s1 /= nbDIME .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for V____. Is:',s1, s2, &
                                                       'should be:', nbDIME, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call set_dofs_multiMAILx(i_bdyty, p_disp, 1, vect)
    case('Pcbeg')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for Pcbeg. Is:',s1, s2, &
                                                       'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call set_dofs_multiMAILx(i_bdyty, p_pc, 2, vect)
    case('Pc___')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for Pc___. Is:',s1, s2, &
                                                       'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call set_dofs_multiMAILx(i_bdyty, p_pc, 1, vect)
    case('Pnbeg')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for Pnbeg. Is:',s1, s2, &
                                                       'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call set_dofs_multiMAILx(i_bdyty, p_pn, 2, vect)
    case('Pn___')
      if( s1 /= 1 .or. s2 /= bdyty(i_bdyty)%nb_nodes ) then
        write(cout,'(A,1x,I0,1x,I0,1x,A,1x,I0,1x,I0)') 'wrong size for Pn___. Is:',s1, s2, &
                                                       'should be:', 1, bdyty(i_bdyty)%nb_nodes
        call faterr(IAM,cout)
      end if
      call set_dofs_multiMAILx(i_bdyty, p_pn, 1, vect)
    case default
      write(cout,'(A,1x,A)') 'Unknown vector id to get: ', id_vect
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Set the vector of dofs
  !> Assume that vect is of the right size...
  subroutine set_dofs_multiMAILx(i_bdyty, i_nf, i_depth, vect)
    implicit none
    !> body id
    integer(kind=4), intent(in) :: i_bdyty
    !> dof type to get
    integer(kind=4), intent(in) :: i_nf
    !> depth: 1=current, 2=begin
    integer(kind=4), intent(in) :: i_depth
    !> vector to set
    real(kind=8), dimension(:,:), intent(in) :: vect
    !
    integer(kind=4) :: i, nf_size, iccdof
    integer(kind=4), dimension(nb_nf) :: shift

    ! \todo: better than that somewhere else
    shift(1) = 1
    shift(2) = nbDIME+1
    shift(3) = nbDIME+2

    do i = 1, bdyty(i_bdyty)%nb_nodes

      nf_size = bdyty(i_bdyty)%ccsize(i_nf,i)

      if( nf_size > 0 ) then
        iccdof = bdyty(i_bdyty)%ccdof(i)
        bdyty(i_bdyty)%Dofs(iccdof+shift(i_nf):iccdof+nf_size,i_depth) = vect(:,i)
      end if
    end do

  end subroutine

  !> initializing X,V,P for the first step iteration;    
  !> initializing is set as follows allowing a single theta method iteration
  !> for constant linear system; 
  subroutine increment_multiMAILx 
    implicit none 
    integer(kind=4) :: i_bdyty

    if (nb_multiMAILx == 0) return

    ! initialisation
    do i_bdyty = 1, size(bdyty)

      if (.not. bdyty(i_bdyty)%visible) cycle

      bdyty(i_bdyty)%Dofs(:,1) = bdyty(i_bdyty)%Dofs(:,2)

      ! X = Xbeg + H*Vbeg
      call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Dofs(:,2), bdyty(i_bdyty)%X(:,:,1))
      bdyty(i_bdyty)%X(:,:,1) = bdyty(i_bdyty)%X(:,:,2) + H*bdyty(i_bdyty)%X(:,:,1)
    end do

  end subroutine increment_multiMAILx 

  !----------------------------------------------------------------------!
  ! Elementary mass matrices computation [Me]=[Ms ;  0   ; 0  ] [dV /dt] !
  !                                           [Mw ; Pww ; Cnw ] [dPc/dt] !  
  !                                           [Mn ! Cwn ; Pnn ] [dPn/dt] !
  !----------------------------------------------------------------------!
  !> \brief Compute mass matrix of a multiMAILx
  subroutine compute_mass_multiMAILx(i_bdyty)
    implicit none
    !> id of the multiMAILx to compute mass matrix
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: iM_bdyty, iM_blmty, i_blmty, i_model, blmnb, i
    integer(kind=4) :: nb_mass_eo, i_eo, field_rank
    integer(kind=4), dimension(:)  , allocatable :: operators
    real(kind=8)   , dimension(:)  , pointer     :: field_ele
    real(kind=8)   , dimension(:,:), pointer     :: coor_ele
    character(len=24) :: IAM
    character(len=80) :: cout
    !      123456789012345678901234
    IAM = 'multiMAILx::compute_mass'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    ! 7 operators: 3 for masses, 2 for compressibility and 2 for coupling
    nb_mass_eo = 7 
    allocate(operators(nb_mass_eo))
    operators = (/ p_mass_s, p_mass_wf, p_mass_nwf, &
                   p_wf_compy, p_nwf_compy, &
                   p_pc2pn_cpl, p_pn2pc_cpl &
                /)

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      blmnb    = bdyty(i_bdyty)%blmty(i_blmty)%blmnb
      i_model  = bdyty(i_bdyty)%blmty(i_blmty)%mdlnb

      coor_ele => get_ptr_coor_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)) 

      do i_eo = 1, nb_mass_eo
        field_ele => get_ptr_field_ele_multiEF(blmnb, operators(i_eo))
        field_rank = bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(operators(i_eo))
        if( field_rank > 0 ) then
          call get_multi_field_MAILx(iM_bdyty, iM_blmty, 1, field_rank, field_ele)
        else
          ! identity
          field_ele = 1.
        end if
        ! almost no parameters since the operator uses its own internal working arrays field just above
        call compute_elementary_operator(blmnb, operators(i_eo), bdyty(i_bdyty)%blmty(i_blmty)%ppsnb,&
                                         bdyty(i_bdyty)%eviz(i_blmty))
        call put_in_augmented_matrix_multiEF(blmnb, operators(i_eo), bdyty(i_bdyty)%blmty(i_blmty)%mass)
      end do

!!$      print*,'+++m'
!!$      do i=1,size(bdyty(i_bdyty)%blmty(i_blmty)%mass,dim=1)
!!$        print*,bdyty(i_bdyty)%blmty(i_blmty)%mass(i,i)
!!$      enddo
!!$      print*,'+++m'

    end do

    deallocate(operators)

  end subroutine

  !-------------------------------------------------------------------------!
  ! Elementary stiffness matrices computation  [Ke]=[K ; 0 ; 0 ] [u]        !
  !                                                 [0 ; 0 ; 0 ] [int Pc dt]!
  !                                                 [0 ; 0 ; 0 ] [int Pn dt]!
  !-------------------------------------------------------------------------!
  !> \brief Compute stiffness matrix of a multiMAILx
  subroutine compute_bulk_multiMAILx(i_bdyty)
    implicit none
    !> id of the multiMAILx to compute bulk matrix
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: iM_bdyty, iM_blmty, i_blmty, i_nodty, blmnb
    integer(kind=4) :: field_rank, i, iccdof
    real(kind=8)   , dimension(:)  , pointer     :: field_ele, disp_ele
    real(kind=8)   , dimension(:,:), pointer     :: coor_ele
    character(len=24) :: IAM
    character(len=80) :: cout
    !      123456789012345678901234
    IAM = 'multiMAILx::compute_bulk'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      blmnb    = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      coor_ele => get_ptr_coor_ele_multiEF(blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)) 

      bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1) = 0.d0

      field_ele => get_ptr_field_ele_multiEF(blmnb, p_stiffness)
      field_rank = bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(p_stiffness)
      if( field_rank > 0 ) then
        call get_multi_field_MAILx(iM_bdyty, iM_blmty, 1, field_rank, field_ele)
      else
        field_ele = 1.
      end if

      ! intermediate disp for incremental formulation
      ! disp_ele = Xbegin + THETA*H*( (1-THETA)*Vbegin + THETA*V )
      disp_ele => get_ptr_node_field_multiEF(blmnb, p_disp)
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
        iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)

        if (is_new) then

          disp_ele((i-1)*nbDIME+1:i*nbDIME) =  bdyty(i_bdyty)%X(:,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i),2)       &
                                              +H*theta*(1.d0-theta)*bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbDIME,2) 
        else 

          disp_ele((i-1)*nbDIME+1:i*nbDIME) =  bdyty(i_bdyty)%X(:,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i),2)       &
                                              +H*theta*(1.d0-theta)*bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbDIME,2) &
                                              +H*theta*theta       *bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbDIME,1)
        endif

      end do

      !almost no parameters since the operator uses its own internal working arrays field just above
      call compute_elementary_operator(blmnb, p_stiffness, bdyty(i_bdyty)%blmty(i_blmty)%ppsnb, &
                                       bdyty(i_bdyty)%eviz(i_blmty))

      call put_in_augmented_matrix_multiEF(blmnb, p_stiffness, bdyty(i_bdyty)%blmty(i_blmty)%stiffness)
      call put_in_augmented_vector_multiEF(blmnb, p_stiffness, bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1))

    end do

  end subroutine

  !> \brief Compute flux field of some operators of a multiMAILx
  subroutine compute_flux_multiMAILx(i_bdyty)
    implicit none
    !> id of the multiMAILx to compute bulk matrix
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: iM_bdyty, iM_blmty, i_blmty, i_nodty, blmnb, nf_size
    integer(kind=4) :: field_rank, i, iccdof, i_nf, i_eo(3), shift(3)
    real(kind=8)   , dimension(:)  , pointer     :: field_ele, dof_ele
    real(kind=8)   , dimension(:,:), pointer     :: coor_ele
    real(kind=8)   , dimension(:,:), pointer :: grad_ele, flux_ele
    character(len=24) :: IAM
    character(len=80) :: cout
    !      123456789012345678901234
    IAM = 'multiMAILx::compute_flux'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    ! dirty...
    shift(1) = 1
    shift(2) = nbDIME+1
    shift(3) = nbDIME+2

    i_eo(p_disp) = p_stiffness
    i_eo(p_pc)   = p_wf_permy
    i_eo(p_pn)   = p_nwf_permy

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      blmnb    = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      coor_ele => get_ptr_coor_ele_multiEF(blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)) 

      do i_nf = 1, nb_nf

        field_ele => get_ptr_field_ele_multiEF(blmnb, i_eo(i_nf))
        field_rank = bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(i_eo(i_nf))
        if( field_rank > 0 ) then
          call get_multi_field_MAILx(iM_bdyty, iM_blmty, 1, field_rank, field_ele)
        else
          field_ele = 1.
        end if

        ! last computed value of dof/disp
        ! dof_ele = X/P
        ! rm: not independent of physics size...
        dof_ele => get_ptr_node_field_multiEF(blmnb, i_nf)
        if( i_nf == p_disp ) then
          do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
            dof_ele((i-1)*nbDIME+1:i*nbDIME) =  bdyty(i_bdyty)%X(:,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i),1)
          end do
        else
          do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
            i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
            nf_size = bdyty(i_bdyty)%ccsize(i_nf,i_nodty)
            if( nf_size > 0 ) then
              iccdof     = bdyty(i_bdyty)%ccdof(i_nodty)
              dof_ele(i) = bdyty(i_bdyty)%Dofs(iccdof+shift(i_nf),2)
            end if
          end do
        end if

        grad_ele => get_ptr_grad_field_ele_multiEF(blmnb, i_eo(i_nf))
        flux_ele => get_ptr_flux_field_ele_multiEF(blmnb, i_eo(i_nf))

        call get_multi_grad_MAILx(iM_bdyty, iM_blmty, i_nf, 2, grad_ele)
        call get_multi_flux_MAILx(iM_bdyty, iM_blmty, i_nf, 2, flux_ele)

        call compute_elementary_flux(blmnb, i_eo(i_nf), bdyty(i_bdyty)%blmty(i_blmty)%ppsnb, &
                                     bdyty(i_bdyty)%eviz(i_blmty))

        call set_multi_grad_MAILx(iM_bdyty, iM_blmty, i_nf, 1, grad_ele)
        call set_multi_flux_MAILx(iM_bdyty, iM_blmty, i_nf, 1, flux_ele)

      end do

    end do

  end subroutine

  !----------------------------------------------------------------------!
  ! Elementary damping matrices computation [De]=[ 0   ; Csw ; Csn ] [V ]!
  !                                              [ Cws ; Hww ; Hwn ] [Pc]!  
  !                                              [ Cns ;  0  ; Hnn ] [Pn]!
  !----------------------------------------------------------------------!
  !> \brief Compute damping matrix of a multiMAILx
  subroutine compute_damping_multiMAILx(i_bdyty)
    implicit none
    !> id of the multiMAILx to compute damping matrix
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: iM_bdyty, iM_blmty, i_blmty, i_model, blmnb, i
    integer(kind=4) :: nb_damp_eo, i_eo, field_rank
    integer(kind=4), dimension(:)  , allocatable :: operators
    real(kind=8)   , dimension(:)  , pointer     :: field_ele
    real(kind=8)   , dimension(:,:), pointer     :: coor_ele
    character(len=27) :: IAM
    character(len=80) :: cout
    !      123456789012345678901234567
    IAM = 'multiMAILx::compute_damping'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    ! 5 operators: 2(+1) for permeability and 2 for coupling
    nb_damp_eo = 5 
    allocate(operators(nb_damp_eo))
    operators = (/ p_wf_permy   , p_wf_permy_cpl, p_nwf_permy  ,&
                   p_pc2disp_cpl, p_pn2disp_cpl , p_disp2pc_cpl, p_disp2pn_cpl &
                /)

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      blmnb    = bdyty(i_bdyty)%blmty(i_blmty)%blmnb
      i_model  = bdyty(i_bdyty)%blmty(i_blmty)%mdlnb

      coor_ele => get_ptr_coor_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)) 

      do i_eo = 1, nb_damp_eo
        field_ele => get_ptr_field_ele_multiEF(blmnb, operators(i_eo))
        field_rank = bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(operators(i_eo))
        if( field_rank > 0 ) then
          call get_multi_field_MAILx(iM_bdyty, iM_blmty, 1, field_rank, field_ele)
        else
          field_ele = 1.
        end if
        ! almost no parameters since the operator uses its own internal working arrays field just above

        call compute_elementary_operator(blmnb, operators(i_eo), bdyty(i_bdyty)%blmty(i_blmty)%ppsnb, &
                                         bdyty(i_bdyty)%eviz(i_blmty))

        call put_in_augmented_matrix_multiEF(blmnb, operators(i_eo), bdyty(i_bdyty)%blmty(i_blmty)%damping)

      end do

!!$      print*,'+++c'
!!$      do i=1,size(bdyty(i_bdyty)%blmty(i_blmty)%damping,dim=1)
!!$        print*,bdyty(i_bdyty)%blmty(i_blmty)%damping(i,i)
!!$      enddo
!!$      print*,'+++c'

    end do


    deallocate(operators)

  end subroutine

  !> \brief Compute external nodal forces/fluxes of a multiMAILx
  !> Composed of gravity and imposed fluxes in DRV_DOF
  subroutine compute_Fext_multiMAILx(i_bdyty)
    implicit none
    !> id of the multiMAILx to compute mass matrix
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: i_blmty, i_nodty, shift, i, shift0, nbn
    integer(kind=4) :: i_dof, i_dual , i_node, iccdof, nb_dof
    real(kind=8)    :: flux_begin, flux, gravy(3), val
    real(kind=8), dimension(:), pointer :: dofs_ele
    real(kind=8), dimension(:,:), allocatable :: vect

    character(len=24) :: IAM

    character(len=40) :: cout

    !      123456789012345678901234
    IAM = 'multiMAILX::compute_Fext'

    gravy(1) = grav1; gravy(2) = grav2; gravy(3) = grav3

    bdyty(i_bdyty)%Fext = 0.d0

    ! gravity are computed locally ...
    if (any( gravy(1:nbDIME) /= 0.d0 )) then

      do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
        
        if (bdyty(i_bdyty)%eviz(i_blmty) == 0) cycle

        dofs_ele => get_ptr_dofs_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
        dofs_ele = 0.d0

        shift = 0
        do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
          dofs_ele(shift+1:shift+nbDIME) = gravy(1:nbDIME)
          shift = shift + get_N_DOF_of_NODE_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,i)
        end do

        !print *,'Gravite : ',DV_ele
        dofs_ele = matmul(bdyty(i_bdyty)%blmty(i_blmty)%mass,dofs_ele)
        !print *,'Effort elementaire Gravite : ',DV_ele

        !if (i_blmty == 1) then
        !  print*, bdyty(i_bdyty)%blmty(i_blmty)%mass
        !  print*,'-'
        !  print*, gravy
        !endif 

        shift = 0
        do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)

          i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
          iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)

          nb_dof  = get_N_DOF_of_NODE_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,i)

          bdyty(i_bdyty)%Fext(iccdof+1:iccdof+nb_dof) = bdyty(i_bdyty)%Fext(iccdof+1:iccdof+nb_dof) + &
                                                        dofs_ele(shift+1:shift+nb_dof)
          shift = shift + nb_dof
        end do

      end do
    end if


    ! driven forces
    !print*,' Fext'
    !do i_nodty=1,bdyty(i_bdyty)%nb_nodes
    !  cout = ' ' 
    !  iccdof = bdyty(i_bdyty)%ccdof(i_nodty)
    !  nb_dof=maxval(bdyty(i_bdyty)%ccsize(:,i_nodty))
    !  write(cout,'(A,I0,A)') "(",nb_dof,"(1x,D12.5))" 
    !  write(*,trim(cout)) bdyty(i_bdyty)%Fext(iccdof+1:iccdof+nb_dof)
    !enddo
  
    do i_dual = 1, bdyty(i_bdyty)%nb_dual_driven_dofs

      call comp_a_driven_dof(bdyty(i_bdyty)%dual_drvdofs(i_dual),flux_begin,flux)

      ! \todo: check if theta method is used. May change how to compute values

      CALL owner_of_a_driven_dof(bdyty(i_bdyty)%dual_drvdofs(i_dual),i_node,i_dof)

      iccdof = bdyty(i_bdyty)%ccdof(i_node) + i_dof   
      bdyty(i_bdyty)%Fext(iccdof) = bdyty(i_bdyty)%Fext(iccdof) + ((1.d0-THETA)*flux_begin) + (THETA*flux)

    end do

    ! %< gestion pression dans les zones erodees

    allocate(vect(1,bdyty(i_bdyty)%nb_nodes))
    call get_physic_values(i_bdyty, p_pc, bdyty(i_bdyty)%Dofs(:,1), vect)

    val=0.

    bdyty(i_bdyty)%flying_nodes = 0

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      bdyty(i_bdyty)%blmty(i_blmty)%Fext=0.d0
        
      if (bdyty(i_bdyty)%eviz(i_blmty) == 1) cycle

      ! size of this array is the total number of dofs of the element 
      dofs_ele => get_ptr_dofs_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
      dofs_ele = 0.d0

      ! put n pressure in dofs_ele
      shift = 0

      nbn = size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
      !print*,nbdime,nbn
      do i = 1, nbn
        dofs_ele(shift+1:shift+nbDIME) = vect(1,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i)) 
        shift = shift + nbDIME
        !shift = shift + get_N_DOF_of_NODE_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,i)

        ! on tague les flying nodes pour s'en occuper plus tard
        if (nbdime == 2 .and. nbn == 8 .and. i > 4) then
          bdyty(i_bdyty)%flying_nodes(bdyty(i_bdyty)%blmty(i_blmty)%NODES(i)) = &
              bdyty(i_bdyty)%flying_nodes(bdyty(i_bdyty)%blmty(i_blmty)%NODES(i)) + 1
        endif
      end do

      !print*,'---'
      !print*,'ele',i_blmty
      !print*,'noeuds',bdyty(i_bdyty)%blmty(i_blmty)%NODES
      !print*,'pression'
      !write(*,'(2(1x,D12.5))') dofs_ele


      ! computes divergence in element
      call compute_elementary_external_body_f_from_divp(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,p_stiffness)


      ! put result in elementary fext
      call put_in_augmented_vector_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb, p_stiffness, & 
                                           bdyty(i_bdyty)%blmty(i_blmty)%fext(:,1))

      !print*,'fext',size(bdyty(i_bdyty)%blmty(i_blmty)%fext,dim=1)
      !shift=0
      !do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
      !  shift0=shift
      !  shift = shift + get_N_DOF_of_NODE_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,i)
      !  write(*,'(4(1x,D12.5))') bdyty(i_bdyty)%blmty(i_blmty)%fext(shift0+1:shift,1)         
      !
      !  if (bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) == 3220) then
      !    val = val + bdyty(i_bdyty)%blmty(i_blmty)%fext(shift0+1,1)
      !    print*,'val ',val
      !  endif
      !enddo

    enddo

    !print*,'val ',val

    deallocate(vect)

    ! gestion pression dans les zones erodees >% 


  end subroutine compute_Fext_multiMAILx

  !----------------------------------------
  ! assembly and computation
  !----------------------------------------

  !> \brief Assemble left hand side matrix of the system of a multiMAILx
  subroutine assemb_KT_multiMAILx(i_bdyty)
    implicit none
    !> multiMAILx id to assemble
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4)   :: i_blmty
    real(kind=8)      :: HT,HT2
    character(len=80) :: cout

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr('multiMAILx::assemb_KT_multiMAILx',cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

!    print*,'assemb_KT_multiMAILx'

    HT  = THETA*H
    HT2 = HT*HT

    !write(*,'(A,I0)') 'Body: ',ibdyty

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

       
!!$       if (bdyty(i_bdyty)%eviz(i_blmty) == 0) then
!!$         print*,i_blmty
!!$         print*,'mass'
!!$         print*,bdyty(i_bdyty)%blmty(i_blmty)%mass
!!$         print*,'damping'
!!$         print*,bdyty(i_bdyty)%blmty(i_blmty)%damping
!!$         print*,'stiffness'
!!$         print*,bdyty(i_bdyty)%blmty(i_blmty)%stiffness
!!$       endif

       call erase_elementary_matrix(bdyty(i_bdyty)%g_sys,i_blmty)
       call add_to_elementary_matrix(bdyty(i_bdyty)%g_sys,i_blmty,bdyty(i_bdyty)%blmty(i_blmty)%mass)
       call add_to_elementary_matrix(bdyty(i_bdyty)%g_sys,i_blmty,bdyty(i_bdyty)%blmty(i_blmty)%damping,HT)
       call add_to_elementary_matrix(bdyty(i_bdyty)%g_sys,i_blmty,bdyty(i_bdyty)%blmty(i_blmty)%stiffness,HT2)

    end do 

  end subroutine assemb_KT_multiMAILx

  !> \brief Applys primal driven dofs to LHS
  subroutine apply_drvdof_KT_multiMAILx(i_bdyty)
    implicit none
    !> index of multiMAILx
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4)   :: i_primal, iccdof, i_dof, i_node, toto, i
    character(len=80) :: cout

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr('multiMAILx::apply_drvdof_KT_multiMAILx',cout)
    end if
    
    if( .not. bdyty(i_bdyty)%visible ) return
    !write(*,'(A,I0)') 'Body: ',ibdyty
   
    !do i=1,100
    !print*,i,bdyty(i_bdyty)%ccdof(i)
    !end do 
 
    !toto=bdyty(i_bdyty)%ccdof(86)
    !print*, toto
    
    if (bdyty(i_bdyty)%nb_primal_driven_dofs /= 0) then

      do i_primal = 1, bdyty(i_bdyty)%nb_primal_driven_dofs
        call owner_of_a_driven_dof(bdyty(i_bdyty)%primal_drvdofs(i_primal),i_node,i_dof)
        bdyty(i_bdyty)%drvdofs(i_primal) = bdyty(i_bdyty)%ccdof(i_node)+i_dof 

      end do

      call set_drvdofs(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%drvdofs)

    else
   
      call erase_drvdofs(bdyty(i_bdyty)%g_sys)

    endif

  end subroutine apply_drvdof_KT_multiMAILx

  !> \brief Assemble right hand side vector of the system of a multiMAILx
  subroutine assemb_RHS_multiMAILx(i_bdyty)
    implicit none
    !> multiMAILx index
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4)   :: i_blmty, i_nodty, iccdof, nb_dof, i, shift
    character(len=80) :: cout
    character(len=22) :: IAM

    real(kind=8) :: UMTT
    real(kind=8), dimension(:), pointer :: dofs_ele

    !      1234567890123456789012
    IAM = 'multiMAILx::assemb_RHS'

!    print*,'assemb_RHS_multiMAILx'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    UMTT  = (1.d0-THETA)

    ! first the elementary contributions

    bdyty(i_bdyty)%RHS = 0.d0

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      !print*,'contribution ',i_blmty 

      bdyty(i_bdyty)%blmty(i_blmty)%RHSloc = 0.d0

      dofs_ele => get_ptr_dofs_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
  
      ! la partie "inertie"

      shift = 0
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
        iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)
        nb_dof  = get_N_DOF_of_NODE_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,i)

        if (is_new) then
          dofs_ele(shift+1:shift+nb_dof) = bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,2)
        else
          dofs_ele(shift+1:shift+nb_dof) = bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,2) - &
                                           bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1)
          endif

        shift = shift + nb_dof
      end do

      dofs_ele = matmul(bdyty(i_bdyty)%blmty(i_blmty)%mass,dofs_ele)
      bdyty(i_bdyty)%blmty(i_blmty)%RHSloc = dofs_ele
      call assemble_elementary_vector(bdyty(i_bdyty)%Fdyn,(dofs_ele/H),bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  

      !print*,'inertie'
      !print*,dofs_ele 
       

      ! la partie "viscosite"  

      shift = 0
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
        iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)
        nb_dof  = get_N_DOF_of_NODE_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,i)

        if (is_new) then           
          dofs_ele(shift+1:shift+nb_dof) = UMTT  * bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,2)
  
        else 
          dofs_ele(shift+1:shift+nb_dof) = THETA * bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) + &
                                           UMTT  * bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,2)
        endif

        shift = shift + nb_dof
      end do

      dofs_ele = matmul(bdyty(i_bdyty)%blmty(i_blmty)%damping,dofs_ele)

      call assemble_elementary_vector(bdyty(i_bdyty)%Fdmp, dofs_ele, &
                                      bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  


      bdyty(i_bdyty)%blmty(i_blmty)%RHSloc = bdyty(i_bdyty)%blmty(i_blmty)%RHSloc       &
                                           - H*dofs_ele

        !print*,'viscosite'
        !print*,-H*dofs_ele 


        ! la partie "rigidite"

      call assemble_elementary_vector(bdyty(i_bdyty)%Fint, bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1), &
                                      bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  

      bdyty(i_bdyty)%blmty(i_blmty)%RHSloc = bdyty(i_bdyty)%blmty(i_blmty)%RHSloc       &
                                           - H*bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1) 


        !print*,'rigidite'
        !print*,-H*bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1)

        !
        !  if (iblmty > 1) then
        !    write(*,'(A,I0)') 'Element: ',iblmty
        !    write(*,'(3(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%RHSloc
        !    print*,bdyty(ibdyty)%blmty(iblmty)%edof2gdof
        !  endif


      !endif

      call assemble_elementary_vector(bdyty(i_bdyty)%RHS,bdyty(i_bdyty)%blmty(i_blmty)%RHSloc, &
                                      bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)

      !print*,'rhs loc'
      !print*,bdyty(i_bdyty)%blmty(i_blmty)%RHSloc

      call assemble_elementary_vector(bdyty(i_bdyty)%Fext, bdyty(i_bdyty)%blmty(i_blmty)%Fext(:,1), &
                                      bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  


      !iccdof  = bdyty(i_bdyty)%ccdof(3220)
      !print*,'Fext',bdyty(i_bdyty)%Fext(iccdof+1:iccdof+2)




    end do

    do i=1,size(bdyty(i_bdyty)%flying_nodes)

      if (bdyty(i_bdyty)%flying_nodes(i) > 1) then
        iccdof  = bdyty(i_bdyty)%ccdof(i)
        bdyty(i_bdyty)%Fext(iccdof+1:iccdof+nbdime)=0.d0
      endif

    enddo

    ! external forces: nodal contributions

    bdyty(i_bdyty)%RHS = bdyty(i_bdyty)%RHS + H*bdyty(i_bdyty)%Fext


    !iccdof  = bdyty(i_bdyty)%ccdof(3220)
    !print*,'Fext',bdyty(i_bdyty)%Fext(iccdof+1:iccdof+2)
    !print*,'Fint',bdyty(i_bdyty)%Fint(iccdof+1:iccdof+2)
    !print*,'Fdmp',bdyty(i_bdyty)%Fdmp(iccdof+1:iccdof+2)
    !print*,'Fdyn',bdyty(i_bdyty)%Fdyn(iccdof+1:iccdof+2)
    !print*,'RHS ',bdyty(i_bdyty)%RHS(iccdof+1:iccdof+2)

  end subroutine assemb_RHS_multiMAILx

  !> \brief Free state computation
  !> incremental formulation of the EF, being in hpp or gd
  !> %Dofs(:,1) holds the prediction of current state which may be different from Dofs(:,2) state at the end of the previous step
  subroutine compute_free_state_multiMAILx(i_bdyty)
    implicit none
    !> index of multiMAILx
    integer(kind=4), intent(in) :: i_bdyty
    !
    real(kind=8)    :: driv, driv_begin
    integer(kind=4) :: i_primal, i_node, i_dof, iccdof, info
    character(len=80) :: cout
    character(len=30) :: IAM
    !      123456789012345678901234567890
    IAM = 'multiMAILx::compute_free_state'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    if (itchache) then

      print*,'object ',i_bdyty

      print*,'prediction of state:'
      print*,bdyty(i_bdyty)%Dofs(:,1)

      print*,' '
      print*,'RHS'
      if (nbdime == 2) then
        write(*,'(2(1x,D12.5))') bdyty(i_bdyty)%RHS
      else if (nbdime == 3) then
        write(*,'(3(1x,D12.5))') bdyty(i_bdyty)%RHS
      endif       
      print*,'==========================='

    end if

    bdyty(i_bdyty)%DofsFree = 0.d0

    ! setting rhs of the system
    call set_vector(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%RHS)

    ! giving drvdofs values to the system if any
    if (bdyty(i_bdyty)%nb_primal_driven_dofs /= 0) then
      do i_primal = 1, bdyty(i_bdyty)%nb_primal_driven_dofs

        call comp_a_driven_dof(bdyty(i_bdyty)%primal_drvdofs(i_primal),driv_begin,driv)
        call owner_of_a_driven_dof(bdyty(i_bdyty)%primal_drvdofs(i_primal),i_node,i_dof)
        iccdof = bdyty(i_bdyty)%ccdof(i_node) + i_dof 

        if (is_new) then
          bdyty(i_bdyty)%drvvalues(i_primal) = driv
        else
          bdyty(i_bdyty)%drvvalues(i_primal) = driv - bdyty(i_bdyty)%Dofs(iccdof,1)
        endif
      end do
     
      call set_drvvalues(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%drvvalues)           

    end if

    ! solving
    call solve_system(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%DofsFree,info)

    if (info > 0) then
      write(cout,'(A,1x,I0)') 'No solution for body :', i_bdyty
      call faterr(IAM,cout)
    end if

    if (.not. is_new) then
       bdyty(i_bdyty)%DofsFree = bdyty(i_bdyty)%DofsFree + bdyty(i_bdyty)%Dofs(:,1)
    endif

    ! \todo: configuration TT !
    if (.not. is_contactdetectionconfiguration_defined) then
      !call compute_configurationTT_multiMAILx(ibdyty)
    end if

  end subroutine compute_free_state_multiMAILx

  !> \brief Compute end of step values of dofs (and integrated values of dofs)
  subroutine compute_dof_multiMAILx(i_bdyty)
    implicit none 
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !
    real(kind=8), dimension(:,:), allocatable :: tmp_V_vect
    real(kind=8)      :: driv, driv_begin
    integer(kind=4)   :: i_primal, i_node, i_dof, iccdof, info,i_blmty,i_nodty,nb_dof
    character(len=23) :: IAM
    character(len=40) :: cout
    !      12345678901234567890123
    IAM = 'multiMAILx::compute_dof'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    !> keep the last iterate
    bdyty(i_bdyty)%DofsLast = bdyty(i_bdyty)%Dofs(:,1)

    !> \todo: test if Ireac modify/non zero to skip resolution

    bdyty(i_bdyty)%Iaux = bdyty(i_bdyty)%RHS + bdyty(i_bdyty)%Ireac


    ! rhs of g_sys is Iaux
    call set_vector(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%Iaux)

    if (bdyty(i_bdyty)%nb_primal_driven_dofs /= 0) then
      ! drvvalues is already up to date... isn't it ?
      do i_primal = 1, bdyty(i_bdyty)%nb_primal_driven_dofs
        call comp_a_driven_dof(bdyty(i_bdyty)%primal_drvdofs(i_primal),driv_begin,driv)
        call owner_of_a_driven_dof(bdyty(i_bdyty)%primal_drvdofs(i_primal),i_node,i_dof)
 
        iccdof = bdyty(i_bdyty)%ccdof(i_node) + i_dof 

        if (is_new) then
          bdyty(i_bdyty)%drvvalues(i_primal) = driv
        else
          bdyty(i_bdyty)%drvvalues(i_primal) = driv - bdyty(i_bdyty)%Dofs(iccdof,1)
        endif

      end do
    
      call set_drvvalues(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%drvvalues)

    end if


    !!!
    !print*,'-objet ',i_bdyty

    !print*,' RHS'
    !do i_nodty=1,bdyty(i_bdyty)%nb_nodes
    !  cout = ' ' 
    !  iccdof = bdyty(i_bdyty)%ccdof(i_nodty)
    !  nb_dof=maxval(bdyty(i_bdyty)%ccsize(:,i_nodty))
    !  write(cout,'(A,I0,A)') "(",nb_dof,"(1x,D12.5))" 
    !  write(*,trim(cout)) bdyty(i_bdyty)%Iaux(iccdof+1:iccdof+nb_dof)
    !enddo
!!$
!!$    print*,' matrice elementaire'
!!$    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
!!$      print*,'--element ',i_blmty
!!$      call display_elementary_matrix(bdyty(i_bdyty)%g_sys,i_blmty)
!!$    enddo
    !!!  

    call solve_system(bdyty(i_bdyty)%g_sys,bdyty(i_bdyty)%DofsAux,info)
    if (info > 0) then
      write(cout,'(A,1x,I0)') 'No solution for body :', i_bdyty
      call faterr(IAM,cout)
    end if
    !print*,'DofsAux >',bdyty(i_bdyty)%DofsAux(260:270)

    if (is_new) then 
      bdyty(i_bdyty)%Dofs(:,1) =  bdyty(i_bdyty)%DofsAux
    else
      bdyty(i_bdyty)%Dofs(:,1) =  bdyty(i_bdyty)%Dofs(:,1) + bdyty(i_bdyty)%DofsAux
    endif 


!!$    print*,' V'
!!$    do i_nodty=1,bdyty(i_bdyty)%nb_nodes
!!$      cout = ' ' 
!!$      iccdof = bdyty(i_bdyty)%ccdof(i_nodty)
!!$      nb_dof=maxval(bdyty(i_bdyty)%ccsize(:,i_nodty))
!!$      write(cout,'(A,I0,A)') "(",nb_dof,"(1x,D12.5))" 
!!$      write(*,trim(cout)) bdyty(i_bdyty)%DofsAux(iccdof+1:iccdof+nb_dof)
!!$    enddo


!!$    iccdof = bdyty(i_bdyty)%ccdof(143)
!!$    nb_dof=2
!!$    bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) = 0.d0
!!$    iccdof = bdyty(i_bdyty)%ccdof(495)
!!$    nb_dof=2
!!$    bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) = 0.d0
!!$    iccdof = bdyty(i_bdyty)%ccdof(532)
!!$    nb_dof=2
!!$    bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) = 0.d0
!!$    iccdof = bdyty(i_bdyty)%ccdof(569)
!!$    nb_dof=2
!!$    bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) = 0.d0
!!$    iccdof = bdyty(i_bdyty)%ccdof(606)
!!$    nb_dof=2
!!$    bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) = 0.d0

    !displacement results from the integration of velocity

    allocate(tmp_V_vect(nbDIME,bdyty(i_bdyty)%nb_nodes))

    call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Dofs(:,2), tmp_V_vect)
    bdyty(i_bdyty)%X(:,:,1) = bdyty(i_bdyty)%X(:,:,2) + &
                              (1.d0-theta)*H * tmp_V_vect

    call get_physic_values(i_bdyty, p_disp, bdyty(i_bdyty)%Dofs(:,1), tmp_V_vect)
    bdyty(i_bdyty)%X(:,:,1) = bdyty(i_bdyty)%X(:,:,1) + &
                              theta*H * tmp_V_vect

    where(dabs(bdyty(i_bdyty)%X(:,:,1))<1.d-24) bdyty(i_bdyty)%X(:,:,1) = 0.d0

    deallocate(tmp_V_vect)

  end subroutine compute_dof_multiMAILx

!------------------------------------------------------------------------ 
  !> \brief Compute the norm of the residue
  subroutine compute_residue_norm_multiMAILx(norm_res, norm_Dofs, ibdyty)
    implicit none
    !> body number
    integer(kind=4), intent(in)  :: ibdyty
    !> norm of the residue
    real(kind=8)   , intent(out) :: norm_res
    !> norm of the difference of degrees of freedom
    real(kind=8)   , intent(out) :: norm_Dofs
    !
    integer(kind=4)   :: ivd, inod, idof, iccdof
    real(kind=8)      :: max_dDofs,max_Dofs
    real(kind=8)      :: max_res,max_reac,max_f,max_fint,max_fdyn,max_fext,max_fdmp
    character(len=40) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'multiMAILx:compute_residue_norm'
 
    norm_res = 1.d+20
 
    if( ibdyty < 1 .or. ibdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
      call faterr(IAM,cout)
    end if
 
    if( .not. bdyty(ibdyty)%visible ) return
 
    norm_Res  = 0.d0
    norm_Dofs = 0.d0
 
    bdyty(ibdyty)%DofsAux = bdyty(ibdyty)%Dofs(:,1) - bdyty(ibdyty)%DofsLast
 
    max_dDofs = max( maxval(bdyty(ibdyty)%DofsAux), abs(minval(bdyty(ibdyty)%DofsAux)) )
    max_Dofs  = max( maxval(bdyty(ibdyty)%Dofs)   , abs(minval(bdyty(ibdyty)%Dofs))    )
 
    if (max_Dofs <= 1.d-10 ) max_Dofs = 1.d0
 
    norm_Dofs = max_dDofs/max_Dofs
 
    !print*,max_dDofs,max_Dofs  

    if (is_new) then

      bdyty(ibdyty)%residu = 0.d0

    else

      bdyty(ibdyty)%DofsAux = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Ireac
 
      do ivd = 1,bdyty(ibdyty)%nb_primal_driven_dofs

        call owner_of_a_driven_dof(bdyty(ibdyty)%primal_drvdofs(ivd),inod,idof)

        iccdof = bdyty(ibdyty)%ccdof(inod) + idof

        bdyty(ibdyty)%DofsAux(iccdof) = 0.d0

      end do  
 
      bdyty(ibdyty)%residu = bdyty(ibdyty)%DofsAux

    endif
 
    max_res  =   max( maxval(bdyty(ibdyty)%residu),abs(minval(bdyty(ibdyty)%residu)) )
    max_fint = H*max( maxval(bdyty(ibdyty)%Fint),  abs(minval(bdyty(ibdyty)%Fint))   )
    max_fdmp = H*max( maxval(bdyty(ibdyty)%Fdmp),  abs(minval(bdyty(ibdyty)%Fdmp))   )
    max_fdyn = H*max( maxval(bdyty(ibdyty)%Fdyn),  abs(minval(bdyty(ibdyty)%Fdyn))   )
    max_fext = H*max( maxval(bdyty(ibdyty)%Fext),  abs(minval(bdyty(ibdyty)%Fext))   )
    max_reac =   max( maxval(bdyty(ibdyty)%Ireac), abs(minval(bdyty(ibdyty)%Ireac))  )
    max_f    =   max( max(max_fint, max(max_fdyn,max_fext)), max_fdmp )
 
    IF (max_f <= 1.d-10 ) max_f = 1.d0
 
    norm_res = max_res/max_f
 
  end subroutine compute_residue_norm_multiMAILx

  !> \brief Compute different forces/fluxes contributions


  ! post-processing only ?

  subroutine compute_flourxces_multiMAILx(i_bdyty)
    implicit none
    !> body number
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4)   :: i_blmty, i, i_nodty, iccdof, iM_bdyty, iM_blmty, i_fd, i_dof
    integer(kind=4)   :: shift, nb_dof, i_eo, blmnb
    character(len=29) :: IAM
    character(len=80) :: cout
    real(kind=8) :: gravity(3), Fe, Febegin
    real(kind=8), dimension(:)  , pointer :: dofs_ele
    !real(kind=8), dimension(:)  , allocatable :: inertia, damping, internal, fext 
    real(kind=8), dimension(:,:), pointer :: coor_ele, flux_ele
    !      12345678901234567890123456789
    IAM = 'multiMAILx::compute_flourxces'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    bdyty(i_bdyty)%residu  = 0.D0
    bdyty(i_bdyty)%Fint    = 0.D0
    bdyty(i_bdyty)%Fdmp    = 0.D0
    bdyty(i_bdyty)%Fdyn    = 0.D0
    bdyty(i_bdyty)%Fext    = 0.D0

    if( .not. bdyty(i_bdyty)%visible ) return

    gravity(1) = grav1
    gravity(2) = grav2
    gravity(3) = grav3

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

      blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb
      dofs_ele => get_ptr_dofs_ele_multiEF(blmnb)

      ! external forces in Fext
      dofs_ele = 0.d0


      ! add g to dofs_ele  
      shift = 0
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        dofs_ele(shift+1:shift+nbDIME) = gravity(1:nbDIME)
        shift = shift + get_N_DOF_of_NODE_multiEF(blmnb,i)
      end do

      ! compute mg      
      if (bdyty(i_bdyty)%eviz(i_blmty) /= 0) dofs_ele = matmul(bdyty(i_bdyty)%blmty(i_blmty)%mass,dofs_ele)

      call assemble_elementary_vector(bdyty(i_bdyty)%Fext,dofs_ele,bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  

      ! inertia forces in Fdyn
      dofs_ele = 0.d0

      ! add a = (vn+1 - vn) / H  to dofs_ele
      shift = 0
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
        iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)
        nb_dof  = get_N_DOF_of_NODE_multiEF(blmnb,i)

        dofs_ele(shift+1:shift+nb_dof) = ( bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1) - &
                                           bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,2) ) / H
        shift = shift + nb_dof
      end do

      ! compute ma
      dofs_ele = matmul(bdyty(i_bdyty)%blmty(i_blmty)%mass,dofs_ele)

      call assemble_elementary_vector(bdyty(i_bdyty)%Fdyn,dofs_ele,bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  

      ! damping in Fdmp
      dofs_ele = 0.d0


      ! add vn+1 to dos_ele
      shift = 0
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
        iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)
        nb_dof  = get_N_DOF_of_NODE_multiEF(blmnb,i)

        dofs_ele(shift+1:shift+nb_dof) = bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nb_dof,1)
        shift = shift + nb_dof
      end do

      coor_ele => get_ptr_coor_ele_multiEF(blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)) 

      ! compute Cv
      dofs_ele = matmul(bdyty(i_bdyty)%blmty(i_blmty)%damping,dofs_ele)

      call assemble_elementary_vector(bdyty(i_bdyty)%Fdmp,dofs_ele,bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  

      ! stiffness in Fint
      bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1) = 0.d0

      i_eo = p_stiffness 

      ! working 
      flux_ele => get_ptr_flux_field_ele_multiEF(blmnb, p_stiffness)
      call get_multi_flux_MAILx(iM_bdyty, iM_blmty, p_disp, 1, flux_ele)

      call compute_elementary_internal_f(blmnb, i_eo, bdyty(i_bdyty)%eviz(i_blmty))
      call put_in_augmented_vector_multiEF(blmnb, i_eo, bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1))

      call assemble_elementary_vector(bdyty(i_bdyty)%Fint, bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1), & 
                                      bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)  

    end do

    do i_fd = 1, bdyty(i_bdyty)%nb_dual_driven_dofs

      call owner_of_a_driven_dof(bdyty(i_bdyty)%dual_drvdofs(i_fd),i_nodty,i_dof)
      iccdof = bdyty(i_bdyty)%ccdof(i_nodty) + i_dof

      call comp_a_driven_dof(bdyty(i_bdyty)%dual_drvdofs(i_fd),Febegin,Fe)
      bdyty(i_bdyty)%Fext(iccdof) = bdyty(i_bdyty)%Fext(iccdof) + Fe

    end do

    ! fd pas bon
    !bdyty(i_bdyty)%residu = bdyty(i_bdyty)%Fint + bdyty(i_bdyty)%Fdmp + bdyty(i_bdyty)%Fdyn + &
    !                        bdyty(i_bdyty)%Fext + bdyty(i_bdyty)%Ireac/H

    bdyty(i_bdyty)%residu = bdyty(i_bdyty)%Fdyn + bdyty(i_bdyty)%Fint + bdyty(i_bdyty)%Fdmp &
                            - bdyty(i_bdyty)%Fext - bdyty(i_bdyty)%Ireac/H

  end subroutine compute_flourxces_multiMAILx

!------------------------------------------------------------------------ 

  !> \brief Update elementary fields of a body
  subroutine update_bulk_multiMAILx(i_bdyty)
    implicit none
    !> multiMAILx index
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4) :: i_blmty
    character(len=23) :: IAM
    character(len=80) :: cout
    !      12345678901234567890123
    IAM = 'multiMAILx::update_bulk'
  
    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
       
      !bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
      bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,2) = bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1)
  
      bdyty(i_bdyty)%blmty(i_blmty)%Fint(:,1) = 0.d0
  
    end do
  
    call update_multigpv_MAILx(bdyty2M_bdyty(i_bdyty))

  end subroutine update_bulk_multiMAILx

  !> \brief Update degrees of freedom values of a body
  subroutine update_dof_multiMAILx(i_bdyty)
    implicit none 
    !> multiMAILx index
    integer(kind=4), intent(in) :: i_bdyty
    !
    integer(kind=4)   :: i_nodty, iccdof
    integer(kind=4)   :: iM_bdyty,iM_nodty, iM_ccdof
    character(len=80) :: cout
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'multiMAILx::update_dof_multiMAILx'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    if( .not. bdyty(i_bdyty)%visible ) return

    !Update dofs and displacement
    bdyty(i_bdyty)%Dofs(:,2) = bdyty(i_bdyty)%Dofs(:,1)               
    bdyty(i_bdyty)%X(:,:,2)  = bdyty(i_bdyty)%X(:,:,1)
     

    !Update of mesh coordinates
    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_nodty = 1, size(bdyty(i_bdyty)%nodty)

      iccdof   = bdyty(i_bdyty)%ccdof(i_nodty)
      iM_nodty = bdyty(i_bdyty)%nodty2M_nodty(i_nodty)
      iM_ccdof = M_bdyty(iM_bdyty)%ccdof(iM_nodty)

      M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) &
                                                           + bdyty(i_bdyty)%X(1:nbDIME,i_nodty,1)

    end do

  end subroutine update_dof_multiMAILx
!------------------------------------------------------------------------

  !> \brief returns reference coordinates of nodes of an element
  function get_cooref_ele(i_bdyty,i_blmty,nb_NODES)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> element number
    integer(kind=4), intent(in) :: i_blmty
    !> number of nodes in the element
    integer(kind=4), intent(in) :: nb_NODES
    !> reference coordinates of the nodes of an element
    real(kind=8), dimension(:,:) :: get_cooref_ele(nbDIME,nb_NODES)
    !
    integer(kind=4) :: i_nodty, i_nodes, iM_bdyty,iM_nodty
  
    iM_bdyty = bdyty2M_bdyty(i_bdyty)
  
    do i_nodes = 1, nb_NODES
      i_nodty  = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i_nodes)
      iM_nodty = bdyty(i_bdyty)%nodty2M_nodty(i_nodty)
  
      get_cooref_ele(:,i_nodes) = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)
  
    end do
  
  end function get_cooref_ele

  !> Get the rank of a field from its name
  integer(kind=4) function get_field_rank(i_bdyty, i_blmty, name)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> element number
    integer(kind=4), intent(in) :: i_blmty
    !> field name to get rank of
    character(len=*) :: name
    !
    integer(kind=4) :: iM_bdyty, iM_blmty
  
    iM_bdyty = bdyty2M_bdyty(i_bdyty)
    iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
  
    get_field_rank = get_multi_field_rank_MAILx(iM_bdyty,iM_blmty,name)
  
  end function

  !> Set the values of an element scalar field from its values at nodes
  subroutine set_field_bynode(i_bdyty, field_rank, fsize, field)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> rank of the field to set
    integer(kind=4), intent(in) :: field_rank
    !> size of field input parameter (should be nb_nodes)
    integer(kind=4), intent(in) :: fsize
    !> new values of field at nodes
    real(kind=8), dimension(fsize), intent(in) :: field
    !
    logical :: eo_found
    integer(kind=4)   :: i_blmty, iM_bdyty,iM_blmty, in, i_gp, i_eo, blmnb
    character(len=28) :: IAM
    real(kind=8), dimension(:), pointer     :: field_ele
    real(kind=8), dimension(:), allocatable :: valnoe
    !      1234567890123456789012345678
    IAM = 'multiMAILx::set_field_bynode'

    if (nb_multiMAILx == 0) return

    if (fsize /= bdyty(i_bdyty)%nb_nodes) then
      call FATERR(IAM,'non conforming vector fsize')
    end if

    !fd interpolation of node field to gp field
    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
      !
      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      !
      blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      ! look for the operator using the desired field
      eo_found = .false.
      do i_eo = 1, nb_eo
        if (field_rank == bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(i_eo) ) then
          eo_found = .true.
          exit
        end if
      end do

      if( .not. eo_found ) then
        call faterr(IAM,'Could not find operator to interpolate desired field')
      end if

      field_ele => get_ptr_field_ele_multiEF(blmnb, i_eo)

      ! \todo : get rid of this allocation
      allocate(valnoe(size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)))
      do in = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        valnoe(in) = field(bdyty(i_bdyty)%blmty(i_blmty)%NODES(in))
      end do

      ! interpolating... output is in field_ele
      call interpolate_node2gp_multiEF(blmnb, i_eo, valnoe)

      ! putting in MAILx database
      call set_multi_field_MAILx(iM_bdyty,iM_blmty,1,field_rank,field_ele)

      deallocate(valnoe)

    end do

  end subroutine set_field_bynode

  !> Set the values of an element scalar field from its value on the element
  subroutine set_field_byelem(i_bdyty, field_rank, fsize, field)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> rank of the field to set
    integer(kind=4), intent(in) :: field_rank
    !> size of field input parameter (should be nb_nodes)
    integer(kind=4), intent(in) :: fsize
    !> new values of field at nodes
    real(kind=8), dimension(fsize), intent(in) :: field
    !
    logical :: eo_found
    integer(kind=4)   :: i_blmty, iM_bdyty,iM_blmty, i_gp, i_eo, blmnb
    character(len=28) :: IAM
    real(kind=8), dimension(:), pointer     :: field_ele
    !      1234567890123456789012345678
    IAM = 'multiMAILx::set_field_byelem'

    if (nb_multiMAILx == 0) return

    if (fsize /= bdyty(i_bdyty)%nb_nodes) then
      call FATERR(IAM,'non conforming vector fsize')
    end if

    !fd interpolation of node field to gp field
    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
      !
      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      !
      blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      ! look for the operator using the desired field
      eo_found = .false.
      do i_eo = 1, nb_eo
        if (field_rank == bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(i_eo) ) then
          eo_found = .true.
          exit
        end if
      end do

      if( .not. eo_found ) then
        call faterr(IAM,'Could not find operator to interpolate desired field')
      end if

      field_ele => get_ptr_field_ele_multiEF(blmnb, i_eo)

      field_ele(:) = field(i_blmty)

      ! putting in MAILx database
      call set_multi_field_MAILx(iM_bdyty,iM_blmty,1,field_rank,field_ele)

    end do

  end subroutine set_field_byelem

  !> Get the rank of a vector field from its name
  integer(kind=4) function get_vfield_rank(i_bdyty, i_blmty, name)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> element number
    integer(kind=4), intent(in) :: i_blmty
    !> field name to get rank of
    character(len=*) :: name
    !
    integer(kind=4) :: iM_bdyty, iM_blmty
  
    iM_bdyty = bdyty2M_bdyty(i_bdyty)
    iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
  
    get_vfield_rank = get_multi_vfield_rank_MAILx(iM_bdyty,iM_blmty,name)
  
  end function

  !> Set the values of an element vector field from its values at nodes
  subroutine set_vfield_bynode(i_bdyty, field_rank, vfield, dim1, dim2)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> rank of the field to set
    integer(kind=4), intent(in) :: field_rank
    !> first dimension of input field array (vector field size)
    integer(kind=4), intent(in) :: dim1
    !> second dimension of input field array (number of nodes)
    integer(kind=4), intent(in) :: dim2
    !> new values of field at nodes
    real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
    !
    logical :: eo_found
    integer(kind=4)   :: i_blmty, iM_bdyty,iM_blmty, in, i_f, i_eo, blmnb
    character(len=28) :: IAM
    real(kind=8), dimension(:)  , pointer     :: field_ele
    real(kind=8), dimension(:,:), allocatable :: valnoe
    !      12345678901234567890123456789
    IAM = 'multiMAILx::set_vfield_bynode'

    if (nb_multiMAILx == 0) return
    if ( i_bdyty<1 .or. i_bdyty>nb_multiMAILx ) call faterr(IAM,'wrong multiMAILx index')

    if (dim2/= bdyty(i_bdyty)%nb_nodes) then
      call faterr(IAM,'non conforming vector fsize')
    end if

    !fd interpolation of node field to gp field
    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
      !
      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      !
      blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      ! look for the operator using the desired field
      eo_found = .false.
      do i_eo = 1, nb_eo
        if (field_rank == bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(i_eo) ) then
          eo_found = .true.
          exit
        end if
      end do

      if( .not. eo_found ) then
        call faterr(IAM,'Could not find operator to interpolate desired field')
      end if

      field_ele => get_ptr_field_ele_multiEF(blmnb, i_eo)

      ! here ?
      if (dim1 > get_multi_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
        call faterr(IAM,'input vector field too large')
      end if
      ! \todo : get rid of this allocation
      ! \todo : have a vector field in genericEF
      allocate(valnoe(size(bdyty(i_bdyty)%blmty(i_blmty)%NODES),dim1))
      do in = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        valnoe(in,:) = vfield(:,bdyty(i_bdyty)%blmty(i_blmty)%NODES(in))
      end do

      ! interpolating... output is in field_ele
      do i_f = 1, dim1
        call interpolate_node2gp_multiEF(blmnb, i_eo, valnoe(:,i_f))

        ! putting in MAILx database
        call set_multi_vfield_MAILx(iM_bdyty,iM_blmty,1,field_rank,i_f,field_ele)
      end do

      deallocate(valnoe)

    end do

  end subroutine set_vfield_bynode

  !> Set the values of an element field from its value on the element
  subroutine set_vfield_byelem(i_bdyty, field_rank, vfield, dim1, dim2)
    implicit none
    !> multiMAILx number
    integer(kind=4), intent(in) :: i_bdyty
    !> rank of the field to set
    integer(kind=4), intent(in) :: field_rank
    !> first dimension of input field array (vector field size)
    integer(kind=4), intent(in) :: dim1
    !> second dimension of input field array (number of elements)
    integer(kind=4), intent(in) :: dim2
    !> new values of field at nodes
    real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
    !
    logical :: eo_found
    integer(kind=4)   :: i_blmty, iM_bdyty,iM_blmty, i_f, i_eo, blmnb
    character(len=29) :: IAM
    real(kind=8), dimension(:), pointer     :: field_ele
    !      12345678901234567890123456789
    IAM = 'multiMAILx::set_vfield_byelem'

    if (nb_multiMAILx == 0) return
    if ( i_bdyty<1 .or. i_bdyty>nb_multiMAILx ) call faterr(IAM,'wrong multiMAILx index')

    if (dim2 /= size(bdyty(i_bdyty)%blmty)) then
      call FATERR(IAM,'non conforming vector fsize')
    end if

    !fd interpolation of node field to gp field
    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
      !
      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      !
      blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      ! look for the operator using the desired field
      eo_found = .false.
      do i_eo = 1, nb_eo
        if (field_rank == bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(i_eo) ) then
          eo_found = .true.
          exit
        end if
      end do

      if( .not. eo_found ) then
        call faterr(IAM,'Could not find operator to interpolate desired field')
      end if

      field_ele => get_ptr_field_ele_multiEF(blmnb, i_eo)

      do i_f = 1, dim1

        field_ele(:) = vfield(i_f,i_blmty)

        ! putting in MAILx database
        call set_multi_vfield_MAILx(iM_bdyty,iM_blmty,1,field_rank,i_f,field_ele)

      end do

    end do

  end subroutine set_vfield_byelem

  !> \brief returns coordinates of nodes of a multiMAILx
  !> Allocation of returned array is done within the functions
  function get_coor_multiMAILx(i_bdyty)
    implicit none 
    !> id of multiMAILx to get coordinates of
    integer(kind=4), intent(in) :: i_bdyty
    !> returned pointer on coordinates
    real(kind=8),dimension(:,:),pointer :: get_coor_multiMAILx
    ! ***
    integer(kind=4) :: i_node, iM_bdyty, iM_nodty
 
    get_coor_multiMAILx => null()
 
    iM_bdyty = bdyty2M_bdyty(i_bdyty)
 
    allocate(get_coor_multiMAILx(nbDIME,bdyty(i_bdyty)%nb_nodes)) 
 
    do i_node = 1, bdyty(i_bdyty)%nb_nodes
      iM_nodty = bdyty(i_bdyty)%nodty2M_nodty(i_node) 
      get_coor_multiMAILx(1:nbDIME,i_node) = get_coor_nodty_MAILx(iM_bdyty,iM_nodty)
    end do
 
  end function

  !> \brief returns all values at nodes of a multiMAILx
  !> Allocation of the returned array is done within the function
  function get_all_multiMAILx(i_bdyty)
    implicit none 
    !> multiMAILx index
    integer(kind=4), intent(in) :: i_bdyty
    !> multiMAILx stored values
    real(kind=8), dimension(:,:), pointer :: get_all_multiMAILx
    ! ***
    integer(kind=4) :: nb_nodes, sz, idx, nb_x, nb_v, nb_pc, nb_pn, i_eo
    integer(kind=4) :: i_nf, nb_f(nb_nf), nb_ff(nb_nf)
    real(kind=8), dimension(:,:), allocatable :: grad, flux

    get_all_multiMAILx => null()

    nb_nodes = bdyty(i_bdyty)%nb_nodes
    
    nb_x  = nbDIME
    nb_v  = nbDIME
    nb_pc = 1
    nb_pn = 1

    nb_f(p_disp) = nbDIME
    nb_f(p_pc)   = 1
    nb_f(p_pn)   = 1

    select case (nbDIME)
    case(2)
      nb_ff(p_disp) = 4 ! no von mises/jacobian yet
      nb_ff(p_pc)   = 2
      nb_ff(p_pn)   = 2
    case(3)
      nb_ff(p_disp) = 6 ! no von mises/jacobian yet
      nb_ff(p_pc)   = 3
      nb_ff(p_pn)   = 3
    end select

    !    X      V      Pc      Pn      Fext, Fint, Fdmp, Fdyn, Ireac, Residu + grad/flux
    sz = nb_x + nb_v + nb_pc + nb_pn + 6*sum(nb_f) + 2*sum(nb_ff)

    allocate(get_all_multiMAILx(sz,nb_nodes)) 
    get_all_multiMAILx = 0.d0

    idx = 0
    call get_vector_multiMAILx('X____', i_bdyty, get_all_multiMAILx(idx+1:idx+nbDIME,:), nbDIME, nb_nodes)
    idx = idx + nb_x
    call get_vector_multiMAILx('V____', i_bdyty, get_all_multiMAILx(idx+1:idx+nbDIME,:), nbDIME, nb_nodes)
    idx = idx + nb_v
    call get_vector_multiMAILx('Pc___', i_bdyty, get_all_multiMAILx(idx+1:idx+1,:), 1, nb_nodes)
    idx = idx + nb_pc
    call get_vector_multiMAILx('Pn___', i_bdyty, get_all_multiMAILx(idx+1:idx+1,:), 1, nb_nodes)
    idx = idx + nb_pn

!!$    idx = 0
!!$    call get_vector_multiMAILx('Xbeg_', i_bdyty, get_all_multiMAILx(idx+1:idx+nbDIME,:), nbDIME, nb_nodes)
!!$    idx = idx + nb_x
!!$    call get_vector_multiMAILx('Vbeg_', i_bdyty, get_all_multiMAILx(idx+1:idx+nbDIME,:), nbDIME, nb_nodes)
!!$    idx = idx + nb_v
!!$    call get_vector_multiMAILx('Pcbeg', i_bdyty, get_all_multiMAILx(idx+1:idx+1,:), 1, nb_nodes)
!!$    idx = idx + nb_pc
!!$    call get_vector_multiMAILx('Pnbeg', i_bdyty, get_all_multiMAILx(idx+1:idx+1,:), 1, nb_nodes)
!!$    idx = idx + nb_pn

    ! Fext, Fint, Fdmp, Fdyn, Ireac/H and residu
    do i_nf = 1, nb_nf
      call get_physic_values(i_bdyty,i_nf,bdyty(i_bdyty)%Fext,get_all_multiMAILx(idx+1:idx+nb_f(i_nf),:))
      idx = idx + nb_f(i_nf)
      call get_physic_values(i_bdyty,i_nf,bdyty(i_bdyty)%Fint,get_all_multiMAILx(idx+1:idx+nb_f(i_nf),:))
      idx = idx + nb_f(i_nf)
      call get_physic_values(i_bdyty,i_nf,bdyty(i_bdyty)%Fdmp,get_all_multiMAILx(idx+1:idx+nb_f(i_nf),:))
      idx = idx + nb_f(i_nf)
      call get_physic_values(i_bdyty,i_nf,bdyty(i_bdyty)%Fdyn,get_all_multiMAILx(idx+1:idx+nb_f(i_nf),:))
      idx = idx + nb_f(i_nf)
      call get_physic_values(i_bdyty,i_nf,bdyty(i_bdyty)%Ireac/H,get_all_multiMAILx(idx+1:idx+nb_f(i_nf),:))
      idx = idx + nb_f(i_nf)
      call get_physic_values(i_bdyty,i_nf,bdyty(i_bdyty)%residu,get_all_multiMAILx(idx+1:idx+nb_f(i_nf),:))
      idx = idx + nb_f(i_nf)
    end do

    ! parameter 1 : Grad, 2 : Flux
    do i_nf = 1, nb_nf
      call get_nodal_field(i_bdyty,i_nf,get_all_multiMAILx(idx+1:idx+nb_ff(i_nf),:),1)
      idx = idx + nb_ff(i_nf)
      call get_nodal_field(i_bdyty,i_nf,get_all_multiMAILx(idx+1:idx+nb_ff(i_nf),:),2)
      idx = idx + nb_ff(i_nf)
    end do

  end function

  !> \brief Compute nodal values from Gauss points fields value
  !> Dans MAILx on stocke tout les champs grad/flux de toutes les physiques dans un seul tableau
  !> C'est la fonction get_grad/flux_field_MAILx qui trie. Pour me simplifier les travail je n'ai
  !> qu'une fonction gpv2node_multiEF, qui prend un champs scalaire en entrée. Il faut donc passer
  !> grad/flux composantes par composantes. Pour s'eviter ça il faudrait peut-être faire un bloc
  !> interface sur gpv2node dans multiEF pour utiliser grad_work ou flux_work au lieu de field_work.
  subroutine get_nodal_field(i_bdyty,i_nf,field,i_field)
    implicit none
    !> body id
    integer(kind=4), intent(in)  :: i_bdyty
    !> nodal field id to get grad of
    integer(kind=4), intent(in)  :: i_nf
    !> nodal value of grad/flux
    real(kind=8), dimension(:,:), intent(out) :: field
    !> integer to choose between grad (=1) or flux (=2)
    integer(kind=4), intent(in)  :: i_field
    !
    integer(kind=4) :: iM_bdyty, iM_blmty, nb_fields
    integer(kind=4) :: blmnb, i_blmty, i_nodty, i_eo, i_f
    character(len=27) :: IAM
    real(kind=8),    dimension(:)  , pointer  :: scalar_field_ele, scalar_field_node
    real(kind=8),    dimension(:,:), pointer  :: tensor_field_ele
    integer(kind=4), dimension(:,:), pointer  :: e2v
    !      123456789012345678901234566
    IAM = 'multiMAILx::get_nodal_field'

    ! choosing elementary operator to use to get grad from gp to node
    ! depending on the node field
    select case(i_nf)
    case( p_disp)
      i_eo = p_stiffness
      ! compute_vms = .true.
    case( p_pc )
      i_eo = p_wf_permy
    case( p_pn )
      i_eo = p_nwf_permy
    case default
      i_eo = 0
    end select

    nb_fields = size(field,1)

    field = 0.d0

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    ! since gpv2node uses only scalar field, once get_multi_grad has been called
    ! each component is put in scalar array field_work
    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
  
      blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      if( i_field == 1 ) then
        tensor_field_ele => get_ptr_grad_field_ele_multiEF(blmnb, i_eo)
        call get_multi_grad_MAILx(iM_bdyty, iM_blmty, i_nf, 2, tensor_field_ele)
      else if( i_field == 2 ) then
        tensor_field_ele => get_ptr_flux_field_ele_multiEF(blmnb, i_eo)
        call get_multi_flux_MAILx(iM_bdyty, iM_blmty, i_nf, 2, tensor_field_ele)
      else
        call faterr(IAM,'unknown required field (must be 1 for grad, 2 for flux)')
      end if

      scalar_field_ele  => get_ptr_field_ele_multiEF(blmnb, i_eo)
      scalar_field_node => get_ptr_node_scalar_field_multiEF(blmnb, i_nf)

      e2v => get_ptr_edge2vertices_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)

      do i_f = 1, size(tensor_field_ele,1)
        scalar_field_node(:) = 0.d0
        scalar_field_ele(:)  = tensor_field_ele(i_f,:)
        call gpv2node_multiEF(blmnb,i_eo)

        ! cannot use assemble_elementary_vector since edof2gdof is linked to dofs
        ! whereas the fields are scalar (or tensorial in the future) thus the map does not correspond
        do i_nodty = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
          if( i_nodty <= size(scalar_field_ele) ) then
            field(i_f,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i_nodty)) = field(i_f,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i_nodty)) &
                                                                      + scalar_field_node(i_nodty)
          else
            field(i_f,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i_nodty)) = field(i_f,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i_nodty)) &
                                                                      + 0.5 * scalar_field_node(e2v(1,i_nodty))               &
                                                                      + 0.5 * scalar_field_node(e2v(2,i_nodty))
          end if
        end do
      end do
      
      !\todo: compute Von Mises Stress...
      !if (compute_vms) then
      !  call compute_vonMisesStress(grad_ele,field_ele)
      !  call gpv2node_multiEF(blmnb,i_eo,valnoe)
      !  call assemble_elementary_vector(grad(i_f+1,:), valnoe, bdyty(i_bdyt)%blmty(i_blmty)%edof2gdof)
      !end if

    end do

    ! computing value at quadratic nodes when need arise
    do i_nodty = 1, bdyty(i_bdyty)%nb_nodes
      ! mean value at node from node of elements values
      field(:,i_nodty) = field(:,i_nodty) / size(M_bdyty(iM_bdyty)%nod2blmty(i_nodty)%G_i)
    end do

  end subroutine get_nodal_field

  !> \brief Compute and return volume of each elements of a body
  !> Memory allocated within the function
  function get_elements_volume(i_bdyty)
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !> allocated array holding volume of each elements
    real(kind=8), dimension(:), pointer :: get_elements_volume
    !
    real(kind=8), dimension(:,:), pointer :: coor_ele
    integer(kind=4)   :: i_blmty
    character(len=31) :: IAM
    character(len=80) :: cout
    !      1234567890123456789012345678901
    IAM = 'multiMAILx::get_elements_volume'

    get_elements_volume => null()
    
    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if
    
    allocate(get_elements_volume(size(bdyty(i_bdyty)%blmty))) 

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      coor_ele => get_ptr_coor_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%nodes)) 
      
      get_elements_volume(i_blmty) = compute_elementary_volume(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)

    enddo

  end function

  !> \brief Compute and return neigbors of each elements of a body
  !> Memory allocated within the function
  function get_elements_neighbor(i_bdyty, tol, max_neighbors )
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !> tolerance
    real(kind=8),    intent(in) :: tol
    !> maximum number of neighbors
    integer(kind=4), intent(in) :: max_neighbors
    !> allocated array holding neighbor of each elements
    integer(kind=4), dimension(:,:), pointer :: get_elements_neighbor
    !
    logical :: skip
    real(kind=8) :: length
    integer(kind=4) :: nb_ele, i_blmty, ie, je, i
    real(kind=8), dimension(nbDIME) :: v
    !real(kind=8) :: length, center(nbdime)
    real(kind=8), dimension(:,:), pointer     :: coor_ele
    real(kind=8), dimension(:,:), allocatable :: centers
    character(len=33) :: IAM
    character(len=80) :: cout
    !      123456789012345678901234567890123
    IAM = 'multiMAILx::get_elements_neighbor'

    get_elements_neighbor => null()
    
    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    nb_ele = size(bdyty(i_bdyty)%blmty)
    allocate(get_elements_neighbor(max_neighbors,nb_ele))

    allocate(centers(nbDIME,nb_ele))

    ! initializations
    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
      coor_ele => get_ptr_coor_ele_multiEF(bdyty(i_bdyty)%blmty(i_blmty)%blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%nodes)) 

      call compute_elementary_center(bdyty(i_bdyty)%blmty(i_blmty)%blmnb,centers(:,i_blmty))
    enddo

    get_elements_neighbor = 0

    ! looking for neighbors
    do ie = 1, nb_ele - 1
      do je = 1, nb_ele

      if (je == ie .or. count(get_elements_neighbor(:,ie) == je) /= 0) cycle

        v(1:nbDIME) = centers(1:nbDIME,ie) - centers(1:nbDIME,je)

        ! rough manhattan test
        skip = .FALSE.
        do i = 1, nbDIME
          if (abs(v(i)) > tol) then
            skip = .TRUE.
            exit
          end if
        end do
        if (skip) cycle

        select case(nbDIME)
        case(2) 
          length = length2(v)                  
        case(3)
          length = length3(v)
        end select 
        if (length > tol) cycle

        i = minloc(get_elements_neighbor(:,ie), mask=get_elements_neighbor(:,ie) == 0, dim=1) 
        if (i ==  max_neighbors) then
          print*, ie 
          print*, Get_Elements_Neighbor(:,ie)
          call faterr(IAM,'max_neighbors reached')
        endif
        get_elements_neighbor(i,ie) = je

        i = minloc(get_elements_neighbor(:,je), mask=get_elements_neighbor(:,je) == 0, dim=1)   
        if (i ==  max_neighbors) then
          print*,je
          print*,Get_Elements_Neighbor(:,je)
          call FATERR(IAM,'max_neighbors reached')
        endif  
        get_elements_neighbor(i,je) = ie

      end do
    end do

  end function

  !> \brief Get pointer on energy of each elements of a body
  function get_ptr_elements_energy(i_bdyty)
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !> energy of elements
    real(kind=8), dimension(:), pointer :: get_ptr_elements_energy
    !
    character(len=35) :: IAM
    character(len=80) :: cout
    !      12345678901234567890123456789012345
    IAM = 'multiMAILx::get_ptr_elements_energy'

    get_ptr_elements_energy => null()
    
    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    get_ptr_elements_energy => bdyty(i_bdyty)%el_energy
  end function

  !> \brief Get pointer on jacobian of each elements of a body
  function get_ptr_elements_jacobian(i_bdyty)
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !> energy of elements
    real(kind=8), dimension(:), pointer :: get_ptr_elements_jacobian
    !
    character(len=37) :: IAM
    character(len=80) :: cout
    !      1234567890123456789012345678901234567
    IAM = 'multiMAILx::get_ptr_elements_jacobian'

    get_ptr_elements_jacobian => null()
    
    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    get_ptr_elements_jacobian => bdyty(i_bdyty)%el_jacobian
  end function

  !> \brief Get pointer on visibility of each elements of a body
  function get_ptr_elements_visibility(i_bdyty)
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !> energy of elements
    integer(kind=4), dimension(:), pointer :: get_ptr_elements_visibility
    !
    character(len=39) :: IAM
    character(len=80) :: cout
    !      123456789012345678901234567890123456789
    IAM = 'multiMAILx::get_ptr_elements_visibility'

    get_ptr_elements_visibility => null()
    
    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    get_ptr_elements_visibility => bdyty(i_bdyty)%eviz
  end function

  subroutine compute_elements_energy(i_bdyty)
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !
    real(kind=8), dimension(:,:), pointer :: coor_ele, grad_ele, flux_ele
    real(kind=8), dimension(:)  , pointer :: dof_ele, field_ele
    real(kind=8)      :: E_def, E_cin!, E_pot
    integer(kind=4)   :: i_blmty, i_nodty, iM_blmty, iM_bdyty, blmnb, i, iccdof, field_rank
    character(len=35) :: IAM
    character(len=80) :: cout
    !      12345678901234567890123456789012345
    IAM = 'multiMAILx::compute_elements_energy'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      if (bdyty(i_bdyty)%eviz(i_blmty) ==  0) then
         bdyty(i_bdyty)%el_energy(i_blmty) = 0.d0
         cycle
      end if
 
      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      blmnb    = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      coor_ele => get_ptr_coor_ele_multiEF(blmnb)
      coor_ele =  get_cooref_ele(i_bdyty,i_blmty,size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)) 

      !============ compute elastic deformation energy with stiffness operator ============!
      !       uses only grad and flux when computing energy associated to stiffness
      grad_ele => get_ptr_grad_field_ele_multiEF(blmnb, p_stiffness)
      flux_ele => get_ptr_flux_field_ele_multiEF(blmnb, p_stiffness)

      call get_multi_grad_MAILx(iM_bdyty, iM_blmty, p_disp, 1, grad_ele)
      call get_multi_flux_MAILx(iM_bdyty, iM_blmty, p_disp, 1, flux_ele)

      E_def = compute_elementary_energy(blmnb, p_stiffness)
      !====================================================================================!

      !============= compute kinetic energy with velocities and mass operator =============!
      !              uses velocity and density when computing kinetic energy
      field_ele => get_ptr_field_ele_multiEF(blmnb, p_mass_s)
      field_rank = bdyty(i_bdyty)%blmty(i_blmty)%eo2fr(p_mass_s)
      if( field_rank > 0 ) then
        call get_multi_field_MAILx(iM_bdyty, iM_blmty, 1, field_rank, field_ele)
      else
        field_ele = 1.
      end if

      dof_ele => get_ptr_node_field_multiEF(blmnb, p_disp)
      do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
        i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
        iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)
        dof_ele((i-1)*nbDIME+1:i*nbDIME) =  bdyty(i_bdyty)%Dofs(iccdof+1:iccdof+nbDIME,1)
      end do

      E_cin = compute_elementary_energy(blmnb, p_mass_s)
      !====================================================================================!

      !======= compute gravity potential energy with displacement and mass operator =======!
      !do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
      !  i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
      !  iccdof  = bdyty(i_bdyty)%ccdof(i_nodty)
      !  dof_ele((i-1)*nbDIME+1:i*nbDIME) = coor_ele(:,i) + bdyty(i_bdyty)%X(:,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i),1)
      !end do
      ! 
      !E_pot = compute_elementary_energy(blmnb, p_mass, g(1:nbDIME))
      !====================================================================================!

      bdyty(i_bdyty)%el_energy(i_blmty) = E_def + E_cin !+ E_pot

    end do

  end subroutine

  subroutine compute_elements_jacobian(i_bdyty)
    implicit none
    !> id of body
    integer(kind=4), intent(in) :: i_bdyty
    !
    real(kind=8), dimension(:,:), pointer :: grad_ele
    integer(kind=4)   :: i_blmty, iM_blmty, iM_bdyty, blmnb
    character(len=37) :: IAM
    character(len=80) :: cout
    !      1234567890123456789012345678901234567
    IAM = 'multiMAILx::compute_elements_jacobian'

    if( i_bdyty < 1 .or. i_bdyty > nb_multiMAILx ) then
      write(cout,'(A,1x,I0)') 'Unknown body:', i_bdyty
      call faterr(IAM,cout)
    end if

    iM_bdyty = bdyty2M_bdyty(i_bdyty)

    do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

      if (bdyty(i_bdyty)%eviz(i_blmty) ==  0) then
         bdyty(i_bdyty)%el_jacobian(i_blmty) = 0.d0
         cycle
      end if
 
      iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)
      blmnb    = bdyty(i_bdyty)%blmty(i_blmty)%blmnb

      grad_ele => get_ptr_grad_field_ele_multiEF(blmnb, p_stiffness)

      call get_multi_grad_MAILx(iM_bdyty, iM_blmty, p_disp, 1, grad_ele)

      bdyty(i_bdyty)%el_jacobian(i_blmty) = compute_elementary_jacobian(blmnb, p_stiffness, &
                                            bdyty(i_bdyty)%blmty(i_blmty)%ppsnb             )

    end do

  end subroutine

  !> computes the deformation energy due to a given field
  function get_deformation_energy(i_bdyty,disp)
     implicit none 
     !> body to compute deformation energy
     integer(kind=4), intent(in) :: i_bdyty
     !> displacement field to use to compute energy
     real(kind=8), dimension(:,:), intent(in) :: disp
     !> compute deformation energy
     real(kind=8) :: get_deformation_energy
     ! ***
     character(len=80) :: cout
     character(len=34) :: IAM
     integer(kind=4) :: i_blmty, i_nodty, i, blmnb
     real(kind=8), dimension(:), pointer :: disp_ele

     !      1234567890123456789012345678901234
     IAM = 'multiMAILx::get_deformation_energy'

     get_deformation_energy = 0.d0

     do i_blmty = 1, size(bdyty(i_bdyty)%blmty)

       if (bdyty(i_bdyty)%eviz(i_blmty) ==  0) cycle

       blmnb = bdyty(i_bdyty)%blmty(i_blmty)%blmnb
       disp_ele => get_ptr_node_field_multiEF(blmnb, p_disp)
  
       do i = 1, size(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
         ! change to global numbering
         i_nodty = bdyty(i_bdyty)%blmty(i_blmty)%NODES(i) 
         disp_ele((i-1)*nbDIME+1:i*nbDIME) = disp(:,bdyty(i_bdyty)%blmty(i_blmty)%NODES(i))
       end do

       get_deformation_energy = get_deformation_energy + &
                                compute_elementary_deformation_energy(blmnb, p_stiffness, bdyty(i_bdyty)%blmty(i_blmty)%stiffness)
     end do

  end function

  !> \brief free memory allocated within the module
  subroutine clean_memory_multiMAILx()
    implicit none
    integer(kind=4) :: i_bdyty, i_blmty, i

    if( allocated(bdyty) ) then
      do i_bdyty = 1, size(bdyty)
        if( associated(bdyty(i_bdyty)%blmty) ) then
          do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%NODES)  ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%stiffness) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%stiffness)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%mass) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%mass)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%damping) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%damping)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fext) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fext)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fint) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fint)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc)
            end if
          end do
          deallocate(bdyty(i_bdyty)%blmty)
        end if

        if( associated(bdyty(i_bdyty)%eviz)        ) deallocate(bdyty(i_bdyty)%eviz)
        if( associated(bdyty(i_bdyty)%el_energy)   ) deallocate(bdyty(i_bdyty)%el_energy)
        if( associated(bdyty(i_bdyty)%el_jacobian) ) deallocate(bdyty(i_bdyty)%el_jacobian)

        if( associated(bdyty(i_bdyty)%blmty2M_blmty) ) deallocate(bdyty(i_bdyty)%blmty2M_blmty)

        if( associated(bdyty(i_bdyty)%nodty) ) then
          deallocate(bdyty(i_bdyty)%nodty)
        end if

        if( associated(bdyty(i_bdyty)%nodty2M_nodty) ) deallocate(bdyty(i_bdyty)%nodty2M_nodty)

        if( associated(bdyty(i_bdyty)%Dofs     ) ) deallocate(bdyty(i_bdyty)%Dofs     )
        if( associated(bdyty(i_bdyty)%DofsLast ) ) deallocate(bdyty(i_bdyty)%DofsLast )

        if( associated(bdyty(i_bdyty)%X     ) ) deallocate(bdyty(i_bdyty)%X     )

        if( associated(bdyty(i_bdyty)%Fext) ) deallocate(bdyty(i_bdyty)%Fext)
        if( associated(bdyty(i_bdyty)%Fint) ) deallocate(bdyty(i_bdyty)%Fint)
        if( associated(bdyty(i_bdyty)%Fdmp) ) deallocate(bdyty(i_bdyty)%Fdmp)

        if( associated(bdyty(i_bdyty)%Ireac)) deallocate(bdyty(i_bdyty)%Ireac)
        if( associated(bdyty(i_bdyty)%Iaux) ) deallocate(bdyty(i_bdyty)%Iaux )

        if( associated(bdyty(i_bdyty)%DofsFree) ) deallocate(bdyty(i_bdyty)%DofsFree)
        if( associated(bdyty(i_bdyty)%DofsAux ) ) deallocate(bdyty(i_bdyty)%DofsAux )

        if( associated(bdyty(i_bdyty)%residu) ) deallocate(bdyty(i_bdyty)%residu)
        if( associated(bdyty(i_bdyty)%Fdyn)   ) deallocate(bdyty(i_bdyty)%Fdyn)

        if( associated(bdyty(i_bdyty)%ccdof) ) deallocate(bdyty(i_bdyty)%ccdof)
        if( associated(bdyty(i_bdyty)%nodnb) ) deallocate(bdyty(i_bdyty)%nodnb)
        if( associated(bdyty(i_bdyty)%dofnb) ) deallocate(bdyty(i_bdyty)%dofnb)
        if( associated(bdyty(i_bdyty)%ccsize)) deallocate(bdyty(i_bdyty)%ccsize)

        if( associated(bdyty(i_bdyty)%coorTT ) ) deallocate(bdyty(i_bdyty)%coorTT )

        if( allocated(bdyty(i_bdyty)%primal_drvdofs) ) then
          do i = 1, size(bdyty(i_bdyty)%primal_drvdofs)
            if( associated(bdyty(i_bdyty)%primal_drvdofs(i)%time_evolution%x) ) then
              deallocate(bdyty(i_bdyty)%primal_drvdofs(i)%time_evolution%x)
            end if

            if( associated(bdyty(i_bdyty)%primal_drvdofs(i)%time_evolution%fx) ) then
              deallocate(bdyty(i_bdyty)%primal_drvdofs(i)%time_evolution%fx)
            end if
          end do

          deallocate(bdyty(i_bdyty)%primal_drvdofs)
        end if

        if( allocated(bdyty(i_bdyty)%dual_drvdofs) ) then
          do i = 1, size(bdyty(i_bdyty)%dual_drvdofs)
            if( associated(bdyty(i_bdyty)%dual_drvdofs(i)%time_evolution%x) ) then
              deallocate(bdyty(i_bdyty)%dual_drvdofs(i)%time_evolution%x)
            end if

            if( associated(bdyty(i_bdyty)%dual_drvdofs(i)%time_evolution%fx) ) then
              deallocate(bdyty(i_bdyty)%dual_drvdofs(i)%time_evolution%fx)
            end if
          end do

          deallocate(bdyty(i_bdyty)%dual_drvdofs)
        end if

        if( associated(bdyty(i_bdyty)%drvdofs)  ) deallocate(bdyty(i_bdyty)%drvdofs)
        if( associated(bdyty(i_bdyty)%drvvalues)) deallocate(bdyty(i_bdyty)%drvvalues)

        call erase_system(bdyty(i_bdyty)%g_sys)
        if( associated(bdyty(i_bdyty)%RHS)) deallocate(bdyty(i_bdyty)%RHS)

      end do
      deallocate(bdyty)

      nb_multiMAILx = 0

    end if
  
    if( allocated(bdyty2M_bdyty) ) deallocate(bdyty2M_bdyty)
    if( allocated(M2multi)       ) deallocate(M2multi)
  
  end subroutine

  function Get_ptr_Boundary_Elements(ibdyty)
    implicit none 
    integer :: ibdyty
    integer,dimension(:),pointer :: Get_ptr_Boundary_Elements
    !***
    integer :: iM_bdyty
    iM_bdyty=bdyty2M_bdyty(ibdyty)

    Get_ptr_Boundary_Elements => M_bdyty(iM_bdyty)%boundary_elements

  end function

  SUBROUTINE set_without_renum_multiMAILx
    IMPLICIT NONE

    with_renum=.FALSE.

  END SUBROUTINE set_without_renum_multiMAILx

 function get_nb_gp_multiMAILx(i_bdyty, i_blmty)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> number of gauss points in the element
   integer :: get_nb_gp_multiMAILx
   !
   integer :: iM_bdyty, iM_blmty

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   if( associated( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv ) ) then
     get_nb_gp_multiMAILx = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv, 1 )
   else
     get_nb_gp_multiMAILx = 0
   end if

 end function get_nb_gp_multiMAILx

 subroutine get_field_multiMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:grad, 2:flux, 3:internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(out) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%grad, 1 )
     field_array(1:field_size,:) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%grad(:,:)
   case( 2 )
     field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%flux, 1 )
     field_array(1:field_size,:) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%flux(:,:)
   case( 3 )
     if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%internal) ) return
     field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%internal, 1 )
     field_array(1:field_size,:) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%internal(:,:)
   end select

 end subroutine get_field_multiMAILx

 subroutine set_field_multiMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:grad, 2:flux, 3:internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(in) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%grad, 1 )
     M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(1)%grad(:,:) = field_array(1:field_size,:)
     M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%grad(:,:) = field_array(1:field_size,:)
   case( 2 )
     field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%flux, 1 )
     M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(1)%flux(:,:) = field_array(1:field_size,:)
     M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%flux(:,:) = field_array(1:field_size,:)
   case( 3 )
     if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%internal) ) return
     field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%internal, 1 )
     M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(1)%internal(:,:) = field_array(1:field_size,:)
     M_bdyty(iM_bdyty)%blmty(iM_blmty)%multi_gpv(2)%internal(:,:) = field_array(1:field_size,:)
   end select

 end subroutine set_field_multiMAILx

end module multiMAILx
