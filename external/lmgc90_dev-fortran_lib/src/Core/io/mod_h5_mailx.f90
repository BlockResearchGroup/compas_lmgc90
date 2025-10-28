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

!> Define the mailx hdf5 subroutines

! Naming convention in the module :
! rdata/idata : data save per body 'r' stands for 'real' and 'i' for 'integer'
! nrdata      : nodal real data saved per body
! erdata      : element real data saved per body

module h5_mailx

  use overall, only : M_INTEGRATOR_ID    , &
                      INTEGRATOR_BETA2   , &
                      INTEGRATOR_MOREAU  , &
                      nbDIME, faterr

  use MAILx_type, only : T_mecaMAILx, &
                         T_poroMAILx, &
                         T_therMAILx, &
                         T_multiMAILx

  use mecaMAILx , only : get_bdyty_mecaMAILx      , &
                         get_nb_mecaMAILx         , &
                         get_nb_elements_mecaMAILx, &
                         get_nb_gp_mecaMAILx      , &
                         get_field_mecaMAILx      , &
                         set_field_mecaMAILx

  use therMAILx , only : get_bdyty_therMAILx      , &
                         get_nb_therMAILx         , &
                         get_nb_elements_therMAILx, &
                         get_nb_gp_therMAILx      , &
                         get_field_therMAILx      , &
                         set_field_therMAILx

  use poroMAILx , only : get_bdyty_poroMAILx      , &
                         get_nb_poroMAILx         , &
                         get_nb_elements_poroMAILx, &
                         get_nb_gp_poroMAILx      , &
                         get_field_poroMAILx      , &
                         set_field_poroMAILx

  use multiMAILx , only : get_bdyty_multiMAILx      , &
                          get_nb_multiMAILx         , &
                          get_nb_elements_multiMAILx, &
                          get_nb_gp_multiMAILx      , &
                          get_field_multiMAILx      , &
                          set_field_multiMAILx

  use models, only : get_max_field_sizes

  use h5_format, only : get_gr_name, get_ds_name, &
                        gr_mecax   , gr_therx   , &
                        gr_porox   , gr_multi   , &
                        ds_idata   , ds_rdata   , &
                        ds_disp    , ds_dofs    , &
                        ds_grad    , ds_flux    , &
                        ds_internal             , &
                        fmt_mecax  , fmt_therx  , &
                        fmt_porox  , fmt_multi

  use lmgc90_hdf5, only : read_h5, write_h5


  implicit none

  private

  public ::  read_h5_data_mecaMAILx , &
            write_h5_data_mecaMAILx , &
             read_h5_data_therMAILx , &
            write_h5_data_therMAILx , &
             read_h5_data_poroMAILx , &
            write_h5_data_poroMAILx , &
             read_h5_data_multiMAILx, &
            write_h5_data_multiMAILx

contains

  subroutine read_h5_data_mecaMAILx(step)
    implicit none
    !> evolution id to read
    integer, intent(in) :: step
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata, disp, dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_dofs, nb_gp, tmp
    integer :: ibdyty, iblmty, ibeg, iend, iend2, rigid_size
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    logical :: with_idata, with_rdata, with_disp, with_dofs
    logical :: with_grad , with_flux , with_internal

    type( T_mecaMAILx ), dimension( : ), pointer :: bdyty

    character(len=32), parameter :: IAM = 'h5_mailx::read_h5_data_mecaMAILx'
    character(len=80)            :: cout

    with_idata    = .false.
    with_rdata    = .false.
    with_disp     = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    idata    => null()
    rdata    => null()
    disp     => null()
    dofs     => null()
    grad     => null()
    flux     => null()
    internal => null()

    if( associated( fmt_mecax%idata ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( associated( fmt_mecax%rdata ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_rdata), rdata )
      with_rdata = .true.
    end if

    if( associated( fmt_mecax%disp_field ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_disp), disp )
      with_disp = .true.
    end if

    if( associated( fmt_mecax%dofs_field ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_dofs), dofs )
      with_dofs = .true.
    end if

    if( associated( fmt_mecax%grad ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_grad), grad )
      with_grad = .true.
    end if

    if( associated( fmt_mecax%flux ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_flux), flux )
      with_flux = .true.
    end if

    if( associated( fmt_mecax%internal ) ) then
      call read_h5( get_gr_name(gr_mecax, step), get_ds_name(ds_internal), internal )
      with_internal = .true.
    end if

    ! not a very good check
    ! but no need to do better yet
    if ( .not. associated( idata ) ) return

    ! Get the structure to write into
    call get_bdyty_mecaMAILx( bdyty )
    nb_bodies = get_nb_mecaMAILx()

    ! Check the size of the data from HDF5 and the size of the structure to write into
    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
       call faterr(IAM,'Please read_bodies before trying to read dof')
    end if

    if ( size(bdyty) /= size(idata, 2)-1 ) then
       call faterr(IAM,'Inconsistent size between already allocated bdyty and data in file')
    end if


    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    ! setting some sizes depending on nbDIME
    select case( nbDIME )
    case( 2 )
      rigid_size = 3
    case( 3 )
      rigid_size = 6
    case default
      rigid_size = 0
    end select

    ! From HDF5 buffers, fill the LMGC90's internal structures
    do ibdyty = 1, nb_bodies

      if( with_idata ) then

        ! check nodes number consitency between file and loaded body
        if( fmt_mecax%node_idx > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%node_idx)%ibeg
          nb_nodes = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_nodes /= bdyty(ibdyty)%nb_nodes ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of nodes in mecaMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nb_nodes, '/', nb_nodes, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! check dofs number consitency between file and loaded body
        if( fmt_mecax%dofs_idx > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%dofs_idx)%ibeg
          nb_dofs = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_dofs /= bdyty(ibdyty)%nbdof ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of dofs in mecaMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nbdof, '/', nb_dofs, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! not usefull to us so skipped
        ! but should count total number of gp for each
        ! mesh and confront it to stored value
        !if( fmt_mecax%gps_idx  > 0 ) then
        !end if

        if( fmt_mecax%is_rigid > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%is_rigid)%ibeg
          if( idata( ibeg, ibdyty+1 ) == 0 ) then
            bdyty(ibdyty)%is_rigid = .false. 
          else
            bdyty(ibdyty)%is_rigid = .true. 
          end if
        end if

        if( fmt_mecax%is_coro > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%is_coro)%ibeg
          if( idata( ibeg, ibdyty+1 ) == 0 ) then
            bdyty(ibdyty)%is_coro = .false. 
          else
            bdyty(ibdyty)%is_coro = .true. 
          end if
        end if

      end if

      if( with_rdata ) then

        if( fmt_mecax%RX > 0 .and. allocated(bdyty(ibdyty)%RX) ) then
          ibeg = fmt_mecax%rdata(fmt_mecax%RX)%ibeg
          iend = fmt_mecax%rdata(fmt_mecax%RX)%iend
          bdyty(ibdyty)%RX(1:rigid_size) = rdata( ibeg:iend, ibdyty )
          bdyty(ibdyty)%RXbegin(:)       = bdyty(ibdyty)%RX(:)
        end if

        if( fmt_mecax%RV > 0  .and. allocated(bdyty(ibdyty)%RV) ) then
          ibeg = fmt_mecax%rdata(fmt_mecax%RV)%ibeg
          iend = fmt_mecax%rdata(fmt_mecax%RV)%iend
          bdyty(ibdyty)%RV(1:rigid_size) = rdata( ibeg:iend, ibdyty )
          bdyty(ibdyty)%RVbegin(:)       = bdyty(ibdyty)%RV(:)
        end if

        if( fmt_mecax%LF > 0  .and. allocated(bdyty(ibdyty)%LocalFrame) ) then
          ibeg = fmt_mecax%rdata(fmt_mecax%LF)%ibeg
          iend = fmt_mecax%rdata(fmt_mecax%LF)%iend
          bdyty(ibdyty)%LocalFrame(1:3,1:3) = reshape( rdata(ibeg:iend, ibdyty), shape=(/3,3/) )
          bdyty(ibdyty)%LocalFrameIni(:,:)  = bdyty(ibdyty)%LocalFrame(:,:)
        end if

      end if

      if( with_disp ) then

        if( fmt_mecax%disp > 0 ) then

          ! always nbDIME
          !ibeg = fmt_mecax%disp(fmt_mecax%disp_field)%ibeg
          !iend = fmt_mecax%disp(fmt_mecax%disp_field)%iend

          ! can check size with idata content
          !ibeg = fmt_mecax%idata(fmt_mecax%node_idx)%ibeg
          !istart = idata( fmt_mecax%idata( fmt_mecax%nodes_idx )%ibeg, ibdyty   ) + 1
          !iend2  = idata( fmt_mecax%idata( fmt_mecax%nodes_idx )%ibeg, ibdyty+1 )
          ! or with loaded body
          nb_nodes = bdyty(ibdyty)%nb_nodes
          iend = i_node_offset + nb_nodes

          if( M_INTEGRATOR_ID == INTEGRATOR_BETA2 ) then
            bdyty(ibdyty)%Xprev(:) = reshape( disp( 1:nbDIME, i_node_offset+1:iend ), shape=(/ nbDIME * nb_nodes /) )
          else
             bdyty(ibdyty)%X(:)      = reshape( disp( 1:nbDIME, i_node_offset+1:iend ), shape=(/ nbDIME * nb_nodes /) )
             bdyty(ibdyty)%Xbegin(:) = bdyty(ibdyty)%X(:)
          end if

        end if

      end if

      if( with_dofs ) then

        if( fmt_mecax%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_mecax%dofs_field(fmt_mecax%dofs)%ibeg
          !iend = fmt_mecax%dofs_field(fmt_mecax%dofs)%iend

          ! can check size with idata content
          !nb_dofs = idata( fmt_mecax%idata(fmt_mecax%dofs_idx)%ibeg, ibdyty+1) 
          !         -idata( fmt_mecax%idata(fmt_mecax%dofs_idx)%ibeg, ibdyty  )
          ! or with loaded body
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          bdyty(ibdyty)%V(:)      = dofs( 1, i_dof_offset+1:iend )
          bdyty(ibdyty)%Vbegin(:) = bdyty(ibdyty)%V(:)

        end if

      end if


      !> \todo replace 1, 2, 3, by field id like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_mecax%strain > 0 ) then

          ibeg = fmt_mecax%grad(fmt_mecax%strain)%ibeg
          iend = fmt_mecax%grad(fmt_mecax%strain)%iend

          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
            nb_gp = get_nb_gp_mecaMAILx(ibdyty, iblmty)
            iend2 = tmp + nb_gp
            call set_field_mecaMAILx( ibdyty, iblmty, 2, grad(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_mecax%stress > 0 ) then

          ibeg = fmt_mecax%flux(fmt_mecax%stress)%ibeg
          iend = fmt_mecax%flux(fmt_mecax%stress)%iend

          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
            nb_gp = get_nb_gp_mecaMAILx(ibdyty, iblmty)
            iend2 = tmp + nb_gp
            call set_field_mecaMAILx( ibdyty, iblmty, 1, flux(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_mecax%meca_internal > 0 ) then

          ibeg = fmt_mecax%internal(fmt_mecax%meca_internal)%ibeg
          iend = fmt_mecax%internal(fmt_mecax%meca_internal)%iend

          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
            nb_gp = get_nb_gp_mecaMAILx(ibdyty, iblmty)
            iend2 = tmp + nb_gp
            call set_field_mecaMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_mecaMAILx(ibdyty, iblmty)
      end do

    end do

    if( with_rdata     ) deallocate( rdata     )
    if( with_idata     ) deallocate( idata     )
    if( with_disp      ) deallocate( disp      )
    if( with_dofs      ) deallocate( dofs      )
    if( with_grad      ) deallocate( grad      )
    if( with_flux      ) deallocate( flux      )
    if( with_internal  ) deallocate( internal  )


  end subroutine read_h5_data_mecaMAILx

!------------------------------------------------------------------------

  subroutine write_h5_data_mecaMAILx()
    implicit none
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata, disp, dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_gp, tmp
    integer :: total_nb_nodes, total_nb_dofs, total_nb_elems, total_nb_gps
    integer :: ibdyty, iblmty, rigid_size
    integer :: ibeg, iend, istart, iend2
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    type( T_mecaMAILx ), dimension( : ), pointer :: bdyty

    logical :: with_idata, with_rdata, with_disp, with_dofs
    logical :: with_grad , with_flux , with_internal

    nb_bodies = get_nb_mecaMAILx( )

    ! If no mesh is used: exit
    if ( nb_bodies <= 0 ) then
       return
    end if

    call get_bdyty_mecaMAILx( bdyty )

    with_idata    = .false.
    with_rdata    = .false.
    with_disp     = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    total_nb_nodes = 0
    total_nb_dofs  = 0
    total_nb_elems = 0
    total_nb_gps   = 0

    ! counting
    do ibdyty = 1, nb_bodies
      total_nb_nodes = total_nb_nodes + bdyty(ibdyty)%nb_nodes
      total_nb_dofs  = total_nb_dofs  + bdyty(ibdyty)%nbdof
      total_nb_elems = total_nb_elems + get_nb_elements_mecaMAILx(ibdyty)
      do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
        total_nb_gps  = total_nb_gps  + get_nb_gp_mecaMAILx(ibdyty, iblmty)
      end do
    end do

    
    ! arrays indexed by body number

    if( associated( fmt_mecax%idata ) ) then
      ! start with 0 to get field boundaries
      allocate( idata( fmt_mecax%idata_sz, 0:nb_bodies ) )
      with_idata = .true.
      idata(:,0) = 0
    end if

    if( associated( fmt_mecax%rdata ) ) then
      allocate( rdata( fmt_mecax%rdata_sz, nb_bodies ) )
      with_rdata = .true.
    end if

    ! array indexed by node index cumulated on all bodies

    if( associated( fmt_mecax%disp_field ) ) then
      allocate( disp( fmt_mecax%disp_field_sz, total_nb_nodes ) )
      with_disp = .true.
    end if

    ! array indexed by dofs index cumulated on all bodies

    if( associated( fmt_mecax%dofs_field ) ) then
      allocate( dofs( fmt_mecax%dofs_field_sz, total_nb_dofs ) )
      with_dofs = .true.
    end if


    ! arrays indexed by gauss point index cumulated on all bodies

    if( associated( fmt_mecax%grad ) ) then
      allocate( grad( fmt_mecax%grad_sz, total_nb_gps ) )
      with_grad = .true.
    end if

    if( associated( fmt_mecax%flux ) ) then
      allocate( flux( fmt_mecax%flux_sz, total_nb_gps ) )
      with_flux = .true.
    end if

    if( associated( fmt_mecax%internal ) ) then
      allocate( internal( fmt_mecax%internal_sz, total_nb_gps ) )
      with_internal = .true.
    end if


    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    ! setting some sizes depending on nbDIME
    select case( nbDIME )
    case( 2 )
      rigid_size = 3
    case( 3 )
      rigid_size = 6
    case default
      rigid_size = 0
    end select

    do ibdyty = 1, nb_bodies

      if( with_idata ) then
   
        if( fmt_mecax%node_idx > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%node_idx)%ibeg
          idata( ibeg, ibdyty) = i_node_offset + bdyty(ibdyty)%nb_nodes
        end if

        if( fmt_mecax%dofs_idx > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%dofs_idx)%ibeg
          idata( ibeg, ibdyty) = i_dof_offset + bdyty(ibdyty)%nbdof
        end if

        if( fmt_mecax%gps_idx  > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%gps_idx)%ibeg
          idata( ibeg, ibdyty) = i_gp_offset
          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
             idata(ibeg, ibdyty) = idata(ibeg, ibdyty) + get_nb_gp_mecaMAILx(ibdyty, iblmty)
          end do
        end if

        if( fmt_mecax%is_rigid > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%is_rigid)%ibeg
          idata( ibeg, ibdyty ) = merge( 1, 0, bdyty(ibdyty)%is_rigid )
        end if

        if( fmt_mecax%is_coro > 0 ) then
          ibeg = fmt_mecax%idata(fmt_mecax%is_coro)%ibeg
          idata( ibeg, ibdyty ) = merge( 1, 0, bdyty(ibdyty)%is_coro )
        end if

      end if

      if( with_rdata ) then

        if( fmt_mecax%RX > 0 .and. allocated(bdyty(ibdyty)%RX) ) then
          ibeg = fmt_mecax%rdata(fmt_mecax%RX)%ibeg
          iend = fmt_mecax%rdata(fmt_mecax%RX)%iend
          rdata( ibeg:iend, ibdyty ) = bdyty(ibdyty)%RX(1:rigid_size)
        end if

        if( fmt_mecax%RV > 0  .and. allocated(bdyty(ibdyty)%RV) ) then
          ibeg = fmt_mecax%rdata(fmt_mecax%RV)%ibeg
          iend = fmt_mecax%rdata(fmt_mecax%RV)%iend
          rdata( ibeg:iend, ibdyty ) = bdyty(ibdyty)%RV(1:rigid_size)
        end if

        if( fmt_mecax%LF > 0  .and. allocated(bdyty(ibdyty)%LocalFrame) ) then
          ibeg = fmt_mecax%rdata(fmt_mecax%LF)%ibeg
          iend = fmt_mecax%rdata(fmt_mecax%LF)%iend
          rdata( ibeg:iend, ibdyty ) = reshape( bdyty(ibdyty)%LocalFrame(1:3,1:3), shape=(/9/) )
        end if

      end if

      if( with_disp ) then

        if( fmt_mecax%disp > 0 ) then

          ! always nbDIME
          !ibeg = fmt_mecax%disp(fmt_mecax%disp_field)%ibeg
          !iend = fmt_mecax%disp(fmt_mecax%disp_field)%iend

          !istart = idata( fmt_mecax%idata( fmt_mecax%nodes_idx )%ibeg, ibdyty-1 ) + 1
          !iend2  = idata( fmt_mecax%idata( fmt_mecax%nodes_idx )%ibeg, ibdyty   )
          nb_nodes = bdyty(ibdyty)%nb_nodes
          iend = i_node_offset + nb_nodes

          if( M_INTEGRATOR_ID == INTEGRATOR_BETA2 ) then
            disp( 1:nbDIME, i_node_offset+1:iend ) = reshape( bdyty(ibdyty)%Xprev(:), shape=(/ nbDIME, nb_nodes /) )
          else
            disp( 1:nbDIME, i_node_offset+1:iend ) = reshape( bdyty(ibdyty)%X(:)    , shape=(/ nbDIME, nb_nodes /) )
          end if

        end if

      end if

      if( with_dofs ) then

        if( fmt_mecax%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_mecax%dofs_field(fmt_mecax%dofs)%ibeg
          !iend = fmt_mecax%dofs_field(fmt_mecax%dofs)%iend
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          dofs( 1, i_dof_offset+1:iend ) = bdyty(ibdyty)%V(:)

        end if

      end if


      !> \todo replace 1, 2, 3, by field id like i_stress, i_strain...

      tmp = i_gp_offset
      if( with_grad ) then
        if( fmt_mecax%strain > 0 ) then

          ibeg = fmt_mecax%grad(fmt_mecax%strain)%ibeg
          iend = fmt_mecax%grad(fmt_mecax%strain)%iend

          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
            nb_gp = get_nb_gp_mecaMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_mecaMAILx( ibdyty, iblmty, 2, grad(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      tmp = i_gp_offset
      if( with_flux ) then
        if( fmt_mecax%stress > 0 ) then

          ibeg = fmt_mecax%flux(fmt_mecax%stress)%ibeg
          iend = fmt_mecax%flux(fmt_mecax%stress)%iend

          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
            nb_gp = get_nb_gp_mecaMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_mecaMAILx( ibdyty, iblmty, 1, flux(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      tmp = i_gp_offset
      if( with_internal ) then
        if( fmt_mecax%meca_internal > 0 ) then

          ibeg = fmt_mecax%internal(fmt_mecax%meca_internal)%ibeg
          iend = fmt_mecax%internal(fmt_mecax%meca_internal)%iend

          do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
            nb_gp = get_nb_gp_mecaMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_mecaMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_mecaMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_mecaMAILx(ibdyty, iblmty)
      end do

    end do

    ! The HDF5 part
    if( with_rdata ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_rdata), rdata )
      deallocate( rdata )
    end if
    if( with_idata ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if
    if( with_disp ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_disp), disp )
      deallocate( disp )
    end if
    if( with_dofs ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_dofs), dofs )
      deallocate( dofs )
    end if
    if( with_grad ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_grad), grad )
      deallocate( grad )
    end if
    if( with_flux ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_flux), flux )
      deallocate( flux )
    end if
    if( with_internal ) then
      call write_h5( get_gr_name(gr_mecax), get_ds_name(ds_internal), internal )
      deallocate( internal )
    end if

  end subroutine write_h5_data_mecaMAILx

!------------------------------------------------------------------------

  subroutine read_h5_data_therMAILx(step)
    implicit none
    !> evolution id to read
    integer, intent(in) :: step
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_dofs, nb_gp, tmp
    integer :: ibdyty, iblmty, ibeg, iend, iend2, rigid_size
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    logical :: with_idata, with_dofs
    logical :: with_grad , with_flux , with_internal

    type( T_therMAILx ), dimension( : ), pointer :: bdyty

    character(len=32), parameter :: IAM = 'h5_mailx::read_h5_data_therMAILx'
    character(len=80)            :: cout

    with_idata    = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    idata    => null()
    dofs     => null()
    grad     => null()
    flux     => null()
    internal => null()

    if( associated( fmt_therx%idata ) ) then
      call read_h5( get_gr_name(gr_therx, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( associated( fmt_therx%dofs_field ) ) then
      call read_h5( get_gr_name(gr_therx, step), get_ds_name(ds_dofs), dofs )
      with_dofs = .true.
    end if

    if( associated( fmt_therx%grad ) ) then
      call read_h5( get_gr_name(gr_therx, step), get_ds_name(ds_grad), grad )
      with_grad = .true.
    end if

    if( associated( fmt_therx%flux ) ) then
      call read_h5( get_gr_name(gr_therx, step), get_ds_name(ds_flux), flux )
      with_flux = .true.
    end if

    if( associated( fmt_therx%internal ) ) then
      call read_h5( get_gr_name(gr_therx, step), get_ds_name(ds_internal), internal )
      with_internal = .true.
    end if

    ! not a very good check
    ! but no need to to better yet
    if ( .not. associated( idata ) ) return

    ! Get the structure to write into
    call get_bdyty_therMAILx( bdyty )
    nb_bodies = get_nb_therMAILx()

    ! Check the size of the data from HDF5 and the size of the structure to write into
    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
       call faterr(IAM,'Please read_bodies before trying to read dof')
    end if

    if ( size(bdyty) /= size(idata, 2)-1 ) then
       call faterr(IAM,'Inconsistent size between already allocated bdyty and data in file')
    end if


    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    ! From HDF5 buffers, fill the LMGC90's internal structures
    do ibdyty = 1, nb_bodies

      if( with_idata ) then

        ! check nodes number consitency between file and loaded body
        if( fmt_therx%node_idx > 0 ) then
          ibeg = fmt_therx%idata(fmt_therx%node_idx)%ibeg
          nb_nodes = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_nodes /= bdyty(ibdyty)%nb_nodes ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of nodes in therMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nb_nodes, '/', nb_nodes, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! check dofs number consitency between file and loaded body
        if( fmt_therx%dofs_idx > 0 ) then
          ibeg = fmt_therx%idata(fmt_therx%dofs_idx)%ibeg
          nb_dofs = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_dofs /= bdyty(ibdyty)%nbdof ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of dofs in therMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nbdof, '/', nb_dofs, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! not usefull to us so skipped
        ! but should count total number of gp for each
        ! mesh and confront it to stored value
        !if( fmt_therx%gps_idx  > 0 ) then
        !end if

      end if


      if( with_dofs ) then

        if( fmt_therx%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_therx%dofs_field(fmt_therx%dofs)%ibeg
          !iend = fmt_therx%dofs_field(fmt_therx%dofs)%iend

          ! can check size with idata content
          !nb_dofs = idata( fmt_therx%idata(fmt_therx%dofs_idx)%ibeg, ibdyty+1) 
          !         -idata( fmt_therx%idata(fmt_therx%dofs_idx)%ibeg, ibdyty  )
          ! or with loaded body
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          bdyty(ibdyty)%T(:)      = dofs( 1, i_dof_offset+1:iend )
          bdyty(ibdyty)%Tbegin(:) = bdyty(ibdyty)%T(:)

        end if

      end if


      !> \todo replace 1, 2, 3, by field id like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_therx%ther_grad > 0 ) then

          ibeg = fmt_therx%grad(fmt_therx%ther_grad)%ibeg
          iend = fmt_therx%grad(fmt_therx%ther_grad)%iend

          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
            nb_gp = get_nb_gp_therMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_therMAILx( ibdyty, iblmty, 1, grad(ibeg:iend, tmp+1:iend2) ) ! ther_grad
            tmp = i_gp_offset + nb_gp
          end do

        end if

      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_therx%ther_flux > 0 ) then

          ibeg = fmt_therx%flux(fmt_therx%ther_flux)%ibeg
          iend = fmt_therx%flux(fmt_therx%ther_flux)%iend

          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
            nb_gp = get_nb_gp_therMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_therMAILx( ibdyty, iblmty, 2, flux(ibeg:iend, tmp+1:iend2) ) ! ther_flux
            tmp = tmp + nb_gp
          end do

        end if

      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_therx%ther_internal > 0 ) then

          ibeg = fmt_therx%internal(fmt_therx%ther_internal)%ibeg
          iend = fmt_therx%internal(fmt_therx%ther_internal)%iend

          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
            nb_gp = get_nb_gp_therMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_therMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) ) ! internal
            tmp = tmp + nb_gp
          end do

        end if

      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_therMAILx(ibdyty, iblmty)
      end do

    end do

    if( with_idata     ) deallocate( idata     )
    if( with_dofs      ) deallocate( dofs      )
    if( with_grad      ) deallocate( grad      )
    if( with_flux      ) deallocate( flux      )
    if( with_internal  ) deallocate( internal  )

  end subroutine read_h5_data_therMAILx

!------------------------------------------------------------------------

  subroutine write_h5_data_therMAILx()
    implicit none
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_gp, tmp
    integer :: total_nb_nodes, total_nb_dofs, total_nb_elems, total_nb_gps
    integer :: ibdyty, iblmty
    integer :: ibeg, iend, istart, iend2
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    type( T_therMAILx ), dimension( : ), pointer :: bdyty

    logical :: with_idata, with_dofs
    logical :: with_grad , with_flux, with_internal

    nb_bodies = get_nb_therMAILx( )

    ! If no mesh is used: exit
    if ( nb_bodies <= 0 ) then
       return
    end if

    call get_bdyty_therMAILx( bdyty )

    with_idata    = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    total_nb_nodes = 0
    total_nb_dofs  = 0
    total_nb_elems = 0
    total_nb_gps   = 0

    ! counting
    do ibdyty = 1, nb_bodies
      total_nb_nodes = total_nb_nodes + bdyty(ibdyty)%nb_nodes
      total_nb_dofs  = total_nb_dofs  + bdyty(ibdyty)%nbdof
      total_nb_elems = total_nb_elems + get_nb_elements_therMAILx(ibdyty)
      do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
        total_nb_gps  = total_nb_gps  + get_nb_gp_therMAILx(ibdyty, iblmty)
      end do
    end do

    
    ! arrays indexed by body number

    if( associated( fmt_therx%idata ) ) then
      ! start with 0 to get field boundaries
      allocate( idata( fmt_therx%idata_sz, 0:nb_bodies ) )
      with_idata = .true.
      idata(:,0) = 0
    end if

    ! array indexed by dofs index cumulated on all bodies

    if( associated( fmt_therx%dofs_field ) ) then
      allocate( dofs( fmt_therx%dofs_field_sz, total_nb_dofs ) )
      with_dofs = .true.
    end if


    ! arrays indexed by gauss point index cumulated on all bodies

    if( associated( fmt_therx%grad ) ) then
      allocate( grad( fmt_therx%grad_sz, total_nb_gps ) )
      with_grad = .true.
    end if

    if( associated( fmt_therx%flux ) ) then
      allocate( flux( fmt_therx%flux_sz, total_nb_gps ) )
      with_flux = .true.
    end if

    if( associated( fmt_therx%internal ) ) then
      allocate( internal( fmt_therx%internal_sz, total_nb_gps ) )
      with_internal = .true.
    end if


    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    do ibdyty = 1, nb_bodies

      if( with_idata ) then
   
        if( fmt_therx%node_idx > 0 ) then
          ibeg = fmt_therx%idata(fmt_therx%node_idx)%ibeg
          idata( ibeg, ibdyty) = i_node_offset + bdyty(ibdyty)%nb_nodes
        end if

        if( fmt_therx%dofs_idx > 0 ) then
          ibeg = fmt_therx%idata(fmt_therx%dofs_idx)%ibeg
          idata( ibeg, ibdyty) = i_dof_offset + bdyty(ibdyty)%nbdof
        end if

        if( fmt_therx%gps_idx  > 0 ) then
          ibeg = fmt_therx%idata(fmt_therx%gps_idx)%ibeg
          idata( ibeg, ibdyty) = i_gp_offset
          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
             idata(ibeg, ibdyty) = idata(ibeg, ibdyty) + get_nb_gp_therMAILx(ibdyty, iblmty)
          end do
        end if

      end if

      if( with_dofs ) then

        if( fmt_therx%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_therx%dofs_field(fmt_therx%dofs)%ibeg
          !iend = fmt_therx%dofs_field(fmt_therx%dofs)%iend
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          dofs( 1, i_dof_offset+1:iend ) = bdyty(ibdyty)%T(:)

        end if

      end if


      !> \todo replace 1, 2, 3, etc by field ids like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_therx%ther_grad > 0 ) then

          ibeg = fmt_therx%grad(fmt_therx%ther_grad)%ibeg
          iend = fmt_therx%grad(fmt_therx%ther_grad)%iend

          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
            nb_gp = get_nb_gp_therMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_therMAILx( ibdyty, iblmty, 1, grad(ibeg:iend, tmp+1:iend2) )
            tmp = i_gp_offset + nb_gp
          end do

        end if

      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_therx%ther_flux > 0 ) then

          ibeg = fmt_therx%flux(fmt_therx%ther_flux)%ibeg
          iend = fmt_therx%flux(fmt_therx%ther_flux)%iend

          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
            nb_gp = get_nb_gp_therMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_therMAILx( ibdyty, iblmty, 2, flux(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if
      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_therx%ther_internal > 0 ) then

          ibeg = fmt_therx%internal(fmt_therx%ther_internal)%ibeg
          iend = fmt_therx%internal(fmt_therx%ther_internal)%iend

          do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
            nb_gp = get_nb_gp_therMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_therMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) )
            tmp = tmp + nb_gp
          end do

        end if

      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_therMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_therMAILx(ibdyty, iblmty)
      end do

    end do

    ! The HDF5 part
    if( with_idata ) then
      call write_h5( get_gr_name(gr_therx), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if
    if( with_dofs ) then
      call write_h5( get_gr_name(gr_therx), get_ds_name(ds_dofs), dofs )
      deallocate( dofs )
    end if
    if( with_grad ) then
      call write_h5( get_gr_name(gr_therx), get_ds_name(ds_grad), grad )
      deallocate( grad )
    end if
    if( with_flux ) then
      call write_h5( get_gr_name(gr_therx), get_ds_name(ds_flux), flux )
      deallocate( flux )
    end if
    if( with_internal ) then
      call write_h5( get_gr_name(gr_therx), get_ds_name(ds_internal), internal )
      deallocate( internal )
    end if

  end subroutine write_h5_data_therMAILx

!------------------------------------------------------------------------

  subroutine read_h5_data_poroMAILx(step)
    implicit none
    !> evolution id to read
    integer, intent(in) :: step
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: disp, dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_dofs, nb_gp, tmp
    integer :: ibdyty, iblmty, ibeg, iend, iend2, rigid_size
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    logical :: with_idata, with_disp, with_dofs
    logical :: with_grad , with_flux , with_internal

    type( T_poroMAILx ), dimension( : ), pointer :: bdyty

    character(len=32), parameter :: IAM = 'h5_mailx::read_h5_data_poroMAILx'
    character(len=80)            :: cout

    with_idata    = .false.
    with_disp     = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    idata    => null()
    disp     => null()
    dofs     => null()
    grad     => null()
    flux     => null()
    internal => null()

    if( associated( fmt_porox%idata ) ) then
      call read_h5( get_gr_name(gr_porox, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( associated( fmt_porox%disp_field ) ) then
      call read_h5( get_gr_name(gr_porox, step), get_ds_name(ds_disp), disp )
      with_disp = .true.
    end if

    if( associated( fmt_porox%dofs_field ) ) then
      call read_h5( get_gr_name(gr_porox, step), get_ds_name(ds_dofs), dofs )
      with_dofs = .true.
    end if

    if( associated( fmt_porox%grad ) ) then
      call read_h5( get_gr_name(gr_porox, step), get_ds_name(ds_grad), grad )
      with_grad = .true.
    end if

    if( associated( fmt_porox%flux ) ) then
      call read_h5( get_gr_name(gr_porox, step), get_ds_name(ds_flux), flux )
      with_flux = .true.
    end if

    if( associated( fmt_porox%internal ) ) then
      call read_h5( get_gr_name(gr_porox, step), get_ds_name(ds_internal), internal )
      with_internal = .true.
    end if

    ! not a very good check
    ! but no need to to better yet
    if ( .not. associated( idata ) ) return

    ! Get the structure to write into
    call get_bdyty_poroMAILx( bdyty )
    nb_bodies = get_nb_poroMAILx()

    ! Check the size of the data from HDF5 and the size of the structure to write into
    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
       call faterr(IAM,'Please read_bodies before trying to read dof')
    end if

    if ( size(bdyty) /= size(idata, 2)-1 ) then
       call faterr(IAM,'Inconsistent size between already allocated bdyty and data in file')
    end if

    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    ! From HDF5 buffers, fill the LMGC90's internal structures
    do ibdyty = 1, nb_bodies

      if( with_idata ) then

        ! check nodes number consitency between file and loaded body
        if( fmt_porox%node_idx > 0 ) then
          ibeg = fmt_porox%idata(fmt_porox%node_idx)%ibeg
          nb_nodes = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_nodes /= bdyty(ibdyty)%nb_nodes ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of nodes in poroMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nb_nodes, '/', nb_nodes, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! check dofs number consitency between file and loaded body
        if( fmt_porox%dofs_idx > 0 ) then
          ibeg = fmt_porox%idata(fmt_porox%dofs_idx)%ibeg
          nb_dofs = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_dofs /= bdyty(ibdyty)%nbdof ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of dofs in poroMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nbdof, '/', nb_dofs, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! not usefull to us so skipped
        ! but should count total number of gp for each
        ! mesh and confront it to stored value
        !if( fmt_porox%gps_idx  > 0 ) then
        !end if

      end if


      if( with_disp ) then

        if( fmt_porox%disp > 0 ) then

          ! always nbDIME
          !ibeg = fmt_porox%disp(fmt_porox%disp_field)%ibeg
          !iend = fmt_porox%disp(fmt_porox%disp_field)%iend

          ! can check size with idata content
          !ibeg = fmt_porox%idata(fmt_porox%node_idx)%ibeg
          !istart = idata( fmt_porox%idata( fmt_porox%nodes_idx )%ibeg, ibdyty   ) + 1
          !iend2  = idata( fmt_porox%idata( fmt_porox%nodes_idx )%ibeg, ibdyty+1 )
          ! or with loaded body
          nb_nodes = bdyty(ibdyty)%nb_nodes
          iend = i_node_offset + nb_nodes

          bdyty(ibdyty)%X(:)      = reshape( disp( 1:nbDIME, i_node_offset+1:iend ), shape=(/ nbDIME * nb_nodes /) )
          bdyty(ibdyty)%Xbegin(:) = bdyty(ibdyty)%X(:)

        end if

      end if

      if( with_dofs ) then

        if( fmt_porox%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_porox%dofs_field(fmt_porox%dofs)%ibeg
          !iend = fmt_porox%dofs_field(fmt_porox%dofs)%iend

          ! can check size with idata content
          !nb_dofs = idata( fmt_porox%idata(fmt_porox%dofs_idx)%ibeg, ibdyty+1) 
          !         -idata( fmt_porox%idata(fmt_porox%dofs_idx)%ibeg, ibdyty  )
          ! or with loaded body
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          bdyty(ibdyty)%V(:)      = dofs( 1, i_dof_offset+1:iend )
          bdyty(ibdyty)%Vbegin(:) = bdyty(ibdyty)%V(:)

        end if

      end if


      !> \todo replace 1, 2, 3, by field id like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_porox%strain > 0 ) then

          ibeg = fmt_porox%grad(fmt_porox%strain)%ibeg
          iend = fmt_porox%grad(fmt_porox%strain)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_poroMAILx( ibdyty, iblmty, 2, grad(ibeg:iend, tmp+1:iend2) ) ! strain
            tmp = i_gp_offset + nb_gp
          end do

        end if

        tmp = i_gp_offset
        if( fmt_porox%ther_grad > 0 ) then

          ibeg = fmt_porox%grad(fmt_porox%ther_grad)%ibeg
          iend = fmt_porox%grad(fmt_porox%ther_grad)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_poroMAILx( ibdyty, iblmty, 4, grad(ibeg:iend, tmp+1:iend2) ) ! ther_grad
            tmp = i_gp_offset + nb_gp
          end do

        end if

      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_porox%stress > 0 ) then

          ibeg = fmt_porox%flux(fmt_porox%stress)%ibeg
          iend = fmt_porox%flux(fmt_porox%stress)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_poroMAILx( ibdyty, iblmty, 1, flux(ibeg:iend, tmp+1:iend2) ) ! stress
            tmp = tmp + nb_gp
          end do

        end if

        tmp = i_gp_offset
        if( fmt_porox%ther_flux > 0 ) then

          ibeg = fmt_porox%flux(fmt_porox%ther_flux)%ibeg
          iend = fmt_porox%flux(fmt_porox%ther_flux)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_poroMAILx( ibdyty, iblmty, 5, flux(ibeg:iend, tmp+1:iend2) ) ! ther_flux
            tmp = tmp + nb_gp
          end do

        end if

      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_porox%meca_internal > 0 ) then

          ibeg = fmt_porox%internal(fmt_porox%meca_internal)%ibeg
          iend = fmt_porox%internal(fmt_porox%meca_internal)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_poroMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) ) ! internal
            tmp = tmp + nb_gp
          end do

        end if

        tmp = i_gp_offset
        if( fmt_porox%ther_internal > 0 ) then

          ibeg = fmt_porox%internal(fmt_porox%ther_internal)%ibeg
          iend = fmt_porox%internal(fmt_porox%ther_internal)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_poroMAILx( ibdyty, iblmty, 6, internal(ibeg:iend, tmp+1:iend2) ) ! internal
            tmp = tmp + nb_gp
          end do

        end if

      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_poroMAILx(ibdyty, iblmty)
      end do

    end do

    if( with_idata     ) deallocate( idata     )
    if( with_disp      ) deallocate( disp      )
    if( with_dofs      ) deallocate( dofs      )
    if( with_grad      ) deallocate( grad      )
    if( with_flux      ) deallocate( flux      )
    if( with_internal  ) deallocate( internal  )

  end subroutine read_h5_data_poroMAILx

!------------------------------------------------------------------------

  subroutine write_h5_data_poroMAILx()
    implicit none
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: disp, dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_gp, tmp
    integer :: total_nb_nodes, total_nb_dofs, total_nb_elems, total_nb_gps
    integer :: ibdyty, iblmty, rigid_size
    integer :: ibeg, iend, istart, iend2
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    type( T_poroMAILx ), dimension( : ), pointer :: bdyty

    logical :: with_idata, with_rdata, with_disp, with_dofs
    logical :: with_grad , with_flux , with_internal

    nb_bodies = get_nb_poroMAILx( )

    ! If no mesh is used: exit
    if ( nb_bodies <= 0 ) then
       return
    end if

    call get_bdyty_poroMAILx( bdyty )

    with_idata    = .false.
    with_disp     = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    total_nb_nodes = 0
    total_nb_dofs  = 0
    total_nb_elems = 0
    total_nb_gps   = 0

    ! counting
    do ibdyty = 1, nb_bodies
      total_nb_nodes = total_nb_nodes + bdyty(ibdyty)%nb_nodes
      total_nb_dofs  = total_nb_dofs  + bdyty(ibdyty)%nbdof
      total_nb_elems = total_nb_elems + get_nb_elements_poroMAILx(ibdyty)
      do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
        total_nb_gps  = total_nb_gps  + get_nb_gp_poroMAILx(ibdyty, iblmty)
      end do
    end do

    
    ! arrays indexed by body number

    if( associated( fmt_porox%idata ) ) then
      ! start with 0 to get field boundaries
      allocate( idata( fmt_porox%idata_sz, 0:nb_bodies ) )
      with_idata = .true.
      idata(:,0) = 0
    end if

    ! array indexed by node index cumulated on all bodies

    if( associated( fmt_porox%disp_field ) ) then
      allocate( disp( fmt_porox%disp_field_sz, total_nb_nodes ) )
      with_disp = .true.
    end if

    ! array indexed by dofs index cumulated on all bodies

    if( associated( fmt_porox%dofs_field ) ) then
      allocate( dofs( fmt_porox%dofs_field_sz, total_nb_dofs ) )
      with_dofs = .true.
    end if


    ! arrays indexed by gauss point index cumulated on all bodies

    if( associated( fmt_porox%grad ) ) then
      allocate( grad( fmt_porox%grad_sz, total_nb_gps ) )
      with_grad = .true.
    end if

    if( associated( fmt_porox%flux ) ) then
      allocate( flux( fmt_porox%flux_sz, total_nb_gps ) )
      with_flux = .true.
    end if

    if( associated( fmt_porox%internal ) ) then
      allocate( internal( fmt_porox%internal_sz, total_nb_gps ) )
      with_internal = .true.
    end if


    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    do ibdyty = 1, nb_bodies

      if( with_idata ) then
   
        if( fmt_porox%node_idx > 0 ) then
          ibeg = fmt_porox%idata(fmt_porox%node_idx)%ibeg
          idata( ibeg, ibdyty) = i_node_offset + bdyty(ibdyty)%nb_nodes
        end if

        if( fmt_porox%dofs_idx > 0 ) then
          ibeg = fmt_porox%idata(fmt_porox%dofs_idx)%ibeg
          idata( ibeg, ibdyty) = i_dof_offset + bdyty(ibdyty)%nbdof
        end if

        if( fmt_porox%gps_idx  > 0 ) then
          ibeg = fmt_porox%idata(fmt_porox%gps_idx)%ibeg
          idata( ibeg, ibdyty) = i_gp_offset
          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
             idata(ibeg, ibdyty) = idata(ibeg, ibdyty) + get_nb_gp_poroMAILx(ibdyty, iblmty)
          end do
        end if

      end if

      if( with_disp ) then

        if( fmt_porox%disp > 0 ) then

          ! always nbDIME
          !ibeg = fmt_porox%disp(fmt_porox%disp_field)%ibeg
          !iend = fmt_porox%disp(fmt_porox%disp_field)%iend

          !istart = idata( fmt_porox%idata( fmt_porox%nodes_idx )%ibeg, ibdyty-1 ) + 1
          !iend2  = idata( fmt_porox%idata( fmt_porox%nodes_idx )%ibeg, ibdyty   )
          nb_nodes = bdyty(ibdyty)%nb_nodes
          iend = i_node_offset + nb_nodes

          disp( 1:nbDIME, i_node_offset+1:iend ) = reshape( bdyty(ibdyty)%X(:)    , shape=(/ nbDIME, nb_nodes /) )

        end if

      end if

      if( with_dofs ) then

        if( fmt_porox%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_porox%dofs_field(fmt_porox%dofs)%ibeg
          !iend = fmt_porox%dofs_field(fmt_porox%dofs)%iend
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          dofs( 1, i_dof_offset+1:iend ) = bdyty(ibdyty)%V(:)

        end if

      end if


      !> \todo replace 1, 2, 3, etc by field ids like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_porox%stress > 0 ) then

          ibeg = fmt_porox%grad(fmt_porox%stress)%ibeg
          iend = fmt_porox%grad(fmt_porox%stress)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_poroMAILx( ibdyty, iblmty, 1, grad(ibeg:iend, tmp+1:iend2) ) ! stress
            tmp = i_gp_offset + nb_gp
          end do

        end if

        tmp = i_gp_offset
        if( fmt_porox%ther_grad > 0 ) then

          ibeg = fmt_porox%grad(fmt_porox%ther_grad)%ibeg
          iend = fmt_porox%grad(fmt_porox%ther_grad)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_poroMAILx( ibdyty, iblmty, 4, grad(ibeg:iend, tmp+1:iend2) ) ! thermal gradient
            tmp = i_gp_offset + nb_gp
          end do

        end if

      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_porox%strain > 0 ) then

          ibeg = fmt_porox%flux(fmt_porox%strain)%ibeg
          iend = fmt_porox%flux(fmt_porox%strain)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_poroMAILx( ibdyty, iblmty, 2, flux(ibeg:iend, tmp+1:iend2) ) ! strain
            tmp = tmp + nb_gp
          end do

        end if

        tmp = i_gp_offset
        if( fmt_porox%ther_flux > 0 ) then

          ibeg = fmt_porox%flux(fmt_porox%ther_flux)%ibeg
          iend = fmt_porox%flux(fmt_porox%ther_flux)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_poroMAILx( ibdyty, iblmty, 5, flux(ibeg:iend, tmp+1:iend2) ) ! thermal flux
            tmp = tmp + nb_gp
          end do

        end if
      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_porox%meca_internal > 0 ) then

          ibeg = fmt_porox%internal(fmt_porox%meca_internal)%ibeg
          iend = fmt_porox%internal(fmt_porox%meca_internal)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_poroMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) ) ! meca internal
            tmp = tmp + nb_gp
          end do

        end if

        tmp = i_gp_offset
        if( fmt_porox%ther_internal > 0 ) then

          ibeg = fmt_porox%internal(fmt_porox%ther_internal)%ibeg
          iend = fmt_porox%internal(fmt_porox%ther_internal)%iend

          do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
            nb_gp = get_nb_gp_poroMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_poroMAILx( ibdyty, iblmty, 6, internal(ibeg:iend, tmp+1:iend2) ) ! internal
            tmp = tmp + nb_gp
          end do

        end if

      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_poroMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_poroMAILx(ibdyty, iblmty)
      end do

    end do

    ! The HDF5 part
    if( with_idata ) then
      call write_h5( get_gr_name(gr_porox), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if
    if( with_disp ) then
      call write_h5( get_gr_name(gr_porox), get_ds_name(ds_disp), disp )
      deallocate( disp )
    end if
    if( with_dofs ) then
      call write_h5( get_gr_name(gr_porox), get_ds_name(ds_dofs), dofs )
      deallocate( dofs )
    end if
    if( with_grad ) then
      call write_h5( get_gr_name(gr_porox), get_ds_name(ds_grad), grad )
      deallocate( grad )
    end if
    if( with_flux ) then
      call write_h5( get_gr_name(gr_porox), get_ds_name(ds_flux), flux )
      deallocate( flux )
    end if
    if( with_internal ) then
      call write_h5( get_gr_name(gr_porox), get_ds_name(ds_internal), internal )
      deallocate( internal )
    end if

  end subroutine write_h5_data_poroMAILx

!------------------------------------------------------------------------

  subroutine read_h5_data_multiMAILx(step)
    implicit none
    !> evolution id to read
    integer, intent(in) :: step
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: disp, dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_dofs, nb_gp, tmp
    integer :: ibdyty, iblmty, ibeg, iend, iend2, rigid_size
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    logical :: with_idata, with_disp, with_dofs
    logical :: with_grad , with_flux , with_internal

    type( T_multiMAILx ), dimension( : ), pointer :: bdyty

    character(len=32), parameter :: IAM = 'h5_mailx::read_h5_data_multiMAILx'
    character(len=80)            :: cout

    with_idata    = .false.
    with_disp     = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    idata    => null()
    disp     => null()
    dofs     => null()
    grad     => null()
    flux     => null()
    internal => null()

    if( associated( fmt_multi%idata ) ) then
      call read_h5( get_gr_name(gr_multi, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( associated( fmt_multi%disp_field ) ) then
      call read_h5( get_gr_name(gr_multi, step), get_ds_name(ds_disp), disp )
      with_disp = .true.
    end if

    if( associated( fmt_multi%dofs_field ) ) then
      call read_h5( get_gr_name(gr_multi, step), get_ds_name(ds_dofs), dofs )
      with_dofs = .true.
    end if

    if( associated( fmt_multi%grad ) ) then
      call read_h5( get_gr_name(gr_multi, step), get_ds_name(ds_grad), grad )
      with_grad = .true.
    end if

    if( associated( fmt_multi%flux ) ) then
      call read_h5( get_gr_name(gr_multi, step), get_ds_name(ds_flux), flux )
      with_flux = .true.
    end if

    if( associated( fmt_multi%internal ) ) then
      call read_h5( get_gr_name(gr_multi, step), get_ds_name(ds_internal), internal )
      with_internal = .true.
    end if

    ! not a very good check
    ! but no need to to better yet
    if ( .not. associated( idata ) ) return

    ! Get the structure to write into
    call get_bdyty_multiMAILx( bdyty )
    nb_bodies = get_nb_multiMAILx()

    ! Check the size of the data from HDF5 and the size of the structure to write into
    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
       call faterr(IAM,'Please read_bodies before trying to read dof')
    end if

    if ( size(bdyty) /= size(idata, 2)-1 ) then
       call faterr(IAM,'Inconsistent size between already allocated bdyty and data in file')
    end if

    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    ! From HDF5 buffers, fill the LMGC90's internal structures
    do ibdyty = 1, nb_bodies

      if( with_idata ) then

        ! check nodes number consitency between file and loaded body
        if( fmt_multi%node_idx > 0 ) then
          ibeg = fmt_multi%idata(fmt_multi%node_idx)%ibeg
          nb_nodes = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_nodes /= bdyty(ibdyty)%nb_nodes ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of nodes in multiMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nb_nodes, '/', nb_nodes, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! check dofs number consitency between file and loaded body
        if( fmt_multi%dofs_idx > 0 ) then
          ibeg = fmt_multi%idata(fmt_multi%dofs_idx)%ibeg
          nb_dofs = idata(ibeg, ibdyty+1) - idata(ibeg,ibdyty)
          if( nb_dofs /= bdyty(ibdyty)%nbdof ) then
            write(cout,'(A,I0,1x,A1,I0,A1,I0,A1)') 'Wrong number of dofs in multiMAILx ', ibdyty, &
                                                   '(', bdyty(ibdyty)%nbdof, '/', nb_dofs, ')'
            call faterr(IAM, cout)
          end if
        end if

        ! not usefull to us so skipped
        ! but should count total number of gp for each
        ! mesh and confront it to stored value
        !if( fmt_multi%gps_idx  > 0 ) then
        !end if

      end if


      if( with_disp ) then

        if( fmt_multi%disp > 0 ) then

          ! always nbDIME
          !ibeg = fmt_multi%disp(fmt_multi%disp_field)%ibeg
          !iend = fmt_multi%disp(fmt_multi%disp_field)%iend

          ! can check size with idata content
          !ibeg = fmt_multi%idata(fmt_multi%node_idx)%ibeg
          !istart = idata( fmt_multi%idata( fmt_multi%nodes_idx )%ibeg, ibdyty   ) + 1
          !iend2  = idata( fmt_multi%idata( fmt_multi%nodes_idx )%ibeg, ibdyty+1 )
          ! or with loaded body
          nb_nodes = bdyty(ibdyty)%nb_nodes
          iend = i_node_offset + nb_nodes

          bdyty(ibdyty)%X(:,:,1) = disp( 1:nbDIME, i_node_offset+1:iend )
          bdyty(ibdyty)%X(:,:,2) = bdyty(ibdyty)%X(:,:,1)

        end if

      end if

      if( with_dofs ) then

        if( fmt_multi%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_multi%dofs_field(fmt_multi%dofs)%ibeg
          !iend = fmt_multi%dofs_field(fmt_multi%dofs)%iend

          ! can check size with idata content
          !nb_dofs = idata( fmt_multi%idata(fmt_multi%dofs_idx)%ibeg, ibdyty+1) 
          !         -idata( fmt_multi%idata(fmt_multi%dofs_idx)%ibeg, ibdyty  )
          ! or with loaded body
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          bdyty(ibdyty)%Dofs(:,1) = dofs( 1, i_dof_offset+1:iend )
          bdyty(ibdyty)%Dofs(:,2) = bdyty(ibdyty)%Dofs(:,1)

        end if

      end if


      !> \todo replace 1, 2, 3, by field id like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_multi%multi_grad > 0 ) then

          ibeg = fmt_multi%grad(fmt_multi%multi_grad)%ibeg
          iend = fmt_multi%grad(fmt_multi%multi_grad)%iend

          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
            nb_gp = get_nb_gp_multiMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_multiMAILx( ibdyty, iblmty, 1, grad(ibeg:iend, tmp+1:iend2) ) ! multi_grad
            tmp = i_gp_offset + nb_gp
          end do

        end if

      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_multi%multi_flux > 0 ) then

          ibeg = fmt_multi%flux(fmt_multi%multi_flux)%ibeg
          iend = fmt_multi%flux(fmt_multi%multi_flux)%iend

          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
            nb_gp = get_nb_gp_multiMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_multiMAILx( ibdyty, iblmty, 2, flux(ibeg:iend, tmp+1:iend2) ) ! multi_flux
            tmp = tmp + nb_gp
          end do

        end if

      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_multi%multi_internal > 0 ) then

          ibeg = fmt_multi%internal(fmt_multi%multi_internal)%ibeg
          iend = fmt_multi%internal(fmt_multi%multi_internal)%iend

          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
            nb_gp = get_nb_gp_multiMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call set_field_multiMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) ) ! internal
            tmp = tmp + nb_gp
          end do

        end if

      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_multiMAILx(ibdyty, iblmty)
      end do

    end do

    if( with_idata     ) deallocate( idata     )
    if( with_disp      ) deallocate( disp      )
    if( with_dofs      ) deallocate( dofs      )
    if( with_grad      ) deallocate( grad      )
    if( with_flux      ) deallocate( flux      )
    if( with_internal  ) deallocate( internal  )

  end subroutine read_h5_data_multiMAILx

!------------------------------------------------------------------------

  subroutine write_h5_data_multiMAILx()
    implicit none
    ! Local variables
    ! HDF5 Buffer: present the data to be saved in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: disp, dofs, grad, flux, internal
    integer     , dimension(:,:), pointer :: idata
    !
    integer :: nb_bodies, nb_nodes, nb_gp, tmp
    integer :: total_nb_nodes, total_nb_dofs, total_nb_elems, total_nb_gps
    integer :: ibdyty, iblmty, rigid_size
    integer :: ibeg, iend, istart, iend2
    integer :: i_node_offset, i_dof_offset, i_gp_offset

    type( T_multiMAILx ), dimension( : ), pointer :: bdyty

    logical :: with_idata, with_rdata, with_disp, with_dofs
    logical :: with_grad , with_flux , with_internal

    nb_bodies = get_nb_multiMAILx( )

    ! If no mesh is used: exit
    if ( nb_bodies <= 0 ) then
       return
    end if

    call get_bdyty_multiMAILx( bdyty )

    with_idata    = .false.
    with_disp     = .false.
    with_dofs     = .false.
    with_grad     = .false.
    with_flux     = .false.
    with_internal = .false.

    total_nb_nodes = 0
    total_nb_dofs  = 0
    total_nb_elems = 0
    total_nb_gps   = 0

    ! counting
    do ibdyty = 1, nb_bodies
      total_nb_nodes = total_nb_nodes + bdyty(ibdyty)%nb_nodes
      total_nb_dofs  = total_nb_dofs  + bdyty(ibdyty)%nbdof
      total_nb_elems = total_nb_elems + get_nb_elements_multiMAILx(ibdyty)
      do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
        total_nb_gps  = total_nb_gps  + get_nb_gp_multiMAILx(ibdyty, iblmty)
      end do
    end do

    
    ! arrays indexed by body number

    if( associated( fmt_multi%idata ) ) then
      ! start with 0 to get field boundaries
      allocate( idata( fmt_multi%idata_sz, 0:nb_bodies ) )
      with_idata = .true.
      idata(:,0) = 0
    end if

    ! array indexed by node index cumulated on all bodies

    if( associated( fmt_multi%disp_field ) ) then
      allocate( disp( fmt_multi%disp_field_sz, total_nb_nodes ) )
      with_disp = .true.
    end if

    ! array indexed by dofs index cumulated on all bodies

    if( associated( fmt_multi%dofs_field ) ) then
      allocate( dofs( fmt_multi%dofs_field_sz, total_nb_dofs ) )
      with_dofs = .true.
    end if


    ! arrays indexed by gauss point index cumulated on all bodies

    if( associated( fmt_multi%grad ) ) then
      allocate( grad( fmt_multi%grad_sz, total_nb_gps ) )
      with_grad = .true.
    end if

    if( associated( fmt_multi%flux ) ) then
      allocate( flux( fmt_multi%flux_sz, total_nb_gps ) )
      with_flux = .true.
    end if

    if( associated( fmt_multi%internal ) ) then
      allocate( internal( fmt_multi%internal_sz, total_nb_gps ) )
      with_internal = .true.
    end if


    ! store offsetting
    i_node_offset = 0
    i_dof_offset  = 0
    i_gp_offset   = 0

    do ibdyty = 1, nb_bodies

      if( with_idata ) then
   
        if( fmt_multi%node_idx > 0 ) then
          ibeg = fmt_multi%idata(fmt_multi%node_idx)%ibeg
          idata( ibeg, ibdyty) = i_node_offset + bdyty(ibdyty)%nb_nodes
        end if

        if( fmt_multi%dofs_idx > 0 ) then
          ibeg = fmt_multi%idata(fmt_multi%dofs_idx)%ibeg
          idata( ibeg, ibdyty) = i_dof_offset + bdyty(ibdyty)%nbdof
        end if

        if( fmt_multi%gps_idx  > 0 ) then
          ibeg = fmt_multi%idata(fmt_multi%gps_idx)%ibeg
          idata( ibeg, ibdyty) = i_gp_offset
          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
             idata(ibeg, ibdyty) = idata(ibeg, ibdyty) + get_nb_gp_multiMAILx(ibdyty, iblmty)
          end do
        end if

      end if

      if( with_disp ) then

        if( fmt_multi%disp > 0 ) then

          ! always nbDIME
          !ibeg = fmt_multi%disp(fmt_multi%disp_field)%ibeg
          !iend = fmt_multi%disp(fmt_multi%disp_field)%iend

          !istart = idata( fmt_multi%idata( fmt_multi%nodes_idx )%ibeg, ibdyty-1 ) + 1
          !iend2  = idata( fmt_multi%idata( fmt_multi%nodes_idx )%ibeg, ibdyty   )
          nb_nodes = bdyty(ibdyty)%nb_nodes
          iend = i_node_offset + nb_nodes

          disp( 1:nbDIME, i_node_offset+1:iend ) = bdyty(ibdyty)%X(1:nbDIME,1:nb_nodes,2)

        end if

      end if

      if( with_dofs ) then

        if( fmt_multi%dofs > 0 ) then

          ! always 1
          !ibeg = fmt_multi%dofs_field(fmt_multi%dofs)%ibeg
          !iend = fmt_multi%dofs_field(fmt_multi%dofs)%iend
          iend = i_dof_offset + bdyty(ibdyty)%nbdof
          dofs( 1, i_dof_offset+1:iend ) = bdyty(ibdyty)%Dofs(:,2)

        end if

      end if


      !> \todo replace 1, 2, 3, etc by field ids like i_stress, i_strain...

      if( with_grad ) then

        tmp = i_gp_offset
        if( fmt_multi%multi_grad > 0 ) then

          ibeg = fmt_multi%grad(fmt_multi%multi_grad)%ibeg
          iend = fmt_multi%grad(fmt_multi%multi_grad)%iend

          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
            nb_gp = get_nb_gp_multiMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_multiMAILx( ibdyty, iblmty, 1, grad(ibeg:iend, tmp+1:iend2) ) ! thermal gradient
            tmp = i_gp_offset + nb_gp
          end do

        end if

      end if

      if( with_flux ) then

        tmp = i_gp_offset
        if( fmt_multi%multi_flux > 0 ) then

          ibeg = fmt_multi%flux(fmt_multi%multi_flux)%ibeg
          iend = fmt_multi%flux(fmt_multi%multi_flux)%iend

          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
            nb_gp = get_nb_gp_multiMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_multiMAILx( ibdyty, iblmty, 2, flux(ibeg:iend, tmp+1:iend2) ) ! thermal flux
            tmp = tmp + nb_gp
          end do

        end if
      end if

      if( with_internal ) then

        tmp = i_gp_offset
        if( fmt_multi%multi_internal > 0 ) then

          ibeg = fmt_multi%internal(fmt_multi%multi_internal)%ibeg
          iend = fmt_multi%internal(fmt_multi%multi_internal)%iend

          do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
            nb_gp = get_nb_gp_multiMAILx(ibdyty, iblmty)
            if( nb_gp == 0 ) cycle
            iend2 = tmp + nb_gp
            call get_field_multiMAILx( ibdyty, iblmty, 3, internal(ibeg:iend, tmp+1:iend2) ) ! meca internal
            tmp = tmp + nb_gp
          end do

        end if

      end if

      i_node_offset = i_node_offset + bdyty(ibdyty)%nb_nodes
      i_dof_offset  = i_dof_offset  + bdyty(ibdyty)%nbdof
      do iblmty = 1, get_nb_elements_multiMAILx(ibdyty)
        i_gp_offset = i_gp_offset + get_nb_gp_multiMAILx(ibdyty, iblmty)
      end do

    end do

    ! The HDF5 part
    if( with_idata ) then
      call write_h5( get_gr_name(gr_multi), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if
    if( with_disp ) then
      call write_h5( get_gr_name(gr_multi), get_ds_name(ds_disp), disp )
      deallocate( disp )
    end if
    if( with_dofs ) then
      call write_h5( get_gr_name(gr_multi), get_ds_name(ds_dofs), dofs )
      deallocate( dofs )
    end if
    if( with_grad ) then
      call write_h5( get_gr_name(gr_multi), get_ds_name(ds_grad), grad )
      deallocate( grad )
    end if
    if( with_flux ) then
      call write_h5( get_gr_name(gr_multi), get_ds_name(ds_flux), flux )
      deallocate( flux )
    end if
    if( with_internal ) then
      call write_h5( get_gr_name(gr_multi), get_ds_name(ds_internal), internal )
      deallocate( internal )
    end if

  end subroutine write_h5_data_multiMAILx

!------------------------------------------------------------------------

end module h5_mailx
