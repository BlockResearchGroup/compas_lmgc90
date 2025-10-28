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

!> Define the interaction hdf5 subroutine
module h5_interaction

  use overall, only : faterr, logmes   , &
                      H                , &
                      smooth_method    , &
                      get_need_internal_tact

  use parameters, only : i_xksid,              &
                         i_dkjcx, i_dkdkx    , &
                         i_dkdkl, i_dkkdx    , &
                         i_p2p2l, i_dkalp    , &
                                  i_plalp    , &
                         i_pljcx, i_dkplx    , &
                         i_plplx, i_ptpt2    , &
                         i_clalp, i_cljcx    , &
                         i_prplx, i_prprx    , &
                         i_csprx, i_cdcdx    , &
                         i_cdplx, i_csasp    , &
                         i_prasp, i_ptpt3    , &
                         i_spcdx, i_spdcx    , &
                         i_spplx, i_spspx    , &
                         i_spprx, i_rbdy3,     &
                         nb_interaction_types, &
                         get_interaction_name_from_id

  use RBDY3, only : get_visibleID

  use inter_meca_handler_2D, only : get_nb_inters_2D => get_nb_inters, &
                                    set_nb_inters_2D => set_nb_inters, &
                                    get_ptr_one_2D   => get_ptr_one  , &
                                    redo_nb_adj_2D   => redo_nb_adj  , &
                                    stock_rloc_2D    => stock_rloc

  use inter_meca_handler_3D, only : get_nb_inters_3D => get_nb_inters, &
                                    set_nb_inters_3D => set_nb_inters, &
                                    get_ptr_one_3D   => get_ptr_one  , &
                                    redo_nb_adj_3D   => redo_nb_adj  , &
                                    stock_rloc_3D    => stock_rloc

  use PRPRx, only : verlet_from_file

  use inter_meca_2D, only : T_interaction_2D => T_interaction
  use inter_meca_3D, only : T_interaction_3D => T_interaction

  use h5_format, only : get_gr_name, &
                        get_ds_name, &
                        gr_vlocrloc, &
                        ds_idata   , &
                        ds_rdata   , &
                        fmt_inter

  use lmgc90_hdf5, only : read_h5, write_h5, ugly_fix


  implicit none

  private

  integer, parameter :: nb_mod_contact_2D = 13
  integer, dimension(nb_mod_contact_2D), parameter :: tab_id_inter_2D = (/ i_dkjcx, i_dkdkx, &
                                                                           i_dkdkl, i_dkkdx, &
                                                                           i_p2p2l, i_dkalp, &
                                                                                    i_plalp, &
                                                                           i_pljcx, i_dkplx, &
                                                                           i_plplx, i_ptpt2, &
                                                                           i_clalp, i_cljcx   /)


  integer, parameter :: nb_mod_contact_3D = 13
  integer, dimension(nb_mod_contact_3D), parameter :: tab_id_inter_3D = (/ i_prplx, i_prprx, &
                                                                           i_csprx, i_cdcdx, &
                                                                           i_cdplx, i_csasp, &
                                                                           i_prasp, i_ptpt3, &
                                                                           i_spcdx, i_spdcx, &
                                                                           i_spplx, i_spprx, i_spspx  /)

  public  read_h5_Vloc_Rloc_2D, &
         write_h5_Vloc_Rloc_2D, &
          read_h5_Vloc_Rloc_3D, &
         write_h5_Vloc_Rloc_3D

contains

!------------------------------------------------------------------------

  !> \brief Read VlocRloc information from hdf5 file
  subroutine read_h5_Vloc_Rloc_2D(step)
    !> evolution id to read
    integer, intent(in) :: step
    !
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    !
    integer, dimension(nb_interaction_types) :: tab_nb_inter
    integer :: nb_inter, id_inter
    integer :: i, inter_iidx, icdan
    !
    type(T_interaction_2D), pointer :: this

    integer :: ibeg, iend, need_internals
    logical :: with_idata, with_rdata

    rdata => null()
    idata => null()

    with_rdata = .false.
    with_idata = .false.

    id_inter = 0
    icdan    = 0

    ! The HDF5 part
    if( associated( fmt_inter%rdata ) ) then
      call read_h5( get_gr_name(gr_vlocrloc, step), get_ds_name(ds_rdata), rdata )
      with_rdata = .true.
    end if

    if( associated( fmt_inter%idata ) ) then
      call read_h5( get_gr_name(gr_vlocrloc, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( .not. with_idata .or. .not. associated( idata ) ) return

    ! so beurgly...
    call apply_ugly_fixer_( idata )

    ! counting number of interactions:
    tab_nb_inter(:) = 0
    do inter_iidx = 1, size(idata, 2)
      id_inter = idata( fmt_inter%idata(fmt_inter%inter_id)%ibeg, inter_iidx )
      tab_nb_inter( id_inter ) = tab_nb_inter( id_inter ) + 1
    end do

    ! sizing all this array
    do i = 1, nb_mod_contact_2D
      id_inter = tab_id_inter_2D(i)
      call set_nb_inters_2D( id_inter, tab_nb_inter( id_inter) )
    end do

    need_internals = get_need_internal_tact()

    ! filling this array
    do inter_iidx = 1, size(idata, 2)

      if( with_idata ) then

        if( fmt_inter%inter_id > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%inter_id)%ibeg
          id_inter = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%icdan > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%icdan)%ibeg
          icdan = idata( ibeg , inter_iidx )
        end if

        this => get_ptr_one_2D(id_inter, icdan)

        if( fmt_inter%ent > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%ent)%ibeg
          iend = fmt_inter%idata(fmt_inter%ent)%iend
          this%icdent = idata( ibeg , inter_iidx )
          this%ianent = idata( iend , inter_iidx )
        end if

        if( fmt_inter%bdyty > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%bdyty)%ibeg
          iend = fmt_inter%idata(fmt_inter%bdyty)%iend
          this%icdbtyp = idata( ibeg , inter_iidx )
          this%ianbtyp = idata( iend , inter_iidx )
        end if

        if( fmt_inter%ibdyty > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%ibdyty)%ibeg
          iend = fmt_inter%idata(fmt_inter%ibdyty)%iend
          this%icdbdy = idata( ibeg , inter_iidx )
          this%ianbdy = idata( iend , inter_iidx )
        end if

        if( fmt_inter%tactype > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%tactype)%ibeg
          iend = fmt_inter%idata(fmt_inter%tactype)%iend
          this%icdctyp = idata( ibeg , inter_iidx )
          this%ianctyp = idata( iend , inter_iidx )
        end if

        if( fmt_inter%itacty > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%itacty)%ibeg
          iend = fmt_inter%idata(fmt_inter%itacty)%iend
          this%icdtac = idata( ibeg , inter_iidx )
          this%iantac = idata( iend , inter_iidx )
        end if

        if( fmt_inter%itacbdy > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%itacbdy)%ibeg
          iend = fmt_inter%idata(fmt_inter%itacbdy)%iend
          this%icdbtac = idata( ibeg , inter_iidx )
          this%ianbtac = idata( iend , inter_iidx )
        end if

        if( fmt_inter%iadj > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%iadj)%ibeg
          this%iadj = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%isee > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%isee)%ibeg
          this%isee = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%lawnb > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%lawnb)%ibeg
          this%lawnb = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%i_law > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%i_law)%ibeg
          this%i_law = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%status > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%status)%ibeg
          this%status = idata( ibeg , inter_iidx )
        end if

        ! geo is the old icdver, ianal, ianseg in 2D
        if( fmt_inter%igeo > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%igeo)%ibeg
          iend = fmt_inter%idata(fmt_inter%igeo)%iend
          this%icdsci = idata( ibeg  , inter_iidx )
          if( id_inter == i_plplx ) then
              this%iansci = idata( iend, inter_iidx )
          else
              this%iansci = idata( ibeg+1, inter_iidx )
          end if
        end if
        if( fmt_inter%isci > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%isci)%ibeg
          iend = fmt_inter%idata(fmt_inter%isci)%iend
          this%icdsci = idata( ibeg, inter_iidx )
          this%iansci = idata( iend, inter_iidx )
        end if

        if( fmt_inter%nb_internal > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%nb_internal)%ibeg
          this%nb_internal = idata( ibeg , inter_iidx )
        end if

      end if

      if( with_rdata ) then

        if( fmt_inter%rl > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%rl)%ibeg
          iend = fmt_inter%rdata(fmt_inter%rl)%iend
          this%rlt = rdata( ibeg, inter_iidx ) * H
          this%rln = rdata( iend, inter_iidx ) * H
        end if

        if( fmt_inter%vl > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%vl)%ibeg
          iend = fmt_inter%rdata(fmt_inter%vl)%iend
          this%vlt = rdata( ibeg, inter_iidx )
          this%vln = rdata( iend, inter_iidx )
        end if

        if( fmt_inter%gapTT > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%gapTT)%ibeg
          this%gapTT = rdata( ibeg, inter_iidx )
        end if

        if( fmt_inter%coor > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%coor)%ibeg
          iend = fmt_inter%rdata(fmt_inter%coor)%iend
          this%coor(1:2) = rdata( ibeg : iend, inter_iidx )
        end if

        if( fmt_inter%uc > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%uc)%ibeg
          iend = fmt_inter%rdata(fmt_inter%uc)%iend
          this%nuc(1:2) = rdata( ibeg : iend, inter_iidx )
        end if

        if( fmt_inter%internals > 0 .and. need_internals > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%internals)%ibeg
          iend = fmt_inter%rdata(fmt_inter%internals)%iend
          this%internal(1:need_internals) = rdata( ibeg : iend, inter_iidx )
        end if

      end if

    end do

    ! redoing adjacent maps
    do i = 1, nb_mod_contact_2D
      id_inter = tab_id_inter_2D(i)
      call redo_nb_adj_2D(id_inter)
      call stock_rloc_2D(id_inter)
    end do

    if( with_rdata ) deallocate( rdata )
    if( with_idata ) deallocate( idata )

  end subroutine read_h5_Vloc_Rloc_2D

  !> Write the 2D interactions information in a HDF5 file
  subroutine write_h5_Vloc_Rloc_2D()
    implicit none
    ! HDF5 Buffer: present the data to save in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    ! Local variables
    integer      :: i, icdan

    integer, dimension( nb_mod_contact_2D ) :: tab_nb_inter
    integer :: nb_inter, id_inter
    integer :: inter_idx, inter_iidx
    integer :: inter_offset

    type(T_interaction_2D), pointer :: this

    integer :: ibeg, iend, need_internals
    logical :: with_idata, with_rdata

    do i = 1, nb_mod_contact_2D
      tab_nb_inter(i) = get_nb_inters_2D( tab_id_inter_2D(i) )
    end do

    ! If nobody has any interaction, exit
    nb_inter = sum( tab_nb_inter )
    if ( nb_inter == 0 ) then
       return
    end if

    !
    if ( smooth_method ) then
      call logmes('[WARNING::write_h5_Vloc_Rloc_2D] not implemented for smooth method', .true.)
      return
    end if

    with_rdata = .false.
    with_idata = .false.

    if( associated( fmt_inter%rdata ) ) then
      allocate( rdata( fmt_inter%rdata_sz, nb_inter ) )
      with_rdata = .true.
    end if

    if( associated( fmt_inter%rdata ) ) then
      allocate( idata( fmt_inter%idata_sz, nb_inter ) )
      with_idata = .true.
    end if


    ! offsetting indexing of interaction depending on type
    inter_offset = 1

    ! get size of internals to write
    need_internals = get_need_internal_tact()

    do inter_idx = lbound(tab_id_inter_2D, 1), ubound(tab_id_inter_2D, 1)

       if ( tab_nb_inter(inter_idx) == 0 ) then
          cycle
       end if

       ! Get the data from the contact_2D's module
       id_inter = tab_id_inter_2D(inter_idx)

       ! Fill HDF5 buffers
       inter_iidx = inter_offset

       do icdan = 1, tab_nb_inter(inter_idx)

          this => get_ptr_one_2D( id_inter, icdan )

          ! Save every integer value
          if( with_idata ) then

            if( fmt_inter%inter_id > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%inter_id)%ibeg
              idata( ibeg , inter_iidx ) = id_inter
            end if

            if( fmt_inter%icdan > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%icdan)%ibeg
              idata( ibeg , inter_iidx ) = icdan
            end if

            if( fmt_inter%ent > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%ent)%ibeg
              iend = fmt_inter%idata(fmt_inter%ent)%iend
              idata( ibeg , inter_iidx ) = this%icdent
              idata( iend , inter_iidx ) = this%ianent
            end if

            if( fmt_inter%bdyty > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%bdyty)%ibeg
              iend = fmt_inter%idata(fmt_inter%bdyty)%iend
              idata( ibeg , inter_iidx ) = this%icdbtyp
              idata( iend , inter_iidx ) = this%ianbtyp
            end if

            if( fmt_inter%ibdyty > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%ibdyty)%ibeg
              iend = fmt_inter%idata(fmt_inter%ibdyty)%iend
              idata( ibeg , inter_iidx ) = this%icdbdy
              idata( iend , inter_iidx ) = this%ianbdy
            end if

            if( fmt_inter%tactype > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%tactype)%ibeg
              iend = fmt_inter%idata(fmt_inter%tactype)%iend
              idata( ibeg , inter_iidx ) = this%icdctyp
              idata( iend , inter_iidx ) = this%ianctyp
            end if

            if( fmt_inter%itacty > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%itacty)%ibeg
              iend = fmt_inter%idata(fmt_inter%itacty)%iend
              idata( ibeg , inter_iidx ) = this%icdtac
              idata( iend , inter_iidx ) = this%iantac
            end if

            if( fmt_inter%itacbdy > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%itacbdy)%ibeg
              iend = fmt_inter%idata(fmt_inter%itacbdy)%iend
              idata( ibeg , inter_iidx ) = this%icdbtac
              idata( iend , inter_iidx ) = this%ianbtac
            end if

            if( fmt_inter%iadj > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%iadj)%ibeg
              idata( ibeg , inter_iidx ) = this%iadj
            end if

            if( fmt_inter%isee > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%isee)%ibeg
              idata( ibeg , inter_iidx ) = this%isee
            end if

            if( fmt_inter%lawnb > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%lawnb)%ibeg
              idata( ibeg , inter_iidx ) = this%lawnb
            end if

            if( fmt_inter%i_law > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%i_law)%ibeg
              idata( ibeg , inter_iidx ) = this%i_law
            end if

            if( fmt_inter%status > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%status)%ibeg
              idata( ibeg , inter_iidx ) = this%status
            end if

            if( fmt_inter%igeo > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%igeo)%ibeg
              iend = fmt_inter%idata(fmt_inter%igeo)%iend
              idata( ibeg  , inter_iidx ) = this%icdsci
              idata( ibeg+1, inter_iidx ) = this%iansci
              idata( iend  , inter_iidx ) = 0
            end if
            if( fmt_inter%isci > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%isci)%ibeg
              iend = fmt_inter%idata(fmt_inter%isci)%iend
              idata( ibeg, inter_iidx ) = this%icdsci
              idata( iend, inter_iidx ) = this%iansci
            end if

            if( fmt_inter%nb_internal > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%nb_internal)%ibeg
              idata( ibeg , inter_iidx ) = this%nb_internal
            end if

          end if

          ! Save value about interaction, mainly velocity and reaction

          if( with_rdata ) then

            if( fmt_inter%rl > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%rl)%ibeg
              iend = fmt_inter%rdata(fmt_inter%rl)%iend
              rdata( ibeg : iend, inter_iidx ) = (/ this%rlt/H, this%rln/H /)
            end if

            if( fmt_inter%vl > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%vl)%ibeg
              iend = fmt_inter%rdata(fmt_inter%vl)%iend
              rdata( ibeg : iend, inter_iidx ) = (/ this%vlt, this%vln /)
            end if

            if( fmt_inter%gapTT > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%gapTT)%ibeg
              rdata( ibeg, inter_iidx ) = this%gapTT
            end if

            if( fmt_inter%coor > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%coor)%ibeg
              iend = fmt_inter%rdata(fmt_inter%coor)%iend
              rdata( ibeg : iend, inter_iidx ) = this%coor(1:2)
            end if

            if( fmt_inter%uc > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%uc)%ibeg
              iend = fmt_inter%rdata(fmt_inter%uc)%iend
              rdata( ibeg : iend, inter_iidx ) = this%nuc(1:2)
            end if

            if( fmt_inter%internals > 0 .and. need_internals > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%internals)%ibeg
              iend = fmt_inter%rdata(fmt_inter%internals)%iend
              rdata( ibeg : iend, inter_iidx ) = this%internal(1:need_internals)
            end if

          end if

          inter_iidx = inter_iidx + 1

       end do

       inter_offset = inter_offset + tab_nb_inter(inter_idx)

    end do

    ! The HDF5 part
    if( with_rdata ) then
      call write_h5( get_gr_name(gr_vlocrloc), get_ds_name(ds_rdata), rdata )
      deallocate( rdata )
    end if
    if( with_idata ) then
      call write_h5( get_gr_name(gr_vlocrloc), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if

  end subroutine write_h5_Vloc_Rloc_2D


  !> \brief Read VlocRloc information from hdf5 file
  subroutine read_h5_Vloc_Rloc_3D(step)
    !> evolution id to read
    integer, intent(in) :: step
    !
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    !
    integer, dimension(nb_interaction_types) :: tab_nb_inter
    integer :: nb_inter, id_inter
    integer :: i, inter_iidx, icdan
    !
    type(T_interaction_3D), pointer :: this

    integer :: ibeg, iend, need_internals
    logical :: with_idata, with_rdata

    rdata => null()
    idata => null()

    with_rdata = .false.
    with_idata = .false.

    id_inter = 0
    icdan    = 0

    ! The HDF5 part
    if( associated( fmt_inter%rdata ) ) then
      call read_h5( get_gr_name(gr_vlocrloc, step), get_ds_name(ds_rdata), rdata )
      with_rdata = .true.
    end if

    if( associated( fmt_inter%idata ) ) then
      call read_h5( get_gr_name(gr_vlocrloc, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( .not. with_idata .or. .not. associated( idata ) ) return

    ! so beurgly...
    call apply_ugly_fixer_( idata )

    ! counting number of interactions:
    tab_nb_inter(:) = 0
    do inter_iidx = 1, size(idata, 2)
      id_inter = idata( fmt_inter%idata(fmt_inter%inter_id)%ibeg, inter_iidx )
      tab_nb_inter( id_inter ) = tab_nb_inter( id_inter ) + 1
    end do

    ! sizing all this array
    do i = 1, nb_mod_contact_3D
      id_inter = tab_id_inter_3D(i)
      call set_nb_inters_3D( id_inter, tab_nb_inter( id_inter) )
    end do

    need_internals = get_need_internal_tact()

    ! filling this array
    do inter_iidx = 1, size(idata, 2)

      if( with_idata ) then

        if( fmt_inter%inter_id > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%inter_id)%ibeg
          id_inter = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%icdan > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%icdan)%ibeg
          icdan = idata( ibeg , inter_iidx )
        end if

        this => get_ptr_one_3D(id_inter, icdan)

        if( fmt_inter%ent > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%ent)%ibeg
          iend = fmt_inter%idata(fmt_inter%ent)%iend
          this%icdent = idata( ibeg , inter_iidx )
          this%ianent = idata( iend , inter_iidx )
        end if

        if( fmt_inter%bdyty > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%bdyty)%ibeg
          iend = fmt_inter%idata(fmt_inter%bdyty)%iend
          this%icdbtyp = idata( ibeg , inter_iidx )
          this%ianbtyp = idata( iend , inter_iidx )
        end if

        if( fmt_inter%ibdyty > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%ibdyty)%ibeg
          iend = fmt_inter%idata(fmt_inter%ibdyty)%iend
          this%icdbdy = idata( ibeg , inter_iidx )
          this%ianbdy = idata( iend , inter_iidx )
        end if

        if( fmt_inter%tactype > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%tactype)%ibeg
          iend = fmt_inter%idata(fmt_inter%tactype)%iend
          this%icdctyp = idata( ibeg , inter_iidx )
          this%ianctyp = idata( iend , inter_iidx )
        end if

        if( fmt_inter%itacty > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%itacty)%ibeg
          iend = fmt_inter%idata(fmt_inter%itacty)%iend
          this%icdtac = idata( ibeg , inter_iidx )
          this%iantac = idata( iend , inter_iidx )
        end if

        if( fmt_inter%itacbdy > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%itacbdy)%ibeg
          iend = fmt_inter%idata(fmt_inter%itacbdy)%iend
          this%icdbtac = idata( ibeg , inter_iidx )
          this%ianbtac = idata( iend , inter_iidx )
        end if

        if( fmt_inter%iadj > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%iadj)%ibeg
          this%iadj = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%isee > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%isee)%ibeg
          this%isee = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%lawnb > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%lawnb)%ibeg
          this%lawnb = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%i_law > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%i_law)%ibeg
          this%i_law = idata( ibeg , inter_iidx )
        end if

        if( fmt_inter%status > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%status)%ibeg
          this%status = idata( ibeg , inter_iidx )
        end if

        ! geo is the old icdver, ianal, ianseg... thus in 3D
        if( fmt_inter%igeo > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%igeo)%ibeg
          iend = fmt_inter%idata(fmt_inter%igeo)%iend
          this%icdsci = idata( ibeg , inter_iidx )
          this%iansci = 0
        end if
        if( fmt_inter%isci > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%isci)%ibeg
          iend = fmt_inter%idata(fmt_inter%isci)%iend
          this%icdsci = idata( ibeg , inter_iidx )
          this%iansci = idata( iend , inter_iidx )
        end if

        if( fmt_inter%nb_internal > 0 ) then
          ibeg = fmt_inter%idata(fmt_inter%nb_internal)%ibeg
          this%nb_internal = idata( ibeg , inter_iidx )
        end if

      end if

      if( with_rdata ) then

        if( fmt_inter%rl > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%rl)%ibeg
          iend = fmt_inter%rdata(fmt_inter%rl)%iend
          this%rlt = rdata( ibeg  , inter_iidx ) * H
          this%rln = rdata( ibeg+1, inter_iidx ) * H
          this%rls = rdata( iend  , inter_iidx ) * H
        end if

        if( fmt_inter%vl > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%vl)%ibeg
          iend = fmt_inter%rdata(fmt_inter%vl)%iend
          this%vlt = rdata( ibeg  , inter_iidx )
          this%vln = rdata( ibeg+1, inter_iidx )
          this%vls = rdata( iend  , inter_iidx )
        end if

        if( fmt_inter%gapTT > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%gapTT)%ibeg
          this%gapTT = rdata( ibeg, inter_iidx )
        end if

        if( fmt_inter%coor > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%coor)%ibeg
          iend = fmt_inter%rdata(fmt_inter%coor)%iend
          this%coor(1:3) = rdata( ibeg : iend, inter_iidx )
        end if

        if( fmt_inter%uc > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%uc)%ibeg
          iend = fmt_inter%rdata(fmt_inter%uc)%iend
          this%tuc(1:3) = rdata( ibeg  : ibeg+2, inter_iidx )
          this%nuc(1:3) = rdata( ibeg+3: ibeg+5,inter_iidx )
          this%suc(1:3) = rdata( ibeg+6: iend  , inter_iidx )
        end if

        if( fmt_inter%internals > 0 .and. need_internals > 0 ) then
          ibeg = fmt_inter%rdata(fmt_inter%internals)%ibeg
          iend = fmt_inter%rdata(fmt_inter%internals)%iend
          this%internal(1:need_internals) = rdata( ibeg : iend, inter_iidx )
        end if

      end if

    end do

    ! redoing adjacent maps
    do i = 1, nb_mod_contact_3D
      id_inter = tab_id_inter_3D(i)
      call redo_nb_adj_3D(id_inter)
      call stock_rloc_3D(id_inter)
    end do

    if( with_rdata ) deallocate( rdata )
    if( with_idata ) deallocate( idata )

    verlet_from_file = .TRUE.

  end subroutine read_h5_Vloc_Rloc_3D

  !> Write the 3D interactions information in a HDF5 file
  subroutine write_h5_Vloc_Rloc_3D()
    implicit none
    ! HDF5 Buffer: present the data to save in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    ! Local variables
    integer :: i, icdan
    integer :: icdbdy, ianbdy

    integer, dimension( nb_mod_contact_3D ) :: tab_nb_inter
    integer :: nb_inter, id_inter
    integer :: inter_idx, inter_iidx
    integer :: inter_offset

    type(T_interaction_3D), pointer :: this

    integer :: ibeg, iend, need_internals
    logical :: with_idata, with_rdata

    do i = 1, nb_mod_contact_3D
      tab_nb_inter(i) = get_nb_inters_3D( tab_id_inter_3D(i) )
    end do

    ! If nobody has any interaction, exit
    nb_inter = sum( tab_nb_inter )
    if ( nb_inter == 0 ) then
       return
    end if

    with_rdata = .false.
    with_idata = .false.

    if( associated( fmt_inter%rdata ) ) then
      allocate( rdata( fmt_inter%rdata_sz, nb_inter ) )
      with_rdata = .true.
    end if

    if( associated( fmt_inter%rdata ) ) then
      allocate( idata( fmt_inter%idata_sz, nb_inter ) )
      with_idata = .true.
    end if

    ! get size of internals to write
    need_internals = get_need_internal_tact()

    ! offsetting indexing of interaction depending on type
    inter_offset = 1

    do inter_idx = lbound(tab_id_inter_3D, 1), ubound(tab_id_inter_3D, 1)

       if ( tab_nb_inter(inter_idx) == 0 ) then
          cycle
       end if

       ! Get the data from the contact_3D's module
       id_inter = tab_id_inter_3D(inter_idx)

       ! Fill HDF5 buffers
       inter_iidx = inter_offset

       do icdan = 1, tab_nb_inter(inter_idx)

          this => get_ptr_one_3D( id_inter, icdan )

          ! Save every integer value
          if( with_idata ) then

            if( fmt_inter%inter_id > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%inter_id)%ibeg
              idata( ibeg , inter_iidx ) = id_inter
            end if

            if( fmt_inter%icdan > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%icdan)%ibeg
              idata( ibeg , inter_iidx ) = icdan
            end if

            if( fmt_inter%ent > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%ent)%ibeg
              iend = fmt_inter%idata(fmt_inter%ent)%iend
              idata( ibeg , inter_iidx ) = this%icdent
              idata( iend , inter_iidx ) = this%ianent
            end if

            if( fmt_inter%bdyty > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%bdyty)%ibeg
              iend = fmt_inter%idata(fmt_inter%bdyty)%iend
              idata( ibeg , inter_iidx ) = this%icdbtyp
              idata( iend , inter_iidx ) = this%ianbtyp
            end if

            if( this%icdbtyp == i_rbdy3 ) then
              icdbdy = get_visibleID(this%icdbdy)
            else
              icdbdy = this%icdbdy
            end if

            if( this%ianbtyp == i_rbdy3 ) then
              ianbdy = get_visibleID(this%ianbdy)
            else
              ianbdy = this%ianbdy
            end if

            if( fmt_inter%ibdyty > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%ibdyty)%ibeg
              iend = fmt_inter%idata(fmt_inter%ibdyty)%iend
              idata( ibeg , inter_iidx ) = icdbdy
              idata( iend , inter_iidx ) = ianbdy
            end if

            if( fmt_inter%tactype > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%tactype)%ibeg
              iend = fmt_inter%idata(fmt_inter%tactype)%iend
              idata( ibeg , inter_iidx ) = this%icdctyp
              idata( iend , inter_iidx ) = this%ianctyp
            end if

            if( fmt_inter%itacty > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%itacty)%ibeg
              iend = fmt_inter%idata(fmt_inter%itacty)%iend
              idata( ibeg , inter_iidx ) = this%icdtac
              idata( iend , inter_iidx ) = this%iantac
            end if

            if( fmt_inter%itacbdy > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%itacbdy)%ibeg
              iend = fmt_inter%idata(fmt_inter%itacbdy)%iend
              idata( ibeg , inter_iidx ) = this%icdbtac
              idata( iend , inter_iidx ) = this%ianbtac
            end if

            if( fmt_inter%iadj > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%iadj)%ibeg
              idata( ibeg , inter_iidx ) = this%iadj
            end if

            if( fmt_inter%isee > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%isee)%ibeg
              idata( ibeg , inter_iidx ) = this%isee
            end if

            if( fmt_inter%lawnb > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%lawnb)%ibeg
              idata( ibeg , inter_iidx ) = this%lawnb
            end if

            if( fmt_inter%i_law > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%i_law)%ibeg
              idata( ibeg , inter_iidx ) = this%i_law
            end if

            if( fmt_inter%status > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%status)%ibeg
              idata( ibeg , inter_iidx ) = this%status
            end if

            if( fmt_inter%igeo > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%igeo)%ibeg
              iend = fmt_inter%idata(fmt_inter%igeo)%iend
              idata( ibeg  , inter_iidx ) = this%icdsci
              idata( ibeg+1, inter_iidx ) = 0
              idata( iend  , inter_iidx ) = this%iansci
            end if
            if( fmt_inter%isci > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%isci)%ibeg
              iend = fmt_inter%idata(fmt_inter%isci)%iend
              idata( ibeg, inter_iidx ) = this%icdsci
              idata( iend, inter_iidx ) = this%iansci
            end if

            if( fmt_inter%nb_internal > 0 ) then
              ibeg = fmt_inter%idata(fmt_inter%nb_internal)%ibeg
              idata( ibeg , inter_iidx ) = this%nb_internal
            end if

          end if

          ! Save value about interaction, mainly velocity and reaction

          if( with_rdata ) then

            if( fmt_inter%rl > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%rl)%ibeg
              iend = fmt_inter%rdata(fmt_inter%rl)%iend
              rdata( ibeg : iend, inter_iidx ) = (/ this%rlt/H, this%rln/H, this%rls/H /)
            end if

            if( fmt_inter%vl > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%vl)%ibeg
              iend = fmt_inter%rdata(fmt_inter%vl)%iend
              rdata( ibeg : iend, inter_iidx ) = (/ this%vlt, this%vln, this%vls /)
            end if

            if( fmt_inter%gapTT > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%gapTT)%ibeg
              rdata( ibeg, inter_iidx ) = this%gapTT
            end if

            if( fmt_inter%coor > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%coor)%ibeg
              iend = fmt_inter%rdata(fmt_inter%coor)%iend
              rdata( ibeg : iend, inter_iidx ) = this%coor(1:3)
            end if

            if( fmt_inter%uc > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%uc)%ibeg
              iend = fmt_inter%rdata(fmt_inter%uc)%iend
              rdata( ibeg  : ibeg+2, inter_iidx ) = this%tuc(1:3)
              rdata( ibeg+3: ibeg+5, inter_iidx ) = this%nuc(1:3)
              rdata( ibeg+6: iend  , inter_iidx ) = this%suc(1:3)
            end if

            if( fmt_inter%internals > 0 .and. need_internals > 0 ) then
              ibeg = fmt_inter%rdata(fmt_inter%internals)%ibeg
              iend = fmt_inter%rdata(fmt_inter%internals)%iend
              rdata( ibeg : iend, inter_iidx ) = this%internal(1:need_internals)
            end if

          end if

          inter_iidx = inter_iidx + 1

       end do

       inter_offset = inter_offset + tab_nb_inter(inter_idx)

    end do

    ! The HDF5 part
    if( with_rdata ) then
      call write_h5( get_gr_name(gr_vlocrloc), get_ds_name(ds_rdata), rdata )
      deallocate( rdata )
    end if
    if( with_idata ) then
      call write_h5( get_gr_name(gr_vlocrloc), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if

  end subroutine write_h5_Vloc_Rloc_3D

  subroutine apply_ugly_fixer_(idata)
    implicit none
    integer, dimension(:,:), pointer :: idata
    !
    integer :: idx, id_inter_idx, id_cdtac_idx, id_antac_idx

    if( ugly_fix == 0 ) then
        ! the fixer in this case is to change the parameter value
        ! of some interactions and contactors id
        id_inter_idx = fmt_inter%idata(fmt_inter%inter_id)%ibeg
        id_cdtac_idx = fmt_inter%idata(fmt_inter%tactype)%ibeg
        id_antac_idx = fmt_inter%idata(fmt_inter%tactype)%iend
        do idx = 1, size(idata,2)
            if( idata(id_inter_idx,idx) > i_dkdkl ) then
                idata(id_inter_idx,idx) = idata(id_inter_idx,idx) - 3
            end if
            if( idata(id_cdtac_idx,idx) > i_xksid ) then
                idata(id_cdtac_idx,idx) = idata(id_cdtac_idx,idx) - 2
            end if
            if( idata(id_antac_idx,idx) > i_xksid ) then
                idata(id_antac_idx,idx) = idata(id_antac_idx,idx) - 2
            end if
        end do
    end if

  end subroutine apply_ugly_fixer_

end module h5_interaction
