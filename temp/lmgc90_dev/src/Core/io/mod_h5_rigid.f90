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

!> Define the rigids hdf5 subroutines

module h5_rigid

  use overall, only : faterr

  use RBDY2_type, only : T_RBDY2
  use RBDY2     , only : get_bdyty_RBDY2

  use RBDY3_type, only : T_RBDY3
  use RBDY3     , only : get_bdyty_RBDY3

  use h5_format, only : get_gr_name, get_ds_name, &
                        gr_rbdy2   , gr_rbdy3   , &
                        ds_idata   , ds_rdata   , &
                        fmt_rigid

  use lmgc90_hdf5, only : read_h5, write_h5

  implicit none

  private

  public ::  read_h5_dof_RBDY2, &
            write_h5_dof_RBDY2, &
             read_h5_dof_RBDY3, &
            write_h5_dof_RBDY3

contains 

  subroutine read_h5_dof_RBDY2(step)
    !> evolution id to read
    integer, intent(in) :: step
    ! HDF5 Buffer: present the data to save in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    ! Standard variables
    type( T_RBDY2 ), dimension( : ), pointer :: bdyty
    integer :: i, ibeg, iend
    logical :: with_idata, with_rdata

    rdata => null()
    idata => null()

    with_rdata = .false.
    with_idata = .false.

    ! The HDF5 part
    if( associated( fmt_rigid%rdata ) ) then
      call read_h5( get_gr_name(gr_rbdy2, step), get_ds_name(ds_rdata), rdata )
      with_rdata = .true.
    end if

    if( associated( fmt_rigid%idata ) ) then
      call read_h5( get_gr_name(gr_rbdy2, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( with_rdata .and. .not. associated( rdata ) ) return

    ! Get the structure to write into
    call get_bdyty_RBDY2( bdyty )

    ! Check the size of the data from HDF5 and the size of the structure to write into
    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
      call faterr('hd5_hl::read_dof_RBDY2_','Please read_bodies before trying to read dof')
    end if

    if ( size( bdyty, 1 ) /= size( rdata, 2 ) ) then
      call faterr('hd5_hl::read_dof_RBDY2_','Inconsistent size between already allocated bdyty and data in file')
    end if

    ! From HDF5 buffers, fill the LMGC90's internal structures
    do i = lbound( bdyty, 1 ), ubound( bdyty, 1 )

       if( with_rdata ) then

         if( fmt_rigid%X > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%X)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%X)%iend
           bdyty( i )%Xbegin = rdata( ibeg : iend, i )
           bdyty( i )%X      = bdyty( i )%Xbegin
         end if

         if( fmt_rigid%V > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%V)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%V)%iend
           bdyty( i )%Vbegin = rdata( ibeg : iend, i )
           bdyty( i )%V      = bdyty( i )%Vbegin
         end if

       end if

       if( with_idata ) then

         if( fmt_rigid%visible > 0 ) then
           ibeg = fmt_rigid%idata(fmt_rigid%visible)%ibeg
           if( idata(ibeg, i) == 0 ) then
             bdyty( i )%visible = .false.
           else
             bdyty( i )%visible = .true.
           end if
         end if

       end if

    end do

    if( with_rdata ) deallocate( rdata )
    if( with_idata ) deallocate( idata )

  end subroutine read_h5_dof_RBDY2

!------------------------------------------------------------------------
  !> \brief Write dofs of RBDY2
  subroutine write_h5_dof_RBDY2()
    implicit none
    ! Local variables
    ! HDF5 Buffer: present the data to save in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    !
    type( T_RBDY2 ), dimension( : ), pointer :: bdyty
    integer :: i, ibeg, iend
    logical :: with_idata, with_rdata

    call get_bdyty_RBDY2( bdyty )

    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
       return
    end if

    with_rdata = .false.
    with_idata = .false.

    if( associated( fmt_rigid%rdata ) ) then
      allocate( rdata( fmt_rigid%rdata_sz, size( bdyty, 1 ) ) )
      with_rdata = .true.
    end if

    if( associated( fmt_rigid%idata ) ) then
      allocate( idata( fmt_rigid%idata_sz, size( bdyty, 1 ) ) )
      with_idata = .true.
    end if

    ! Fill the buffer to write in HDF5
    do i = lbound( bdyty, 1 ), ubound( bdyty, 1 )

       if( with_rdata ) then

         if( fmt_rigid%X > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%X)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%X)%iend
           rdata( ibeg : iend, i ) = bdyty( i )%X
         end if

         if( fmt_rigid%V > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%V)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%V)%iend
           rdata( ibeg : iend, i ) = bdyty( i )%V
         end if

       end if

       if( with_idata ) then
         if( fmt_rigid%visible > 0 ) then
           ! merge is an intrinsic Fortran function.
           ! It is equivalent to: bdyty( i )%visible ? 1 : 0
           ! If bdyty( i )%visible is .TRUE., return 1, else return 0
           ibeg = fmt_rigid%idata(fmt_rigid%visible)%ibeg
           idata( ibeg, i ) = merge( 1, 0, bdyty( i )%visible )
         end if

       end if

    end do

    ! The HDF5 part
    if( with_rdata ) then
      call write_h5( get_gr_name(gr_rbdy2), get_ds_name(ds_rdata), rdata )
      deallocate( rdata )
    end if
    if( with_idata ) then
      call write_h5( get_gr_name(gr_rbdy2), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if


  end subroutine write_h5_dof_RBDY2

!------------------------------------------------------------------------

  subroutine read_h5_dof_RBDY3(step)
    !> evolution id to read
    integer, intent(in) :: step
    ! Local variables
    integer, parameter :: nb_flag = 1 ! nb_flag to save for each rbdy2

    ! HDF5 Buffer: present the data to save in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    ! Standard variables
    type( T_RBDY3 ), dimension( : ), pointer :: bdyty
    integer :: i, ibeg, iend
    logical :: with_idata, with_rdata

    rdata => null()
    idata => null()

    with_rdata = .false.
    with_idata = .false.

    ! The HDF5 part
    if( associated( fmt_rigid%rdata ) ) then
      call read_h5( get_gr_name(gr_rbdy3, step), get_ds_name(ds_rdata), rdata )
      with_rdata = .true.
    end if

    if( associated( fmt_rigid%idata ) ) then
      call read_h5( get_gr_name(gr_rbdy3, step), get_ds_name(ds_idata), idata )
      with_idata = .true.
    end if

    if( with_rdata .and. .not. associated( rdata ) ) return

    ! Get the structure to write into
    call get_bdyty_RBDY3( bdyty )

    ! Check the size of the data from HDF5 and the size of the structure to write into
    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
      call faterr('hd5_hl::read_dof_RBDY3_','Please read_bodies before trying to read dof')
    end if

    if ( size( bdyty, 1 ) /= size( rdata, 2 ) ) then
      call faterr('hd5_hl::read_dof_RBDY3_','Inconsistent size between already allocated bdyty and data in file')
    end if

    ! From HDF5 buffers, fill the LMGC90's internal structures
    do i = lbound( bdyty, 1 ), ubound( bdyty, 1 )

       if( with_rdata ) then

         if( fmt_rigid%X > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%X)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%X)%iend
           bdyty( i )%Xbegin = rdata( ibeg : iend, i )
           bdyty( i )%X      = bdyty( i )%Xbegin
         end if

         if( fmt_rigid%V > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%V)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%V)%iend
           bdyty( i )%Vbegin = rdata( ibeg : iend, i )
           bdyty( i )%V      = bdyty( i )%Vbegin
         end if

         if( fmt_rigid%LF > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%LF)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%LF)%iend
           bdyty( i )%LocalFrame(1:3,1:3)    = reshape( rdata(ibeg:iend, i), shape=(/3,3/) )
           bdyty( i )%LocalFrameIni(1:3,1:3) = bdyty( i )%LocalFrame(1:3,1:3)
         end if

       end if

       if( with_idata ) then

         if( fmt_rigid%visible > 0 ) then
           ibeg = fmt_rigid%idata(fmt_rigid%visible)%ibeg
           if( idata(ibeg, i) == 0 ) then
             bdyty( i )%visible = .false.
           else
             bdyty( i )%visible = .true.
           end if
         end if

       end if

    end do

    if( with_rdata ) deallocate( rdata )
    if( with_idata ) deallocate( idata )

  end subroutine read_h5_dof_RBDY3

!------------------------------------------------------------------------

  subroutine write_h5_dof_RBDY3()
    implicit none
    ! Local variables
    ! HDF5 Buffer: present the data to save in the right way to HDF5
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata
    ! Standard variables
    type( T_RBDY3 ), dimension( : ), pointer :: bdyty
    integer :: i, ibeg, iend
    logical :: with_idata, with_rdata

    call get_bdyty_RBDY3( bdyty )

    if ( ( .not. associated( bdyty ) ) .or. ( size( bdyty, 1 ) <= 0 ) ) then
       return
    end if

    with_rdata = .false.
    with_idata = .false.

    if( associated( fmt_rigid%rdata ) ) then
      allocate( rdata( fmt_rigid%rdata_sz, size( bdyty, 1 ) ) )
      with_rdata = .true.
    end if

    if( associated( fmt_rigid%idata ) ) then
      allocate( idata( fmt_rigid%idata_sz, size( bdyty, 1 ) ) )
      with_idata = .true.
    end if

    ! Fill the buffer to write in HDF5
    do i = lbound( bdyty, 1 ), ubound( bdyty, 1 )

       if( with_rdata ) then

         if( fmt_rigid%X > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%X)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%X)%iend
           rdata( ibeg : iend, i ) = bdyty( i )%X(1:6)
         end if

         if( fmt_rigid%V > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%V)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%V)%iend
           rdata( ibeg : iend, i ) = bdyty( i )%V(1:6)
         end if

         if( fmt_rigid%LF > 0 ) then
           ibeg = fmt_rigid%rdata(fmt_rigid%LF)%ibeg
           iend = fmt_rigid%rdata(fmt_rigid%LF)%iend
           rdata( ibeg : iend, i ) = reshape( bdyty( i )%LocalFrame(1:3,1:3), shape=(/9/) )
         end if
       end if

       if( with_idata ) then

         if( fmt_rigid%visible > 0 ) then
           ! merge is an intrinsic Fortran function.
           ! It is equivalent to: bdyty( i )%visible ? 1 : 0
           ! If bdyty( i )%visible is .TRUE., return 1, else return 0
           ibeg = fmt_rigid%idata(fmt_rigid%visible)%ibeg
           idata( ibeg, i ) = merge( 1, 0, bdyty( i )%visible )
         end if

       end if

    end do

    ! The HDF5 part
    if( with_rdata ) then
      call write_h5( get_gr_name(gr_rbdy3), get_ds_name(ds_rdata), rdata )
      deallocate( rdata )
    end if
    if( with_idata ) then
      call write_h5( get_gr_name(gr_rbdy3), get_ds_name(ds_idata), idata )
      deallocate( idata )
    end if

  end subroutine write_h5_dof_RBDY3

end module h5_rigid
