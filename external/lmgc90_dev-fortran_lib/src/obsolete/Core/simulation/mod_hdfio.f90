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

!> This module manage the database of the state variables (dof,fields,interaction,etc)
module hdfio

  use hdf5

  use state

  implicit none

  private

  public write_mecaState_hdf5

  contains

  subroutine write_mecaState_hdf5()
    implicit none
    integer(kind=4) :: i, rank, error
                                    !123456789012
    character(len=12) :: filename = 'mecaState.h5'
    character(len=6)  :: dsetname
    character(len=13) :: groupname
    integer(hid_t) :: file_id, group_id, dset_id, dspace_id, dtype_id
    integer(hsize_t), dimension(:), allocatable :: dims
    real(kind=8), dimension(:), pointer :: data

    ! initialize fortran interface
    call h5open_f(error)

    ! create a new file with default properties
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

    call h5tcopy_f(H5T_NATIVE_DOUBLE,dtype_id, error)

    do i = 1, get_nb_mecaStates()
      write(groupname, '(A,I1)') 'mecaState : ', i 
      call h5gcreate_f(file_id, groupname, group_id, error)
      ! create a dataspace
      data => get_V(i)
      dsetname = 'V'
      rank = size( shape(data) )
      allocate(dims(rank))
      dims = shape(data)
      call h5screate_simple_f(rank, dims , dspace_id, error)

      ! create a dataset with default properties
      call h5dcreate_f(group_id, dsetname, dtype_id, dspace_id, dset_id, error)
      call h5dwrite_f(dset_id, dtype_id, data, dims, error)!, dspace_id)
      deallocate(dims)
      call h5dclose_f(dset_id, error)
      call h5sclose_f(dspace_id, error)
      call h5gclose_f(group_id, error)
    end do

    ! close file
    call h5fclose_f(file_id, error)
    ! close fortran inteface
    call h5close_f(error)

  end subroutine

end module hdfio
