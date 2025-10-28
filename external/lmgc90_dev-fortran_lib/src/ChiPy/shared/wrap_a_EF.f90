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
!===========================================================================

module wrap_a_EF
  
  use iso_c_binding

  use a_EF, only: get_interpolation_id_from_nodes_and_dim, &
                  interpolate_field, compute_center

  implicit none

  public

contains

  subroutine InterpolateField(field,nb_nodes,point,space_dim,res) bind(c, name='a_EF_InterpolateField')
    implicit none
    integer(kind=c_int), intent(in), value :: nb_nodes
    real(kind=c_double), dimension(nb_nodes) :: field
    integer(kind=c_int), intent(in), value :: space_dim
    real(kind=c_double), dimension(space_dim) :: point
    real(kind=c_double) :: res
    !
    integer(kind=4) :: i

    i = get_interpolation_id_from_nodes_and_dim(nb_nodes,space_dim) 
    call interpolate_field(field,nb_nodes,i,space_dim,point,res)

  end subroutine

  subroutine ComputeCenter(coor,nb_nodes,isize,ccenter,csize) bind(c, name='a_EF_ComputeCenter')
    implicit none
    integer(kind=c_int), intent(in), value :: nb_nodes, isize
    real(kind=c_double), dimension(isize,nb_nodes) :: coor
    integer(kind=c_int) :: csize
    type(c_ptr) :: ccenter
    !
    integer(kind=4) :: i
    real(kind=8), dimension(:), pointer :: center
 
    csize = isize
    allocate(center(csize))
    i = get_interpolation_id_from_nodes_and_dim(nb_nodes,isize) 
    call compute_center(i,coor,center)

    ccenter = c_loc(center(1))
  end subroutine
 
end module wrap_a_EF
