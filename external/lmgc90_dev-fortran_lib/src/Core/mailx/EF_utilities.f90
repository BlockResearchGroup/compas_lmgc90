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
! frederic.dubois@lmgcuniv-montp2.fr
!
!===========================================================================


! file to be included in a xxxxEF_iso.f90 module

!> \brief compute map from Gauss points to nodes of a linear element
!> I do not know why, but it is considered better to compute this map
!> only for linear elements. For quadratic points, mean between adjacent
!> linear nodes is used.
!> If there is only one Gauss points... the map is one
!> (and not computed otherwise the resolution bugs)
subroutine compute_gp2node(interpolation_id,nb_node,quadrature_id,nb_gp,mat)
  use a_matrix
  implicit none
  !> interpolation id
  integer(kind=4), intent(in) :: interpolation_id
  !> quadrature rule id
  integer(kind=4), intent(in) :: quadrature_id
  !> number of nodes of the supporting element
  integer(kind=4), intent(in) :: nb_node
  !> number of Gauss points on the element for the quadrature
  integer(kind=4), intent(in) :: nb_gp
  !> output mapping matrix
  real(kind=8), dimension(nb_node,nb_gp), intent(out) :: mat
  ! ***
  integer(kind=4) :: i, j, i_gp, fo_inter_id, nb_n
  real(kind=8)    :: tmp
  type(T_mat_sym) :: M, M_fact

  ! for resolution
  character(len=1) :: isprecon
  real(kind=8), dimension(:), allocatable :: scale

  ! Gauss points position (dim,nbgp)
  real(kind=long), dimension(:,:), pointer :: CG
  ! form funcions values at Gauss points (and weigths but not used)
  real(kind=long), dimension(:)  , pointer :: N, poids

  integer(kind=4), dimension(:,:), pointer :: e2v
  
  !first order interpolation id
  fo_inter_id = get_first_order_interpolation_id(interpolation_id, nb_n)

  if( nb_gp > 1 ) then

    allocate(scale(nb_n))
    nullify(CG,poids)
    call pos_gauss(quadrature_id, CG, poids)

    if (size(cg,dim=2) /= nb_gp) then
      call faterr('EF_utilities::compute_gp2node','nbgp inconsistancy')
    end if

    call new_matrix(M,nb_n)
    call zero_matrix(M)

    call new_matrix(M_fact,nb_n)
    call zero_matrix(M_fact)

    do i = 1, nb_n
      do j = 1, nb_n
        do i_gp = 1, nb_gp 
          nullify(N)
          call fonct_forme(fo_inter_id,CG(:,i_gp),N)
          tmp = N(i)*N(j)
          call add_to_matrix(M,tmp,i,j)
          deallocate(N)
        end do
      end do
    end do

    do i_gp = 1, nb_gp 
      nullify(N)
      call fonct_forme(fo_inter_id,CG(:,i_gp),N)

      if (i_gp==1) then
        call x_solve_linear_system(M,N,M_fact,scale,isprecon,'E')
      else
        call x_solve_linear_system(M,N,M_fact,scale,isprecon,'F')
      endif

      mat(1:nb_n,i_gp) = N(:)

      deallocate(N)

    end do

    call free_matrix(M)
    call free_matrix(M_fact)

    deallocate(cg,poids,scale)

  else
    mat(1:nb_n,:) = 1.d0
  end if

  ! in case of quadratic element :
  ! use edge2vertices map to compute quadratic points
  if( fo_inter_id /= interpolation_id ) then
    e2v => get_ptr_edge2vertices(interpolation_id)
    do i = nb_n+1, nb_node
      mat(i,:) = 0.5 * ( mat(e2v(1,i),:) + mat(e2v(2,i),:) )
    end do
  end if

end subroutine

