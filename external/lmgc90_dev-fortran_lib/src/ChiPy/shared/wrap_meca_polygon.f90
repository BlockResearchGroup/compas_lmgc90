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

module wrap_meca_polygon
  
  use iso_c_binding

  use meca_polygon, only: compute_central_kernel, &
                          compute_stress_field

  implicit none

  public

contains

  subroutine computeCentralKernel(c_pts, c_s1, c_s2, c_n, c_ns, c_pout, c_s1o, c_s2o) bind(c, name='MecaPolyg_CentralKernel')
    implicit none
    type(c_ptr)   , value :: c_pts, c_n
    integer(c_int), value :: c_s1, c_s2, c_ns
    type(c_ptr)           :: c_pout
    integer(c_int)        :: c_s1o, c_s2o
    !
    real(kind=8)       , dimension(:,:), pointer :: points, points_ck
    integer(kind=c_int), dimension(:)  , pointer :: sizes
    real(kind=8) :: decomp
    integer      :: nb_pts, is_in
    logical      :: bavard

    decomp = 0.d0
    bavard = .false.

    points_ck => null()

    call c_f_pointer( cptr=c_pts, fptr=points, shape=(/c_s2, c_s1/) )
    call c_f_pointer( cptr=c_n  , fptr=sizes , shape=(/c_ns/)       )

    call compute_central_kernel(points, sizes, decomp, nb_pts, points_ck, null(), is_in, bavard)

    if( associated(points_ck) ) then
      c_pout = c_loc( points_ck(1,1) )
      c_s1o  = size(points_ck,1)
      c_s2o  = size(points_ck,2)
    else
      c_pout = c_null_ptr
      c_s1o  = 0
      c_s2o  = 0
    end if

  end subroutine computeCentralKernel

  subroutine computeStressField(c_pts, c_ps1, c_ps2, & !<--- inputs
                                c_sz , c_ss1,        &
                                c_cop, c_cs1,        &
                                c_rn , c_rs1,        &
                                c_cc , cc_s1, cc_s2, & !<--- outputs
                                c_nc , nc_s,         &
                                c_cd , cd_s1, cd_s2, &
                                c_nd , nd_s,         &
                                s_ptr, s_s , decomp  ) bind(C, name='MecaPolyg_StressField')
    implicit none
    ! inputs
    type(c_ptr), value                :: c_pts, c_sz, c_cop, c_rn
    integer(c_int), intent(in), value :: c_ps1, c_ps2, c_ss1, c_cs1, c_rs1
    ! outputs
    integer(c_int)                    :: cc_s1, cc_s2, nc_s, cd_s1, cd_s2, nd_s, s_s
    type(c_ptr)                       :: c_cc, c_nc, c_cd, c_nd, s_ptr
    real(c_double)                    :: decomp
    !
    integer(c_int), dimension(:)  , pointer :: sizes, nc, nd
    real(c_double), dimension(:)  , pointer :: cop, normal, s
    real(c_double), dimension(:,:), pointer :: face, cc, cd
    real(c_double) :: rn
    integer :: err
    logical :: bavard

    cc_s1 = 0
    cc_s2 = 0
    nc_s  = 0
    cd_s1 = 0
    cd_s2 = 0
    nd_s  = 0
    s_s   = 0

    c_cc  = c_null_ptr
    c_nc  = c_null_ptr
    c_cd  = c_null_ptr
    c_nd  = c_null_ptr
    s_ptr = c_null_ptr

    cc => null()
    cd => null()
    nc => null()
    nd => null()
    s  => null()

    call c_f_pointer( cptr=c_pts, fptr=face  , shape=(/c_ps2, c_ps1/) )
    call c_f_pointer( cptr=c_sz , fptr=sizes , shape=(/c_ss1/) )
    call c_f_pointer( cptr=c_cop, fptr=cop   , shape=(/c_cs1/) )
    call c_f_pointer( cptr=c_rn , fptr=normal, shape=(/c_rs1/) )

    rn = norm2( normal )
    normal(:) = normal(:) / rn

    bavard = .false.
 
    call compute_stress_field(face, sizes, cop, normal, rn, cc, nc, s, cd, nd, decomp, err, bavard)

    if( associated(nc) ) then
      cc_s1 = size(cc,1); cc_s2 = size(cc,2)
      c_cc  = c_loc(cc(1,1))
      nc_s  = size(nc,1)
      c_nc  = c_loc(nc(1))
    end if
    if( associated(nd) ) then
      cd_s1 = size(cd,1); cd_s2 = size(cd,2)
      c_cd  = c_loc(cd(1,1))
      nd_s  = size(nd,1)
      c_nd  = c_loc(nd(1))
    end if
    if( associated(s) ) then
      s_s   = size(s,1)
      s_ptr = c_loc(s(1))
    end if

  end subroutine
 
end module wrap_meca_polygon
