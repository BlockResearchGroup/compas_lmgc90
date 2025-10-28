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
MODULE wrap_cpg_3D

  USE ISO_C_BINDING

  USE cpg_3D,ONLY: &
       set_cpg_parameter, &
       ex_iter_cpg, & 
       ex_check_cpg, &
       write_norm_check_cpg, &
       ex_post_cpg, &
       ex_prep_cpg, &
       set_diag_precond_cpg, &
       set_frictionless, &
       scale_rloc_cpg, &
       Nullify_EntityList_cpg, &
       set_bimodal_list
  
CONTAINS
!!!-------------------------------------------------------

  SUBROUTINE ExIter() bind(C, name = 'cpg_3D_ExIter')
       !! PURPOSE
       !!  Execute one CPG iteration over the contact loop

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL ex_iter_cpg

  END SUBROUTINE ExIter

  SUBROUTINE AfterIterCheck() bind(C, name = 'cpg_3D_AfterIterCheck')
       !! PURPOSE
       !!  Control CPG convergence

       IMPLICIT NONE

       integer :: iconv

       !CALL LOGCHIC('CPG_3D')
       CALL ex_check_cpg(iconv)
       CALL write_norm_check_cpg(2)

  END SUBROUTINE AfterIterCheck

  SUBROUTINE ExPost() bind(C, name = 'cpg_3D_ExPost')
       !! PURPOSE
       !!  transfert local solution
       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL ex_post_cpg
       CALL Nullify_EntityList_cpg
       CALL write_norm_check_cpg(3)

  END SUBROUTINE ExPost

  SUBROUTINE ExPrep() bind(C, name = 'cpg_3D_ExPrep')
       !! PURPOSE
       !!  prepare the matrix and the RHS of the contact problem

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL ex_prep_cpg

  END SUBROUTINE ExPrep

  SUBROUTINE ScaleRloc() bind(C, name = 'cpg_3D_ScaleRloc')
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL scale_rloc_cpg

  END SUBROUTINE ScaleRloc

  SUBROUTINE SetDiagonalPrecond() bind(C, name = 'cpg_3D_SetDiagonalPrecond')
       !! PURPOSE
       !!  active diagonal preconditioner

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL set_diag_precond_cpg

  END SUBROUTINE SetDiagonalPrecond

  SUBROUTINE SetFrictionless() bind(C, name = 'cpg_3D_SetFrictionless') 
       !! PURPOSE
       !!  active frictionless solver

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL set_frictionless

  END SUBROUTINE SetFrictionless

  SUBROUTINE BimodalContactOrder() bind(C, name = 'cpg_3D_BimodalContactOrder')
       !! PURPOSE
       !!  active bimodal list

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL set_bimodal_list

  END SUBROUTINE BimodalContactOrder

  SUBROUTINE SetCheckType(checktype_c, tol, idproj) bind(C, name = 'cpg_3D_SetCheckType')
       !! PURPOSE
       !!  define numerical parameters of the CPG algorithm
       !!  convergence check keywords:
       !!  Quad  : quadratic norm (faulty contacts are redeemed by accurate
       !!          contacts; laxist norm)
       !!  Maxm  : maximum norm (faulty contacts must comply; severe norm)
       !!  where Quad,Maxm are keywords for the check test, and the 
       !!  following real number is the tolerance value.
       !!  The identifiant projection parameter corrsponds to :
       !!   PYRAMIDAL APPROXIMATION (1) 
       !!    Efficient but no more isotropic friction
       !!   NORMAL PROJECTION (2)
       !!    The basic projection but not really efficient
       !!   HYBRID CORRECTION (3)
       !!    Efficient for sphere but not really sense for other bodies.

       IMPLICIT NONE

       character(c_char), dimension(5), intent(in) :: checktype_c
       real(c_double), intent(in), value :: tol
       integer(c_int), intent(in), value :: idproj

       character(len=5) :: checktype
       integer          :: i
       !CALL LOGCHIC('CPG_3D')

       checktype = ''
       do i = 1, 5
         checktype = checktype(1:i-1) // checktype_c(i)
       end do

       CALL set_cpg_parameter(checktype,tol,idproj)

  END SUBROUTINE SetCheckType

  SUBROUTINE NormCheck() bind(C, name = 'cpg_3D_NormCheck')
       !! PURPOSE
       !!  Active one step norm evolution

       IMPLICIT NONE

       !CALL LOGCHIC('CPG_3D')
       CALL write_norm_check_cpg(1)

  END SUBROUTINE NormCheck
!!! MACRO COMMAND -----------------------------------------

  SUBROUTINE ExSolver(checktype_c, tol, idproj, nb_iter_check, nb_block_iter) bind(C, name = 'cpg_3D_ExSolver')
       !! PURPOSE
       !!  solve fully the local contact problem

       IMPLICIT NONE

       character(c_char), dimension(5), intent(in) :: checktype_c
       real(c_double), intent(in), value :: tol
       integer(c_int), intent(in), value :: idproj, nb_iter_check, nb_block_iter

       character(len=5) :: checktype
       integer          :: i, ib,ik, iconv
       !CALL LOGCHIC('CPG_3D')
!        IF (KHOZZZ == 1) THEN
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' WARNING: NEW FROM SEPT. 2006')
!           CALL LOGMESCHIC(' You use a new MACRO COMMAND ')
!           CALL LOGMESCHIC(' to solve the contact problem')
!           CALL LOGMESCHIC(' with the CPG algorithm.     ')
!           CALL LOGMESCHIC(' Parameters are:')
!           CALL LOGMESCHIC(' [NormID] [NormValue]')
!           CALL LOGMESCHIC('      Quad  x.xxxxD-xx')
!           CALL LOGMESCHIC(' [Iteration number for check ]')
!           CALL LOGMESCHIC('  I1xxx')
!           CALL LOGMESCHIC(' [Block iteration number]')
!           CALL LOGMESCHIC('  I2xxx')  
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' ******************************')     
!        END IF

       checktype = ''
       do i = 1, 5
         checktype = checktype(1:i-1) // checktype_c(i)
       end do
       
       CALL set_cpg_parameter(checktype,tol,idproj)
       CALL ex_prep_cpg
       DO ib=1,nb_block_iter
          DO ik=1,nb_iter_check
             CALL ex_iter_cpg
          END DO
          CALL ex_check_cpg(iconv)
          IF (iconv == 0) EXIT
       END DO
       CALL ex_post_cpg
       CALL Nullify_EntityList_cpg

  END SUBROUTINE ExSolver
!!!-------------------------------------------------------
END MODULE wrap_cpg_3D
