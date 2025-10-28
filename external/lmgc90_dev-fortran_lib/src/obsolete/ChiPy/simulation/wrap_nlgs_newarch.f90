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
module wrap_nlgs_newarch
  
  use ISO_C_BINDING

  use nlgs_newarch, only: solve_nlgs, &
                          comp_check_nlgs, &
                          scramble_nlgs, &
                          quick_scramble_nlgs, &
                          reverse_nlgs, &
                          scale_rloc_nlgs, &
                          RnodHRloc_nlgs, &
                          set_nlgs_parameter, &
                          prep_nlgs, &
                          prep_check_nlgs, &
                          Nullify_EntityList_nlgs,&
                          write_norm_check_nlgs, &
                          !bimodal_list_nlgs, &
                          display_check_nlgs, &
                          display_rlocn_sum_nlgs, &
                          !update_tact_behav_nlgs, &
                          !init_cohe_nlgs
                          get_all_this

   logical         :: with_quick_scramble = .FALSE.
   integer(kind=4) :: nb_iter_in_module

CONTAINS

    SUBROUTINE ExPrep(cvalue1_c) bind(c, name='nlgs_newarch_ExPrep')
      IMPLICIT NONE
      CHARACTER(C_CHAR), dimension(30) :: cvalue1_c
      LOGICAL           :: SDLactif
      !! PURPOSE
      !!  prepare the matrix and the RHS of the contact problem
      !!  in regards of the selected matrix storage:
      !!  - Exchange_Local_Global (the standard case)
      !!   only the diagonal blocks are computed and stored.
      !!  - Stored_Delassus_Loops (faster but memory expensive)
      !!   the complete Delassus matrix is computed.

      character(len=30) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,len(cvalue1)
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      SDLactif = .FALSE.
      IF(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
      CALL prep_nlgs(SDLactif)

      nb_iter_in_module = 0

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ExIter(nb_iter) bind(c, name='nlgs_newarch_ExIter')
      implicit none
      integer(c_int), intent(in), value :: nb_iter
      !! PURPOSE
      !!  Execute nb_iter NLGS iteration over the contact loop
      integer(kind=4) :: ik

      do ik = 1, nb_iter
        call solve_nlgs(1)
        nb_iter_in_module = nb_iter_in_module + 1
      end do

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ExPost() bind(c, name='nlgs_newarch_ExPost')
      IMPLICIT NONE
       !! PURPOSE
       !!  run a jacobi iteration with the solution obtain
       !!  with the NLGS algorithm

       CALL RnodHRloc_nlgs
       CALL solve_nlgs(3)
       CALL Nullify_EntityList_nlgs
       CALL write_norm_check_nlgs(3)

    END SUBROUTINE
!!!--------------------------------------------------------
    function AfterIterCheck() bind(c, name='nlgs_newarch_AfterIterCheck')
      IMPLICIT NONE
      INTEGER(C_INT) :: AfterIterCheck
       !! PURPOSE
       !!  Control NLGS convergence

       AfterIterCheck = 0
       CALL prep_check_nlgs(AfterIterCheck)
       IF (AfterIterCheck == 0 ) RETURN
       CALL solve_nlgs(2)
       CALL comp_check_nlgs(AfterIterCheck)
       CALL write_norm_check_nlgs(2)

    END function
!!!--------------------------------------------------------
    SUBROUTINE DisplayAfterIterCheck() bind(c, name='nlgs_newarch_DisplayAfterIterCheck')
      IMPLICIT NONE
       !! PURPOSE
       !!  display NLGS convergence results

       CALL display_check_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE NormCheck() bind(c, name='nlgs_newarch_NormCheck')
      IMPLICIT NONE
       !! PURPOSE
       !!  Active one step norm evolution

       CALL write_norm_check_nlgs(1)

    END SUBROUTINE
!!!--------------------------------------------------------
    !SUBROUTINE UpdateTactBehav() bind(c, name='nlgs_newarch_UpdateTactBehav')
    !  IMPLICIT NONE
    !   !! PURPOSE
    !   !!  update internal parameters of contact laws for each contact

    !   CALL update_tact_behav_nlgs

    !END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE SetCheckType(cvalue1_c,rvalue1,rvalue2) bind(c, name='nlgs_newarch_SetCheckType')
      IMPLICIT NONE
      character(C_CHAR), dimension(5)   :: cvalue1_c
      real(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2
       !! PURPOSE
       !!  define numerical parameters of the NLGS algorithm
       !!  convergence check keywords:
       !!  Quad  : quadratic norm (faulty contacts are redeemed by accurate
       !!          contacts; laxist norm)
       !!  Maxm  : maximum norm (faulty contacts must comply; severe norm)
       !!  QM/16 : maximum of Quad and Maxm/16 norms (a compromise). For
       !!          large dense collections Quad ranges usually around 1/16 Maxm
       !!  where Quad,Maxm,QM/16 are keywords for the check test, and the 
       !!  following real number is the tolerance value.

       character(len=5) :: cvalue1
       integer :: i

       cvalue1 = ''
       do i=1,5
         cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
       end do

       CALL set_nlgs_parameter(cvalue1,rvalue1,rvalue2)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ScrambleContactOrder() bind(c, name='nlgs_newarch_ScrambleContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  random renumbering of the contact list

       CALL scramble_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE QuickScrambleContactOrder() bind(c, name='nlgs_newarch_QuickScrambleContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  random renumbering of the contact list

       CALL quick_scramble_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE SetWithQuickScramble() bind(C, name = 'nlgs_newarch_SetWithQuickScramble')
       !! PURPOSE
       !!  active quick scramble in macro function ExSolver

       IMPLICIT NONE

       !CALL LOGCHIC('NLGS_3D')
       with_quick_scramble = .TRUE.

    END SUBROUTINE SetWithQuickScramble

!!!--------------------------------------------------------
    SUBROUTINE ReverseContactOrder() bind(c, name='nlgs_newarch_ReverseContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  reverse the numbering of the contact list

       CALL reverse_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    !SUBROUTINE BimodalContactOrder() bind(c, name='nlgs_newarch_BimodalContactOrder')
    !  IMPLICIT NONE
    !   !! PURPOSE
    !   !!  renumbering the contact list using the definition of 
    !   !!  weak and strong network in granular assemblies

    !   CALL bimodal_list_nlgs       
    !END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ScaleRloc() bind(c, name='nlgs_newarch_ScaleRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1

       CALL scale_rloc_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ComputeRnod() bind(c, name='nlgs_newarch_ComputeRnod')
      IMPLICIT NONE
       !! PURPOSE
       !!  mapping from local contact forces to global ones 

       CALL RnodHRloc_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE DisplayRlocNSum() bind(c, name='nlgs_newarch_DisplayRlocNSum')
      IMPLICIT NONE
       !! PURPOSE 
       !!  display the sum of normal contact forces

       CALL display_rlocn_sum_nlgs

    END SUBROUTINE
!!! MACRO COMMAND -----------------------------------------
    SUBROUTINE ExSolver(cvalue1_c,cvalue2_c,rvalue1,rvalue2,ivalue1,ivalue2) bind(c, name='nlgs_newarch_ExSolver')
      IMPLICIT NONE

      CHARACTER(C_CHAR), dimension(30)  :: cvalue1_c
      CHARACTER(C_CHAR), dimension(5 )  :: cvalue2_c  
      REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2
      INTEGER(C_INT), INTENT(IN), VALUE :: ivalue1,ivalue2
      INTEGER                           :: iconv,iter,ib,ik
      LOGICAL                           :: SDLactif
      !! PURPOSE
      !!  solve fully the local contact problem

      character(len=30) :: cvalue1
      character(len=5 ) :: cvalue2
      integer :: i

      cvalue1 = ''
      cvalue2 = ''
      do i=1,30
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do
      do i=1,5
        cvalue2 = cvalue2(1:i-1) // cvalue2_c(i)
      end do

      CALL set_nlgs_parameter(cvalue2,rvalue1,rvalue2)

      SDLactif = .FALSE.
      IF(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
      CALL prep_nlgs(SDLactif)
      iter = 0
      DO ib=1,ivalue2
         IF( with_quick_scramble ) THEN
           CALL quick_scramble_nlgs
         END IF

         DO ik=1,ivalue1
            iter = iter + 1
            CALL solve_nlgs(1)
         END DO

         iconv=0
         CALL prep_check_nlgs(iconv)

         IF (iconv == 0 ) RETURN
         CALL solve_nlgs(2)

         CALL comp_check_nlgs(iconv)
         IF (iconv == 0) EXIT
      END DO

      CALL RnodHRloc_nlgs
      CALL solve_nlgs(3)
      CALL Nullify_EntityList_nlgs

    END SUBROUTINE

  !SUBROUTINE InitCohesiveBehav() bind(C, name = 'nlgs_newarch_InitCohesiveBehav')
  !     !! PURPOSE
  !     !!  update internal parameters of contact laws for each contact

  !     IMPLICIT NONE

  !     CALL init_cohe_nlgs

  !END SUBROUTINE InitCohesiveBehav

  SUBROUTINE GetAllThis(r8_matrix, dim1, dim2) bind(C, name = 'nlgs_newarch_GetAllThis')
    IMPLICIT NONE
    TYPE(C_PTR)    :: r8_matrix
    INTEGER(C_INT) :: dim1, dim2

    REAL(KIND=8), DIMENSION(:,:), POINTER :: r8

    r8 => get_all_this()

    if( associated(r8) ) then
      r8_matrix = c_loc(r8(1,1))
      dim1 = size(r8,1)
      dim2 = size(r8,2)
    else
      r8_matrix = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  END SUBROUTINE

END MODULE wrap_nlgs_newarch
