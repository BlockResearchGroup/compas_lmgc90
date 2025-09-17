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
MODULE wrap_nlgs
  
  USE ISO_C_BINDING

  use overall, only : faterr

  USE nlgs,ONLY:&
       solve_nlgs, &
       comp_check_nlgs, &
       write_norm_check_nlgs, &
       scramble_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       bimodal_list_nlgs, &
       display_check_nlgs, &
       scale_rloc_nlgs, &
       RnodHRloc_nlgs, &
       display_rlocn_sum_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs,&
       assume_is_initialized, &
       update_cohe_nlgs, &
       update_fric_nlgs, &
       get_all_this, &
       use_jacobi_solver, &
       use_regul, &
       set_temporary_variable_nlgs, &
       get_temporary_variable_nlgs
       

   logical         :: with_quick_scramble = .FALSE.
   integer(kind=4) :: nb_iter_in_module

CONTAINS

    SUBROUTINE ExPrep(cvalue1_c) bind(c, name='nlgs_ExPrep')
      use timer
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
        if( cvalue1_c(i) == c_null_char ) exit
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      if( trim(cvalue1) == 'Stored_Delassus_Loops' ) then
        SDLactif = .true.
      else if( trim(cvalue1) == 'Exchange_Local_Global' ) then
        SDLactif = .false.
      else
        call faterr( 'wrap_nlgs::ExPrep', &
                     'Unknown solver type, choose among "Stored_Delassus_Loops" &
                     &and "Exchange_Local_Global"' )
      end if

      call prep_nlgs(SDLactif)

      nb_iter_in_module = 0

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ExIter(nb_iter) bind(c, name='nlgs_ExIter')
      use timer
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
    SUBROUTINE ExPost() bind(c, name='nlgs_ExPost')
      use timer
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
    function AfterIterCheck() bind(c, name='nlgs_AfterIterCheck')
      use timer
      IMPLICIT NONE
      INTEGER(C_INT) :: AfterIterCheck

       AfterIterCheck = 0
       CALL prep_check_nlgs(AfterIterCheck)
       IF (AfterIterCheck == 0 ) RETURN
       CALL solve_nlgs(2)
       CALL comp_check_nlgs(AfterIterCheck)
       CALL write_norm_check_nlgs(2)

    END function
!!!--------------------------------------------------------
    SUBROUTINE DisplayAfterIterCheck() bind(c, name='nlgs_DisplayAfterIterCheck')
      IMPLICIT NONE
       !! PURPOSE
       !!  display NLGS convergence results

       CALL display_check_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE NormCheck() bind(c, name='nlgs_NormCheck')
      IMPLICIT NONE
       !! PURPOSE
       !!  Active one step norm evolution

       CALL write_norm_check_nlgs(1)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE UpdateTactBehav() bind(c, name='nlgs_UpdateTactBehav')
      use timer
      IMPLICIT NONE
      !! PURPOSE
      !!  update internal parameters of contact laws for each contact
      integer(kind=4), save :: timer_id = 0
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[NLGS2] update tact ')
      call start_itimer(timer_id)

      CALL update_tact_behav_nlgs

      call stop_itimer(timer_id)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE SetCheckType(cvalue1_c,rvalue1,rvalue2) bind(c, name='nlgs_SetCheckType')
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
    SUBROUTINE ScrambleContactOrder() bind(c, name='nlgs_ScrambleContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  random renumbering of the contact list

       CALL scramble_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE QuickScrambleContactOrder() bind(c, name='nlgs_QuickScrambleContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  random renumbering of the contact list

       CALL quick_scramble_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE SetWithQuickScramble() bind(C, name = 'nlgs_SetWithQuickScramble')
       !! PURPOSE
       !!  active quick scramble in macro function ExSolver

       IMPLICIT NONE

       !CALL LOGCHIC('NLGS_3D')
       with_quick_scramble = .TRUE.

    END SUBROUTINE SetWithQuickScramble

!!!--------------------------------------------------------
    SUBROUTINE ReverseContactOrder() bind(c, name='nlgs_ReverseContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  reverse the numbering of the contact list

       CALL reverse_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE BimodalContactOrder() bind(c, name='nlgs_BimodalContactOrder')
      IMPLICIT NONE
       !! PURPOSE
       !!  renumbering the contact list using the definition of 
       !!  weak and strong network in granular assemblies

       CALL bimodal_list_nlgs       
    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ScaleRloc() bind(c, name='nlgs_ScaleRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1

       CALL scale_rloc_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ComputeRnod() bind(c, name='nlgs_ComputeRnod')
      IMPLICIT NONE
       !! PURPOSE
       !!  mapping from local contact forces to global ones 

       CALL RnodHRloc_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE DisplayRlocNSum() bind(c, name='nlgs_DisplayRlocNSum')
      IMPLICIT NONE
       !! PURPOSE 
       !!  display the sum of normal contact forces

       CALL display_rlocn_sum_nlgs

    END SUBROUTINE
!!! MACRO COMMAND -----------------------------------------
    SUBROUTINE ExSolver(cvalue1_c,cvalue2_c,rvalue1,rvalue2,ivalue1,ivalue2) bind(c, name='nlgs_ExSolver')
      use timer
      use overall, only : logmes
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
      character(len=80) :: cout
      integer :: i
      integer(kind=4) :: timer_id_prep  = 0
      integer(kind=4) :: timer_id_iter  = 0
      integer(kind=4) :: timer_id_check = 0
      integer(kind=4) :: timer_id_post  = 0

                                                                   !12345678901234567890
      if( timer_id_prep  == 0 ) timer_id_prep  = get_new_itimer_ID('[NLGS2] prep        ')
      if( timer_id_iter  == 0 ) timer_id_iter  = get_new_itimer_ID('[NLGS2] iter        ')
      if( timer_id_check == 0 ) timer_id_check = get_new_itimer_ID('[NLGS2] check       ')
      if( timer_id_post  == 0 ) timer_id_post  = get_new_itimer_ID('[NLGS2] post        ')

      cvalue1 = ''
      cvalue2 = ''
      do i=1,30
        if( cvalue1_c(i) == c_null_char ) exit
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do
      do i=1,5
        cvalue2 = cvalue2(1:i-1) // cvalue2_c(i)
      end do

      call start_itimer(timer_id_prep)

      CALL set_nlgs_parameter(cvalue2,rvalue1,rvalue2)

      if( trim(cvalue1) == 'Stored_Delassus_Loops' ) then
        SDLactif = .true.
      else if( trim(cvalue1) == 'Exchange_Local_Global' ) then
        SDLactif = .false.
      else
        call faterr( 'wrap_nlgs::ExSolver', &
                     'Unknown solver type, choose among "Stored_Delassus_Loops" &
                     &and "Exchange_Local_Global"' )
      end if

      call prep_nlgs(SDLactif)

      call stop_itimer(timer_id_prep)

      iter = 0
      DO ib=1,ivalue2

         call start_itimer(timer_id_iter)

         IF( with_quick_scramble ) THEN
           CALL quick_scramble_nlgs
         END IF

         DO ik=1,ivalue1
            iter = iter + 1
            CALL solve_nlgs(1)
         END DO

         call stop_itimer(timer_id_iter)

         call start_itimer(timer_id_check)

         iconv=0
         CALL prep_check_nlgs(iconv)

         IF (iconv == 0 ) RETURN !<- c est quoi cette merde !!!
         CALL solve_nlgs(2)

         CALL comp_check_nlgs(iconv)

         call stop_itimer(timer_id_check)

         call display_check_nlgs

         IF (iconv == 0 .or. iconv == -1) EXIT
      END DO

      call start_itimer(timer_id_post)

      CALL RnodHRloc_nlgs
      CALL solve_nlgs(3)
      CALL Nullify_EntityList_nlgs

      call stop_itimer(timer_id_post)

      if (iconv == -1 ) then
         write(cout,'(A32,I8)') '  @ NLGS diverged at ITERATION: ',iter
         call logmes(cout, .TRUE.)
         call faterr( 'wrap_nlgs::ExSolver', cout)
      end if
      write(cout,'(A20,I8)') '  @ NLGS ITERATION : ',iter
      call logmes(cout)
      ! if( iter == ivalue1*ivalue2 ) call logmes('  @ WARNING : NLGS not converged', .true.)
      if( iter == ivalue1*ivalue2 ) call logmes('  @ WARNING : NLGS not converged')      

    END SUBROUTINE

  SUBROUTINE UpdateCohesiveBehav() bind(C, name = 'nlgs_UpdateCohesiveBehav')
    !! PURPOSE
    !!  update internal parameters of contact laws for each contact
    
    IMPLICIT NONE
    
    CALL update_cohe_nlgs
    
  END SUBROUTINE UpdateCohesiveBehav

  SUBROUTINE UpdateFrictionalBehav() bind(C, name = 'nlgs_UpdateFrictionalBehav')
    !! PURPOSE
    !!  update internal parameters of contact laws for each contact
    
    IMPLICIT NONE
    
    CALL update_fric_nlgs
    
  END SUBROUTINE UpdateFrictionalBehav

  SUBROUTINE GetAllThis(r8_matrix, dim1, dim2) bind(C, name = 'nlgs_GetAllThis')
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

  SUBROUTINE UseJacobiSolver(jacobi) bind(C, name = 'nlgs_UseJacobiSolver')
    IMPLICIT NONE
    LOGICAL(C_BOOL) ,INTENT(IN), VALUE :: jacobi
    LOGICAL(KIND=4)                    :: jacobi_
    
    jacobi_ = jacobi
    call use_jacobi_solver( jacobi_ )

  END SUBROUTINE

  SUBROUTINE UseRegul(rvalue1,rvalue2) bind(C, name = 'nlgs_UseRegularization')
    IMPLICIT NONE
    REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2

    call use_regul(rvalue1,rvalue2)

  END SUBROUTINE

  SUBROUTINE SetTemporaryVariable(ivalue1,ivalue2,rvalue1) bind(C, name = 'nlgs_SetTemporaryVariable')
    IMPLICIT NONE
    INTEGER(C_INT), INTENT(IN), VALUE :: ivalue1,ivalue2    
    REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1

    call set_temporary_variable_nlgs(ivalue1,ivalue2,rvalue1)

  END SUBROUTINE

  FUNCTION GetTemporaryVariable(ivalue1,ivalue2) bind(C, name = 'nlgs_GetTemporaryVariable')
    IMPLICIT NONE
    INTEGER(C_INT), INTENT(IN), VALUE :: ivalue1,ivalue2    
    REAL(C_DOUBLE) :: GetTemporaryVariable

    GetTemporaryVariable = get_temporary_variable_nlgs(ivalue1,ivalue2)

  END FUNCTION
  
  subroutine IsInitialized(is_init) bind(C, name = 'nlgs_IsInitialized')
    implicit none
    integer(kind=c_int), intent(in), value :: is_init

    call assume_is_initialized(is_init)

  end subroutine

END MODULE wrap_nlgs
