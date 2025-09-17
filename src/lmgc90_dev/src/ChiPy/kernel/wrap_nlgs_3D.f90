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
MODULE wrap_NLGS_3D

  USE ISO_C_BINDING

  use overall, only : faterr

  USE NLGS_3D,ONLY:&
       solve_nlgs, &
       comp_check_nlgs, &
       write_norm_check_nlgs, &
       scramble_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       display_check_nlgs, &
       scale_rloc_nlgs, &
       RnodHRloc_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs, &
       active_diagonal_resolution, &
!!$       init_cohe_nlgs_3D, &
       assume_is_initialized, &
       display_tacinfo, &
       use_jacobi_solver,&
       use_regul, &
       use_cut_open_czm, &
       use_manage_interpenetrated_czm

   logical         :: with_quick_scramble = .FALSE.
   logical         :: with_reverse_order  = .FALSE.
   integer(kind=4) :: nb_iter_in_module

CONTAINS

!!!--------------------------------------------------------

  SUBROUTINE ExIter(nb_iter) bind(C, name = 'nlgs_3D_ExIter')
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

  SUBROUTINE ExIterJacobi(nb_iter) bind(C, name = 'nlgs_3D_ExIterJacobi')
    implicit none
    integer(c_int), intent(in), value :: nb_iter
    !! PURPOSE
    !!  Execute nb_iter NLGS iteration over the contact loop
    integer(kind=4) :: ik

    call use_jacobi_solver( .TRUE. )
    do ik = 1, nb_iter
      call solve_nlgs(1)
      nb_iter_in_module = nb_iter_in_module + 1
    end do
    call use_jacobi_solver( .FALSE. )


  END SUBROUTINE

  function AfterIterCheck() bind(C, name = 'nlgs_3D_AfterIterCheck')
       !! PURPOSE
       !!  Control NLGS convergence

       IMPLICIT NONE

       integer(c_int) :: AfterIterCheck
       integer :: iconv, i_check = 2

       AfterIterCheck = 0

       CALL prep_check_nlgs(iconv)
       IF (iconv == 0 ) RETURN
       CALL solve_nlgs(i_check)
       CALL comp_check_nlgs(iconv)
       CALL write_norm_check_nlgs(2)

       AfterIterCheck = iconv

  END function

  function AfterIterCheckJacobi() bind(C, name = 'nlgs_3D_AfterIterCheckJacobi')
       !! PURPOSE
       !!  Control NLGS convergence

       IMPLICIT NONE

       integer(c_int) :: AfterIterCheckJacobi
       integer :: iconv, i_check = 2

       AfterIterCheckJacobi = 0

       CALL prep_check_nlgs(iconv)
       IF (iconv == 0 ) RETURN
       CALL use_jacobi_solver( .TRUE. )
       CALL solve_nlgs(i_check)
       CALL use_jacobi_solver( .FALSE. )
       CALL comp_check_nlgs(iconv)
       CALL write_norm_check_nlgs(2)

       AfterIterCheckJacobi = iconv

  END function

  SUBROUTINE Scramble() bind(C, name = 'nlgs_3D_ScrambleContactOrder')
       !! PURPOSE
       !!  random renumbering of the contact list

       IMPLICIT NONE

       CALL scramble_nlgs       

  END SUBROUTINE Scramble

  SUBROUTINE QuickScramble() bind(C, name = 'nlgs_3D_QuickScrambleContactOrder')
       !! PURPOSE
       !!  random renumbering of the contact list

       IMPLICIT NONE

       CALL quick_scramble_nlgs       

  END SUBROUTINE QuickScramble

  SUBROUTINE ReverseContactOrder() bind(C, name = 'nlgs_3D_ReverseContactOrder')
       !! PURPOSE
       !!  reverse the numbering of the contact list

       IMPLICIT NONE

       !CALL LOGCHIC('NLGS_3D')
       CALL reverse_nlgs

  END SUBROUTINE ReverseContactOrder

  SUBROUTINE DisplayAfterIterCheck() bind(C, name = 'nlgs_3D_DisplayAfterIterCheck')
       !! PURPOSE
       !!  display NLGS convergence results

       IMPLICIT NONE

       !CALL LOGCHIC('NLGS_3D')
       CALL display_check_nlgs

  END SUBROUTINE DisplayAfterIterCheck

  SUBROUTINE ScaleRloc() bind(C, name = 'nlgs_3D_ScaleRloc')
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1

       IMPLICIT NONE

       !CALL LOGCHIC('NLGS_3D')
       CALL scale_rloc_nlgs

  END SUBROUTINE ScaleRloc

  SUBROUTINE RnodHRloc() bind(C, name = 'nlgs_3D_ComputeRnod')
       !! PURPOSE
       !!  mapping from local contact forces to global ones 

       IMPLICIT NONE

       !CALL LOGCHIC('NLGS_3D')
       CALL RnodHRloc_nlgs

  END SUBROUTINE RnodHRloc

  SUBROUTINE ExPost() bind(C, name = 'nlgs_3D_ExPost')
       !! PURPOSE
       !!  run a jacobi iteration with the solution obtain
       !!  with the NLGS algorithm

       IMPLICIT NONE

       integer :: i_post = 3
       !CALL LOGCHIC('NLGS_3D')
       CALL RnodHRloc_nlgs
       CALL solve_nlgs(i_post)
       CALL Nullify_EntityList_nlgs
       CALL write_norm_check_nlgs(3)

  END SUBROUTINE ExPost

  SUBROUTINE ExPostJacobi() bind(C, name = 'nlgs_3D_ExPostJacobi')
       !! PURPOSE
       !!  run a jacobi iteration with the solution obtain
       !!  with the NLGS algorithm

       IMPLICIT NONE

       integer :: i_post = 3
       !CALL LOGCHIC('NLGS_3D')
       CALL RnodHRloc_nlgs
       CALL use_jacobi_solver( .TRUE. )
       CALL solve_nlgs(i_post)
       CALL use_jacobi_solver( .FALSE. )
       CALL Nullify_EntityList_nlgs
       CALL write_norm_check_nlgs(3)

  END SUBROUTINE ExPostJacobi



  SUBROUTINE UpdateTactBehav() bind(C, name = 'nlgs_3D_UpdateTactBehav')
       use timer
       IMPLICIT NONE
       !! PURPOSE
       !!  update internal parameters of contact laws for each contact
       integer(kind=4), save :: timer_id = 0
                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[NLGS3] update tact ')
       call start_itimer(timer_id)

       !CALL LOGCHIC('NLGS_3D')
       CALL update_tact_behav_nlgs
       
       call stop_itimer(timer_id)

  END SUBROUTINE UpdateTactBehav

!!$  SUBROUTINE InitCohesiveBehav() bind(C, name = 'nlgs_3D_InitCohesiveBehav')
!!$       !! PURPOSE
!!$       !!  update internal parameters of contact laws for each contact
!!$
!!$       IMPLICIT NONE
!!$
!!$       CALL init_cohe_nlgs_3D
!!$
!!$  END SUBROUTINE InitCohesiveBehav

  SUBROUTINE SetCheckType(checktype_c, tol, RELAX) bind(C, name = 'nlgs_3D_SetCheckType')
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

       IMPLICIT NONE

       character(c_char), dimension(5), intent(in) :: checktype_c
       real(c_double), intent(in),value :: tol, RELAX

       character(len=5) :: checktype
       integer :: i

       !CALL LOGCHIC('NLGS_3D')

       checktype = ''
       do i=1, 5
         checktype = checktype(1:i-1) // checktype_c(i)
       end do

       CALL set_nlgs_parameter(checktype,tol,RELAX)

  END SUBROUTINE SetCheckType

  SUBROUTINE ExPrep(Wstorage_c) bind(C, name = 'nlgs_3D_ExPrep')
       !! PURPOSE
       !!  prepare the matrix and the RHS of the contact problem
       !!  in regards of the selected matrix storage:
       !!  - Exchange_Local_Global (the standard case)
       !!   only the diagonal blocks are computed and stored.
       !!  - Stored_Delassus_Loops (faster but memory expensive)
       !!   the complete Delassus matrix is computed.

       IMPLICIT NONE

       character(c_char), dimension(30), intent(in) :: Wstorage_c

       character(len=30) :: Wstorage
       integer :: i
       logical SDLactif

       !CALL LOGCHIC('NLGS_3D')
       
       Wstorage = ''
       do i = 1, 30
         if( Wstorage_c(i) == c_null_char ) exit
         Wstorage = Wstorage(1:i-1) // Wstorage_c(i)
       end do

       !IF (KHOZZZ == 1) THEN
       !   WRITE(cout,'(A14,1X,A30)') ' @ Wstorage = ',Wstorage
       !   CALL LOGMESCHIC(cout)
       !END IF
       if( trim(Wstorage) == 'Stored_Delassus_Loops' ) then
         SDLactif = .true.
       else if( trim(Wstorage) == 'Exchange_Local_Global' ) then
         SDLactif = .false.
       else
         call faterr( 'wrap_nlgs_3D::ExPrep', &
                      'Unknown solver type, choose among "Stored_Delassus_Loops" &
                      &and "Exchange_Local_Global"' )
       end if
       call prep_nlgs(SDLactif)

       nb_iter_in_module = 0

  END SUBROUTINE ExPrep

  SUBROUTINE WriteNormCheck() bind(C, name = 'nlgs_3D_WriteNormCheck')
       !! USES
       !!  LMGC90.CORE/NLGS_3D/write_norm_check_nlgs

       IMPLICIT NONE

       CALL write_norm_check_nlgs(1)

  END SUBROUTINE WriteNormCheck

  SUBROUTINE DiagonalResolution() bind(C, name = 'nlgs_3D_DiagonalResolution')

       IMPLICIT NONE

       CALL active_diagonal_resolution

  END SUBROUTINE DiagonalResolution
!!! MACRO COMMAND -----------------------------------------

  SUBROUTINE SetWithQuickScramble() bind(C, name = 'nlgs_3D_SetWithQuickScramble')
       !! PURPOSE
       !!  active quick scramble in macro function ExSolver

       IMPLICIT NONE

       with_quick_scramble = .TRUE.

  END SUBROUTINE SetWithQuickScramble

  SUBROUTINE SetWithReverseContactOrder() bind(C, name = 'nlgs_3D_SetWithReverseContactOrder')
       !! PURPOSE
       !!  active reverse order in macro function ExSolver

       IMPLICIT NONE

       with_reverse_order = .TRUE.

  END SUBROUTINE SetWithReverseContactOrder

  SUBROUTINE ExSolver(Wstorage_c, checktype_c, tol, RELAX, nb_iter_check, nb_block_iter) bind(C, name = 'nlgs_3D_ExSolver')
       !! PURPOSE
       !!  solve fully the local contact problem
       use overall, only : logmes
       use timer
       implicit none
 
       character(c_char), dimension(30), intent(in) :: Wstorage_c
       character(c_char), dimension(5),  intent(in) :: checktype_c
       real(c_double), intent(in), value :: tol, RELAX
       integer(c_int), intent(in), value :: nb_iter_check, nb_block_iter
       !
       logical :: SDLactif
       character(len=30) :: Wstorage
       character(len=5)  :: checktype
       character(len=80) :: cout
       integer :: i_iter = 1, i_check = 2, i_post = 3
       integer :: iconv, iter, ib, ik

       integer(kind=4) :: timer_id_prep = 0
       integer(kind=4) :: timer_id_iter = 0
       integer(kind=4) :: timer_id_check = 0
       integer(kind=4) :: timer_id_post = 0

                                                                    !12345678901234567890
       if( timer_id_prep  == 0 ) timer_id_prep  = get_new_itimer_ID('[NLGS3] prep        ')
       if( timer_id_iter  == 0 ) timer_id_iter  = get_new_itimer_ID('[NLGS3] iter        ')
       if( timer_id_check == 0 ) timer_id_check = get_new_itimer_ID('[NLGS3] check       ')
       if( timer_id_post  == 0 ) timer_id_post  = get_new_itimer_ID('[NLGS3] post        ')

       Wstorage = ''
       do iter = 1, 30
         if( Wstorage_c(iter) == c_null_char ) exit
         Wstorage = Wstorage(1:iter-1) // Wstorage_c(iter)
       end do

       checktype = ''
       do iter = 1, 5
         checktype = checktype(1:iter-1) // checktype_c(iter)
       end do

       call start_itimer(timer_id_prep)

       CALL set_nlgs_parameter(checktype,tol,RELAX)

       if( trim(Wstorage) == 'Stored_Delassus_Loops' ) then
         SDLactif = .true.
       else if( trim(Wstorage) == 'Exchange_Local_Global' ) then
         SDLactif = .false.
       else
         call faterr( 'wrap_nlgs_3D::ExSolver', &
                      'Unknown solver type, choose among "Stored_Delassus_Loops" &
                      &and "Exchange_Local_Global"' )
       end if

       call prep_nlgs(SDLactif)

       call stop_itimer(timer_id_prep)

       iter = 0
       DO ib=1,nb_block_iter

          call start_itimer(timer_id_iter)
 
          IF( with_quick_scramble ) THEN
             CALL quick_scramble_nlgs       
          END IF
          IF( with_reverse_order  ) THEN
             CALL reverse_nlgs
          END IF
 
          DO ik=1,nb_iter_check
             iter = iter + 1
             CALL solve_nlgs(i_iter)
          END DO

          call stop_itimer(timer_id_iter)

          call start_itimer(timer_id_check)

          iconv=0
          CALL prep_check_nlgs(iconv)
          IF (iconv == 0 ) RETURN
          CALL solve_nlgs(i_check)
          CALL comp_check_nlgs(iconv)

          call stop_itimer(timer_id_check)
  
          call display_check_nlgs

          IF (iconv == 0 .or. iconv == -1) EXIT
       END DO

       call start_itimer(timer_id_post)

       CALL RnodHRloc_nlgs
       CALL solve_nlgs(i_post)
       CALL Nullify_EntityList_nlgs
 
       call stop_itimer(timer_id_post)

       if (iconv == -1 ) then
          write(cout,'(A32,I8)') '  @ NLGS diverged at ITERATION: ',iter
          call logmes(cout, .TRUE.)
          call faterr( 'wrap_nlgs_3D::ExSolver', cout)
       end if
      write(cout,'(A20,I8)') '  @ NLGS ITERATION: ',iter
      call logmes(cout)!, .true.)
      ! if( iter == nb_iter_check*nb_block_iter ) call logmes('  @ WARNING : NLGS not converged',.true.)
      if( iter == nb_iter_check*nb_block_iter ) call logmes('  @ WARNING : NLGS not converged')      
      
  END SUBROUTINE ExSolver

  subroutine IsInitialized(is_init) bind(C, name = 'nlgs_3D_IsInitialized')
    implicit none
    integer(kind=c_int), intent(in), value :: is_init
     
    call assume_is_initialized(is_init)

  end subroutine

  SUBROUTINE DisplayTacInfo(ik) bind(C, name = 'nlgs_3D_DisplayTacInfo')
    implicit none
    integer(c_int), intent(in), value :: ik

    call display_tacinfo(ik)

  END SUBROUTINE
  
  SUBROUTINE UseJacobiSolver(jacobi) bind(C, name = 'nlgs_3D_UseJacobiSolver')
    IMPLICIT NONE
    LOGICAL(C_BOOL) ,INTENT(IN), VALUE :: jacobi
    LOGICAL(KIND=4)                    :: jacobi_
    
    jacobi_ = jacobi
    call use_jacobi_solver( jacobi_ )

  END SUBROUTINE

  SUBROUTINE UseRegul(rvalue1,rvalue2) bind(C, name = 'nlgs_3D_UseRegularization')
    IMPLICIT NONE
    REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2

    call use_regul(rvalue1,rvalue2)

  END SUBROUTINE UseRegul
  
  SUBROUTINE CutOpenCZM(rvalue1) bind(C, name = 'nlgs_3D_CutOpenCZM')
    IMPLICIT NONE
    REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1

    call use_cut_open_czm(rvalue1)

  END SUBROUTINE CutOpenCZM
  
  SUBROUTINE ManageInterpenetratedCZM() bind(C, name = 'nlgs_3D_ManageInterpenetratedCZM')
    IMPLICIT NONE

    call use_manage_interpenetrated_czm()

  END SUBROUTINE

END MODULE wrap_NLGS_3D
