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
module wrap_nlgs_new_int_2D
  
  use ISO_C_BINDING

  use nlgs_new_int_2D, only: solve_nlgs             , &
                             comp_check_nlgs        , &
                             scramble_nlgs          , &
                             quick_scramble_nlgs    , &
                             reverse_nlgs           , &
                             scale_rloc_nlgs        , &
                             RnodHRloc_nlgs         , &
                             set_nlgs_parameter     , &
                             prep_nlgs              , &
                             prep_check_nlgs        , &
                             Nullify_EntityList_nlgs, &
                             write_norm_check_nlgs  , &
                             !bimodal_list_nlgs, &
                             display_check_nlgs     , &
                             display_rlocn_sum_nlgs , &
                             !update_tact_behav_nlgs, &
                             !init_cohe_nlgs
                             get_all_this

   logical         :: with_quick_scramble = .false.
   integer(kind=4) :: nb_iter_in_module

contains

    subroutine ExPrep(cvalue1_c) bind(c, name='nlgs_newarch_ExPrep')
      implicit none
      character(c_char), dimension(30) :: cvalue1_c
      logical           :: SDLactif
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

      SDLactif = .false.
      if(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.true.
      call prep_nlgs(SDLactif)

      nb_iter_in_module = 0

    end subroutine
!!!--------------------------------------------------------
    subroutine ExIter(nb_iter) bind(c, name='nlgs_newarch_ExIter')
      implicit none
      integer(c_int), intent(in), value :: nb_iter
      !! PURPOSE
      !!  Execute nb_iter NLGS iteration over the contact loop
      integer(kind=4) :: ik

      do ik = 1, nb_iter
        call solve_nlgs(1)
        nb_iter_in_module = nb_iter_in_module + 1
      end do

    end subroutine
!!!--------------------------------------------------------
    subroutine ExPost() bind(c, name='nlgs_newarch_ExPost')
      implicit none
       !! PURPOSE
       !!  run a jacobi iteration with the solution obtain
       !!  with the NLGS algorithm

       call RnodHRloc_nlgs
       call solve_nlgs(3)
       call Nullify_EntityList_nlgs
       call write_norm_check_nlgs(3)

    end subroutine
!!!--------------------------------------------------------
    function AfterIterCheck() bind(c, name='nlgs_newarch_AfterIterCheck')
      implicit none
      integer(c_int) :: AfterIterCheck
       !! PURPOSE
       !!  Control NLGS convergence

       AfterIterCheck = 0
       call prep_check_nlgs(AfterIterCheck)
       if (AfterIterCheck == 0 ) return
       call solve_nlgs(2)
       call comp_check_nlgs(AfterIterCheck)
       call write_norm_check_nlgs(2)

    end function
!!!--------------------------------------------------------
    subroutine DisplayAfterIterCheck() bind(c, name='nlgs_newarch_DisplayAfterIterCheck')
      implicit none
       !! PURPOSE
       !!  display NLGS convergence results

       call display_check_nlgs

    end subroutine
!!!--------------------------------------------------------
    subroutine NormCheck() bind(c, name='nlgs_newarch_NormCheck')
      implicit none
       !! PURPOSE
       !!  Active one step norm evolution

       call write_norm_check_nlgs(1)

    end subroutine
!!!--------------------------------------------------------
    !SUBROUTINE UpdateTactBehav() bind(c, name='nlgs_newarch_UpdateTactBehav')
    !  IMPLICIT NONE
    !   !! PURPOSE
    !   !!  update internal parameters of contact laws for each contact

    !   CALL update_tact_behav_nlgs

    !END SUBROUTINE
!!!--------------------------------------------------------
    subroutine SetCheckType(cvalue1_c,rvalue1,rvalue2) bind(c, name='nlgs_newarch_SetCheckType')
      implicit none
      character(c_char), dimension(5)   :: cvalue1_c
      real(c_double), intent(in), VALUE :: rvalue1,rvalue2
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

       call set_nlgs_parameter(cvalue1,rvalue1,rvalue2)

    end subroutine
!!!--------------------------------------------------------
    subroutine ScrambleContactOrder() bind(c, name='nlgs_newarch_ScrambleContactOrder')
      implicit none
       !! PURPOSE
       !!  random renumbering of the contact list

       call scramble_nlgs

    end subroutine
!!!--------------------------------------------------------
    subroutine QuickScrambleContactOrder() bind(c, name='nlgs_newarch_QuickScrambleContactOrder')
      implicit none
       !! PURPOSE
       !!  random renumbering of the contact list

       call quick_scramble_nlgs

    end subroutine
!!!--------------------------------------------------------
    subroutine SetWithQuickScramble() bind(C, name = 'nlgs_newarch_SetWithQuickScramble')
       !! PURPOSE
       !!  active quick scramble in macro function ExSolver
       implicit none

       !CALL LOGCHIC('NLGS_3D')
       with_quick_scramble = .true.

    end subroutine SetWithQuickScramble

!!!--------------------------------------------------------
    subroutine ReverseContactOrder() bind(c, name='nlgs_newarch_ReverseContactOrder')
      implicit none
       !! PURPOSE
       !!  reverse the numbering of the contact list

       call reverse_nlgs       

    end subroutine
!!!--------------------------------------------------------
    !SUBROUTINE BimodalContactOrder() bind(c, name='nlgs_newarch_BimodalContactOrder')
    !  IMPLICIT NONE
    !   !! PURPOSE
    !   !!  renumbering the contact list using the definition of 
    !   !!  weak and strong network in granular assemblies

    !   CALL bimodal_list_nlgs       
    !END SUBROUTINE
!!!--------------------------------------------------------
    subroutine ScaleRloc() bind(c, name='nlgs_newarch_ScaleRloc')
      implicit none
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1

       call scale_rloc_nlgs

    end subroutine
!!!--------------------------------------------------------
    subroutine ComputeRnod() bind(c, name='nlgs_newarch_ComputeRnod')
      implicit none
       !! PURPOSE
       !!  mapping from local contact forces to global ones 

       call RnodHRloc_nlgs

    end subroutine
!!!--------------------------------------------------------
    subroutine DisplayRlocNSum() bind(c, name='nlgs_newarch_DisplayRlocNSum')
      implicit none
       !! PURPOSE 
       !!  display the sum of normal contact forces

       call display_rlocn_sum_nlgs

    end subroutine
!!! MACRO COMMAND -----------------------------------------
    subroutine ExSolver(cvalue1_c,cvalue2_c,rvalue1,rvalue2,ivalue1,ivalue2) bind(c, name='nlgs_newarch_ExSolver')
      implicit none

      character(c_char), dimension(30)  :: cvalue1_c
      character(c_char), dimension(5 )  :: cvalue2_c  
      real(c_double), intent(in), value :: rvalue1,rvalue2
      integer(c_int), intent(in), value :: ivalue1,ivalue2
      integer                           :: iconv,iter,ib,ik
      logical                           :: SDLactif
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

      call set_nlgs_parameter(cvalue2,rvalue1,rvalue2)

      SDLactif = .false.
      if(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.true.
      call prep_nlgs(SDLactif)
      iter = 0
      do ib = 1, ivalue2
         if( with_quick_scramble ) then
           call quick_scramble_nlgs
         end if

         do ik = 1, ivalue1
            iter = iter + 1
            call solve_nlgs(1)
         end do

         iconv = 0
         call prep_check_nlgs(iconv)

         if (iconv == 0 ) return
         call solve_nlgs(2)

         call comp_check_nlgs(iconv)
         if (iconv == 0) exit
      end do

      call RnodHRloc_nlgs
      call solve_nlgs(3)
      call Nullify_EntityList_nlgs

    end subroutine

  !SUBROUTINE InitCohesiveBehav() bind(C, name = 'nlgs_newarch_InitCohesiveBehav')
  !     !! PURPOSE
  !     !!  update internal parameters of contact laws for each contact

  !     IMPLICIT NONE

  !     CALL init_cohe_nlgs

  !END SUBROUTINE InitCohesiveBehav

  subroutine GetAllThis(r8_matrix, dim1, dim2) bind(C, name = 'nlgs_newarch_GetAllThis')
    implicit none
    type(c_ptr)    :: r8_matrix
    integer(c_int) :: dim1, dim2

    real(kind=8), dimension(:,:), pointer :: r8

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

  end subroutine

end module wrap_nlgs_new_int_2D
