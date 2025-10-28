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
module wrap_DDM_ExternalFEM

   use ISO_C_BINDING
   use overall
   USE DDM_ExternalFEM
   USE LMGC90_MPI
  
   !implicit none
   
   logical         :: with_quick_scramble = .FALSE. 
  
   contains
!!!----------------------------------------------------------------------------------------------------------

   subroutine SetWorkingDirectory_DDM_ExternalFEM() bind(C, name = 'DDM_ExternalFEM_SetDDWorkingDirectory')
      implicit none
      call set_working_directory_in_DDM_ExternalFEM
      DDM_SCHWARTZ = .true.
   end subroutine SetWorkingDirectory_DDM_ExternalFEM
!!!----------------------------------------------------------------------------------------------------------  

   SUBROUTINE ExSolver_DDM_ExternalFEM(cvalue1_c,cvalue2_c,rvalue1,rvalue2,ivalue1,ivalue2) &
                                   bind(C, name='DDM_ExternalFEM_ExSolver')
      use timer
      use nlgs
      IMPLICIT NONE

      CHARACTER(C_CHAR), dimension(30)  :: cvalue1_c
      CHARACTER(C_CHAR), dimension(5 )  :: cvalue2_c  
      REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2
      INTEGER(C_INT), INTENT(IN), VALUE :: ivalue1,ivalue2
      INTEGER                           :: iconv,iter,ib,ik
      LOGICAL                           :: SDLactif
      integer(kind=4), dimension(:), allocatable :: list_of_contacts_interface
      !! PURPOSE
      !!  solve fully the local contact problem

      character(len=30) :: cvalue1
      character(len=5 ) :: cvalue2
      integer :: i
      integer(kind=4) :: timer_id_prep = 0
      integer(kind=4) :: timer_id_iter = 0
      integer(kind=4) :: timer_id_check = 0
      integer(kind=4) :: timer_id_post = 0
      
      integer(kind=4) :: timer_id_rnodhrolc = 0
      integer(kind=4) :: timer_id_exchange = 0

                                                                 !12345678901234567890
      if( timer_id_prep == 0 ) timer_id_prep = get_new_itimer_ID('i Prep NLGS         ')
      if( timer_id_iter == 0 ) timer_id_iter = get_new_itimer_ID('i Iter NLGS         ')
      if( timer_id_check == 0 ) timer_id_check = get_new_itimer_ID('i Check NLGS        ')
      if( timer_id_post == 0 ) timer_id_post = get_new_itimer_ID('i Post NLGS         ')
      
      if( timer_id_rnodhrolc == 0 ) timer_id_rnodhrolc = get_new_itimer_ID('i RnodHRloc NLGS    ')
      if( timer_id_exchange == 0 ) timer_id_exchange = get_new_itimer_ID('i Exchange NLGS     ')

      cvalue1 = ''
      cvalue2 = ''
      do i=1,30
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do
      do i=1,5
        cvalue2 = cvalue2(1:i-1) // cvalue2_c(i)
      end do

      if( timer_id_prep /= 0 ) call start_itimer(timer_id_prep)
      CALL set_nlgs_parameter(cvalue2,rvalue1,rvalue2)
      SDLactif = .FALSE.
      IF(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
   
      CALL prep_nlgs(SDLactif)

      ! will allocate the array
      call sub_get_contacts_interface( list_of_contacts_interface )

call MPI_BARRIER(MPI_COMM_WORLD, code_MPI)
      if( timer_id_prep /= 0 ) call stop_itimer(timer_id_prep)

      iter = 0
      ! comm
      !call compute_dv_from_other_domains
      DO ib=1,ivalue2

         if( timer_id_iter /= 0 ) call start_itimer(timer_id_iter)

         IF( with_quick_scramble ) THEN
           CALL quick_scramble_nlgs
         END IF

         DO ik=1,ivalue1
            iter = iter + 1
            !comm
            if( timer_id_iter /= 0 ) call stop_itimer(timer_id_iter)
            if( timer_id_rnodhrolc /= 0 ) call start_itimer(timer_id_rnodhrolc)
            call RnodHRloc_nlgs( list_INTRF = list_of_contacts_interface )
            if( timer_id_rnodhrolc /= 0 ) call stop_itimer(timer_id_rnodhrolc)
            if( timer_id_exchange /= 0 ) call start_itimer(timer_id_exchange)
            call compute_dv_from_other_domains
            if( timer_id_exchange /= 0 ) call stop_itimer(timer_id_exchange)
            if( timer_id_iter /= 0 ) call start_itimer(timer_id_iter)
            CALL solve_nlgs(1)
            !call exchange_contacts_DDM
            !CALL RnodHRloc_nlgs ?
            !call compute_dv_from_other_domains
         END DO
         if( timer_id_iter /= 0 ) call stop_itimer(timer_id_iter)

         if( timer_id_check /= 0 ) call start_itimer(timer_id_check)
         iconv=0
         CALL prep_check_nlgs(iconv)
         if( timer_id_check /= 0 ) call stop_itimer(timer_id_check)
         
         !IF (iconv == 0 ) RETURN !<- c est quoi cette merde !!!
         
         !raf: je commente cette fonction est deja dans prep_check_nlgs
         !if( timer_id_rnodhrolc /= 0 ) call start_itimer(timer_id_rnodhrolc)
         !call RnodHRloc_nlgs
         !if( timer_id_rnodhrolc /= 0 ) call stop_itimer(timer_id_rnodhrolc)
         if( timer_id_exchange /= 0 ) call start_itimer(timer_id_exchange)
         call compute_dv_from_other_domains
         if( timer_id_exchange /= 0 ) call stop_itimer(timer_id_exchange)
         if( timer_id_check /= 0 ) call start_itimer(timer_id_check)
         CALL solve_nlgs(2)

         call reduce_over_domains_convergence_elements_2D
         if (rang_COMM_WORLD == 0) then
            CALL comp_check_nlgs(iconv)
         end if
         ! rank 0 send iconv to all
         call broadcast_of_iconv(iconv)

         if( timer_id_check /= 0 ) call stop_itimer(timer_id_check)

         IF (iconv == 0) EXIT
      END DO
      !call compute_dv_from_other_domains      

      !if( timer_id_rnodhrolc /= 0 ) call start_itimer(timer_id_rnodhrolc)
      if( timer_id_post /= 0 ) call start_itimer(timer_id_post)
      call RnodHRloc_nlgs
      !if( timer_id_rnodhrolc /= 0 ) call stop_itimer(timer_id_rnodhrolc)
      if( timer_id_post /= 0 ) call stop_itimer(timer_id_post)
      
      if( timer_id_exchange /= 0 ) call start_itimer(timer_id_exchange)
      call compute_dv_from_other_domains
      if( timer_id_exchange /= 0 ) call stop_itimer(timer_id_exchange)
      
      if( timer_id_post /= 0 ) call start_itimer(timer_id_post)
      CALL solve_nlgs(3)
      CALL Nullify_EntityList_nlgs
call MPI_BARRIER(MPI_COMM_WORLD, code_MPI)
      if( timer_id_post /= 0 ) call stop_itimer(timer_id_post)

      ! just in case
      if( allocated( list_of_contacts_interface ) ) then
         deallocate( list_of_contacts_interface  )
      end if
   END SUBROUTINE
!!!----------------------------------------------------------------------------------------------------------

 SUBROUTINE ExSolver3D_DDM_ExternalFEM(Wstorage_c, checktype_c, tol, RELAX, nb_iter_check, nb_block_iter) &
                                                 bind(C, name = 'DDM_ExternalFEM_ExSolver_3D')
      !! PURPOSE
      !!  solve fully the local contact problem
      
      use timer 
      use nlgs_3D
       
      IMPLICIT NONE

      character(c_char), dimension(30), intent(in) :: Wstorage_c
      character(c_char), dimension(5),  intent(in) :: checktype_c
      real(c_double), intent(in), value :: tol, RELAX
      integer(c_int), intent(in), value :: nb_iter_check, nb_block_iter

      logical :: SDLactif
      integer(kind=4), dimension(:), allocatable :: list_of_contacts_interface
      character(len=30) :: Wstorage
      character(len=5)  :: checktype
      integer :: i_iter = 1, i_check = 2, i_post = 3
      integer :: iconv, iter, ib, ik
       
      integer(kind=4) :: timer_id_prep = 0
      integer(kind=4) :: timer_id_iter = 0
      integer(kind=4) :: timer_id_check = 0
      integer(kind=4) :: timer_id_post = 0
      
      integer(kind=4) :: timer_id_rnodhrolc = 0
      integer(kind=4) :: timer_id_exchange = 0

                                                                 !12345678901234567890
      if( timer_id_prep == 0 ) timer_id_prep = get_new_itimer_ID('i Prep NLGS         ')
      if( timer_id_iter == 0 ) timer_id_iter = get_new_itimer_ID('i Iter NLGS         ')
      if( timer_id_check == 0 ) timer_id_check = get_new_itimer_ID('i Check NLGS        ')
      if( timer_id_post == 0 ) timer_id_post = get_new_itimer_ID('i Post NLGS         ')
      
      if( timer_id_rnodhrolc == 0 ) timer_id_rnodhrolc = get_new_itimer_ID('i RnodHRloc NLGS    ')
      if( timer_id_exchange == 0 ) timer_id_exchange = get_new_itimer_ID('i Exchange NLGS     ')

       Wstorage = ''
       do iter = 1, 30
         Wstorage = Wstorage(1:iter-1) // Wstorage_c(iter)
       end do

       checktype = ''
       do iter = 1, 5
         checktype = checktype(1:iter-1) // checktype_c(iter)
       end do

       if( timer_id_prep /= 0 ) call start_itimer(timer_id_prep)
       CALL set_nlgs_parameter(checktype,tol,RELAX)
       SDLactif = .FALSE.
       IF(Wstorage == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
       CALL prep_nlgs(SDLactif)

       ! will allocate the array
       call sub_get_contacts_interface( list_of_contacts_interface )

  call MPI_BARRIER(MPI_COMM_WORLD, code_MPI)
       if( timer_id_prep /= 0 ) call stop_itimer(timer_id_prep)

       iter = 0
       DO ib=1,nb_block_iter
          if( timer_id_iter /= 0 ) call start_itimer(timer_id_iter)
          IF( with_quick_scramble ) THEN
             CALL quick_scramble_nlgs       
          END IF
          DO ik=1,nb_iter_check
             iter = iter + 1
             if( timer_id_iter /= 0 ) call stop_itimer(timer_id_iter)
             if( timer_id_rnodhrolc /= 0 ) call start_itimer(timer_id_rnodhrolc)
             !raf ajout
             call RnodHRloc_nlgs( list_INTRF = list_of_contacts_interface )
             if( timer_id_rnodhrolc /= 0 ) call stop_itimer(timer_id_rnodhrolc)
             if( timer_id_exchange /= 0 ) call start_itimer(timer_id_exchange)
             !raf ajout
             call compute_dv_from_other_domains
             if( timer_id_exchange /= 0 ) call stop_itimer(timer_id_exchange)
             if( timer_id_iter /= 0 ) call start_itimer(timer_id_iter)
             CALL solve_nlgs(i_iter)
             
          END DO
          if( timer_id_iter /= 0 ) call stop_itimer(timer_id_iter)

          if( timer_id_check /= 0 ) call start_itimer(timer_id_check)
          CALL prep_check_nlgs(iconv)
          if( timer_id_check /= 0 ) call stop_itimer(timer_id_check)
          !IF (iconv == 0 ) RETURN
          !raf ajout
          if( timer_id_exchange /= 0 ) call start_itimer(timer_id_exchange)
          call compute_dv_from_other_domains
          if( timer_id_exchange /= 0 ) call stop_itimer(timer_id_exchange)
          if( timer_id_check /= 0 ) call start_itimer(timer_id_check)
          CALL solve_nlgs(i_check)
          
          !raf remplacement
          call reduce_over_domains_convergence_elements_3D
          if (rang_COMM_WORLD == 0) then
             CALL comp_check_nlgs(iconv)
          end if
          ! rank 0 send iconv to all
          call broadcast_of_iconv(iconv)
          if( timer_id_check /= 0 ) call stop_itimer(timer_id_check)
          IF (iconv == 0) EXIT
       END DO
       if( timer_id_post /= 0 ) call start_itimer(timer_id_post)
       call RnodHRloc_nlgs
       if( timer_id_post /= 0 ) call stop_itimer(timer_id_post)
       !raf ajout
       if( timer_id_exchange /= 0 ) call start_itimer(timer_id_exchange)
       call compute_dv_from_other_domains
       if( timer_id_exchange /= 0 ) call stop_itimer(timer_id_exchange)
       
       if( timer_id_post /= 0 ) call start_itimer(timer_id_post)
       CALL solve_nlgs(i_post)
       CALL Nullify_EntityList_nlgs
call MPI_BARRIER(MPI_COMM_WORLD, code_MPI)
       if( timer_id_post /= 0 ) call stop_itimer(timer_id_post)

       ! just in case
       if( allocated( list_of_contacts_interface ) ) then
          deallocate( list_of_contacts_interface  )
       end if

  END SUBROUTINE ExSolver3D_DDM_ExternalFEM


end module wrap_DDM_ExternalFEM
