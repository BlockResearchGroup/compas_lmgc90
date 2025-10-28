!==========================================================================
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

  
  !=======================================================================!
  !============================ IQS_CLB ==================================!

  subroutine iqs_clb_prep(inter, is_init)
    implicit none
    type(T_interaction) :: inter
    logical, intent(in) :: is_init

    inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)

  end subroutine

  ! solver is mu_clb_solver_2d

!!  !=======================================================================!
!!  !============================ IQS_CLB_nosldt ===========================!
!!
!!  subroutine iqs_clb_nosldt_prep_2d(inter, is_init)
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    inter%covfree(2) = inter%gapTTbegin/H
!!
!!    inter%internal(1) = inter%area
!!
!!    if( NSTEP == 1 ) inter%internal(2) = 0.d0
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_nosldt_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    use tact_behaviour, only : get_offset
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!    !
!!    real(kind=8) :: bt
!!
!!    call mu_clb_solver_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!
!!    call get_offset(inter%lawnb,bt)
!!    if( stat == 'noctc' ) then
!!
!!       vliki(1) = vlfik(1)
!!
!!       if (dabs(inter%internal(2) + (H*vliki(2))) >= bt) then
!!         ! print*,'on bloque'
!!         vliki(2) = 0.d0
!!         rliki(2) = -vlfik(1) / WWik(1,1)
!!         stat     ='stick'
!!       end if
!!
!!    else
!!
!!       vliki(1) = vlfik(1) + WWik(1,2)*rliki(2) + WWik(1,1) * rliki(1)
!!
!!       if (dabs(inter%internal(2) + (H*vliki(2))) >= bt) then
!!         ! print*,'on bloque'
!!         vliki(2) = 0.d0
!!
!!         rliki(2) = (-WWik(1,1)*vlfik(2) + WWik(2,1)*vlfik(1))/inter%det
!!         rliki(1) = (-WWik(2,2)*vlfik(1) + WWik(1,2)*vlfik(2))/inter%det
!!
!!         if (rlik(2) < 0.d0) then
!!           rliki(2) = 0.d0
!!           rliki(1) = -vlfik(1)/WWik(1,1)
!!         end if
!!         stat = 'stick'
!!
!!       end if
!!    endif
!!
!!  end subroutine
!!
!!  !=======================================================================!
!!  !============================ IQS_CLB_noslds ===========================!
!!
!!  ! nothing  yet
!!
!!  !=======================================================================!
!!  !============================ IQS_CLB_RGR ==============================!
!!
!!  subroutine iqs_clb_rgr_prep(inter, is_init)
!!    use tact_behaviour, only : get_gap_tol, get_ToverH
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gap_tol, ToverH, OTH
!!
!!    call get_gap_tol(inter%lawnb,gap_tol)
!!
!!    if( inter%gapTTbegin >= gap_tol ) then
!!      inter%covfree(2)  = max(0.d0,inter%gapTTbegin/H)
!!      inter%corl(2)     = 0.d0
!!    else
!!      call get_ToverH(inter%lawnb,ToverH)
!!      OTH = Oneover2pi*ToverH
!!      inter%corl(2)    =-(2.d0/(inter%W(2,2)*OTH*OTH))*((inter%gapTTbegin-gap_tol)/H)
!!      inter%covfree(:) = inter%W(:,2)*inter%corl(2)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_rgr_prep_diag(inter, is_init)
!!    use tact_behaviour, only : get_gap_tol, get_ToverH
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gap_tol, ToverH, OTH
!!
!!    call get_gap_tol(inter%lawnb,gap_tol)
!!
!!    if( inter%gapTTbegin >= gap_tol ) then
!!      inter%covfree(2)  = max(0.d0,inter%gapTTbegin/H)
!!      inter%corl(2)     = 0.d0
!!    else
!!      call get_ToverH(inter%lawnb,ToverH)
!!      OTH = Oneover2pi*ToverH
!!      inter%corl(2)    =-(2.d0/(inter%W(2,2)*OTH*OTH))*((inter%gapTTbegin-gap_tol)/H)
!!      inter%covfree(2) = inter%W(2,2)*inter%corl(2)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_rgr_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    call mu_sc_std_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!
!!    rliki(2) = rliki(2) + inter%corl(2)
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_rgr_iter_3d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    call mu_ng_iter_3d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!
!!    rliki(2) = rliki(2) + inter%corl(2)
!!
!!  end subroutine
!!
!!  !=======================================================================!
!!  !============================ IQS_DS_CLB ===============================!
!!
!!  ! like iqs_clb
!!
!!  !=======================================================================!
!!  !============================ IQS_WET_DS_CLB ===========================!
!!
!!  subroutine iqs_wet_ds_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( inter%gapTTbegin <= Wethk ) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!      inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!      inter%corl(2)    = -normalcoh*H
!!    else 
!!      inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine iqs_wet_ds_clb_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( inter%gapTTbegin <= Wethk ) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(2) = -normalcoh*inter%W(2,2)*H + max(0.d0,inter%gapTTbegin/H)
!!      inter%corl(2)    = -normalcoh*H
!!    else 
!!      inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_g0_prep_2d(inter, is_init)
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    if( .not. is_init ) inter%internal(1) = inter%gapTTbegin
!!    inter%covfree(2) = max(0.d0,(inter%gapTTbegin-inter%internal(1))/H)
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_g0_prep_3d(inter, is_init)
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    if( .not. is_init ) inter%internal(1) = max(0.d0,inter%gapTTbegin)
!!    inter%covfree(2) = max(0.d0,(inter%gapTTbegin-inter%internal(1))/H)
!!
!!  end subroutine
!!
!!  subroutine iqs_map_prep_2d(inter, is_init)
!!    use tact_behaviour, only : get_fric
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: fric
!!
!!    inter%fric        = get_fric(inter%lawnb,inter%statusBEGIN)
!!    inter%internal(1) = inter%fric
!!
!!    inter%covfree(2)  = max(0.d0,inter%gapTTbegin/H)
!!
!!  end subroutine
!!
!!  subroutine rst_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_rst
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    integer(kind=4) :: i_dim
!!    real(kind=8)    :: tangalrest, normalrest
!!
!!    if (inter%gapTTbegin .le. 0.d0) then
!!       call get_rst(inter%lawnb,tangalrest,normalrest)
!!       do i_dim = 1, inter_dim
!!         if( i_dim == 2) then
!!           inter%covfree(i_dim) = normalrest*inter%vlBEGIN(i_dim)
!!         else
!!           inter%covfree(i_dim) = tangalrest*inter%vlBEGIN(i_dim)
!!         end if
!!       end do
!!    else
!!       inter%forecast        = 'noact'
!!       inter%status          = 'noctc'
!!       inter%rl(1:inter_dim) = 0.d0
!!       inter%covfree(2)      = 0.d0
!!    end if
!!
!!  end subroutine
!!
!!  subroutine rst_wet_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_rst, get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    integer(kind=4) :: i_dim
!!    real(kind=8)    :: normalrest, tangalrest, normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( inter%gapTTbegin <= 0.d0 ) then
!!
!!      call change_status_wet(inter%statusBEGIN)
!!      call get_rst(inter%lawnb,tangalrest,normalrest)
!!
!!      do i_dim = 1, inter_dim
!!        if( i_dim == 2 ) then
!!          inter%covfree(i_dim) = normalrest*inter%vlBEGIN(i_dim)-normalcoh*inter%W(i_dim,2)*H
!!        else
!!          inter%covfree(i_dim) = tangalrest*inter%vlBEGIN(i_dim)-normalcoh*inter%W(i_dim,2)*H
!!        end if
!!      end do
!!      inter%corl(2) = - normalcoh*H
!!
!!    else if(inter%gapTTbegin <= Wethk) then
!!
!!      call change_status_wet(inter%statusBEGIN)
!!
!!      do i_dim = 1, inter_dim
!!        if( i_dim == 2 ) then
!!          inter%covfree(i_dim) = normalrest*inter%vlBEGIN(i_dim)
!!        else
!!          inter%covfree(i_dim) = tangalrest*inter%vlBEGIN(i_dim)
!!        end if
!!      end do
!!      inter%corl(2) = - normalcoh*H
!!
!!    else 
!!
!!       inter%forecast = 'noact'
!!       inter%status   = 'noctc'
!!       inter%rl(:)    = 0.d0
!!
!!    end if
!!
!!  end subroutine
!!
!!  subroutine rst_wet_clb_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_rst, get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    integer(kind=4) :: i_dim
!!    real(kind=8)    :: normalrest, tangalrest, normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( inter%gapTTbegin <= 0.d0 ) then
!!
!!      call change_status_wet(inter%statusBEGIN)
!!      call get_rst(inter%lawnb,tangalrest,normalrest)
!!
!!      inter%covfree(2) = normalrest*inter%vlBEGIN(2)-normalcoh*inter%W(2,2)*H
!!      inter%corl(2)    = - normalcoh*H
!!
!!    else if(inter%gapTTbegin <= Wethk) then
!!
!!      call change_status_wet(inter%statusBEGIN)
!!
!!      do i_dim = 1, inter_dim
!!        if( i_dim == 2 ) then
!!          inter%covfree(i_dim) = normalrest*inter%vlBEGIN(i_dim)
!!        else
!!          inter%covfree(i_dim) = tangalrest*inter%vlBEGIN(i_dim)
!!        end if
!!      end do
!!      inter%corl(2) = - normalcoh*H
!!
!!    else 
!!
!!       inter%forecast = 'noact'
!!       inter%status   = 'noctc'
!!       inter%rl(:)    = 0.d0
!!
!!    end if
!!
!!  end subroutine
!!

  !=======================================================================!
  !========================== GAP_SGR_CLB ================================!

  subroutine gap_sgr_clb_prep(inter, is_init)
    implicit none
    type(T_interaction) :: inter
    logical, intent(in) :: is_init

    inter%covfree(2) = inter%gapTTbegin/H

    ! used only in output file
    inter%internal(1) = inter%area

  end subroutine

!!  subroutine pregap_sgr_clb_prep_2d(inter, is_init)
!!    use tact_behaviour, only : get_pregap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: pre_gap
!!
!!    call get_pregap(inter%lawnb, pre_gap)
!!    inter%covfree(2) = (inter%gapTTbegin+pre_gap)/H
!!
!!    inter%internal(1) = inter%area
!!
!!  end subroutine
!!
!!  subroutine vel_sgr_clb_prep(inter, is_init)
!!    !use tact_behaviour, only : get_gap_tol, get_ToverH
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    if (inter%gapTTbegin > 0.d0) then
!!      inter%forecast= 'noact'
!!      inter%status  = 'noctc'
!!      inter%rl(:)   = 0.d0
!!    end if
!!
!!    inter%internal(1) = inter%area
!!
!!  end subroutine
!!
!!  subroutine gap_wet_ds_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, area
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( inter%gapTTbegin <= Wethk ) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!      inter%covfree(2) =  inter%covfree(2) + inter%gapTTbegin/H
!!      inter%corl(2)    = -normalcoh*H
!!    else 
!!      inter%covfree(2) = inter%gapTTbegin/H
!!    end if
!!
!!    inter%internal(1) = inter%area
!!
!!  end subroutine
!!
!!  subroutine gap_wet_ds_clb_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( inter%gapTTbegin <= Wethk ) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(2) = -normalcoh*inter%W(2,2)*H + inter%gapTTbegin/H
!!      inter%corl(2)    = -normalcoh*H
!!    else 
!!      inter%covfree(2) = inter%gapTTbegin/H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine xqs_wet_clb_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if( .not. is_init ) then
!!      if( inter%gapTTbegin <= Wethk ) then
!!        if (inter%statusBEGIN == 'nknow')then
!!          inter%statusBEGIN='Wnnow'
!!        else if (inter%statusBEGIN == 'noctc')then
!!          inter%statusBEGIN='Wnctc'
!!        else if (inter%statusBEGIN == 'stick')then
!!          inter%statusBEGIN='Wstck'
!!        else if (inter%statusBEGIN == 'slibw')then
!!          inter%statusBEGIN='Wslbw'
!!        else if (inter%statusBEGIN == 'slifw')then
!!          inter%statusBEGIN='Wslfw'
!!        end if
!!        inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!        inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!        inter%corl(2)    = -normalcoh*H
!!      else 
!!        inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!      end if
!!    else 
!!      if (inter%statusBEGIN(1:1) == 'W'.and. inter%gapTTbegin .le. Wethk) then
!!        inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!        inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!        inter%corl(2)    = -normalcoh*H
!!      else 
!!         if (inter%statusBEGIN == 'Wnnow')then
!!            inter%statusBEGIN='nknow'
!!         else if (inter%statusBEGIN == 'Wnctc')then
!!            inter%statusBEGIN='noctc'
!!         else if (inter%statusBEGIN == 'Wstck')then
!!            inter%statusBEGIN='stick'
!!         else if (inter%statusBEGIN == 'Wslbw')then
!!            inter%statusBEGIN='slibw'
!!         else if (inter%statusBEGIN == 'Wslfw')then
!!            inter%statusBEGIN='slifw'
!!         end if
!!         inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!      end if
!!    end if
!!
!!  end subroutine
!!
!!  subroutine iqs_mohr_ds_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_fric, get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    if (Nstep == 1 .and. inter%statusBEGIN == 'nknow') then
!!       inter%statusBEGIN='Mstck'
!!       inter%fric = get_fric(inter%lawnb,inter%statusBEGIN)
!!    end if
!!
!!    if (inter%statusBEGIN == 'Mstck') then
!!
!!       call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!       ! here Wethk is inactive
!!       inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!       inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!       inter%corl(2)    = -normalcoh*H
!!    else
!!       inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine gap_mohr_ds_clb_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_fric, get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    if (Nstep == 1 .and. inter%statusBEGIN == 'nknow') then
!!       inter%statusBEGIN='Mstck'
!!       inter%fric = get_fric(inter%lawnb,inter%statusBEGIN)
!!    end if
!!
!!    if (inter%statusBEGIN == 'Mstck') then
!!
!!       call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!       ! here Wethk is inactive
!!       inter%covfree(:) = -normalcoh*inter%area*inter%W(:,2)*H
!!       inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!       inter%corl(2)    = -normalcoh*inter%area*H
!!    else
!!       inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine gap_cap_mohr_ds_clb_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_fric, get_fric_cap, get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    if (Nstep == 1 .and. inter%statusBEGIN == 'nknow') then
!!       inter%statusBEGIN='Mstck'
!!       inter%fric = get_fric(inter%lawnb,inter%statusBEGIN)
!!    end if
!!
!!    if (inter%statusBEGIN == 'Mstck') then
!!
!!       inter%fric = get_fric_cap(inter%lawnb,inter%rl(2))
!!
!!       call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!       ! here Wethk is inactive
!!       inter%covfree(:) = -normalcoh*inter%area*inter%W(:,2)*H
!!       inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!       inter%corl(2)    = -normalcoh*inter%area*H
!!    else
!!       inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine elastic_repell_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: forcePERgap, ToverH, vOVERcv, OTH
!!
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!    inter%WW(2)      = inter%W(2,2) + 1.d0/(forcePERgap*H*H)
!!    inter%covfree(2) = inter%gapTTbegin/H
!!
!!  end subroutine
!!
!!  subroutine elastic_repell_clb_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: forcePERgap, ToverH, vOVERcv, OTH
!!
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!    inter%WW(2)      = inter%W(2,2) + 1.d0/(forcePERgap*H*H)
!!    inter%covfree(2) = inter%gapTTbegin/H
!!    inter%invW(2)    = 1.d0 / inter%WW(2)
!!
!!  end subroutine
!!
!!  subroutine critical_voigt_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_ToverH, get_viscOVERcritvisc
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: ToverH, vOVERcv, OTH
!!
!!    call get_ToverH(inter%lawnb,ToverH)
!!    call get_viscOVERcritvisc(inter%lawnb,vOVERcv)
!!    OTH=Oneover2pi*ToverH
!!
!!    inter%WW(2)      =  inter%W(2,2) * (1.d0+0.5d0*(OTH*OTH/(1.d0+2.d0*vOVERcv*OTH)))
!!    inter%covfree(2) = (inter%gapTTbegin/H) * (1.d0/(1.d0+2.d0*vOVERcv*OTH))
!!
!!    if (inter%statusBEGIN == "stick") then
!!      inter%WW(1)      =  inter%W(1,1) * (1.d0+0.5d0*(OTH*OTH/(1.d0+2.d0*vOVERcv*OTH)))
!!      inter%WW(3)      =  inter%W(3,3) * (1.d0+0.5d0*(OTH*OTH/(1.d0+2.d0*vOVERcv*OTH)))
!!      inter%covfree(1) = -inter%rl(1)      * inter%W(1,1)*0.5d0*OTH*OTH/(1.d0+2.d0*vOVERcv*OTH) &
!!                         -inter%vlBEGIN(1) * 2.d0*vOVERcv*OTH/(1.d0+2.d0*vOVERcv*OTH)
!!      inter%covfree(3) = -inter%rl(3)      * inter%W(3,3)*0.5d0*OTH*OTH/(1.d0+2.d0*vOVERcv*OTH) &
!!                         -inter%vlBEGIN(3) * 2.d0*vOVERcv*OTH/(1.d0+2.d0*vOVERcv*OTH)
!!    else ! v is set to zero
!!      inter%WW(1)      =  inter%W(1,1) * (1.d0+0.5d0*(OTH*OTH))
!!      inter%WW(3)      =  inter%W(3,3) * (1.d0+0.5d0*(OTH*OTH))
!!      inter%covfree(1) = -inter%rl(1)  * inter%W(1,1) * 0.5d0*(OTH*OTH)
!!      inter%covfree(3) = -inter%rl(3)  * inter%W(3,3) * 0.5d0*(OTH*OTH)
!!    end if   
!!
!!  end subroutine
!!
!!  subroutine critical_voigt_clb_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_ToverH, get_viscOVERcritvisc
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    call faterr('critical_voigt_clb_prep_diag','no diagonal resolution with this law')
!!
!!  end subroutine
!!
!!  subroutine elastic_repell_wet_clb_prep(inter, is_init)
!!    use tact_behaviour, only: get_coh, get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!    inter%WW(2)      = inter%W(2,2) + 1.d0/(forcePERgap*H*H)
!!    inter%covfree(2) = inter%gapTTbegin/H
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(:) = inter%covfree(:) - normalcoh*inter%W(:,2)*H
!!      inter%corl(2)    =-normalcoh*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine elastic_repell_wet_clb_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_coh, get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!    inter%WW(2)      = inter%W(2,2) + 1.d0/(forcePERgap*H*H)
!!    inter%covfree(2) = inter%gapTTbegin/H
!!    inter%invW(2)    = 1.d0 / inter%WW(2)
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(2) = inter%covfree(2) - normalcoh*inter%W(2,2)*H
!!      inter%corl(2)    =-normalcoh*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine elastic_repell_mac_czm_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_coh, get_forcePERgap, init_czm, prep_czm
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, un
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!
!!    if (.not. is_init) then
!!      if (inter%statusBEGIN == 'nknow') inter%statusBEGIN='Cnnow'
!!      call init_CZM(inter%lawnb,inter%internal) 
!!    end if
!!
!!    if (inter%gapTTbegin <= Wethk) then
!!      !inter%WWnn    = inter%Wnn+1.d0/(forcePERgap*H*H)
!!      inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!      call prep_CZM(inter%lawnb,inter%internal,0.d0,0.d0, inter%gapTTbegin)
!!      if (inter%statusBEGIN(1:1) == 'C') then
!!        write(inter%statusBEGIN(1:1),'(A1)') 'E'
!!      else if (inter%statusBEGIN == 'noctc') then
!!        inter%statusBEGIN='Enctc'
!!      else if (inter%statusBEGIN == 'stick') then
!!        inter%statusBEGIN='Estck'
!!      else if (inter%statusBEGIN == 'slibw') then
!!        inter%statusBEGIN='Eslbw'
!!      else if (inter%statusBEGIN == 'slifw') then
!!        inter%statusBEGIN='Eslfw'
!!      end if
!!     !*********************
!!    else if ((inter%gapTTbegin <= 0.d0) .and. (inter%gapTTbegin > Wethk)) then
!!      inter%WW(2)      = inter%W(2,2) + 1.d0/(forcePERgap*H*H)
!!      inter%covfree(2) = inter%gapTTbegin/H
!!      call prep_CZM(inter%lawnb,inter%internal,0.d0,0.d0,inter%gapTTbegin)
!!
!!      if (inter%statusBEGIN(1:1) == 'C') then
!!        write(inter%statusBEGIN(1:1),'(A1)') 'E'
!!      else if (inter%statusBEGIN == 'noctc') then
!!        inter%statusBEGIN='Enctc'
!!      else if (inter%statusBEGIN == 'stick') then
!!        inter%statusBEGIN='Estck'
!!      else if (inter%statusBEGIN == 'slibw') then
!!        inter%statusBEGIN='Eslbw'
!!      else if (inter%statusBEGIN == 'slifw') then
!!        inter%statusBEGIN='Eslfw'
!!      end if
!!    end if
!!    !*********************
!!    if ((inter%gapTTbegin >= 0.d0)) then
!!       ! is_cohesive = .TRUE.
!!       un = max(0.d0,inter%gapTTbegin) 
!!       call prep_CZM(inter%lawnb,inter%internal,inter%area,0.d0,un)
!!       inter%covfree(1) = 0.d0
!!       inter%covfree(2) = un/H    
!!       if (inter%statusBEGIN(1:1) == 'E') then
!!         write(inter%statusBEGIN(1:1),'(A1)') 'C'
!!       end if
!!    end if
!!  end subroutine
!!
!!  subroutine visco_elastic_repell_wet_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_coh, get_forcePERgap, get_forcePERstrainrate
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, forcePERstrainrate, etan, OTH
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!    call get_forcePERstrainrate(inter%lawnb,forcePERstrainrate)
!!
!!    call faterr('visco_elastic_repell_wet_prep_2d','cannot get eff')
!!
!!    !etan = forcePERstrainrate*sqrt(inter%reff)
!!    OTH  = H*(forcePERgap*H+etan)
!!
!!    inter%WW(2)       = inter%W(2,2) + 1.d0/OTH
!!    inter%covfree(2)  = inter%gapTTbegin/H
!!    inter%internal(:) = 0.d0
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(:) = inter%covfree(:) - normalcoh*inter%W(:,2)*H
!!      inter%corl(2)    =-normalcoh*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine visco_elastic_repell_wet_prep_3d(inter, is_init)
!!    use tact_behaviour, only: get_coh, get_forcePERgap, get_viscosity
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, etan, etat, OTH
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_viscosity(inter%lawnb,etan,etat)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!
!!    call faterr('visco_elastic_repell_wet_prep_3d','cannot get eff')
!!
!!    !etan = forcePERstrainrate*sqrt(inter%reff)
!!    OTH  = H*(forcePERgap*H+etan)
!!
!!    inter%WW(2)       = inter%W(2,2) + 1.d0/OTH
!!    inter%covfree(2)  = inter%gapTTbegin * ( 1.d0 + etan/(forcePERgap*H+etan) )/H
!!    inter%internal(:) = 0.d0
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      normalcoh = normalcoh + abs(inter%vlBEGIN(2))*etan
!!      inter%covfree(:) = inter%covfree(:) - normalcoh*inter%W(:,2)*H
!!      inter%corl(2)    =-normalcoh*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine visco_elastic_repell_wet_prep_3d_diag(inter, is_init)
!!    use tact_behaviour, only: get_coh, get_forcePERgap, get_viscosity
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, etan, etat, OTH
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_viscosity(inter%lawnb,etan,etat)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!
!!    call faterr('visco_elastic_repell_wet_prep_3d','cannot get eff')
!!
!!    !etan = forcePERstrainrate*sqrt(inter%reff)
!!    OTH  = H*(forcePERgap*H+etan)
!!
!!    inter%WW(2)       = inter%W(2,2) + 1.d0/OTH
!!    inter%covfree(2)  = inter%gapTTbegin * ( 1.d0 + etan/(forcePERgap*H+etan) )/H
!!    inter%invW(2)     = 1.d0 / inter%WW(2)
!!    inter%internal(:) = 0.d0
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      normalcoh = normalcoh + abs(inter%vlBEGIN(2))*etan
!!      inter%covfree(2) = inter%covfree(2) - normalcoh*inter%W(2,2)*H
!!      inter%corl(2)    =-normalcoh*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine skf_grease_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_viscosity, get_coh, get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, etan, etat
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_viscosity(inter%lawnb,etan,etat)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!
!!    call faterr('skf_grease_prep_2d','cannot get eff')
!!
!!    !inter%WW(2)      = inter%W(2,2) + 1.d0 / (H*(forcePERgap*H+inter%meff*etan))
!!    !inter%covfree(2) = inter%gapTTbegin*forcePERgap / (forcePERgap*H+inter%meff*etan)
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(:) = inter%covfree(:) - normalcoh*inter%W(:,2)*H
!!      inter%corl(2)    =-normalcoh*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine skf_grease_prep_3d(inter, is_init)
!!    use tact_behaviour, only: get_viscosity, get_coh, get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, etan, etat
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_viscosity(inter%lawnb,etan,etat)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!
!!    inter%WW(2)      = inter%W(2,2) + 1.d0 / (H*(forcePERgap*H+etan))
!!    inter%covfree(2) = inter%gapTTbegin * (1.d0 + etan/(forcePERgap*H+etan))/H
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(1) = inter%vlBEGIN(1)*etat*inter%W(1,1)*H - ( normalcoh*inter%W(1,2)-inter%vlBEGIN(3)*etat*inter%W(1,3) )*H
!!      inter%covfree(3) = inter%vlBEGIN(3)*etat*inter%W(3,3)*H - ( normalcoh*inter%W(3,2)-inter%vlBEGIN(1)*etat*inter%W(3,1) )*H
!!      inter%covfree(2) = inter%covfree(2) - normalcoh*inter%W(2,2)*H  &
!!                        - ( inter%vlBEGIN(1)*inter%W(2,1) + inter%vlBEGIN(3)*inter%W(2,3) ) *etat*H
!!      inter%corl(1)    = inter%vlBEGIN(1)*etat*H
!!      inter%corl(2)    =-normalcoh*H
!!      inter%corl(3)    = inter%vlBEGIN(3)*etat*H
!!
!!      inter%internal(1) = normalcoh
!!      inter%internal(2) = inter%fric
!!      inter%internal(3) =-inter%vlBEGIN(3)*etat*H
!!      inter%internal(4) =-inter%vlBEGIN(1)*etat*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine skf_grease_prep_3d_diag(inter, is_init)
!!    use tact_behaviour, only: get_viscosity, get_coh, get_forcePERgap
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, forcePERgap, etan, etat
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!    call get_viscosity(inter%lawnb,etan,etat)
!!    call get_forcePERgap(inter%lawnb,forcePERgap)
!!
!!    inter%WW(2)      = inter%W(2,2) + 1.d0 / (H*(forcePERgap*H+etan))
!!    inter%covfree(2) = inter%gapTTbegin * (1.d0 + etan/(forcePERgap*H+etan))/H
!!    inter%invW(2)    = 1.d0 / inter%WW(2)
!!
!!    if (inter%gapTTbegin .le. Wethk) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(1) = inter%vlBEGIN(1)*etat*inter%W(1,1)*H
!!      inter%covfree(3) = inter%vlBEGIN(3)*etat*inter%W(3,3)*H
!!      inter%covfree(2) = inter%covfree(2) - normalcoh*inter%W(2,2)*H  &
!!                        - ( inter%vlBEGIN(1)*inter%W(2,1) + inter%vlBEGIN(3)*inter%W(2,3) ) *etat*H
!!
!!      inter%corl(1)    = inter%vlBEGIN(1)*etat*H
!!      inter%corl(2)    =-normalcoh*H
!!      inter%corl(3)    = inter%vlBEGIN(3)*etat*H
!!
!!      inter%internal(1) = normalcoh
!!      inter%internal(2) = inter%fric
!!      inter%internal(3) =-inter%vlBEGIN(3)*etat*H
!!      inter%internal(4) =-inter%vlBEGIN(1)*etat*H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine elastic_prep(inter, is_init)
!!    use tact_behaviour, only: get_forcePERstrain, get_prestrain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF, forcePERstrain, prestrain
!!
!!    if( .not. is_init .and. inter%internal(1) == 0.d0 ) inter%internal(1) = inter%gapTTbegin
!!
!!    gapREF = inter%internal(1)
!!    
!!    if (gapREF .le. 1.D-18) then
!!      call faterr('elastic_prep','gapREF too small')
!!    end if
!!    
!!    call get_forcePERstrain(inter%lawnb,forcePERstrain)
!!    call get_prestrain(inter%lawnb,prestrain)
!!
!!    inter%WW(2)      = inter%W(2,2) + (gapREF/(forcePERstrain*H*H))
!!    inter%covfree(2) = (inter%gapTTbegin-(1.d0+prestrain)*gapREF)/H
!!
!!  end subroutine
!!
!!  subroutine elastic_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_forcePERstrain, get_prestrain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF, forcePERstrain, prestrain
!!
!!    if( .not. is_init .and. inter%internal(1) == 0.d0 ) inter%internal(1) = inter%gapTTbegin
!!
!!    gapREF = inter%internal(1)
!!    
!!    if (gapREF .le. 1.D-18) then
!!      call faterr('elastic_prep_diag','gapREF too small')
!!    end if
!!    
!!    call get_forcePERstrain(inter%lawnb,forcePERstrain)
!!    call get_prestrain(inter%lawnb,prestrain)
!!
!!    inter%WW(2)      = inter%W(2,2) + (gapREF/(forcePERstrain*H*H))
!!    inter%covfree(2) = (inter%gapTTbegin-(1.d0+prestrain)*gapREF)/H
!!    inter%invW(2)    = 1.d0 / inter%WW(2)
!!
!!  end subroutine
!!
!!  subroutine brittle_elastic_wire_prep(inter, is_init)
!!    use tact_behaviour, only: get_forcePERstrain, get_prestrain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF, forcePERstrain, prestrain
!!
!!    if (inter%statusBEGIN /= 'vnish' ) then
!!
!!      gapREF = inter%internal(1)
!!      if (gapREF .le. 1.D-18) then
!!        call faterr('brittle_elastic_wire_prep','gapREF too small')
!!      end if
!!    
!!      call get_forcePERstrain(inter%lawnb,forcePERstrain)
!!      call get_prestrain(inter%lawnb,prestrain)
!!
!!      inter%WW(2)      = inter%W(2,2) + (gapREF/(forcePERstrain*H*H))
!!      inter%covfree(2) = (inter%gapTTbegin-(1.d0+prestrain)*gapREF)/H
!!
!!    else
!!
!!      inter%forecast = 'noact'
!!      inter%status   = 'vnish'
!!      inter%rl(:)    = 0.d0
!!
!!    end if
!!
!!  end subroutine
!!
!!  subroutine brittle_elastic_wire_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_forcePERstrain, get_prestrain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF, forcePERstrain, prestrain
!!
!!    if (inter%statusBEGIN /= 'vnish' ) then
!!
!!      gapREF = inter%internal(1)
!!      if (gapREF .le. 1.D-18) then
!!        call faterr('brittle_elastic_wire_prep','gapREF too small')
!!      end if
!!    
!!      call get_forcePERstrain(inter%lawnb,forcePERstrain)
!!      call get_prestrain(inter%lawnb,prestrain)
!!
!!      inter%WW(2)      = inter%W(2,2) + (gapREF/(forcePERstrain*H*H))
!!      inter%covfree(2) = (inter%gapTTbegin-(1.d0+prestrain)*gapREF)/H
!!      inter%invW(2)     = inter%WW(2)
!!
!!    else
!!
!!      inter%forecast = 'noact'
!!      inter%status   = 'vnish'
!!      inter%rl(:)    = 0.d0
!!
!!    end if
!!
!!  end subroutine
!!
!!  subroutine voigt_rod_prep(inter, is_init)
!!    use tact_behaviour, only: get_forcePERstrain, get_forcePERstrainrate, get_prestrain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF, forcePERstrain, forcePERstrainrate, prestrain
!!
!!    gapREF = inter%internal(1)
!!    if (gapREF .le. 1.D-18) then
!!      call faterr('voigt_prep','gapREF too small')
!!    end if
!!    
!!    call get_forcePERstrain(inter%lawnb,forcePERstrain)
!!    call get_prestrain(inter%lawnb,prestrain)
!!    call get_forcePERstrainrate(inter%lawnb,forcePERstrainrate)
!!
!!    inter%WW(2)      = inter%W(2,2) + (gapREF/(forcePERstrain*H*H+forcePERstrainrate*H))
!!    inter%covfree(2) = H*forcePERstrain/(H*forcePERstrain+forcePERstrainrate) * &
!!                       (inter%gapTTbegin-(1.d0+prestrain)*gapREF)/H
!!
!!  end subroutine
!!
!!  subroutine voigt_rod_prep_diag(inter, is_init)
!!    use tact_behaviour, only: get_forcePERstrain, get_forcePERstrainrate, get_prestrain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF, forcePERstrain, forcePERstrainrate, prestrain
!!
!!    gapREF = inter%internal(1)
!!    if (gapREF .le. 1.D-18) then
!!      call faterr('voigt_prep_diag','gapREF too small')
!!    end if
!!    
!!    call get_forcePERstrain(inter%lawnb,forcePERstrain)
!!    call get_prestrain(inter%lawnb,prestrain)
!!    call get_forcePERstrainrate(inter%lawnb,forcePERstrainrate)
!!
!!    inter%WW(2)      = inter%W(2,2) + (gapREF/(forcePERstrain*H*H+forcePERstrainrate*H))
!!    inter%covfree(2) = H*forcePERstrain/(H*forcePERstrain+forcePERstrainrate) * &
!!                       (inter%gapTTbegin-(1.d0+prestrain)*gapREF)/H
!!    inter%invW(2)    = 1.d0 / inter%WW(2)
!!
!!  end subroutine
!!
!!  subroutine tex_sol_prep_2d(inter, is_init)
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    inter%covfree(2) = 0.d0
!!
!!  end subroutine
!!
!!  subroutine rigid_wire_prep(inter, is_init)
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: gapREF
!!
!!    gapREF = inter%internal(1)
!!    inter%covfree(2) = ( inter%gapTTbegin - gapREF ) /H
!!
!!  end subroutine
!!
!!  subroutine czm_prep_2d(inter, is_init)
!!    use tact_behaviour, only: init_czm, prep_czm
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    if (.not. is_init ) then
!!      if( inter%statusBegin == 'nknow' ) inter%statusBegin = 'Cnnow'
!!      call init_CZM(inter%lawnb, inter%internal)
!!    end if
!!
!!    call prep_CZM(inter%lawnb,inter%internal,inter%area,0.d0,inter%gapTTbegin)
!!
!!    inter%covfree(1) = 0.d0
!!    inter%covfree(2) = inter%gapTTbegin/H
!!
!!  end subroutine
!!
!!  subroutine czm_prep_3d(inter, is_init)
!!    use tact_behaviour, only: init_czm_3D, prep_czm_3D
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    if (.not. is_init ) then
!!      if( inter%statusBegin == 'nknow' ) inter%statusBegin = 'Cnnow'
!!      call init_CZM_3D(inter%lawnb, inter%internal)
!!    end if
!!
!!    call prep_CZM_3D(inter%lawnb,inter%internal,inter%area,0.d0,inter%gapTTbegin,0.d0)
!!
!!    inter%covfree(1) = 0.d0
!!    inter%covfree(2) = inter%gapTTbegin/H
!!    inter%covfree(3) = 0.d0
!!
!!  end subroutine
!!
!!  subroutine iqs_czm_prep_2d(inter, is_init)
!!    use tact_behaviour, only: init_czm, prep_czm
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: un
!!
!!    if (.not. is_init ) then
!!      if( inter%statusBegin == 'nknow' ) inter%statusBegin = 'Cnnow'
!!      call init_CZM(inter%lawnb, inter%internal)
!!    end if
!!
!!    un = max(0.d0,inter%gapTTbegin)       
!!    call prep_CZM(inter%lawnb,inter%internal,inter%area,0.d0,un)
!!
!!    inter%covfree(1) = 0.d0
!!    inter%covfree(2) = un / H
!!
!!  end subroutine
!!
!!  subroutine iqs_czm_prep_3d(inter, is_init)
!!    use tact_behaviour, only: init_czm_3D, prep_czm_3D, get_dilatancy_height
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: un
!!
!!    if (.not. is_init ) then
!!      if( inter%statusBegin == 'nknow' ) inter%statusBegin = 'Cnnow'
!!      call init_CZM_3D(inter%lawnb, inter%internal)
!!    end if
!!
!!    un = max(0.d0,inter%gapTTbegin)       
!!    call prep_CZM_3D(inter%lawnb,inter%internal,inter%area,0.d0,max(0.d0,inter%gapTTbegin),0.d0)
!!
!!    inter%covfree(1) = 0.d0
!!    inter%covfree(2) = max(0.d0,inter%gapTTbegin-get_dilatancy_height(inter%lawnb,inter%internal)) / H
!!    inter%covfree(3) = 0.d0
!!
!!  end subroutine
!!
!!  subroutine iqs_wet_czm_prep_2d(inter, is_init)
!!    use tact_behaviour, only : get_coh, init_czm, prep_czm
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk, un
!!
!!    if (.not. is_init ) then
!!      if( inter%statusBegin == 'nknow' ) inter%statusBegin = 'Cnnow'
!!      call init_CZM(inter%lawnb, inter%internal)
!!    end if
!!
!!    un = max(0.d0,inter%gapTTbegin)       
!!    call prep_CZM(inter%lawnb,inter%internal,inter%area,0.d0,un)
!!
!!    if (inter%internal(4)==0.d0) then
!!      call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!      if (inter%gapTTbegin <= Wethk) then
!!        if (inter%statusBEGIN == 'nknow')then
!!          inter%statusBEGIN='Wnnow'
!!        else if (inter%statusBEGIN == 'noctc')then
!!          inter%statusBEGIN='Wnctc'
!!        else if (inter%statusBEGIN == 'stick')then
!!          inter%statusBEGIN='Wstck'
!!        else if (inter%statusBEGIN == 'slibw')then
!!          inter%statusBEGIN='Wslbw'
!!        else if (inter%statusBEGIN == 'slifw')then
!!          inter%statusBEGIN='Wslfw'
!!        end if
!!        inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!        inter%covfree(2) = inter%covfree(2) + un/H
!!        inter%corl(2)    = -normalcoh*H
!!      else 
!!        inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!      end if
!!    else
!!      inter%covfree(1) = 0.d0
!!      inter%covfree(2) = un / H
!!    end if
!!
!!  end subroutine
!!
!!  subroutine postgap_iqs_mac_czm_prep_2d(inter, is_init)
!!    use tact_behaviour, only: init_czm, prep_czm
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: un
!!
!!    if (.not. is_init ) then
!!      if( inter%statusBegin == 'nknow' ) inter%statusBegin = 'Cnnow'
!!      call init_CZM(inter%lawnb, inter%internal)
!!      inter%internal(6) = inter%gapTTbegin
!!    end if
!!
!!    un = max(0.d0,inter%gapTTbegin-inter%internal(6))
!!    call prep_CZM(inter%lawnb,inter%internal,inter%area,0.d0,un)
!!
!!    inter%covfree(1) = 0.d0
!!    inter%covfree(2) = un / H
!!
!!  end subroutine
!!
!!  subroutine gap_sgr_clb_wear_prep_2d(inter, is_init)
!!    use tact_behaviour, only : get_kwear
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    call faterr('gap_sgr_clb_wear_prep_2d','kcdwea and kanwear not in inter')
!!    !call get_kwear(inter%lawnb,inter%kcdwear,inter%kanwear)
!!    inter%covfree(2) = inter%internal(1)
!!
!!  end subroutine
!!
!!  subroutine iqs_bw_clb_prep_2d(inter, is_init)
!!    use tact_behaviour, only : get_fric_BW, get_threshold_BW, get_alpha_BW, get_TshVlt_BW
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: TshVlt, nbc
!!
!!    if ( inter%internal(1) == 0.d0 ) then
!!      inter%internal(1) = H/0.2
!!    else
!!      TshVlt = get_TshVlt_BW(inter%lawnb)
!!      !mr the motion is not relevant to consider an increment of internal(1)
!!      if ( abs(inter%vlBEGIN(1)) >= TshVlt ) then
!!        ! Test are made with a frequency of 5 Hertz thus 1 cycle is performed in 0.2 s.
!!        inter%internal(1) = inter%internal(1) + H/0.2
!!      end if
!!    end if
!!
!!    nbc = inter%internal(1)
!!
!!    inter%fric        = get_fric_BW(inter%lawnb,nbc)
!!    inter%internal(2) = get_threshold_BW(inter%lawnb,nbc)
!!    inter%internal(3) = get_alpha_BW(inter%lawnb)
!!    inter%internal(4) = inter%fric
!!
!!  end subroutine
!!
!!  subroutine iqs_sgr_clb_wear_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_coh
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: normalcoh, tangalcoh, Wethk
!!
!!    call get_coh(inter%lawnb,normalcoh,tangalcoh,Wethk)
!!
!!    if (Nstep == 1)then
!!      inter%internal(1) = normalcoh ! static cohesion
!!      inter%internal(2) = tangalcoh ! dynamic cohesion
!!      inter%internal(3) = 0.d0      ! 0 clean status ( 1 for broken )
!!    end if
!!
!!    if ( inter%internal(3) == 0 ) then 
!!      normalcoh = inter%internal(1)
!!    else
!!      normalcoh = inter%internal(2)
!!    end if
!!
!!    if( inter%gapTTbegin <= Wethk ) then
!!      call change_status_wet(inter%statusBEGIN)
!!      inter%covfree(:) = -normalcoh*inter%W(:,2)*H
!!      inter%covfree(2) =  inter%covfree(2) + max(0.d0,inter%gapTTbegin/H)
!!      inter%corl(2)    = -normalcoh*H
!!    else 
!!      inter%covfree(2) = max(0.d0,inter%gapTTbegin/H)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine broken_dof_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_threshold
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!
!!    call faterr('broken_dof_prep_2d','threshold not in inter')
!!    !call get_threshold(inter%lawnb,inter%threshold)
!!
!!    if (.not. is_init) inter%internal(1) = 1.d0
!!
!!  end subroutine
!!  
!!  subroutine perio_dof_prep_2d(inter, is_init)
!!    use tact_behaviour, only: get_periodic_strain
!!    implicit none
!!    type(T_interaction) :: inter
!!    logical, intent(in) :: is_init
!!    !
!!    real(kind=8) :: E(3), y_x, y_y, ey_x, ey_y
!!
!!    call get_periodic_strain(inter%lawnb,E)
!!
!!    call faterr('perio_dof_prep_2d','nx and ny not in inter')
!!    !y_x  = inter%gapTTbegin * inter%nx
!!    !y_y  = inter%gapTTbegin * inter%ny
!!
!!    !ey_x = E(1)*y_x + E(3)*y_y
!!    !ey_y = E(3)*y_x + E(2)*y_y
!!
!!    !inter%covfree(1) = ey_x*inter%ny - ey_y*inter%nx
!!    !inter%covfree(2) = ey_x*inter%nx + ey_y*inter%ny
!!
!!  end subroutine
!!  
!!  subroutine tex_sol_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    rliki(2) = -vlfik(2) / WWik(2,2)
!!    if( (inter%fric*inter%W(1,1)*rliki(2)+vlfik(1)) < 0.d0 ) then
!!      rliki(2) = inter%fric*rliki(2)
!!    else if ( (-inter%fric*inter%W(1,1)*rliki(2)+vlfik(1)) > 0.d0 ) then
!!      rliki(2) = -inter%fric*rlik(2)
!!    else
!!      rliki(2) = -vlfik(1) / WWik(1,1)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine brittle_iter_diag(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    use tact_behaviour, only : get_snmax
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!    !
!!    real(kind=8) :: snmax
!!
!!    vlfik(2) = vlik(2) + inter%covfree(2) - inter%W(2,2)*rlik(2)
!!
!!    if (vlfik(2) > 0.d0) then
!!
!!      rliki(2) = -vlfik(2)/WWik(2,2)
!!      call get_snmax(inter%lawnb,snmax)
!!
!!      if( rliki(2) < -H*snmax ) then
!!        stat     = 'vnish'
!!        rliki(2) = 0.d0
!!      else
!!        stat = 'stick'
!!      end if
!!    else
!!      rliki(2) = 0.d0
!!      stat     = 'noctc'
!!    end if
!!
!!  end subroutine
!!
!!  subroutine brittle_iter(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    use tact_behaviour, only : get_snmax
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!    !
!!    real(kind=8) :: snmax
!!
!!    if (vlfik(2) > 0.d0) then
!!
!!      rliki(2) = -vlfik(2)/WWik(2,2)
!!      vliki(2) = -inter%covfree(2)
!!
!!      call get_snmax(inter%lawnb,snmax)
!!
!!      if( rliki(2) < -H*snmax ) then
!!        stat     = 'vnish'
!!        rliki(2) = 0.d0
!!        vliki(2) = vlfik(2) - inter%covfree(2)
!!      else
!!        stat = 'stick'
!!      end if
!!    else
!!      rliki(2) = 0.d0
!!      vliki(2) = vlfik(2) - inter%covfree(2)
!!      stat     = 'noctc'
!!    end if
!!
!!  end subroutine
!!
!!  subroutine broken_dof_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    if( inter%internal(1) == 1.d0 ) then
!!
!!      rliki(1) =-WWik(2,2)*vlfik(1)-WWik(1,2)*vlfik(2) / inter%det
!!      rliki(2) = WWik(2,1)*vlfik(1)+WWik(1,1)*vlfik(2) / inter%det
!!
!!      stat = 'stick'
!!
!!      call faterr('broke_dof_iter_2d','threshold data not in interaction')
!!      !if (rliki(2) > inter%threshold) then
!!      !   inter%internal = 0.d0
!!      !   vliki(2) = vlfik(2) - inter%covfree(2)
!!      !   rliki(:) = 0.d0
!!      !   stat     = 'noctc'
!!      !end if
!!
!!    else
!!
!!      vliki(2) = vlfik(2) - inter%covfree(2)
!!      rliki(:) = 0.d0
!!      stat     = 'noctc'
!!
!!    end if
!!
!!  end subroutine
!!
!!  subroutine wet_clb_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    call mu_sc_std_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!
!!    if (stat(1:1) == 'W') then
!!      rliki(2) = rliki(2) + inter%corl(2)
!!    end if
!!    call revert_status_wet(stat)
!!
!!  end subroutine
!!
!!  subroutine mohr_ds_clb_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    use tact_behaviour, only : iter_czm, get_fric_czm
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    call mu_sc_std_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!
!!    if (stat == 'Mstck') then 
!!      rliki(2) = rliki(2) + inter%corl(2)
!!      if (stat == 'stick') stat = 'Mstck'
!!    end if
!!
!!  end subroutine
!!
!!  subroutine skf_grease_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    if (stat(1:1) == 'W') then
!!      rliki(2) = rliki(2) + inter%corl(2)
!!      call revert_status_wet(stat)
!!    end if
!!
!!  end subroutine
!!    
!!  subroutine iqs_bw_clb_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    if ( rliki(2) > inter%internal(2) ) then
!!      rliki(1) = min(rliki(1), inter%internal(3)*rliki(2) &
!!                              -inter%internal(2)*(inter%internal(3)-inter%fric))
!!      call revert_status_wet(stat)
!!    end if
!!
!!  end subroutine
!!    
!!  subroutine mu_sc_viscous_iter_2d(inter, rlik, vlik, vlfik, rliki, vliki, WWik, stat)
!!    implicit none
!!    type(T_interaction) :: inter
!!    !> local reaction at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: rlik
!!    !> local velocity at begining of iteration
!!    real(kind=8), dimension(inter_dim), intent(in)    :: vlik
!!    !> local free velocity at of iteration
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
!!    !> local reaction computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(out)   :: rliki
!!    !> local velocity computed value during iteraction
!!    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
!!    !> during iteration
!!    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) :: WWik
!!    !> status of contact during iteration
!!    character(len=5), intent(inout) :: stat
!!
!!    DFT = WWik(2,2)*vlfik(1)
!!    DFN = WWik(1,1)*vlfik(2)
!!    FFN =-vlfik(2) / WWik(2,2)
!!    Cforward  = DFT+inter%fric*DFN
!!    Cbackward = DFT-inter%fric*DFN
!!    
!!    NUT = vlfik(1)*inter%internal(2)/(1.d0+WWik(1,1)*inter%internal(2))
!!
!!    if (vlfik(2) >= 0.d0) then
!!       !no contact
!!       rliki(:) = 0.d0
!!       stat     ='noctc'
!!    else if (vlfik(2) < 0.d0 .and. Cforward >= 0.d0) then
!!       !sliding forward
!!       rliki(2) = FFN/inter%forward
!!       rliki(1) =-inter%fric*rliki(1) - NUT
!!       stat     = 'slifw'                    
!!    else if (vlfik(2) < 0.d0 .and. Cbackward < 0.d0) then
!!       !sliding backward
!!       rliki(2) = FFN/inter%backward
!!       rliki(1) = inter%fric*rliki(2) - NUT
!!       stat     = 'slibw'                    
!!    else if (vlfik(2) < 0.d0 .and. Cforward <= 0.d0 .and. Cbackward >= 0.d0) then
!!       !sticking
!!       rliki(2) =-vlfik(2) /WWik(2,2)
!!       rliki(1) =-vlfik(1) /WWik(1,1)
!!       stat     = 'stick'
!!    else
!!       rliki(:) = 0.d0
!!       stat     = 'vnish'                    
!!    end if
!! 
!!    if (stat(1:1) == 'W') then
!!      rliki(2) = rliki(2) + inter%corl(2)
!!      call revert_status_wet(stat)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine czm_post_2d(inter)
!!    use tact_behav, only : updt_CZM, raz_CZM
!!    !> interaction
!!    type(T_interaction) :: inter
!!
!!    if (inter%status(1:1) == 'C') then 
!!      call updt_CZM(inter%lawnb,.true.,inter%internal,H*inter%vl(1),H*inter%vl(2))
!!    else
!!      call raz_CZM(inter%lawnb,inter%internal)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine czm_post_3d(inter)
!!    use tact_behav, only : updt_CZM_3D, raz_CZM
!!    !> interaction
!!    type(T_interaction) :: inter
!!
!!    if (inter%status(1:1) == 'C') then 
!!      call updt_CZM_3D(inter%lawnb,.true.,inter%internal,H*inter%vl(1),H*inter%vl(2),H*inter%vl(inter_dim))
!!    else
!!      call raz_CZM(inter%lawnb,inter%internal)
!!    end if
!!
!!  end subroutine
!!
!!  subroutine postgap_iqs_czm_post(inter)
!!    !> interaction
!!    type(T_interaction) :: inter
!!
!!    call czm_post_2d(inter)
!!
!!    ! why 0.002 ?
!!    inter%internal(6) = (-0.002)* (1.d0 - inter%internal(4))
!!
!!  end subroutine
!!
!!  subroutine postgap_iqs_czm_post(inter)
!!    !> interaction
!!    type(T_interaction) :: inter
!!
!!    if ( inter%internal(2) == 0 ) then
!!      if( inter%rl(2) -  inter%internal(1) <= 0.d0 ) then
!!        inter%internal(2) = 1
!!      end if
!!    end if
!!
!!  end subroutine
!!
!!  subroutine iqs_clb_nosldt_post(inter)
!!    !> interaction
!!    type(T_interaction) :: inter
!!
!!    inter%internal(2) = inter%internal(2) + H*inter%vl(1)
!!
!!  end subroutine

  !===============================================================================!
  ! SOLVER LIST : coulomb_friction                                                !
  !               coupled_dof                                                     !
  !               normal_coupled_dof                                              !
  !               tangential_coupled_dof                                          !
  !               plastic_coupled_dof                                             !
  !               wire                                                            !
  !               czm                                                             !
  !===============================================================================!

  !> \brief Normal coupled dof diagonal solver
  subroutine normal_coupled_dof_diag_solver(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    vlfik(2) = vlik(2)  + inter%covfree(2) - inter%invW(2)*rlik(2)
    rliki(2) =-vlfik(2) * inter%invW(2)
    statusik = 'stick'

  end subroutine

  !> \brief Wire diag solver
  subroutine wire_solver_diag(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,stat)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: stat

    vlfik(2) = vlik(2) + inter%covfree(2) - inter%W(2,2)*rlik(2)
    if (vlfik(2) > 0.d0) then
      rliki(2) = -vlfik(2)*inter%invW(2)
      stat     = 'stick'
    else
      rliki(2) = 0.d0
      stat     = 'vnish'
    end if

  end subroutine

  !> \brief Wire solver
  subroutine wire_solver(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,stat)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: stat

    if (vlfik(2) > 0.d0) then
      rliki(2) = -vlfik(2)/WWik(2,2)
      vliki(2) = -inter%covfree(2)
      stat     = 'stick'
    else
      rliki(2) = 0.d0
      vliki(2) = vlfik(2) - inter%covfree(2)
      stat     = 'vnish'
    end if

  end subroutine

  !======== stupid utilities ======!

  subroutine change_status_wet(stat)
    implicit none
    character(len=5), intent(inout) :: stat

    if (stat == 'nknow')then
      stat='Wnnow'
    else if (stat == 'noctc')then
      stat='Wnctc'
    else if (stat == 'stick')then
      stat='Wstck'
    else if (stat == 'slide')then
      stat='Wslid'
    else if (stat == 'slibw')then
      stat='Wslbw'
    else if (stat == 'slifw')then
      stat='Wslfw'
    end if
  end subroutine

  subroutine revert_status_wet(stat)
    implicit none
    character(len=5), intent(inout) :: stat

    if (stat == 'noctc') then 
      stat='Wnctc'
    else if (stat == 'stick') then 
      stat='Wstck'
    else if (stat == 'slibw') then 
      stat='Wslbw'
    else if (stat == 'slifw') then
      stat='Wslfw'
    end if
  end subroutine

