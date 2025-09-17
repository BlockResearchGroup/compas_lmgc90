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
module wrap_tact_behav

  use ISO_C_BINDING

  use tact_behaviour
  use utilities
  use overall
  
  public
  
contains
!!!-------------------------------------------------------------------------
  subroutine OpenBehavContainer() bind(c, name='tact_behav_OpenBehavContainer')
    implicit none

    call open_tact_behav_ll()

  end subroutine
  subroutine CloseBehavContainer() bind(c, name='tact_behav_CloseBehavContainer')
    implicit none

    call close_tact_behav_ll()

  end subroutine

  subroutine OpenSeeContainer() bind(c, name='tact_behav_OpenSeeContainer')
    implicit none

    call open_see_ll()

  end subroutine

  subroutine CloseSeeContainer() bind(c, name='tact_behav_CloseSeeContainer')
    implicit none

    call close_see_ll()

  end subroutine

  subroutine FillContainersFromFile() bind(c, name='tact_behav_FillContainersFromFile')
    implicit none

    call read_xxx_tact_behav(1)

  end subroutine


   subroutine AddToSeeContainer(c_cdbdy,c_cdtac,c_cdcol,c_behav,c_anbdy,c_antac,c_ancol,c_alert,c_global_alert) &
   bind(c, name='tact_behav_AddToSeeContainer')
      implicit none
      character(C_CHAR), dimension(5) :: c_cdbdy,c_cdtac,c_cdcol,c_behav,c_anbdy,c_antac,c_ancol
      real(C_DOUBLE), intent(IN), value :: c_alert,c_global_alert

      character(len=5) :: cdbdy,cdtac,cdcol,behav,anbdy,antac,ancol
      !real(kin=8) :: alert,global_alert
      integer :: i

      do i=1,5
         cdbdy = cdbdy(1:i-1) // c_cdbdy(i)
         cdtac = cdtac(1:i-1) // c_cdtac(i)
         cdcol = cdcol(1:i-1) // c_cdcol(i)
         behav = behav(1:i-1) // c_behav(i)
         anbdy = anbdy(1:i-1) // c_anbdy(i)
         antac = antac(1:i-1) // c_antac(i)
         ancol = ancol(1:i-1) // c_ancol(i)
      end do

      call add_to_See_ll(cdbdy,cdtac,cdcol,behav,anbdy,antac,ancol,c_alert,c_global_alert)

  end subroutine

  subroutine ReadBehaviours() bind(c, name='tact_behav_ReadBehaviours')
    implicit none

    call open_tact_behav_ll()
    call open_see_ll()
    call read_xxx_tact_behav(1)
    call close_tact_behav_ll()
    call close_see_ll()

  end subroutine

  subroutine CollectOutTactBehav() bind(c, name='tact_behav_CollectOutTactBehav')
    implicit none

    call read_xxx_tact_behav(2)

  end subroutine

  subroutine WriteBehaviours() bind(c, name='tact_behav_WriteBehaviours')
    implicit none

    call write_xxx_tact_behav(2)

  end subroutine

  subroutine AppendOutTactBehav() bind(c, name='tact_behav_AppendOutTactBehav')
    implicit none
 
    call write_xxx_tact_behav(3)

  end subroutine

  subroutine RebuildInTactBehav() bind(c, name='tact_behav_RebuildInTactBehav')
    implicit none

    call write_xxx_tact_behav(1)

  end subroutine

  subroutine CleanOutTactBehav() bind(c, name='tact_behav_CleanOutTactBehav')
    implicit none

    call clean_out_tact_behav

  end subroutine

  function getNbTactBehav() bind(c, name='tact_behav_GetNbTactBehav')
    implicit none
    integer(c_int) :: getNbTactBehav

    getNbTactBehav = get_nb_tact_behav()

  end function

  subroutine getTactBehav(i_tb, string_out, string_size, real_size, c5, pointer_out, length) bind(c, name='tact_behav_GetTactBehav')
    implicit none
    integer(c_int), intent(in), value :: i_tb
    type(c_ptr) :: string_out
    type(c_ptr) :: pointer_out
    type(c_ptr) :: c5
    integer(c_int), intent(out) :: string_size, real_size
    integer(c_int), intent(out) :: length
    !
    integer(kind=4)  :: i, nb_param
    character(len=5), pointer :: behav
    character(len=30), pointer :: lawty
    real(kind=8), pointer, dimension(:) :: param
    real(kind=8), pointer, dimension(:) :: loc_param

    allocate(lawty,behav)
    call get_tact_behav(i_tb, lawty, behav, param, length)

    string_size = len(trim(lawty))
    real_size   = len(lawty)
    string_out  = c_loc(lawty(1:1))

    c5 = c_loc(behav(1:1))

    loc_param => null()
    if( length > 0 ) then
      allocate(loc_param(length))
      loc_param(1:length) = param(1:length)
      pointer_out = c_loc(loc_param(1))
    else
      pointer_out = c_null_ptr
    endif

  end subroutine getTactBehav

  subroutine getInternalComment(ivalue, string_out, string_size, real_size) bind(C, name='tact_behav_GetInternalComment')
    implicit none
    integer(c_int), intent(in), value :: ivalue
    type(c_ptr) :: string_out
    integer(c_int), intent(out) :: string_size, real_size
    !
    character(len=300), pointer :: cvect

    allocate(cvect)
    cvect = get_internal_comment(ivalue)

    string_size = len(trim(cvect))
    real_size   = len(cvect)
    string_out  = c_loc(cvect(1:1))

  end subroutine

!!!-PTA 18 02 11-----------------------------------------------------------------
  subroutine SetCZMwithInitialFriction(p) bind(c, name='tact_behav_SetCZMwithInitialFriction')
     implicit none
     !fd integer(c_int), intent(in), value :: p
     real(c_double), intent(in), value :: p

     call set_czm_initial_friction(p)

  end subroutine
!!!--------------------------------------------------------------
  subroutine initFrictionEvolutionTactBehav() bind(c, name='tact_behav_initFrictionEvolution')

    implicit none

    call init_friction_evolution

  end subroutine initFrictionEvolutionTactBehav
!!!--------------------------------------------------------------
  subroutine SetRandomFrictionTactBehav(rvalue) bind(C, name='tact_behav_setRandomFriction')
    implicit none
    real(c_double),intent(in),value :: rvalue

    call set_random_friction(rvalue)

  end subroutine SetRandomFrictionTactBehav
!!!--------------------------------------------------------------
  function GetTactBehavRankFromName(cvalue1_c)  bind(C, name='tact_behav_GetTactBehavRankFromName')
    implicit none
    character(c_char), dimension(5),intent(in) :: cvalue1_c
    integer(c_int)  :: GetTactBehavRankFromName
    !
    character(len=5) :: name
    integer :: i

    name = ''
    do i=1,5
       name = name(1:i-1) // cvalue1_c(i)
    end do

    call get_tact_behav_rank_from_name(name,GetTactBehavRankFromName)

  end function
!!!--------------------------------------------------------------
  function GetParamRankFromName(ivalue1,cvalue1_c) bind(C, name='tact_behav_GetParamRankFromName')
    implicit none
    integer(c_int),intent(in),value :: ivalue1
    character(c_char), dimension(5),intent(in) :: cvalue1_c
    integer(c_int) :: GetParamRankFromName
    !
    character(len=5) :: name
    integer :: i

    name = ''
    do i=1,5
       name = name(1:i-1) // cvalue1_c(i)
    end do

    call get_param_rank_from_name(ivalue1,name,GetParamRankFromName)

  end function
!!!--------------------------------------------------------------
  function GetParam(ivalue1,ivalue2) bind(C, name='tact_behav_GetParam')
    implicit none
    integer(c_int),intent(in),value :: ivalue1,ivalue2
    real(c_double)                  :: GetParam

    call get_param(ivalue1,ivalue2,GetParam) 

  end function
!!!--------------------------------------------------------------
  subroutine SetParam(ivalue1,ivalue2,rvalue) bind(C, name='tact_behav_SetParam')
    implicit none
    integer(c_int),intent(in),value :: ivalue1,ivalue2
    real(c_double),intent(in),value :: rvalue

    call set_param(ivalue1,ivalue2,rvalue) 

  end subroutine 

  subroutine getLawInternalComment(name, string_out, string_size, real_size) bind(C, name='tact_behav_GetLawInternalComment')
    implicit none
    character(c_char), dimension(30), intent(in) :: name
    type(c_ptr) :: string_out
    integer(c_int), intent(out) :: string_size, real_size
    !
    character(len=30) :: law_name
    character(len=internal_tact_comment_length), pointer :: internal_comment
    !
    integer :: id, nb_param, nb_internal
    character(len=5),dimension(:),pointer :: param_name

    do id = 1, 30
       law_name = law_name(1:id-1) // name(id)
    end do
    allocate(internal_comment)
    call tact_behav_info(law_name, id, nb_param, param_name, nb_internal, internal_comment)

    string_size = len(trim(internal_comment))
    real_size   = len(internal_comment)
    string_out  = c_loc(internal_comment(1:1))

  end subroutine
!<fd obsolete law are now managing mixity
!    2019-02-07 keep intentionally this here for the moment 
  
! !!!--------------------------------------------------------------
!   subroutine SetG1overG2(rvalue) bind(C, name='tact_behav_SetG1overG2')
!     implicit none
!     real(c_double),intent(in),value :: rvalue

!     call set_g1overg2(rvalue) 

!   end subroutine 
! !!!--------------------------------------------------------------
!   subroutine SetS1overS2(rvalue) bind(C, name='tact_behav_SetS1overS2')
!     implicit none
!     real(c_double),intent(in),value :: rvalue

!     call set_S1overS2(rvalue) 

!   end subroutine 
! !!!--------------------------------------------------------------
!   subroutine SetD1overD2(rvalue) bind(C, name='tact_behav_SetD1overD2')
!     implicit none
!     real(c_double),intent(in),value :: rvalue

!     call set_D1overD2(rvalue) 

!   end subroutine 

!fd />
  
!!!--------------------------------------------------------------
  subroutine SetRNcap(rvalue) bind(C, name='tact_behav_SetRNcap')
    implicit none
    real(c_double),intent(in),value :: rvalue

    call set_RNcap(rvalue) 

  end subroutine 

!!!--------------------------------------------------------------
  subroutine SetDilatancyParameters(fric,height) bind(C, name='tact_behav_SetDilatancyParameters')
    implicit none
    real(c_double),intent(in),value :: fric,height

    call set_dilatancy_parameters(fric,height) 

  end subroutine 
!!!--------------------------------------------------------------
  subroutine SetPressureParameters(ibehav,flag,rvect,ivalue2) bind(C, name='tact_behav_SetPressureParameters')
    implicit none
    integer(c_int),intent(in),value :: ibehav,flag,ivalue2
    real(c_double),INTENT(in)       :: rvect(ivalue2)    
    real(kind=8)                    :: params(10)


    params=0.d0
    params(1:min(10,ivalue2)) = rvect(1:min(10,ivalue2))
    
    call set_parameters_pressure(ibehav,flag,params) 

  end subroutine 
!!!--------------------------------------------------------------
  subroutine CleanMemory() bind(c, name='tact_behav_CleanMemory')
    implicit none

    call clean_memory

  end subroutine

end module wrap_tact_behav
