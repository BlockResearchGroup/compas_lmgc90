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
MODULE wrap_bulk_behav

  USE ISO_C_BINDING

  USE bulk_behaviour
  
  PUBLIC
  
CONTAINS

!-------------------------------------------------------------------------
  SUBROUTINE ReadBehaviours() bind(c, name='bulk_behav_ReadBehaviours')
    IMPLICIT NONE

    CALL read_in_bulk_behav 

  END SUBROUTINE
!-------------------------------------------------------------------------
  SUBROUTINE WriteBehaviours() bind(c, name='bulk_behav_WriteBehaviours')
    IMPLICIT NONE

    CALL write_out_bulk_behav

  END SUBROUTINE
!-------------------------------------------------------------------------    
  SUBROUTINE CollectOutBulkBehav() bind(c, name='bulk_behav_CollectOutBulkBehav')
    IMPLICIT NONE
 
    CALL read_out_bulk_behav

  END SUBROUTINE
!-------------------------------------------------------------------------    
  SUBROUTINE CleanOutBulkBehav() bind(c, name='bulk_behav_CleanOutBulkBehav')
    IMPLICIT NONE

    CALL clean_out_bulk_behav

  END SUBROUTINE
!-------------------------------------------------------------------------
  SUBROUTINE AppendOutBulkBehav() bind(c, name='bulk_behav_AppendOutBulkBehav')
    IMPLICIT NONE

    CALL append_out_bulk_behav

  END SUBROUTINE
!-------------------------------------------------------------------------
  SUBROUTINE RebuildInBulkBehav() bind(c, name='bulk_behav_RebuildInBulkBehav')
    IMPLICIT NONE

    CALL write_in_bulk_behav

  END SUBROUTINE
!-------------------------------------------------------------------------
  subroutine GetGravity(gravity, length) bind(c, name='bulk_behav_GetGravity')
    implicit none
    integer(c_int) :: length
    type(c_ptr)    :: gravity
    !
    real(kind=8), dimension(:), pointer :: g

    ! on sait que c'est 3 
    length = 3
    allocate(g(length))

    g(:) = get_gravity()

    gravity = c_loc(g(1))

   end subroutine
!-------------------------------------------------------------------------
  subroutine SetGravity(gravity, length) bind(c, name='bulk_behav_SetGravity')
    implicit none
    integer(C_INT), intent(in),value :: length
    real(C_DOUBLE), dimension(length), intent(in) :: gravity
    real(kind=8), dimension(3) :: tmp
    
    ! on sait que c'est 3 
    if (length /=3) then
      print*,'Error: length must be equal to 3'
      stop
    endif

    tmp = gravity

    call set_gravity(tmp)

   end subroutine

!-------------------------------------------------------------------------
! DA : pour appliquer des valeurs sur des loi de comportement

   subroutine SetConductivity(cvalue1_c , ivalue , rvalue) bind(c, name='bulk_behav_SetConductivity')

    implicit none
    
    character(c_char),dimension(5),intent(in)  :: cvalue1_c
    integer(C_INT), intent(in),value :: ivalue
    real(C_DOUBLE),  intent(in), value :: rvalue
    character(len=5) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    call set_conductivity(cvalue1, ivalue, rvalue)

   end subroutine

   subroutine SetCapacity(cvalue1_c , ivalue , rvalue) bind(c, name='bulk_behav_SetCapacity')

    implicit none
    
    character(c_char),dimension(5),intent(in)  :: cvalue1_c
    integer(C_INT), intent(in),value :: ivalue
    real(C_DOUBLE),  intent(in), value :: rvalue
    character(len=5) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    call set_capacity(cvalue1, ivalue, rvalue)

   end subroutine

   subroutine SetBiot(cvalue1_c , ivalue , rvalue) bind(c, name='bulk_behav_SetBiot')

    implicit none
    
    character(c_char),dimension(5),intent(in)  :: cvalue1_c
    integer(C_INT), intent(in),value :: ivalue
    real(C_DOUBLE),  intent(in), value :: rvalue
    character(len=5) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    call set_biot(cvalue1, ivalue, rvalue)

   end subroutine

   subroutine SetExternalFlux(cvalue1_c , ivalue , rvalue) bind(c, name='bulk_behav_SetExternalFlux')

    implicit none
    
    character(c_char),dimension(5),intent(in)  :: cvalue1_c
    integer(C_INT), intent(in),value :: ivalue
    real(C_DOUBLE),  intent(in), value :: rvalue
    character(len=5) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    call set_external_flux(cvalue1, ivalue, rvalue)

   end subroutine

   subroutine SetDensity(cvalue1_c , rvalue) bind(c, name='bulk_behav_SetDensity')

    implicit none
    
    character(c_char),dimension(5),intent(in)  :: cvalue1_c
    real(C_DOUBLE),  intent(in), value :: rvalue
    character(len=5) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    call set_solid_density(cvalue1, rvalue)

   end subroutine

   ! introspection

   function getNbBulkBehav() bind(c, name='bulk_behav_GetNbBulkBehav')
     implicit none
     integer(c_int) :: getNbBulkBehav

     getNbBulkBehav = get_nb_bulk_behav()

   end function

   subroutine getBulkBehav(i_tb, string_out, string_size, real_size, c5) bind(c, name='bulk_behav_GetBulkBehav')
    implicit none
    integer(c_int), intent(in), value :: i_tb
    type(c_ptr) :: string_out
    type(c_ptr) :: c5
    integer(c_int), intent(out) :: string_size, real_size
    !
    integer(kind=4)  :: i, nb_param
    character(len=5), pointer :: behav
    character(len=30), pointer :: lawty

    allocate(lawty,behav)
    call get_bulk_behav(i_tb, lawty, behav)

    string_size = len(trim(lawty))
    real_size   = len(lawty)
    string_out = c_loc(lawty(1:1))

    c5 = c_loc(behav(1:1))

  end subroutine getBulkBehav

  subroutine CleanMemory() bind(c, name='bulk_behav_CleanMemory')
    implicit none

    call clean_memory

  end subroutine

END MODULE wrap_bulk_behav
