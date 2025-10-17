!>binding of an external mbs library
module ExternalMBS

  use overall, only : faterr

  implicit none

  contains
  
  !------------------------------------------------------------------------
  !> Initialize the MBS library
  subroutine initialize()
    implicit none

    call faterr('External_MBS:initialize','no external MBS library found')

  end subroutine

  !> Prepare libray for next time step
  subroutine increment(tbegin)
    implicit none
    !> value of simulation time at the begining of the time step
    real(kind=8), intent(in) :: tbegin

    call faterr('External_MBS:increment','no external MBS library found')

  end subroutine 

  ! debut boucle Newton 
  
  !> Compute the velocity of all multibody
  subroutine compute_free_vlocy(h, theta)
    implicit none
    !> time step size
    real(kind=8), intent(in) :: h
    !> theta parameter for the theta-integration scheme
    real(kind=8), intent(in) :: theta

    call faterr('External_MBS:compute_free_vlocy','no external MBS library found')

  end subroutine

  !> Get nodes coordinates and local frame in a configuration for 3D
  subroutine update_nodes_3D(i_mbs, coor, localFrame, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs 
    !> coordinates in desired configuration
    real(kind=8), dimension(:,:), pointer :: coor
    !> local frame in desired configuration
    real(kind=8), dimension(:,:), pointer :: localFrame
    !> desired configuration
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:update_nodes_3D','no external MBS library found')

  end subroutine

  !> Get nodes coordinates in desired configuration for 2D
  subroutine update_nodes_2D(i_mbs, coor, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> coordinates in desired configuration
    real(kind=8), dimension(:,:), pointer :: coor
    !> desired configuration
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:update_nodes_2D','no external MBS library found')

  end subroutine

  !> Nullify a reaction array
  subroutine nullify_reac(i_mbs, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> array to work on
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:nullify_reac','no external MBS library found')

  end subroutine

  !> Set a reaction array for 3D
  subroutine add_reac_3D(i_mbs, i_node, reac, storage, frame)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> node index of multi-body
    integer(kind=4), intent(in) :: i_node
    !> reaction array
    real(kind=8)   , dimension(6), intent(in) :: reac
    !> array to work on
    integer(kind=4), intent(in) :: storage
    !> local frame in which to express torques
    real(kind=8), dimension(3,3), intent(in) :: frame

    call faterr('External_MBS:add_reac_3D','no external MBS library found')

  end subroutine

  !> Set a reaction array for 2D
  subroutine add_reac_2D(i_mbs, i_node, reac, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> node index of multi-body
    integer(kind=4), intent(in) :: i_node
    !> reaction array
    real(kind=8)   , dimension(3), intent(in) :: reac
    !> array to work on
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:adD_reac_2D','no external MBS library found')

  end subroutine

  !> Set a velocity array to zero 
  subroutine nullify_vlocy(i_mbs, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> array to work on
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:nullify_vlocy','no external MBS library found')

  end subroutine

  !> Compute a velocity array
  subroutine comp_vlocy(i_mbs, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> array to work on
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:comp_vlocy','no external MBS library found')

  end subroutine

  !> Get a velocity array for 3D
  subroutine get_vlocy_3D(i_mbs, i_node, vlocy, storage, frame)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> node index of multi-body
    integer(kind=4), intent(in) :: i_node
    !> velocity array
    real(kind=8)   , dimension(6), intent(inout) :: vlocy
    !> array to work on
    integer(kind=4), intent(in) :: storage
    !> local frame in which to express rotation
    real(kind=8), dimension(3,3), intent(in) :: frame

    call faterr('External_MBS:get_vlocy_3D','no external MBS library found')

  end subroutine

  !> Get a velocity array for 2D
  subroutine get_vlocy_2D(i_mbs, i_node, vlocy, storage)
    implicit none
    !> multi-body index
    integer(kind=4), intent(in) :: i_mbs
    !> node index of multi-body
    integer(kind=4), intent(in) :: i_node
    !> velocity array
    real(kind=8)   , dimension(3), intent(inout) :: vlocy
    !> array to work on
    integer(kind=4), intent(in) :: storage

    call faterr('External_MBS:get_vlocy_2D','no external MBS library found')

  end subroutine

  !> Compute displacements
  subroutine compute_dof(h, theta)
    implicit none
    !> time step size
    real(kind=8), intent(in) :: h
    !> theta parameter for the theta-integration scheme
    real(kind=8), intent(in) :: theta

    call faterr('External_MBS:compute_dof','no external MBS library found')

  end subroutine


  ! fin boucle newton 
  
  !> Update values at the end of time step
  subroutine update_dof()
    implicit none

    ! calcul nouvelle configuration 

    ! update integration
    call faterr('External_MBS:update_dof','no external MBS library found')

  end subroutine 

  !> Clean library
  subroutine finalize()
    implicit none 

    ! on termine le calcul
    call faterr('External_MBS:finalize','no external MBS library found')

  end subroutine 
  
end module
