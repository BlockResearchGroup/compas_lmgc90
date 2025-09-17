module self_gravity

real(kind=8), private :: G=6.67d-00 ! Gravitational constant in Kg/M/S Unit
                                    ! This value of G imposes that masses are in Kg and
                                    ! distances in meters

logical :: is_first = .true.        ! tells if Fg_prev is not stored
real(kind=8), dimension(:,:), allocatable :: Fg_prev ! Stores gravity forces at each call

CONTAINS

subroutine self_gravity_Force_2D (Nbod, X, Y, M, Fx,Fy )
  ! Return the for in newton on each particle
  ! INPUT
  ! Nbod : Number of bodies
  !  X,Y : Vectors with Nbod elements containing X and y coordinates of bodies in M
  !  M : Vector with Nbod elements containing the Mass in Kg
  ! OUTPUT
  ! Fx : Nbod elements vector with the X coordinate of Force (in Newtons)
  ! Fy : Nbod elements vector with the X coordinate of Force (in Newtons)
  implicit none
  ! Input
  integer(kind=4), intent(in) :: Nbod ! Number of bodies
  real(kind=8), intent(in), dimension(Nbod) :: X, Y ,M ! X,y coordinates, M=Mass in Kg
  ! Output
  real(kind=8), intent(out), dimension(Nbod) :: Fx,Fy ! X and Y coordinates of Force in Newtons
  !
  integer(kind=4) :: I
  real(kind=8), dimension(Nbod) :: R3
  
  ! Loop on all bodies
  do i=1, Nbod
  
       ! distance to body I ^3
       R3=sqrt((X-X(i))**2+(Y-Y(i))**2)
       R3=R3*R3*R3
       R3(i)=1.d0 ! To avoid division by 0
  
       Fx(i)=G*M(i)*SUM( (X-X(i))*M/R3  )
       Fy(i)=G*M(i)*SUM( (Y-Y(i))*M/R3  )
  
  enddo

end subroutine self_gravity_Force_2D

subroutine self_gravity_Force_3D (Nbod, X, Y, Z, M, Fx,Fy,Fz )
  ! Return the for in newton on each particle
  ! INPUT
  ! Nbod : Number of bodies
  !  X,Y, Z : Vectors with Nbod elements containing X , Y and Z coordinates of bodies in M
  !  M : Vector with Nbod elements containing the Mass in Kg
  ! OUTPUT
  ! Fx : Nbod elements vector with the X coordinate of Force (in Newtons)
  ! Fy : Nbod elements vector with the X coordinate of Force (in Newtons)
  ! Fz : Nbod elements vector with the Z coordinate of Force (in Newtons)
  implicit none
  ! Input
  integer(kind=4), intent(in) :: Nbod ! Number of bodies
  real(kind=8), intent(in), dimension(Nbod) :: X, Y ,Z ,M ! X,y coordinates, M=Mass in Kg
  ! Output
  real(kind=8), intent(out), dimension(Nbod) :: Fx,Fy,Fz ! X and Y coordinates of Force in Newtons
  
  integer(kind=4) :: I
  real(kind=8), dimension(Nbod) :: R3
  
  ! Loop on all bodies
  do i=1, Nbod
  
       ! distance to body I ^3
       R3=sqrt((X-X(i))**2+(Y-Y(i))**2+(Z-Z(i))**2)
       R3=R3*R3*R3
       R3(i)=1.d0 ! To avoid division by 0
  
       Fx(i)=G*M(i)*SUM( (X-X(i))*M/R3  )
       Fy(i)=G*M(i)*SUM( (Y-Y(i))*M/R3  )
       Fz(i)=G*M(i)*SUM( (Z-Z(i))*M/R3  )
  
  enddo

end subroutine self_gravity_Force_3D

!> compute the gravitation force term to add to Fext in lmgc90
!> explicit
!> input : position and mass of every particles, number of particles
!>         theta and time step
!> output : fext to add
subroutine compute_fext_2d(pos, masses, fext, nb_part, theta, dt)
  implicit none
  !input
  integer(kind=4) :: nb_part
  real(kind=8)    :: theta, dt
  real(kind=8), dimension(nb_part,3) :: pos
  real(kind=8), dimension(nb_part)   :: masses
  !output
  real(kind=8), dimension(3,nb_part) :: fext
  !
  real(kind=8), dimension(2,nb_part) :: Fg

  call self_gravity_Force_2d(nb_part,pos(:,1), pos(:,2), masses, Fg(1,:), Fg(2,:))
  
  fext(3,:) = 0.
  if(is_first) then
    fext(1:2,:) = dt*Fg
    allocate(Fg_prev(2,nb_part))
    Fg_prev=Fg
    is_first = .false.
  else
    fext(1:2,:) = dt*(theta*Fg_prev + (1.-theta)*Fg)
  end if

end subroutine
  
!> compute the gravitation force term to add to Fext in lmgc90
!> explicit
!> input : position and mass of every particles, number of particles
!>         theta and time step
!> output : fext to add
subroutine compute_fext_3d(pos, masses, fext, nb_part, theta, dt)
  implicit none
  !input
  integer(kind=4) :: nb_part
  real(kind=8)    :: theta, dt
  real(kind=8), dimension(nb_part,6) :: pos
  real(kind=8), dimension(nb_part)   :: masses
  !output
  real(kind=8), dimension(6,nb_part) :: fext
  !
  real(kind=8), dimension(3,nb_part) :: Fg
  !output

  call self_gravity_Force_3d(nb_part,pos(:,1), pos(:,2), pos(:,3), masses, Fg(1,:), Fg(2,:), Fg(3,:))

  fext(4:6,:) = 0.
  if(is_first) then
    fext(1:3,:) = dt*Fg
    allocate(Fg_prev(3,nb_part))
    Fg_prev=Fg
    is_first = .false.
  else
    fext(1:3,:) = dt*(theta*Fg_prev + (1.-theta)*Fg)
  end if

end subroutine
  
end module self_gravity

