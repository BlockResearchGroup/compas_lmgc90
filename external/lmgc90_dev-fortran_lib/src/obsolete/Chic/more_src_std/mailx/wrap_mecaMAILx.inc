  SUBROUTINE more_chic_command_mecaMAILx

    IMPLICIT NONE
    integer :: ibdy,sens,nx,ny
    real(kind=8) :: T1,T2,T3,T4
    
    real(kind=8) :: TPS_start,TPS_stop,TPS,xx
    real(kind=8) :: T1_start,T1_stop
    real(kind=8) :: T2_start,T2_stop
    real(kind=8) :: T3_start,T3_stop
    real(kind=8) :: T4_start,T4_stop

    character(len=30) :: zobzz
    real(kind=8) :: value

    IF (INDEX(CHZZZ,'SET BRICK THERMAL FIELD')==1) THEN
       !!****e* CHIC_cmd/SET BRICK THERMAL FIELD
       !! NAME
       !!   SET BRICK THERMAL FIELD
       !! PURPOSE
       !!  Set a linear thermal field on a rectangular domain
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_field_bynode
       !!****
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),*) ibdy
       READ(CHLZZZ(NZZZ+2),*) sens,nx,ny
       zobzz = CHLZZZ(NZZZ+3)
       IF (INDEX(zobzz,'EVOLUTIF')==1) THEN       
         READ(CHLZZZ(NZZZ+4),*) TPS_start,TPS_stop
         READ(CHLZZZ(NZZZ+5),*) T1_start,T2_start,T3_start,T4_start
         READ(CHLZZZ(NZZZ+6),*) T1_stop ,T2_stop ,T3_stop ,T4_stop       
         TPS=get_time()
         if ( TPS <= TPS_start) then 
           xx = 0.d0 
	 else if ( TPS > TPS_start  .and. TPS < TPS_stop) then
	   xx = (TPS - TPS_start) / (TPS_stop - TPS_start)
         else
           xx = 1.d0
         endif
	 T1 = ((1.d0 - xx) * T1_start) + (xx * T1_stop)
	 T2 = ((1.d0 - xx) * T2_start) + (xx * T2_stop)
	 T3 = ((1.d0 - xx) * T3_start) + (xx * T3_stop)
	 T4 = ((1.d0 - xx) * T4_start) + (xx * T4_stop)
       ELSE
         READ(CHLZZZ(NZZZ+3),*) T1,T2,T3,T4
       ENDIF

       CALL SetThermalField_block(ibdy,sens,nx,ny,T1,T2,T3,T4)

       IETZZZ = 1
       RETURN      
    END IF


    IF (INDEX(CHZZZ,'SET MECA CONSTANT FIELD')==1) THEN
       !!****e* CHIC_cmd/SET BRICK THERMAL FIELD
       !! NAME
       !!   SET BRICK THERMAL FIELD
       !! PURPOSE
       !!  Set a linear thermal field on a rectangular domain
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_field_bynode
       !!****
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),*) ibdy,zobzz,value

       CALL SetConstantField(ibdy,zobzz,value)

       IETZZZ = 1
       RETURN      
    END IF



  END SUBROUTINE more_chic_command_mecaMAILx
!!!----------------------------------------------------

  subroutine SetThermalField_block(ibdy,sens,nx,ny,T1,T2,T3,T4)
    ! petite routine pour affecter un champ de Temperature
    ! lineaire sur un domaine rectangulaire 
    ! l espacement des noeuds est suppose regulier

    implicit none
       integer      :: ibdy,sens,nx,ny
       real(kind=8) :: T1,T2,T3,T4
       integer      :: i,j,nbnoe,inoe
       real(kind=8) :: Lx,Ly,ksi,eta
       real(kind=8),allocatable, dimension(:) :: T   
       logical      :: is_first_time=.true.
       integer      :: rank=0

       if (is_first_time) then
         is_first_time=.false.
         rank = get_field_rank(ibdy,1,'TEMPERATURE')        
       endif

       Lx = nx
       Ly = ny
       nbnoe=(nx+1)*(ny+1)
       allocate(T(nbnoe))

       select case (sens)
       case(0) !sens x puis y    
         inoe=0
         do i=1,nx+1
	   do j=1,ny+1
             inoe=inoe+1
             ksi= ((2.d0*(i-1)) - Lx)/Lx 
             eta= ((2.d0*(j-1)) - Ly)/Ly 
	     T(inoe)=interpolate_Q4(ksi,eta,T1,T2,T3,T4)       
           enddo 
         enddo        
       case(1) !sens y puis x
         inoe=0
	 do j=1,ny+1
           do i=1,nx+1
             inoe=inoe+1
             ksi= ((2.d0*(i-1)) - Lx)/Lx 
             eta= ((2.d0*(j-1)) - Ly)/Ly 
	     T(inoe)=interpolate_Q4(ksi,eta,T1,T2,T3,T4)       
           enddo 
         enddo        

       case default
         print *,'FATAL ERROR:: sens must be 0 or 1'
         stop

       end select

       !print *,ibdy,nbnoe
       !write(*,'(1x,D12.5)') T
       call set_field_bynode(ibdy,rank,nbnoe,T)

       deallocate(T)

  end subroutine

  subroutine SetConstantField(ibdy,name,value)
    ! petite routine pour affecter une valeur constante a un field

    implicit none
       integer      :: ibdy
       real(kind=8) :: value
       character(len=*) :: name

       integer      :: nbnoe
       real(kind=8),allocatable, dimension(:) :: field
       logical      :: is_first_time=.true.
       integer      :: rank=0

       if (is_first_time) then
         is_first_time=.false.
         rank = get_field_rank(ibdy,1,name)        
       endif

       nbnoe=get_nb_nodes(ibdy)
       allocate(field(nbnoe))
       field=value
       print *,ibdy,nbnoe
       call set_field_bynode(ibdy,rank,nbnoe,field)

       deallocate(field)

  end subroutine



  real(kind=8) function interpolate_Q4(ksi,eta,T1,T2,T3,T4)

    implicit none
    real(kind=8) :: ksi,eta,T1,T2,T3,T4

    interpolate_Q4= (0.25*(1.d0-KSI)*(1.d0-ETA)*T1) + &
                    (0.25*(1.d0+KSI)*(1.d0-ETA)*T2) + & 
                    (0.25*(1.d0+KSI)*(1.d0+ETA)*T3) + & 
                    (0.25*(1.d0-KSI)*(1.d0+ETA)*T4) 

  end function
