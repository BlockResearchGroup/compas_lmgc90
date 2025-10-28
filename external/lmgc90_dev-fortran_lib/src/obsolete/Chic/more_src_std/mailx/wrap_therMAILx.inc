  SUBROUTINE more_chic_command_therMAILx

    IMPLICIT NONE
    integer :: ibdy
    character(len=30) :: zobzz
    real(kind=8) :: value


    IF (INDEX(CHZZZ,'SET THER CONSTANT FIELD')==1) THEN
       !!****e* CHIC_cmd/SET BRICK THERMAL FIELD
       !! NAME
       !!   SET BRICK THERMAL FIELD
       !! PURPOSE
       !!  Set a linear thermal field on a rectangular domain
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_field_bynode
       !!****
       CALL LOGCHIC('therMAILx')
       READ(CHLZZZ(NZZZ+1),*) ibdy,zobzz,value

       CALL SetConstantField(ibdy,zobzz,value)

       IETZZZ = 1
       RETURN      
    END IF

    
  END SUBROUTINE more_chic_command_therMAILx
!!!----------------------------------------------------

  subroutine SetConstantField(ibdy,name,value)
    ! petite routine pour affecter une valeur constante a un field

    implicit none
       integer      :: ibdy
       real(kind=8) :: value
       character(len=*) :: name

       integer      :: nbnoe
       real(kind=8),allocatable, dimension(:) :: field
       integer      :: rank=0

       rank = get_field_rank(ibdy,1,name)        

       nbnoe=get_nb_nodes(ibdy)
       allocate(field(nbnoe))

       field=value
       call set_field_bynode(ibdy,rank,nbnoe,field)

       deallocate(field)

  end subroutine
