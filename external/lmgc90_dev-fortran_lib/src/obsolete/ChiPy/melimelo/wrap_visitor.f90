!> LMGC90 wrap of visitor
!> \author F. Dubois
!> \date   January 2009

module wrap_visitor

  use ISO_C_BINDING

  use visitor, only : &
      set_visitor

  IMPLICIT NONE

  contains

  subroutine setVisitor(what,where) bind(C, name="visitor_setVisitor")
    implicit none

    ! variables d'entree :
    character(c_char), dimension(5) :: what ! chaine de caractere format C, i.e.
       ! tableau de chaines de 1 caractere en fortran
    integer(kind=c_int), value   :: where

    ! variables locales :
    character(len=5) :: what_for ! chaine de 5 caracteres format fortran 
    integer :: i ! indice de boucle
    
    ! on reconstruit une chaine format fortran a partir du tabeleau de chaines
    ! venant du C
    what_for=''
    do i=1, 5
       what_for = what_for(1:i - 1) // what(i)
    end do

    call set_visitor(what_for, where)

  end subroutine

  function openFile(file_name,where) bind(C, name="visitor_openFile")
    implicit none
    
    ! variables d'entree :
    character(c_char), dimension(10) :: file_name ! chaine de caractere format 
       ! C, i.e. tableau de chaines de 1 caractere en fortran
    integer(kind=c_int), value     :: where
    integer(kind=c_int)            :: openFile

    ! variables locales :
    character(len=10) :: file_name_for ! chaine de 10 caracteres format fortran 
    integer :: i ! indice de boucle
 
    ! on reconstruit une chaine format fortran a partir du tabeleau de chaines
    ! venant du C
    file_name_for=''
    do i=1, 10
       file_name_for = file_name_for(1:i - 1) // file_name(i)
    end do

    open(unit=where, file=file_name_for, iostat=openFile)
  end function

  subroutine closeFile(where) bind(C, name="visitor_closeFile")
    implicit none
    integer(kind=c_int), value     :: where

    close(where)
  end subroutine

end module
