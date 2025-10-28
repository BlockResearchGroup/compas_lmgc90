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
MODULE utilities

!!****h* LMGC90.CORE/UTILITIES
!! NAME
!!  module UTILITIES
!!****

  use exception, only : f_jump

! Utilities class
! IO:
! ---
!
!     READ_CLIN a modified read considering the lines begining by
!               ! and # as comment lines.
!
!
!
! MIG utilities:
! ------------
!
!     BOMBED(MESSAGE) writes the catastrophic failure MESSAGE and then
!                     immediately terminate the calculation.
!
!     FATERR(CALLER,MESSAGE) writes a fatal error MESSAGE and increments
!                     a counter of fatal errors NERR.
!
!     FATRET(NERR) returns the number of calls to FATERR in the integer NERR.
!
!     LOGMES(MESSAGE) writes messages to the output log file.
!     
!
  IMPLICIT NONE

  logical :: no_stop = .false.

  integer(kind=4), private   :: NERR = 0
  character(len=256), dimension(1), target :: error_log

! we define one for all some global names for input, 
! end of file, file number
! 
  CHARACTER(len=180) :: G_clin
  LOGICAL            :: G_eof
  INTEGER            :: G_nfich

! we define one for all the name of the output string

  CHARACTER(len=103) :: G_cout

! some global type
!
! integer list
!
  TYPE,PUBLIC :: G_i_list

    INTEGER,DIMENSION(:),POINTER :: G_i => null()

  END TYPE G_i_list

! 
! evolution table
!
  TYPE, PUBLIC :: G_evolution
    CHARACTER(len=250):: file
    REAL(kind=8),DIMENSION(:),POINTER :: x  => null()
    REAL(kind=8),DIMENSION(:),POINTER :: fx => null()
  END TYPE G_evolution

  CHARACTER(len=9),PRIVATE :: BOBO='UTILITIES'

!
! io unit to write logs and error messages
!
  INTEGER, PRIVATE :: io_logmes = 6, io_faterr = 6

  LOGICAL,private :: BAVARD=.TRUE.

  INTEGER :: min_unit = 10, max_unit=999

  !> A type to have an array of array of strings
  type :: T_string_array
    character(len=40), dimension(:), pointer :: sdata
  end type

  !> A type to get a matrix of integer
  type :: T_i4_matrix
    integer(kind=4), dimension(:,:), allocatable :: idata
  end type

  !> A type to get a vector of real
  type :: T_r8_vector
    real(kind=8), dimension(:), allocatable :: rdata
  end type

  !> A type to get a matrix of real
  type :: T_r8_matrix
    real(kind=8), dimension(:,:), allocatable :: rdata
  end type T_r8_matrix

CONTAINS
!===================================================================================================
!!!!!!! handling of evolution table !!!!!!
!===================================================================================================
 SUBROUTINE read_G_evolution(path,file,evolution)
   IMPLICIT NONE
   CHARACTER(len=*),intent(in):: path,file
   TYPE(G_evolution)          :: evolution
   INTEGER                    :: err
   REAL(kind=8)               :: x,fx 
   INTEGER                    :: nbval,ival,nfich

   err=0
   evolution%file = file

   nfich=get_io_unit()

   OPEN(UNIT=nfich,FORM='FORMATTED',STATUS='OLD',FILE=trim(path)//trim(file),IOSTAT=err)
   
   IF (err > 0) THEN
     WRITE(*,*) ' !-------------------------------------------------------------!'
     WRITE(*,*) ' ! Error in mod_utilies while opening evolution file           !'
     WRITE(*,*) ' !  You should look to the file:                               !'
     WRITE(*,*) trim(file)
     WRITE(*,*) ' !-------------------------------------------------------------!'
     call faterr('utilities::read_G_evolution','read log')
   ENDIF
 
   nbval=0   

   DO 
     READ(nfich,*,END=98) x,fx
     nbval=nbval+1
   ENDDO 

98 CONTINUE
  
   IF (nbval < 2) THEN 
     WRITE(*,*) ' !-------------------------------------------------------------!'
     WRITE(*,*) ' ! Error in mod_utilies while processing evolution file        !'
     WRITE(*,*) ' ! Your evolution array must contain more than 1 field         !'
     WRITE(*,*) ' !  You should look to the file:                               !'
     WRITE(*,*) file
     WRITE(*,*) ' !-------------------------------------------------------------!'
     call faterr('utilities::read_G_evolution','read log')
   ENDIF

   IF (ASSOCIATED(evolution%x)) THEN 
     DEALLOCATE(evolution%x)
     NULLIFY(evolution%x)
   ENDIF
   IF (ASSOCIATED(evolution%fx)) THEN 
     DEALLOCATE(evolution%fx)
     NULLIFY(evolution%fx)
   ENDIF

   ALLOCATE(evolution%x(nbval),evolution%fx(nbval),STAT=err)

   IF (err > 0) THEN
     WRITE(*,*) ' !-------------------------------------------------------------!'
     WRITE(*,*) ' ! Error in mod_utilies while allocating evolution file        !'
     WRITE(*,*) ' !-------------------------------------------------------------!'
     call faterr('utilities::read_G_evolution','read log')
   ENDIF

   REWIND(nfich)

   DO ival=1,nbval
     READ(nfich,*,END=99) x,fx
     evolution%x(ival)=x
     evolution%fx(ival)=fx

     IF (ival > 1) THEN
       IF (ABS(evolution%x(ival) - evolution%x(ival-1)) < 1d-20) THEN
         WRITE(*,*) ' !-------------------------------------------------------------!'
         WRITE(*,*) ' ! Error in mod_utilies while processing evolution file        !'
         WRITE(*,*) ' ! Your evolution array seems to have some multipled defined   !'
         WRITE(*,*) ' ! points                                                      !'
         PRINT*,ival,evolution%x(ival),evolution%x(ival-1)
         WRITE(*,*) ' !  You should look to the file:                               !'
         WRITE(*,*) file
         WRITE(*,*) ' !-------------------------------------------------------------!'
         call faterr('utilities::read_G_evolution','read log')
       ENDIF
     ENDIF

     CYCLE
 99  CONTINUE
     WRITE(*,*) ' !-------------------------------------------------------------!'
     WRITE(*,*) ' ! Error in mod_utilies while reading evolution file           !'
     WRITE(*,*) ' !-------------------------------------------------------------!'
     call faterr('utilities::read_G_evolution','read log')
   ENDDO 

   CLOSE(nfich)

 END SUBROUTINE read_G_evolution
!===================================================================================================
 SUBROUTINE write_G_evolution(path,evolution)
   IMPLICIT NONE
   CHARACTER(len=*),intent(in):: path
   TYPE(G_evolution) :: evolution
   INTEGER          :: ival,err,nfich

   nfich=get_io_unit()

   OPEN(UNIT=nfich,STATUS='REPLACE',FILE=trim(path)//trim(evolution%file),IOSTAT=err)
   
   IF (err > 0) THEN
     WRITE(*,*) ' !-------------------------------------------------------------!'
     WRITE(*,*) ' ! Error in mod_utilies while opening evolution file           !'
     WRITE(*,*) ' !  You should look to the file:                               !'
     WRITE(*,*) evolution%file
     WRITE(*,*) ' !-------------------------------------------------------------!'
     call faterr('utilities::write_G_evolution','read log')
   ENDIF

   DO ival=1,SIZE(evolution%x)
     WRITE(nfich,*) evolution%x(ival),evolution%fx(ival)
   ENDDO 

   CLOSE(nfich)

 END SUBROUTINE write_G_evolution
!===================================================================================================
 REAL (kind=8) FUNCTION eval_G_evolution(evolution,x)
   IMPLICIT NONE
   TYPE(G_evolution) :: evolution
   REAL (kind=8) :: x,pente
   INTEGER :: i

   DO i=2,SIZE(evolution%x)
     IF (x <= evolution%x(i)) EXIT
   END DO

   IF (i >= SIZE(evolution%x)) i=SIZE(evolution%x)

   pente = ( evolution%fx(i) - evolution%fx(i-1) ) / (evolution%x(i) - evolution%x(i-1))     
   eval_G_evolution = evolution%fx(i-1) + (pente*(x - evolution%x(i-1)))

 END FUNCTION eval_G_evolution
!===================================================================================================
!!!!!!! IO safe read/write functions !!!!!!
!===================================================================================================
  LOGICAL FUNCTION read_G_clin(rien)

    IMPLICIT NONE

    INTEGER,OPTIONAL :: rien
    character(len=180) :: tampon

    read_G_clin=.TRUE.

    G_clin =' '
    DO
      tampon=' '
      READ(G_nfich,'(A180)',END=10) tampon 
      IF (tampon(1:1) == '!' .OR. tampon(1:1) == '#') CYCLE 
      IF (trim(tampon) == '') CYCLE 
      G_clin(1:len(tampon))=tampon(1:len(tampon))
      EXIT      
 10   read_G_clin=.FALSE.
      EXIT
    END DO   
  END FUNCTION read_G_clin 
!===================================================================================================
  INTEGER FUNCTION G_clin_read(ib,IF,FORMAT)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: ib,IF
    CHARACTER(len=*),INTENT(in) :: FORMAT

    READ(G_clin(ib:IF),FORMAT) G_clin_read   

   END FUNCTION G_clin_read
!===================================================================================================
!!!!!!! MIG utilities !!!!!
!===================================================================================================
  SUBROUTINE DISABLE_LOGMES()
    IMPLICIT NONE
  
    BAVARD=.FALSE.  

  END SUBROUTINE

  SUBROUTINE ENABLE_LOGMES()
    IMPLICIT NONE
  
    BAVARD=.TRUE.  

  END SUBROUTINE

  SUBROUTINE LOGMES(MESSAGE,force)

    IMPLICIT NONE
      
    CHARACTER(len=*) :: MESSAGE
    logical, optional :: force

    IF (BAVARD .or. present(force) ) WRITE(io_logmes,'(A)') MESSAGE(1:LEN_TRIM(MESSAGE))

  END SUBROUTINE 
!===================================================================================================
  subroutine faterr(caller,message)
    use iso_c_binding
    implicit none
    character(len=*) :: caller, message
    !
    type(c_ptr) :: c_mess

    nerr = len(caller)+len(message)+11
    error_log(1) = 'ERROR ['//caller//']: '//message//c_null_char

    if( no_stop ) then
      c_mess = c_loc(error_log(1))
      call f_jump(nerr)
    else
      write(io_faterr,'(A)') error_log(1)
      stop 1
    end if

  end subroutine faterr
!===================================================================================================
 FUNCTION get_io_unit() RESULT(next)
! 
! On cherche un numero libre entre 10 et 999
! La methode test si l'unite choisie est deja ouverte avec inquire.
! Soit on choisie la suivante a la derniere ouverte, soit on scanne tout.
!
   IMPLICIT NONE
   INTEGER           :: next
   LOGICAL           :: is_open
   
   INTEGER,SAVE      :: last_unit=0


! le cas facile on regarde si la suivante est libre

   IF (last_unit > 0 .AND. last_unit < max_unit  ) THEN 
     next=last_unit + 1 
     INQUIRE(unit=next,opened=is_open)
     IF (.NOT. is_open) THEN
       last_unit=next
       RETURN
     ENDIF
   ENDIF

! sinon on repart du debut

   next = 0
   last_unit=0
   DO next=min_unit,max_unit
     INQUIRE(unit=next,opened=is_open)
     IF (.NOT. is_open) THEN
       last_unit=next
       EXIT
     ENDIF
   ENDDO

   IF (last_unit==0) THEN 
     CALL FATERR(BOBO//'get_io_unit','number of io unit exceeded')
   ENDIF
 END FUNCTION get_io_unit

 !> \brief Close all possible opened units within the program
 subroutine close_all_io_unit
   implicit none
   integer :: i_unit
   logical :: is_open

   do i_unit = min_unit, max_unit

     inquire( unit=i_unit, opened=is_open)

     if (is_open) close(i_unit)

   end do

 end subroutine close_all_io_unit

   ! am : factorisation d'une procedure utilisee par plusieurs modules des "tools" du Pre
   ! procedure qui initialise le generateur de nombres pseudo-aleatoires
   ! (exemple de https://gcc.gnu.org/onlinedocs/gcc-4.3.1/gfortran/RANDOM_005fSEED.html)
   !> Initialization of the generator of random numbers.
   subroutine init_random_seed(input_seed)
     !> an optional input seed to use instead of regenerate one
     integer(kind=4), dimension(:), intent(in), optional :: input_seed
     !
     integer(kind=4) :: i, n, clock
     integer(kind=4), dimension(:), allocatable :: generated_seed
     !
     character(len=120) :: cout

     if( present(input_seed) ) then

       call random_seed( size = n )

       if( size(input_seed) < n ) then
         write(cout,*) "input seed too short, must be at least of size ", n, " and is of size ", size(input_seed)
         call faterr("utilities::init_random_seed", cout)
       end if

       call random_seed( put = input_seed )

     else

       call random_seed( size = n )
       allocate(generated_seed(n))

       call system_clock( count = clock )

       generated_seed = clock + 37 * (/ (i - 1, i = 1, n) /)
       call random_seed( put = generated_seed )

       deallocate(generated_seed)

     end if

   end subroutine

   subroutine set_io_unit_limits(min_u, max_u)
     implicit none
     integer(kind=4), intent(in) :: min_u, max_u

     min_unit = min_u
     max_unit = max_u
   end subroutine


   !> upper to lower sorting
   subroutine bubble_sort(array,size)
    implicit none
    integer(kind=4), intent(in)    :: size
    integer(kind=4), intent(inout) :: array(size)

    integer(kind=4)  :: inner   ! Counter variable for the inner loop
    integer(kind=4)  :: outer   ! Counter variable for the outer loop
    integer(kind=4)  :: temp,in,out
    ! ***
    ! Perform calculations
    outer = 1
    inner = outer+1
    DO WHILE (outer <= size -1)
      out = size + 1 - outer
      DO WHILE (inner <= size)
        in=size + 1 - inner
        
        IF (array(in) < array(out)) THEN
          temp         = array(in)
          array(in) = array(out)
          array(out) = temp
        END IF
        inner = inner + 1
      END DO
      outer = outer + 1
      inner = outer + 1
    END DO
   end subroutine

  subroutine reset_fatal()
    implicit none

    nerr = 0
    error_log(1) = ''

  end subroutine

  function check_fatal(mess)
    implicit none
    character(len=256) :: mess
    integer(kind=4)  :: check_fatal

    check_fatal = NERR
    mess = error_log(1)

  end function
  
  subroutine open_file_standard_output(filename)
     implicit none
     character(len=*)   :: filename
     integer(kind=4)    :: out_unit

     open (unit=out_unit,file=filename(1:LEN_TRIM(filename)),action="write",status="replace")
     io_logmes = out_unit
     io_faterr = out_unit
     
  end subroutine
  
  subroutine close_file_standard_output()
     implicit none
     character(len=37) :: IAM = "utilities::close_file_standard_output"

     if( io_logmes /= 6 ) then
        close (io_logmes)
     else
        call faterr(IAM,"Unable to close standard output file, file not defined") 
     endif
     
  end subroutine
  
  function get_io_logmes()
    implicit none
    integer(kind=4)  :: get_io_logmes

    get_io_logmes = io_logmes

  end function

END MODULE utilities
