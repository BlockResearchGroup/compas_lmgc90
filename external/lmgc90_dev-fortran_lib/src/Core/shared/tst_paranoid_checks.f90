program test

  use paranoid_checks

  implicit none

  integer(kind=4) :: nb_tests = 1000000
  integer(kind=4) :: i_test

  call test_i4_checks(nb_tests)
  call test_r8_checks(nb_tests)
  call test_c5_checks(nb_tests)

  write(*,*) "TEST PASSED"

contains     

  subroutine test_i4_checks(nb_tests)
    implicit none
    integer(kind=4) :: nb_tests
    !
    integer(kind=4) :: size, i_test
    integer(kind=4), dimension(:), allocatable :: i4_tab
    integer(kind=4), dimension(:), pointer     :: i4_ptr
    character(len=14) :: IAM
    IAM = 'test_i4_checks'

    size = 12
    allocate(i4_tab(size))
    allocate(i4_ptr(size))

    do i_test = 1, nb_tests
      call paranoid_check_i4     (IAM, i4_tab, size/2)
      call paranoid_check_i4_size(IAM, i4_tab, size)

      call paranoid_check_i4_ptr (IAM, i4_ptr, size/2)
      call paranoid_check_i4_size(IAM, i4_ptr, size)
    end do

    deallocate(i4_tab)
    deallocate(i4_ptr)

  end subroutine test_i4_checks

  subroutine test_r8_checks(nb_tests)
    implicit none
    integer(kind=4) :: nb_tests
    !
    integer(kind=4) :: size, i_test
    real(kind=8), dimension(:), allocatable :: r8_tab
    real(kind=8), dimension(:), pointer     :: r8_ptr
    character(len=14) :: IAM
    IAM = 'test_r8_checks'

    size = 12
    allocate(r8_tab(size))
    allocate(r8_ptr(size))

    do i_test = 1, nb_tests
      call paranoid_check_r8     (IAM, r8_tab, size/2)
      call paranoid_check_r8_size(IAM, r8_tab, size)

      call paranoid_check_r8_ptr (IAM, r8_ptr, size/2)
      call paranoid_check_r8_size(IAM, r8_ptr, size)
    end do

    deallocate(r8_tab)
    deallocate(r8_ptr)

  end subroutine test_r8_checks

  subroutine test_c5_checks(nb_tests)
    implicit none
    integer(kind=4) :: nb_tests
    !
    integer(kind=4) :: size, i_test
    character(len=5), dimension(:), allocatable :: c5_tab
    character(len=5), dimension(:), pointer     :: c5_ptr
    character(len=14) :: IAM
    IAM = 'test_c5_checks'

    size = 12
    allocate(c5_tab(size))
    allocate(c5_ptr(size))

    do i_test = 1, nb_tests
      call paranoid_check_c5     (IAM, c5_tab, size/2)
      call paranoid_check_c5_size(IAM, c5_tab, size)

      call paranoid_check_c5_ptr (IAM, c5_ptr, size/2)
      call paranoid_check_c5_size(IAM, c5_ptr, size)
    end do

    deallocate(c5_tab)
    deallocate(c5_ptr)

  end subroutine test_c5_checks

end program test
