module wrap_PRPR_DDM_MPI_DECENT

  use ISO_C_BINDING

  use PRPR_DDM_MPI_DECENT

  implicit none

  contains

  subroutine ResousProbleme() bind(c, name='resousProbleme')
    implicit none

    call do_all

  end subroutine

end module wrap_PRPR_DDM_MPI_DECENT
