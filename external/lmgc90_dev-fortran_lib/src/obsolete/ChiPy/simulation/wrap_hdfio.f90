!> LMGC90 python wrap of model_handler
!> \author F. Dubois
!> \date   January 2010
!>

module wrap_hdfio

  use ISO_C_BINDING

  use hdfio , only : write_mecaState_hdf5

  IMPLICIT NONE

  public writeMecaState

contains

  subroutine writeMecaState() bind(C, name="hdfio_writeMecaState")
    implicit none

    call write_mecaState_hdf5()

  end subroutine

end module
