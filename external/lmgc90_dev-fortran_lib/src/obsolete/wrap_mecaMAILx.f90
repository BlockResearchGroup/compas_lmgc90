!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE wrap_mecaMAILx                                       

  USE ISO_C_BINDING

  USE mecaMAILx,ONLY:&
       init_ther_field, &
       update_ther_field,&
       compute_ther_fint

CONTAINS

!!$    SUBROUTINE ComputeBulk(ikine,ither)bind(c, name='mecaMAILx_ComputeBulk')
!!$
!!$      !ikine : 0=hpp,1=gd   
!!$
!!$      IMPLICIT NONE
!!$      integer(c_int), intent(in), value :: ikine
!!$
!!$      !ither : 0=no thermal effect, 1= with thermal effect
!!$      ! ATTENTION rustine qui ne marche qu'avec lmgc90 sans matlib !! 
!!$      !integer,optional :: ither 
!!$      integer(c_int), intent(in), value :: ither 
!!$
!!$      integer :: icomp 
!!$
!!$      if (ikine == 0) then
!!$
!!$        CALL compute_bulk_hpp_mecaMAILx(0)
!!$
!!$        if (ither==1) call compute_ther_fint
!!$
!!$      else if (ikine == 1) then
!!$
!!$        CALL compute_bulk_gd_mecaMAILx(0)
!!$
!!$      endif
!!$
!!$    END SUBROUTINE
!!$
!!$    SUBROUTINE UpdateBulk(ikine) bind(c, name='mecaMAILx_UpdateBulk')
!!$      IMPLICIT NONE
!!$      integer(c_int), intent(in), value :: ikine
!!$
!!$      if (ikine == 0) then
!!$
!!$        CALL compute_bulk_hpp_mecaMAILx(1)
!!$
!!$      else if (ikine == 1) then
!!$
!!$        CALL compute_bulk_gd_mecaMAILx(1)
!!$
!!$      endif
!!$
!!$      CALL update_bulk_mecaMAILx 
!!$
!!$    END SUBROUTINE

!!$    SUBROUTINE AddSpringToNode(ibdyty,inodty,idof,k) bind(c, name='mecaMAILx_AddSpringToNode')
!!$      IMPLICIT NONE
!!$      integer(c_int),intent(in), value :: ibdyty,inodty,idof
!!$      real(c_double),intent(in),value :: k
!!$
!!$      call add_spring_to_node_mecaMAILx(ibdyty,inodty,idof,k)
!!$
!!$    END SUBROUTINE

    SUBROUTINE InitTherField(IdBody,Tini) bind(c, name='mecaMAILx_InitTherField')
      IMPLICIT NONE
      integer(c_int), intent(in), value :: IdBody
      real(c_double), intent(in), value :: Tini
       !! PURPOSE
       !!  Initialise the thermal field to a constant value on a given body 
          
       CALL init_ther_field(IdBody,Tini)

    END SUBROUTINE

    SUBROUTINE UpdateTherField(IdBody,T,Tsize) bind(c, name='mecaMAILx_UpdateTherField')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,Tsize
      REAL(C_DOUBLE), INTENT(IN), dimension(Tsize) :: T
       !! PURPOSE
       !!  Update the thermal field on a given body
       !!  You need to initialize with InitTherField

       CALL update_ther_field(IdBody,T,Tsize)

    END SUBROUTINE

END MODULE
