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
MODULE wrap_CSxxx
  
  USE iso_c_binding

  USE CSxxx,ONLY: load_tactors_CSxxx        , &
                  set_precon_node_CSxxx     , &
                  flip_orientation_CSxxx    , &
                  flip_orientation_one_CSpxx, &
                  set_shrink_CSxxx          , &
                  apply_pressure_CSpxx      , &
                  add_reac_CSxxx            , &
                  get_nb_CSxxx              , &
                  clean_memory_CSxxx        , &
                  apply_surface_load_CSpxx  , &
                  increment_CSpxx           , &
                  get_connec_CSpxx          , &
                  get_all_data_CSpxx

CONTAINS

!!!----------------------------------------------------

  SUBROUTINE LoadTactors() bind(c, name='CSxxx_LoadTactors')
  IMPLICIT NONE
     !! PURPOSE
     !!  Initializes existing_entities variable for CSxxx contactors

     CALL load_tactors_CSxxx

  END SUBROUTINE

  SUBROUTINE PushPreconNodes() bind(c, name='CSxxx_PushPreconNodes')
    IMPLICIT NONE
     !! PURPOSE
     !!  Initializes existing_entities variable for CSxxx contactors

     CALL set_precon_node_CSxxx

  END SUBROUTINE 

  SUBROUTINE FlipOrientation(ibdyty) bind(c, name='CSxxx_FlipOrientation')
  IMPLICIT NONE
     !! PURPOSE
     !!  Flip CSxxx normal
     integer(c_int), intent(in), value :: ibdyty

     CALL flip_orientation_CSxxx(ibdyty)

  END SUBROUTINE

  SUBROUTINE FlipOneOrientation(ibdyty,icspxx) bind(c, name='CSxxx_FlipOrientationOnePatch')
  IMPLICIT NONE
     !! PURPOSE
     !!  Flip CSxxx normal
     integer(c_int), intent(in), value :: ibdyty,icspxx

     CALL flip_orientation_one_CSpxx(ibdyty,icspxx)

  END SUBROUTINE

  SUBROUTINE SetShrink(shrink) bind(c, name='CSxxx_SetShrink')
  IMPLICIT NONE

     real(c_double), intent(in), value :: shrink

     CALL Set_Shrink_CSxxx(shrink)

  END SUBROUTINE

  subroutine SetQuadratureCSxxx(ivalue) bind(c, name='CSxxx_SetQuadrature')
    use utilities, only : logmes
    implicit none

    integer(c_int), value :: ivalue

    call logmes('SetQuadratureCSxxx : Obsolete function please remove it in the future',.true.)
    !call faterr('wrap::SetQuadratureCSxxx','Obsolete function')
    !call set_Quadrature_CSxxx(ivalue)

  end subroutine

  SUBROUTINE ApplyPressureCSxxx(ivalue,rvalue) bind(c, name='CSpxx_ApplyPressure')
  IMPLICIT NONE

     integer(c_int), value :: ivalue
     real(c_double), intent(in), value :: rvalue

     ! actualisation de la structure CSpxx courante au cas ou on n'a pas de contact
     call increment_CSpxx()

     
     CALL Apply_Pressure_CSpxx(ivalue,rvalue)

  END SUBROUTINE

  SUBROUTINE ApplySurfaceLoadCSxxx(ivalue,rvalue,length) bind(c, name='CSpxx_ApplySurfaceLoad')
  IMPLICIT NONE

     integer(c_int), value :: ivalue,length
     real(c_double), intent(in) :: rvalue(length)

     CALL apply_surface_load_CSpxx(ivalue,rvalue)

  END SUBROUTINE

  SUBROUTINE AddReacCSxxx(cvalue1_c,iCSxxx,rvalue,length) bind(c, name='CSxxx_AddReac')
     use overall
     use utilities

     IMPLICIT NONE
     character(c_char),intent(in), dimension(5) :: cvalue1_c
     integer(c_int), value :: iCSxxx,length
     real(c_double), intent(in) :: rvalue(length)

      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      select case(cvalue1)
      case('Ireac')
         call add_reac_CSxxx(iCSxxx,rvalue,iIreac)
      case('Iaux_')
        call add_reac_CSxxx(iCSxxx,rvalue,iIaux_)
      case default
        call logmes('moi pas comprendre')
      end select
  end subroutine

  function GetNbCSxxx() bind(c, name='CSxxx_GetNbCSxxx')
    implicit none
    integer(c_int) :: GetNbCSxxx

     GetNBCSxxx = get_nb_CSxxx()

  end function

  subroutine getAllConnec(iptr,idim1) bind(c,name='CSpxx_GetAllConnec')
    implicit none
    type(c_ptr)    :: iptr
    integer(c_int) :: idim1
    integer     , dimension(:), pointer :: idata

    idata => get_connec_CSpxx()

    if( associated(idata) ) then
      iptr  = c_loc(idata(1))
      idim1 = size(idata,1)
    else
      iptr  = c_null_ptr
      idim1 = 0
    end if

  end subroutine getAllConnec

  subroutine getAllData(iptr,idim1,idim2, rptr, rdim1, rdim2) bind(c,name='CSpxx_GetAllData')
    implicit none
    type(c_ptr)    :: iptr, rptr
    integer(c_int) :: idim1, idim2, rdim1, rdim2
    real(kind=8), dimension(:,:), pointer :: rdata
    integer     , dimension(:,:), pointer :: idata

    call get_all_data_CSpxx(idata, rdata)

    if( associated(idata) ) then
      iptr  = c_loc(idata(1,1))
      idim1 = size(idata,1)
      idim2 = size(idata,2)
    else
      iptr  = c_null_ptr
      idim1 = 0
      idim2 = 0
    end if

    if( associated(rdata) ) then
      rptr  = c_loc(rdata(1,1))
      rdim1 = size(rdata,1)
      rdim2 = size(rdata,2)
    else
      rptr  = c_null_ptr
      rdim1 = 0
      rdim2 = 0
    end if
    
  end subroutine getAllData

  subroutine CleanMemory() bind(c, name='CSxxx_CleanMemory')
    implicit none

    call clean_memory_CSxxx

  end subroutine

!!!----------------------------------------------------
END MODULE wrap_CSxxx
