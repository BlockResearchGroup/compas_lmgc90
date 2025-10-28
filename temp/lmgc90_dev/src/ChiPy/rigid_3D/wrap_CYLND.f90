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
MODULE wrap_CYLND

  USE ISO_C_BINDING

  USE CYLND,ONLY: &
       read_bodies_CYLND           , &
       get_nb_CYLND                , &
       get_axes_CYLND              , &
       get_ptr_CYLND2BDYTY         , &
       init_outlines_CYLND         , &
       init_scalarfields_CYLND     , &
       get_nb_scalarfields_CYLND   , &
       get_nb_point_outlines_CYLND , &
       update_postdata_CYLND       , &
       get_all_connectivities_CYLND, &
       clean_memory_CYLND          , &
       get_visible_CYLND

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE LoadTactors() bind(c, name='CYLND_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for CYLND contactors

       CALL read_bodies_CYLND

  END SUBROUTINE LoadTactors
!!!------------------------------------------------------------------------

  FUNCTION IsVisible(itacty) bind(c, name='CYLND_IsVisible')
    IMPLICIT NONE
    INTEGER(c_int), INTENT(in), value :: itacty
    INTEGER(c_int) :: IsVisible

    IF ( get_visible_CYLND(itacty) ) THEN
         IsVisible = 1
    ELSE
         IsVisible = 0
    END IF

  END FUNCTION

  !-- for external vtk visu --!

  function GetNbCYLND() bind(c, name='CYLND_GetNbCYLND')
    IMPLICIT NONE
    integer(c_int) :: GetNbCYLND

    GetNbCYLND=get_nb_CYLND()

  end function GetNbCYLND

  subroutine GetShapeCYLND(itacty,shape,sz) bind(c, name='CYLND_GetShape')
    IMPLICIT NONE
    integer(c_int), intent(in),value :: itacty
    integer(c_int)                   :: sz
    type(c_ptr)                      :: shape
    !
    real(c_double),dimension(:),pointer :: js

    sz = 2
    allocate(js(sz))

    js=get_axes_CYLND(itacty)

    shape = c_loc(js(1))

  end subroutine GetShapeCYLND



  subroutine GetPtrCYLND2BDYTY(ptr, dim1, dim2) bind(c, name='CYLND_GetPtrCYLND2BDYTY')
     implicit none
     integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur cylnd2bdyty
     type(c_ptr)                 :: ptr ! pointeur sur la table de correpondance
     !
     integer(c_int), dimension(:,:), pointer :: ivect

     ivect => get_ptr_CYLND2BDYTY()

   if( associated(ivect) ) then
     ptr = c_loc(ivect(1,1))
     dim1 = size(ivect,1)
     dim2 = size(ivect,2)
   else
     ptr = c_null_ptr
     dim1 = 0
     dim2 = 0
   end if

  end subroutine GetPtrCYLND2BDYTY

  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='CYLND_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_CYLND()

    if( associated(outlines) ) then
      ptr    = c_loc(outlines(1,1))
      dim1 = size(outlines,1)
      dim2 = size(outlines,2)
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='CYLND_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_CYLND()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_CYLND()
      dim2 = get_nb_CYLND()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine UpdatePostdata() bind(c, name='CYLND_UpdatePostdata')
    implicit none

    call update_postdata_CYLND

  end subroutine

  subroutine GetNbPointOutlines(ptr, length) bind(c, name='CYLND_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_CYLND()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
  

  function GetNbScalarfields() bind(c, name='CYLND_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_CYLND()

  end function GetNbScalarfields
    
  subroutine GetAllConnectivities(ptr, dim1) bind(c, name='CYLND_GetPtrAllConnectivities')
    implicit none
    integer(kind=c_int) :: dim1
    type(c_ptr) :: ptr
    !
    integer(kind=4), dimension(:), pointer :: connec

    connec => get_all_connectivities_CYLND()

    if( associated(connec) ) then
      ptr    = c_loc(connec(1))
      dim1 = size(connec)
    else
      ptr = c_null_ptr
      dim1 = 0
    end if

  end subroutine

  subroutine CleanMemory() bind(c, name='CYLND_CleanMemory')
    implicit none

    call clean_memory_CYLND

  end subroutine

END MODULE wrap_CYLND
