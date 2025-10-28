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
MODULE wrap_PLANx

  USE ISO_C_BINDING

  USE PLANx,ONLY: &
       read_bodies_PLANx           , &
       get_nb_PLANx                , & !vt
       get_ptr_PLANx2BDYTY         , &
       init_outlines_PLANx         , &
       init_scalarfields_PLANx     , &
       get_nb_scalarfields_PLANx   , &
       get_nb_point_outlines_PLANx , &
       update_postdata_PLANx       , &
       get_all_connectivities_PLANx, &
       clean_memory_PLANx          , &
       get_visible_PLANx
CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE LoadTactors() bind(c, name='PLANx_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for PLANx contactors

       CALL read_bodies_PLANx

  END SUBROUTINE LoadTactors
!!!------------------------------------------------------------------------

  !--vt-- function giving the number of PLANx
  function GetNbPLANx() bind(c, name='PLANx_GetNbPLANx')
    IMPLICIT NONE
    integer(c_int) :: GetNbPLANx

    GetNbPLANx=get_nb_PLANx()

  END function GetNbPLANx

  FUNCTION IsVisible(itacty) bind(c, name='PLANx_IsVisible')
    IMPLICIT NONE
    INTEGER(c_int), INTENT(in), value :: itacty
    INTEGER(c_int) :: IsVisible

    IF ( get_visible_PLANx(itacty) ) THEN
         IsVisible = 1
    ELSE
         IsVisible = 0
    END IF

  END FUNCTION
  !-- for external vtk visu --!

  subroutine GetPtrPLANx2BDYTY(ptr, dim1, dim2) bind(c, name='PLANx_GetPtrPLANx2BDYTY')
     implicit none
     integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur planx2bdyty
     type(c_ptr)                 :: ptr ! pointeur sur la table de correpondance
     !
     integer(c_int), dimension(:,:), pointer :: ivect

     ivect => get_ptr_PLANx2BDYTY()

   if( associated(ivect) ) then
     ptr = c_loc(ivect(1,1))
     dim1 = size(ivect,1)
     dim2 = size(ivect,2)
   else
     ptr = c_null_ptr
     dim1 = 0
     dim2 = 0
   end if

  end subroutine GetPtrPLANx2BDYTY

  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='PLANx_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_PLANx()

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

  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='PLANx_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_PLANx()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_PLANx()
      dim2 = get_nb_PLANx()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine UpdatePostdata() bind(c, name='PLANx_UpdatePostdata')
    implicit none

    call update_postdata_PLANx

  end subroutine

  subroutine GetNbPointOutlines(ptr, length) bind(c, name='PLANx_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_PLANx()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
  

  function GetNbScalarfields() bind(c, name='PLANx_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_PLANx()

  end function GetNbScalarfields

  subroutine GetAllConnectivities(ptr, dim1) bind(c, name='PLANx_GetPtrAllConnectivities')
    implicit none
    integer(kind=c_int) :: dim1
    type(c_ptr) :: ptr
    !
    integer(kind=4), dimension(:), pointer :: connec

    connec => get_all_connectivities_PLANx()

    if( associated(connec) ) then
      ptr    = c_loc(connec(1))
      dim1 = size(connec)
    else
      ptr = c_null_ptr
      dim1 = 0
    end if

  end subroutine

  subroutine CleanMemory() bind(c, name='PLANx_CleanMemory')
    implicit none

    call clean_memory_PLANx

  end subroutine

END MODULE wrap_PLANx
