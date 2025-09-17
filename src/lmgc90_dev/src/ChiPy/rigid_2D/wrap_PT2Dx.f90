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
MODULE wrap_PT2Dx


  !!****h* LMGC90.CHIPY/PT2Dx
  !! NAME
  !!  module wrap_PT2Dx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/PT2Dx
  !!****


  USE ISO_C_BINDING

  USE PT2Dx,ONLY:&
       read_bodies_PT2Dx, &
       set_pt2dx_radius_pt2dx, &
       get_nb_pt2dx, &
       get_ptr_PT2Dx2BDYTY, &
       get_visible          , &
       get_nb_point_outlines_PT2Dx, &
       get_nb_scalarfields_PT2Dx  , &
       init_outlines_PT2Dx        , &
       init_scalarfields_PT2Dx    , &
       update_postdata_PT2Dx      , &
       clean_memory_PT2Dx

  PUBLIC 

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE LoadTactors() bind(c, name='PT2Dx_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for PT2Dx contactors

       CALL read_bodies_PT2Dx

  END SUBROUTINE

  function GetNbPT2Dx() bind(c, name='PT2Dx_GetNbPT2Dx')
    implicit none
    integer(c_int) :: GetNbPT2Dx
     !! PURPOSE
     !!  return the number of diskx in container

    GetNbPT2Dx=get_nb_PT2Dx()

  end function GetNbPT2Dx
    
  SUBROUTINE SetDisplayRadius(rvalue) bind(c, name='PT2Dx_SetDisplayRadius')
    implicit none
    real(c_double), intent(in), value :: rvalue

    CALL set_pt2dx_radius_pt2dx(rvalue)

  END SUBROUTINE

  subroutine GetPtrPT2Dx2BDYTY(ptr, dim1, dim2) bind(c, name='PT2Dx_GetPtrPT2Dx2BDYTY')
     implicit none
     integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur pt2dx2rbdy2
     type(c_ptr)                 :: ptr ! pointeur sur la table de correpondance
     !
     integer(c_int), dimension(:,:), pointer :: ivect

     ivect => get_ptr_PT2Dx2BDYTY()


     if( associated(ivect) ) then
       ptr = c_loc(ivect(1,1))
       dim1 = size(ivect,1)
       dim2 = size(ivect,2)
     else
       ptr = c_null_ptr
       dim1 = 0
       dim2 = 0
     end if

  
  end subroutine GetPtrPT2Dx2BDYTY

  function IsVisible(itact) bind(c, name='PT2Dx_IsVisible')
     implicit none
     integer(c_int), intent(in), value :: itact
     integer(c_int) :: IsVisible

     if (get_visible(itact)) then
       IsVisible = 1
     else
       IsVisible = 0
     end if

  end function

  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='PT2Dx_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_PT2Dx()

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

  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='PT2Dx_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_PT2Dx()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_PT2Dx()
      dim2 = get_nb_PT2Dx()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine UpdatePostdata() bind(c, name='PT2Dx_UpdatePostdata')
    implicit none

    call update_postdata_PT2Dx

  end subroutine

  subroutine GetNbPointOutlines(ptr, length) bind(c, name='PT2Dx_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_PT2Dx()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
  

  function GetNbScalarfields() bind(c, name='PT2Dx_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_PT2Dx()

  end function GetNbScalarfields

  subroutine CleanMemory() bind(c, name='PT2Dx_CleanMemory')
    implicit none
  
    call clean_memory_PT2Dx
  
  end subroutine

END MODULE wrap_PT2Dx
