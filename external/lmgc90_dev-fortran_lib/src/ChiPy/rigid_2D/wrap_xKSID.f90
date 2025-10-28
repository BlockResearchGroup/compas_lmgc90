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
MODULE wrap_xKSID

  USE ISO_C_BINDING

  USE xKSID,ONLY:&
       read_bodies_xKSID          , &
       get_nb_xKSID               , &
       get_ptr_xKSID2BDYTY        , &
       get_radius_xKSID           , &
       get_coor                   , &
       xksid2bdyty                , &
       !get_visible                , &
       get_nb_point_outlines_xKSID, &
       get_nb_scalarfields_xKSID  , &
       init_outlines_xKSID        , &
       init_scalarfields_xKSID    , &
       update_postdata_xKSID      , &
       clean_memory_xKSID         , &
       set_Vd_xKSID               , &
       set_Xd_xKSID              

  PUBLIC 

CONTAINS

!!!---------------------------------------------------------------------
  SUBROUTINE LoadTactors() bind(c, name='xKSID_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for xKSID contactors

       CALL read_bodies_xKSID

  END SUBROUTINE

  function GetNbxKSID() bind(c, name='xKSID_GetNbxKSID')
    implicit none
    integer(c_int) :: GetNbxKSID
     !! PURPOSE
     !!  return the number of xksid in container

    GetNbxKSID=get_nb_xKSID()

  end function GetNbxKSID
    
  subroutine GetPtrxKSID2BDYTY(ptr, dim1, dim2) bind(c, name='xKSID_GetPtrxKSID2BDYTY')
     implicit none
     integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur xksid2rbdy2
     type(c_ptr)                 :: ptr ! pointeur sur la table de correpondance
     !
     integer(c_int), dimension(:,:), pointer :: ivect

     ivect => get_ptr_xKSID2BDYTY()

     if( associated(ivect) ) then
       ptr = c_loc(ivect(1,1))
       dim1 = size(ivect,1)
       dim2 = size(ivect,2)
     else
       ptr = c_null_ptr
       dim1 = 0
       dim2 = 0
     end if
 
  
  end subroutine GetPtrxKSID2BDYTY

  function IsVisible(itact) bind(c, name='xKSID_IsVisible')
     implicit none
     integer(c_int), intent(in), value :: itact
     integer(c_int) :: IsVisible

     !if (get_visible(itact)) then
       IsVisible = 1
     !else
     !  IsVisible = 0
     !end if

  end function

  function GetContactorRadiusxKSID(itact) bind(c, name='xKSID_GetContactorRadius')
    implicit none
    integer(c_int), intent(in), value :: itact ! indice du xKSID dont on veut recuperer le rayon
    real(c_double) :: GetContactorRadiusxKSID  ! rayon du xKSID, d'indice itact
    !! PURPOSE
    !!  return the radius of a given xksid

    GetContactorRadiusxKSID = get_radius_xKSID(itact)
  end function GetContactorRadiusxKSID
  
  subroutine GetContactorCoor(itacty, coor, nbdof) bind(c, name='xKSID_GetContactorCoor')
    implicit none
    integer(c_int),intent(in), value :: itacty
    integer(c_int)                   :: nbdof
    type(c_ptr)                      :: coor
    !
    real(c_double), dimension(:), pointer :: c

    !                         1234567890123
    character(len=13) :: IAM='xKSID_GetCoor'

    nbdof = 3
    allocate(c(nbdof))

    ! on calcule less coordonnees du centre d'inertie du contacteur considere
    c = get_coor(itacty)

    coor = c_loc(c(1))

  end subroutine GetContactorCoor
 
  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='xKSID_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_xKSID()

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

  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='xKSID_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_xKSID()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_xKSID()
      dim2 = get_nb_xKSID()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine UpdatePostdata() bind(c, name='xKSID_UpdatePostdata')
    implicit none

    call update_postdata_xKSID

  end subroutine

  subroutine GetNbPointOutlines(ptr, length) bind(c, name='xKSID_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_xKSID()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
  

  function GetNbScalarfields() bind(c, name='xKSID_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_xKSID()

  end function GetNbScalarfields
    
  subroutine CleanMemory() bind(c, name='xKSID_CleanMemory')
    implicit none
  
    call clean_memory_xKSID
  
  end subroutine

!!!---------------------------------------------------------------------    
    
    subroutine setVdxKSID(itacty,V) bind(C,name='xKSID_SetVdilation')
    implicit none
      integer(C_INT),intent(in),value :: itacty
      real(C_DOUBLE),intent(in),value :: V
      call set_Vd_xKSID(itacty,V)
    end subroutine setVdxKSID

!!!---------------------------------------------------------------------

   subroutine setXdxKSID(itacty,V) bind(C,name='xKSID_SetXdilation')
   implicit none
     integer(C_INT),intent(in),value :: itacty
     real(C_DOUBLE),intent(in),value :: V
     call set_Xd_xKSID(itacty,V)
   end subroutine setXdxKSID


  
END MODULE wrap_xKSID
