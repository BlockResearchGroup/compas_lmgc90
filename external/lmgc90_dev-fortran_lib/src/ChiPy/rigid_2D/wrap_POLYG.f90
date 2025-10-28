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
MODULE wrap_POLYG

  !!****h* LMGC90.CHIPY/POLYG
  !! NAME
  !!  module wrap_POLYG
  !! PURPOSE
  !!  Wrap the public API
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/POLYG
  !!****

  USE ISO_C_BINDING

  USE POLYG,ONLY:&
       read_bodies_POLYG, &
       !change_polyg2mailx, &
       !change_polyg2mailx_fine, &
       get_nb_POLYG, &
       get_polyg2bdyty, &
       get_radius_POLYG, &
       get_visible               , &
       get_nb_vertices_POLYG, &
       get_vertices_POLYG, &
       get_nb_vertex_POLYG, &
       get_vertex_POLYG, &
       get_body_id_POLYG, &
       get_ptr_polyg2bdyty, &
       get_nb_point_outlines_POLYG, &
       get_nb_scalarfields_POLYG  , &
       init_outlines_POLYG        , &
       init_scalarfields_POLYG    , &
       update_postdata_POLYG, &
       clean_memory_POLYG, &
       set_Xd_POLYG, &
       set_Vd_POLYG, &
       update_radii_POLYG, &
       update_normals_ref_POLYG, &
       get_min_radius_polyg, &
       get_max_radius_polyg


  PUBLIC
  
CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE LoadTactors() bind(c, name='POLYG_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for POLYG contactors

       CALL read_bodies_POLYG

  END SUBROUTINE

!!!---------------------------------------------------------------------

!!$  SUBROUTINE Polyg2Mailx() bind(c, name='POLYG_Polyg2Mailx')
!!$    IMPLICIT NONE
!!$       !! PURPOSE
!!$       !!  transformation of POLYG in MAILx
!!$       print*,' @ Warning: we transform all POLYG into MAILx' 
!!$       CALL change_polyg2mailx
!!$
!!$       STOP
!!$  
!!$  END SUBROUTINE
!!$
!!$!!!---------------------------------------------------------------------
!!$
!!$  SUBROUTINE Polyg2fineMAILx() bind(c, name='POLYG_Polyg2fineMailx')
!!$    IMPLICIT NONE
!!$       !! PURPOSE
!!$       !!  transformation of POLYG in fine MAILx
!!$       print*,' @ Warning: we transform all POLYG into fine MAILx' 
!!$       CALL change_polyg2mailx_fine
!!$
!!$       STOP
!!$
!!$  END SUBROUTINE
!!$
!!!---------------------------------------------------------------------
  function GetNbPOLYG() bind(c, name='POLYG_GetNbPOLYG')
    IMPLICIT NONE
    integer(c_int) :: GetNbPOLYG
     !! PURPOSE
     !!  return the number of polyg in container
    GetNbPOLYG=get_nb_POLYG()

  END function GetNbPOLYG
  
  ! subroutine GetPOLYG2RBDY2(polyg2rbdy2_, nb_polyg_) bind(c, name='POLYG_GetPOLYG2RBDY2')
  !    implicit none
  !    integer(c_int) :: nb_polyg_
  !    type(c_ptr)    :: polyg2rbdy2_
  !    !
  !    integer(c_int), dimension(:), pointer :: p2r 

  !    nb_polyg_ = get_nb_POLYG()

  !    allocate(p2r(nb_polyg_))
  !    call get_POLYG2RBDY2(p2r, nb_polyg_)

  !    polyg2rbdy2_ = c_loc(p2r(1))
  
  ! end subroutine GetPOLYG2RBDY2
  
  subroutine GetPOLYG2BDYTY(map, dim1, dim2) bind(C, name='POLYG_GetPOLYG2BDYTY')
    implicit none
    type(c_ptr)    :: map
    integer(c_int) :: dim1, dim2
    !
    integer(kind=4), dimension(:,:), pointer :: ptr

    ptr => null()
    call get_POLYG2BDYTY(ptr, dim1, dim2)
    if( associated(ptr) ) then
      map = c_loc(ptr(1,1))
    else
      dim1 = 0
      dim2 = 0
      map = c_null_ptr
    end if

  end subroutine

!!!---------------------------------------------------------------------

  subroutine GetPtrPOLYG2BDYTY(ptr, dim1, dim2) bind(c, name='POLYG_GetPtrPOLYG2BDYTY')
     implicit none
     integer(c_int), intent(out) :: dim1,dim2 ! taille du vecteur polyg2rbdy2
     type(c_ptr)                 :: ptr       ! pointeur sur la table de correpondance
     !
     integer(c_int), dimension(:,:), pointer :: ivect

     ivect => get_ptr_POLYG2BDYTY()

     if( associated(ivect) ) then
       ptr = c_loc(ivect(1,1))
       dim1 = size(ivect,1)
       dim2 = size(ivect,2)
     else
       ptr = c_null_ptr
       dim1 = 0
       dim2 = 0
     end if
  
  end subroutine GetPtrPOLYG2BDYTY
  
  function IsVisible(itact) bind(c, name='POLYG_IsVisible')
     implicit none
     integer(c_int), intent(in), value :: itact
     integer(c_int) :: IsVisible

     if (get_visible(itact)) then
       IsVisible = 1
     else
       IsVisible = 0
     end if

  end function

  function GetContactorRadiusPOLYG(itact) bind(c, name='POLYG_GetContactorRadius')
    implicit none
    integer(c_int), intent(in), value :: itact ! indice du POLYG dont on veut recuperer le rayon
    real(c_double) :: GetContactorRadiusPOLYG  ! rayon du POLYG, d'indice itact
    !! PURPOSE
    !!  return the radius of a given polyg

    GetContactorRadiusPOLYG = get_radius_POLYG(itact)

  end function GetContactorRadiusPOLYG
  

  function getNbVerticesPOLYG(ibdyty) bind(C, name='POLYG_GetNbVertices')
    implicit none
    integer(C_INT), intent(in), value :: ibdyty
    integer(C_INT) :: getNbVerticesPOLYG

    getNbVerticesPOLYG = get_nb_vertices_POLYG(ibdyty)

  end function

  subroutine getVerticesPOLYG(ibdyty, vertices, size1, size2) bind(C, name='POLYG_GetVertices')
    implicit none
    integer(C_INT), intent(in), value :: ibdyty
    type(C_PTR)                       :: vertices
    integer(C_INT)                    :: size1, size2
    !
    real(C_DOUBLE), dimension(:,:), pointer :: ptr_vertices

    size1 = 2
    size2 = get_nb_vertices_POLYG(ibdyty)

    allocate(ptr_vertices(size1,size2))
    call get_vertices_POLYG(ibdyty, ptr_vertices)

    vertices = c_loc(ptr_vertices(1,1))

  end subroutine

!!!---------------------------------------------------------------------
  
  subroutine updateRadiiPOLYG() bind(C, name='POLYG_UpdateRadii')
    implicit none
    call update_radii_POLYG()
  end subroutine updateRadiiPOLYG

!!!---------------------------------------------------------------------
  
  subroutine updateNormalsRefPOLYG() bind(C, name='POLYG_UpdateNormalsRef')
    implicit none
    call update_normals_ref_POLYG()
  end subroutine updateNormalsRefPOLYG

!!!---------------------------------------------------------------------
  
  function getMinRadius() bind(c, name='POLYG_GetMinRadius')
    implicit none
    real(C_DOUBLE) :: getMinRadius

    getMinRadius = get_min_radius_POLYG()

  end function

!!!---------------------------------------------------------------------

  function getMaxRadius() bind(c, name='POLYG_GetMaxRadius')
    implicit none
    real(C_DOUBLE) :: getMaxRadius

    getMaxRadius = get_max_radius_POLYG()

  end function


!!!---------------------------------------------------------------------  
  
  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='POLYG_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_POLYG()

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

!!!---------------------------------------------------------------------
  
  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='POLYG_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_POLYG()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_POLYG()
      dim2 = get_nb_POLYG()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

!!!---------------------------------------------------------------------
  
  subroutine UpdatePostdata() bind(c, name='POLYG_UpdatePostdata')
    implicit none

    call update_postdata_POLYG

  end subroutine

!!!---------------------------------------------------------------------
  
  subroutine GetNbPointOutlines(ptr, length) bind(c, name='POLYG_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_POLYG()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
    
!!!---------------------------------------------------------------------
  
  function GetNbScalarfields() bind(c, name='POLYG_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_POLYG()

  end function GetNbScalarfields
  
!!!---------------------------------------------------------------------
  
  function get_nb_vertex(itacty) bind(c, name='POLYG_GetNbVertex')
  implicit none
  integer(C_INT), intent(in), value :: itacty
  integer(C_INT) :: get_nb_vertex

  get_nb_vertex = get_nb_vertex_POLYG(itacty)
  end function

!!!---------------------------------------------------------------------
  
  subroutine get_vertex(itacty, vertex, nb_vertices) bind(C, name='POLYG_GetVertex')
  implicit none
  integer(C_INT), intent(in), value :: itacty, nb_vertices
  real(C_DOUBLE), dimension(nb_vertices) :: vertex

  call get_vertex_polyg(itacty, vertex)
  end subroutine get_vertex

!!!---------------------------------------------------------------------

  function GetBodyId_POLYG(itacty) bind(C,name='POLYG_GetBodyId')
    implicit none
    integer(C_INT),intent(in),value :: itacty
    integer(C_INT)                  :: GetBodyId_POLYG
    GetBodyId_POLYG = get_body_id_POLYG(itacty)
  end function

!!!---------------------------------------------------------------------
  
  subroutine setVdPolyg(itacty,ivertex,V) bind(C,name='POLYG_SetVdilation')
    implicit none
    integer(C_INT),intent(in),value :: itacty,ivertex
    real(C_DOUBLE),dimension(2)     :: V
    call set_Vd_POLYG(itacty,ivertex,V)
  end subroutine setVdPolyg

!!!---------------------------------------------------------------------
  
  subroutine setXdPolyg(itacty,ivertex,V) bind(C,name='POLYG_SetXdilation')
    implicit none
    integer(C_INT),intent(in),value :: itacty,ivertex
    real(C_DOUBLE),dimension(2)     :: V
    call set_Xd_POLYG(itacty,ivertex,V)
  end subroutine setXdPolyg

!!!---------------------------------------------------------------------  
  
  subroutine CleanMemory() bind(c, name='POLYG_CleanMemory')
    implicit none
  
    call clean_memory_POLYG
  
  end subroutine

END MODULE wrap_POLYG
