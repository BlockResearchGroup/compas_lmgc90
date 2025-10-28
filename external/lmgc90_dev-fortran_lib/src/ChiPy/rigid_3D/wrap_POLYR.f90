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
MODULE wrap_POLYR

  USE ISO_C_BINDING

  USE POLYR,ONLY:                          &
       read_bodies_POLYR                 , &
       MOVE_POLYR                        , &
       save_vertex_POLYR                 , &
       set_radius_correction_POLYR       , &
       assume_good_orientation_POLYR     , &
       skip_HE_POLYR                     , &
       set_topo_angle, set_flatness_angle, &
       get_wireframe_POLYR               , &
       get_nb_POLYR                      , &
       get_vertex_POLYR                  , & !<- rm
       get_ptr_vertexTT_POLYR            , &
       get_ptr_normalTT_POLYR            , &
       get_POLYR2BDYTY                   , &
       get_ptr_POLYR2BDYTY               , &
       set_threshold_big_POLYR           , &
       set_nb_big_POLYR                  , &
       set_big_POLYR                     , &
       skip_topo_big_POLYR               , &              
       init_outlines_POLYR               , &
       init_scalarfields_POLYR           , &
       get_nb_scalarfields_POLYR         , &
       get_nb_point_outlines_POLYR       , &
       update_postdata_POLYR             , &
       get_all_connectivities_POLYR      , &
       get_ptr_connectivity_POLYR        , &
       get_color_POLYR                   , &
       get_ptr_vertex_ref_POLYR          , &
       get_topo_data_POLYR               , &
       clean_memory_POLYR                , &
       get_visible_POLYR

  use utilities, only : faterr,logmes

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE LoadTactors() bind(c, name='POLYR_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for POLYR contactors

       CALL read_bodies_POLYR

  END SUBROUTINE LoadTactors


  subroutine GetContactorColor(itact, c5) bind(c, name='POLYR_GetContactorColor')
     implicit none
     !! PURPOSE
     !!  return the color of a given polyr

     ! variable d'entree
     integer(c_int), intent(in), value :: itact ! numero du POLYR considere
     type(c_ptr) :: c5
     !
     character(len=5),  pointer :: color
     character(c_char), pointer :: gniii

     allocate(color)
     color = get_color_POLYR(itact)
     gniii => color(1:1)
     c5 = c_loc(gniii)

  end subroutine GetContactorColor


!!!------------------------------------------------------------------------

  SUBROUTINE SaveVertex() bind(c, name='POLYR_SaveVertex')
    IMPLICIT NONE

       CALL save_vertex_POLYR

  END SUBROUTINE SaveVertex

!!!------------------------------------------------------------------------

  SUBROUTINE ModifyRadius(ratio) bind(c, name='POLYR_ModifyRadius')
       implicit none
       real(c_double), intent(in), value :: ratio
       !! PURPOSE
       !!  apply a amplificatio/reduction size factor 

       CALL set_radius_correction_POLYR(ratio)

  END SUBROUTINE ModifyRadius

!!!------------------------------------------------------------------------

  SUBROUTINE SetThresholdBigPolyr(ratio) bind(c, name='POLYR_SetThresholdBigPolyr')
       implicit none
       real(c_double), intent(in), value :: ratio

       CALL set_threshold_big_POLYR(ratio)

  END SUBROUTINE SetThresholdBigPolyr

!!!------------------------------------------------------------------------

  SUBROUTINE SetBigPolyr(itacty) bind(c, name='POLYR_SetBigPolyr')
       implicit none
       integer(c_int), intent(in), value :: itacty

       CALL set_big_POLYR(itacty)

  END SUBROUTINE SetBigPolyr

!!!------------------------------------------------------------------------

  SUBROUTINE SetNbBigPolyr(nb) bind(c, name='POLYR_SetNbBigPolyr')
       implicit none
       integer(c_int), intent(in), value :: nb

       CALL set_nb_big_POLYR(nb)

  END SUBROUTINE SetNbBigPolyr

!!!------------------------------------------------------------------------
  
  SUBROUTINE SkipTopoBigPolyr() bind(c, name='POLYR_SkipTopoBigPolyr')
       implicit none

       CALL skip_topo_big_POLYR()

  END SUBROUTINE SkipTopoBigPolyr

!!!------------------------------------------------------------------------

  SUBROUTINE SkipAutomaticReorientation() bind(c, name='POLYR_SkipAutomaticReorientation')
    implicit none
    !!  disable orientation check based on convexity of objects

       CALL assume_good_orientation_POLYR

  END SUBROUTINE

!!!------------------------------------------------------------------------

  SUBROUTINE SkipHEBuild() bind(c, name='POLYR_SkipHEBuild')
    implicit none
    !! disable HE structure computing 

       CALL skip_HE_POLYR

  END SUBROUTINE

!!!------------------------------------------------------------------------

  SUBROUTINE Topo_angle(angle) bind(c, name='POLYR_TopologyAngle')
    implicit none
    real(c_double), intent(in), value :: angle

    CALL set_topo_angle(angle)

  END SUBROUTINE
!!!------------------------------------------------------------------------
  SUBROUTINE Flatness_angle(angle) bind(c, name='POLYR_FlatnessAngle')
    implicit none
    real(c_double), intent(in), value :: angle

    CALL set_flatness_angle(angle)

  END SUBROUTINE
!!!------------------------------------------------------------------------
  SUBROUTINE GetWireframePolyr(ivalue1,angle,rmat,idim1,idim2,outvect2,size2) bind(c, name='POLYR_GetWireframe')
    implicit none
    ! le rang du polyr
    integer(c_int),intent(in), value :: ivalue1
    real(c_double), intent(in), value :: angle    
    integer(c_int),intent(out) :: idim1, idim2, size2
    ! pointeur sur coor et connectivity
    type(c_ptr) :: rmat,outvect2

    real(kind=8), dimension(:,:), pointer       :: coor         => null()
    integer     , dimension(:)  , pointer, save :: connectivity => null()
    
    if (associated(coor)) then
      deallocate(coor)
      nullify(coor)
    endif
    if (associated(connectivity)) then
      deallocate(connectivity)
      nullify(connectivity)
    endif

    call get_wireframe_POLYR(ivalue1,angle,coor,connectivity)

    if (.not. associated(coor) .or. &
        .not. associated(connectivity)) then
      call faterr('wrap_POLYR::GetWireframe','pointers not associated')
    endif


    idim1 = size(coor,1)
    idim2 = size(coor,2)
    rmat  = c_loc(coor(1,1))

    size2 = size(connectivity)
    outvect2 = c_loc(connectivity(1))

  END SUBROUTINE

!!$  SUBROUTINE GetNbPolyr(nbp) bind(c, name='POLYR_GetNb')
!!$    implicit none
!!$    integer(c_int),intent(out) :: nbp
!!$
!!$    nbp=get_nb_polyr()
!!$       
!!$
!!$  END SUBROUTINE

  subroutine get_vertex(itacty, vertex, dime, nb_vertices) bind(C, name='POLYR_GetVertex')
    implicit none
    integer(c_int), intent(in), value :: itacty
    type(c_ptr)    :: vertex
    integer(c_int) :: dime, nb_vertices
    !
    real(kind=8), dimension(:,:), pointer :: ptr

    ptr => null()
    call get_vertex_polyr(itacty, ptr, dime, nb_vertices)
    if( associated(ptr) ) then
      vertex = c_loc(ptr(1,1))
    else
      dime = 0
      nb_vertices = 0
      vertex = c_null_ptr
    end if

  end subroutine

  subroutine get_ptr_vertexTT(itacty, vertex, dime, nb_vertices) bind(C, name='POLYR_GetPtrVertexTT')
    implicit none
    integer(c_int), intent(in), value :: itacty
    type(c_ptr)    :: vertex
    integer(c_int) :: dime, nb_vertices
    !
    real(kind=8), dimension(:,:), pointer :: ptr

    ptr => get_ptr_vertexTT_polyr(itacty)
    if( associated(ptr) ) then
      dime        = size(ptr,1)
      nb_vertices = size(ptr,2)
      vertex      = c_loc(ptr(1,1))
    else
      dime        = 0
      nb_vertices = 0
      vertex = c_null_ptr
    end if

  end subroutine

  subroutine get_ptr_normalTT(itacty, normal, dime, nb_faces) bind(C, name='POLYR_GetPtrNormalTT')
    implicit none
    integer(c_int), intent(in), value :: itacty
    type(c_ptr)    :: normal
    integer(c_int) :: dime, nb_faces
    !
    real(kind=8), dimension(:,:), pointer :: ptr

    ptr => get_ptr_normalTT_polyr(itacty)
    if( associated(ptr) ) then
      dime        = size(ptr,1)
      nb_faces = size(ptr,2)
      normal      = c_loc(ptr(1,1))
    else
      dime        = 0
      nb_faces = 0
      normal = c_null_ptr
    end if

  end subroutine

  subroutine MoveToConfigurationTT() bind(c, name='POLYR_MoveToConfigurationTT')
    IMPLICIT NONE

       CALL move_POLYR

  end subroutine


  subroutine GetPOLYR2BDYTY(map, dim1, dim2) bind(C, name='POLYR_GetPOLYR2BDYTY')
    implicit none
    type(c_ptr)    :: map
    integer(c_int) :: dim1, dim2
    !
    integer(kind=4), dimension(:,:), pointer :: ptr

    ptr => null()
    call get_POLYR2BDYTY(ptr, dim1, dim2)
    if( associated(ptr) ) then
      map = c_loc(ptr(1,1))
    else
      dim1 = 0
      dim2 = 0
      map = c_null_ptr
    end if

  end subroutine

  subroutine GetPtrPOLYR2BDYTY(map, dim1, dim2) bind(C, name='POLYR_GetPtrPOLYR2BDYTY')
    implicit none
    type(c_ptr)    :: map
    integer(c_int) :: dim1, dim2
    !
    integer(kind=4), dimension(:,:), pointer :: ptr

    ptr => get_ptr_POLYR2BDYTY()
    if( associated(ptr) ) then
      dim1 = size(ptr,1)
      dim2 = size(ptr,2)
      map  = c_loc(ptr(1,1))
    else
      dim1 = 0
      dim2 = 0
      map  = c_null_ptr
    end if

  end subroutine

  FUNCTION IsVisible(itacty) bind(c, name='POLYR_IsVisible')
    IMPLICIT NONE
    INTEGER(c_int), INTENT(in), value :: itacty
    INTEGER(c_int) :: IsVisible

    IF ( get_visible_POLYR(itacty) ) THEN
         IsVisible = 1
    ELSE
         IsVisible = 0
    END IF

  END FUNCTION

  !-- for external vtk visu --!

  function GetNb_POLYR() bind(c, name='POLYR_GetNbPOLYR')
    IMPLICIT NONE
    integer(c_int) :: GetNb_POLYR

    GetNb_POLYR=get_nb_POLYR()

  end function GetNb_POLYR

  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='POLYR_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_POLYR()

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

  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='POLYR_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_POLYR()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_POLYR()
      dim2 = get_nb_POLYR()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine UpdatePostdata() bind(c, name='POLYR_UpdatePostdata')
    implicit none

    call update_postdata_POLYR

  end subroutine

  subroutine GetNbPointOutlines(ptr, length) bind(c, name='POLYR_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_POLYR()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
  

  function GetNbScalarfields() bind(c, name='POLYR_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_POLYR()

  end function GetNbScalarfields

  subroutine GetAllConnectivities(ptr, dim1) bind(c, name='POLYR_GetPtrAllConnectivities')
    implicit none
    integer(kind=c_int) :: dim1
    type(c_ptr) :: ptr
    !
    integer(kind=4), dimension(:), pointer :: connec

    connec => get_all_connectivities_POLYR()

    if( associated(connec) ) then
      ptr    = c_loc(connec(1))
      dim1 = size(connec)
    else
      ptr = c_null_ptr
      dim1 = 0
    end if

  end subroutine

  subroutine get_ptr_Connectivity(itacty, connec, dim1, dim2) bind(c, name='POLYR_GetPtrConnectivity')
    implicit none
    integer(c_int), intent(in), value :: itacty
    type(c_ptr)    :: connec
    integer(c_int) :: dim1, dim2
    !
    integer(kind=4), dimension(:,:), pointer :: ptr

    ptr => null()
    ptr => get_ptr_connectivity_POLYR(itacty)

    if( associated(ptr) ) then
      connec = c_loc(ptr(1,1))
      dim1 = size(ptr,1)
      dim2 = size(ptr,2)
    else
      dim1 = 0
      dim2 = 0
      connec = c_null_ptr
    end if

  end subroutine

  subroutine get_ptr_vertex_ref(itacty, vertex, dime, nb_vertices) bind(C, name='POLYR_GetPtrVertexRef')
    use overall, only: nbDIME
    implicit none
    integer(c_int), intent(in), value :: itacty
    type(c_ptr)    :: vertex
    integer(c_int) :: dime, nb_vertices
    !
    real(kind=8), dimension(:), pointer :: ptr

    ptr => null()
    ptr => get_ptr_vertex_ref_polyr(itacty)
    if( associated(ptr) ) then
      vertex = c_loc(ptr(1))
      dime        = nbDIME ! rm : beurk
      nb_vertices = size(ptr)/nbDIME
    else
      dime        = 0
      nb_vertices = 0
      vertex = c_null_ptr
    end if

  end subroutine

  subroutine GetTopoData(ptr, dim1, dim2) bind(c, name='POLYR_GetTopoData')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    integer, dimension(:,:), pointer :: topo_data

    topo_data => get_topo_data_POLYR()

    if( associated(topo_data) ) then
      ptr    = c_loc(topo_data(1,1))
      dim1 = size(topo_data,1)
      dim2 = size(topo_data,2)
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine CleanMemory() bind(c, name='POLYR_CleanMemory')
    implicit none

    call clean_memory_POLYR

  end subroutine

end module wrap_POLYR
