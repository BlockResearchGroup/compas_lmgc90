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
MODULE wrap_DISKx

  USE ISO_C_BINDING

  use utilities

  USE diskx,ONLY:&
      read_bodies_DISKx, &
      get_nb_DISKx, &
      get_DISKx2BDYTY, & ! <- am: debut des fonctions supplementaires
      get_ptr_DISKx2BDYTY, &
      get_radius_DISKx, &
      get_mean_radius_DISKx, &
      get_max_radius_DISKx, &
      get_min_radius_DISKx, &
      get_color_diskx     => get_color  , &
      !get_body_radius_DISKx, &
      get_coor_diskx      => get_coor   , &
      diskx2bdyty, &
      get_visible_diskx   => get_visible, &
      get_nb_point_outlines_DISKx, &
      get_nb_scalarfields_DISKx  , &
      init_outlines_DISKx        , &
      init_scalarfields_DISKx    , &
      update_postdata_DISKx      , &
      clean_memory_DISKx         , &
      set_Vd_DISKx               , &
      set_Xd_DISKx              

  PUBLIC 

CONTAINS

!!!---------------------------------------------------------------------

    SUBROUTINE LoadTactors() bind(c, name='DISKx_LoadTactors')
      IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for DISKx contactors

       CALL read_bodies_DISKx

    END SUBROUTINE

    function GetNbDISKx() bind(c, name='DISKx_GetNbDISKx')
      IMPLICIT NONE
      integer(c_int) :: GetNbDISKx
       !! PURPOSE
       !!  return the number of diskx in container

      GetNbDISKx=get_nb_DISKx()

    END function GetNbDISKx
    
    !fd mierda 
    ! subroutine GetDISKx2RBDY2(diskx2rbdy2_, nb_disk_) bind(c, name='DISKx_GetDISKx2RBDY2')
    !    implicit none
    !    integer(c_int) :: nb_disk_ 
    !    type(c_ptr)    :: diskx2rbdy2_
    !    !
    !    integer(c_int), dimension(:), pointer :: d2r

    !    nb_disk_ = get_nb_DISKx()
    !    allocate(d2r(nb_disk_))

    !    call get_DISKx2RBDY2(d2r, nb_disk_)

    !    diskx2rbdy2_ = c_loc(d2r(1))
    
    ! end subroutine GetDISKx2RBDY2
    
  subroutine GetDISKx2BDYTY(map, dim1, dim2) bind(C, name='DISKx_GetDISKx2BDYTY')
    implicit none
    type(c_ptr)    :: map
    integer(c_int) :: dim1, dim2
    !
    integer(kind=4), dimension(:,:), pointer :: ptr

    ptr => null()
    call get_DISKx2BDYTY(ptr, dim1, dim2)
    if( associated(ptr) ) then
      map = c_loc(ptr(1,1))
    else
      dim1 = 0
      dim2 = 0
      map = c_null_ptr
    end if

  end subroutine

    subroutine GetPtrDISKx2BDYTY(ptr, dim1, dim2) bind(c, name='DISKx_GetPtrDISKx2BDYTY')
       implicit none
       integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur diskx2bdyty
       type(c_ptr)                 :: ptr ! pointeur sur la table de correpondance
       !
       integer(c_int), dimension(:,:), pointer :: ivect

       ivect => get_ptr_DISKx2BDYTY()

     if( associated(ivect) ) then
       ptr = c_loc(ivect(1,1))
       dim1 = size(ivect,1)
       dim2 = size(ivect,2)
     else
       ptr = c_null_ptr
       dim1 = 0
       dim2 = 0
     end if

    
    end subroutine GetPtrDISKx2BDYTY

    function IsVisible(itact) bind(c, name='DISKx_IsVisible')
       implicit none
       integer(c_int), intent(in), value :: itact
       integer(c_int) :: IsVisible

       if (get_visible_diskx(itact)) then
         IsVisible = 1
       else
         IsVisible = 0
       end if

    end function

    function GetContactorRadiusDISKx(itact) bind(c, name='DISKx_GetContactorRadius')
      implicit none
      integer(c_int), intent(in), value :: itact ! indice du DISKx dont on veut recuperer le rayon
      real(c_double) :: GetContactorRadiusDISKx  ! rayon du DISKx, d'indice itact
      !! PURPOSE
      !!  return the radius of a given diskx

      GetContactorRadiusDISKx = get_radius_DISKx(itact)
    end function GetContactorRadiusDISKx
  
    function GetMeanRadiusDISKx() bind(c, name='DISKx_GetMeanRadius')
       implicit none
       real(c_double) :: GetMeanRadiusDISKx ! rayon moyen des DISKx
       !! PURPOSE
       !!  return the mean radius of the diskx in container

       GetMeanRadiusDISKx = get_mean_radius_DISKx()

    end function GetMeanRadiusDISKx

    function GetMaxRadiusDISKx() bind(c, name='DISKx_GetMaxRadius')
       implicit none
       real(c_double) :: GetMaxRadiusDISKx ! rayon du plus grand DISKx
       !! PURPOSE
       !!  return the max radius of the diskx in container
                
       GetMaxRadiusDISKx = get_max_radius_DISKx()

    end function GetMaxRadiusDISKx

    function GetMinRadiusDISKx() bind(c, name='DISKx_GetMinRadius')
       implicit none
       real(c_double) :: GetMinRadiusDISKx ! rayon du plus petit DISKx
       !! PURPOSE
       !!  return the min radius of the diskx in container

       GetMinRadiusDISKx = get_min_radius_DISKx()

    end function GetMinRadiusDISKx

    subroutine GetContactorColor(itact, c5) bind(c, name='DISKx_GetContactorColor')
       implicit none
       !! PURPOSE
       !!  return the color of a given diskx

       ! variable d'entree
       integer(c_int), intent(in), value :: itact ! numero du DISKx considere
       type(c_ptr) :: c5
       !
       character(len=5),  pointer :: color
       character(c_char), pointer :: gniii

       allocate(color)
       color = get_color_DISKx(itact)
       gniii => color(1:1)
       c5 = c_loc(gniii)

    end subroutine GetContactorColor

    ! function GetRadiusDISKx(ibdyty) bind(c, name='DISKx_GetRadius')
    !   implicit none
    !   integer(c_int), intent(in), value :: ibdyty
    !   real(c_double) :: getRadiusDISKx

    !   getRadiusDISKx = get_body_radius_DISKx(ibdyty)

    ! end function
    
    function GetRadiusDISKx(itacty) bind(c, name='DISKx_GetRadius')
      implicit none
      integer(c_int), intent(in), value :: itacty
      real(c_double) :: getRadiusDISKx

      getRadiusDISKx = get_radius_DISKx(itacty)

    end function

    SUBROUTINE GetContactorCoor(itacty, coor, nbdof) bind(c, name='DISKx_GetContactorCoor')
      !! PURPOSE
      !!
      IMPLICIT NONE
      integer(c_int),intent(in), value :: itacty
      integer(c_int)                   :: nbdof
      type(c_ptr)                      :: coor
      !
      real(c_double), dimension(:), pointer :: c

      !                         1234567890123
      character(len=13) :: IAM='DISKx_GetCoor'

      nbdof = 3
      allocate(c(nbdof))

      ! on calcule less coordonnees du centre d'inertie du contacteur considere
      c = get_coor_DISKx(itacty)

      coor = c_loc(c(1))

    END SUBROUTINE GetContactorCoor
 
    subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='DISKx_InitOutlines')
      implicit none
      integer(kind=c_int) :: dim1, dim2
      type(c_ptr) :: ptr
      !
      real(kind=8), dimension(:,:), pointer :: outlines

      outlines => init_outlines_DISKx()

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

    subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='DISKx_InitScalarFields')
      implicit none
      integer(kind=c_int) :: dim1, dim2
      type(c_ptr) :: ptr
      !
      real(kind=8), dimension(:), pointer :: scalarfields

      scalarfields => init_scalarfields_DISKx()

      if( associated(scalarfields) ) then
        ptr  = c_loc(scalarfields(1))
        dim1 = get_nb_scalarfields_DISKx()
        dim2 = get_nb_DISKx()
      else
        ptr = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine

    subroutine UpdatePostdata() bind(c, name='DISKx_UpdatePostdata')
      implicit none

      call update_postdata_DISKx

    end subroutine

    subroutine GetNbPointOutlines(ptr, length) bind(c, name='DISKx_GetNbPointOutlines')
      implicit none
      type(c_ptr)    :: ptr
      integer(c_int) :: length
      !
      integer(kind=4), dimension(:), pointer :: nb_point_outlines

      nb_point_outlines => get_nb_point_outlines_DISKx()

      if( associated(nb_point_outlines) ) then
        ptr    = c_loc(nb_point_outlines(1))
        length = size(nb_point_outlines)
      else
        ptr = c_null_ptr
        length = 0
      end if

    end subroutine GetNbPointOutlines
    

    function GetNbScalarfields() bind(c, name='DISKx_GetNbScalarFields')
      implicit none
      integer(c_int) :: GetNbScalarfields

      GetNbScalarfields = get_nb_scalarfields_DISKx()

    end function GetNbScalarfields
    
    subroutine CleanMemory() bind(c, name='DISKx_CleanMemory')
      implicit none
  
      call clean_memory_DISKx
  
    end subroutine

!!!---------------------------------------------------------------------    
    
    subroutine setVdDISKx(itacty,V) bind(C,name='DISKx_SetVdilation')
    implicit none
      integer(C_INT),intent(in),value :: itacty
      real(C_DOUBLE),intent(in),value :: V
      call set_Vd_DISKx(itacty,V)
    end subroutine setVdDISKx

!!!---------------------------------------------------------------------

   subroutine setXdDISKx(itacty,V) bind(C,name='DISKx_SetXdilation')
   implicit none
     integer(C_INT),intent(in),value :: itacty
     real(C_DOUBLE),intent(in),value :: V
     call set_Xd_DISKx(itacty,V)
   end subroutine setXdDISKx


END MODULE wrap_DISKx
