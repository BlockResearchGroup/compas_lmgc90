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
MODULE wrap_JONCx

  USE ISO_C_BINDING

  use JONCx, only : joncx2bdyty, &
                    read_bodies_JONCx, &
                    get_nb_JONCx, &
                    get_axes_JONCx, &
                    get_body_id_JONCx, &
                    get_coor , &
                    get_ptr_JONCx2BDYTY, &
                    get_visible                , &
                    get_nb_point_outlines_JONCx, &
                    get_nb_scalarfields_JONCx  , &
                    init_outlines_JONCx        , &
                    init_scalarfields_JONCx    , &
                    update_postdata_JONCx      , &
                    clean_memory_JONCx

  PUBLIC

CONTAINS

!!!---------------------------------------------------------------------

    SUBROUTINE LoadTactors() bind(c, name='JONCx_LoadTactors')
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for JONCx contactors
       IMPLICIT NONE

       CALL read_bodies_JONCx

    END SUBROUTINE

    function GetNbJONCx() bind(c, name='JONCx_GetNbJONCx')
      IMPLICIT NONE
      integer(c_int) :: GetNbJONCx
       !! PURPOSE
       !!  return the number of joncx in container

      GetNbJONCx=get_nb_JONCx()

    END function GetNbJONCx

    subroutine GetShapeJONCx(itacty,shape,sz) bind(c, name='JONCx_GetShape')
      IMPLICIT NONE
      integer(c_int), intent(in),value :: itacty
      integer(c_int)                   :: sz
      type(c_ptr)                      :: shape
      !
      real(c_double),dimension(:),pointer :: js

      sz = 2
      allocate(js(sz))

      js=get_axes_JONCx(itacty)

      shape = c_loc(js(1))

    END subroutine GetShapeJONCx

    subroutine GetCoorJONCx(itacty,coor,sz) bind(c, name='JONCx_GetCoor')
      IMPLICIT NONE
      integer(c_int), intent(in),value :: itacty
      integer(c_int)                   :: sz
      type(c_ptr)                      :: coor
      !
      real(c_double),dimension(:),pointer :: jc

      sz = 3
      allocate(jc(sz))
      jc=get_coor(itacty)

      coor = c_loc(jc(1))

    END subroutine GetCoorJONCx


   function GetBodyIdJONCx(itacty) bind(C,name='JONCx_GetBodyId')
     implicit none
     integer(C_INT),intent(in),value :: itacty
     integer(C_INT)                  :: GetBodyIdJONCx

     GetBodyIdJONCx = get_body_id_JONCx(itacty)

   end function

   subroutine GetPtrJONCx2BDYTY(ptr, dim1, dim2) bind(c, name='JONCx_GetPtrJONCx2BDYTY')
     implicit none
     integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur joncx2rbdy2
     type(c_ptr)                 :: ptr ! pointeur sur la table de correpondance
     !
     integer(c_int), dimension(:,:), pointer :: ivect

     ivect => get_ptr_JONCx2BDYTY()

     if( associated(ivect) ) then
       ptr = c_loc(ivect(1,1))
       dim1 = size(ivect,1)
       dim2 = size(ivect,2)
     else
       ptr = c_null_ptr
       dim1 = 0
       dim2 = 0
     end if
   
   end subroutine GetPtrJONCx2BDYTY

   function IsVisible(itact) bind(c, name='JONCx_IsVisible')
      implicit none
      integer(c_int), intent(in), value :: itact
      integer(c_int) :: IsVisible

      if (get_visible(itact)) then
        IsVisible = 1
      else
        IsVisible = 0
      end if

   end function

   subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='JONCx_InitOutlines')
     implicit none
     integer(kind=c_int) :: dim1, dim2
     type(c_ptr) :: ptr
     !
     !real(kind=8), dimension(:,:), pointer :: outlines
     real(c_double), dimension(:,:), pointer :: outlines     

     outlines => init_outlines_JONCx()

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

   subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='JONCx_InitScalarFields')
     implicit none
     integer(kind=c_int) :: dim1, dim2
     type(c_ptr) :: ptr
     !
     !real(kind=8), dimension(:), pointer :: scalarfields
     real(c_double), dimension(:), pointer :: scalarfields
     
     scalarfields => init_scalarfields_JONCx()

     if( associated(scalarfields) ) then
       ptr  = c_loc(scalarfields(1))
       dim1 = get_nb_scalarfields_JONCx()
       dim2 = get_nb_JONCx()
     else
       ptr = c_null_ptr
       dim1 = 0
       dim2 = 0
     end if

   end subroutine

   subroutine UpdatePostdata() bind(c, name='JONCx_UpdatePostdata')
     implicit none

     call update_postdata_JONCx

   end subroutine

   subroutine GetNbPointOutlines(ptr, length) bind(c, name='JONCx_GetNbPointOutlines')
     implicit none
     type(c_ptr)    :: ptr
     integer(c_int) :: length
     !
     !integer(kind=4), dimension(:), pointer :: nb_point_outlines
     integer(c_int), dimension(:), pointer :: nb_point_outlines     

     nb_point_outlines => get_nb_point_outlines_JONCx()

     if( associated(nb_point_outlines) ) then
       ptr    = c_loc(nb_point_outlines(1))
       length = size(nb_point_outlines)
     else
       ptr = c_null_ptr
       length = 0
     end if

   end subroutine GetNbPointOutlines
   

   function GetNbScalarfields() bind(c, name='JONCx_GetNbScalarFields')
     implicit none
     integer(c_int) :: GetNbScalarfields

     GetNbScalarfields = get_nb_scalarfields_JONCx()

   end function GetNbScalarfields
    
   subroutine CleanMemory() bind(c, name='JONCx_CleanMemory')
     implicit none
  
     call clean_memory_JONCx
  
   end subroutine
    
END MODULE wrap_JONCx
