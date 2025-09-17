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
MODULE wrap_SPHER

  USE ISO_C_BINDING

  USE SPHER,ONLY: &
       read_bodies_SPHER, &
       set_sphere_radius_correction, &
       get_nb_SPHER, &
       get_SPHER2BDYTY, &
       get_radius_SPHER, &
       get_coor_SPHER, &
       get_coorb_SPHER, &       
       spher2bdyty   , &
       get_ptr_SPHER2BDYTY         , &
       init_outlines_SPHER         , &
       init_scalarfields_SPHER     , &
       get_nb_scalarfields_SPHER   , &
       get_nb_point_outlines_SPHER , &
       update_postdata_SPHER       , &
       get_all_connectivities_SPHER, &
       clean_memory_SPHER          , &
       get_visible_SPHER

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE LoadTactors() bind(c, name='SPHER_LoadTactors')
    IMPLICIT NONE
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for SPHER contactors

       CALL read_bodies_SPHER

  END SUBROUTINE LoadTactors

  SUBROUTINE SetRadiusCorrection(corr) bind(c, name='SPHER_SetRadiusCorrection')
    IMPLICIT NONE
    real(c_double), intent(in), value :: corr

    CALL set_sphere_radius_correction(corr)

  END SUBROUTINE SetRadiusCorrection
!!!------------------------------------------------------------------------
  
  !!--vt--function giving the number of spheres
  function GetNbSPHER() bind(c, name='SPHER_GetNbSPHER')
    IMPLICIT NONE
    integer(c_int) :: GetNbSPHER

    GetNbSPHER=get_nb_SPHER()

  END function GetNbSPHER

  !fd mierda
  ! !! function giving the correspondance between tactors and its bdyty
  ! subroutine GetSPHER2BDYTY(map, dim1, dim2) bind(c, name='SPHER_GetSPHER2BDYTY')
  !    implicit none
  !    integer(c_int), intent(in), value :: nb_spher_ ! nombre de spheres attendues par celui qui appelle cette 
  !                                                   ! fonction, i.e. la taille de spher2bdyty_	
  !    integer(c_int), intent(out), dimension(nb_spher_) :: spher2bdyty_ ! vecteur pour récupérer la table de

  !    call get_SPHER2BDYTY(spher2bdyty_, nb_spher_)
  
  ! end subroutine GetSPHER2BDYTY

  subroutine GetSPHER2BDYTY(map, dim1, dim2) bind(C, name='SPHER_GetSPHER2BDYTY')
    implicit none
    type(c_ptr)    :: map
    integer(c_int) :: dim1, dim2
    !
    integer(kind=4), dimension(:,:), pointer :: ptr

    ptr => null()
    call get_SPHER2BDYTY(ptr, dim1, dim2)
    if( associated(ptr) ) then
      map = c_loc(ptr(1,1))
    else
      dim1 = 0
      dim2 = 0
      map = c_null_ptr
    end if

  end subroutine

  subroutine GetPtrSPHER2BDYTY(ptr, dim1, dim2) bind(c, name='SPHER_GetPtrSPHER2BDYTY')
    implicit none
    integer(c_int), intent(out) :: dim1,dim2  ! taille du vecteur spher2bdyty
    type(c_ptr)                 :: ptr        ! pointeur sur la table de correpondance
    !
    integer(c_int), dimension(:,:), pointer :: ivect

    ivect => get_ptr_SPHER2BDYTY()

    if( associated(ivect) ) then
      ptr = c_loc(ivect(1,1))
      dim1 = size(ivect,1)
      dim2 = size(ivect,2)
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine GetPtrSPHER2BDYTY

  
  !--vt-- fonction qui récupère le rayon du SPHER, d'indice itact
  function GetContactorRadiusSPHER(itact) bind(c, name='SPHER_GetContactorRadius')
    implicit none
    integer(c_int), intent(in), value :: itact ! indice du SPHER dont on veut récupérer le rayon
    real(c_double) :: GetContactorRadiusSPHER ! rayon du SPHER, d'indice itact

    GetContactorRadiusSPHER = get_radius_SPHER(itact)

  end function GetContactorRadiusSPHER

  subroutine GetContactorCoor(itacty, coor, nbdof) bind(c, name='SPHER_GetContactorCoor')
    implicit none
    integer(c_int),intent(in), value :: itacty
    integer(c_int)                   :: nbdof
    type(c_ptr)                      :: coor
    !
    real(c_double), dimension(:), pointer :: c

    !                         1234567890123
    character(len=13) :: IAM='SPHER_GetCoor'

    nbdof = 6
    allocate(c(nbdof))

    ! on calcule less coordonnees du centre d'inertie du contacteur considere
    c = get_coor_SPHER(spher2bdyty(1, itacty), spher2bdyty(2, itacty))

    coor = c_loc(c(1))

  end subroutine GetContactorCoor
  
  subroutine GetContactorCoorb(itacty, coorb, nbdof) bind(c, name='SPHER_GetContactorCoorb')
    implicit none
    integer(c_int),intent(in), value :: itacty
    integer(c_int)                   :: nbdof
    type(c_ptr)                      :: coorb
    !
    real(c_double), dimension(:), pointer :: c

    !                         12345678901234
    character(len=14) :: IAM='SPHER_GetCoorb'

    nbdof = 6
    allocate(c(nbdof))

    ! on calcule less coordonnees du centre d'inertie du contacteur considere
    c = get_coorb_SPHER(spher2bdyty(1, itacty), spher2bdyty(2, itacty))

    coorb = c_loc(c(1))

  end subroutine GetContactorCoorb
 
  !-- for external vtk visu --!

  FUNCTION IsVisible(itacty) bind(c, name='SPHER_IsVisible')
    IMPLICIT NONE
    INTEGER(c_int), INTENT(in), value :: itacty
    INTEGER(c_int) :: IsVisible

    IF ( get_visible_SPHER(itacty) ) THEN
         IsVisible = 1
    ELSE
         IsVisible = 0
    END IF

  END FUNCTION
 
  !-- for external vtk visu --!


  subroutine InitOutlines(ptr, dim1, dim2) bind(c, name='SPHER_InitOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines_SPHER()

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

  subroutine InitScalarfields(ptr, dim1, dim2) bind(c, name='SPHER_InitScalarFields')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:), pointer :: scalarfields

    scalarfields => init_scalarfields_SPHER()

    if( associated(scalarfields) ) then
      ptr  = c_loc(scalarfields(1))
      dim1 = get_nb_scalarfields_SPHER()
      dim2 = get_nb_SPHER()
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine UpdatePostdata() bind(c, name='SPHER_UpdatePostdata')
    implicit none

    call update_postdata_SPHER

  end subroutine

  subroutine GetNbPointOutlines(ptr, length) bind(c, name='SPHER_GetNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines_SPHER()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine GetNbPointOutlines
  

  function GetNbScalarfields() bind(c, name='SPHER_GetNbScalarFields')
    implicit none
    integer(c_int) :: GetNbScalarfields

    GetNbScalarfields = get_nb_scalarfields_SPHER()

  end function GetNbScalarfields
    
  subroutine GetAllConnectivities(ptr, dim1) bind(c, name='SPHER_GetPtrAllConnectivities')
    implicit none
    integer(kind=c_int) :: dim1
    type(c_ptr) :: ptr
    !
    integer(kind=4), dimension(:), pointer :: connec

    connec => get_all_connectivities_SPHER()

    if( associated(connec) ) then
      ptr    = c_loc(connec(1))
      dim1 = size(connec)
    else
      ptr = c_null_ptr
      dim1 = 0
    end if

  end subroutine

  subroutine CleanMemory() bind(c, name='SPHER_CleanMemory')
    implicit none

    call clean_memory_SPHER

  end subroutine

END MODULE wrap_SPHER
