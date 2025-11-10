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
module wrap_inter_handler_2D

  use iso_c_binding
  
  ! coming from inter_handler_2D
  use inter_meca_handler_2D, only: get_nb_inters    , & !this
                                   get_nb_recup     , & !this
                                   get_tact_lawnb   , & !this
                                   get_icdbdy       , & !this
                                   get_ianbdy       , & !this
                                   get_idata        , & !this
                                   get_local_frame  , & !this
                                   get_vloc         , & !this
                                   get_rloc         , & !this
                                   get_gaps         , & !this
                                   get_internal     , & !this
                                   set_internal     , & !this
                                   set_internal_bv  , & !this
                                   get_nb_verlets   , & !vrlt
                                   get_all_idata    , & !vrlt
                                   get_all          , & !vrlt
                                   get_all_internal , & !vrlt
                                   get_all_tactlawnb, & !vrlt
                                   get_verlet_adjsz , & !vrlt
                                   get_verlet_iantac, & !vrlt
                                   compute_rnod     , &
                                   stock_rloc       , &
                                   recup_rloc       , &
                                   recup_rloc_by_pos
                                   

  use overall, only : max_internal_tact

! Be carefull in the following :
!  get|set  have access to verlet data structures
! tget|tset have access to this data structures
  
contains

  function tgetNb(inter_id) bind(c, name='inter_handler_2D_tgetNb')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    integer(c_int)                    :: tgetNb

    tgetNb = get_nb_inters(inter_id)

  end function tgetNb

  function tgetTactLawNb(inter_id, icdan) bind(c, name='inter_handler_2D_tgetTactLawNb')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    integer(c_int), intent(in), value :: icdan
    integer(c_int)                    :: tgetTactLawNb

    tgetTactLawNb = get_tact_lawnb(inter_id, icdan)

  end function tgetTactLawNb

  subroutine tgetIdBodies(inter_id, icdan, ivect, ivalue1) bind(c, name='inter_handler_2D_tgetIdBodies')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    integer(c_int), intent(in), value :: icdan
    type(c_ptr)                       :: ivect
    integer(c_int)                    :: ivalue1    

    integer(c_int), dimension(:), pointer :: vector
    
    ivalue1 = 2
    allocate(vector(ivalue1))
    vector = (/ get_icdbdy(inter_id,icdan), get_ianbdy(inter_id,icdan) /)

    ivect = c_loc(vector(1))

  end subroutine tgetIdBodies
  
  subroutine tgetIData(inter_id, icdan, ivect, ivalue) bind(c, name='inter_handler_2D_tgetIData')
    implicit none
    integer(c_int), intent(in), value   :: inter_id
    integer(c_int), intent(in), value   :: icdan
    integer(c_int)                      :: ivalue
    type(c_ptr)                         :: ivect
    !
    integer(c_int), dimension(:), pointer :: vector

    ! 13 = icdbtyp, icdbdy , ianbtyp, ianbdy ,
    !      icdctyp, ianctyp, icdbtac, ianbtac,
    !      icdsci, ianasci ,
    !      status, lawnb   , nb_internal
    ivalue = 13
    allocate( vector(ivalue) )

    call get_idata( inter_id, icdan, vector )
    ivect = c_loc(vector(1))

  end subroutine tgetIData

  subroutine tgetRData(inter_id, icdan, rvect, rvalue) bind(c, name='inter_handler_2D_tgetRData')
    implicit none
    integer(c_int), intent(in), value   :: inter_id
    integer(c_int), intent(in), value   :: icdan
    integer(c_int)                      :: rvalue
    type(c_ptr)                         :: rvect
    !
    real(c_double),dimension(:),pointer :: vector
    integer      :: st
    real(kind=8) :: gapTTbegin
 
    ! 11 = size coor + size tuc + size nuc + rlt + rln + vlt + vln + gapTT
    rvalue = 11
    allocate( vector(rvalue) )
    
    call get_local_frame( inter_id, icdan, vector(1:6) )
    call get_rloc( inter_id, icdan, vector(7), vector(8), st )
    call get_vloc( inter_id, icdan, vector(9), vector(10) )
    call get_gaps( inter_id, icdan, vector(11), gapTTbegin )

    rvect = c_loc(vector(1))

  end subroutine tgetRdata

  subroutine tsetInternal(inter_id, icdan, rvect, ivalue, idx, val) bind(c, name='inter_handler_2D_tsetInternal')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    integer(c_int), intent(in), value :: icdan
    integer(c_int), intent(in), value :: ivalue
    real(c_double), intent(in)        :: rvect(ivalue)
    integer(c_int), intent(in), value :: idx
    real(c_double), intent(in), value :: val

    if( ivalue /= 0 ) then
      call set_internal( inter_id, icdan, rvect)
    else
      !bv stands for by value
      call set_internal_bv( inter_id, icdan, idx, val)
    end if

  end subroutine tsetInternal

  subroutine tgetInternal(inter_id, icdan, rvect, ivalue) bind(c, name='inter_handler_2D_tgetInternal')
    implicit none
    integer(c_int), intent(in), value   :: inter_id
    integer(c_int), intent(in), value   :: icdan
    integer(c_int)                      :: ivalue
    type(c_ptr)                         :: rvect
    !
    real(c_double),dimension(:),pointer :: vector
    
    allocate(vector(max_internal_tact))
    
    call get_internal( inter_id, icdan, vector)

    ivalue = max_internal_tact
    rvect=c_loc(vector(1))
    
  end subroutine tgetInternal
  
  function getNbRecup(inter_id) bind(c, name='inter_handler_2D_getNbRecup')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    integer(c_int)                    :: getNbRecup

    getNbRecup = get_nb_recup(inter_id)

  end function getNbRecup

  function getNb(inter_id) bind(c, name='inter_handler_2D_getNb')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    integer(c_int)                    :: getNb

    getNb = get_nb_verlets(inter_id)

  end function getNb

  subroutine getAllTactLawNb(inter_id, ptr, dim1) bind(c, name='inter_handler_2D_getAllTactLawNb')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    type(c_ptr)                       :: ptr
    integer(c_int)                    :: dim1

    integer(kind=4), dimension(:), pointer :: alltactlaw
    
    alltactlaw => get_all_tactlawnb(inter_id)

    if( associated(alltactlaw) ) then
      ptr  = c_loc(alltactlaw(1))
      dim1 = size(alltactlaw)
    else
      ptr  = c_null_ptr
      dim1 = 0
    end if
  end subroutine getAllTactLawNb
  
  subroutine getAll(inter_id, ptr, dim1, dim2) bind(c, name='inter_handler_2D_getAll')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    type(c_ptr)                       :: ptr
    integer(c_int)                    :: dim1, dim2
    !
    real(kind=8), dimension(:,:), pointer :: all

    all => get_all(inter_id)

    if( associated(all) ) then
      ptr  = c_loc(all(1,1))
      dim1 = size(all,1)
      dim2 = size(all,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine getAll
  
  subroutine getAllInternal(inter_id, ptr, dim1, dim2) bind(c, name='inter_handler_2D_getAllInternal')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1, dim2
    !
    real(kind=8), dimension(:,:), pointer :: all

    all => get_all_internal(inter_id)

    if( associated(all) ) then
      ptr  = c_loc(all(1,1))
      dim1 = size(all,1)
      dim2 = size(all,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine getAllInternal

  subroutine getAllIdata(inter_id, ptr, dim1, dim2) bind(c, name='inter_handler_2D_getAllIdata')
    implicit none
    integer(c_int), intent(in), value :: inter_id
    type(c_ptr)    :: ptr
    integer(c_int) :: dim1, dim2
    !
    integer, dimension(:,:), pointer :: all

    all => get_all_idata(inter_id)

    if( associated(all) ) then
      ptr  = c_loc(all(1,1))
      dim1 = size(all,1)
      dim2 = size(all,2)
    else
      ptr  = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine getAllIdata

  function getVerletAdjsz(id_inter, icdtac) bind(c, name="inter_handler_2D_getVerletAdjsz")
    implicit none
    integer(kind=c_int), intent(in), value :: id_inter
    integer(kind=c_int), intent(in), value :: icdtac
    !
    integer(kind=c_int) :: getVerletAdjsz

    getVerletAdjsz = get_verlet_adjsz(id_inter, icdtac)

  end function

  function getVerletIantac(id_inter, icdtac, iadj) bind(c, name="inter_handler_2D_getVerletIantac")
    implicit none
    integer(kind=c_int), intent(in), value :: id_inter
    integer(kind=c_int), intent(in), value :: icdtac
    integer(kind=c_int), intent(in), value :: iadj
    !
    integer(kind=c_int) :: getVerletIantac

    getVerletIantac = get_verlet_iantac(id_inter, icdtac, iadj)

  end function

  subroutine computeRnod() bind(c, name="inter_handler_2D_computeRnod")
    implicit none

    call compute_rnod

  end subroutine

  subroutine stockRloc(id_inter) bind(c, name="inter_handler_2D_stockRloc")
    use timer
    implicit none
    integer(kind=c_int), intent(in), value :: id_inter
    !
    integer, save :: timer_id = 0

    !                                                 12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[inter_2D] stock    ')
    call start_itimer(timer_id)
    call stock_rloc(id_inter)
    call stop_itimer(timer_id)

  end subroutine

  subroutine recupRloc(id_inter) bind(c, name="inter_handler_2D_recupRloc")
    use timer
    implicit none
    integer(kind=c_int), intent(in), value :: id_inter
    !
    integer, save :: timer_id = 0

    !                                                 12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[inter_2D] recup    ')
    call start_itimer(timer_id)
    call recup_rloc(id_inter)
    call stop_itimer(timer_id)

  end subroutine

  subroutine recupRlocByPos(id_inter, rtol) bind(c, name="inter_handler_2D_recupRlocByPos")
    use timer
    implicit none
    integer(kind=c_int), intent(in), value :: id_inter
    real(c_double)     , intent(in), value :: rtol
    !
    integer, save :: timer_id = 0

    !                                                 12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[inter_2D] recupByPos')
    call start_itimer(timer_id)
    call recup_rloc_by_pos(id_inter, rtol)
    call stop_itimer(timer_id)

  end subroutine

end module wrap_inter_handler_2D
