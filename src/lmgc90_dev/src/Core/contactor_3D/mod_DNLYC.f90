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
MODULE DNLYC                                       

  !!  Modelize an empty cylinder  

  USE utilities
  USE overall
  USE bulk_behaviour
  USE a_DOF
  USE parameters

  USE RBDY3,ONLY: &
       get_nb_RBDY3,get_nb_tacty,get_tacid,get_color,get_data, &
       get_coorTT_DNLYC=> get_coorTT, &
       get_coor_DNLYC => get_coor, &
       get_cooref_DNLYC => get_cooref, &
       get_inertia_frameTT_DNLYC => get_inertia_frameTT, & 
       get_inertia_frame_DNLYC  => get_inertia_frame, &
       add_reac_DNLYC=>add_reac, &
       get_Xbegin_DNLYC => get_Xbegin, &
       get_X_DNLYC => get_X, &
       get_vlocy_DNLYC => get_vlocy, &
       get_V_DNLYC => get_V, &
       get_reac_DNLYC => get_reac, &
       comp_vlocy_DNLYC => comp_vlocy, &
       nullify_reac_DNLYC => nullify_reac, &       
       nullify_vlocy_DNLYC => nullify_vlocy, &       
       get_behav_DNLYC => get_behav, &
       get_ENT_DNLYC => get_entity_RBDY3, &
       get_WS_DNLYC => get_WS, &
       set_bdyty2tacty_RBDY3, &
       get_visible, get_visibleID

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------------
  ! dnlyc2bdyty(1,itac) : serial number of body RBDY3 to which is attached 
  !                       the contactor DNLYC numbered itac in the list of 
  !                       all contactors DNLYC 
  ! dnlyc2bdyty(2,itac) : serial number of contactor DNLYC itac in the list
  !                       of contactors DNLYC attached to a body (generically 1)
  ! ----------------------------------------------------------------------

  INTEGER,DIMENSION(:,:),POINTER  ::  dnlyc2bdyty

  INTEGER,PRIVATE                   :: nb_DNLYC=0
  REAL(kind=8),DIMENSION(2),PRIVATE :: mean_radius_CYLD
  REAL(kind=8)                      :: max_radius,min_radius
  
  integer(kind=4), private :: nb_cells = 60
  integer(kind=4), private :: nbpto = 120 ! nb points describing the contactor outline (2*nb_cells)
  real(kind=8), dimension(3,120) :: unit_dnlyc ! DIME x nbpto

  integer(kind=4) :: nbsf=15
  integer(kind=4),dimension(:),  pointer :: nb_point_outlines_DNLYC => null() 
  integer(kind=4),dimension(:),  pointer :: all_connectivities      => null()
  real(kind=8),   dimension(:,:),pointer :: outlines_DNLYC          => null()
  real(kind=8),   dimension(:),  pointer :: scalarfields_DNLYC      => null()


  PUBLIC dnlyc2bdyty

  PUBLIC &
       read_bodies_DNLYC

  PUBLIC &
       get_color_DNLYC,get_behav_DNLYC,get_mean_radius_DNLYC,get_cooref_DNLYC, &
       get_coorTT_DNLYC,get_coor_DNLYC,get_inertia_frame_DNLYC,add_reac_DNLYC, &
       get_X_DNLYC,get_Xbegin_DNLYC,get_vlocy_DNLYC,get_V_DNLYC,get_reac_DNLYC,& 
       comp_vlocy_DNLYC,nullify_reac_DNLYC,get_nb_DNLYC,nullify_vlocy_DNLYC,   &
       get_inertia_frameTT_DNLYC,get_max_radius_DNLYC,get_min_radius_DNLYC,    &
       get_ENT_DNLYC, &
       get_WS_DNLYC, & ! ,get_data
       get_visible_DNLYC, get_visibleID_DNLYC

  public &! <- rm: visu vtk
       get_ptr_DNLYC2BDYTY        ,&
       get_nb_point_outlines_DNLYC,&
       init_outlines_DNLYC        ,&
       get_nb_scalarfields_DNLYC  ,&
       init_scalarfields_DNLYC    ,&
       update_postdata_DNLYC      ,&
       get_all_connectivities_DNLYC

  public clean_memory_DNLYC

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_DNLYC

    IMPLICIT NONE

    INTEGER                   :: ibdyty,itacty,errare,nb_RBDY3,id
    CHARACTER(len=18)         :: IAM='DNLYC::read_bodies'
    REAL(kind=8),DIMENSION(2) :: DATA
    REAL(kind=8),DIMENSION(2) :: mean_radius 
    CHARACTER(len=80)         :: cout 

    nb_DNLYC = 0
    nb_RBDY3 = get_nb_RBDY3()

    DO ibdyty=1,nb_RBDY3   
      DO itacty=1,get_nb_tacty(ibdyty)
        IF (get_tacID(ibdyty,itacty) == 'DNLYC') then
          nb_DNLYC = nb_DNLYC + 1
          id=get_contactor_id_from_name('DNLYC')
          call set_bdyty2tacty_rbdy3(ibdyty,itacty,id,nb_DNLYC) 
        endif
      END DO
    END DO

    WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_DNLYC,'DNLYC found'
    CALL LOGMES(cout)

    IF (nb_DNLYC.EQ.0) RETURN

    allocate(dnlyc2bdyty(3,nb_DNLYC),stat=errare)

    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating dnlyc2bdyty')
    END IF

    nb_DNLYC = 0

    DO ibdyty=1,nb_RBDY3   
       DO itacty=1,get_nb_tacty(ibdyty)
          IF (get_tacID(ibdyty,itacty) == 'DNLYC') THEN
             nb_DNLYC = nb_DNLYC + 1
             dnlyc2bdyty(1,nb_DNLYC) = ibdyty
             dnlyc2bdyty(2,nb_DNLYC) = itacty
             dnlyc2bdyty(3,nb_DNLYC) = i_rbdy3
          END IF
       END DO
    END DO

    min_radius = 1.D20
    max_radius =-1.D20
    mean_radius= 0.D0

    DO ibdyty=1,nb_DNLYC
       CALL  get_data(dnlyc2bdyty(1,ibdyty),dnlyc2bdyty(2,ibdyty),DATA)
       
       min_radius  = MIN(DATA(1)+DATA(2),min_radius)
       max_radius  = MAX(DATA(1)+DATA(2),max_radius)

       mean_radius(1) = DATA(1)
       mean_radius(2) = DATA(2)
       
    END DO
    mean_radius_CYLD=mean_radius/REAL(nb_DNLYC,8)
   
  END SUBROUTINE read_bodies_DNLYC
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_DNLYC(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome
  
    get_nb_DNLYC = nb_DNLYC
    
  END FUNCTION get_nb_DNLYC
!!!------------------------------------------------------------------------   
  FUNCTION get_mean_radius_DNLYC(fantome)

    IMPLICIT NONE 
    REAL(kind=8),OPTIONAL     :: fantome
    REAL(kind=8),DIMENSION(2) :: get_mean_radius_DNLYC

    get_mean_radius_DNLYC=mean_radius_CYLD

  END FUNCTION get_mean_radius_DNLYC
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_max_radius_DNLYC(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_max_radius_DNLYC=max_radius

  END FUNCTION get_max_radius_DNLYC
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_min_radius_DNLYC(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_min_radius_DNLYC=min_radius

  END FUNCTION get_min_radius_DNLYC
!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_DNLYC(itact)

    IMPLICIT NONE
    INTEGER      :: itact 
   
    get_color_DNLYC = get_color(dnlyc2bdyty(1,itact),dnlyc2bdyty(2,itact))
 
  END FUNCTION get_color_DNLYC
!!!------------------------------------------------------------------------
  
 function get_ptr_DNLYC2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_DNLYC2BDYTY

   get_ptr_DNLYC2BDYTY => dnlyc2bdyty

 end function get_ptr_DNLYC2BDYTY

!------------------------------------------------------------------------ 
 subroutine updt_outline_DNLYC(itacty,outline)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,nbpto) :: outline
   !
   integer(kind=4)             :: k
   real(kind=8),dimension(3)   :: X, vertex_ref
   real(kind=8),dimension(3,3) :: frame
   real(kind=8),dimension(2)   :: data

   !fd    
   !fd get_coor_DNLYC gere le shift
   !fd 
   X     = get_coor_DNLYC(dnlyc2bdyty(1,itacty),dnlyc2bdyty(2,itacty))
   frame = get_inertia_frame_DNLYC(dnlyc2bdyty(1,itacty))
   !
   call get_data(dnlyc2bdyty(1,itacty),dnlyc2bdyty(2,itacty),data)

   do k = 1, nbpto
      vertex_ref(1:2) = unit_dnlyc(1:2,k) * data(2)
      vertex_ref(3)   = unit_dnlyc(3,k)   * data(1)
 
      outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
      outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
      outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

   end do

 end subroutine updt_outline_DNLYC

 !------------------------------------------------------------------------
 function get_nb_point_outlines_DNLYC()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_DNLYC

   get_nb_point_outlines_DNLYC => nb_point_outlines_DNLYC

 end function get_nb_point_outlines_DNLYC

 !------------------------------------------------------------------------
 function init_outlines_DNLYC()
   implicit none
   integer(kind=4) :: itacty,sz,i_f
   real(kind=8)    :: r, dpi
   real(kind=8),dimension(:,:),pointer :: init_outlines_DNLYC

   if ( nb_DNLYC .eq. 0 ) then
     init_outlines_DNLYC => null()
     return
   endif 

   if (associated(nb_point_outlines_DNLYC)) deallocate(nb_point_outlines_DNLYC)
   allocate(nb_point_outlines_DNLYC(nb_DNLYC+1)) 
   nb_point_outlines_DNLYC(1) = 0
   do itacty = 1, nb_DNLYC
     nb_point_outlines_DNLYC(itacty+1) = nb_point_outlines_DNLYC(itacty) + nbpto
   end do

   sz =  nb_point_outlines_DNLYC(nb_DNLYC+1)

   if (associated(outlines_DNLYC)) deallocate(outlines_DNLYC)
   allocate(outlines_DNLYC(3,sz)) 

   outlines_DNLYC(1:3,1:sz) = 0.d0

   init_outlines_DNLYC => outlines_DNLYC

   !storing connectivities of each dnlyc
   ! 1/ sizing : nb_cells quad (for sides only)
   !sz = 0
   !do itacty = 1, nb_DNLYC
   !  sz = sz + nb_cells*5 + 1
   !end do
   sz = nb_DNLYC * ( nb_cells*5 + 1 )

   if( associated(all_connectivities) ) deallocate(all_connectivities)
   allocate(all_connectivities(sz))

   ! 2/ filling
   sz = 1
   do itacty = 1, nb_DNLYC
     all_connectivities(sz) = nb_cells
     sz = sz+1
     do i_f = 1, nb_cells-1
       all_connectivities(sz) = 4
       all_connectivities(sz+1) = i_f
       all_connectivities(sz+2) = i_f + nb_cells
       all_connectivities(sz+3) = i_f + nb_cells + 1
       all_connectivities(sz+4) = i_f + 1
       sz = sz+5
     end do
     all_connectivities(sz) = 4
     all_connectivities(sz+1) = 4
     all_connectivities(sz+2) = nb_cells
     all_connectivities(sz+3) = 2*nb_cells
     all_connectivities(sz+4) = nb_cells + 1
     all_connectivities(sz+1) = 1
     sz = sz+5
   end do

   !coordinates of referenc dnlyc
   DPI = 2.D0*PI_g/REAL(nb_cells,8)

   unit_dnlyc = 0.D0

   do i_f = 1, nb_cells

     r = cos(i_f*DPI)
     unit_dnlyc(1,i_f         ) = r
     unit_dnlyc(1,i_f+nb_cells) = r

     r = sin(i_f*DPI)
     unit_dnlyc(2,i_f         ) = r
     unit_dnlyc(2,i_f+nb_cells) = r

     unit_dnlyc(3,i_f         ) = -1.0D0  
     unit_dnlyc(3,i_f+nb_cells) =  1.0D0  

    end do

 end function init_outlines_DNLYC

 !------------------------------------------------------------------------
 function get_nb_scalarfields_DNLYC()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_DNLYC

   get_nb_scalarfields_DNLYC = nbsf

 end function get_nb_scalarfields_DNLYC
 !------------------------------------------------------------------------
 function init_scalarfields_DNLYC()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_DNLYC

   if ( nb_DNLYC .eq. 0 ) then
     init_scalarfields_DNLYC => null()
     return
   endif 

   sz = nbsf * nb_DNLYC

   if (associated(scalarfields_DNLYC)) deallocate(scalarfields_DNLYC)
   allocate(scalarfields_DNLYC(sz)) 

   scalarfields_DNLYC(1:sz) = 0.d0

   init_scalarfields_DNLYC => scalarfields_DNLYC

 end function init_scalarfields_DNLYC
 !------------------------------------------------------------------------
 subroutine updt_scalarfield_DNLYC(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3)   = get_X_DNLYC(dnlyc2bdyty(1,itacty))
   scalarfield(4:9)   = get_V_DNLYC(dnlyc2bdyty(1,itacty))
   scalarfield(10:15) = get_REAC_DNLYC(dnlyc2bdyty(1,itacty))

 end subroutine updt_scalarfield_DNLYC
 !------------------------------------------------------------------------
 subroutine update_postdata_DNLYC()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf

   if (.not. associated(outlines_DNLYC) ) call faterr('DNLYC::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_DNLYC) ) call faterr('DNLYC::update_postdata','init_scalarfields is mandatory')
   
   iszo = 0
   iszsf = 0
   do itacty=1,nb_DNLYC
     call updt_outline_DNLYC(itacty,outlines_DNLYC(1:3,iszo+1:iszo+nbpto)) 
     iszo = iszo + nbpto
     call updt_scalarfield_DNLYC(itacty,scalarfields_DNLYC(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_DNLYC

 !------------------------------------------------------------------------ 
 function get_all_connectivities_DNLYC()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_all_connectivities_DNLYC

   get_all_connectivities_DNLYC => all_connectivities

 end function

!!!------------------------------------------------------------------------
  LOGICAL FUNCTION get_visible_DNLYC(itact)

    IMPLICIT NONE    
    INTEGER :: itact 

    get_visible_DNLYC = get_visible(dnlyc2bdyty(1,itact))
    
  END FUNCTION get_visible_DNLYC
!!!------------------------------------------------------------------------ 

!!!-PTA----------------------------------------------------------------------- 
  INTEGER FUNCTION get_visibleID_DNLYC(itact)

    IMPLICIT NONE
    INTEGER :: itact 
 
    get_visibleID_DNLYC = get_visibleID(dnlyc2bdyty(1,itact))

  END FUNCTION get_visibleID_DNLYC
!!!-PTA----------------------------------------------------------------------- 

 subroutine clean_memory_DNLYC()
   implicit none

   nb_DNLYC = 0

   if( associated(dnlyc2bdyty) ) then
     deallocate(dnlyc2bdyty)
     nullify(dnlyc2bdyty)
   end if

   if( associated(nb_point_outlines_DNLYC) ) then
     deallocate(nb_point_outlines_DNLYC)
     nullify(nb_point_outlines_DNLYC)
   end if

   if( associated(all_connectivities) ) then
     deallocate(all_connectivities)
     nullify(all_connectivities)
   end if

   if( associated(outlines_DNLYC) ) then
     deallocate(outlines_DNLYC)
     nullify(outlines_DNLYC)
   end if

   if( associated(scalarfields_DNLYC) ) then
     deallocate(scalarfields_DNLYC)
     nullify(scalarfields_DNLYC)
   end if

 end subroutine

END MODULE DNLYC
