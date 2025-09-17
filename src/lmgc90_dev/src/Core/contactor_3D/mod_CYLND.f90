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
MODULE CYLND

  !!****h* LMGC90.CORE/CYLND
  !! NAME
  !!  module CYLND
  !! AUTHORS
  !!  M. Renouf & G. Saussine
  !!  2006 Bug Fix F.Dubois
  !! FUNCTION
  !!  Modelize an empty cylinder  
  !!****

  USE utilities
  USE overall
  USE bulk_behaviour
  USE a_DOF
  USE parameters

  USE RBDY3,ONLY: &
       get_nb_RBDY3,get_nb_tacty,get_tacid,get_color,get_data, &
       get_coorTT_CYLND=> get_coorTT, &
       get_coor_CYLND => get_coor, &
       get_cooref_CYLND => get_cooref, &
       get_inertia_frameTT_CYLND => get_inertia_frameTT, & 
       get_inertia_frame_CYLND  => get_inertia_frame, &
       add_reac_CYLND=>add_reac, &
       get_Xbegin_CYLND => get_Xbegin, &
       get_X_CYLND => get_X, &
       get_vlocy_CYLND => get_vlocy, &
       get_V_CYLND => get_V, &
       get_reac_CYLND => get_reac, &
       comp_vlocy_CYLND => comp_vlocy, &
       nullify_reac_CYLND => nullify_reac, &       
       nullify_vlocy_CYLND => nullify_vlocy, &       
       get_behav_CYLND => get_behav, &
       get_ENT_CYLND => get_entity_RBDY3,&
       get_mass_CYLND    => get_mass,&
       get_visible,&
       get_shiftTT_CYLND => get_shiftTT, &
       get_embeded_frame_RBDY3 => get_embeded_frame, &
       set_bdyty2tacty_RBDY3, &
       get_visibleID


  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------------
  ! cylnd2bdyty(1,itac) : serial number of body RBDY3 to which is attached 
  !                       the contactor CYLND numbered itac in the list of 
  !                       all contactors CYLND 
  ! cylnd2bdyty(2,itac) : serial number of contactor CYLND itac in the list
  !                       of contactors CYLND attached to a body (generically 1)
  ! ----------------------------------------------------------------------

  INTEGER,DIMENSION(:,:),POINTER  ::  cylnd2bdyty

  INTEGER,PRIVATE                   :: nb_CYLND=0
  REAL(kind=8),DIMENSION(2),PRIVATE :: mean_radius_CYLD
  REAL(kind=8)                      :: max_radius,min_radius

  ! nb slices (2 tri + 1 quad) descretizing the section of the cylinder
  ! must be a multiple of 4 and at the very least 8
  integer(kind=4), parameter :: nb_cells = 32
  ! nb points describing the contactor outline 2*nb_cells+2*(nb_cells*(nb_cells/4-1)+1)
  integer(kind=4), parameter :: nbpto = 2*nb_cells + 2 * (nb_cells*(nb_cells/4-1)+1)
  real(kind=8), dimension(3,nbpto) :: unit_cylnd

  integer(kind=4) :: nbsf=15
  integer(kind=4),dimension(:),  pointer :: nb_point_outlines_CYLND => null() 
  integer(kind=4),dimension(:),  pointer :: all_connectivities      => null()
  real(kind=8),   dimension(:,:),pointer :: outlines_CYLND          => null()
  real(kind=8),   dimension(:),  pointer :: scalarfields_CYLND      => null()

  PUBLIC cylnd2bdyty

  PUBLIC &
       read_bodies_CYLND

  PUBLIC &
       get_color_CYLND,get_behav_CYLND,get_mean_radius_CYLND,get_cooref_CYLND, &
       get_coorTT_CYLND,get_coor_CYLND,get_inertia_frame_CYLND,add_reac_CYLND, &
       get_X_CYLND,get_Xbegin_CYLND,get_vlocy_CYLND,get_V_CYLND,get_reac_CYLND,& 
       comp_vlocy_CYLND,nullify_reac_CYLND,get_nb_CYLND,nullify_vlocy_CYLND,   &
       get_inertia_frameTT_CYLND,get_max_radius_CYLND,get_min_radius_CYLND,    &
       get_ENT_CYLND,get_visible_CYLND,is_CYLND_same_RBDY3,get_mass_CYLND,get_shiftTT_CYLND, &   ! ,get_data
       get_visibleID_CYLND,get_axes_CYLND

  public &! <- rm: visu vtk
       get_ptr_CYLND2BDYTY        ,&
       get_nb_scalarfields_CYLND  ,&
       get_nb_point_outlines_CYLND,&
       init_outlines_CYLND        ,&
       init_scalarfields_CYLND    ,&
       update_postdata_CYLND      ,&
       get_all_connectivities_CYLND

  public clean_memory_CYLND

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_CYLND

    IMPLICIT NONE

    INTEGER                   :: ibdyty,itacty,errare,nb_RBDY3,id
    CHARACTER(len=18)         :: IAM='CYLND::read_bodies'
    REAL(kind=8),DIMENSION(2) :: DATA
    REAL(kind=8),DIMENSION(2) :: mean_radius 
    CHARACTER(len=80)         :: cout 

    nb_CYLND = 0
    nb_RBDY3 = get_nb_RBDY3()

    DO ibdyty=1,nb_RBDY3   
      DO itacty=1,get_nb_tacty(ibdyty)
        IF (get_tacID(ibdyty,itacty) == 'CYLND') then
          nb_CYLND = nb_CYLND + 1
          id=get_contactor_id_from_name('CYLND')
          call set_bdyty2tacty_rbdy3(ibdyty,itacty,id,nb_CYLND) 
        endif
      END DO
    END DO

    WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_CYLND,'CYLND found'
    CALL LOGMES(cout)

    IF (nb_CYLND.EQ.0) RETURN

    allocate(cylnd2bdyty(3,nb_CYLND),stat=errare)

    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating cylnd2bdyty')
    END IF

    nb_CYLND = 0

    DO ibdyty=1,nb_RBDY3   
       DO itacty=1,get_nb_tacty(ibdyty)
          IF (get_tacID(ibdyty,itacty) == 'CYLND') THEN
             nb_CYLND = nb_CYLND + 1
             cylnd2bdyty(1,nb_CYLND) = ibdyty
             cylnd2bdyty(2,nb_CYLND) = itacty
             cylnd2bdyty(3,nb_CYLND) = i_rbdy3
          END IF
       END DO
    END DO

    min_radius = 1.D20
    max_radius =-1.D20
    mean_radius= 0.D0

    DO ibdyty=1,nb_CYLND
       CALL  get_data(cylnd2bdyty(1,ibdyty),cylnd2bdyty(2,ibdyty),DATA)
       
       min_radius  = MIN(DATA(1)+DATA(2),min_radius)
       max_radius  = MAX(DATA(1)+DATA(2),max_radius)

       mean_radius(1:2) = mean_radius(1:2) + DATA(1:2)
       
    END DO
    mean_radius_CYLD=mean_radius/REAL(nb_CYLND,8)
   
  END SUBROUTINE read_bodies_CYLND
!------------------------------------------------------------------------
 function get_axes_CYLND(itact)
   implicit none
   integer(kind=4)      :: itact
   real(kind=8),dimension(2) :: get_axes_CYLND
   real(kind=8),dimension(2) :: data

   call get_data(cylnd2bdyty(1,itact),cylnd2bdyty(2,itact),data)

   get_axes_CYLND=data

 end function get_axes_CYLND
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_CYLND(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome
  
    get_nb_CYLND = nb_CYLND
    
  END FUNCTION get_nb_CYLND
!!!------------------------------------------------------------------------   
  FUNCTION get_mean_radius_CYLND(fantome)

    IMPLICIT NONE 
    REAL(kind=8),OPTIONAL     :: fantome
    REAL(kind=8),DIMENSION(2) :: get_mean_radius_CYLND

    get_mean_radius_CYLND=mean_radius_CYLD

  END FUNCTION get_mean_radius_CYLND
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_max_radius_CYLND(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_max_radius_CYLND=max_radius

  END FUNCTION get_max_radius_CYLND
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_min_radius_CYLND(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_min_radius_CYLND=min_radius

  END FUNCTION get_min_radius_CYLND
!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_CYLND(itact)

    IMPLICIT NONE
    INTEGER      :: itact 
   
    get_color_CYLND = get_color(cylnd2bdyty(1,itact),cylnd2bdyty(2,itact))
 
  END FUNCTION get_color_CYLND
!!!------------------------------------------------------------------------
  LOGICAL FUNCTION get_visible_CYLND(itact)

    IMPLICIT NONE    
    INTEGER :: itact 

    get_visible_CYLND = get_visible(cylnd2bdyty(1,itact))
    
  END FUNCTION get_visible_CYLND
!!!------------------------------------------------------------------------ 
  FUNCTION is_CYLND_same_RBDY3(itact1,itact2)

    IMPLICIT NONE
    
    INTEGER :: itact1,itact2 
    LOGICAL :: is_CYLND_same_RBDY3
    
    is_CYLND_same_RBDY3=.FALSE.
    IF(cylnd2bdyty(1,itact1) == cylnd2bdyty(1,itact2)) is_CYLND_same_RBDY3=.TRUE.
    
  END FUNCTION is_CYLND_same_RBDY3
!------------------------------------------------------------------------ 
 function get_ptr_CYLND2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_CYLND2BDYTY

   get_ptr_CYLND2BDYTY => cylnd2bdyty

 end function get_ptr_CYLND2BDYTY

!------------------------------------------------------------------------ 
 subroutine updt_outline_CYLND(itacty,outline)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,nbpto) :: outline
   !
   integer(kind=4)             :: k
   real(kind=8),dimension(3)   :: X, vertex_ref
   real(kind=8),dimension(3,3) :: frame, lframe
   real(kind=8),dimension(2)   :: data

   !fd    
   !fd get_coor_CYLND gere le shift
   !fd 

   X     = get_coor_CYLND(cylnd2bdyty(1,itacty),cylnd2bdyty(2,itacty))
   frame = get_inertia_frame_CYLND(cylnd2bdyty(1,itacty))
   lframe= get_embeded_frame_CYLND(itacty)

   frame = matmul(frame,lframe)

   call get_data(cylnd2bdyty(1,itacty),cylnd2bdyty(2,itacty),data)

   do k = 1, nbpto
      vertex_ref(1:2) = unit_cylnd(1:2,k) * data(2)
      if( k <= nb_cells .or. k > nb_cells*nb_cells/4+1 .and. k<= nb_cells*nb_cells/4+nb_cells ) then
          vertex_ref(3) = unit_cylnd(3,k)   * data(1)
      else if( k <= nb_cells*nb_cells/4+1 ) then
          vertex_ref(3) = (unit_cylnd(3,k)-1.0d0) * data(2) + data(1)
      else
          vertex_ref(3) = (unit_cylnd(3,k)+1.0d0) * data(2) - data(1)
      end if
 
      outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
      outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
      outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

   end do

 end subroutine updt_outline_CYLND

 !------------------------------------------------------------------------
 function get_nb_point_outlines_CYLND()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_CYLND

   get_nb_point_outlines_CYLND => nb_point_outlines_CYLND

 end function get_nb_point_outlines_CYLND


 !------------------------------------------------------------------------
 function init_outlines_CYLND()
   implicit none
   integer(kind=4) :: itacty,sz,i_f
   integer(kind=4) :: i_s, iss, i_h, half_sphere, nb_quad, nb_tri
   real(kind=8)    :: rc, rs, rrc, rrs, dpi
   real(kind=8),dimension(:,:),pointer :: init_outlines_CYLND

   if ( nb_CYLND .eq. 0 ) then
     init_outlines_CYLND => null()
     return
   endif 

   if (associated(nb_point_outlines_CYLND)) deallocate(nb_point_outlines_CYLND)
   allocate(nb_point_outlines_CYLND(nb_CYLND+1)) 
   nb_point_outlines_CYLND(1) = 0
   do itacty = 1, nb_CYLND
     nb_point_outlines_CYLND(itacty+1) = nb_point_outlines_CYLND(itacty) + nbpto
   end do

   sz =  nb_point_outlines_CYLND(nb_CYLND+1)

   if (associated(outlines_CYLND)) deallocate(outlines_CYLND)
   allocate(outlines_CYLND(3,sz)) 

   outlines_CYLND(1:3,1:sz) = 0.d0

   init_outlines_CYLND => outlines_CYLND

   !storing connectivities of each cylnd
   ! 1/ sizing :
   ! - nb_cells quad for sides
   ! - 2 * nb_cells*(nb_cells/4-1) quad for the two half spheres poles excluded
   ! - 2 * nb_cells                tri  for the two poles
   ! => nb_cells*(nb_cells/2-1) quad + 2*nb_cells tri
   nb_quad = nb_cells*(nb_cells/2-1)
   nb_tri  = 2*nb_cells
   sz = nb_CYLND * ( nb_quad*5 + nb_tri*4 + 1 )

   if( associated(all_connectivities) ) deallocate(all_connectivities)
   allocate(all_connectivities(sz))

   ! 2/ filling
   sz = 1
   do itacty = 1, nb_CYLND
     all_connectivities(sz) = nb_quad+nb_tri
     sz = sz+1
     ! sides
     do i_f = 1, nb_cells
       i_h = nb_cells*nb_cells/4 + 1
       all_connectivities(sz) = 4
       all_connectivities(sz+1) =       i_f
       all_connectivities(sz+2) = i_h + i_f
       all_connectivities(sz+3) = i_h + mod(i_f, nb_cells)+1
       all_connectivities(sz+4) =       mod(i_f, nb_cells)+1
       sz = sz+5
     end do
     ! half spheres
     do half_sphere = 0, 1
       i_h = half_sphere * ( nb_cells/4 * nb_cells + 1 )
       do i_s = 0, nb_cells/4-2
         do i_f = 1, nb_cells
           all_connectivities(sz) = 4
           all_connectivities(sz+1) = i_h +   i_s *nb_cells + i_f
           all_connectivities(sz+2) = i_h +   i_s *nb_cells + mod(i_f,nb_cells)+1
           all_connectivities(sz+3) = i_h +(1+i_s)*nb_cells + mod(i_f,nb_cells)+1
           all_connectivities(sz+4) = i_h +(1+i_s)*nb_cells + i_f
           sz = sz+5
         end do
       end do
       do i_f = 1, nb_cells
         all_connectivities(sz) = 3
         all_connectivities(sz+1) = i_h + (nb_cells/4-1)*nb_cells + i_f
         all_connectivities(sz+2) = i_h + (nb_cells/4-1)*nb_cells + mod(i_f,nb_cells)+1
         all_connectivities(sz+3) = i_h + (nb_cells/4  )*nb_cells + 1
         sz = sz+4
       end do
     end do
   end do

   !coordinates of referenc cylnd
   DPI = 2.D0*PI_g/REAL(nb_cells,8)

   unit_cylnd = 0.D0

   do i_f = 1, nb_cells

     !generatrice
     rc = cos(i_f*DPI)
     rs = sin(i_f*DPI)

     !half spheres (poles excluded)
     do i_s = 0, nb_cells/4-1

         rrc = cos(i_s*DPI)
         rrs = sin(i_s*DPI)

         iss = i_s*nb_cells + i_f
         unit_cylnd(1,iss) = rrc*rc
         unit_cylnd(2,iss) = rrc*rs
         unit_cylnd(3,iss) = rrs+1.0D0

         iss = iss + nb_cells * nb_cells/4 + 1
         unit_cylnd(1,iss) = rrc*rc
         unit_cylnd(2,iss) = rrc*rs
         unit_cylnd(3,iss) =-rrs-1.0D0
     end do
    end do

    !poles
    unit_cylnd(3,nb_cells*nb_cells/4+1) = 2.0D0
    unit_cylnd(3,nb_cells*nb_cells/2+2) =-2.0D0

    !print *, "[CYLND] unit vertices"
    !do i_s = 1, nbpto
    !    print *, i_s, unit_cylnd(:,i_s)
    !end do
    !print *, "[CYLND] unit connec"
    !print *, all_connectivities(1), sz
    !i_s = 2
    !do i_h = 1, all_connectivities(1)
    !    i_f = all_connectivities(i_s)
    !    print *, i_s, i_f
    !    print *, all_connectivities(i_s:i_s+i_f)
    !    i_s = i_s + i_f + 1
    !end do
 end function init_outlines_CYLND

 !------------------------------------------------------------------------
 function get_nb_scalarfields_CYLND()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_CYLND

   get_nb_scalarfields_CYLND = nbsf

 end function get_nb_scalarfields_CYLND

 !------------------------------------------------------------------------
 subroutine updt_scalarfield_CYLND(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3)   = get_X_CYLND(cylnd2bdyty(1,itacty))
   scalarfield(4:9)   = get_V_CYLND(cylnd2bdyty(1,itacty))
   scalarfield(10:15) = get_REAC_CYLND(cylnd2bdyty(1,itacty))

 end subroutine updt_scalarfield_CYLND
 
 !------------------------------------------------------------------------
 function init_scalarfields_CYLND()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_CYLND

   if ( nb_CYLND .eq. 0 ) then
     init_scalarfields_CYLND => null()
     return
   endif 

   sz = nbsf * nb_CYLND

   if (associated(scalarfields_CYLND)) deallocate(scalarfields_CYLND)
   allocate(scalarfields_CYLND(sz)) 

   scalarfields_CYLND(1:sz) = 0.d0

   init_scalarfields_CYLND => scalarfields_CYLND

 end function init_scalarfields_CYLND

 !------------------------------------------------------------------------
 subroutine update_postdata_CYLND()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf

   if (nb_CYLND == 0) return

   if (.not. associated(outlines_CYLND) ) call faterr('CYLND::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_CYLND) ) call faterr('CYLND::update_postdata','init_scalarfields is mandatory')
   
   iszo = 0
   iszsf = 0
   do itacty=1,nb_CYLND
     call updt_outline_CYLND(itacty,outlines_CYLND(1:3,iszo+1:iszo+nbpto))
     iszo = iszo + nbpto
     call updt_scalarfield_CYLND(itacty,scalarfields_CYLND(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_CYLND

 !------------------------------------------------------------------------ 
 function get_all_connectivities_CYLND()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_all_connectivities_CYLND

   get_all_connectivities_CYLND => all_connectivities

 end function
!------------------------------------------------------------------------
 function get_embeded_frame_CYLND(itacty)
    implicit none
    integer(kind=4) :: itacty
    real(kind=8), dimension(3,3) :: get_embeded_frame_CYLND
    
    get_embeded_frame_CYLND = get_embeded_frame_RBDY3(cylnd2bdyty(1,itacty), cylnd2bdyty(2,itacty))

  end function get_embeded_frame_CYLND
  
!!!-PTA----------------------------------------------------------------------- 
  INTEGER FUNCTION get_visibleID_CYLND(itact)

    IMPLICIT NONE
    INTEGER :: itact 
 
    get_visibleID_CYLND = get_visibleID(cylnd2bdyty(1,itact))

  END FUNCTION get_visibleID_CYLND
!!!-PTA----------------------------------------------------------------------- 

 subroutine clean_memory_CYLND()
   implicit none

   nb_CYLND = 0

   if( associated(cylnd2bdyty) ) then
     deallocate(cylnd2bdyty)
     nullify(cylnd2bdyty)
   end if

   if( associated(nb_point_outlines_CYLND) ) then
     deallocate(nb_point_outlines_CYLND)
     nullify(nb_point_outlines_CYLND)
   end if

   if( associated(all_connectivities) ) then
     deallocate(all_connectivities)
     nullify(all_connectivities)
   end if

   if( associated(outlines_CYLND) ) then
     deallocate(outlines_CYLND)
     nullify(outlines_CYLND)
   end if

   if( associated(scalarfields_CYLND) ) then
     deallocate(scalarfields_CYLND)
     nullify(scalarfields_CYLND)
   end if

 end subroutine

END MODULE CYLND
