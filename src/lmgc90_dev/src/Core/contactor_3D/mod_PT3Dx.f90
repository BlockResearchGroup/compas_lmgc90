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
MODULE PT3Dx                                       

  !!****h* LMGC90.CORE/PT3Dx
  !! NAME
  !!  module PT3Dx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/a_DOF
  !!  LMGC90.CORE/RBDY3
  !!****
  
  USE utilities
  USE overall
  USE bulk_behaviour
  USE a_DOF
  USE parameters

  USE RBDY3,ONLY: &
       get_nb_RBDY3,get_nb_tacty,get_tacid,get_color,get_data, &
       get_data_sz,get_idata_sz,get_idata, &
       get_coorTT_PT3Dx => get_coorTT, &             
       get_coor_PT3Dx => get_coor, &                 
       get_cooref_PT3Dx => get_cooref, &             
       get_X_PT3Dx => get_X, &                       
       get_inertia_frame_PT3Dx => get_inertia_frame, & 
       get_inertia_frameTT_PT3Dx => get_inertia_frameTT, & 
       add_reac_PT3Dx=>add_reac, &
       get_Xbegin_PT3Dx => get_Xbegin, &
       get_Vbegin_PT3Dx => get_Vbegin, &
       get_vlocy_PT3Dx => get_vlocy, &
       get_V_PT3Dx => get_V, &
       get_reac_PT3Dx => get_reac, &
       comp_vlocy_PT3Dx => comp_vlocy, &
       nullify_reac_PT3Dx => nullify_reac,  &
       nullify_vlocy_PT3Dx => nullify_vlocy,  &
       get_behav_PT3Dx    => get_behav, &
       get_ENT_PT3Dx => get_entity_RBDY3, &
       get_visible,get_inertia_tensor, &
       get_shiftTT_PT3Dx => get_shiftTT, &
       set_bdyty2tacty_RBDY3, &
       get_visibleID

  IMPLICIT NONE
 
  PRIVATE

  ! pt3dxbdyty(1,itac): 
  ! serial number of body RBDY3 to which is attached the contactor 
  ! PT3Dx numbered itac in the list of all contactors PT3Dx 
  ! pt3dx2bdyty(2,itac): 
  ! serial number of contactor PT3Dx itac in the list of contactors 
  ! of any kind attached to a body  pt3dx2bdyty(1,itac)
  INTEGER,DIMENSION(:,:),ALLOCATABLE,target  ::  pt3dx2bdyty

  INTEGER :: nb_PT3Dx=0

  integer(kind=4) :: nb_di = 7 ! nb points discretizing the equator of the pt3dxe
  integer(kind=4) :: nbpto = 44! nb points describing the contactor outline nb_di*(nb_di-1)+2
  real(kind=8), dimension(1:3,44) :: unit_pt3dx
  real(kind=8)                    :: ref_radius = 1.D0 ! radius of pt3dxe representing the point

  integer(kind=4) :: nbsf = 15
  integer(kind=4),dimension(:),  pointer :: nb_point_outlines_PT3Dx => null() 
  integer(kind=4),dimension(:),  pointer :: all_connectivities      => null()
  real(kind=8),   dimension(:,:),pointer :: outlines_PT3Dx          => null()
  real(kind=8),   dimension(:),  pointer :: scalarfields_PT3Dx      => null()

  ! les donnees publiques

  PUBLIC pt3dx2bdyty

  ! les routines publiques

  PUBLIC &
       read_bodies_PT3Dx

  PUBLIC &
       get_nb_PT3Dx, &
       get_mean_radius_PT3Dx,get_max_radius_PT3Dx,get_min_radius_PT3Dx, &
       get_behav_PT3Dx,get_cooref_PT3Dx,get_coor_PT3Dx, get_coorTT_PT3Dx,&
       get_Xbegin_PT3Dx,get_Vbegin_PT3Dx,get_V_PT3Dx,&
       get_reac_PT3Dx,get_inertia_frame_PT3Dx,get_inertia_frameTT_PT3Dx,&
       get_X_PT3Dx,get_color_PT3Dx, &
       nullify_reac_PT3Dx, comp_vlocy_PT3Dx,nullify_vlocy_PT3Dx, &
       get_vlocy_PT3Dx,add_reac_PT3Dx, &
       get_ENT_PT3Dx,get_visible_PT3Dx,get_shiftTT_PT3Dx, & 
       get_visibleID_PT3DX, &
       is_PT3Dx_same_RBDY3

  public &! <- rm: visu vtk
       set_ref_radius_PT3Dx       ,&
       get_ptr_PT3Dx2BDYTY        ,&
       get_nb_point_outlines_PT3Dx,&
       init_outlines_PT3Dx        ,&
       get_nb_scalarfields_PT3Dx  ,&
       init_scalarfields_PT3Dx    ,&
       update_postdata_PT3Dx      ,&
       get_all_connectivities_PT3Dx

  public clean_memory_PT3Dx
  
CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_PT3Dx

    IMPLICIT NONE
    INTEGER           :: ibdyty,itacty,errare,id
    INTEGER           :: nb_RBDY3
    CHARACTER(len=18) :: IAM='PT3Dx::read_bodies'
    CHARACTER(len=80) :: cout 

    nb_PT3Dx = 0
    nb_RBDY3 = get_nb_RBDY3()

    DO ibdyty=1,nb_RBDY3   
      DO itacty=1,get_nb_tacty(ibdyty)
        IF (get_tacID(ibdyty,itacty) == 'PT3Dx')  then
          nb_PT3Dx=nb_PT3Dx+1
          id=get_contactor_id_from_name('PT3Dx')
          call set_bdyty2tacty_rbdy3(ibdyty,itacty,id,nb_PT3Dx) 
        endif

       END DO
    END DO
    
    WRITE(cout,'(A,A,A,1x,I0,1X,A)') '[',IAM,']:',nb_PT3Dx,'PT3Dx found'
    CALL LOGMES(cout)

    IF (nb_PT3Dx == 0) RETURN
    
    allocate(pt3dx2bdyty(3,nb_PT3Dx),stat=errare)

    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating pt3dx2bdyty')
    END IF

    nb_PT3Dx = 0

    DO ibdyty=1,nb_RBDY3   
       DO itacty=1,get_nb_tacty(ibdyty)
          IF (get_tacID(ibdyty,itacty).NE.'PT3Dx') CYCLE
          nb_PT3Dx=nb_PT3Dx+1
          pt3dx2bdyty(1,nb_PT3Dx) = ibdyty
          pt3dx2bdyty(2,nb_PT3Dx) = itacty
          pt3dx2bdyty(3,nb_PT3Dx) = i_rbdy3
       END DO
    END DO

  END SUBROUTINE read_bodies_PT3Dx
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_PT3Dx(fantome)
    
    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome
  
    get_nb_PT3Dx = nb_PT3Dx
  
  END FUNCTION get_nb_PT3Dx
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_mean_radius_PT3Dx(fantome)

    IMPLICIT NONE   
    
    REAL(kind=8),OPTIONAL :: fantome

    get_mean_radius_PT3Dx = 0.D0

  END FUNCTION get_mean_radius_PT3Dx
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_max_radius_PT3Dx(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome

    get_max_radius_PT3Dx = 0.D0

  END FUNCTION get_max_radius_PT3Dx
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_min_radius_PT3Dx(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
    
    get_min_radius_PT3Dx = 0.D0
    
  END FUNCTION get_min_radius_PT3Dx
!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_PT3Dx(itact)

    IMPLICIT NONE
    INTEGER :: itact 
   
    get_color_PT3Dx = get_color(pt3dx2bdyty(1,itact),pt3dx2bdyty(2,itact))
 
  END FUNCTION get_color_PT3Dx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_visible_PT3Dx(itact)

    IMPLICIT NONE
    INTEGER :: itact 
   
    get_visible_PT3Dx = get_visible(pt3dx2bdyty(1,itact))
 
  END FUNCTION get_visible_PT3Dx
!!!------------------------------------------------------------------------ 

!!! for vtk visu !!!

 subroutine set_ref_radius_PT3Dx(radius)
   implicit none
   real(kind=8) :: radius

   ref_radius = radius

 end subroutine

 function get_ptr_PT3Dx2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_PT3Dx2BDYTY

   get_ptr_PT3Dx2BDYTY => pt3dx2bdyty

 end function get_ptr_PT3Dx2BDYTY

!------------------------------------------------------------------------ 
 subroutine updt_outline_PT3Dx(itacty,outline)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,nbpto) :: outline
   !
   integer(kind=4)             :: k
   real(kind=8),dimension(3)   :: X, vertex_ref
   real(kind=8),dimension(3,3) :: frame

   !fd    
   !fd get_coor_PT3Dx gere le shift
   !fd 

   X     = get_coor_PT3Dx(pt3dx2bdyty(1,itacty),pt3dx2bdyty(2,itacty))
   frame = get_inertia_frame_PT3Dx(pt3dx2bdyty(1,itacty))
   !

   do k = 1, nbpto
      vertex_ref(1:3) = unit_pt3dx(1:3,k) * ref_radius
 
      outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
      outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
      outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

   end do

 end subroutine updt_outline_PT3Dx

 !------------------------------------------------------------------------
 function get_nb_point_outlines_PT3Dx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_PT3Dx

   get_nb_point_outlines_PT3Dx => nb_point_outlines_PT3Dx

 end function get_nb_point_outlines_PT3Dx


 !------------------------------------------------------------------------
 function init_outlines_PT3Dx()
   implicit none
   integer(kind=4) :: itacty,sz,i,j,k
   real(kind=8)    :: dphi,dthe
   real(kind=8),dimension(:,:),pointer :: init_outlines_PT3Dx

   if ( nb_PT3Dx .eq. 0 ) then
     init_outlines_PT3Dx => null()
     return
   endif 

   if (associated(nb_point_outlines_PT3Dx)) deallocate(nb_point_outlines_PT3Dx)
   allocate(nb_point_outlines_PT3Dx(nb_PT3Dx+1)) 
   nb_point_outlines_PT3Dx(1) = 0
   do itacty = 1, nb_PT3Dx
     nb_point_outlines_PT3Dx(itacty+1) = nb_point_outlines_PT3Dx(itacty) + nbpto
   end do

   sz =  nb_point_outlines_PT3Dx(nb_PT3Dx+1)

   if (associated(outlines_PT3Dx)) deallocate(outlines_PT3Dx)
   allocate(outlines_PT3Dx(3,sz)) 

   outlines_PT3Dx(1:3,1:sz) = 0.d0

   init_outlines_PT3Dx => outlines_PT3Dx

   !storing connectivities of each pt3dx
   ! 1/ sizing : nb_di*(nb_di-2) quad + 2 *(nb_di-1)tri 
   !sz = 0
   !do itacty = 1, nb_PT3Dx
   !  sz = sz + nb_di*(nb_di-2)*5 + 2*(nb_di-1)*4 + 1
   !end do
   sz = nb_PT3Dx * ( nb_di*(nb_di-2)*5 + 2*(nb_di)*4 + 1 )

   if( associated(all_connectivities) ) deallocate(all_connectivities)
   allocate(all_connectivities(sz))

   ! 2/ filling
   sz = 1
   do itacty = 1, nb_PT3Dx
     all_connectivities(sz) = nb_di*(nb_di-2) + 2*(nb_di)
     sz = sz+1
     do i = 1, nb_di-2
       do j = 1, nb_di-1
         all_connectivities(sz) = 4
         all_connectivities(sz+1) = (i-1)*nb_di+j
         all_connectivities(sz+2) = (i-1)*nb_di+j+1
         all_connectivities(sz+3) =  i   *nb_di+j+1
         all_connectivities(sz+4) =  i   *nb_di+j
         sz = sz+5
       end do
       all_connectivities(sz) = 4
       all_connectivities(sz+1) = (i-1)*nb_di+1
       all_connectivities(sz+2) = (i-1)*nb_di+nb_di
       all_connectivities(sz+3) =  i   *nb_di+nb_di
       all_connectivities(sz+4) =  i   *nb_di+1
       sz = sz+5
     end do
     !north pole
     do i = 1, nb_di-1
       all_connectivities(sz) = 3
       all_connectivities(sz+1) = i
       all_connectivities(sz+2) = i+1
       all_connectivities(sz+3) = nbpto-1
       sz = sz+4
     end do
     all_connectivities(sz) = 3
     all_connectivities(sz+1) = nb_di
     all_connectivities(sz+2) = 1
     all_connectivities(sz+3) = nbpto-1
     sz = sz+4
     !south pole
     do i = 1, nb_di-1
       all_connectivities(sz) = 3
       all_connectivities(sz+1) = nb_di*(nb_di-2)+i+1
       all_connectivities(sz+2) = nb_di*(nb_di-2)+i
       all_connectivities(sz+3) = nbpto
       sz = sz+4
     end do
     all_connectivities(sz) = 3
     all_connectivities(sz+1) = nb_di*(nb_di-2)+1
     all_connectivities(sz+2) = nb_di*(nb_di-1)
     all_connectivities(sz+3) = nbpto
     sz = sz+4
   end do

   !computing vertices of unit pt3dx
   dphi = PI_g/real(nb_di,8)
   dthe = 2.D0*dphi
   k = 1
   do j = 1, nb_di-1
     do i = 0, nb_di-1
       unit_pt3dx(1,k) = sin(j*dphi)*cos(i*dthe)
       unit_pt3dx(2,k) = sin(j*dphi)*sin(i*dthe)
       unit_pt3dx(3,k) = cos(j*dphi)
       k = k+1
     end do
   end do
   ! poles
   unit_pt3dx(1:3,k)   = (/ 0., 0., 1.0/)
   unit_pt3dx(1:3,k+1) = (/ 0., 0.,-1.0/)

 end function init_outlines_PT3Dx

 !------------------------------------------------------------------------
 function get_nb_scalarfields_PT3Dx()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_PT3Dx

   get_nb_scalarfields_PT3Dx = nbsf

 end function get_nb_scalarfields_PT3Dx
 
 !------------------------------------------------------------------------
 subroutine updt_scalarfield_PT3Dx(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3)   = get_X_PT3Dx(pt3dx2bdyty(1,itacty))
   scalarfield(4:9)   = get_V_PT3Dx(pt3dx2bdyty(1,itacty))
   scalarfield(10:15) = get_REAC_PT3Dx(pt3dx2bdyty(1,itacty))

 end subroutine updt_scalarfield_PT3Dx

 !------------------------------------------------------------------------
 function init_scalarfields_PT3Dx()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_PT3Dx

   if ( nb_PT3Dx .eq. 0 ) then
     init_scalarfields_PT3Dx => null()
     return
   endif 

   sz = nbsf * nb_PT3Dx

   if (associated(scalarfields_PT3Dx)) deallocate(scalarfields_PT3Dx)
   allocate(scalarfields_PT3Dx(sz)) 

   scalarfields_PT3Dx(1:sz) = 0.d0

   init_scalarfields_PT3Dx => scalarfields_PT3Dx

 end function init_scalarfields_PT3Dx
 
 !------------------------------------------------------------------------
 subroutine update_postdata_PT3Dx()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf

   if (nb_PT3Dx == 0) return

   if (.not. associated(outlines_PT3Dx) ) call faterr('PT3Dx::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_PT3Dx) ) call faterr('PT3Dx::update_postdata','init_scalarfields is mandatory')

   
   iszo = 0
   iszsf = 0
   do itacty=1,nb_PT3Dx
     call updt_outline_PT3Dx(itacty,outlines_PT3Dx(1:3,iszo+1:iszo+nbpto)) 
     iszo = iszo + nbpto
     call updt_scalarfield_PT3Dx(itacty,scalarfields_PT3Dx(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_PT3Dx

 function get_all_connectivities_PT3Dx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_all_connectivities_PT3Dx

   get_all_connectivities_PT3Dx => all_connectivities

 end function

!!!-PTA-------------------------------------------------------------------- 
  INTEGER FUNCTION get_visibleID_PT3Dx(itact)

    IMPLICIT NONE
    INTEGER :: itact 
   
    get_visibleID_PT3Dx = get_visibleID(pt3dx2bdyty(1,itact))
 
  END FUNCTION get_visibleID_PT3Dx
!!!-PTA-------------------------------------------------------------------- 

 subroutine clean_memory_PT3Dx()
   implicit none

   nb_PT3Dx = 0

   if( allocated(pt3dx2bdyty) ) deallocate(pt3dx2bdyty)

   if( associated(nb_point_outlines_PT3Dx) ) then
     deallocate(nb_point_outlines_PT3Dx)
     nullify(nb_point_outlines_PT3Dx)
   end if

   if( associated(all_connectivities) ) then
     deallocate(all_connectivities)
     nullify(all_connectivities)
   end if

   if( associated(outlines_PT3Dx) ) then
     deallocate(outlines_PT3Dx)
     nullify(outlines_PT3Dx)
   end if

   if( associated(scalarfields_PT3Dx) ) then
     deallocate(scalarfields_PT3Dx)
     nullify(scalarfields_PT3Dx)
   end if

 end subroutine

!!!------------------------------------------------------------------------
  FUNCTION is_PT3Dx_same_RBDY3(itact1,itact2)

    IMPLICIT NONE

    INTEGER :: itact1,itact2 
    LOGICAL :: is_PT3Dx_same_RBDY3
   
    is_PT3Dx_same_RBDY3 = .FALSE.

    IF ((pt3dx2bdyty(3,itact1) == pt3dx2bdyty(3,itact2)) .and. &
        (pt3dx2bdyty(1,itact1) == pt3dx2bdyty(1,itact2))) is_PT3Dx_same_RBDY3=.TRUE.
        
 END FUNCTION is_PT3Dx_same_RBDY3
 

END MODULE PT3Dx

