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
MODULE SPHER                                       

  !!****h* LMGC90.CORE/SPHER
  !! NAME
  !!  module SPHER
  !! USES
  !!  LMGC90.CORE/utilities
  !!  LMGC90.CORE/overall
  !!  LMGC90.CORE/bulk_behaviour
  !!  LMGC90.CORE/a_DOF
  !!  LMGC90.CORE/RBDY3
  !!****

  USE utilities
  USE overall
  USE bulk_behaviour
  USE a_DOF
  USE parameters

  USE RBDY3,ONLY: &
       get_nb_RBDY3,get_nb_tacty,get_tacid,get_color,get_data,put_data, &
       get_coorTT_SPHER => get_coorTT, &
       get_coor_SPHER => get_coor, &
       get_coorb_SPHER => get_coorb, &       
       get_cooref_SPHER => get_cooref, &
       get_inertia_frame_SPHER => get_inertia_frame, &
       get_inertia_frameTT_SPHER => get_inertia_frameTT, & 
       add_reac_SPHER=>add_reac, &
       get_Xbegin_SPHER => get_Xbegin, &
       get_X_SPHER => get_X, &
       get_vlocy_SPHER => get_vlocy, &
       get_V_SPHER => get_V, &
       get_reac_SPHER => get_reac, &
       comp_vlocy_SPHER => comp_vlocy, &
       nullify_reac_SPHER => nullify_reac, &
       nullify_vlocy_SPHER => nullify_vlocy, &
       get_behav_SPHER => get_behav, &
       get_ENT_SPHER => get_entity_RBDY3, &
       get_mass_SPHER    => get_mass, &
       get_shiftTT_SPHER => get_shiftTT, &
       get_WS_SPHER => get_WS, &
       get_inertia_frameIni_SPHER => get_inertia_frameIni, &
       get_visible, set_bdyty2tacty_rbdy3, &
       get_thread, &
       get_visibleID , &
       is_dof_driven_RBDY3, &
       print_info_RBDY3
       ! is_Xperiodic_RBDY3, &
       ! is_Yperiodic_RBDY3, &
       ! get_xperiode_RBDY3, &
       ! get_yperiode_RBDY3
  

  IMPLICIT NONE

  PRIVATE

  ! -----------------------------------------------------------------------
  ! spher2bdyty :
  !   spher2bdyty(1,itac) : serial number of body RBDY3 to which is attached
  !                         the contactor SPHER numbered itac in the list of
  !                         all contactors RBDY3 
  !   spher2bdyty(2,itac) : serial number of contactor SPHER itac in the list 
  !                         of all contactors attached to a body (mainly = 1)
  ! -----------------------------------------------------------------------  
  INTEGER,DIMENSION(:,:),POINTER  ::  spher2bdyty
  
  INTEGER      :: nb_SPHER=0,Init_gmv=0
  REAL(kind=8) :: min_radius,max_radius,mean_radius

  REAL(kind=8) :: radius_variation=1.D0

  LOGICAL :: is_verbose = .TRUE.

  PUBLIC spher2bdyty

  integer(kind=4) :: nb_di = 7 ! nb points discretizing the equator of the sphere
  integer(kind=4) :: nbpto = 44! nb points describing the contactor outline nb_di*(nb_di-1)+2
  real(kind=8), dimension(1:3,44) :: unit_spher

  integer(kind=4) :: nbsf=15
  integer(kind=4),dimension(:),  pointer :: nb_point_outlines_SPHER => null() 
  integer(kind=4),dimension(:),  pointer :: all_connectivities      => null()
  real(kind=8),   dimension(:,:),pointer :: outlines_SPHER          => null()
  real(kind=8),   dimension(:),  pointer :: scalarfields_SPHER      => null()


  PUBLIC &
       read_bodies_SPHER, &
       set_sphere_radius_correction

  PUBLIC &
       get_nb_SPHER, get_mean_radius_SPHER, get_max_radius_SPHER, &
       get_min_radius_SPHER, get_behav_SPHER, get_color_SPHER, &
       get_cooref_SPHER, get_coor_SPHER, get_coorb_SPHER, get_X_spher, get_vlocy_SPHER, &
       get_mass_SPHER, get_inertia_frameTT_SPHER, get_inertia_frame_SPHER, &
       get_V_SPHER, get_ENT_SPHER, get_radius_SPHER,get_visible_SPHER, &
       nullify_reac_SPHER, nullify_vlocy_SPHER, comp_vlocy_SPHER, &
       add_reac_SPHER, get_coorTT_SPHER,is_SPHER_same_RBDY3, &
       get_inertia_frameIni_SPHER, get_shiftTT_SPHER, & ! , get_data, put_data
       get_WS_SPHER, & 
       get_SPHER2BDYTY,& ! <- am: debut des fonctions supplementaires
       set_data_SPHER, &
       is_SPHER_same_THREAD, &
       get_visibleID_SPHER , &
       all_dof_driven_SPHER, &
       print_info_SPHER

  public &! <- rm: visu vtk
       get_ptr_SPHER2BDYTY        ,&
       get_nb_point_outlines_SPHER,&
       init_outlines_SPHER        ,&
       get_nb_scalarfields_SPHER  ,&
       init_scalarfields_SPHER    ,&
       update_postdata_SPHER      ,&
       get_all_connectivities_SPHER

  public clean_memory_SPHER

CONTAINS
!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_SPHER

    IMPLICIT NONE
   
    INTEGER                   :: ibdyty,itacty,errare,nb_RBDY3,itact,id
    REAL(kind=8),DIMENSION(1) :: DATA
    CHARACTER(len=18)         :: IAM='SPHER::read_bodies'
    CHARACTER(len=80)         :: cout 

    nb_SPHER=0
    nb_RBDY3=get_nb_RBDY3()

    DO ibdyty=1,nb_RBDY3   
      DO itacty=1,get_nb_tacty(ibdyty)
        IF ( get_tacID(ibdyty,itacty) == 'SPHER' .OR. &
             get_tacID(ibdyty,itacty) == 'SPHEb')  then
          nb_SPHER=nb_SPHER+1
          id=get_contactor_id_from_name('SPHER')
          call set_bdyty2tacty_rbdy3(ibdyty,itacty,id,nb_SPHER) 
        endif
      END DO
    END DO
    WRITE(cout,'(A,A,A,1x,I0,1X,A)') '[',IAM,']:',nb_SPHER,'SPHER found'
    CALL LOGMES(cout)
    
    IF (nb_SPHER == 0) RETURN

    allocate(spher2bdyty(3,nb_SPHER),stat=errare)
    
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating spher2bdyty')
    END IF
    
    nb_SPHER=0

    DO ibdyty=1,nb_RBDY3   
       DO itacty=1,get_nb_tacty(ibdyty)
          IF (get_tacID(ibdyty,itacty) == 'SPHER' &
               .OR. get_tacID(ibdyty,itacty) == 'SPHEb') THEN
             nb_SPHER=nb_SPHER+1
             spher2bdyty(1,nb_SPHER)=ibdyty
             spher2bdyty(2,nb_SPHER)=itacty
             spher2bdyty(3,nb_SPHER)=i_rbdy3
          END IF
       END DO
    END DO

    min_radius = 1.D20
    max_radius = 0.D0
    mean_radius= 0.D0
    
    DO itact=1,nb_SPHER
       CALL get_data(spher2bdyty(1,itact),spher2bdyty(2,itact),DATA)
       DATA = DATA*radius_variation
       CALL put_data(spher2bdyty(1,itact),spher2bdyty(2,itact),DATA)
       min_radius  = MIN(DATA(1),min_radius)
       max_radius  = MAX(DATA(1),max_radius)
       mean_radius = mean_radius + DATA(1)
    END DO
    mean_radius=mean_radius/REAL(nb_SPHER,8)


    if (is_verbose) then
      print*,'SPHERES:'
      print*,'  max_radius:  ',max_radius
      print*,'  min_radius:  ',min_radius     
      print*,'  mean_radius: ',mean_radius
    endif

  END SUBROUTINE read_bodies_SPHER
!!!------------------------------------------------------------------------ 
  SUBROUTINE set_sphere_radius_correction(r)

    IMPLICIT NONE
    REAL(kind=8) :: r

    radius_variation = r

  END SUBROUTINE set_sphere_radius_correction
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_SPHER(fantome)
    
    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome

    get_nb_SPHER = nb_SPHER
  
  END FUNCTION get_nb_SPHER
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_mean_radius_SPHER(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
    
    get_mean_radius_SPHER=mean_radius
    
  END FUNCTION get_mean_radius_SPHER
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_max_radius_SPHER(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_max_radius_SPHER=max_radius
    
  END FUNCTION get_max_radius_SPHER
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_min_radius_SPHER(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_min_radius_SPHER=min_radius

  END FUNCTION get_min_radius_SPHER
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_radius_SPHER(itact)

    IMPLICIT NONE
    INTEGER      :: itact
    REAL(kind=8),DIMENSION(1) :: DATA
   
    CALL get_data(spher2bdyty(1,itact),spher2bdyty(2,itact),DATA)
    get_radius_SPHER=DATA(1)

  END FUNCTION get_radius_SPHER
!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_SPHER(itact)

    IMPLICIT NONE
    INTEGER :: itact 
   
    get_color_SPHER = get_color(spher2bdyty(1,itact),spher2bdyty(2,itact))
 
  END FUNCTION get_color_SPHER
!!!------------------------------------------------------------------------
  LOGICAL FUNCTION get_visible_SPHER(itact)

    IMPLICIT NONE    
    INTEGER :: itact 

    get_visible_SPHER = get_visible(spher2bdyty(1,itact))
    
  END FUNCTION get_visible_SPHER
!!!------------------------------------------------------------------------ 
  FUNCTION is_SPHER_same_RBDY3(itact1,itact2)

    IMPLICIT NONE
    
    INTEGER :: itact1,itact2 
    LOGICAL :: is_SPHER_same_RBDY3
    
    is_SPHER_same_RBDY3=.FALSE.
    IF(spher2bdyty(1,itact1) == spher2bdyty(1,itact2)) is_SPHER_same_RBDY3=.TRUE.
    
  END FUNCTION is_SPHER_same_RBDY3
!------------------------------------------------------------------------ 
  LOGICAL FUNCTION is_SPHER_same_THREAD(icdtac,iantac)

    IMPLICIT NONE
    
    INTEGER :: icdtac,iantac
    
    is_SPHER_same_THREAD=.FALSE.
    
    IF( get_thread(spher2bdyty(1,icdtac)) .EQ. get_thread(spher2bdyty(1,iantac))) &
         is_SPHER_same_THREAD=.TRUE.
    
  END FUNCTION is_SPHER_same_THREAD

!------------------------------------------------------------------------
!------------------------------------------------------------------------

 subroutine get_SPHER2BDYTY(map, size1, size2)
   implicit none
   integer(kind=4), dimension(:,:), pointer :: map
   integer(kind=4), intent(out)             :: size1, size2
   !
   if(associated(map) ) nullify(map)

   size1 = 3
   size2 = nb_SPHER

   if( nb_SPHER > 0 ) then

     allocate(map(size1,size2))

     map(1:size1,1:size2) = SPHER2bdyty(1:size1,1:size2)

   end if

 end subroutine

 function get_ptr_SPHER2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_SPHER2BDYTY

   nullify(get_ptr_SPHER2BDYTY)

   if( nb_SPHER > 0 ) then
   
      get_ptr_SPHER2BDYTY => spher2bdyty

   endif   

 end function get_ptr_SPHER2BDYTY

 !> \brief use anonymous data to initialize a contactor
 subroutine set_data_SPHER(idata, rdata, node_list, nb_support, support, Brd)
   implicit none
   !> [in] idata: anonymous integer data (NULL)
   integer(kind=4), dimension(:), pointer :: idata
   !> [in] rdata: anonymous real data (radius, [shift_x, shift_y, shift_z])
   real(kind=8),    dimension(:), pointer :: rdata
   !> [out] node_list: list of nodes of the model_handle to use
   integer(kind=4), dimension(:),   allocatable, intent(inout) :: node_list
   !> [out] nb_support: number of support nodes to the contactor
   integer(kind=4),                              intent(out)   :: nb_support
   !> [out] support: support nodes to the contactor
   real(kind=8),    dimension(:,:), allocatable, intent(inout) :: support
   !> [out] Brd: boundary radius
   real(kind=8),    intent(out) :: Brd

   if( allocated(node_list) ) deallocate(node_list)
   if( allocated(support)   ) deallocate(support)

   ! only center of inertia
   allocate( node_list(1) )
   node_list(1) = 1

   ! only center of the spher
   nb_support = 1
   allocate( support(nbDIME,nb_support) )

   support = 0.d0
   if( size(rdata) == 4 ) then !there is a shift
     support(1:3,1) = rdata(2:4)
   end if

   Brd = rdata(1)

 end subroutine

!!! for vtk visu !!!
!------------------------------------------------------------------------ 
 subroutine updt_outline_SPHER(itacty,outline)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,nbpto) :: outline
   !
   integer(kind=4)             :: k
   real(kind=8),dimension(3)   :: X, vertex_ref
   real(kind=8),dimension(3,3) :: frame
   real(kind=8),dimension(1)   :: data

   !real(kind=8) :: xperiode,yperiode
   !fd    
   !fd get_coor_SPHER gere le shift
   !fd 

   X     = get_coor_SPHER(spher2bdyty(1,itacty),spher2bdyty(2,itacty))
   frame = get_inertia_frame_SPHER(spher2bdyty(1,itacty))
   !
   call get_data(spher2bdyty(1,itacty),spher2bdyty(2,itacty),data)

   ! if(is_Xperiodic_RBDY3())then
   !    xperiode = get_xperiode_RBDY3()
   !    if (X(1).gt.xperiode)then
   !       X(1) = X(1)-xperiode
   !    else if (X(1).lt.0.d0)then
   !       X(1) = X(1)+xperiode
   !    end if
   ! end if
   ! if(is_Yperiodic_RBDY3())then
   !    yperiode = get_yperiode_RBDY3()
   !    if (X(2).gt.yperiode)then
   !       X(2) = X(2)-yperiode
   !    else if (X(2).lt.0.d0)then
   !       X(2) = X(2)+yperiode
   !    end if
   ! end if

   do k = 1, nbpto
      vertex_ref(1:3) = unit_spher(1:3,k) * data(1)
 
      outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
      outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
      outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

   end do

 end subroutine updt_outline_SPHER

!------------------------------------------------------------------------
 function get_nb_point_outlines_SPHER()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_SPHER

   get_nb_point_outlines_SPHER => nb_point_outlines_SPHER

 end function get_nb_point_outlines_SPHER


 !------------------------------------------------------------------------
 function init_outlines_SPHER()
   implicit none
   integer(kind=4) :: itacty,sz,i,j,k
   real(kind=8)    :: dphi,dthe
   real(kind=8),dimension(:,:),pointer :: init_outlines_SPHER

   if ( nb_SPHER .eq. 0 ) then
     init_outlines_SPHER => null()
     return
   endif 

   if (associated(nb_point_outlines_SPHER)) deallocate(nb_point_outlines_SPHER)
   allocate(nb_point_outlines_SPHER(nb_SPHER+1)) 
   nb_point_outlines_SPHER(1) = 0
   do itacty = 1, nb_SPHER
     nb_point_outlines_SPHER(itacty+1) = nb_point_outlines_SPHER(itacty) + nbpto
   end do

   sz =  nb_point_outlines_SPHER(nb_SPHER+1)

   if (associated(outlines_SPHER)) deallocate(outlines_SPHER)
   allocate(outlines_SPHER(3,sz)) 

   outlines_SPHER(1:3,1:sz) = 0.d0

   init_outlines_SPHER => outlines_SPHER

   !storing connectivities of each spher
   ! 1/ sizing : nb_di*(nb_di-2) quad + 2 *(nb_di-1)tri 
   !sz = 0
   !do itacty = 1, nb_SPHER
   !  sz = sz + nb_di*(nb_di-2)*5 + 2*(nb_di-1)*4 + 1
   !end do
   sz = nb_SPHER * ( nb_di*(nb_di-2)*5 + 2*(nb_di)*4 + 1 )

   if( associated(all_connectivities) ) deallocate(all_connectivities)
   allocate(all_connectivities(sz))

   ! 2/ filling
   sz = 1
   do itacty = 1, nb_SPHER
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

   !computing vertices of unit spher
   dphi = PI_g/real(nb_di,8)
   dthe = 2.D0*dphi
   k = 1
   do j = 1, nb_di-1
     do i = 0, nb_di-1
       unit_spher(1,k) = sin(j*dphi)*cos(i*dthe)
       unit_spher(2,k) = sin(j*dphi)*sin(i*dthe)
       unit_spher(3,k) = cos(j*dphi)
       k = k+1
     end do
   end do
   ! poles
   unit_spher(1:3,k)   = (/ 0., 0., 1.0/)
   unit_spher(1:3,k+1) = (/ 0., 0.,-1.0/)

 end function init_outlines_SPHER

 !------------------------------------------------------------------------
 function get_nb_scalarfields_SPHER()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_SPHER

   get_nb_scalarfields_SPHER = nbsf

 end function get_nb_scalarfields_SPHER

 !------------------------------------------------------------------------
 subroutine updt_scalarfield_SPHER(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3)   = get_X_SPHER(spher2bdyty(1,itacty))
   scalarfield(4:9)   = get_V_SPHER(spher2bdyty(1,itacty))
   scalarfield(10:15) = get_REAC_SPHER(spher2bdyty(1,itacty))

 end subroutine updt_scalarfield_SPHER
 
 !------------------------------------------------------------------------
 function init_scalarfields_SPHER()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_SPHER

   if ( nb_SPHER .eq. 0 ) then
     init_scalarfields_SPHER => null()
     return
   endif 

   sz = nbsf * nb_SPHER

   if (associated(scalarfields_SPHER)) deallocate(scalarfields_SPHER)
   allocate(scalarfields_SPHER(sz)) 

   scalarfields_SPHER(1:sz) = 0.d0

   init_scalarfields_SPHER => scalarfields_SPHER

 end function init_scalarfields_SPHER

 !------------------------------------------------------------------------
 subroutine update_postdata_SPHER()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf

   if (nb_SPHER == 0) return

   if (.not. associated(outlines_SPHER) ) call faterr('SPHER::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_SPHER) ) call faterr('SPHER::update_postdata','init_scalarfields is mandatory')

   iszo = 0
   iszsf = 0
   do itacty=1,nb_SPHER
     call updt_outline_SPHER(itacty,outlines_SPHER(1:3,iszo+1:iszo+nbpto))
     iszo = iszo + nbpto
     call updt_scalarfield_SPHER(itacty,scalarfields_SPHER(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_SPHER

 !------------------------------------------------------------------------ 
 function get_all_connectivities_SPHER()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_all_connectivities_SPHER

   get_all_connectivities_SPHER => all_connectivities

 end function

 logical function all_dof_driven_SPHER(itact)
   implicit none
   integer, intent(in) :: itact

   all_dof_driven_SPHER = is_dof_driven_RBDY3( spher2bdyty(1,itact) )

 end function all_dof_driven_SPHER

!!!-PTA----------------------------------------------------------------------- 
  INTEGER FUNCTION get_visibleID_SPHER(itact)

    IMPLICIT NONE
    INTEGER :: itact 
 
    get_visibleID_SPHER = get_visibleID(spher2bdyty(1,itact))

  END FUNCTION get_visibleID_SPHER
!!!-PTA----------------------------------------------------------------------- 

 subroutine print_info_SPHER(itact)
   implicit none
   integer(kind=4), intent(in) :: itact

   if( spher2bdyty(3,itact) == i_rbdy3 ) then
     call print_info_RBDY3(spher2bdyty(1,itact))
   end if

 end subroutine

 subroutine clean_memory_SPHER()
   implicit none

   nb_SPHER = 0

   if( associated(spher2bdyty) ) then
     deallocate(spher2bdyty)
     nullify(spher2bdyty)
   end if

   if( associated(nb_point_outlines_SPHER) ) then
     deallocate(nb_point_outlines_SPHER)
     nullify(nb_point_outlines_SPHER)
   end if

   if( associated(all_connectivities) ) then
     deallocate(all_connectivities)
     nullify(all_connectivities)
   end if

   if( associated(outlines_SPHER) ) then
     deallocate(outlines_SPHER)
     nullify(outlines_SPHER)
   end if

   if( associated(scalarfields_SPHER) ) then
     deallocate(scalarfields_SPHER)
     nullify(scalarfields_SPHER)
   end if

 end subroutine

END MODULE SPHER


