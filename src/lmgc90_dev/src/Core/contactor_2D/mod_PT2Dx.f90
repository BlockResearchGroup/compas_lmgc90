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
module PT2Dx                                       

  use utilities
  use overall
  use BULK_BEHAVIOUR
  use a_DOF

  use RBDY2,only:  get_nb_RBDY2,get_nb_tacty,get_tacid, &
                   get_color_RBDY2=>get_color, &
                   add_reac_RBDY2=>add_reac, &
                   get_vlocy_RBDY2=>get_vlocy, &
                   get_cooref_RBDY2 => get_cooref, &
                   get_coor_RBDY2 => get_coor, &
                   get_coorTT_RBDY2 => get_coorTT, &
                   !
                   get_Xbegin_RBDY2 => get_Xbegin, &
                   get_X_RBDY2 => get_X, &
                   get_Vbegin_RBDY2 => get_Vbegin, &
                   get_V_RBDY2 => get_V, &
                   get_reac_RBDY2 => get_reac, &
                   nullify_vlocy_RBDY2 => nullify_vlocy, &
                   comp_vlocy_RBDY2 => comp_vlocy, &
                   nullify_reac_RBDY2 => nullify_reac, &
                   get_shiftTT_RBDY2 => get_shiftTT, &
                   get_entity_RBDY2, &
                   print_info_PT2Dx => print_info_RBDY2, &
                   get_visible_RBDY2=>get_visible

  use mbs2d, only : get_nb_MBS2D              => get_nb             , &
                    get_nb_tacty_MBS2D        => get_nb_tacty       , &
                    get_tacID_MBS2D           => get_tacID          , &
                    get_color_MBS2D           => get_color          , &
                    get_ptr_idata_MBS2D       => get_ptr_idata      , &
                    get_ptr_rdata_MBS2D       => get_ptr_rdata      , &
                    get_coor_MBS2D            => get_coor           , &
                    get_coorTT_MBS2D          => get_coorTT         , &
                    get_entity_MBS2D          => get_entity         , &
                    add_reac_MBS2D            => add_reac           , &
                    comp_vlocy_MBS2D          => comp_vlocy         , &
                    nullify_vlocy_MBS2D       => nullify_vlocy      , &
                    nullify_reac_MBS2D        => nullify_reac       , &
                    get_vlocy_MBS2D           => get_vlocy          , &
                    get_shiftTT_MBS2D         => get_shiftTT        , &
                    get_nodeID_MBS2D          => get_nodeID

  
  
 implicit none
 
 private
 
 ! pt2dxbdyty(1,itac): 
 ! serial number of body RBDY2 to which is attached the contactor 
 ! PT2Dx numbered itac in the list of all contactors PT2Dx 
 ! pt2dx2bdyty(2,itac): 
 ! serial number of contactor PT2Dx itac in the list of contactors 
 ! of any kind attached to a body  pt2dx2bdyty(1,itac)

 integer,dimension(:,:),allocatable,target  ::  pt2dx2bdyty

 !> pointing on pt2dx2bdyty to use include
 integer(kind=4), dimension(:,:), pointer ::  tactype2bdyty => null()
 
 integer      :: nb_PT2Dx

 ! display parameters ...
 ! ... radius of the glyph
 real(kind=8) :: REF_RADIUS = 1.0
 ! .. nb of points to describe the outline of the glyph
 integer      :: nbpto=12
 integer(kind=4),dimension(:),pointer :: nb_point_outlines_PT2Dx => null() 
 real(kind=8),dimension(:,:),pointer  :: outlines_PT2Dx => null()

 ! ... scalar fields attached to a point (coor, reac)
 integer(kind=4) :: nbsf=9 
 real(kind=8),dimension(:),pointer    :: scalarfields_PT2Dx => null()

 ! les donnees publiques

 public pt2dx2bdyty

 ! les routines publiques

 public read_bodies_PT2Dx

 public get_nb_PT2Dx, &
        get_mean_radius_PT2Dx,get_max_radius_PT2Dx,get_min_radius_PT2Dx, &
        !get_cooref_PT2Dx,get_coor_PT2Dx, get_coorTT_PT2Dx,&
        get_cooref,get_coor, get_coorTT,&        
        get_Xbegin,get_Vbegin,get_V,&
        get_reac,&
        get_color, &
        nullify_reac, nullify_vlocy, comp_vlocy, &
        get_vlocy,add_reac, &
        get_shiftTT, &
        get_ENT,get_visible,is_PT2Dx_same_RBDY2,print_info_PT2Dx, & 
        set_PT2Dx_radius_PT2Dx, &
        get_ptr_PT2Dx2BDYTY, &
        !
        get_nb_point_outlines_PT2Dx,&
        init_outlines_PT2Dx, &
        !
        get_nb_scalarfields_PT2Dx,&
        init_scalarfields_PT2Dx,&
        
        update_postdata_PT2Dx  ,&
        clean_memory_PT2Dx

 contains

  include 'contactor_2D.f90'
 ! defines the following functions/subroutines :
 !integer(kind=4) function get_ENT(itact)
 !function get_color(itact)
 !function get_Xbegin(itact)
 !function get_X(itact)
 !function get_Vbegin(itact)
 !function get_V(itact)
 !function get_reac(itact)
 !function get_cooref(itact)  
 !function get_coor(itact)
 !function get_visible(itact)
 !subroutine add_reac(itact,xxccdof,xxreac,storage)
 !function get_coorTT(itact)
 !subroutine comp_vlocy(itact,storage)
 !subroutine nullify_vlocy(itact,storage)
 !subroutine nullify_reac(itact,storage)
 !subroutine get_vlocy(itact,istate,vlocy)
 !function get_shiftTT(itact)
   
!------------------------------------------------------------------------
 subroutine read_bodies_PT2Dx
   implicit none
   integer(kind=4) :: ibdyty,itacty,errare
   integer(kind=4) :: nb_RBDY2
   character(len=18) :: IAM='PT2Dx::read_bodies'
   character(len=80) :: cout

   nb_PT2Dx=0

   nb_RBDY2=get_nb_RBDY2()

   do ibdyty=1,nb_RBDY2   
     do itacty=1,get_nb_tacty(ibdyty)
       if (get_tacID(ibdyty,itacty) == 'PT2Dx')  nb_PT2Dx=nb_PT2Dx+1
     end do 
   end do

   write(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_PT2Dx,'PT2Dx found'
   call LOGMES(cout)

   if (nb_PT2Dx == 0) return

   allocate(pt2dx2bdyty(3,nb_PT2Dx),stat=errare)
   if (errare /= 0) then
     call FATERR(IAM,'error allocating pt2dx2bdyty')
   end if
   tactype2bdyty => pt2dx2bdyty
   

   nb_PT2Dx=0

   do ibdyty=1,nb_RBDY2   
     do itacty=1,get_nb_tacty(ibdyty)
       if (get_tacID(ibdyty,itacty) == 'PT2Dx') then
         nb_PT2Dx=nb_PT2Dx+1
         pt2dx2bdyty(1,nb_PT2Dx)=ibdyty  !   pt2dx2bdyty(1,itac) : serial number of body RBDY2 to which is attached the 
                                         !                         contactor PT2Dx numbered itac in the list of all 
                                         !                         contactors PT2Dx 
         pt2dx2bdyty(2,nb_PT2Dx)=itacty  !   pt2dx2bdyty(2,itac) : serial number of contactor PT2Dx itac in the list of 
                                         !                         contactors of any kind attached to body pt2dx2bdyty(1,itac)
         pt2dx2bdyty(3,nb_PT2Dx)=i_rbdy2 !   pt2dx2bdyty(3,itac): type of body the contactor is attached to
       end if
     end do 
   end do

 end subroutine read_bodies_PT2Dx
!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES 
!
!------------------------------------------------------------------------ 
 function get_nb_PT2Dx(fantome)
   implicit none
   integer(kind=4),optional :: fantome
   integer(kind=4) :: get_nb_PT2Dx
  
   get_nb_PT2Dx = nb_PT2Dx
  
 end function get_nb_PT2Dx
!------------------------------------------------------------------------ 
 function get_mean_radius_PT2Dx(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_mean_radius_PT2Dx

   get_mean_radius_PT2Dx=0.d0

 end function get_mean_radius_PT2Dx
!------------------------------------------------------------------------ 
 function get_max_radius_PT2Dx(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_max_radius_PT2Dx

   get_max_radius_PT2Dx=0.d0

 end function get_max_radius_PT2Dx
!------------------------------------------------------------------------ 
 function get_min_radius_PT2Dx(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_min_radius_PT2Dx

   get_min_radius_PT2Dx=0.d0

 end function get_min_radius_PT2Dx
! !------------------------------------------------------------------------ 
!  function get_color_PT2Dx(itact)
!    implicit none
!    integer(kind=4)      :: itact 
!    character(len=5) :: get_color_PT2Dx
   
!     get_color_PT2Dx = get_color(pt2dx2bdyty(1,itact),pt2dx2bdyty(2,itact))
 
!  end function get_color_PT2Dx  
! !------------------------------------------------------------------------ 
!  function get_visible_PT2Dx(itact)
!    implicit none
!    integer(kind=4) :: itact 
!    logical :: get_visible_PT2Dx
   
!    get_visible_PT2Dx = get_visible(pt2dx2bdyty(1,itact))
 
!  end function get_visible_PT2Dx  
!------------------------------------------------------------------------ 
 function is_PT2Dx_same_RBDY2(itact1,itact2)
   implicit none
   integer(kind=4) :: itact1,itact2 
   logical :: is_PT2Dx_same_RBDY2
   
   is_PT2Dx_same_RBDY2=.false.
   if(pt2dx2bdyty(1,itact1) == pt2dx2bdyty(1,itact2)) is_PT2Dx_same_RBDY2=.true.
 
 end function is_PT2Dx_same_RBDY2
! !------------------------------------------------------------------------ 
!  integer function get_nb_point_outline_PT2Dx(fantome)
!    implicit none
!    integer(kind=4),optional :: fantome

!    get_nb_point_outline_PT2Dx = nbpto

 ! end function get_nb_point_outline_PT2Dx 
!------------------------------------------------------------------------ 
 subroutine updt_outline_PT2Dx(itacty,outline)
   implicit none
   integer(kind=4) :: itacty,k
   real(kind=8)                      :: r,DPI
   real(kind=8),dimension(3)         :: X
   real(kind=8),dimension(2,0:nbpto) :: outline

   X=get_coor(itacty)
   r = REF_RADIUS

   outline(1,0:0) = X(1)
   outline(2,0:0) = X(2)

   DPI=2.d0*PI_g/(nbpto-1)

   do k=1,nbpto

     outline(1,k) = X(1) + r*cos(X(3)+(k-1)*DPI)
     outline(2,k) = X(2) + r*sin(X(3)+(k-1)*DPI)

   end do

 end subroutine updt_outline_PT2Dx
!------------------------------------------------------------------------ 
  subroutine set_pt2dx_radius_PT2Dx(radius)
    implicit none
    real(kind=8) :: radius

    REF_RADIUS = radius

  end subroutine set_pt2dx_radius_PT2Dx
!------------------------------------------------------------------------
 function get_nb_point_outlines_PT2Dx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_PT2Dx

   get_nb_point_outlines_PT2Dx => nb_point_outlines_PT2Dx

 end function get_nb_point_outlines_PT2Dx
!------------------------------------------------------------------------
 function init_outlines_PT2Dx()
   implicit none
   integer(kind=4) :: itacty,sz
   real(kind=8),dimension(:,:),pointer :: init_outlines_PT2Dx

   if ( nb_PT2Dx .eq. 0 ) then
     init_outlines_PT2Dx => null()
     return
   endif 

   if (associated(nb_point_outlines_PT2Dx)) deallocate(nb_point_outlines_PT2Dx)
   allocate(nb_point_outlines_PT2Dx(nb_PT2Dx+1)) 
   nb_point_outlines_PT2Dx(1) = 0
   do itacty = 1, nb_PT2Dx
     nb_point_outlines_PT2Dx(itacty+1) = nb_point_outlines_PT2Dx(itacty) + nbpto-1
   end do

   sz =  nb_point_outlines_PT2Dx(nb_PT2Dx+1)

   if (associated(outlines_PT2Dx)) deallocate(outlines_PT2Dx)
   allocate(outlines_PT2Dx(2,sz)) 

   outlines_PT2Dx(1:2,1:sz) = 0.d0

   init_outlines_PT2Dx => outlines_PT2Dx

 end function init_outlines_PT2Dx
!!!-----------------------------------------------------------------------
  function get_nb_scalarfields_PT2Dx()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_PT2Dx

   get_nb_scalarfields_PT2Dx = nbsf

 end function get_nb_scalarfields_PT2Dx
!------------------------------------------------------------------------
 subroutine updt_scalarfield_PT2Dx(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3) = get_X_RBDY2(pt2dx2bdyty(1,itacty))
   scalarfield(4:6) = get_V_RBDY2(pt2dx2bdyty(1,itacty))
   scalarfield(7:9) = get_REAC_RBDY2(pt2dx2bdyty(1,itacty))

 end subroutine updt_scalarfield_PT2Dx
!------------------------------------------------------------------------
 function init_scalarfields_PT2Dx()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_PT2Dx

   if ( nb_PT2Dx .eq. 0 ) then
     init_scalarfields_PT2Dx => null()
     return
   endif 

   sz = nbsf * nb_PT2Dx

   if (associated(scalarfields_PT2Dx)) deallocate(scalarfields_PT2Dx)
   allocate(scalarfields_PT2Dx(sz)) 

   scalarfields_PT2Dx(1:sz) = 0.d0

   init_scalarfields_PT2Dx => scalarfields_PT2Dx

 end function init_scalarfields_PT2Dx
!------------------------------------------------------------------------
 subroutine update_postdata_PT2Dx()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf
   real(kind=8) :: outline(2,0:nbpto)

   if (nb_PT2Dx == 0 ) return
   if (.not. associated(outlines_PT2Dx)) call faterr('PT2Dx::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_PT2Dx)) call faterr('PT2Dx::update_postdata','init_scalarfields is mandatory')

   iszo = 0
   iszsf = 0
   do itacty=1,nb_PT2Dx
     call updt_outline_PT2Dx(itacty,outline) 
     ! attention la routine get_outline a ete ecrite pour gmv 
     ! et necessite un contour ferme
     ! mj pas touche ...
     outlines_PT2Dx(:,iszo+1:iszo+nbpto-1) = outline(:,1:nbpto-1)    
     iszo = iszo + nbpto-1
     call updt_scalarfield_PT2Dx(itacty,scalarfields_PT2Dx(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_PT2Dx
!------------------------------------------------------------------------
 function get_ptr_PT2Dx2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_PT2Dx2BDYTY

   get_ptr_PT2Dx2BDYTY => pt2dx2bdyty

 end function get_ptr_PT2Dx2BDYTY
!------------------------------------------------------------------------

 subroutine clean_memory_PT2Dx()
   implicit none
   integer(kind=4) :: i

   nb_pt2dx = 0

   if( allocated(pt2dx2bdyty) ) deallocate(pt2dx2bdyty)

   if( associated(nb_point_outlines_PT2Dx) ) then
     deallocate(nb_point_outlines_PT2Dx)
     nullify(nb_point_outlines_PT2Dx)
   end if

   if( associated(outlines_PT2Dx) ) then
     deallocate(outlines_PT2Dx)
     nullify(outlines_PT2Dx)
   end if

   if( associated(scalarfields_PT2Dx) ) then
     deallocate(scalarfields_PT2Dx)
     nullify(scalarfields_PT2Dx)
   end if

 end subroutine

end module PT2Dx

