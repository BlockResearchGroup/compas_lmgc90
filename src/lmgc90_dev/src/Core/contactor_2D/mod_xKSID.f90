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
module xKSID                                       

  !!****h* LMGC90.CORE/xKSID
  !! NAME
  !!  module xKSID
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/BULK_BEHAVIOUR
  !!  LMGC90.CORE/a_DOF
  !!  LMGC90.CORE/RBDY2
  !!****

  use utilities
  use overall
  use BULK_BEHAVIOUR
  use a_DOF

  use RBDY2,only:   get_nb_RBDY2,get_nb_tacty,get_tacid,get_data, &
                    get_color_RBDY2     => get_color, &
                    add_reac_RBDY2      => add_reac, &
                    get_vlocy_RBDY2     => get_vlocy, &
                    get_cooref_RBDY2    => get_cooref, &
                    get_coor_RBDY2      => get_coor, &
                    get_coorTT_RBDY2    => get_coorTT, & 
                    get_Xbegin_RBDY2    => get_Xbegin, &
                    get_X_RBDY2         => get_X, &
                    get_Vbegin_RBDY2    => get_Vbegin, &
                    get_V_RBDY2         => get_V, &
                    get_reac_RBDY2      => get_reac, &
                    nullify_vlocy_RBDY2 => nullify_vlocy, &
                    comp_vlocy_RBDY2    => comp_vlocy, &
                    nullify_reac_RBDY2  => nullify_reac, &
                    get_mass_xKSID      => get_mass, &
                    get_inv_mass_xKSID  => get_inv_mass, &    
                    print_info_xKSID    => print_info_RBDY2, &
                    get_shiftTT_RBDY2   => get_shiftTT        , &                    
                    get_entity_RBDY2                       , &
                    get_visible_RBDY2   => get_visible   , &
                    get_WS_xKSID        => get_WS

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

  ! xksid2bdyty(1,itac): 
  ! serial number of body RBDY2 to which is attached the contactor 
  ! xKSID numbered itac in the list of all contactors xKSID
  ! xksid2bdyty(2,itac):
  ! serial number of contactor xKSID itac in the list of contactors 
  ! of any kind attached to body xKSID2bdyty(1,itac)

  integer(kind=4),dimension(:,:),allocatable,target  ::  xksid2bdyty

  !> pointing on xksid2bdyty to use include
  integer(kind=4), dimension(:,:), pointer ::  tactype2bdyty => null()

  
 integer(kind=4) :: nb_xKSID
 real(kind=8) :: min_radius,max_radius,mean_radius

 ! fd structure pour gerer la dilatation (Xd,Vd)*nb_DISKx
 real(kind=8), dimension(:,:), allocatable :: dila
 
 integer(kind=4) :: nbpto=50 ! nb points describing the outline
 integer(kind=4) :: nbsf=9
 integer(kind=4),dimension(:),pointer :: nb_point_outlines_xKSID => null() 
 real(kind=8),dimension(:,:),pointer  :: outlines_xKSID => null()
 real(kind=8),dimension(:),pointer    :: scalarfields_xKSID => null()

! public data

 public xksid2bdyty

! les routines publiques

 public read_bodies_xKSID

 public get_nb_xKSID, &
        get_mean_radius_xKSID, get_max_radius_xKSID, get_min_radius_xKSID, &
        !get_cooref_xKSID, get_coor_xKSID, get_coorTT_xKSID, &
        get_cooref, get_coor, get_coorTT, &        
        get_Xbegin, get_X, &
        get_Vbegin, get_V, &
        get_reac,&
        get_radius_xKSID, get_color, &
        nullify_reac,nullify_vlocy, comp_vlocy, &
        get_vlocy, add_reac, &
        get_mass_xKSID,get_inv_mass_xKSID, &
        get_ptr_xKSID2BDYTY, &
        get_ENT,print_info_xKSID,get_WS_xKSID, &
        get_nb_point_outlines_xKSID,&
        init_outlines_xKSID, &
        get_nb_scalarfields_xKSID,&
        init_scalarfields_xKSID,&
        update_postdata_xKSID,&
        clean_memory_xKSID,&
        get_Vd_xKSID, &
        set_Xd_xKSID, &
        set_Vd_xKSID
        

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
 subroutine read_bodies_xKSID
   implicit none
   integer(kind=4) :: ibdyty,itacty,errare,itact
   real(kind=8),dimension(1) :: data
   integer(kind=4) :: nb_RBDY2
                            !123456789012345678
   character(len=18) :: IAM='xKSID::read_bodies'
   character(len=80) :: cout

   nb_xKSID=0

   nb_RBDY2=get_nb_RBDY2()

   do ibdyty=1,nb_RBDY2   
     do itacty=1,get_nb_tacty(ibdyty)
       if (get_tacID(ibdyty,itacty) == 'xKSID')  nb_xKSID=nb_xKSID+1
     end do 
   end do

   write(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_xKSID,'xKSID found'
   call LOGMES(cout)

   if (nb_xKSID == 0) return

   allocate(xksid2bdyty(3,nb_xKSID),stat=errare)

   if (errare /= 0) then
     call FATERR(IAM,'error allocating xksid2bdyty')
   end if

   tactype2bdyty => xksid2bdyty
   
   nb_xKSID=0

   do ibdyty=1,nb_RBDY2   
     do itacty=1,get_nb_tacty(ibdyty)
       if (get_tacID(ibdyty,itacty) == 'xKSID') then
         nb_xKSID=nb_xKSID+1
         xksid2bdyty(1,nb_xKSID)=ibdyty  !   xksid2bdyty(1,itac): serial number of body RBDY2 to which is attached the 
                                         !                        contactor xKSID numbered itac in the list of all 
                                         !                        contactors xKSID 
         xksid2bdyty(2,nb_xKSID)=itacty  !   xksid2bdyty(2,itac): serial number of contactor xKSID itac in the list of 
                                         !                        contactors of any kind attached to body xksid2bdyty(1,itac)
         xksid2bdyty(3,nb_xKSID)=i_rbdy2 !   xksid2bdyty(3,itac): type of body the contactor is attached to
       end if
     end do 
   end do

   ! fd gestion de la dilatation

   allocate(dila(2,nb_xKSID),stat=errare)
   if (errare /= 0) then
      call FATERR(IAM,'error allocating dila')
   end if

   dila = 0.d0
   
   ! on pre-calcule quelques valeurs pour la suite

   min_radius = 1.D20
   max_radius = 0.D0
   mean_radius= 0.D0

   do itact=1,nb_xKSID
     call get_data(xksid2bdyty(1,itact),xksid2bdyty(2,itact),data)
     min_radius  = min(data(1),min_radius)
     max_radius  = max(data(1),max_radius)
     mean_radius = mean_radius + data(1)
   end do 
   mean_radius=mean_radius/nb_xKSID

 end subroutine read_bodies_xKSID
!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES ...
!
!------------------------------------------------------------------------ 
 function get_nb_xKSID(fantome)
   implicit none
   integer(kind=4),optional :: fantome
   integer(kind=4) :: get_nb_xKSID
  
   get_nb_xKSID = nb_xKSID
  
 end function get_nb_xKSID
!------------------------------------------------------------------------ 
 function get_mean_radius_xKSID(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_mean_radius_xKSID

   get_mean_radius_xKSID=mean_radius

 end function get_mean_radius_xKSID
!------------------------------------------------------------------------ 
 function get_max_radius_xKSID(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_max_radius_xKSID

   get_max_radius_xKSID=max_radius

 end function get_max_radius_xKSID
!------------------------------------------------------------------------ 
 function get_min_radius_xKSID(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_min_radius_xKSID

   get_min_radius_xKSID=min_radius

 end function get_min_radius_xKSID
!------------------------------------------------------------------------ 
 function get_radius_xKSID(itact)
   implicit none
   integer(kind=4)                   :: itact
   real(kind=8)              :: get_radius_xKSID
   real(kind=8),dimension(1) :: radius

   call get_data(xksid2bdyty(1,itact),xksid2bdyty(2,itact),radius)
   
   get_radius_xKSID = radius(1) + dila(1,itact)

 end function get_radius_xKSID  
!------------------------------------------------------------------------ 
!  function get_color_xKSID(itact)
!    implicit none
!    integer(kind=4)      :: itact 
!    character(len=5) :: get_color_XKSID
   
!     get_color_xKSID = get_color(xksid2bdyty(1,itact),xksid2bdyty(2,itact))
 
!  end function get_color_xKSID  
! !------------------------------------------------------------------------ 
 function get_ptr_xKSID2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_xKSID2BDYTY

   get_ptr_Xksid2BDYTY => xksid2bdyty

 end function get_ptr_xKSID2BDYTY

!------------------------------------------------------------------------ 
 subroutine updt_outline_xKSID(itacty,outline)
   implicit none
   integer(kind=4) :: itacty,k
   real(kind=8),dimension(1)         :: data
   real(kind=8)                      :: r,DPI
   real(kind=8),dimension(3)         :: X
   real(kind=8),dimension(2,0:nbpto) :: outline

   call get_data(xksid2bdyty(1,itacty),xksid2bdyty(2,itacty),data)

   r=get_radius_xKSID(itacty)

   X=get_coor_RBDY2(xksid2bdyty(1,itacty),xksid2bdyty(2,itacty))

   outline(1,0) = X(1)
   outline(2,0) = X(2)

   DPI=2.d0*PI_g/((nbpto/2)-1)

   do k=1,nbpto/2

     outline(1,k) = X(1) + r*cos(X(3)+(k-1)*DPI)
     outline(2,k) = X(2) + r*sin(X(3)+(k-1)*DPI)

   enddo

   r = r*1.1
   do k=1,nbpto/2

     outline(1,nbpto/2+k) = X(1) + r*cos(X(3)+(nbpto/2-k)*DPI)
     outline(2,nbpto/2+k) = X(2) + r*sin(X(3)+(nbpto/2-k)*DPI)

   enddo

 end subroutine updt_outline_xKSID
!------------------------------------------------------------------------
 function get_nb_point_outlines_xKSID()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_xKSID

   get_nb_point_outlines_xKSID => nb_point_outlines_xKSID

 end function get_nb_point_outlines_xKSID
!------------------------------------------------------------------------
 function init_outlines_xKSID()
   implicit none
   integer(kind=4) :: itacty,sz
   real(kind=8),dimension(:,:),pointer :: init_outlines_xKSID

   if ( nb_xKSID .eq. 0 ) then
     init_outlines_xKSID => null()
     return
   endif 

   if (associated(nb_point_outlines_xKSID)) deallocate(nb_point_outlines_xKSID)
   allocate(nb_point_outlines_xKSID(nb_xKSID+1)) 
   nb_point_outlines_xKSID(1) = 0
   do itacty = 1, nb_xKSID
     nb_point_outlines_xKSID(itacty+1) = nb_point_outlines_xKSID(itacty) + nbpto!-1
   end do

   sz =  nb_point_outlines_xKSID(nb_xKSID+1)

   if (associated(outlines_xKSID)) deallocate(outlines_xKSID)
   allocate(outlines_xKSID(2,sz)) 

   outlines_xKSID(1:2,1:sz) = 0.d0

   init_outlines_xKSID => outlines_xKSID

 end function init_outlines_xKSID
 
 !------------------------------------------------------------------------
 function get_nb_scalarfields_xKSID()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_xKSID

   get_nb_scalarfields_xKSID = nbsf

 end function get_nb_scalarfields_xKSID
 
 !------------------------------------------------------------------------
 subroutine updt_scalarfield_xKSID(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3) = get_X_RBDY2(xksid2bdyty(1,itacty))
   scalarfield(4:6) = get_V_RBDY2(xksid2bdyty(1,itacty))
   scalarfield(7:9) = get_REAC_RBDY2(xksid2bdyty(1,itacty))

 end subroutine updt_scalarfield_xKSID

 !------------------------------------------------------------------------
 function init_scalarfields_xKSID()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_xKSID

   if ( nb_xKSID .eq. 0 ) then
     init_scalarfields_xKSID => null()
     return
   endif 

   sz = nbsf * nb_xKSID

   if (associated(scalarfields_xKSID)) deallocate(scalarfields_xKSID)
   allocate(scalarfields_xKSID(sz)) 

   scalarfields_xKSID(1:sz) = 0.d0

   init_scalarfields_xKSID => scalarfields_xKSID

 end function init_scalarfields_xKSID
 
 !------------------------------------------------------------------------
 subroutine update_postdata_xKSID()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf
   real(kind=8) :: outline(2,0:nbpto)

   if ( nb_xKSID == 0) return
   if (.not. associated(outlines_xKSID)) call faterr('xKSID::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_xKSID)) call faterr('xKSID::update_postdata','init_scalarfields is mandatory')
   
   iszo = 0
   iszsf = 0
   do itacty=1,nb_xKSID
     call updt_outline_xKSID(itacty,outline) 
     outlines_xKSID(:,iszo+1:iszo+nbpto) = outline(:,1:nbpto)
     iszo = iszo + nbpto
     call updt_scalarfield_xKSID(itacty,scalarfields_xKSID(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_xKSID
!------------------------------------------------------------------------

 subroutine clean_memory_xKSID()
   implicit none
   integer(kind=4) :: i

   nb_xksid = 0

   if( allocated(xksid2bdyty) ) deallocate(xksid2bdyty)

   if( associated(nb_point_outlines_xKSID) ) then
     deallocate(nb_point_outlines_xKSID)
     nullify(nb_point_outlines_xKSID)
   end if

   if( associated(outlines_xKSID) ) then
     deallocate(outlines_xKSID)
     nullify(outlines_xKSID)
   end if

   if( associated(scalarfields_xKSID) ) then
     deallocate(scalarfields_xKSID)
     nullify(scalarfields_xKSID)
   end if

   if( allocated(dila) ) deallocate(dila)
   
 end subroutine

!------------------------------------------------------------------------
 subroutine get_Vd_xKSID(itact,v)
   implicit none
   integer(kind=4)      :: itact 
   real(kind=8)         :: v 
   
   v = dila(2,itact) 

 end subroutine get_Vd_xKSID  
!------------------------------------------------------------------------
 subroutine set_Vd_xKSID(itact,v)
   implicit none
   integer(kind=4) :: itact 
   real(kind=8)    :: v
   
   dila(2,itact) = v 

 end subroutine set_Vd_xKSID  
!------------------------------------------------------------------------
 subroutine set_Xd_xKSID(itact,x)
   implicit none
   integer(kind=4) :: itact 
   real(kind=8)    :: x
   
   dila(1,itact) = x

 end subroutine set_Xd_xKSID  
!------------------------------------------------------------------------

 
end module xKSID
