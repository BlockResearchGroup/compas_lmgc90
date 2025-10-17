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
module JONCx                                       

  !!****h* LMGC90.CORE/JONCx
  !! NAME
  !!  module JONCx
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

  use RBDY2 ,only : get_nb_RBDY2, get_entity_RBDY2, get_data, &
                    get_cooref_RBDY2        => get_cooref       , &
                    get_mass_JONCx          => get_mass         , &
                    get_inv_mass_JONCx      => get_inv_mass     , &
                    print_info_JONCx        => print_info_RBDY2 , &
                    get_WS_JONCx            => get_WS           , &
                    get_therm_cond_JONCx    => get_therm_cond   , &
                    get_thermal_value_JONCx => get_thermal_value, &
                    get_average_WS_JONCx    =>  get_average_WS  , &
                    get_elec_cond_JONCx     => get_elec_cond    , &
                    get_betai_JONCx         => get_betai        , &
                    get_electric_potentiel_JONCx => get_electric_potentiel, &
                    !
                    get_nb_tacty_RBDY2  => get_nb_tacty , &
                    get_tacID_RBDY2     => get_tacid    , &
                    get_color_RBDY2     => get_color    , &
                    get_visible_RBDY2   => get_visible  , &
                    add_reac_RBDY2      => add_reac     , &
                    get_vlocy_RBDY2     => get_vlocy    , &
                    get_shiftTT_RBDY2   => get_shiftTT  , &
                    get_coor_RBDY2      => get_coor     , &
                    get_coorTT_RBDY2    => get_coorTT   , &
                    get_Xbegin_RBDY2    => get_Xbegin   , &
                    get_X_RBDY2         => get_X        , &
                    get_Vbegin_RBDY2    => get_Vbegin   , &
                    get_V_RBDY2         => get_V        , &
                    get_reac_RBDY2      => get_reac     , &
                    nullify_vlocy_RBDY2 => nullify_vlocy, &
                    comp_vlocy_RBDY2    => comp_vlocy   , &
                    nullify_reac_RBDY2  => nullify_reac


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
                    get_shiftTT_MBS2D         => get_shiftTT

 implicit none
 
 private

 integer(kind=4),dimension(:,:),allocatable,target  ::  joncx2bdyty   ! joncx2bdyty(1,itac): 
                                                       ! serial number of body RBDY2 to which is attached the contactor 
                                                       ! JONCx numbered itac in the list of all contactors JONCx 
                                                       ! joncx2bdyty(2,itac): 
                                                       ! serial number of contactor JONCx itac in the list of contactors
                                                       ! of any kind attached to body joncx2bdyty(1,itac)

 !> pointing on joncx2bdyty to use include
 integer(kind=4), dimension(:,:), pointer ::  tactype2bdyty => null()

 integer(kind=4)  :: nb_JONCx
 real(kind=8) :: min_radius,max_radius,mean_radius

 integer(kind=4) :: nbpto=20 ! nb points describing the outline
 integer(kind=4) :: nbsf=9 
 integer(kind=4),dimension(:),pointer :: nb_point_outlines_JONCx => null() 
 real(kind=8),dimension(:,:),pointer  :: outlines_JONCx => null()
 real(kind=8),dimension(:),pointer    :: scalarfields_JONCx => null()

 !logical :: mp_computation=.false.
 !logical :: betai_computation=.false.

! public data

 public joncx2bdyty

! public subroutines

 public read_bodies_JONCx

 public get_nb_JONCx, &
        get_mean_radius_JONCx,get_max_radius_JONCx,get_min_radius_JONCx, &
        get_radius_JONCx, &
        get_axes_JONCx, & 
        get_mass_JONCx,get_inv_mass_JONCx, &
        print_info_JONCx,get_WS_JONCx, &
        set_data_JONCx, &
        get_body_id_JONCx, &
        get_ptr_JONCx2BDYTY, &
        get_nb_point_outlines_JONCx,&
        init_outlines_JONCx, &
        get_nb_scalarfields_JONCx,&
        init_scalarfields_JONCx,&
        update_postdata_JONCx, &
        get_therm_cond_JONCx, &
        get_thermal_value_JONCx, &
        get_average_WS_JONCx, &
        get_electric_potentiel_JONCx, &
        get_elec_cond_JONCx, &
        clean_memory_JONCx, &
        !
        get_ENT, &
        get_color, &
        get_vlocy, get_reac, add_reac, &
        get_Xbegin, get_X, &
        get_Vbegin, get_V, &
        get_cooref, get_coor, get_coorTT, get_shiftTT, &
        nullify_reac, nullify_vlocy, comp_vlocy, &
        get_visible

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
 subroutine read_bodies_JONCx
   implicit none
   integer(kind=4) :: ibdyty,itacty,errare
   integer(kind=4) :: itact
   real(kind=8),dimension(2) :: data
   integer(kind=4) :: nb_RBDY2, nb_MBS2D
   character(len=18) :: IAM='JONCx::read_bodies'
   character(len=80) :: cout

   nb_JONCx=0

   nb_RBDY2=get_nb_RBDY2()

   do ibdyty=1,nb_RBDY2
     do itacty=1,get_nb_tacty_RBDY2(ibdyty)
       if (get_tacID_RBDY2(ibdyty,itacty) == 'JONCx')  nb_JONCx=nb_JONCx+1
     end do
   end do

   nb_MBS2D = get_nb_MBS2D()

   do ibdyty = 1, nb_MBS2D
     do itacty=1,get_nb_tacty_MBS2D(ibdyty)
       if (get_tacID_MBS2D(ibdyty,itacty) == i_joncx)  nb_JONCx=nb_JONCx+1
     end do
   end do

   write(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_JONCx,'JONCx found'
   call LOGMES(cout)

   if (nb_JONCx == 0) return

   allocate(joncx2bdyty(3,nb_JONCx),stat=errare)
   tactype2bdyty => joncx2bdyty

   if (errare /= 0) then
     call FATERR(IAM,'error allocating joncx2bdyty')
   end if

   nb_JONCx=0

   do ibdyty=1,nb_RBDY2
     do itacty=1,get_nb_tacty_RBDY2(ibdyty)
       if (get_tacID_RBDY2(ibdyty,itacty) == 'JONCx') then
         nb_JONCx=nb_JONCx+1
         joncx2bdyty(1,nb_JONCx)=ibdyty  !   joncx2bdyty(1,itac): serial number of body RBDY2 to which is attached the 
                                         !                        contactor JONCx numbered itac in the list of all 
                                         !                        contactors JONCx 
         joncx2bdyty(2,nb_JONCx)=itacty  !   joncx2bdyty(2,itac): serial number of contactor JONCx itac in the list of 
                                         !                        contactors of any kind attached to body joncx2bdyty(1,itac)
         joncx2bdyty(3,nb_JONCx)=i_rbdy2 !   joncx2bdyty(3,itac): type of body the contactor is attached to
       end if
     end do 
   end do

   do ibdyty = 1, nb_MBS2D
     do itacty = 1, get_nb_tacty_MBS2D(ibdyty)
       if (get_tacID_MBS2D(ibdyty,itacty) == i_joncx) then
         nb_JONCx=nb_JONCx+1
         joncx2bdyty(1,nb_JONCx) = ibdyty
         joncx2bdyty(2,nb_JONCx) = itacty
         joncx2bdyty(3,nb_JONCx) = i_mbs2
       end if
     end do
   end do

   ! on pre-calcule quelques valeurs pour la suite
   ! le rayon d'encombrement est radius=ax1+ax2 on en calcule le min et le max

   min_radius = 1.D20
   max_radius = 0.D0
   mean_radius= 0.D0

   do itact=1,nb_JONCx
     if( joncx2bdyty(3,itact) == i_rbdy2 ) then
       call get_data(joncx2bdyty(1,itact),joncx2bdyty(2,itact),data)
     else if( joncx2bdyty(3,itact) == i_mbs2 ) then
       data(:) = get_ptr_rdata_MBS2D(joncx2bdyty(1,itact),joncx2bdyty(2,itact))
     end if
     min_radius  = min(data(1)+data(2),min_radius)
     max_radius  = max(data(1)+data(2),max_radius)
     mean_radius = mean_radius + (data(1)+data(2))
   end do 
   mean_radius=mean_radius/nb_JONCx

 end subroutine read_bodies_JONCx
!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES ...
!
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------
 function get_nb_JONCx(fantome)
   implicit none
   integer(kind=4),optional :: fantome
   integer(kind=4) :: get_nb_JONCx
  
   get_nb_JONCx = nb_JONCx
  
 end function get_nb_JONCx
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 function get_mean_radius_JONCx(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_mean_radius_JONCx

   get_mean_radius_JONCx=mean_radius

   call faterr('JONCx::get_mean_radius','cette fontion n est pas utilisable actuellement')

 end function get_mean_radius_JONCx
!------------------------------------------------------------------------ 
 function get_max_radius_JONCx(fantome)
   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_max_radius_JONCx

   get_max_radius_JONCx=max_radius

 end function get_max_radius_JONCx
!------------------------------------------------------------------------ 
 function get_min_radius_JONCx(fantome)

   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_min_radius_JONCx

   get_min_radius_JONCx=min_radius

 end function get_min_radius_JONCx
!------------------------------------------------------------------------ 
 function get_axes_JONCx(itact)
   implicit none
   integer(kind=4)      :: itact 
   real(kind=8),dimension(2) :: get_axes_JONCx
   real(kind=8),dimension(2) :: data
   
   if( joncx2bdyty(3,itact) == i_rbdy2 ) then
     call get_data(joncx2bdyty(1,itact),joncx2bdyty(2,itact),data)
   else if( joncx2bdyty(3,itact) == i_mbs2 ) then
     data(:) = get_ptr_rdata_MBS2D(joncx2bdyty(1,itact),joncx2bdyty(2,itact))
   end if
   get_axes_JONCx=data 

 end function get_axes_JONCx  
!------------------------------------------------------------------------ 
 function get_radius_JONCx(itact)
   implicit none
   integer(kind=4)      :: itact 
   real(kind=8) :: get_radius_JONCx
   real(kind=8),dimension(2) :: data
   
   if( joncx2bdyty(3,itact) == i_rbdy2 ) then
     call get_data(joncx2bdyty(1,itact),joncx2bdyty(2,itact),data)
   else if( joncx2bdyty(3,itact) == i_mbs2 ) then
     data(:) = get_ptr_rdata_MBS2D(joncx2bdyty(1,itact),joncx2bdyty(2,itact))
   end if
   get_radius_JONCx=data(1)+data(2)

   call faterr('JONCx::get_radius','cette fontion n est pas utilisable actuellement')

 end function get_radius_JONCx  
!------------------------------------------------------------------------ 
 !> \brief use anonymous data to initialize a contactor
 !> rm : to remove... function used in old "new arch"
 subroutine set_data_JONCx(idata, rdata, node_list, nb_support, support, Brd)
   implicit none
   !> [in] idata: anonymous integer data (NULL)
   integer(kind=4), dimension(:), pointer :: idata
   !> [in] rdata: anonymous real data (axe_x, axe_y, [shift_x, shift_y, frame])
   real(kind=8)   , dimension(:), pointer :: rdata
   !> [out] node_list: list of nodes of the model_handle to use
   integer(kind=4), dimension(:), allocatable,   intent(inout) :: node_list
   !> [out] nb_support: number of support nodes to the contactor
   integer(kind=4),                              intent(out)   :: nb_support
   !> [out] support: support nodes to the contactor
   real(kind=8)   , dimension(:,:), allocatable, intent(inout) :: support
   !> [out] Brd: boundary radius
   real(kind=8)   , intent(out) :: Brd
   !
   integer(kind=4) :: i

   if( allocated(node_list) ) deallocate(node_list)
   if( allocated(support)   ) deallocate(support)

   ! only center of inertia
   allocate( node_list(1) )
   node_list(1) = 1

   ! 4 points defines the JONCx
   nb_support = 4
   allocate( support(nbDIME,nb_support) )

   support(1,1) = -rdata(1) ; support(2,1) = -rdata(2) 
   support(1,2) =  rdata(1) ; support(2,2) = -rdata(2) 
   support(1,3) =  rdata(1) ; support(2,3) =  rdata(2) 
   support(1,4) = -rdata(1) ; support(2,4) =  rdata(2) 

   ! when we will want to have a cluster of joncx...
   !if( size(data) == 4 ) then !there is a shift
   !  support(1,:) = rdata(3) + support(1,:)
   !  support(2,:) = rdata(4) + support(2,:)
   !else if( size(data) == 6 ) then !there is a frame
   !  do i = 1, nb_support
   !    support(1:2,i) = matmul( reshape(rdata(3:6),shape=(/2,2/)), support(1:2,i) )
   !  end do
   !else if( size(data) == 8 ) then !there is a shift and a frame
   !  do i = 1, nb_support
   !    support(1:2,i) = rdata(3:4) + matmul( reshape(rdata(3:6),shape=(/2,2/)), support(1:2,i) )
   !  end do
   !end if

   Brd = rdata(1) + rdata(2)

  end subroutine
!------------------------------------------------------------------------
  integer function get_body_id_JONCx(itacty)
    implicit none
    integer(kind=4), intent(in) :: itacty
 
    get_body_id_JONCx = joncx2bdyty(1,itacty)

  end function
!------------------------------------------------------------------------
 function get_ptr_JONCx2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_JONCx2BDYTY

   get_ptr_JONCx2BDYTY => joncx2bdyty

 end function get_ptr_JONCx2BDYTY
 
 !------------------------------------------------------------------------
 function get_nb_scalarfields_JONCx()
   implicit none

   integer(kind=4) :: get_nb_scalarfields_JONCx
   integer(kind=4) :: nbsf_   

   nbsf_ = nbsf
   
   !mr: rigid multi-physical case
   if(MpComputation_flag)then
      nbsf_ = nbsf_ + 1 ! temperature
      nbsf_ = nbsf_ + 1 ! thermal conductivity
      nbsf_ = nbsf_ + 1 ! surface energy
      nbsf_ = nbsf_ + 1 ! electrical potential
      nbsf_ = nbsf_ + 1 ! electrical conductivity
   end if
   if(BetaiComputation_flag)then
      nbsf_ = nbsf_ + 1 ! betai
   end if

    get_nb_scalarfields_JONCx = nbsf_
   
 end function get_nb_scalarfields_JONCx
 
 !------------------------------------------------------------------------
 subroutine updt_scalarfield_JONCx(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty,id_rbdy2,id_tacty,inbsf
   real(kind=8),dimension(nbsf) :: scalarfield
   
   id_rbdy2 = joncx2bdyty(1,itacty)
   id_tacty = joncx2bdyty(2,itacty)

   !mr : displacement and velocity of the center of mass
   scalarfield(1:3) = get_X(itacty)
   scalarfield(4:6) = get_V(itacty)
   scalarfield(7:9) = get_REAC(itacty)

   inbsf = 9
   if(MpComputation_flag)then
      scalarfield(inbsf+1)  = get_thermal_value_JONCx(id_rbdy2,id_tacty)
      scalarfield(inbsf+2)  = get_therm_cond_JONCx(id_rbdy2,id_tacty)
      scalarfield(inbsf+3)  = get_average_WS_JONCx(id_rbdy2,id_tacty)
      scalarfield(inbsf+4)  = get_electric_potentiel_JONCx(id_rbdy2)
      scalarfield(inbsf+5)  = get_elec_cond_JONCx(id_rbdy2)
      inbsf = inbsf + 5
   end if
   
   if(BetaiComputation_flag)then
      scalarfield(inbsf+1)  = get_betai_JONCx(id_rbdy2,id_tacty)
      inbsf = inbsf + 1
   end if
 end subroutine updt_scalarfield_JONCx
 
 !------------------------------------------------------------------------
 function init_scalarfields_JONCx()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_JONCx
   integer(kind=4) :: nbsf_

   if ( nb_JONCx .eq. 0 ) then
     init_scalarfields_JONCx => null()
     return
   endif 

   nbsf_ = nbsf
   
   !call get_mp_computation_FLAG(mp_computation)
   !call get_betai_computation_FLAG(betai_computation)

   !mr: rigid multi-physical case
   if(MpComputation_flag)then
      nbsf_ = nbsf_ + 1 ! temperature
      nbsf_ = nbsf_ + 1 ! thermal conductivity
      nbsf_ = nbsf_ + 1 ! surface energy
      nbsf_ = nbsf_ + 1 ! electrical potential
      nbsf_ = nbsf_ + 1 ! electrical conductivity
   end if
   
   if(BetaiComputation_flag)then
      nbsf_ = nbsf_ + 1 ! betai
   end if

   sz = nbsf_ * nb_JONCx

   if (associated(scalarfields_JONCx)) deallocate(scalarfields_JONCx)
   allocate(scalarfields_JONCx(sz)) 

   scalarfields_JONCx(1:sz) = 0.d0

   init_scalarfields_JONCx => scalarfields_JONCx

 end function init_scalarfields_JONCx
 
 !------------------------------------------------------------------------
 function get_nb_point_outlines_JONCx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_JONCx

   get_nb_point_outlines_JONCx => nb_point_outlines_JONCx

 end function get_nb_point_outlines_JONCx
 
 !------------------------------------------------------------------------ 
 subroutine updt_outline_JONCx(itacty,outline)
   implicit none
   integer(kind=4) :: itacty,k
   real(kind=8),dimension(2)         :: data
   real(kind=8)                      :: ax1,ax2,PI_2,DPI
   real(kind=8),dimension(3)         :: X
   real(kind=8),dimension(2,0:2*nbpto+1) :: outline

   if( joncx2bdyty(3,itacty) == i_rbdy2 ) then

     call get_data(joncx2bdyty(1,itacty),joncx2bdyty(2,itacty),data)
     X = get_coor_RBDY2(joncx2bdyty(1,itacty),joncx2bdyty(2,itacty))

   else if( joncx2bdyty(3,itacty) == i_mbs2 ) then

     data(:) = get_ptr_rdata_MBS2D(joncx2bdyty(1,itacty),joncx2bdyty(2,itacty))
     X = get_coor_MBS2D(joncx2bdyty(1,itacty),joncx2bdyty(2,itacty))

   end if

   ax1=data(1);ax2=data(2)
   outline(1,0) = X(1)
   outline(2,0) = X(2)

   DPI=PI_g/(nbpto-1)

   PI_2=PI_g/2.d0

   do k=1,nbpto

     outline(1,k)       = X(1) + ( cos(X(3))*(ax1+(ax2*cos(-PI_2+((k-1)*DPI)))))+ &
                                 (-sin(X(3))*(ax2*sin(-PI_2+((k-1)*DPI)))) 

     outline(2,k)       = X(2) + ( sin(X(3))*(ax1+(ax2*cos(-PI_2+((k-1)*DPI)))))+ &
                                 ( cos(X(3))*(ax2*sin(-PI_2+((k-1)*DPI))))
   end do

   do k=1,nbpto

     outline(1,nbpto+k) = X(1) + ( cos(X(3))*(-ax1+(ax2*cos(PI_2+((k-1)*DPI)))))+ &
                                 (-sin(X(3))*((ax2*sin(PI_2+((k-1)*DPI)))))

     outline(2,nbpto+k) = X(2) + ( sin(x(3))*(-ax1+(ax2*cos(PI_2+((k-1)*DPI)))))+ &
                                 ( cos(x(3))*(ax2*sin(PI_2+((k-1)*DPI))))
   end do


   outline(1,2*nbpto+1) = X(1)+(cos(X(3))*(ax1+(ax2*cos(-PI_2))))+(-sin(X(3))*((ax2*sin(-PI_2))))

   outline(2,2*nbpto+1) = X(2)+(sin(X(3))*(ax1+(ax2*cos(-PI_2))))+(cos(x(3))*(ax2*sin(-PI_2)))

 end subroutine updt_outline_JONCx
 
 !------------------------------------------------------------------------
 function init_outlines_JONCx()
   implicit none
   integer(kind=4) :: itacty,sz
   real(kind=8),dimension(:,:),pointer :: init_outlines_JONCx

   if ( nb_JONCx .eq. 0 ) then
     init_outlines_JONCx => null()
     return
   endif 

   ! attention de outlines on ne concerve pas le centre (noeud 0) et le dernier noeud ( retour au noeud 1)
   if (associated(nb_point_outlines_JONCx)) deallocate(nb_point_outlines_JONCx)
   allocate(nb_point_outlines_JONCx(nb_JONCx+1)) 
   nb_point_outlines_JONCx(1) = 0
   do itacty = 1, nb_JONCx
     nb_point_outlines_JONCx(itacty+1) = nb_point_outlines_JONCx(itacty) + 2*nbpto
   end do

   sz =  nb_point_outlines_JONCx(nb_JONCx+1)

   if (associated(outlines_JONCx)) deallocate(outlines_JONCx)
   allocate(outlines_JONCx(2,sz)) 

   outlines_JONCx(1:2,1:sz) = 0.d0

   init_outlines_JONCx => outlines_JONCx

 end function init_outlines_JONCx
 
 !------------------------------------------------------------------------
 subroutine update_postdata_JONCx()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf,nbsf_
   real(kind=8) :: outline(2,0:2*nbpto+1)

   if (nb_JONCx == 0 ) return
   if (.not. associated(outlines_JONCx)) call faterr('JONCx::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_JONCx)) call faterr('JONCx::update_postdata','init_scalarfields is mandatory')
   
   iszo = 0
   iszsf = 0
   nbsf_= get_nb_scalarfields_JONCx()
   do itacty=1,nb_JONCx
     ! attention la routine get_outline a ete ecrite pour gmv 
     ! et necessite un contour ferme ...
     call updt_outline_JONCx(itacty,outline)
     ! ... toutefois on ne garde pas le centre et le dernier        
     outlines_JONCx(:,iszo+1:iszo+2*nbpto) = outline(:,1:2*nbpto)    
     iszo = iszo + 2*nbpto
     !
     call updt_scalarfield_JONCx(itacty,scalarfields_JONCx(iszsf+1:iszsf+nbsf_))
     iszsf = iszsf +nbsf_
   enddo    

 end subroutine update_postdata_JONCx
!------------------------------------------------------------------------

  subroutine clean_memory_JONCx()
    implicit none
    integer(kind=4) :: i
 
    nb_joncx = 0
 
    if( allocated(joncx2bdyty) ) deallocate(joncx2bdyty)
 
    if( associated(nb_point_outlines_JONCx) ) then
      deallocate(nb_point_outlines_JONCx)
      nullify(nb_point_outlines_JONCx)
    end if
 
    if( associated(outlines_JONCx) ) then
      deallocate(outlines_JONCx)
      nullify(outlines_JONCx)
    end if
 
    if( associated(scalarfields_JONCx) ) then
      deallocate(scalarfields_JONCx)
      nullify(scalarfields_JONCx)
    end if
 
    tactype2bdyty => null()

  end subroutine

 end module JONCx
