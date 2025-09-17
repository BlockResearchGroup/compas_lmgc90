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
module POLYG                                       
  
  use utilities
  use predicates
  use overall
  use BULK_BEHAVIOUR
  use a_DOF

  use RBDY2, only: get_nb_RBDY2, get_entity_RBDY2, &
                   get_idata, get_data, &
                   get_mass_POLYG => get_mass, &
                   get_inv_mass_POLYG => get_inv_mass, &
                   add_stress_POLYG => add_stress, &
                   get_ENT_POLYG => get_entity_RBDY2,&
                   print_info_POLYG => print_info_RBDY2, &
                   get_r2m_POLYG    => get_r2m, &
                   set_vcooref_RBDY2, &
                   get_therm_cond_POLYG => get_therm_cond, &
                   get_thermal_value_POLYG => get_thermal_value, &
                   get_average_WS_POLYG =>  get_average_WS, &
                   get_electric_potentiel_POLYG => get_electric_potentiel, &
                   get_elec_cond_POLYG => get_elec_cond, &
                   get_betai_POLYG => get_betai, &
                   add_betai_to_POLYG => add_betai, &
                   get_WS_POLYG        => get_WS       , &
                   !
                   get_nb_tacty_RBDY2  => get_nb_tacty , &
                   get_tacID_RBDY2     => get_tacid    , &
                   get_color_RBDY2     => get_color    , &
                   get_visible_RBDY2   => get_visible  , &
                   add_reac_RBDY2      => add_reac     , &
                   get_vlocy_RBDY2     => get_vlocy    , &
                   get_cooref_RBDY2    => get_cooref   , &
                   get_coor_RBDY2      => get_coor     , &
                   get_coorTT_RBDY2    => get_coorTT   , &
                   get_shiftTT_RBDY2   => get_shiftTT  , &
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
                    get_shiftTT_MBS2D         => get_shiftTT        , &
                    get_nodeID_MBS2D          => get_nodeID

 implicit none
 
 private
 
 !polyg2bdyty(1,itac) : serial number of body RBDY2 to which 
 !                      is attached the contactor POLYG numbered
 !                      itac in the list of all contactors POLYG  
 !                         
 !polyg2bdyty(2,itac) : serial number of contactor POLYG itac in
 !                      the list of contactors POLYG attached  
 !                      to a body (generically 1)
 integer(kind=4),dimension(:,:),allocatable,target :: polyg2bdyty
 
 !> pointing on polyg2bdyty to use include
 integer(kind=4), dimension(:,:), pointer ::  tactype2bdyty => null()

 integer(kind=4) :: nb_POLYG
 real(kind=8) :: min_radius,max_radius,mean_radius


 integer(kind=4) :: nbsf = 9
 integer(kind=4),dimension(:),pointer :: nb_point_outlines_POLYG => null() 
 real(kind=8), dimension(:,:),pointer :: outlines_POLYG => null()
 real(kind=8), dimension(:),pointer :: scalarfields_POLYG => null()

!------------------------------------------------------------------------
! local datum for POLYG
!------------------------------------------------------------------------

 type,public ::  T_POLYG
    integer(kind=4)                          ::  nb_vertex
    real(kind=8),dimension(:,:),pointer      ::  vertex    
    real(kind=8),dimension(:,:),pointer      ::  vertex_ref
    real(kind=8),dimension(:,:),pointer      ::  normale
    real(kind=8),dimension(:,:),pointer      ::  normale_ref
    real(kind=8)                             ::  outer_radius
    real(kind=8)                             ::  inner_radius
    real(kind=8),dimension(:,:),pointer      :: Xd
    real(kind=8),dimension(:,:),pointer      :: Vd
 end type T_POLYG

 type (T_POLYG),dimension(:),allocatable      :: l_POLYG

type,public :: T_BDYTY_2_POLYG
   integer(kind=4)                           :: polyTactyBegin
   integer(kind=4)                           :: polyTactyEnd
end type T_BDYTY_2_POLYG

type(T_BDYTY_2_POLYG),dimension(:),allocatable :: l_bdyty_2_polyg
! les donnees publiques
 public polyg2bdyty !,change_polyg2mailx,change_polyg2mailx_fine

! les routines publiques

 public read_bodies_POLYG

 public get_nb_polyg,&
        get_mean_radius_polyg,get_max_radius_polyg,get_min_radius_polyg, &
        !get_cooref_POLYG, &
        get_radius_POLYG, &
        get_l_POLYG,&
        get_mass_POLYG,get_inv_mass_POLYG, &
        move_bdary_polyg, &
        is_POLYG_same_BDYTY,print_info_POLYG,&
        get_r2m_POLYG, &
        get_Vd_POLYG, &
        push_vcooref, &
        add_stress_POLYG, &
        get_POLYG2BDYTY, &
        get_nb_vertices_POLYG, &
        get_vertices_POLYG, &
        get_nb_vertex_POLYG, &
        get_vertex_POLYG, &
        get_body_id_POLYG, &
        get_nb_scalarfields_POLYG,&
        get_nb_point_outlines_POLYG,&
        init_outlines_POLYG, &
        init_scalarfields_POLYG, &
        update_postdata_POLYG, &
        get_ptr_POLYG2BDYTY, &
        get_therm_cond_POLYG, &
        get_thermal_value_POLYG, &
        get_average_WS_POLYG, &
        get_electric_potentiel_POLYG, &
        get_elec_cond_POLYG, &
        get_betai_POLYG, &
        !
        get_ENT, &
        get_color, &
        get_vlocy, get_reac, add_reac, &
        get_Xbegin, get_X, &
        get_Vbegin, get_V, &
        get_cooref,get_coor, get_coorTT, get_shiftTT, &
        nullify_reac, nullify_vlocy, comp_vlocy, &
        get_visible, &
        !
        add_betai_to_POLYG, &
        clean_memory_POLYG, &
        !!> B.o.B.o.R
        set_Vd_POLYG, &
        set_Xd_POLYG, &
        get_Xd_POLYG, &
        update_radii_POLYG, &
        update_normals_ref_POLYG, &
        get_radii_POLYG, &
        get_WS_POLYG   , &
        get_tact_id


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
 !function get_tact_id(ibdyty,itacty,bdyty)

!------------------------------------------------------------------------
 subroutine read_bodies_POLYG
   implicit none
   integer(kind=4)                         :: ibdyty,itacty,errare
   integer(kind=4)                         :: nb_RBDY2, nb_MBS2D
   integer(kind=4),dimension(1)            :: idata
   integer(kind=4)                         :: itact,i
   real(kind=8),dimension(:),allocatable   :: data
   real(kind=8)                            :: inorme,d1,d2,radius

   character(len=18)                       :: IAM='POLYG::read_bodies'
   character(len=80)                       :: cout

   nb_POLYG=0

   nb_RBDY2=get_nb_RBDY2()

   allocate(l_bdyty_2_polyg(nb_RBDY2),stat=errare)

   do ibdyty=1,nb_RBDY2  
     ! > B.o.B.o.R
     l_bdyty_2_polyg(ibdyty)%polyTactyBegin=nb_POLYG
     do itacty=1,get_nb_tacty_RBDY2(ibdyty)
       if (get_tacID_RBDY2(ibdyty,itacty) == 'POLYG')  nb_POLYG=nb_POLYG+1
     end do 
     ! > B.o.B.o.R
     l_bdyty_2_polyg(ibdyty)%polyTactyEnd=nb_POLYG
   end do

   nb_MBS2D = get_nb_MBS2D()

   do ibdyty = 1, nb_MBS2D
     do itacty=1,get_nb_tacty_MBS2D(ibdyty)
       if (get_tacID_MBS2D(ibdyty,itacty) == i_polyg)  nb_POLYG=nb_POLYG+1
     end do
   end do

   write(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_POLYG,'POLYG found'
   call LOGMES(cout)

   if (nb_POLYG == 0) return
   if(allocated(polyg2bdyty)) deallocate(polyg2bdyty)
   allocate(polyg2bdyty(3,nb_POLYG),stat=errare)
   tactype2bdyty => polyg2bdyty

   if (errare /= 0) then
     call FATERR(IAM,'error allocating polyg2bdyty')
   end if

   nb_POLYG=0

   do ibdyty=1,nb_RBDY2   
     do itacty=1,get_nb_tacty_RBDY2(ibdyty)
       if (get_tacID_RBDY2(ibdyty,itacty) == 'POLYG') then
         nb_POLYG=nb_POLYG+1
         polyg2bdyty(1,nb_POLYG)=ibdyty  !   polyg2bdyty(1,itac) : serial number of body RBDY2 to which is attached the 
                                         !                         contactor POLYG numbered itac in the list of all 
                                         !                         contactors POLYG 
         polyg2bdyty(2,nb_POLYG)=itacty  !   polyg2bdyty(2,itac) : serial number of contactor POLYG itac in the list of 
                                         !                         contactors POLYG attached to a body (generically 1)
         polyg2bdyty(3,nb_POLYG)=i_rbdy2 !   polyg2bdyty(3,itac): type of body the contactor is attached to
       end if
     end do 
   end do

   do ibdyty = 1, nb_MBS2D
     do itacty = 1, get_nb_tacty_MBS2D(ibdyty)
       if (get_tacID_MBS2D(ibdyty,itacty) == i_polyg) then
         nb_POLYG=nb_POLYG+1
         polyg2bdyty(1,nb_POLYG) = ibdyty
         polyg2bdyty(2,nb_POLYG) = itacty
         polyg2bdyty(3,nb_POLYG) = i_mbs2
       end if
     end do
   end do

   ! routine de preparation des donnees locales aux polygones 
   ! et servant aussi a la generation d'un BODIES.DAT propre ...


   if (allocated(l_POLYG)) deallocate(l_POLYG)
   allocate(l_POLYG(nb_POLYG))

   do itact=1,nb_POLYG

     if( polyg2bdyty(3,itact) == i_rbdy2 ) then
       call get_idata(polyg2bdyty(1,itact),polyg2bdyty(2,itact),idata(:))
     else if( polyg2bdyty(3,itact) == i_mbs2 ) then
       idata(:) = get_ptr_idata_MBS2D(polyg2bdyty(1,itact),polyg2bdyty(2,itact))
     end if

     if (allocated(data)) deallocate(data)
     allocate(data( 2*idata(1) ))

     if( polyg2bdyty(3,itact) == i_rbdy2 ) then
       call get_data(polyg2bdyty(1,itact),polyg2bdyty(2,itact),data)
     else if( polyg2bdyty(3,itact) == i_mbs2 ) then
       data(:) = get_ptr_rdata_MBS2D(polyg2bdyty(1,itact),polyg2bdyty(2,itact))
     end if
      
     allocate(l_POLYG(itact)%vertex(2,idata(1)))        
     allocate(l_POLYG(itact)%vertex_ref(2,idata(1)))
     allocate(l_POLYG(itact)%normale(2,idata(1)))
     allocate(l_POLYG(itact)%normale_ref(2,idata(1)))
     allocate(l_POLYG(itact)%Xd(idata(1),2))
     allocate(l_POLYG(itact)%Vd(idata(1),2))

     l_POLYG(itact)%nb_vertex=idata(1)
     l_POLYG(itact)%nb_vertex=idata(1)

     l_POLYG(itact)%vertex_ref  = reshape(source=data,shape=(/ 2, idata(1) /))

     l_POLYG(itact)%Xd         = 0.d0
     l_POLYG(itact)%Vd         = 0.d0

     l_POLYG(itact)%vertex      = 0.D0
      
     l_POLYG(itact)%normale     = 0.D0
     l_POLYG(itact)%normale_ref = 0.D0
 
     ! construction des normales
     do i=1,idata(1)-1
       d1=l_POLYG(itact)%vertex_ref(1,i)-l_POLYG(itact)%vertex_ref(1,i+1)
       d2=l_POLYG(itact)%vertex_ref(2,i)-l_POLYG(itact)%vertex_ref(2,i+1)
       inorme=1.d0/sqrt((d1*d1)+(d2*d2))
       l_POLYG(itact)%normale_ref(1,i)=-d2*inorme
       l_POLYG(itact)%normale_ref(2,i)= d1*inorme
     end do
    
     d1=l_POLYG(itact)%vertex_ref(1,idata(1))-l_POLYG(itact)%vertex_ref(1,1)
     d2=l_POLYG(itact)%vertex_ref(2,idata(1))-l_POLYG(itact)%vertex_ref(2,1)
     inorme=1.d0/sqrt((d1*d1)+(d2*d2))
     l_POLYG(itact)%normale_ref(1,idata(1))=-d2*inorme
     l_POLYG(itact)%normale_ref(2,idata(1))= d1*inorme
    
     ! détermination du rayon d'encombrement
     d1=0.D0;d2=0.D0
     do i=1,idata(1)
       d1=d1+l_POLYG(itact)%vertex_ref(1,i)
       d2=d2+l_POLYG(itact)%vertex_ref(2,i)
     end do
     d1=d1/real(idata(1),8)
     d2=d2/real(idata(1),8) 
     radius=0.D0
     do i=1,idata(1)
       radius=max(radius,sqrt((d1-l_POLYG(itact)%vertex_ref(1,i))**2+(d2-l_POLYG(itact)%vertex_ref(2,i))**2))
     end do
     l_POLYG(itact)%outer_radius=radius
     l_POLYG(itact)%inner_radius = compute_inner_radius_POLYG(itact)
   enddo

  ! détermination du rayon max, min et moyen de l'ensemble des polygones   

   min_radius  = 1.D20
   max_radius  = 0.D0
   mean_radius = 0.D0

   do itact=1,nb_POLYG

     min_radius  = min(l_POLYG(itact)%inner_radius,min_radius)
     max_radius  = max(l_POLYG(itact)%outer_radius,max_radius)
     mean_radius = mean_radius + l_POLYG(itact)%outer_radius

   enddo

   mean_radius = mean_radius/real(nb_POLYG,8)

 end subroutine read_bodies_POLYG
!
!------------------------------------------------------------------------
 function get_nb_POLYG(fantome)
   implicit none
   integer(kind=4),optional :: fantome
   integer(kind=4)          :: get_nb_POLYG
  
   get_nb_POLYG = nb_POLYG
  
 end function get_nb_POLYG
!-----------------------------------------------------------------------
subroutine update_normals_ref_POLYG(fantome)
 implicit none
 integer(kind=4),optional :: fantome
 integer(kind=4)          :: i,nb_vertex,itact
 real(kind=8)             :: inorme,d1,d2
 real(kind=8),dimension(2):: X1,X2
do itact=1,nb_POLYG
 nb_vertex = l_POLYG(itact)%nb_vertex
 ! construction des normales
 do i=1,nb_vertex-1
       !!call get_Xth_POLYG(itact,i,X1(1:2))
       !!call get_Xth_POLYG(itact,i+1,X2(1:2))
       !!d1=l_POLYG(itact)%vertex_ref(1,i)+X1(1)-l_POLYG(itact)%vertex_ref(1,i+1)-X2(1)
       !!d2=l_POLYG(itact)%vertex_ref(2,i)+X1(2)-l_POLYG(itact)%vertex_ref(2,i+1)-X2(2)
       d1=l_POLYG(itact)%vertex_ref(1,i)-l_POLYG(itact)%vertex_ref(1,i+1)
       d2=l_POLYG(itact)%vertex_ref(2,i)-l_POLYG(itact)%vertex_ref(2,i+1)
       inorme=1.d0/sqrt((d1*d1)+(d2*d2))
       l_POLYG(itact)%normale_ref(1,i)=-d2*inorme
       l_POLYG(itact)%normale_ref(2,i)= d1*inorme
 end do
 ! call get_Xth_POLYG(itact,nb_vertex,X1(1:2))
 ! call get_Xth_POLYG(itact,1,X2(1:2)) 
 ! d1=l_POLYG(itact)%vertex_ref(1,nb_vertex)+X1(1)-l_POLYG(itact)%vertex_ref(1,1)-X2(1)
 ! d2=l_POLYG(itact)%vertex_ref(2,nb_vertex)+X1(2)-l_POLYG(itact)%vertex_ref(2,1)-X2(2)
 d1=l_POLYG(itact)%vertex_ref(1,nb_vertex)-l_POLYG(itact)%vertex_ref(1,1)
 d2=l_POLYG(itact)%vertex_ref(2,nb_vertex)-l_POLYG(itact)%vertex_ref(2,1)
 inorme=1.d0/sqrt((d1*d1)+(d2*d2))
 l_POLYG(itact)%normale_ref(1,nb_vertex)=-d2*inorme
 l_POLYG(itact)%normale_ref(2,nb_vertex)= d1*inorme
end do
end subroutine update_normals_ref_POLYG
!----------------------------------------------------------------------
subroutine update_radii_POLYG(fantome)
 implicit none
 integer(kind=4),optional :: fantome
 integer(kind=4)          :: itact,i,nb_vertex
 real(kind=8)             :: d1,d2,radius

  
   do itact=1,nb_POLYG
     ! > B.o.B.o.R
     nb_vertex = l_POLYG(itact)%nb_vertex
     ! détermination du rayon d'encombrement
     d1=0.D0;d2=0.D0
     do i=1,nb_vertex
       d1=d1+l_POLYG(itact)%vertex(1,i)
       d2=d2+l_POLYG(itact)%vertex(2,i)
     end do
     d1=d1/real(nb_vertex,8)
     d2=d2/real(nb_vertex,8) 
     radius=0.D0
     do i=1,nb_vertex
       radius=max(radius,sqrt((d1-l_POLYG(itact)%vertex(1,i))**2+(d2-l_POLYG(itact)%vertex(2,i))**2))
     end do
     l_POLYG(itact)%outer_radius=radius
     l_POLYG(itact)%inner_radius = compute_inner_radius_POLYG(itact)
   enddo
   min_radius  = 1.D20
   max_radius  =-1.D20
   mean_radius = 0.D0
   do itact=1,nb_POLYG
     min_radius  = min(l_POLYG(itact)%inner_radius,min_radius)
     max_radius  = max(l_POLYG(itact)%outer_radius,max_radius)
     mean_radius = mean_radius + l_POLYG(itact)%outer_radius
   enddo
   mean_radius = mean_radius/real(nb_POLYG,8)

end subroutine update_radii_POLYG
!------------------------------------------------------------------------ 
 function get_mean_radius_POLYG(fantome)
   implicit none   
   integer               :: itact
   real(kind=8),optional :: fantome
   real(kind=8)          :: get_mean_radius_POLYG
   
   get_mean_radius_POLYG=mean_radius

 end function get_mean_radius_POLYG
!------------------------------------------------------------------------
 function get_min_radius_POLYG(fantome)
   implicit none   
   integer               :: itact
   real(kind=8),optional :: fantome
   real(kind=8)          :: get_min_radius_POLYG
  

   get_min_radius_POLYG=min_radius

 end function get_min_radius_POLYG
!------------------------------------------------------------------------ 
 function get_max_radius_POLYG(fantome)
   implicit none   
   integer               :: itact
   real(kind=8),optional :: fantome
   real(kind=8)          :: get_max_radius_POLYG
 
   get_max_radius_POLYG=max_radius

 end function get_max_radius_POLYG
!------------------------------------------------------------------------ 
 function get_l_POLYG(itact)
   implicit none
   integer(kind=4)       :: itact 
   type(T_POLYG)         :: get_l_POLYG
   
   get_l_POLYG = l_POLYG(itact)
 
 end function get_l_POLYG
!------------------------------------------------------------------------ 
 function get_radius_POLYG(itact)
   implicit none
   integer(kind=4)           :: itact,i
   integer(kind=4)           :: nb_vertex
   real(kind=8)              :: get_radius_POLYG,d1,d2,radius

     get_radius_POLYG=l_POLYG(itact)%outer_radius
 
 end function get_radius_POLYG  
!------------------------------------------------------------------------
 subroutine move_BDARY_POLYG(itacty,X)
   use algebra, only : length2  
   implicit none
   integer(kind=4)             :: itacty,i,ibdyty
   real(kind=8)                :: a,Tx,Ty  ! a: rotation angle, Tx,Ty: translation
   real(kind=8)                :: c,s      ! cos(a) and sin(a) values
   real(kind=8),dimension(3)   :: X
   real(kind=8),dimension(2)   :: Xd       ! dilation
   logical                     :: does_shape_change

   does_shape_change = .FALSE. 
   
   ibdyty=polyg2bdyty(1,itacty)

   Tx= X(1)
   Ty= X(2)
   a = X(3)

   c=cos(a);s=sin(a)

   do i=1,l_POLYG(itacty)%nb_vertex

     call get_Xd_POLYG(itacty,i,Xd(1:2))
     if (length2(Xd) > 0.d0) does_shape_change = .TRUE. 

     l_POLYG(itacty)%vertex(1,i)= c*(l_POLYG(itacty)%vertex_ref(1,i)+Xd(1)) &
                                 -s*(l_POLYG(itacty)%vertex_ref(2,i)+Xd(2)) &
                                 +Tx

     l_POLYG(itacty)%vertex(2,i)= s*(l_POLYG(itacty)%vertex_ref(1,i)+Xd(1)) &
                                 +c*(l_POLYG(itacty)%vertex_ref(2,i)+Xd(2)) &
                                 +Ty

     l_POLYG(itacty)%normale(1,i)= c*l_POLYG(itacty)%normale_ref(1,i) &
                                  -s*l_POLYG(itacty)%normale_ref(2,i) 
      
     l_POLYG(itacty)%normale(2,i)= s*l_POLYG(itacty)%normale_ref(1,i) &
                                  +c*l_POLYG(itacty)%normale_ref(2,i)

   enddo

   if ( does_shape_change) then
     call update_radii_POLYG()
     call update_normals_ref_POLYG()
   endif   
   
 end subroutine move_BDARY_POLYG
!------------------------------------------------------------------------
function is_POLYG_same_BDYTY(itact1,itact2)
   implicit none
   integer(kind=4) :: itact1,itact2 
   logical :: is_POLYG_same_BDYTY
   
   is_POLYG_same_BDYTY=.false.
   if(polyg2bdyty(1,itact1) == polyg2bdyty(1,itact2) .and. &
      polyg2bdyty(3,itact1) == polyg2bdyty(3,itact2) ) is_POLYG_same_BDYTY=.true.
   
   ! for MBS2D, need to check if the node on the mbs is the same
   if (polyg2bdyty(3,itact1) == i_mbs2 .and. &
       polyg2bdyty(3,itact2) == i_mbs2) then
      if (get_nodeID_MBS2D(polyg2bdyty(1,itact1), polyg2bdyty(2,itact1)) /= &
          get_nodeID_MBS2D(polyg2bdyty(1,itact2), polyg2bdyty(2,itact2))) is_POLYG_same_BDYTY=.false.
   endif     
 end function is_POLYG_same_BDYTY

 !------------------------------------------------------------------------
 !> push vertex coordinates to RBDY2 in order ton compute dilatation for example
 subroutine push_vcooref
   implicit none

   integer(kind=4)                                :: itacty,i,nbv
   real(kind=8),dimension(:,:),allocatable  :: X
   real(kind=8),dimension(3) :: Xc 

   do itacty=1,nb_POLYG

     allocate(X(2,l_POLYG(itacty)%nb_vertex))

     Xc = get_cooref(itacty)

     do i=1,l_POLYG(itacty)%nb_vertex
       X(1:2,i) = Xc(1:2) + l_POLYG(itacty)%vertex_ref(1:2,i) 
     enddo

     nbv = l_POLYG(itacty)%nb_vertex

     call set_vcooref_RBDY2(polyg2bdyty(1,itacty),nbv,X)

     deallocate(X)

   enddo

 end subroutine push_vcooref

 !------------------------------------------------------------------------
 ! subroutine get_POLYG2RBDY2(polyg2rbdy2_, nb_poly_)

 !        integer(kind=4), intent(in) :: nb_poly_ ! nombre de disques attentu par celui qui appelle cette
 !                                        ! fonction, i.e. la taille de diskx2rbdy2_	
 !        integer(kind=4), dimension(nb_poly_), intent(out) :: polyg2rbdy2_ ! vecteur pour récupérer la table de
 !                                                                  ! correpondance

 !        ! on vérifie que la taille de polug2rbdy2_ est consistante avec le nombre de disques
 !        if (nb_poly_ .ne. nb_POLYG) then

 !           ! si ce n'est pas le cas, c'est l'erreur fatale!
 !           call FATERR('mod_POLYG::get_POLYG2RBDY2', 'Error: unconsistant number of polyg!')

 !        end if

 !        ! on recupere la table de corespondance
 !        polyg2rbdy2_(:) = polyg2bdyty(1, :)

 ! end subroutine get_POLYG2RBDY2

 subroutine get_POLYG2BDYTY(map, size1, size2)
   implicit none
   integer(kind=4), dimension(:,:), pointer :: map
   integer(kind=4), intent(out)             :: size1, size2
   !
   if(associated(map) ) nullify(map)

   size1 = 3
   size2 = nb_POLYG

   if( nb_POLYG > 0 ) then

     allocate(map(size1,size2))

     map(1:size1,1:size2) = POLYG2bdyty(1:size1,1:size2)

   end if

 end subroutine

 !------------------------------------------------------------------------
 function get_ptr_POLYG2BDYTY()
   implicit none
   integer(kind=4),dimension(:,:),pointer :: get_ptr_POLYG2BDYTY

   get_ptr_POLYG2BDYTY => polyg2bdyty

 end function get_ptr_POLYG2BDYTY

 !------------------------------------------------------------------------
 function get_body_id_POLYG(itacty)
  implicit none
  integer, intent(in) :: itacty
  integer :: get_body_id_POLYG
  get_body_id_POLYG = polyg2bdyty(1,itacty)
 end function get_body_id_POLYG

 !---------------------------------------------------

 ! rm & vt : super grouille des supers couplages...
 ! pour peligriff on utilise le numero de contacteur
 ! pour recuperer des infos sur les vertex d'un polyg
 ! pour siconos on utilise le numero de corps
 ! pour recuperer des infos sur les vertices d'un polyg
 ! faudra trancher un jour...

 function get_nb_vertex_POLYG(itacty)
   implicit none
   integer(kind=4), intent(in) :: itacty
   integer(kind=4) :: get_nb_vertex_POLYG
 
   get_nb_vertex_POLYG = l_POLYG(itacty)%nb_vertex
 end function

 !------------------------------------------------------------------------
 subroutine get_vertex_POLYG(itacty, vertex)
   implicit none

   integer(kind=4), intent(in) :: itacty
   real(kind=8),dimension(2,l_POLYG(itacty)%nb_vertex) :: vertex
   !
   integer(kind=4) :: i
   real(kind=8), dimension(3) :: X
 
   X=get_coor(itacty)
   call move_BDARY_POLYG(itacty,X)

   do i=1,l_POLYG(itacty)%nb_vertex
      vertex(:,i)=l_POLYG(itacty)%vertex(:,i)
   enddo
 end subroutine get_vertex_POLYG
 
 !------------------------------------------------------------------------
 function get_nb_vertices_POLYG(ibdyty)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   integer(kind=4) :: get_nb_vertices_POLYG
   !
   integer(kind=4) :: itacty
   
   get_nb_vertices_POLYG = 0
   do itacty = 1, nb_POLYG
     if( polyg2bdyty(1,itacty) == ibdyty ) then
       get_nb_vertices_POLYG = l_POLYG(itacty)%nb_vertex
       return
     end if
   end do

 end function

 !------------------------------------------------------------------------
 subroutine get_vertices_POLYG(ibdyty, coor)
   implicit none
   integer(kind=4), intent(in) :: ibdyty
   real(kind=8), dimension(:,:) :: coor
   !
   real(kind=8), dimension(3) :: X
   integer(kind=4) :: itacty

   coor(:,:) = 0.D0
   do itacty = 1, nb_POLYG
     if( polyg2bdyty(1,itacty) == ibdyty ) then
       if( any(shape(coor) /= (/ 2,l_POLYG(itacty)%nb_vertex /)) ) then
         call FATERR('get_vertices_coor_POLYG', 'Error: output array wrong size!')
       end if
       X=get_coor(itacty)
       call move_BDARY_POLYG(itacty,X)
       coor = l_POLYG(itacty)%vertex
       return
     end if
   end do

 end subroutine

 !------------------------------------------------------------------------ 
 function init_outlines_POLYG()
   implicit none
   integer(kind=4) :: itacty,sz 
   real(kind=8), dimension(:,:),pointer :: init_outlines_POLYG

   if ( nb_POLYG .eq. 0 ) then
      init_outlines_POLYG => null()
      return
   end if
   
   if (associated(nb_point_outlines_POLYG)) deallocate(nb_point_outlines_POLYG)
   allocate(nb_point_outlines_POLYG(nb_POLYG+1)) 
   nb_point_outlines_POLYG(1) = 0
   do itacty = 1, nb_POLYG
     nb_point_outlines_POLYG(itacty+1) =  &
          nb_point_outlines_POLYG(itacty) + l_POLYG(itacty)%nb_vertex
   end do

   sz =  nb_point_outlines_POLYG(nb_POLYG+1)

   if (associated(outlines_POLYG)) deallocate(outlines_POLYG)
   allocate(outlines_POLYG(2,sz))

   outlines_POLYG(1:2,1:sz) = 0.D0

   init_outlines_POLYG => outlines_POLYG

 end function init_outlines_POLYG
 
 !------------------------------------------------------------------------
 function get_nb_point_outlines_POLYG()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_POLYG

   get_nb_point_outlines_POLYG => nb_point_outlines_POLYG

 end function get_nb_point_outlines_POLYG

 ! !------------------------------------------------------------------------
 ! integer function get_nb_point_outline_POLYG(itacty)
 !   implicit none
 !   integer(kind=4) :: itacty

 !   get_nb_point_outline_POLYG = l_POLYG(itacty)%nb_vertex+1

 ! end function get_nb_point_outline_POLYG

 !------------------------------------------------------------------------ 
 subroutine updt_outline_POLYG(itacty,outline)
   implicit none
   integer(kind=4) :: itacty,k
   real(kind=8),dimension(3)         :: X
   real(kind=8),dimension(2,0:l_POLYG(itacty)%nb_vertex+1) :: outline

   X=get_coor(itacty)

   call move_BDARY_POLYG(itacty,X)

   outline(1,0) = X(1)
   outline(2,0) = X(2)

   do k=1,l_POLYG(itacty)%nb_vertex
     outline(1,k)= l_POLYG(itacty)%vertex(1,k)
     outline(2,k)= l_POLYG(itacty)%vertex(2,k)
   enddo
   
   outline(1,l_POLYG(itacty)%nb_vertex+1) = outline(1,1)
   outline(2,l_POLYG(itacty)%nb_vertex+1) = outline(2,1)
 end subroutine updt_outline_POLYG

 !------------------------------------------------------------------------ 
 function init_scalarfields_POLYG()
   implicit none
   integer(kind=4) :: sz
   real(kind=8), dimension(:),pointer :: init_scalarfields_POLYG

   !default value: nbsf = 9

   if ( nb_POLYG .eq. 0 ) then
      init_scalarfields_POLYG => null()
      return
   end if
   
   sz = nbsf * nb_POLYG

   if (associated(scalarfields_POLYG)) deallocate(scalarfields_POLYG)
   allocate(scalarfields_POLYG(sz))

   scalarfields_POLYG(1:sz) = 0.D0

   init_scalarfields_POLYG => scalarfields_POLYG

 end function init_scalarfields_POLYG
 
 !------------------------------------------------------------------------
 function get_nb_scalarfields_POLYG()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_POLYG

   get_nb_scalarfields_POLYG = nbsf

 end function get_nb_scalarfields_POLYG

 !------------------------------------------------------------------------
 subroutine updt_scalarfield_POLYG(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty,id_rbdy2,id_tacty
   real(kind=8),dimension(nbsf) :: scalarfield

   id_rbdy2 = polyg2bdyty(1,itacty)
   id_tacty = polyg2bdyty(2,itacty)

   !mr : displacement and velocity of the center of mass
   scalarfield(1:3) = get_X(itacty)
   scalarfield(4:6) = get_V(itacty)
   scalarfield(7:9) = get_REAC(itacty)
 end subroutine updt_scalarfield_POLYG

 !------------------------------------------------------------------------ 
 subroutine update_postdata_POLYG()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf,szo,nbpto

   if ( nb_POLYG == 0 ) return
   if (.not. associated(outlines_POLYG)) call faterr('POLYG::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_POLYG)) call faterr('POLYG::update_postdata','init_scalarfields is mandatory')
   
   iszo  = 0
   iszsf = 0
   do itacty=1,nb_POLYG
      nbpto = l_POLYG(itacty)%nb_vertex
      call get_vertex_POLYG(itacty,outlines_POLYG(1:2,iszo+1:iszo+nbpto))
      iszo = iszo + nbpto
      call updt_scalarfield_POLYG(itacty,scalarfields_POLYG(iszsf+1:iszsf+nbsf))
      iszsf = iszsf + nbsf
   end do

 end subroutine update_postdata_POLYG


!------------------------------------------------------------------------ 
!!$ subroutine change_polyg2mailx
!!$   implicit none
!!$   integer(kind=4)             :: itacty,i
!!$   real(kind=8)                :: a,Tx,Ty                !a: angle de rotation, Tx,Ty: translation
!!$   real(kind=8)                :: c,s                    !cos(a) et sin(a) pr les calculer qu'une seule fois
!!$   real(kind=8),dimension(3)   :: X
!!$
!!$   integer(kind=4) :: nfich,imailx,it3xxx,inod,itac
!!$   real(kind=8) :: dir
!!$   nfich=99
!!$   open(unit=nfich,FILE='pipo_fred.dat',STATUS='REPLACE')
!!$
!!$   imailx = 0
!!$
!!$   do itacty=1,nb_POLYG
!!$
!!$     imailx=imailx+1
!!$
!!$     X=get_coor_POLYG(polyg2bdyty(1,itacty),polyg2bdyty(2,itacty))
!!$
!!$     Tx= X(1)
!!$     Ty= X(2)
!!$     a = X(3)
!!$
!!$     c=cos(a);s=sin(a)
!!$
!!$     do i=1,l_POLYG(itacty)%nb_vertex
!!$  
!!$       l_POLYG(itacty)%vertex(1,i)= c*l_POLYG(itacty)%vertex_ref(1,i) &
!!$                                   -s*l_POLYG(itacty)%vertex_ref(2,i) &
!!$                                   +Tx
!!$       l_POLYG(itacty)%vertex(2,i)= s*l_POLYG(itacty)%vertex_ref(1,i) &
!!$                                   +c*l_POLYG(itacty)%vertex_ref(2,i) &
!!$                                   +Ty
!!$       l_POLYG(itacty)%normale(1,i)= c*l_POLYG(itacty)%normale_ref(1,i) &
!!$                                    -s*l_POLYG(itacty)%normale_ref(2,i) 
!!$      
!!$       l_POLYG(itacty)%normale(2,i)= s*l_POLYG(itacty)%normale_ref(1,i) &
!!$                                    +c*l_POLYG(itacty)%normale_ref(2,i)
!!$     enddo
!!$
!!$     write(nfich,'(A72)') '$bdyty                                                                  '
!!$     write(nfich,10)'MAILx',imailx
!!$                      !123456789012345678901234567890123456789012345678901234567890123456789012
!!$     write(nfich,'(A72)') '$blmty                                                                  '
!!$     do it3xxx=1,l_POLYG(itacty)%nb_vertex-1
!!$       write(nfich,20) it3xxx,1,it3xxx+1,it3xxx+2
!!$       write(nfich,30) 'M2D_L','stone'
!!$     enddo
!!$     write(nfich,20) l_POLYG(itacty)%nb_vertex,1,l_POLYG(itacty)%nb_vertex+1,2
!!$     write(nfich,30) 'M2D_L','stone'
!!$
!!$     write(nfich,'(A72)') '$nodty                                                                  '
!!$     call write_a_nodty('NO2xx',1,X,'coo',nfich)
!!$
!!$     do inod=1,l_POLYG(itacty)%nb_vertex
!!$       X(1:2)=l_POLYG(itacty)%vertex(1:2,inod)
!!$       call write_a_nodty('NO2xx',inod+1,X,'coo',nfich)
!!$     end do
!!$
!!$     write(nfich,'(A72)') '$tacty                                                                  '
!!$
!!$     itac=0
!!$     do i=1,l_POLYG(itacty)%nb_vertex-1
!!$       itac=itac+1
!!$       dir= l_POLYG(itacty)%normale(1,i)*dsqrt(2.d0) + l_POLYG(itacty)%normale(2,i)*dsqrt(2.d0)
!!$       if (dir < 0.d0) then
!!$         write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',i+2,'nodb=',i+1,'apab=',0.2
!!$         itac=itac+1
!!$         write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',i+2,'nodb=',i+1,'apab=',0.8 
!!$       else 
!!$         write(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',i+2,'nodb=',i+1 
!!$       endif
!!$    enddo
!!$    dir= l_POLYG(itacty)%normale(1,l_POLYG(itacty)%nb_vertex)*dsqrt(2.d0) + &
!!$         l_POLYG(itacty)%normale(2,l_POLYG(itacty)%nb_vertex)*dsqrt(2.d0)
!!$    itac=itac+1
!!$    if (dir < 0.d0) then
!!$      write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',l_POLYG(itacty)%nb_vertex+1,'apab=',0.2 
!!$      itac=itac+1
!!$      write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',l_POLYG(itacty)%nb_vertex+1,'apab=',0.8
!!$    else
!!$      write(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',2,'nodb=',l_POLYG(itacty)%nb_vertex+1
!!$    endif
!!$
!!$     write(nfich,'(A72)') '$$$$$$                                                                  '
!!$   enddo
!!$
!!$   close(nfich)
!!$
!!$10 format(1X,A5,2X,I5)            
!!$20 format(' T3xxx',2x,I5,2x,'nodes',8(2x,I5))
!!$30 format(15x,'model',2x,A5,2x,'behav',2x,A5)
!!$40 format(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,D14.7)
!!$50 format(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)
!!$
!!$ end subroutine change_polyg2mailx
!!$!------------------------------------------------------------------------ 
!!$!------------------------------------------------------------------------ 
!!$ subroutine change_polyg2mailx_fine
!!$
!!$   implicit none
!!$
!!$   integer(kind=4)                     :: itacty,i
!!$   real(kind=8)                :: a,Tx,Ty                !a: angle de rotation, Tx,Ty: translation
!!$   real(kind=8)                :: c,s                    !cos(a) et sin(a) pr les calculer qu'une seule fois
!!$   real(kind=8),dimension(3)   :: X
!!$
!!$   integer(kind=4) :: nfich,imailx,it3xxx,inode,itac
!!$   real(kind=8) :: dir
!!$   nfich=99
!!$   open(unit=nfich,FILE='pipo_fred.dat',STATUS='REPLACE')
!!$
!!$   imailx = 0
!!$
!!$   do itacty=1,nb_POLYG
!!$
!!$     imailx=imailx+1
!!$
!!$     X=get_coor_POLYG(polyg2bdyty(1,itacty),polyg2bdyty(2,itacty))
!!$
!!$     Tx= X(1)
!!$     Ty= X(2)
!!$     a = X(3)
!!$
!!$     c=cos(a);s=sin(a)
!!$
!!$     do i=1,l_POLYG(itacty)%nb_vertex
!!$  
!!$       l_POLYG(itacty)%vertex(1,i)= c*l_POLYG(itacty)%vertex_ref(1,i) &
!!$                                   -s*l_POLYG(itacty)%vertex_ref(2,i) &
!!$                                   +Tx
!!$       l_POLYG(itacty)%vertex(2,i)= s*l_POLYG(itacty)%vertex_ref(1,i) &
!!$                                   +c*l_POLYG(itacty)%vertex_ref(2,i) &
!!$                                   +Ty
!!$       l_POLYG(itacty)%normale(1,i)= c*l_POLYG(itacty)%normale_ref(1,i) &
!!$                                    -s*l_POLYG(itacty)%normale_ref(2,i) 
!!$      
!!$       l_POLYG(itacty)%normale(2,i)= s*l_POLYG(itacty)%normale_ref(1,i) &
!!$                                    +c*l_POLYG(itacty)%normale_ref(2,i)
!!$     enddo
!!$
!!$     write(nfich,'(A72)') '$bdyty                                                                  '
!!$     write(nfich,10)'MAILx',imailx
!!$                      !123456789012345678901234567890123456789012345678901234567890123456789012
!!$     write(nfich,'(A72)') '$blmty                                                                  '
!!$
!!$     inode=1
!!$     it3xxx=0
!!$     do i=1,l_POLYG(itacty)%nb_vertex-1
!!$       it3xxx=it3xxx+1
!!$       inode=inode+1
!!$       write(nfich,20) it3xxx,1,inode,inode+1
!!$       write(nfich,30) 'M2D_L','stone'
!!$       it3xxx=it3xxx+1
!!$       inode=inode+1
!!$       write(nfich,20) it3xxx,1,inode,inode+1
!!$       write(nfich,30) 'M2D_L','stone'
!!$     enddo
!!$     it3xxx=it3xxx+1
!!$     inode=inode+1
!!$     write(nfich,20) it3xxx,1,inode,inode+1
!!$     write(nfich,30) 'M2D_L','stone'
!!$     it3xxx=it3xxx+1
!!$     inode=inode+1
!!$     write(nfich,20) it3xxx,1,inode,2
!!$     write(nfich,30) 'M2D_L','stone'
!!$
!!$
!!$     inode=1
!!$     write(nfich,'(A72)') '$nodty                                                                  '
!!$     call write_a_nodty('NO2xx',inode,X,'coo',nfich)
!!$
!!$     do i=1,l_POLYG(itacty)%nb_vertex-1
!!$       inode=inode+1
!!$       X(1:2)=l_POLYG(itacty)%vertex(1:2,i)
!!$       call write_a_nodty('NO2xx',inode,X,'coo',nfich)
!!$       inode=inode+1
!!$       X(1:2)=(l_POLYG(itacty)%vertex(1:2,i)+l_POLYG(itacty)%vertex(1:2,i+1))/2
!!$       call write_a_nodty('NO2xx',inode,X,'coo',nfich)
!!$     end do
!!$     inode=inode+1
!!$     X(1:2)=l_POLYG(itacty)%vertex(1:2,l_POLYG(itacty)%nb_vertex)
!!$     call write_a_nodty('NO2xx',inode,X,'coo',nfich)
!!$     inode=inode+1
!!$     X(1:2)=(l_POLYG(itacty)%vertex(1:2,l_POLYG(itacty)%nb_vertex)+l_POLYG(itacty)%vertex(1:2,1))/2
!!$     call write_a_nodty('NO2xx',inode,X,'coo',nfich)
!!$
!!$
!!$     write(nfich,'(A72)') '$tacty                                                                  '
!!$     itac=0
!!$     inode=1
!!$     do i=1,l_POLYG(itacty)%nb_vertex-1
!!$       itac=itac+1
!!$       inode=inode+1
!!$       dir= l_POLYG(itacty)%normale(1,i)*dsqrt(2.d0) + l_POLYG(itacty)%normale(2,i)*dsqrt(2.d0)
!!$       if (dir < 0.d0) then
!!$         write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
!!$         itac=itac+1
!!$         write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
!!$
!!$         inode=inode+1
!!$         itac=itac+1
!!$         write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
!!$         itac=itac+1
!!$         write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
!!$       else 
!!$         write(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode 
!!$         itac=itac+1
!!$         inode=inode+1
!!$         write(nfich,40) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode
!!$       endif
!!$    enddo
!!$    dir= l_POLYG(itacty)%normale(1,l_POLYG(itacty)%nb_vertex)*dsqrt(2.d0) + &
!!$         l_POLYG(itacty)%normale(2,l_POLYG(itacty)%nb_vertex)*dsqrt(2.d0)
!!$    itac=itac+1
!!$    inode=inode+1
!!$    if (dir < 0.d0) then
!!$      write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
!!$      itac=itac+1
!!$      write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
!!$
!!$      inode=inode+1
!!$      itac=itac+1
!!$      write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.25
!!$      itac=itac+1
!!$      write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.75
!!$    else
!!$      write(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode 
!!$      itac=itac+1
!!$      inode=inode+1
!!$      write(nfich,40) 'ALpxx',itac,'color','REDxx','noda=',2,'nodb=',inode
!!$
!!$    endif
!!$
!!$     write(nfich,'(A72)') '$$$$$$                                                                  '
!!$   enddo
!!$
!!$   close(nfich)
!!$
!!$10 format(1X,A5,2X,I5)            
!!$20 format(' T3xxx',2x,I5,2x,'nodes',8(2x,I5))
!!$30 format(15x,'model',2x,A5,2x,'behav',2x,A5)
!!$40 format(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,D14.7)
!!$50 format(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)
!!$
!!$ end subroutine change_polyg2mailx_fine

 integer function has_cooref_in_tacty(cooref,itacty)
   implicit none
   integer::itacty,i
   integer::nbvertices
   real(kind=8)::aligne
   real(kind=8),dimension(2)::cooref

   has_cooref_in_tacty=-1
   nbvertices = l_POLYG(itacty)%nb_vertex
   do i=1,nbvertices
     aligne=orientation(l_POLYG(itacty)%vertex_ref(:,i),l_POLYG(itacty)%vertex_ref(:,modulo(i,nbvertices)+1),cooref)
     if(abs(aligne).le.1.D-10)then
        has_cooref_in_tacty = i
     endif
   enddo

 end function has_cooref_in_tacty

 real(kind=8)function distance(p1,p2)
   implicit none
   real(kind=8),dimension(2)::p1,p2

   distance = dsqrt((p2(1)-p1(1))**2.+(p2(2)-p1(2))**2.)

 end function distance

 real(kind=8)function orientation(p1,p2,p3)
   implicit none

   real(kind=8),dimension(2)::p1,p2,p3
   real(kind=8)             :: dx1,dx2,dy1,dy2

   dx1 = p3(1)  - p1(1)
   dx2 = p3(1)  - p2(1)
   dy1 = p3(2)  - p1(2)
   dy2 = p3(2)  - p2(2)
   orientation = dx1*dy2-dy1*dx2
 end function orientation

 subroutine get_Vd_POLYG(ibdyty,Pcooref,V)
   implicit none
   integer::ibdyty,itact
   integer::index,nbvertices
   real(kind=8) dist1,dist2,L,c,s
   real(kind=8),dimension(2):: V,Vref,Pcooref,V1,V2,X1,X2
   real(kind=8),dimension(3):: X
                                   !12345678901234
   character(len=14)        :: IAM='POLYG::get_Vth'
   
   V    = 0.
   Vref = 0.


   do itact=l_bdyty_2_polyg(ibdyty)%polyTactyBegin+1,l_bdyty_2_polyg(ibdyty)%polyTactyEnd

    index = has_cooref_in_tacty(Pcooref,itact)

    if (index.gt.-1 ) then

      nbvertices = l_POLYG(itact)%nb_vertex
      X1 = l_POLYG(itact)%vertex_ref(:,index)
      X2 = l_POLYG(itact)%vertex_ref(:,modulo(index,nbvertices)+1)

      call get_vertex_Vd_POLYG(itact,index  ,V1)
      call get_vertex_Vd_POLYG(itact,modulo(index,nbvertices)+1,V2)
   
      dist1 = distance(Pcooref,X1)
      dist2 = distance(Pcooref,X2)
      L     = distance(X1,X2)

      if (L < tiny(1.d0)) then
        call faterr(IAM,'superimposed vertices')
      endif
         
      Vref  = V1*dist2/L+V2*dist1/L
   
      X=get_coor(itact)
    
      c=cos(X(3));s=sin(X(3))

      V(1)= c*Vref(1) - s*Vref(2) 
      V(2)= s*Vref(1) + c*Vref(2)

    endif
   
   enddo

 end subroutine get_Vd_POLYG

 subroutine get_vertex_Vd_POLYG(itact,ivertex,V)
   implicit none
   integer::itact,ivertex
   real(kind=8),dimension(2):: V


    V = l_POLYG(itact)%Vd(ivertex,1:2)

 end subroutine get_vertex_Vd_POLYG

 subroutine get_Xd_POLYG(itacty,ivertex,V)
   implicit none
   integer::itacty,ivertex
   real(kind=8),dimension(2):: V

   V = l_POLYG(itacty)%Xd(ivertex,1:2)

 end subroutine get_Xd_POLYG

 subroutine set_Vd_POLYG(itacty,ivertex,V)
   implicit none
   integer::itacty,ivertex
   real(kind=8),dimension(2):: V
   
   l_POLYG(itacty)%Vd(ivertex,1:2) = V

 end subroutine set_Vd_POLYG

 subroutine set_Xd_POLYG(itacty,ivertex,V)
   implicit none
   integer::itacty,ivertex
   real(kind=8),dimension(2):: V

   l_POLYG(itacty)%Xd(ivertex,1:2) = V

 end subroutine set_Xd_POLYG

 subroutine clean_memory_POLYG()
   implicit none
   integer(kind=4) :: i
   nb_polyg = 0

   if( allocated(polyg2bdyty) ) deallocate(polyg2bdyty)

   if( allocated(l_POLYG) ) then
     do i = 1, size(l_POLYG)
       if( associated(l_POLYG(i)%vertex)     ) deallocate(l_POLYG(i)%vertex)
       if( associated(l_POLYG(i)%vertex_ref) ) deallocate(l_POLYG(i)%vertex_ref)
       if( associated(l_POLYG(i)%normale)    ) deallocate(l_POLYG(i)%normale)
       if( associated(l_POLYG(i)%normale_ref)) deallocate(l_POLYG(i)%normale_ref)
       if( associated(l_POLYG(i)%Xd)     )     deallocate(l_POLYG(i)%Xd)
       if( associated(l_POLYG(i)%Vd) )         deallocate(l_POLYG(i)%Vd)
     end do
     deallocate(l_POLYG)
   end if

   if( associated(nb_point_outlines_POLYG) ) then
     deallocate(nb_point_outlines_POLYG)
     nullify(nb_point_outlines_POLYG)
   end if

   if( associated(outlines_POLYG) ) then
     deallocate(outlines_POLYG)
     nullify(outlines_POLYG)
   end if

   if( associated(scalarfields_POLYG) ) then
     deallocate(scalarfields_POLYG)
     nullify(scalarfields_POLYG)
   end if

  if(allocated( l_bdyty_2_polyg)) then
     deallocate(l_bdyty_2_polyg)
  end if

 end subroutine

 !------------------------------------------------------------------------ 
 subroutine get_radii_POLYG(itact, min_radius, max_radius)
   implicit none
   integer(kind=4)           :: itact
   real(kind=8)              :: min_radius,max_radius

     min_radius = l_POLYG(itact)%inner_radius
     max_radius = l_POLYG(itact)%outer_radius
 
 end subroutine get_radii_POLYG  
 
 !------------------------------------------------------------------------
 function compute_inner_radius_POLYG(itact)
   implicit none
   integer(kind=4)             :: itact,i,a
   real(kind=8)                :: c,s,lx,ly,min_l
   real(kind=8)                :: compute_inner_radius_POLYG
   real(kind=8),dimension(:,:),allocatable :: vertex

   allocate(vertex(2,l_POLYG(itact)%nb_vertex))
   vertex = 0.d0

   min_l = 1.d20

   do a = 1,45
      c = cos(pi_g/180.d0*a)
      s = sin(pi_g/180.d0*a)

       ! tourne le POLYG d'un angle a
       do i = 1,l_POLYG(itact)%nb_vertex
         vertex(1,i) = c*l_POLYG(itact)%vertex_ref(1,i) - s*l_POLYG(itact)%vertex_ref(2,i)
         vertex(2,i) = s*l_POLYG(itact)%vertex_ref(1,i) + c*l_POLYG(itact)%vertex_ref(2,i)
       enddo
       
       ! dimensions du POLYG sur x et y
       lx = MAXVAL(vertex(1,:)) - MINVAL(vertex(1,:))
       ly = MAXVAL(vertex(2,:)) - MINVAL(vertex(2,:))
       
       ! stocke la valeur mini
       min_l = min(lx,ly,min_l)
       
   enddo

   compute_inner_radius_POLYG = 0.5*min_l
   
   deallocate(vertex) 

 end function compute_inner_radius_POLYG


end module POLYG
