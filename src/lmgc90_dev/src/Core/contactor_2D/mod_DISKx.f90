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
module DISKx                                       
  
  !!****h* LMCG90.CORE/DISKx
  !! NAME
  !!  module DISKx
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
  
  use RBDY2,only:   get_nb_RBDY2, get_entity_RBDY2, get_data         , &
                    get_mass_DISKx               => get_mass         , &
                    get_inv_mass_DISKx           => get_inv_mass     , &
                    add_stress_DISKx             => add_stress       , &
                    print_info_DISKx             => print_info_RBDY2 , &
                    get_therm_cond_DISKx         => get_therm_cond   , &
                    get_thermal_value_DISKx      => get_thermal_value, &
                    get_average_WS_DISKx         =>  get_average_WS  , &
                    get_electric_potentiel_DISKx => get_electric_potentiel, &
                    get_elec_cond_DISKx          => get_elec_cond    , &
                    get_betai_DISKx              => get_betai        , &
                    add_betai_to_DISKx           => add_betai        , &
                    ! Before: common with POLYG
                    get_WS_DISKx                 => get_WS              , &
                    get_surface_sectors_DISKx    => get_surface_sectors , &
                    get_T_DISKx                  => get_T_RBDY2         , &
                    update_status_sector_DISKx   => update_status_sector, &
                    get_xperiode_RBDY2, &
                    is_periodic_RBDY2 , &
                    !
                    get_nb_tacty_RBDY2  => get_nb_tacty , &
                    get_tacid_RBDY2     => get_tacid    , &
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
                    nullify_reac_RBDY2  => nullify_reac , &
                    is_dof_driven_RBDY2

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

  ! ----------------------------------------------------------------------
  ! diskx2bdyty
  ! diskx2bdyty(1,itac) : serial number of body RBDY2 to which is attached
  !                       the contactor DISKx numbered itac in the list of
  !                       all contactors DISKx
  ! diskx2bdyty(2,itac) : serial number of contactor DISKx itac in the list
  !                       of contactors DISKx attached to a body(Generically
  !                       1 except if the underlying model is "i_mbs2")
  ! diskx2bdyty(3,itac) : kind of underlying model i_rbdy2:rigid, i_mbs2:MBS
  ! ----------------------------------------------------------------------
  integer(kind=4),dimension(:,:),allocatable,target  ::  diskx2bdyty

  !> pointing on polyg2bdyty to use include
  integer(kind=4), dimension(:,:), pointer ::  tactype2bdyty => null()

  integer(kind=4) :: nb_DISKx
  real(kind=8) :: min_radius,max_radius,mean_radius

  ! fd structure pour gerer la dilatation (Xd,Vd)*nb_DISKx
  real(kind=8), dimension(:,:), allocatable :: dila

  integer(kind=4) :: nbpto=20 ! nb points describing the conntactor outline
  integer(kind=4) :: nbsf=9 
  integer(kind=4),dimension(:),pointer :: nb_point_outlines_DISKx => null() 
  real(kind=8),dimension(:,:),pointer  :: outlines_DISKx => null()
  real(kind=8),dimension(:),pointer    :: scalarfields_DISKx => null()

  ! public data

  public diskx2bdyty

  ! public subroutines

  public read_bodies_DISKx

  public get_nb_DISKx, &
         get_mean_radius_DISKx, get_max_radius_DISKx, get_min_radius_DISKx, &
         get_radius_DISKx, &
         get_mass_DISKx,get_inv_mass_DISKx, &
         is_DISKx_same_BDYTY, &
         print_info_DISKx,get_T_DISKx,get_WS_DISKx, &
         get_surface_sectors_DISKx, &
         add_stress_DISKx, &
         get_DISKx2BDYTY, &  ! <- am: debut des fonctions supplementaires
         get_ptr_DISKx2BDYTY, & ! <- rm: debut des fonctions supplementaires
         !get_body_radius_DISKx, &
         set_data_DISKx, &
         get_nb_point_outlines_DISKx,&
         init_outlines_DISKx, &
         get_nb_scalarfields_DISKx,&
         init_scalarfields_DISKx,&
         update_postdata_DISKx, &
         update_status_sector_DISKx, &
         get_therm_cond_DISKx, &
         get_thermal_value_DISKx, &
         get_average_WS_DISKx, &
         get_electric_potentiel_DISKx, &
         get_elec_cond_DISKx, &
         get_betai_DISKx, &
         add_betai_to_DISKx, &
         clean_memory_DISKx, &
         get_Vd_DISKx, &
         set_Xd_DISKx, &
         set_Vd_DISKx, &
         !
         get_ENT      , &
         get_color    , &
         get_visible  , &
         add_reac     , &
         get_vlocy    , &
         get_cooref   , &
         get_coor     , &
         get_coorTT   , &
         get_shiftTT  , &
         get_Xbegin   , &
         get_X        , &
         get_Vbegin   , &
         get_V        , &
         get_reac     , &
         nullify_vlocy, &
         comp_vlocy   , &
         nullify_reac , &
         get_tact_id  , &
         all_dof_driven_DISKx


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

!!!------------------------------------------------------------------------
  subroutine read_bodies_DISKx

    implicit none

    integer(kind=4) :: ibdyty,itacty,errare
    integer(kind=4) :: itact
    integer(kind=4) :: nb_RBDY2, nb_MBS2D
    
    real(kind=8),dimension(1) :: data
    character(len=18) :: IAM='DISKx::read_bodies'
    character(len=80) :: cout

    nb_DISKx=0
    
    nb_RBDY2=get_nb_RBDY2()
    
    do ibdyty=1,nb_RBDY2
       do itacty=1,get_nb_tacty_RBDY2(ibdyty)
          if (get_tacid_RBDY2(ibdyty,itacty) == 'DISKx' .or. &
               get_tacid_RBDY2(ibdyty,itacty) == 'DISKb')  nb_DISKx=nb_DISKx+1
       end do
    end do

    nb_MBS2D = get_nb_MBS2D()

    do ibdyty=1,nb_MBS2D
       do itacty=1,get_nb_tacty_MBS2D(ibdyty)
          if (get_tacID_MBS2D(ibdyty,itacty) == i_diskx)  nb_DISKx=nb_DISKx+1
       end do
    end do

    write(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_DISKx,'DISKx found'
    call LOGMES(cout)
    
    if (nb_DISKx == 0) return
    
    allocate(diskx2bdyty(3,nb_DISKx),stat=errare)
    tactype2bdyty => diskx2bdyty
    
    if (errare /= 0) then
       call FATERR(IAM,'error allocating diskx2bdyty')
    end if
    
    nb_DISKx=0
    
    do ibdyty=1,nb_RBDY2
       do itacty=1,get_nb_tacty_RBDY2(ibdyty)
          if (get_tacid_RBDY2(ibdyty,itacty) == 'DISKx' .or. &
               get_tacid_RBDY2(ibdyty,itacty) == 'DISKb') then
             nb_DISKx=nb_DISKx+1
             diskx2bdyty(1,nb_DISKx)=ibdyty  !   diskx2bdyty(1,itac): serial number of body RBDY2 to which is attached the 
                                             !                        contactor DISKx numbered itac in the list of all 
                                             !                        contactors DISKx 
             diskx2bdyty(2,nb_DISKx)=itacty  !   diskx2bdyty(2,itac): serial number of contactor DISKx itac in the list of 
                                             !                        contactors of any kind attached to body diskx2bdyty(1,itac)
             diskx2bdyty(3,nb_DISKx)=i_rbdy2 !   diskx2bdyty(3,itac): type of body the contactor is attached to
          end if
       end do
    end do

    do ibdyty = 1, nb_MBS2D
        do itacty = 1, get_nb_tacty_MBS2D(ibdyty)
            if (get_tacID_MBS2D(ibdyty,itacty) == i_diskx) then
                nb_DISKx=nb_DISKx+1
                diskx2bdyty(1,nb_DISKx) = ibdyty
                diskx2bdyty(2,nb_DISKx) = itacty
                diskx2bdyty(3,nb_DISKx) = i_mbs2
            end if
        end do
    end do

    ! fd gestion de la dilatation

    allocate(dila(2,nb_DISKx),stat=errare)
    if (errare /= 0) then
       call FATERR(IAM,'error allocating dila')
    end if

    dila = 0.d0

    ! 

    min_radius = 1.D20
    max_radius = 0.D0
    mean_radius= 0.D0
    
    do itact=1,nb_DISKx
        if( diskx2bdyty(3,itact) == i_rbdy2 ) then
            call get_data(diskx2bdyty(1,itact),diskx2bdyty(2,itact),data)
        else if( diskx2bdyty(3,itact) == i_mbs2 ) then
            data(:) = get_ptr_rdata_MBS2D(diskx2bdyty(1,itact),diskx2bdyty(2,itact))
        end if

       min_radius  = min(data(1),min_radius)
       max_radius  = max(data(1),max_radius)
       mean_radius = mean_radius + data(1)
    end do
    mean_radius=mean_radius/nb_DISKx
    
  end subroutine read_bodies_DISKx
!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES ...
!
!------------------------------------------------------------------------ 
 function get_nb_DISKx(fantome)

   implicit none
   integer(kind=4),optional :: fantome
   integer(kind=4) :: get_nb_DISKx
  
   get_nb_DISKx = nb_DISKx
  
 end function get_nb_DISKx
!------------------------------------------------------------------------ 
 function get_mean_radius_DISKx(fantome)

   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_mean_radius_DISKx
 
   get_mean_radius_DISKx=mean_radius

 end function get_mean_radius_DISKx
!------------------------------------------------------------------------ 
 function get_max_radius_DISKx(fantome)

   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_max_radius_DISKx
 
   get_max_radius_DISKx=max_radius

 end function get_max_radius_DISKx
!------------------------------------------------------------------------ 
 function get_min_radius_DISKx(fantome)

   implicit none   
   real(kind=8),optional :: fantome
   real(kind=8)  :: get_min_radius_DISKx
 
   get_min_radius_DISKx=min_radius

 end function get_min_radius_DISKx
!------------------------------------------------------------------------
 function get_radius_DISKx(itact)
   implicit none
   integer(kind=4)      :: itact 
   real(kind=8) :: get_radius_DISKx
   real(kind=8),dimension(1) :: data

   if( diskx2bdyty(3,itact) == i_rbdy2 ) then
        call get_data(diskx2bdyty(1,itact),diskx2bdyty(2,itact),data)
   else if( diskx2bdyty(3,itact) == i_mbs2 ) then
        data(:) = get_ptr_rdata_MBS2D(diskx2bdyty(1,itact),diskx2bdyty(2,itact))
   end if

   !fd dilatation get_radius_DISKx=data(1)
   get_radius_DISKx = data(1) + dila(1,itact)

 end function get_radius_DISKx
!------------------------------------------------------------------------
 function is_DISKx_same_BDYTY(itact1,itact2)

   implicit none

   integer(kind=4) :: itact1,itact2 
   logical :: is_DISKx_same_BDYTY
   
   is_DISKx_same_BDYTY=.false.
   if( diskx2bdyty(1,itact1) == diskx2bdyty(1,itact2) .and. &
       diskx2bdyty(3,itact1) == diskx2bdyty(3,itact2) ) is_DISKx_same_BDYTY=.true.
 
   ! for MBS2D, need to check if the node on the mbs is the same
   if (diskx2bdyty(3,itact1) == i_mbs2 .and. &
       diskx2bdyty(3,itact2) == i_mbs2) then
      if (get_nodeID_MBS2D(diskx2bdyty(1,itact1), diskx2bdyty(2,itact1)) /= &
          get_nodeID_MBS2D(diskx2bdyty(1,itact2), diskx2bdyty(2,itact2))) is_DISKx_same_BDYTY=.false.
   endif
 end function is_DISKx_same_BDYTY

 logical function all_dof_driven_DISKx(itact)
   implicit none
   integer, intent(in) :: itact

   all_dof_driven_DISKx = is_dof_driven_RBDY2( diskx2bdyty(1,itact) )

 end function all_dof_driven_DISKx
!------------------------------------------------------------------------ 
 ! integer function get_nb_point_outline_DISKx(fantome)

 !   implicit none
 !   integer(kind=4),optional :: fantome

 !   get_nb_point_outline_DISKx = nbpto

 ! end function get_nb_point_outline_DISKx 
!------------------------------------------------------------------------ 
 subroutine updt_outline_DISKx(itacty,outline)

   implicit none
   integer(kind=4) :: itacty,k
   real(kind=8),dimension(1)         :: data
   real(kind=8)                      :: r,DPI,periode
   real(kind=8),dimension(3)         :: X
   real(kind=8),dimension(2,0:nbpto) :: outline

   !fd get_coor_DISKx gere le shift
   X=get_coor(itacty)
   
   if( diskx2bdyty(3,itacty) == i_rbdy2 ) then
        call get_data(diskx2bdyty(1,itacty),diskx2bdyty(2,itacty),data)
   else if( diskx2bdyty(3,itacty) == i_mbs2 ) then
        data(:) = get_ptr_rdata_MBS2D(diskx2bdyty(1,itacty),diskx2bdyty(2,itacty))
   end if

   !fd dilatation
   r=data(1) + dila(1,itacty) 

   if(is_periodic_RBDY2())then
      periode = get_xperiode_RBDY2()
      if (X(1).gt.periode)then
         X(1) = X(1)-periode
      else if (X(1).lt.0.d0)then
         X(1) = X(1)+periode
      end if
   end if

   outline(1,0) = X(1)
   outline(2,0) = X(2)

   DPI=2.d0*PI_g/(nbpto-1)

   do k=1,nbpto

     outline(1,k) = X(1) + r*cos(X(3)+(k-1)*DPI)
     outline(2,k) = X(2) + r*sin(X(3)+(k-1)*DPI)

   end do

 end subroutine updt_outline_DISKx

 !------------------------------------------------------------------------
 function get_nb_point_outlines_DISKx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_DISKx

   get_nb_point_outlines_DISKx => nb_point_outlines_DISKx

 end function get_nb_point_outlines_DISKx

 !------------------------------------------------------------------------
 function init_outlines_DISKx()
   implicit none
   integer(kind=4) :: itacty,sz
   real(kind=8),dimension(:,:),pointer :: init_outlines_DISKx

   if ( nb_DISKx .eq. 0 ) then
     init_outlines_DISKx => null()
     return
   endif 

   if (associated(nb_point_outlines_DISKx)) deallocate(nb_point_outlines_DISKx)
   allocate(nb_point_outlines_DISKx(nb_DISKx+1)) 
   nb_point_outlines_DISKx(1) = 0
   do itacty = 1, nb_DISKx
     nb_point_outlines_DISKx(itacty+1) = nb_point_outlines_DISKx(itacty) + nbpto-1
   end do

   sz =  nb_point_outlines_DISKx(nb_DISKx+1)

   if (associated(outlines_DISKx)) deallocate(outlines_DISKx)
   allocate(outlines_DISKx(2,sz)) 

   outlines_DISKx(1:2,1:sz) = 0.d0

   init_outlines_DISKx => outlines_DISKx

 end function init_outlines_DISKx

 !------------------------------------------------------------------------
 function get_nb_scalarfields_DISKx()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_DISKx

   get_nb_scalarfields_DISKx = nbsf

 end function get_nb_scalarfields_DISKx

 !------------------------------------------------------------------------
 subroutine updt_scalarfield_DISKx(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty,id_rbdy2,id_tacty,inbsf
   real(kind=8),dimension(nbsf) :: scalarfield
   
   id_rbdy2 = diskx2bdyty(1,itacty)
   id_tacty = diskx2bdyty(2,itacty)

   !mr : displacement and velocity of the center of mass
   scalarfield(1:3) = get_X(itacty)
   scalarfield(4:6) = get_V(itacty)
   scalarfield(7:9) = get_REAC(itacty)

 end subroutine updt_scalarfield_DISKx
 
 !------------------------------------------------------------------------
 function init_scalarfields_DISKx()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_DISKx

   !default value: nbsf = 9

   if ( nb_DISKx .eq. 0 ) then
     init_scalarfields_DISKx => null()
     return
   endif 

   sz = nbsf * nb_DISKx

   if (associated(scalarfields_DISKx)) deallocate(scalarfields_DISKx)
   allocate(scalarfields_DISKx(sz)) 

   scalarfields_DISKx(1:sz) = 0.d0

   init_scalarfields_DISKx => scalarfields_DISKx

 end function init_scalarfields_DISKx
!------------------------------------------------------------------------
 subroutine update_postdata_DISKx()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf
   real(kind=8) :: outline(2,0:nbpto)

   if (nb_DISKx == 0) return
   if (.not. associated(outlines_DISKx)) call faterr('DISKx::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_DISKx)) call faterr('DISKx::update_postdata','init_scalarfields is mandatory')
   
   iszo = 0
   iszsf = 0
   do itacty=1,nb_DISKx
     call updt_outline_DISKx(itacty,outline) 
     ! attention la routine get_outline a ete ecrite pour gmv 
     ! et necessite un contour ferme
     ! mj pas touche ...
     outlines_DISKx(:,iszo+1:iszo+nbpto-1) = outline(:,1:nbpto-1)    
     iszo = iszo + nbpto-1
     !update for mp
     call updt_scalarfield_DISKx(itacty,scalarfields_DISKx(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_DISKx
!------------------------------------------------------------------------
!!!-am----- début des définitions de fonctions supplémentaires...------

 !fd mierda
 
 ! ! procèdure qui récupère la table de correspondance entre les contacteurs
 ! ! disques et les corps associés, dans RBDY2:
 ! ! préconditions:
 ! !	- diskx2rbdy2_: vecteur pour récupérer la table de correspondance
 ! !	- nbdisk_: nombre de disques attendus par celui qui appelle cette fonction
 ! !			   , i.e. la taille de diskx2rbdy2_
 ! ! postconditions:
 ! !	- diskx2rbdy2_ contient la table de dorrepondance, i.e., pour
 ! !	  i dans {1, ..., nb_DISKx}, on a:
 ! !		diskx2rbdy2_(i) = le numéro du corps, dans RBDY2, supportant le 
 ! !    contacteur disque numéro i
 ! subroutine get_DISKx2RBDY2(diskx2rbdy2_, nb_disk_)

 !        integer(kind=4), intent(in) :: nb_disk_ ! nombre de disques attentu par celui qui appelle cette
 !                                        ! fonction, i.e. la taille de diskx2rbdy2_	
 !        integer(kind=4), dimension(nb_disk_), intent(out) :: diskx2rbdy2_ ! vecteur pour récupérer la table de
 !                                                                  ! correpondance

 !        ! on vérifie que la taille de diskx2rbdy2_ est consistante avec le nombre de disques
 !        if (nb_disk_ .ne. nb_DISKx) then

 !           ! si ce n'est pas le cas, c'est l'erreur fatale!
 !           call FATERR('mod_DISKx::get_DISKx2RBDY2', 'Error: unconsistant number of disks!')

 !        end if

 !        ! on recupere la table de corespondance
 !        diskx2rbdy2_(:) = diskx2bdyty(1, :)

 ! end subroutine get_DISKx2RBDY2

 subroutine get_DISKx2BDYTY(map, size1, size2)
   implicit none
   integer(kind=4), dimension(:,:), pointer :: map
   integer(kind=4), intent(out)             :: size1, size2
   !
   if(associated(map) ) nullify(map)

   size1 = 3
   size2 = nb_DISKx

   if( nb_DISKx > 0 ) then

     allocate(map(size1,size2))

     map(1:size1,1:size2) = DISKx2bdyty(1:size1,1:size2)

   end if

 end subroutine

!------------------------------------------------------------------------
 function get_ptr_DISKx2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_DISKx2BDYTY

   get_ptr_DISKx2BDYTY => diskx2bdyty

 end function get_ptr_DISKx2BDYTY
 !------------------------------------------------------------------------

 !fd vire le 2020-06-17
 
 ! !fd TODO a virer car on remonte la map DISKx2bdyty !
 
 ! function get_body_radius_DISKx(ibdyty)
 !   implicit none
 !   integer(kind=4), intent(in) :: ibdyty
 !   real(kind=8) :: get_body_radius_DISKx
 !   !
 !   integer(kind=4) :: itacty
 !   real(kind=8), dimension(1) :: data

 !   get_body_radius_DISKx = 0.D0

 !   do itacty = 1, nb_DISKx
 !     if( diskx2bdyty(1,itacty) == ibdyty .and. &
 !         diskx2bdyty(3,itacty) == i_rbdy2) then
 !       call get_data(ibdyty,diskx2bdyty(2,itacty),data)
 !       !fd dilatation
 !       get_body_radius_DISKx = data(1) + dila(1,itacty)
 !       return
 !     end if
 !   end do

 ! end function
!------------------------------------------------------------------------
 !> \brief use anonymous data to initialize a contactor
 subroutine set_data_DISKx(idata, rdata, node_list, nb_support, support, Brd)
   implicit none
   !> [in] idata: anonymous integer data (NULL)
   integer(kind=4), dimension(:), pointer :: idata
   !> [in] rdata: anonymous real data (radius, [shift_x, shift_y])
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

   ! only center of the disk
   nb_support = 1
   allocate( support(nbDIME,nb_support) )

   support = 0.d0
   if( size(rdata) == 3 ) then !there is a shift
     support(1:2,1) = rdata(2:3)
   end if

   Brd = rdata(1)

  end subroutine
!!!------------------------------------------------------------------------

  subroutine clean_memory_DISKx()
    implicit none
    integer(kind=4) :: i
 
    nb_diskx = 0
 
    if( allocated(diskx2bdyty) ) deallocate(diskx2bdyty)
 
    if( associated(nb_point_outlines_DISKx) ) then
      deallocate(nb_point_outlines_DISKx)
      nullify(nb_point_outlines_DISKx)
    end if
 
    if( associated(outlines_DISKx) ) then
      deallocate(outlines_DISKx)
      nullify(outlines_DISKx)
    end if
 
    if( associated(scalarfields_DISKx) ) then
      deallocate(scalarfields_DISKx)
      nullify(scalarfields_DISKx)
    end if
 
    if( allocated(dila) ) deallocate(dila)
    
    tactype2bdyty => null()

  end subroutine

!------------------------------------------------------------------------
 subroutine get_Vd_DISKx(itact,v)
   implicit none
   integer(kind=4)      :: itact 
   real(kind=8)         :: v 
   
   v = dila(2,itact) 

 end subroutine get_Vd_DISKx  
!------------------------------------------------------------------------
 subroutine set_Vd_DISKx(itact,v)
   implicit none
   integer(kind=4) :: itact 
   real(kind=8)    :: v
   
   dila(2,itact) = v 

 end subroutine set_Vd_DISKx  
!------------------------------------------------------------------------
 subroutine set_Xd_DISKx(itact,x)
   implicit none
   integer(kind=4) :: itact 
   real(kind=8)    :: x
   
   dila(1,itact) = x

 end subroutine set_Xd_DISKx  
!------------------------------------------------------------------------

end module DISKx

