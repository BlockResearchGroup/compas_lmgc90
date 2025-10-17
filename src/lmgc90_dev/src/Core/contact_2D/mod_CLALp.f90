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
module CLALp

  !>  This module deals with geometric and kinematic operations between CLxxx and ALpxx contactors.
  !>  In this module candidate contactors are CLxxx and antagonist contactors are ALpxx

  use overall
  use tact_behaviour

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use CLxxx, only : get_nb_cdtac        => get_nb_clxxx        , &
                    nullify_reac_cdtac  => nullify_reac_CLxxx  , &
                    nullify_vlocy_cdtac => nullify_vlocy_CLxxx , &
                    comp_vlocy_cdtac    => comp_vlocy_CLxxx    , &
  !
  ! RIP
  !
                    get_nb_clxxx,nullify_reac_CLxxx,nullify_vlocy_CLxxx,comp_vlocy_CLxxx, &
                    get_nodes_clxxx,get_coorTT_clxxx,l_clxxx,get_apab_clxxx,clxxx2bdyty, &
                    get_visible_clxxx,get_ent_clxxx,get_normalTT_clxxx,get_length_clxxx, &
                    get_bulk_strain_clxxx,get_coor_support_clxxx,put_vwear_clxxx,get_vlocy_clxxx,&
                    add_reac_clxxx, get_bulk_stress_clxxx, get_bulk_strain_triaxiality_clxxx, &
                    get_bulk_stress_triaxiality_clxxx, &
                    all_dof_driven_CLxxx
             
  use ALpxx, only : get_nb_antac        => get_nb_alpxx        ,&
                    nullify_reac_antac  => nullify_reac_ALpxx  ,&
                    nullify_vlocy_antac => nullify_vlocy_ALpxx , &
                    comp_vlocy_antac    => comp_vlocy_ALpxx    , &
  !
  ! RIP
  !
                    get_nb_alpxx,nullify_reac_ALpxx,nullify_vlocy_ALpxx,comp_vlocy_ALpxx, &
                    get_nb_node_alpxx,get_nodes_alxxx,get_coorTT_alpxx,l_alpxx,alpxx2bdyty, &
                    get_visible_alpxx,get_ent_alpxx,get_coor_support_alxxx,put_vwear_alpxx,&
                    get_vlocy_alpxx,add_reac_alpxx, &
                    all_dof_driven_ALxxx
  
  
  use ann, ann_clean_memory => clean_memory

  use parameters, only : i_clalp, i_clxxx, i_alpxx, i_mailx, i_rbdy2, i_mbs2

  use inter_meca_2D

  use ExternalFEM
  use ExternalDetection

  implicit none

  private 
  
  logical :: bavard= .false. !.TRUE.

  logical :: is_externalDetection = .FALSE.
 
  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer :: nb_CLxxx
  integer :: nb_ALpxx
  
  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  !------------------------------------------------------------------------ 
  ! nb_CLALp = number of selected candidates CLxxx against ALpxx <= size(this).
  
   integer          :: nb_CLALp=0                
   integer          :: nb_vCLALp=0

  !fd />
   
  !------------------------------------------------------------------------ 

  type(T_this_adjac), dimension(:), allocatable, target :: adjac

  !------------------------------------------------------------------------  

  integer, dimension(:), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs CLxxx-ALpxx
                                                        ! to candidate contactor CLxxx icdtac.
  !------------------------------------------------------------------------

  type(T_verlet), dimension(:), allocatable, target :: verlt

  !------------------------------------------------------------------------

  type T_rough_CLALp

   integer :: ialpxx, &  ! la fontiere la plus proche
              inode,  &  ! le noeud de cette fontiere le plus proche        
              isee 
   real(kind=8) :: adist 

 end type T_rough_CLALp

 type(T_rough_CLALp),dimension(:),allocatable :: rough_CLALp

 integer :: nb_rough_CLALp,nstep_rough_seek_CLALp=1
 integer :: nb_recup_CLALp

 ! says if verlet is available   
 logical :: READ =.false.
 ! says if this is available
 logical :: RUN=.false.

 logical      :: module_checked_ = .FALSE.
 logical      :: check_CLALp_    = .FALSE.

 !------------------------------------------------------------------------ 
 ! coordinates of CL candidate 
 real(kind=8),dimension(:,:),allocatable :: CLcoor  

 type T_coor
    real(kind=8),dimension(:,:),pointer :: coor
    real(kind=8),dimension(:,:),pointer :: normal
 end type T_coor

 ! coordinates of body owning ALxxx to be used in selecting prox tactors
 type(T_coor),dimension(:),allocatable  :: ALp      

 real(kind=8)  :: global_adist=1.d+20

 real(kind=8) :: Reac_CLALp_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

 logical :: is_open_energy=.false.,is_open_internal=.false.,is_nonsymmetric_detection=.false.

 logical :: trim_contact = .FALSE. 

!------------------------------------------------------------------------

! liste des fonctions publiques 
 public &
      stock_rloc_CLALp, &
      recup_rloc_CLALp, &
      recup_rloc_by_position_CLALp, &
      compute_box_CLALp, &
      read_ini_Vloc_Rloc_CLALp, &
      write_xxx_Vloc_Rloc_CLALp, &
      coor_prediction_CLALp, &
      creation_tab_visu_CLALp, &
      compute_contact_CLALp, &
      external_detection_CLALp, &
      display_prox_tactors_CLALp, &
      update_wear_CLALp, &
      RUN_CLALp, &
      CHECK_CLALp, &
      reset_CHECK_CLALp, &
      get_write_Vloc_Rloc_CLALp, &
      set_nonsymmetric_detection_CLALp, &
      trim_CLALp, &
      is_external_detection_CLALp
!!$      display_contact_energy_CLALp, &
!!$      display_contact_internal_CLALp, &

 
 public nullify_reac_CLALp, &
        nullify_vlocy_CLALp, &
        injj_CLALp, prjj_CLALp, vitrad_CLALp, & 
        get_nb_CLALp, &
        get_length_CLALp, &
        print_info_CLALp, &
        CLALp2CLxxx, &
        CLALp2ALpxx, &
        get_external_pressure_CLALp, &
        clean_memory_CLALp

  !rm for handler
  public get_this    , &
         set_nb_CLALp, &
         redo_nb_adj_CLALp

  ! for global_thermal_solver, must be in handler !
  public get_iannodes_CLALp     , &
         get_icdnodes_CLALp     , &
         !fd get_internal_CLALp  , &
         get_rel_pos_CLALp      , &
         get_nb_max_adj_CLALp   , &
         get_diffusion_lhs_CLALp, &
         get_tact_lawnb_CLALp   , &
         get_verlet_tact_lawnb  , &
         get_an_tacty           , &
         get_bulk_strain_clalp  , &
         get_bulk_stress_clalp  , &
         get_bulk_strain_triaxiality_clalp, &
         get_bulk_stress_triaxiality_clalp, & 
         get_bulk_strain_rate_triaxiality_clalp, &
         get_bulk_temperature_clalp


! public get_rough_CLALp              , &
!        reset_nb_adj_CLALp           , &
!        get_nb_adj_CLALp             , &
!        add_adj_CLALp                , &
!        reset_violation_CLALp        , &
!        compute_contacts_in_t2t_CLALp, &
!        write_out_one_vloc_rloc_CLALp

contains

  ! common functions
  
  include 'interaction_common.f90'
  ! provides the following subroutines
  !subroutine get_behaviour_( icdan, see, tact_behav )
  !subroutine get_this(this_inter, verlet_inter, violation_inter)
  !function get_an_tacty(i_mdl, i_bdy, i_tac)
  !subroutine redo_nb_adj_( nb_cd )
  !subroutine new_verlet_(icdtac, size, errare)
  !subroutine free_verlet_(icdtac)
  !subroutine nullify_verlet_(icdtac)
  !subroutine clean_memory_inter_meca_()
  include 'interaction_common_2D.f90'
  ! defines the following subroutines
  !function get_verlet_tact_lawnb( icdtac, iadj )


  !fd not used
  ! !------------------------------------------------------------------------
  ! subroutine get_antac_CLALp( icdtac, iadj, iantac )
  !   implicit none
  !   integer, intent(in)  :: icdtac
  !   integer, intent(in)  :: iadj
  !   integer, intent(out) :: iantac

  !   if (READ) then
  !     iantac = verlt(icdtac)%lantac(iadj)
  !   else
  !     call faterr('dtc','bp')  
  !   endif
   
  ! end subroutine get_antac_CLALp

  !fd not used
  ! !------------------------------------------------------------------------
  ! subroutine put_internal_CLALp( icdan, internal )
  !   implicit none
  !   integer                   , intent(in) :: icdan
  !   real(kind=8), dimension(:), intent(in) :: internal

  !   if (RUN) then
  !     this(icdan)%internal(1:max_internal_tact) = internal(1:max_internal_tact)
  !  else
  !     call faterr('dtc','bp')  
  !  endif
   
  ! end subroutine put_internal_CLALp

  !fd not used
  ! !------------------------------------------------------------------------ 
  ! subroutine get_internal_CLALp( icdan, internal )
  !   implicit none
  !   integer                   , intent(in)  :: icdan
  !   real(kind=8), dimension(:), intent(out) :: internal

  !   if (RUN) then
  !     internal(1:max_internal_tact) = this(icdan)%internal(1:max_internal_tact)
  !   else
  !     call faterr('dtc','bp')        
  !   endif  
  ! end subroutine get_internal_CLALp

  !------------------------------------------------------------------------
  subroutine coor_prediction_CLALp

    implicit none
    
    integer      :: iclxxx,ialpxx,ial
    real(kind=8) :: norm, tmp(2)

    nb_CLxxx=get_nb_CLxxx()
    nb_ALpxx=get_nb_ALpxx()

    do iclxxx=1,nb_CLxxx

       CLcoor(1:2,iclxxx) = get_coorTT_CLxxx(iclxxx)

    end do

    ! coordonnee de a et b

    do ialpxx=1,nb_ALpxx
       
       call get_coorTT_ALpxx(ialpxx,get_nb_node_ALpxx(ialpxx),ALp(ialpxx)%coor)

       ! calcul de la normale
       do ial=1,get_nb_node_ALpxx(ialpxx)-1       
         tmp = ALp(ialpxx)%coor(:,ial+1) - ALp(ialpxx)%coor(:,ial) 
         norm=dsqrt(tmp(1)**2 + tmp(2)**2)
         tmp=tmp/norm
         ALp(ialpxx)%normal(:,ial)=(/ -tmp(2), tmp(1) /) 
       enddo
    end do

  end subroutine coor_prediction_CLALp

  !------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_CLALp(step)
    implicit none
    integer(kind=4), intent(in) :: step
    
    G_nfich=get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_Vloc_Rloc(:))))
    else
      open(unit=G_nfich,file=trim(location(last_Vloc_Rloc(:))))
    end if

    call read_ini_Vloc_Rloc
    close(G_nfich)

    ! assume verlet available
    READ=.true.
    
  end subroutine read_ini_Vloc_Rloc_CLALp

  !------------------------------------------------------------------------
  subroutine write_xxx_Vloc_Rloc_CLALp(which)
    
    implicit none
    
    integer :: which,nfich,lc
    
    nfich = get_io_unit()
    
    select case(which)
    case(1)
       lc = len_trim(out_Vloc_Rloc)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_Vloc_Rloc(1:lc))))
       call write_out_Vloc_Rloc(nfich)
       close(nfich)
    case(2)
       lc = len_trim(last_Vloc_Rloc)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_Vloc_Rloc(1:lc))))
       call write_out_Vloc_Rloc(nfich)
       close(nfich)
    case(6)
       call write_out_Vloc_Rloc(6)
    end select
    
  end subroutine write_xxx_Vloc_Rloc_CLALp

  !------------------------------------------------------------------------
  subroutine set_nonsymmetric_detection_CLALp()
    
    implicit none
    
    is_nonsymmetric_detection = .true.
    
  end subroutine set_nonsymmetric_detection_CLALp

  !------------------------------------------------------------------------
  subroutine trim_CLALp
    implicit none

    trim_contact = .TRUE.

  end subroutine

 !------------------------------------------------------------------------
 subroutine compute_box_CLALp

   implicit none

   integer :: nb_CLxxx, nb_ALpxx, ial,errare
   integer :: itact,ibdy,icl
   character(len=80) :: cout
                              !123456789012345678
   character(len=18) :: IAM = 'CLALp::compute_box'

   nb_CLxxx=get_nb_CLxxx()

   if (allocated(nb_adj)) deallocate(nb_adj)
   allocate(nb_adj(nb_CLxxx),stat=errare)
   if (errare /=0 ) then
     call faterr(IAM,' error allocating nb_adj, in select_prox_tactors, in mod_CLALp')
   end if    

   nb_adj=0
        
   if (allocated(adjac)) then

     do itact = 1, size(adjac)
       if (associated(adjac(itact)%icdan))  deallocate(adjac(itact)%icdan)
       nullify(adjac(itact)%icdan)
     end do

     if ( nb_CLxxx > size(adjac) ) deallocate(adjac)

   end if

   if (.not. allocated(adjac)) then
     allocate(adjac(nb_CLxxx),stat=errare)
     if (errare /=0 ) then
       call faterr(IAM,'error in allocating adjac, in select_prox_tactors in mod_CLALp')
     end if
     do itact=1,nb_CLxxx
       nullify(adjac(itact)%icdan)

       allocate(adjac(itact)%icdan(1))

     end do
   else
     do itact=1,nb_CLxxx
       if (associated(adjac(itact)%icdan))  deallocate(adjac(itact)%icdan)
       nullify(adjac(itact)%icdan)
!
! pour gagner du temps ... un CL ne vera jamais qu'un iantac
!
       allocate(adjac(itact)%icdan(1))
!
     enddo
   endif  
  
   if (allocated(CLcoor)) deallocate(CLcoor)
   allocate(CLcoor(2,nb_CLxxx),stat=errare)   

   if (errare /=0 ) then
     call faterr(IAM,' error allocating CL_coor, in select_prox_tactors, in mod_CLALp')
   end if    


   if (allocated(ALp)) then
     do ial = 1,size(Alp) 
       deallocate(Alp(ial)%coor)
       deallocate(Alp(ial)%normal)
     enddo
     deallocate(ALp)
   endif

   nb_ALpxx=get_nb_ALpxx()
   allocate(ALp(nb_ALpxx),stat=errare)

   if (errare /=0 ) then
     call faterr(IAM,' error allocating ALpxx, in select_prox_tactors, in mod_CLALp')
   end if    
 
   do ial=1,nb_ALpxx
     allocate(ALp(ial)%coor(2,get_nb_node_ALpxx(ial)),stat=errare)     
     allocate(ALp(ial)%normal(2,get_nb_node_ALpxx(ial)-1),stat=errare)     
   enddo

   if (errare /=0 ) then
     call faterr(iam,' error allocating ALp%coor, in select_prox_tactors, in mod_CLALp')
   end if    

   if (allocated(rough_CLALp)) deallocate(rough_CLALp)
   allocate(rough_CLALp(nb_CLxxx),stat=errare)   

   if (errare /=0 ) then
     call faterr(iam,' error allocating rough_CLALp, in select_prox_tactors, in mod_CLALp')
   end if    

   do icl=1,nb_CLxxx
     rough_CLALp(icl)%ialpxx=0
     rough_CLALp(icl)%inode=0
     rough_CLALp(icl)%isee=0     
     rough_CLALp(icl)%adist=0.d0     
   enddo

 end subroutine compute_box_CLALp

 !------------------------------------------------------------------------
 subroutine creation_tab_visu_CLALp
 
   implicit none 
 
   integer      :: errare,isee
   integer      :: icdtac,ial,iantac,inode,inode_min,inode_l_min,skip_iantac,ialpxx_min
   real(kind=8) :: dist,dist_min,dist_l_min,pscal,gap_l_min,gap_min

   real(kind=8) :: norm,adist=10.
   real(kind=8) :: xa,ya,xb,yb,xp,yp,un1,un2,ab,ap

   character(len=5)  :: cdcol,ancol
   character(len=34) :: cout

   real(kind=8),dimension(2) :: cd_n,an_n

                              !123456789012345678901234
   character(len=24) :: IAM = 'CLALp::creation_tab_visu'

   !logical :: bavard=.TRUE.


   logical :: is_good, &
              is_first, & ! premiere recherche sur un ALp (pour eviter de cycler)
              is_plus     ! sens de parcourt de l'ALp

   nb_CLxxx=get_nb_CLxxx()
   nb_ALpxx=get_nb_ALpxx()

   nb_rough_CLALp=0 

   do icdtac=1,nb_CLxxx
     if (.not.get_visible_CLxxx(icdtac)) cycle
     if (bavard) &
       print*,'icdtac: ',icdtac 

     cdcol=get_color_MAILx(clxxx2bdyty(1,icdtac),clxxx2bdyty(2,icdtac))
     cd_n= get_normalTT_CLxxx(icdtac)

     if (bavard) &
       write(*,'(A,2(1x,D12.5))') 'normale cd ',cd_n 

 
     skip_iantac=0

     !fd
     !fd on avait un plus proche ... on repart de la
     !fd

     !if ( rough_CLALp(icdtac)%ialpxx /= 0 ) then
     !
     !  iantac    = rough_CLALp(icdtac)%ialpxx

     !  if (.not.get_visible_ALpxx(iantac)) cycle

     !  inode_min = rough_CLALp(icdtac)%inode

     !  dist_min = dsqrt((CLcoor(1,icdtac)-ALp(iantac)%coor(1,inode_min))**2 + &
     !                   (CLcoor(2,icdtac)-ALp(iantac)%coor(2,inode_min))**2   )

     !  !fd principe on cherche le noeud le plus proche ET
     !  !fd le segment sur lequel on projete avant ou apres le noeud
     !  !fd il y a le cas particulier ou on est aux bouts.

     !  is_first=.true.

     !  do

     !    if (bavard) &
     !      print*,'Le candidat ',icdtac,' semble voir l antagoniste ',iantac,'ppp',inode_min,'dist_min',dist_min

     !    !fd on est au milieu d'un poly-ALpxx

     !    if ( inode_min > 1     .and.  &
     !         inode_min < get_nb_node_ALpxx(iantac)) then

     !      !xx
     !      !print*,'in'

     !      an_n= ALp(iantac)%normal(:,inode_min)
     !      pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

     !      ! face + licite ?

     !      if (pscal < -1.d-03) then

     !        ! ok licite

     !        pscal = ( (ALp(iantac)%coor(1,inode_min+1) - ALp(iantac)%coor(1,inode_min)) &
     !                 *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode_min)) ) &
     !               +( (ALp(iantac)%coor(2,inode_min+1) - ALp(iantac)%coor(2,inode_min)) &
     !                 *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode_min)) )
 
     !        if (is_first) then
     !          is_first = .false.
     !          if (pscal >= 0.d0) then
     !            is_plus = .true.
     !          else
     !            is_plus = .false.
     !          endif
     !        end if

     !        !xx
     !        !print*,is_first,is_plus,pscal

     !        if (is_plus) then
     !          if (pscal >= 0.d0) then
     !            !on part dans le sens +

     !            !xx
     !            !print*,'on avance sens +'

     !            !fd on va tester le suivant ...

     !            inode = inode_min + 1

     !            ! distance noeud a noeud
     !            dist = dsqrt((CLcoor(1,icdtac)-ALp(iantac)%coor(1,inode))**2 + &
     !                         (CLcoor(2,icdtac)-ALp(iantac)%coor(2,inode))**2   )

     !            if ( (dist - dist_min) < -1.d-07 ) then

     !              !fd on a trouve un suivant plus pret avec une normale "admissible" on le garde
  
     !              if (bavard) then
     !                print*,dist,'<',dist_min
     !                print*,'on change de ppp ',inode_min,'->',inode
     !              endif

     !              inode_min = inode
     !              dist_min=dist

     !              cycle

     !            else

     !              !fd on ne trouve pas mieux ...

     !              if (abs(dist_min) <= adist) then

     !                !fd ... on garde

     !                rough_CLALp(icdtac)%inode  = inode_min

     !                if (bavard) print*,'on garde le ppp ',inode_min

     !                nb_rough_CLALp = nb_rough_CLALp + 1

     !                if (bavard) &
     !                  write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

     !              else

     !                !fd ... on jete

     !                skip_iantac = rough_CLALp(icdtac)%ialpxx
     !                rough_CLALp(icdtac)%ialpxx  = 0
     !                rough_CLALp(icdtac)%inode   = 0
     !                rough_CLALp(icdtac)%isee    = 0
     !                rough_CLALp(icdtac)%adist   = 0.d0

     !                !print*,'skip'


     !              endif
     !            endif

     !            ! termine
     !            exit

     !          else

     !            ! on est bloque
     !            ! on ne peux pas trouver mieux ...

     !            if (abs(dist_min) <= adist) then

     !              !fd ... on garde
     !              rough_CLALp(icdtac)%inode  = inode_min

     !              if (bavard) print*,'on garde le ppp ',inode_min

     !              nb_rough_CLALp = nb_rough_CLALp + 1

     !              if (bavard) &
     !                write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

     !            else

     !              !fd ... on jete

     !              skip_iantac = rough_CLALp(icdtac)%ialpxx
     !              rough_CLALp(icdtac)%ialpxx  = 0
     !              rough_CLALp(icdtac)%inode   = 0
     !              rough_CLALp(icdtac)%isee    = 0
     !              rough_CLALp(icdtac)%adist   = 0.d0

     !              !print*,'skip'


     !            endif

     !            ! termine
     !            exit

     !          endif

     !        !
     !        ! pscal negatif on part dans l'autre sens
     !        !
     !        else
     !          if ( pscal <=0.d0 ) then

     !          !xx
     !          !print*,'on part dans le sens -'
  
     !            inode = inode_min - 1

     !            dist = dsqrt((CLcoor(1,icdtac)-ALp(iantac)%coor(1,inode))**2 + &
     !                       (CLcoor(2,icdtac)-ALp(iantac)%coor(2,inode))**2   )

     !            !fd test des distances ...

     !            if ( (dist - dist_min) < -1.d-07) then

     !              !fd on a trouve mieux

     !              if (bavard) then
     !                print*,dist,'<',dist_min
     !                print*,'on change de ppp ',inode_min,'->',inode
     !              endif

     !              inode_min = inode
     !              dist_min=dist
     !              cycle
     !            else

     !              !fd on n a pas trouve mieux

     !              if (abs(dist_min) <= adist) then

     !                !fd ... on garde
 
     !                rough_CLALp(icdtac)%inode  = inode_min

     !                if (bavard) print*,'on garde le ppp ',inode_min

     !                nb_rough_CLALp = nb_rough_CLALp + 1

     !                if (bavard) &
     !                  write(*,'(A,2(1x,D12.5))') 'normale an ',an_n


     !              else

     !                !fd ... on jete
 
     !                skip_iantac = rough_CLALp(icdtac)%ialpxx
     !                rough_CLALp(icdtac)%ialpxx  = 0
     !                rough_CLALp(icdtac)%inode   = 0
     !                rough_CLALp(icdtac)%isee    = 0
     !                rough_CLALp(icdtac)%adist   = 0.d0

     !                !print*,'skip'

     !              endif
     !            endif

     !            ! termine
     !            exit

     !          else

     !            ! on est bloque
     !            !fd on n a pas trouve mieux

     !            if (abs(dist_min) <= adist) then

     !              !fd ... on garde

     !              rough_CLALp(icdtac)%inode  = inode_min

     !              if (bavard) print*,'on garde le ppp ',inode_min

     !              nb_rough_CLALp = nb_rough_CLALp + 1

     !              if (bavard) &
     !                write(*,'(A,2(1x,D12.5))') 'normale an ',an_n


     !            else

     !              !fd ... on jete
 
     !              skip_iantac = rough_CLALp(icdtac)%ialpxx
     !              rough_CLALp(icdtac)%ialpxx  = 0
     !              rough_CLALp(icdtac)%inode   = 0
     !              rough_CLALp(icdtac)%isee    = 0
     !              rough_CLALp(icdtac)%adist   = 0.d0

     !              !print*,'skip'

     !            endif



     !          endif

     !          !termine
     !          exit

     !        endif

     !      else

     !        !print*,'stop'

     !        ! arete non licite

!!$  !           if (is_first) then
!!$
!!$  !             ! on part sens -
!!$
!!$  !             is_first=.FALSE.
!!$  !             is_plus=.FALSE.
!!$
!!$  !             inode_min = inode_min - 1
!!$
!!$  !           else
!!$
!!$  !             ! c'est termine
!!$
!!$  !             if (abs(dist_min) <= adist) then
!!$
!!$  !               !fd ... on garde
!!$ 
!!$  !               if (is_plus) then
!!$  !                 rough_CLALp(icdtac)%inode  = inode_min - 1
!!$  !               else
!!$  !                 rough_CLALp(icdtac)%inode  = inode_min + 1
!!$  !               endif
!!$  !               if (bavard) print*,'on garde le ppp ',inode_min
!!$
!!$  !               nb_rough_CLALp = nb_rough_CLALp + 1
!!$
!!$  !               if (bavard) &
!!$  !                 write(*,'(A,2(1x,D12.5))') 'normale an ',an_n
!!$
!!$  !             else

     !            !fd ... on jete
  
     !            skip_iantac = rough_CLALp(icdtac)%ialpxx
     !            rough_CLALp(icdtac)%ialpxx  = 0
     !            rough_CLALp(icdtac)%inode   = 0
     !            rough_CLALp(icdtac)%isee    = 0
     !            rough_CLALp(icdtac)%adist   = 0.d0

     !            !print*,'skip'

!!$  !             endif

     !          ! on arrete pour ce candidat
     !          exit

!!$  !           endif
     !      endif
     !    !
     !    ! on est au debut
     !    !
     !    else if ( inode_min == 1) then

     !      !xx
     !      !print*,'left'

     !      an_n= ALp(iantac)%normal(:,inode_min)
     !      pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

     !      ! face licite ?

     !      if (pscal < -1.d-03) then

     !        ! ok

     !        pscal =  ( (ALp(iantac)%coor(1,inode_min+1) - ALp(iantac)%coor(1,inode_min)) &
     !                  *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode_min)) ) &
     !                +( (ALp(iantac)%coor(2,inode_min+1) - ALp(iantac)%coor(2,inode_min)) &
     !                  *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode_min)) )

     !        if (is_first) then
     !          is_first = .false.
     !          if (pscal >= 0.d0) then
     !            is_plus = .true.
     !          else
     !            is_plus = .false.
     !          endif
     !        end if

     !        !xx
     !        !print*,is_first,is_plus,pscal

     !        if (is_plus) then
     !          if (pscal >= 0.d0) then

     !            !print*,'on part dans le sens +'

     !            inode = inode_min + 1

     !            dist = dsqrt((CLcoor(1,icdtac)-ALp(iantac)%coor(1,inode))**2 + &
     !                         (CLcoor(2,icdtac)-ALp(iantac)%coor(2,inode))**2   )
    
     !            if ((dist - dist_min) < -1.d-07) then

     !              if (bavard) then
     !                print*,dist,'<',dist_min
     !                print*,'on change de ppp ',inode_min,'->',inode
     !              endif
 
     !              inode_min = inode
     !              dist_min=dist

     !              cycle

     !            else

     !              if (abs(dist_min) <= adist) then
 
     !                rough_CLALp(icdtac)%inode  = inode_min
 
     !                if (bavard) print*,'on garde le ppp ',inode_min

     !                nb_rough_CLALp = nb_rough_CLALp + 1

     !                if (bavard) &
     !                  write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

  
     !              else
     !                skip_iantac = rough_CLALp(icdtac)%ialpxx
     !                rough_CLALp(icdtac)%ialpxx  = 0
     !                rough_CLALp(icdtac)%inode   = 0
     !                rough_CLALp(icdtac)%isee    = 0
     !                rough_CLALp(icdtac)%adist   = 0.d0
 
     !                !print*,'skip'

     !              endif
     !              ! termine
     !              exit
     !            endif
     !          else
 
     !            ! on ne peut pas avancer

     !            if (abs(dist_min) <= adist) then

     !              rough_CLALp(icdtac)%inode  = inode_min

     !              if (bavard) print*,'on garde le ppp ',inode_min
  
     !              nb_rough_CLALp = nb_rough_CLALp + 1

     !              if (bavard) &
     !                write(*,'(A,2(1x,D12.5))') 'normale an ',an_n


     !            else

     !              if (bavard) print*,'on perd le noeud',pscal
     !              skip_iantac = rough_CLALp(icdtac)%ialpxx
     !              rough_CLALp(icdtac)%ialpxx  = 0
     !              rough_CLALp(icdtac)%inode   = 0
     !              rough_CLALp(icdtac)%isee    = 0
     !              rough_CLALp(icdtac)%adist   = 0.d0

     !              !print*,'skip'
     !            endif

     !            exit

     !          endif

     !        else

     !          ! on ne peut pas reculer

     !          if (abs(dist_min) <= adist) then

     !            rough_CLALp(icdtac)%inode  = inode_min

     !            if (bavard) print*,'on garde le ppp ',inode_min
  
     !            nb_rough_CLALp = nb_rough_CLALp + 1

     !            if (bavard) &
     !              write(*,'(A,2(1x,D12.5))') 'normale an ',an_n


     !          else

     !            if (bavard) print*,'on perd le noeud',pscal
     !            skip_iantac = rough_CLALp(icdtac)%ialpxx
     !            rough_CLALp(icdtac)%ialpxx  = 0
     !            rough_CLALp(icdtac)%inode   = 0
     !            rough_CLALp(icdtac)%isee    = 0
     !            rough_CLALp(icdtac)%adist   = 0.d0

     !            !print*,'skip'
     !          endif

     !          exit


     !        endif

     !      else

     !        !print*,'stop'

     !        ! arete non licite ...

     !        if (is_first .or. .not. is_plus) then
     !          !fd ... on jete

     !          skip_iantac = rough_CLALp(icdtac)%ialpxx
     !          rough_CLALp(icdtac)%ialpxx  = 0
     !          rough_CLALp(icdtac)%inode   = 0
     !          rough_CLALp(icdtac)%isee    = 0
     !          rough_CLALp(icdtac)%adist   = 0.d0
     !
     !          !print*,'skip'

     !          exit

     !        endif

     !        if (abs(dist_min) <= adist) then

     !          !fd ... on garde
 

     !          rough_CLALp(icdtac)%inode  = 1

     !          if (bavard) print*,'on garde le ppp ',1

     !          nb_rough_CLALp = nb_rough_CLALp + 1

     !          if (bavard) &
     !            write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

     !        else

     !          !fd ... on jete

     !          skip_iantac = rough_CLALp(icdtac)%ialpxx
     !          rough_CLALp(icdtac)%ialpxx  = 0
     !          rough_CLALp(icdtac)%inode   = 0
     !          rough_CLALp(icdtac)%isee    = 0
     !          rough_CLALp(icdtac)%adist   = 0.d0

     !          !print*,'skip'

     !        endif

     !        exit

     !      endif

     !    else if ( inode_min == get_nb_node_ALpxx(iantac)) then

     !      !xx
     !      !print*,'right'

     !      an_n= ALp(iantac)%normal(:,inode_min-1)
     !      pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

     !      ! face licite ?

     !      if (pscal < -1.d-03) then

     !        !xx
     !        !print*,'on se voit'


     !        pscal =  ( (ALp(iantac)%coor(1,inode_min) - ALp(iantac)%coor(1,inode_min-1)) &
     !                  *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode_min-1)) ) &
     !                +( (ALp(iantac)%coor(2,inode_min) - ALp(iantac)%coor(2,inode_min-1)) &
     !                  *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode_min-1)) )

     !        if (is_first) then
     !          is_first = .false.
     !          if (pscal >= 0.d0) then
     !            is_plus = .true.
     !          else
     !            is_plus = .false.
     !          endif
     !        end if

     !        !xx
     !        !print*,is_first,is_plus,pscal


     !        if (.not. is_plus) then
     !          if (pscal <  0.d0) then

     !            !xx
     !            !print*,'on part sens -'

     !            inode = inode_min - 1
 
     !            dist = dsqrt((CLcoor(1,icdtac)-ALp(iantac)%coor(1,inode))**2 + &
     !                         (CLcoor(2,icdtac)-ALp(iantac)%coor(2,inode))**2   )

     !            if ( (dist - dist_min) < -1.d-07) then

     !              if (bavard) then
     !                print*,dist,'<',dist_min
     !                print*,'on change de ppp ',inode_min,'->',inode
     !              endif

     !              inode_min = inode
     !              dist_min=dist
     !              cycle
     !            else

     !              if (abs(dist_min) <= adist) then

     !                rough_CLALp(icdtac)%inode  = inode_min

     !                if (bavard) &
     !                  print*,'on garde le ppp ',inode_min
  
     !                nb_rough_CLALp = nb_rough_CLALp + 1

     !                if (bavard) &
     !                  write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

     !              else

     !                skip_iantac = rough_CLALp(icdtac)%ialpxx
     !                rough_CLALp(icdtac)%ialpxx  = 0
     !                rough_CLALp(icdtac)%inode   = 0
     !                rough_CLALp(icdtac)%isee    = 0
     !                rough_CLALp(icdtac)%adist   = 0.d0

     !                !print*,'skip'

     !              endif

     !            endif

     !            exit

     !          else

     !            if (abs(dist_min) <= adist) then

     !              rough_CLALp(icdtac)%inode  = inode_min

     !              if (bavard) &
     !                print*,'on garde le ppp ',inode_min
  
     !              nb_rough_CLALp = nb_rough_CLALp + 1

     !              if (bavard) &
     !                write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

     !            else

     !              skip_iantac = rough_CLALp(icdtac)%ialpxx
     !              rough_CLALp(icdtac)%ialpxx  = 0
     !              rough_CLALp(icdtac)%inode   = 0
     !              rough_CLALp(icdtac)%isee    = 0
     !              rough_CLALp(icdtac)%adist   = 0.d0

     !              !print*,'skip'

     !            endif

     !            exit

     !          endif

     !        else
     !           ! on ne peut pas avancer

     !          if (abs(dist_min) <= adist) then

     !            rough_CLALp(icdtac)%inode  = inode_min

     !            if (bavard) print*,'on garde le ppp ',inode_min

     !            nb_rough_CLALp = nb_rough_CLALp + 1

     !            if (bavard) &
     !              write(*,'(A,2(1x,D12.5))') 'normale an ',an_n


     !          else

     !            if (bavard) print*,'on perd le noeud',pscal
     !            skip_iantac = rough_CLALp(icdtac)%ialpxx
     !            rough_CLALp(icdtac)%ialpxx  = 0
     !            rough_CLALp(icdtac)%inode   = 0
     !            rough_CLALp(icdtac)%isee    = 0
     !            rough_CLALp(icdtac)%adist   = 0.d0

     !            !print*,'skip'
     !          endif

     !          exit

     !        endif

     !      else

     !        !print*,'stop'

     !        ! face non licite

     !        if (is_first .or. is_plus) then
     !          !fd ... on jete

     !          skip_iantac = rough_CLALp(icdtac)%ialpxx
     !          rough_CLALp(icdtac)%ialpxx  = 0
     !          rough_CLALp(icdtac)%inode   = 0
     !          rough_CLALp(icdtac)%isee    = 0
     !          rough_CLALp(icdtac)%adist   = 0.d0

     !          !print*,'skip'
     !
     !          exit

     !        endif

     !        if (abs(dist_min) <= adist) then

     !          !fd ... on garde
 
     !          rough_CLALp(icdtac)%inode  = inode_min-1

     !          if (bavard) print*,'on garde le ppp ',inode_min-1

     !          nb_rough_CLALp = nb_rough_CLALp + 1

     !          if (bavard) &
     !            write(*,'(A,2(1x,D12.5))') 'normale an ',an_n

     !        else

     !          !fd ... on jete

     !          skip_iantac = rough_CLALp(icdtac)%ialpxx
     !          rough_CLALp(icdtac)%ialpxx  = 0
     !          rough_CLALp(icdtac)%inode   = 0
     !          rough_CLALp(icdtac)%isee    = 0
     !          rough_CLALp(icdtac)%adist   = 0.d0

     !          !print*,'skip'


     !        endif

     !        exit

     !      endif

     !    endif
     !  end do
     !endif

     !
     ! cas ou on ne voit rien on scanne tout ce qui existe pour
     ! trouver le noeud le plus proche et qui est contenu dans le contour des ialpxx
     !

     !if (rough_CLALp(icdtac)%ialpxx == 0) then

       !rm : need that ?
       rough_CLALp(icdtac)%ialpxx = 0

       rough_CLALp(icdtac)%inode  = 0
       rough_CLALp(icdtac)%isee=0
       rough_CLALp(icdtac)%adist=0.d0

       dist_min=1.d+20
       gap_min=1.d+20
       ialpxx_min=0
       inode_min =0

       do iantac=1,nb_ALpxx

         if (iantac == skip_iantac) cycle 
         if (.not.get_visible_ALpxx(iantac)) cycle

         if (is_nonsymmetric_detection .and. alpxx2bdyty(1,iantac) > clxxx2bdyty(1,icdtac))  cycle

         if (bavard) print*, 'iantac ',iantac

         ancol=get_color_MAILx(alpxx2bdyty(1,iantac),alpxx2bdyty(2,iantac))

         isee=get_isee('MAILx','CLxxx',cdcol,'MAILx','ALpxx',ancol)

         !
         ! on essaie de rejeter les noeuds d'un meme solide qui sont sur des cotes
         ! differents. Par convention les noeuds d'un meme solide qui ont les memes 
         ! couleurs ne se voient pas !!  
         !  
     
         if (l_clxxx(icdtac)%ibdyty == l_alpxx(iantac)%ibdyty .and. cdcol == ancol ) then

           ! print*,'Warning: faux contact'
           ! print*,icdtac,iantac,'appartiennent au meme corps',l_clxxx(icdtac)%ibdyty ,l_alpxx(iantac)%ibdyty
           ! print*,'avec les memes couleurs',cdcol,ancol,' donc on vire'

           isee = 0

         endif

         if (isee == 0) cycle

         dist_l_min=1.d+20
         gap_l_min=1.d+20
         inode_l_min=0
       

         do inode=1,get_nb_node_ALpxx(iantac)

           dist = dsqrt((CLcoor(1,icdtac)-ALp(iantac)%coor(1,inode))**2 + &
                        (CLcoor(2,icdtac)-ALp(iantac)%coor(2,inode))**2) 

           !fd on evacue les contacteurs trop loins

           if (see(isee)%global_alert /= 0.d0) then
              if (dist > see(isee)%global_alert) cycle
           endif
 
           !fd on ne refait pas ce calcul si on est sur le dernier noeud 

           if (inode < get_nb_node_ALpxx(iantac)) then
             !cd_n= get_normalTT_CLxxx(icdtac)
             an_n= ALp(iantac)%normal(:,inode) 
            
             pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)
           endif

           if ((dist - dist_l_min) < -1.d-07 .and. pscal < -1.d-03 ) then
             dist_l_min=dist
             inode_l_min=inode
           endif
 
         enddo

         ! 
         ! on regarde si les facettes adjacentes sont licites
         !         

         if (inode_l_min /= 0) then

           if (bavard) &
             print*,' dist min ',dist_l_min, ' with node ', inode_l_min

           if (inode_l_min == 1) then
  
             is_good = .false.

             ! arete licite ?
             !cd_n= get_normalTT_CLxxx(icdtac)
             an_n= ALp(iantac)%normal(:,inode_l_min) 
             pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

             if (pscal < -1d-3) then    

               ! ok licite

               ! sens + 

               pscal =  ( (ALp(iantac)%coor(1,inode_l_min+1) - ALp(iantac)%coor(1,inode_l_min)) &
                          *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode_l_min)) ) &
                          +( (ALp(iantac)%coor(2,inode_l_min+1) - ALp(iantac)%coor(2,inode_l_min)) &
                          *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode_l_min)) )

               if (bavard) print*, pscal, ' NOK < 0 > OK ' 

               if ( pscal  >= 0.d0 ) then
                 xa=ALp(iantac)%coor(1,1)
                 ya=ALp(iantac)%coor(2,1)
                 xb=ALp(iantac)%coor(1,2)
                 yb=ALp(iantac)%coor(2,2)

                 is_good = .true.

               endif
             endif

             if ( .not. is_good) then

               ! on tombe peut etre avant le premier noeud car boucle fermee ?

               if (((ALp(iantac)%coor(1,get_nb_node_ALpxx(iantac)) - ALp(iantac)%coor(1,inode_l_min))**2 + &
                    (ALp(iantac)%coor(2,get_nb_node_ALpxx(iantac)) - ALp(iantac)%coor(2,inode_l_min))**2 ) < 1d-14) then

                 ! ok boucle fermee

                 ! arete licite ?

                 !cd_n= get_normalTT_CLxxx(icdtac)
                 an_n= ALp(iantac)%normal(:,get_nb_node_ALpxx(iantac) - 1) 
                 pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

                 if (pscal < -1d-3) then    

                   ! ok licite
 
                   inode_l_min = get_nb_node_ALpxx(iantac)

                   xa=ALp(iantac)%coor(1,get_nb_node_ALpxx(iantac) - 1)
                   ya=ALp(iantac)%coor(2,get_nb_node_ALpxx(iantac) - 1)
                   xb=ALp(iantac)%coor(1,get_nb_node_ALpxx(iantac))
                   yb=ALp(iantac)%coor(2,get_nb_node_ALpxx(iantac))

                   is_good = .true.

                 endif
               endif
             endif 

             if ( .not. is_good) cycle            

           else if (inode_l_min == get_nb_node_ALpxx(iantac)) then

             is_good = .false.

             ! arete licite ?
             !cd_n= get_normalTT_CLxxx(icdtac)
             an_n= ALp(iantac)%normal(:,inode_l_min - 1) 
             pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

             if (pscal < -1d-3) then    

               pscal =  ( (ALp(iantac)%coor(1,inode_l_min-1) - ALp(iantac)%coor(1,inode_l_min)) &
                          *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode_l_min)) ) &
                         +( (ALp(iantac)%coor(2,inode_l_min-1) - ALp(iantac)%coor(2,inode_l_min)) &
                          *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode_l_min)) )


               if (bavard) print*, pscal, ' NOK < 0 > OK ' 

               if (pscal >= 0.d0) then
                 xa=ALp(iantac)%coor(1,get_nb_node_ALpxx(iantac) - 1)
                 ya=ALp(iantac)%coor(2,get_nb_node_ALpxx(iantac) - 1)
                 xb=ALp(iantac)%coor(1,get_nb_node_ALpxx(iantac))
                 yb=ALp(iantac)%coor(2,get_nb_node_ALpxx(iantac))

                 is_good = .true.

               endif
             endif

             if (.not. is_good) then

               ! on tombe peut etre apres le dernier noeud car boucle fermee ?

               if (((ALp(iantac)%coor(1,get_nb_node_ALpxx(iantac)) - ALp(iantac)%coor(1,inode_l_min))**2 + &
                    (ALp(iantac)%coor(2,get_nb_node_ALpxx(iantac)) - ALp(iantac)%coor(2,inode_l_min))**2 ) < 1d-14) then

                 ! ok boucle fermee

                 ! arete licite ?

                 !cd_n= get_normalTT_CLxxx(icdtac)
                 an_n= ALp(iantac)%normal(:,1) 
                 pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

                 if (pscal < -1d-3) then    

                   ! ok licite

                   inode_l_min = 1
  
                   xa=ALp(iantac)%coor(1,1)
                   ya=ALp(iantac)%coor(2,1)
                   xb=ALp(iantac)%coor(1,2)
                   yb=ALp(iantac)%coor(2,2)

                   is_good = .true.

                 endif
               endif
             endif

             if ( .not. is_good) cycle            

           else

             is_good = .false.

             ! arete licite ?
             !cd_n= get_normalTT_CLxxx(icdtac)
             an_n= ALp(iantac)%normal(:,inode_l_min) 
             pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

             if (pscal < -1e-3) then    

               pscal =  ( (ALp(iantac)%coor(1,inode_l_min+1) - ALp(iantac)%coor(1,inode_l_min)) &
                          *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode_l_min)) ) &
                        +( (ALp(iantac)%coor(2,inode_l_min+1) - ALp(iantac)%coor(2,inode_l_min)) &
                          *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode_l_min)) )

               if (bavard) print*,pscal 

               if (pscal >= 0.d0) then
                 xa=ALp(iantac)%coor(1,inode_l_min)
                 ya=ALp(iantac)%coor(2,inode_l_min)
                 xb=ALp(iantac)%coor(1,inode_l_min+1)
                 yb=ALp(iantac)%coor(2,inode_l_min+1)
           
                 is_good = .true.

               endif
             endif  

             if (.not. is_good) then

               !cd_n= get_normalTT_CLxxx(icdtac)
               an_n= ALp(iantac)%normal(:,inode_l_min - 1) 
               pscal = an_n(1)*cd_n(1) + an_n(2)*cd_n(2)

               if (pscal < -1d-3) then    

                 xa=ALp(iantac)%coor(1,inode_l_min-1)
                 ya=ALp(iantac)%coor(2,inode_l_min-1)
                 xb=ALp(iantac)%coor(1,inode_l_min)
                 yb=ALp(iantac)%coor(2,inode_l_min)

                 is_good = .true.

               endif
             endif

             if ( .not. is_good) cycle            

           endif 

           if (bavard) &
              write(*,'(A,2(1x,D12.5))') 'normale an ',an_n  


           AB = DSQRT((xa-xb)**2+(ya-yb)**2) 
  
           UN1 = -(yb-ya)/AB
           UN2 =  (xb-xa)/AB 

           xp=CLcoor(1,icdtac)
           yp=CLcoor(2,icdtac)

           !fd 15/10/08 new - un oubli regretable

           AP = DSQRT((xa-xp)**2+(ya-yp)**2) 

           if (AP > AB) cycle
           !df

           gap_l_min = abs(((xp-xa)*UN1) + ((yp-ya)*UN2))

           !fd if ((gap_l_min - gap_min) < -1.d-07 .and. gap_l_min <= 3.*see(isee)%alert) then

           if ((gap_l_min - gap_min) < -1.d-07 .and. gap_l_min <= see(isee)%alert) then

             if (dist_l_min - dist_min < 1d-03) then 

               ! print*,gap_l_min, gap_min
               ! print*,see(isee)%alert
               ! print*,dist_l_min,dist_min
               ! print*,xa,ya,xb,yb
               ! print*,ALp(iantac)%coor(1,inode_l_min),ALp(iantac)%coor(2,inode_l_min)
               ! print*,CLcoor(1,icdtac),CLcoor(2,icdtac)
               ! print*,UN1,UN2

               dist_min=dist_l_min
               gap_min=gap_l_min
               rough_CLALp(icdtac)%ialpxx=iantac
               rough_CLALp(icdtac)%inode=inode_l_min
               rough_CLALp(icdtac)%adist=see(isee)%alert
               rough_CLALp(icdtac)%isee=isee
             endif
           endif
         endif

       enddo


       if (rough_CLALp(icdtac)%ialpxx /= 0) nb_rough_CLALp = nb_rough_CLALp + 1

     !endif

     if (bavard) then
       print*,'=========predetection==================='
       print*,'Le candidat ',icdtac
       print*,'qui est sur le corps', l_clxxx(icdtac)%ibdyty
       if (rough_CLALp(icdtac)%ialpxx == 0) then
         print*,'ne voit rien'
       else
         print*,'voit la ligne:',rough_CLALp(icdtac)%ialpxx, &
                'point le plus proche de rang:',rough_CLALp(icdtac)%inode
         print*,'qui est sur le corps:',l_alpxx(rough_CLALp(icdtac)%ialpxx)%ibdyty
         !if (gap_min == 1.d+20) then
         !  print*,'contact conserve sans recalculer le gap'
         !else
         !  print*,'avec un gap de',gap_min
         !endif
       endif
       print*,'=========fin predetection==============='
     end if

   end do

   write(cout,'(4X,I10,A20)') nb_rough_CLALp,' CLALp roughly found'
   call logmes(cout)

   if (allocated(this)) deallocate(this)
   allocate(this(nb_rough_CLALp),stat=errare)
   if (errare /=0 ) then
     call faterr(IAM,'error in allocating this, in select_prox_tactors in mod_CLALp')
   end if 

 end subroutine creation_tab_visu_CLALp

 !------------------------------------------------------------------------
 subroutine compute_contact_CLALp
   implicit none  
   integer(kind=4)   :: errare
   integer(kind=4)   :: icdan, iadj, icdtac, iantac
   integer(kind=4)   :: ialxxx, inode, i_vert, i_vert2
   integer(kind=4)   :: i4_input(4), i4_output(4)
   real(kind=8)      :: r8_output(8), cd_Vbegin(2), an_Vbegin(2), v1(2), v2(2), pscal
   character(len=31) :: cout
   logical :: to_keep

   logical :: all_dof_cd, all_dof_an

   nb_CLxxx=get_nb_CLxxx()
   nb_ALpxx=get_nb_ALpxx()

   icdan=0

   do icdtac=1,nb_CLxxx
     nb_adj(icdtac)=0
   end do

   do icdtac=1,nb_CLxxx     
     if (.not.get_visible_CLxxx(icdtac)) cycle

     iantac=rough_CLALp(icdtac)%ialpxx

     if (iantac == 0) cycle
     if (.not. get_visible_ALpxx(iantac)) cycle

     ! ugly check to know what second vertex of ALp to check
     i_vert = rough_CLALp(icdtac)%inode

     if (i_vert == 1) then 
       i_vert2 = i_vert + 1
     else if (i_vert == get_nb_node_ALpxx(iantac)) then
       i_vert2 = i_vert - 1
     else
       v1 = ALp(iantac)%coor(:,i_vert+1) - ALp(iantac)%coor(:,i_vert)
       v2 = CLcoor(:,icdtac) - ALp(iantac)%coor(:,i_vert)
       pscal = dot_product( v1, v2)
       if (pscal > 0.d0) then
         i_vert2 = i_vert + 1
       else 
         i_vert2 = i_vert - 1
       end if
     end if
     all_dof_cd = all_dof_driven_CLxxx(icdtac)
    
     all_dof_an = all_dof_driven_ALxxx(iantac, i_vert, i_vert2)

     if( all_dof_cd .and. all_dof_an ) cycle

     i4_input(1) = icdtac
     i4_input(2) = iantac
     i4_input(3) = rough_CLALp(icdtac)%inode
     i4_input(4) = rough_CLALp(icdtac)%isee

     call compute_one_contact_CLALp(i4_input, i4_output, r8_output, to_keep)

     if( .not. to_keep ) cycle

     icdan = icdan + 1

     this(icdan)%icdent = get_ENT_CLxxx(icdtac)
     this(icdan)%ianent = get_ENT_ALpxx(iantac)

     this(icdan)%icdtac = i4_input(1)
     this(icdan)%iantac = i4_input(2)
     this(icdan)%isee   = i4_input(4)

     this(icdan)%icdbtac = clxxx2bdyty(2, i4_input(1))
     this(icdan)%ianbtac = alpxx2bdyty(2, i4_input(2))

     this(icdan)%icdbtyp = clxxx2bdyty(3, i4_input(1))
     this(icdan)%ianbtyp = alpxx2bdyty(3, i4_input(2))
     this(icdan)%icdctyp = i_clxxx
     this(icdan)%ianctyp = i_alpxx

     this(icdan)%iadj   = i4_output(1)
     this(icdan)%icdbdy = i4_output(2)
     this(icdan)%iansci = 0
     this(icdan)%ianbdy = i4_output(3)
     this(icdan)%iansci = i4_output(4)

     this(icdan)%nuc(1)     = r8_output(2)
     this(icdan)%nuc(2)     = r8_output(3)
     this(icdan)%tuc(1)     = r8_output(4)
     this(icdan)%tuc(2)     = r8_output(5)
     this(icdan)%gapTTBEGIN = r8_output(1)
     this(icdan)%cpcd       = r8_output(6)

     call get_vlocy_CLxxx(icdtac,iVbeg_,cd_Vbegin)
     call get_vlocy_ALpxx(iantac,i4_output(4),iVbeg_,an_Vbegin,r8_output(6))

     this(icdan)%vltBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) + &
                            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2)
     this(icdan)%vlnBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) + &
                            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2) 

     this(icdan)%rlt         = 0.d0
     this(icdan)%rln         = 0.d0
     this(icdan)%vlt         = this(icdan)%vltBEGIN
     this(icdan)%vln         = this(icdan)%vlnBEGIN
     this(icdan)%gapTT       = this(icdan)%gapTTBEGIN
     this(icdan)%status      = i_nknow
     this(icdan)%statusBEGIN = i_nknow

     this(icdan)%coor(1) = r8_output(7)
     this(icdan)%coor(2) = r8_output(8)

     ! Since selection of candidates for contact has been refined, nb_adj(icdtac) is less or equal than size(adjac(icdtac)%icdan).
     ! Loops are now to run from 1 to nb_adj(icdtac) (where data are available).
     if (this(icdan)%iadj > size(adjac(icdtac)%icdan)) then
       write(cout,'(A)') 'extra adjacent contacts found when refining !'
       call faterr('CLALp::compute_contact',cout)
     end if

     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan

   end do

   nb_CLALp = icdan

   write(cout,'(1X,I10,A12)') nb_CLALp,' CLALp found'
   call logmes(cout)

  ! Since selection of candidates for contact has been refined, nb_CLALp is less or equal size(this).
  ! Loops are now to run from 1 to nb_CLALp where data are available.

   do icdan = 1, nb_CLALp
      call get_behaviour_( icdan, see, tact_behav )
   end do

   if (allocated(violation)) deallocate(violation)
   allocate(violation(nb_CLALp),stat=errare)

   ! this exists
   RUN=.TRUE.
   
 end subroutine compute_contact_CLALp


 !------------------------------------------------------------------------
 !> \brief Compute one contact from a CLALp rough
 subroutine compute_one_contact_CLALp(i4_input, i4_output, r8_output, to_keep)
   implicit none
   !> integer inputs
   integer(kind=4), dimension(4), intent(in)  :: i4_input
   !> integer outputs
   integer(kind=4), dimension(4), intent(out) :: i4_output
   !> real outputs
   real(kind=8)   , dimension(8), intent(out) :: r8_output
   !> is rough interaction to keep
   logical, intent(out) :: to_keep
   !
   integer(kind=4) :: icdtac, iantac, inode, ialxxx, isee, cd_ent, an_ent
   integer(kind=4) :: i_cpcd, ial_s1, ial_s2, cmp_inode
   real(kind=8)    :: pscal, adist, gap, cpcd, XP2, YP2, r_cpcd
   real(kind=8)    :: coordcd(2), ut1, ut2, un1, un2, xl1, xl2, norml, gapl, unl1, unl2
   real(kind=8), dimension(2,2) :: coordan
   ! tol to assume we are at the extremity of an edge
   real(kind=8)    :: ntol = 1e-5   

   to_keep = .false.

   icdtac = i4_input(1)
   iantac = i4_input(2)
   inode  = i4_input(3)
   isee   = i4_input(4)

   if (inode == 1) then 
     ialxxx = 1
   else if (inode == get_nb_node_ALpxx(iantac)) then
     ialxxx = inode - 1
   else
     pscal =  ( (ALp(iantac)%coor(1,inode+1) - ALp(iantac)%coor(1,inode)) &
               *(CLcoor(1,icdtac) - ALp(iantac)%coor(1,inode)) )          &
             +( (ALp(iantac)%coor(2,inode+1) - ALp(iantac)%coor(2,inode)) &
               *(CLcoor(2,icdtac) - ALp(iantac)%coor(2,inode)) )

     if (pscal >= 0.d0) then
       ialxxx = inode
     else
       ialxxx = inode-1 
     end if

   endif

   coordan(1:2,1) = ALp(iantac)%coor(1:2,ialxxx)
   coordan(1:2,2) = ALp(iantac)%coor(1:2,ialxxx+1)
   coordcd(1:2)   = CLcoor(1:2,icdtac)

   adist = see(isee)%alert

   call local_framing(coordcd(1),coordcd(2), coordan(1,1),coordan(2,1), &
                      coordan(1,2),coordan(2,2), ut1,ut2,un1,un2,gap,cpcd,XP2,YP2)

   ! checking distance against alert distance           
   if (gap > adist) return

   ! checking against trimming 
   if ( trim_contact .and. &
       ((ialxxx == 1 .and. cpcd < ntol) .or. &
        (ialxxx == get_nb_node_ALpxx(iantac) - 1 .and. cpcd > 1.d0 - ntol))) return

   to_keep = .true.

   nb_adj(icdtac) = nb_adj(icdtac) + 1

   i4_output(1) = nb_adj(icdtac)
   i4_output(2) = l_clxxx(icdtac)%ibdyty
   i4_output(3) = l_alpxx(iantac)%ibdyty
   i4_output(4) = ialxxx

   cd_ent = get_ent_CLxxx(icdtac)
   an_ent = get_ent_ALpxx(iantac) 

   if (i4_output(2) /= i4_output(3)) then
     entity(cd_ent)%nb = entity(cd_ent)%nb+1
     entity(an_ent)%nb = entity(an_ent)%nb+1
   else
     ! on ne se compte pas 2 fois
     entity(cd_ent)%nb = entity(cd_ent)%nb+1
   endif 

   ! contact standard
   if ((cpcd >= ntol) .and. ((cpcd - 1.d0) <= -ntol)) then
     if (bavard) then 
       print*,'==================================================================='
       print*,'candidat: ',icdtac,'corps: ',l_clxxx(icdtac)%ibdyty,clxxx2bdyty(2,icdtac)
       print*,'antagoniste: ',iantac,'corps: ',l_ALpxx(iantac)%ibdyty,clxxx2bdyty(2,iantac)
       print*,'numnoda:',l_ALpxx(iantac)%idata(ialxxx)
       print*,'numnodb:',l_ALpxx(iantac)%idata(ialxxx+1)
       print*,'p1',coordcd(1),coordcd(2)
       print*,'pc',coordan(1,1),coordan(2,1)
       print*,'pd',coordan(1,2),coordan(2,2)
       print*,'p2',xp2,yp2
       print*,'n ',un1,un2
       print*,'cpcd',cpcd,'gap',gap,'adist',adist
       print*,'==================================================================='
     endif

     r8_output(1) = gap
     r8_output(2) = un1
     r8_output(3) = un2
     r8_output(4) = ut1
     r8_output(5) = ut2
     r8_output(6) = cpcd
                
     r8_output(7) = xp2
     r8_output(8) = yp2

   ! contact dans le cas ou on est dans le cone pour une surface convexe
   else
     !fd on est dans le cas ou le noeud est tout comme il faut
     !fd mais il se projete en dehors de la face, 
     !fd on le ramene sur le noeud

     !fd pour la normale on a plusieurs choix
     !fd 1/ on prend le vecteur directeur qui relie an1 a cd 
     !fd 2/ si les points sont trop proches on prend la moyenne des normales
     !fd 3/ si les points sont trop proches et qu'on est au bout on garde la normale

     if (cpcd < ntol) then
       if (bavard) print*,'on est dans le cone gauche'
       r_cpcd = 0.d0
       i_cpcd = 1
       ial_s1 =-1
       ial_s2 = 0
       cmp_inode = 1
     else if (cpcd > 1.d0 - ntol) then
       if (bavard) print*,'on est dans le cone droit'
       r_cpcd = 1.d0
       i_cpcd = 2
       ial_s1 = 1
       ial_s2 = 2
       cmp_inode = get_nb_node_ALpxx(iantac)
     end if
       
     !fd on calcul le vecteur norme qui va de an1 a cd
 
     xl1 = coordcd(1) - coordan(1,i_cpcd)
     xl2 = coordcd(2) - coordan(2,i_cpcd)

     norml = dsqrt(xl1**2+xl2**2) 
     if (norml > adist * 1.d-3) then
       !fd cas 1
       if (bavard) print*,'cas 1'

       unl1 = xl1/norml 
       unl2 = xl2/norml 
         
       !fd on verifie si il est rentrant ou sortant de la matiere
       !fd en faisant le produit scalaire avec la normale a la facette active         
       pscal = un1*unl1 + un2*unl2

       !fd si il est negatif on le tourne
       if (pscal < 0) then
         unl1 = -unl1 
         unl2 = -unl2
       end if

       !fd le gap est la distance projetee sur la norme
       gapl = xl1*unl1 + xl2*unl2

     else
       if (inode == cmp_inode) then  ! .or. inode == get_nb_node_ALpxx(iantac)) then 

         !fd cas 3 on garde la normale de la face
         if (bavard) print*,'cas 3'

         unl1=un1
         unl2=un2
         gapl=gap

       else
         !fd cas 2 on fait la moyenne      

         if (bavard) print*,'cas 2'

         coordan(1:2,1) = ALp(iantac)%coor(1:2,ialxxx+ial_s1)
         coordan(1:2,2) = ALp(iantac)%coor(1:2,ialxxx+ial_s2)

         xl1 = coordan(1,2) - coordan(1,1)
         xl2 = coordan(2,2) - coordan(2,1)

         norml = dsqrt(xl1**2+xl2**2) 

         unl1 = (un1 - (xl2/norml))*0.5 
         unl2 = (un2 + (xl1/norml))*0.5 

         norml = dsqrt(unl1**2+unl2**2) 

         unl1 = unl1/norml
         unl2 = unl2/norml

         coordan(1:2,1) = ALp(iantac)%coor(1:2,ialxxx)
         coordan(1:2,2) = ALp(iantac)%coor(1:2,ialxxx+1)

         xl1 = coordcd(1) - coordan(1,i_cpcd)
         xl2 = coordcd(2) - coordan(2,i_cpcd)

         gapl = xl1*unl1 + xl2*unl2

       end if
     end if

     r8_output(1) = gapl
     r8_output(2) = unl1
     r8_output(3) = unl2
     r8_output(4) = unl2
     r8_output(5) =-unl1
     r8_output(6) = r_cpcd
     r8_output(7) = ( coordcd(1)+coordan(1,i_cpcd) ) * 0.5
     r8_output(8) = ( coordcd(2)+coordan(2,i_cpcd) ) * 0.5

     if (bavard) then
       print*,'==================================================================='
       print*,'cas degenere'
       print*,'candidat: ',icdtac,'corps: ',l_clxxx(icdtac)%ibdyty,clxxx2bdyty(2,icdtac)
       print*,'antagoniste: ',iantac,'corps: ',l_ALpxx(iantac)%ibdyty,clxxx2bdyty(2,iantac)
       print*,'numnoda:',l_ALpxx(iantac)%idata(ialxxx)
       print*,'numnodb:',l_ALpxx(iantac)%idata(ialxxx+1)
       print*,'p1',coordcd(1),coordcd(2)
       print*,'pc',coordan(1,1),coordan(2,1)
       print*,'pd',coordan(1,2),coordan(2,2)
       print*,'local framing :'
       print*,'cpcd',cpcd,'gap',gap,'adist',adist
       print*,'n ',un1,un2
       print*,'apres correction :'
       print*,'cpcd',r8_output(6),'gap',r8_output(1)
       print*,'n ',r8_output(2),r8_output(3)
       print*,'==================================================================='
     end if

   end if

 end subroutine compute_one_contact_CLALp

 !------------------------------------------------------------------------
 subroutine external_detection_CLALp
   implicit none
   integer           :: icdan , itact
   integer           :: icdbdy, icdtac, icdtact
   integer           :: ianbdy, iantac, iantact
   integer           :: ialxxx, inode, isee
   integer           :: i4_input(4), i4_output(4)
   real(kind=8)      :: r8_output(8), cd_Vbegin(2), an_Vbegin(2)
   character(len=5)  :: cdcol, ancol
   character(len=45) :: cout
   character(len=25), parameter :: IAM = 'CLALp::external_detection'
   logical :: to_keep

   nb_CLALp = externalDetection_get_nb()

   if( nb_CLALp < 1 ) return

   ! allocating this only if not large enough
   if( allocated(this) ) then
     !if( size(this) < nb_CLALp ) deallocate(this)
     deallocate(this)
   end if

   !if( .not. allocated(this) ) allocate(this(nb_CLALp))
   allocate(this(nb_CLALp))

   do icdtac = 1, nb_CLxxx
     nb_adj(icdtac) = 0
   end do

   do icdan = 1, nb_CLALp

     call externalDetection_get_contact(icdan, icdbdy, icdtact, ianbdy, iantact, inode)

     icdtac = 0
     do itact = 1, nb_CLxxx
       if( clxxx2bdyty(1,itact) == icdbdy .and. clxxx2bdyty(2,itact) == icdtact ) then
         icdtac = itact
         exit
       end if
     end do

     iantac = 0
     do itact = 1, nb_ALpxx
       if( alpxx2bdyty(1,itact) == ianbdy .and. alpxx2bdyty(2,itact) == iantact ) then
         iantac = itact
         exit
       end if
     end do

     if( icdtac == 0 ) call faterr( IAM, 'support candidate not found !')
     if( iantac == 0 ) call faterr( IAM, 'support antagonist not found !')

     if( .not. get_visible_CLxxx(icdtac) ) cycle
     if( .not. get_visible_ALpxx(iantac) ) cycle

     cdcol = get_color_MAILx( clxxx2bdyty(1,icdtac), clxxx2bdyty(2,icdtac) )
     ancol = get_color_MAILx( alpxx2bdyty(1,iantac), alpxx2bdyty(2,iantac) )

     isee  = get_isee('MAILx', 'CLxxx', cdcol, 'MAILx', 'ALpxx', ancol)

     i4_input(1) = icdtac
     i4_input(2) = iantac
     i4_input(3) = inode
     i4_input(4) = isee

     call compute_one_contact_CLALp(i4_input, i4_output, r8_output, to_keep)

     if( .not. to_keep ) cycle

     !icdan = icdan + 1
     this(icdan)%icdent = get_ENT_CLxxx(icdtac)
     this(icdan)%ianent = get_ENT_ALpxx(iantac)


     this(icdan)%icdtac = i4_input(1)
     this(icdan)%iantac = i4_input(2)
     this(icdan)%isee   = i4_input(4)

     this(icdan)%iadj   = i4_output(1)
     this(icdan)%icdbdy = i4_output(2)
     this(icdan)%ianbdy = i4_output(3)
     this(icdan)%iansci = i4_output(4)

     this(icdan)%nuc(1)     = r8_output(2)
     this(icdan)%nuc(2)     = r8_output(3)
     this(icdan)%tuc(1)     = r8_output(4)
     this(icdan)%tuc(2)     = r8_output(5)
     this(icdan)%gapTTBEGIN = r8_output(1)
     this(icdan)%cpcd       = r8_output(6)

     call get_vlocy_CLxxx(icdtac,iVbeg_,cd_Vbegin)
     call get_vlocy_ALpxx(iantac,i4_output(4),iVbeg_,an_Vbegin,r8_output(6))

     this(icdan)%vltBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) + &
                            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2)
     this(icdan)%vlnBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) + &
                            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2)

     this(icdan)%rlt         = 0.d0
     this(icdan)%rln         = 0.d0
     this(icdan)%vlt         = this(icdan)%vltBEGIN
     this(icdan)%vln         = this(icdan)%vlnBEGIN
     this(icdan)%gapTT       = this(icdan)%gapTTBEGIN
     this(icdan)%status      = i_nknow
     this(icdan)%statusBEGIN = i_nknow

     this(icdan)%coor(1) = r8_output(7)
     this(icdan)%coor(2) = r8_output(8)

     ! Since selection of candidates for contact has been refined, nb_adj(icdtac) is less or equal than size(adjac(icdtac)%icdan).
     ! Loops are now to run from 1 to nb_adj(icdtac) (where data are available).
     if (this(icdan)%iadj > size(adjac(icdtac)%icdan)) then
       write(cout,'(A)') 'extra adjacent contacts found when refining !'
       call faterr(IAM,cout)
     end if

     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan

   end do


   write(cout,'(1X,I10,A12)') nb_CLALp,' CLALp found'
   call logmes(cout)

   ! Since selection of candidates for contact has been refined, nb_CLALp is less or equal size(this).
   ! Loops are now to run from 1 to nb_CLALp where data are available.

   do icdan = 1, nb_CLALp
     call get_behaviour_( icdan, see, tact_behav )
   end do

   if (allocated(violation)) deallocate(violation)
   allocate( violation(nb_CLALp) )

   RUN=.TRUE.   
   
 end subroutine external_detection_CLALp

 !------------------------------------------------------------------------ 
 subroutine display_prox_tactors_CLALp

   implicit none
   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact,nb_CLxxx
   character(len=5) :: cdmodel, anmodel

   if (.not. RUN) return
   
   nb_CLxxx=get_nb_CLxxx()

   do icdtact=1,nb_CLxxx    
     do iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       cdmodel = get_body_model_name_from_id( clxxx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( alpxx2bdyty(3,iantac) )
       write(*,'(A1)')' '
       write(*,'(A6,2X,I5)')'$icdan',icdan     
                      !123456789012345678901234567890123456789012345678901234567890123456789012
       write(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '   
       write(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,clxxx2bdyty(1,icdtac),'CLxxx',clxxx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,alpxx2bdyty(1,iantac),'ALpxx',alpxx2bdyty(2,iantac)
       write(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       write(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       write(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       write(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBegin
       write(*,'(A1)')' '               
     end do                           
   end do

104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_CLALp
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 subroutine stock_rloc_CLALp
 
   
   ! get data from this and put into verlt
   !           
 
   implicit none
   integer :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,nb_CLxxx

   integer :: errare

   character(len=80) :: cout
                              !123456789012345678912
   character(len=22) :: IAM = 'mod_CLALp::stock_rloc'

   nb_CLxxx=get_nb_CLxxx()

  ! sizing verlt:
   if (.not. allocated(verlt)) then
     allocate(verlt(nb_CLxxx),stat=errare)
     if (errare /=0 ) then
       call faterr(IAM,'Error allocating verlt')
     end if
     do icdtac=1,nb_CLxxx
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       else
         call nullify_verlet_(icdtac)
       end if
       if (errare /=0 ) then
         write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
         call faterr(IAM,cout)
       end if
     end do
   else 
     do icdtac=1,size(verlt)
       verlt(icdtac)%adjsz=0
       call free_verlet_(icdtac)
     end do

     if( size(verlt) < nb_CLxxx ) deallocate(verlt)

     allocate(verlt(nb_CLxxx),stat=errare)
     do icdtac=1,nb_CLxxx
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       else
         call nullify_verlet_(icdtac)
        end if
     end do
   end if

  ! filling data:
   do icdan=1,nb_CLALp
     icdtac = this(icdan)%icdtac              ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac              ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj                ! serial adjacent number of pair contactor 
                                              ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)    = icdan
     verlt(icdtac)%cdbdy          = clxxx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac          = clxxx2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel        = clxxx2bdyty(3,icdtac)
     verlt(icdtac)%cdsci          = this(icdan)%icdsci
     verlt(icdtac)%anbdy(iadj)    = alpxx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)    = alpxx2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)  = alpxx2bdyty(3,iantac)
     verlt(icdtac)%ansci          = this(icdan)%iansci
     verlt(icdtac)%rlt(iadj)      = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)      = this(icdan)%rln/H
     verlt(icdtac)%vlt(iadj)      = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)      = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)    = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)   = this(icdan)%status
     verlt(icdtac)%cdsci(iadj)    = 0
     verlt(icdtac)%ansci(iadj)    = this(icdan)%iansci
     verlt(icdtac)%nuc(1:2,iadj)  = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2,iadj) = this(icdan)%coor(1:2)
     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
   end do

   nb_vCLALp = nb_CLALp

   ! verlet available
   READ=.true.

   WRITE(cout,'(1X,I10,A12)') nb_vCLALp,' stock CLALp'
   call logmes(cout)

 end subroutine stock_rloc_CLALp

 !------------------------------------------------------------------------ 
 subroutine recup_rloc_CLALp
 
   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   implicit none

   integer :: info
   integer :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj

   integer ::  nb_stock_CLALp

   character(len=21) :: IAM = 'mod_CLALp::recup_rloc'
   character(len=31) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if
 
   nb_recup_CLALp=0
   nb_stock_CLALp=0
 
   do icdtac=1,size(verlt)

    nb_stock_CLALp = nb_stock_CLALp + verlt(icdtac)%adjsz 

   enddo

   do icdan=1,nb_CLALp
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac  ! serial number of candidate  contactor for contact icdan
     iantac = this(icdan)%iantac  ! serial number of antagonist contactor for contact icdan

     if (verlt(icdtac)%adjsz /= 0) then
       if( verlt(icdtac)%cdbdy  == clxxx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == clxxx2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== clxxx2bdyty(3,icdtac)       &
         ) then
         do iadj=1,verlt(icdtac)%adjsz

           if( verlt(icdtac)%anbdy(iadj)  == alpxx2bdyty(1,iantac) .and. &
               verlt(icdtac)%antac(iadj)  == alpxx2bdyty(2,iantac) .and. &
               verlt(icdtac)%anmodel(iadj)== alpxx2bdyty(3,iantac)       &
             ) then
               
             this(icdan)%rlt         = verlt(icdtac)%rlt(iadj)*H 
             this(icdan)%rln         = verlt(icdtac)%rln(iadj)*H 
             this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

             this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)

!fd             print*,icdan,icdtac,iantac
!fd             print*,clxxx2bdyty(1,icdtac),clxxx2bdyty(2,icdtac)
!fd             print*,alpxx2bdyty(1,iantac),alpxx2bdyty(2,iantac)

!fd
!fd completement stupide ... si on change de facette ca le desactive !! 
!fd
!fd             this(icdan)%iansci = verlt(icdtac)%ansci(iadj)

             nb_recup_CLALp = nb_recup_CLALp + 1
             exit
           end if
         end do
       end if
     end if
   end do
   
   write(cout,'(1X,I10,A12)') nb_recup_CLALp,' recup CLALp'
   call logmes(cout)

   ! assume that verlet not available
   READ=.false.
   ! assume that this is available
   RUN =.true.
   
 end subroutine recup_rloc_CLALp
!------------------------------------------------------------------------  

 !> Get data from verlet list verlt and put into this
 subroutine recup_rloc_by_position_CLALp(tol)
   implicit none
   !> max distance to accept identification
   real(kind=8), intent(in) :: tol
   !
   real(kind=8) :: dist
   integer(kind=4) :: icdan, icdtac, iadj, i_old, itree
   real(kind=8), dimension(:)  , pointer :: point
   real(kind=8), dimension(:,:), pointer :: verlt_coor
   integer     , dimension(:,:), pointer :: imap
   !
   character(len=33), parameter :: IAM = 'mod_CLALp::recup_rloc_by_position'
   character(len=31) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_CLALp = 0
   if( nb_vCLALp < 1 ) then
     do icdan=1,nb_CLALp

       this(icdan)%rlt = 0.d0
       this(icdan)%rln = 0.d0
       this(icdan)%statusBEGIN = i_nknow

     end do
     write(cout,'(1X,I10,A12)') nb_recup_CLALp,' recup CLALp'
     call logmes(cout)
     return
   end if

   call set_nb_kd(1)  

   allocate( verlt_coor(nbDIME, nb_vCLALp) )
   allocate( point(nbDIME) )

   ! map to link index in ann tree and candidate and adjacent id
   ! and if this interaction has already been got back
   allocate( imap(3, nb_vCLALp) )
   imap = 0

   itree = 0
   do icdtac = 1, min(nb_CLxxx, size(verlt))
     if (verlt(icdtac)%adjsz /= 0) then
       do iadj = 1, verlt(icdtac)%adjsz
         itree = itree + 1
         verlt_coor(1:2,itree) = verlt(icdtac)%coor(1:2,iadj)

         imap(1,itree) = icdtac
         imap(2,itree) = iadj
       end do
     end if
   end do

   call add_kd_tree(1, verlt_coor, nb_vCLALp, nbDIME)

   do icdan = 1, nb_CLALp

     this(icdan)%rlt = 0.d0
     this(icdan)%rln = 0.d0
     this(icdan)%statusBEGIN = i_nknow

     point(1:2) = this(icdan)%coor(1:2)
     call search_nearest_kd_tree(1, point, i_old, dist)

     ! check if nearest index found, and close enough
     ! AND if not already used by another interaction
     if( i_old>0 .and. imap(3,i_old) == 0 .and. dist<tol ) then

       icdtac = imap(1,i_old)
       iadj   = imap(2,i_old)

       imap(3,i_old) = 1

       this(icdan)%rlt         = verlt(icdtac)%rlt(iadj)*H
       this(icdan)%rln         = verlt(icdtac)%rln(iadj)*H
       this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

       this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)

       nb_recup_CLALp = nb_recup_CLALp + 1

     end if ! if ann found a nearest

   end do ! loop on this

   write(cout,'(1X,I10,A12)') nb_recup_CLALp,' recup CLALp'
   call logmes(cout)

   call ann_clean_memory()

   deallocate( verlt_coor )
   deallocate( point )
   deallocate( imap )

   ! assume that verlet not available
   READ=.false.
   ! assume that this is available
   RUN =.true.
   
 end subroutine recup_rloc_by_position_CLALp

 !------------------------------------------------------------------------ 
 subroutine read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   implicit none
   character(len=103) :: clin
   integer            :: icdan,icdbdy,icdtac,ianbdy,iantac,cdmodel,anmodel
   integer            :: iadj,icdtact,nb_CLxxx,nb_ALpxx
   real(kind=8)       :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2) :: nuc,coor
   character(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus

   integer            :: errare 

   integer :: ialxxx
  
   integer :: ibehav,nb_internal,i_internal
   
   character(len=80):: cout
                           !1234567890123456789012345
   character(len=25):: IAM='CLALp::read_ini_Vloc_Rloc'

   nb_CLxxx=get_nb_CLxxx()
   nb_ALpxx=get_nb_ALpxx()

   ! first reading: sizing verlt
   ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
   ! For this purpose nb_adj is introduced.

   if (.not. allocated(nb_adj)) then
     allocate(nb_adj(nb_CLxxx),stat=errare)
     if (errare /= 0 ) call faterr(IAM,'error allocating nb_adj')
   end if    

   do icdtac=1,nb_CLxxx
     nb_adj(icdtac)=0
   end do

   !print*,IAM
   !print*,nb_CLxxx
   !print*,nb_adj

   !write(*,'(I0,1x,I0)') clxxx2bdyty

   do    
    if( .not. read_G_clin()) exit
     if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
     if (G_clin(9:13)/= 'CLALp') cycle     
     if( .not. read_G_clin()) exit
     if( .not. read_G_clin()) exit
     read(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5)')   &
     cdbdy,icdbdy,cdtac,icdtac,                                          &
     behav,                                                              &
     anbdy,ianbdy,antac,iantac,ialxxx,                                   &
     sttus

     !print*,icdbdy,icdtac

     cdmodel = get_body_model_id_from_name( cdbdy )
     do icdtact=1,nb_CLxxx
       if (clxxx2bdyty(1,icdtact) == icdbdy .and. &
           clxxx2bdyty(2,icdtact) == icdtac .and. &
           clxxx2bdyty(3,icdtact) == cdmodel  ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1

         !print*,'ok'
         exit
       end if
     end do
   end do   

   if (.not. allocated(verlt)) then
     allocate(verlt(nb_CLxxx),stat=errare)
     if (errare /=0 ) then
       call faterr(IAM,'Error allocating verlt')
     end if

     do icdtac=1,nb_CLxxx
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       else
         call nullify_verlet_(icdtac)
       end if
       if (errare /=0 ) then
         write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
         call faterr(IAM,cout)
       end if
     end do
   else 
     do icdtac=1,nb_CLxxx
       call free_verlet_(icdtac)
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       else
         call free_verlet_(icdtac)
      end if
     end do
   end if
    
   ! second reading: filling data
   rewind(G_nfich)

   do icdtac=1,nb_CLxxx
     nb_adj(icdtac)=0
   end do
   icdan = 0

   do
     if( .not. read_G_clin()) exit
     if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
     if (G_clin(9:13)/= 'CLALp') cycle     
     if( .not. read_G_clin()) exit
     if( .not. read_G_clin()) exit

     read(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5)')   &
     cdbdy,icdbdy,cdtac,icdtac,                                          &
     behav,                                                              &
     anbdy,ianbdy,antac,iantac,ialxxx,                                   &
     sttus
     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )
     do icdtact=1,nb_CLxxx
       if (clxxx2bdyty(1,icdtact) == icdbdy .and. &
           clxxx2bdyty(2,icdtact) == icdtac .and. &
           clxxx2bdyty(3,icdtact) == cdmodel ) then

         icdan = icdan + 1

         nb_adj(icdtact)=nb_adj(icdtact)+1 
         verlt(icdtact)%icdan(nb_adj(icdtact))  = icdan
         verlt(icdtact)%cdbdy                   = icdbdy
         verlt(icdtact)%cdtac                   = icdtac
         verlt(icdtact)%cdmodel                 = cdmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
         verlt(icdtact)%ansci(nb_adj(icdtact))  = ialxxx
         if( .not. read_G_clin()) exit
         read(G_clin(1:90),'(27X,2(7X,D14.7))') rlt,rln
         verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
         verlt(icdtact)%rln(nb_adj(icdtact))=rln
         if( .not. read_G_clin()) exit
         read(G_clin(1:90),'(27X,2(7X,D14.7))') vlt,vln
         verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
         verlt(icdtact)%vln(nb_adj(icdtact))=vln
         if( .not. read_G_clin()) exit 
         read(G_clin(1:90),'(27X,1(7X,D14.7))') gapTT
         verlt(icdtact)%gapTT(nb_adj(icdtact))=gapTT
         if( .not. read_G_clin()) exit
         read(G_clin(1:90),'(27X,2(7X,D14.7))') nuc(1),nuc(2)
         verlt(icdtact)%nuc(1,nb_adj(icdtact))=nuc(1)
         verlt(icdtact)%nuc(2,nb_adj(icdtact))=nuc(2)
         if( .not. read_G_clin()) exit
         read(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
         verlt(icdtact)%coor(1,nb_adj(icdtact))=coor(1)
         verlt(icdtact)%coor(2,nb_adj(icdtact))=coor(2)

         verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
         ibehav = get_ibehav(behav)
         nb_internal = get_nb_internal(ibehav)
         if (nb_internal /= 0 ) then  
           if( .not. read_G_clin()) exit
           do i_internal=1, nb_internal
             read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
           enddo
         endif

         exit
       end if
     end do
   end do   

   nb_vCLALp=0

   do icdtact=1,nb_CLxxx
     nb_vCLALp = nb_vCLALp + nb_adj(icdtact)

     if ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) then 
       write(cout,'(A,1x,I0)')      'Very strange for the contactor',icdtact
       call logmes(cout) 
       write(cout,'(A,1x,I0,1x,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       call logmes(cout)        
       write(cout,'(A,1x,I0)')      'value of verlet%adjsz is',verlt(icdtact)%adjsz
       call faterr(IAM,cout)
     endif

   enddo

   write(cout,'(I0,1x,A)') nb_vCLALp,'CLALp read'
   call logmes(cout) 

   READ = .TRUE.

 end subroutine read_ini_Vloc_Rloc

 !------------------------------------------------------------------------ 
 subroutine write_out_Vloc_Rloc(nfich)

  !
  ! write into file out_Vloc_Rloc data from this, in verlt style
  !

   implicit none
   integer :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,nfich,icdtact,nb_CLxxx,nb_ALpxx
   integer :: lc

   integer :: ialxxx

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_ALpxx=get_nb_ALpxx()
   nb_CLxxx=get_nb_CLxxx()

   do icdtact=1,nb_CLxxx    
     do iadj=1,nb_adj(icdtact)    
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       ialxxx = this(icdan)%iansci
       cdmodel = get_body_model_name_from_id( clxxx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( alpxx2bdyty(3,iantac) )
       write(nfich,'(A6,2X,A5,2X,I7)')'$icdan','CLALp',icdan     
                            !12345678901234567890123456789012345678901234567890123456789012345678901234567890123
       write(nfich,'(A83)') ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  segmt  sttus   iadj'   
       write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,clxxx2bdyty(1,icdtac),'CLxxx',clxxx2bdyty(2,icdtac),  &
       see(this(icdan)%isee)%behav,  &
       anmodel,alpxx2bdyty(1,iantac),'ALpxx',alpxx2bdyty(2,iantac),ialxxx,  &
       get_contact_status_name_from_id(this(icdan)%status), iantac
       write(nfich,104)'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls/H',0.D0
       write(nfich,104)'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',0.D0
       write(nfich,103)'gapTT',this(icdan)%gapTT
       write(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',0.D0
       write(nfich,104)'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',0.D0

       if (this(icdan)%nb_internal /= 0) then
         call write_internal_comment(nfich,this(icdan)%lawnb)
         write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
         write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
       endif
       write(nfich,'(A1)')' '
       
     end do                           
   end do
   
103  format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine write_out_Vloc_Rloc

 !------------------------------------------------------------------------ 
 subroutine local_framing(XP1,YP1,XC,YC,XD,YD, &
                          UT1,UT2,UN1,UN2,gap,cpcd,XP2,YP2)

    !
    !   mjean, 1986,1989, 05.01.91, 28.05.91, 28.02.93, 02.05.01
    ! 

    implicit none
 
    real(kind=8) :: XC,XD,YC,YD,XP1,XP2,YP1,YP2,CD
    real(kind=8),intent(out) :: UT1,UT2,UN1,UN2,gap,cpcd
    

    CD = DSQRT((XC-XD)**2+(YC-YD)**2)


    UT1 = (XD-XC)/CD 
    UT2 = (YD-YC)/CD
    UN1 = -UT2
    UN2 =  UT1

    CPCD = (((XP1-XC)*UT1) + ((YP1-YC)*UT2))/CD     

    XP2 = XC + (CPCD*CD*UT1)
    YP2 = YC + (CPCD*CD*UT2)


    gap = ((XP1-XP2)*UN1) + ((YP1-YP2)*UN2)


 end subroutine local_framing

 !------------------------------------------------------------------------ 
 subroutine nullify_reac_CLALp(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer :: icdtac
   integer :: iantac
   integer :: storage   
    
   icdtac=this(icdan)%icdtac
   call nullify_reac_CLxxx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_reac_ALpxx(iantac,storage)
    
 end subroutine nullify_reac_CLALp

 !------------------------------------------------------------------------ 
 subroutine nullify_vlocy_CLALp(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer :: icdtac
   integer :: iantac
   integer :: storage   
    
   icdtac=this(icdan)%icdtac
   call nullify_vlocy_CLxxx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_vlocy_ALpxx(iantac,storage)
    
 end subroutine nullify_vlocy_CLALp

 !------------------------------------------------------------------------ 
 subroutine vitrad_CLALp(icdan,storage,need_full_vlocy)

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac,ialxxx
   integer            :: storage   
   logical, optional  :: need_full_vlocy
    
   icdtac=this(icdan)%icdtac

   call comp_vlocy_CLxxx(icdtac,storage, need_full_vlocy)
    
   iantac=this(icdan)%iantac
   ialxxx=this(icdan)%iansci
   call comp_vlocy_ALpxx(iantac,ialxxx,storage,need_full_vlocy)
    
 end subroutine vitrad_CLALp

 !------------------------------------------------------------------------  
 subroutine injj_CLALp(icdan,RTIK,RNIK,storage)
 
   implicit none
   integer     ,intent(in) :: icdan
   real(kind=8),intent(in) :: RTIK,RNIK
   real(kind=8),dimension(2)  :: cdreac, anreac
   integer :: icdbdy,icdtac,numnoda,numnodb
   integer :: iantac,ialxxx
   integer :: storage   
   
   icdtac=this(icdan)%icdtac

   iantac=this(icdan)%iantac
   ialxxx=this(icdan)%iansci

   cdreac(1)=RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)=-cdreac(1)

   cdreac(2)=RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
   anreac(2)=-cdreac(2)

   call add_reac_CLxxx(icdtac,cdreac(1:2),storage)

   call add_reac_ALpxx(iantac,ialxxx,anreac(1:2),this(icdan)%cpcd,storage)

 end subroutine injj_CLALp 

 !------------------------------------------------------------------------  
 subroutine prjj_CLALp(icdan,VTIK,VNIK,storage)
 
   implicit none
   integer     ,intent(in)   :: icdan
   real(kind=8),intent(out)  :: VTIK,VNIK
   real(kind=8),dimension(2) :: Vcd,Van
   integer     ,intent(in)   :: storage
   integer :: icdtac,iantac,ialxxx
   
   icdtac=this(icdan)%icdtac

   iantac=this(icdan)%iantac
   ialxxx=this(icdan)%iansci

   call get_vlocy_CLxxx(icdtac,storage,Vcd)
   call get_vlocy_ALpxx(iantac,ialxxx,storage,Van,this(icdan)%cpcd) 

   VTIK=Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2) &
       -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)
   VNIK=Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2) &
       -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)

 end subroutine prjj_CLALp 

 !------------------------------------------------------------------------ 
 integer function get_nb_CLALp(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_CLALp = nb_CLALp
   case(i_verlet_tactor)
      get_nb_CLALp = nb_vCLALp
   case(i_rough_tactor)
      get_nb_CLALp = nb_rough_CLALp
   case(i_recup_tactor)
      get_nb_CLALp = nb_recup_CLALp
   end select

 end function get_nb_CLALp

 !------------------------------------------------------------------------ 
 function get_tact_lawnb_CLALp(icdan)

   implicit none
   integer  ::  icdan,get_tact_lawnb_CLALp
   ! ***
   integer :: isee,i_behav
   character(len=5)  :: cdcol,ancol

   
   if (allocated(this)) then
     get_tact_lawnb_CLALp=this(icdan)%lawnb
   else
     get_tact_lawnb_CLALp=0
   endif   

 end function get_tact_lawnb_CLALp

 !------------------------------------------------------------------------ 
 subroutine get_rel_pos_CLALp(icdan,apab,cpcd)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   real(kind=8)   , intent(out) :: apab, cpcd

   apab = get_apab_CLxxx(this(icdan)%icdtac)
   cpcd = this(icdan)%cpcd

 end subroutine get_rel_pos_CLALp

 !------------------------------------------------------------------------ 
 integer function CLALp2CLxxx(icdan)
   implicit none
   integer :: icdan
   
   CLALp2CLxxx = this(icdan)%icdtac

 end function CLALp2CLxxx

 !------------------------------------------------------------------------ 
 integer function CLALp2ALpxx(icdan)
   implicit none
   integer :: icdan
   
   CLALp2ALpxx = this(icdan)%iantac

 end function CLALp2ALpxx

 !------------------------------------------------------------------------ 
 real(kind=8) function get_length_CLALp(icdan)

   implicit none
   integer,intent(in) :: icdan 
   integer :: icdtac
    
   icdtac=this(icdan)%icdtac
   get_length_CLALp=get_length_CLxxx(icdtac)
   
 end function get_length_CLALp
!------------------------------------------------------------------------ 

 !------------------------------------------------------------------------
 logical function RUN_CLALp(fantome)

   implicit none
   integer,optional :: fantome

   RUN_CLALp = RUN_TACTOR

end function RUN_CLALp
!------------------------------------------------------------------------
  subroutine reset_CHECK_CLALp()
  implicit none

  module_checked_ = .false.
  check_clalp_    = .false.

  end subroutine
!------------------------------------------------------------------------
logical function CHECK_CLALp()
  implicit none
  !   
  integer :: isee

  ! if check already made just return result
  if( module_checked_ ) then
    CHECK_CLALp = check_CLALp_
    return
  end if
  
  con_pedigree%module_name = 'CLALp'

  con_pedigree%id_cdan  = i_clalp
  con_pedigree%id_cdtac = i_clxxx
  con_pedigree%id_antac = i_alpxx

  cdtact2bdyty => clxxx2bdyty
  antact2bdyty => alpxx2bdyty

  ! check only once if module may be used
  module_checked_ = .TRUE.

  ! checking if enough cd/an
  nb_ALpxx = get_nb_ALpxx()
  nb_CLxxx = get_nb_CLxxx()
  if( nb_ALpxx == 0 .or. nb_CLxxx == 0 ) then
    CHECK_CLALp = check_CLALp_ ! still false
    return
  end if
  
  ! checking if any seetable with the good cd/an type
  do isee = 1, size(see)
    if (see(isee)%cdtac == 'CLxxx' .and. see(isee)%antac == 'ALpxx') then
      check_CLALp_ = .true.
      exit
    end if
  end do

  CHECK_CLALp = check_CLALp_
  return
 end function CHECK_CLALp

 !------------------------------------------------------------------------
 subroutine update_wear_CLALp
   implicit none

   integer :: icdan,icdtac,iantac,ialxxx
   real(kind=8),dimension(2) :: cdvwear,anvwear

   do icdan=1,nb_CLALp

     icdtac=this(icdan)%icdtac
     iantac=this(icdan)%iantac
     ialxxx=this(icdan)%iansci

     cdvwear(:)=this(icdan)%internal(2)*this(icdan)%nuc(:)

     call put_vwear_CLxxx(icdtac,cdvwear)

     anvwear(:)=this(icdan)%internal(3)*this(icdan)%nuc(:)

     call put_vwear_ALpxx(iantac,ialxxx,anvwear,this(icdan)%cpcd)

   enddo   

 end subroutine update_wear_CLALp

 !------------------------------------------------------------------------
 subroutine compute_energy_increment(NRJ_t,NRJ_n)
   implicit none

   integer :: icdan

   !fd normal and tangential part of the incremental energy stored or dissipated by a contact
   real(kind=8)               :: NRJ_n,NRJ_t


   NRJ_n = 0.d0
   NRJ_t = 0.d0

   do icdan=1,nb_CLALp
     NRJ_n = NRJ_n + (this(icdan)%rln*this(icdan)%vln)
     NRJ_t = NRJ_t + (this(icdan)%rlt*this(icdan)%vlt)
   enddo

  ! pas utile car R est deja R*H !
  !   NRJ_n = NRJ_n * H
  !   NRJ_t = NRJ_t * H

 end subroutine compute_energy_increment

 !------------------------------------------------------------------------
 subroutine print_info_CLALp(icdan)
   implicit none
   integer          :: icdan,icdtac,iantac,ianal,icdbdy,ianbdy

   character(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac
   ianal=this(icdan)%iansci

   write(cout,"(1x,'CLxxx:',1x,I5,1x,'ALpxx:',1x,I5,1x,'segment:',1x,I5)") icdtac,iantac,ianal
   call LOGMES(cout)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   write(cout,"(1x,'cd body:',1x,I5,1x,'an body:',1x,I5)") icdbdy,ianbdy
   call LOGMES(cout)

   !   call print_info_CLxxx(icdbdy)
   !   call print_info_ALpxx(ianbdy)

 end subroutine print_info_CLALp

 !------------------------------------------------------------------------ 
 logical function get_write_Vloc_Rloc_CLALp(fantome)

    implicit none
    integer,optional :: fantome

    get_write_Vloc_Rloc_CLALp = write_Vloc_Rloc

 end function get_write_Vloc_Rloc_CLALp

 !------------------------------------------------------------------------ 
 function get_icdtac_CLALp(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_CLALp
   !
   integer(kind=4) :: icc, icdtac, iadj
   logical :: found

   found = .false.

   icc = 0
   do icdtac = 1, nb_CLxxx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_CLALp = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('CLALp::get_icdtac','unknown contact index')
   
 end function

 !------------------------------------------------------------------------  
 function get_iantac_CLALp(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_CLALp
   !
   integer(kind=4) :: icc, icdtac, iadj
   logical :: found

   found = .false.

   icc = 0
   do icdtac = 1, nb_CLxxx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_CLALp =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('CLALp::get_icdtac','unknown contact index')
   

   get_iantac_CLALp = this(icdan)%iantac

 end function

 ! rm : functions for siconos wrapper
 function get_iannodes_CLALp(icdan)
   implicit none
   integer(kind=4), intent(in)   :: icdan
   integer(kind=4), dimension(2) :: get_iannodes_CLALp

   get_iannodes_CLALp = get_nodes_ALxxx(this(icdan)%iantac, this(icdan)%iansci)
 end function

 function get_icdnodes_CLALp(icdan)
   implicit none
   integer(kind=4), intent(in)   :: icdan
   integer(kind=4), dimension(2) :: get_icdnodes_CLALp

   get_icdnodes_CLALp = get_nodes_CLxxx(this(icdan)%icdtac)
 end function

 function get_old_index_CLALp(icdan)
   implicit none
   integer :: icdan
   integer :: get_old_index_CLALp
   !
   integer :: icdtac,iantac,iadj

   get_old_index_CLALp = 0

   if (.not. allocated(verlt)) then
      return
   endif
   
   icdtac = this(icdan)%icdtac ! serial number of candidate  contactor for contact icdan
   iantac = this(icdan)%iantac ! serial number of antagonist contactor for contact icdan 

   if (verlt(icdtac)%adjsz /= 0) then
      if (verlt(icdtac)%cdbdy  == clxxx2bdyty(1,icdtac) .and. &
          verlt(icdtac)%cdtac  == clxxx2bdyty(2,icdtac) .and. &
          verlt(icdtac)%cdmodel== clxxx2bdyty(3,icdtac)       &
         ) then
        do iadj = 1, verlt(icdtac)%adjsz
           if (verlt(icdtac)%anbdy(iadj)  == alpxx2bdyty(1,iantac) .and. &
               verlt(icdtac)%antac(iadj)  == alpxx2bdyty(2,iantac) .and. &
               verlt(icdtac)%anmodel(iadj)== alpxx2bdyty(3,iantac)       &
                ) then
              get_old_index_CLALp = verlt(icdtac)%icdan(iadj)
              exit
           end if
        end do
      end if
   end if

 end function get_old_index_CLALp

 subroutine get_g2l_CLALp(icdan,g2l)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   real(kind=8)   , intent(out) :: g2l(2,8)
   !
   real(kind=8) :: apab

   apab = get_apab_CLxxx(this(icdan)%icdtac)

   g2l(1:2,1:8) = 0.d0

   g2l(1,1) = apab * this(icdan)%tuc(1)
   g2l(1,2) = apab * this(icdan)%tuc(2)
   g2l(1,3) = (1.d0-apab) * this(icdan)%tuc(1) 
   g2l(1,4) = (1.d0-apab) * this(icdan)%tuc(2) 
   g2l(1,5) = - this(icdan)%cpcd * this(icdan)%tuc(1)
   g2l(1,6) = - this(icdan)%cpcd * this(icdan)%tuc(2) 
   g2l(1,7) = (this(icdan)%cpcd-1.d0) * this(icdan)%tuc(1)
   g2l(1,8) = (this(icdan)%cpcd-1.d0) * this(icdan)%tuc(2) 
   
   g2l(2,1) = apab * this(icdan)%nuc(1)
   g2l(2,2) = apab * this(icdan)%nuc(2) 
   g2l(2,3) = (1.d0-apab) * this(icdan)%nuc(1)
   g2l(2,4) = (1.d0-apab) * this(icdan)%nuc(2) 
   g2l(2,5) = - this(icdan)%cpcd * this(icdan)%nuc(1)
   g2l(2,6) = - this(icdan)%cpcd * this(icdan)%nuc(2) 
   g2l(2,7) = (this(icdan)%cpcd-1.d0) * this(icdan)%nuc(1)
   g2l(2,8) = (this(icdan)%cpcd-1.d0) * this(icdan)%nuc(2) 
   
 end subroutine get_g2l_CLALp

 subroutine get_local_contact_CLALp(icdan, loc)
   implicit none
   integer(kind=4), intent(in) :: icdan
   real(kind=8), dimension(6)  :: loc

   loc(1:2) = this(icdan)%coor(1:2)
   loc(3:4) = this(icdan)%tuc(1:2)
   loc(5:6) = this(icdan)%nuc(1:2)

 end subroutine

 function get_nb_max_adj_CLALp()
   implicit none
   integer(kind=4) :: get_nb_max_adj_CLALp

   if( nb_CLALp <= 0 ) then
     get_nb_max_adj_CLALp = 0
   else
     get_nb_max_adj_CLALp = maxval(nb_adj)
   end if

 end function

!! ! rm : functions to test itrHdl
!! !> \brief Get a rough interaction 
!! subroutine get_rough_CLALp(icdan, icdtac, iantac, isee, periodic, i4)
!!   implicit none
!!   integer(kind=4), intent(in) :: icdan
!!   integer(kind=4), intent(out) :: icdtac, iantac, isee, periodic, i4
!!   !
!!   integer(kind=4) :: i
!!
!!   ! since CLALp rough detection is really different from CLALp or SPSPx ones
!!   i = 0
!!
!!
!!   do icdtac = 1, get_nb_CLxxx()
!!     if( rough_CLALp(icdtac)%ialpxx /= 0 ) then
!!       i = i+1
!!       if( i == icdan ) then
!!         i4       = rough_CLALp(icdtac)%inode
!!         iantac   = rough_CLALp(icdtac)%ialpxx
!!         isee     = rough_CLALp(icdtac)%isee
!!         exit
!!       end if
!!     end if
!!   end do
!!
!! end subroutine
!!
!! !> \brief Reset violation array
!! subroutine reset_violation_CLALp(nb)
!!   implicit none
!!   integer(kind=4), intent(in) :: nb
!!
!!   if( allocated(violation) ) deallocate(violation)
!!   allocate( violation(nb) )
!! end subroutine
!!
!! subroutine reset_nb_adj_CLALp()
!!   implicit none
!!
!!   if (.not. allocated(nb_adj)) allocate(nb_adj(get_nb_CLxxx()))
!!
!!   nb_adj = 0
!!
!! end subroutine
!!
!! !> \brief Get the number of interaction involving a candidat contactor
!! function get_nb_adj_CLALp(i_tact)
!!   implicit none
!!   !> contactor number
!!   integer(kind=4), intent(in) :: i_tact
!!   !> number of interactions attached to contactor
!!   integer(kind=4) :: get_nb_adj_CLALp
!!
!!   get_nb_adj_CLALp = nb_adj(i_tact)
!!
!! end function
!!
!! !> add an adjacent
!! subroutine add_adj_CLALp(icdbdy, icdtac)
!!   implicit none
!!   !> body number of candidat to add adjacent to
!!   integer(kind=4), intent(in) :: icdbdy
!!   !> contactor number of candidat to add adjacent to
!!   integer(kind=4), intent(in) :: icdtac
!!   !
!!   integer(kind=4) :: i_tact
!!
!!   do i_tact =1, get_nb_CLxxx()
!!     if (clxxx2bdyty(1,i_tact) == icdbdy .and. &
!!         clxxx2bdyty(2,i_tact) == icdtac       ) then
!!       nb_adj(i_tact) = nb_adj(i_tact) + 1
!!       exit
!!     end if
!!   end do
!!
!! end subroutine
!!
!! !> \brief Compute interactions from a rough interaction
!! subroutine compute_contacts_in_t2t_CLALp(t2t, icdan)
!!   implicit none  
!!   !> t2t in which to compute interactions
!!   type(T_tact2tact) :: t2t
!!   !> current index of clalp
!!   integer(kind=4), intent(in) :: icdan
!!   !
!!   integer(kind=4)    :: i, itest, ibehav, i4_input(4), i4_output(4)
!!   real(kind=8)       :: r8_output(8), cd_Vbegin(2), an_Vbegin(2)
!!   logical            :: to_keep
!!   character(len=40)  :: IAM
!!   character(len=103) :: cout
!!   !      1234567890123456789012345678901234567890
!!   IAM = 'mod_CLALp::compute_contacts_in_t2t_CLALp'
!!
!!   i4_input(1) = t2t%icdtac
!!   i4_input(2) = t2t%iantac
!!   i4_input(3) = t2t%i_param ! inode
!!   i4_input(4) = t2t%isee
!!
!!   call compute_one_contact_CLALp(i4_input, i4_output, r8_output, to_keep)
!!
!!   if( to_keep ) then
!!
!!     call set_nb_face2faces(t2t, 1)
!!
!!     t2t%f2f(1)%nb_ctc = 1
!!     call initialize_interactions(t2t%f2f(1), 1)
!!
!!     t2t%f2f(1)%ctc(1)%inter%cdan   = i_clalp
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdent = get_ent_CLxxx(t2t%icdtac)
!!     t2t%f2f(1)%ctc(1)%inter%ianent = get_ent_ALpxx(t2t%iantac) 
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdtyp = i_clxxx
!!     t2t%f2f(1)%ctc(1)%inter%iantyp = i_alpxx
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdtac = i4_input(1)
!!     t2t%f2f(1)%ctc(1)%inter%iantac = i4_input(2)
!!     t2t%f2f(1)%ctc(1)%inter%isee   = i4_input(4)
!!
!!     t2t%f2f(1)%ctc(1)%inter%iadj   = i4_output(1)
!!     t2t%f2f(1)%ctc(1)%inter%icdbdy = i4_output(2)
!!     t2t%f2f(1)%ctc(1)%inter%ianbdy = i4_output(3)
!!     t2t%f2f(1)%ctc(1)%inter%ian_al = i4_output(4)
!!
!!     t2t%f2f(1)%ctc(1)%inter%uc(1:2,2)  = r8_output(2:3)
!!     t2t%f2f(1)%ctc(1)%inter%uc(1:2,1)  = r8_output(4:5)
!!     t2t%f2f(1)%ctc(1)%inter%gapTTBEGIN = r8_output(1)
!!     t2t%f2f(1)%ctc(1)%inter%cpcd       = r8_output(6)
!!
!!     call get_vlocy_CLxxx(t2t%icdtac,iVbeg_,cd_Vbegin)
!!     call get_vlocy_ALpxx(t2t%iantac,t2t%isee,iVbeg_,an_Vbegin,r8_output(6))
!!
!!     t2t%f2f(1)%ctc(1)%inter%vlBEGIN = dot_product( cd_Vbegin(1:2)-an_Vbegin(1:2), r8_output(2:3) )
!!     t2t%f2f(1)%ctc(1)%inter%vlBEGIN = dot_product( cd_Vbegin(1:2)-an_Vbegin(1:2), r8_output(4:5) )
!!
!!     t2t%f2f(1)%ctc(1)%inter%rl(1:2) = 0.d0
!!     t2t%f2f(1)%ctc(1)%inter%vl(1:2) = t2t%f2f(1)%ctc(1)%inter%vlBEGIN
!!     t2t%f2f(1)%ctc(1)%inter%gapTT   = t2t%f2f(1)%ctc(1)%inter%gapTTBEGIN
!!     t2t%f2f(1)%ctc(1)%inter%status  = 'nknow'
!!
!!     t2t%f2f(1)%ctc(1)%inter%coor(1:2) = r8_output(7:8)
!!
!!     itest = 0
!!     do ibehav = 1, size(tact_behav)
!!       if( see(t2t%isee)%behav == tact_behav(ibehav)%behav ) then
!!         t2t%f2f(1)%ctc(1)%inter%lawnb       = ibehav
!!         t2t%f2f(1)%ctc(1)%inter%i_law       = tact_behav(ibehav)%ilaw
!!         t2t%f2f(1)%ctc(1)%inter%nb_internal = get_nb_internal(ibehav)
!!         t2t%f2f(1)%ctc(1)%inter%internal    = init_internal(ibehav)
!!         itest = 1
!!         exit
!!       end if
!!     end do
!!     if( itest == 0 ) then
!!        write(cout,'(A,1x,I0,1x,A)') 'nickname',see(t2t%isee)%behav,'unknown in lawty'
!!        call logmes('check TACT-BEHAV.DAT in DATBOX')
!!        call faterr(IAM,cout)
!!     end if
!!
!!     t2t%f2f(1)%ctc(1)%inter%area = get_length_CLxxx(t2t%icdtac)
!!
!!   end if
!!
!! end subroutine

 subroutine clean_memory_CLALp
   implicit none
   integer(kind=4) :: i

   call clean_memory_inter_meca_( )

   nb_CLxxx = 0
   nb_ALpxx = 0
 
   nb_CLALp  = 0
   nb_vCLALp = 0

   if( allocated(rough_CLALp) ) deallocate(rough_CLALp)

   nb_rough_CLALp = 0
   nstep_rough_seek_CLALp = 1
   nb_recup_CLALp = 0

   RUN = .false.
   READ = .false.

   if( allocated(CLcoor) ) deallocate(CLcoor)
   if( allocated(ALp)    ) then
     do i =1, size(ALp)
       if( associated(ALp(i)%coor)   ) deallocate(ALp(i)%coor)
       if( associated(ALp(i)%normal) ) deallocate(ALp(i)%normal)
     end do
     deallocate(ALp)
   end if

   global_adist   = 1.d+20
   Reac_CLALp_MAX = 0.D0

   is_open_energy            = .false.
   is_open_internal          = .false.
   is_nonsymmetric_detection = .false.


   module_checked_ = .FALSE.
   check_CLALp_    = .FALSE.

!------------------------------------------------------------------------
   
 end subroutine

 !> \brief Compute the left hand side elementary matrix of thermal diffusion due to contact
 subroutine get_diffusion_lhs_CLALp(icdan,lhs)
   use a_EF, only : fonct_forme, derive_forme, T_PT_GAUSS, i_l_p1
   implicit none
   !> contact id
   integer(kind=4), intent(in) :: icdan
   !> computed left hand side
   real(kind=8), dimension(4,4), intent(out) :: lhs
   !
   type(T_PT_GAUSS) :: gp_cd, gp_an
   integer(kind=4)  :: i_node
   real(kind=8)     :: R, coef, conduc, apab, lambdas, lambdac
   real(kind=8), dimension(1)   :: coor_pg_cd, coor_pg_an
   real(kind=8), dimension(2,2) :: support_cd, support_an
   real(kind=8), dimension(1,2) :: J

   lhs = 0.d0

   !!! if too far (un_gap>h) : skip
   !!if( this(icdan)%internal(2) > this(icdan)%internal(1) ) return

   ! if sane interface
   if( this(icdan)%internal(4) >= 1.d0 ) then
     call faterr('CLALp::get_diffusion_lhs','impossible call with sane interface')
   end if

   ! compute effective conductivity depending on the case
   call get_lambda(this(icdan)%lawnb,lambdas,lambdac)

   ! if interface broken:
   if( this(icdan)%internal(4) <= 0.d0 ) then
     ! - and no contact then skip
     !if( this(icdan)%internal(2) > 0.d0 ) return
     ! - and contact then effective conductivity of the two materials
     conduc = lambdac
   else
   ! else: then effective conductivity of the two materials weighted by damage
     conduc = this(icdan)%internal(4)*lambdas
   end if

   call get_coor_support_CLxxx( this(icdan)%icdtac, support_cd )
   call get_coor_support_ALxxx( this(icdan)%iantac, this(icdan)%iansci, support_an )

   ! Relying on mesh conformance : use of apab and cpcd as reference coordinates
   ! (except that apab/cpcd is between 0 and 1 and ref coordinate is between -1 and 1),
   ! it is strictly equivalent as long as the CL and AL elements are parallel and nodes
   ! facing each others.

   apab = get_apab_CLxxx(this(icdan)%icdtac)
   coor_pg_cd = apab*2.d0-1.d0
   coor_pg_an = this(icdan)%cpcd*2.d0-1.d0

   ! Another possiblity is : to use the apab on the AL too...
   ! provided that the nodes on AL and CL are correctly ordere
   ! the code would be something like:

   !l1 = support_cd(:,1)-support_an(:,1)
   !l2 = support_cd(:,1)-support_an(:,2)
   !if( dot_product( l1, l1 ) < dot_product( l2, l2 ) ) then
   !  coor_pg_an = coor_pg_cd
   !else
   !  coor_pg_an =-coor_pg_cd
   !end if

   call fonct_forme(i_l_p1,coor_pg_an,gp_an%N)
   call fonct_forme(i_l_p1,coor_pg_cd,gp_cd%N)

   ! Scale factor of the CL element
   call derive_forme(i_l_p1,coor_pg_cd,gp_cd%DN)
   J = matmul(gp_cd%DN,transpose(support_cd))
   coef = sqrt( dot_product(J(1,:),J(1,:)) )

   ! Weight of the interaction in the flux computation
   ! Constant  quadrature : 2.d0
   ! Linear    quadrature : 1.d0, 1.d0
   ! Quadratic quadrature : 5/9, 8/9, 5/9
   ! Divided by the size of the interface
   !
   ! dixit FD : this is so ugly !
   ! currently the user set as many contact points as desired
   ! wherever on the CL with whatever weights, there is no reasons that
   ! the input weights are consistant with a quadrature rule...
   ! And no geometric criterium could ever help to get this information
   ! correctly. The quadrature should be manage in the same way than with the CS
   !
   if(apab==0.d0 .or. apab==1.d0) then
     call faterr('CLALp::get_lhs_diffisuvity','nodal approach not allowed')
   elseif(apab==0.5d0) then !constant
     coef = coef * 2.d0 / this(icdan)%internal(1)
   else                     !linear
     coef = coef * 1.d0 / this(icdan)%internal(1)
   end if

   ! Calcul du rayon dans le cas axisymetrique
   if( DIME_mod == i_2D_axisym ) then
     call fonct_forme(i_l_p1,coor_pg_cd,gp_cd%N)
     R    = dot_product(gp_cd%N,support_cd(1,:))
     coef = coef * R * 2.d0*PI_g
   end if

   do i_node = 1, 2
     lhs(1:2,i_node  ) = gp_an%N(:)*gp_an%N(i_node)*coef
     lhs(1:2,i_node+2) =-gp_an%N(:)*gp_cd%N(i_node)*coef
     lhs(3:4,i_node  ) =-gp_cd%N(:)*gp_an%N(i_node)*coef
     lhs(3:4,i_node+2) = gp_cd%N(:)*gp_cd%N(i_node)*coef
   end do

   lhs = lhs * conduc

 end subroutine

 !------------------------------------------------------------------------
 subroutine set_nb_CLALp(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_CLALp = nb

 end subroutine

 !------------------------------------------------------------------------   
 subroutine redo_nb_adj_CLALp()
   implicit none

   call redo_nb_adj_( get_nb_CLxxx() )

 end subroutine redo_nb_adj_CLALp
 
 !------------------------------------------------------------------------   
 subroutine is_external_detection_CLALp()
   implicit none

   is_externalDetection = .True.
   
 end subroutine is_external_detection_CLALp
 
 !------------------------------------------------------------------------   
 subroutine get_bulk_strain_clalp(icdan,vec)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: vec(:)

   if (is_externalFEM) then

     call externalFEM_get_strain( icdan, vec )
  
   else

     call get_bulk_strain_clxxx(this(icdan)%icdtac,vec)

   endif   

 end subroutine get_bulk_strain_clalp

 !------------------------------------------------------------------------       
 subroutine get_bulk_stress_clalp(icdan,vec)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: vec(:)

   if (is_externalFEM) then

      call externalFEM_get_stress( icdan, vec )

   else

      call get_bulk_stress_clxxx(this(icdan)%icdtac,vec)

   endif

 end subroutine

 !------------------------------------------------------------------------       
 subroutine get_bulk_strain_triaxiality_clalp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

    if (is_externalFEM) then

      call externalFEM_get_straintriaxiality( icdan, value )

    else

      call get_bulk_strain_triaxiality_clxxx(this(icdan)%icdtac,value)

    endif

 end subroutine

 !------------------------------------------------------------------------        
 subroutine get_bulk_strain_rate_triaxiality_clalp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

    if(is_externalFEM) then

      call externalFEM_get_strainratetriaxiality( icdan, value )

    else
      ! fonction à coder sur LMGC90
     ! call get_bulk_strain_rate_triaxiality_clxxx(this(icdan)%icdtac,value)

    endif

 end subroutine

 !------------------------------------------------------------------------       
 subroutine get_bulk_stress_triaxiality_clalp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

    if (is_externalFEM) then

      call externalFEM_get_stresstriaxiality( icdan, value )

    else

      call get_bulk_stress_triaxiality_clxxx(this(icdan)%icdtac,value)

    endif

 end subroutine


 


 !------------------------------------------------------------------------        
 subroutine get_bulk_temperature_clalp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

    if (is_externalFEM) then

      call externalFEM_get_temperature( icdan, value )

    else

      ! fonction à coder sur LMGC90
      !call get_bulk_temperature_clxxx(this(icdan)%icdtac,value)

    endif

 end subroutine

 !------------------------------------------------------------------------    
 subroutine get_external_pressure_clalp(icdan,pext)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: pext

   if (is_externalFEM) then
     call externalFEM_get_pressure(icdan,pext)
   else    
     pext=0.d0
  endif
  
 end subroutine get_external_pressure_clalp
 
   
end module CLALp
