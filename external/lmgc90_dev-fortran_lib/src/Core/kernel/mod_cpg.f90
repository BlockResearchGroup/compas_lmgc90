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
module cpg

  !!****h* LMGC90.CORE/CPG
  !! NAME
  !!  module CPG
  !! AUTHOR
  !!  M. Renouf
  !! PURPOSE
  !!  Conjugate Projected Gradient algorithm
  !!  refer to
  !!   M. Renouf and P. Alart," Conjugate gradient type algorithms for frictional multicontact
  !!   problems: applications to granular materials" in Comput. Methods Appl. Mech. Engrg., 
  !!   vol. 194(18-20), pp 2019-2041 (2004)
  !!****
  
  ! shared modules

  use overall

  use parameters

  use tact_behaviour
  use utilities
  
  use inter_meca_handler_2D, only : get_nb_inters    , &
                                    set_loc          , &
                                    get_rloc         , &
                                    get_vlocBEGIN    , &
                                    get_internal     , &
                                    inter2ENT       , &
                                    get_tact_lawnb   , &
                                    injj             , &
                                    prjj             , &
                                    vitrad           , &
                                    nullify_reac     , &
                                    nullify_vlocy    , &
                                    get_length   !    , &
  
  implicit none
  
  private
  
  logical :: bavard =.false.
  
  type T_ctct_element
     
     integer                      :: CDAN                 ! this(ik)%CDAN; type of contact element ik
     integer                      :: icdan                ! this(ik)%icdan; 
                                                        ! serial type number in type CDAN of contact element ik
     real(kind=8)                 ::   rlt,   rln         ! this(ik)%rlt,this(ik)%rln; components of reaction
     real(kind=8)                 ::  rlti,  rlni         ! this(ik)%rlti,this(ik)%rlni; components of reaction for convergence
     integer(kind=4)              :: status               ! ...status
     integer(kind=4)              :: statuscheck          ! ...statuscheck

     real(kind=8)                 ::  Wtt, Wtn, Wnt, Wnn  ! ...components of genuine local dynamic matrix W
     real(kind=8)                 :: WWtt,          WWnn  ! ...components of auxiliary local dynamic matric WW
     real(kind=8)                 :: det                  ! ...det of auxiliary local dynamic matrix
     real(kind=8)                 ::    freet,    freen   ! 
     real(kind=8)                 ::   vfreet,   vfreen   ! ...genuine free velocity components
     real(kind=8)                 :: covfreet, covfreen   ! ...complementary free velocity components to add to
                                                        ! ...genuine free velocity components to get auxiliaries
   
     real(kind=8)                 :: vltBEGIN, vlnBEGIN   ! ...components of the relative velocity at the 
                                                        ! beginning of the time step   
     real(kind=8)                 :: gapTTbegin             ! ...gap at the beginning of the time step 
     real(kind=8)                 :: gapREF
     integer(kind=4)              :: statusBEGIN          ! ...status at the beginning of the time step
     integer(kind=4)              :: forecast             ! ...set to 1 'acton' (reactions have to be computed)
                                                          !        or 0 'noact' (reactions are to be found null)
                                                          !        according to some forecast criterion
     integer                      :: lawnb                ! this(ik)%lawnb; law number of contact ik
     real(kind=8)                 :: fric                 ! this(ik)%fric; friction coefficient for contact ik

!!! Gradient informations

     real(kind=8)                 :: WPt,WPn,WRt,WRn      ! this(ik)%XXX is the product of W(i,:) and P or R 
     real(kind=8)                 :: Zt,Zn                ! 
     real(kind=8)                 :: Pt,Pn                ! residue projection
     real(kind=8)                 :: Wt,Wn                ! 
     real(kind=8)                 :: Ut,Un                ! residue

!!! information for completed matrix W 

     integer                      :: nbadj,icdent,ianent,istart
     integer,dimension(:),pointer :: adjjl

!!! internal variables

     real(kind=8),dimension(max_internal_tact) :: internal
!!!
     real(kind=8)               :: nx,ny
     integer                    :: i_law
!!!
  end type T_ctct_element

  type (T_ctct_element),dimension(:),allocatable  :: this  

!!!mr about computation of Wab ------------------------------------------------------------------------
!!!
!!! Wab is the vector which contains all components of each matrix Wik,jl. the this(ik)%istart contains 
!!! the last range before ik where a values have been written.
!!!  W(this(ik)%istart+4*iadj-3) = WTT
!!!  W(this(ik)%istart+4*iadj-2) = WTN
!!!  W(this(ik)%istart+4*iadj-1) = WNT
!!!  W(this(ik)%istart+4*iadj  ) = WNN
!!!
!!!----------------------------------------------------------------------------------------------------
!!!
  real(kind=8),dimension(:),allocatable :: Wab
!
!-----------------------------------------------------
!
  integer          :: nb_CDAN,cpg_loop,nb_ENTITY

  integer          :: Nactif
!!!
  real(kind=8)     :: ALPHA,BETA,PWP,Enrg=0.0
!!!

  logical          :: PWPcheck      =.false., &
                      preconditioner=.false., &
                      norm_check    =.false., &
                      frictionless  =.false., &
                      conjugaison   =.true.
!!!
!!! Check test
!!!
  integer          :: i_checktype = 1,norm_fich
  character(len=5) :: checktype='Quad ' ! default value
!!!
  real(kind=8)     :: tol=0.1666D-03,inv_tol,Scale
  real(kind=8)     :: MeanDVoR,QuadDV,QuadDVR,sumWRR,sumWRWR
  real(kind=8)     :: sumDVoR,sumDVDV,sumDVDVRR,sumWDRDR,QuadDR
!!!
!!! RGR CRITIC
!!!
  real(kind=8),parameter :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)
!!!
!!! cpg check keyword

  integer,parameter :: i_Quad = 1 , i_Maxm = 2 , i_Stag = 3 , i_QuadN = 4

  ! map between interaction numbering in dedicated modules (DKDKx, DKJCx, etc) and interaction numbering in this module.
  ! For example, for a given interaction index in DKDKx (icdan_DKDKx), the coresponding index in this module (icdan_nlgs) is :
  !    icdan_nlgs = icdan_DKDKx + shift_icdan(i_dkdkx)
  ! N.B.: if there no interaction for a given tinteraction type, shift_icdan=-1. for example, if there's no DKJCx, then :
  !    shift(i_dkjcx) == -1
  integer, dimension(nb_interaction_types) :: shift_icdan

  public shift_icdan

  public &
       set_cpg_parameter, &
       ex_iter_cpg, & 
       ex_check_cpg, &
       write_norm_check_cpg, &
       ex_post_cpg, &
       ex_prep_cpg, &
       set_diag_precond_cpg, &
       set_frictionless, &
       set_no_conjugaison, &
       scale_rloc_cpg, &
       Nullify_EntityList_cpg

  public get_cpg_loop , get_cpg_contact_status

  private prjj_, injj_, vitrad_, &
          nullify_reac_        , &
          nullify_vlocy_       , &
          prep_cpg_aux_

contains

!------------------------------------------------------------------------ 
  subroutine set_cpg_parameter(normtype,tolerance)
    implicit none
    real(kind=8)     :: tolerance
    character(len=5) :: normtype

    tol = tolerance
    inv_tol = 1./tol

    select case(normtype)
    case('QuaN ')
       i_checktype = i_QuadN
    case('Quad ')
       i_checktype = i_Quad
    case('Maxm ')
       i_checktype = i_Maxm
    case('Stag ')
       i_checktype = i_Stag
    end select

  end subroutine set_cpg_parameter
!-----------------------------------------------------------
  subroutine ex_check_cpg(iconv)
    implicit none
    integer :: iconv

    iconv = 1

    call check_cpg

    select case(i_checktype)
    case(i_Quad,i_QuadN)
       meanDVoR = meanDVoR*inv_tol
       QuadDVR  =  QuadDVR*inv_tol
       QuadDV   =   QuadDV*inv_tol
       
       if ( PWPcheck .or. &
            ( ( QuadDV .lt. 1.D0 ) .and. &
             ( QuadDVR .lt. 1.D0 ) .and. &
             ( abs(meanDVoR) .lt. 1.D0 ) ) ) iconv = 0

    case(i_Stag)
       QuadDV   =   QuadDV*inv_tol
       
       if ( PWPcheck .or. ( QuadDV .lt. 1.D0 ) ) iconv = 0
       
    case(i_Maxm)
       
    end select

    Scale    = 1.D0-meanDVoR

  end subroutine ex_check_cpg
!-----------------------------------------------------------------
  subroutine ex_prep_cpg
    implicit none

    call prep_cpg

    Scale  = 0.D0

    cpg_solver=.true.
    cpg_loop  = 0

  end subroutine ex_prep_cpg
!-----------------------------------------------------------------
  subroutine set_diag_precond_cpg
    implicit none

    preconditioner = .true.
    
  end subroutine set_diag_precond_cpg
!--------------------------------------------------------------------
  subroutine set_frictionless
    implicit none
    
    frictionless = .true.
    
  end subroutine set_frictionless
!--------------------------------------------------------------------
  subroutine set_no_conjugaison
    implicit none

    conjugaison = .false.

  end subroutine set_no_conjugaison
!---------------------------------------------------------------------
  subroutine write_norm_check_cpg(istat)
    implicit none
    integer :: istat

    select case(istat)
    case(1)
       norm_check = .true.
       norm_fich  = get_io_unit() 
       open(UNIT=norm_fich,FILE='NORM_CPG.DAT',STATUS='REPLACE')
    case(2)
       if (norm_check) then
          write(norm_fich,'(3(2X,D14.7),3(1X,I7))') abs(meanDVoR),QuadDVR,QuadDV
       end if
    case(3)
       if (norm_check) close(norm_fich)
    end select

  end subroutine write_norm_check_cpg
!---------------------------------------------------------------------
  subroutine scale_rloc_cpg
    implicit none
    integer :: ik

    if (nb_CDAN .eq. 0) return

    Scale = DMIN1(Scale,1.1D0)
    Scale = DMAX1(Scale,0.9D0)      

    do ik=1,nb_CDAN
       this(ik)%rlt = this(ik)%rlt*Scale
       this(ik)%rln = this(ik)%rln*Scale
    end do
    
    ! Rnod = [H] Rloc
    do ik=1,nb_CDAN 
       call nullify_reac_(ik,iIreac)
    end do
    do ik=1,nb_CDAN
       call injj_(ik,this(ik)%rlt,this(ik)%rln,iIreac)
    end do

  end subroutine scale_rloc_cpg
!------------------------------------------------------------------------
  subroutine prep_cpg_aux_( id_inter, nb_inter, reac_mean, nb_CDAN )
    
    implicit none
    
    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ) :: nb_inter
    real( kind = 8 )    :: reac_mean
    integer( kind = 4 ) :: nb_CDAN

    ! Local variables
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: ik
    integer( kind = 4 ) :: icdent
    integer( kind = 4 ) :: ianent

    if ( nb_inter /= 0 ) shift_icdan( id_inter ) = nb_CDAN

    do icdan = 1, nb_inter

       ik             = nb_CDAN + icdan
       this(ik)%CDAN  = id_inter
       this(ik)%icdan = icdan

       call get_rloc( id_inter, icdan, this( ik )%rlt, this( ik )%rln, this( ik )%status )
       call get_vlocBEGIN( id_inter, icdan, this( ik )%vltBEGIN, this( ik )%vlnBEGIN, this( ik )%gapTTbegin, this( ik )%statusBEGIN )
       call get_internal( id_inter, icdan, this( ik )%internal )
       this(ik)%lawnb = get_tact_lawnb( id_inter, icdan )

       call inter2ENT( id_inter, icdan, icdent, ianent )
       this( ik )%icdent   = icdent
       this( ik )%ianent   = ianent
       reac_mean = reac_mean + this( ik )%rln
       
       entity( icdent )%ik                      = entity( icdent )%ik + 1
       entity( ianent )%ik                      = entity( ianent )%ik + 1
       entity( icdent )%list(entity(icdent)%ik) = ik
       entity( ianent )%list(entity(ianent)%ik) = ik

    end do
    nb_CDAN = nb_CDAN + nb_inter

  end subroutine prep_cpg_aux_
!------------------------------------------------------------------------
  subroutine prep_cpg
!!!
!!! Preparing input data
!!!
!!! extracting informations from the contribs in order to use 
!!! a shared solver
!!!
    implicit none

    character(len=15) :: IAM='cpg::prep_cpg'
    character(len=80) :: cout
    integer           :: errare=0
    integer           :: ik,ibehav,icdan,i,ient
    real(kind=8)      :: vtik,vnik,rtik,rnik,det,rtjl,rnjl
    
    ! contactors part 

    integer           :: nb_DKDKx,nb_DKKDx,nb_DKJCx,nb_PTPT2,nb_PLPLx,nb_PLJCx  !fd ,nb_P2P2x
    integer           :: nb_CLJCx,nb_CLALp,nb_P2P2L,nb_PLALp,nb_DKPLx,nb_DKALp,nb_DKDKL

    ! behaviours part

    real(kind=8)      :: fric,                         & ! friction coefficient
                         tangalrest,normalrest,        & ! restitution coefficients
                         normalcoh,tangalcoh,Wethk,    &
                         forcePERgap,forcePERstrain,   &
                         forcePERstrainrate,prestrain, &
                         WRt,WRn,                      &
!!! RGR CRITIC
                         ToverH,OTH,vOVERcv,cd_length, &
                         gap_tol

    integer           :: jl,iadj,jadj,nbadj,icdik,ianik,bandwith,ikjl,icdent,ianent,iistart,istart
    logical           :: inter,is_present=.false.

    integer :: ikadj,ikstart
    integer :: jladj,jlstart

    logical :: ok

    real(kind=8) :: reac_mean = 0.D0
 

    PWPcheck   = .false.
    reac_mean  = 0.D0

    !mr a voir
    nb_CDAN = 0

    ! mod_DKDKx
    nb_DKDKx = get_nb_inters( i_dkdkx )
    nb_CDAN  = nb_CDAN + nb_DKDKx
    
    ! mod_DKKDx
    nb_DKKDx = get_nb_inters( i_dkkdx )
    nb_CDAN  = nb_CDAN + nb_DKKDx
    
    ! mod_DKJCx
    nb_DKJCx = get_nb_inters( i_dkjcx )
    nb_CDAN  = nb_CDAN + nb_DKJCx
    
    ! mod_PTPT2
    nb_PTPT2 = get_nb_inters( i_ptpt2 )
    nb_CDAN  = nb_CDAN + nb_PTPT2
    
    ! mod_PLPLx
    nb_PLPLx = get_nb_inters( i_plplx )
    nb_CDAN  = nb_CDAN + nb_PLPLx
    
    ! mod_PLJCx
    nb_PLJCx = get_nb_inters( i_pljcx )
    nb_CDAN  = nb_CDAN + nb_PLJCx
    
    ! mod_CLALp
    nb_CLALp = get_nb_inters( i_clalp )
    nb_CDAN  = nb_CDAN + nb_CLALp
    
    ! mod_CLJCx
    nb_CLJCx = get_nb_inters( i_cljcx )
    nb_CDAN  = nb_CDAN + nb_CLJCx
    
    ! mod_P2P2L
    nb_P2P2L = get_nb_inters( i_p2p2l )
    nb_CDAN  = nb_CDAN + nb_P2P2L
    
    ! mod_PLALp
    nb_PLALp = get_nb_inters( i_plalp )
    nb_CDAN  = nb_CDAN + nb_PLALp
    
    ! mod_DKALp
    nb_DKALp = get_nb_inters( i_dkalp )
    nb_CDAN  = nb_CDAN + nb_DKALp
    
    ! mod_DKPLx
    nb_DKPLx = get_nb_inters( i_dkplx )
    nb_CDAN  = nb_CDAN + nb_DKPLx

    ! mod_DKDKL
    nb_DKDKL = get_nb_inters( i_dkdkl )
    nb_CDAN  = nb_CDAN + nb_DKDKL

    if (nb_CDAN .eq. 0) return 
!!!
!!!mr The list array has to be allocated here because the number of contact changes during the 
!!!   computation. The list array has to be  deallocated/reallocated after each updating of the contact list.

    nb_ENTITY = get_nb_ENTITY()

    call Create_EntityList   

    errare = 0
!!!
    do ient=1,nb_ENTITY
      
       if ( associated(entity(ient)%list) ) deallocate(entity(ient)%list)
       allocate(entity(ient)%list(entity(ient)%nb),stat=errare)
       if (errare /= 0) then
          call FATERR(IAM,'error allocating entity(ient)%list')
       end if
       entity(ient)%list = 0
       entity(ient)%ik   = 0
    end do
!!!
    errare = 0
!!!
    if (allocated(this) ) deallocate(this)
    allocate( this(nb_CDAN) , stat = errare )
    if ( errare /= 0 ) then
       call FATERR(IAM,'error allocating this')
    endif

    do ik=1,nb_CDAN
       this(ik)%rlt        = 0.D0
       this(ik)%rln        = 0.D0
       this(ik)%rlti       = 0.D0
       this(ik)%rlni       = 0.D0
       this(ik)%vltBEGIN   = 0.D0
       this(ik)%vlnBEGIN   = 0.D0
       this(ik)%vfreet     = 0.D0
       this(ik)%vfreen     = 0.D0
       this(ik)%covfreet   = 0.D0
       this(ik)%covfreen   = 0.D0     
       this(ik)%gapTTbegin = 0.D0
       this(ik)%status     = i_nknow
       this(ik)%statusBEGIN= i_nknow
       this(ik)%statuscheck= i_nknow
       this(ik)%forecast   = 1 !'acton'
!!!
       this(ik)%Wtt        = 0.D0
       this(ik)%Wtn        = 0.D0
       this(ik)%Wnt        = 0.D0
       this(ik)%Wnn        = 0.D0
       this(ik)%WWtt       = 1.D0
       this(ik)%WWnn       = 1.D0
       this(ik)%det        = 1.D0
       this(ik)%fric       = 0.D0
       this(ik)%lawnb      = 0
!!!
       this(ik)%WPt        = 0.D0
       this(ik)%WPn        = 0.D0
       this(ik)%WRt        = 0.D0
       this(ik)%WRn        = 0.D0
       this(ik)%Zt         = 0.D0
       this(ik)%Zn         = 0.D0
       this(ik)%Pt         = 0.D0
       this(ik)%Pn         = 0.D0
       this(ik)%Wt         = 0.D0
       this(ik)%Wn         = 0.D0
       this(ik)%Ut       = 0.D0
       this(ik)%Un       = 0.D0
!!!
       this(ik)%icdent     = 0
       this(ik)%ianent     = 0
!!!
       this(ik)%istart     = 0
       this(ik)%nbadj      = 0
       nullify(this(ik)%adjjl)
!!!
       this(ik)%internal = 0.d0
!!!
       this(ik)%i_law      = 0
!!!
    end do
    
    ! shifts are initialized to an impossible value
    shift_icdan = -1

    PWP = 0.D0
   
    nb_CDAN = 0

    call prep_cpg_aux_( i_dkdkx, nb_DKDKx, reac_mean, nb_CDAN )

    call prep_cpg_aux_( i_dkkdx, nb_DKKDx, reac_mean, nb_CDAN )

    call prep_cpg_aux_( i_dkjcx, nb_DKJCx, reac_mean, nb_CDAN )

    call prep_cpg_aux_( i_dkplx, nb_DKPLx, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_ptpt2, nb_PTPT2, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_plplx, nb_PLPLx, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_pljcx, nb_PLJCx, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_clalp, nb_CLALp, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_cljcx, nb_CLJCx, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_p2p2l, nb_P2P2L, reac_mean, nb_CDAN )
    
    call prep_cpg_aux_( i_plalp, nb_PLALp, reac_mean, nb_CDAN )

    call prep_cpg_aux_( i_dkalp, nb_DKALp, reac_mean, nb_CDAN )

    call prep_cpg_aux_( i_dkdkl, nb_DKDKL, reac_mean, nb_CDAN )

    if ( nb_CDAN /= 0 ) reac_mean = reac_mean/real(nb_CDAN,8)

!!!-----------------
!!! Rnod = [H] Rloc
!!!-----------------

    do ik=1,nb_CDAN  
       call nullify_reac_(ik,iIreac)
       call nullify_vlocy_(ik,iVaux_)
    end do

    do ik = 1,nb_CDAN
       call injj_(ik,this(ik)%rlt,this(ik)%rln,iIreac)
    end do
    
    bandwith = 0
    istart   = 0

    ! In this case the W matrix is going to be built and stored
    bandwith=0
    istart  = 0  

    do ik=1,nb_CDAN
       nbadj = 0
       icdik = this(ik)%icdent
       ianik = this(ik)%ianent
       
!!!mr computation of nb_adj for contact ik; the number of adjacent contact is equal to
!!!   the number of contact active for icdbdy plus the number of contact for ianbdy.
       if (icdik .eq. ianik) then
          !fd & mr autocontact (avec la langue)       
          nbadj = entity(icdik)%nb-1
       else
          nbadj = entity(icdik)%nb+entity(ianik)%nb-2
       endif

       jl = 0
       
       if (nbadj /= 0) then
            
!!!fd attention la desallocation doit se faire avec celle de this !!
!!!fd       if(associated(this(ik)%adjjl)) deallocate(this(ik)%adjjl)
          
          allocate(this(ik)%adjjl(nbadj),stat=errare)
          if (errare /= 0) then
             call FATERR(IAM,'error allocating this(ik)%adjjl')
          end if
          this(ik)%adjjl = 0
            
          do iadj=1,entity(icdik)%nb
             if(entity(icdik)%list(iadj) .eq. ik) cycle
             jl = jl+1
             this(ik)%adjjl(jl) = entity(icdik)%list(iadj)
          end do
            
          do iadj=1,entity(ianik)%nb
             if(entity(ianik)%list(iadj) .eq. ik) cycle
             
!!!fd evacuation des contacts partages
             is_present = .false.
!!! precedemment on a range entity(icdik)%nb-1 elements on les reparcours 
!!! a la recherche de doublons
             do jadj=1,entity(icdik)%nb-1
                if (this(ik)%adjjl(jadj) /= entity(ianik)%list(iadj)) cycle
                is_present = .true.
                exit
             end do
             if (is_present) cycle
             jl = jl+1
             this(ik)%adjjl(jl) = entity(ianik)%list(iadj)
          end do
       end if
       
       this(ik)%istart = istart
       istart = 4*jl + istart
       
       this(ik)%nbadj = jl
       bandwith = bandwith+jl
         
    end do
      
!!!fd paranoiac test
    do ient=1,nb_ENTITY
       if (entity(ient)%ik /= entity(ient)%nb) then
          write(cout,'(A33,I10)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
          call FATERR(IAM,'mismatch in the entity connectivity for')
       end if
       
    end do
    
    if ( allocated( Wab ) ) deallocate( Wab )
    allocate( Wab(4*bandwith) , stat=errare )
    Wab = 0.D0
    if (errare /= 0) then
       call FATERR(IAM,'error allocating Wab')
    end if

!!!--------------------------------------------------
!!! computing local dynamic matrix W(ik,ik)
!!!--------------------------------------------------

    do ik=1,nb_CDAN
       
       rnik = 0.D0
       rtik = 1.D0
       call nullify_reac_(ik,iIaux_)
       call injj_(ik,rtik,rnik,iIaux_)
       call vitrad_(ik,iVaux_e_invM_t_Iaux_)
       call prjj_(ik,vtik,vnik,iVaux_)

       this(ik)%Wtt = vtik
       this(ik)%Wnt = vnik

       do ikadj=1,this(ik)%nbadj

          jl = this(ik)%adjjl(ikadj)
          jlstart = this(jl)%istart

          call prjj_(jl,vtik,vnik,iVaux_)
 
          ok = .false.

          do jladj=1,this(jl)%nbadj
          
             if (this(jl)%adjjl(jladj) .eq. ik) then
                Wab(jlstart + 4*jladj-3) =vtik
                Wab(jlstart + 4*jladj-1) =vnik
                ok = .true.
             end if
             
          end do

          if (.not. ok) then               
             call FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
          end if
          
       end do

       call nullify_vlocy_(ik,iVaux_)

       rnik = 1.D0
       rtik = 0.D0
       call nullify_reac_(ik,iIaux_)
       call injj_(ik,rtik,rnik,iIaux_)
       call vitrad_(ik,iVaux_e_invM_t_Iaux_)
       call prjj_(ik,vtik,vnik,iVaux_)
       this(ik)%Wtn = vtik
       this(ik)%Wnn = vnik

       do ikadj=1,this(ik)%nbadj
        
          jl=this(ik)%adjjl(ikadj)
          jlstart = this(jl)%istart

          call prjj_(jl,vtik,vnik,iVaux_)
          
          ok = .false.
          
          do jladj=1,this(jl)%nbadj
             
             if (this(jl)%adjjl(jladj) .eq. ik) then
                Wab(jlstart + 4*jladj-2) =vtik
                Wab(jlstart + 4*jladj  ) =vnik
                ok = .true.
             endif
             
          end do
          
          if (.not. ok) then
             call FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
          end if

       end do

       call nullify_vlocy_(ik,iVaux_)

!!! --------------------------------------
!!! Warning and coping with critical cases
!!! --------------------------------------

       if (this(ik)%Wnn .le. 1.D-18) then
          write(cout,543) ik,this(ik)%Wnn
543       format(1X,'  Wnn(',I5,') =',D12.5,' < 1.D-18')
          call FATERR(IAM,cout)
       end if
       
       if (this(ik)%Wtt .le. 1.D-06*this(ik)%Wnn) then
          write(cout,544)ik,this(ik)%Wtt,ik
544       format(1X,'   Wtt(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
          call LOGMES(cout)
          this(ik)%Wtt=1.D-06*this(ik)%Wnn
       end if

!!! setting statuscheck and status to 'nknow'

       this(ik)%statuscheck= this(ik)%statusBEGIN

!!!*************************************
!!! Preparing auxiliaries
!!!     
!!!! default values

       ibehav=this(ik)%lawnb
       
       this(ik)%fric =  get_fric(ibehav,this(ik)%statusBEGIN)
       
       this(ik)%WWtt = this(ik)%Wtt
       this(ik)%WWnn = this(ik)%Wnn
       
       this(ik)%covfreet = 0.D0
       this(ik)%covfreen = 0.D0     

!!! preparing auxiliaries--------------------------------------------
       
       select case(tact_behav(ibehav)%lawty)
!!!---------123456789012345678901234567890--
       case('IQS_CLB                       ')
          this(ik)%i_law   = i_IQS_CLB
          this(ik)%covfreen= max(0.D0,this(ik)%gapTTbegin/H)         
!!!-------------------------------------
       case('IQS_DS_CLB                    ')
          this(ik)%i_law = i_IQS_DS_CLB
          this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)         
!!!-------------------------------------
       case('RST_CLB                       ')
          this(ik)%i_law = i_RST_CLB
          
!!! This unilateral condition prescribes that the normal relative velocity should satisfy 
!!! a complementary condition together with the normal reaction force, as soon as a contact
!!! occurs. Since deciding the occurence of a contact is in these circumstances 
!!! a matter of numerical approximation, it may be decided that a contact is forecasted
!!! when the predicted gap at some time between 0.D0 and H (from the beginning of the time step)
!!! becomes negative. 
!!! It is relevant to use this(ik)%gapTTbegin as a forecast criterium. 

          if (this(ik)%gapTTbegin .le. 0.D0) then
             ! this(ik)%forecast='acton' (default is 'acton')
             !fd needs further checking by !mj
             call get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN
          else
             this(ik)%forecast= 0
             this(ik)%rlt     = 0.D0
             this(ik)%rln     = 0.D0
             this(ik)%status  = i_noctc
          end if
!!!-------------------------------------
       case('RST_DS_CLB                    ')
          this(ik)%i_law = i_RST_DS_CLB
          
          if (this(ik)%gapTTbegin .le. 0.D0) then
             ! this(ik)%forecast='acton' (default is 'acton')
             !fd needs further checking by !mj
             call get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN
          else
             this(ik)%forecast= 0
             this(ik)%rlt     = 0.D0
             this(ik)%rln     = 0.D0
             this(ik)%status  = i_noctc
          end if
!!!-------------------------------------
       case('GAP_SGR_CLB                   ')
          this(ik)%i_law = i_GAP_SGR_CLB
          
          this(ik)%covfreen=this(ik)%gapTTbegin/H
          
          if ( ( this(ik)%CDAN == i_CLALp ) .or. &
               ( this(ik)%CDAN == i_CLJCx ) ) then
             cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
          end if

          this(ik)%internal(1) = cd_length
!!!-------------------------------------
       case('VEL_SGR_CLB                   ')
          this(ik)%i_law = i_VEL_SGR_CLB

!!! This unilateral condition prescribes that the normal relative velocity should satisfy 
!!! a complementary condition together with the normal reaction force, as soon as a contact
!!! occurs. Since deciding the occurence of a contact is in these circumstances 
!!! a matter of numerical approximation, it may be decided that a contact is forecasted
!!! when the predicted gap at some time between 0.D0 and H (from the beginning of the time step)
!!! becomes negative. 
!!! It is relevant to use this(ik)%gapTTbegin as a forecast criterium. 

          if (this(ik)%gapTTbegin .le. 0.D0) then
             ! this(ik)%forecast='acton' (default is 'acton')
          else
             this(ik)%forecast= 0
             this(ik)%rlt     = 0.D0
             this(ik)%rln     = 0.D0
             this(ik)%status  = i_noctc
          end if
          
          if ( ( this(ik)%CDAN == i_CLALp ) .or. &
               ( this(ik)%CDAN == i_CLJCx ) ) then
             cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
          end if

          this(ik)%internal(1) = cd_length
!!!-------------------------------------
       case('GAP_SGR_DS_CLB                ')
          this(ik)%i_law = i_GAP_SGR_DS_CLB
          this(ik)%covfreen = this(ik)%gapTTbegin/H
          
          if ( ( this(ik)%CDAN == i_CLALp ) .or. &
               ( this(ik)%CDAN == i_CLJCx ) ) then
             cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
          end if

          this(ik)%internal(1) = cd_length
!!!-------------------------------------
       case('ELASTIC_REPELL_CLB            ')
          this(ik)%i_law = i_ELASTIC_REPELL_CLB
          call get_forcePERgap(ibehav,forcePERgap)
          this(ik)%WWnn    = this(ik)%Wnn+1.D0/(forcePERgap*H*H)
          this(ik)%covfreen= this(ik)%gapTTbegin/H
!!!-------------------------------------
       case('ELASTIC_ROD                   ')
          this(ik)%i_law = i_ELASTIC_ROD
          this(ik)%gapREF=this(ik)%internal(1)
          
          if (this(ik)%gapREF .le. 1.D-18) then
             write(cout,555) ik,this(ik)%gapREF
             call FATERR(IAM,cout)
          end if
          
          call get_forcePERstrain(ibehav,forcePERstrain)
          call get_prestrain(ibehav,prestrain)
          this(ik)%WWnn    = this(ik)%Wnn+(this(ik)%gapREF/(forcePERstrain*H*H))
          this(ik)%covfreen=(this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H
!!!fd paranoiac initializations
          this(ik)%Wtt = 0.D0; this(ik)%Wnt = 0.D0; this(ik)%Wtn = 0.D0
          this(ik)%WWtt= 0.D0
!!!-------------------------------------
       case('ELASTIC_WIRE                  ')
          this(ik)%i_law = i_ELASTIC_WIRE
          
          this(ik)%gapREF=this(ik)%internal(1)
          
          if (this(ik)%gapREF .le. 1.D-18) then
             write(cout,555) ik,this(ik)%gapREF
             call FATERR(IAM,cout)
          end if
          
          call get_forcePERstrain(ibehav,forcePERstrain)
          call get_prestrain(ibehav,prestrain)
          this(ik)%WWnn    = this(ik)%Wnn+(this(ik)%gapREF/(forcePERstrain*H*H))
          this(ik)%covfreen=(this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H
!!!-------------------------------------
       case('VOIGT_ROD                     ')
          this(ik)%i_law = i_VOIGT_ROD
          
          this(ik)%gapREF=this(ik)%internal(1)
          
          if (this(ik)%gapREF .le. 1.D-18) then
             write(cout,555) ik,this(ik)%gapREF
             call FATERR(IAM,cout)
          end if
          
          call get_forcePERstrain(ibehav,forcePERstrain)
          call get_prestrain(ibehav,prestrain)
          call get_forcePERstrainrate(ibehav,forcePERstrainrate)
          this(ik)%WWnn    = this(ik)%Wnn+(this(ik)%gapREF/((forcePERstrain*H*H)+(forcePERstrainrate*H)))
          this(ik)%covfreen=(H*forcePERstrain/(H*forcePERstrain+forcePERstrainrate))* &
               (this(ik)%gapTTbegin-(1.d0+prestrain)*this(ik)%gapREF)/H
!!!fd paranoiac initializations
          this(ik)%Wtt=0.D0;this(ik)%Wnt=0.D0;this(ik)%Wtn=0.D0
          this(ik)%WWtt=0.D0
!!!-------------------------------------
       case('VOIGT_WIRE                    ')
          this(ik)%i_law = i_VOIGT_WIRE
          
          this(ik)%gapREF=this(ik)%internal(1)
          
          if (this(ik)%gapREF .le. 1.D-18) then
             write(cout,555) ik,this(ik)%gapREF
             call FATERR(IAM,cout)
          end if
          
          call get_forcePERstrain(ibehav,forcePERstrain)
          call get_prestrain(ibehav,prestrain)
          call get_forcePERstrainrate(ibehav,forcePERstrainrate)
          this(ik)%WWnn=this(ik)%Wnn+(this(ik)%gapREF/((forcePERstrain*H*H)+(forcePERstrainrate*H)))
          this(ik)%covfreen = (H*forcePERstrain/(H*forcePERstrain+forcePERstrainrate))* &
               (this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H
!!!---------------------------------------
       case('RIGID_WIRE                    ')
          this(ik)%i_law = i_RIGID_WIRE
          
          this(ik)%gapREF=this(ik)%internal(1)
          this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%gapREF)/H      
!!!---------------------------------------
       case default
          call LOGMES('WARNING: default case selected')
          call LOGMES(tact_behav(ibehav)%lawty)
       end select
!!!
555    format(1X,'  this(',I5,')%gapREF =',D12.5,' < 1.D-18')
!!!
!!!------------------------------
!!! Warning non uniqueness cases
!!!------------------------------
       
       det = this(ik)%WWtt*this(ik)%WWnn-this(ik)%Wtn*this(ik)%Wnt
       this(ik)%det=det
       
       if (det .lt. 1.D-24) then
          write(cout,545)ik,det
545       format(1X,'    WWtt*WWnn-Wtn*Wnt (',I5,') =',D12.5,' < 1.D-24')
          call LOGMES(cout)
       end if
       
!!!--------------------------------------------------
!!! Computing free velocity for each contact
!!!--------------------------------------------------
       
       call prjj_(ik,this(ik)%vfreet,this(ik)%vfreen,iVfree)
       
    end do
    
!!!-----------------------------------
!!! Computing the initial residue
!!!-----------------------------------
    
    if (frictionless) then
       
       do ik = 1,nb_CDAN
          
          WRt = 0.D0
          WRn = 0.D0
          
          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 4*iadj
             WRn = WRn + Wab(iistart  )*this(ikjl)%rln
          end do
          
          this(ik)%WRn  = WRn+this(ik)%Wnn*this(ik)%rln
          this(ik)%Un = -(this(ik)%vfreen+this(ik)%covfreen)-this(ik)%WRn
          if(this(ik)%forecast .eq. 0) cycle
          this(ik)%wn  = this(ik)%Un
          this(ik)%Pn  = this(ik)%wn
       end do
       
       if ( preconditioner ) then
          do ik = 1,nb_CDAN
             if(this(ik)%forecast .eq. 0) cycle
             this(ik)%wn = this(ik)%Un/this(ik)%Wnn
             this(ik)%Pn  = this(ik)%wn
          end do
       end if
    else
       do ik = 1,nb_CDAN
          
          WRt = 0.D0
          WRn = 0.D0
          
          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl = this(ik)%adjjl(iadj)
             iistart = istart + 4*iadj
             WRt = WRt + Wab(iistart-3)*this(ikjl)%rlt + Wab(iistart-2)*this(ikjl)%rln
             WRn = WRn + Wab(iistart-1)*this(ikjl)%rlt + Wab(iistart  )*this(ikjl)%rln
          end do
          
          this(ik)%WRt = WRt + this(ik)%Wtt*this(ik)%rlt + this(ik)%Wtn*this(ik)%rln
          this(ik)%WRn = WRn + this(ik)%Wnt*this(ik)%rlt + this(ik)%Wnn*this(ik)%rln

          this(ik)%Ut = -( this(ik)%vfreet + this(ik)%covfreet + this(ik)%WRt)
          this(ik)%Un = -( this(ik)%vfreen + this(ik)%covfreen + this(ik)%WRn)

          if(this(ik)%forecast .eq. 0) cycle
          
          this(ik)%Wt  = this(ik)%Ut
          this(ik)%Wn  = this(ik)%Un
          this(ik)%Pt  = this(ik)%Wt
          this(ik)%Pn  = this(ik)%Wn
          
       end do
       
       if (preconditioner) then
          do ik = 1,nb_CDAN
             if (this(ik)%forecast .eq. 0) cycle
             this(ik)%Wn = this(ik)%Un/this(ik)%Wnn
             this(ik)%Wt = 0.0!this(ik)%Ut/this(ik)%Wtt
             this(ik)%Pt = 0.0!this(ik)%wt
             this(ik)%Pn = this(ik)%wn
          end do
       end if
    end if
    
  end subroutine prep_cpg
!------------------------------------------------------------------------  
  subroutine ex_iter_cpg()
    implicit none
    integer(kind=4)   :: ik,iadj,ikjl,Nstat,istart,iistart,ibehav
    real(kind=8)      :: WPt,WPn,WRt,WRn,WRti,WRni,Wdrn,Wdrt
    real(kind=8)      :: RWR,bR
    real(kind=8)      :: rlnik,rltik,rlniki,rltiki,Pnik,Ptik
    character(len=13) :: IAM
    character(len=80) :: cout
   
    IAM = 'cpg::ex_iter_cpg'
    cpg_loop = cpg_loop + 1

    Nactif = 0
    PWP    = 0.D0
   
    if ( frictionless ) then
       
!!!-- FRICTIONLESS CASE -----------------------------------

       !$OMP PARALLEL DEFAULT(SHARED)             &
       !$OMP PRIVATE(ik,istart,ikjl,iistart,iadj, &
       !$OMP         Wpn,Pnik,                    &
       !$OMP         rlnik,ibehav,Nstat,WRn,WRni, &
       !$OMP         rlniki,Wdrn)
       
       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:PWP)
       do ik = 1,nb_CDAN

          WPn = 0.D0

          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 4*iadj
             WPn = WPn + Wab(iistart  )*this(ikjl)%Pn
          end do
          
          Pnik  = this(ik)%Pn
          WPn = WPn + this(ik)%Wnn*Pnik
          
          PWP = PWP + WPn*Pnik
          this(ik)%WPn=WPn
       end do
       !$OMP END DO

       !$OMP SINGLE
       if ( abs(PWP) < 1.D-16 ) PWPcheck=.true.

       ALPHA = 0.D0
       !$OMP END SINGLE

       if ( .not.PWPcheck ) then

          !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:ALPHA)
          do ik=1,nb_CDAN
             if(this(ik)%forecast .eq. 0) cycle
             ALPHA = ALPHA + this(ik)%Un*this(ik)%Pn
          end do
          !$OMP END DO
          
          !$OMP SINGLE
          ALPHA  = ALPHA/PWP
          BETA = 0.D0
          Enrg = 0.D0
          RWR  = 0.D0
          bR   = 0.D0
          !$OMP END SINGLE

          !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:Nactif)
          do ik=1,nb_CDAN
             if(this(ik)%forecast .eq. 0) cycle
          
             rlnik = this(ik)%rln
             rlniki= rlnik + ALPHA*this(ik)%Pn
          
             if ( rlniki .le. 0.D0 ) then
                ! No contact
                this(ik)%rln = 0.D0
                this(ik)%statuscheck = i_noctc
             else
                Nactif = Nactif + 1
                this(ik)%rln = rlniki
                this(ik)%statuscheck = i_slide
             end if
             this(ik)%rlni = this(ik)%rln - rlnik
          end do
          !$OMP END DO
       
          !$OMP DO SCHEDULE(RUNTIME)
          do ik=1,nb_CDAN
          
             WRn = 0.D0
          
             istart = this(ik)%istart
          
             do iadj=1,this(ik)%nbadj
                ikjl=this(ik)%adjjl(iadj)
                iistart = istart + 4*iadj
                WRn = WRn + Wab(iistart)*this(ikjl)%rln
             end do
          
             rlniki = this(ik)%rlni
             rlnik  = this(ik)%rln
             
             WRn = WRn + this(ik)%Wnn*rlnik
             
             this(ik)%WRn = WRn          
             this(ik)%Un  = -(this(ik)%vfreen+this(ik)%covfreen+WRn)

             this(ik)%status = this(ik)%statuscheck
             
          end do
          !$OMP END DO

          if (norm_check) then
             !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:RWR,bR)
             do ik=1,nb_CDAN
                RWR = RWR + this(ik)%WRn*this(ik)%rln
                bR  = bR  + (this(ik)%vfreen+this(ik)%covfreen)*this(ik)%rln
             end do
             !$OMP END DO
             !$OMP SINGLE
             Enrg = 0.5*RWR + bR
             !$OMP END SINGLE
          end if
       
          if(preconditioner)then
             !$OMP DO SCHEDULE(RUNTIME)
             do ik=1,nb_CDAN
                this(ik)%Un   = this(ik)%Un/this(ik)%Wnn
             end do
             !$OMP END DO
          end if
              
!!! Gradient Projections

          !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:BETA)
          do ik=1,nb_CDAN
             if(this(ik)%forecast .eq. 0) cycle
             if(this(ik)%status .eq. i_noctc)then
                ! Status de non contact
                ! Premiere projection: r -> w
                if(this(ik)%Un <= 0.D0)then
                   this(ik)%Wn = 0.D0
                   this(ik)%Zn = 0.D0
                else
                   this(ik)%Wn = this(ik)%Un
                   if(this(ik)%Pn <= 0.D0)then
                      this(ik)%Zn = 0.D0
                   else
                      this(ik)%Zn = this(ik)%Pn
                   end if
                end if
             else
                this(ik)%Wn = this(ik)%Un
                this(ik)%Zn = this(ik)%Pn
             end if
             BETA = BETA + this(ik)%WPn*this(ik)%Wn
          end do
          !$OMP END DO
          
          !$OMP SINGLE
          BETA =-BETA/PWP
          !$OMP END SINGLE
          
          if (conjugaison) then
             !$OMP DO SCHEDULE(RUNTIME)
             do ik=1,nb_CDAN
                if(this(ik)%forecast .eq. 0) cycle
                this(ik)%Pn = this(ik)%Wn+BETA*this(ik)%Zn
             end do
             !$OMP END DO
          else
             !$OMP DO SCHEDULE(RUNTIME)
             do ik=1,nb_CDAN
                if(this(ik)%forecast .eq. 0) cycle
                this(ik)%Pn = this(ik)%Wn
             end do
             !$OMP END DO
          end if
       end if
       !$OMP END PARALLEL


    else
!!!
!!! FRICTIONAL CASE ------------------------------------------
!!!       
       !$OMP PARALLEL DEFAULT(SHARED)                               &
       !$OMP PRIVATE(ik,WPt,WPn,istart,ikjl,iistart,iadj,Pnik,Ptik, &
       !$OMP         rlnik,rltik,ibehav,Nstat,WRt,WRn,WRti,WRni, &
       !$OMP         rlniki,rltiki,Wdrt,Wdrn)
       
       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:PWP)
       do ik=1,nb_CDAN
          
          WPt= 0.D0
          WPn= 0.D0

          istart = this(ik)%istart
          
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 4*iadj
             WPt = WPt + Wab(iistart-3)*this(ikjl)%Pt + Wab(iistart-2)*this(ikjl)%Pn
             WPn = WPn + Wab(iistart-1)*this(ikjl)%Pt + Wab(iistart  )*this(ikjl)%Pn
          end do
          
          Pnik  = this(ik)%Pn
          Ptik  = this(ik)%Pt
          
          WPt = WPt + this(ik)%Wtt*Ptik + this(ik)%Wtn*Pnik
          WPn = WPn + this(ik)%Wnt*Ptik + this(ik)%Wnn*Pnik
          
          PWP = PWP + WPt*Ptik + WPn*Pnik
          this(ik)%WPt=WPt
          this(ik)%WPn=WPn

       end do
       !$OMP END DO
       
       !$OMP SINGLE
       if ( abs(PWP) < 1.D-16) PWPcheck=.true.
       
       ALPHA = 0.D0
       !$OMP END SINGLE

       if ( .not.PWPcheck ) then
 
          !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:ALPHA)
          do ik=1,nb_CDAN
             if(this(ik)%forecast .eq. 0) cycle
             ALPHA = ALPHA+(this(ik)%Ut*this(ik)%Pt+this(ik)%Un*this(ik)%Pn)
          end do
          !$OMP END DO
          
          !$OMP SINGLE
          ALPHA  = ALPHA/PWP
          
          Enrg      = 0.D0
          RWR       = 0.D0
          bR        = 0.D0
          !$OMP END SINGLE

          !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:Nactif)
          do ik=1,nb_CDAN
             
             if (this(ik)%forecast .eq. 0) cycle
             ibehav= this(ik)%lawnb
             
             rlnik = this(ik)%rln
             rltik = this(ik)%rlt
             
             select case(this(ik)%i_law)
             case(i_ELASTIC_ROD,i_VOIGT_ROD)
                this(ik)%rlt = 0.D0
                this(ik)%rln = -(this(ik)%WRn-this(ik)%vfreen+this(ik)%covfreen)/this(ik)%WWnn-rlnik
                this(ik)%statuscheck=i_stick
                Nactif = Nactif + 1
                
             case default
                
                this(ik)%rlni = rlnik + ALPHA*this(ik)%Pn
                this(ik)%rlti = rltik + ALPHA*this(ik)%Pt

                call proj_cone(ik,Nstat)
                Nactif = Nactif + Nstat
                
             end select

             this(ik)%rlni = this(ik)%rln - rlnik
             this(ik)%rlti = this(ik)%rlt - rltik

          end do
          !$OMP END DO

          !$OMP DO SCHEDULE(RUNTIME)
          do ik=1,nb_CDAN
             
             WRt = 0.D0
             WRn = 0.D0
             WRti= 0.D0
             WRni= 0.D0
             
             istart = this(ik)%istart
             
             do iadj=1,this(ik)%nbadj
                ikjl=this(ik)%adjjl(iadj)
                iistart = istart + 4*iadj
                WRt = WRt + Wab(iistart-3)*this(ikjl)%rlt + Wab(iistart-2)*this(ikjl)%rln
                WRn = WRn + Wab(iistart-1)*this(ikjl)%rlt + Wab(iistart  )*this(ikjl)%rln
             end do
     

             rlniki = this(ik)%rlni
             rltiki = this(ik)%rlti
             
             rlnik = this(ik)%rln
             rltik = this(ik)%rlt
                
             WRti= this(ik)%Wtt*rltik+this(ik)%Wtn*rlnik
             WRni= this(ik)%Wnt*rltik+this(ik)%Wnn*rlnik
                
             WRt = WRt+WRti
             WRn = WRn+WRni
                
             this(ik)%Ut   = -(this(ik)%vfreet+this(ik)%covfreet+WRt)
             this(ik)%Un   = -(this(ik)%vfreen+this(ik)%covfreen+WRn)
                
             this(ik)%status = this(ik)%statuscheck
             
             this(ik)%WRt = WRt
             this(ik)%WRn = WRn
          end do
          !$OMP END DO
          
          if(norm_check)then
             !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:RWR,bR)
             do ik=1,nb_CDAN
                RWR = RWR + this(ik)%WRt*this(ik)%rlt + this(ik)%WRn*this(ik)%rln
                bR  = bR  + (this(ik)%vfreet+this(ik)%covfreet)*this(ik)%rlt + (this(ik)%vfreen+this(ik)%covfreen)*this(ik)%rln
             end do
             !$OMP END DO
             !$OMP SINGLE
             Enrg = 0.5*RWR + bR
             !$OMP END SINGLE
          end if
          
          if (preconditioner) then
             !$OMP DO SCHEDULE(RUNTIME)
             do ik=1,nb_CDAN
                this(ik)%Ut   = this(ik)%Ut/this(ik)%Wtt
                this(ik)%Un   = this(ik)%Un/this(ik)%Wnn
             end do
             !$OMP END DO
             
          end if

          !$OMP SINGLE

          BETA = 0.D0
          !$OMP END SINGLE

          !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:BETA)
          do ik=1,nb_CDAN
             if(this(ik)%forecast .eq. 0) cycle
             select case(this(ik)%i_law)
             case(i_ELASTIC_ROD,i_VOIGT_ROD)
                this(ik)%Wn = 0.D0
                this(ik)%Wt = 0.D0
                this(ik)%Zn = 0.D0
                this(ik)%Zt = 0.D0
             case default
                call proj_facette(ik)
             end select
             BETA = BETA + this(ik)%WPt*this(ik)%Wt + this(ik)%WPn*this(ik)%Wn
          end do
          !$OMP END DO
          
          !$OMP SINGLE
          BETA =-BETA/PWP
          !$OMP END SINGLE
          
          if (conjugaison) then
             !$OMP DO SCHEDULE(RUNTIME)
             do ik = 1,nb_CDAN
                if(this(ik)%forecast .eq. 0) cycle
                this(ik)%Pt = this(ik)%Wt+BETA*this(ik)%Zt
                this(ik)%Pn = this(ik)%Wn+BETA*this(ik)%Zn
             end do
             !$OMP END DO
          else
             !$OMP DO SCHEDULE(RUNTIME)
             do ik = 1,nb_CDAN
                if(this(ik)%forecast .eq. 0) cycle
                this(ik)%Pt = this(ik)%Wt
                this(ik)%Pn = this(ik)%Wn
             end do
             !$OMP END DO
          end if
       end if
       !$OMP END PARALLEL
    end if
    
  end subroutine ex_iter_cpg
!------------------------------------------------------------------------  
! SUBROUTINE check_cpg
! 
! CHECK CPG ALGORITHM CONVERGENCE ACCORDING TO SELECTED TEST
!
!------------------------------------------------------------------------  
  subroutine check_cpg()
    implicit none
    integer(kind=4) :: ik
    real(kind=8)    :: DVDV,rlniki,rltiki,rlnik,rltik
    real(kind=8)    :: Wdrt,Wdrn,WRti,WRni 

    meanDVoR  = 0.D0
    QuadDVR   = 0.D0
    QuadDV    = 0.D0
    sumWRR    = 0.D0
    sumDVoR   = 0.D0
    sumWRWR   = 0.D0
    sumDVDV   = 0.D0
    sumDVDVRR = 0.D0
    DVDV      = 0.D0

    !$OMP PARALLEL DEFAULT(SHARED)                               &
    !$OMP PRIVATE(ik,rlniki,rltiki,rlnik,rltik,WRti,WRni,        &
    !$OMP          Wdrt,Wdrn,DVDV)
    
    select case(i_checktype)
    case(i_Quad)

       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:sumWRR,sumWRWR,sumDVoR,sumDVDV,sumDVDVRR)
       do ik=1,nb_CDAN
        
          rlniki = this(ik)%rlni
          rltiki = this(ik)%rlti
        
          rlnik = this(ik)%rln
          rltik = this(ik)%rlt
        
          WRti = this(ik)%Wtt*rltik + this(ik)%Wtn*rlnik
          WRni = this(ik)%Wnt*rltik + this(ik)%Wnn*rlnik
        
          sumWRR  = sumWRR  + WRti*rltik + WRni*rlnik
          sumWRWR = sumWRWR + WRti*WRti + WRni*WRni
        
          Wdrt = this(ik)%Wtt*rltiki+this(ik)%Wtn*rlniki
          Wdrn = this(ik)%Wnt*rltiki+this(ik)%Wnn*rlniki
        
          DVDV = Wdrt*Wdrt + Wdrn*Wdrn
        
          sumDVoR   = sumDVoR + Wdrt*rltik + Wdrn*rlnik 
          sumDVDV   = sumDVDV + DVDV
          sumDVDVRR = sumDVDVRR + DVDV*(rlnik*rlnik+rltik*rltik)
       end do
       !$OMP END DO

    case(i_QuadN)

       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:sumWRR,sumWRWR,sumDVoR,sumDVDV,sumDVDVRR)
       do ik=1,nb_CDAN
   
          rlniki = this(ik)%rlni
          rlnik = this(ik)%rln
          
          WRni = this(ik)%Wnn*rlnik
                
          sumWRR  = sumWRR  + WRni*rlnik
          sumWRWR = sumWRWR + WRni*WRni
                
          Wdrn= this(ik)%Wnn*rlniki
                
          DVDV = Wdrn*Wdrn
                
          sumDVoR   = sumDVoR + Wdrn*rlnik 
          sumDVDV   = sumDVDV + DVDV
          sumDVDVRR = sumDVDVRR + DVDV*(rlnik*rlnik)
       end do
       !$OMP END DO

    case(i_Maxm)

       ! not yet define

    case(i_Stag)
       
       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:sumWRWR,sumDVDV)
       do ik=1,nb_CDAN  
          rlniki = this(ik)%rlni
          rltiki = this(ik)%rlti
          
          rlnik = this(ik)%rln
          rltik = this(ik)%rlt
          
          sumDVDV   = sumDVDV + rlniki*rlniki + rltiki*rltiki
          sumWRWR   = sumWRWR + rlnik*rlnik   + rltik*rltik
       end do
       !$OMP END DO

    end select
    
    !$OMP SINGLE
    if(sumWRR  < 1.D-16) sumWRR  = 1.D0
    if(sumWRWR < 1.D-16) sumWRWR = 1.D0

    if(bavard)then
       write(*,*) '@ sumDvoR  =',sumDvoR,'@ sumWRR= ',sumWRR
       write(*,*) '@ sumDVDV  =',sumDVDV,'@ sumWRWR=',sumWRWR
       write(*,*) '@ sumDVDVRR=',sumDVDVRR
    end if
   
    MeanDVoR  =  sumDvoR/sumWRR
    QuadDV    =  sqrt(sumDVDV/sumWRWR)
    QuadDVR   =  sqrt(Nactif*sumDVDVRR)/sumWRR

    !$OMP END SINGLE
    !$OMP END PARALLEL

  end subroutine check_cpg
!------------------------------------------------------------------------  
  subroutine proj_facette(icdan)
    implicit none
    integer,intent(in) :: icdan
    
    if(this(icdan)%status .eq. i_noctc)then
       ! Status de non contact
       ! Premiere projection: r -> w
       if(this(icdan)%Un <= 0.D0)then
          this(icdan)%Wn = 0.D0
          this(icdan)%Wt = 0.D0
          this(icdan)%Zn = 0.D0
          this(icdan)%Zt = 0.D0
       else
          this(icdan)%wn = this(icdan)%Un
          this(icdan)%wt = 0.0
          ! Seconde projection: p -> z
          if(this(icdan)%Pn <= 0.D0)then
             this(icdan)%Zn = 0.D0
             this(icdan)%Zt = 0.D0
          else
             this(icdan)%Zn = this(icdan)%Pn
             this(icdan)%Zt = 0.0
          end if
       end if
    else if ( this(icdan)%status .eq. i_slibw ) then
       ! Status de contact glissant 
       this(icdan)%Wn = this(icdan)%Un
       this(icdan)%Zn = this(icdan)%Pn

       this(icdan)%Wt = min( 0.D0 , this(icdan)%Ut )
       if ( this(icdan)%Wt .eq. 0.D0 ) then
          this(icdan)%Zt = 0.D0
       else
          this(icdan)%Zt = min( 0.D0 , this(icdan)%Pt )
       end if
    else if (this(icdan)%status .eq. i_slifw) then
       ! Status de contact glissant
       this(icdan)%Wn = this(icdan)%Un
       this(icdan)%Zn = this(icdan)%Pn

       this(icdan)%Wt = max( 0.D0 , this(icdan)%Ut )
       if(this(icdan)%Wt .eq. 0.D0)then
          this(icdan)%Zt = 0.D0
       else
          this(icdan)%Zt = max( 0.D0 , this(icdan)%Pt )
       end if
    else
       ! Status de contact adherent
       this(icdan)%Wn = this(icdan)%Un
       this(icdan)%Wt = this(icdan)%Ut
       
       this(icdan)%Zn = this(icdan)%Pn
       this(icdan)%Zt = this(icdan)%Pt
    end if

  end subroutine proj_facette
!------------------------------------------------------------------------  
  subroutine proj_cone(icdan,Nstat)
    implicit none
    integer,intent(in)  :: icdan
    integer,intent(out) :: Nstat
    real(kind=8)        :: murnik

    if(this(icdan)%rlni <= 0.D0)then
       ! No contact
       this(icdan)%rln = 0.D0
       this(icdan)%rlt = 0.D0
       this(icdan)%statuscheck = i_noctc
       Nstat = 0
    else
       Nstat = 1
       this(icdan)%rln = this(icdan)%rlni

       ! Frottement de Coulomb
       murnik = this(icdan)%rlni*this(icdan)%fric

       if (this(icdan)%rlti .le. -murnik ) then
          this(icdan)%rlt = -murnik
          this(icdan)%statuscheck = i_slifw
       else if(this(icdan)%rlti .ge. murnik )then
          this(icdan)%rlt = murnik
          this(icdan)%statuscheck = i_slibw
       else
          if ( this(icdan)%Pt .eq. 0.D0 ) then
             if ( this(icdan)%status .eq. i_slifw )then
                this(icdan)%rlt = -murnik
                this(icdan)%statuscheck = i_slifw
             else if ( this(icdan)%status .eq. i_slibw )then
                this(icdan)%rlt = murnik
                this(icdan)%statuscheck = i_slibw
             else
                this(icdan)%rlt = this(icdan)%rlti
                this(icdan)%statuscheck = i_stick
             end if
          else
             this(icdan)%rlt = this(icdan)%rlti
             this(icdan)%statuscheck = i_stick
          end if
       end if
    end if
    
  end subroutine proj_cone
!------------------------------------------------------------------------
  subroutine prjj_(ik,vtik,vnik,storage)
    implicit none
    integer(kind=4),intent(in) :: ik,storage
    real(kind=8),intent(out)   :: vtik,vnik   
    character(len=10)          :: IAM
    character(len=80)          :: cout
    
    IAM = 'cpg::prjj'

    call prjj( this(ik)%CDAN, this(ik)%icdan, vtik, vnik, storage )

  end subroutine prjj_
!------------------------------------------------------------------------
  subroutine injj_(ik,rtik,rnik,storage)
    implicit none
    integer(kind=4),intent(in) :: ik,storage
    real(kind=8),intent(in)    :: rtik,rnik
    character(len=10)          :: IAM
    character(len=80)          :: cout

    IAM='cpg::injj'

    call injj( this(ik)%CDAN, this(ik)%icdan, rtik, rnik, storage )
    
  end subroutine injj_
!------------------------------------------------------------------------
  subroutine nullify_reac_(ik,storage)
    implicit none
    integer(kind=4),intent(in) :: ik,storage
    character(len=18)          :: IAM
    character(len=80)          :: cout
    
    IAM = 'cpg::nullify_reac'

    call nullify_reac( this(ik)%CDAN, this(ik)%icdan, storage )
    
  end subroutine nullify_reac_
!------------------------------------------------------------------------
  subroutine vitrad_(ik,storage)  
    implicit none
    integer(kind=4),intent(in) :: ik,storage    
    logical                    :: need_full_V
    character(len=12)          :: IAM
    character(len=80)          :: cout

    IAM = 'cpg::vitrad'

    !fd to say if all terms of a deformable body velocity are mandatory, default is no
    need_full_V = .false.
    if (storage .eq. iVaux_e_invM_t_Iaux_) need_full_V = .true.

    call vitrad( this(ik)%CDAN, this(ik)%icdan, storage, need_full_V )
    
  end subroutine vitrad_
!------------------------------------------------------------------------
  subroutine nullify_vlocy_(ik,storage)
    implicit none
    integer(kind=4),intent(in) :: ik,storage
    character(len=23)          :: IAM
    character(len=80)          :: cout

    IAM = 'cpg::nullify_vlocy'

    call nullify_vlocy( this(ik)%CDAN, this(ik)%icdan, storage )
  
  end subroutine nullify_vlocy_
!------------------------------------------------------------------------  
 subroutine Nullify_EntityList_cpg
   implicit none

   call Free_EntityList

 end subroutine Nullify_EntityList_cpg
!--------------------------------------------------------------------------
  subroutine get_cpg_loop(compteur,err1,err2,err3,contact)
    implicit none
    integer(kind=4),intent(out) :: compteur,contact
    real(kind=8),intent(out)    :: err1,err2,err3
    
    compteur = cpg_loop
    contact  = nb_cdan
    err1  = MeanDVoR
    err2  = QuadDV
    err3  = QuadDVR

  end subroutine get_cpg_loop
!---------------------------------------------------------------------
  subroutine get_cpg_contact_status(noctc,slide,stick)
    implicit none
    integer(kind=4)             :: ik
    integer(kind=4),intent(out) :: noctc,slide,stick
    
    do ik = 1,nb_CDAN
       select case(this(ik)%status)
       case(i_noctc)
          noctc = noctc + 1
       case(i_stick)
          stick = stick + 1
       case(i_slifw,i_slibw)
          slide = slide + 1
       case default
          !nothing to do
       end select
    end do
    
  end subroutine get_cpg_contact_status
!---------------------------------------------------------------------
  subroutine ex_post_cpg
    implicit none
    integer(kind=4)   :: ik,ient,ibehav,NOKSbegin,NOKWbegin
    real(kind=8)      :: gapTT,vltiki,vlniki
    character(len=5)  :: sstatusik
    character(len=13) :: IAM
    character(len=80) :: cout
    
    IAM = 'cpg::post_cpg'

    ! Rnod = [H] Rloc
    do ik=1,nb_CDAN  
       call nullify_reac_(ik,iIreac)
    end do

    do ik=1,nb_CDAN
       if(DABS(this(ik)%rln) <1.D-18) this(ik)%rln = 0.0
       if(DABS(this(ik)%rlt) <1.D-18) this(ik)%rlt = 0.0
       call injj_(ik,this(ik)%rlt,this(ik)%rln,iIreac)
    end do

    NOKSbegin = 0
    NOKWbegin = 0

    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(vltiki,vlniki,gapTT )
    !$OMP DO SCHEDULE(RUNTIME)
    do ik=1,nb_CDAN
       
       if (this(ik)%forecast .eq. 0) then
          this(ik)%rlt   = 0.D0
          this(ik)%rln   = 0.D0
          this(ik)%status= i_noctc
       end if

       vltiki = -(this(ik)%Ut + this(ik)%covfreet)
       vlniki = -(this(ik)%Un + this(ik)%covfreen)

       if (DABS(vltiki) .lt. 1.D-18) vltiki=0.D0
       if (DABS(vlniki) .lt. 1.D-18) vlniki=0.D0
       
       gapTT = this(ik)%gapTTbegin+H*vlniki

       if (dabs(gapTT) .lt. 1.D-24) gapTT=0.D0
       
       call set_loc( this(ik)%CDAN, this(ik)%icdan, this(ik)%status, &
                     vltiki, vlniki, this(ik)%rlt, this(ik)%rln, gapTT )

    end do
    !$OMP END DO
    !$OMP END PARALLEL
    
    do ik=1,nb_CDAN
       if (associated(this(ik)%adjjl) ) then
          deallocate(this(ik)%adjjl)
          nullify(this(ik)%adjjl)
       end if
    end do
    
    if ( allocated(Wab) ) then
       deallocate(Wab)
    end if
   
  end subroutine ex_post_cpg

end module cpg 
