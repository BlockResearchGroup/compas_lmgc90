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

!TODO
! reprendre les structures de donnees sous forme de matrices, vecteurs, etc au contact  
! etre plus explicite sur le W du probleme dynamique, du probleme avec changement de variable ou avec algo diago
! uniformiser les statuts entre 2D/3D (e.g. slide vs slifw/slibw) !fd 2018-11-26 vire slifw/slibw en 3D
! utiliser les indexes de parameters
! a voir si il faut separer les cas SDL/ELG et diago/plein ou mettre des routines pour rendre les choses plus lisibles
! a voir l'utilisation de type d'interaction avec pointeurs de fonctions pour ecrire un GSNL sans tests

MODULE NLGS_3D

  USE overall
  USE LMGC90_MPI

  USE parameters

  USE tact_behaviour
  USE utilities
  USE ALGEBRA

  USE PRPRx, only: print_info_PRPRx
  USE SPPRx, only: print_info_SPPRx  
  USE CSASp, only: print_info_CSASp, &
                   get_bulk_stress_triaxiality_CSASp, &
                   get_bulk_temperature_csasp 

  USE PRASp, only: get_corresponding_polyr_radius_PRASx

  use inter_meca_handler_3D, only : get_nb_inters    , &
                                    set_loc          , &
                                    get_rloc         , &
                                    get_vlocBEGIN    , &
                                    get_internal     , &
                                    set_internal     , &
                                    inter2ENT        , &
                                    get_tact_lawnb   , &
                                    injj             , &
                                    prjj             , &
                                    vitrad           , &
                                    nullify_reac     , &
                                    nullify_vlocy    , &
                                    get_surf         , &
                                    get_eff          , &
                                    set_violation    , &
                                    get_external_pressure
  
  IMPLICIT NONE

  PRIVATE

  !> data stored for each contact ik
  TYPE T_ctct_element
     !> type of contact ik
     INTEGER                      :: CDAN
     !> serial type number in type CDAN of contact element ik
     INTEGER                      :: icdan

     !> components of the local contact impulse
     REAL(kind=8)                 :: rlt,  rln ,  rls
     ! components of  local contact impulse (at previous jacobi iteration)
     REAL(kind=8)                 :: rlt_jacobi,  rln_jacobi ,  rls_jacobi

     !> components of the relative velocity at the beginning of the time step
     REAL(kind=8)                 :: vltBEGIN, vlnBEGIN, vlsBEGIN
     !> components of the relative velocity     
     REAL(kind=8)                 :: vlt,  vln ,  vls
     

     !> components of the complementary relative velocity
     REAL(kind=8)                 :: corlt,corln ,corls

     !> status at the beginning of the time step
     integer(kind=4)              :: statusBEGIN
     !> contact status
     integer(kind=4)              :: status
     !> contact status while checking convergence
     integer(kind=4)              :: statuscheck
     
     !> components of genuine local dynamic matrix W
     REAL(kind=8)                 :: Wtt, Wtn, Wts
     REAL(kind=8)                 :: Wnt, Wnn, Wns
     REAL(kind=8)                 :: Wst, Wsn, Wss
     !> components of auxiliary local dynamic matric WW
     REAL(kind=8)                 :: WWtt,WWnn,WWss
     !> det of auxiliary local dynamic matrix
     REAL(kind=8)                 :: det
     !> for diagonal resolution
     REAL(kind=8)                 :: invWnn,invWtt,invWss
     !> genuine free velocity components
     REAL(kind=8)                 :: vfreet,   vfreen,   vfrees
     !> complementary free velocity components to add togenuine free velocity components to get auxiliaries
     REAL(kind=8)                 :: covfreet, covfreen, covfrees
     !> gap at the beginning of the time step                                                                 
     REAL(kind=8)                 :: gapTTbegin
     !> reference gap used for far contact laws
     REAL(kind=8)                 :: gapREF
     !> set to 1 'acton' (reactions have to be computed) or 0 'noact' (reactions are to be found null)
     !> according to some forecast criterion
     integer(kind=4)              :: forecast
     ! internal flags to manage forcast
     INTEGER                      :: inoctc,iskip
     !> rank of contact law                                     
     INTEGER                      :: lawnb
     !> kind of contact law
     INTEGER                      :: i_law
     !> friction coefficient of contact
     REAL(kind=8)                 :: fric
     !> for Newton resolution     
     REAL(kind=8)                 :: ron,rot
     ! mapping for sdl solver
     !> cd and an rank in entity list  
     INTEGER                      :: icdent,ianent
     !> number of adjacent contacts to ik
     INTEGER                      :: nbadj
     !> rank of an adjacent contacts (in the overall contact list)
     ! jl = this(ik)%adjjl(p) where p is the pth adjacent contact to ik
     INTEGER,DIMENSION(:),POINTER :: adjjl
     !> rank of contact ik in the list of adjacent contacts
     ! if jl is the pth adjacent of ik, then ik = this(jl)%adjjl(this(ik)%rev_adj(p)) 
     INTEGER,DIMENSION(:),POINTER :: rev_adj
     !> index of Wikjl's storage chunk in the Wab matrix 
     INTEGER(kind=4)              :: istart
     !> internal parameters
     REAL(kind=8),DIMENSION(max_internal_tact) :: internal
     ! taz - working values (taz(1): beta, taz(2): ep, taz(3): pext)
     real(kind=8),dimension(max_taz)                 :: taz

     !> in CRITICAL_VOIGT_CLB, DYNAMIC_VOIGT_CLB ... velocity of tangential micro pad    
     REAL(kind=8)                 :: vepadt
     !> in CRITICAL_VOIGT_CLB, DYNAMIC_VOIGT_CLB ... velocity of tangential micro pad
     REAL(kind=8)                 :: vepads                
     
  END TYPE T_ctct_element

  TYPE (T_ctct_element),DIMENSION(:),ALLOCATABLE  :: this  
  
  ! About Wab ----------------------------------------------------------------------
  ! Notations are related to the way the matrix is build in the code ;
  ! we loop on ik which is the second index of the matrix jl is the first. 
  ! Wab stores each Wjl,ik matrices in a vector form (diagonal terms are not stored here).
  ! this(jl)%istart contains the index of the chunk of Wjl,ik storage zone
  ! iadj is the rank of ik in the adjacent contacts list of jl
  ! each term is stored in the following way
  
  ! W(this(jl)%istart+9*iadj-8) = WSS W(this(jl)%istart+9*iadj-7) = WST W(this(jl)%istart+9*iadj-6) = WSN
  ! W(this(jl)%istart+9*iadj-5) = WTS W(this(jl)%istart+9*iadj-4) = WTT W(this(jl)%istart+9*iadj-3) = WTN 
  ! W(this(jl)%istart+9*iadj-2) = WNS W(this(jl)%istart+9*iadj-1) = WNT W(this(jl)%istart+9*iadj  ) = WNN  

  ! Wab is computed noting that if we take a dR vector null everywhere except on one index ik
  ! then dU = W dR will contain the ikth column of W.
  ! dU is computed using a local to global to local exchange.

  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Wab
  
  ! variable indicating if the Delassus matrix Wab is built or not and how
  CHARACTER(len=30)  :: Wstorage                       
  LOGICAL            :: SDLactif=.FALSE.

  !!!----------------------------------------------------------------------------------------------------
  
  INTEGER, PRIVATE                        :: nb_CDAN=0,nb_ENTITY
  
  ! to use jacobi solver instead of nlgs solver
  LOGICAL            :: JACOBI_SOLVER=.false.
 
  INTEGER, DIMENSION(:), ALLOCATABLE,PRIVATE      :: ialeat,ialeatr,randomlist
  REAL(kind=8), DIMENSION(:), ALLOCATABLE,PRIVATE :: Xvlton,WRRarray

  INTEGER :: NOKsta,Nactif,Nvnish,Nstick,Nslide,Nnoctc,Ntract,Ncompr,Nnknow,Nnoact
  
  !mj randomlist
  
  INTEGER      :: IAL1
  REAL(kind=8) :: RA
  
  REAL(kind=8) :: somme_rn
  REAL(kind=8) :: HH
  
  !!!  For a single candidate for contact
  !!!
  !!!  DV        : increment of velocity during a gs iteration  
  !!!  R         : mean impulse reaction value during a gs iteration
  !!!  DVDV      : DV euclidian norm power 2
  !!!  DVDVRR    : DV euclidian norm power 2 multiplied by R euclidian norm power 2 (dimension: an energy power 2)  
  !!!  DVoR      : scalar product between DV R (dimension: an energy) 
  !!!  WRR       : mean scalar product between WR and R during a gs iteration (dimension: an energy)
  !!!  (WR is the relative velocity caused by the only impulse R) 
  !!!

   REAL(kind=8) :: DVDV,DVDVRR,DVoR,WRR
  
  !!!  Summation on the collection of candidates for contact 
  !!!
  !!!  SumDVDV   : summation of DVDV
  !!!  maxDVDV   : maximal DVDV value
  !!!  SumDVDVRR : summation of DVDVDRR
  !!!  maxDVDVRR : maximal DVDVRR value
  !!!  SumDVoR   : summation of DVoR
  !!!  SumWRR    : summation of WRR
  !!!  SumWRWR   : summation of WRWR

   REAL(kind=8) :: SumDVDV,MaxDVDV,SumDVDVRR,MaxDVDVRR,SumDVoR,SumWRWR,SumWRR,dynstat

  !!!  QuadWR    : quadratic mean of WR
  !!!  QuadDV    : quadratic mean of DV
  !!!  MaxmDV    : maximal DV value
  !!!  MeanWRR   : mean energy
  !!!  QuadDVR   : quadratic mean of DVR
  !!!  MaxmDVR   : maximal DVR value
  !!!  MeanDVoR  : mean value of DVR (algebraic value, a measure of the average penetration in the direction of R)

  REAL(kind=8) :: QuadWR=0.D0,QuadDV=0.D0,MaxmDV=0.D0,MeanWRR=0.D0,QuadDVR=0.D0, &
                  MaxmDVR=0.D0,MeanDVoR=0.D0

  !!!  Dreac     : "reacteristic distance" (distance run on the effect of the only impulse R)
  REAL(kind=8) :: Dreac


  REAL(kind=8) :: rcvltm,Dcrac,Scale=1.D0

  !!
  REAL(kind=8) :: tol=1.D-04,RELAX=1.D0,RELAX1=0.D0

  ! variable indicating the type of iter check test
  INTEGER           :: i_checktype=1                           
  ! variable indicating the type of iter check test
  CHARACTER(len=5)  :: checktype                               



  
  !!! parameter table -------------------------------------------------------------

  !!! nlgs check keyword

  INTEGER,PARAMETER :: i_Quad = 1 , i_Maxm = 2 , i_QMs16 = 3 , i_QuadN = 4

  !!! nlgs keyword

  INTEGER,PARAMETER :: i_iter = 1 , i_check = 2 , i_post = 3

  !!! tact behav keyword

  !!! RGR CRITIC

  INTEGER                :: Nb_RGR
  REAL(kind=8),PARAMETER :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)

  INTEGER      :: nlgs_loop=0,itermin
  
  LOGICAL      :: diagonal_resolution=.FALSE.,itchatche=.FALSE.

  !fd asserts that the solver was not run => needs initializing internals, etc
  !fd-todo virer les tests avec nstep==1
  LOGICAL      :: is_initialized=.FALSE.

  ! map between interaction serial number in dedicated modules (SPSPx, SPPLx, etc) and
  ! interaction serial number in this module.
  ! For example, for a given interaction index in SPSPx (icdan_SPSPx),
  ! the corresponding index in this module (icdan_nlgs) is :
  !    icdan_nlgs = icdan_SPSPx + shift_icdan(i_spspx)
  ! N.B.: if there is no interaction for a given interaction type, shift_icdan=-1.
  !       for example, if there's no SPPLx, then : shift(i_spplx) == -1
  !fd-todo cette gestion est totalement debile !! mettre 0 pour ne pas avoir de soucis.
  integer, dimension(nb_interaction_types) :: shift_icdan

  !vv: Nombre de W_alpha_beta avec alpha/=beta, non nuls
  INTEGER :: nb_adjac = 0

  !fd-wip gestion regul
  logical :: regul = .FALSE.
  real(kind=8) :: krn=0.d0,krt=0.d0  

  integer(kind=4),parameter :: i_acton=1,i_noact=0


  !fd 01/2021 gestion des defauts d'appariement
  logical :: manage_interp = .False.
  logical :: cut_open_czm = .False.
  real(kind=8) :: open_czm_tol = 1e-6
  
  public shift_icdan

  PUBLIC &
       solve_nlgs, &
       comp_check_nlgs, &
       write_norm_check_nlgs, &
       scramble_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       display_check_nlgs, &
       scale_rloc_nlgs, &
       RnodHRloc_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs, & 
       active_diagonal_resolution, &
!!$       init_cohe_nlgs_3D, &
       assume_is_initialized, &
       !am DDM : fonctions necessaires a la DDM
       compute_local_free_vlocy, &
       compute_convergence_norms_nlgs, &
       put_convergence_norms_nlgs, &
       check_convergence_nlgs, &
       get_error, &
       !vv:
       get_nb_adjac_nlgs_3D, &
       display_tacinfo, &
       !raf:
       use_jacobi_solver, &
       !fd-wip
       use_regul, &
       use_cut_open_czm, &
       use_manage_interpenetrated_czm

  PUBLIC  &
       get_nlgs3D_contact_status,get_nlgs3D_network_change,get_nlgs3D_loop

  private prjj_, injj_  , &
          vitrad_       , &
          nullify_reac_ , &
          nullify_vlocy_, &
          mu_NG_solver_ , &
          coupled_dof_solver_ , &
          get_external_pressure_

CONTAINS

!!!------------------------------------------------------------------------
  subroutine prep_nlgs_aux( id_inter, nb_inter, reac_mean, nb_CDAN )
    implicit none

    !> interaction id
    integer( kind = 4 ) :: id_inter
    !> nb of interactions of this kind
    integer( kind = 4 ) :: nb_inter
    !> cumulated normal reaction
    real( kind = 8 )    :: reac_mean
    !> cumulated nb of interaction
    integer( kind = 4 ) :: nb_CDAN

    !** Local variables
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: ik
    integer( kind = 4 ) :: icdent
    integer( kind = 4 ) :: ianent

    if ( nb_inter /= 0 ) shift_icdan( id_inter ) = nb_CDAN
    
    do icdan = 1, nb_inter
       ik               = nb_CDAN + icdan
       this( ik )%CDAN  = id_inter
       this( ik )%icdan = icdan

       !fd?? pourquoi on recupere %status alors qu'on recupere %statusbegin ?       
       call get_rloc( id_inter, icdan, this( ik )%rlt, this( ik )%rln, this( ik )%rls, this( ik )%status )
       reac_mean = reac_mean + this(ik)%rln
       
       call get_vlocBEGIN( id_inter, icdan, this( ik )%vltBEGIN, this( ik )%vlnBEGIN, this( ik )%vlsBEGIN, &
                           this( ik )%gapTTbegin, this( ik )%statusBEGIN )
       call get_internal( id_inter, icdan, this( ik )%internal )
       this( ik )%lawnb = get_tact_lawnb( id_inter, icdan )


       ! construction de la map des adjacents
       call inter2ENT( id_inter, icdan, icdent, ianent )

       this( ik )%icdent                          = icdent
       this( ik )%ianent                          = ianent

       if (icdent /= ianent) then
         entity(icdent)%ik                        = entity( icdent )%ik + 1
         entity(ianent)%ik                        = entity( ianent )%ik + 1
         entity(icdent)%list( entity(icdent)%ik ) = ik
         entity(ianent)%list( entity(ianent)%ik ) = ik
       else
         entity(icdent)%ik                        = entity(icdent)%ik + 1
         entity(icdent)%list( entity(icdent)%ik ) = ik
       end if

    end do
    nb_CDAN = nb_CDAN + nb_inter

  end subroutine prep_nlgs_aux

  !!!------------------------------------------------------------------------
  SUBROUTINE prep_nlgs(FLAG)
 
    IMPLICIT NONE
    
    ! common part

                              !123456789012345678
    CHARACTER(len=18)  :: IAM='nlgs_3D::prep_nlgs'
    CHARACTER(len=120) :: cout
    INTEGER            :: errare
    
    INTEGER            :: ik,ibehav,icdan,ient,itact
    REAL(kind=8)       :: vsik,vtik,vnik
    real(kind=8)       :: rsik,rtik,rnik
    real(kind=8)       :: det,det1,det2,det3 

    REAL(kind=8)       :: Dw,Sw,epsilon
    REAL(kind=8)       :: reac_mean = 0.D0

    ! contactors part 
    INTEGER            :: nb_SPSPx, nb_SPPLx, nb_SPPRx, nb_SPCDx
    INTEGER            :: nb_PRPLx, nb_PRPRx, nb_PTPT3
    INTEGER            :: nb_SPDCx
    INTEGER            :: nb_CSPRx,nb_CSASx,nb_PRASx
    INTEGER            :: nb_CDCDx,nb_CDPLx

    ! behaviours part
    REAL(kind=8)       :: fric,tangalrest,normalrest
    REAL(kind=8)       :: normalcoh,tangalcoh,Wethk,forcePERgap
    REAL(kind=8)       :: forcePERstrain,prestrain,forcePERstrainrate

    ! RGR CRITIC
    REAL(kind=8)       :: ToverH,OTH,OTH2,OvO2,vOVERcv,gap_tol,L0

    real(kind=8)       :: TshVlt,nbc

    ! Wab variables
    INTEGER            :: jl,iadj,jadj,ikadj,jladj
    INTEGER            :: nbadj,icdik,ianik,bandwidth,icdent,ianent
    integer(kind=4)    :: istart,jlstart,size_
    LOGICAL            :: is_present=.FALSE., ok=.FALSE.
    
    ! CZM 3D
    REAL(kind=8)       :: cd_surf,meff,reff,cohemin,cohemax
    LOGICAL            :: FLAG

    ! GREASE
    INTEGER      :: IAL1
    REAL(kind=8) :: RA,NormVt,kn,kt,etan,etat,cohes,cohet


    !
    !< nard_rod
    real(kind=8) :: Ksn,Kst,Kss,Kvn,Kvt,Kvs,Dtmax,Dsmax
    ! nard_rod />

    if ( JACOBI_SOLVER .and. RELAX >0.999d0 ) then
      CALL LOGMES('WARNING!! Usage of a jacobi solver with RELAX=1 may not converge.')
    endif

    
    nb_CDAN=0
    SDLactif = FLAG
    nlgs_solver3D = .TRUE.
    nlgs_loop   = 0

    nb_SPSPx = get_nb_inters(i_spspx)
    nb_CDAN  = nb_CDAN + nb_SPSPx

    nb_SPPLx = get_nb_inters(i_spplx)
    nb_CDAN  = nb_CDAN + nb_SPPLx

    nb_SPPRx = get_nb_inters(i_spprx)
    nb_CDAN  = nb_CDAN + nb_SPPRx

    nb_SPCDx = get_nb_inters(i_spcdx)
    nb_CDAN  = nb_CDAN + nb_SPCDx

    nb_SPDCx = get_nb_inters(i_spdcx)
    nb_CDAN  = nb_CDAN + nb_SPDCx

    nb_PRPLx = get_nb_inters(i_prplx)
    nb_CDAN  = nb_CDAN + nb_PRPLx

    nb_PRPRx = get_nb_inters(i_prprx)
    nb_CDAN  = nb_CDAN + nb_PRPRx
    
    nb_PTPT3 = get_nb_inters(i_ptpt3)
    nb_CDAN  = nb_CDAN + nb_PTPT3
    
    nb_CSPRx = get_nb_inters(i_csprx)
    nb_CDAN  = nb_CDAN + nb_CSPRx

    nb_CSASx = get_nb_inters(i_csasp)
    nb_CDAN  = nb_CDAN + nb_CSASx

    nb_PRASx = get_nb_inters(i_prasp)
    nb_CDAN  = nb_CDAN + nb_PRASx

    nb_CDCDx = get_nb_inters(i_cdcdx)
    nb_CDAN  = nb_CDAN + nb_CDCDx

    nb_CDPLx = get_nb_inters(i_cdplx)
    nb_CDAN  = nb_CDAN + nb_CDPLx

    IF (nb_CDAN == 0) RETURN 
    
    IF (ALLOCATED(ialeat)) DEALLOCATE(ialeat)
    ALLOCATE(ialeat(nb_CDAN),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating ialeat')
    END IF
    
    IF (ALLOCATED(ialeatr)) DEALLOCATE(ialeatr)
    ALLOCATE(ialeatr(nb_CDAN),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating ialeatr')
    END IF
    
    DO ik=1,nb_CDAN
       ialeat(ik)=ik
       ialeatr(ik)=ik
    END DO
    
    !mj randomlist

    IF (ALLOCATED(randomlist)) DEALLOCATE(randomlist)
    ALLOCATE(randomlist(nb_CDAN),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating randomlist')
    END IF
    
    DO ik=1,nb_CDAN
       CALL RANDOM_NUMBER(RA)
       IAL1=IDINT(RA*REAL(nb_CDAN,8))+1
       IAL1=MIN0(IAL1,nb_CDAN)
       IAL1=MAX0(1,IAL1)
       randomlist(ik)=IAL1
    END DO
    
    IF (ALLOCATED(Xvlton)) DEALLOCATE(Xvlton)
    ALLOCATE(Xvlton(nb_CDAN),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating Xvlton')
    END IF
    
    IF (ALLOCATED(WRRarray)) DEALLOCATE(WRRarray)
    ALLOCATE(WRRarray(nb_CDAN),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating WRRarray')
    END IF

    IF (ALLOCATED(this)) THEN
       DO ik=1,SIZE(this)
          IF(ASSOCIATED(this(ik)%adjjl)) DEALLOCATE(this(ik)%adjjl)
          IF(ASSOCIATED(this(ik)%rev_adj)) DEALLOCATE(this(ik)%rev_adj)
       END DO
       DEALLOCATE(this)
    END IF
    ALLOCATE(this(nb_CDAN),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating this')
    END IF

    !!! ------------------------------------

    nb_ENTITY = get_nb_ENTITY()
    
    call Create_EntityList

    !!!
    DO ik=1,nb_CDAN
       Xvlton(ik)          = 0.D0
       WRRarray(ik)        = 1.D0
       !
       this(ik)%rls        = 0.D0
       this(ik)%rlt        = 0.D0
       this(ik)%rln        = 0.D0
       this(ik)%vls        = 0.D0
       this(ik)%vlt        = 0.D0
       this(ik)%vln        = 0.D0
       this(ik)%corls      = 0.D0
       this(ik)%corlt      = 0.D0
       this(ik)%corln      = 0.D0
       this(ik)%status     = i_nknow
       this(ik)%vlsBEGIN   = 0.D0
       this(ik)%vltBEGIN   = 0.D0
       this(ik)%vlnBEGIN   = 0.D0
       this(ik)%gapTTbegin = 0.D0
       this(ik)%gapREF     = 0.D0
       this(ik)%statusBEGIN= i_nknow
       this(ik)%Wss        = 0.D0
       this(ik)%Wtt        = 0.D0
       this(ik)%Wtn        = 0.D0
       this(ik)%Wnt        = 0.D0
       this(ik)%Wnn        = 0.D0
       this(ik)%Wst        = 0.D0
       this(ik)%Wts        = 0.D0
       this(ik)%Wsn        = 0.D0
       this(ik)%Wns        = 0.D0
       !
       this(ik)%WWtt       = 0.D0
       this(ik)%WWnn       = 0.D0
       this(ik)%WWss       = 0.D0

       this(ik)%invWnn     = 0.D0
       this(ik)%invWtt     = 0.D0
       this(ik)%invWss     = 0.D0

       this(ik)%vfrees     = 0.D0
       this(ik)%vfreet     = 0.D0
       this(ik)%vfreen     = 0.D0
       this(ik)%covfrees   = 0.D0
       this(ik)%covfreet   = 0.D0
       this(ik)%covfreen   = 0.D0     
       this(ik)%lawnb      = 0
       this(ik)%statuscheck= i_nknow
       this(ik)%forecast   = i_acton  ! default status 'acton'
       this(ik)%fric       = 0.D0
       this(ik)%inoctc     = 0
       this(ik)%iskip      = 1

       this(ik)%icdent     = 0
       this(ik)%ianent     = 0

       this(ik)%istart     = 0
       this(ik)%nbadj      = 0
       NULLIFY(this(ik)%adjjl)
       NULLIFY(this(ik)%rev_adj)
       
       this(ik)%internal   = 0.D0
       this(ik)%taz        = 0.D0
       
       this(ik)%i_law      = 0

    END DO
   
    ! shifts are initialized to an impossible value
    shift_icdan = -1

    nb_CDAN=0

    call prep_nlgs_aux( i_spspx, nb_SPSPx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_spplx, nb_SPPLx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_spprx, nb_SPPRx, reac_mean, nb_CDAN )    
    call prep_nlgs_aux( i_spcdx, nb_SPCDx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_spdcx, nb_SPDCx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_prplx, nb_PRPLx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_prprx, nb_PRPRx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_ptpt3, nb_PTPT3, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_csprx, nb_CSPRx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_csasp, nb_CSASx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_prasp, nb_PRASx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_cdcdx, nb_CDCDx, reac_mean, nb_CDAN )
    call prep_nlgs_aux( i_cdplx, nb_CDPLx, reac_mean, nb_CDAN )

    IF(nb_CDAN /= 0) reac_mean = reac_mean/REAL(nb_CDAN,8)

    DO ient=1,nb_ENTITY
      IF (entity(ient)%ik /= entity(ient)%nb) THEN
        CALL LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
        WRITE(cout,'(A7,I0,A4,I0,A4,I0)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
        CALL FATERR(IAM,cout)
      END IF
    END DO
    
    !------------------
    ! Rnod = [H] Rloc
    !------------------
    
    DO ik=1,nb_CDAN  
       CALL nullify_reac_(ik,iIreac)
       CALL nullify_vlocy_(ik,iVaux_)
    END DO
    
    DO ik=1,nb_CDAN
       CALL injj_(ik,this(ik)%rls,this(ik)%rlt,this(ik)%rln,iIreac)
    END DO
    
    IF (SDLactif) THEN

       ! sizing the Wab matrix (Delassus matrix)
       
       bandwidth = 0
       istart   = 0
       !vv:
       nb_adjac = 0
       DO ik=1,nb_CDAN
          
          nbadj = 0
          icdik = this(ik)%icdent
          ianik = this(ik)%ianent
          
          ! computation of the number of adjacent contacts to ik 
          ! it is equal to the sum of active contacts with icdent and ianent minus the contact ik it self for each of them
          !
          ! new october 2018
          ! objects with all their dof imposed (clamped) are contributing to Wab with null matrix
          ! therefore they are skipped when counting adjacents contacts

          ! special case of auto contact ; means deformable objects which can't be clamped          
          IF (icdik == ianik) THEN
            nbadj = entity(icdik)%nb-1
          ELSE
            ! skip clamped objects contribution ...
            if (get_status_entity(icdik) == 0 ) nbadj = entity(icdik)%nb - 1
            if (get_status_entity(ianik) == 0 ) nbadj = nbadj + entity(ianik)%nb - 1            

            !old fashion: nbadj = entity(icdik)%nb+entity(ianik)%nb-2
            
          ENDIF

          jl = 0
          
          IF (nbadj /= 0) THEN
             IF( ASSOCIATED(this(ik)%adjjl) .and. nbadj /= size(this(ik)%adjjl) ) DEALLOCATE(this(ik)%adjjl)
             IF( .not. ASSOCIATED(this(ik)%adjjl) ) then
               ALLOCATE(this(ik)%adjjl(nbadj),stat=errare)
               IF (errare /= 0) THEN
                 CALL FATERR(IAM,'error allocating this(ik)%adjjl')
               END IF
             endif 
             this(ik)%adjjl = 0
             
             IF( ASSOCIATED(this(ik)%rev_adj) .and. nbadj /= size(this(ik)%rev_adj) ) DEALLOCATE(this(ik)%rev_adj)
             IF( .not. ASSOCIATED(this(ik)%rev_adj) ) then
               ALLOCATE(this(ik)%rev_adj(nbadj),stat=errare)
               IF (errare /= 0) THEN
                  CALL FATERR(IAM,'error allocating this(ik)%rev_adj')
               END IF
             endif  
             this(ik)%rev_adj = 0

             if (get_status_entity(icdik) == 0) then
               DO iadj=1,entity(icdik)%nb
                  IF (entity(icdik)%list(iadj) == ik) CYCLE
                  
                  jl = jl+1
                  this(ik)%adjjl(jl) = entity(icdik)%list(iadj)

                  !vv:
                  nb_adjac = nb_adjac + 1
                  
               END DO
             endif

             if (get_status_entity(ianik) == 0 ) then             
               DO iadj=1,entity(ianik)%nb
                  IF(entity(ianik)%list(iadj) == ik) CYCLE

                  !!!fd evacuation des contacts partages (uniquement auto-contact)                  
                  is_present = .FALSE.
                  if (get_status_entity(icdik) == 0) then
                    DO jadj=1,entity(icdik)%nb-1
                       IF (this(ik)%adjjl(jadj) /= entity(ianik)%list(iadj)) CYCLE
                       is_present = .TRUE.
                       EXIT
                    END DO
                  endif
                  IF (is_present) CYCLE
                  
                  jl = jl+1
                  this(ik)%adjjl(jl) = entity(ianik)%list(iadj)

                  !vv:
                  nb_adjac = nb_adjac + 1

               END DO
             endif  
          END IF
          
          ! storing the index of Wik,jl chunk
          this(ik)%istart = istart

          ! preparing next one
          istart = istart + 9*jl

          ! storing number of adjacent contacts
          this(ik)%nbadj = jl

          ! storing cumulated length of wab
          bandwidth = bandwidth+jl

          ! in some situation it may happens that wab is huge and index are badly computed due to integer description
          if (this(ik)%istart < 0) then
            write(cout,*) 'ik ',ik,' jl ',jl
            call logmes(cout)
            write(cout,*) 'index ik-1 ',this(ik-1)%istart,' number of adjacent contacts of ik-1 ',this(ik-1)%nbadj
            call logmes(cout)
            write(cout,*) 'bit size used for index ',bit_size(istart),' max value of index ', huge(istart)
            call logmes(cout)             
            call faterr(IAM,'something is going wrong in compute Wikjl index in Wab storage')
          endif  
        
       END DO

       !!!fd paranoiac test 1
       if (bandwidth /= nb_adjac) then
         CALL FATERR(IAM,'bandwidth and nb_adjac should be the same')          
       endif       

       !!!fd paranoiac test 2
       do ient=1,nb_ENTITY
         if (entity(ient)%ik /= entity(ient)%nb) then
            call LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
            write(cout,'(A7,I5,A4,I5,A4,I5)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
            call FATERR(IAM,cout)
         end if
       end do

       ! !</fd debug
       ! print*,'bandwidth ',bandwidth
       ! do ik=1,nb_cdan
       !   DO ikadj=1,this(ik)%nbadj
             
       !     jl = this(ik)%adjjl(ikadj)
       !     jlstart = this(jl)%istart
          
       !     if (jlstart < 0) print*,'ik ',ik,' ikadj ',ikadj,' jl ',jl,' jlstart ',jlstart,' nb jl adj ',this(jl)%nbadj
       !   enddo
       ! enddo   
       ! !fd/>
       
       IF(ALLOCATED(Wab)) DEALLOCATE(Wab)
       size_ = 9*bandwidth
       ALLOCATE(Wab(size_),stat=errare)
       IF (errare /= 0) then
          write(cout,*) 'Wab size ', size_
          call logmes(cout)
          CALL FATERR(IAM,'error allocating Wab')
       END IF

       Wab = 0.D0
       
    END IF

    ! computing Wjl,ik matrix
    ! be carefull ik is the second index !!
    
    DO ik=1,nb_CDAN

       rtik=1.D0
       rsik=0.D0
       rnik=0.D0

       CALL nullify_reac_(ik,iIaux_)
       CALL injj_(ik,rsik,rtik,rnik,iIaux_)
       CALL vitrad_(ik,iVaux_e_invM_t_Iaux_)
       CALL prjj_(ik,vsik,vtik,vnik,iVaux_)
       this(ik)%Wtt=vtik
       this(ik)%Wnt=vnik
       this(ik)%Wst=vsik
       
       IF(SDLactif)THEN

          ! looking for jl's concerned 
          DO ikadj=1,this(ik)%nbadj
             
             jl = this(ik)%adjjl(ikadj)
             jlstart = this(jl)%istart

             CALL prjj_(jl,vsik,vtik,vnik,iVaux_)
             
             ok = .FALSE.

             ! searching for jl,ik place and adding to Wjl,ik
             DO jladj=1,this(jl)%nbadj
                
                IF (this(jl)%adjjl(jladj) == ik) THEN

                   Wab(jlstart + 9*jladj-7) = vsik ! Wst
                   Wab(jlstart + 9*jladj-4) = vtik ! Wtt
                   Wab(jlstart + 9*jladj-1) = vnik ! Wnt
                   
                   ok = .TRUE.
                   ! on garde l'info
                   this(ik)%rev_adj(ikadj)=jladj
                   exit
                END IF
                
             END DO
             
             IF (.NOT. ok) THEN
                CALL FATERR(IAM,'ERROR: unable to find ik in the adjacent contacts of jl!!')
             END IF
             
          END DO
          
          CALL nullify_vlocy_(ik,iVaux_)
          
       END IF
       
       rtik=0.D0
       rsik=1.D0
       rnik=0.D0
       
       CALL nullify_reac_(ik,iIaux_)
       CALL injj_(ik,rsik,rtik,rnik,iIaux_)
       CALL vitrad_(ik,iVaux_e_invM_t_Iaux_)
       CALL prjj_(ik,vsik,vtik,vnik,iVaux_)
       this(ik)%Wts=vtik
       this(ik)%Wns=vnik
       this(ik)%Wss=vsik
       
       IF(SDLactif)THEN
          
          DO ikadj=1,this(ik)%nbadj
             
             jl = this(ik)%adjjl(ikadj)
             jlstart = this(jl)%istart
             
             CALL prjj_(jl,vsik,vtik,vnik,iVaux_)

             jladj = this(ik)%rev_adj(ikadj)

             Wab(jlstart + 9*jladj-8) = vsik ! Wss
             Wab(jlstart + 9*jladj-5) = vtik ! Wts
             Wab(jlstart + 9*jladj-2) = vnik ! Wns
             
          END DO
          
          CALL nullify_vlocy_(ik,iVaux_)
          
       END IF
       
       rtik=0.D0
       rsik=0.D0
       rnik=1.D0
       
       CALL nullify_reac_(ik,iIaux_)
       CALL injj_(ik,rsik,rtik,rnik,iIaux_)
       CALL vitrad_(ik,iVaux_e_invM_t_Iaux_)
       CALL prjj_(ik,vsik,vtik,vnik,iVaux_)

       this(ik)%Wtn=vtik
       this(ik)%Wnn=vnik
       this(ik)%Wsn=vsik
       
       IF(SDLactif)THEN
          
          DO ikadj=1,this(ik)%nbadj
             
             jl = this(ik)%adjjl(ikadj)
             jlstart = this(jl)%istart
             
             CALL prjj_(jl,vsik,vtik,vnik,iVaux_)
             
             jladj = this(ik)%rev_adj(ikadj)
             Wab(jlstart + 9*jladj-6) = vsik ! Wsn
             Wab(jlstart + 9*jladj-3) = vtik ! Wtn
             Wab(jlstart + 9*jladj  ) = vnik ! Wnn
             
          END DO
          
          CALL nullify_vlocy_(ik,iVaux_)
         
       END IF

       ! Todo : essayer de virer les termes extra diagonaux trop petits
       ! IF(SDLactif)THEN
       !   norm_ikik = 0. ! a calculer avec this(is)%W...
       !   DO ikadj=1,this(ik)%nbadj 
       !     jl = this(ik)%adjjl(ikadj)
       !     jlstart = this(jl)%istart
       !     jladj = this(ik)%rev_adj(ikadj)           
       !     norm_ikjl = 0. ! a calculer avec Wab(jlstart + 9*(jladj-1) +1 : jlstart + 9*(jladj) )
       !     if (norm_ikjl < norm_ikik*tol) this(ik)%skipadjjl(ikadj)=1 
       !   ENDDO
       ! ENDIF
!!! --------------------------------------

       ibehav=this(ik)%lawnb

!!! --------------------------------------
!!! Warning and coping with critical cases
!!! --------------------------------------

       IF (this(ik)%Wnn .LE. 1.D-18) THEN

          print*,''
          call print_info(ik)
          print*,''
          print*,get_inter_law_name_from_id(tact_behav(ibehav)%ilaw)
          print*,''
          WRITE(cout,543) ik,this(ik)%Wnn
543       FORMAT(1x,'  Wnn(',I0,') =',D12.5,' < 1.D-18')
          CALL LOGMES('Error '//IAM//': '//cout)
          WRITE(cout,'(I0,A2,I0,A1,I0,A1,D12.5)') this(ik)%CDAN,': ',this(ik)%icdent,',',this(ik)%ianent,' ',this(ik)%gapTTbegin
          CALL LOGMES('Error '//IAM//': '//cout)

       END IF
       
       !- For Wtt ------------------------------------
       IF (this(ik)%Wtt .LE. 1.D-06*this(ik)%Wnn        .AND. &
           tact_behav(ibehav)%ilaw /= i_ELASTIC_ROD     .AND. &
           tact_behav(ibehav)%ilaw /= i_VOIGT_ROD  ) THEN

          print*,''
          call print_info(ik)
          print*,''
          print*,get_inter_law_name_from_id(tact_behav(ibehav)%ilaw)
          print*,''
          WRITE(cout,544) ik, this(ik)%Wtt, ik
544       FORMAT(1X,'   Wtt(',I0,') =',D12.5,' < 1.D-06 * Wnn(',I0,')')
          CALL LOGMES(cout)

          WRITE(cout,600) ik,this(ik)%Wnn          
600       FORMAT(1x,'  Wnn(',I0,') =',D12.5)
          CALL LOGMES(cout)

          this(ik)%Wtt=1.D-06*this(ik)%Wnn          
       END IF
       
       !- For Wss ------------------------------------
       IF (this(ik)%Wss .LE. 1.D-06*this(ik)%Wnn    .AND. &
           tact_behav(ibehav)%ilaw /= i_ELASTIC_ROD .AND. &
           tact_behav(ibehav)%ilaw /= i_VOIGT_ROD          ) THEN

          print*,''
          call print_info(ik)
          print*,''
          print*,get_inter_law_name_from_id(tact_behav(ibehav)%ilaw)
          print*,''
          WRITE(cout,545) ik, this(ik)%Wss, ik
545       FORMAT(1X,'   Wss(',I0,') =',D12.5,' < 1.D-06 * Wnn(',I0,')')
          CALL LOGMES(cout)

          WRITE(cout,600) ik,this(ik)%Wnn          
          CALL LOGMES(cout)

          this(ik)%Wss=1.D-06*this(ik)%Wnn
       END IF
       
      !!!-----------------------------------------------
      !!! Preparing auxiliaries for contact ik.
      !!!-----------------------------------------------
       
       this(ik)%fric = get_fric(ibehav,this(ik)%statusBEGIN)
       
       this(ik)%WWtt = this(ik)%Wtt
       this(ik)%WWnn = this(ik)%Wnn
       this(ik)%WWss = this(ik)%Wss

       !!! -- changement de variables pour les lois    

       SELECT CASE(tact_behav(ibehav)%ilaw)

       !!!----------------------------------------          
       CASE(i_IQS_CLB)
          this(ik)%i_law = i_IQS_CLB
          this(ik)%covfreen=MAX(0.D0,this(ik)%gapTTbegin/H)

          if (regul) then
             if (krn /= 0.d0) then
                this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H))
                this(ik)%covfreen= this(ik)%gapTTbegin/H
              endif                
             if (krt /= 0.d0) then
                this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*H))
                this(ik)%WWss    = this(ik)%Wss+(1.D0/(krt*H))
             endif   
          endif   



       !!!----------------------------------------
       CASE(i_IQS_CLB_g0)
          this(ik)%i_law = i_IQS_CLB_g0

          IF (.NOT. is_initialized) THEN
            !fd on stoque le max(0.,g0) - decollement 
            this(ik)%internal(1) = max(0.d0,this(ik)%gapTTbegin)       
          END IF
          !fd ça gere automatiquement les defauts d'appariement
          this(ik)%covfreen=MAX(0.D0,(this(ik)%gapTTbegin-this(ik)%internal(1))/H)
          
          if (regul) then
             if (krn /= 0.d0) then
                this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H))
                this(ik)%covfreen= (this(ik)%gapTTbegin-this(ik)%internal(1))/H
             endif 
             if (krt /= 0.d0) then
               this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*H))
               this(ik)%WWss    = this(ik)%Wss+(1.D0/(krt*H))
             endif   
          endif   



       !!!----------------------------------------
       CASE(i_IQS_PLAS_CLB)
          this(ik)%i_law = i_IQS_PLAS_CLB

          IF (.NOT. is_initialized) THEN
             call init_IQS_PLAS(ibehav,this(ik)%internal)
             !! pta 2023/02/27  this(ik)%internal(3) = 0.2*get_corresponding_polyr_radius_PRASx(this(ik)%icdan)
             this(ik)%internal(3) = 0.003 ! pta 2023/02/27

             ! pta 2023/04/03 -> on essaie de se rapprocher du cas iqs g0 en forcant g0 = gapTTbegin a l'initialisation 
             !this(ik)%internal(1) = this(ik)%gapTTbegin !pta 2023/04/03 

          END IF
          
          this(ik)%covfreen=MAX(0.D0,(this(ik)%gapTTbegin-this(ik)%internal(1))/H)
          !this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%internal(1))/H

          if (this(ik)%internal(1) < -this(ik)%internal(3)) then
             this(ik)%covfreen=0.d0
          end if

       !!!-------------------------------------
       case(i_IQS_BW_CLB)
         this(ik)%i_law = i_IQS_BW_CLB

         if ( this(ik)%internal(1) .eq. 0.D0 ) then
            this(ik)%internal(1) = H/0.2
         else

            TshVlt = get_TshVlt_BW(ibehav)

            if ( abs(this(ik)%vltBEGIN) .lt. TshVlt ) then
               !mr the motion is not relevant to consider an increment of internal(1)
            else  
               ! Test are made with a frequency of 5 Hertz thus 1 cycle is performed in 0.2 s.
               this(ik)%internal(1) = this(ik)%internal(1) + H/0.2
            end if
         end if

         nbc = this(ik)%internal(1)

         this(ik)%fric        = get_fric_BW(ibehav,nbc)
         this(ik)%internal(2) = get_threshold_BW(ibehav,nbc)
         this(ik)%internal(3) = get_alpha_BW(ibehav)
         this(ik)%internal(4) = this(ik)%fric

       !!!----------------------------------------
       CASE(i_GAP_SGR_CLB)
          this(ik)%i_law = i_GAP_SGR_CLB

          !fd todo gestion erreur appariement
          ! IF (.NOT. is_initialized) THEN
          !   if (manage_interp) then
          !     ! pour les gaps negatifs                
          !     this(ik)%internal(2) = min(0.d0,this(ik)%gapTTbegin)
          !   endif   
          ! END IF

          if (regul) then
             if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H))
             if (krt /= 0.d0) then
                this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*H))
                this(ik)%WWss    = this(ik)%Wss+(1.D0/(krt*H))
             endif   
          endif   

          this(ik)%covfreen=this(ik)%gapTTbegin/H
          !fd todo gestion erreur appariement
          ! this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%internal(2))/H          

          select case( this(ik)%CDAN )
          case( i_CSASp, i_CSPRx, i_PRASp )
             cd_surf = get_surf( this( ik )%CDAN, this( ik )%icdan )
          case default
             cd_surf=0.d0
          end select         
          !IF (.NOT. is_initialized) THEN
          this(ik)%internal(1)=cd_surf
          !ENDIF   

       !!!----------------------------------------
       !!! loi frottement sec avec pre gap defo/defo rigides/defo
       CASE(i_GAP_SGR_CLB_g0)
          
          this(ik)%i_law = i_GAP_SGR_CLB_g0

          IF (.NOT. is_initialized) THEN
            if (manage_interp) then
              ! pour tous les gaps                
              this(ik)%internal(2) = this(ik)%gapTTbegin
            else   
              ! que pour les gaps positifs (joints ouverts)
              this(ik)%internal(2) = max(0.d0,this(ik)%gapTTbegin)
            endif   
          END IF

          this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%internal(2))/H

       !!!----------------------------------------
       CASE(i_IQS_DS_CLB)
          this(ik)%i_law = i_IQS_DS_CLB

          this(ik)%covfreen=MAX(0.D0,this(ik)%gapTTbegin/H)         

       !!!----------------------------------------
       CASE(i_GAP_SGR_DS_CLB)
          this(ik)%i_law = i_GAP_SGR_DS_CLB
          
          if (regul) then
             if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H))
             if (krt /= 0.d0) then
                this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*H))
                this(ik)%WWss    = this(ik)%Wss+(1.D0/(krt*H))
             endif   
          endif   

          this(ik)%covfreen=this(ik)%gapTTbegin/H
          
       !!!----------------------------------------
       !!! This unilateral condition prescribes that the normal relative velocity should satisfy 
       !!! a complementary condition together with the normal reaction force, as soon as a contact
       !!! occurs. Since deciding the occurence of a contact is in these circumstances 
       !!! a matter of numerical approximation, it may be decided that a contact is forecasted
       !!! when the predicted gap at some time between 0.D0 and H (from the beginning of the time step)
       !!! becomes negative. 
       CASE(i_RST_CLB)
          this(ik)%i_law = i_RST_CLB
          
          IF (this(ik)%gapTTbegin .LE. 0.D0) THEN
             !!! It is relevant to use this(ik)%gapTTbegin as a forecast criterium. 
             ! this(ik)%forecast='acton' !(default is 'acton')
             CALL get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfrees=tangalrest*this(ik)%vlsBEGIN
             this(ik)%covfreet=tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen=normalrest*this(ik)%vlnBEGIN
          ELSE
             this(ik)%forecast=i_noact
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status=i_noctc
             normalrest = 0.D0
             tangalrest = 0.D0
             this(ik)%covfreen = 0.D0
          END IF

       !!!----------------------------------------
       !!! This unilateral condition prescribes that the normal relative velocity should satisfy 
       !!! a complementary condition together with the normal reaction force, as soon as a contact
       !!! occurs. Since deciding the occurence of a contact is in these circumstances 
       !!! a matter of numerical approximation, it may be decided that a contact is forecasted
       !!! when the predicted gap at some time between 0.D0 and H (from the beginning of the time step)
       !!! becomes negative. 
       CASE(i_VEL_SGR_CLB)
          this(ik)%i_law = i_VEL_SGR_CLB

          IF (this(ik)%gapTTbegin .LE. 0.D0) THEN 
             !!! It is relevant to use this(ik)%gapTTbegin as a forecast criterium. 
             ! this(ik)%forecast='acton' (default is 'acton')
          ELSE
             this(ik)%forecast=i_noact
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status=i_noctc
          END IF

       !!!----------------------------------------
       CASE(i_RST_DS_CLB)
          this(ik)%i_law = i_RST_DS_CLB
          
          IF (this(ik)%gapTTbegin .LE. 0.D0) THEN
             !this(ik)%forecast='acton' (default is 'acton')
             CALL get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfrees=tangalrest*this(ik)%vlsBEGIN
             this(ik)%covfreet=tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen=normalrest*this(ik)%vlnBEGIN
          ELSE
             this(ik)%forecast=i_noact
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status=i_noctc
          END IF

      !!!-------------------------------------
      CASE(i_RST_WET_CLB)
         this(ik)%i_law = i_RST_WET_CLB
         CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)

         IF (this(ik)%gapTTbegin .LE. 0.D0) THEN
            IF (this(ik)%statusBEGIN == i_nknow)THEN
               this(ik)%statusBEGIN=i_Wnnow
            ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
               this(ik)%statusBEGIN=i_Wnctc
            ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
               this(ik)%statusBEGIN=i_Wstck
            ELSE IF (this(ik)%statusBEGIN == i_slide)THEN
               this(ik)%statusBEGIN=i_Wslid
            END IF
            CALL get_rst(ibehav,tangalrest,normalrest)
            this(ik)%covfrees = tangalrest*this(ik)%vlsBEGIN
            this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN

            if (.not. diagonal_resolution) then
              this(ik)%covfrees = this(ik)%covfrees - normalcoh*this(ik)%Wsn*H
              this(ik)%covfreet = this(ik)%covfreet - normalcoh*this(ik)%Wtn*H
            endif
            this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN-normalcoh*this(ik)%Wnn*H
            this(ik)%corln    = -normalcoh*H

         ELSE IF(this(ik)%gapTTbegin .LE. Wethk) THEN
            IF (this(ik)%statusBEGIN == i_nknow)THEN
               this(ik)%statusBEGIN=i_Wnnow
            ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
               this(ik)%statusBEGIN=i_Wnctc
            ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
               this(ik)%statusBEGIN=i_Wstck
            ELSE IF (this(ik)%statusBEGIN == i_slide)THEN
               this(ik)%statusBEGIN=i_Wslid
            END IF

            this(ik)%covfrees = tangalrest*this(ik)%vlsBEGIN
            this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN

            this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN
            this(ik)%corln    = -normalcoh*H
         ELSE 
            this(ik)%forecast= i_noact
            this(ik)%rls     = 0.D0
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%status  = i_noctc
         END IF

       !!!----------------------------------------
       CASE(i_GAP_WET_DS_CLB)
          this(ik)%i_law = i_GAP_WET_DS_CLB
          
          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
          ! here tangalcoh is inactive
          IF (this(ik)%gapTTbegin .LE. Wethk) THEN
             IF (this(ik)%statusBEGIN == i_nknow) THEN
                this(ik)%statusBEGIN=i_Wnnow
             ELSE IF (this(ik)%statusBEGIN == i_noctc) THEN
                this(ik)%statusBEGIN=i_Wnctc
             ELSE IF (this(ik)%statusBEGIN == i_stick) THEN
                this(ik)%statusBEGIN=i_Wstck
             ELSE IF (this(ik)%statusBEGIN == i_slide) THEN
                this(ik)%statusBEGIN=i_Wslid
             END IF
             if (.not. diagonal_resolution) then
               this(ik)%covfrees = -normalcoh*this(ik)%Wsn*H
               this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
             endif
             this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + this(ik)%gapTTbegin/H
             this(ik)%corln    = -normalcoh*H
          ELSE 
             this(ik)%covfreen = this(ik)%gapTTbegin/H
          END IF

       !!!----------------------------------------
       CASE(i_IQS_WET_DS_CLB)
          this(ik)%i_law = i_IQS_WET_DS_CLB

          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
          ! here tangalcoh is inactive
          IF (this(ik)%gapTTbegin .LE. Wethk) THEN
             IF (this(ik)%statusBEGIN == i_nknow) THEN 
                this(ik)%statusBEGIN=i_Wnnow
             ELSE IF (this(ik)%statusBEGIN == i_noctc) THEN 
                this(ik)%statusBEGIN=i_Wnctc
             ELSE IF (this(ik)%statusBEGIN == i_stick) THEN
                this(ik)%statusBEGIN=i_Wstck
             ELSE IF (this(ik)%statusBEGIN == i_slide) THEN
                this(ik)%statusBEGIN=i_Wslid
             END IF
             if (.not. diagonal_resolution) then               
               this(ik)%covfrees = -normalcoh*this(ik)%Wsn*H
               this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
             endif
             !fd 12/12/07 a voir 
             !this(ik)%covfreen = -tangalcoh*this(ik)%Wns*H - tangalcoh*this(ik)%Wnt*H - normalcoh*this(ik)%Wnn*H &
             this(ik)%covfreen = - normalcoh*this(ik)%Wnn*H &
                  + MAX(0.D0,this(ik)%gapTTbegin/H)
             this(ik)%corln    = -normalcoh*H
          ELSE
             this(ik)%corln    = 0.D0
             this(ik)%covfreen = MAX(0.D0,this(ik)%gapTTbegin/H)
          END IF

       !!!----------------------------------------
       CASE(i_xQS_WET_DS_CLB)
          this(ik)%i_law = i_xQS_WET_DS_CLB

          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
          ! eXtended IQS_WET_DS_CLB law 
          ! once cohesion is broken it cant be recovered
          ! here tangalcoh is inactive
          if (.not. is_initialized) THEN          
            IF (this(ik)%gapTTbegin .LE. Wethk) THEN
              IF (this(ik)%statusBEGIN == i_nknow) THEN 
                 this(ik)%statusBEGIN=i_Wnnow
              ELSE IF (this(ik)%statusBEGIN == i_noctc) THEN 
                 this(ik)%statusBEGIN=i_Wnctc
              ELSE IF (this(ik)%statusBEGIN == i_stick) THEN
                 this(ik)%statusBEGIN=i_Wstck
              ELSE IF (this(ik)%statusBEGIN == i_slide) THEN
                 this(ik)%statusBEGIN=i_Wslid
              END IF
              if (.not. diagonal_resolution) then               
                this(ik)%covfrees = -normalcoh*this(ik)%Wsn*H
                this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
              endif

              this(ik)%covfreen = - normalcoh*this(ik)%Wnn*H &
                                  + MAX(0.D0,this(ik)%gapTTbegin/H)
              this(ik)%corln    = -normalcoh*H
            ELSE
              this(ik)%corln    = 0.D0
              this(ik)%covfreen = MAX(0.D0,this(ik)%gapTTbegin/H)
            END IF
          else  
            !if (this(ik)%statusBEGIN(1:1) == 'W'.and. this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslid .and. this(ik)%gapTTbegin .le. Wethk) then

              if (.not. diagonal_resolution) then 
                this(ik)%covfrees = -normalcoh*this(ik)%Wsn*H
                this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
              endif               
              this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
              this(ik)%corln    = -normalcoh*H
               
             else 
            
               if (this(ik)%statusBEGIN == i_Wnnow)then
                 this(ik)%statusBEGIN=i_nknow
               else if (this(ik)%statusBEGIN == i_Wnctc)then
                  this(ik)%statusBEGIN=i_noctc
               else if (this(ik)%statusBEGIN == i_Wstck)then
                  this(ik)%statusBEGIN=i_stick
               else if (this(ik)%statusBEGIN == i_Wslid)then
                  this(ik)%statusBEGIN=i_slide
               end if
               
               this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)
             end if
          endif   
      !!!-------------------------------------
      CASE(i_IQS_MOHR_DS_CLB)
         this(ik)%i_law = i_IQS_MOHR_DS_CLB
         
         IF (Nstep == 1 .AND. this(ik)%statusBEGIN == i_nknow) THEN
            this(ik)%statusBEGIN=i_Mstck
            this(ik)%fric=get_fric(ibehav,this(ik)%statusBEGIN)
         END IF

         IF (this(ik)%statusBEGIN == i_Mstck) THEN

            select case( this(ik)%CDAN )
            case( i_PRPLx, i_PRPRx, i_SPSPx, i_SPPLx, i_SPPRx, i_SPCDx, i_SPDCx, i_CDPLx, i_CDCDx, i_PTPT3 )
               
               cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )

            case default

              call faterr(IAM,'this contact element doesn t work with'//tact_behav(ibehav)%lawty)

            end select

           if (regul .and. cd_surf > 1d-14) then
             ! une modif pour reduire l indetermination
             if (krn /= 0.d0) this(ik)%WWnn = this(ik)%Wnn+(1.D0/(krn*cd_surf*H*H))
             ! c'est une viscosite 
             if (krt /= 0.d0) then
               this(ik)%WWtt    = this(ik)%Wtt +(1.D0/(krt*cd_surf*H))
               this(ik)%WWss    = this(ik)%Wss +(1.D0/(krt*cd_surf*H))
             endif  
           endif 
           ! here Wethk is inactive
           CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)

           ! si diagonal resolution W est diagonal donc pas de couplage
           if (.not. diagonal_resolution) then
             this(ik)%covfreet=-normalcoh*cd_surf*this(ik)%Wtn*H
             this(ik)%covfrees=-normalcoh*cd_surf*this(ik)%Wsn*H
           endif
 
           this(ik)%covfreen=MAX(0.D0,this(ik)%gapTTbegin/H) - (normalcoh*cd_surf*this(ik)%WWnn*H)
           this(ik)%corln   =-normalcoh*cd_surf*H

           !write(*,'(I0,3(1x,D12.5))') this(ik)%icdan, this(ik)%gapTTbegin, this(ik)%covfreen, this(ik)%corln

           !write(*,'(I0,4(1x,D12.5))') this(ik)%icdan, this(ik)%gapTTbegin/H, normalcoh, cd_surf,this(ik)%Wnn 
           if (regul .and. cd_surf > 1d-14) then
             if (krn /= 0.d0) this(ik)%covfreen = (-cd_surf*normalcoh*this(ik)%Wnn*H) + &
                                                  (this(ik)%gapTTbegin - (normalcoh/krn))/H
           endif

         ELSE
            this(ik)%covfreen = MAX(0.D0,this(ik)%gapTTbegin/H)
         END IF

         !print*,ik,this(ik)%statusBEGIN, this(ik)%covfreen, this(ik)%corln
         
       !!!-------------------------------------
       CASE(i_ELASTIC_REPELL_CLB)
          this(ik)%i_law = i_ELASTIC_REPELL_CLB
          
          CALL get_forcePERgap(ibehav,forcePERgap)

          this(ik)%WWnn     = this(ik)%Wnn+(1.D0/(forcePERgap*H*H))

          this(ik)%covfreen= this(ik)%gapTTbegin/H

       !!!-------------------------------------
       CASE(i_ELASTIC_REPELL_CLB_g0)
          this(ik)%i_law = i_ELASTIC_REPELL_CLB_g0
          
          IF (.NOT. is_initialized) THEN
            !fd on stoque le max(0.,g0) mais c'est possible que ca ne fasse pas ce qu'on souhaite
            !fd si on ne fait pas ca c'est comme si on introduisait un jeu fictif

            !this(ik)%internal(1) = max(0.d0,this(ik)%gapTTbegin)
            this(ik)%internal(1) = this(ik)%gapTTbegin                    

          END IF

          CALL get_forcePERgap(ibehav,forcePERgap)

          this(ik)%WWnn     = this(ik)%Wnn+(1.D0/(forcePERgap*H*H))

          this(ik)%covfreen= (this(ik)%gapTTbegin-this(ik)%internal(1))/H

       !!!------------------------------------- PTA sncf 2023
       CASE(i_ELASTIC_REPELL_CLB_adapt)
          this(ik)%i_law = i_ELASTIC_REPELL_CLB_adapt

          IF (.NOT. is_initialized) THEN
            !pta on stoque le F/gp des param de la loi initialement
            call init_ELAS_REP_adapt(ibehav,this(ik)%internal)

          END IF

          !print*,'internal = ',this(ik)%internal(1)
          forcePERgap = this(ik)%internal(1)     
          if (forcePERgap /= 0.d0) then
             
          else
             CALL get_forcePERgap(ibehav,forcePERgap)
          end if
          !print*,'forcePERgap = ',forcePERgap

          if (forcePERgap /= 0.d0) then
             this(ik)%WWnn     = this(ik)%Wnn+(1.D0/(forcePERgap*H*H))
          end if

          this(ik)%covfreen= this(ik)%gapTTbegin/H

       !!!----------------------------------------
       CASE(i_VISCO_ELASTIC_REPELL_CLB)
          this(ik)%i_law = i_VISCO_ELASTIC_REPELL_CLB


          if (this(ik)%gapTTbegin .le. 0.D0) then
            ! this(ik)%forecast='acton' (default is 'acton')
            CALL get_forcePERgap(ibehav,forcePERgap)
            CALL get_viscosity(ibehav,etan,etat)

            this(ik)%WWnn     = this(ik)%Wnn + (1.D0 / ( (forcePERgap*H*H) + (etan*H) ))
            this(ik)%covfreen = forcePERgap/(forcePERgap*H + etan) * this(ik)%gapTTbegin
         else
            this(ik)%forecast= i_noact !'noact'
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%rls     = 0.D0            
            this(ik)%status  = i_noctc
         end if  
 

       !!!-------------------------------------
       CASE(i_ELASTIC_REPELL_WET_CLB)
         this(ik)%i_law = i_ELASTIC_REPELL_WET_CLB

         ! This law is a non smooth approximation of the so called Lennard-Jones law, which
         ! is used to approximate superficial tension effects when a thin local fluid layer 
         ! is acting between contactors. Unilateral conditions are described by the elastic 
         ! repell law. Attraction is active while the gap is less than Wethk.
         ! Thus the energy of rupture is normalcoh*Wethk.
          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
          ! here tangalcoh is inactive
          CALL get_forcePERgap(ibehav,forcePERgap)

          this(ik)%WWnn     = this(ik)%Wnn+(1.D0/(forcePERgap*H*H))

          this(ik)%covfreen= this(ik)%gapTTbegin/H

          IF (this(ik)%gapTTbegin .LE. Wethk) THEN
            IF (this(ik)%statusBEGIN == i_nknow)THEN
               this(ik)%statusBEGIN=i_Wnnow
            ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
               this(ik)%statusBEGIN=i_Wnctc
            ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
               this(ik)%statusBEGIN=i_Wstck
            ELSE IF (this(ik)%statusBEGIN == i_slide)THEN
               this(ik)%statusBEGIN=i_Wslid
            END IF
            if (.not. diagonal_resolution) then
              this(ik)%covfrees= -normalcoh*this(ik)%Wsn*H
              this(ik)%covfreet= -normalcoh*this(ik)%Wtn*H
            endif

            this(ik)%covfreen= this(ik)%covfreen-normalcoh*this(ik)%Wnn*H

            this(ik)%corln   =-normalcoh*H
         END IF

       !!!-------------------------------------
       CASE(i_VISCO_ELASTIC_REPELL_WET)
          this(ik)%i_law = i_VISCO_ELASTIC_REPELL_WET
          
         ! This law is a non smooth approximation of the so called Lennard-Jones law, which
         ! is used to approximate superficial tension effects when a thin local fluid layer 
         ! is acting between contactors. Unilateral conditions are described by the elastic 
         ! repell law. Attraction is active while the gap is less than Wethk.
         ! Thus the energy of rupture is normalcoh*Wethk.
          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
          CALL get_viscosity(ibehav,etan,etat)
          CALL get_forcePERgap(ibehav,forcePERgap)

          this(ik)%WWnn     = this(ik)%Wnn+(1.D0/(H*(forcePERgap*H + etan)))

          this(ik)%covfreen = this(ik)%gapTTbegin*(1.0 + etan/(forcePERgap*H + etan))/H

          this(ik)%internal = 0.D0

          IF (this(ik)%gapTTbegin .LE. Wethk) THEN
            IF (this(ik)%statusBEGIN == i_nknow)THEN
               this(ik)%statusBEGIN=i_Wnnow
            ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
               this(ik)%statusBEGIN=i_Wnctc
            ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
               this(ik)%statusBEGIN=i_Wstck
            ELSE IF (this(ik)%statusBEGIN == i_slide)THEN
               this(ik)%statusBEGIN=i_Wslid
            END IF

            select case( this(ik)%CDAN )
            case( i_SPSPx, i_SPPLx, i_SPCDx, i_SPDCx )

               call get_eff( this( ik )%CDAN, this( ik )%icdan, meff, reff )

            case default

               call faterr(IAM,'contact type not available for VISCO_ELASTIC_REPELL_WET law')

            end select

            CALL get_viscosity(ibehav,etan,etat)

            normalcoh = normalcoh + ABS(this(ik)%vlnBEGIN)*etan
            if (.not. diagonal_resolution) then
              this(ik)%covfrees= -normalcoh*this(ik)%Wsn*H
              this(ik)%covfreet= -normalcoh*this(ik)%Wtn*H
            endif
            this(ik)%covfreen= this(ik)%covfreen-normalcoh*this(ik)%Wnn*H
            this(ik)%corln    =-normalcoh*H

         END IF

       !!!-------------------------------------
       CASE(i_ELASTIC_ROD)
          this(ik)%i_law = i_ELASTIC_ROD

          IF (.NOT. is_initialized .and. this(ik)%internal(1) == 0.d0) THEN

            !fd on stoque le g0
            this(ik)%internal(1) = this(ik)%gapTTbegin       

          END IF

          this(ik)%gapREF=this(ik)%internal(1)
          
          IF (this(ik)%gapREF .LE. 1.D-18) THEN
             WRITE(cout,555) ik,this(ik)%gapREF
             CALL FATERR(IAM,cout)
          END IF
          
          CALL get_forcePERstrain(ibehav,forcePERstrain)
          CALL get_prestrain(ibehav,prestrain)

          this(ik)%WWnn    = this(ik)%Wnn+(this(ik)%gapREF/(forcePERstrain*H*H))

          this(ik)%covfreen=(this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H

       !!!-------------------------------------
       CASE(i_ELASTIC_WIRE)
          this(ik)%i_law = i_ELASTIC_WIRE
          this(ik)%gapREF=this(ik)%internal(1)
          
          IF (this(ik)%gapREF .LE. 1.D-18) THEN
             WRITE(cout,555) ik,this(ik)%gapREF
             CALL FATERR(IAM,cout)
          END IF
          
          CALL get_forcePERstrain(ibehav,forcePERstrain)
          CALL get_prestrain(ibehav,prestrain)

          this(ik)%WWnn     = this(ik)%Wnn+(this(ik)%gapREF/(forcePERstrain*H*H))

          this(ik)%covfreen = (this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H

       !!!-------------------------------------
       CASE(i_BRITTLE_ELASTIC_WIRE)
          this(ik)%i_law = i_BRITTLE_ELASTIC_WIRE

          IF (this(ik)%statusBEGIN /= i_vnish ) THEN
            this(ik)%gapREF=this(ik)%internal(1)
          
            IF (this(ik)%gapREF .LE. 1.D-18) THEN
               WRITE(cout,555) ik,this(ik)%gapREF
               CALL FATERR(IAM,cout)
            END IF
          
            CALL get_forcePERstrain(ibehav,forcePERstrain)
            CALL get_prestrain(ibehav,prestrain)

            this(ik)%WWnn     = this(ik)%Wnn+(this(ik)%gapREF/(forcePERstrain*H*H))

            this(ik)%covfreen = (this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H

            !print*,'(gapTT - gapref)/H',this(ik)%covfreen 

          ELSE 
             this(ik)%forecast= i_noact
             this(ik)%rls     = 0.D0
             this(ik)%rlt     = 0.D0
             this(ik)%rln     = 0.D0
             this(ik)%status  = i_noctc
          END IF

      !!!---------------------------------------
      CASE(i_RIGID_WIRE)
         this(ik)%i_law = i_RIGID_WIRE

         this(ik)%gapREF=this(ik)%internal(1)
         this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%gapREF)/H    

      !!!-------------------------------------
      CASE(i_COUPLED_DOF)
         this(ik)%i_law = i_COUPLED_DOF
         
         if (regul) then
           if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H))
           if (krt /= 0.d0) then
             this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*H))
             this(ik)%WWss    = this(ik)%Wss+(1.D0/(krt*H))
           endif   
         endif   

      !!!-------------------------------------
      CASE(i_NORMAL_COUPLED_DOF)
          this(ik)%i_law = i_NORMAL_COUPLED_DOF
 
          if (regul) then
             if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H))
          endif   

      !!!-------------------------------------
      CASE(i_WET_3C)
          this(ik)%i_law = i_WET_3C

      !!!-------------------------------------
      CASE(i_VOIGT_ROD)
          this(ik)%i_law = i_VOIGT_ROD
         
          this(ik)%gapREF=this(ik)%internal(1)
         
          IF (this(ik)%gapREF .LE. 1.D-18) THEN
             WRITE(cout,555) ik,this(ik)%gapREF
             CALL FATERR(IAM,cout)
          END IF
         
          CALL get_forcePERstrain(ibehav,forcePERstrain)
          CALL get_prestrain(ibehav,prestrain)
          CALL get_forcePERstrainrate(ibehav,forcePERstrainrate)

          this(ik)%WWnn     = this(ik)%Wnn+(this(ik)%gapREF/((forcePERstrain*H*H)+(forcePERstrainrate*H)))

          this(ik)%covfreen = (H*forcePERstrain/(H*forcePERstrain+forcePERstrainrate))* &
                              (this(ik)%gapTTbegin-(1.d0+prestrain)*this(ik)%gapREF)/H

      !!!-------------------------------------
      CASE(i_VOIGT_WIRE)
          this(ik)%i_law = i_VOIGT_WIRE
         
          this(ik)%gapREF=this(ik)%internal(1)
         
          IF (this(ik)%gapREF .LE. 1.D-18) THEN
             WRITE(cout,555) ik,this(ik)%gapREF
             CALL FATERR(IAM,cout)
          END IF
         
          CALL get_forcePERstrain(ibehav,forcePERstrain)
          CALL get_prestrain(ibehav,prestrain)
          CALL get_forcePERstrainrate(ibehav,forcePERstrainrate)

          this(ik)%WWnn=this(ik)%Wnn+(this(ik)%gapREF/((forcePERstrain*H*H)+(forcePERstrainrate*H)))

          this(ik)%covfreen = (H*forcePERstrain/(H*forcePERstrain+forcePERstrainrate))* &
                              (this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H

          
      !!!-------------------------------------
      CASE(i_MAC_CZM,i_MP_CZM,i_MP3_CZM,i_MAL_CZM,i_TH_CZM,i_ABP_CZM,i_EXPO_CZM,i_EXPO_CZM_P)
            
         this(ik)%i_law = tact_behav(ibehav)%ilaw

          IF (.NOT. is_initialized) THEN

             ! fd merdasse pour gerer les contacts trop decolles au debut
             ! si trop decolle on les casse
             
             if (cut_open_czm .and. this(ik)%gapTTbegin >= open_czm_tol) then
               CALL init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,.True.) 
               if (manage_interp ) then
                 ! on stocke les gaps negatifs
                 this(ik)%internal(7) = min(0.d0,this(ik)%gapTTbegin)
               endif    
             else          
               IF (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow
               CALL init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)
               if (manage_interp ) then
                 ! on stocke tous les gaps
                 this(ik)%internal(7) = this(ik)%gapTTbegin
               endif    
             endif
             
          else
             this(ik)%taz(1) = this(ik)%internal(5) 
          END IF
          
          select case( this( ik )%CDAN )
          case( i_CSASp, i_CSPRx, i_PRASp )
             cd_surf = get_surf( this( ik )%CDAN, this( ik )%icdan )

          case default
             call faterr(IAM,'this contact element does not work with'//tact_behav(ibehav)%lawty)

          end select

          if (regul) then
             if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*H*H*cd_surf))
             if (krt /= 0.d0) then
                this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*H))
                this(ik)%WWss    = this(ik)%Wss+(1.D0/(krt*H))
             endif   
          endif   
          
          CALL prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,this(ik)%gapTTbegin,0.d0)
          
          this(ik)%covfreet=0.d0
          this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(7) - get_dilatancy_height(ibehav,this(ik)%internal))/H
          this(ik)%covfrees=0.d0

      !!!-------------------------------------          
      case(i_EXPO_CZM_SPRING)

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement au tout debut du calcul
            this(ik)%internal(9) = this(ik)%gapTTbegin
            
         else

            ! beta 
            this(ik)%taz(1) = this(ik)%internal(5)
            
         end if


         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine
         !!!fd

         select case( this( ik )%CDAN )
         case( i_CSASp, i_CSPRx, i_PRASp )
            cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )
         case default       
            call faterr(IAM,'CZM not implemented for this contact element')
         end select

         !                                                      aucune importance pas stocke                                                                            
         call prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,this(ik)%gapTTbegin-this(ik)%internal(9),0.d0)

         !!!fd
         !!!fd C'est une loi en gap - gap0 on prepare donc le changement de variables
         !!!fd
         
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(9))/H

         !!!fd
         !!!fd c'est une loi en vitesse tangentielle (frottement)
         !!!fd

         this(ik)%covfreet=0.d0
         this(ik)%covfrees=0.d0         
      !!!-------------------------------------          
      case(i_EXPO_CZM_SPRING_P)

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement au tout debut du calcul
            this(ik)%internal(9) = this(ik)%gapTTbegin
            
         else

            ! beta 
            this(ik)%taz(1) = this(ik)%internal(5)
            
         end if


         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine
         !!!fd

         select case( this( ik )%CDAN )
         case( i_CSASp, i_CSPRx, i_PRASp )
            cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )
         case default       
            call faterr(IAM,'CZM_SPRING_P not implemented for this contact element')
         end select

         !                                                      aucune importance pas stocke                                                                            
         call prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,this(ik)%gapTTbegin-this(ik)%internal(9),0.d0)

         !!!fd
         !!!fd C'est une loi en gap - gap0 on prepare donc le changement de variables
         !!!fd
         
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(9))/H

         !!!fd
         !!!fd c'est une loi en vitesse tangentielle (frottement)
         !!!fd

         this(ik)%covfreet=0.d0
         this(ik)%covfrees=0.d0         
          
       !!!-------------------------------------
       CASE(i_IQS_MAC_CZM,i_IQS_MAL_CZM,i_IQS_TH_CZM,i_IQS_ABP_CZM,i_IQS_EXPO_CZM,i_IQS_EXPO_CZM_P)

          this(ik)%i_law = tact_behav(ibehav)%ilaw !i_IQS_MAC_CZM

          IF (.NOT. is_initialized) THEN

             ! fd merdasse pour gerer les contacts trop decolles au debut
             ! si trop decolle on les casse
             
             if (cut_open_czm .and. this(ik)%gapTTbegin >= open_czm_tol) then
               CALL init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,.True.) 
               if (manage_interp ) then
                 ! on stocke les gaps negatifs
                 this(ik)%internal(7) = min(0.d0,this(ik)%gapTTbegin)
               endif    
             else          
               IF (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow
               CALL init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)
               if (manage_interp ) then
                 ! on stocke tous les gaps
                 this(ik)%internal(7) = this(ik)%gapTTbegin

               endif    
             endif
             
          else

             this(ik)%taz(1) = this(ik)%internal(5) 

          END IF
          
          select case( this(ik)%CDAN )
          case( i_PRPLx, i_PRPRx, i_SPSPx, i_SPPLx, i_SPPRx, i_SPCDx, i_SPDCx, i_CDPLx, i_CDCDx, i_PTPT3 )
             
             cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )
             
          case default

             call faterr(IAM,'this contact element does not work with'//tact_behav(ibehav)%lawty)

          end select

          if (manage_interp ) then
             CALL prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,this(ik)%gapTTbegin,0.d0)
          else    
             CALL prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,MAX(0.d0,this(ik)%gapTTbegin),0.d0)          
          endif
          
          this(ik)%covfreet = 0.D0
          this(ik)%covfreen = MAX(0.D0,(this(ik)%gapTTbegin - this(ik)%internal(7) - get_dilatancy_height(ibehav,this(ik)%internal))/H)
          this(ik)%covfrees = 0.D0

          
      !!!-------------------------------------
      case(i_IQS_EXPO_CZM_SPRING )

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement initial
            !this(ik)%internal(9) = max(0.d0, this( ik )%gapTTbegin)
            this(ik)%internal(9) = this( ik )%gapTTbegin            

         else

            ! beta
            this(ik)%taz(1) = this(ik)%internal(5)
            
         end if

         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 
         cd_surf = 1.d0

         select case( this(ik)%CDAN )
         case( i_PRPLx, i_PRPRx, i_SPSPx, i_SPPLx, i_SPPRx, i_SPCDx, i_SPDCx, i_CDPLx, i_CDCDx, i_PTPT3 )

            cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )

         case default

            call faterr(IAM,'IQS_EXPO_CZM_SPRING not implemented for this contact element')
            
         end select

        
         !                                                               aucune importance pas stocke 
         ! call prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,max(0.d0,this(ik)%gapTTbegin-this(ik)%internal(9)),0.d0)
         call prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,this(ik)%gapTTbegin-this(ik)%internal(9),0.d0)         
         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd

         ! faudrait il verifier que c'est bien positif ? pas sur car c'est un ressort en compression
         
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(9) )/H       
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0
         this(ik)%covfrees=0.d0         
      !!!-------------------------------------
      case(i_IQS_EXPO_CZM_SPRING_P )

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement initial
            this(ik)%internal(9) = max(0.d0, this( ik )%gapTTbegin)

         else

            ! beta
            this(ik)%taz(1) = this(ik)%internal(5)
            
         end if

         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 
         cd_surf = 1.d0

         select case( this(ik)%CDAN )
         case( i_PRPLx, i_PRPRx, i_SPSPx, i_SPPLx, i_SPCDx, i_SPDCx, i_CDPLx, i_CDCDx, i_PTPT3 )

            cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )

         case default

            call faterr(IAM,'IQS_EXPO_CZM_SPRING_P not implemented for this contact element')
            
         end select

        
         !                                                               aucune importance pas stocke 
         call prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,max(0.d0,this(ik)%gapTTbegin-this(ik)%internal(9)),0.d0)
         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd

         ! faudrait il verifier que c'est bien positif ? pas sur car c'est un ressort en compression
         
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(9) )/H       
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0
         this(ik)%covfrees=0.d0         


       !!!-------------------------------------
       CASE(i_ER_MAC_CZM)
          this(ik)%i_law = i_ER_MAC_CZM
          
          IF (.NOT. is_initialized) THEN
             
             IF (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow
             
             CALL init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)
             
          else
             this(ik)%taz(1) = this(ik)%internal(5) 
             
          END IF
          
          select case( this(ik)%CDAN )
          case( i_PRPLx, i_PRPRx, i_SPSPx, i_SPPLx, i_SPPRx, i_SPCDx, i_SPDCx, i_CDPLx, i_CDCDx, i_PTPT3 )

             cd_surf = get_surf( this(ik)%CDAN, this(ik)%icdan )

          case default

             call faterr(IAM,'ER_MAC_CZM not implemented for this contact element')

          end select

          CALL prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,MAX(0.d0,this(ik)%gapTTbegin),0.d0)
          
          this(ik)%covfreet = 0.D0
          this(ik)%covfreen = this(ik)%gapTTbegin/H
          this(ik)%covfrees = 0.D0

       !!!---------------------------------------
       CASE(i_IQS_CLB_RGR)
          this(ik)%i_law = i_IQS_CLB_RGR
          
          ! same as previous but with a mild gap violation restoring procedure, 
          ! so called "Radjai Gap Rescue", "RGR".
          ! see doc mjean
          ! write(*,'(A12,I5,1X,D12.5)')' k          ',ik,2.D0/(this(ik)%Wnn*OTH*OTH*H*H)
          ! The secure choice is ToverH>2*sqrt(2)*pi=8.8857578... One may try ToverH=1.D0, speeding rescuing, at your own risk.
          ! In case of troubles decrease the time step H.
          
          CALL get_gap_tol(ibehav,gap_tol)
          IF (this(ik)%gapTTbegin .GE. gap_tol) THEN
             this(ik)%covfreen = MAX(0.D0,this(ik)%gapTTbegin/H)
             this(ik)%corln = 0.D0
          ELSE
             CALL get_ToverH(ibehav,ToverH)
             OTH = Oneover2pi*ToverH
             this(ik)%corln   =-(2.D0/(this(ik)%Wnn*OTH*OTH))*((this(ik)%gapTTbegin-gap_tol)/H)
             if (.not. diagonal_resolution) then
               this(ik)%covfreet= this(ik)%Wtn*this(ik)%corln
               this(ik)%covfrees= this(ik)%Wsn*this(ik)%corln
             endif

             this(ik)%covfreen= this(ik)%Wnn*this(ik)%corln


          END IF

       !!!---------------------------------------
       CASE(i_CRITICAL_VOIGT_CLB)


         !!!---------------------------------------
         !<<<  update CVC
         !mj 15.11.06 revisited ...03.2007, 10.2007, 11.2007, 08.2009, 10.2013

         if (diagonal_resolution) then
           call faterr(IAM,'no diagonal resolution with CRITICAL_VOIGT_CLB contact law yet (dixit dubois)')
         endif

         this(ik)%i_law = i_CRITICAL_VOIGT_CLB

         !This model introduces an elementary dipole made of a spring and a parallel damper acting in 
         !the normal direction, and also such an elementary dipole acting in the tangential direction.
         !One end of the normal dipole is attached to the candidate contactor, the other end meeting 
         !the antagonist contactor. One end of the tangential dipole is attached to the candidate body. 
         !A micro pad is attached at the other end rubing the antagonist body with Coulomb friction.
         !See doc "Differentes manieres de gerer le contact unilateral et le frottement de Coulomb",
         !section Critical Voigt.

         !The interaction is monitored using an internal variable, internal(2), allowing
         !to activate either the IQS_CLB law or to activate the dipoles interaction, see below.
        
         !Stiffness and viscosity are chosen so that the elementary pair of contactors interacting through 
         !the dipoles be an oscillator with period T, critical damping (cv), or quite more (v). 
         !The spring damper analogy is used to provide a relevant strategy to restore penetration
         !when using extensively the quasi-inelastic shock law and to allow some ease between grains 
         !when locking is occuring.
 
         ! The parameters are T/H (ToverH) (H time step) and v/cv (vOVERcv) and the Coulomb friction coefficient.
         ! The internal parameters are,
         !internal(1) , internal(3), the lag (le decalage) or the length of the infinitesimal tangential spring,
         !(note that the gap (l'interstice) the length of the infinitesimal normal spring, is a local variable already stored).
         !internal(2) , an index taking the value 0.0 and 1.0 when the IQS_CLB law is on, and the value 2.0
         !when the dipoles interaction is on.
         !These internal parameters are printed in Vloc_Rloc.OUT.xxx 
         !The values T/H=1.0 and 1.0 < vOVERcv < 100.0 are convenient, 
         !knowing that the larger vOVERcv, the slower and harmless is the restitution process.
         !The time step should be small enough so as to ensure the stiffness is large enough (T small enough) to
         !limit the penetration.

         !BEWARE, THE FORMULA USED HEREAFTER ARE THOSE FOR THETA=1.D0.
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
         !write(*,*)'tata'
         !write(*,*)this(ik)%statusBEGIN
         !write(*,*)this(ik)%internal(2)

         
         !SWITCH : IQS_CLB on, dipoles interaction off
         !When internal(2)=2.0 , the IQS_CLB law is off, the dipoles interaction is on.
         !If the status is not anymore some contact status, internal(2) is switched to 0.0 ,
         !the dipoles interaction law is set to on and the IQS_CLB law is set to off,         
         if (this(ik)%statusBEGIN .eq. i_noctc .or. &
             this(ik)%statusBEGIN .eq. i_nknow .or. &
             this(ik)%statusBEGIN .eq. i_vnish .and. &
             this(ik)%internal(2) .gt. 1.5D0) &
              this(ik)%internal(2)=0.D0    
         !The lag is set to zero when the IQS_CLB law is on
         if (this(ik)%internal(2) .lt. 1.5D0) then 
           this(ik)%internal(1)=0.D0
           this(ik)%internal(3)=0.D0
         endif
        
         !END OF SWITCH

         !IQS_CLB on, dipoles interaction off. IQS_CLB is on as long as internal(2) is equal to 0.0 or 1.0 
         if (this(ik)%internal(2) .lt. 1.5D0) then

           !A quasi inelastic shock law is prescribed.
           !IQS law, i.e. default WWtt, WWnn, this(ik)%covfreet=0.D0, and          
           this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)

         endif
          
         !IQS_CLB off, dipoles interaction on. Dipoles remains active as long as internal(2)=2.0 .               
         if (this(ik)%internal(2) .gt. 1.5D0) then
                   
           call get_ToverH(ibehav,ToverH)
           call get_viscOVERcritvisc(ibehav,vOVERcv)
           OTH=Oneover2pi*ToverH

           this(ik)%WWnn=this(ik)%Wnn*(1.D0+0.5D0*(OTH*OTH/(1.D0+2.D0*vOVERcv*OTH)))

           !write(*,*)'H=',H,'T/H=',ToverH,'v/vc=',vOVERcv
           !write(*,'(1X,A3,D12.5)')'kn=',1.D0/( ( this(ik)%Wnn*0.5D0*(OTH*OTH/(1.D0+2.D0*vOVERcv*OTH)) )*H*H )
           !It is decided that the attachment point of the dipole is hooked on the candidate boundary
           !so that the geometric estimation of the gap is used to estimate this(ik)%covfreen. 

           this(ik)%covfreen=(this(ik)%gapTTbegin/H)*(1.D0/(1.D0+2.D0*vOVERcv*OTH))

           !It may be noticed that the equality to be satisfied with this law during the step [i,i+1] is,
           ! rlN(i+1) = - kN gap(i+1) - vN UN(i+1).
           !This equality is not to be, and cannot be satisfied at the step where an inelastic shock is prescribed.          

           !kTvT strategy*******

           this(ik)%WWtt=this(ik)%Wtt*(1.D0+0.5D0*(OTH*OTH/(1.D0+2.D0*vOVERcv*OTH)))
           this(ik)%WWss=this(ik)%Wss*(1.D0+0.5D0*(OTH*OTH/(1.D0+2.D0*vOVERcv*OTH)))

           !write(*,'(1X,A3,D12.5)')'kt=',1.D0/( ( this(ik)%Wtt*0.5D0*(OTH*OTH/(1.D0+2.D0*vOVERcv*OTH)) )*H*H )
           !write(*,'(1X,A3,D12.5)')'kt=',1.D0/(this(ik)%Wtt*OTH*OTH*H*H)
           !The lag (tangential spring length) is stored in this(ik)%internal(1), this(ik)%internal(3),
           !the pad sliding velocity is stored in this(ik)%vepadt, this(ik)%vepads, computed while iterating.
           !When no contact (this(ik)%gapTTbegin > 0.D0) the lag this(ik)%internal(1), this(ik)%internal(3), is set to zero. 
           !The prediction of the lag is used to estimate this(ik)%covfreet, this(ik)%covfrees 

           this(ik)%covfreet=(this(ik)%internal(1)/H)*(1.D0/(1.D0+2.D0*vOVERcv*OTH)) 
           this(ik)%covfrees=(this(ik)%internal(3)/H)*(1.D0/(1.D0+2.D0*vOVERcv*OTH))     

         endif 

         !SWITCH : IQS_CLB off, dipoles interaction on
         !At steps when internal(2)=0.0 the IQS_CLB law is on ; if the status comes to some  
         !contact status, internal(2) is increased from 0.0 to 1.0 and next step from 1.0 to 2.0 in order  
         !to allow the IQS_CLB law to be completed within two time steps. At the third step
         !internal(2)=2.0 , the IQS_CLB law is off and the dipoles interaction is on.
         if (this(ik)%statusBEGIN .ne. i_noctc .and. &
             this(ik)%statusBEGIN .ne. i_nknow .and. &
             this(ik)%statusBEGIN .ne. i_vnish .and. &
             this(ik)%internal(2) .lt. 1.5D0) &
             this(ik)%internal(2)=this(ik)%internal(2)+1.D0
         !END OF SWITCH

         !write(*,*)'tbtb'
         !write(*,*)this(ik)%statusBEGIN
         !write(*,*)this(ik)%internal(2)
                         
       !>>> update CVC
       !!!------------------------------------
       CASE(i_KV_WET)
          this(ik)%i_law = i_KV_WET

          this(ik)%gapREF=this(ik)%internal(2)
          L0 = this(ik)%internal(3)

          CALL get_forcePERstrain(ibehav,forcePERstrain)
          CALL get_forcePERstrainrate(ibehav,forcePERstrainrate)
          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)

          this(ik)%WWnn = this(ik)%Wnn + (this(ik)%gapREF/((forcePERstrain*H*H)+(forcePERstrainrate*H)))

          this(ik)%covfreen = ((H*forcePERstrain+(1-L0))/(H*forcePERstrain+forcePERstrainrate))* &
                              (this(ik)%gapTTbegin-this(ik)%gapREF)/H

          IF (this(ik)%gapTTbegin .LE. Wethk) THEN
             IF (this(ik)%statusBEGIN == i_nknow)THEN
                this(ik)%statusBEGIN=i_Wnnow
             ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
                this(ik)%statusBEGIN=i_Wnctc
             ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
                this(ik)%statusBEGIN=i_Wstck
             ELSE IF (this(ik)%statusBEGIN == i_slide)THEN
                this(ik)%statusBEGIN=i_Wslid
             END IF

             if (.not. diagonal_resolution) then
               this(ik)%covfrees= -normalcoh*this(ik)%Wsn*H
               this(ik)%covfreet= -normalcoh*this(ik)%Wtn*H
             endif

             this(ik)%covfreen= this(ik)%covfreen-normalcoh*this(ik)%WWnn*H
             this(ik)%corln   =-normalcoh*H
          END IF
       !!!------------------------------------

      CASE(i_NARD_ROD)
          this(ik)%i_law = i_NARD_ROD

          CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)

         !IF (.not. is_initialized) THEN
         IF (Nstep == 1) THEN

           !fd on stoque le max(0.,g0)             
           this(ik)%internal(1) = max(0.d0,this(ik)%gapTTbegin) 

           IF (this(ik)%CDAN .ne. i_PTPT3 ) THEN
             CALL faterr(IAM,'NARD_ROD not implemented for this contact element')
           END IF

           !on initialise le deplacement total tangent a 0   
           this(ik)%internal(2:3) = 0.d0 

           this(ik)%statusBEGIN=i_Cstck

         END IF

         !! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * !!
         ! Raideurs de contact et amortissement

          CALL get_nard_coeff(ibehav,this(ik)%internal,Ksn,Kst,Kvn,Kvt)

          Kss = Kst
          Kvs = Kvt

         !! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * !!

         ! Criteres de rupture normal

         Wethk=this(ik)%internal(1)+((this(ik)%internal(4)*normalcoh)/Ksn)
        
         ! Criteres de rupture tangent

         Dtmax = (this(ik)%internal(4)*tangalcoh)/Kst
         Dsmax = Dtmax

         !! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * !!

         IF (this(ik)%statusBEGIN == i_Cstck        .and. &
             (this(ik)%gapTTbegin .LE. Wethk)       .AND. &
             (abs(this(ik)%internal(2)) .LE. Dtmax) .AND. &
             (abs(this(ik)%internal(3)) .LE. Dsmax) ) THEN 

             IF (.not. diagonal_resolution) THEN

                this(ik)%covfreen = this(ik)%gapTTbegin/H &
                     + this(ik)%Wnn*(ksn*this(ik)%internal(1)*H + kvn*this(ik)%gapTTbegin) &
                     - this(ik)%Wnt*H*kst*this(ik)%internal(2) &
                     - this(ik)%Wns*H*kss*this(ik)%internal(3)
           
                this(ik)%covfreet = this(ik)%Wtn*(this(ik)%internal(1)*Ksn*H+this(ik)%gapTTbegin*Kvn) &
                                 - this(ik)%Wtt*H*kst*this(ik)%internal(2) &! increment tangent
                                 - this(ik)%Wts*H*kss*this(ik)%internal(3)
   

                this(ik)%covfrees = this(ik)%Wsn*(this(ik)%internal(1)*Ksn*H+this(ik)%gapTTbegin*Kvn) &
                                 - this(ik)%Wst*H*kst*this(ik)%internal(2) &! increment tangent
                              - this(ik)%Wss*H*kss*this(ik)%internal(3)

             ELSE

                this(ik)%covfreen = this(ik)%gapTTbegin/H &
                     + this(ik)%Wnn*(ksn*this(ik)%internal(1)*H + kvn*this(ik)%gapTTbegin)

                this(ik)%covfreet = - (this(ik)%Wtt+this(ik)%Wss)*0.5*H*kst*this(ik)%internal(2) ! increment tangent

                this(ik)%covfrees = - (this(ik)%Wtt+this(ik)%Wss)*0.5*H*kss*this(ik)%internal(3)

             END IF

          ELSE

             IF ((this(ik)%internal(1) .NE. 1.d0) .AND. &
                 (this(ik)%internal(1) .NE. 2.d0) .AND. &
                 (this(ik)%internal(1) .NE. 3.d0) .AND. &
                 (this(ik)%internal(1) .NE. 4.d0) ) THEN

                IF ( (this(ik)%gapTTbegin .GT. Wethk)       .AND. &
                     (abs(this(ik)%internal(2)) .GT. Dtmax) .AND. &
                     (abs(this(ik)%internal(3)) .LE. Dsmax) ) THEN

                   this(ik)%internal(1) = 4.d0

                ELSE IF ( (this(ik)%gapTTbegin .GT. Wethk) ) THEN

                   this(ik)%internal(1) = 1.d0

                ELSE IF ( (abs(this(ik)%internal(2)) .GT. Dtmax) ) THEN

                   this(ik)%internal(1) = 2.d0

                ELSE IF ( (abs(this(ik)%internal(3)) .GT. Dsmax) ) THEN

                   this(ik)%internal(1) = 3.d0

                END IF 
             END IF

             this(ik)%statusBEGIN = i_noctc

          END IF


      case(i_TOSI_CZM,i_TOSI_CZM_INCRE)
          
          this(ik)%i_law = tact_behav(ibehav)%ilaw !i_MAC_CZM
          
          IF (.NOT. is_initialized) THEN
             
             IF (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow
             
             CALL init_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz)
             
          END IF

         this(ik)%taz(1) = this(ik)%vltBEGIN        
         this(ik)%taz(2) = this(ik)%vlnBEGIN
         this(ik)%taz(3) = this(ik)%vlsBEGIN         
         this(ik)%taz(4) = get_time()

         call get_bulk_stress_triaxiality_csasp(this(ik)%icdan,this(ik)%taz(5))
         call get_bulk_temperature_csasp(this(ik)%icdan,this(ik)%taz(6))

         !OPEN(unit=556, file='triaxialityy.txt', iostat=ios)
         !WRITE(556,*) this(ik)%taz(5)
          
          select case( this( ik )%CDAN )
          case( i_CSASp )
             cd_surf = get_surf( this( ik )%CDAN, this( ik )%icdan )

          case default
             call faterr(IAM,'this contact element does not work with'//tact_behav(ibehav)%lawty)

          end select

          CALL prep_CZM_3D(ibehav,this(ik)%internal,cd_surf,0.d0,this(ik)%gapTTbegin,0.d0)
          
          this(ik)%covfreet=0.d0
          this(ik)%covfreen=(this(ik)%gapTTbegin - get_dilatancy_height(ibehav,this(ik)%internal))/H
          this(ik)%covfrees=0.d0

          
       !!!------------------------------------
       CASE default
          call faterr(IAM,'default select case '//tact_behav(ibehav)%lawty )

       !!!------------------------------------
       END SELECT

       IF(.NOT.diagonal_resolution)THEN
          
          !-----------------------------------------------
          ! Warning non uniqueness cases
          !-----------------------------------------------
          det1 = this(ik)%WWss*this(ik)%WWnn-this(ik)%Wns*this(ik)%Wsn
          det2 = this(ik)%Wts*this(ik)%WWnn-this(ik)%Wns*this(ik)%Wtn
          det3 = this(ik)%Wts*this(ik)%Wsn-this(ik)%WWss*this(ik)%Wtn
          det  = this(ik)%WWtt*det1-this(ik)%Wst*det2+this(ik)%Wnt*det3
          
          this(ik)%det=det
          IF (det .LT. 1.D-24) THEN
             WRITE(cout,546)ik,det
             CALL LOGMES(cout)

             print*,ik
             print*,this(ik)%WWtt,this(ik)%Wtn,this(ik)%Wts
             print*,this(ik)%Wnt,this(ik)%WWnn,this(ik)%Wns
             print*,this(ik)%Wst,this(ik)%Wsn,this(ik)%WWss

          END IF
          
          this(ik)%ron = 1.D0/this(ik)%Wnn
          
          Sw = this(ik)%WWtt+this(ik)%WWss   
          Dw = (Sw*Sw)-4.D0*(this(ik)%WWtt*this(ik)%WWss-this(ik)%Wts*this(ik)%Wst) 
          
          IF (Dw>0.D0) THEN
             Dw = SQRT(Dw)
          ELSE
             Dw = 0.D0
          END IF
          
          this(ik)%rot=2.D0*(Sw-Dw)/((Sw+Dw)*(Sw+Dw))    
          
       ELSE
          
          ! Case of diagonal resolution
          
          this(ik)%invWnn = 1.D0/this(ik)%WWnn
          this(ik)%invWtt = 1.D0/this(ik)%WWtt
          this(ik)%invWss = 1.D0/this(ik)%WWss
          
          !-----------------------------------------------
          ! Warning non uniqueness cases
          !-----------------------------------------------
          
          det = this(ik)%invWtt*this(ik)%invWss*this(ik)%invWnn
          
          IF (det .LT. 1.D-24) THEN

             print*,''
             call print_info(ik)
             print*,''
             print*,get_inter_law_name_from_id(tact_behav(ibehav)%ilaw)
             print*,''
             print*,this(ik)%WWtt,this(ik)%Wtn,this(ik)%Wts
             print*,this(ik)%Wnt,this(ik)%WWnn,this(ik)%Wns
             print*,this(ik)%Wst,this(ik)%Wsn,this(ik)%WWss
             print*,''             
             WRITE(cout,546) ik,det
546          FORMAT(1X,'    det(',I5,') =',D12.5,' < 1.D-25')
             CALL LOGMES(cout)
          END IF
       END IF

       !-----------------------------------------------
       ! Computing free local vlocy
       !-----------------------------------------------
       
       CALL prjj_(ik,this(ik)%vfrees,this(ik)%vfreet,this(ik)%vfreen,iVfree)

       if(get_pressure_flag(ibehav) .eq. 4 .and.  this(ik)%taz(1) /= 1.d0) then 
           call get_external_pressure_(ik,this(ik)%taz(3))
       endif

       if (.false.) then
         print*,'-----'
         print*,'contact ',ik
         print*,'Vfree'
         write(*,'(3(1x,D12.5))') this(ik)%vfreet,this(ik)%vfreen,this(ik)%vfrees
         print*,'W'
         write(*,'(3(1x,D12.5))') this(ik)%WWtt,this(ik)%Wtn,this(ik)%Wts
         write(*,'(3(1x,D12.5))') this(ik)%Wnt,this(ik)%WWnn,this(ik)%Wns
         write(*,'(3(1x,D12.5))') this(ik)%Wst,this(ik)%Wsn,this(ik)%WWss
         print*,'covfree'
         print*,this(ik)%covfreet,this(ik)%covfreen,this(ik)%covfrees
       endif

      !  if ( abs(this(ik)%WWnn) >1d10 .or. abs(this(ik)%WWtt) >1d10 .or. abs(this(ik)%WWss) >1d10 .or. &
      !      abs(this(ik)%vfreet)>1d2 .or. abs(this(ik)%vfreen) > 1d2 .or. abs(this(ik)%vfrees)> 1d2 ) then
      !        print*,''
      !        call print_info(ik)
      !        print*,''
      !        print*,get_inter_law_name_from_id(tact_behav(ibehav)%ilaw)
      !        print*,''
      ! endif
    END DO
    
    is_initialized = .TRUE.
    
555 FORMAT(1X,'  this(',I0,')%gapREF =',D12.5,' < 1.D-18') 
    
  END SUBROUTINE prep_nlgs
!!!------------------------------------------------------------------------  
  SUBROUTINE solve_nlgs(i_what)

   IMPLICIT NONE
                            !1234567890123456789
   CHARACTER(len=19) :: IAM='nlgs_3D::solve_nlgs'
   CHARACTER(len=80) :: cout
   INTEGER           :: i_what
   INTEGER           :: ik,ikk,ibehav,ilaw
   integer(kind=4)   :: sstatusik
   REAL(kind=8)      :: DET,FFN
   REAL(kind=8)      :: fricik
   REAL(kind=8)      :: WWttik,WWtnik,WWtsik,WWntik,WWnnik,WWnsik,WWstik,WWsnik,WWssik

   REAL(kind=8)      :: vlocfreetik,vlocfreenik,vlocfreesik
   REAL(kind=8)      :: vvlocfreesik,vvlocfreetik,vvlocfreenik

   REAL(kind=8)      :: Wrlsik,Wrltik,Wrlnik,Wrlsiki,Wrltiki,Wrlniki

   REAL(kind=8)      :: vsik,  vtik,  vnik,   rsik,   rtik,   rnik
   REAL(kind=8)      :: vlsik, vltik, vlnik,  rlsik,  rltik,  rlnik
   REAL(kind=8)      :: vlsiki,vltiki,vlniki, rlsiki, rltiki, rlniki

   REAL(kind=8)      :: rlocs,rloct,rlocn,vls,vlt,vln,Dvls,Dvlt,Dvln 
   INTEGER           :: ikjl,iadj
   integer(kind=4)   :: istart,iistart
   REAL(kind=8)      :: WRRmin=1.D20,WRRmax=-1.D20,gapTTik
   !fd cohesifs
   REAL(kind=8)      :: hradh_t,hradh_n,hradh_s,Hp,k_t,k_n,ut,un,us
   LOGICAL           :: is_cohesive,is_traction

   REAL(kind=8)      ::    Att,   Atn,   Ats,   Ant,   Ann,   Ans,   Ast,   Asn,   Ass
   REAL(kind=8)      :: invAtt,invAtn,invAts,invAnt,invAnn,invAns,invAst,invAsn,invAss,inv_deta,vt,vn,vs

   REAL(kind=8)      :: snmax,forceperstrain,forcepergap,alphaik,fnik,rtt

   !< nard_rod
   real(kind=8)      :: Ksn, Kst, Kss, Kvn, Kvt, Kvs
   REAL(kind=8)      :: Ttt, Ttn, Tts, Tnt, Tnn, Tns, Tst, Tsn, Tss
   REAL(kind=8)      :: inv_detT
   ! nard_rod />

   !fd auxiliary variable for expo_spring
   real(kind=8)     :: cn,ct,s1,s2,G1,G2,eta,k1,k2,AA,vdt,vds,ktt,rt,kkk,mu_g
   
   
   if( JACOBI_SOLVER .and. RELAX >0.999d0 ) then
      CALL LOGMES('WARNING!! Usage of a jacobi solver with RELAX=1 may not converge.')
   endif

   IF (nb_CDAN == 0) RETURN

   IF (i_what == i_iter) nlgs_loop = nlgs_loop + 1

   !Pour solveur jacobi
   if( JACOBI_SOLVER ) then
      do ik=1,nb_CDAN
         this(ik)%rlt_jacobi = this(ik)%rlt
         this(ik)%rln_jacobi = this(ik)%rln
         this(ik)%rls_jacobi = this(ik)%rls
      end do
   end if
   
   !$OMP PARALLEL DEFAULT(SHARED)                                                                  &
   !$OMP PRIVATE(ikk,ik,rlsik,rltik,rlnik,rlsiki,rltiki,rlniki,                                    &
   !$OMP         Wrlsik,Wrltik,Wrlnik,Wrlsiki,Wrltiki,Wrlniki,                                     &
   !$OMP         vlsik,vltik,vlnik,vlsiki,vltiki,vlniki,vlocfreesik,vlocfreetik,vlocfreenik,       &
   !$OMP         sstatusik,vsik,vtik,vnik,istart,iadj,iistart,ikjl,ibehav,ilaw,                    &
   !$OMP         WWssik,WWstik,WWsnik,WWtsik,WWttik,WWtnik,WWnsik,WWntik,WWnnik,                   &
   !$OMP         vvlocfreesik,vvlocfreetik,vvlocfreenik,fricik,                                    &
   !$OMP         k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s,Hp,ut,us,un,                          &
   !$OMP         Att,Ant,Ast,Atn,Ann,Asn,Ats,Ans,Ass,                                              &
   !$OMP         invAtt,invAnt,invAst,invAtn,invAnn,invAsn,invAts,invAns,invAss,inv_detA,          &
   !$OMP         vt,vn,vs,                                                                         &
   !$OMP         rsik,rtik,rnik,rlocs,rloct,rlocn,vls,vlt,vln,Dvls,Dvlt,Dvln,                      &
   !$OMP         DVDV,DVDVRR,DVoR,WRR,FFN,DET,gapTTik,alphaik,fnik,rtt,                            &
   !$OMP         Ksn,Kst,Kss,Kvn,Kvt,Kvs,Ttt,Ttn,Tts,Tnt,Tnn,Tns,Tst,Tsn,Tss,inv_detT,             &
   !$OMP          cn,ct,s1,s2,G1,G2,eta,k1,k2,AA,vdt,vds,ktt,rt)
   
   !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:SumDVDV,SumDVDVRR,SumDVoR,SumWRR,SumWRWR,Nb_RGR,  &
   !$OMP                                  Nnoact,Nnoctc,Nactif,Ncompr,Ntract,Nstick,Nslide,NOKsta) &
   !$OMP                      REDUCTION(MAX:MaxDVDV,MaxDVDVRR,WRRmax)                              &
   !$OMP                      REDUCTION(MIN:WRRmin)
   
   DO ikk=1,nb_CDAN
      
      ! Changing at random computational ordering of contact elements
      ik=IALEAT(ikk)
      
      rlsik = 0.D0 ; rltik = 0.D0 ; rlnik = 0.D0 
      rlsiki= 0.D0 ; rltiki= 0.D0 ; rlniki= 0.D0
      
      vlsik = 0.D0 ; vltik = 0.D0 ; vlnik = 0.D0
      vlsiki= 0.D0 ; vltiki= 0.D0 ; vlniki= 0.D0

      Wrlsik = 0.D0 ; Wrltik = 0.D0 ; Wrlnik = 0.D0
      Wrlsiki= 0.D0 ; Wrltiki= 0.D0 ; Wrlniki= 0.D0
      
      vlocfreesik = 0.D0 ; vlocfreetik = 0.D0 ; vlocfreenik = 0.D0
      
      IF (this(ik)%forecast == i_noact) THEN   

         ! rlik and rliki = 0

         vlsik = this(ik)%vfrees
         vltik = this(ik)%vfreet
         vlnik = this(ik)%vfreen

         vlocfreesik = vlsik
         vlocfreetik = vltik
         vlocfreenik = vlnik

         !fd un bug ? 
         ! si dans le prep on decide de ne pas traiter le contact   
         ! alors on lui donne comme statut celui defini a ce moment la
         sstatusik   = this(ik)%status

      ELSE IF (this(ik)%forecast == i_acton) THEN

         ! Skipping if status is repeatedly i_noctc but safeguardly checking every i*(i+1)/2 iterations

         IF (i_what == i_iter) THEN
            IF (this(ik)%status == i_noctc) THEN
               this(ik)%inoctc=this(ik)%inoctc+1
               IF (this(ik)%inoctc < this(ik)%iskip*(this(ik)%iskip+1)/2) THEN
                  CYCLE
               ELSE
                  this(ik)%iskip=this(ik)%iskip+1
               END IF
            ELSE
               this(ik)%inoctc=0
               this(ik)%iskip=1
            ENDIF
         END IF
         
         IF (i_what == i_check) THEN
            this(ik)%inoctc=0
            this(ik)%iskip=1
         ENDIF

         ! Computing vlocfree
         
         rlsik = this(ik)%rls
         rltik = this(ik)%rlt
         rlnik = this(ik)%rln

         ! Computing_________________________________________ H~ p~W p H Rloc(ik)

         Wrltik = this(ik)%Wts*rlsik + this(ik)%Wtt*rltik + this(ik)%Wtn*rlnik
         Wrlnik = this(ik)%Wns*rlsik + this(ik)%Wnt*rltik + this(ik)%Wnn*rlnik
         Wrlsik = this(ik)%Wss*rlsik + this(ik)%Wst*rltik + this(ik)%Wsn*rlnik

         ! Computing_________________________________________ H* p* invM p H Rloc(ik)

         IF(.NOT.SDLactif)THEN
            CALL vitrad_(ik,iVaux_e_invM_t_Ireac)   
            CALL prjj_ (ik,vsik,vtik,vnik,iVaux_)
            
            vltik = this(ik)%vfreet+vtik
            vlnik = this(ik)%vfreen+vnik
            vlsik = this(ik)%vfrees+vsik

            ! Computing_________________________________________ H~ p~W p H Rloc - H~ p~W p H Rloc(ik)     
            !                                                  = vlocfree(ik)
            vlocfreetik = vltik-Wrltik            
            vlocfreenik = vlnik-Wrlnik  
            vlocfreesik = vlsik-Wrlsik
            !
         ELSE
            ! Computing contribution of contacts jl
            istart = this(ik)%istart

            vlocfreesik = 0.D0
            vlocfreetik = 0.D0
            vlocfreenik = 0.D0
            
            if( DDM_SCHWARTZ ) then
               call prjj_(ik, vlocfreesik, vlocfreetik, vlocfreenik, iVddm_ )
            end if

            DO iadj=1,this(ik)%nbadj
               ikjl=this(ik)%adjjl(iadj)
               iistart = istart + 9*iadj
               
               !Pour solveur Gauss Seidel
               if(  .not. JACOBI_SOLVER ) then
                  vlocfreesik = vlocfreesik + Wab(iistart-8)*this(ikjl)%rls &
                                            + Wab(iistart-7)*this(ikjl)%rlt &
                                            + Wab(iistart-6)*this(ikjl)%rln
                  vlocfreetik = vlocfreetik + Wab(iistart-5)*this(ikjl)%rls &
                                            + Wab(iistart-4)*this(ikjl)%rlt &
                                            + Wab(iistart-3)*this(ikjl)%rln
                  vlocfreenik = vlocfreenik + Wab(iistart-2)*this(ikjl)%rls &
                                            + Wab(iistart-1)*this(ikjl)%rlt &
                                            + Wab(iistart  )*this(ikjl)%rln
               !Pour solveur Jacobi                             
               else
                  vlocfreesik = vlocfreesik + Wab(iistart-8)*this(ikjl)%rls_jacobi &
                                            + Wab(iistart-7)*this(ikjl)%rlt_jacobi &
                                            + Wab(iistart-6)*this(ikjl)%rln_jacobi
                  vlocfreetik = vlocfreetik + Wab(iistart-5)*this(ikjl)%rls_jacobi &
                                            + Wab(iistart-4)*this(ikjl)%rlt_jacobi &
                                            + Wab(iistart-3)*this(ikjl)%rln_jacobi
                  vlocfreenik = vlocfreenik + Wab(iistart-2)*this(ikjl)%rls_jacobi &
                                            + Wab(iistart-1)*this(ikjl)%rlt_jacobi &
                                            + Wab(iistart  )*this(ikjl)%rln_jacobi
               end if
               
            END DO

            
            vlocfreesik = vlocfreesik+this(ik)%vfrees            
            vlocfreetik = vlocfreetik+this(ik)%vfreet
            vlocfreenik = vlocfreenik+this(ik)%vfreen 

            vlsik = vlocfreesik+Wrlsik       
            vltik = vlocfreetik+Wrltik       
            vlnik = vlocfreenik+Wrlnik 

         END IF  ! IF(.NOT.SDLactif)THEN
         
         !----------------------
         ! Convert to auxiliary
         !----------------------
         
         WWssik=this(ik)%WWss ; WWstik=this(ik)%Wst  ; WWsnik=this(ik)%Wsn
         WWtsik=this(ik)%Wts  ; WWttik=this(ik)%WWtt ; WWtnik=this(ik)%Wtn
         WWnsik=this(ik)%Wns  ; WWntik=this(ik)%Wnt  ; WWnnik=this(ik)%WWnn
         
         vvlocfreesik = vlocfreesik+this(ik)%covfrees
         vvlocfreetik = vlocfreetik+this(ik)%covfreet
         vvlocfreenik = vlocfreenik+this(ik)%covfreen
         
         fricik = this(ik)%fric
         ibehav = this(ik)%lawnb 
         ilaw   = this(ik)%i_law


         !fd resolution 


         !print*,ik
         !print*,vtik,vnik,vsik
         !print*,vvlocfreetik,vvlocfreenik,vvlocfreesik
         !print*,vltik,vlnik,vlsik

         IF (diagonal_resolution) THEN

            ! on fait a la Moreau ... vv va aussi contenir la reaction 
            ! du coup precedent donc on travaille sur la correction
            ! l'operateur WW est approxime par un operateur diagonal

            SELECT CASE(ilaw)

            !!!-------------------------------------------
            CASE(i_ELASTIC_ROD,i_VOIGT_ROD,i_NORMAL_COUPLED_DOF)

               !fd me semble faux ...
               !fd vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
               vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik               

               ! default 
               !rlsiki = 0.D0
               !rltiki = 0.D0

               rlniki = -vvlocfreenik*this(ik)%invWnn

               sstatusik=i_stick

            !!!-------------------------------------------
            CASE(i_RIGID_WIRE,i_ELASTIC_WIRE,i_VOIGT_WIRE)

               !fd me semble faux ...
               !fd vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
               vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik

               ! default 
               !rlsiki = 0.D0
               !rltiki = 0.D0

               IF (vvlocfreenik .GT. 0.D0) THEN
                  rlniki = -vvlocfreenik*this(ik)%invWnn
                  sstatusik=i_stick
               ELSE
                  rlniki = 0.D0
                  sstatusik=i_vnish
               ENDIF

            !!!--------------------------------------
            CASE(i_BRITTLE_ELASTIC_WIRE)

               ! default
               !rlsiki = 0.D0
               !rltiki = 0.D0

               !fd me semble faux ...
               !fd vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
               vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik               

               !print*,'vvlocfreenik ', vvlocfreenik

               IF (vvlocfreenik .GT. 0.D0) THEN

                  rlniki = - vvlocfreenik*this(ik)%invWnn

                  !print*,'rlniki', rlniki

                  call get_snmax(ibehav,snmax)
                  if (rlniki < -H*snmax) then
                    sstatusik=i_vnish
                    rlniki = 0.d0
                  else
                    sstatusik=i_stick
                  endif
               ELSE
                  rlniki = 0.D0
                  sstatusik=i_noctc
               ENDIF

            !!!--------------------------------------
            CASE(i_COUPLED_DOF)
               !fd me semble faux ...           
               !fd vvlocfreetik = vltik + this(ik)%covfreet - this(ik)%WWtt*rltik
               !fd vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
               !fd vvlocfreesik = vlsik + this(ik)%covfrees - this(ik)%WWss*rlsik
               vvlocfreetik = vltik + this(ik)%covfreet - this(ik)%Wtt*rltik
               vvlocfreenik = vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik
               vvlocfreesik = vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik

               rltiki   = - this(ik)%invWtt*vvlocfreetik
               rlniki   = - this(ik)%invWnn*vvlocfreenik
               rlsiki   = - this(ik)%invWss*vvlocfreesik

               sstatusik=i_stick
               
            !!!-------------------------------------------
            CASE(i_MAC_CZM,i_MP_CZM,i_MP3_CZM,i_MAL_CZM,i_TH_CZM,i_ABP_CZM,i_EXPO_CZM, &
                 i_IQS_MAC_CZM,i_IQS_MAL_CZM,i_IQS_TH_CZM,i_IQS_ABP_CZM,i_IQS_EXPO_CZM, &
                 i_TOSI_CZM,i_TOSI_CZM_INCRE)

               CALL iter_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s,Hp)

               CALL get_fric_CZM(ibehav,this(ik)%taz,fricik)

               IF (is_cohesive) THEN

                  !fd me semble pas terrible ...
                  !fd WWttik=(this(ik)%Wss+this(ik)%Wtt)*0.5 ; WWtnik=0.d0          ; WWtsik=0.d0
                  !fd WWntik=0.d0                            ; WWnnik=this(ik)%Wnn  ; WWnsik=0.d0
                  !fd WWstik=0.d0                            ; WWsnik=0.d0          ; WWssik=(this(ik)%Wss+this(ik)%Wtt)*0.5
                  
                  WWttik=this(ik)%WWtt ; WWtnik=0.d0          ; WWtsik=0.d0                              
                  WWntik=0.d0          ; WWnnik=this(ik)%WWnn  ; WWnsik=0.d0                              
                  WWstik=0.d0          ; WWsnik=0.d0           ; WWssik=this(ik)%WWss

                  vvlocfreetik=vltik + this(ik)%covfreet - WWttik*rltik
                  vvlocfreenik=vlnik + this(ik)%covfreen - WWnnik*rlnik
                  vvlocfreesik=vlsik + this(ik)%covfrees - WWssik*rlsik

                  ut = vvlocfreetik + (WWttik * Hradh_t) + (WWtnik * Hp)
                  un = vvlocfreenik                      + (WWnnik * Hp)
                  us = vvlocfreesik + (WWssik * Hradh_s) + (WWsnik * Hp)
                  
                  Att = 1.D0 - (WWttik*k_t) ; Atn = 0.d0                ; Ats = 0.D0     
                  Ant = 0.d0                ; Ann = 1.D0 - (WWnnik*k_n) ; Ans = 0.D0     
                  Ast = 0.d0                ; Asn = 0.d0                ; Ass = 1.D0 - (WWssik*k_t) 
                  
                  invAtt =  1.d0/Att ; invAtn =  0.d0     ; invAts =  0.d0
                  invAnt =  0.d0     ; invAnn =  1.d0/Ann ; invAns =  0.d0
                  invAst =  0.d0     ; invAsn =  0.d0     ; invAss =  1.d0/Ass

                  !fd matrices diagonales qui simplifient tout !!

                  vvlocfreetik = (invAtt * ut) 
                  vvlocfreenik =                 (invAnn * un)
                  vvlocfreesik =                                (invAss *us)

                  Att = (invAtt * WWttik) 
                  Atn = 0.d0
                  Ats = 0.d0
                  Ant = 0.d0
                  Ann = (invAnn * WWnnik)
                  Ans = 0.d0
                  Ast = 0.d0
                  Asn = 0.d0
                  Ass = (invAss * WWssik)
                  
                  WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                  WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                  WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 

                  rltiki   = - vvlocfreetik/WWttik
                  rlniki   = - vvlocfreenik/WWnnik
                  rlsiki   = - vvlocfreesik/WWssik

                  IF (rlniki<=0.D0) THEN
                     sstatusik=i_noctc
                     rltiki=0.D0
                     rlniki=0.D0
                     rlsiki=0.D0
                  ELSE
                     FFN = fricik*rlniki
                     DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                     
                     IF (DET.LT.FFN) THEN
                        sstatusik=i_stick
                     ELSE
                        sstatusik=i_slide
                        IF (DET .GT. 1D-18) THEN
                           rltiki = rltiki*FFN/DET
                           rlsiki = rlsiki*FFN/DET
                        END IF
                     END IF
                  END IF

               ELSE  ! plus cohesif
                  !fd me semble faux ...
                  !fd vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%WWtt*rltik
                  !fd vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
                  !fd vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%WWss*rlsik
                  vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (WWtnik * Hp)
                  vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + (WWnnik * Hp) 
                  vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (WWsnik * Hp) 

                  IF (vvlocfreenik >= 0.0)THEN

                     sstatusik=i_noctc
                     rlniki   = 0.0
                     rltiki   = 0.0
                     rlsiki   = 0.0
                     
                  ELSE
                     
                     rltiki   =  - (this(ik)%invWtt*vvlocfreetik)
                     rlniki   =  - (this(ik)%invWnn*vvlocfreenik)
                     rlsiki   =  - (this(ik)%invWss*vvlocfreesik)
                     
                     FFN = fricik*rlniki
                     DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                        
                     IF (DET.LT.FFN) THEN
                       sstatusik=i_stick
                     ELSE
                       sstatusik=i_slide
                       IF (DET .GT. 1D-18) THEN
                         rltiki = rltiki*FFN/DET
                         rlsiki = rlsiki*FFN/DET
                       END IF
                     END IF
                  END IF
               END IF
			   
            !!!-------------------------------------------
            CASE(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)

               CALL iter_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s,Hp)

               CALL get_fric_CZM(ibehav,this(ik)%taz,fricik)

               IF (is_cohesive) THEN

                  ! print*,ik
                  ! print*,this(ik)%internal
                  ! print*,this(ik)%taz
                  
                  !fd me semble pas terrible ...
                  !fd WWttik=(this(ik)%Wss+this(ik)%Wtt)*0.5 ; WWtnik=0.d0          ; WWtsik=0.d0
                  !fd WWntik=0.d0                            ; WWnnik=this(ik)%Wnn  ; WWnsik=0.d0
                  !fd WWstik=0.d0                            ; WWsnik=0.d0          ; WWssik=(this(ik)%Wss+this(ik)%Wtt)*0.5

                  WWttik=this(ik)%WWtt ; WWtnik=0.d0           ; WWtsik=0.d0                              
                  WWntik=0.d0          ; WWnnik=this(ik)%WWnn  ; WWnsik=0.d0                              
                  WWstik=0.d0          ; WWsnik=0.d0           ; WWssik=this(ik)%WWss

                  vvlocfreetik=vltik + this(ik)%covfreet - WWttik*rltik
                  vvlocfreenik=vlnik + this(ik)%covfreen - WWnnik*rlnik
                  vvlocfreesik=vlsik + this(ik)%covfrees - WWssik*rlsik

                  ut = vvlocfreetik + (WWttik * Hradh_t) + (WWtnik * Hp)
                  un = vvlocfreenik + (WWnnik * Hradh_n) + (WWnnik * Hp)
                  us = vvlocfreesik + (WWssik * Hradh_s) + (WWsnik * Hp)
                  
                  Att = 1.D0 - (WWttik*k_t) ; Atn = 0.d0                ; Ats = 0.D0     
                  Ant = 0.d0                ; Ann = 1.D0 - (WWnnik*k_n) ; Ans = 0.D0     
                  Ast = 0.d0                ; Asn = 0.d0                ; Ass = 1.D0 - (WWssik*k_t) 
                  
                  invAtt =  1.d0/Att ; invAtn =  0.d0     ; invAts =  0.d0
                  invAnt =  0.d0     ; invAnn =  1.d0/Ann ; invAns =  0.d0
                  invAst =  0.d0     ; invAsn =  0.d0     ; invAss =  1.d0/Ass

                  !fd matrices diagonales qui simplifient tout !!

                  vvlocfreetik = (invAtt * ut) 
                  vvlocfreenik =                 (invAnn * un)
                  vvlocfreesik =                                (invAss *us)

                  Att = (invAtt * WWttik) 
                  Atn = 0.d0
                  Ats = 0.d0
                  Ant = 0.d0
                  Ann = (invAnn * WWnnik)
                  Ans = 0.d0
                  Ast = 0.d0
                  Asn = 0.d0
                  Ass = (invAss * WWssik)
                  
                  WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                  WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                  WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 

                  rltiki   = - vvlocfreetik/WWttik
                  rlniki   = - vvlocfreenik/WWnnik
                  rlsiki   = - vvlocfreesik/WWssik

                  IF (rlniki<=0.D0) THEN
                     sstatusik=i_noctc
                     rltiki=0.D0
                     rlniki=0.D0
                     rlsiki=0.D0
                  ELSE
                     FFN = fricik*rlniki
                     DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                     
                     IF (DET.LT.FFN) THEN
                        sstatusik=i_stick
                     ELSE
                        sstatusik=i_slide
                        IF (DET .GT. 1D-18) THEN
                           rltiki = rltiki*FFN/DET
                           rlsiki = rlsiki*FFN/DET
                        END IF
                     END IF
                  END IF

               ELSE  ! plus cohesif
                  !fd me semble faux ...
                  !fd vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%WWtt*rltik
                  !fd vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
                  !fd vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%WWss*rlsik
                  vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (WWtnik * Hp)
                  vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + (WWnnik * Hp) 
                  vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (WWsnik * Hp) 

                  IF (vvlocfreenik >= 0.0)THEN

                     sstatusik=i_noctc
                     rlniki   = 0.0
                     rltiki   = 0.0
                     rlsiki   = 0.0
                     
                  ELSE
                     
                     rltiki   =  - (this(ik)%invWtt*vvlocfreetik)
                     rlniki   =  - (this(ik)%invWnn*vvlocfreenik)
                     rlsiki   =  - (this(ik)%invWss*vvlocfreesik)
                     
                     FFN = fricik*rlniki
                     DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                        
                     IF (DET.LT.FFN) THEN
                       sstatusik=i_stick
                     ELSE
                       sstatusik=i_slide
                       IF (DET .GT. 1D-18) THEN
                         rltiki = rltiki*FFN/DET
                         rlsiki = rlsiki*FFN/DET
                       END IF
                     END IF
                  END IF
               END IF


!!!-------------------------------------
         case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)

            !fd il s'agit d'un montage serie elastique + czm ( contact/frottant cohesif endommageable )

            !fd - en test - resolution dans l'esprit "diago"
            !fd ainsi on resoud d'abord la partie normale, puis la tangente

            call get_czm_expo_spring(ibehav,cn,ct,s1,s2,G1,G2,eta,k1,k2)
            call get_fric_CZM_spring(10,ibehav,this(ik)%taz,this(ik)%internal,fricik) !! 10=i_beta_coh_th indice de beta_coh_th dans la table des variables internes

            ! partie normale

            ! status indetermine
            sstatusik=i_nknow  

            ! cas de la compression (contact - elas)
            if ( vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik <= 0.d0 ) then

              ! seule la partie elastique travaille 
              WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*k1*H*H)) 

              !fd 
              rlniki = - (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik) / WWnnik                            

            ! cas de la traction (cohesif endo - elas)
            else

              if (this(ik)%taz(1) > 0.d0 .and. (this(ik)%internal(10) > 0.d0))  then                  
                ! parties cohesive endo et elastique travaillent 
                WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*(k1*this(ik)%internal(11))*H*H))
                WWnnik = 1.d0 + (WWnnik*this(ik)%internal(1)*this(ik)%taz(1)*(cn*this(ik)%internal(10))*H*H)                
                !fd on calcule l'allongement endo/H
                AA = (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik) / WWnnik
                !fd l'allongement endo cumule
                AA  = H*AA                
                !fd sa partie positive 
                AA = (dabs(AA)+AA)*0.5
                ! l'impulsion cohesive
                rlniki = -this(ik)%internal(1)*this(ik)%taz(1)*(cn*this(ik)%internal(10))*H*AA
              else
                ! c'est casse ... on garde le WWnnik mais reaction nulle                 
                WWnnik = 1.d0 
                rlniki = 0.d0
                rlsiki = 0.d0
                rltiki = 0.d0                
                vdt=0.d0
                vds=0.d0
                sstatusik=i_noctc
              endif
            endif

            if (sstatusik /= i_noctc) then
            
               ! partie tangente
               ktt = (this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H)*(this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*(k2*this(ik)%internal(11))*H*H)))
               kss = (this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H)*(this(ik)%Wss+(1.D0/(this(ik)%internal(1)*(k2*this(ik)%internal(11))*H*H)))

               ! traction (cohesif endo - elas)
               if (rlniki < 0.d0) then

                 ! traction-cisaillement 

                 ! cohesif endo et elastiques travaillent 

                 if (this(ik)%taz(1) > 0.d0) then                 
                   WWttik = 1.d0 + ktt
                   WWssik = 1.d0 + kss 
                 else
                   WWttik = 1.d0
                   WWssik = 1.d0                 
                 endif  

                 ! vitesse endo tangentielle
                 vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(7)/H) - &
                       (ktt*this(ik)%internal(2)/H))/WWttik
                 vds = (vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (this(ik)%internal(8)/H) - &
                       (kss*this(ik)%internal(4)/H))/WWssik

                 !fd 
                 rltiki = -this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H*(vdt+this(ik)%internal(2)/H)
                 rlsiki = -this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H*(vds+this(ik)%internal(4)/H)              

                 sstatusik=i_stick                 

               ! compression (frottement/cohesif endo - elas)
               else
                 ! 
                 WWttik = this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*(k2*this(ik)%internal(11))*H*H))
                 WWssik = this(ik)%Wss+(1.D0/(this(ik)%internal(1)*(k2*this(ik)%internal(11))*H*H))  

                 ! calcul de la prediction It+Itcohesif 
                 rltiki = - (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(7)/H) - &
                          (ktt*this(ik)%internal(2)/H)) / WWttik
                 rlsiki = - (vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (this(ik)%internal(8)/H) - &
                          (kss*this(ik)%internal(4)/H)) / WWssik

                 rt = dsqrt(rltiki**2 + rlsiki**2)

                 ! il y a glissement 
                 if( rt > fricik*rlniki) then
                     !fd
                     vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(7)/H) - &
                           (ktt*this(ik)%internal(2)/H) + (WWttik*fricik*rlniki*rltiki/rt))/(1.d0 + ktt)
                     vds = (vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (this(ik)%internal(8)/H) - &
                           (kss*this(ik)%internal(4)/H) + (WWssik*fricik*rlniki*rlsiki/rt))/(1.d0 + kss)

                     !fd 
                     rltiki = (fricik*rlniki*rltiki/rt) -this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H*(vdt+(this(ik)%internal(2)/H))
                     rlsiki = (fricik*rlniki*rlsiki/rt) -this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H*(vds+(this(ik)%internal(4)/H))

                     sstatusik=i_slide                 
                 else
                     rltiki = rltiki - (this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H*(this(ik)%internal(2)/H))
                     rlsiki = rlsiki - (this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(10))*H*H*(this(ik)%internal(4)/H))                 
                     !maj vdt
                     vdt=0.d0
                     vds=0.d0

                     sstatusik=i_stick                                 
                 endif
               endif
            endif   
!!!-------------------------------------
         case(i_EXPO_CZM_SPRING_P,i_IQS_EXPO_CZM_SPRING_P)

            !fd il s'agit d'un montage serie elastique + czm ( contact/frottant cohesif endommageable )

            !fd - en test - resolution dans l'esprit "diago"
            !fd ainsi on resoud d'abord la partie normale, puis la tangente

            call get_czm_expo_spring_p(ibehav,cn,ct,s1,s2,G1,G2,mu_g,eta,k1,k2)
            call get_fric_CZM(ibehav,this(ik)%taz,fricik)

            ! partie normale

	    kkk = (this(ik)%internal(1)*this(ik)%taz(1)*cn*H*H)*(this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*k1*H*H)))

            ! status indetermine
            sstatusik=i_nknow  
            
            ! cas de la compression (contact - elas)
            if ( vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + (kkk*(this(ik)%internal(15)/H)) <= 0.d0 ) then

              ! seule la partie elastique travaille 
              WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*k1*H*H)) 

              !fd 
              rlniki = H*this(ik)%internal(1)*this(ik)%taz(1)*cn*this(ik)%internal(15) - &
                       (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + (kkk*(this(ik)%internal(15)/H))) / WWnnik
              
            ! cas de la traction (cohesif endo - elas)
            else

              if (this(ik)%taz(1) > 0.d0) then                  
                ! parties cohesif endo et elastiques travaillent 
                WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*k1*H*H))
                WWnnik = 1.d0 + (WWnnik*this(ik)%internal(1)*this(ik)%taz(1)*cn*H*H)                
                !fd on calcule l'allongement endo/H
                AA = (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik+ kkk*(this(ik)%internal(15)/H)) / WWnnik
                !fd l'allongement endo cumule
                AA  = H*AA                
                !fd sa partie positive 
                AA = (dabs(AA)+AA)*0.5
                ! l'impulsion cohesive
                rlniki = -this(ik)%internal(1)*this(ik)%taz(1)*cn*H*(AA-this(ik)%internal(15))
              else
                ! c'est casse ... on garde le WWnnik mais reaction nulle                 
                WWnnik = 1.d0 
                rlniki = 0.d0
                rlsiki = 0.d0
                rltiki = 0.d0                
                vdt=0.d0
                vds=0.d0
                sstatusik=i_noctc                
              endif
            endif

            if (sstatusik /= i_noctc) then
               
               ! partie tangente
               ktt = (this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H)*(this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*k2*H*H)))
               kss = (this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H)*(this(ik)%Wss+(1.D0/(this(ik)%internal(1)*k2*H*H)))
               
               ! traction (cohesif endo - elas)
               if (rlniki < 0.d0) then

                 ! cohesif endo et elastiques travaillent 
                 if (this(ik)%taz(1) > 0.d0) then                 
                   WWttik = 1.d0 + ktt
                   WWssik = 1.d0 + kss 
                 else
                   WWttik = 1.d0
                   WWssik = 1.d0                 
                 endif  

                 ! vitesse endo tangentielle
                 vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(7)/H) - &
                       (ktt*(this(ik)%internal(2)-this(ik)%internal(13))/H))/WWttik
                 vds = (vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (this(ik)%internal(8)/H) - &
                       (kss*(this(ik)%internal(4)-this(ik)%internal(14))/H))/WWssik

                 !fd 
                 rltiki = -this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vdt+(this(ik)%internal(2)-this(ik)%internal(13))/H)
                 rlsiki = -this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vds+(this(ik)%internal(4)-this(ik)%internal(14))/H)
                 
                 sstatusik=i_stick                 
                 
               ! compression (frottement/cohesif endo - elas)
               else

                 ! 
                 WWttik = this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*k2*H*H))
                 WWssik = this(ik)%Wss+(1.D0/(this(ik)%internal(1)*k2*H*H))  

                 ! calcul de la prediction It+Itcohesif 
                 rltiki = - (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(7)/H) - &
                          (ktt*(this(ik)%internal(2)-this(ik)%internal(13))/H)) / WWttik
                 rlsiki = - (vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (this(ik)%internal(8)/H) - &
                          (kss*(this(ik)%internal(4)-this(ik)%internal(14))/H)) / WWssik

                 rt = dsqrt(rltiki**2 + rlsiki**2)

                 ! il y a glissement 
                 if ( rt > fricik*rlniki) then

                    !fd
                   vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(7)/H) - &
                         (ktt*(this(ik)%internal(2)-this(ik)%internal(13))/H) + (WWttik*fricik*rlniki*rltiki/rt))/(1.d0 + ktt)
                   vds = (vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik + (this(ik)%internal(8)/H) - &
                         (kss*(this(ik)%internal(4)-this(ik)%internal(14))/H) + (WWssik*fricik*rlniki*rlsiki/rt))/(1.d0 + kss)

                   !fd 
                   rltiki = (fricik*rlniki*rltiki/rt) - &
                            this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vdt+((this(ik)%internal(2)-this(ik)%internal(13))/H))
                   rlsiki = (fricik*rlniki*rlsiki/rt) - &
                            this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vds+((this(ik)%internal(4)-this(ik)%internal(14))/H))

                   sstatusik=i_slide
                 else
                   rltiki = rltiki - (this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*((this(ik)%internal(2)-this(ik)%internal(13))/H))
                   rlsiki = rlsiki - (this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*((this(ik)%internal(4)-this(ik)%internal(14))/H))
                   !maj vdt
                   vdt=0.d0
                   vds=0.d0
                   
                   sstatusik=i_stick                                 
                 endif
               endif   
            endif

!!!-------------------------------------------
         CASE(i_ER_MAC_CZM)

               if (this(ik)%statusBEGIN >= i_Cnnow .and. this(ik)%statusBEGIN <= i_Cslid ) then
               !IF ( this(ik)%statusBEGIN(1:1) == 'C' ) THEN
             
                  CALL iter_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s,Hp)

               ELSE
                  is_cohesive = .FALSE.
                  Hradh_t = 0.d0
                  Hradh_n = 0.d0
                  Hradh_s = 0.d0
               END IF

               CALL get_fric_CZM(ibehav,this(ik)%taz,fricik)

               IF (is_cohesive) THEN

                 !fd me semble pas terrible ...
                 !fd WWttik=(this(ik)%Wss+this(ik)%Wtt)*0.5 ; WWtnik=0.d0          ; WWtsik=0.d0                              
                 !fd WWntik=0.d0                            ; WWnnik=this(ik)%Wnn  ; WWnsik=0.d0                              
                 !fd WWstik=0.d0                            ; WWsnik=0.d0          ; WWssik=(this(ik)%Wss+this(ik)%Wtt)*0.5 
                 WWttik=this(ik)%Wtt ; WWtnik=0.d0          ; WWtsik=0.d0                              
                 WWntik=0.d0         ; WWnnik=this(ik)%Wnn  ; WWnsik=0.d0                              
                 WWstik=0.d0         ; WWsnik=0.d0          ; WWssik=this(ik)%Wss

                 vvlocfreetik=vltik + this(ik)%covfreet - WWttik*rltik
                 vvlocfreenik=vlnik + this(ik)%covfreen - WWnnik*rlnik
                 vvlocfreesik=vlsik + this(ik)%covfrees - WWssik*rlsik 
  
                 ut = vvlocfreetik + (WWttik * Hradh_t) 
                 un = vvlocfreenik
                 us = vvlocfreesik + (WWssik * Hradh_s) 

                 if (vvlocfreenik > 0.d0) then   !traction ici la variable c'est g/h

                   !pour se replacer dans le cas czm on jarte la partie negative si il y a
                   un = vlnik + max(0.d0,this(ik)%covfreen) - WWnnik*rlnik

                   !print*,ik,' traction ',vvlocfreenik,un,Hradh_n,this(ik)%covfreen

                   is_traction=.TRUE.

                   Att = 1.D0 - (WWttik*k_t) ; Atn = 0.d0                ; Ats = 0.D0     
                   Ant = 0.d0                ; Ann = 1.D0 - (WWnnik*k_n) ; Ans = 0.D0     
                   Ast = 0.d0                ; Asn = 0.d0                ; Ass = 1.D0 - (WWssik*k_t) 
                  
                   invAtt =  1.d0/Att ; invAtn =  0.d0     ; invAts =  0.d0
                   invAnt =  0.d0     ; invAnn =  1.d0/Ann ; invAns =  0.d0
                   invAst =  0.d0     ; invAsn =  0.d0     ; invAss =  1.d0/Ass

                   !fd matrices diagonales qui simplifient tout !!

                   vvlocfreetik = (invAtt * ut) 
                   vvlocfreenik =                 (invAnn * un)
                   vvlocfreesik =                                (invAss *us)

                   Att = (invAtt * WWttik) ; Atn = 0.d0              ; Ats = 0.d0
                   Ant = 0.d0              ; Ann = (invAnn * WWnnik) ; Ans = 0.d0
                   Ast = 0.d0              ; Asn = 0.d0              ; Ass = (invAss * WWssik)
                  
                   WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                   WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                   WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 

                 else   !compression

                   !print*,ik,' compression ',un,Hradh_n
 
                   is_traction=.FALSE.

                   if (Hradh_n > 0.d0) print*,'Humm unexpected situation for ER_CZM'

                   Att = 1.D0 - (WWttik*k_t) ; Atn = 0.d0                ; Ats = 0.D0     
                   Ant = 0.d0                ; Ann = 1.D0                ; Ans = 0.D0     
                   Ast = 0.d0                ; Asn = 0.d0                ; Ass = 1.D0 - (WWssik*k_t) 
                  
                   invAtt =  1.d0/Att ; invAtn =  0.d0     ; invAts =  0.d0
                   invAnt =  0.d0     ; invAnn =  1.d0/Ann ; invAns =  0.d0
                   invAst =  0.d0     ; invAsn =  0.d0     ; invAss =  1.d0/Ass

                   !fd matrices diagonales qui simplifient tout !!

                   vvlocfreetik = (invAtt * ut) 
                   vvlocfreenik =                 (invAnn * un)
                   vvlocfreesik =                                (invAss *us)

                   Att = (invAtt * WWttik) ; Atn = 0.d0              ; Ats = 0.d0
                   Ant = 0.d0              ; Ann = (invAnn * WWnnik) ; Ans = 0.d0
                   Ast = 0.d0              ; Asn = 0.d0              ; Ass = (invAss * WWssik)

                   WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                   WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                   WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 

                   CALL get_forcePERgap(ibehav,forcePERgap)
                   !fd lourenco
                   !forcePERgap=274.D+10                    
                   if (this(ik)%internal(1) /=0.d0) then
                     WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*forcePERgap*H*H))
                   else
                     WWnnik = WWnnik + (1.D0/(forcePERgap*H*H))
                   endif

                 endif
                  
                 if (vvlocfreenik >= 0.d0) then

                   sstatusik=i_noctc
                   rlniki   = 0.0
                   rltiki   = 0.0
                   rlsiki   = 0.0

                 else

                   rltiki   = - (vvlocfreetik/WWttik)
                   rlniki   = - (vvlocfreenik/WWnnik)
                   rlsiki   = - (vvlocfreesik/WWssik)
                  
                   FFN = fricik*rlniki
                   DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                     
                   IF (DET.LT.FFN) THEN
                     sstatusik=i_stick
                   ELSE
                     sstatusik=i_slide
                     IF (DET .GT. 1D-18) THEN
                       rltiki = rltiki*FFN/DET
                       rlsiki = rlsiki*FFN/DET
                     END IF
                   END IF
                 endif
               ELSE  ! plus cohesif

                 if (this(ik)%statusBEGIN == i_vnish) then
                   rltiki = 0.d0
                   rlniki = 0.d0
                   rlsiki = 0.d0
                   sstatusik=i_vnish
                 else
                   vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%Wtt*rltik
                   vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik
                   vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik

                   IF (vvlocfreenik >= 0.0)THEN

                     sstatusik=i_noctc
                     rlniki   = 0.0
                     rltiki   = 0.0
                     rlsiki   = 0.0
                      
                  ELSE
                     CALL get_forcePERgap(ibehav,forcePERgap)
                     !fd lourenco
                     !forcePERgap=274.D+10                    
                     if (this(ik)%internal(1) /=0.d0) then
                       WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*forcePERgap*H*H))
                     else
                       WWnnik = this(ik)%Wnn+(1.D0/(forcePERgap*H*H))                
                     endif                   
                
                     rltiki   =  - (this(ik)%invWtt*vvlocfreetik)
                     rlniki   =  - (vvlocfreenik/WWnnik)
                     rlsiki   =  - (this(ik)%invWss*vvlocfreesik)
                     
                     FFN = fricik*rlniki
                     DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                       
                     IF (DET.LT.FFN) THEN
                       sstatusik=i_stick
                     ELSE
                       sstatusik=i_slide
                       IF (DET .GT. 1D-18) THEN
                         rltiki = rltiki*FFN/DET
                         rlsiki = rlsiki*FFN/DET
                       END IF
                     END IF
                   END IF
                 END IF
               endif

               ! lourenco cap
               if (rlniki > H*this(ik)%internal(1)*get_RNcap()) then
                 rltiki = 0.d0
                 rlniki = 0.d0
                 rlsiki = 0.d0
                 sstatusik=i_vnish
               endif

            !!!-------------------------------------------
!!$            CASE( i_VISCO_ELASTIC_REPELL_CLB)
!!$               !fd me semble faux ...
!!$               !fd vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%WWtt*rltik
!!$               !fd vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
!!$               !fd vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%WWss*rlsik
!!$
!!$               vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%Wtt*rltik
!!$               vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik
!!$               vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik
!!$
!!$
!!$               !print*,'contact: ' ,ik
!!$
!!$               !print*,'Wnn'
!!$               !print*,this(ik)%Wnn,this(ik)%WWnn
!!$
!!$               !print*,'W'
!!$               !print*,this(ik)%Wtt,this(ik)%Wtn,this(ik)%Wts
!!$               !print*,this(ik)%Wnt,this(ik)%Wnn,this(ik)%Wns
!!$               !print*,this(ik)%Wst,this(ik)%Wsn,this(ik)%Wss
!!$               !print*,'vfree + WabRb' 
!!$               !print*,vltik,vlnik,vlsik
!!$               !print*,'cov v'
!!$               !print*,this(ik)%covfreet,this(ik)%covfreen,this(ik)%covfrees
!!$               !print*,'vloc free'
!!$               !print*,vvlocfreetik,vvlocfreenik,vvlocfreesik 
!!$
!!$               IF (vvlocfreenik .GE. 0.0)THEN
!!$                  sstatusik=i_noctc
!!$                  rlniki   = 0.0
!!$                  rltiki   = 0.0
!!$                  rlsiki   = 0.0
!!$               ELSE
!!$
!!$
!!$                  rlniki   = - (this(ik)%invWnn*vvlocfreenik)
!!$
!!$                  FFN = fricik*(rlniki+this(ik)%corln)
!!$
!!$                  IF (FFN .LE. 0.0)THEN
!!$                    sstatusik=i_noctc
!!$                    rlniki   = 0.0
!!$                    rltiki   = 0.0
!!$                    rlsiki   = 0.0
!!$                  else
!!$                    rltiki   = - (this(ik)%invWtt*vvlocfreetik)
!!$                    rlsiki   = - (this(ik)%invWss*vvlocfreesik)
!!$
!!$                    DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
!!$
!!$                    IF (DET.LT.FFN) THEN
!!$                     sstatusik=i_stick
!!$                    ELSE
!!$                      sstatusik=i_slide
!!$                      IF (DET .GT. 1D-18) THEN
!!$                        rltiki = rltiki*FFN/DET
!!$                        rlsiki = rlsiki*FFN/DET
!!$                      END IF
!!$                    END IF
!!$                  endif
!!$               ENDIF
!!$               !print*,'reactions'
!!$               !print*,rltiki,rlniki,rlsiki 



            CASE(i_NARD_ROD)
          
               IF (this(ik)%statusBEGIN == i_Cstck) THEN
                 is_cohesive = .true.
               else
                 is_cohesive = .false.
               end if         

               if (is_cohesive) then

                 CALL get_nard_coeff(ibehav,this(ik)%internal,Ksn,Kst,Kvn,Kvt)
   

                 ! raideurs identiques en tangent/shear

                 kss = kst
                 kvs = kvt

                 ! ********************

                 WWttik=(this(ik)%Wss+this(ik)%Wtt)*0.5 ; WWtnik=0.d0          ; WWtsik=0.d0                              
                 WWntik=0.d0                            ; WWnnik=this(ik)%Wnn  ; WWnsik=0.d0                              
                 WWstik=0.d0                            ; WWsnik=0.d0          ; WWssik=(this(ik)%Wss+this(ik)%Wtt)*0.5 

                 vvlocfreetik=vltik + this(ik)%covfreet - WWttik*rltik
                 vvlocfreenik=vlnik + this(ik)%covfreen - WWnnik*rlnik
                 vvlocfreesik=vlsik + this(ik)%covfrees - WWssik*rlsik

                 Att = 1.D0 + (WWttik*H*(Kst*H+Kvt)) ; Atn = 0.d0   ; Ats = 0.D0     
                 Ant = 0.d0  ; Ann = 1.D0 + (WWnnik*H*(Ksn*H+Kvn)) ; Ans = 0.D0     
                 Ast = 0.d0  ; Asn = 0.d0  ; Ass = 1.D0 + (WWssik*H*(Kss*H+Kvs)) 
                  
                 invAtt =  1.d0/Att ; invAtn =  0.d0     ; invAts =  0.d0
                 invAnt =  0.d0     ; invAnn =  1.d0/Ann ; invAns =  0.d0
                 invAst =  0.d0     ; invAsn =  0.d0     ; invAss =  1.d0/Ass

                 !fd matrices diagonales qui simplifient tout !!

                 vvlocfreetik = (invAtt * vvlocfreetik) 
                 vvlocfreenik =                (invAnn * vvlocfreenik)
                 vvlocfreesik =                               (invAss *vvlocfreesik)

                 Att = (invAtt * WWttik) 
                 Atn = 0.d0
                 Ats = 0.d0
                 Ant = 0.d0
                 Ann = (invAnn * WWnnik)
                 Ans = 0.d0
                 Ast = 0.d0
                 Asn = 0.d0
                 Ass = (invAss * WWssik)
                  
                 WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                 WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                 WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 

                 rltiki   = - vvlocfreetik/WWttik
                 rlniki   = - vvlocfreenik/WWnnik
                 rlsiki   = - vvlocfreesik/WWssik

                 IF (rlniki<=0.D0) THEN
                   sstatusik=i_noctc
                   rltiki=0.D0
                   rlniki=0.D0
                   rlsiki=0.D0
                 ELSE
                   FFN = fricik*rlniki
                   DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                     
                   IF (DET.LT.FFN) THEN
                     sstatusik=i_stick
                   ELSE
                     sstatusik=i_slide
                     IF (DET .GT. 1D-18) THEN
                       rltiki = rltiki*FFN/DET
                       rlsiki = rlsiki*FFN/DET
                     END IF
                   END IF
                 END IF

               ELSE  ! plus cohesif

                  vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%Wtt*rltik
                  vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik
                  vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik

                  IF (vvlocfreenik >= 0.0)THEN

                     sstatusik=i_noctc
                     rlniki   = 0.0
                     rltiki   = 0.0
                     rlsiki   = 0.0
                     
                  ELSE
                     
                     rltiki   =  - (this(ik)%invWtt*vvlocfreetik)
                     rlniki   =  - (this(ik)%invWnn*vvlocfreenik)
                     rlsiki   =  - (this(ik)%invWss*vvlocfreesik)
                     
                     FFN = fricik*rlniki
                     DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)
                        
                     IF (DET.LT.FFN) THEN
                       sstatusik=i_stick
                     ELSE
                       sstatusik=i_slide
                       IF (DET .GT. 1D-18) THEN
                         rltiki = rltiki*FFN/DET
                         rlsiki = rlsiki*FFN/DET
                       END IF
                     END IF
                  END IF
               END IF

            !!!-------------------------------------------             
            CASE default

               !fd me semble faux ...
               !vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%WWtt*rltik
               !vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%WWnn*rlnik
               !vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%WWss*rlsik
               !fd ... me semble juste
               vvlocfreetik=vltik + this(ik)%covfreet - this(ik)%Wtt*rltik
               vvlocfreenik=vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik
               vvlocfreesik=vlsik + this(ik)%covfrees - this(ik)%Wss*rlsik

               !write(*,'(I0,4(1x,D12.5))') this(ik)%icdan,vlnik,this(ik)%covfreen,-this(ik)%Wnn*rlnik,vvlocfreenik
               
               !write(*,'(I0,1x,I0,4(1x,D12.5))') this(ik)%icdan,this(ik)%i_law, &
               !   vvlocfreenik,this(ik)%invWtt,this(ik)%invWnn,this(ik)%invWss


               IF (vvlocfreenik .GE. 0.d0)THEN
                  if (vvlocfreenik .GT. 0.d0) then 
                     sstatusik=i_noctc
                  else
                     sstatusik=i_stick
                  endif   
                  rlniki   = 0.d0
                  rltiki   = 0.d0
                  rlsiki   = 0.d0
               ELSE

                  rltiki   = - (this(ik)%invWtt*vvlocfreetik)
                  rlniki   = - (this(ik)%invWnn*vvlocfreenik)
                  rlsiki   = - (this(ik)%invWss*vvlocfreesik)

                  FFN = fricik*rlniki
                  DET = SQRT(rltiki*rltiki+rlsiki*rlsiki)

                  IF (DET.LT.FFN) THEN
                    sstatusik=i_stick
                  ELSE
                    sstatusik=i_slide
                    IF (DET .GT. 1D-18) THEN
                      rltiki = rltiki*FFN/DET
                      rlsiki = rlsiki*FFN/DET
                    END IF
                  END IF
               END IF

               !write(*,'(I0,2(1x,D12.5),1x,A)') this(ik)%icdan,this(ik)%covfreen,vvlocfreenik,sstatusik
  
            !!!-------------------------------------------
            END SELECT
      
         ELSE  ! pas diagonal_resolution ; toutes les lois ne peuvent pas passer ici
         !--------------------------------------------------------------
         ! standard resolution  standard resolution  standard resolution 
         !-------------------------------------------------------------- 

            !!!--------------------------------------
            SELECT CASE(ilaw)
               !123456789012345678901234567890

            !!!--------------------------------------
            CASE(i_KV_WET)
               vvlocfreenik = -vvlocfreenik
               vvlocfreetik = -vvlocfreetik
               vvlocfreesik = -vvlocfreesik
               
               CALL mu_NG_solver_(ik,WWnnik,WWttik,WWssik,WWstik,WWtsik,WWnsik,WWsnik,WWtnik,WWntik,  &
                                  vvlocfreenik,vvlocfreetik,vvlocfreesik,rlnik,rltik,rlsik,fricik, &
                                  Wrlnik,Wrltik,Wrlsik,rlniki,rltiki,rlsiki,sstatusik)
            
            !!!-------------------------------------------
            CASE(i_ELASTIC_ROD,i_VOIGT_ROD,i_NORMAL_COUPLED_DOF)

               ! default
               !rlsiki = 0.D0
               !rltiki = 0.D0

               rlniki = -vvlocfreenik/WWnnik

               vlniki = 0.D0
               vltiki= vvlocfreetik + WWtnik*rlniki
               vlsiki= vvlocfreesik + WWsnik*rlniki

               sstatusik=i_stick

            !!!-------------------------------------------
            CASE(i_RIGID_WIRE,i_ELASTIC_WIRE,i_VOIGT_WIRE)
               ! default 
               !rlsiki = 0.D0
               !rltiki = 0.D0
               !vltiki = 0.D0
               !vlsiki = 0.D0

               IF (vvlocfreenik .GT. 0.D0) THEN
                  rlniki = -vvlocfreenik/WWnnik 
                  vlniki = -this(ik)%covfreen
                  sstatusik=i_stick
               ELSE
                  rlniki = 0.D0
                  vlniki  = vvlocfreenik - this(ik)%covfreen
                  sstatusik=i_vnish
               ENDIF

            !!!-------------------------------------------
            CASE(i_BRITTLE_ELASTIC_WIRE)

               ! default
               !rlsiki = 0.D0
               !rltiki = 0.D0
               !vltiki = 0.D0
               !vlsiki = 0.D0
               IF (vvlocfreenik .GT. 0.D0) THEN

                  rlniki = -vvlocfreenik/WWnnik 
                  vlniki = -this(ik)%covfreen

                  call get_snmax(ibehav,snmax)
                  if (rlniki < -H*snmax) then
                    sstatusik=i_vnish
                    rlniki = 0.d0
                    vlniki  = vvlocfreenik - this(ik)%covfreen
                  else
                    sstatusik=i_stick
                  endif
               ELSE
                  rlniki = 0.D0
                  vlniki  = vvlocfreenik - this(ik)%covfreen
                  sstatusik=i_noctc
               ENDIF

            !!!-------------------------------------------
            CASE(i_MAC_CZM,i_MP_CZM,i_MP3_CZM,i_MAL_CZM,i_TH_CZM,i_ABP_CZM,i_EXPO_CZM, &
                 i_IQS_MAC_CZM,i_IQS_MAL_CZM,i_IQS_TH_CZM,i_IQS_ABP_CZM,i_IQS_EXPO_CZM, &
                 i_TOSI_CZM,i_TOSI_CZM_INCRE)
               
               ! if (this(ik)%statusBEGIN >= i_Cnnow .and. this(ik)%statusBEGIN <= i_Cslid ) then
               ! !IF (this(ik)%statusBEGIN(1:1) == 'C') THEN 
                  
               CALL iter_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s,Hp)
                  
               ! ELSE
               !    is_cohesive = .FALSE.
               !    Hradh_t=0.d0
               !    Hradh_n=0.d0
               !    Hradh_s=0.d0
               ! ENDIF
               
               CALL get_fric_CZM(ibehav,this(ik)%taz,fricik)

               IF (is_cohesive) THEN
                  
                  Att = 1.D0 - (WWttik*k_t) ; Atn =      -  WWtnik*k_n  ; Ats =      -  WWtsik*k_t   
                  Ant =      -  WWntik*k_t  ; Ann = 1.D0 - (WWnnik*k_n) ; Ans =      -  WWnsik*k_t
                  Ast =      -  WWstik*k_t  ; Asn =      -  WWsnik*k_n  ; Ass = 1.D0 - (WWssik*k_t) 
                  
                  inv_detA = 1.d0/((Att*Ann*Ass + Ant*Asn*Ats + Ast*Atn*Ans) - (Ast*Ann*Ats + Asn*Ans*Att + Ass*Ant*Atn))
                  
                  invAtt =  inv_detA*(Ann*Ass-Asn*Ans) ; invAtn = -inv_detA*(Atn*Ass-Asn*Ats) ; invAts =  inv_detA*(Atn*Ans-Ann*Ats)
                  invAnt = -inv_detA*(Ant*Ass-Ast*Ans) ; invAnn =  inv_detA*(Att*Ass-Ast*Ats) ; invAns = -inv_detA*(Att*Ans-Ant*Ats)
                  invAst =  inv_detA*(Ant*Asn-Ast*Ann) ; invAsn = -inv_detA*(Att*Asn-Ast*Atn) ; invAss =  inv_detA*(Att*Ann-Ant*Atn)
                  
                  ut = vvlocfreetik + (WWttik * Hradh_t) + (WWtsik * Hradh_s) 
                  un = vvlocfreenik + (WWntik * Hradh_t) + (WWnsik * Hradh_s) 
                  us = vvlocfreesik + (WWstik * Hradh_t) + (WWssik * Hradh_s) 
                  
                  vvlocfreetik = (invAtt * ut) + (invAtn * un) + (invAts *us)
                  vvlocfreenik = (invAnt * ut) + (invAnn * un) + (invAns *us)
                  vvlocfreesik = (invAst * ut) + (invAsn * un) + (invAss *us)
                  
                  !             print*,'mod vvlocfreex',vvlocfreetik,vvlocfreenik,vvlocfreesik
                  
                  Att = (invAtt * WWttik) + (invAtn * WWntik) + (invAts*WWstik)           
                  Atn = (invAtt * WWtnik) + (invAtn * WWnnik) + (invAts*WWsnik)           
                  Ats = (invAtt * WWtsik) + (invAtn * WWnsik) + (invAts*WWssik)           
                  
                  Ant = (invAnt * WWttik) + (invAnn * WWntik) + (invAns*WWstik)
                  Ann = (invAnt * WWtnik) + (invAnn * WWnnik) + (invAns*WWsnik)            
                  Ans = (invAnt * WWtsik) + (invAnn * WWnsik) + (invAns*WWssik)
                  
                  Ast = (invAst * WWttik) + (invAsn * WWntik) + (invAss*WWstik)
                  Asn = (invAst * WWtnik) + (invAsn * WWnnik) + (invAss*WWsnik)            
                  Ass = (invAst * WWtsik) + (invAsn * WWnsik) + (invAss*WWssik)
                  
                  WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                  WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                  WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 
                  
                  !             print*,WWttik, WWtnik, WWtsik 
                  !             print*,WWntik, WWnnik, WWnsik
                  !             print*,WWstik, WWsnik, WWssik 
                  
               END IF   
               CALL mu_NG_solver_(ik,WWnnik,WWttik,WWssik,WWstik,WWtsik,WWnsik,WWsnik,WWtnik,WWntik,  &
                                  vvlocfreenik,vvlocfreetik,vvlocfreesik,rlnik,rltik,rlsik,fricik, &
                                  Wrlnik,Wrltik,Wrlsik,rlniki,rltiki,rlsiki,sstatusik)

            !!!-------------------------------------------               
            CASE(i_EXPO_CZM_P,i_IQS_EXPO_CZM_P)
               
               ! if (this(ik)%statusBEGIN >= i_Cnnow .and. this(ik)%statusBEGIN <= i_Cslid ) then
               ! !IF (this(ik)%statusBEGIN(1:1) == 'C') THEN 
                  
               CALL iter_CZM_3D(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s,Hp)
                                 
               CALL get_fric_CZM(ibehav,this(ik)%taz,fricik)

               IF (is_cohesive) THEN
                  
                  Att = 1.D0 - (WWttik*k_t) ; Atn =      -  WWtnik*k_n  ; Ats =      -  WWtsik*k_t   
                  Ant =      -  WWntik*k_t  ; Ann = 1.D0 - (WWnnik*k_n) ; Ans =      -  WWnsik*k_t
                  Ast =      -  WWstik*k_t  ; Asn =      -  WWsnik*k_n  ; Ass = 1.D0 - (WWssik*k_t) 
                  
                  inv_detA = 1.d0/((Att*Ann*Ass + Ant*Asn*Ats + Ast*Atn*Ans) - (Ast*Ann*Ats + Asn*Ans*Att + Ass*Ant*Atn))
                  
                  invAtt =  inv_detA*(Ann*Ass-Asn*Ans) ; invAtn = -inv_detA*(Atn*Ass-Asn*Ats) ; invAts =  inv_detA*(Atn*Ans-Ann*Ats)
                  invAnt = -inv_detA*(Ant*Ass-Ast*Ans) ; invAnn =  inv_detA*(Att*Ass-Ast*Ats) ; invAns = -inv_detA*(Att*Ans-Ant*Ats)
                  invAst =  inv_detA*(Ant*Asn-Ast*Ann) ; invAsn = -inv_detA*(Att*Asn-Ast*Atn) ; invAss =  inv_detA*(Att*Ann-Ant*Atn)
                  
                  ut = vvlocfreetik + (WWtnik * Hradh_n) + (WWttik * Hradh_t) + (WWtsik * Hradh_s) 
                  un = vvlocfreenik + (WWnnik * Hradh_n) + (WWntik * Hradh_t) + (WWnsik * Hradh_s) 
                  us = vvlocfreesik + (WWsnik * Hradh_n) + (WWstik * Hradh_t) + (WWssik * Hradh_s) 
                  
                  vvlocfreetik = (invAtt * ut) + (invAtn * un) + (invAts *us)
                  vvlocfreenik = (invAnt * ut) + (invAnn * un) + (invAns *us)
                  vvlocfreesik = (invAst * ut) + (invAsn * un) + (invAss *us)
                  
                  
                  Att = (invAtt * WWttik) + (invAtn * WWntik) + (invAts*WWstik)           
                  Atn = (invAtt * WWtnik) + (invAtn * WWnnik) + (invAts*WWsnik)           
                  Ats = (invAtt * WWtsik) + (invAtn * WWnsik) + (invAts*WWssik)           
                  
                  Ant = (invAnt * WWttik) + (invAnn * WWntik) + (invAns*WWstik)
                  Ann = (invAnt * WWtnik) + (invAnn * WWnnik) + (invAns*WWsnik)            
                  Ans = (invAnt * WWtsik) + (invAnn * WWnsik) + (invAns*WWssik)
                  
                  Ast = (invAst * WWttik) + (invAsn * WWntik) + (invAss*WWstik)
                  Asn = (invAst * WWtnik) + (invAsn * WWnnik) + (invAss*WWsnik)            
                  Ass = (invAst * WWtsik) + (invAsn * WWnsik) + (invAss*WWssik)
                  
                  WWttik = Att ; WWtnik = Atn ; WWtsik = Ats 
                  WWntik = Ant ; WWnnik = Ann ; WWnsik = Ans 
                  WWstik = Ast ; WWsnik = Asn ; WWssik = Ass 
                                    
               END IF   
               CALL mu_NG_solver_(ik,WWnnik,WWttik,WWssik,WWstik,WWtsik,WWnsik,WWsnik,WWtnik,WWntik,  &
                                  vvlocfreenik,vvlocfreetik,vvlocfreesik,rlnik,rltik,rlsik,fricik, &
                                  Wrlnik,Wrltik,Wrlsik,rlniki,rltiki,rlsiki,sstatusik) 

            !!!--------------------------------------
            CASE(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING,i_EXPO_CZM_SPRING_P,i_IQS_EXPO_CZM_SPRING_P)

              !fd a mettre au propre 
               
              call faterr(IAM,'Needs diagonal resolution') 
               
               
            !!!--------------------------------------
            CASE(i_COUPLED_DOF)
            
              CALL coupled_dof_solver_(WWttik,WWtsik,WWtnik,WWstik,WWssik,WWsnik,WWntik,WWnsik,WWnnik,&
                                       vvlocfreetik,vvlocfreesik,vvlocfreenik, &
                                       sstatusik,rltiki,rlsiki,rlniki)


            !!!--------------------------------------
           CASE(i_NARD_ROD)
              IF (this(ik)%statusBEGIN == i_Cstck) THEN
                  is_cohesive = .true.
              ELSE
                  is_cohesive = .false.
              END IF         

              IF (is_cohesive) THEN

                CALL get_nard_coeff(ibehav,this(ik)%internal,Ksn,Kst,Kvn,Kvt)
                
                ! raideurs identiques en tangent/shear

                kss = kst
                kvs = kvt


                ! ********************
                !calcul de la matrice T-1 =A


                !****  changer les lettres pour utiliser les variables deja declarer ....

                Tnn=1.d0+this(ik)%Wnn*H*(Ksn*H+Kvn)
                Tnt=this(ik)%Wnt*H*(Kst*H+Kvt)
                Tns=this(ik)%Wns*H*(Kss*H+Kvs)
    
                Ttn=this(ik)%Wtn*H*(Ksn*H+Kvn)
                Ttt=1.d0+this(ik)%Wtt*H*(Kst*H+Kvt)
                Tts=this(ik)%Wts*H*(Kss*H+Kvs)

                Tsn=this(ik)%Wsn*H*(Ksn*H+Kvn)
                Tst=this(ik)%Wst*H*(Kst*H+Kvt)
                Tss=1.d0+this(ik)%Wss*H*(Kss*H+Kvs)


                inv_detT = 1.d0/((Ttt*Tnn*Tss + Tnt*Tsn*Tts + Tst*Ttn*Tns) - (Tst*Tnn*Tts + Tsn*Tns*Ttt + Tss*Tnt*Ttn))
 
                Att =  inv_detT*(Tnn*Tss-Tsn*Tns) ; Atn = -inv_detT*(Ttn*Tss-Tsn*Tts) ; Ats =  inv_detT*(Ttn*Tns-Tnn*Tts)
                Ant = -inv_detT*(Tnt*Tss-Tst*Tns) ; Ann =  inv_detT*(Ttt*Tss-Tst*Tts) ; Ans = -inv_detT*(Ttt*Tns-Tnt*Tts)
                Ast =  inv_detT*(Tnt*Tsn-Tst*Tnn) ; Asn = -inv_detT*(Ttt*Tsn-Tst*Ttn) ; Ass =  inv_detT*(Ttt*Tnn-Tnt*Ttn)
	
                un=vvlocfreenik*Ann+vvlocfreetik*Ant+vvlocfreesik*Ans
                ut=vvlocfreenik*Atn+vvlocfreetik*Att+vvlocfreesik*Ats
                us=vvlocfreenik*Asn+vvlocfreetik*Ast+vvlocfreesik*Ass
  
                vvlocfreenik=un
                vvlocfreetik=ut
                vvlocfreesik=us
            
                !T -> G-1*W
                Ttt = (Att * WWttik) + (Atn * WWntik) + (Ats * WWstik)       
                Ttn = (Att * WWtnik) + (Atn * WWnnik) + (Ats * WWsnik)
                Tts = (Att * WWtsik) + (Atn * WWnsik) + (Ats * WWssik)

                Tnt = (Ant * WWttik) + (Ann * WWntik) + (Ans * WWstik)
                Tnn = (Ant * WWtnik) + (Ann * WWnnik) + (Ans * WWsnik)  
                Tns = (Ant * WWtsik) + (Ann * WWnsik) + (Ans * WWssik)  

                Tst = (Ast * WWttik) + (Asn * WWntik) + (Ass * WWstik)  
                Tsn = (Ast * WWtnik) + (Asn * WWnnik) + (Ass * WWsnik)  
                Tss = (Ast * WWtsik) + (Asn * WWnsik) + (Ass * WWssik) 

               
                WWttik = Ttt; WWtnik = Ttn; WWtsik = Tts
                WWntik = Tnt; WWnnik = Tnn; WWnsik = Tns
                WWstik = Tst; WWsnik = Tsn; WWssik = Tss
               
              END IF

              CALL mu_NG_solver_(ik,WWnnik,WWttik,WWssik,WWstik,WWtsik,WWnsik,WWsnik,WWtnik,WWntik,  &
                                vvlocfreenik,vvlocfreetik,vvlocfreesik,rlnik,rltik,rlsik,fricik, &
                                Wrlnik,Wrltik,Wrlsik,rlniki,rltiki,rlsiki,sstatusik)
              
            !!!--------------------------------------
            CASE default
     
               CALL mu_NG_solver_(ik,WWnnik,WWttik,WWssik,WWstik,WWtsik,WWnsik,WWsnik,WWtnik,WWntik,  &
                                  vvlocfreenik,vvlocfreetik,vvlocfreesik,rlnik,rltik,rlsik,fricik, &
                                  Wrlnik,Wrltik,Wrlsik,rlniki,rltiki,rlsiki,sstatusik)

            !!!--------------------------------------               
            END SELECT

         END IF  ! IF (diagonal_resolution) THEN

         !---------------------------------------
         ! Restore to Genuine  Restore to Genuine
         !---------------------------------------

         !!!--------------------------------------
         SELECT CASE(ilaw)

         !!!--------------------------------------
!!$         CASE(i_VISCO_ELASTIC_REPELL_CLB)
!!$           IF (sstatusik /= i_noctc) rlniki=rlniki+this(ik)%corln

         !!!--------------------------------------
         CASE(i_IQS_WET_DS_CLB, i_ELASTIC_REPELL_WET_CLB,&
              i_xQS_WET_DS_CLB, i_GAP_WET_DS_CLB, i_RST_WET_CLB)
            if (this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslid ) then
            !IF (this(ik)%statusBEGIN(1:1) == 'W') THEN
               rlniki=rlniki+this(ik)%corln
               rltiki=rltiki+this(ik)%corlt
               rlsiki=rlsiki+this(ik)%corls
               IF (sstatusik == i_noctc) sstatusik=i_Wnctc
               IF (sstatusik == i_stick) sstatusik=i_Wstck
               IF (sstatusik == i_slide) sstatusik=i_Wslid
            END IF

         !!!--------------------------------------
         CASE(i_VISCO_ELASTIC_REPELL_WET)
            if (this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslid ) then
            !IF (this(ik)%statusBEGIN(1:1) == 'W') THEN
               rlniki=rlniki+this(ik)%corln
               rltiki=rltiki+this(ik)%corlt
               rlsiki=rlsiki+this(ik)%corls
               IF (sstatusik == i_noctc) sstatusik=i_Wnctc
               IF (sstatusik == i_stick) sstatusik=i_Wstck
               IF (sstatusik == i_slide) sstatusik=i_Wslid
            END IF

         !!!--------------------------------------
         CASE(i_MAC_CZM,i_MAL_CZM,i_TH_CZM,i_ABP_CZM,i_EXPO_CZM,i_MP_CZM,i_MP3_CZM, &
              i_IQS_MAC_CZM,i_IQS_MAL_CZM,i_IQS_TH_CZM,i_IQS_ABP_CZM,i_IQS_EXPO_CZM, &
              i_TOSI_CZM,i_TOSI_CZM_INCRE)

            vt = 0.D0 ; vn = 0.D0 ; vs = 0.D0
            
            IF (diagonal_resolution) THEN
               vt = vvlocfreetik + WWttik*rltiki
               vn = vvlocfreenik + WWnnik*rlniki - this(ik)%covfreen
               vs = vvlocfreesik + WWssik*rlsiki  
            ELSE
               vt = vvlocfreetik + WWtsik*rlsiki + WWttik*rltiki + WWtnik*rlniki 
               vn = vvlocfreenik + WWnsik*rlsiki + WWntik*rltiki + WWnnik*rlniki - this(ik)%covfreen
               vs = vvlocfreesik + WWssik*rlsiki + WWstik*rltiki + WWsnik*rlniki 
            END IF

            Hradh_t = Hradh_t + (k_t * vt)
            Hradh_n = Hradh_n + (k_n * vn)
            Hradh_s = Hradh_s + (k_t * vs)
               
            rltiki = rltiki + Hradh_t 
            rlniki = rlniki + Hradh_n + Hp
            rlsiki = rlsiki + Hradh_s

            !fd correction du statut qui est faux lorsqu'on est en traction
            if (sstatusik == i_noctc .and. rlniki /= 0.d0 ) sstatusik=i_stick

            !fd
            IF (i_what == i_check) THEN 

               CALL updt_CZM_3D(ibehav,.FALSE.,this(ik)%internal,this(ik)%taz,vt,vn,vs)

            endif
            
            IF (i_what == i_post) THEN 

               CALL updt_CZM_3D(ibehav,.FALSE.,this(ik)%internal,this(ik)%taz,vt,vn,vs)

               !fr: why do we deal only these particular interaction cases and not every cases?
               if ( ( this(ik)%CDAN == i_SPPLx ) .or. &
                    ( this(ik)%CDAN == i_SPPRx ) .or. &
                    ( this(ik)%CDAN == i_PRPRx ) .or. &
                    ( this(ik)%CDAN == i_CSASp ) ) THEN

                  call set_internal( this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

               end if

            END IF
               
            IF (is_cohesive) THEN 
               
               IF (sstatusik == i_noctc) sstatusik=i_Cnctc
               IF (sstatusik == i_stick) sstatusik=i_Cstck
               IF (sstatusik == i_slide) sstatusik=i_Cslid

            END IF
			
         CASE(i_EXPO_CZM_P,i_IQS_EXPO_CZM_P)

            vt = 0.D0 ; vn = 0.D0 ; vs = 0.D0
               
            IF (diagonal_resolution) THEN
               vt = vvlocfreetik + WWttik*rltiki
               vn = vvlocfreenik + WWnnik*rlniki - this(ik)%covfreen
               vs = vvlocfreesik + WWssik*rlsiki  
            ELSE
               vt = vvlocfreetik + WWtsik*rlsiki + WWttik*rltiki + WWtnik*rlniki 
               vn = vvlocfreenik + WWnsik*rlsiki + WWntik*rltiki + WWnnik*rlniki - this(ik)%covfreen
               vs = vvlocfreesik + WWssik*rlsiki + WWstik*rltiki + WWsnik*rlniki 
            END IF

            Hradh_t = Hradh_t + (k_t * vt)
            Hradh_n = Hradh_n + (k_n * (vn + (this(ik)%internal(3)/H))) 
            Hradh_s = Hradh_s + (k_t * vs)
               

            rltiki = rltiki + Hradh_t 
            rlniki = rlniki + Hradh_n + Hp
            rlsiki = rlsiki + Hradh_s

            !fd
            IF (i_what == i_check) THEN 

               CALL updt_CZM_3D(ibehav,.FALSE.,this(ik)%internal,this(ik)%taz,vt,vn,vs)

            endif
            
            IF (i_what == i_post) THEN 

               CALL updt_CZM_3D(ibehav,.FALSE.,this(ik)%internal,this(ik)%taz,vt,vn,vs)

               !fr: why do we deal only these particular interaction cases and not every cases?
               if ( ( this(ik)%CDAN == i_SPPLx ) .or. &
                    ( this(ik)%CDAN == i_PRPRx ) .or. &
                    ( this(ik)%CDAN == i_CSASp ) ) THEN

                  call set_internal( this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

               end if

            END IF
               
            IF (is_cohesive) THEN 
               
               IF (sstatusik == i_noctc) sstatusik=i_Cnctc
               IF (sstatusik == i_stick) sstatusik=i_Cstck
               IF (sstatusik == i_slide) sstatusik=i_Cslid

            END IF

         !-------------------------------------------------------------------------
         case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)


            ! mise a jour de taz
            if (i_what == i_check) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki) + (this(ik)%Wts*rlsiki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki) + (this(ik)%Wns*rlsiki)
              vs = vlocfreesik + (this(ik)%Wst*rltiki) + (this(ik)%Wsn*rlniki) + (this(ik)%Wss*rlsiki)
              
              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring_3D(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt, &
                                      (this(ik)%gapTTbegin-this(ik)%internal(9))+H*vn,H*vs,rltiki,rlniki,rlsiki)
            endif 

            
            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_post) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki) + (this(ik)%Wts*rlsiki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki) + (this(ik)%Wns*rlsiki)
              vs = vlocfreesik + (this(ik)%Wst*rltiki) + (this(ik)%Wsn*rlniki) + (this(ik)%Wss*rlsiki)
              
              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring_3D(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt, &
                                      (this(ik)%gapTTbegin-this(ik)%internal(9))+H*vn,H*vs,rltiki,rlniki,rlsiki)
              call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )
            endif

            !fd mise a jour des status            
            if (is_cohesive) then                
               if (sstatusik == i_noctc) then
                  !fd sstatusik=i_Cstck
                  sstatusik=i_Cnctc                  
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slide) then
                  sstatusik=i_Cslid
               end if
            end if

         !-------------------------------------------------------------------------
         case(i_EXPO_CZM_SPRING_P,i_IQS_EXPO_CZM_SPRING_P)


            ! mise a jour de taz
            if (i_what == i_check) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki) + (this(ik)%Wts*rlsiki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki) + (this(ik)%Wns*rlsiki)
              vs = vlocfreesik + (this(ik)%Wst*rltiki) + (this(ik)%Wsn*rlniki) + (this(ik)%Wss*rlsiki)
              
              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring_p_3D(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt, &
                                      (this(ik)%gapTTbegin-this(ik)%internal(9))+H*vn,H*vs,rltiki,rlniki,rlsiki)
            endif 
            
            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_post) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki) + (this(ik)%Wts*rlsiki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki) + (this(ik)%Wns*rlsiki)
              vs = vlocfreesik + (this(ik)%Wst*rltiki) + (this(ik)%Wsn*rlniki) + (this(ik)%Wss*rlsiki)
              
              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring_p_3D(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt, &
                                        (this(ik)%gapTTbegin-this(ik)%internal(9))+H*vn,H*vs,rltiki,rlniki,rlsiki)
              call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )
            endif

            !fd mise a jour des status            
            if (is_cohesive) then                
               if (sstatusik == i_noctc) then
                  !fd sstatusik=i_Cstck
                  sstatusik=i_Cnctc                  
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slide) then
                  sstatusik=i_Cslid
               end if
            end if
            
         !!!--------------------------------------
         CASE(i_ER_MAC_CZM)

            IF (is_cohesive) THEN 
               
               vt = 0.D0 ; vn = 0.D0 ; vs = 0.D0
               
               IF (diagonal_resolution) THEN
                  vs = vvlocfreesik + WWssik*rlsiki  
                  vt = vvlocfreetik + WWttik*rltiki
                  if (is_traction) then
                    vn = vvlocfreenik + WWnnik*rlniki - max(0.d0,this(ik)%covfreen) 
                  else
                    vn=0.d0
                  endif
               ELSE
                  call faterr(IAM,'impossible to not have diagonal resolution with ER_MAC_CZM law')
                  vs = vvlocfreesik + WWssik*rlsiki + WWstik*rltiki + WWsnik*rlniki 
                  vt = vvlocfreetik + WWtsik*rlsiki + WWttik*rltiki + WWtnik*rlniki 
                  vn = vvlocfreenik + WWnsik*rlsiki + WWntik*rltiki + WWnnik*rlniki - this(ik)%covfreen 
               END IF



               !if (is_traction) then
               !  print*,vn
               !  stop
               !endif

               IF (i_what == i_post) THEN 
                  
                  CALL updt_CZM_3D(ibehav,.FALSE.,this(ik)%internal,this(ik)%taz,vt,vn,vs)

                  !fr: why do we deal only these particular interaction cases and not every cases?
                  if ( ( this(ik)%CDAN == i_SPPLx ) .or. &
                       ( this(ik)%CDAN == i_SPPRx ) .or. &
                       ( this(ik)%CDAN == i_PRPRx ) ) THEN

                     call set_internal( this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

                  end if
                  
               END IF
               
               Hradh_t = Hradh_t + (k_t * vt)
               Hradh_n = Hradh_n + (k_n * vn)
               if (.not. is_traction)  Hradh_n = 0.d0 
               Hradh_s = Hradh_s + (k_t * vs)
               
               IF (Hradh_n > 1.d-3) then
                 write(cout,'(A22,D10.3,A13,I5)') 'strange cohesive part ',Hradh_n,' for contact ',ik
                 call LOGMES(cout)
                 call display_tacinfo(ik)
               endif

               rlsiki = rlsiki + Hradh_s 
               rltiki = rltiki + Hradh_t 
               rlniki = rlniki + Hradh_n
               
               !if (nstep > 635) then
               !  print*,ik,is_traction 
               !  print*,rlniki,Hradh_n
               !  print*,vt,vn,vs
               !endif

               IF (sstatusik == i_noctc) sstatusik=i_Cnctc
               IF (sstatusik == i_stick) sstatusik=i_Cstck
               IF (sstatusik == i_slide) sstatusik=i_Cslid

            END IF

         !!!--------------------------------------
         CASE(i_KV_WET) 
            if (this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslid ) then
            !IF (this(ik)%statusBEGIN(1:1) == 'W') THEN
               rlniki=rlniki+this(ik)%corln
               rltiki=rltiki+this(ik)%corlt
               rlsiki=rlsiki+this(ik)%corls
               IF (sstatusik == i_noctc) sstatusik=i_Wnctc
               IF (sstatusik == i_stick) sstatusik=i_Wstck
               IF (sstatusik == i_slide) sstatusik=i_Wslid
            END IF

            IF (sstatusik == i_noctc) this(ik)%internal(1) = 0.D0

            !fr: why do we deal only these particular interaction cases and not every cases?
            if ( ( this(ik)%CDAN == i_SPPLx ) .or. &
                 ( this(ik)%CDAN == i_SPPRx ) .or. &
                 ( this(ik)%CDAN == i_SPSPx ) ) THEN

               call set_internal( this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

            end if

         !!!--------------------------------------
         CASE(i_IQS_CLB_RGR)

            rlniki = rlniki + this(ik)%corln

            IF (this(ik)%corln .GT. 0.D0 .AND. sstatusik == i_noctc) sstatusik=i__RGR_

         !!!-------------------------------------
         CASE(i_IQS_MOHR_DS_CLB)
            !print*,ik,this(ik)%statusBEGIN,sstatusik
            IF (this(ik)%statusBEGIN == i_Mstck .and. sstatusik /= i_noctc) THEN 
               rlniki = rlniki + this(ik)%corln
               IF (sstatusik == i_stick) sstatusik=i_Mstck
            !ELSE IF (this(ik)%statusBEGIN == i_Mstck .and. sstatusik == i_noctc) THEN 
            END IF

         !!!--------------------------------------
         CASE(i_CRITICAL_VOIGT_CLB)
          
            !this(ik)%vepad = pad velocity = WWttik*rrltik+WWtnik*rrlnik+vvlocfreetik

            this(ik)%vepadt = WWttik*rltiki+WWtnik*rlniki+vvlocfreetik
            this(ik)%vepads = WWssik*rlsiki+WWsnik*rlniki+vvlocfreesik
            IF (i_what == i_post) THEN
              this(ik)%internal(1)=this(ik)%internal(1)+H*(this(ik)%vlt-this(ik)%vepadt)
              this(ik)%internal(3)=this(ik)%internal(3)+H*(this(ik)%vlt-this(ik)%vepads)

              !fr: why do we deal only these particular interaction cases and not every cases?
              if ( ( this(ik)%CDAN == i_SPPLx ) .or. &
                   ( this(ik)%CDAN == i_SPPRx ) .or. &
                   ( this(ik)%CDAN == i_PRPRx ) .or. &
                   ( this(ik)%CDAN == i_CSASp ) ) THEN

                 call set_internal( this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

              end if

            END IF

          !!!--------------------------------------
          case(i_IQS_BW_CLB)
             if ( rlniki .gt. this(ik)%internal(2) ) then
                !projection sur le cone modifie
                alphaik = this(ik)%internal(3)
                fnik    = this(ik)%internal(2)
                rtt = sqrt(rlsiki*rlsiki+rltiki*rltiki)
                if (rtt.gt.1e-16) then
                   rlsiki = rlsiki/rtt
                   rltiki = rltiki/rtt
                   rtt  = min(rtt,alphaik*rlniki-fnik*(alphaik-fricik))
                   rlsiki = rlsiki*rtt
                   rltiki = rltiki*rtt
                else
                   rlsiki = 0.d0
                   rltiki = 0.d0
                end if
             else
                !nothing to do
             end if

          !!!--------------------------------------
          CASE(i_NARD_ROD)

            IF (is_cohesive) THEN 
               
               vt = 0.D0 ; vn = 0.D0 ; vs = 0.D0
               
               vs = vvlocfreesik + WWssik*rlsiki + WWstik*rltiki + WWsnik*rlniki 
               vt = vvlocfreetik + WWtsik*rlsiki + WWttik*rltiki + WWtnik*rlniki 
               vn = vvlocfreenik + WWnsik*rlsiki + WWntik*rltiki + WWnnik*rlniki !g/h
               
               Hradh_n = 0.D0
               Hradh_t = 0.D0
               Hradh_s = 0.D0

               Hradh_n = this(ik)%internal(1)*Ksn*H  &
                       + vn*H*(-Kvn-Ksn*H)+this(ik)%gapTTbegin*Kvn

               Hradh_t =-Kvt*vt*H - &
                         kst*H*(H*vt + this(ik)%internal(2))

               Hradh_s =-Kvs*vs*H - &
                         kss*H*(H*vs + this(ik)%internal(3))

               rlniki = rlniki +  Hradh_n
               rltiki = rltiki +  Hradh_t 
               rlsiki = rlsiki +  Hradh_s

               IF (i_what == i_post) THEN  
               
                 IF (sstatusik == i_noctc) THEN
                    sstatusik=i_Cstck
                 ELSE IF (sstatusik == i_stick) THEN
                    sstatusik=i_Cstck
                 ELSE IF (sstatusik == i_slide) THEN
                    sstatusik=i_Cstck
	         END IF

               END IF

            ELSE

               rlniki = 0.d0
               rltiki = 0.d0
               rlsiki = 0.d0

            END IF

         ! if (abs(rltiki)>1d8 .or. abs(rlniki)>1d8 .or. abs(rlsiki)>1d8) then 
         !     print*,''
         !     call print_info(ik)
         !     print*,''
         !     print*,get_inter_law_name_from_id(tact_behav(ibehav)%ilaw)
         !     print*,''

         !    endif
   

            
            IF (i_what == i_post) THEN  


              this(ik)%internal(2) = this(ik)%internal(2) + vt*H  
              this(ik)%internal(3) = this(ik)%internal(3) + vs*H    

               
              ! IF (this(ik)%CDAN == i_SPSPx) THEN               
              !    CALL put_internal_SPSPx(this(ik)%icdan,this(ik)%internal)
              ! ELSE IF (this(ik)%CDAN == i_PRPRx) THEN               
              !    CALL put_internal_PRPRx(this(ik)%icdan,this(ik)%internal)
              ! ELSE

              IF (this(ik)%CDAN == i_PTPT3) THEN               
                 CALL set_internal(this(ik)%CDAN, this(ik)%icdan, this(ik)%internal)
              ELSE
                 CALL FATERR(IAM,'NARD_ROD internal put!?')
              END IF

            END IF


         !!!--------------------------------------
         CASE(i_IQS_CLB_g0,i_IQS_PLAS_CLB,i_GAP_SGR_CLB_g0,i_ELASTIC_REPELL_CLB_g0,i_ELASTIC_ROD)
            CALL set_internal(this(ik)%CDAN, this(ik)%icdan, this(ik)%internal)            
         !!!--------------------------------------
         CASE default
            !nothing to do

         !!!--------------------------------------
         END SELECT

      END IF
        
      IF (i_what /= i_check) THEN
      !--------------------------      
         
         IF (this(ik)%forecast == i_noact) THEN

            rlsiki=0.D0
            rltiki=0.D0
            rlniki=0.D0
            this(ik)%rls=rlsiki
            this(ik)%rlt=rltiki
            this(ik)%rln=rlniki

            this(ik)%status=sstatusik

            ! No further updating is necessary since noact candidates for contact are, by definition,
            ! assigned not to interfer with other candidates.
            
         ELSE IF (this(ik)%forecast == i_acton) THEN
            
            this(ik)%rls=RELAX*rlsiki+RELAX1*rlsik
            this(ik)%rlt=RELAX*rltiki+RELAX1*rltik
            this(ik)%rln=RELAX*rlniki+RELAX1*rlnik

            IF (DABS(this(ik)%rls) .LT. 1.D-24) this(ik)%rls=0.D0
            IF (DABS(this(ik)%rlt) .LT. 1.D-24) this(ik)%rlt=0.D0
            IF (DABS(this(ik)%rln) .LT. 1.D-24) this(ik)%rln=0.D0

            this(ik)%status=sstatusik

            IF(.NOT.SDLactif)THEN
               ! Computing_________________________________________  R - H Rloc(ik) + H Rloc(ik)
               rsik=this(ik)%rls-rlsik
               rtik=this(ik)%rlt-rltik  
               rnik=this(ik)%rln-rlnik 
               ! Injecting difference between impulse reactions after and before going through the single
               ! contact solver
               CALL injj_(ik,rsik,rtik,rnik,iIreac)
               
            END IF
            
         END IF

      ELSE
         
         ! Updating is purposedly omitted while check, according to the definition of violations.
         ! Only rltiki, rlniki, (weighting with RELAX is also omitted), rltik, rlnik, sstatusik, 
         ! are used for computing violations.
         ! check is an apart process and does not interfer with iter process values. 

      END IF  ! IF (i_what /= i_check) THEN
      
      IF (i_what == i_post) THEN
      !-------------------------- 

         ! Rebuilding relative velocities, gap, status, with last known reaction values 
         
         Wrltiki = this(ik)%Wts*rlsiki+this(ik)%Wtt*rltiki+this(ik)%Wtn*rlniki
         Wrlniki = this(ik)%Wns*rlsiki+this(ik)%Wnt*rltiki+this(ik)%Wnn*rlniki
         Wrlsiki = this(ik)%Wss*rlsiki+this(ik)%Wst*rltiki+this(ik)%Wsn*rlniki
         
         vltiki=Wrltiki+vlocfreetik
         vlniki=Wrlniki+vlocfreenik
         vlsiki=Wrlsiki+vlocfreesik
         
         IF (DABS(vlsiki) .LT. 1.D-24) vlsiki=0.D0
         IF (DABS(vltiki) .LT. 1.D-24) vltiki=0.D0
         IF (DABS(vlniki) .LT. 1.D-24) vlniki=0.D0

         ! si il y a compression on doit corriger la vitesse relative et le gap
         if (regul .and. krn /= 0.d0 .and. rlniki > 0.d0) vlniki = vlniki - (rlniki/(H*H*krn*this(ik)%internal(1))) 
         
         vls=vlsiki
         vlt=vltiki
         vln=vlniki
         
         this(ik)%vls=vls
         this(ik)%vlt=vlt
         this(ik)%vln=vln
         
         gapTTik=this(ik)%gapTTbegin+H*vlniki
         IF (DABS(gapTTik) .LT. 1.D-24) gapTTik=0.D0

         
         this(ik)%status=sstatusik

         IF (itchatche) PRINT*,ik,'vlt ',vlt,' vln ',vln,' vls ',vls, 'rlt ',this(ik)%rlt,' rln ',this(ik)%rln,' rls ',this(ik)%rls 

         ! Sending local data   
         call set_loc( this(ik)%CDAN, this(ik)%icdan,this(ik)%status, &
                       vlt         , vln         , vls         ,      &
                       this(ik)%rlt, this(ik)%rln, this(ik)%rls, gapTTik)

      END IF  ! IF (i_what == i_post) THEN
      
      IF (i_what == i_check) THEN
      !-------------------------- 
         
         ! Computing discrepancies, violations, mean values
         
         ! Rebuilding relative velocities, gap, status, with last known reaction values 
         Wrlsiki = this(ik)%Wss*rlsiki+this(ik)%Wst*rltiki+this(ik)%Wsn*rlniki
         Wrltiki = this(ik)%Wts*rlsiki+this(ik)%Wtt*rltiki+this(ik)%Wtn*rlniki
         Wrlniki = this(ik)%Wns*rlsiki+this(ik)%Wnt*rltiki+this(ik)%Wnn*rlniki

         vlsiki=Wrlsiki+vlocfreesik
         vltiki=Wrltiki+vlocfreetik
         vlniki=Wrlniki+vlocfreenik

         IF (DABS(vlsiki) .LT. 1.D-24) vlsiki=0.D0
         IF (DABS(vltiki) .LT. 1.D-24) vltiki=0.D0
         IF (DABS(vlniki) .LT. 1.D-24) vlniki=0.D0

         this(ik)%statuscheck=sstatusik
         
         ! rlsik=this(ik)%rls, rltik=this(ik)%rlt, rlnik=this(ik)%rln, vlsik, vltik, vlnik, is the solution to be checked,
         ! rlsiki, rltiki, rlniki, vlsiki, vltiki, vlniki, is a non updated solution constructed after rlsik, rltik, rlnik,
         ! vlsik, vltik, vlnik,see the definition of violation in 
         ! Micro Mecanique des Materiaux Granulaires, B. Cambou, M. Jean, Hermes 2001
         
         rlocs=0.5D0*(rlsik+rlsiki)
         rloct=0.5D0*(rltik+rltiki) 
         rlocn=0.5D0*(rlnik+rlniki)
         
         vls=0.5D0*(vlsik+vlsiki)
         vlt=0.5D0*(vltik+vltiki)
         vln=0.5D0*(vlnik+vlniki)
         
         Dvls=vlsik-vlsiki
         Dvlt=vltik-vltiki
         Dvln=vlnik-vlniki
         
         DVDV   = Dvls*Dvls+Dvlt*Dvlt+Dvln*Dvln
         DVDVRR = DVDV*(rlocs*rlocs+rloct*rloct+rlocn*rlocn)
         
         DVoR = rlocs*Dvls+rloct*Dvlt+rlocn*Dvln  
         
         SumDVDV=SumDVDV+DVDV
         MaxDVDV=DMAX1(MaxDVDV,DVDV)
         
         SumDVDVRR=SumDVDVRR+DVDVRR
         MaxDVDVRR=DMAX1(MaxDVDVRR,DVDVRR)
         
         SumDVoR=SumDVoR+DVoR
         
         WRR=0.5D0*(Wrlsik*rlsik  +Wrltik*rltik  +Wrlnik*rlnik   &
              +Wrlsiki*rlsiki+Wrltiki*rltiki+Wrlniki*rlniki )
         
         SumWRR=SumWRR+WRR
         SumWRWR=SumWRWR+0.5D0*(Wrlsik*Wrlsik  +Wrltik*Wrltik  +Wrlnik*Wrlnik   &
              + Wrlsiki*Wrlsiki+Wrltiki*Wrltiki+Wrlniki*Wrlniki)           
         
         ! Arrays for fine violation analysis
         Xvlton(ik)   = DVDV  ! Xvlton(ik)=DVDVRR
         WRRarray(ik) = WRR
         
         ! Analyzing status
         IF (this(ik)%forecast == i_noact) THEN
            Nnoact=Nnoact+1
         ELSEIF (this(ik)%statuscheck == i_noctc)THEN
            Nnoctc=Nnoctc+1 
         ELSE
            Nactif=Nactif+1
            IF (rlocn > 0.D0) Ncompr=Ncompr+1
            IF (rlocn < 0.D0) Ntract=Ntract+1
            IF     (this(ik)%statuscheck == i_stick .OR.  &
                    this(ik)%statuscheck == i_Wstck .OR.  &
                    this(ik)%statuscheck == i_Cstck) THEN  
              Nstick=Nstick+1
            ELSEIF (this(ik)%statuscheck == i_slide .OR. &
                    this(ik)%statuscheck == i_Wslid .OR. &
                    this(ik)%statuscheck == i_Cslid) THEN  
               Nslide=Nslide+1
            END IF
         ENDIF
         
         IF (this(ik)%statuscheck /= this(ik)%status)   NOKsta=NOKsta+1
         
         IF (this(ik)%statuscheck == i__RGR_) Nb_RGR = Nb_RGR+1
         
         ! Fine violation analysis
         IF (this(ik)%statuscheck .NE. i_noctc) THEN
            WRRmin=DMIN1(WRRmin,WRRarray(ik))
            WRRmax=DMAX1(WRRmax,WRRarray(ik))
            IF (QuadWR == 0.d0) THEN
               Xvlton(ik)=DSQRT(Xvlton(ik))
            ELSE  
               Xvlton(ik)=DSQRT(Xvlton(ik))/(QuadWR*tol)
            ENDIF
            ! The violation has the form (sqrt(DVDV)/sqrt(WRWR))/tol
         END IF
         
         call set_violation( this(ik)%CDAN, this( ik )%icdan, Xvlton( ik ) )

      END IF  ! IF (i_what == i_check) THEN
   

   END DO  ! DO ikk=1,nb_CDAN
   !$OMP END DO
   !$OMP END PARALLEL

   !-------------------------------------
   ! Summarize rough violation analysis
   !-------------------------------------
   
   IF (i_what == i_check) THEN
   !-------------------------- 
      
      ! compute quantities used to check the convergence (QuadDV, MaxmDV, QuadDVR, MaxmDVR and MeanDVoR)
      call compute_convergence_norms_nlgs(Nactif, SumWRWR, SumDVDV, MaxDVDV, &
                                          SumWRR, SumDVDVRR, MaxDVDVRR, SumDVoR, tol, &
                                          QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR)

      ! compute more check quantities
      IF (Nactif .GE. 1 .AND. SumWRR .GT. 1.D-18 .AND. SumWRWR .GT. 1.D-18) THEN   
         QuadWR   = DSQRT(SumWRWR/REAL(Nactif,8))
         Dreac    = QuadWR*H  
         
         MeanWRR  = SumWRR/REAL(Nactif,8)
         rcvltm = - MeanDVoR*tol                                                   
         dynstat  = 0.5D0*dynstat/SumWRR
     ELSE   
         QuadWR   = 0.111D-11 
         Dreac    = 0.111D-11 
         MeanWRR  = 0.111D-11
         rcvltm =   0.000D+00 
         dynstat  = 0.111D-11
      END IF

   END IF
   
 END SUBROUTINE solve_nlgs
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------

 !am: this function compute the quantities used to check the convergence from given summations (the stored ones or external ones)
 !    N.B. this function is used in DDM
 subroutine compute_convergence_norms_nlgs(Nactif_, SumWRWR_, SumDVDV_, MaxDVDV_, &
                                           SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_, &
                                           QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_)

    implicit none

    ! inputs
    integer, intent(in) :: Nactif_
    real(kind=8), intent(in) :: SumWRWR_, SumDVDV_, MaxDVDV_, SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_

    ! outputs
    real(kind=8), intent(out) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_

    ! locals
    real(kind=8) :: QuadWR_, MeanWRR_

    if (Nactif_ >= 1 .and. SumWRR_ > 1.d-18 .and. SumWRWR_ > 1.d-18) then
       QuadWR_   = DSQRT(SumWRWR_/REAL(Nactif_,8))
       QuadDV_   = DSQRT(SumDVDV_/REAL(Nactif_,8))   / (QuadWR_*tol_)
       MaxmDV_   = DSQRT(MaxDVDV_)                  / (QuadWR_*tol_)

       MeanWRR_  = SumWRR_/REAL(Nactif_,8)
       QuadDVR_  = DSQRT(SumDVDVRR_/REAL(Nactif_,8)) / (MeanWRR_*tol_)
       MaxmDVR_  = DSQRT(MaxDVDVRR_)                / (MeanWRR_*tol_)
       MeanDVoR_ = SumDVoR_                         / (SumWRR_*tol_)
    else
       QuadDV_   = 0.111D-11
       MaxmDV_   = 0.111D-11
       
       QuadDVR_  = 0.111D-11
       MaxmDVR_  = 0.111D-11
       MeanDVoR_ = 0.111D-11
    end if

 end subroutine compute_convergence_norms_nlgs
 
!------------------------------------------------------------------------
 
 !raf: this function put convergence norms into global variables, when convergence norms are not compute in solve_nlgs fonctions.
 !    N.B. this function is used in DDM
 subroutine put_convergence_norms_nlgs( QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_)
                                           
    real(kind=8), intent(in) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_
    
    QuadDV   = QuadDV_
    MaxmDV   = MaxmDV_
    QuadDVR  = QuadDVR_
    MaxmDVR  = MaxmDVR_ 
    MeanDVoR = MeanDVoR_

 end subroutine put_convergence_norms_nlgs
 
!------------------------------------------------------------------------

 !am: this function allows to get all, or a part of, summations used to compute the quantities required to check the convergence
 !    N.B. this function is used in DDM 
 SUBROUTINE get_error(SumDVDVRR_, Nactif_, MeanWRR_, &
                      tol_, SumDVDV_, QuadWR_, SumDVoR_, &
                      SumWRR_, SumWRWR_, MaxDVDV_, MaxDVDVRR_) 
    
   IMPLICIT NONE

   !am: all outpout variables are optional in order to allow the user to define which variables he want to collect
   !    N.B. some new variables were added but the order of the first variable did not change, so that callings in Iceta DDM code should still be working!
   INTEGER, INTENT(out), optional :: Nactif_
   REAL(kind=8), INTENT(out), optional :: SumDVDVRR_, &
                                          MeanWRR_, tol_, SumDVDV_, &
                                          QuadWR_, SumDVoR_, SumWRR_, &
                                          SumWRWR_, MaxDVDV_, MaxDVDVRR_
  
   ! only the asked values are returned 
   if (present(SumDVDVRR_)) SumDVDVRR_ = SumDVDVRR
   if (present(Nactif_))    Nactif_    = Nactif
   if (present(MeanWRR_))   MeanWRR_   = MeanWRR
   if (present(tol_))       tol_       = tol
   if (present(SumDVDV_))   SumDVDV_   = SumDVDV
   if (present(QuadWR_))    QuadWR_    = QuadWR
   if (present(SumDVoR_))   SumDVoR_   = SumDVoR
   if (present(SumWRR_))    SumWRR_    = SumWRR
   if (present(SumWRWR_))   SumWRWR_   = SumWRWR
   if (present(MaxDVDV_))   MaxDVDV_   = MaxDVDV
   if (present(MaxDVDVRR_)) MaxDVDVRR_ = MaxDVDVRR
 
 END SUBROUTINE get_error

!!!------------------------------------------------------------------------
!!!---------------------------------------------------------------
  SUBROUTINE RnodHRloc_nlgs(list_INTRF, storage_reac)

    IMPLICIT NONE

    ! optional inputs, used in DDM
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: list_INTRF
    INTEGER, INTENT(IN), OPTIONAL :: storage_reac

    ! locals
    INTEGER :: i,ik
    !am: storage specifies where to store the reaction torque.
    INTEGER :: storage

    ! the reaction torque will be stored in Ireac..
    storage = iIreac

    ! ... unless the user choose another location
    if (present(storage_reac)) storage = storage_reac

    IF (nb_CDAN == 0) RETURN

    IF (.NOT. PRESENT(list_INTRF)) THEN
       DO ik=1,nb_CDAN  
          CALL nullify_reac_(ik, storage)
       END DO
       DO ik=1,nb_CDAN
          CALL injj_(ik, this(ik)%rls, this(ik)%rlt, this(ik)%rln, storage)
       END DO
    ELSE
       DO i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          CALL nullify_reac_(ik, storage)
       END DO
       DO i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          CALL injj_(ik, this(ik)%rls, this(ik)%rlt, this(ik)%rln, storage)
       END DO
    END IF

  END SUBROUTINE RnodHRloc_nlgs
!!!---------------------------------------------------------------
  SUBROUTINE set_nlgs_parameter(normtype,tolerance,relaxation)

    CHARACTER(len=5)  :: normtype
    REAL(kind=8)      :: relaxation,tolerance

    tol   = tolerance
    RELAX = relaxation
    RELAX1= 1.D0-RELAX

    SELECT CASE(normtype)
    CASE('QuaN ')
       i_checktype = i_QuadN
    CASE('Quad ')
       i_checktype = i_Quad
    CASE('Maxm ')
       i_checktype = i_Maxm
    CASE('QM/16')
       i_checktype = i_QMs16
    END SELECT

    nlgs_solver3D = .TRUE.

  END SUBROUTINE set_nlgs_parameter
!------------------------------------------------------------------------
 subroutine prjj_(ik,vsik,vtik,vnik,storage)
   implicit none
   integer     , intent(in)  :: ik
   real(kind=8), intent(out) :: vsik,vtik,vnik   
   integer     , intent(in)  :: storage

   call prjj( this(ik)%CDAN, this(ik)%icdan, vtik, vnik, vsik, storage )

 end subroutine prjj_
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine injj_(ik,rsik,rtik,rnik,storage)
   implicit none
   integer     , intent(in) :: ik
   real(kind=8), intent(in) :: rsik,rtik,rnik
   integer     , intent(in) :: storage  
  
   call injj( this(ik)%CDAN, this(ik)%icdan, rtik, rnik, rsik, storage)

 end subroutine injj_
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine nullify_reac_(ik,storage)
   implicit none
   integer, intent(in) :: ik
   integer, intent(in) :: storage

   call nullify_reac( this(ik)%CDAN, this(ik)%icdan, storage )

 end subroutine nullify_reac_
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine vitrad_(ik,storage)
   integer, intent(in) :: ik
   integer, intent(in) :: storage
   !
   logical :: need_full_V
   
   !fd to say if all terms of a deformable body velocity are mandatory, default is no
   need_full_V = .false.
   if (storage == iVaux_e_invM_t_Iaux_ .and. SDLACTIF) need_full_V = .true.

   call vitrad( this(ik)%CDAN, this(ik)%icdan, storage, need_full_V )
   
 end subroutine vitrad_
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine nullify_vlocy_(ik,storage)
   implicit none
   integer, intent(in) :: ik
   integer, intent(in) :: storage

   call nullify_vlocy( this(ik)%CDAN, this(ik)%icdan, storage )

 end subroutine nullify_vlocy_
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine mu_NG_solver_(ik,Wnn,Wtt,Wss,Wst,Wts,Wns,Wsn,Wtn,Wnt, &
                          Sn,St,Ss,rln,rlt,rls,fric,     &
                          Wrln,Wrlt,Wrls,rlnik,rltik,rlsik,sstatus)
   IMPLICIT NONE

   REAL(kind=8)                :: Wnn,Wtt,Wss,Wst,Wts,Wns,Wsn,Wtn,Wnt        ! in
   REAL(kind=8)                :: Wrln,Wrlt,Wrls,rls,rlt,rln,fric,Sn,St,Ss   ! inout
   REAL(kind=8)                :: rlsik,rltik,rlnik,Wrlnik,Wrltik,Wrlsik
   REAL(kind=8)                :: Wrlniki,Wrltiki,Wrlsiki,rlsiki,rltiki,rlniki
   REAL(kind=8)                :: ron,rot,rlvn,rlvt,rlvs,rlv,fricik
   REAL(kind=8)                :: phi3,phis4,phit4,rlv3,rlv_1,Wrefik,Erwik,Ntol=0.1D-05,fric_inew
   REAL(kind=8)                :: Att,Ass,Ats,Ast,Btn,Bsn,Btt,Bst,Bts,Bss
   REAL(kind=8),DIMENSION(3)   :: Err,Erv,det
   REAL(kind=8),DIMENSION(3,3) :: AWB
   integer(kind=4)             :: sstatus
   INTEGER                     :: inew,ik
   INTEGER                     :: INEWTX=15

   REAL(kind=8)                :: delta,inv_delta,A,B,C
   
   !calcul préliminaire fait dans le prep: choix de ron et rot rendant contractantes les applications
   ! Rn --> hRn - ronUn et Rt--> hRt - rotUt

   ron=this(ik)%ron
   rot=this(ik)%rot

   Wrlnik=Wrln
   Wrltik=Wrlt
   Wrlsik=Wrls

   rlnik=rln
   rltik=rlt
   rlsik=rls

   DO inew=1,INEWTX                             ! début des itérations de Newton. INEWTX est le nombre 
                                                ! d'itérations maximum, fixé par COMMAND.DAT.
     Wrlniki=Wrlnik                             ! On cherche à exprimer la matrice gradient:
     Wrltiki=Wrltik                             ! Dphi=| I  -W  |
     Wrlsiki=Wrlsik                             !      | Ap  Bp | 
     rlsiki=rlsik                               ! p étant l'indice inew.
     rltiki=rltik                               ! On effectue tour à tour les projections sur le cone
     rlniki=rlnik                               ! positif et sur le cone de Coulomb pour exprimer les différentes
                                                ! valeurs des matrices Ap et Bp.
     rlvn=rlnik-ron*(Wrlnik+Sn)                     
     IF( rlvn >= 0.D0 )THEN                     ! Projection sur le cone positif
       phi3 = ron*(Wrlnik+Sn)
       !Ann  = ron
       !Bnn  = 0.D0
       AWB(1,1)= ron*Wnn;AWB(1,2)= ron*Wnt;AWB(1,3)= ron*Wns
     ELSE
       phi3=rlnik
       !Ann=0.D0
       !Bnn=1.D0
       AWB(1,1)=1.D0;AWB(1,2)=0.D0;AWB(1,3)=0.D0
       sstatus=i_noctc
     ENDIF

     rlvt   = rltik-rot*(Wrltik+St)
     rlvs   = rlsik-rot*(Wrlsik+Ss)
     rlv    = SQRT(rlvt*rlvt+rlvs*rlvs)

     !if (inew<6) then
     !   fric_inew=(fric*0.2D0)*real(inew,8)
     !   else
     !fric_inew = fric
     !endif
     fric_inew = fric
     fricik = fric_inew*rlnik

     IF(( rlv < fricik).OR.(rlv < 1.D-24))THEN  ! Projection sur le cone de Coulomb
       phit4=rot*(Wrltik+St)
       phis4=rot*(Wrlsik+Ss)
       !Att=rot
       !Ats=0.D0
       !Ast=0.D0
       !Ass=rot
       !Btt=0.D0 ; Btn=0.D0 ; Bsn=0.D0 
       !Bts=0.D0 ; Bst=0.D0 ; Bss=0.D0       
       ! composantes TN,TT et TS.
       AWB(2,1)= rot*Wtn;AWB(2,2)=rot*Wtt;AWB(2,3)= rot*Wts       
       ! composantes TN,TT et TS.
       AWB(3,1)= rot*Wsn;AWB(3,2)= rot*Wst;AWB(3,3)= rot*Wss       
       sstatus=i_stick
     ELSE
       rlv_1 = 1.D0/rlv
       phit4 = rltik-fricik*rlvt*rlv_1
       phis4 = rlsik-fricik*rlvs*rlv_1
       Btn   = -fric_inew*rlvt*rlv_1
       Bsn   = -fric_inew*rlvs*rlv_1
       rlv3  = fricik*rlv_1*rlv_1*rlv_1
       Att= rlv3*rlvs*rlvs ; Ats =-rlv3*rlvt*rlvs
       Ast= Ats            ; Ass = rlv3*rlvt*rlvt
       Btt= 1.D0-Att       ; Bts = -Ats
       Bst= -Ast           ; Bss = 1.D0-Ass
       Att= rot*Att        ; Ass = rot*Ass
       Ast= rot*Ast        ; Ats = rot*Ats
       ! composantes TN,TT et TS.
       AWB(2,1)= Att*Wtn+Ats*Wsn+Btn
       AWB(2,2)= Att*Wtt+Ats*Wst+Btt
       AWB(2,3)= Att*Wts+Ats*Wss+Bts

       ! composantes TN,TT et TS.
       AWB(3,1)= Ast*Wtn+Ass*Wsn+Bsn
       AWB(3,2)= Ast*Wtt+Ass*Wst+Bst
       AWB(3,3)= Ast*Wts+Ass*Wss+Bss

       sstatus=i_slide
     ENDIF

     ! Calcul de l'inverse de AWB
   
     det(1)=AWB(2,2)*AWB(3,3)-AWB(2,3)*AWB(3,2)
     det(2)=AWB(2,1)*AWB(3,3)-AWB(3,1)*AWB(2,3)
     det(3)=AWB(2,1)*AWB(3,2)-AWB(3,1)*AWB(2,2)

     delta=AWB(1,1)*det(1)-AWB(1,2)*det(2)+AWB(1,3)*det(3)

     IF(ABS(delta) <= 1.D-24)THEN
        err=1
        call faterr('nlgs_3D::mu_NG_solver','WARNING! A.W+B is not inversible')
     ENDIF
     
     inv_delta=1.D0/delta

     A=det(1)*phi3-(AWB(1,2)*AWB(3,3)-AWB(3,2)*AWB(1,3))*phit4+(AWB(1,2)*AWB(2,3)-AWB(2,2)*AWB(1,3))*phis4
     rlnik=MAX(0.D0,rlnik-A*inv_delta)

     B=-det(2)*phi3+(AWB(1,1)*AWB(3,3)-AWB(3,1)*AWB(1,3))*phit4-(AWB(1,1)*AWB(2,3)-AWB(2,1)*AWB(1,3))*phis4
     rltik= rltik-B*inv_delta

     C=det(3)*phi3-(AWB(1,1)*AWB(3,2)-AWB(3,1)*AWB(1,2))*phit4+(AWB(1,1)*AWB(2,2)-AWB(2,1)*AWB(1,2))*phis4
     rlsik= rlsik-C*inv_delta

     Wrltik=Wtt*rltik+Wtn*rlnik+Wts*rlsik
     Wrlnik=Wnt*rltik+Wnn*rlnik+Wns*rlsik
     Wrlsik=Wst*rltik+Wsn*rlnik+Wss*rlsik

     Err(1)=rlnik-rlniki
     Err(2)=rltik-rltiki
     Err(3)=rlsik-rlsiki

     Erv(1)=Wrlnik-Wrlniki
     Erv(2)=Wrltik-Wrltiki
     Erv(3)=Wrlsik-Wrlsiki

     Wrefik=Wrlnik*rlnik+Wrltik*rltik+Wrlsik*rlsik

     IF( Wrefik <= 1.D-24 ) Wrefik=1.D0
     Erwik=(Err(1)*Erv(1)+Err(2)*Erv(2)+Err(3)*Erv(3))/(Wrefik*Ntol*Ntol)

     IF(Erwik<1.D0) EXIT

   END DO

 end subroutine mu_NG_solver_
!!!------------------------------------------------------------------------  
  SUBROUTINE Nullify_EntityList_nlgs
    
    IMPLICIT NONE
    
    CALL Free_EntityList
    
  END SUBROUTINE Nullify_EntityList_nlgs
!!!---------------------------------------------------------------
  SUBROUTINE prep_check_nlgs(iconv)
    
    IMPLICIT NONE
    INTEGER :: iconv
    
    iconv = 0
    conv_contact = .TRUE.
    IF (nb_CDAN == 0) RETURN

    iconv = 1
    conv_contact = .FALSE.
    
    CALL RnodHRloc_nlgs

    Dcrac  = 1.D0    !distance caracteristique

    SumDVDV  = 0.D0
    MaxDVDV  = 0.D0
    SumDVDVRR= 0.D0
    MaxDVDVRR= 0.D0
    SumWRWR  = 0.D0
    SumWRR   = 0.D0
    SumDVoR  = 0.D0
    dynstat  = 0.D0
    
    Dreac  = 0.D0      ! "reacteristic" distance 
    Nnoact = 0         ! number of contacts being forecasted inactive 
    Nactif = 0         ! number of active contacts (contacts where the normal reaction is not vanishing)
    
    Nnoctc = 0         ! number of no contacts, obsolete!
    
    Nslide = 0         ! number of sliding contacts
    Nstick = 0         ! number of sticking contacts
    Ncompr = 0         ! number of compressing contacts
    Ntract = 0         ! number of tensile contacts
    NOKsta = 0         ! number of questionable status
    Nb_RGR = 0         ! number of contacts where the "Radjai Gap Rescue" is active
    
    ! nbCDAN-Nnoact-Nnoctc = Nactif = Ncomp+Ntract = Nslide+Nstick+Nb_RGR

  END SUBROUTINE prep_check_nlgs
!!!-------------------------------------------------------------------------------------
  SUBROUTINE comp_check_nlgs(iconv)

    IMPLICIT NONE

    ! locals
    LOGICAL :: converged, s, t, n
  
    ! outputs
    INTEGER, INTENT(OUT) :: iconv

    iconv = 1
    
    !am: check convergence using stored quantities 
    call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, converged)

    conv_contact = converged

    if (converged) iconv = 0

    s = any(isnan(this(:)%rls))
    t = any(isnan(this(:)%rlt))
    n = any(isnan(this(:)%rln))
    if ( s .or. t .or. n ) then
      iconv = -1
    end if

    Scale = 1.D0+rcvltm

  END SUBROUTINE comp_check_nlgs

  !am: this function check convergence using given quantities (the stored ones or some external ones)
  !    N.B. this function is used in DDM
  subroutine check_convergence_nlgs(QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_, converged)

     implicit none

     ! inputs
     real(kind=8), intent(in) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_

     ! outputs
     logical, intent(out) :: converged

     converged = .false.

     SELECT CASE(i_checktype)
        CASE(i_Quad, i_QuadN)
           IF ( DABS(MeanDVoR_) .LT. 1.D0 .AND. &
                DABS(QuadDV_)   .LT. 1.D0 .AND. &
                DABS(QuadDVR_)  .LT. 1.D0) THEN
              converged = .true.
           END IF
        CASE(i_Maxm)
           IF ( DABS(MeanDVoR_) .LT. 1.D0 .AND. &
                DABS(MaxmDV_)   .LT. 1.D0 .AND. &
                DABS(MaxmDVR_)  .LT. 1.D0) THEN
              converged = .true.
           END IF
   !fd 12/12/07c'est quoi cette merde !!
   !       QuadDV  = MaxmDV
   !       QuadDVR = MaxmDVR
        CASE(i_QMs16)
           IF ( DABS(MeanDVoR_) .LT. 1.D0 .AND. &
                DABS(QuadDV_)   .LT. 1.D0 .AND. &
                DABS(QuadDVR_)  .LT. 1.D0 .AND. &
                DABS(MaxmDV_)   .LT. 16.666D0 .AND. &
                DABS(MaxmDVR_)  .LT. 16.666D0) THEN
              converged = .true.
           END IF
     END SELECT

  end subroutine check_convergence_nlgs

!!!------------------------------------------------------------------
  SUBROUTINE display_check_nlgs
    
    IMPLICIT NONE
    CHARACTER(len=103)         :: cout 
    IF (nb_CDAN == 0) RETURN
       
    CALL LOGMES(' ')
    SELECT CASE(i_checktype)
    CASE(i_Quad,i_QuadN)
       WRITE(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Quad ','     Maxm'
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
       CALL LOGMES(cout)
    CASE(i_Maxm)
       WRITE(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Maxm ','     Maxm'
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
       CALL LOGMES(cout)
    CASE(i_QMs16)
       WRITE(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  QM/16','1/16 Maxm'
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV *0.06D0 
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR*0.06D0 
       CALL LOGMES(cout)
    END SELECT
    
    WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','MeanDVoR/SumWRR  =',MeanDVoR,'Free run length  =',Dreac 
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','dynamic/static   =',dynstat
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ncompr =',Ncompr,'  Nnoact =', Nnoact, 'Nslide =',Nslide
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ntract =',Ntract,'  NOKsta =',NOKsta , 'Nstick =',Nstick 
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,23X,A10,I10)')                      ' @ ',                   '  Nb_RGR =',Nb_RGR 
    CALL LOGMES(cout)
    CALL LOGMES('  ')

  END SUBROUTINE display_check_nlgs
!!!------------------------------------------------------------------------  
  SUBROUTINE get_after_iter_check(ddynstat,nnb_CDAN,NNnoact,NNcompr,NNtract,NNslide,NNstick,NNOKsta,NNb_RGR)

    IMPLICIT NONE
    INTEGER   :: nnb_CDAN,NNnoact,NNcompr,NNtract,NNslide,NNstick,NNOKsta,NNb_RGR
    REAL(kind=8) :: ddynstat
    
    ddynstat=dynstat
    nnb_CDAN=nb_CDAN
    NNnoact=Nnoact
    NNcompr=Ncompr
    NNtract=Ntract
    NNslide=Nslide
    NNstick=Nstick
    NNOKsta=NOKsta
    NNb_RGR=Nb_RGR
    
  END SUBROUTINE get_after_iter_check
!!!---------------------------------------------------------------
  SUBROUTINE scale_rloc_nlgs

    IMPLICIT NONE    

    INTEGER :: ik

    IF (nb_CDAN == 0) RETURN

    Scale=DMIN1(Scale,1.1D0)
    Scale=DMAX1(Scale,0.9D0)      

    DO ik=1,nb_CDAN
       this(ik)%rlt=this(ik)%rlt*Scale
       this(ik)%rln=this(ik)%rln*Scale
    END DO

    ! Rnod = [H] Rloc
    DO ik=1,nb_CDAN 
       CALL nullify_reac_(ik,iIreac)
    END DO
    DO ik=1,nb_CDAN
       CALL injj_(ik,this(ik)%rls,this(ik)%rlt,this(ik)%rln,iIreac)
    END DO

  END SUBROUTINE scale_rloc_nlgs
!!! ----------------------------------------------------------------------
 SUBROUTINE get_nlgs3D_loop(compteur,err1,err2,err3,contact)

   IMPLICIT NONE
   INTEGER      :: compteur,contact
   REAL(kind=8) :: err1,err2,err3

   compteur = nlgs_loop
   contact  = nb_CDAN
   err1     = MeanDVoR
   err2     = QuadDV
   err3     = QuadDVR

 END SUBROUTINE get_nlgs3D_loop
!!! ----------------------------------------------------------------------
  SUBROUTINE get_nlgs3D_contact_status(noctc,stick,slide)

    IMPLICIT NONE
    
    INTEGER :: noctc,stick,slide
    
    noctc = Nnoctc
    stick = Nstick
    slide = Nslide
    
  END SUBROUTINE get_nlgs3D_contact_status
!!!------------------------------------------------------------------------  
  SUBROUTINE get_nlgs3D_network_change(nctc,nweak,nstrong)

    IMPLICIT NONE
    INTEGER :: ik,nctc,nweak,nstrong
    
    nctc    = 0
    nweak   = 0
    nstrong = 0
    
    DO ik = 1,nb_CDAN
       IF(this(ik)%status == this(ik)%statusBEGIN) CYCLE
       nctc = nctc + 1
    ENDDO
    
  END SUBROUTINE get_nlgs3D_network_change
!!!---------------------------------------------------------------------
  SUBROUTINE update_tact_behav_nlgs

    IMPLICIT NONE

    INTEGER :: ik
       
    IF (nb_CDAN == 0) RETURN
    
    DO ik=1,nb_CDAN
       CALL update_internal(ik)
    END DO

  END SUBROUTINE update_tact_behav_nlgs
!!!---------------------------------------------------------------------
  SUBROUTINE write_norm_check_nlgs(istat)
    
    IMPLICIT NONE

    INTEGER      :: istat
    integer,save :: norm_fich=0
    logical,save :: norm_check=.FALSE.
    
    SELECT CASE(istat)
    CASE(1)
       norm_check = .TRUE.
       norm_fich  = get_io_unit() 
       OPEN(UNIT=norm_fich,FILE='NORM_NLGS.DAT',STATUS='REPLACE')
    CASE(2)
       IF (norm_check) THEN
          SELECT CASE(i_checktype)
          CASE(i_Quad,i_QuadN)
             WRITE(norm_fich,'(3(2X,D14.7),1(1X,I8))') ABS(meanDVoR),QuadDVR,QuadDV,NOKsta
          CASE(i_Maxm)
             WRITE(norm_fich,'(3(2X,D14.7),1(1X,I8))') ABS(meanDVoR),MaxmDVR,MaxmDV,NOKsta
          END SELECT
       END IF
    CASE(3)
       IF (norm_check) CLOSE(norm_fich)
    END SELECT

  END SUBROUTINE write_norm_check_nlgs
!!!------------------------------------------------------------------------  
  SUBROUTINE update_internal(ik)

    IMPLICIT NONE

                             !123456789012345678912345 
    CHARACTER(len=25) :: IAM='nlgs_3D::update_internal'
    CHARACTER(len=80) :: cout

    INTEGER :: ik,ibehav,ilaw

    REAL(kind=8) :: dut,dun,dus
    
    ibehav=this(ik)%lawnb
    ilaw  =this(ik)%i_law
 
    SELECT CASE(ilaw)
       !123456789012345678901234567890
    CASE(i_MAC_CZM,i_MP_CZM,i_MP3_CZM,i_MAL_CZM,i_TH_CZM,i_ABP_CZM,i_EXPO_CZM,i_EXPO_CZM_P, &
         i_IQS_MAC_CZM,i_IQS_MAL_CZM,i_IQS_TH_CZM,i_IQS_ABP_CZM,i_IQS_EXPO_CZM,i_IQS_EXPO_CZM_P, &
         i_ER_MAC_CZM,i_TOSI_CZM,i_TOSI_CZM_INCRE)
       
       !fd if (this(ik)%status >= i_Cnnow .and. this(ik)%status <= i_Cslid ) then
       ! IF (this(ik)%status(1:1) == 'C') THEN 
          

          CALL updt_CZM_3D(ibehav,.TRUE.,this(ik)%internal,this(ik)%taz,this(ik)%vlt,this(ik)%vln,this(ik)%vls)

       ! ELSE
       !  CALL raz_CZM(ibehav,this(ik)%internal)
       ! END IF
    case(i_IQS_PLAS_CLB)
       call updt_IQS_PLAS(ibehav,this(ik)%internal,this(ik)%rln/H)
    CASE default


   case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)

       ! print*,'updt avec stockage' 
       ! print*,'expoczs allongements'
       ! print*,'dut= ',H*this(ik)%vlt,' dun= ',H*this(ik)%vln,' dus= ',H*this(ik)%vls
       ! print*,' ut= ',H*this(ik)%vlt+this(ik)%internal(2),' un= ',H*this(ik)%vln+this(ik)%gapTTbegin,' us= ',H*this(ik)%vls+this(ik)%internal(4)
      
      !fd mise a jour des sauts de deplacements et de beta 
      call updt_CZM_spring_3D(ibehav,.true.,this(ik)%internal,this(ik)%taz,H*this(ik)%vlt,(this(ik)%gapTTbegin-this(ik)%internal(9))+H*this(ik)%vln,H*this(ik)%vls, &
                              this(ik)%rlt,this(ik)%rln,this(ik)%rls)   
							  
   case(i_EXPO_CZM_SPRING_P,i_IQS_EXPO_CZM_SPRING_P)

       ! print*,'updt avec stockage' 
       ! print*,'expoczs allongements'
       ! print*,'dut= ',H*this(ik)%vlt,' dun= ',H*this(ik)%vln,' dus= ',H*this(ik)%vls
       ! print*,' ut= ',H*this(ik)%vlt+this(ik)%internal(2),' un= ',H*this(ik)%vln+this(ik)%gapTTbegin,' us= ',H*this(ik)%vls+this(ik)%internal(4)
      
      !fd mise a jour des sauts de deplacements et de beta 
      call updt_CZM_spring_p_3D(ibehav,.true.,this(ik)%internal,this(ik)%taz,H*this(ik)%vlt,(this(ik)%gapTTbegin-this(ik)%internal(9))+H*this(ik)%vln,H*this(ik)%vls, &
                              this(ik)%rlt,this(ik)%rln,this(ik)%rls)
       
    END SELECT
    
    ! Sending local data
    call set_internal( this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

  END SUBROUTINE update_internal
!!!---------------------------------------------------------------
  SUBROUTINE reverse_nlgs

    IMPLICIT NONE    

    INTEGER :: ik

    IF (nb_CDAN == 0) RETURN

    DO ik=1,nb_CDAN
       ialeatr(ik)=ialeat(ik)
    END DO
    DO ik=1,nb_CDAN
       ialeat(ik)=nb_CDAN-ialeatr(ik)+1
    END DO

  END SUBROUTINE reverse_nlgs
!!!------------------------------------------------------------------------ 
  SUBROUTINE scramble_nlgs
   
   IMPLICIT NONE
   
   INTEGER       :: ik
   INTEGER       :: IALEATik,IAL1,IAL2
   REAL(kind=8)  :: RA
   
   DO ik=1,nb_CDAN/2
      !
      CALL RANDOM_NUMBER(RA)
      IAL1=IDINT(RA*REAL(nb_CDAN,8))+1
      IAL1=MIN0(IAL1,nb_CDAN)
      IAL1=MAX0(1,IAL1)
      
      CALL RANDOM_NUMBER(RA)
      IAL2=IDINT(RA*REAL(nb_CDAN,8))+1
      IAL2=MIN0(IAL2,nb_CDAN)
      IAL2=MAX0(1,IAL2)
      
      IALEATik= IALEAT(IAL1)
      IALEAT(IAL1)=IALEAT(IAL2)
      IALEAT(IAL2)=IALEATik
      !
   END DO
   
 END SUBROUTINE scramble_nlgs
!!!------------------------------------------------------------------------ 
  SUBROUTINE quick_scramble_nlgs

   ! This subroutine scrambles the ordering of candidates for contact

    IMPLICIT NONE
    INTEGER       :: ik,IALEATik
    
    DO ik=1,nb_CDAN
       
       IALEATik=IALEAT(ik)
       IALEAT(ik)=IALEAT(randomlist(ik))
       IALEAT(randomlist(ik))=IALEATik
       
    END DO
    
  END SUBROUTINE quick_scramble_nlgs
!!!---------------------------------------------------------------------- 
 SUBROUTINE active_diagonal_resolution
   IMPLICIT NONE

   diagonal_resolution = .TRUE.

 END SUBROUTINE active_diagonal_resolution
!!!mr------------------------------------------------------------------------
!!$ SUBROUTINE init_cohe_nlgs_3D
!!$
!!$   IMPLICIT NONE
!!$                            !123456789012345678901234
!!$   CHARACTER(len=24) :: IAM='nlgs_3D::update_internal'
!!$   CHARACTER(len=80) :: cout
!!$   INTEGER      :: ik,ibehav
!!$   REAL(kind=8) :: cohe,normalcoh,tangalcoh,Wethk,cd_surf
!!$
!!$   DO ik=1,nb_CDAN
!!$      SELECT CASE (this(ik)%CDAN)
!!$      CASE (i_SPSPx)
!!$         CALL update_cohe_SPSPx(this(ik)%icdan,cohe)
!!$      CASE (i_SPPLx)
!!$         CALL update_cohe_SPPLx(this(ik)%icdan,cohe)
!!$      CASE (i_SPCDx)
!!$         CALL update_cohe_SPDCx(this(ik)%icdan,cohe)
!!$      CASE default
!!$         WRITE(cout,'(I5,A31)') this(ik)%CDAN,' is not implemented'
!!$         CALL FATERR(IAM,cout)
!!$      END SELECT
!!$
!!$      ibehav = this(ik)%lawnb
!!$
!!$      SELECT CASE(this(ik)%i_law)
!!$      CASE(i_IQS_WET_DS_CLB)
!!$         CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
!!$         normalcoh = cohe
!!$         IF (this(ik)%gapTTbegin .LE. Wethk) THEN
!!$!            this(ik)%covfrees = -normalcoh*this(ik)%Wsn*H
!!$!            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
!!$            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + MAX(0.D0,this(ik)%gapTTbegin/H)
!!$            this(ik)%corln    = -normalcoh*H
!!$         END IF
!!$      CASE(i_IQS_MOHR_DS_CLB)
!!$
!!$         IF (this(ik)%statusBEGIN == i_Mstck) THEN
!!$           IF (this(ik)%CDAN == i_PRPLx) THEN
!!$              cd_surf = get_surf_PRPLx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_PRPRx) THEN
!!$              cd_surf = get_surf_PRPRx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_SPSPx) THEN
!!$              cd_surf = get_surf_SPSPx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_SPPLx) THEN
!!$              cd_surf = get_surf_SPPLx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_SPCDx) THEN
!!$              cd_surf = get_surf_SPCDx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_SPDCx) THEN
!!$              cd_surf = get_surf_SPDCx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_CDPLx) THEN
!!$              cd_surf = get_surf_CDPLx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_CDCDx) THEN
!!$              cd_surf = get_surf_CDCDx(this(ik)%icdan)
!!$           ELSE IF (this(ik)%CDAN == i_PTPT3) THEN
!!$              cd_surf = get_surf_PTPT3(this(ik)%icdan)
!!$           ELSE
!!$              call faterr(IAM,'this contact element doesn t work with'//tact_behav(ibehav)%lawty)
!!$           END IF
!!$
!!$           CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
!!$           normalcoh = cohe*cd_surf
!!$           this(ik)%covfrees = -normalcoh*this(ik)%Wsn*H
!!$           this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
!!$           this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + MAX(0.D0,this(ik)%gapTTbegin/H)
!!$           this(ik)%corln    = -normalcoh*H
!!$         END IF
!!$      CASE(i_ELASTIC_REPELL_WET_CLB)
!!$         CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
!!$         normalcoh = cohe
!!$         this(ik)%covfreen = this(ik)%gapTTbegin/H
!!$         this(ik)%covfrees = 0.D0
!!$         this(ik)%covfreet = 0.D0
!!$         IF (this(ik)%gapTTbegin .LE. Wethk) THEN
!!$            this(ik)%covfrees = this(ik)%covfrees - normalcoh*this(ik)%Wsn*H
!!$            this(ik)%covfreet = this(ik)%covfreet - normalcoh*this(ik)%Wtn*H
!!$            this(ik)%covfreen = this(ik)%covfreen - normalcoh*this(ik)%Wnn*H
!!$            this(ik)%corln    =-normalcoh*H
!!$         END IF
!!$      CASE DEFAULT
!!$         
!!$      END SELECT
!!$
!!$   END DO
!!$
!!$ END SUBROUTINE init_cohe_nlgs_3D
!!!----------------------------------------------------------------------
!------------------------------------------------------
 subroutine coupled_dof_solver_(WWtt,WWts,WWtn,WWst,WWss,WWsn,WWnt,WWns,WWnn,vvlocfreet,vvlocfrees,vvlocfreen,  &
                                sstatus,rrlt,rrls,rrln)

   IMPLICIT NONE
   REAL(kind=8)     :: WWtt,WWts,WWtn,WWst,WWss,WWsn,WWnt,WWns,WWnn, &
                       vvlocfreet,vvlocfrees,vvlocfreen, &
                       rrlt,rrls,rrln

   integer(kind=4)  :: sstatus

   INTEGER                     :: err
   REAL(kind=8),dimension(3,3) :: A
   REAL(kind=8),dimension(3)   :: b

                             !123456789012345678901234567890
   CHARACTER(len=27)  :: IAM='nlgs_3D::coupled_dof_solver'

   A(1,1) = WWtt; A(1,2) = WWts; A(1,3) = WWtn
   A(2,1) = WWst; A(2,2) = WWss; A(2,3) = WWsn
   A(3,1) = WWnt; A(3,2) = WWns; A(3,3) = WWnn

   !print*,'W'
   !write(*,'(3(1x,D12.5))') A

   call inverse33(A,err)

   !print*,'W^-1'
   !write(*,'(3(1x,D12.5))') A

   if (err == 1) then
     print*,A(1,:)
     print*,A(2,:)
     print*,A(3,:)
     call faterr(IAM,'Non inversible matrix')
   endif

   b(1)=vvlocfreet
   b(2)=vvlocfrees
   b(3)=vvlocfreen
   
   b = matmul(A,b)

   !print*,'R'
   !write(*,'(3(1x,D12.5))') b

   rrlt=-b(1)
   rrls=-b(2)
   rrln=-b(3)

   sstatus=i_stick
 
 end subroutine coupled_dof_solver_
!------------------------------------------------------
 subroutine assume_is_initialized(is_init)
   implicit none
   integer, intent(in) :: is_init
 
   if( is_init > 0 ) then
     is_initialized = .true.
   else
     is_initialized = .false.
   end if

 end subroutine

!------------------------------------------------------------------------
!am : nouvelles fonctions pour rendre possible un calcul multi-domaines sequentiel
!     dans le cadre de l'architecture actuelle
!------------------------------------------------------------------------

!------------------------------------------------------------------------  
 subroutine compute_local_free_vlocy(list_INTRF)
   implicit none

   ! optional input
   integer, dimension(:), intent(in), optional :: list_INTRF  

   ! locals
   integer :: i, ik

   if (nb_CDAN == 0) return

   if (.not. present(list_INTRF)) then
      do ik=1, nb_CDAN
         CALL prjj_(ik, this(ik)%vfrees, this(ik)%vfreet, this(ik)%vfreen, iVfree)
      end do
   else
      do i=1, size(list_INTRF)
         ik=list_INTRF(i)
         call prjj_(ik, this(ik)%vfrees, this(ik)%vfreet, this(ik)%vfreen, iVfree)
      end do
   end if

 end subroutine compute_local_free_vlocy
!------------------------------------------------------------------------
 subroutine display_tacinfo(ik)
   implicit none
   integer :: ik,icdbdy,ianbdy
   CHARACTER(len=120)         :: cout 

   write(cout,'(A,I0)') 'contact: ',ik
   CALL LOGMES(cout)
   write(cout,'(I0,1x,I0)') this(ik)%CDAN,this(ik)%icdan !,i_CSPRx,i_CSASp
   CALL LOGMES(cout)
   
   !select case( this(ik)%CDAN )
   !case( i_SPSPx, i_SPPLx, i_PRPRx, i_PRPLx, i_CSASp, i_PRASp, i_CSPRx )

   !   icdbdy = get_icdbdy_inter_( this( ik )%CDAN, this( ik )%icdan )
   !   ianbdy = get_ianbdy_inter_( this( ik )%CDAN, this( ik )%icdan )

   !case default 

   !   write( cout, * ) '[nlgs_3D::tacinfo] : unknown contact type ', this( ik )%CDAN
   !   call logmes( cout )

   !end select
        
   write(cout,'(A)') '[nlgs_3D::display_tact_info] cannot get support body index'
   !write(cout,'(I0,1x,I0)') icdbdy,ianbdy
   CALL LOGMES(cout)

   write(cout,'(A)') 'W'
   CALL LOGMES(cout)
   write(cout,'(3(1x,D12.5))') this(ik)%Wtt,this(ik)%Wtn,this(ik)%Wts
   CALL LOGMES(cout)
   write(cout,'(3(1x,D12.5))') this(ik)%Wnt,this(ik)%Wnn,this(ik)%Wns
   CALL LOGMES(cout)
   write(cout,'(3(1x,D12.5))') this(ik)%Wst,this(ik)%Wsn,this(ik)%Wss
   CALL LOGMES(cout)
   write(cout,'(A)') 'Vfree'
   CALL LOGMES(cout)
   write(cout,'(3(1x,D12.5))')  this(ik)%vfreet, this(ik)%vfreen, this(ik)%vfrees
   CALL LOGMES(cout)
   write(cout,'(A)') 'co Vfree'
   CALL LOGMES(cout)
   write(cout,'(3(1x,D12.5))') this(ik)%covfreet, this(ik)%covfreen, this(ik)%covfrees
   CALL LOGMES(cout)

   !select case ( this( ik )%CDAN )
   !case( i_CSASp, i_CSPRx )
   !   call display_vlocy_inter_( this( ik )%CDAN, this( ik )%icdan, iVfree )

   !end select

 end subroutine
!!!------------------------------------------------------------------------

 !vv: Get du nombre de W_alpha_beta avec alpha/=beta, non nuls (a-priori)
 function get_nb_adjac_nlgs_3D()
   implicit none
   integer(kind=4) :: get_nb_adjac_nlgs_3D

   get_nb_adjac_nlgs_3D=nb_adjac
 end function get_nb_adjac_nlgs_3D
   
 subroutine print_info(ik)
   
   implicit none
   
                            !1234567890123456
   character(len=16) :: IAM='nlgs::print_info'
   character(len=80) :: cout
   
   integer :: ik
  
   write(cout,1) get_interaction_name_from_id(this(ik)%CDAN),this(ik)%icdan
   call LOGMES(cout)

   select case (this(ik)%CDAN)
   case (i_SPSPx)

   case (i_SPPLx) 

   case (i_SPPRx) 
      call print_info_SPPRx(this(ik)%icdan)
   case (i_PRPLx) 

   case (i_PRPRx) 
      call print_info_PRPRx(this(ik)%icdan)

   case (i_PTPT3) 
      
   case (i_CSPRx) 

   case (i_CSASp)
      call print_info_CSASp(this(ik)%icdan)
   case default
      write(cout,'(I0,A31)')this(ik)%CDAN,' is not implemented'
      call FATERR(IAM,cout)
   end select
   
1  format(1X,'Interaction de type: ',A,' rang: ',I0)
   
 end subroutine print_info

 subroutine use_jacobi_solver( jacobi )
    implicit none
    logical :: jacobi
    
    JACOBI_SOLVER = jacobi
 end subroutine 

 subroutine use_regul(v1,v2)
    implicit none
    real(kind=8) :: v1,v2
    
    regul=.TRUE.
    krn=v1
    krt=v2
    
 end subroutine use_regul

 !------------------------------------------------------------------------
 subroutine get_external_pressure_(ik,pext)

   implicit none
                            !123456789012345678901234567
   character(len=27) :: IAM='nlgs::get_external_pressure'
   character(len=80) :: cout
   
   integer :: ik

   real(kind=8),intent(out) :: pext

   call get_external_pressure(this(ik)%CDAN, this(ik)%icdan, pext)

 end subroutine get_external_pressure_

 !fd 01-2021 gestion des appaiements pourris avec la loi expo_czm 
 
 !-------------------------------------
 subroutine use_cut_open_czm( tol )
    implicit none
    real(kind=8) :: tol
    
    cut_open_czm = .True.    
    open_czm_tol = tol

 end subroutine use_cut_open_czm

 !-------------------------------------
 subroutine use_manage_interpenetrated_czm()  
   implicit none
   manage_interp = .True.
 end subroutine use_manage_interpenetrated_czm
 
END MODULE NLGS_3D

