!==========================================================================
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
module nlgs

!mj
!mj 1)04-05-20 
!mj Introduction du rapport dynstat, chercher le mot cle dynstat.
!mj Ce rapport est cense mesure le taux de "dynamique" par rapport a la "quasistatique"
!mj ne donne pas jusqu'ici des renseignements bien clairs. 
!mj
!mj 2) 
!mj En contemplant l'exemple CSF10000 de !mr, il m'est tombe dans l'oeil un exemple antologique de violation.  
!mj Il existe plusieurs types de violation.
!mj   a) Non existence de solution, a cause par exemple de liaisons cinematiques; l'algorithme mouline et atteint le
!mj   nombre maximal d'iterations autorise. Une violation est creee.
!mj   b) Une detection est manquee, par exemple la distance d'alerte est trop petite. On y reviendra.
!mj   c) Voici l'exemple qui nous occupe : "le syndrome du nouvel arrivant".
!mj Dans l'exemple CSF10000, un grain musarde sur la surface libre, la gravite le fait basculer et il s'apprete a s'avachir
!mj sur un grain voisin. Un nouveau contact s'etablit. Le malheureux grain agresse doit reagir par une impulsion. 
!mj Comme le contact est nouveau, aucune information concernant la reaction n'est disponible en archive dans verlet,
!mj sauf peut etre une information vetuste. Ainsi l'algorithme prends pour ce contact une valeur de debut d'iteration nulle.
!mj Le critere usuel avec lequel les valeurs iterees sont estimees est celui de la norme quadratique. Nous sommes en 
!mj presence d'une collection de n+1 contacts, les reactions des n (ou presque) contacts deja actifs ayant ete bien estimees 
!mj au cours des iterations precedentes. Peu d'iterations sont necessaires pour ces habitues, et la violation du nouvel arrivant
!mj compte pour 1/n (n=10000) dans le lot. Consequence, les iterations s'arretent avant que la reaction du nouvel contact ait pu
!mj atteindre une valeur convenable. Et des penetrations s'en suivent au cours des pas, jusqu'a ce que la reaction du nouveau 
!mj contact atteigne une valeur convenable. 
!mj Un test montre que si l'on utilise la norme du max, cet accident ne se produit pas.
!mj Evidemment la norme quadratique est laxiste et la norme du max est severe. 
!mj Je suis encleint a penser que cette perversion se produit dans le probleme du ballast, ou, apres une decharge du blochet, de 
!mj nouveaux arrivant sont attendus lors de la charge suivante.
!mj On verra sous la commande AFTER ITER CHECK, comment on peut adopter differents criteres (variable checktype).  
!mj Pour la syntaxe, voir plus bas. Un WARNING donne des instructions et indique la modification mineure a effectuer dans 
!mj le fichier de commandes. Voir exemples. 
!mj On verra que, outre le critere classique (checktype ='Quad '), on peut essayer le critere  (checktype ='Maxm '), et un
!mj critere  (checktype ='QM/16') qui est un compromis entre les deux. Ce dernier critere qui controle la norme quadratique 
!mj et en partie le norme du max a donne de bons resultats sur CSF10000 sans trop augmenter le temps de calcul.
!mj
!mj 3) mrs-04-04-18
!mj Il est apparu une equivoque dans la notion de "contact actif" et de "non contact". En effet un (candidat au) contact actif 
!mj est usuellement compris comme un contact ou la reaction est non nulle. Dans l'unilateralite classique (au sens de Signorini)
!mj le statut de non contact i_noctc s'accompagne d'une reaction nulle. Il n'en est pas de meme dans le cas de lois de type WET
!mj ou le statut i_Wnctc peut indiquer un interstice strictement positif, alors que s'exerce encore une force de traction 
!mj si cet interstice est inferieur a la distance Wthck. On peut imaginer d'autres cas. Je n'ai pas change la definition 
!mj des statuts. On les verra apparaitre comme precedemment dans Vloc_Rloc.LAST etc. 
!mj   J'ai simplement change la maniere de comptabiliser les statuts dans DISPLAY AFTER ITER CHECK, voir mes commentaires 
!mj plus bas. J'ai ete ammene a introduire Nvnish,le nombre de contacts ou la reaction s'annule (comme vanish), et Nhover 
!mj le nombre de contacts a interstices positifs, a reaction nulle ou pas, (en Anglais, to hover, survoler a basse altitude, 
!mj exemple hovercraft).
!mj La procedure skip est maintenant pilotee par les reactions nulles et inoctc est devenu ivnish.
!mj
!mj 4) mrs-04-12-12
!mj Introduction de La procedure de correction d'interpenetration intempestive dite "Radjai Gap Rescue", suivre le mot cle RGR,
!mj voir doc en cours.

  ! shared modules

  use utilities, only : logmes, &
                        faterr

  use overall

  use LMGC90_MPI, only : DDM_SCHWARTZ, &
                         nb_procs_COMM_WORLD

  use parameters

  use tact_behaviour


  use inter_meca_handler_2D, only : get_nb_inters    , &
                                    set_loc          , &
                                    get_rloc         , &
                                    get_vlocBEGIN    , &
                                    get_local_frame  , &
                                    get_internal     , &
                                    set_internal     , &
                                    inter2ENT        , &
                                    get_tact_lawnb   , &
                                    injj             , &
                                    prjj             , &
                                    vitrad           , &
                                    nullify_reac     , &
                                    nullify_vlocy    , &
                                    get_length       , &
                                    get_eff          , &
                                    set_violation    , &
                                    update_cohe      , &
                                    update_fric      , &
                                    print_info       , &
                                    get_external_pressure



  use clalp, only : get_bulk_strain_clalp, &
                    get_bulk_stress_clalp, &
                    get_bulk_strain_triaxiality_clalp, &
                    get_bulk_stress_triaxiality_clalp, &                    
                    get_bulk_strain_rate_triaxiality_clalp, &
                    get_bulk_temperature_clalp


  
  implicit none
  
  private
  
  type T_ctct_element
 
     !> type of contact ik
     integer                    :: CDAN
     !> serial type number in type CDAN of contact element ik     
     integer                    :: icdan
     
     !> components of the local contact impulse
     real(kind=8)               :: rlt, rln
     ! components of  local contact impulse (at previous jacobi iteration)     
     real(kind=8)               :: rlt_jacobi, rln_jacobi
     
     !> components of the relative velocity at the beginning of the time step
     real(kind=8)               :: vltBEGIN, vlnBEGIN
     !> components of the relative velocity          
     real(kind=8)               :: vlt, vln

     !> components of the complementary relative velocity
     real(kind=8)               :: corlt, corln

     !> status at the beginning of the time step     
     integer(kind=4)            :: statusBEGIN
     !> contact status
     integer(kind=4)            :: status
     !> contact status while checking convergence     
     integer(kind=4)            :: statuscheck
     
     !> components of genuine local dynamic matrix W
     real(kind=8)               :: Wtt, Wtn
     real(kind=8)               :: Wnt, Wnn
     !> components of auxiliary local dynamic matric WW     
     real(kind=8)               :: WWtt, WWnn
     !> det of auxiliary local dynamic matrix     
     real(kind=8)               :: det
     !> auxiliary computation for mu_sc_std          
     real(kind=8)               :: forward, backward      
     !> genuine free velocity components
     real(kind=8)               ::   vfreet, vfreen
     !> complementary free velocity components to add togenuine free velocity components to get auxiliaries     
     real(kind=8)               :: covfreet, covfreen
     
     !> gap at the beginning of the time step
     real(kind=8)               :: gapTTbegin
     !> reference gap used for far contact laws     
     real(kind=8)               :: gapREF

     !> set to 1 'acton' (reactions have to be computed) or 0 'noact' (reactions are to be found null)
     !> according to some forecast criterion
     integer(kind=4)            :: forecast
     ! internal flags to manage forcast     
     integer                    :: ivnish,iskip

     !> rank of contact law                                     
     integer                    :: lawnb
     !> kind of contact law     
     integer                    :: i_law     
     !> friction coefficient of contact     
     real(kind=8)               :: fric
     ! mapping for sdl solver
     !> cd and an rank in entity list  
     integer                       :: icdent,ianent
     !> number of adjacent contacts to ik     
     integer                       :: nbadj
     !> rank of an adjacent contacts (in the overall contact list)
     ! jl = this(ik)%adjjl(p) where p is the pth adjacent contact to ik
     integer,dimension(:),pointer  :: adjjl
     !> rank of contact ik in the list of adjacent contacts
     ! if jl is the pth adjacent of ik, then ik = this(jl)%adjjl(this(ik)%rev_adj(p)) 
     integer,dimension(:),pointer  :: rev_adj
     !> index of Wikjl's storage chunk in the Wab matrix 
     INTEGER(kind=4)              :: istart

     ! wear
     ! Wear coefficient of the candidate and the antagonist body
     real(kind=8)               :: kcdwear,kanwear      
     real(kind=8)               :: threshold 

     !> internal variables
     real(kind=8),dimension(max_internal_tact) :: internal
     ! taz - working values (taz(1): beta, taz(2): ep, taz(3): pext)
     real(kind=8),dimension(max_taz)                 :: taz
     
     !fd treatement of periodic conditions it needs the local frame 
     real(kind=8)               :: nx,ny

     !> 0 for weak ; 1 for strong
     integer                    :: ws

     !> in CRITICAL_VOIGT_CLB, DYNAMIC_VOIGT_CLB velocity of tangential micro pad
     REAL(kind=8)               :: vepad

     !fd-wip -- currently disabled

     !> is_not_null =0 if reaction is equal to 0. else 1
     ! used to avoid matmul when rl is equal to 0
     integer                    :: is_not_null

     !> is_in_queue = 0 if the contact doesn't need to be reevaluated else 1
     ! used to skip computation of a contact if nothing has changed for it
     integer                    :: is_in_queue

  end type T_ctct_element

  type (T_ctct_element),dimension(:),allocatable  ::  this  
  
  ! About Wab ----------------------------------------------------------------------
  ! Notations are related to the way the matrix is build in the code ;
  ! we loop on ik which is the second index of the matrix jl is the first. 
  ! Wab stores each Wjl,ik matrices in a vector form (diagonal terms are not stored here).
  ! this(jl)%istart contains the index of the chunk of Wjl,ik storage zone
  ! iadj is the rank of ik in the adjacent contacts list of jl
  ! each term is stored in the following way
  
  !  W(this(ik)%istart+4*iadj-3) = WTT  W(this(ik)%istart+4*iadj-2) = WTN
  !  W(this(ik)%istart+4*iadj-1) = WNT  W(this(ik)%istart+4*iadj  ) = WNN

  ! Wab is computed noting that if we take a dR vector null everywhere except on one index ik
  ! then dU = W dR will contain the ikth column of W.
  ! dU is computed using a local to global to local exchange.

  real(kind=8),dimension(:),allocatable         :: Wab

  ! variable indicating if the Wab Delassus matrix is built or not
  character(len=30)  :: Wstorage          
  logical            :: SDLactif=.false.
  
  !----------------------------------------------------------------------------------------------------

  integer, private                        :: nb_CDAN,nb_ENTITY

  ! to use jacobi solver instead of nlgs solver
  logical                                 :: JACOBI_SOLVER=.false.

  integer,      dimension(:), allocatable :: ialeat,ialeatr,iwksg,randomlist
  real(kind=8), dimension(:), allocatable :: Xvlton,WRRarray

  integer            :: Nnoact,NOKsta,Nactif,Nstick,Nslide
  integer            :: Nvnish,Nhover,Nnoctc,Ntract,Ncompr,Nnknow,Nb_RGR
  integer            :: NOKweak,NOKstrg,SNstick,SNslide,WNstick,WNslide

  !mj randomlist

  integer      :: IAL1
  real(kind=8) :: RA

  real(kind=8) :: somme_rn
  real(kind=8) :: HH
  
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

  real(kind=8) :: DVDV,DVDVRR,DVoR,WRR

  !!!  Summation on the collection of candidates for contact 
  !!!
  !!!  SumDVDV   : summation of DVDV
  !!!  maxDVDV   : maximal DVDV value
  !!!  SumDVDVRR : summation of DVDVDRR
  !!!  maxDVDVRR : maximal DVDVRR value
  !!!  SumDVoR   : summation of DVoR
  !!!  SumWRR    : summation of WRR
  !!!  SumWRWR   : summation of WRWR

  real(kind=8) :: SumDVDV,MaxDVDV,SumDVDVRR,MaxDVDVRR,SumDVoR,SumWRWR,SumWRR

  !!!  QuadWR    : quadratic mean of WR
  !!!  QuadDV    : quadratic mean of DV
  !!!  MaxmDV    : maximal DV value
  !!!  MeanWRR   : mean energy
  !!!  QuadDVR   : quadratic mean of DVR
  !!!  MaxmDVR   : maximal DVR value
  !!!  MeanDVoR  : mean value of DVR (algebraic value, a measure of the average penetration in the direction of R)

  real(kind=8) :: QuadWR,QuadDV,MaxmDV,MeanWRR,QuadDVR,MaxmDVR,MeanDVoR

  !!!mj
  !!!  dynstat   : One defines the "relative acceleration force" as W^(-1)(Vl - VlBegin)/H, i.e. the local mass matrix
  !!!            : multiplied by the relative velocity jump divided by the time step. The energy of this force is   
  !!!            : W^(-1)(Vloc - VlocBegin)/H * 0.5D0*(Vloc + VlocBegin) * H
  !!!            : A summation of the absolute value of this power is made on all contacts and divided by SumWRR.
  !!!            : This is the definition of dynstat. This coefficient is small if the relative velocities jumps are small.
  !!!            : This coefficient is supposed to give an idea of dynamical effects WITHIN the collection of contacts.
  !!!            : It has nothing to do with kinetic or static energy. Note also that the words "relative acceleration force"
  !!!            : is a nickname and has nothing to do with the second derivative of a relevant mechanical term. 
  real(kind=8) :: dynstat

  !!!  Dreac     : "reacteristic distance" (distance run on the effect of the only impulse R)
  REAL(kind=8) :: Dreac

  real(kind=8) :: rcvltm,Dcrac,Scale=1.D0
  
  real(kind=8) :: tol=1.D-04,RELAX=1.D0,RELAX1=0.D0

  ! variable indicating the type of iter check test
  integer           :: i_checktype=1                           

  ! variable indicating the type of iter check test
  character(len=5)  :: checktype                               
  
  !!! parameter table -------------------------------------------------------------

  !!! nlgs check keyword

  integer,parameter :: i_Quad = 1 , i_Maxm = 2 , i_QMs16 = 3 , i_QuadN = 4

  !!! nlgs keyword

  integer,parameter :: i_iter = 1 , i_check = 2 , i_post = 3

  ! RGR CRITIC

  real(kind=8),parameter :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)

  integer      :: nlgs_loop

  ! map between interaction serial number in dedicated modules (DKDKx, DKJCx, etc) and interaction serial number in this module.
  ! For example, for a given interaction index in DKDKx (icdan_DKDKx), the coresponding index in this module (icdan_nlgs) is :
  !    icdan_nlgs = icdan_DKDKx + shift_icdan(i_dkdkx)
  ! N.B.: if there is no interaction for a given interaction type then shift_icdan=-1
  !       for example, if there's no DKJCx, then : shift_icdan(i_dkjcx) == -1
  !fd-todo : cette gestion est totalement debile !!
  integer, dimension(nb_interaction_types) :: shift_icdan

  !vv: Nombre de W_alpha_beta avec alpha/=beta, non nuls
  INTEGER :: nb_adjac = 0

  !fd-wip gestion regul
  logical      :: regul = .FALSE.
  real(kind=8) :: krn=0.d0,krt=0.d0

  !fd asserts that the solver was not run => needs initializing internals, etc
  !fd-todo virer les tests avec nstep==1
  logical      :: is_initialized = .false.

  integer(kind=4),parameter :: i_acton=1,i_noact=0
  
  public shift_icdan

  public &
       bimodal_list_nlgs, &
       comp_check_nlgs, &
       display_check_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       scale_rloc_nlgs, &
       scramble_nlgs, &
       solve_nlgs, &
       write_norm_check_nlgs, &
       RnodHRloc_nlgs, &
       VnodHVloc_nlgs, &
       compute_local_free_vlocy, &
       display_rlocn_sum_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs, &
       assume_is_initialized, &
       update_cohe_nlgs, &
       update_fric_nlgs, &
       get_error, &
       !vv:
       get_nb_adjac_nlgs_2D, &
       !am: functions for check convergence in DDM
       compute_convergence_norms_nlgs, & 
       put_convergence_norms_nlgs, &
       check_convergence_nlgs , &
       use_jacobi_solver, &
       !fd-wip
       use_regul, &
       !fd-wip access to pext in taz
       set_temporary_variable_nlgs, &
       get_temporary_variable_nlgs


  public &
       !post traitement
       get_nlgs2D_loop, get_nlgs2D_network_change,get_nlgs2D_contact_status,&
       get_after_iter_check, get_somme_rn, &
       get_all_this

  private prjj_, injj_, vitrad_, &
          nullify_reac_              , &
          nullify_vlocy_             , &
          mu_SC_std_solver_          , &
          mu_SC_wear_solver_         , &
          coupled_dof_solver_        , &
          plastic_coupled_dof_solver_, &
          get_external_pressure_          

  private prep_nlgs_aux_

contains
!------------------------------------------------------------------------
  subroutine prep_nlgs_aux_( id_inter, nb_inter, reac_mean, nb_CDAN )
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
       
       if (icdent /= ianent) then       
          entity( icdent )%ik                      = entity( icdent )%ik + 1
          entity( ianent )%ik                      = entity( ianent )%ik + 1
          entity( icdent )%list(entity(icdent)%ik) = ik
          entity( ianent )%list(entity(ianent)%ik) = ik
       else 
          ! pour ne pas se compter 2 fois 
          entity( icdent )%ik                      = entity(icdent)%ik+1
          entity( icdent )%list(entity(icdent)%ik) = ik
          if( id_inter == i_p2p2l ) entity(ianent)%list( entity(ianent)%ik ) = ik
       end if

    end do
    nb_CDAN = nb_CDAN + nb_inter

  end subroutine prep_nlgs_aux_

 !!!------------------------------------------------------------------------  
 subroutine prep_nlgs(actif)

   implicit none

   logical, intent(in) :: actif

   ! common part
                            !123456789012345
   character(len=15) :: IAM='nlgs::prep_nlgs'
   character(len=80) :: cout
   integer           :: errare=0
   
   integer           :: ik,ibehav,icdan,i
   
   integer           :: ient !sdl

   real(kind=8)      :: forward,backward,nbc,TshVlt
   real(kind=8)      :: vtik,vnik,rtik,rnik,det

   ! contactors part 

   integer           :: nb_DKDKx,nb_DKKDx,nb_DKJCx
   integer           :: nb_PTPT2,nb_PLPLx,nb_PLJCx  !fd ,nb_P2P2x
   integer           :: nb_CLJCx,nb_CLALp,nb_P2P2L,nb_PLALp,nb_DKPLx,nb_DKALp,nb_DKDKL

   ! behaviours part

   real(kind=8)      :: fric,                         & ! friction coefficient
                        tangalrest,normalrest,        & ! restitution coefficients
                        normalcoh,tangalcoh,Wethk,    &
                        forcePERgap,forcePERstrain,   &
                        forcePERstrainrate,prestrain, &
   ! RGR CRITIC
                        ToverH,OTH,vOVERcv, &
                        gap_tol,meff,reff,etan,etat,vloc,Freqik

   logical           :: pret=.false.,inter,is_present=.false.,ok
   
   integer           :: jl,iadj,jadj,nbadj,jll,icdik,ianik,bandwidth,icdent,ianent,istart,iistart
   
   integer           :: ikadj,ikstart,jladj,jlstart
   
   real(kind=8)      :: cd_length,thickness ! mac_czm

   ! bimodal list
   real(kind=8)      :: reac_mean = 0.D0
   integer           :: nb_WEAK,nb_STRONG
   
   !local periodic computation
   real(kind=8)              :: Y_x,Y_y,EY_x,EY_y
   real(kind=8),dimension(3) :: E

   !mj randomlist

   integer      :: IAL1
   real(kind=8) :: RA
   
   real(kind=8) :: un
   real(kind=8) :: pre_gap, g0, snmax

   logical      :: need_allocation = .true.

   integer( kind = 4 ) :: id_inter

   !< nard_rod
   real(kind=8) :: Ksn,Kst,Kvn,Kvt,Dtmax
   ! nard_rod />

   ! Modif 3 : Relatif à Modif 1
   real(kind=8),dimension(9) :: vec  ! Tenseur volumique
   real(kind=8) :: ts
   integer :: ios 
   ! Fin Modif 3
      
   SDLactif    = actif
   nlgs_solver2D = .true.
   nlgs_loop   = 0

   if ( JACOBI_SOLVER .and. RELAX >0.999d0 ) then
     CALL LOGMES('WARNING!! Usage of a jacobi solver with RELAX=1 may not converge.')
   endif

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

   !fd mod_P2P2x
   !fd   nb_P2P2x = get_nb_P2P2x()
   !fd   nb_CDAN = nb_CDAN + nb_P2P2x

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
   nb_CDAN = nb_CDAN + nb_DKDKL

   if (nb_CDAN == 0) return 
  
   if (get_with_experimental_dev()) then
     need_allocation = .false. 
     !am : si this n'est pas alloue
     if (.not. allocated(this)) then
        ! on doit faire des allocations memoire
        need_allocation = .true.
     ! sinon,
     else
        ! on teste que sa taille soit suffisante
        if (nb_cdan > size(this)) need_allocation = .true. 
     end if
   else      
     need_allocation = .true.
   endif

   if (need_allocation) then

     if (allocated(ialeat)) deallocate(ialeat)
     allocate(ialeat(nb_CDAN),stat=errare)
     if (errare /= 0) then
        call FATERR(IAM,'error allocating ialeat')
     end if
   
     if (allocated(ialeatr)) deallocate(ialeatr)
     allocate(ialeatr(nb_CDAN),stat=errare)
     if (errare /= 0) then
       call FATERR(IAM,'error allocating ialeatr')
     end if
   
     if (allocated(iwksg)) deallocate(iwksg) 
     allocate(iwksg(nb_CDAN),stat=errare)
     if (errare /= 0) then
       call FATERR(IAM,'error allocating iwksg')
     end if
   
     !!!mj randomlist
   
     if (allocated(randomlist)) deallocate(randomlist)
     allocate(randomlist(nb_CDAN),stat=errare)
     if (errare /= 0) then
       call FATERR(IAM,'error allocating randomlist')
     end if
   
     if (allocated(Xvlton)) deallocate(Xvlton)
     allocate(Xvlton(nb_CDAN),stat=errare)
     if (errare /= 0) then
        call FATERR(IAM,'error allocating Xvlton')
     end if
   
     if (allocated(WRRarray)) deallocate(WRRarray)
     allocate(WRRarray(nb_CDAN),stat=errare)
     if (errare /= 0) then
        call FATERR(IAM,'error allocating WRRarray')
     end if
   
     if (allocated(this)) then
        do ik=1,size(this)
           if(associated(this(ik)%adjjl)) deallocate(this(ik)%adjjl)
           if(associated(this(ik)%rev_adj)) deallocate(this(ik)%rev_adj)
        end do
        deallocate(this)
     end if

     allocate(this(nb_CDAN),stat=errare)
     if (errare /= 0) then
       call FATERR(IAM,'error allocating this')
     end if

     do ik=1,nb_CDAN
       nullify(this(ik)%adjjl)
       nullify(this(ik)%rev_adj)
     enddo

   endif

   do ik=1,nb_CDAN
      ialeat(ik)=ik
      ialeatr(ik)=ik

      call random_number(RA)
      IAL1 = IDINT(RA*real(nb_CDAN,8))+1
      IAL1 = MIN0(IAL1,nb_CDAN)
      IAL1 = MAX0(1,IAL1)
      randomlist(ik)=IAL1
   end do

   nb_ENTITY = get_nb_ENTITY()

   call Create_EntityList   

   !!!!
   do ik=1,nb_CDAN
      Xvlton(ik)=0.D0
      WRRarray(ik)=1.D0
      !
      this(ik)%rlt        = 0.D0
      this(ik)%rln        = 0.D0
      this(ik)%corlt      = 0.D0
      this(ik)%corln      = 0.D0
      this(ik)%vlt        = 0.D0
      this(ik)%vln        = 0.D0
      this(ik)%status     = i_nknow
      this(ik)%vltBEGIN   = 0.D0
      this(ik)%vlnBEGIN   = 0.D0
      this(ik)%gapTTbegin = 0.D0
      this(ik)%gapREF     = 0.D0
      this(ik)%statusBEGIN= i_nknow
      this(ik)%Wtt        = 0.D0
      this(ik)%Wtn        = 0.D0
      this(ik)%Wnt        = 0.D0
      this(ik)%Wnn        = 0.D0
      this(ik)%WWtt       = 1.D0
      this(ik)%WWnn       = 1.D0
      this(ik)%det        = 1.D0
      this(ik)%vfreet     = 0.D0
      this(ik)%vfreen     = 0.D0
      this(ik)%covfreet   = 0.D0
      this(ik)%covfreen   = 0.D0   
      this(ik)%lawnb      = 0
      this(ik)%statuscheck= i_nknow
      this(ik)%forecast   = i_acton    ! default status
      this(ik)%fric       = 0.D0
      this(ik)%forward    = 0.D0
      this(ik)%backward   = 0.D0

      this(ik)%ivnish     = 0
      this(ik)%iskip      = 1   
      !
      this(ik)%icdent     = 0
      this(ik)%ianent     = 0
      this(ik)%istart     = 0  
      this(ik)%nbadj      = 0
      !
      this(ik)%kcdwear    = 0.D0
      this(ik)%kanwear    = 0.D0
      !
      this(ik)%internal   = 0.D0
      this(ik)%taz        = 0.D0
      ! weak/strong
      this(ik)%ws         = 0
      !
      this(ik)%i_law      = 0
      !
      !fd-wip
      this(ik)%is_not_null = 1      
      this(ik)%is_in_queue = 1      
   end do

   ! shifts are initialized to an impossible value
   shift_icdan = -1

   nb_CDAN   = 0
   reac_mean = 0.D0
  
   call prep_nlgs_aux_( i_dkdkx, nb_DKDKx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_dkkdx, nb_DKKDx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_dkjcx, nb_DKJCx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_dkplx, nb_DKPLx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_ptpt2, nb_PTPT2, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_plplx, nb_PLPLx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_pljcx, nb_PLJCx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_clalp, nb_CLALp, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_cljcx, nb_CLJCx, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_p2p2l, nb_P2P2L, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_plalp, nb_PLALp, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_dkalp, nb_DKALp, reac_mean, nb_CDAN )
   call prep_nlgs_aux_( i_dkdkl, nb_DKDKL, reac_mean, nb_CDAN )

   if(nb_CDAN /= 0) reac_mean = reac_mean/real(nb_CDAN,8)

   DO ient=1,nb_ENTITY
     IF (entity(ient)%ik /= entity(ient)%nb) THEN
       CALL LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
       WRITE(cout,'(A7,I0,A4,I0,A4,I0)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
       CALL FATERR(IAM,cout)
     END IF
   END DO
 
   
   nb_STRONG = 0
   nb_WEAK   = 0
   
   do ik=1,nb_CDAN
      call injj_(ik,this(ik)%rlt,this(ik)%rln,iIreac)
      if(this(ik)%rln < reac_mean)then
         nb_WEAK = nb_WEAK+1
         iwksg(nb_CDAN+1-nb_WEAK) = ik
      else
         this(ik)%ws = 1
         nb_STRONG = nb_STRONG+1
         iwksg(nb_STRONG) = ik
      end if
   end do
   
   do ik=1,nb_CDAN  
      call nullify_reac_(ik,iIreac)
      call nullify_vlocy_(ik,iVaux_)
   end do

   if ( SDLactif ) then
      ! In this case the W matrix is going to be built and stored
      bandwidth=0
      istart  = 0  
      !vv:
      nb_adjac = 0
      do ik=1,nb_CDAN
         nbadj = 0
         icdik = this(ik)%icdent
         ianik = this(ik)%ianent

         !!! computation of nb_adj for contact ik; the number of adjacent contact is equal to
         !!! the number of contact active for icdbdy plus the number of contact for ianbdy.
         !!! cette valeur est surevaluee car il peut y avoir plus d'1 contact partagee entre 2 objets
         !!! 
         if (icdik == ianik) then
           nbadj = entity(icdik)%nb - 1
         else
           ! fd xxx gestion des corps bloques ...
           if (get_status_entity(icdik) == 0 ) nbadj = entity(icdik)%nb - 1
           if (get_status_entity(ianik) == 0 ) nbadj = nbadj + entity(ianik)%nb - 1            
           
           ! nbadj = entity(icdik)%nb+entity(ianik)%nb-2
         endif

         jl = 0
         
         if (nbadj /= 0) then
            
            !!!fd attention la desallocation doit se faire avec celle de this !!
            !!!fd gestion qui va avec experimental:      
            if ( associated(this(ik)%adjjl) .and. nbadj > size(this(ik)%adjjl)) deallocate(this(ik)%adjjl) 
            if (.not. associated(this(ik)%adjjl)) allocate(this(ik)%adjjl(nbadj),stat=errare)
            if (errare /= 0) then
               call FATERR(IAM,'error allocating this(ik)%adjjl')
            end if
            this(ik)%adjjl = 0
            
            if ( associated(this(ik)%rev_adj) .and. nbadj > size(this(ik)%rev_adj)) deallocate(this(ik)%rev_adj)
            if (.not. associated(this(ik)%rev_adj)) allocate(this(ik)%rev_adj(nbadj),stat=errare)
            if (errare /= 0) then
               call FATERR(IAM,'error allocating this(ik)%rev_adj')
            end if
            this(ik)%rev_adj = 0

            if (get_status_entity(icdik) == 0) then
              do iadj=1,entity(icdik)%nb
                 if (entity(icdik)%list(iadj) == ik) cycle
                 jl = jl+1
                 this(ik)%adjjl(jl) = entity(icdik)%list(iadj)

                 !vv:
                 nb_adjac = nb_adjac + 1
              end do
            endif

            if (get_status_entity(ianik) == 0 ) then                         
              do iadj=1,entity(ianik)%nb
                 if(entity(ianik)%list(iadj) == ik) cycle
               
                 !!!fd evacuation des contacts partages (uniquement auto-contact)
                 is_present = .false.
                 if (get_status_entity(icdik) == 0) then                 
                   !!! precedemment on a range entity(icdik)%nb-1 contacts on les reparcours 
                   !!! a la recherche d'un doublon avec celui qu'on veut poser
                   do jadj=1,entity(icdik)%nb-1
                      if (this(ik)%adjjl(jadj) /= entity(ianik)%list(iadj)) cycle
                      is_present = .true.
                      exit
                   end do
                 endif
                 if (is_present) cycle
                 
                 jl = jl+1
                 this(ik)%adjjl(jl) = entity(ianik)%list(iadj)
                 
                 !vv:
                 nb_adjac = nb_adjac + 1
              end do
            endif  
         end if
         
         this(ik)%istart = istart
         istart = 4*jl + istart
         
         this(ik)%nbadj = jl
         bandwidth = bandwidth+jl
         
      end do
      
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
      
      if (allocated(Wab)) deallocate(Wab)
      allocate(Wab(4*bandwidth),stat=errare)
      if (errare /= 0) then
         call FATERR(IAM,'error allocating Wab')
      end if
      Wab=0.d0
      
   end if

   ! computing Wjl,ik matrix
   ! be carefull ik is the second index !!

   do ik=1,nb_CDAN

      ! --------------------------------------------
      ! Computing the bloc diagonal part of matrix W
      ! --------------------------------------------

     rnik = 0.d0
     rtik = 1.d0
     call nullify_reac_(ik,iIaux_)
     call injj_(ik,rtik,rnik,iIaux_)
     call vitrad_(ik,iVaux_e_invM_t_Iaux_)
     call prjj_(ik,vtik,vnik,iVaux_)
     this(ik)%Wtt = vtik
     this(ik)%Wnt = vnik
	 
      
     if ( SDLactif ) then
       do ikadj=1,this(ik)%nbadj
            
         jl=this(ik)%adjjl(ikadj)
         if (jl == 0) then
           icdik = this(ik)%icdent
           ianik = this(ik)%ianent
           write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'contact :',ik,' icdent ',icdik,' ianent ',ianik
           call logmes(cout)
           ! print*,entity(icdik)%nb
           ! print*,entity(icdik)%list
           ! print*,entity(ianik)%nb
           ! print*,entity(ianik)%list
           ! write(*,'(A,1x,I0,1x,A)') ' nbabj ',this(ik)%nbadj,'liste :'
           ! print*,this(ik)%adjjl(:)

           write(cout,'(A,1x,I0)') 'ca chie pour adj ',ikadj
           call logmes(cout)
         endif

         jlstart = this(jl)%istart
            
         call prjj_(jl,vtik,vnik,iVaux_)
            
         ok = .false.
            
         do jladj=1,this(jl)%nbadj
               
           if (this(jl)%adjjl(jladj) == ik) then
             Wab(jlstart + 4*jladj-3) =vtik
             Wab(jlstart + 4*jladj-1) =vnik
             ok = .true.
             this(ik)%rev_adj(ikadj)=jladj
             exit
           end if
               
         end do
            
         if (.not. ok) then
           call FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
         end if
            
       end do
         
       call nullify_vlocy_(ik,iVaux_)

     end if
     
     rnik = 1.d0
     rtik = 0.d0
     call nullify_reac_(ik,iIaux_)
     call injj_(ik,rtik,rnik,iIaux_)
     call vitrad_(ik,iVaux_e_invM_t_Iaux_)
     call prjj_(ik,vtik,vnik,iVaux_)
     this(ik)%Wtn = vtik
     this(ik)%Wnn = vnik
     
     if ( SDLactif ) then
       do ikadj=1,this(ik)%nbadj
           
         jl = this(ik)%adjjl(ikadj)
         jlstart = this(jl)%istart
           
         call prjj_(jl,vtik,vnik,iVaux_)

         jladj = this(ik)%rev_adj(ikadj)
         Wab(jlstart + 4*jladj-2) =vtik
         Wab(jlstart + 4*jladj  ) =vnik
           
       end do

       call nullify_vlocy_(ik,iVaux_)
         
     end if

     ibehav = this(ik)%lawnb


     ! ! burk
     ! this(ik)%Wnt = 0.d0
     ! this(ik)%Wtn = 0.d0 

     ! print*,this(ik)%Wnn,this(ik)%Wnt
     ! print*,this(ik)%Wtn,this(ik)%Wtt
     ! ! burk
	 
     
     !!! --------------------------------------
     !!! Warning and coping with critical cases
     !!! --------------------------------------

     if (this(ik)%Wnn .le. 1.D-18) then
       write(cout,"(1X,'  Wnn(',I5,') =',D12.5,' < 1.D-18')") ik,this(ik)%Wnn
       call LOGMES('Error '//IAM//': '//cout)
       write(cout,'(I8,A2,I7,A1,I7,A1,D12.5)') this(ik)%CDAN,': ',this(ik)%icdent,',',this(ik)%ianent,' ',this(ik)%gapTTbegin
       call LOGMES('Error '//IAM//': '//cout)
     end if

     if (this(ik)%Wtt .le. 1.D-06*this(ik)%Wnn.and. &
         tact_behav(ibehav)%ilaw /= i_ELASTIC_ROD.and. &
         tact_behav(ibehav)%ilaw /= i_VOIGT_ROD ) then
         
       write(cout,"(1X,'   Wtt(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')")ik,this(ik)%Wtt,ik
       call LOGMES(cout)

       write(cout,'(2(1x,D14.7))') this(ik)%Wnn,this(ik)%Wnt
       call LOGMES(cout)
       write(cout,'(2(1x,D14.7))') this(ik)%Wtn,this(ik)%Wtt
       call LOGMES(cout)
       write(cout,'(1(1x,D14.7))') this(ik)%fric
       call LOGMES(cout)
       call print_info_(ik) 
       this(ik)%Wtt=1.D-06*this(ik)%Wnn
     end if

     !!!*************************************
     !!! Preparing auxiliaries
     !!!     
     !!!*************************************

      fric = get_fric(ibehav,this(ik)%statusBEGIN)
      this(ik)%fric=fric
	  
	  ! print*, 'fric = ', this(ik)%fric

      this(ik)%WWtt = this(ik)%Wtt
      this(ik)%WWnn = this(ik)%Wnn

      this(ik)%covfreet = 0.D0
      this(ik)%covfreen = 0.D0     
      this(ik)%corln    = 0.D0      

      cd_length=0.d0

      ! customized values
      
      select case(tact_behav(ibehav)%ilaw)

      !!!---------
      case(i_IQS_CLB)
         this(ik)%i_law = i_IQS_CLB

         this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)

         cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         if( regul .and. cd_length > 1d-14 ) then
           if ( krn /= 0.d0 ) then
             this(ik)%WWnn = this(ik)%Wnn + ( 1.d0 / (krn*cd_length*H*H) )
             this(ik)%covfreen=this(ik)%gapTTbegin/H
           endif
           if ( krt /= 0.d0 ) this(ik)%WWtt = this(ik)%Wtt + ( 1.d0 / (krt*cd_length*H)   )
         end if

      !!!---------
      case(i_IQS_MAP)
         this(ik)%i_law = i_IQS_MAP
         
         this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)   

         this(ik)%internal(1) = fric
         this(ik)%fric=fric

       !!!-------------------------------------
       case(i_IQS_CLB_g0)
          this(ik)%i_law = i_IQS_CLB_g0

          !fd on stoque le g0
          if (.not. is_initialized) then

            this(ik)%internal(1) = max(0.d0,this(ik)%gapTTbegin)       

          end if
          this(ik)%covfreen=max(0.D0,(this(ik)%gapTTbegin-this(ik)%internal(1))/H)
          
          cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
          if ( regul .and. cd_length > 1d-14 ) then
            if ( krn /= 0.d0 ) then
               this(ik)%WWnn = this(ik)%Wnn + ( 1.d0 / (krn*cd_length*H*H) )
               this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%internal(1))/H
            endif
            if( krt /= 0.d0 ) this(ik)%WWtt = this(ik)%Wtt + ( 1.d0 / (krt*cd_length*H)   )
          end if

      !!!----------------------------------------
      case(i_IQS_CLB_RGR)
         this(ik)%i_law = i_IQS_CLB_RGR

         ! same as previous but with a mild gap violation restoring procedure, 
         ! so called "Radjai Gap Rescue", "RGR".
         ! see doc mjean
         ! write(*,'(A12,I5,1X,D12.5)')' k          ',ik,2.D0/(this(ik)%Wnn*OTH*OTH*H*H)
         ! The secure choice is ToverH>2*sqrt(2)*pi=8.8857578... One may try ToverH=1.D0, speeding rescuing, at your own risk.
         ! In case of troubles decrease the time step H.

         call get_gap_tol(ibehav,gap_tol)
         if (this(ik)%gapTTbegin .ge. gap_tol) then
            this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln=0.D0
         else
            call get_ToverH(ibehav,ToverH)
            OTH=Oneover2pi*ToverH
            this(ik)%corln=-(2.D0/(this(ik)%Wnn*OTH*OTH))*((this(ik)%gapTTbegin-gap_tol)/H)
            this(ik)%covfreet=this(ik)%Wtn*this(ik)%corln
            this(ik)%covfreen=this(ik)%Wnn*this(ik)%corln
         end if
         
      !!!-------------------------------------
      case(i_IQS_DS_CLB)
         this(ik)%i_law = i_IQS_DS_CLB
         this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)         

      !!!-------------------------------------
      case(i_IQS_STICK)
         this(ik)%i_law = i_IQS_STICK
         this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)         

      !!!-------------------------------------
      case(i_GAP_SGR_STICK)
         this(ik)%i_law = i_GAP_SGR_STICK
         this(ik)%covfreen=this(ik)%gapTTbegin/H

      !!!-------------------------------------
      case(i_IQS_CLB_nosldt)
         this(ik)%i_law = i_IQS_CLB_nosldt

         this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)

         if ( NSTEP ==1 ) this(ik)%internal(1) = 0.d0

      case(i_preGAP_SGR_CLB)
         this(ik)%i_law = i_preGAP_SGR_CLB

         call get_pregap(ibehav,pre_gap)
         this(ik)%covfreen=(this(ik)%gapTTbegin+pre_gap)/H

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         end if

         this(ik)%internal(1) = cd_length

      !!!-------------------------------------         
      case(i_GAP_SGR_CLB_nosldt)
         this(ik)%i_law = i_IQS_CLB_nosldt

         this(ik)%covfreen=this(ik)%gapTTbegin/H

         if ( NSTEP ==1 ) this(ik)%internal(1) = 0.d0

      !!!-------------------------------------
      case(i_RST_CLB)
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
            this(ik)%forecast= i_noact !'noact'
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%status  = i_noctc
         end if
         
      !!!-------------------------------------
      case(i_RST_DS_CLB)
         this(ik)%i_law = i_RST_DS_CLB
         
         if (this(ik)%gapTTbegin .le. 0.D0) then
            ! this(ik)%forecast='acton' (default is 'acton')
            !fd needs further checking by !mj
            call get_rst(ibehav,tangalrest,normalrest)
            this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN
            this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN
         else
            this(ik)%forecast= i_noact !'noact'
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%status  = i_noctc
         end if

      !!!-------------------------------------
      case(i_RST_WET_CLB)
         this(ik)%i_law = i_RST_WET_CLB
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)

         if (this(ik)%gapTTbegin .le. 0.D0) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
            else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if
            call get_rst(ibehav,tangalrest,normalrest)
            this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN-normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN-normalcoh*this(ik)%Wnn*H
            this(ik)%corln    = -normalcoh*H

         else if(this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
            else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if
            this(ik)%covfreet = tangalrest*this(ik)%vltBEGIN
            this(ik)%covfreen = normalrest*this(ik)%vlnBEGIN
            this(ik)%corln    = -normalcoh*H
         else 
            this(ik)%forecast= i_noact !'noact'
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%status  = i_noctc
         end if

      !!!-------------------------------------
      case(i_GAP_SGR_CLB)
         this(ik)%i_law = i_GAP_SGR_CLB

         this(ik)%covfreen=this(ik)%gapTTbegin/H

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         end if

         this(ik)%internal(1) = cd_length

         if (regul .and. cd_length > 1d-14) then
           ! une modif pour reduire l indetermination
           if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*cd_length*H*H))
           !! c'est une viscosite 
           if (krt /= 0.d0) this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*cd_length*H))
         endif   

      !!!-------------------------------------
      case(i_GAP_SGR_CLB_g0)
         this(ik)%i_law = i_GAP_SGR_CLB_g0

          !fd on stoque le g0
          if (.not. is_initialized) then

            this(ik)%internal(2) = this(ik)%gapTTbegin       

          end if

         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(2))/H

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         end if

         this(ik)%internal(1) = cd_length
         
      !!!-------------------------------------
      case(i_VEL_SGR_CLB)
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
            this(ik)%forecast= i_noact !'noact'
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
      case(i_GAP_SGR_DS_CLB)
         this(ik)%i_law = i_GAP_SGR_DS_CLB
         this(ik)%covfreen = this(ik)%gapTTbegin/H

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         end if

         this(ik)%internal(1) = cd_length
         
      !!!-------------------------------------
      case(i_GAP_WET_DS_CLB)

         this(ik)%i_law = i_GAP_WET_DS_CLB

         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         ! here tangalcoh is inactive
         if (this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
            else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if
            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H+(this(ik)%gapTTbegin/H)
            this(ik)%corln    = -normalcoh*H
         else 
            this(ik)%covfreen = this(ik)%gapTTbegin/H
         end if

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         end if

         this(ik)%internal(1) = cd_length

      !!!-------------------------------------
      case(i_IQS_WET_DS_CLB)
         this(ik)%i_law = i_IQS_WET_DS_CLB
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         ! here tangalcoh is inactive
         if (this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
            else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if
            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln    = -normalcoh*H
         else
            this(ik)%statusBEGIN = i_noctc
            this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)
         end if
         
      !!!-------------------------------------
      case(i_xQS_WET_DS_CLB)
         this(ik)%i_law = i_xQS_WET_DS_CLB
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         ! eXtended IQS_WET_DS_CLB law 
         ! once cohesion is broken it cant be recovered
         ! here tangalcoh is inactive
         if (.not. is_initialized) THEN
            if (this(ik)%gapTTbegin .le. Wethk) then
               if (this(ik)%statusBEGIN == i_nknow)then
                  this(ik)%statusBEGIN=i_Wnnow
               else if (this(ik)%statusBEGIN == i_noctc)then
                  this(ik)%statusBEGIN=i_Wnctc
               else if (this(ik)%statusBEGIN == i_stick)then
                  this(ik)%statusBEGIN=i_Wstck
               else if (this(ik)%statusBEGIN == i_slibw)then
                  this(ik)%statusBEGIN=i_Wslbw
               else if (this(ik)%statusBEGIN == i_slifw)then
                  this(ik)%statusBEGIN=i_Wslfw
               end if
               this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
               this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
               this(ik)%corln    = -normalcoh*H 
            else
               this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)
            end if
         else
            !if (this(ik)%statusBEGIN(1:1) == 'W'.and. this(ik)%gapTTbegin .le. Wethk) then
            if ( this(ik)%gapTTbegin .le. Wethk  .and. &
                 this(ik)%statusBEGIN >= i_Wnnow .and. &
                 this(ik)%statusBEGIN <= i_Wslfw       ) then
            
               this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
               this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
               this(ik)%corln    = -normalcoh*H
               
            else 
            
               if (this(ik)%statusBEGIN == i_Wnnow)then
                  this(ik)%statusBEGIN=i_nknow
               else if (this(ik)%statusBEGIN == i_Wnctc)then
                  this(ik)%statusBEGIN=i_noctc
               else if (this(ik)%statusBEGIN == i_Wstck)then
                  this(ik)%statusBEGIN=i_stick
               else if (this(ik)%statusBEGIN == i_Wslbw)then
                  this(ik)%statusBEGIN=i_slibw
               else if (this(ik)%statusBEGIN == i_Wslfw)then
                  this(ik)%statusBEGIN=i_slifw
               end if
               
               this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)
            end if
         end if
         
      !!!-------------------------------------
      case(i_IQS_MOHR_DS_CLB)
         this(ik)%i_law = i_IQS_MOHR_DS_CLB

         if (Nstep == 1 .and. this(ik)%statusBEGIN == i_nknow) then
            this(ik)%statusBEGIN=i_Mstck
            ! le fric calcule avant etait faux a cause du statut
            fric = get_fric(ibehav,this(ik)%statusBEGIN)
            this(ik)%fric=fric
         end if

         if (this(ik)%statusBEGIN == i_Mstck) then

           !fd todo virer ca 
           cd_length = 1.d0

           if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
                ( this(ik)%CDAN == i_DKJCx ) .or. &
                ( this(ik)%CDAN == i_PLPLx ) .or. &
                ( this(ik)%CDAN == i_PLJCx ) ) then
              cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
           else if (this(ik)%CDAN == i_DKDKL) then
             cd_length=0.05
           else
             call faterr(IAM,'IQS_MOHR_DS_CLB not implemented for this contact element')
           end if

           call get_coh(ibehav,normalcoh,tangalcoh,Wethk)

           if (regul .and. cd_length > 1d-14) then
             ! une modif pour reduire l indetermination
             if (krn /= 0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*cd_length*H*H))
             ! c'est une viscosite 
             if (krt /= 0.d0) this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*cd_length*H))
           endif   
           
           ! here Wethk is inactive
           this(ik)%covfreet = -cd_length*normalcoh*this(ik)%Wtn*H
           this(ik)%covfreen = (-cd_length*normalcoh*this(ik)%WWnn*H) + max(0.D0,this(ik)%gapTTbegin/H)
           this(ik)%corln    = -cd_length*normalcoh*H

           if (regul .and. cd_length > 1d-14) then
             if (krn /= 0.d0) this(ik)%covfreen = (-cd_length*normalcoh*this(ik)%Wnn*H) + &
                                                  (this(ik)%gapTTbegin - (normalcoh/krn))/H
           endif
         else
           this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)
         end if
        
      !!!-------------------------------------
      case(i_GAP_MOHR_DS_CLB)
         
         this(ik)%i_law = i_GAP_MOHR_DS_CLB

         if (Nstep == 1 .and. this(ik)%statusBEGIN == i_nknow) then
            this(ik)%statusBEGIN=i_Mstck
            ! le fric calcule avant etait faux a cause du statut
            fric = get_fric(ibehav,this(ik)%statusBEGIN)
            this(ik)%fric=fric
         end if

         if (this(ik)%statusBEGIN == i_Mstck) then

           call get_coh(ibehav,normalcoh,tangalcoh,Wethk)

           if ( ( this(ik)%CDAN == i_CLALp ) .or. &
                ( this(ik)%CDAN == i_CLJCx ) ) then
              cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
           else
              call faterr(IAM,'GAP_MOHR_DS_CLB not implemented for this contact element')
           end if

           if (regul .and. cd_length > 1d-14 ) then
             ! une modif pour reduire l indetermination
             if (krn /=0.d0) this(ik)%WWnn    = this(ik)%Wnn+(1.D0/(krn*cd_length*H*H))
             if (krt /=0.d0) this(ik)%WWtt    = this(ik)%Wtt+(1.D0/(krt*cd_length*H)) 
           endif   
            
           ! here Wethk is inactive
           this(ik)%covfreet = -cd_length*normalcoh*this(ik)%Wtn*H
           this(ik)%covfreen = (-cd_length*normalcoh*this(ik)%WWnn*H) + (this(ik)%gapTTbegin/H)
           this(ik)%corln    = -cd_length*normalcoh*H

           if (regul .and. cd_length > 1d-14) then
             if (krn /= 0.d0) this(ik)%covfreen = (-cd_length*normalcoh*this(ik)%Wnn*H) + &
                                                  (this(ik)%gapTTbegin - (normalcoh/krn))/H
           endif
           
         else
           this(ik)%covfreen = this(ik)%gapTTbegin/H
         end if

      !!!-------------------------------------
      case(i_GAP_CAP_MOHR_DS_CLB)
         this(ik)%i_law = i_GAP_CAP_MOHR_DS_CLB
         
         if (Nstep == 1 .and. this(ik)%statusBEGIN == i_nknow) then
            this(ik)%statusBEGIN=i_Mstck
            ! le fric calcule avant etait faux a cause du statut
            fric = get_fric(ibehav,this(ik)%statusBEGIN)
            this(ik)%fric=fric

         end if

         if (this(ik)%statusBEGIN == i_Mstck) then

            fric = get_fric_CAP(ibehav,this(ik)%rln)
            this(ik)%fric=fric

            call get_coh(ibehav,normalcoh,tangalcoh,Wethk)

            if ( ( this(ik)%CDAN == i_CLALp ) .or. &
                 ( this(ik)%CDAN == i_CLJCx ) ) then
               cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
            else
               call faterr(IAM,'GAP_Mohr not implemented for this contact element')
            end if


            ! here Wethk is inactive
            this(ik)%covfreet = -normalcoh*cd_length*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*cd_length*this(ik)%Wnn*H + this(ik)%gapTTbegin/H
            this(ik)%corln    = -normalcoh*cd_length*H
         else
            this(ik)%covfreen = this(ik)%gapTTbegin/H
         end if

      !-------------------------------------
      case(i_ELASTIC_REPELL_CLB)
         this(ik)%i_law = i_ELASTIC_REPELL_CLB
         
         call get_forcePERgap(ibehav,forcePERgap)
         this(ik)%WWnn    = this(ik)%Wnn+1.D0/(forcePERgap*H*H)
         this(ik)%covfreen= this(ik)%gapTTbegin/H

       !!!-------------------------------------
       CASE(i_ELASTIC_REPELL_CLB_g0)
          this(ik)%i_law = i_ELASTIC_REPELL_CLB_g0
          
          if ( .NOT. is_initialized ) then

            this( ik )%internal( 1 ) = max( 0.d0, this( ik )%gapTTbegin )

          end if

          call get_forcePERgap( ibehav, forcePERgap )
          this( ik )%WWnn     = this( ik )%Wnn + 1.D0 / ( forcePERgap * H * H )
          this( ik )%covfreen = ( this( ik )%gapTTbegin - this( ik )%internal( 1 ) ) / H

       !!!------------------------------------- PTA sncf 2023
       CASE(i_ELASTIC_REPELL_CLB_adapt)
          this(ik)%i_law = i_ELASTIC_REPELL_CLB_adapt
          
          if ( .NOT. is_initialized ) then

            call get_forcePERgap( ibehav, forcePERgap )
            this( ik )%internal( 1 ) = forcePERgap

          end if

          forcePERgap = this(ik)%internal(1)

          this(ik)%WWnn    = this(ik)%Wnn+1.D0/(forcePERgap*H*H)
          this(ik)%covfreen= this(ik)%gapTTbegin/H

       !!!----------------------------------------
       CASE(i_VISCO_ELASTIC_REPELL_CLB)
          this(ik)%i_law = i_VISCO_ELASTIC_REPELL_CLB


         if (this(ik)%gapTTbegin .le. 0.D0) then
            ! this(ik)%forecast='acton' (default is 'acton')
            CALL get_forcePERgap(ibehav, forcePERgap)
            CALL get_viscosity(ibehav  , etan, etat)

            this(ik)%WWnn     = this(ik)%Wnn + ( 1.D0 / ( (forcePERgap*H*H) + (etan*H) ) )
            this(ik)%covfreen = forcePERgap/(forcePERgap*H + etan) * this(ik)%gapTTbegin

         else
            this(ik)%forecast= 0 !'noact'
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%status  = i_noctc
         end if  
          
      !-------------------------------------
      case(i_CRITICAL_VOIGT_CLB)
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
         !internal(1) , the lag (le decalage) or the length of the infinitesimal tangential spring,
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
            if (this(ik)%internal(2) .lt. 1.5D0) this(ik)%internal(1)=0.D0
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

           !write(*,'(1X,A3,D12.5)')'kt=',1.D0/( ( this(ik)%Wtt*0.5D0*(OTH*OTH/(1.D0+2.D0*vOVERcv*OTH)) )*H*H )
           !write(*,'(1X,A3,D12.5)')'kt=',1.D0/(this(ik)%Wtt*OTH*OTH*H*H)
           !The lag (tangential spring length) is stored in this(ik)%internal(1), 
           !the pad sliding velocity is stored in this(ik)%vepad computed while iterating.
           !When no contact (this(ik)%gapTTbegin > 0.D0) the lag this(ik)%internal(1) is set to zero. 
           !The prediction of the lag is used to estimate this(ik)%covfreet. 

           this(ik)%covfreet=(this(ik)%internal(1)/H)*(1.D0/(1.D0+2.D0*vOVERcv*OTH))   

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

      !-------------------------------------
      case(i_ELASTIC_REPELL_WET_CLB)
         this(ik)%i_law = i_ELASTIC_REPELL_WET_CLB
         ! This law is a non smooth approximation of the so called Lennard-Jones law, which
         ! is used to approximate superficial tension effects when a thin local fluid layer 
         ! is acting between contactors. Unilateral conditions are described by the elastic 
         ! repell law. Attraction is active while the gap is less than Wethk.
         ! Thus the energy of rupture is normalcoh*Wethk.
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         ! here tangalcoh is inactive
         call get_forcePERgap(ibehav,forcePERgap)
         this(ik)%WWnn    = this(ik)%Wnn+1.D0/(forcePERgap*H*H)
         this(ik)%covfreen= this(ik)%gapTTbegin/H

         if (this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
            else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if
            this(ik)%covfreet= this(ik)%covfreet-normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen= this(ik)%covfreen-normalcoh*this(ik)%Wnn*H
            this(ik)%corln   =-normalcoh*H
         end if

      !-------------------------------------
      CASE(i_ELASTIC_REPELL_MAC_CZM) !vhnhu
         this(ik)%i_law = i_ELASTIC_REPELL_MAC_CZM
         !CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         ! here tangalcoh is inactive
         CALL get_forcePERgap(ibehav,forcePERgap)
         CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)

         IF (.NOT. is_initialized) THEN
              IF (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow
              CALL init_CZM(ibehav,this(ik)%internal,this(ik)%taz) 
         else
            this(ik)%taz(1) = this(ik)%internal(4)
         END IF

         IF (this(ik)%gapTTbegin .LE. Wethk) THEN
           !this(ik)%WWnn    = this(ik)%Wnn+1.D0/(forcePERgap*H*H)
           this(ik)%covfreen=MAX(0.D0,this(ik)%gapTTbegin/H)
           CALL prep_CZM(ibehav,this(ik)%internal,0.D0,0.d0, this(ik)%gapTTbegin)

           if( this(ik)%statusBEGIN >= i_Cnnow .and. this(ik)%statusBEGIN <= i_Cslfw ) then
               this(ik)%statusBEGIN = this(ik)%statusBEGIN + i_Ennow-i_Cnnow
           else if( this(ik)%statusBEGIN >= i_nknow .and. this(ik)%statusBEGIN <= i_slifw ) then
               this(ik)%statusBEGIN = this(ik)%statusBEGIN + i_Ennow-i_nknow
           end if
           !IF (this(ik)%statusBEGIN(1:1) == 'C')THEN
           !    WRITE(this(ik)%statusBEGIN(1:1),'(A1)') 'E'
           !ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
           !    this(ik)%statusBEGIN='Enctc'
           !ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
           !    this(ik)%statusBEGIN='Estck'
           !ELSE IF (this(ik)%statusBEGIN == i_slibw)THEN
           !    this(ik)%statusBEGIN='Eslbw'
           !ELSE IF (this(ik)%statusBEGIN == i_slifw)THEN
           !    this(ik)%statusBEGIN='Eslfw'
           !END IF
          !*********************
         ELSE IF ((this(ik)%gapTTbegin .LE. 0.D0).AND.(this(ik)%gapTTbegin .GT. Wethk)) THEN
           this(ik)%WWnn    = this(ik)%Wnn+1.D0/(forcePERgap*H*H)
           this(ik)%covfreen= this(ik)%gapTTbegin/H
           CALL prep_CZM(ibehav,this(ik)%internal,0.D0,0.d0, this(ik)%gapTTbegin)

           if( this(ik)%statusBEGIN >= i_Cnnow .and. this(ik)%statusBEGIN <= i_Cslfw ) then
               this(ik)%statusBEGIN = this(ik)%statusBEGIN + i_Ennow-i_Cnnow
           else if( this(ik)%statusBEGIN >= i_nknow .and. this(ik)%statusBEGIN <= i_slifw ) then
               this(ik)%statusBEGIN = this(ik)%statusBEGIN + i_Ennow-i_nknow
           end if
           !IF (this(ik)%statusBEGIN(1:1) == 'C')THEN
           !    WRITE(this(ik)%statusBEGIN(1:1),'(A1)') 'E'
           !ELSE IF (this(ik)%statusBEGIN == i_noctc)THEN
           !    this(ik)%statusBEGIN='Enctc'
           !ELSE IF (this(ik)%statusBEGIN == i_stick)THEN
           !    this(ik)%statusBEGIN='Estck'
           !ELSE IF (this(ik)%statusBEGIN == i_slibw)THEN
           !    this(ik)%statusBEGIN='Eslbw'
           !ELSE IF (this(ik)%statusBEGIN == i_slifw)THEN
           !    this(ik)%statusBEGIN='Eslfw'
           !END IF
         END IF
         !*********************
         IF ((this(ik)%gapTTbegin .GE. 0.D0)) THEN
           ! is_cohesive = .TRUE.
            cd_length = 1.d0

           if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
                ( this(ik)%CDAN == i_DKJCx ) .or. &
                ( this(ik)%CDAN == i_PLPLx ) .or. &
                ( this(ik)%CDAN == i_PLJCx ) ) then
              cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
            ELSE
               call faterr(IAM,'ELASTC_REPELL_MAC_CZM not implemented for this contact element')
            END IF
            un = MAX(0.d0,this(ik)%gapTTbegin) 
            CALL prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,un)
            this(ik)%covfreen=un/H    
            this(ik)%covfreet=0.d0
            if( this(ik)%statusBEGIN >= i_Ennow .and. this(ik)%statusBEGIN <= i_Eslfw ) then
                this(ik)%statusBEGIN = this(ik)%statusBEGIN - i_Ennow-i_Cnnow
            end if
            !IF (this(ik)%statusBEGIN(1:1) == 'E')THEN
            !   WRITE(this(ik)%statusBEGIN(1:1),'(A1)') 'C'
            !END IF

         END IF

       !!!-------------------------------------
       case(i_VISCO_ELASTIC_REPELL_WET)
          this(ik)%i_law = i_VISCO_ELASTIC_REPELL_WET
          
          this(ik)%internal = 0.D0
          call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
          ! la partie compression visco elastique
          if (this(ik)%gapTTbegin .le. 0.D0) then
            call get_forcePERgap(ibehav,forcePERgap)
            CALL get_viscosity(ibehav  , etan, etat)
            
            this(ik)%WWnn     = this(ik)%Wnn + ( 1.D0 / ( (forcePERgap*H*H) + (etan*H) ) )
            this(ik)%covfreen = forcePERgap/(forcePERgap*H + etan) * this(ik)%gapTTbegin
            
            ! ? call get_forcePERstrainrate(ibehav,forcePERstrainrate)
            !id_inter = this(ik)%CDAN
            !if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
            !     ( this(ik)%CDAN == i_DKJCx ) .or. &
            !     ( this(ik)%CDAN == i_DKKDx ) ) then
            !   call get_eff_inter_( id_inter, this(ik)%icdan, meff, reff )
            !else 
            !   call faterr( IAM, 'VISCO_ELASTIC_REPELL_WET not implemented for this contact element' )
            !end if
            !etan = forcePERstrainrate*sqrt(reff)
            !OTH = H*(forcePERgap*H+etan)
            !this(ik)%WWnn     = this(ik)%Wnn+(1.D0/OTH)
            !this(ik)%covfreen = this(ik)%gapTTbegin*(1-etan/OTH)/H

          ! la partie cohesive  
          else if (this(ik)%gapTTbegin .gt. 0.D0 .and. &
                   this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
              else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if

            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln    = -normalcoh*H

          !la partie rompue
          else
            this(ik)%forecast= 0 !'noact'
            this(ik)%rlt     = 0.D0
            this(ik)%rln     = 0.D0
            this(ik)%status  = i_noctc
          end if

      !!!-------------------------------------
      case(i_ELASTIC_ROD)
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
      case(i_ELASTIC_WIRE)
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
       case(i_BRITTLE_ELASTIC_WIRE)
          this(ik)%i_law = i_BRITTLE_ELASTIC_WIRE

          if (this(ik)%statusBEGIN /= i_vnish ) then
            this(ik)%gapREF=this(ik)%internal(1)
          
            if (this(ik)%gapREF .le. 1.D-18) then
               write(cout,555) ik,this(ik)%gapREF
               call FATERR(IAM,cout)
            end if
          
            call get_forcePERstrain(ibehav,forcePERstrain)
            call get_prestrain(ibehav,prestrain)

            this(ik)%WWnn     = this(ik)%Wnn+(this(ik)%gapREF/(forcePERstrain*H*H))

            this(ik)%covfreen = (this(ik)%gapTTbegin-(1.D0+prestrain)*this(ik)%gapREF)/H

            !print*,'(gapTT - gapref)/H',this(ik)%covfreen 

          else 
             this(ik)%forecast= i_noact !'noact'
             this(ik)%rlt     = 0.D0
             this(ik)%rln     = 0.D0
             this(ik)%status  = i_noctc
          end if


      !!!-------------------------------------
      case(i_VOIGT_ROD)
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
      case(i_VOIGT_WIRE)
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

      !!!-------------------------------------
      case(i_TEX_SOL)
         this(ik)%i_law = i_TEX_SOL

         this(ik)%covfreen=0.D0!max(0.D0,this(ik)%gapTTbegin/H)         

      !!!---------------------------------------
      case(i_TEX_SOL_UNILAT)
         this(ik)%i_law = i_TEX_SOL_UNILAT

         this(ik)%gapREF=this(ik)%internal(1)
         this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%gapREF)/H      

      !!!---------------------------------------
      case(i_RIGID_WIRE)
         this(ik)%i_law = i_RIGID_WIRE

         this(ik)%gapREF=this(ik)%internal(1)
         this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%gapREF)/H      

      !!!-------------------------------------
      case(i_MAC_CZM,i_MSMP_CZM,i_MAL_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER, &
           i_TH_CZM,i_ABP_CZM,i_EXPO_CZM,i_EXPO_CZM_P)

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)
			

         else
            this(ik)%taz(1) = this(ik)%internal(4)
         end if
		 
         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else if (this(ik)%CDAN == i_PLALp) then
            !!!fd&hb font les porcs         cd_length=get_length_PLALp(this(ik)%icdan)
            cd_length=0.35
         else if (this(ik)%CDAN == i_DKALp) then
            !!!fd&hb font les porcs         cd_length=get_length_DKALp(this(ik)%icdan)
            cd_length=0.35
         else
            call faterr(IAM,'CZM not implemented for this contact element')
         end if

         !fd a voir
         if (regul .and. cd_length > 1d-14) then
           ! une modif pour reduire l indetermination
           if (krn /=0.d0) this(ik)%WWnn  = this(ik)%Wnn+(1.D0/(krn*cd_length*H*H))
           ! c'est une viscosite 
           if (krt /= 0.d0) this(ik)%WWtt = this(ik)%Wtt+(1.D0/(krt*cd_length*H))
         endif   
         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,this(ik)%gapTTbegin)

         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd
         
         this(ik)%covfreen=this(ik)%gapTTbegin/H
         
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0

      !!!-------------------------------------
      case(i_TOSI_CZM,i_TOSI_CZM_INCRE)

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

         end if

         this(ik)%taz(1) = this(ik)%vltBEGIN        
         this(ik)%taz(2) = this(ik)%vlnBEGIN

         this(ik)%taz(4) = get_time()

         ! Modif 1 : calcul de la triaxialité (lien avec XPER)
         call get_bulk_stress_triaxiality_clalp(this(ik)%icdan,this(ik)%taz(5))
         call get_bulk_temperature_clalp(this(ik)%icdan,this(ik)%taz(6))
         !call get_bulk_stress_clalp(this(ik)%icdan,vec)

         !ts= this(ik)%taz(5)
         !OPEN(unit=556, file='triaxiality.txt', iostat=ios)
         !WRITE(556,*) this(ik)%taz(5)

         ! Fin Modif 1

         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine
         !!!fd 
         if ( ( this(ik)%CDAN == i_CLALp ) ) then 
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else
            call faterr(IAM,'CZM not implemented for this contact element')
         end if
         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,this(ik)%gapTTbegin)

         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd
         this(ik)%covfreen=this(ik)%gapTTbegin/H
         
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0

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

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement au tout debut du calcul
            this(ik)%internal(7) = this(ik)%gapTTbegin
            
         else

            ! beta 
            this(ik)%taz(1) = this(ik)%internal(4)
            
         end if

         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine
         !!!fd

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) .or. &
              ( this(ik)%CDAN == i_DKALp ) .or. &             
              ( this(ik)%CDAN == i_PLALp )) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else            
            call faterr(IAM,'CZM not implemented for this contact element')
         end if

         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,this(ik)%gapTTbegin-this(ik)%internal(7))

         !!!fd
         !!!fd C'est une loi en gap - gap0 on prepare donc le changement de variables
         !!!fd
         
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(7))/H

         !!!fd
         !!!fd c'est une loi en vitesse tangentielle (frottement)
         !!!fd

         this(ik)%covfreet=0.d0      
		 
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

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement au tout debut du calcul
            this(ik)%internal(7) = this(ik)%gapTTbegin
            
         else

            ! beta 
            this(ik)%taz(1) = this(ik)%internal(4)
            
         end if

         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine
         !!!fd

         if ( ( this(ik)%CDAN == i_CLALp ) .or. &
              ( this(ik)%CDAN == i_CLJCx ) .or. &
              ( this(ik)%CDAN == i_DKALp ) .or. &             
              ( this(ik)%CDAN == i_PLALp )) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else            
            call faterr(IAM,'CZM_SPRING_P not implemented for this contact element')
         end if

         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,this(ik)%gapTTbegin-this(ik)%internal(7))

         !!!fd
         !!!fd C'est une loi en gap - gap0 on prepare donc le changement de variables
         !!!fd
         
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(7))/H

         !!!fd
         !!!fd c'est une loi en vitesse tangentielle (frottement)
         !!!fd

         this(ik)%covfreet=0.d0

         
      !!!-------------------------------------
      case(i_IQS_MAC_CZM, i_IQS_MAL_CZM, i_IQS_TH_CZM, i_IQS_ABP_CZM, i_IQS_EXPO_CZM, i_IQS_EXPO_CZM_P)

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

         else
            this(ik)%taz(1) = this(ik)%internal(4)

         end if
         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 
         cd_length = 1.d0

         if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
              ( this(ik)%CDAN == i_DKJCx ) .or. &
              ( this(ik)%CDAN == i_PLPLx ) .or. &
              ( this(ik)%CDAN == i_PLJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else if (this(ik)%CDAN == i_DKDKL) then
            cd_length=0.05
         else
            call faterr(IAM,'IQS_EXPO_CZM not implemented for this contact element')
         end if
   
         un = max(0.d0,this(ik)%gapTTbegin)       
         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,un)
         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd
         this(ik)%covfreen=un/H       
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0
         
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

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement initial - on suppose que c'est ouvert
            ! this(ik)%internal(7) = max(0.d0, this( ik )%gapTTbegin)
            this(ik)%internal(7) = this( ik )%gapTTbegin            

         else

            ! beta
            this(ik)%taz(1) = this(ik)%internal(4)
            
         end if
         
         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 
         cd_length = 1.d0

         if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
              ( this(ik)%CDAN == i_DKJCx ) .or. &
              ( this(ik)%CDAN == i_PLPLx ) .or. &
              ( this(ik)%CDAN == i_PLJCx ) .or. &
              ( this(ik)%CDAN == i_DKDKL )      &
            ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else

            call faterr(IAM,'IQS_EXPO_CZM_SPRING not implemented for this contact element')
            
         end if
           
         !call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,max(0.d0,this(ik)%gapTTbegin-this(ik)%internal(7)))
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,this(ik)%gapTTbegin-this(ik)%internal(7))         
         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd

         ! il faudrait verifier que c'est bien positif
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(7))/H
         
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0
		 
  !!!-------------------------------------
      case(i_IQS_EXPO_CZM_SPRING_P)

         this(ik)%i_law = tact_behav(ibehav)%ilaw

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

            ! saut de deplacement initial - on suppose que c'est ouvert
            this(ik)%internal(7) = max(0.d0, this( ik )%gapTTbegin)

         else

            ! beta
            this(ik)%taz(1) = this(ik)%internal(4)
            
         end if
         
         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 
         cd_length = 1.d0

         if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
              ( this(ik)%CDAN == i_DKJCx ) .or. &
              ( this(ik)%CDAN == i_PLPLx ) .or. &
              ( this(ik)%CDAN == i_PLJCx ) .or. &
              ( this(ik)%CDAN == i_DKDKL )      &
            ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else

            call faterr(IAM,'IQS_EXPO_CZM_SPRING_P not implemented for this contact element')
            
         end if
           
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,max(0.d0,this(ik)%gapTTbegin-this(ik)%internal(7)))
         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd

         ! il faudrait verifier que c'est bien positif
         this(ik)%covfreen=(this(ik)%gapTTbegin - this(ik)%internal(7))/H
         
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0

      !!!-------------------------------------
      !!!mr cohesive zone model with cohesion
      case(i_IQS_WET_CZM)
         this(ik)%i_law = i_IQS_WET_CZM

         if (.not. is_initialized) then
            
            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

         else
            this(ik)%taz(1) = this(ik)%internal(4)
            
         end if

         cd_length = 1.d0

         if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
              ( this(ik)%CDAN == i_DKJCx ) .or. &
              ( this(ik)%CDAN == i_PLPLx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else
            call faterr(IAM,'IQS_WET_CZM not implemented for this contact element')
         end if

         un = max(0.d0,this(ik)%gapTTbegin)       
         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,un)

         if (this(ik)%internal(4).eq.0.D0) then

            call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
            if (this(ik)%gapTTbegin .le. Wethk) then
               if (this(ik)%statusBEGIN == i_nknow)then
                  this(ik)%statusBEGIN=i_Wnnow
               else if (this(ik)%statusBEGIN == i_noctc)then
                  this(ik)%statusBEGIN=i_Wnctc
               else if (this(ik)%statusBEGIN == i_stick)then
                  this(ik)%statusBEGIN=i_Wstck
               else if (this(ik)%statusBEGIN == i_slibw)then
                  this(ik)%statusBEGIN=i_Wslbw
               else if (this(ik)%statusBEGIN == i_slifw)then
                  this(ik)%statusBEGIN=i_Wslfw
               end if
               this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
               this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + un/H
               this(ik)%corln    = -normalcoh*H
            else 
               this(ik)%covfreen = max(0.D0,this(ik)%gapTTbegin/H)
            end if

         else

            this(ik)%covfreen=un/H       
            this(ik)%covfreet=0.d0

         end if
         
      !!!-------------------------------------
      case(i_postGAP_IQS_MAC_CZM)

         this(ik)%i_law = i_postGAP_IQS_CZM

         !!!fd
         !!!fd Au premier pas on initialise les statuts standards a Cxxx 
         !!!fd apres si on a un nouveau contact avec cette loi ou si beta=0
         !!!fd il est comme du GAP_SGR_CLB
         !!!fd        
         if (.not. is_initialized) then

            if (this(ik)%statusBEGIN == i_nknow) this(ik)%statusBEGIN=i_Cnnow

            call init_CZM(ibehav,this(ik)%internal,this(ik)%taz)

            !fd on stoque le g0

            this(ik)%internal(6) = this(ik)%gapTTbegin       
         else
            this(ik)%taz(1) = this(ik)%internal(4)

         end if
         !!!fd On a deja recupere les internals dans le processus par defaut
         !!!fd On remonte la valeur de la longueur de la surface de contact
         !!!fd
         !!!fd a mettre au propre dans une routine         
         !!!fd 
            
         cd_length = 1.d0

         if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
              ( this(ik)%CDAN == i_DKJCx ) ) then
            cd_length = get_length( this(ik)%CDAN, this(ik)%icdan )
         else if (this(ik)%CDAN == i_PLPLx) then
            !!!fd&hb font les porcs         cd_length=get_length_PLPLx(this(ik)%icdan)
            cd_length=0.02
         else if (this(ik)%CDAN == i_DKDKL) then
            cd_length=0.05
         else

            call faterr(IAM,'postGAP_IQS_MAC_CZM not implemented for this contact element')
         end if
   
         un = max(0.d0,(this(ik)%gapTTbegin-this(ik)%internal(6)))       
         
         call prep_CZM(ibehav,this(ik)%internal,cd_length,0.d0,un)
         !!!fd
         !!!fd C'est une loi en gap on prepare donc le changement de variables
         !!!fd
         this(ik)%covfreen=un/H       
         !!!fd
         !!!fd c'est une loi en vitesse tangentielle
         !!!fd
         this(ik)%covfreet=0.d0
       
       
      !!!-------------------------------------
      case(i_GAP_SGR_CLB_WEAR)
         this(ik)%i_law = i_GAP_SGR_CLB_WEAR
         
         call get_kwear(ibehav,this(ik)%kcdwear,this(ik)%kanwear)
         this(ik)%covfreen=this(ik)%internal(1)

      !!!-------------------------------------
      case(i_IQS_BW_CLB)
         this(ik)%i_law = i_IQS_BW_CLB

         Freqik = get_Frequency_BW(ibehav) 
         if ( this(ik)%internal(1) .eq. 0.D0 ) then
            
            this(ik)%internal(1) = H*Freqik
         else

            TshVlt = get_TshVlt_BW(ibehav)

            if ( abs(this(ik)%vltBEGIN) .lt. TshVlt ) then
               !mr the motion is not relevant to consider an increment of internal(1)
            else  
               this(ik)%internal(1) = this(ik)%internal(1) + H*Freqik
            end if
         end if

         nbc = this(ik)%internal(1)

         this(ik)%fric        = get_fric_BW(ibehav,nbc)
         this(ik)%internal(2) = get_threshold_BW(ibehav,nbc)
         this(ik)%internal(3) = get_alpha_BW(ibehav)
         this(ik)%internal(4) = this(ik)%fric
         ! PRINT*,nbc,this(ik)%fric,this(ik)%internal(2),this(ik)%internal(3)

      !!!-------------------------------------
      case(i_IQS_SGR_CLB_WEAR)
         this(ik)%i_law = i_IQS_SGR_CLB_WEAR
         !mr normalcoh is the initial cohesion
         !mr tangalcoh is the dynamic cohesion
         
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         
         if (Nstep == 1)then
            this(ik)%internal(1) = normalcoh ! static cohesion
            this(ik)%internal(2) = tangalcoh ! dynamic cohesion
            this(ik)%internal(3) = 0         ! 0 clean status ( 1 for broken )
         end if
         
         if ( this(icdan)%internal(3) == 0 ) then 
            normalcoh = this(icdan)%internal(1)
         else
            normalcoh = this(icdan)%internal(2)
         end if
         
         if (this(ik)%gapTTbegin .le. Wethk) then
            if (this(ik)%statusBEGIN == i_nknow)then
               this(ik)%statusBEGIN=i_Wnnow
            else if (this(ik)%statusBEGIN == i_noctc)then
               this(ik)%statusBEGIN=i_Wnctc
            else if (this(ik)%statusBEGIN == i_stick)then
               this(ik)%statusBEGIN=i_Wstck
            else if (this(ik)%statusBEGIN == i_slibw)then
               this(ik)%statusBEGIN=i_Wslbw
            else if (this(ik)%statusBEGIN == i_slifw)then
               this(ik)%statusBEGIN=i_Wslfw
            end if
            this(ik)%covfreet=-normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen=-normalcoh*this(ik)%Wnn*H+max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln   =-normalcoh*H
         else 
            this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)
         end if
         
      case(i_COUPLED_DOF)
         this(ik)%i_law = i_COUPLED_DOF

      case(i_BROKEN_DOF)
         this(ik)%i_law = i_BROKEN_DOF

         call get_threshold(ibehav,this(ik)%threshold)

         if (.not. is_initialized) then
            this(ik)%internal(1) = 1.0
         end if

      case(i_PLASTIC_COUPLED_DOF)
         this(ik)%i_law = i_PLASTIC_COUPLED_DOF
         
      case(i_TANGENTIAL_COUPLED_DOF)
         this(ik)%i_law = i_TANGENTIAL_COUPLED_DOF
         
      case(i_NORMAL_COUPLED_DOF)
         this(ik)%i_law = i_NORMAL_COUPLED_DOF
         
      case(i_PERIO_DOF)
         this(ik)%i_law = i_PERIO_DOF
         
         !!!fd on doit introduire la correction -E Ycd->an
         !!!fd quelle convention sur le gap ?
         !!!fd a priori ici est plutot gapTT = Yan->cd
         !!!
         !!!fd on recuper la defo macro E_xx,E_yy,E_xy
         !!!
         call get_periodic_strain(ibehav,E)
         !!!
         !!!fd on passe gapTT en x,y -> Y
         !!!
         Y_x = this(ik)%gapTTbegin * this(ik)%nx
         Y_y = this(ik)%gapTTbegin * this(ik)%ny
         !!!
         !!!fd on calcule E.Y =<E_xx Y_x + E_xy Y_y, E_yx Y_x + E_yy Y_y>
         !!!
         EY_x = E(1)*Y_x + E(3)*Y_y
         EY_y = E(3)*Y_x + E(2)*Y_y
         !!!
         !!!fd on le passe en n,t
         !!!fd le repere est t,n direct donc tx=ny ty=-nx
         !!!
         this(ik)%covfreet= (EY_x * this(ik)%ny) + (EY_y * (-this(ik)%nx))
         this(ik)%covfreen= (EY_x * this(ik)%nx) + (EY_y *  this(ik)%ny)
      !!!
      case(i_BRITTLE_COATING_CLB)
        this(ik)%i_law = i_BRITTLE_COATING_CLB
        
        call get_g0(ibehav,g0)
        call get_forcePERgap(ibehav,forcePERgap)
        call get_snmax(ibehav,snmax)
      
        if( snmax > forcePERgap*(this(ik)%gapTTbegin-g0) ) then
          this(ik)%covfreen = this(ik)%gapTTbegin/H + (this(ik)%Wnn*H*g0*forcePERgap)
          this(ik)%covfreet = this(ik)%Wtn*H*g0*forcePERgap
        else
          this(ik)%covfreen = max(0.d0,this(ik)%gapTTbegin/H)
        end if

      !!!----------------------------------------
      CASE(i_NARD_ROD)
        this(ik)%i_law = i_NARD_ROD

        CALL get_coh(ibehav,normalcoh,tangalcoh,Wethk)

        !IF (.not. is_initialized) THEN
        IF (Nstep == 1) THEN

          !fd on stoque le max(0.,g0)             
          this(ik)%internal(1) = max(0.d0,this(ik)%gapTTbegin) 

          IF (this(ik)%CDAN .ne. i_PTPT2 ) THEN           
            CALL faterr(IAM,'NARD_ROD not implemented for this contact element')
          END IF

          !an on initialise le deplacement total tangent a 0   
          this(ik)%internal(2:3) = 0.d0 
            
          ! traitement specifique pour PTPT2 : detection paires cohesives
          ! faite avant la simus (NETWORK.DAT)

          this(ik)%statusBEGIN=i_Cstck

	END IF


        !! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * !!
        ! Raideurs de contact et amortissement

        CALL get_nard_coeff(ibehav,this(ik)%internal,Ksn,Kst,Kvn,Kvt)

        !! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * !!

        ! Criteres de rupture normal en traction
        Wethk=this(ik)%internal(1)+((this(ik)%internal(4)*normalcoh)/Ksn)

        ! Criteres de rupture tangent
        Dtmax = (this(ik)%internal(4)*tangalcoh)/Kst

	! Critere rupture energie
        ! Wethk=normalcoh/(((ksn*((this(ik)%gapTTbegin-this(ik)%internal(1))**2))+(kst*(this(ik)%internal(3))**2)) &
        !      	     /this(ik)%internal(2))


        !! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * !!

        ! critere en deplacement

        IF ((this(ik)%statusBEGIN == i_Cstck)        .AND. &
            (this(ik)%gapTTbegin .LE. Wethk)         .AND. &
            (abs(this(ik)%internal(2)) .LE. Dtmax) ) THEN 

         ! critere en energie
         ! IF ((this(ik)%statusBEGIN == i_Cstck) .AND. Wethk .gt. 1.d0) THEN 


            this(ik)%covfreen= this(ik)%gapTTbegin/H  &
	                     + this(ik)%Wnn*(this(ik)%internal(1)*Ksn*H+this(ik)%gapTTbegin*Kvn) &
                             - this(ik)%Wnt*H*kst*this(ik)%internal(2) ! increment tangent

            this(ik)%covfreet= this(ik)%Wtn*(this(ik)%internal(1)*Ksn*H+this(ik)%gapTTbegin*Kvn) &
                             - this(ik)%Wtt*H*kst*this(ik)%internal(2) ! increment tangent
           
        ELSE
          ! permet de savoir comment ca a casse: 1 normal, 2 tangent, 3 mixte
          IF ((this(ik)%internal(1) .NE. 1.d0) .AND. (this(ik)%internal(1) .NE. 2.d0) .AND. (this(ik)%internal(1) .NE. 3.d0)) THEN

            IF ( (this(ik)%gapTTbegin .GT. Wethk) .AND. (abs(this(ik)%internal(2)) .GT. Dtmax) ) THEN

              this(ik)%internal(1) = 3.d0

            ELSE IF ( (this(ik)%gapTTbegin .GT. Wethk) ) THEN

              this(ik)%internal(1) = 1.d0

            ELSE IF ( (abs(this(ik)%internal(2)) .GT. Dtmax) ) THEN

               this(ik)%internal(1) = 2.d0

            END IF 
          END IF

          this(ik)%statusBEGIN = i_noctc

        END IF
        
      case default
         call LOGMES('WARNING: default case selected')
         call LOGMES(tact_behav(ibehav)%lawty)
      end select
    
555   format(1X,'  this(',I5,')%gapREF =',D12.5,' < 1.D-18')

      !!!--------------------------------------
      !!! Warning non uniqueness cases
      !!!--------------------------------------
       
       det          = this(ik)%WWtt*this(ik)%WWnn-this(ik)%Wtn*this(ik)%Wnt
       this(ik)%det = det
	          
       if (det .lt. 1.D-24 .and. &
            tact_behav(ibehav)%ilaw /= i_ELASTIC_ROD .and. &
            tact_behav(ibehav)%ilaw /= i_VOIGT_ROD ) then

          write(cout,"(1X,'    WWtt*WWnn-Wtn*Wnt (',I5,') =',D12.5,' < 1.D-24')") ik,det
          call LOGMES(cout)
       end if

       forward = this(ik)%WWnn-this(ik)%fric*this(ik)%Wnt
       this(ik)%forward = forward

       if (forward .le. 1.D-18) then
          this(ik)%forward = 0.d0
       end if
       
       backward=this(ik)%WWnn+this(ik)%fric*this(ik)%Wnt
       this(ik)%backward=backward
       
       if (backward .le. 1.D-18) then

          this(ik)%backward = 0.d0
          
       end if
       
       !!!-------------------------------------
       !!! Computing free local vlocy
       !!!-------------------------------------

       call prjj_(ik,this(ik)%vfreet,this(ik)%vfreen,iVfree)


       ! print*,' *,this(ik)%vfreet,this(ik)%vfreen = ', this(ik)%vfreet,this(ik)%vfreen
       ! print*,' backward = ', backward
       ! print*,' forward = ', forward
       
       
       !!!-------------------------------------
       !!! Computing free local vlocy
       !!!-------------------------------------

       this(ik)%statuscheck=i_nknow
       this(ik)%status     =i_nknow

       if(get_pressure_flag(ibehav) .eq. 4 .and.  this(ik)%taz(1) /= 1.d0) then 
           call get_external_pressure_(ik,this(ik)%taz(3))
       endif
	   
	   
	   ! print*, 'this(ik)%rln , this(ik)%rlt , this(ik)%vln , this(ik)%vlt :: ', this(ik)%rln , this(ik)%rlt , this(ik)%vln , this(ik)%vlt !! ali boukham


       
    end do

    is_initialized = .true.

  end subroutine prep_nlgs

  !!!------------------------------------------------------------------------ 
  !!!mr 2004-09-23
  !!!
  !!! The parallel version (openMP) of the Non Linear Gauss Seidel algorithm is now operating;
  !!! To deal with new laws in the nlgs subroutine, new variables should be introduced
  !!! as SHARED (default) or PRIVATE variables in the OpenMP directives.
  !!!
  !!! refer to :
  !!!
  !!! "A parallel version of the Non Smooth Contact Dynamics algorithm applied to the
  !!!  simulation of granular material",
  !!!  M. Renouf, F. Dubois, P. Alart, J. Comp. Appl. Math. vol. 168(2004) pages 375-382
  !!!------------------------------------------------------------------------  

  !> this routine compute one iteration of NLGS
  !> depending on i_what (i_iter | i_check | i_post) it computes and store additional things
  subroutine solve_nlgs(i_what)

    implicit none
    !                         1234567890123456
    character(len=16) :: IAM='nlgs::solve_nlgs'
    character(len=80) :: cout
    integer           :: i_what

    ! contact index
    integer(kind=4)  :: ik,ikk
    ! managing sdl
    integer(kind=4)  :: iadj,ikjl,ibdy,istart,iistart

    ! solver 
    real(kind=8)     :: DET,forward,backward !,DFT,DFN,FFN,Cforward,Cbackward

    ! working variables
    real(kind=8)     ::   vtik,  vnik,  rtik,  rnik

    ! managing contact law
    real(kind=8)     :: fricik
    integer(kind=4)  :: ibehav,ilaw
    
    ! values at the beginning of the iteration
    ! ... impulse
    real(kind=8)     ::  rltik, rlnik
    ! ... vfree + Sum W^ab r^b
    real(kind=8)     ::  vltik, vlnik
    ! ... W^aa rlik^a 
    real(kind=8)     :: Wrltik,Wrlnik
    ! ...  vfree + Sum W^ab r^b - W^aa rlik^a 
    real(kind=8)     :: vlocfreetik,vlocfreenik
    ! ... auxiliary variables: vlocfree + covfree
    real(kind=8)     :: vvlocfreetik,vvlocfreenik
    ! ... auxiliary W
    real(kind=8)     :: WWttik,WWtnik,WWntik,WWnnik

    ! updated auxiliary variables
    real(kind=8)     :: vvltik,vvlnik,rrltik,rrlnik
    integer(kind=4)  :: sstatusik
    
    ! genuine values at the end of the iteration i
    ! ... impulse
    real(kind=8)     :: rltiki,rlniki
    ! ... W^aa rliki^a
    real(kind=8)     :: Wrltiki,Wrlniki
    ! ... velocity = vlocfreeik + W^aa rliki^a 
    real(kind=8)     :: vltiki,vlniki

    ! calcul norme
    real(kind=8)     :: rloct,rlocn,modrl,vlt,vln,Dvlt,Dvln 
    real(kind=8)     :: WRRmin,WRRmax,alphaik,fnik

    !
    real(kind=8)     :: normalcoh,tangalcoh,Wethk
    
    !fd czm 
    real(kind=8)     :: Hradh_t,Hradh_n,Hp
    logical          :: is_cohesive
    real(kind=8)     :: k_n,k_t,detJ,Att,Atn,Ant,Ann,vt,vn,ut,un,Ttt,Ttn,Tnt,Tnn

    !fd nosldt
    real(kind=8)     :: bt

    !fd
    real(kind=8)     :: snmax, forcePERgap, g0

    !< nard_rod
    real(kind=8) :: Ksn,Kst,Kvn,Kvt
    ! nard_rod />

    !fd expo_spring
    real(kind=8) :: cn,ct,s1,s2,G1,G2,mu_g,eta,k1,k2,bcth,bsth


    !fd
    real(kind=8)     :: gapTT

    integer(kind=4)  :: err

    !fd auxiliary variable
    real(kind=8)     :: AA,vdt,kkk

    if (nb_CDAN == 0) return
   
    !fd ce commentaire a ete deplace dans prep ;
    ! on suppose que celui qui utilise ca sait ce qu'il fait
    ! sinon ca pourri totalement l'affichage 
    ! if ( JACOBI_SOLVER .and. RELAX >0.999d0 ) then
    !   CALL LOGMES('WARNING!! Usage of a jacobi solver with RELAX=1 may not converge.')
    ! endif
	
	! print*, 'i_iter , i_check , i_post :: ', i_iter , i_check , i_post !! ali boukham à supprimer

    if (i_what == i_iter) nlgs_loop = nlgs_loop + 1
 
    WRRmin= 1.D24
    WRRmax= 0.D0

    !Pour solveur jacobi
    if( JACOBI_SOLVER ) then
      !$OMP PARALLEL DEFAULT(SHARED)                                                            &
      !$OMP PRIVATE(ik)
      !$OMP DO SCHEDULE(RUNTIME)
      do ik=1,nb_CDAN
         this(ik)%rlt_jacobi = this(ik)%rlt
         this(ik)%rln_jacobi = this(ik)%rln
      end do
      !$OMP END DO
      !$OMP END PARALLEL   
    end if

    !$OMP PARALLEL DEFAULT(SHARED)                                                            &
    !$OMP PRIVATE(ik,ikk,istart,ikjl,iadj,iistart,                                            &
    !$OMP         vtik,vnik,rtik,rnik,fricik,ibehav,ilaw,                                     &
    !$OMP         rltik,rlnik,vltik,vlnik,Wrltik,Wrlnik,vlocfreetik,vlocfreenik,              &
    !$OMP         vvlocfreetik,vvlocfreenik,WWttik,WWntik,WWtnik,WWnnik,                      &
    !$OMP         rrltik,rrlnik,vvltik,vvlnik,sstatusik,                                      &
    !$OMP         rltiki,rlniki,Wrltiki,Wrlniki,vltiki,vlniki,                                &
    !$OMP         rloct,rlocn,vlt,vln,Dvlt,Dvln,DVDV,DVDVRR,DVoR,WRR,                         &
    !$OMP         Hradh_t,Hradh_n,Hp,is_cohesive,k_n,k_t,detJ,Att,Atn,Ant,Ann,vt,vn,ut,un,    &
    !$OMP         Ttt,Ttn,Tnt,Tnn,det,forward,backward,                                       &
    !$OMP         modrl,alphaik,fnik,snmax,bt,g0,forcePERgap,err,gapTT,Ksn,Kst,Kvn,Kvt,       &
    !$OMP         AA,vdt,kkk )

   
    !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:SumDVDVRR,SumDVDV,SumWRR,SumWRWR,SumDVoR,          &
    !$OMP                      Nnoact,Nstick,Nslide,NOKsta,Nnoctc,WNslide,SNslide,     &
    !$OMP                      Ncompr,Ntract,dynstat,Nvnish,Nhover,Nb_RGR,WNstick,SNstick,    &
    !$OMP                      NOKweak,NOKstrg)                                               &
    !$OMP                      REDUCTION(MAX:MaxDVDV,MAXDVDVRR,WRRmax) REDUCTION(MIN:WRRmin) 


    !!! ---------------------------------------------------------------------------------------
    !!! Primary variables are impulse reactions;
    !!!   rltik, rlnik are values of the impulse reaction before correction
    !!!   through the single contact solver are
    !!!   rltiki, rlniki are values of the impulse reaction after correction
    !!!   through the single contact solver are
    !!! 
    !!!   vltik, vlnik are values of the relative velocity before correction
    !!!   through the single contact solver 
    !!!   vltiki, vlniki are values of the relative velocity after correction
    !!!   through the single contact solver
    !!!   Since the primary variables are the impulse reactions, the computation 
    !!!   of vltiki, vlniki, is omitted during iterations.
    !!!----------------------------------------------------------------------------------------

    ! Various computations for case i_what = 'i_iter' or case what = 'i_post ' or case what = 'i_check'
    
    do ikk=1,nb_CDAN

      ! Changing at random computational ordering of contact elements
      
      ik=IALEAT(ikk)

      ! fd desactive pour le moment
      ! ! if the contact doesn't need to be recomputed we skip it
      ! if ( this(ik)%is_in_queue == 0 ) cycle 
	  
	  ! print*, 'this(ik)%forecast = ', this(ik)%forecast !! ali boukham
      
      if (this(ik)%forecast == i_noact) then
         
         rltik = 0.D0
         rlnik = 0.D0
         vltik = this(ik)%vfreet
         vlnik = this(ik)%vfreen
         
         rltiki= 0.D0
         rlniki= 0.D0
         vlocfreetik=vltik
         vlocfreenik=vlnik

         sstatusik=i_noctc

         Wrltik = 0.d0
         Wrlnik = 0.d0

         !fd desactive pour le moment
         ! this(ik)%is_not_null=0
         
      else if (this(ik)%forecast == i_acton) then
         
         if (i_what == i_iter) then
            ! Skipping if forces are repeatedly equal to 0. but safeguardly checking every i*(i+1)/2 iterations
            if (this(ik)%rln == 0.D0 .and. this(ik)%rlt == 0.D0) then
               this(ik)%ivnish=this(ik)%ivnish+1
               if (this(ik)%ivnish < this(ik)%iskip*(this(ik)%iskip+1)/2) then
                  cycle
               else
                  this(ik)%iskip = this(ik)%iskip+1
               end if

               !fd desactive pour le moment
               ! if (this(ik)%is_not_null /= 0 .and. nlgs_loop > 1) then
               !    print*,ik,this(ik)%vfreen
               !    call faterr(IAM,'wtf is_not_null must be 0')
               ! endif   
            else
               this(ik)%ivnish= 0
               this(ik)%iskip = 1

               !fd desactive pour le moment               
               ! if (this(ik)%is_not_null /= 1) call faterr(IAM,'wtf is_not_null must be 1')                
            end if
         else
            this(ik)%ivnish= 0
            this(ik)%iskip = 1
         end if

         !!! Computing vlocfree ***************************
       
         rltik =this(ik)%rlt
         rlnik =this(ik)%rln
         
         Wrltik = this(ik)%Wtt*rltik + this(ik)%Wtn*rlnik
         Wrlnik = this(ik)%Wnt*rltik + this(ik)%Wnn*rlnik
         
         ! Computing_______________________________ H* p* invM p H Rloc(ik)
         if( .not. SDLactif )then
            ! Computing____________________________ H* p* invM p H Rloc
            call vitrad_(ik,iVaux_e_invM_t_Ireac)
            call prjj_(ik,vtik,vnik,iVaux_)
            vltik = this(ik)%vfreet + vtik
            vlnik = this(ik)%vfreen + vnik
            ! Computing____________________________ H* p* invM p H Rloc - H* p* invM p H Rloc(ik)     
            !                                       = vlocfree(ik)
            vlocfreetik = vltik - Wrltik            
            vlocfreenik = vlnik - Wrlnik 
            !
         else
		    ! print*, '$$$$ i m here $$$$'! ali boukham 
            ! Computing contribution of contacts jl
            vlocfreetik = 0.D0
            vlocfreenik = 0.D0
            istart = this(ik)%istart
            
            if( DDM_SCHWARTZ ) then
               call prjj_(ik, vlocfreetik, vlocfreenik, iVddm_ )
            end if
            
            if (this(ik)%nbadj /= 0) then
               do iadj=1,this(ik)%nbadj
                  ikjl = this(ik)%adjjl(iadj)

                  !fd desactive pour le moment
                  ! ! if we know that the reaction is equal to 0 we do not perform matmul
                  ! if (this(ikjl)%is_not_null == 0) cycle
                  
                  iistart = istart + 4*iadj

                  ! print*, 'JACOBI_SOLVER', JACOBI_SOLVER !! ali boukham
                  if( .not. JACOBI_SOLVER ) then
                     ! print*, 'Wab(iistart) , Wab(iistart-1) , Wab(iistart-2) , Wab(iistart-3)' , Wab(iistart) , Wab(iistart-1) , Wab(iistart-2) , Wab(iistart-3) !! ali boukham
                     ! solveur Gauss-Seidel                     
                     vlocfreetik = vlocfreetik + Wab(iistart-3)*this(ikjl)%rlt &
                                               + Wab(iistart-2)*this(ikjl)%rln
                     vlocfreenik = vlocfreenik + Wab(iistart-1)*this(ikjl)%rlt &
                                               + Wab(iistart  )*this(ikjl)%rln

                  else
                     ! solveur Jacobi
                     vlocfreetik = vlocfreetik + Wab(iistart-3)*this(ikjl)%rlt_jacobi &
                                               + Wab(iistart-2)*this(ikjl)%rln_jacobi
                     vlocfreenik = vlocfreenik + Wab(iistart-1)*this(ikjl)%rlt_jacobi &
                                               + Wab(iistart  )*this(ikjl)%rln_jacobi
                  end if

               end do
            end if

            vlocfreetik=vlocfreetik+this(ik)%vfreet            
            vlocfreenik=vlocfreenik+this(ik)%vfreen 

            !fd ce signe est normal il vient des 2 constructions differentes

            vltik = vlocfreetik+Wrltik       
            vlnik = vlocfreenik+Wrlnik       

         end if

         !!! Convert to auxiliary ********************************************************

         WWttik = this(ik)%WWtt ; WWtnik=this(ik)%Wtn 
         WWntik = this(ik)%Wnt  ; WWnnik=this(ik)%WWnn

         vvlocfreetik=this(ik)%covfreet+vlocfreetik
         vvlocfreenik=this(ik)%covfreen+vlocfreenik

         fricik = this(ik)%fric
         ilaw   = this(ik)%i_law
         ibehav = this(ik)%lawnb

         if (.FALSE.) then
           print*,'@iter inter ',ik
           print*,'vlocfree(n:t)ik ',vlocfreenik,vlocfreetik
           print*,'vvlocfree(n:t)ik ',vvlocfreenik,vvlocfreetik
           print*,'vl(n:t)ik ',vlnik,vltik
         endif
        
         !!! Calling single contact solver ***********************************************

         select case(ilaw)
         !!!------------------------------------
         case(i_ELASTIC_ROD,i_VOIGT_ROD)
            rrltik    = 0.D0
            rrlnik    =-vvlocfreenik/WWnnik
            sstatusik =i_stick

         !!!------------------------------------
         case(i_TEX_SOL)
            vvlnik = 0.D0
            rrlnik =-vvlocfreenik/WWnnik
            
            if ( (fricik*this(ik)%Wtt*rrlnik+vvlocfreetik) .lt. 0.D0 ) then
               rrltik = fricik*rrlnik
            else if ( (-fricik*this(ik)%Wtt*rrlnik+vvlocfreetik) .gt. 0.D0 ) then
               rrltik = -fricik*rrlnik
            else
               rrltik = -vvlocfreetik/WWttik
            end if

            !fd et le fucking status ?
            
         !!!-------------------------------------
         case(i_TEX_SOL_UNILAT,i_RIGID_WIRE,i_ELASTIC_WIRE,i_VOIGT_WIRE)

            !fd tous les problemes avec graphe inverse sont traites ensemble.
            
            if ( vvlocfreenik >= 0.D0 ) then
               vvlnik   = -this(ik)%covfreen
               rrlnik   = -vvlocfreenik/WWnnik
               sstatusik= i_stick
               !this(ik)%is_not_null = 1               
            else
               vvlnik   = vvlocfreenik - this(ik)%covfreen
               rrlnik   = 0.D0
               sstatusik= i_noctc
               !this(ik)%is_not_null = 0
            end if
            vvltik = 0.D0
            rrltik = 0.D0

         !!!--------------------------------------
         case(i_BRITTLE_ELASTIC_WIRE)
            if (vvlocfreenik .gt. 0.D0) then

              rrlnik = -vvlocfreenik/WWnnik 
              vvlnik = -this(ik)%covfreen

              call get_snmax(ibehav,snmax)
              if (rrlnik < -H*snmax) then
                sstatusik=i_vnish
                rrlnik = 0.d0
                vvlnik  = vvlocfreenik - this(ik)%covfreen
                !this(ik)%is_not_null = 0
              else
                sstatusik=i_stick
                !this(ik)%is_not_null = 1                 
              endif
            else
              rrlnik = 0.D0
              vvlnik  = vvlocfreenik - this(ik)%covfreen
              sstatusik=i_noctc
              !this(ik)%is_not_null = 0              
            endif
            vvltik = 0.D0
            rrltik = 0.D0

         !!!--------------------------------------
         case(i_TANGENTIAL_COUPLED_DOF)
            
            vvltik=0.d0
            rrltik= -vvlocfreetik/WWttik 

            vvlnik=vvlocfreenik + WWntik*rrltik
            rrlnik= 0.d0

            sstatusik=i_stick

         !!!--------------------------------------
         case(i_NORMAL_COUPLED_DOF)
            
            vvlnik   = 0.D0
            rrlnik   =-vvlocfreenik/WWnnik

            vvltik   = vvlocfreetik + WWtnik*rrlnik
            rrltik   = 0.D0

            sstatusik= i_stick

         !!!--------------------------------------
         case(i_COUPLED_DOF,i_PERIO_DOF)
            
            call coupled_dof_solver_(this(ik)%det, &
                                     WWttik,WWtnik,WWntik,WWnnik, &
                                     vvlocfreetik,vvlocfreenik, &
                                     sstatusik,rrltik,rrlnik)

         !!!--------------------------------------
         case(i_BROKEN_DOF)
            
            if ( this(ik)%internal(1) .eq. 1.0 ) then
               call coupled_dof_solver_(this(ik)%det, &
                                        WWttik,WWtnik,WWntik,WWnnik, &
                                        vvlocfreetik,vvlocfreenik, &
                                        sstatusik,rrltik,rrlnik)
               !this(ik)%is_not_null = 1
               if (rrlnik.gt.this(ik)%threshold) then
                  this(ik)%internal = 0.0
                  vvlnik   = vvlocfreenik - this(ik)%covfreen
                  rrlnik   = 0.D0
                  rrltik   = 0.D0
                  sstatusik= i_noctc
                  !this(ik)%is_not_null = 0                  
               end if

            else

               vvlnik   = vvlocfreenik - this(ik)%covfreen
               rrlnik   = 0.D0
               rrltik   = 0.D0
               sstatusik= i_noctc
               !this(ik)%is_not_null = 0
            end if

         !!!-------------------------------------
         case(i_PLASTIC_COUPLED_DOF)
          
            call plastic_coupled_dof_solver_(this(ik)%det, &
                                             WWttik,WWtnik,WWntik,WWnnik, &
                                             vvlocfreetik,vvlocfreenik, &
                                             sstatusik,rrltik,rrlnik,fricik)


         !!!-------------------------------------
         case(i_GAP_SGR_CLB_WEAR)

            call mu_SC_wear_solver_(this(ik)%internal,this(ik)%vlt,this(ik)%vln, & 
                                    this(ik)%vlnBEGIN,this(ik)%gapTTbegin,this(ik)%fric, &
                                    this(ik)%kcdwear,this(ik)%kanwear, &
                                    WWttik,WWtnik,WWntik,WWnnik,vvlocfreetik,vvlocfreenik,sstatusik, &
                                    rrltik,rrlnik)
            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0
            

!!!-------------------------------------
         case(i_MAC_CZM,i_MSMP_CZM,i_MAL_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER,i_TH_CZM,i_ABP_CZM, &
              i_EXPO_CZM, &
              i_IQS_MAC_CZM, i_IQS_MAL_CZM, i_IQS_TH_CZM, i_IQS_ABP_CZM,i_IQS_EXPO_CZM, &
              i_postGAP_IQS_CZM,i_IQS_WET_CZM,i_ELASTIC_REPELL_MAC_CZM,i_TOSI_CZM,i_TOSI_CZM_INCRE)
            
            call iter_CZM(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hp)

            call get_fric_CZM(ibehav,this(ik)%taz,fricik)

            if (is_cohesive) then

               !fd since beta is fixed during nlgs iterations                
               detJ = 1.d0 /(((1.d0 - (WWnnik*k_n))*(1.d0 - (WWttik*k_t))) - ((WWtnik*k_n)*(WWntik*k_t)))
               

               !fd be careful kn=-S*beta*beta*H*H*cn ...
               Ann = (1.d0 - (WWttik*k_t))*detJ
               Att = (1.d0 - (WWnnik*k_n))*detJ
               Ant = WWntik*k_t*detJ 
               Atn = WWtnik*k_n*detJ 
               
               ut = vvlocfreetik + (WWttik * Hradh_t) + (WWtnik * Hp) 
               un = vvlocfreenik + (WWntik * Hradh_t) + (WWnnik * Hp) 
               
               vvlocfreetik = (Att * ut) + (Atn * un)
               vvlocfreenik = (Ant * ut) + (Ann * un)
               
               Ttt = (Att * WWttik) + (Atn * WWntik)           
               Ttn = (Att * WWtnik) + (Atn * WWnnik) 
               Tnt = (Ant * WWttik) + (Ann * WWntik)
               Tnn = (Ant * WWtnik) + (Ann * WWnnik)  
               
               WWttik = Ttt; WWtnik = Ttn
               WWntik = Tnt; WWnnik = Tnn
               
               det = (WWttik*WWnnik)-(WWtnik*WWntik)
               
               forward  = WWnnik - (fricik*WWntik)
               if (forward .lt. 1d-18) forward = 0.d0 
               
               backward = WWnnik + (fricik*WWntik)
               if (backward .lt. 1d-18) backward = 0.d0
               
            else

               det      = this(ik)%det
               forward  = this(ik)%forward
               backward = this(ik)%backward
            
               ut = 0.d0
               un = 0.d0

               vvlocfreetik = vvlocfreetik + (WWtnik * Hp) 
               vvlocfreenik = vvlocfreenik + (WWnnik * Hp) 
               
            end if
         
            call mu_SC_std_solver_(det,forward,backward,fricik, &
                                   WWttik,WWtnik,WWntik,WWnnik, &
                                   vvlocfreetik,vvlocfreenik, &
                                   sstatusik,rrltik,rrlnik,err)
            
            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0
            
            if (err/=0) then
              call LOGMES(' ') 
              write(cout,'(1x,"WWnn= ",D14.7," WWnt= ",D14.7)') WWnnik,WWntik
              call LOGMES(cout)
              write(cout,'(1x,"WWtn= ",D14.7," WWtt= ",D14.7)') WWtnik,WWttik
              call LOGMES(cout)
              call LOGMES(' ') 
              write(cout,'("fric= ",D14.7)') fricik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("vvlocfreen= ",D14.7," vvlocfreet= ",D14.7)') vvlocfreenik,vvlocfreetik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("gap= ",D14.7)') this(ik)%gapTTbegin
              call LOGMES(cout)
              call LOGMES(' ')
              call print_info_(ik)
            endif 
            
            if (err == 1) then
              write(cout,"('WWnn(',I5,') - fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik
              call logmes(cout)
              write(cout,"('contact ',I0,' forward impossible')") ik
              call FATERR(IAM,cout)
            endif
           
            if (err ==-1) then
              write(cout,"('WWnn(',I5,') + fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik
              call logmes(cout)
              write(cout,"('contact ',I0,' backward impossible')") ik               
              call FATERR(IAM,cout)
            endif
           
            if (err == 3) then
              call FATERR(IAM,'wtf')
            endif   
            
            ! lourenco cap
            if (rrlnik > H*this(ik)%internal(1)*get_RNcap()) then
               rrltik = 0.d0
               rrlnik = 0.d0
               sstatusik=i_vnish
               !this(ik)%is_not_null = 0
            endif
			
!!!-------------------------------------
         case(i_EXPO_CZM_P,i_IQS_EXPO_CZM_P)
            
            call iter_CZM(ibehav,this(ik)%internal,this(ik)%taz,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hp)

            call get_fric_CZM(ibehav,this(ik)%taz,fricik)

            if (is_cohesive) then
			   
               !fd since beta is fixed during nlgs iterations                
               detJ = 1.d0 /(((1.d0 - (WWnnik*k_n))*(1.d0 - (WWttik*k_t))) - ((WWtnik*k_n)*(WWntik*k_t)))

               !fd be careful kn=-S*beta*beta*H*H*cn ...
               Ann = (1.d0 - (WWttik*k_t))*detJ
               Att = (1.d0 - (WWnnik*k_n))*detJ
               Ant = WWntik*k_t*detJ 
               Atn = WWtnik*k_n*detJ 
               
               ut = vvlocfreetik + (WWttik * Hradh_t) + (WWtnik * Hp) 
               un = vvlocfreenik + (WWntik * Hradh_t) + (WWnnik * Hp) 
               
               vvlocfreetik = (Att * ut) + (Atn * un)
               vvlocfreenik = (Ant * ut) + (Ann * un)
               
               Ttt = (Att * WWttik) + (Atn * WWntik)           
               Ttn = (Att * WWtnik) + (Atn * WWnnik) 
               Tnt = (Ant * WWttik) + (Ann * WWntik)
               Tnn = (Ant * WWtnik) + (Ann * WWnnik)  
               
               WWttik = Ttt; WWtnik = Ttn
               WWntik = Tnt; WWnnik = Tnn
               
               det = (WWttik*WWnnik)-(WWtnik*WWntik)
               
               forward  = WWnnik - (fricik*WWntik)
               if (forward .lt. 1d-18) forward = 0.d0 
               
               backward = WWnnik + (fricik*WWntik)
               if (backward .lt. 1d-18) backward = 0.d0
               
            else
			   
               det      = this(ik)%det
               forward  = this(ik)%forward
               backward = this(ik)%backward
            
               ut = 0.d0
               un = 0.d0

            end if
         
            call mu_SC_std_solver_(det,forward,backward,fricik, &
                                   WWttik,WWtnik,WWntik,WWnnik, &
                                   vvlocfreetik,vvlocfreenik, &
                                   sstatusik,rrltik,rrlnik,err)
            
            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0
            
            if (err/=0) then
              call LOGMES(' ') 
              write(cout,'(1x,"WWnn= ",D14.7," WWnt= ",D14.7)') WWnnik,WWntik
              call LOGMES(cout)
              write(cout,'(1x,"WWtn= ",D14.7," WWtt= ",D14.7)') WWtnik,WWttik
              call LOGMES(cout)
              call LOGMES(' ') 
              write(cout,'("fric= ",D14.7)') fricik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("vvlocfreen= ",D14.7," vvlocfreet= ",D14.7)') vvlocfreenik,vvlocfreetik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("gap= ",D14.7)') this(ik)%gapTTbegin
              call LOGMES(cout)
              call LOGMES(' ')
              call print_info_(ik)
            endif 
            
            if (err == 1) then
              write(cout,"('WWnn(',I5,') - fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik
              call logmes(cout)
              write(cout,"('contact ',I0,' forward impossible')") ik
              call FATERR(IAM,cout)
            endif
           
            if (err ==-1) then
              write(cout,"('WWnn(',I5,') + fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik
              call logmes(cout)
              write(cout,"('contact ',I0,' backward impossible')") ik               
              call FATERR(IAM,cout)
            endif
           
            if (err == 3) then
              call FATERR(IAM,'wtf')
            endif   
            
            ! lourenco cap
            if (rrlnik > H*this(ik)%internal(1)*get_RNcap()) then
               rrltik = 0.d0
               rrlnik = 0.d0
               sstatusik=i_vnish
               !this(ik)%is_not_null = 0
            endif

!!!-------------------------------------
         case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)

            !fd il s'agit d'un montage serie elastique + czm ( contact/frottant cohesif endommageable )

            !fd - en test - resolution dans l'esprit "diago"
            !fd ainsi on resoud d'abord la partie normale, puis la tangente

            call get_czm_expo_spring(ibehav,cn,ct,s1,s2,G1,G2,eta,k1,k2)
            call get_fric_CZM_spring(8,ibehav,this(ik)%taz,this(ik)%internal,fricik)

            ! partie normale

            sstatusik=i_nknow  
            
            ! cas de la compression (contact - elas)
            if ( vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik <= 0.d0 ) then

              ! seule la partie elastique travaille 
              WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*(k1*this(ik)%internal(9))*H*H)) 

              !fd 
              rrlnik = - (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik) / WWnnik                            

            ! cas de la traction (cohesif endo - elas)
            else

              if (this(ik)%taz(1) > 0.d0) then                  
                ! parties cohesif endo et elastiques travaillent 
                WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*(k1*this(ik)%internal(9))*H*H))
                WWnnik = 1.d0 + (WWnnik*this(ik)%internal(1)*this(ik)%taz(1)*(cn*this(ik)%internal(8))*H*H)                
                !fd on calcule l'allongement endo/H
                AA = (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik) / WWnnik
                !fd l'allongement endo cumule
                AA  = H*AA                
                !fd sa partie positive 
                AA = (dabs(AA)+AA)*0.5
                ! l'impulsion cohesive
                ! rrlnik = -this(ik)%internal(1)*this(ik)%internal(4)*cn*H*AA
                rrlnik = -this(ik)%internal(1)*this(ik)%taz(1)*(this(ik)%internal(8)*cn)*H*AA
              else
                ! c'est casse ... on garde le WWnnik mais reaction nulle                 
                WWnnik = 1.d0 
                rrlnik = 0.d0
                rrltik = 0.d0
                vdt   =  0.d0
                sstatusik=i_noctc                
              endif
            endif

            if (sstatusik /= i_noctc) then           
           
              ! partie tangente

              kkk = (this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(8))*H*H)*(this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*(k2*this(ik)%internal(9))*H*H)))            
            
              ! traction (cohesif endo - elas)
              if (rrlnik < 0.d0) then

                ! cohesif endo et elastiques travaillent 
                ! if (this(ik)%internal(4) > 0.d0) then
                if (this(ik)%taz(1) > 0.d0) then                 
                  WWttik = 1.d0 + kkk
                else
                  WWttik = 1.d0
                endif  

                ! vitesse endo tangentielle
                vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                      (kkk*this(ik)%internal(2)/H))/WWttik

                !fd 
                rrltik = -this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(8))*H*H*(vdt+this(ik)%internal(2)/H)
              
                sstatusik=i_stick
                
              ! compression (frottement/cohesif endo - elas)
              else
                ! 
                WWttik = this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*(k2*this(ik)%internal(9))*H*H)) 

                ! calcul de la prediction It+Itcohesif 
                rrltik = - (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                         (kkk*this(ik)%internal(2)/H)) / WWttik              
              
                ! il y a glissement 
                if ( dabs(rrltik) > fricik*rrlnik) then

                  ! si la prediction nous donne du glissement negatif 
                  if (rrltik < 0d0) then
                   
                    vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                          (kkk*this(ik)%internal(2)/H) - (WWttik*fricik*rrlnik))/(1.d0 + kkk)

                    !fd 
                    rrltik = - fricik*rrlnik -this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(8))*H*H*(vdt+(this(ik)%internal(2)/H))
                    
                    sstatusik=i_slibw
                  ! glissement positif   
                  else

                    vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                           (kkk*this(ik)%internal(2)/H) + (WWttik*fricik*rrlnik))/(1.d0 + kkk)

                    !fd 
                    rrltik = fricik*rrlnik -this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vdt+(this(ik)%internal(2)/H))
                    

                    sstatusik=i_slifw                    
                  endif
                else
                  rrltik = rrltik - (this(ik)%internal(1)*this(ik)%taz(1)*(ct*this(ik)%internal(8))*H*H*(this(ik)%internal(2)/H))
                 
                  !maj vdt
                  vdt=0.d0
                 
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

            sstatusik=i_nknow  

            ! cas de la compression (contact - elas)
            if ( vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + kkk*(this(ik)%internal(11)/H) <= 0.d0 ) then

              ! seule la partie elastique travaille 
              WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*k1*H*H)) 

              !fd 
              rrlnik = H*this(ik)%internal(1)*this(ik)%taz(1)*cn*this(ik)%internal(11) - &
                       (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + kkk*(this(ik)%internal(11)/H)) / WWnnik

              
            ! cas de la traction (cohesif endo - elas)
            else

              if (this(ik)%taz(1) > 0.d0) then                  
                ! parties cohesif endo et elastiques travaillent 
                WWnnik = this(ik)%Wnn+(1.D0/(this(ik)%internal(1)*k1*H*H))
                WWnnik = 1.d0 + (WWnnik*this(ik)%internal(1)*this(ik)%taz(1)*cn*H*H)                
                !fd on calcule l'allongement endo/H
                AA = (vlnik + this(ik)%covfreen - this(ik)%Wnn*rlnik + kkk*(this(ik)%internal(11)/H)) / WWnnik
                !fd l'allongement endo cumule
                AA  = H*AA                
                !fd sa partie positive 
                AA = (dabs(AA)+AA)*0.5
                ! l'impulsion cohesive
                rrlnik = -this(ik)%internal(1)*this(ik)%taz(1)*cn*H*(AA-this(ik)%internal(11))
              else
                ! c'est casse ... on garde le WWnnik mais reaction nulle                 
                WWnnik = 1.d0 
                rrlnik = 0.d0
                rrltik = 0.d0
                vdt   =  0.d0
                sstatusik=i_noctc                
              endif
            endif

            if (sstatusik /= i_noctc) then           
              ! partie tangente

              kkk = (this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H)*(this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*k2*H*H)))            
            
              ! traction (cohesif endo - elas)
              if (rrlnik < 0.d0) then

                ! cohesif endo et elastiques travaillent 
                if (this(ik)%taz(1) > 0.d0) then                 
                  WWttik = 1.d0 + kkk
                else
                  WWttik = 1.d0
                endif  

                ! vitesse endo tangentielle
                vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                      (kkk*(this(ik)%internal(2)-this(ik)%internal(10))/H))/WWttik

               !fd 
               rrltik = -this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vdt+this(ik)%internal(2)/H-this(ik)%internal(10)/H)              

               sstatusik=i_stick

              
              ! compression (frottement/cohesif endo - elas)
              else
                ! 
                WWttik = this(ik)%Wtt+(1.D0/(this(ik)%internal(1)*k2*H*H)) 

                ! calcul de la prediction It+Itcohesif 
                rrltik = - (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                        (kkk*(this(ik)%internal(2)-this(ik)%internal(10))/H)) / WWttik              
              
                ! il y a glissement 
                if ( dabs(rrltik) > fricik*rrlnik) then

                  ! si la prediction nous donne du glissement negatif 
                  if (rrltik < 0d0) then
                   
                    vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                          (kkk*(this(ik)%internal(2)-this(ik)%internal(10))/H) - (WWttik*fricik*rrlnik))/(1.d0 + kkk)

                    !fd 
                    rrltik = - fricik*rrlnik - &
                            this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vdt+(this(ik)%internal(2)/H)-(this(ik)%internal(10)/H))

                    sstatusik=i_slibw
                   
                  ! glissement positif   
                  else
 
                    vdt = (vltik + this(ik)%covfreet - this(ik)%Wtt*rltik + (this(ik)%internal(6)/H) - &
                          (kkk*(this(ik)%internal(2)-this(ik)%internal(10))/H) + (WWttik*fricik*rrlnik))/(1.d0 + kkk)

                    !fd 
                    rrltik = fricik*rrlnik - &
                             this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*(vdt+(this(ik)%internal(2)/H)-(this(ik)%internal(10)/H))


                    sstatusik=i_slifw                                       
                  endif
                else

                  rrltik = rrltik - (this(ik)%internal(1)*this(ik)%taz(1)*ct*H*H*((this(ik)%internal(2)-this(ik)%internal(10))/H))
                 
                  !maj vdt
                  vdt=0.d0
                  
                  sstatusik=i_stick                                                  
                endif
              endif   
            endif
!!!-------------------------------------
         case(i_IQS_CLB_nosldt, &
              i_GAP_SGR_CLB_nosldt)

            !fd on resoud classiquement:

            call mu_SC_std_solver_(this(ik)%det,this(ik)%forward,this(ik)%backward, & 
                                   fricik, WWttik,WWtnik,WWntik,WWnnik,vvlocfreetik,vvlocfreenik, &
                                   sstatusik,rrltik,rrlnik,err)
            
            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0

            
            if (err/=0) then
              call LOGMES(' ') 
              write(cout,'(1x,"WWnn= ",D14.7," WWnt= ",D14.7)') WWnnik,WWntik
              call LOGMES(cout)
              write(cout,'(1x,"WWtn= ",D14.7," WWtt= ",D14.7)') WWtnik,WWttik
              call LOGMES(cout)
              call LOGMES(' ') 
              write(cout,'("fric= ",D14.7)') fricik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("vvlocfreen= ",D14.7," vvlocfreet= ",D14.7)') vvlocfreenik,vvlocfreetik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("gap= ",D14.7)') this(ik)%gapTTbegin
              call LOGMES(cout)
              call LOGMES(' ')
              call print_info_(ik)
            endif 

            if (err == 1) then
              write(cout,"('WWnn(',I5,') - fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik               
              call logmes(cout)
              
              write(cout,"('contact ',I0,' forward impossible')") ik
              call FATERR(IAM,cout)
            endif              
            if (err ==-1) then
              write(cout,"('WWnn(',I5,') + fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik               
              call logmes(cout)

              write(cout,"('contact ',I0,' backward impossible')") ik               
              call FATERR(IAM,cout)
            endif

            if (err == 3) then
              call FATERR(IAM,'wtf')
            endif   
            
            !fd on teste si on tape les butees

            call get_offset(ibehav,bt)

            if (sstatusik == i_noctc) then

               !fd chelou?? pour quoi pas vvltik
               vltiki=vvlocfreetik

               !fd si on ne s'ecarte pas de la butee

               !fd chelou?? 
               if (dabs(this(ik)%internal(1) + (H*vltiki)) < bt) then

               else

                 vltiki= 0.d0
                 rrltik   = -vvlocfreetik/WWttik
                 sstatusik=i_stick
                 
               endif

            else

               vltiki = vvlocfreetik + WWtnik*rrlnik + WWttik*rrltik

               !fd si on ne s'ecarte pas de la butee

               if (dabs(this(ik)%internal(1) + (H*vltiki)) < bt) then

               else

                 vltiki= 0.d0

                 rrlnik   = (-WWttik*vvlocfreenik + WWntik*vvlocfreetik)/this(ik)%det
                 rrltik   = (-WWnnik*vvlocfreetik + WWtnik*vvlocfreenik)/this(ik)%det

                 if (rrlnik < 0.d0) then
                   rrlnik = 0.d0
                   rrltik = -vvlocfreetik/WWttik
                 endif

                 sstatusik=i_stick

               endif

            endif

         !!!-------------------------------------
         !!!For spherical geometry only WTN and WNT equal to zero
         case(i_VISCO_ELASTIC_REPELL_WET)
            call mu_SC_viscous_solver_(this(ik)%det,this(ik)%forward,this(ik)%backward, & 
                                       fricik, this(ik)%internal(2), &
                                       WWttik,WWnnik, &
                                       vvlocfreetik,vvlocfreenik, &
                                       sstatusik,rrltik,rrlnik)

            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0
            
         !!!-------------------------------------
         case(i_BRITTLE_COATING_CLB)
         
            call get_g0(ibehav,g0)
            call get_forcePERgap(ibehav,forcePERgap)
            call get_snmax(ibehav,snmax)
         
            if( snmax > forcePERgap*(this(ik)%gapTTbegin-g0) ) then
         
              ! G matrix computation
              Tnn = 1.d0 + H*H*forcePERgap*this(ik)%Wnn
              Ttn = H*H*forcePERgap*this(ik)%Wtn
              Tnt = 0.d0
              Ttt = 1.d0
         
              ! compute A = G^-1
              detJ = Ttt*Tnn-Ttn*Tnt
              Ann  = Ttt / detJ
              Ant  =-Tnt / detJ
              Atn  =-Ttn / detJ
              Att  = Tnn / detJ
         
              ! u = G^-1 * ~ufree
              un = vvlocfreenik*Ann+vvlocfreetik*Ant
              ut = vvlocfreenik*Atn+vvlocfreetik*Att
              vvlocfreenik = un
              vvlocfreetik = ut
         
              ! W = G^-1 * W
              Ttt = (Att * WWttik) + (Atn * WWntik)
              Ttn = (Att * WWtnik) + (Atn * WWnnik)
              Tnt = (Ant * WWttik) + (Ann * WWntik)
              Tnn = (Ant * WWtnik) + (Ann * WWnnik)
         
              WWttik = Ttt; WWtnik = Ttn
              WWntik = Tnt; WWnnik = Tnn
         
              det = (WWttik*WWnnik)-(WWtnik*WWntik)
         
              forward  = WWnnik - (fricik*WWntik)
              if (forward .le. 1.D-18) forward = 0.d0
              backward = WWnnik + (fricik*WWntik)
              if (backward .le. 1.D-18) backward=0.d0
         
            else
              det     = this(ik)%det
              forward = this(ik)%forward
              backward = this(ik)%backward
         
            end if
         
            call mu_SC_std_solver_(det,forward,backward, &
                                   fricik, WWttik,WWtnik,WWntik,WWnnik,vvlocfreetik,vvlocfreenik, &
                                   sstatusik,rrltik,rrlnik,err)
            
            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0

            if (err /=0) then
              call LOGMES(' ') 
              write(cout,'(1x,"WWnn= ",D14.7," WWnt= ",D14.7)') WWnnik,WWntik
              call LOGMES(cout)
              write(cout,'(1x,"WWtn= ",D14.7," WWtt= ",D14.7)') WWtnik,WWttik
              call LOGMES(cout)
              call LOGMES(' ') 
              write(cout,'("fric= ",D14.7)') fricik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("vvlocfreen= ",D14.7," vvlocfreet= ",D14.7)') vvlocfreenik,vvlocfreetik
              call LOGMES(cout)
              call LOGMES(' ')               
              write(cout,'("gap= ",D14.7)') this(ik)%gapTTbegin
              call LOGMES(cout)
              call LOGMES(' ')
              call print_info_(ik)
            endif 

            if (err == 1) then
              call LOGMES(' ')
              write(cout,"(1X,'   WWnn(',I5,') - fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik               
              call LOGMES(cout)
              write(cout,"('contact ',I0,' forward impossible')") ik
              call FATERR(IAM,cout)
            endif              
            if (err ==-1) then
              call LOGMES(' ')
              write(cout,"(1X,'   WWnn(',I5,') + fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik
              call LOGMES(cout)
              write(cout,"('contact ',I0,' backward impossible')") ik               
              call FATERR(IAM,cout)
            endif
            if (err == 3) then
              call LOGMES(' ')
              call FATERR(IAM,'wtf')
            endif
           
         !!!-------------------------------------
         case(i_IQS_STICK,i_GAP_SGR_STICK) 

           if (vvlocfreenik .ge. 0.D0) then
             !no contact
             rrlnik=0.D0
             rrltik=0.D0
             sstatusik=i_noctc
             !this(ik)%is_not_null = 0
           else if (vvlocfreenik .lt. 0.D0) then
             !vv = 0 => rrl = - WW^-1 vvlocfree   
             rrlnik=-(WWttik*vvlocfreenik - WWntik*vvlocfreetik)/this(ik)%det
             rrltik=-(WWnnik*vvlocfreetik - WWtnik*vvlocfreenik)/this(ik)%det
             sstatusik=i_stick
             !this(ik)%is_not_null = 1
           endif

         case(i_NARD_ROD)
           
            if (this(ik)%statusBEGIN == i_Cstck) then
               is_cohesive = .true.
            else
               is_cohesive = .false.
            end if         

            if (is_cohesive) then

              call get_nard_coeff(ibehav,this(ik)%internal,Ksn,Kst,Kvn,Kvt)     

              !calcul de la matrice T-1 =A
              Tnn=1.d0+this(ik)%Wnn*H*(Ksn*H+Kvn)
              Tnt=this(ik)%Wnt*H*(Kst*H+Kvt)
              Ttn=this(ik)%Wtn*H*(Ksn*H+Kvn)
              Ttt=1.d0+this(ik)%Wtt*H*(Kst*H+Kvt)
            
              detJ=Tnn*Ttt-Tnt*Ttn
            
              Ann=Ttt/detJ
              Ant=-Tnt/detJ
              Atn=-Ttn/detJ
              Att=Tnn/detJ

              !auxiliary vfree

              un=vvlocfreenik*Ann+vvlocfreetik*Ant
              ut=vvlocfreenik*Atn+vvlocfreetik*Att

              vvlocfreenik=un
              vvlocfreetik=ut
              
              Ttt = (Att * WWttik) + (Atn * WWntik)           
              Ttn = (Att * WWtnik) + (Atn * WWnnik) 
              Tnt = (Ant * WWttik) + (Ann * WWntik)
              Tnn = (Ant * WWtnik) + (Ann * WWnnik)  
               
              WWttik = Ttt; WWtnik = Ttn
              WWntik = Tnt; WWnnik = Tnn
               
              det = (WWttik*WWnnik)-(WWtnik*WWntik)
               
              forward  = 1.D0 - (fricik*WWntik/WWnnik)
              backward = 1.D0 + (fricik*WWntik/WWnnik)
            
            else

              det      = this(ik)%det
              forward  = this(ik)%forward
              backward = this(ik)%backward
            
              ut = 0.d0
              un = 0.d0

            end if
     
            call mu_SC_std_solver_(det,forward,backward, & 
                                  fricik, WWttik,WWtnik,WWntik,WWnnik,vvlocfreetik,vvlocfreenik, &
                                  sstatusik,rrltik,rrlnik,err)

            if (err == 1) then
              write(cout,"('contact ',I0,' forward impossible')") ik
              call FATERR(IAM,cout)
            endif              
            if (err ==-1) then
              write(cout,"('contact ',I0,' backward impossible')") ik               
              call FATERR(IAM,cout)
            endif
            
           

         !!!-------------------------------------
         case default
            call mu_SC_std_solver_(this(ik)%det,this(ik)%forward,this(ik)%backward, & 
                                   fricik, WWttik,WWtnik,WWntik,WWnnik,vvlocfreetik,vvlocfreenik, &
                                   sstatusik,rrltik,rrlnik,err)
            
            !this(ik)%is_not_null = 1
            !if (sstatusik == i_noctc) this(ik)%is_not_null = 0


            if (err/=0) then
              call LOGMES(' ',.TRUE.)                
              write(cout,'(1x,"WWnn= ",D14.7," WWnt= ",D14.7)') WWnnik,WWntik
              call LOGMES(cout,.TRUE.)
              write(cout,'(1x,"WWtn= ",D14.7," WWtt= ",D14.7)') WWtnik,WWttik
              call LOGMES(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)               
              write(cout,'("fric= ",D14.7)') fricik
              call LOGMES(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)               
              write(cout,'("vvlocfreen= ",D14.7," vvlocfreet= ",D14.7)') vvlocfreenik,vvlocfreetik
              call LOGMES(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)               
              write(cout,'("vlocfreen= ",D14.7," vlocfreet= ",D14.7)') vlocfreenik,vlocfreetik
              call LOGMES(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)               
              write(cout,'("vfreen= ",D14.7," vfreet= ",D14.7)') this(ik)%vfreen,this(ik)%vfreet
              call LOGMES(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)               
              write(cout,'("gap= ",D14.7)') this(ik)%gapTTbegin
              call LOGMES(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)
              call print_info_(ik)
            endif 
            
            if (err == 1) then
              call LOGMES(' ',.TRUE.)                
              write(cout,"('WWnn(',I5,') - fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik
              call logmes(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)               
              write(cout,"('contact ',I0,' forward impossible')") ik
              call FATERR(IAM,cout)
            endif              
            if (err ==-1) then
              call LOGMES(' ',.TRUE.)                
              write(cout,"('WWnn(',I5,') + fric(',I5,')*Wnt(',I5,') < 1.D-18')") ik,ik,ik               
              call logmes(cout,.TRUE.)
              call LOGMES(' ',.TRUE.)
              write(cout,"('contact ',I0,' backward impossible')") ik               
              call FATERR(IAM,cout)
            endif
            if (err == 3) then
              call LOGMES(' ',.TRUE.)                
              call FATERR(IAM,'wtf')
            endif   
            
         end select

         !!! Restore genuine **********************************************************

         ! default
         rltiki = rrltik
         rlniki = rrlnik
         ! vltiki, vlniki, this(ik)%gapTT, are so far not to be used; computation is saved.

         ! 
         select case(ilaw)
         case default            
 

         !!!-------------------------------------
         case(i_IQS_CLB_RGR)
            rlniki = rrlnik + this(ik)%corln
            if (this(ik)%corln .gt. 0.D0 .and. sstatusik == i_noctc) sstatusik=i__RGR_

            
         !!!-------------------------------------
         case(i_IQS_WET_DS_CLB,i_GAP_WET_DS_CLB, &
              i_xQS_WET_DS_CLB,i_ELASTIC_REPELL_WET_CLB, &
              i_IQS_SGR_CLB_WEAR,i_RST_WET_CLB)
            if ( this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslfw ) then 
               rlniki=rrlnik+this(ik)%corln
               if (sstatusik == i_noctc) then 
                  sstatusik=i_Wnctc
               else if (sstatusik == i_stick) then 
                  sstatusik=i_Wstck
               else if (sstatusik == i_slibw) then 
                  sstatusik=i_Wslbw
               else if (sstatusik == i_slifw) then
                  sstatusik=i_Wslfw
               end if
            end if
            
         !!!-------------------------------------
         case(i_VISCO_ELASTIC_REPELL_WET)

            if ( this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslfw ) then 
               rlniki=rrlnik+this(ik)%corln
               if (sstatusik == i_noctc) then
                  sstatusik=i_Wnctc
               elseif (sstatusik == i_stick) then
                  sstatusik=i_Wstck
               elseif (sstatusik == i_slibw) then
                  sstatusik=i_Wslbw
               elseif (sstatusik == i_slifw) then
                  sstatusik=i_Wslfw
               end if
            end if

            if (i_what == i_post) then 

               call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )

            endif

         !!!-------------------------------------
         case(i_IQS_MOHR_DS_CLB,i_GAP_MOHR_DS_CLB,i_GAP_CAP_MOHR_DS_CLB)
         
            if (this(ik)%statusBEGIN == i_Mstck) then 
               rlniki = rrlnik + this(ik)%corln
               if (sstatusik == i_stick) sstatusik=i_Mstck
            end if

         !!!-------------------------------------
         case(i_MAC_CZM,i_MSMP_CZM,i_MAL_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER,i_TH_CZM,i_ABP_CZM, &
              i_IQS_MAC_CZM, i_IQS_MAL_CZM, i_IQS_TH_CZM, i_IQS_ABP_CZM, &
              i_EXPO_CZM, i_IQS_EXPO_CZM, &
              i_postGAP_IQS_CZM,i_ELASTIC_REPELL_MAC_CZM,i_TOSI_CZM,i_TOSI_CZM_INCRE)
               
            !fd on calcule une correction de deplacement
            vt = vvlocfreetik + ((WWttik*rltiki) + (WWtnik*rlniki))
            vn = (vvlocfreenik - this(ik)%covfreen) + ((WWntik*rltiki) + (WWnnik*rlniki)) 

            !fd correction due a la rigidite 
            Hradh_t = Hradh_t + (k_t * vt)
            Hradh_n = Hradh_n + (k_n * vn)

            !fd a voir
            ! if ( Hradh_n > 1.D-03 ) then
            !   write(cout,'(A22,D10.3,A13,I5)') 'strange cohesive part ',Hradh_n,' for contact ',ik  
            !   call LOGMES(cout)
            ! endif

            !fd calcul de la reaction de contact
            rltiki = rltiki + Hradh_t
            rlniki = rlniki + Hradh_n + Hp

            !fd correction du statut qui est faux lorsqu'on est en traction
            if (sstatusik == i_noctc .and. rlniki /= 0.d0 ) sstatusik=i_stick

            !fd
            IF (i_what == i_check) THEN 

               CALL updt_CZM(ibehav,.FALSE.,this(ik)%internal,this(ik)%taz,vt,vn)

            endif
            
            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_post) then

              call updt_CZM(ibehav,.false.,this(ik)%internal,this(ik)%taz,vt,vn)
              call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )
            endif

            !fd mise a jour des status            
            if (is_cohesive) then                
               if (sstatusik == i_noctc) then
                  !fd sstatusik=i_Cstck
                  sstatusik=i_Cnctc                  
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slibw) then
                  sstatusik=i_Cslbw
               else if (sstatusik == i_slifw) then
                  sstatusik=i_Cslfw
               end if
            ! else
            !    !fd encore du cohesif !? pour du WET ... pas clair !!
            !    if ( this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslfw ) then 
            !       rlniki=rrlnik+this(ik)%corln
            !       if (sstatusik == i_noctc) then 
            !          sstatusik=i_Wnctc
            !       else if (sstatusik == i_stick) then
            !          sstatusik=i_Wstck
            !       else if (sstatusik == i_slibw) then
            !          sstatusik=i_Wslbw
            !       else if (sstatusik == i_slifw) then
            !          sstatusik=i_Wslfw
            !       end if
            !    end if
            end if

         !!!-------------------------------------
         case(i_EXPO_CZM_P, i_IQS_EXPO_CZM_P)
		 
               
            !fd on calcule une correction de deplacement
            vt = vvlocfreetik + ((WWttik*rltiki) + (WWtnik*rlniki))
            vn = (vvlocfreenik - this(ik)%covfreen) + ((WWntik*rltiki) + (WWnnik*rlniki)) 

            !fd correction due a la rigidite 
            Hradh_t = k_t * ((this(ik)%internal(2)/H) + vvlocfreetik + ((WWttik*rltiki) + (WWtnik*rlniki)) - (this(ik)%internal(8)/H))
            Hradh_n = k_n * (vvlocfreenik + ((WWntik*rltiki) + (WWnnik*rlniki)) - (this(ik)%internal(9)/H))

            !fd a voir
            ! if ( Hradh_n > 1.D-03 ) then
            !   write(cout,'(A22,D10.3,A13,I5)') 'strange cohesive part ',Hradh_n,' for contact ',ik  
            !   call LOGMES(cout)
            ! endif

            !fd calcul de la reaction de contact
            rltiki = rltiki + Hradh_t
            rlniki = rlniki + Hradh_n
			
			! print*, 'rltiki, rlniki :: ', rltiki, rlniki
			! print*, 'rltiki, rlniki :: ', rltiki, rlniki

            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_check) then

              call updt_CZM(ibehav,.false.,this(ik)%internal,this(ik)%taz,vt,vn)

            endif
           
            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_post) then

              call updt_CZM(ibehav,.false.,this(ik)%internal,this(ik)%taz,vt,vn)
              call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )

            endif

            !fd mise a jour des status            
            if (is_cohesive) then                
               if (sstatusik == i_noctc) then
                  !fd sstatusik=i_Cstck
                  sstatusik=i_Cnctc                  
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slibw) then
                  sstatusik=i_Cslbw
               else if (sstatusik == i_slifw) then
                  sstatusik=i_Cslfw
               end if
            ! else
            !    !fd encore du cohesif !? pour du WET ... pas clair !!
            !    if ( this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslfw ) then 
            !       rlniki=rrlnik+this(ik)%corln
            !       if (sstatusik == i_noctc) then 
            !          sstatusik=i_Wnctc
            !       else if (sstatusik == i_stick) then
            !          sstatusik=i_Wstck
            !       else if (sstatusik == i_slibw) then
            !          sstatusik=i_Wslbw
            !       else if (sstatusik == i_slifw) then
            !          sstatusik=i_Wslfw
            !       end if
            !    end if
            end if
!-------------------------------------------------------------------------
         case(i_EXPO_CZM_SPRING,i_IQS_EXPO_CZM_SPRING)

            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_check) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki)
               
              ! if (.TRUE.) then
              !   print*,'updt sans stockage' 
              !   print*,'expoczs allongements'
              !   print*,'dun= ',vn,' dut= ',vt
              !   print*,'un= ',vn+this(ik)%internal(3),' ut= ',vt+this(ik)%internal(2)
              ! endif

              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt,this(ik)%gapTTbegin-this(ik)%internal(7)+H*vn,rltiki,rlniki)
            endif
            
            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_post) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki)
               
              ! if (.TRUE.) then
              !   print*,'updt sans stockage' 
              !   print*,'expoczs allongements'
              !   print*,'dun= ',vn,' dut= ',vt
              !   print*,'un= ',vn+this(ik)%internal(3),' ut= ',vt+this(ik)%internal(2)
              ! endif

              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt,this(ik)%gapTTbegin-this(ik)%internal(7)+H*vn,rltiki,rlniki)
              call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )
            endif

            !fd mise a jour des status            
            if (is_cohesive) then                
               if (sstatusik == i_noctc) then
                  !fd sstatusik=i_Cstck
                  sstatusik=i_Cnctc                  
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slibw) then
                  sstatusik=i_Cslbw
               else if (sstatusik == i_slifw) then
                  sstatusik=i_Cslfw
               end if
            end if
			
!-------------------------------------------------------------------------
         case(i_EXPO_CZM_SPRING_P,i_IQS_EXPO_CZM_SPRING_P)

            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_check) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki)
               
              ! if (.TRUE.) then
              !   print*,'updt sans stockage' 
              !   print*,'expoczs allongements'
              !   print*,'dun= ',vn,' dut= ',vt
              !   print*,'un= ',vn+this(ik)%internal(3),' ut= ',vt+this(ik)%internal(2)
              ! endif

              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring_p(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt,this(ik)%gapTTbegin-this(ik)%internal(7)+H*vn,rltiki,rlniki)
            endif
           
            !fd mise a jour des sauts de deplacement (pas de beta)
            if (i_what == i_post) then

              !fd on calcule vitesse relative = ufree + wab Ib + waa Ia
              vt = vlocfreetik + (this(ik)%Wtt*rltiki) + (this(ik)%Wtn*rlniki)
              vn = vlocfreenik + (this(ik)%Wnt*rltiki) + (this(ik)%Wnn*rlniki)
               
              ! if (.TRUE.) then
              !   print*,'updt sans stockage' 
              !   print*,'expoczs allongements'
              !   print*,'dun= ',vn,' dut= ',vt
              !   print*,'un= ',vn+this(ik)%internal(3),' ut= ',vt+this(ik)%internal(2)
              ! endif

              ! on passe :
              !  - en tangent : correction de deplacement tangent
              !  - en normal : deplacement total
              call updt_CZM_spring_p(ibehav,.false.,this(ik)%internal,this(ik)%taz,H*vt,this(ik)%gapTTbegin-this(ik)%internal(7)+H*vn,rltiki,rlniki)
              call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )
            endif

            !fd mise a jour des status            
            if (is_cohesive) then                
               if (sstatusik == i_noctc) then
                  !fd sstatusik=i_Cstck
                  sstatusik=i_Cnctc                  
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slibw) then
                  sstatusik=i_Cslbw
               else if (sstatusik == i_slifw) then
                  sstatusik=i_Cslfw
               end if
            end if
            
!-------------------------------------------------------------------------
         case(i_IQS_WET_CZM)

            if (is_cohesive) then 
               
              !fd on calcule une correction de deplacement
              
              vt = vvlocfreetik + ((WWttik*rltiki) + (WWtnik*rlniki))
              vn = (vvlocfreenik - this(ik)%covfreen) + ((WWntik*rltiki) + (WWnnik*rlniki)) 
               
              if (i_what == i_post) then 
                  call updt_CZM(ibehav,.false.,this(ik)%internal,this(ik)%taz,vt,vn)
                  call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )
              endif
               
               Hradh_t = Hradh_t + (k_t * vt)
               Hradh_n = Hradh_n + (k_n * vn)
               
               if ( Hradh_n > 1.D-03 ) then
                  write(cout,'(A22,D10.3,A13,I5)') 'strange cohesive part ',Hradh_n,' for contact ',ik  
                  call LOGMES(cout)
               endif
               
               rltiki = rltiki + Hradh_t 
               rlniki = rlniki + Hradh_n 
               
               if (sstatusik == i_noctc) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_stick) then
                  sstatusik=i_Cstck
               else if (sstatusik == i_slibw) then
                  sstatusik=i_Cslbw
               else if (sstatusik == i_slifw) then
                  sstatusik=i_Cslfw
               end if
            else
               if ( this(ik)%statusBEGIN >= i_Wnnow .and. this(ik)%statusBEGIN <= i_Wslfw ) then 
                  rlniki=rrlnik+this(ik)%corln
                  if (sstatusik == i_noctc) then
                     sstatusik=i_Wnctc
                  else if (sstatusik == i_stick) then
                     sstatusik=i_Wstck
                  else if (sstatusik == i_slibw) then
                     sstatusik=i_Wslbw
                  else if (sstatusik == i_slifw) then
                     sstatusik=i_Wslfw
                  end if
               end if
            end if

         !-------------------------------------------------------------------------
         case(i_IQS_BW_CLB)
            if ( rlniki .gt. this(ik)%internal(2) ) then
               !projection sur le cone modifie
               alphaik = this(ik)%internal(3)
               fnik    = this(ik)%internal(2)
               if (rltiki.gt.0.d0)then
                  rltiki  = min(rltiki,alphaik*rlniki-fnik*(alphaik-fricik))
               else if (rltiki.lt.0.d0)then
                  rltiki = - rltiki
                  rltiki  = min(rltiki,alphaik*rlniki-fnik*(alphaik-fricik))
                  rltiki = - rltiki
               end if
            else
               !nothing to do
            end if

            if (i_what == i_post) then 

               call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )

            end if

!-------------------------------------------------------------------------
         CASE(i_CRITICAL_VOIGT_CLB)
            !this(ik)%vepad = pad velocity = WWttik*rrltik+WWtnik*rrlnik+vvlocfreetik
             this(ik)%vepad = WWttik*rrltik+WWtnik*rrlnik+vvlocfreetik

!-------------------------------------------------------------------------
         case(i_BRITTLE_COATING_CLB)
         
           if( snmax > forcePERgap*(this(ik)%gapTTbegin+H*(Wrlniki+vlocfreenik)-g0) ) then
              this(ik)%corln = H*(g0-this(ik)%gapTTbegin-H*(Wrlniki+vlocfreenik))*forcePERgap
           else
              this(ik)%corln = 0.d0
           end if

           rlniki = rrlnik + this(ik)%corln
           rltiki = rrltik


         !-------------------------------------------------------------------------           
         case(i_NARD_ROD)

           if (is_cohesive) then   

             vn = vvlocfreenik + ((WWntik*rrltik) + (WWnnik*rrlnik)) ! pas vn mais g/h !!
             vt = vvlocfreetik + ((WWttik*rrltik) + (WWtnik*rrlnik))

             Hradh_n = 0.D0
             Hradh_t = 0.D0

             Hradh_n = this(ik)%internal(1)*Ksn*H  &
                     + vn*H*(-Kvn-Ksn*H)+this(ik)%gapTTbegin*Kvn

             Hradh_t =-Kvt*vt*H  &
                     - kst*H*(H*vt + this(ik)%internal(2))

             rlniki = rrlnik +  Hradh_n
             rltiki = rrltik +  Hradh_t 

             if (sstatusik == i_noctc) then
               sstatusik=i_Cstck
             else if (sstatusik == i_stick) then
               sstatusik=i_Cstck
             else if (sstatusik == i_slibw) then
               sstatusik=i_Cstck
             else if (sstatusik == i_slifw) then
               sstatusik=i_Cstck
             end if

           else

             rlniki = 0.d0
             rltiki = 0.d0

           end if

           if (i_what == i_post) then

             this(ik)%internal(2)=this(ik)%internal(2)+H*vt
              
             if (this(ik)%CDAN == i_PTPT2) then
               call set_internal(this(ik)%CDAN, this(ik)%icdan, this(ik)%internal)
             !else if (this(ik)%CDAN == i_PLPLx) then               
             !  call put_internal_PLPLx(this(ik)%icdan,this(ik)%internal)
             !else if (this(ik)%CDAN == i_DKDKx) then               
             !  call put_internal_DKDKx(this(ik)%icdan,this(ik)%internal)
             else
               call FATERR(IAM,'NARD_ROD internal put!?')
             endif
           endif
          
         !!!--------------------------------------
         CASE(i_IQS_CLB_g0,i_IQS_PLAS_CLB,i_GAP_SGR_CLB_g0,i_ELASTIC_REPELL_CLB_g0,i_ELASTIC_ROD)
            CALL set_internal(this(ik)%CDAN, this(ik)%icdan, this(ik)%internal)            
            
         end select
         
      end if
      ! end of this(ik)%forecast == i_acton

      !!!
      !!! Updating *********************************************
      !!!

      ! Updating is purposedly omitted while check, according to the definition of violations.
      ! i_check is an apart process and does not interfer with i_iter and i_post process values.       
      if (i_what /= i_check) then

         if (this(ik)%forecast == i_noact) then

            rltiki          = 0.D0
            rlniki          = 0.D0
            this(ik)%rlt    = rltiki
            this(ik)%rln    = rlniki
            this(ik)%status = sstatusik
            
            ! No further updating is necessary since noact candidates for contact are, by definition,
            ! assigned not to interfer with other candidates.

         else if (this(ik)%forecast == i_acton) then
         
            rltiki=RELAX*rltiki+RELAX1*rltik
            rlniki=RELAX*rlniki+RELAX1*rlnik
            
            if (DABS(rltiki) .lt. 1.D-24) rltiki = 0.D0
            if (DABS(rlniki) .lt. 1.D-24) rlniki = 0.D0

            this(ik)%rlt    = rltiki
            this(ik)%rln    = rlniki
            this(ik)%status = sstatusik

            if ( .not. SDLactif ) then
               ! Computing_________________________________________  R - H Rloc(ik) + H Rloc(ik)
               rtik = this(ik)%rlt - rltik  
               rnik = this(ik)%rln - rlnik 
               ! Injecting difference between impulse reactions after and before going through the single
               ! contact solver
               call injj_(ik,rtik,rnik,iIreac)
            end if
            
         end if
      end if
      !!! end case i_what /= 'i_check'
      
      !!! Further computations for case i_what = 'i_post '
      if (i_what == i_post) then

         ! Rebuilding relative velocities, gap, status, with last known reaction values 

         Wrltiki = this(ik)%Wtt*rltiki + this(ik)%Wtn*rlniki
         Wrlniki = this(ik)%Wnt*rltiki + this(ik)%Wnn*rlniki
         vltiki  = Wrltiki + vlocfreetik
         vlniki  = Wrlniki + vlocfreenik
         
         if (DABS(vltiki) .lt. 1.D-24 ) vltiki = 0.D0
         if (DABS(vlniki) .lt. 1.D-24 ) vlniki = 0.D0

         this(ik)%vlt  = vltiki
         this(ik)%vln  = vlniki

         if (.FALSE.) then
           print*,'@post inter ',ik
           print*,'rln= ',this(ik)%rln,' rlt= ',this(ik)%rlt
           print*,'dun= ',H*this(ik)%vln,'dut= ',H*this(ik)%vlt
         endif
         
         gapTT= this(ik)%gapTTbegin + H*vlniki
         if ( DABS(gapTT ) .lt. 1.D-24 ) gapTT = 0.D0
         
         this(ik)%status = sstatusik
		 
		 ! print*, 'this(ik)%rln , this(ik)%rlt :: ' , this(ik)%rln , this(ik)%rlt!! ali boukham

         ! Sending local data      
         call set_loc( this(ik)%CDAN, this(ik)%icdan, this(ik)%status, &
                       this(ik)%vlt, this(ik)%vln, this(ik)%rlt, this(ik)%rln, gapTT )

         !!! <<< update CVC -------------------------------------
         ilaw   = this(ik)%i_law 
         SELECT CASE(ilaw)
           CASE(i_CRITICAL_VOIGT_CLB)
           !kTvT strategy*******
           this(ik)%internal(1)=this(ik)%internal(1)+H*(this(ik)%vlt-this(ik)%vepad)
           call update_internal(ik)
         END SELECT
         !!! >>> update CVC --------------------------------------
      end if
      !!! end if i_what == i_post


      !!! Further computations for case i_what = i_check
      ! Only rltiki, rlniki, (weighting with RELAX is also omitted), rltik, rlnik, sstatusik, 
      ! are used for computing violations.
      if (i_what == i_check) then

         ! Computing discrepancies, violations, mean values
         if (i_checktype == i_QuadN ) then
            ! calcul de la norme uniquement sur la partie normale

            Wrlniki = this(ik)%Wnn*rlniki
            vlniki  = Wrlniki+vlocfreenik

            if (DABS(vlniki) .lt. 1.D-24) vlniki=0.D0
            this(ik)%statuscheck=sstatusik

            rlocn = 0.5D0*(rlnik+rlniki)
            vln   = 0.5D0*(vlnik+vlniki)      
            Dvln  = vlnik-vlniki

            DVDV   = Dvln*Dvln
            DVDVRR = DVDV*(rlocn*rlocn)
            DVoR   = rlocn*Dvln
            modrl  = DVoR            

            SumDVDV   = SumDVDV+DVDV
            MaxDVDV   = DMAX1(MaxDVDV,DVDV)
            SumDVDVRR = SumDVDVRR+DVDVRR
            MaxDVDVRR = DMAX1(MaxDVDVRR,DVDVRR)
            SumDVoR   = SumDVoR+DVoR

            WRR=0.5D0*(this(ik)%Wnn*rlnik*rlnik+Wrlniki*rlniki)
            
            SumWRR = SumWRR+WRR
            SumWRWR= SumWRWR+this(ik)%Wnn*WRR

         else

            ! According to above computations

            ! Wrltik =this(ik)%Wtt*rltik +this(ik)%Wtn*rlnik
            ! Wrlnik =this(ik)%Wnt*rltik +this(ik)%Wnn*rlnik  

            ! vltik =Wrltik +vlocfreetik
            ! vlnik =Wrlnik +vlocfreenik
            
            ! rltik=this(ik)%rlt, rlnik=this(ik)%rln, vltik, vlnik, is the solution to be checked,
            
            ! rltiki, rlniki, vltiki, vlniki, is a non updated solution constructed
            ! after rltik, rlnik, vltik, vlnik,
            ! see the definition of violation in 
            ! Micro Mecanique des Materiaux Granulaires, B. Cambou, M. Jean, Hermes 2001
            
            ! Rebuilding relative velocities, gap, status, with last known reaction values 
            Wrltiki = this(ik)%Wtt*rltiki + this(ik)%Wtn*rlniki
            Wrlniki = this(ik)%Wnt*rltiki + this(ik)%Wnn*rlniki
            vltiki  = Wrltiki + vlocfreetik
            vlniki  = Wrlniki + vlocfreenik
            if (DABS(vltiki) .lt. 1.D-24) vltiki=0.D0
            if (DABS(vlniki) .lt. 1.D-24) vlniki=0.D0
            this(ik)%statuscheck=sstatusik

            rloct  = 0.5D0*(rltik+rltiki) 
            rlocn  = 0.5D0*(rlnik+rlniki)
  
            vlt    = 0.5D0*(vltik+vltiki)
            vln    = 0.5D0*(vlnik+vlniki)      
  
            Dvlt   = vltik-vltiki
            Dvln   = vlnik-vlniki

            DVDV   = Dvlt*Dvlt+Dvln*Dvln

            modrl  = rloct*rloct+rlocn*rlocn

            DVDVRR = DVDV*modrl
            modrl  = sqrt(modrl)

            DVoR   = rloct*Dvlt+ rlocn*Dvln   


            SumDVDV   = SumDVDV + DVDV
            MaxDVDV   = DMAX1(MaxDVDV,DVDV)
            SumDVDVRR = SumDVDVRR + DVDVRR
            MaxDVDVRR = DMAX1(MaxDVDVRR,DVDVRR)

            SumDVoR   = SumDVoR + DVoR

            WRR = ( Wrltik *rltik +Wrlnik *rlnik &
                 +  Wrltiki*rltiki+Wrlniki*rlniki)*0.5


            SumWRR  = SumWRR + WRR

            SumWRWR = SumWRWR + 0.5D0*( Wrltik *Wrltik +Wrlnik *Wrlnik   &
                                      + Wrltiki*Wrltiki+Wrlniki*Wrlniki) 

            !!!mr .... faudrait pas définir un mot clés afin de ne pas avoir à faire ces opérations
            !!!        tout le temps?!
            dynstat = dynstat & 
                    + dabs( ( this(ik)%Wnn*(vltiki-this(ik)%vltBEGIN)-this(ik)%Wtn*(vlniki-this(ik)%vlnBEGIN)) &
                    * (vltiki+this(ik)%vltBEGIN)/this(ik)%det &
                    + (-this(ik)%Wnt*(vltiki-this(ik)%vltBEGIN)+this(ik)%Wtt*(vlniki-this(ik)%vlnBEGIN)) &
                    * (vlniki+this(ik)%vlnBEGIN)/this(ik)%det)
         end if
         ! Arrays for fine violation analysis
         Xvlton(ik)   = DVDV  ! Xvlton(ik)=DVDVRR
         WRRarray(ik) = WRR 

         this(ik)%statuscheck = sstatusik

         !!! Analyzing status
         !!!mj
         !!!mj The analysis is made according to the following definitions.
         !!!mj
         !!!mj nbCDAN is the number of pairs candidate-antagonist selected in the sorting process.
         !!!mj
         !!!mj Nnoact according to the intergranular law, specially when using a forecast contact
         !!!mj        test, it may be decided to discard the pairs where a contact is not forecasted.
         !!!mj        These pairs have the 'noact' status and are equipped with null reactions.
         !!!mj        Nnoact is the number of these pairs.
         !!!mj 
         !!!mj Nvnish is the number of pairs of candidate-antagonist with vanishing (null) reactions, modrl=0.D0, 
         !!!mj        modrl=module(rloc)
         !!!mj 
         !!!mj Ncompr
         !!!mj Ntract Among pairs of candidate-antagonist, one may distinguish those with strictly
         !!!mj        positive normal reaction force, and those with strictly negative reaction force
         !!!mj
         !!!mj so that nb_CDAN=Nvnish+Ncompr+Ntract
         !!!mj This equality might be wrong, since it may occur that some RlocT is different from zero
         !!!mj while RlocN is null. This happens in some cohesive Mohr_Coulomb like laws (not yet implemented).
         !!!mj
         !!!mj Nactif is the number of pairs with non vanishing reactions, i.e.
         !!!mj        Nactif=nb_CDAN-Nvnish
         !!!mj
         !!!mj Nslide
         !!!mj Nstick One may also distinguish those pairs with a sliding status or sticking status with numbers 
         !!!mj
         !!!mj Nhover (like hovering) is the total number of these pairs is one may introduce with status
         !!!mj i_noctc, i_Wnctc, i_Mnctc,... with vanishing or not reactions.
         !!!mj        
         !!!mj 
         !!!mj Nb_RGR is the number of contacts where the Radjai Gap Rescue is active.
         !!!mj        It happens that nb_CDAN=Nhover+Nslide+Nstick+Nb_RGR
         !!!mj        
         !!!mj NOKsta is the number of discrepancies between status exhibited during iterations and those 
         !!!mj        exhibited by after_iter_check.
         
         if (this(ik)%forecast == i_noact) Nnoact=Nnoact+1

         if (rlocn > 0.D0) then
            Ncompr=Ncompr+1
         else if (rlocn < 0.D0) then
            Ntract=Ntract+1
         endif
         if (modrl == 0.D0) then
            Nvnish=Nvnish+1
         end if
         
         if (this(ik)%statuscheck == i_noctc  .or. &
             this(ik)%statuscheck == i_Wnctc  .or. &
             this(ik)%statuscheck == i_Mnctc) then
            Nhover=Nhover+1 
         end if
         
         if (this(ik)%statuscheck == i_stick  .or. &
             this(ik)%statuscheck == i_Wstck  .or. & 
             this(ik)%statuscheck == i_Mstck  .or. &
             this(ik)%statuscheck == i_Cstck       &
              ) then   
            Nstick=Nstick+1
            if(this(ik)%ws == 0)then
               WNstick = WNstick + 1
            else
               SNstick = SNstick + 1
            end if
         endif
         
         if (this(ik)%statuscheck == i_slifw .or. this(ik)%statuscheck == i_slibw   .or. &
             this(ik)%statuscheck == i_Wslfw .or. this(ik)%statuscheck == i_Wslbw   .or. &  
             this(ik)%statuscheck == i_Mslfw .or. this(ik)%statuscheck == i_Mslbw   .or. &   
             this(ik)%statuscheck == i_Cslfw .or. this(ik)%statuscheck == i_Cslbw        &   
              ) then  
            Nslide=Nslide+1
            if(this(ik)%ws == 0)then
               WNslide = WNslide + 1
            else
               SNslide = SNslide + 1
            end if
         end if
         
         if( this(ik)%status /= this(ik)%statuscheck)then
            NOKsta = NOKsta + 1
            if(this(ik)%ws == 0)then
               NOKweak = NOKweak + 1
            else
               NOKstrg = NOKstrg + 1
            end if
         end if
         
         if (this(ik)%statuscheck == i__RGR_) Nb_RGR = Nb_RGR+1
         
         ! Fine violation analysis

         if (modrl .ne. 0.D0) then !mj
            WRRmin=DMIN1(WRRmin,WRRarray(ik))
            WRRmax=DMAX1(WRRmax,WRRarray(ik))

            !fd 12012017 a corriger car QuadWR pas forcement connu ici
            !if (QuadWR == 0.d0) then
               Xvlton(ik)=DSQRT(Xvlton(ik))
            !else  
            !   Xvlton(ik)=DSQRT(Xvlton(ik))/(QuadWR*tol)
            !end if
               
            ! The violation has the form (sqrt(DVDV)/sqrt(WRWR))/tol
         end if
         
         ! Sending violations for violation chart
         call set_violation( this(ik)%CDAN, this(ik)%icdan, Xvlton(ik) )

      end if
      !!! end of if i_what == i_check

   end do
   !!! end of do ikk=1,nb_CDAN

   !$OMP END DO
   !$OMP END PARALLEL   

   !!! Summarize rough violation analysis
   if (i_what == i_check) then
      
      Nactif = nb_CDAN-Nvnish
      
      !raf :  En DDM la suite est recalculée plus tard
      if( DDM_SCHWARTZ .and. nb_procs_COMM_WORLD /= 1 ) return

      ! compute quantities used to check the convergence (QuadDV, MaxmDV, QuadDVR, MaxmDVR and MeanDVoR)
      call compute_convergence_norms_nlgs(Nactif, SumWRWR, SumDVDV, MaxDVDV, &
                                          SumWRR, SumDVDVRR, MaxDVDVRR, SumDVoR, tol, &
                                          QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR)

      ! compute more check quantities
      if (Nactif .ge. 1 .and. SumWRR .gt. 1.D-18 .and. SumWRWR .gt. 1.D-18) then   
         QuadWR   = DSQRT(SumWRWR/real(Nactif,8))
         Dreac    = QuadWR*H  
         
         MeanWRR  = SumWRR/real(Nactif,8)
         rcvltm = - MeanDVoR*tol  
         dynstat  = 0.5D0*dynstat/SumWRR
      else   
         QuadWR   = 0.111D-11 
         Dreac    = 0.111D-11 
         MeanWRR  = 0.111D-11
         rcvltm =   0.000D+00 
         dynstat  = 0.111D-11
      end if
   end if
   
 end subroutine solve_nlgs

 !am: this function computes the quantities used to check the convergence
 !    from given summations (the stored ones or external ones)
 !    N.B. this function is used in DDM
 subroutine compute_convergence_norms_nlgs(Nactif_, SumWRWR_, SumDVDV_, MaxDVDV_, &
                                           SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_, &
                                           QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_)

    implicit none

    ! inputs
    integer :: Nactif_
    real(kind=8), intent(in) :: SumWRWR_, SumDVDV_, MaxDVDV_, SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_

    ! outputs
    real(kind=8), intent(out) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_

    ! locals
    real(kind=8) :: QuadWR_, MeanWRR_

    if (Nactif_ >= 1 .and. SumWRR_ > 1.d-18 .and. SumWRWR_ > 1.d-18) then
       QuadWR_   = DSQRT(SumWRWR_/real(Nactif_,8))
       QuadDV_   = DSQRT(SumDVDV_/real(Nactif_,8))   / (QuadWR_*tol_)
       MaxmDV_   = DSQRT(MaxDVDV_)                  / (QuadWR_*tol_)

       MeanWRR_  = SumWRR_/real(Nactif_,8)
       QuadDVR_  = DSQRT(SumDVDVRR_/real(Nactif_,8)) / (MeanWRR_*tol_)
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
 !raf: this function puts convergence norms into global variables,
 !     when convergence norms are not compute in solve_nlgs fonctions.
 !     N.B. this function is used in DDM
 subroutine put_convergence_norms_nlgs( QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_)
                                           
    real(kind=8), intent(in) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_
    
    QuadDV   = QuadDV_
    MaxmDV   = MaxmDV_
    QuadDVR  = QuadDVR_
    MaxmDVR  = MaxmDVR_ 
    MeanDVoR = MeanDVoR_
    
    ! compute more check quantities
    if (Nactif .ge. 1 .and. SumWRR .gt. 1.D-18 .and. SumWRWR .gt. 1.D-18) then   
       QuadWR   = DSQRT(SumWRWR/real(Nactif,8))
       Dreac    = QuadWR*H  
         
       MeanWRR  = SumWRR/real(Nactif,8)
       rcvltm = - MeanDVoR*tol  
       dynstat  = 0.5D0*dynstat/SumWRR
    else   
       QuadWR   = 0.111D-11 
       Dreac    = 0.111D-11 
       MeanWRR  = 0.111D-11
       rcvltm =   0.000D+00 
       dynstat  = 0.111D-11
    end if

 end subroutine put_convergence_norms_nlgs
 !------------------------------------------------------------------------  
 !------------------------------------------------------------------------  
 subroutine compute_local_free_vlocy(list_INTRF, storage_vlocy)
   implicit none

   integer, dimension(:), intent(in), optional :: list_INTRF
   integer, intent(IN), optional :: storage_vlocy
   integer                       :: storage

   integer :: i,ik

   ! Get Vfree...
   storage = iVfree
   ! ... unless the user choose another location
   if (present(storage_vlocy)) storage = storage_vlocy

   if (.not. present(list_INTRF)) then

      if (nb_CDAN == 0) return

      do ik=1,nb_CDAN
         call prjj_(ik,this(ik)%vfreet,this(ik)%vfreen,storage)
      end do
   
   else

      do i = 1, size(list_INTRF)
         ik=list_INTRF(i)
         call prjj_(ik,this(ik)%vfreet,this(ik)%vfreen,storage)
      end do

   end if

 end subroutine compute_local_free_vlocy

 !------------------------------------------------------------------------
 subroutine prjj_(ik,vtik,vnik,storage)

   implicit none
                            !1234567890
   character(len=10) :: IAM='nlgs::prjj'
   character(len=80) :: cout
   
   integer :: ik
   integer :: storage

   real(kind=8),intent(out) :: vtik,vnik   

   call prjj( this(ik)%CDAN, this(ik)%icdan, vtik, vnik, storage )

 end subroutine prjj_ 

 !------------------------------------------------------------------------
 subroutine injj_(ik,rtik,rnik,storage)
   
   implicit none
   
   !                         1234567890
   character(len=10) :: IAM='nlgs::injj'
   character(len=80) :: cout
   
   integer :: ik
   integer :: storage  
   
   real(kind=8),intent(in) :: rtik,rnik

   call injj( this(ik)%CDAN, this(ik)%icdan, rtik, rnik, storage )

 end subroutine injj_

 !------------------------------------------------------------------------
 subroutine nullify_reac_(ik,storage)

   implicit none

                            !123456789012345678
   character(len=18) :: IAM='nlgs::nullify_reac'
   character(len=80) :: cout

   integer :: ik
   integer :: storage

   call nullify_reac( this(ik)%CDAN, this(ik)%icdan, storage )

 end subroutine nullify_reac_

 !------------------------------------------------------------------------
 subroutine vitrad_(ik,storage)
   
   !
   !computing velocity of adajacent ctct_elements
   !
   
   implicit none
   
   !                         123456789012
   character(len=12) :: IAM='nlgs::vitrad'
   character(len=80) :: cout
   
   integer :: ik
   integer :: storage
   logical :: need_full_V

   !fd to say if all terms of a deformable body velocity are mandatory, default is no
   need_full_V = .false.
   if ( storage == iVaux_e_invM_t_Iaux_ .and. SDLACTIF ) need_full_V = .true.

   call vitrad( this(ik)%CDAN, this(ik)%icdan, storage, need_full_V )

 end subroutine vitrad_

 !------------------------------------------------------------------------
 subroutine nullify_vlocy_(ik,storage)
   
   implicit none
   
   !                         12345678901234567890123
   character(len=23) :: IAM='nlgs_box::nullify_vlocy'
   character(len=80) :: cout
   
   integer :: ik,storage
   
   call nullify_vlocy( this(ik)%CDAN, this(ik)%icdan, storage )
   
 end subroutine nullify_vlocy_

 !------------------------------------------------------------------------
 subroutine print_info_(ik)
   
   implicit none
   
                            !1234567890123456
   character(len=16) :: IAM='nlgs::print_info'
   character(len=80) :: cout
   
   integer :: ik
  
   write( cout, "(1X,'Interaction :',I5,1X,I5)" ) this(ik)%CDAN, this(ik)%icdan
   call LOGMES(cout,.TRUE.)

   call print_info( this(ik)%CDAN, this(ik)%icdan )
   
 end subroutine print_info_


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
 
 !!!------------------------------------------------------------------------
 !!! NOT TO BE MODIFIED ...
 !!!------------------------------------------------------------------------  
 subroutine mu_SC_std_solver_(det,forward,backward, &
                              fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen,  &
                              sstatus,rrlt,rrln,err)

   !!! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   !!! sstatus,rrlt,rrln are output data.
   !!! if err /= 0 it was not possible to compute a solution   
   !!! det,forward,backward are auxiliaries which have been precomputed in prep_nlgs
   !!! in order to save floating point operations.


   implicit none
   integer(kind=4)  :: sstatus
   real(kind=8)     :: fric,WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   real(kind=8)     :: vvlt,vvln,rrlt,rrln
   real(kind=8)     :: WItt,WItn,WInt,WInn,DFT,DFN,Cforward,Cbackward,forward,backward,FFN
   integer(kind=4)  :: err

   err = 0 
   
   if (vvlocfreen .ge. 0.D0) then
      !no contact
      rrln=0.D0
      rrlt=0.D0
      sstatus=i_noctc

   else

     ! WW inverse (*det) 
     WItt= WWnn
     WItn=-WWtn
     WInt=-WWnt
     WInn= WWtt

     ! impulse prediction (*det)
     DFT = -(WItt*vvlocfreet + WItn*vvlocfreen)
     DFN = -(WInt*vvlocfreet + WInn*vvlocfreen)
     
     FFN =-vvlocfreen
     
     Cforward  = DFT+fric*DFN
     Cbackward = DFT-fric*DFN
      
     if ( Cforward .lt. 0.D0) then
       !sliding forward
       if (forward == 0.d0) then
         err = 1 
       else
         rrln=FFN/forward
         rrlt=-fric*rrln
         sstatus=i_slifw
       endif  
     else if ( Cbackward .gt. 0.D0) then
       !sliding backward      
       if (backward == 0.d0) then
         err =-1
       else
         rrln=FFN/backward
         rrlt=fric*rrln
         sstatus=i_slibw
       endif      
     else if (Cforward .ge. 0.D0 .and. Cbackward .le. 0.D0) then
       !sticking      
       rrln=DFN/det
       rrlt=DFT/det
       sstatus=i_stick
     else
      err = 3
      ! rrln=0.D0
      ! rrlt=0.D0
      ! sstatus=i_vnish                    
     end if
   endif
 end subroutine mu_SC_std_solver_

 !------------------------------------------------------------------------
 subroutine mu_SC_wear_solver_(internal,vvlt,vvln,vlnBEGIN,gapTTbegin, &
                               fric,kcdwear,kanwear,WWtt,WWtn,WWnt,WWnn, &
                               vvlocfreet,vvlocfreen,  &
                               sstatus,rrlt,rrln)
   implicit none

   integer :: i,j
   real(kind=8), dimension(2,2):: B,D
   real(kind=8), dimension(2)  :: ulib,delta,rloc,lambda
   real(kind=8), dimension(4,4):: MM,Grad
   real(kind=8), dimension(4):: y
   real(kind=8), dimension(4)  :: e,gy
   real(kind=8)                :: p,diff,sign1,sign2,vwear,xnbegin,kw,kcdwear,kanwear
   real(kind=8)                :: fric,WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   real(kind=8)                :: vvlt,vvln,rrlt,rrln,vlnBEGIN,gapTTbegin,norm
   integer(kind=4)             :: sstatus
   integer, dimension(4)       :: ipiv
   character(len=1)            :: trans
   
   real(kind=8),dimension(3)   :: internal

   integer :: info


   p = 1.D0/WWnn
   e = 0.D0
   kw = kcdwear+kanwear
   vwear = internal(1)
   e(1) = vvlocfreet
   e(2) = vvlocfreen

   delta(1) = vvlt
   delta(2) = vvln-vwear
   rloc(1)  = rrlt
   rloc(2)  = rrln

   D = 0.D0
   D(1,1) = 1.D0
   D(2,2) = 1.D0
   B(1,1) = -H*WWtt 
   B(1,2) = -H*WWtn
   B(2,1) = -H*WWnt
   B(2,2) = -H*WWnn
  

!!!fd to fj xnbegin et gapTT doivent etre la meme chose !

   xnbegin = gapTTbegin+H*(1-THETA)*vlnBEGIN

!!! newton ou le nombre d'iterations max = 300

   do i = 1,300
      MM   = 0.D0
      Grad = 0.D0
      gy=0.d0
      MM(1:2,1:2)=D;MM(1:2,3:4)=B;MM(3,3)=-1.D0;MM(4,4)=-1.D0
      lambda(2)=rloc(2)-p*(xnbegin+H*THETA*(delta(2)+kw*rloc(2)*dabs(delta(1))))

! NO CONTACT
    if (lambda(2)<=0.d0) then   
      y=e
      call dgesv(4,1,MM,4,ipiv,y,4,info)
      sstatus=i_noctc
! CONTACT ...
    else                        
      lambda(1)=rloc(1)-p*delta(1)
!     ... STICKING
      if ( dabs(lambda(1))<=fric*lambda(2)) then   
        Grad(4,4)=1.d0-p*kw*dabs(delta(1)); Grad(3,1)=-p
        Grad(3,3)=1.d0;Grad(4,2)=-H*THETA*p
        if (delta(1)>0.D0) then
          Grad(4,1)=-p*H*THETA*kw*rloc(2)
        else if (delta(1)<0.D0) then
          Grad(4,1)=p*H*THETA*kw*rloc(2)
        end if
        MM=MM+Grad
        gy(1:2)=0.d0;gy(3)=lambda(1);gy(4)=lambda(2)
        y=matmul(Grad,y)-gy+e
        call dgesv(4,1,MM,4,ipiv,y,4,info)
        sstatus=i_stick
!     ... SLIDING ...
      else                                 
!             ... BACKWARD
        if (lambda(1)>fric*lambda(2)) then
          Grad(4,4)=1.d0-p*H*THETA*kw*dabs(delta(1));Grad(4,2)=-p*H*THETA
          Grad(3,4)=fric*(1.d0-p*H*THETA*kw*dabs(delta(1)))
          Grad(3,2)=-fric*p*H*THETA
          if (delta(1)>0.D0) then
            Grad(4,1)=-p*H*THETA*kw*rloc(2) ;Grad(3,1)=-p*H*THETA*fric*kw*rloc(2)
          else if (delta(1)<0.D0) then
            Grad(4,1)=p*H*THETA*kw*rloc(2) ;Grad(3,1)=p*H*THETA*fric*kw*rloc(2)
          end if
          MM=MM+Grad
          gy(1:2)=0.d0;gy(4)=lambda(2);gy(3)=fric*lambda(2)
          y=matmul(Grad,y)-gy+e
          call dgesv(4,1,MM,4,ipiv,y,4,info)
          sstatus=i_slibw
!             ... FORWARD
        else if (lambda(1)<-(fric*lambda(2))) then
          Grad(4,4)=1.d0-p*H*THETA*kw*dabs(delta(1));Grad(4,2)=-p*H*THETA
          Grad(3,4)=-fric*(1.d0-p*H*THETA*kw*dabs(delta(1)))
          Grad(3,2)=fric*p*H*THETA
          if (delta(1)>0.D0) then
            Grad(4,1)=-p*H*THETA*kw*rloc(2) ;Grad(3,1)=p*H*THETA*fric*kw*rloc(2)
          else if (delta(1)<0.D0) then
            Grad(4,1)=p*H*THETA*kw*rloc(2) ;Grad(3,1)=-p*H*THETA*fric*kw*rloc(2)
          end if
          MM=MM+Grad
          gy(1:2)=0.d0;gy(4)=lambda(2);gy(3)=-fric*lambda(2)
          y=matmul(Grad,y)-gy+e
          call dgesv(4,1,MM,4,ipiv,y,4,info)
          sstatus=i_slifw
        end if
      end if
    end if

    norm=sqrt(rloc(1)**2+rloc(2)**2+delta(1)**2+delta(2)**2)
    if (norm/=0.D0) then
      diff=dabs(rloc(1)-y(3))+dabs(rloc(2)-y(4))+dabs(delta(1)-y(1))+ &
           dabs(delta(2)-y(2))/norm
    else
      diff=dabs(rloc(1)-y(3))+dabs(rloc(2)-y(4))+dabs(delta(1)-y(1))+dabs(delta(2)-y(2)) 
    end if
    if (diff<1.D-4) exit
    delta=y(1:2)
    rloc=y(3:4)
  end do
  delta=y(1:2)
  rloc=y(3:4)
  vwear=kw*rloc(2)*dabs(delta(1))
  vvln=delta(2)+vwear
  vvlt=delta(1)
  rrln=H*rloc(2)
  rrlt=H*rloc(1)

  if (i>=300) then
    write(*,*)'Nombre iterations de Newton =',i
    write(*,*)
    write(*,*)
    write(*,*)'Reactions',rloc(1), rloc(2)
    write(*,*)
    write(*,*)
    write(*,*)'vitesse locale elastique',delta(1),delta(2)
    write(*,*)
    write(*,*)
    write(*,*)'vitesse d usure',vwear
    write(*,*)
    write(*,*)
    write(*,*)'vitesse locale total',vvlt,vvln
    call faterr('nlgs::mu_SC_wear_solver','read log')
  end if

  internal(1)= vwear
  internal(2)= vwear*kcdwear/kw
  internal(3)=-vwear*kanwear/kw
  
 end subroutine mu_SC_wear_solver_

!------------------------------------------------------------------------
 subroutine coupled_dof_solver_(det,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen,  &
                                sstatus,rrlt,rrln)

   ! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   ! sstatus,vvlt,vvln,rrlt,rrln, are output data.
   ! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
   ! They may be computed within the subroutine from input data.
   ! Here they have been precomputed in prep_nlgs in order to save floating point operations.

   implicit none
   real(kind=8)     :: WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   real(kind=8)     :: DFT,DFN
   real(kind=8)     :: vvlt,vvln,rrlt,rrln
   integer(kind=4)  :: sstatus

   DFT= WWnn*vvlocfreet-WWtn*vvlocfreen
   DFN=-WWnt*vvlocfreet+WWtt*vvlocfreen

   rrlt=-DFT/det
   rrln=-DFN/det

   vvln=0.d0
   vvlt=0.d0

   sstatus=i_stick
 
 end subroutine coupled_dof_solver_

 !------------------------------------------------------------------------
 subroutine plastic_coupled_dof_solver_(det,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen,  &
                                        sstatus,rrlt,rrln,seuil)

   ! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   ! sstatus,vvlt,vvln,rrlt,rrln, are output data.
   ! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
   ! They may be computed within the subroutine from input data.
   ! Here they have been precomputed in prep_nlgs in order to save floating point operations.

   implicit none
   real(kind=8)     :: WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   real(kind=8)     :: DFT,DFN
   real(kind=8)     :: vvlt,vvln,rrlt,rrln,seuil
   integer(kind=4)  :: sstatus

   DFT= WWnn*vvlocfreet-WWtn*vvlocfreen
   DFN=-WWnt*vvlocfreet+WWtt*vvlocfreen

   rrlt=-DFT/det
   rrln=-DFN/det

   vvln=0.d0
   vvlt=0.d0

   if (rrln < -seuil) then

      rrln = -seuil
      rrlt =-(vvlocfreet + (WWtn*rrln))/WWtt 

      vvln = vvlocfreen + (WWnn*rrln) + (WWnt*rrlt)

   endif
   sstatus=i_stick
 
 end subroutine plastic_coupled_dof_solver_


 !!!------------------------------------------------------------------------
 !!! NOT TO BE MODIFIED ...
 !!!------------------------------------------------------------------------  
 subroutine mu_SC_viscous_solver_(det,forward,backward, &
                                  fric,internal,WWtt,WWnn,vvlocfreet,vvlocfreen,  &
                                  sstatus,rrlt,rrln)

   !!! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   !!! sstatus,vvlt,vvln,rrlt,rrln, are output data.
   !!! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
   !!! They may be computed within the subroutine from input data.
   !!! Here they have been precomputed in prep_nlgs in order to save floating point operations.


   implicit none
   integer(kind=4)  :: sstatus
   real(kind=8)     :: fric,WWtt,WWnn,det,vvlocfreet,vvlocfreen
   real(kind=8)     :: vvlt,vvln,rrlt,rrln,NUT,internal
   real(kind=8)     :: DFT,DFN,Cforward,Cbackward,forward,backward,FFN


   if (vvlocfreen .ge. 0.D0) then
     !no contact
     rrln=0.D0
     rrlt=0.D0
     sstatus=i_noctc
   else
     DFT = WWnn*vvlocfreet
     DFN = WWtt*vvlocfreen
     FFN =-vvlocfreen
     Cforward  = DFT+fric*DFN
     Cbackward = DFT-fric*DFN
   
     NUT = vvlocfreet*internal/(1.D0+WWtt*internal)
     
     if (Cforward .gt. 0.D0) then
       !sliding forward
       rrln=FFN/forward
       rrlt=-fric*rrln - NUT
       sstatus=i_slifw                    
     else if (Cbackward .lt. 0.D0) then
       !sliding backward      
       rrln=FFN/backward
       rrlt=fric*rrln - NUT
       sstatus=i_slibw                    
     else if (Cforward .le. 0.D0 .and. Cbackward .ge. 0.D0) then
       !sticking      
       rrln=-vvlocfreen/WWnn
       rrlt=-vvlocfreet/WWtt
       sstatus=i_stick
     else
       ! never possible !!  
       rrln=0.D0
       rrlt=0.D0
       sstatus=i_vnish                    
     endif
   end if
   
 end subroutine mu_SC_viscous_solver_

 !------------------------------------------------------------------------  
 subroutine Nullify_EntityList_nlgs

   implicit none

   call Free_EntityList

 end subroutine Nullify_EntityList_nlgs

 !------------------------------------------------------------------------  
 subroutine get_nlgs2D_loop(compteur,err1,err2,err3,contact)

   implicit none
   integer      :: compteur,contact
   real(kind=8) :: err1,err2,err3

   compteur = nlgs_loop
   contact  = nb_CDAN
   err1     = MeanDVoR
   err2     = QuadDV
   err3     = QuadDVR

 end subroutine get_nlgs2D_loop

 !------------------------------------------------------------------------  
 subroutine get_nlgs2D_network_change(nctc,nweak,nstrong)

   implicit none

   integer :: nctc,nweak,nstrong

   nctc    = NOKsta
   nweak   = NOKweak
   nstrong = NOKstrg

 end subroutine get_nlgs2D_network_change

 !------------------------------------------------------------------------  
 subroutine get_nlgs2D_contact_status(noctc,Wslide,Sslide,Wstick,Sstick)

  implicit none

  integer :: noctc,Wslide,Sslide,Sstick,Wstick
  
  noctc  = Nhover !fd obsolete Nnoctc
  Wslide = WNslide
  Sslide = SNslide
  Sstick = SNstick
  Wstick = WNstick

 end subroutine get_nlgs2D_contact_status

 !------------------------------------------------------------------------  
 subroutine get_after_iter_check(ddynstat,nnb_CDAN,NNnoact,NNvnish,NNhover,NNcompr,&
                                 NNtract,NNslide,NNstick,NNOKsta,NNb_RGR)

   implicit none
   
   integer   :: nnb_CDAN,NNnoact,NNvnish,NNhover,NNcompr,NNtract,NNslide,NNstick,NNOKsta,NNb_RGR
   real(kind=8) :: ddynstat

   ddynstat=dynstat
   nnb_CDAN=nb_CDAN
   NNnoact=Nnoact
   NNvnish=Nvnish
   NNcompr=Ncompr
   NNtract=Ntract
   NNslide=Nslide
   NNstick=Nstick
   NNOKsta=NOKsta
   NNb_RGR=Nb_RGR
  
 end subroutine get_after_iter_check  

 !------------------------------------------------------------------------  
 subroutine update_internal(ik)

   implicit none

                            !1234567890123456789012
   character(len=22) :: IAM='nlgs::update_internal'
   character(len=80) :: cout

   integer :: ik,ibehav,ilaw

   ibehav = this(ik)%lawnb
   ilaw   = this(ik)%i_law 

   select case(ilaw)

   case(i_MAC_CZM,i_MSMP_CZM,i_MAL_CZM,i_MP_CZM,i_MP3_CZM,i_MP3_CZM_THER,i_TH_CZM,i_ABP_CZM, &
        i_IQS_MAC_CZM, i_IQS_MAL_CZM, i_IQS_TH_CZM, i_IQS_ABP_CZM, &
        i_postGAP_IQS_CZM,i_IQS_WET_CZM,i_ELASTIC_REPELL_MAC_CZM, &
        i_EXPO_CZM,i_IQS_EXPO_CZM,i_EXPO_CZM_P,i_IQS_EXPO_CZM_P,i_TOSI_CZM,i_TOSI_CZM_INCRE)

      !fd mise a jour des sauts de deplacements et de beta 
      call updt_CZM(ibehav,.true.,this(ik)%internal,this(ik)%taz,this(ik)%vlt,this(ik)%vln)
         
      ! fd est un pd
      ! if (ilaw == i_postGAP_IQS_MAC_CZM) this(ik)%internal(6) = this(ik)%internal(6) * this(ik)%internal(4)
      if (ilaw == i_postGAP_IQS_CZM) this(ik)%internal(6) =  (-0.002)* (1. - this(ik)%internal(4))


   case(i_EXPO_CZM_spring,i_IQS_EXPO_CZM_spring)

      ! print*,'updt avec stockage' 
      
      !fd mise a jour des sauts de deplacements et de beta 
      call updt_CZM_spring(ibehav,.true.,this(ik)%internal,this(ik)%taz,H*this(ik)%vlt,this(ik)%gapTTbegin-this(ik)%internal(7)+H*this(ik)%vln,this(ik)%rlt,this(ik)%rln)
	  
   case(i_EXPO_CZM_spring_P,i_IQS_EXPO_CZM_spring_p)

      ! print*,'updt avec stockage' 
      
      !fd mise a jour des sauts de deplacements et de beta 
      call updt_CZM_spring_p(ibehav,.true.,this(ik)%internal,this(ik)%taz,H*this(ik)%vlt,this(ik)%gapTTbegin-this(ik)%internal(7)+H*this(ik)%vln,this(ik)%rlt,this(ik)%rln)
         
   case(i_IQS_SGR_CLB_WEAR)

      if ( this(ik)%internal(2) == 0 ) then
         if( this(ik)%rln -  this(ik)%internal(1) .le. 0.D0 ) then
            this(ik)%internal(2) = 1
         end if
      end if

   case(i_IQS_CLB_nosldt, &
        i_GAP_SGR_CLB_nosldt)

      this(ik)%internal(1) = this(ik)%internal(1) + H*this(ik)%vlt

   case default
      
   end select

   ! Sending local data      
   call set_internal( this(ik)%CDAN, this(ik)%icdan, this(ik)%internal )

   if (.FALSE.) then
     print*,'@update_internal inter',ik
     print*,this(ik)%internal(2:4)
     print*,this(ik)%taz(2)
   endif  
   
 end subroutine update_internal

 !!!---------------------------------------------------------------
 subroutine scale_rloc_nlgs

    implicit none    

    integer :: ik

    if (nb_CDAN == 0) return

    Scale=DMIN1(Scale,1.1D0)
    Scale=DMAX1(Scale,0.9D0)      

    do ik=1,nb_CDAN
       this(ik)%rlt=this(ik)%rlt*Scale
       this(ik)%rln=this(ik)%rln*Scale
    end do

    ! Rnod = [H] Rloc
    do ik=1,nb_CDAN 
       call nullify_reac_(ik,iIreac)
    end do
    do ik=1,nb_CDAN
       call injj_(ik,this(ik)%rlt,this(ik)%rln,iIreac)
    end do

 end subroutine scale_rloc_nlgs

 !!!---------------------------------------------------------------
 subroutine RnodHRloc_nlgs(list_INTRF, storage_reac)

    implicit none

    ! optional inputs, used in DDM
    integer, dimension(:), intent(in), optional :: list_INTRF
    integer, intent(IN), optional :: storage_reac

    ! locals
    integer :: i,ik
    !am: storage specifies where to store the reaction torque.
    integer :: storage

    ! the reaction torque will be stored in Reac...
    storage = iIreac

    ! ... unless the user choose another location
    if (present(storage_reac)) storage = storage_reac

    if (.not. present(list_INTRF)) then

       if (nb_CDAN == 0) return
   
       do ik=1,nb_CDAN  
          call nullify_reac_(ik, storage)
       end do
       do ik=1,nb_CDAN
          call injj_(ik, this(ik)%rlt, this(ik)%rln, storage)
       end do

    else

       do i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          call nullify_reac_(ik, storage)
       end do
       do i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          call injj_(ik, this(ik)%rlt, this(ik)%rln, storage)
       end do

    end if

 end subroutine RnodHRloc_nlgs

 !!!---------------------------------------------------------------
 !vv: Pour la DDM enrichie
 !    Voir Pierre Alart, Damien Iceta, David Dureisseix, Comput. Methods Appl. Engrg. 2012
 !!!---------------------------------------------------------------
 subroutine VnodHVloc_nlgs(list_INTRF, storage_vlocy)

    implicit none

    ! optional inputs, used in DDM
    integer, dimension(:), intent(in), optional :: list_INTRF
    integer, intent(IN), optional :: storage_vlocy

    ! locals
    integer :: i,ik
    !am: storage specifies where to store the reaction torque.
    integer :: storage

    ! the computed velocity will be stored in Vaux...
    storage = iVaux_

    ! ... unless the user choose another location
    if (present(storage_vlocy)) storage = storage_vlocy

    if (.not. present(list_INTRF)) then

       if (nb_CDAN == 0) return
   
       do ik=1,nb_CDAN  
          call nullify_vlocy_(ik, storage)
       end do
       do ik=1,nb_CDAN
          call injj_(ik, this(ik)%vlt, this(ik)%vln, storage)
       end do

    else

       do i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          call nullify_vlocy_(ik, storage)
       end do
       do i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          call injj_(ik, this(ik)%vlt, this(ik)%vln, storage)
       end do

    end if

 end subroutine VnodHVloc_nlgs

 !!!---------------------------------------------------------------
 subroutine set_nlgs_parameter(normtype,tolerence,relaxation)

    character(len=5)  :: normtype
    real(kind=8)      :: relaxation,tolerence

    tol   = tolerence
    RELAX = relaxation
    RELAX1=1.D0-RELAX

    select case(normtype)
    case('QuaN ')
       i_checktype = i_QuadN
    case('Quad ')
       i_checktype = i_Quad
    case('Maxm ')
       i_checktype = i_Maxm
    case('QM/16')
       i_checktype = i_QMs16
    case DEFAULT
       call LOGMES(normtype)
       call FATERR('nlgs::set_nlgs_parameter','unknown norm type')
    end select

 end subroutine set_nlgs_parameter

 !!!---------------------------------------------------------------
 subroutine prep_check_nlgs(iconv)
    
    implicit none
    integer :: ik,iconv
    
    iconv = 0
    conv_contact = .true.
    if (nb_CDAN == 0) return
    iconv = 1
    conv_contact = .false.
    
    call RnodHRloc_nlgs

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
    
    Nvnish = 0         ! number of candidates with vanishing reactions
    Nnoctc = 0         ! number of no contacts, obsolete!
    Nhover = 0         ! number of hovering contacts (separated contacts active or not)
    
    Nslide = 0         ! number of sliding contacts
    Nstick = 0         ! number of sticking contacts
    Ncompr = 0         ! number of compressing contacts
    Ntract = 0         ! number of tensile contacts
    NOKsta = 0         ! number of questionable status
    Nb_RGR = 0         ! number of contacts where the "Radjai Gap Rescue" is active
    
    ! mj & fd -> comments ?
    WNslide = 0
    WNstick = 0
    SNstick = 0
    SNslide = 0
    NOKweak = 0
    NOKstrg = 0

    ! nbCDAN-Nnoact-Nnoctc = Nactif = Ncomp+Ntract = Nslide+Nstick+Nb_RGR

 end subroutine prep_check_nlgs

 !!!-------------------------------------------------------------------------------------
 subroutine comp_check_nlgs(iconv)

    implicit none

    ! locals
    logical :: converged, t, n

    ! outputs
    integer, intent(OUT) :: iconv

    iconv = 1
   
    !am: check convergence using stored quantities 
    call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, converged)

    conv_contact = converged

    if (converged) iconv = 0

    if (i_checktype == i_Maxm .and. converged) then
       QuadDV  = MaxmDV
       QuadDVR = MaxmDVR
    end if

    t = any(isnan(this(:)%rlt))
    n = any(isnan(this(:)%rln))
    if ( t .or. n ) then
      iconv = -1
    end if

    Scale = 1.D0+rcvltm

 end subroutine comp_check_nlgs

 !!!-------------------------------------------------------------------------------------
 !am: this function check convergence using given quantities (the stored ones or some external ones)
 !    N.B. this function is used in DDM
 subroutine check_convergence_nlgs(QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_, converged)

     implicit none

     ! inputs
     real(kind=8), intent(in) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_

     ! outputs
     logical, intent(out) :: converged

     converged = .false.

     select case(i_checktype)
        case(i_Quad, i_QuadN)
           if ( DABS(MeanDVoR_) .lt. 1.D0 .and. &
                DABS(QuadDV_)   .lt. 1.D0 .and. &
                DABS(QuadDVR_)  .lt. 1.D0) then
              converged = .true.
           end if
        case(i_Maxm)
           if ( DABS(MeanDVoR_) .lt. 1.D0 .and. &
                DABS(MaxmDV_)   .lt. 1.D0 .and. &
                DABS(MaxmDVR_)  .lt. 1.D0) then
              converged = .true.
           end if
        case(i_QMs16)
           if ( DABS(MeanDVoR_) .lt. 1.D0 .and. &
                DABS(QuadDV_)   .lt. 1.D0 .and. &
                DABS(QuadDVR_)  .lt. 1.D0 .and. &
                DABS(MaxmDV_)   .lt. 16.666D0 .and. &
                DABS(MaxmDVR_)  .lt. 16.666D0) then
              converged = .true.
           end if
     end select

 end subroutine check_convergence_nlgs

 !!!------------------------------------------------------------------
 subroutine display_check_nlgs
    
    implicit none
    character(len=103)         :: cout 

    if (nb_CDAN == 0) return
       
    call LOGMES(' ')
    select case(i_checktype)
    case(i_Quad,i_QuadN)
       write(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Quad ','     Maxm'
       call LOGMES(cout)
       write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
       call LOGMES(cout)
       write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
       call LOGMES(cout)
    case(i_Maxm)
       write(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Maxm ','     Maxm'
       call LOGMES(cout)
       write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
       call LOGMES(cout)
       write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
       call LOGMES(cout)
    case(i_QMs16)
       write(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  QM/16','1/16 Maxm'
       call LOGMES(cout)
       write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV *0.06D0 
       call LOGMES(cout)
       write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR*0.06D0 
       call LOGMES(cout)
    end select
    
    write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','MeanDVoR/SumWRR  =',MeanDVoR,'Free run length  =',Dreac 
    call LOGMES(cout)
    write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','dynamic/static   =',dynstat
    call LOGMES(cout)
    write(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Nvnish =',Nvnish,'  nbCDAN =',nb_CDAN, 'Nhover =',Nhover
    call LOGMES(cout)
    write(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ncompr =',Ncompr,'  Nnoact =', Nnoact, 'Nslide =',Nslide
    call LOGMES(cout)
    write(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ntract =',Ntract,'  NOKsta =',NOKsta , 'Nstick =',Nstick 
    call LOGMES(cout)
    write(cout,'(1X,A3,23X,A10,I10)')                      ' @ ',                   '  Nb_RGR =',Nb_RGR 
    call LOGMES(cout)
    call LOGMES('  ')

 end subroutine display_check_nlgs

 !------------------------------------------------------------------------  
 subroutine get_somme_rn(ssomme_rn)


   implicit none

   integer :: ik
   real(kind=8) :: ssomme_rn,somme_rn

   somme_rn=0.D0
   do ik=1,nb_CDAN
     somme_rn=somme_rn+this(ik)%rln
   end do
   ssomme_rn=somme_rn/H
  
 end subroutine get_somme_rn  

 !!!---------------------------------------------------------------------
 subroutine display_rlocn_sum_nlgs

    implicit none

    integer :: ik
    
    somme_rn=0.D0
    do ik=1,nb_CDAN
       somme_rn = somme_rn+this(ik)%rln
    end do
    write(6,'(A11,D14.7)') 'RlocN SUM: ', somme_rn/H

  end subroutine display_rlocn_sum_nlgs

 !!!---------------------------------------------------------------------
 subroutine update_tact_behav_nlgs

    implicit none

    integer :: ik
       
    if (nb_CDAN == 0) return
    
    do ik=1,nb_CDAN
       call update_internal(ik)
    end do

 end subroutine update_tact_behav_nlgs

 !!!---------------------------------------------------------------------
 subroutine write_norm_check_nlgs(istat)
    
    implicit none

    integer :: istat
    integer,save :: norm_fich=0
    logical,save :: norm_check=.false.
    
    select case(istat)
    case(1)
       norm_check = .true.
       norm_fich  = get_io_unit() 
       open(UNIT=norm_fich,FILE=trim(location('NORM_NLGS.DAT')),STATUS='REPLACE')
    case(2)
       if (norm_check) then
          select case(i_checktype)
          case(i_Quad,i_QuadN)
             write(norm_fich,'(3(2X,D14.7),3(1X,I8))') abs(meanDVoR),QuadDVR,QuadDV,NOKsta,NOKweak,NOKstrg
          case(i_Maxm)
             write(norm_fich,'(3(2X,D14.7),3(1X,I8))') abs(meanDVoR),MaxmDVR,MaxmDV,NOKsta,NOKweak,NOKstrg
          end select
       end if
    case(3)
       if (norm_check) close(norm_fich)
    end select

 end subroutine write_norm_check_nlgs

 !!!---------------------------------------------------------------
 subroutine reverse_nlgs

    implicit none    

    integer :: ik

    if (nb_CDAN == 0) return

    do ik=1,nb_CDAN
       ialeatr(ik)=ialeat(ik)
    end do
    do ik=1,nb_CDAN
       ialeat(ik)=nb_CDAN-ialeatr(ik)+1
    end do

 end subroutine reverse_nlgs

 !!!---------------------------------------------------------------
 subroutine bimodal_list_nlgs

    implicit none
    integer :: ik

    if (nb_CDAN == 0) return

    do ik=1,nb_CDAN
       ialeat(ik)=iwksg(ik)
    end do

 end subroutine bimodal_list_nlgs

 !!!------------------------------------------------------------------------
 subroutine scramble_nlgs

   ! This subroutine scrambles the ordering of candidates for contact
   
   implicit none

   integer       :: ik
   integer       :: IALEATik,IAL1,IAL2
   real(kind=8)  :: RA

   do ik=1,nb_CDAN/2
      
      call random_number(RA)
      IAL1 = IDINT(RA*real(nb_CDAN,8))+1
      IAL1 = MIN0(IAL1,nb_CDAN)
      IAL1 = MAX0(1,IAL1)
      
      call random_number(RA)
      IAL2 = IDINT(RA*real(nb_CDAN,8))+1
      IAL2 = MIN0(IAL2,nb_CDAN)
      IAL2 = MAX0(1,IAL2)
      
      IALEATik     = IALEAT(IAL1)
      IALEAT(IAL1) = IALEAT(IAL2)
      IALEAT(IAL2) = IALEATik
      
   end do
   
 end subroutine scramble_nlgs

 !!!mj---------------------------------------------------------------------- 
 subroutine quick_scramble_nlgs

   ! This subroutine scrambles the ordering of candidates for contact

   implicit none

   integer       :: ik,IALEATik

   do ik=1,nb_CDAN
      
      IALEATik=IALEAT(ik)
      IALEAT(ik)=IALEAT(randomlist(ik))
      IALEAT(randomlist(ik))=IALEATik
      
   end do
   
 end subroutine quick_scramble_nlgs

 !!!mr----------------------------------------------------------------------------
 subroutine update_fric_nlgs
   implicit none
   !                         12345678901234567
   character(len=17) :: IAM='nlgs::update_fric'
   character(len=80) :: cout
   integer           :: ik,ibehav
   real(kind=8)      :: fric

   do ik=1,nb_CDAN
      if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
           ( this(ik)%CDAN == i_DKJCx ) .or. &
           ( this(ik)%CDAN == i_DKKDx ) .or. &
           ( this(ik)%CDAN == i_PLPLx ) .or. &
           ( this(ik)%CDAN == i_PLJCx )) then
         call update_fric( this(ik)%CDAN, this(ik)%icdan, fric )
      else 
         write(cout,'(I5,A31)') this(ik)%CDAN,' is not implemented'
         call FATERR(IAM,cout)
      end if

      this(ik)%fric    = fric

   end do
   
 end subroutine update_fric_nlgs

 !!!mr&vhn------------------------------------------------------------------------
 !!! update_cohe_nlgs:
 !!! allow the actuaisation of local cohesion according to particle properties.
 !!! It could be a function of temperature, surface energy, conductivity, ...
 subroutine update_cohe_nlgs
   implicit none
                            !12345678901234567
   character(len=17) :: IAM='nlgs::update_cohe'
   character(len=80) :: cout
   integer      :: ik,ibehav
   real(kind=8) :: cohe,normalcoh,tangalcoh,Wethk

   do ik=1,nb_CDAN
      
      if ( ( this(ik)%CDAN == i_DKDKx ) .or. &
           ( this(ik)%CDAN == i_DKJCx ) .or. &
           ( this(ik)%CDAN == i_DKKDx ) ) then
         call update_cohe( this(ik)%CDAN, this(ik)%icdan, cohe )
      else 
         write(cout,'(I5,A31)') this(ik)%CDAN,' is not implemented'
         call FATERR(IAM,cout)
      end if

      ibehav = this(ik)%lawnb

      select case(this(ik)%i_law)
      case(i_IQS_WET_DS_CLB)
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         normalcoh = cohe
         if (this(ik)%gapTTbegin .le. Wethk) then
            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln    = -normalcoh*H
         end if
      case(i_xQS_WET_DS_CLB)
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         normalcoh = cohe        
         if ( this(ik)%statusBEGIN >= i_Wnnow .and. &
              this(ik)%statusBEGIN <= i_Wslfw .and. &
              this(ik)%gapTTbegin .le. Wethk        ) then
            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln    = -normalcoh*H
         end if
      case(i_IQS_MOHR_DS_CLB)
         if (this(ik)%statusBEGIN == i_Mstck)then
            call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
            normalcoh = cohe
            this(ik)%covfreet = -normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen = -normalcoh*this(ik)%Wnn*H + max(0.D0,this(ik)%gapTTbegin/H)
            this(ik)%corln    = -normalcoh*H
         end if
      case(i_ELASTIC_REPELL_WET_CLB)
         call get_coh(ibehav,normalcoh,tangalcoh,Wethk)
         normalcoh = cohe
         this(ik)%covfreen = this(ik)%gapTTbegin/H
         this(ik)%covfreet = 0.D0
         if (this(ik)%gapTTbegin .le. Wethk) then
            this(ik)%covfreet= this(ik)%covfreet-normalcoh*this(ik)%Wtn*H
            this(ik)%covfreen= this(ik)%covfreen-normalcoh*this(ik)%Wnn*H
            this(ik)%corln   =-normalcoh*H
         end if
      case DEFAULT
         
      end select

   end do
 end subroutine update_cohe_nlgs

 !!!------------------------------------------------------------------------
 !am: this function allows to get all, or a part of, summations used to compute
 !    the quantities required to check the convergence
 !    N.B. this function is used in DDM 
 subroutine get_error(SumDVDVRR_, Nactif_, MeanWRR_, &
                      tol_, SumDVDV_, QuadWR_, SumDVoR_, &
                      SumWRR_, SumWRWR_, MaxDVDV_, MaxDVDVRR_) 
    
   implicit none

   !am: all outpout variables are optional in order to allow the user to define which variables he want to collect
   !    N.B. some new variables were added but the order of the first variable did not change, 
   !         so that callings in Iceta DDM code should still be working!

   integer, intent(out), optional :: Nactif_
   real(kind=8), intent(out), optional :: SumDVDVRR_, &
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
 
 end subroutine get_error

 !!!------------------------------------------------------------------------
 !vv: Get du nombre de W_alpha_beta avec alpha/=beta, non nuls (a-priori)
 function get_nb_adjac_nlgs_2D()
   implicit none
   integer(kind=4) :: get_nb_adjac_nlgs_2D

   get_nb_adjac_nlgs_2D=nb_adjac
 end function get_nb_adjac_nlgs_2D

 !!!------------------------------------------------------------------------ 
 function get_all_this()
   implicit none
   real(kind=8), dimension(:,:), pointer :: get_all_this
   !
   integer(kind=4) :: i_cdan

   get_all_this => null()

   if( nb_cdan <= 0 ) return

   allocate( get_all_this(11,nb_cdan) )

   do i_cdan = 1, nb_cdan

      call get_local_frame( this(i_cdan)%CDAN, this(i_cdan)%icdan, get_all_this( 1:6, i_cdan ) )

      get_all_this(7  ,i_cdan) = this(i_cdan)%rlt
      get_all_this(8  ,i_cdan) = this(i_cdan)%rln
      get_all_this(9  ,i_cdan) = this(i_cdan)%vlt
      get_all_this(10 ,i_cdan) = this(i_cdan)%vln
      get_all_this(11 ,i_cdan) = this(i_cdan)%gapTTbegin
     
   end do

 end function get_all_this
 
 !--------------------------------------------------------------------------
 subroutine set_temporary_variable_nlgs(icdan,id,taz)
   implicit none
   integer :: icdan,id
   real(kind=8) :: taz

   this(icdan)%taz(id) = taz
   
 end subroutine set_temporary_variable_nlgs

 !--------------------------------------------------------------------------
 function get_temporary_variable_nlgs(icdan,id)
   implicit none
   integer :: icdan,id
   real(kind=8) :: get_temporary_variable_nlgs

   get_temporary_variable_nlgs = this(icdan)%taz(id)
   
 end function get_temporary_variable_nlgs
 
 !--------------------------------------------------------------------------
 subroutine use_jacobi_solver( jacobi )
    implicit none
    logical :: jacobi
    
    JACOBI_SOLVER = jacobi
 end subroutine 

 !-------------------------------------------------------------------------- 
 subroutine use_regul(v1,v2)
    implicit none
    real(kind=8) :: v1,v2
    
    regul=.TRUE.
    krn=v1
    krt=v2
    
 end subroutine use_regul

 !-------------------------------------------------------------------------- 
 subroutine assume_is_initialized(is_init)
   implicit none
   integer, intent(in) :: is_init
 
   if( is_init > 0 ) then
     is_initialized = .true.
   else
     is_initialized = .false.
   end if

 end subroutine
! Modif 2 : fonctions relatives à la Modif 1

function Hydrostatic(vec) result(res)
   implicit none

   real(kind=8), dimension(9), intent(in) :: vec
   real(kind=8) :: res

    !In the case of Matlib formalism (shape [a1,a2,a3,a4] for a general 2D symmetric tensor)
!    res = (1.d0/3.d0)*(vec(1)+vec(3)+vec(4))

    !In the case of a general tensor  [a1,a2,a3,a4,a5,a6,a7,a8,a9]
  res = (1.d0/3.d0)*(vec(1)+vec(5)+vec(9))

 end function Hydrostatic

function Mises_Stress(vec) result(res)

   implicit none

   real(kind=8), dimension(9), intent(in) :: vec
   real(kind=8) :: res

   !if Matlib formalism [a1,a2,a3,a4]
!    res = sqrt(vec(1)**2+3.d0*vec(2)**2+vec(3)**2-vec(3)*vec(4)+vec(4)**2-vec(1)*(vec(3)+vec(4)))

   !In the case of a general tensor  [a1,a2,a3,a4,a5,a6,a7,a8,a9]

  !res = (1.d0/2.d0)*sqrt(abs(3*((vec(2)+vec(4))**2+(vec(3)+vec(7))**2+(vec(6)+vec(8))**2)+&
!4*(vec(1)**2+vec(5)**2-vec(5)*vec(9)+vec(9)**2-vec(1)*(vec(5)+vec(9)))))

  res = sqrt((3.d0/2.d0)*(((2.d0/3.d0)*vec(1)-(1.d0/3.d0)*(vec(5)+vec(9)))**2+((2.d0/3.d0)*vec(5)&
-(1.d0/3.d0)*(vec(1)+vec(9)))**2+((2.d0/3.d0)*vec(9)-(1.d0/3.d0)*(vec(5)+vec(1)))**2+vec(2)**2+&
vec(3)**2+vec(4)**2+vec(6)**2+vec(7)**2+vec(8)**2))

 end function Mises_Stress



! Fin de modif 2

end module nlgs 


