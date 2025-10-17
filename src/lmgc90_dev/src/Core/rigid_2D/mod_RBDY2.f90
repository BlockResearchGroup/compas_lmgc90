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
module RBDY2                                       

  !!****h* LMGC90/CORE.RBDY2
  !! NAME
  !!  module RBDY2
  !! AUTHOR
  !!  F.Dubois & M.Jean
  !! PURPOSE
  !!  RBDY2 module defines generically a type T_RBDY2 for 2 dimensional rigid bodies, with 3 degrees of freedom.
  !!  The first two degrees of freedom are related to the motion of the center of gravity, and the third to the 
  !!  rotation of the body. All data are stored in a single list bdyty.
  !!  
  !!  The standard bulk properties are defined in a bulk element called PLAIN.
  !!  These properties are the mass and the inertia moment. They are computed from:
  !!  the specific mass defined in the file BULK_BEHAV.DAT from RIGID law called by its nickname behav;
  !!  the average radius and the inertia moment, data from the blmty PLAIN, see below.
  !!
  !!  The standard nodty is NO3xx, with 3 degrees of freedom as introduced above.
  !! 
  !!  The feature used as a customized property distinguishing a rigid body from another body is the boundary.
  !!  These boundaries,for examples tacty DISKx, JONCx, POLYG, are managed in moduli such as mod_DISKx, mod_JONCx, 
  !!  mod_POLYG. Boundary datas, radius of DISKx, half axes ax1, ax2, of JONCx, etc., are stored in a T_BDARY type in  
  !!  some array data, the meaning of it being described in some class a_BDARY_DISKx, a_BDARY_JONCx, a_BDARY_POLYG. 
  !!
  !!  It happens that the abstract class RBDY2 may be extented to some other applications, for instance DISPx, 
  !!  a pneumatic disk, or xPSID a reverse pneumatic disk. A pneumatic disk, is a 2 dimensional rigid body 
  !!  with boundary such as DISKx but equipped with a variable boundary radius. The deformation of the boundary
  !!  radius is then a fourth degree of freedom. The nodty used for this body is NO4Px, a "pneumatic" node.
  !!  More to read in comp_mass subroutine comments. The adjective "reverse", means that the free region is 
  !!  the inner disk.
  !! USES
  !!  LMGC90/CORE.utilities
  !!  LMGC90/CORE.overall
  !!  LMGC90/CORE.BULK_BEHAVIOUR
  !!  LMGC90/CORE.a_DOF
  !!  LMGC90/CORE.a_BDARY_DISKx
  !!  LMGC90/CORE.a_BDARY_xKSID
  !!  LMGC90/CORE.a_BDARY_JONCx
  !!  LMGC90/CORE.a_BDARY_PT2Dx
  !!  LMGC90/CORE.a_BDARY_POLYG
  !!****

!mj a !fd: Au sujet de DISPx, xPSID, NO4Px:
!mj Il y a deux choix possibles pour gerer un corps 2D avec parametres internes, par exemple DISPx. 
!mj Le premier choix consiste a cabler la dimension des tableaux get_X, get_V, get_Xbegin... a 3,
!mj (3 ddl) comme pour un rigide ordinaire, les ddl correspondant aux coordonnees du centre
!mj de gravite et a la rotation. Le calcul du rayon est renvoye dans mod_DISPx.
!mj Ce calcul faisant intervenir la quatrieme composante, celle ci est obtenue
!mj par un get_quatriemecomposante residant dans mod_DISPx;

!mj Il reste un hic cependant. Supposons qu'on melange plusieurs corps possedant un quatrieme degre de liberte.
!mj Il y a des equivoques a lever concernant
!mj le calcul de la matrice de masse (ou ce qui en tient lieu). Selon la signification du parametrage, la masse
!mj (le poids) affecte au quatrieme ddl doit etre calcule differemment. La solution consiste a indicer le type NO4xx
!mj qui est banalise. Par exemple on peut creer un noeud de type NO4Px, pour signifier que le quatrieme ddl
!mj est la deformation du rayon, ou un noeud NO4qq pour un ddl supplementaire selon son imagination. Dans ce cas,
!mj le calcul de la masse, qui incombe au noeud NO4Px, et non pas au contacteur DISPx, trouve bien sa place dans 
!mj le module RBDY2.

!mj Ceci conduit au deuxieme choix, qui consiste a dimensionner les tableaux get_X, get_V, get_Xbegin avec 
!mj la taille de X, V, etc.
!mj J'ai fait ce dernier choix, qui, a mon avis, simplifie enormement la vie (penser a read_bodies, read_dof_ini, 
!mj vlocy_driven, etc.). Enfin ce choix laisse la structure RBDY2 invariante quel que soit le nodty porte par le corps.
!mj Il suffit de s'accommoder du fait que la structure RBDY2 est plus riche que celle d'un
!mj rigide ordinaire. Sinon, c'etait pas la peine de s'epuiser a fabriquer cette structure (censee servir aussi
!mj aux corps mailles). Il y avait plus simple a faire.
!mj La seule "personnalisation" reside dans le calcul de la masse, voir comp_mass.

!mj Il ne faut pas alors oublier d'ajouter le type NO4Px dans mod_a_DOF.f90. Ca ne pose pas de probleme, sauf que
!mj la fonction get_nodty_for_nbdof ne peut rendre a la fois NO4xx, NO4Pxx. Il y a certainement une reflexion a 
!mj mener pour differencier les types de noeuds ayant le meme nombre de degres de liberte. En attendant
!mj je fais rendre a cette fonction la valeur NO4xx. Ca n'a pas l'air de gener le disque pneumatique. 

  use utilities
  use overall
  use BULK_BEHAVIOUR
  use a_matrix
  use a_DOF
  use algebra

  use a_BDARY_DISKx
  use a_BDARY_xKSID
  use a_BDARY_JONCx
  use a_BDARY_PT2Dx
  use a_BDARY_POLYG

  use paranoid_checks

  use RBDY2_type, only : T_RBDY2

  implicit none

  private

  !------------------------------------------------------------------------ 

  type( T_RBDY2 ), dimension( : ), allocatable, target :: bdyty

  integer(kind=4) :: nb_RBDY2=0               ! number of RBDY2 in the collection
  !fd 
  integer(kind=4) :: nb_existing_entities = 0 ! when abonning new kind of body to entity list
  ! you need to know how many entities already exist  

  !fd du mr j imagine !?
  !mr yep! Permet de faire du visible dans tout les sens.
  integer      :: nb_falling_RBDY2=0,first_RBDY2=0  ! visible particles (for point source subroutine)
  !fd 
  logical      :: BOUNDS=.false.
  real(kind=8) :: limit_inf  = -1.D+24, limit_sup   =  1.D+24 
  real(kind=8) :: limit_left = -1.D+24, limit_right =  1.D+24 

  real(kind=8) :: sp_radius,sp_shift_x,sp_shift_y   ! source point radius i.e minimal distance between

  ! the source point and the last visible particle  
  ! to let a new invisible particle to become a visible one  
  !
  ! parametres permettant de stopper un calcul si la vitesse est stabilisee.
  !
  real(kind=8)      :: eqs_tol
  integer           :: eqs_ichecktype
  integer,parameter :: iQvlcy = 1 , iMvlcy = 2

  ! parametres de la construction brique par brique d'un mur (a la romaine)
  !
  real(kind=8) :: CWx0,CWxf,CWxi,CWy0,CWyf,CWyi
  integer      :: CWnx,CWny
  logical      :: first_construct=.true.

  ! parameter for periodic condition
  
  logical      :: PERIODIC=.false.
  real(KIND=8) :: periode                           ! interval lenght to apply horizontal periodic condition
                                                    ! The initial point is put to zero

  real(KIND=8)     :: CW_XMIN = 0.D0, CW_XMAX = 0.D0, CW_CV__ = 0.D0
  character(len=5) :: CW_BEHAV

  ! to skip invisible bodies in write_bodies 
  ! and write_out_dof once called
  ! the routine set_skip_invisible
  logical :: skip_invisible = .false.

  !
  !mr T_FREE_BOUNDARY TYPE
  !mr use to determine the particle which compose the free surface of a sample
  !
  type T_FREE_BOUNDARY

     integer(kind=4) :: ID_RBDY2,ID_TACTY
     real(KIND=8)    :: YMAX
     logical         :: ACTIVE

  end type T_FREE_BOUNDARY

  type(T_FREE_BOUNDARY),dimension(:),allocatable  :: FREE_SURFACE

  integer(kind=4) :: FB_NX
  real(KIND=8)    :: FB_XMIN,FB_XMAX,FB_DX
  logical         :: FREE_BOUNDARY = .false.

  !mr
  integer(kind=4) :: nb_WSsect = 1!< use to define the discretization of surface
  !
  ! --------------------------
  ! subroutines set to private
  ! --------------------------

  private &
       comp_mass_one_body

  ! -------------------------
  ! subroutines set to public
  ! -------------------------

  public &
       increment_RBDY2, &
       is_dof_driven_RBDY2, &
       set_vlocy_drvdof_RBDY2, &
       comp_dof_RBDY2, &
       comp_V_RBDY2, &
       comp_X_RBDY2, &
       comp_dof_one_RBDY2, &
       update_dof_RBDY2, &
       comp_free_vlocy_RBDY2, &
       comp_free_vlocy_one_RBDY2,&
       comp_Fext_RBDY2, &
       comp_Fint_RBDY2, &
       check_equilibrium_state_RBDY2, &
       ghost2invisible_RBDY2, &
       check_source_point_RBDY2, &
       out_of_bounds_RBDY2, &
       fatal_damping_RBDY2, &
       partial_damping_RBDY2, &
       read_in_bodies_RBDY2, &
       update_existing_entities_RBDY2, &
       read_in_dof_RBDY2, &
       read_in_body_snapshot_sample_RBDY2, &
       read_in_driven_dof_RBDY2, &
       read_behaviours_RBDY2, &
       write_out_bodies_RBDY2, &
       write_out_cleared_bodies_RBDY2, &
       write_xxx_dof_RBDY2, &
       write_xxx_Rnod_RBDY2, &
       write_out_driven_dof_RBDY2, &
       comp_mass_RBDY2, &
       set_periodic_data_RBDY2, &
       resize_RBDY2, &
       nullify_X_dof_RBDY2, &
       nullify_V_dof_RBDY2, &
       init_source_point_RBDY2, &
       set_init_boundary_RBDY2, &
       set_data_equilibrium_RBDY2, &
       add_dof2bodies_RBDY2, &
       get_write_Rnod_RBDY2, &
       get_write_DOF_RBDY2, &
       init_mp_behaviours_RBDY2, &
       update_WSvsT_RBDY2, &
       update_WSvsTime_RBDY2, &
       put_invmass_RBDY2,put_precon_W_RBDY2, &
       put_vector_RBDY2,get_vector_RBDY2, &
       get_ptr_vector_RBDY2, &
       get_T_RBDY2, &
       compute_window_forces, &
       compute_window_velocity, &
       increment_window_velocity, &
       set_init_cnstrt_window, &
       biaxial_loading,&
       Oedemetric_test,&
       Biaxial_def_walls, &
       shear_def_walls, &
       set_invisible, & 
       set_visible, &
       set_dilatation_increment, &
       update_dilatation, &
       set_vcooref_RBDY2, &
       get_drv_vlocy_RBDY2, &
       comp_drv_vlocy_RBDY2, &
       comp_coor_4all_RBDY2, &
       !vv: routine pour version enrichie
       comp_MVfree_one_RBDY2, &
       is_vlocy_drvdof_RBDY2, &
       nullify_reac_of_vlocy_driven_dof_RBDY2, &
       comp_vlocy_4all_RBDY2
  public &
       get_nb_RBDY2, &
       add_reac, nullify_reac, &
       comp_vlocy, nullify_vlocy, get_vlocy, &
       get_V, get_Vbegin, get_Vaux, get_Vfree, &
       get_coor, get_cooref, get_coorTT, put_cooref, get_Xbegin, get_X, &
       get_data, get_idata, get_bdyID, get_nb_tacty, get_tacID, get_color, indent_color, &
       get_behav, indent_behav, &
       get_nb_outline, get_reac,get_mass,get_inv_mass,get_gyr_radius,get_avr_radius, &
       get_area, get_area_tacty,get_shiftTT,get_visible,get_entity_RBDY2,print_info_RBDY2, &
       get_r2m, put_r2m, get_Fext, get_Fint, &
       get_Vth,get_Xth, get_periode, &
       get_elec_cond,get_elec_condini,put_elec_cond, &
       get_electric_potentiel,put_electric_potentiel,&
       get_electric_current,put_electric_current,&
       get_therm_cond,get_therm_condini,put_therm_cond,get_thermal_value,put_thermal_value, put_therm_sheat, put_therm_diffu, & !jr
       get_ani_therm_cond, get_therm_diffu_alpha_rtheta, get_therm_diffu_alpha_z, & !jr
       get_therm_pcond, get_therm_scond, get_therm_xcond, get_therm_ycond, get_therm_sheat, & !jr
       get_WS,get_thermal_coefficient,get_bulk_behav_number_RBDY2,get_avr_radius_tacty, &
       get_thermal_ID,put_thermal_ID, &
       check_partial_equilibrium_state_RBDY2, & ! <- am: debut des fonctions supplementaires
       compute_partial_equilibrium_state_RBDY2, & 
       is_periodic_RBDY2, &
       get_xperiode_RBDY2, &
       get_betai, &
       put_coor, &
       put_coor_begin, &
       get_coor_begin, &
       free_boundary_computation, &
       IS_IN_THE_FREE_BOUNDARY, &
       init_free_boundary_RBDY2, &
       get_stress_RBDY2, &
       add_stress,&
       check_periodic, & ! <- am: on rend visible la fonction check_periodic
       get_ptr_mass, &   ! <- rm: fonction pour binding avec siconos
       get_ptr_fint, &
       get_ptr_fext, &
       get_ptr_coor, &
       get_ptr_vbeg, &
       get_density, & ! <- rm : fonctions pour binding avec peligriff
       set_mass, &
       set_nb_RBDY2, & ! <- rm: pour simulation
       set_bulk_of_RBDY2, &
       set_skip_invisible_RBDY2, & ! vv: 
       comp_mass_one_body_RBDY2, &
       comp_Fext_one_body_RBDY2, &
       comp_Bulk_one_body_RBDY2, &
       get_idof_RBDY2, &
       get_ccfield_RBDY2, &
       add_mass_to_matrix_RBDY2, &
       copy_bodies_RBDY2, &
       set_visibility, &
       set_visibility_4all_RBDY2, &
       get_bulk_behav_ID_RBDY2, &
       set_surface_sectors, &
       get_surface_sectors, &
       get_average_WS, &
       get_average_WS_active, &
       get_max_WS, & !vhn
       initialize_status_sectors, &
       update_status_sector, &
       modify_body, &
       initialize_stress, &
       get_WStime, get_WSstatus, &
       add_betai, &
       clean_memory_RBDY2, &
       initialize_WS_sectors, &
       switch_vlocy_driven_dof, &
       set_color_RBDY2

  public get_bdyty_RBDY2

contains

!------------------------------------------------------------------------

  subroutine get_bdyty_RBDY2( arg_bdyty )

    implicit none

    type( T_RBDY2 ), dimension( : ), pointer :: arg_bdyty

    arg_bdyty => bdyty

  end subroutine get_bdyty_RBDY2

!!!------------------------------------------------------------------------
!!! In/Out Subroutine interface
!!!------------------------------------------------------------------------
  subroutine read_in_bodies_RBDY2(factor)

    implicit none

    ! variable d'entree optionelle
    integer, intent(in), optional :: factor ! on surdimenssionne le
       ! tabelau des corps en multipliant le nombre de corps lus par
       ! ce facteur

    G_nfich = get_io_unit()
    open(unit=G_nfich,file=trim(location(in_bodies(:)) ))
    call read_bodies(factor)
    close(G_nfich)
 
  end subroutine read_in_bodies_RBDY2
!!!------------------------------------------------------------------------
  !> \brief Read a DOF file to initialize database
  subroutine read_in_dof_RBDY2(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_dof(:))))
    else
      open(unit=G_nfich,file=trim(location(last_dof(:))))
    end if

    call read_in_dof
    close(G_nfich)
    
  end subroutine read_in_dof_RBDY2
!!!------------------------------------------------------------------------
  !> brief
  subroutine read_in_body_snapshot_sample_RBDY2
    implicit none

    G_nfich = get_io_unit()
    open(unit=G_nfich,file=trim(location(post_body_sample(:))))
    call read_in_body_snapshot_sample
    close(G_nfich)

  end subroutine read_in_body_snapshot_sample_RBDY2
!!!------------------------------------------------------------------------
  subroutine read_in_driven_dof_RBDY2

    implicit none
    
    G_nfich = get_io_unit()
    open(unit=G_nfich,file=trim(location(in_driven_dof(:))))
    call read_driven_dof
    close(G_nfich)
   
  end subroutine read_in_driven_dof_RBDY2
!!!------------------------------------------------------------------------
  subroutine write_out_bodies_RBDY2

    implicit none

    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_bodies(:))))
    call write_bodies(nfich)
    close(nfich)

  end subroutine write_out_bodies_RBDY2
!!!------------------------------------------------------------------------
  subroutine write_out_driven_dof_RBDY2

    implicit none
    
    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_driven_dof(:))))
    call write_driven_dof(nfich)
    close(nfich)
    
  end subroutine write_out_driven_dof_RBDY2
!!!------------------------------------------------------------------------
  subroutine write_out_cleared_bodies_RBDY2

    implicit none
 
    integer :: nfich

    nfich = get_io_unit()

    open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_bodies(:))))
    call write_cleared_bodies(nfich)
    close(nfich)

  end subroutine write_out_cleared_bodies_RBDY2
!!!------------------------------------------------------------------------
  subroutine write_xxx_dof_RBDY2(which,ifrom,ito)

    implicit none

    integer :: which,ifrom,ito
    integer :: nfich,lc 

    nfich = get_io_unit()

    select case(which)
    case(1)
!       lc = LEN_TRIM(out_dof)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',& 
            file=trim(location(out_dof(:)))) 
       call write_out_dof(nfich,ifrom,ito)
       close(nfich)
    case(2)
!       lc = LEN_TRIM(last_dof)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND', &
            file=trim(location(last_dof(:)))) 
       call write_out_dof(nfich,ifrom,ito)
       close(nfich)
    case(6)
       call write_out_dof(6,ifrom,ito)
    end select

  end subroutine write_xxx_dof_RBDY2
!!!------------------------------------------------------------------------
  subroutine write_xxx_Rnod_RBDY2(which,ifrom,ito)

    implicit none

    integer :: which,ifrom,ito
    integer :: nfich,lc 

    nfich = get_io_unit()
    
    select case(which)
    case(1)
       lc = len_trim(out_Rnod)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_Rnod(1:lc)))) 
       call write_out_Rnod(nfich,ifrom,ito)
       close(nfich)
    case(2)
       lc = len_trim(last_Rnod)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_Rnod(1:lc)))) 
       call write_out_Rnod(nfich,ifrom,ito)
       close(nfich)
    case(6)
       call write_out_Rnod(6,ifrom,ito)
    end select

  end subroutine write_xxx_Rnod_RBDY2
!!!------------------------------------------------------------------------
  subroutine read_bodies(factor)

    implicit none

    ! variable d'entree optionelle
    integer, intent(in), optional :: factor ! on surdimenssionne le
       ! tabelau des corps en multipliant le nombre de corps lus par
       ! ce facteur

    ! variables locales
    integer            :: ibdyty,iblmty,inodty,itacty,iccdof,idof,nbdof
    integer            :: itest,nfich
    integer            :: errare
    character(len=22)  :: IAM='mod_RBDY2::read_bodies'
    character(len=120) :: cout
    integer            :: nb_polyg_vertex
    
    real(kind=8) :: area,mT,iT,mi,ii,d2
    logical      :: comp_blmty,comp_nodty

    real(kind=8),dimension(2) :: OG

    integer :: size_bdyty ! taille du tableau des corps

    ! verification de la coherence des donnees :

    ! si on adonne un facteur
    if (present(factor)) then
       ! si le facteur est plus petit que 1
       if (factor < 1) then
          ! on affiche un message d'erreur
          call logmes('Error '//IAM// ": factor must be greater than one!")
       end if
    end if

    ! first reading: sizing array of bodies bdyty  
    
    ibdyty=0
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                  ! fishing for the keyword 'bdyty'
       if( .not. read_G_clin()) exit
       itest = itest_bdyty(G_clin)                      
       if (itest == ifound) ibdyty = ibdyty+1
       cycle
    end do
    rewind(G_nfich)

    write(cout,'(1X,I7,1X,A5)') ibdyty,'found'

    call LOGMES(cout)

    nb_RBDY2=ibdyty

    if (nb_RBDY2==0) return

    ! si le tableau des corps doit etre surdimensionne
    if (present(factor)) then
       ! la taille du tableau des corps est le nombre de corps lus
       ! multiplie par le facteur
       size_bdyty = factor*nb_RBDY2
    ! sinon,
    else
       ! la taille du tableau des corps est directement le nombre de corps lus
       size_bdyty = nb_RBDY2
    end if

    allocate(bdyty(size_bdyty),stat=errare)
    if (errare /= 0) THEN
       call FATERR(IAM,'error allocating bdyty')
    end if


    ! second reading: 
    ! sizing list of bulk elements
    ! sizing list of nodes 
    ! sizing list of contactors

    ibdyty=0
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                 ! fishing for the keyword 'bdyty'
       if( .not. read_G_clin()) exit
       itest=itest_bdyty(G_clin)                      
       if (itest /= ifound) cycle
       ibdyty=ibdyty+1
       read(G_clin(2:6),'(A5)') bdyty(ibdyty)%bdyID


       iblmty=0
       inodty=0
       itacty=0

       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'blmty') cycle               ! fishing for the keyword 'blmty'
          do
             if( .not. read_G_clin()) exit
             itest=itest_blmty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                      
             if (itest == ifound) iblmty=iblmty+1
             cycle
          end do
          exit
       end do
       backspace(G_nfich)

       if (iblmty /= 0) then
          allocate(bdyty(ibdyty)%blmty(iblmty),stat=errare)
          if (errare /= 0) then
             call FATERR(IAM,'error allocating bdyty%blmty')
          end if
       else
          nullify(bdyty(ibdyty)%blmty)
          write(cout,'(A27,I7)') 'warning: RBDY2 without bulk',ibdyty
          call LOGMES(cout)
       end if

       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'nodty') cycle                ! fishing for the keyword 'nodty'
          do
             if( .not. read_G_clin()) exit
             itest=itest_nodty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit
             if (itest == ifound) inodty=inodty+1
             cycle
          end do
          exit
       end do
       backspace(G_nfich)

       if (inodty /= 1) then
          write(cout,'(A33,I7)') 'warning: RBDY2 should have 1 node',ibdyty
          call faterr(IAM,cout)
       end if

       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'tacty') cycle                ! fishing for the keyword 'tacty' 
          do
             if( .not. read_G_clin()) exit
             itest=itest_tacty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit
             if (itest == ifound) itacty=itacty+1
             cycle
          end do
          exit
       end do
       backspace(G_nfich)

       if (itacty /= 0) then
          allocate(bdyty(ibdyty)%tacty(itacty),stat=errare)
          if (errare /= 0) then
             call FATERR(IAM,'error allocating tacty')
          end if
          do itacty=1,size(bdyty(ibdyty)%tacty)
             nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%data)
          enddo
       else 
          nullify(bdyty(ibdyty)%tacty)
          write(cout,'(A32,I7)') 'warning: RBDY2 without contactor',ibdyty
          call LOGMES(cout)
       end if
       cycle
    end do
    rewind(G_nfich)   

    ! third reading: filling types

    ibdyty=0
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                 ! fishing for the keyword 'bdyty'
       if( .not. read_G_clin()) exit
       itest=itest_bdyty(G_clin)                      
       if (itest /= ifound) cycle
       ibdyty=ibdyty+1

       iblmty=0
       inodty=0
       itacty=0

       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'blmty') cycle                ! fishing for the keyword 'blmty' 
          do
             if( .not. read_G_clin()) exit
             itest=itest_blmty(G_clin,ibdyty)                      
             if (itest == isskip) cycle
             if (itest == inomor) exit
             if (itest == ifound) then
                iblmty=iblmty+1  
                read(G_clin(2:6),'(A5)') bdyty(ibdyty)%blmty(iblmty)%blmID
             end if
             cycle
          end do
          exit
       end do
       backspace(G_nfich)

       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'nodty') cycle                ! fishing for the keyword 'nodty' 
          do
             if( .not. read_G_clin()) exit
             itest=itest_nodty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                      
             if (itest == ifound) call new_nodty(bdyty(ibdyty)%nodty,G_clin(2:6))
             cycle
          end do
          exit       
       end do
       backspace(G_nfich)   

       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'tacty') cycle                ! fishing for the keyword 'tacty'
          do
             if( .not. read_G_clin()) exit
             itest=itest_tacty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                                            
             if (itest == ifound) then
                itacty=itacty+1
                read(G_clin(2:6),'(A5)')bdyty(ibdyty)%tacty(itacty)%tacID
             end if
             cycle
          end do
          exit
       end do
       cycle
    end do
    rewind(G_nfich)   

    ! concatenate dof to operate bodies node data
    ! first: sizing 

    do ibdyty=1,nb_RBDY2

       iccdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)

       if (iccdof /= 0) then
          allocate(bdyty(ibdyty)%cooref(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%coor(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Vbegin(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Xbegin(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%V(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%X(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Vfree(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Vaux(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Fext(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Fint(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Ireac(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%Iaux(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%mass(iccdof),stat=errare)
          allocate(bdyty(ibdyty)%inv_mass(iccdof),stat=errare)

          if (smooth_method) then 
             allocate(bdyty(ibdyty)%A(iccdof),stat=errare)
             allocate(bdyty(ibdyty)%Abegin(iccdof),stat=errare)
             allocate(bdyty(ibdyty)%B(iccdof),stat=errare)
             allocate(bdyty(ibdyty)%Bbegin(iccdof),stat=errare)
             allocate(bdyty(ibdyty)%C(iccdof),stat=errare)
             allocate(bdyty(ibdyty)%Cbegin(iccdof),stat=errare)
          else
             nullify(bdyty(ibdyty)%A)
             nullify(bdyty(ibdyty)%Abegin)
             nullify(bdyty(ibdyty)%B)
             nullify(bdyty(ibdyty)%Bbegin)
             nullify(bdyty(ibdyty)%C)
             nullify(bdyty(ibdyty)%Cbegin)
          end if

          if (errare /= 0) then
             call FATERR(IAM,'error allocating X,V,...')
          end if
       else 
          nullify(bdyty(ibdyty)%cooref)
          nullify(bdyty(ibdyty)%coor)
          nullify(bdyty(ibdyty)%Vbegin)
          nullify(bdyty(ibdyty)%Xbegin)
          nullify(bdyty(ibdyty)%V)
          nullify(bdyty(ibdyty)%X)
          nullify(bdyty(ibdyty)%Vfree)
          nullify(bdyty(ibdyty)%Vaux)
          nullify(bdyty(ibdyty)%Fext)
          nullify(bdyty(ibdyty)%Fint)
          nullify(bdyty(ibdyty)%Ireac)
          nullify(bdyty(ibdyty)%Iaux)
          nullify(bdyty(ibdyty)%mass)
          nullify(bdyty(ibdyty)%inv_mass)
          nullify(bdyty(ibdyty)%A)
          nullify(bdyty(ibdyty)%Abegin)
          nullify(bdyty(ibdyty)%B)
          nullify(bdyty(ibdyty)%Bbegin)
          nullify(bdyty(ibdyty)%C)
          nullify(bdyty(ibdyty)%Cbegin)
          write(cout,'(A26,I7)')'warning: RBDY2 without DOF',ibdyty 
          call faterr(IAM,cout)
       end if

    end do

    ! fourth reading: filling in data

    ibdyty=0
    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                 ! fishing for the keyword 'bdyty'
       if( .not. read_G_clin()) exit
       itest=itest_bdyty(G_clin)                      
       if (itest /= ifound) cycle

       ibdyty=ibdyty+1


       iblmty=0
       inodty=0
       itacty=0

       do  
          if( .not. read_G_clin()) exit  
          if (G_clin(2:6) /= 'blmty') cycle                ! fishing for the keyword 'blmty' 
          do
             if( .not. read_G_clin()) exit  
             itest=itest_blmty(G_clin,ibdyty)                      
             if (itest == isskip) cycle
             if (itest == inomor) exit
             if (itest == ifound) then
                iblmty=iblmty+1  
                select case(G_clin(2:6))
                   ! check here all bulk elements selected to build body RBDY2
                case('PLAIN')    
                   call read_PLAIN(bdyty(ibdyty),iblmty)
                case('NULLx')
                   call read_NULLx(bdyty(ibdyty),iblmty)
                   !case('BLMXX')    
                   !call read_BLMXX(ibdyty,iblmty,G_clin)
                case default  
                end select
             end if
             cycle
          end do
          exit
       end do
       backspace(G_nfich)

       do    
          if( .not. read_G_clin()) exit  
          if (G_clin(2:6) /= 'nodty') cycle                ! fishing for the keyword 'nodty' 
          do
             if( .not. read_G_clin()) exit  
             itest=itest_nodty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                      
             if (itest == ifound) then     
                nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
                !
                call read_a_nodty(G_clin,bdyty(ibdyty)%cooref(1:nbdof))

                bdyty(ibdyty)%coor  = bdyty(ibdyty)%cooref

                bdyty(ibdyty)%Xbegin  = 0.D0
                bdyty(ibdyty)%Vbegin  = 0.D0
                bdyty(ibdyty)%X       = 0.D0
                bdyty(ibdyty)%V       = 0.D0
                bdyty(ibdyty)%Vfree   = 0.D0
                bdyty(ibdyty)%Ireac   = 0.D0
                bdyty(ibdyty)%Iaux    = 0.D0
                bdyty(ibdyty)%visible = .true.
                bdyty(ibdyty)%r2m     = .false.
                bdyty(ibdyty)%SIGMA   = 0.D0
                if (smooth_method) then 
                   bdyty(ibdyty)%A      = 0.D0
                   bdyty(ibdyty)%B      = 0.D0
                   bdyty(ibdyty)%C      = 0.D0
                   bdyty(ibdyty)%Abegin = 0.D0
                   bdyty(ibdyty)%Bbegin = 0.D0
                   bdyty(ibdyty)%Cbegin = 0.D0
                end if
             end if
             cycle
          end do
          exit       
       end do
       backspace(G_nfich)   

       do    
          if( .not. read_G_clin()) exit  
          if (G_clin(2:6) /= 'tacty') cycle                ! fishing for the keyword 'tacty'
          do
             if( .not. read_G_clin()) exit  
             itest=itest_tacty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                                            
             if (itest == ifound) then
                itacty=itacty+1
                read(G_clin( 2: 6),'(A5)')bdyty(ibdyty)%tacty(itacty)%tacID         
                read(G_clin(23:27),'(A5)')bdyty(ibdyty)%tacty(itacty)%color
                nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%data)
                nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
                nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%Ws)
                nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%Wsini)
                nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%Wstime)
                nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%Wsstatus)
                select case(G_clin(2:6))
                case('DISKx')
                   call read_BDARY_DISKx(bdyty(ibdyty)%tacty(itacty)%BDARY%data,& 
                        bdyty(ibdyty)%tacty(itacty)%BDARY%area, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%rdg, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%grdg)

                   bdyty(ibdyty)%tacty(itacty)%BDARY%shift=0.D0

                case('xKSID')
                   call read_BDARY_xKSID(bdyty(ibdyty)%tacty(itacty)%BDARY%data,&
                        bdyty(ibdyty)%tacty(itacty)%BDARY%area)

                   bdyty(ibdyty)%tacty(itacty)%BDARY%rdg = 0.d0
                   bdyty(ibdyty)%tacty(itacty)%BDARY%grdg =0.d0
                   bdyty(ibdyty)%tacty(itacty)%BDARY%shift=0.D0

                case('JONCx')
                   call read_BDARY_JONCx(bdyty(ibdyty)%tacty(itacty)%BDARY%data,&
                        bdyty(ibdyty)%tacty(itacty)%BDARY%area,& 
                        bdyty(ibdyty)%tacty(itacty)%BDARY%rdg, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%grdg)

                   bdyty(ibdyty)%tacty(itacty)%BDARY%shift=0.D0

                case('POLYG')
                   call read_BDARY_POLYG(ibdyty,bdyty(ibdyty)%tacty(itacty)%BDARY%data, & 
                        bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%area, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%rdg, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%grdg, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%shift)


                   !fd             print*,ibdyty,itacty
                   !fd             print*,bdyty(ibdyty)%cooref
                   !fd             print*,bdyty(ibdyty)%tacty(itacty)%BDARY%idata,bdyty(ibdyty)%tacty(itacty)%BDARY%data
                   !fd             print*,bdyty(ibdyty)%tacty(itacty)%BDARY%area,bdyty(ibdyty)%tacty(itacty)%BDARY%shift

                case('PT2Dx')
                   call read_BDARY_PT2Dx(bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

                   bdyty(ibdyty)%tacty(itacty)%BDARY%area = 0.d0
                   bdyty(ibdyty)%tacty(itacty)%BDARY%rdg  = 0.d0
                   bdyty(ibdyty)%tacty(itacty)%BDARY%grdg = 0.d0

                   !fd
                   !fd balourd DISKx which means excentered DISKx
                   !fd
                case('DISKb')
                   call read_BDARY_DISKb(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%area, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%rdg, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%grdg, &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

                case default  
                end select
                !mr
                bdyty(ibdyty)%tacty(itacty)%BDARY%betai=0.d0
             end if
             cycle
          end do
          exit
       end do
       cycle
    end do

    do ibdyty=1,nb_RBDY2
       comp_blmty=.false.       
       comp_nodty=.false.       
       !fd
       !fd stupide il n'y a qu'un iblmty
       !fd
       do iblmty=1,size(bdyty(ibdyty)%blmty)
          select case(bdyty(ibdyty)%blmty(iblmty)%blmID)
          case('PLAIN')
             !fd
             !fd on ajoute un flag pour recalculer avrd et gyrd si ils sont nuls
             !fd
             if ( bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius == 0.d0 .or. &
                  bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius == 0.d0) comp_blmty=.true.
          case default  
             call faterr(IAM,'blmty is not PLAIN')
          end select
       end do

       !fd
       !fd on ajoute un flag pour recalculer cooref si ils sont nuls
       !fd
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)

       if (dot_product(bdyty(ibdyty)%cooref(1:nbdof),bdyty(ibdyty)%cooref(1:nbdof)) == 0.d0) comp_nodty=.true.

       if (size(bdyty(ibdyty)%tacty) == 1) then
          if (comp_blmty) then
             bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius = bdyty(ibdyty)%tacty(1)%BDARY%rdg
             bdyty(ibdyty)%blmty(1)%PLAIN%gyr_radius = bdyty(ibdyty)%tacty(1)%BDARY%grdg
          endif
          !fd
          !fd on est dans le cas ou on a volontairement decrit les vertex dans le repere 
          !fd absolu et pas dans le repere barycentrique
          !fd
          if (comp_nodty) then
             bdyty(ibdyty)%cooref(1:2)         = bdyty(ibdyty)%tacty(1)%BDARY%shift
             bdyty(ibdyty)%cooref(3:nbdof)     = 0.d0
             bdyty(ibdyty)%tacty(1)%BDARY%shift= 0.d0
          else
             !fd
             !fd on est dans un cas non gere car si cooref /= (0.,0.) alors OG = (0. 0.) (i.e. le shift)
             !fd
             !rm 10/11/2016 : since OG computation changed in mod_a_BDARY_POLYG some old DATBOX may have
             !                a wrong value for cooref, in this case it has been decided to adjust the cooref
             !                after warning the user
             if (dot_product(bdyty(ibdyty)%tacty(1)%BDARY%shift(1:2),bdyty(ibdyty)%tacty(1)%BDARY%shift(1:2)) >= 1.d-6) then

                OG = bdyty(ibdyty)%tacty(1)%BDARY%shift(1:2)

                write(cout, '(A,I0)')   '[WARNING][RBDY2:read_bodies] In contactor definition  of body ', ibdyty
                call logmes(cout,.true.)

                write(cout, '(A,D7.5)') '                             Inertia center provided is offset from the computed one by ', &
                                          sqrt( dot_product( OG, OG ) )
                                          !norm2(bdyty(ibdyty)%tacty(1)%BDARY%shift(1:2))
                call logmes(cout,.true.)

                write(cout, '(A)')      '                             The center of the polyg will be adjusted accordingly to computed value!'
                call logmes(cout,.true.)
                bdyty(ibdyty)%cooref(1:2)          = bdyty(ibdyty)%cooref(1:2) + bdyty(ibdyty)%tacty(1)%BDARY%shift
                bdyty(ibdyty)%tacty(1)%BDARY%shift = 0.d0
             else 
                !fd            print*,ibdyty,dot_product(bdyty(ibdyty)%tacty(1)%BDARY%shift(1:2),bdyty(ibdyty)%tacty(1)%BDARY%shift(1:2))
                bdyty(ibdyty)%tacty(1)%BDARY%shift = 0.d0
             endif
          endif
          if (bdyty(ibdyty)%tacty(1)%BDARY%area > 0.d0) then
             bdyty(ibdyty)%blmty(1)%PLAIN%area=bdyty(ibdyty)%tacty(1)%BDARY%area
          else
             bdyty(ibdyty)%blmty(1)%PLAIN%area=0.d0
          endif
       else
          !fd
          !fd si necessaire on recalcule le centre d'inertie et on corrige le shift
          !fd
          if (comp_nodty) then

             OG=0.d0
             mT=0.d0

             do itacty=1,size(bdyty(ibdyty)%tacty) 
                mi = bdyty(ibdyty)%tacty(itacty)%BDARY%rdg*bdyty(ibdyty)%tacty(itacty)%BDARY%rdg            
                OG = OG + (mi*bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
                mT = mT + mi
             enddo
             OG = OG/mT      
             do itacty=1,size(bdyty(ibdyty)%tacty) 
                bdyty(ibdyty)%tacty(itacty)%BDARY%shift = bdyty(ibdyty)%tacty(itacty)%BDARY%shift - OG 
             enddo
             nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
             bdyty(ibdyty)%cooref(1:2)    = OG
             bdyty(ibdyty)%cooref(3:nbdof)=0.d0
          endif
          !fd
          !fd
          !fd
          if (comp_blmty) then
             mT=0.d0
             iT=0.d0
             bdyty(ibdyty)%blmty(1)%PLAIN%area=0.D0
             do itacty=1,size(bdyty(ibdyty)%tacty)

                mi = PI_g*bdyty(ibdyty)%tacty(itacty)%BDARY%rdg*bdyty(ibdyty)%tacty(itacty)%BDARY%rdg
                ii = mi*bdyty(ibdyty)%tacty(itacty)%BDARY%grdg*bdyty(ibdyty)%tacty(itacty)%BDARY%grdg
                d2 = dot_product(bdyty(ibdyty)%tacty(itacty)%BDARY%shift,bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

                mT = mT + mi
                iT = iT + ii + (mi*d2)

                if (bdyty(ibdyty)%tacty(itacty)%BDARY%area > 0.D0) then
                   bdyty(ibdyty)%blmty(1)%PLAIN%area=bdyty(ibdyty)%blmty(1)%PLAIN%area + &
                        bdyty(ibdyty)%tacty(itacty)%BDARY%area
                endif
             enddo

             bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius = dsqrt(mT/PI_g)
             bdyty(ibdyty)%blmty(1)%PLAIN%gyr_radius = dsqrt(iT/mT)
          endif

       endif
    enddo

  end subroutine read_bodies

  subroutine modify_body(ibdyty,itacty,r8_vector)
    IMPLICIT NONE
    integer                    :: ibdyty,itacty
    real(kind=8),dimension(:)  :: r8_vector

                              !1234567890123456788901
    character(len=21)  :: IAM='mod_RBDY2::modify_body'


    if (itacty /= 1 .or. size(bdyty(ibdyty)%tacty) /= 1) then 
      call FATERR(IAM,'one can modify rbdy2 containing only one tacty')
    endif

    if (associated(bdyty(ibdyty)%tacty(itacty)%BDARY%data)) &
        deallocate(bdyty(ibdyty)%tacty(itacty)%BDARY%data)  
    if (associated(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)) &
        deallocate(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)  

    nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%data)
    nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)

    call comp_BDARY_POLYG(ibdyty,r8_vector,bdyty(ibdyty)%tacty(itacty)%BDARY%data, & 
                          bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                          bdyty(ibdyty)%tacty(itacty)%BDARY%area, &
                          bdyty(ibdyty)%tacty(itacty)%BDARY%rdg, &
                          bdyty(ibdyty)%tacty(itacty)%BDARY%grdg, &
                          bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

   bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius = bdyty(ibdyty)%tacty(1)%BDARY%rdg
   bdyty(ibdyty)%blmty(1)%PLAIN%gyr_radius = bdyty(ibdyty)%tacty(1)%BDARY%grdg

   bdyty(ibdyty)%cooref(1:2)    = bdyty(ibdyty)%tacty(1)%BDARY%shift
   bdyty(ibdyty)%cooref(3)=0.d0
   bdyty(ibdyty)%tacty(1)%BDARY%shift=0.d0

   bdyty(ibdyty)%blmty(1)%PLAIN%area=bdyty(ibdyty)%tacty(1)%BDARY%area

  end subroutine 
!!!------------------------------------------------------------------------
  subroutine update_existing_entities_RBDY2

    implicit none

    nb_existing_entities = get_nb_ENTITY()
    call add_nb_ENTITY(nb_RBDY2)

  end subroutine update_existing_entities_RBDY2
!!!------------------------------------------------------------------------
  subroutine set_periodic_data_RBDY2(per)

    implicit none
    
    real(kind=8) :: per

    periode  = per
    PERIODIC = .true.

  end subroutine set_periodic_data_RBDY2
!!!------------------------------------------------------------------------ 
  subroutine write_bodies(nfich)

    implicit none

    integer            :: ibdyty,iblmty,itacty,nbdof
    character(len=23)  :: IAM ='mod_RBDY2::write_bodies'
    character(len=103) :: cout

    ! fd & bc conversion r2m

    real(kind=8)                :: a,Tx,Ty                !a: angle de rotation, Tx,Ty: translation
    real(kind=8)                :: c,s                    !cos(a) et sin(a) pr les calculer qu'une seule fois
    real(kind=8),dimension(3)   :: X

    integer      :: i,nfich,imailx,nb_vertex,it3xxx,inode,itac
    real(kind=8) :: dir,inorme,d1,d2
    real(kind=8),dimension(:,:),allocatable :: vertex,vertex_ref,normale,normale_ref

    character(len=5) :: old_color

    do ibdyty=1,nb_RBDY2

       !bc & fd cuisine pour trasformer un rigide en maille  PAS TOUCHE !
       if (bdyty(ibdyty)%r2m) then
          if (size(bdyty(ibdyty)%tacty) > 1 ) then
             call faterr(IAM,'You can not generate a MAILx from a RBDY2 containing more than one tacty')
          end if
          if (bdyty(ibdyty)%tacty(1)%tacID /= 'POLYG') then
             call faterr(IAM,'You can not generate a MAILx from a tacty that is not a POLYG')
          end if
          
          imailx=imailx+1
          nfich=1
          
          X = bdyty(ibdyty)%cooref + bdyty(ibdyty)%X 
          
          Tx= X(1)
          Ty= X(2)
          a = X(3)
          
          c=cos(a);s=sin(a)

          nb_vertex=bdyty(ibdyty)%tacty(1)%BDARY%idata(1)
          allocate(vertex_ref(2,nb_vertex),vertex(2,nb_vertex))
          allocate(normale_ref(2,nb_vertex),normale(2,nb_vertex))
          vertex_ref  = reshape(source=bdyty(ibdyty)%tacty(1)%BDARY%data,shape=(/ 2, nb_vertex /))

          do i=1,nb_vertex-1
             d1=vertex_ref(1,i)-vertex_ref(1,i+1)
             d2=vertex_ref(2,i)-vertex_ref(2,i+1)
             inorme=1.d0/sqrt((d1*d1)+(d2*d2))
             normale_ref(1,i)=-d2*inorme
             normale_ref(2,i)= d1*inorme
          end do

          d1=vertex_ref(1,nb_vertex)-vertex_ref(1,1)
          d2=vertex_ref(2,nb_vertex)-vertex_ref(2,1)
          inorme=1.d0/sqrt((d1*d1)+(d2*d2))
          normale_ref(1,nb_vertex)=-d2*inorme
          normale_ref(2,nb_vertex)= d1*inorme

          do i=1,nb_vertex

             vertex(1,i)= c*vertex_ref(1,i) &
                  -s*vertex_ref(2,i) &
                  +Tx
             vertex(2,i)= s*vertex_ref(1,i) &
                  +c*vertex_ref(2,i) &
                  +Ty
             normale(1,i)= c*normale_ref(1,i) &
                  -s*normale_ref(2,i) 

             normale(2,i)= s*normale_ref(1,i) &
                  +c*normale_ref(2,i)
          enddo

          write(nfich,'(A72)') '$bdyty                                                                  '
          write(nfich,10)'MAILx',imailx
          !123456789012345678901234567890123456789012345678901234567890123456789012
          write(nfich,'(A72)') '$blmty                                                                  '

          inode=1
          it3xxx=0
          do i=1,nb_vertex-1
             it3xxx=it3xxx+1
             inode=inode+1
             write(nfich,20) it3xxx,1,inode,inode+1
             write(nfich,30) 'M2D_L','stone'
             it3xxx=it3xxx+1
             inode=inode+1
             write(nfich,20) it3xxx,1,inode,inode+1
             write(nfich,30) 'M2D_L','stone'
          enddo
          it3xxx=it3xxx+1
          inode=inode+1
          write(nfich,20) it3xxx,1,inode,inode+1
          write(nfich,30) 'M2D_L','stone'
          it3xxx=it3xxx+1 
          inode=inode+1
          write(nfich,20) it3xxx,1,inode,2
          write(nfich,30) 'M2D_L','stone'

          inode=1
          write(nfich,'(A72)') '$nodty                                                                  '
          call write_a_nodty('NO2xx',inode,X,'coo',nfich)

          do i=1,nb_vertex-1
             inode=inode+1
             X(1:2)=vertex(1:2,i)
             call write_a_nodty('NO2xx',inode,X,'coo',nfich)
             inode=inode+1
             X(1:2)=(vertex(1:2,i)+vertex(1:2,i+1))/2
             call write_a_nodty('NO2xx',inode,X,'coo',nfich)
          end do
          inode=inode+1
          X(1:2)=vertex(1:2,nb_vertex)
          call write_a_nodty('NO2xx',inode,X,'coo',nfich)
          inode=inode+1
          X(1:2)=(vertex(1:2,nb_vertex)+vertex(1:2,1))/2
          call write_a_nodty('NO2xx',inode,X,'coo',nfich)


          old_color=bdyty(ibdyty)%tacty(1)%color

          write(nfich,'(A72)') '$tacty                                                                  '
          itac=0
          inode=1
          do i=1,nb_vertex-1
             itac=itac+1
             inode=inode+1
             dir= normale(1,i)*dsqrt(2.d0) + normale(2,i)*dsqrt(2.d0)

             write(nfich,50) 'ALpxx',itac,'color',old_color,'noda=',inode+1,'nodb=',inode 
             itac=itac+1
             write(nfich,40) 'ALpxx',itac,'color',old_color,'noda=',inode+2,'nodb=',inode+1

             if (dir < 0.d0) then
                write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.2
                itac=itac+1
                write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.8 

                inode=inode+1
                itac=itac+1
                write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.2
                itac=itac+1
                write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.8 
             else 
                write(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode 
                itac=itac+1
                inode=inode+1
                write(nfich,40) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode
             endif
          enddo
          dir= normale(1,nb_vertex)*dsqrt(2.d0) + &
               normale(2,nb_vertex)*dsqrt(2.d0)
          itac=itac+1
          inode=inode+1

          write(nfich,50) 'ALpxx',itac,'color',old_color,'noda=',inode+1,'nodb=',inode 
          itac=itac+1
          write(nfich,40) 'ALpxx',itac,'color',old_color,'noda=',2,'nodb=',inode+1


          if (dir < 0.d0) then
             write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.2
             itac=itac+1
             write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.8 

             inode=inode+1
             itac=itac+1
             write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.2
             itac=itac+1
             write(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.8 
          else
             write(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode 
             itac=itac+1
             inode=inode+1
             write(nfich,40) 'ALpxx',itac,'color','REDxx','noda=',2,'nodb=',inode
          endif

          write(nfich,'(A72)') '$$$$$$                                                                  '

10        format(1X,A5,2X,I5)            
20        format(' T3xxx',2x,I5,2x,'nodes',8(2x,I5))
30        format(15x,'model',2x,A5,2x,'behav',2x,A5)
40        format(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,D14.7)
50        format(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)


          bdyty(ibdyty)%blmty(1)%behav='ghost'
          
       end if

       ! vv: to skip invisible bodies if skip_invisible setted 
       ! to true by calling routine set_skip_invisible.
       IF (skip_invisible .AND. .NOT. bdyty(ibdyty)%visible) CYCLE

       write(nfich,'(A6)') '$bdyty'
       write(nfich,102)bdyty(ibdyty)%bdyID,ibdyty
       
       write(nfich,'(A6)') '$blmty'
       do iblmty=1,size(bdyty(ibdyty)%blmty)
          select case(bdyty(ibdyty)%blmty(iblmty)%blmID)
          case('PLAIN')
             call write_PLAIN(nfich,bdyty(ibdyty),iblmty)                   
          case('NULLx')
             call write_NULLx(nfich,bdyty(ibdyty),iblmty)                   
             !case('BLMXX')
             !call write_BLMXX(ibdyty,iblmty)                   
          case default  
             write(cout,'(A7,A5,A18,I5)')' blmty ',bdyty(ibdyty)%blmty(iblmty)%blmID,' unknown in RBDY2 ',ibdyty
             !1234567                                     123456789012   34567   8
             call FATERR(IAM,cout)
          end select
       end do

       write(nfich,'(A6)') '$nodty'
       nbdof = nbdof_a_nodty(bdyty(ibdyty)%nodty)
       call write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1, &
            bdyty(ibdyty)%cooref(1:nbdof), &
            'coo',nfich)
       
       write(nfich,'(A6)') '$tacty'
       do itacty=1,size(bdyty(ibdyty)%tacty) 
          select case(bdyty(ibdyty)%tacty(itacty)%tacID)
          case('DISKx')
             call write_BDARY_DISKx(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1))
          case('xKSID') 
             call write_BDARY_xKSID(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1))
          case('JONCx')
             call write_BDARY_JONCx(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1), &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(2))
          case('POLYG')
             call write_BDARY_POLYG(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(:), &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1),&
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          case('PT2Dx') 
             call write_BDARY_PT2Dx(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1), &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2) )
          case('DISKb')
             call write_BDARY_DISKb(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1),&
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

          case default
             write(cout,'(A6,A5,A8)') 'tacty ',bdyty(ibdyty)%tacty(itacty)%tacID,' unknown'
             call FATERR(IAM,cout)
          end select
       end do
       write(nfich,'(A6)')'$$$$$$'
       write(nfich,'(A6)')'      '
    end do
    !                 123456789012345678901234567890123456789012345678901234567890123456789012
    write(nfich,'(A72)') '!-----------------------------------------------------------------------' 

102 format(1X,A5,2X,I7)                       

  end subroutine write_bodies
!!!------------------------------------------------------------------------   
  subroutine write_cleared_bodies(nfich)

    implicit none

    integer            :: ibdyty,iblmty,itacty,nbdof,nfich
    character(len=31)  :: IAM ='mod_RBDY2::write_bodies_cleared'
    character(len=103) :: cout

    do ibdyty=1,nb_RBDY2

       write(nfich,'(A6)') '$bdyty'
       write(nfich,102) bdyty(ibdyty)%bdyID,ibdyty
       
       write(nfich,'(A6)') '$blmty'
       do iblmty=1,size(bdyty(ibdyty)%blmty)
          select case(bdyty(ibdyty)%blmty(iblmty)%blmID)
          case('PLAIN')
             call write_PLAIN_cleared(nfich,bdyty(ibdyty),iblmty)                   
          case('NULLx')
             call write_NULLx(nfich,bdyty(ibdyty),iblmty)                   
             !case('BLMXX')
             !call write_BLMXX(ibdyty,iblmty)                   
          case default  
             write(cout,'(A7,A5,A18,I5)')' blmty ',bdyty(ibdyty)%blmty(iblmty)%blmID,' unknown in RBDY2 ',ibdyty
             call FATERR(IAM,cout)
          end select
       end do

       write(nfich,'(A6)') '$nodty'
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       call write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1, &
            bdyty(ibdyty)%cooref(1:nbdof), &
            'coo',nfich)

       write(nfich,'(A6)') '$tacty'
       do itacty=1,size(bdyty(ibdyty)%tacty) 
          select case(bdyty(ibdyty)%tacty(itacty)%tacID)
          case('DISKx')
             call write_BDARY_DISKx(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1))
          case('xKSID') 
             call write_BDARY_xKSID(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1))
          case('JONCx')
             call write_BDARY_JONCx(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1), &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(2))
          case('POLYG')
             call write_BDARY_POLYG(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(:), &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%idata(1),&
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          case('PT2Dx') 
             call write_BDARY_PT2Dx(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1), &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2) )
          case('DISKb')
             call write_BDARY_DISKb(nfich,itacty,bdyty(ibdyty)%tacty(itacty)%tacID, &
                  bdyty(ibdyty)%tacty(itacty)%color, &
                  bdyty(ibdyty)%tacty(itacty)%BDARY%data(1),&
                  bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

          case default
             write(cout,'(A6,A5,A8)') 'tacty ',bdyty(ibdyty)%tacty(itacty)%tacID,' unknown'
             call FATERR(IAM,cout)
          end select
       end do
       write(nfich,'(A6)')'$$$$$$'
       write(nfich,'(A6)')'      '
    end do
    !                 123456789012345678901234567890123456789012345678901234567890123456789012
    write(nfich,'(A72)') '!-----------------------------------------------------------------------' 
    
102 format(1X,A5,2X,I7)                       
    
  end subroutine write_cleared_bodies
!!!------------------------------------------------------------------------   
  subroutine read_behaviours_RBDY2

    implicit none
    integer :: ibdyty,iblmty,ibehav,itest
    character(len=26)  :: IAM = 'mod_RBDY2::read_behaviours'
    character(len=103) :: cout

    do ibdyty=1,nb_RBDY2
       do iblmty=1,size(bdyty(ibdyty)%blmty)

          bdyty(ibdyty)%blmty(iblmty)%lawnb = &
               get_bulk_behav_nb(bdyty(ibdyty)%blmty(iblmty)%behav)

          if (bdyty(ibdyty)%blmty(iblmty)%lawnb == 0) then
             write(cout,'(A9,A5,A9,I7,A17)') 'nickname ',bdyty(ibdyty)%blmty(iblmty)%behav,& 
                  ' body nb ',ibdyty,' unknown in lawty'
             call LOGMES('check BODIES.DAT in DATBOX')
             call LOGMES('check BULK_BEHAV.DAT in DATBOX')
             call FATERR(IAM,cout)
          end if
       end do
       cycle
    end do

  end subroutine read_behaviours_RBDY2
!!!------------------------------------------------------------------------  
  subroutine init_mp_behaviours_RBDY2(disper,flag)
    !!****f* RBDY2/init_mp_behaviours_RBDY2
    !! NAME
    !!  init_mp_behaviours_RBDY2
    !! SYNPOSIS
    !!  init_mp_behaviours_RBDY2(disper,flag)
    !! INPUTS
    !!  REAL(kind=8) : disper   variable dispersion 
    !! PURPOSE
    !!  Initialize electrical and thermal variables according to the description given
    !!  in the file BULK_BEHAV.DAT. The [disper] variable allows to give a variation in
    !!  electrical and thermal properties.
    !!****
    implicit none
    
    integer            :: ibdyty,iblmty,ibehav,itest,itacty,nb_tacty
    real(kind=8)       :: X,ECond,TCond,area,avrd,disper,WS,WSini,WStime
    real(kind=8)       :: PTcond,STcond,Tnx,Tny
    character(len=5)   :: flag
    character(len=32)  :: IAM
    character(len=103) :: cout
    character(len=3)   :: thmodel

    !    12345678901234567890123456789012
    IAM='mod_RBDY2::init_mp_behaviours'

    select case(flag)
    case('therm')
       do ibdyty=1,nb_RBDY2

          !mr : we assume that iblmty = 1
          iblmty = 1
          ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb

          nb_tacty = size(bdyty(ibdyty)%tacty)

          do itacty=1,nb_tacty

             call random_number(X)
             thmodel = get_Tmodel(ibehav)
             
             select case(thmodel)
             case('iso')
                TCond = (1+(2*X-1)*disper)*get_TCond(ibehav)
                bdyty(ibdyty)%tacty(itacty)%BDARY%TCond    = TCond
                bdyty(ibdyty)%tacty(itacty)%BDARY%TCondini = TCond
                bdyty(ibdyty)%tacty(itacty)%BDARY%T        = 0.D0
             case('ani')
                call get_AniTCond(ibehav,PTcond,STcond,Tnx,Tny)
                bdyty(ibdyty)%tacty(itacty)%BDARY%PTCond = PTCond
                bdyty(ibdyty)%tacty(itacty)%BDARY%STCond = STCond
                bdyty(ibdyty)%tacty(itacty)%BDARY%Tnx    = Tnx
                bdyty(ibdyty)%tacty(itacty)%BDARY%Tny    = Tny
!see jr
!mr to jr: Tcond should defined for the case internal(4)
                bdyty(ibdyty)%tacty(itacty)%BDARY%TCond    = 0.5*(PTCond+STCond)
                bdyty(ibdyty)%tacty(itacty)%BDARY%TCondini = 0.5*(PTCond+STCond)
                bdyty(ibdyty)%tacty(itacty)%BDARY%T        = 0.D0
             case default
                write(cout,'(A25)') 'Thermal model not defined'
                call FATERR(IAM,cout)
             end select
          end do
          
       end do
    case('elect')
       do ibdyty=1,nb_RBDY2

          !mr : we assume that iblmty = 1
          iblmty = 1
          ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
          
          call random_number(X)
             
          ECond = (1+(2*X-1)*disper)*get_ECond(ibehav)
          !cz&mr: Econd represents the conductivity
          !ECond = bdyty(ibdyty)%tacty(itacty)%BDARY%area*ECond

          bdyty(ibdyty)%ECond    = ECond
          bdyty(ibdyty)%ECondini = ECond
          bdyty(ibdyty)%EPot     = 0.D0
          bdyty(ibdyty)%ECur     = 0.D0

       end do
    case('sener')

       do ibdyty=1,nb_RBDY2

          !mr : we assume that iblmty = 1
          iblmty = 1
          ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
          
          nb_tacty = size(bdyty(ibdyty)%tacty)
          do itacty=1,nb_tacty
             allocate(bdyty(ibdyty)%tacty(itacty)%BDARY%WS(nb_WSsect))
             allocate(bdyty(ibdyty)%tacty(itacty)%BDARY%WSini(nb_WSsect))
             allocate(bdyty(ibdyty)%tacty(itacty)%BDARY%WStime(nb_WSsect))
             allocate(bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(nb_WSsect))
             
             bdyty(ibdyty)%tacty(itacty)%BDARY%WStime(1:nb_WSsect) = 0.
             bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(1:nb_WSsect) = i_free !i_undefined !vhnh&mr

             WS     = 0.d0
             WStime = 0.d0

             !mr: on recupere la valeur donnee par le modele
             !TO DO thinks about moving such part
             !call compute_WSvsTime(ibehav,i_undefined,WS,Wstime,0.D0)  !WSini =0.D0 vhn&mr
             call Initialize_WS(ibehav,WS)
             !print*,ibehav,ws
             call random_number(X)
             WS = (1.+(2*X-1)*disper)*WS
             !print*,ibdyty,ws
             bdyty(ibdyty)%tacty(itacty)%BDARY%WS(1:nb_WSsect)    = WS
             bdyty(ibdyty)%tacty(itacty)%BDARY%WSini(1:nb_WSsect) = WS
             
          end do
          
       end do
    case default
       call faterr(IAM,'Warning this Multi-Physical model does not exist')
    end select

  end subroutine init_mp_behaviours_RBDY2
!!!------------------------------------------------------------------------
  subroutine read_in_dof

    implicit none

    integer            :: ibdyty,inodty,nbdof,itest
    character(len=5)   :: chnod
    integer            :: errare
    character(len=22)  :: IAM
    character(len=103) :: cout

    !1234567890123456789012
    IAM='mod_RBDY2::read_in_dof'

    ! default values
    !

    do ibdyty = 1,nb_RBDY2
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       bdyty(ibdyty)%Xbegin =0.D0
       bdyty(ibdyty)%X      =0.D0
       bdyty(ibdyty)%Vbegin =0.D0
       bdyty(ibdyty)%V      =0.D0
    end do

    do    
       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                 ! fishing for the keyword 'bdyty'
       if( .not. read_G_clin()) exit
       itest=itest_bdyty(G_clin)
       if (itest /= ifound) cycle
       read(G_clin(9:15),'(I7)') ibdyty
       if (ibdyty <= 0 .or. ibdyty > nb_RBDY2) then
          write(cout,'(A12,I7,A60)') 'body number ',ibdyty,' does not belong to collection'
          call FATERR(IAM,cout)
       end if
       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'nodty') cycle                ! fishing for the keyword 'nodty' 
          do
             if( .not. read_G_clin()) exit
             itest=itest_nodty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                      
             if (itest == ifound) then
                read(G_clin(9:13),'(I5)')inodty
                if (inodty /= 1) then 
                   write(cout,'(A12,I5,A25,I7,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                   !123456789012          1234567890123456789012345
                   call FATERR(IAM,cout)
                end if
                if (get_node_id_from_name(G_clin(2:6)) > nbdof_a_nodty(bdyty(ibdyty)%nodty)) then 
                   write(cout,'(A5,I5,A22,I7)') G_clin(2:6),inodty,' is not nodty of body ', ibdyty
                   !1234567890123456789012
                   call FATERR(IAM,cout)
                end if

                nbdof = nbdof_a_nodty(bdyty(ibdyty)%nodty)

                chnod=G_clin(2:6)

                call G_read_a_nodty(bdyty(ibdyty)%Xbegin(1:nbdof),chnod)

                if( .not. read_G_clin()) exit 
                
                call G_read_a_nodty(bdyty(ibdyty)%Vbegin(1:nbdof),chnod)
                
                !fd
                bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin
                bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin
             
             end if
             cycle
          end do
          exit       
       end do
       cycle
    end do

    !fd test perio a voir 
    if (BOUNDS)  call out_of_bounds_RBDY2

    
  end subroutine read_in_dof
!!!------------------------------------------------------------------------
  subroutine read_in_body_snapshot_sample
    implicit none
    integer            :: ibdyty,itest
    character(len=22)  :: IAM

    !    12345678901234567890123456789012
    IAM='mod_RBDY2::read_in_body_snapshot'

    do ibdyty = 1,nb_RBDY2
       bdyty(ibdyty)%Xbegin = 0.d0
       bdyty(ibdyty)%X      = 0.d0
       bdyty(ibdyty)%Vbegin = 0.d0
       bdyty(ibdyty)%V      = 0.d0
    end do

    itest=0

    do    
       if( .not. read_G_clin()) exit
       itest = itest + 1
       if(itest.gt.nb_RBDY2)then
          call faterr(IAM,'number of bodies do not match with the one in BODIES.DAT')
       end if
       read(G_clin(1:6),'(2(1X,D14.7),15X,3(1X,D14.7))') bdyty(itest)%Xbegin(1),bdyty(itest)%Xbegin(2), &
                                                         bdyty(itest)%Vbegin(1),bdyty(itest)%Vbegin(2),bdyty(itest)%Vbegin(3)
       
       bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin
       bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin

    end do

    if(itest.ne.nb_RBDY2)then
       call logmes('warning, number of bodies do not match with the one in BODIES.DAT')
       print*, itest,'/',nb_RBDY2
    end if

  end subroutine read_in_body_snapshot_sample
!!!------------------------------------------------------------------------
  subroutine write_out_dof(nfich,ifrom,ito)
    
    implicit none

    integer :: ibdyty,nfich,ifrom,ito,nbdof
    integer :: lc 
    
    do ibdyty = ifrom,ito

       ! vv: to skip invisible bodies if skip_invisible setted 
       ! to true by calling routine set_skip_invisible.
       if (skip_invisible .and. .not. bdyty(ibdyty)%visible) cycle
       write(nfich,'(A6)') '$bdyty'
       write(nfich,102) bdyty(ibdyty)%bdyID,ibdyty
       
       write(nfich,'(A6)') '$nodty'

       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)

       call write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1, &
            bdyty(ibdyty)%X(1:nbdof), &
            'X  ',nfich)

       call write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1, &
            bdyty(ibdyty)%V(1:nbdof), &
            'V  ',nfich)
       
       write(nfich,'(A6)')'$$$$$$'
       write(nfich,'(A6)')'      '
    end do
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    write(nfich,'(A72)') '!-----------------------------------------------------------------------' 
    
102 format(1X,A5,2X,I7)            
    
  end subroutine write_out_dof
!!!------------------------------------------------------------------------
  subroutine write_out_Rnod(nfich,ifrom,ito)

    implicit none

    integer :: ibdyty,nfich,ifrom,ito,nbdof 
    
    do ibdyty = ifrom,ito
       
       write(nfich,'(A6)') '$bdyty'
       write(nfich,101) bdyty(ibdyty)%bdyID,ibdyty
       write(nfich,'(A6)') '$nodty'

       nbdof = nbdof_a_nodty(bdyty(ibdyty)%nodty)

       if (smooth_method) then
         call write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1, &
              bdyty(ibdyty)%Ireac(1:nbdof), &
              'F  ',nfich)
       else
         call write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1, &
              bdyty(ibdyty)%Ireac(1:nbdof)/H, &
              'R/H',nfich)
       endif
       write(nfich,'(A6)')'$$$$$$'
       write(nfich,'(A6)')'      '
    end do
    !                      123456789012345678901234567890123456789012345678901234567890123456789012
    write(nfich,'(A72)') '!-----------------------------------------------------------------------' 

101 format(1X,A5,2X,I7)            

  end subroutine write_out_Rnod
!!!------------------------------------------------------------------------
  subroutine read_driven_dof

    implicit none
    
    integer            :: ivd,ifd,ibdyty,inodty,dofnb,itest,nfich
    character(len=5)   :: chnod
    integer            :: errare
    character(len=26)  :: IAM ='mod_RBDY2::read_driven_dof'
    character(len=103) :: cout

    if (nb_RBDY2 == 0) return

    do ibdyty=1,nb_RBDY2
       nullify(bdyty(ibdyty)%vlocy_driven_dof)
       nullify(bdyty(ibdyty)%Vdriv)
       nullify(bdyty(ibdyty)%Xdriv)
       nullify(bdyty(ibdyty)%force_driven_dof)
       nullify(bdyty(ibdyty)%Fdriv)
       bdyty(ibdyty)%nb_vlocy_driven_dof=0
       bdyty(ibdyty)%nb_force_driven_dof=0
       bdyty(ibdyty)%periode = 0
    end do

    ! first reading: sizing array vlocy_driven_dof  

    do    

       ivd=0
       ifd=0

       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                  ! fishing for the keyword 'bdyty'
       if( .not. read_G_clin()) exit
       itest=itest_bdyty(G_clin)  
       if (itest /= ifound) cycle
       read(G_clin(9:15),'(I7)')ibdyty
       
       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'nodty') cycle                ! fishing for the keyword 'nodty' 

          do
             if( .not. read_G_clin()) exit
             itest=itest_nodty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                      
             if (itest == ifound) then
                do
                   if( .not. read_G_clin()) exit
                   if (G_clin(2:6) /= 'dofty') cycle          ! fishing for the keyword 'dofty'
                   do
                      if( .not. read_G_clin()) exit
                      select case(G_clin(2:6))
                      case('vlocy') 
                         ivd=ivd+1
                      case('force') 
                         ifd=ifd+1
                      case default
                         backspace(G_nfich)
                         exit
                      end select
                      cycle
                   end do
                   exit
                end do
             end if
             cycle
          end do
          exit
       end do

       bdyty(ibdyty)%nb_vlocy_driven_dof=ivd

       if(ivd /=0 )then
          allocate(bdyty(ibdyty)%vlocy_driven_dof(ivd),stat=errare)
          allocate(bdyty(ibdyty)%Vdriv(ivd),stat=errare)
          allocate(bdyty(ibdyty)%Xdriv(ivd),stat=errare)
          if (errare/=0) then
             call FATERR(IAM,'error allocating vlocy_driven_dof,Xdriv or Vdriv')
          end if
       end if

       bdyty(ibdyty)%nb_force_driven_dof=ifd

       if(ifd/=0)then
          allocate(bdyty(ibdyty)%force_driven_dof(ifd),stat=errare)
          allocate(bdyty(ibdyty)%Fdriv(ifd),stat=errare)
          if (errare/=0) then
             call FATERR(IAM,'error allocating force_driven_dof or Fdriv')
          end if
       end if

       cycle
    end do
    
    if (sum(bdyty(1:nb_RBDY2)%nb_vlocy_driven_dof) == 0) call LOGMES('warning: no RBDY2 with vlocy_driven_dof')
    if (sum(bdyty(1:nb_RBDY2)%nb_force_driven_dof) == 0) call LOGMES('warning: no RBDY2 with force_driven_dof')

    ! second reading: filling in data

    rewind(G_nfich)

    do    
       ivd=0
       ifd=0

       if( .not. read_G_clin()) exit
       if (G_clin(2:6) /= 'bdyty') cycle                  ! fishing for the keyword 'bdyty' 
       if( .not. read_G_clin()) exit
       itest=itest_bdyty(G_clin)                      
       if (itest /= ifound) cycle
       read(G_clin(9:15),'(I7)')ibdyty
       if (ibdyty <= 0 .or. ibdyty > nb_RBDY2) then

          write(cout,'(A12,I7,A30)') 'body number ',ibdyty,' does not belong to collection'
          !123456789012          123456789012345678901234567890
          call FATERR(IAM,cout)
       end if
       do    
          if( .not. read_G_clin()) exit
          if (G_clin(2:6) /= 'nodty') cycle                ! fishing for the keyword 'nodty' 
          do
             if( .not. read_G_clin()) exit
             itest=itest_nodty(G_clin,ibdyty)
             if (itest == isskip) cycle
             if (itest == inomor) exit                      
             if (itest == ifound) then
                chnod=G_clin(2:6)
                read(G_clin(9:13),'(I5)') inodty
                if (inodty /= 1 ) then 

                   write(cout,'(A12,I5,A25,I7)') 'node number ',inodty,' does not belong to body ',ibdyty
                   !123456789012          1234567890123456789012345
                   call FATERR(IAM,cout)
                endif
                do
                   if( .not. read_G_clin()) exit
                   if (G_clin(2:6) /= 'dofty') cycle          ! fishing for the keyword 'dofty'
                   do
                      if( .not. read_G_clin()) exit
                      select case(G_clin(2:6))
                      case('vlocy') 
                         ivd=ivd+1

                         read(G_clin( 9: 13),'(I5)   ')dofnb

                         if (dofnb <= 0 .or. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty)) then
                            write(cout,'(A11,I5,A25,I7)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                            call FATERR(IAM,cout)
                         endif

                         call read_a_driven_dof(ibdyty,chnod,inodty,G_clin,bdyty(ibdyty)%vlocy_driven_dof(ivd))

                      case('force')
                         ifd=ifd+1

                         read(G_clin( 9: 13),'(I5)   ')dofnb

                         if (dofnb <= 0 .or. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty)) then
                            write(cout,'(A11,I5,A25,I7)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                            call FATERR(IAM,cout)
                         endif

                         call read_a_driven_dof(ibdyty,chnod,inodty,G_clin,bdyty(ibdyty)%force_driven_dof(ifd))

                      case default
                         backspace(G_nfich)
                         exit
                      end select
                      cycle
                   end do
                   exit
                end do
             end if
             cycle
          end do
          exit
       end do
       cycle
    end do

  end subroutine read_driven_dof
!!!------------------------------------------------------------------------ 
  subroutine write_driven_dof(nfich)

    implicit none

    integer :: ivd,ifd,ibdyty,iccdof,nfich

    do ibdyty=1,nb_RBDY2
       if (      (bdyty(ibdyty)%nb_vlocy_driven_dof /= 0 ) &
            .or. (bdyty(ibdyty)%nb_force_driven_dof /= 0 )) then
          ! The ibdyty body has some driven dof
          write(nfich,'(A6)') '$bdyty'
          write(nfich,102) bdyty(ibdyty)%bdyID,ibdyty
          
          write(nfich,'(A6)') '$nodty'
          write(nfich,101) get_nodNAME(bdyty(ibdyty)%nodty),1
          call write_a_driven_dof(nfich)

          do iccdof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty)

             do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
                if (is_a_driven_dof(iccdof,bdyty(ibdyty)%vlocy_driven_dof(ivd))) then
                   call write_a_driven_dof(nfich,'vlocy',bdyty(ibdyty)%vlocy_driven_dof(ivd))
                end if
             end do

             do ifd=1,bdyty(ibdyty)%nb_force_driven_dof
                if (is_a_driven_dof(iccdof,bdyty(ibdyty)%force_driven_dof(ifd))) then
                   call write_a_driven_dof(nfich,'force',bdyty(ibdyty)%force_driven_dof(ifd))
                end if
             end do

          end do
          write(nfich,'(A6)')'$$$$$$'
          write(nfich,'(A6)')'      '
       end if
    end do

    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    write(nfich,'(A72)') '!-----------------------------------------------------------------------' 

101 format(1X,A5,2X,I5)    
102 format(1X,A5,2X,I7)    
    
  end subroutine write_driven_dof
!!!------------------------------------------------------------------------   
  subroutine increment_RBDY2

    implicit none 
    integer :: ibdyty,ivd,iccdof
    real(kind=8) :: Vdrivenbegin,Vdriven,Xdrivenbegin,Xdriven,UMTTH

    if (nb_RBDY2 == 0) return

    select case(M_INTEGRATOR_ID)
!!!-----------------------------------------------------------------------------------------------
    case(INTEGRATOR_MOREAU)
       ! initializing X,V, for the first step iteration;    
       ! initializing is set as follows allowing a single theta method iteration
       ! for constant linear system;
       
       UMTTH =  (1.D0-THETA)*H

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ivd,iccdof,Xdrivenbegin,Xdriven,Vdrivenbegin,Vdriven)

       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2

          if (.not. bdyty(ibdyty)%visible) cycle
          
          bdyty(ibdyty)%Ireac = 0.d0
          bdyty(ibdyty)%Iaux  = 0.d0

          bdyty(ibdyty)%V= bdyty(ibdyty)%Vbegin                
          !fd c'est trop dangereux et ca n'est utilise nul part 
          ! bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin + UMTTH*bdyty(ibdyty)%Vbegin
          ! je remets la prediction lineaire
          bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin + (THETA*H*bdyty(ibdyty)%Vbegin)
          
          if (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle
          
          ! computing driven dof

          do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

             call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)

             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))

             Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
             Xdriven      = Xdrivenbegin + (1.D0-THETA)*H*Vdrivenbegin+THETA*H*Vdriven
             bdyty(ibdyty)%Vdriv(ivd) = Vdriven
             bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

          end do

          call apply_vlocy_driven_dof(ibdyty,iV____)

       end do
       !$OMP END DO
       !$OMP END PARALLEL
!!!-----------------------------------------------------------------------------------------------
    case(INTEGRATOR_GEAR)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ivd,iccdof,Xdrivenbegin,Xdriven,Vdrivenbegin,Vdriven)

       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
          
          if (.not. bdyty(ibdyty)%visible) cycle
          
          bdyty(ibdyty)%Ireac = 0.d0
          bdyty(ibdyty)%Iaux  = 0.d0

          bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin &
               + c1*bdyty(ibdyty)%Vbegin + c2*bdyty(ibdyty)%Abegin  &
               + c3*bdyty(ibdyty)%Bbegin + c4*bdyty(ibdyty)%Cbegin
          
          bdyty(ibdyty)%V= bdyty(ibdyty)%Vbegin &
               + c1*bdyty(ibdyty)%Abegin + c2*bdyty(ibdyty)%Bbegin &
               + c3*bdyty(ibdyty)%Cbegin
          
          bdyty(ibdyty)%A= bdyty(ibdyty)%Abegin &
               + c1*bdyty(ibdyty)%Bbegin + c2*bdyty(ibdyty)%Cbegin

          bdyty(ibdyty)%B= bdyty(ibdyty)%Bbegin + c1*bdyty(ibdyty)%Cbegin               

          if (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle

          ! computing driven dof

          do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
             
             call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
             
             Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
             Xdriven      = Xdrivenbegin + c1*Vdrivenbegin
             bdyty(ibdyty)%Vdriv(ivd) = Vdriven
             bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

          end do

          call apply_vlocy_driven_dof(ibdyty,iV____)
       end do
       !$OMP END DO
       !$OMP END PARALLEL

!!!-----------------------------------------------------------------------------------------------
    case(INTEGRATOR_VERLET)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ivd,iccdof,Xdrivenbegin,Xdriven,Vdrivenbegin,Vdriven)

       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
       
          if (.not. bdyty(ibdyty)%visible) cycle
       
          bdyty(ibdyty)%Ireac = 0.d0
          bdyty(ibdyty)%Iaux  = 0.d0

          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + H*bdyty(ibdyty)%Vbegin + 0.5*H*H*bdyty(ibdyty)%Abegin
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin + 0.5*H*bdyty(ibdyty)%Abegin

          if (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle
          
          ! computing driven dof
          
          do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
             
             call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)
             
             iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
             
             Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
             Xdriven      = Xdrivenbegin + H*Vdrivenbegin
             bdyty(ibdyty)%Vdriv(ivd) = Vdriven
             bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

          end do
          
          call apply_vlocy_driven_dof(ibdyty,iV____)
          
       end do
       !$OMP END DO
       !$OMP END PARALLEL

    case DEFAULT

       call faterr('RBDY2::increment','INTEGRATOR NOT SUPPORTED YET!')

    end select

  end subroutine increment_RBDY2

  logical function is_dof_driven_RBDY2(ibdyty)
    implicit none
    integer, intent(in) :: ibdyty
    !
    integer :: ivd

    is_dof_driven_RBDY2 = .false.

    if( bdyty(ibdyty)%nb_vlocy_driven_dof /= 3 ) return

    is_dof_driven_RBDY2 = .true.
    do ivd = 1, bdyty(ibdyty)%nb_vlocy_driven_dof

      if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active ) then
        is_dof_driven_RBDY2 = .false.
        exit
      end if

    end do

  end function is_dof_driven_RBDY2

  !> Set the value of a velocity driven dof
  subroutine set_vlocy_drvdof_RBDY2(ibdyty, idrvdof, Vdriven)
    implicit none 
    integer(kind=4), intent(in) :: ibdyty       !< body number
    integer(kind=4), intent(in) :: idrvdof      !< driven dof index
    real(kind=8),    intent(in) :: Vdriven      !< Vdriv at end of time step
    !
    integer(kind=4)   :: ivd, iccdof, i
    real(kind=8)      :: Vdrivenbegin, Xdrivenbegin, Xdriven
    character(len=22) :: IAM
    !      1234567890123456789012
    IAM = 'set_vlocy_drvdof_RBDY2'


    if( nb_RBDY2 < ibdyty ) call faterr(IAM,'RBDY2 index too large')

    ivd = 0
    do i = 1, bdyty(ibdyty)%nb_vlocy_driven_dof
      if( bdyty(ibdyty)%vlocy_driven_dof(i)%dofnb == idrvdof ) ivd=i
    end do

    Vdrivenbegin = bdyty(ibdyty)%Vbegin(idrvdof)

    if( ivd == 0 ) call faterr(IAM,'driven dof index not found')

    select case(M_INTEGRATOR_ID)
!!!-----------------------------------------------------------------------------------------------
    case(INTEGRATOR_MOREAU)

      if( .not. bdyty(ibdyty)%visible ) return
      
      iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))

      Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
      Xdriven      = Xdrivenbegin + (1.D0-THETA)*H*Vdrivenbegin+THETA*H*Vdriven
      bdyty(ibdyty)%Vdriv(ivd) = Vdriven
      bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

      call apply_vlocy_driven_dof(ibdyty,iV____)

!!!-----------------------------------------------------------------------------------------------
    case(INTEGRATOR_GEAR)

      if( .not. bdyty(ibdyty)%visible ) return
      
      iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
      
      Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
      Xdriven      = Xdrivenbegin + c1*Vdrivenbegin
      bdyty(ibdyty)%Vdriv(ivd) = Vdriven
      bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

      call apply_vlocy_driven_dof(ibdyty,iV____)

!!!-----------------------------------------------------------------------------------------------
    case(INTEGRATOR_VERLET)

      if( .not. bdyty(ibdyty)%visible ) return
    
      iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
      
      Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
      Xdriven      = Xdrivenbegin + H*Vdrivenbegin
      bdyty(ibdyty)%Vdriv(ivd) = Vdriven
      bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

      call apply_vlocy_driven_dof(ibdyty,iV____)
          
    case default

      call FATERR(IAM,'INTEGRATOR NOT SUPPORTED YET!')

    end select

  end subroutine set_vlocy_drvdof_RBDY2
!!!------------------------------------------------------------------------
  subroutine is_vlocy_drvdof_RBDY2(ibdyty,is_vlocy_drvdof_)

    integer(kind=4), intent(in) :: ibdyty
    logical,        intent(out) :: is_vlocy_drvdof_

    is_vlocy_drvdof_ = .false.

    if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) is_vlocy_drvdof_ = .true.
 
  end subroutine is_vlocy_drvdof_RBDY2
!!!------------------------------------------------------------------------  
  subroutine fatal_damping_RBDY2(ibdyty)
    implicit none 
    integer(kind=4), intent(in), optional :: ibdyty
    integer(kind=4)   :: i
    !
    character(len=40) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = 'RBDY2:fatal_damping'

    if (nb_RBDY2 == 0) return

    if (present(ibdyty)) then

      if( .not. bdyty(ibdyty)%visible ) return
       
      if( ibdyty < 1 .or. ibdyty > nb_RBDY2 ) then
        write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
        call faterr(IAM,cout)
      end if
    
      bdyty(ibdyty)%Vbegin=0.D0
      bdyty(ibdyty)%V=0.D0                                

    else
      do i = 1,nb_RBDY2
        if( .not. bdyty(i)%visible ) cycle
        bdyty(i)%Vbegin=0.D0
        bdyty(i)%V=0.D0                                
      enddo
    endif      
  end subroutine fatal_damping_RBDY2
!!!------------------------------------------------------------------------  
  subroutine partial_damping_RBDY2(iv,Vmax)

    implicit none 

    integer      :: iv,ibdyty
    real(kind=8) :: Vmax,normV,ratio   

    if (nb_RBDY2 == 0) return
    if (modulo(Nstep,iv) .ne. 0) return

    ! imposing a vanishing begin velocity

    do ibdyty = 1,nb_RBDY2
       if (.not. bdyty(ibdyty)%visible) cycle
       normV = dsqrt((bdyty(ibdyty)%Vbegin(1)**2)+(bdyty(ibdyty)%Vbegin(2)**2))
       ratio = normV/Vmax
       if ( ratio > 1.D0 ) then
          bdyty(ibdyty)%Vbegin(1:2)= bdyty(ibdyty)%Vbegin(1:2)/ratio
          bdyty(ibdyty)%V          = bdyty(ibdyty)%Vbegin                                
       end if
    end do

  end subroutine partial_damping_RBDY2
!!!------------------------------------------------------------------------
  subroutine set_data_equilibrium_RBDY2(checktype,tol)

    implicit none
    
    character(len=5) :: checktype
    real(kind=8)     :: tol

    eqs_tol = tol

    select case(checktype)
    case('Qvlcy')
       eqs_ichecktype = iQvlcy    
    case('Mvlcy')
       eqs_ichecktype = iMvlcy
    case default
       call faterr('RBDY2::seT_data_equilibrium',' @ WARNING: unknown checktype: '//checktype)
    end select

  end subroutine set_data_equilibrium_RBDY2
!!!------------------------------------------------------------------------
  subroutine check_equilibrium_state_RBDY2(info)

    implicit none 

    integer           :: ibdyty
    real(kind=8)      :: norm,Qnorm,Mnorm
    logical           :: info
    character(len=80) :: cout

    Qnorm = 0.D0
    Mnorm =-1.D20

    do ibdyty=1,nb_RBDY2
       if(.not.bdyty(ibdyty)%visible) cycle
       norm = dsqrt(&
            bdyty(ibdyty)%Vbegin(1)*bdyty(ibdyty)%Vbegin(1) + &
            bdyty(ibdyty)%Vbegin(2)*bdyty(ibdyty)%Vbegin(2) + &
            bdyty(ibdyty)%Vbegin(3)*bdyty(ibdyty)%Vbegin(3))
       
       Qnorm = Qnorm + norm
       Mnorm= max (Mnorm,norm)                                
    end do

    Qnorm = Qnorm / real(nb_RBDY2,8)  
    
    write(cout,'(1X,A3,2(3X,A12,D10.3,1X))') &
         ' @ ','Qnorm / tol=',Qnorm/eqs_tol,'Mnorm / tol=',Mnorm/eqs_tol 
    call LOGMES(cout)

    info = .false.

    select case(eqs_ichecktype)
    case(iQvlcy)
       if (Qnorm <= eqs_tol) info = .true.
    case(iMvlcy)
       if (Mnorm <= eqs_tol) info = .true.
    end select

    
  end subroutine check_equilibrium_state_RBDY2
!!!------------------------------------------------------------------------    
  subroutine update_dof_RBDY2

    implicit none 
    integer :: ibdyty

    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
          bdyty(ibdyty)%Vbegin=bdyty(ibdyty)%V                
          bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X
          bdyty(ibdyty)%coor  =bdyty(ibdyty)%cooref+bdyty(ibdyty)%X
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_GEAR)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
          bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X 
          bdyty(ibdyty)%Vbegin=bdyty(ibdyty)%V                
          bdyty(ibdyty)%Abegin=bdyty(ibdyty)%A
          bdyty(ibdyty)%Bbegin=bdyty(ibdyty)%B
          bdyty(ibdyty)%Cbegin=bdyty(ibdyty)%C
          bdyty(ibdyty)%coor  =bdyty(ibdyty)%cooref+bdyty(ibdyty)%X
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_VERLET)  
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
          bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X 
          bdyty(ibdyty)%Vbegin=bdyty(ibdyty)%V                
          bdyty(ibdyty)%Abegin=bdyty(ibdyty)%A
          bdyty(ibdyty)%coor  =bdyty(ibdyty)%cooref+bdyty(ibdyty)%X
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case DEFAULT

       call faterr('RBDY2::update_dof','INTEGRATOR NOT SUPPORTED YET!')

    end select

    if(FREE_BOUNDARY) call free_boundary_computation

  end subroutine update_dof_RBDY2
!!!------------------------------------------------------------------------  
  subroutine comp_free_vlocy_RBDY2

    implicit none

    integer :: ibdyty

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP PRIVATE(ibdyty)
    !$OMP DO SCHEDULE(RUNTIME)
    do ibdyty=1,nb_RBDY2

       if(.not.bdyty(ibdyty)%visible) cycle
       
       !fd a quoi ca sert de faire si complique ?!
       !fd     bdyty(ibdyty)%Vfree= bdyty(ibdyty)%V + &
       !fd                          bdyty(ibdyty)%inv_mass* &
       !fd     (-bdyty(ibdyty)%mass*(bdyty(ibdyty)%V-bdyty(ibdyty)%Vbegin)+H*(bdyty(ibdyty)%Fext+bdyty(ibdyty)%Fint))

       bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vbegin + &
            (bdyty(ibdyty)%inv_mass*H*(bdyty(ibdyty)%Fext+bdyty(ibdyty)%Fint))

       if ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle

       call apply_vlocy_driven_dof(ibdyty,iVfree)

       if (bdyty(ibdyty)%nb_vlocy_driven_dof == 3) call set_clamped_status_ENTITY(get_entity_RBDY2(ibdyty))


    end do
    !$OMP END DO
    !$OMP END PARALLEL

  end subroutine comp_free_vlocy_RBDY2
!!!------------------------------------------------------------------------  
!!!------------------------------------------------------------------------  
  subroutine comp_free_vlocy_one_RBDY2(ibdyty)

    implicit none

    integer, intent(in) :: ibdyty

    ! todo: Voir si cette routine peut etre remontee dans comp_free_vlocy_RBDY2
    
    if(bdyty(ibdyty)%visible) then
       bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vbegin + &
            (bdyty(ibdyty)%inv_mass*H*(bdyty(ibdyty)%Fext+bdyty(ibdyty)%Fint))

       if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) & 
          call apply_vlocy_driven_dof(ibdyty,iVfree)
    end if

  end subroutine comp_free_vlocy_one_RBDY2
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------  
  subroutine comp_MVfree_one_RBDY2(ibdyty,Rfree)

    implicit none

    integer(kind=4), intent(in) :: ibdyty
    real(kind=8), dimension(3), intent(out) :: Rfree

    Rfree(1:3) = bdyty(ibdyty)%mass(1:3) * bdyty(ibdyty)%Vfree(1:3)

  end subroutine comp_MVfree_one_RBDY2
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  subroutine comp_dof_RBDY2

    implicit none 
    integer :: ibdyty
    real(kind=8),dimension(3) :: corr,acce
   
    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    

          if( .not. bdyty(ibdyty)%visible) cycle

          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%Ireac

          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + & 
                            H*(((1.d0-THETA)*bdyty(ibdyty)%Vbegin) + (THETA*bdyty(ibdyty)%V)) 

          if ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle

          call apply_vlocy_driven_dof(ibdyty,iV____)
       
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_GEAR)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty, acce, corr)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
          
          if(.not.bdyty(ibdyty)%visible) cycle

!fd          acce = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac/H+bdyty(ibdyty)%Fext)
          acce = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac+bdyty(ibdyty)%Fext)
          corr = acce - bdyty(ibdyty)%A

          bdyty(ibdyty)%X=bdyty(ibdyty)%X + cr*corr 
          bdyty(ibdyty)%V=bdyty(ibdyty)%V + cv*corr
          bdyty(ibdyty)%A=acce
          bdyty(ibdyty)%B=bdyty(ibdyty)%B + cb*corr
          bdyty(ibdyty)%C=bdyty(ibdyty)%C + cc*corr
          
          if ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle
          
          call apply_vlocy_driven_dof(ibdyty,iV____)

       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_VERLET)    

!       print*,'---------------'
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2
          
          if(.not.bdyty(ibdyty)%visible) cycle
          
!fd pourquoi diviser par H si c'est une force !?        
!fd bdyty(ibdyty)%A = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac/H+bdyty(ibdyty)%Fext)

!          print *,'Corps',ibdyty
!          print *,'Reac ',bdyty(ibdyty)%Ireac
!          print *,'Fext ',bdyty(ibdyty)%Fext
!          print *,'imas ',bdyty(ibdyty)%inv_mass



          bdyty(ibdyty)%A = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac+bdyty(ibdyty)%Fext)
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin + 0.5*H*(bdyty(ibdyty)%A+bdyty(ibdyty)%Abegin)
          
!          print*,'A     ',bdyty(ibdyty)%A

          if ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle
       
          call apply_vlocy_driven_dof(ibdyty,iV____)
       
       end do
       !$OMP END DO
       !$OMP END PARALLEL
!       print*,'------------------'
    
    case DEFAULT

       call faterr('RBDY2::comp_dof','INTEGRATOR NOT SUPPORTED YET!')
       
    end select

    if (PERIODIC) call check_periodic
    if (BOUNDS)  call out_of_bounds_RBDY2

  end subroutine comp_dof_RBDY2
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  subroutine comp_dof_one_RBDY2(ibdyty)

    implicit none 
    integer, intent(in) :: ibdyty
    real(kind=8),dimension(3) :: corr,acce
    
    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)

       !print*, 'Reac(', ibdyty, ')=', bdyty(ibdyty)%Ireac
       !print*, 'Raux(', ibdyty, ')=', bdyty(ibdyty)%Iaux
 
       if ( bdyty(ibdyty)%visible) then

          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%Ireac

          !fd trop dangereux effet de bord
          ! bdyty(ibdyty)%X = bdyty(ibdyty)%X + THETA*H*bdyty(ibdyty)%V 
          ! je remets comme avant

          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + & 
                            H*(((1.d0-THETA)*bdyty(ibdyty)%Vbegin) + (THETA*bdyty(ibdyty)%V)) 

          if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
             call apply_vlocy_driven_dof(ibdyty,iV____)
       end if

       !print*, 'V   (', ibdyty, ')=', bdyty(ibdyty)%V
       !print*, 'Vaux(', ibdyty, ')=', bdyty(ibdyty)%Vaux

    case(INTEGRATOR_GEAR)
          
       if ( bdyty(ibdyty)%visible ) then

!fd          acce = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac/H+bdyty(ibdyty)%Fext)
          acce = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac+bdyty(ibdyty)%Fext)
          corr = acce - bdyty(ibdyty)%A

          bdyty(ibdyty)%X=bdyty(ibdyty)%X + cr*corr 
          bdyty(ibdyty)%V=bdyty(ibdyty)%V + cv*corr
          bdyty(ibdyty)%A=acce
          bdyty(ibdyty)%B=bdyty(ibdyty)%B + cb*corr
          bdyty(ibdyty)%C=bdyty(ibdyty)%C + cc*corr
       
          if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
             call apply_vlocy_driven_dof(ibdyty,iV____)
       end if

    case(INTEGRATOR_VERLET)    

!       print*,'---------------'
          
       if (bdyty(ibdyty)%visible) then
          
!fd pourquoi diviser par H si c'est une force !?        
!fd bdyty(ibdyty)%A = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac/H+bdyty(ibdyty)%Fext)

!          print *,'Corps',ibdyty
!          print *,'Reac ',bdyty(ibdyty)%Ireac
!          print *,'Fext ',bdyty(ibdyty)%Fext
!          print *,'imas ',bdyty(ibdyty)%inv_mass

          bdyty(ibdyty)%A = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac+bdyty(ibdyty)%Fext)
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin + 0.5*H*(bdyty(ibdyty)%A+bdyty(ibdyty)%Abegin)
          
!          print*,'A     ',bdyty(ibdyty)%A

          if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
             call apply_vlocy_driven_dof(ibdyty,iV____)
       end if
!       print*,'------------------'
    
    case DEFAULT

       call faterr('RBDY2::comp_dof_one','INTEGRATOR NOT SUPPORTED YET!')
       
    end select

    if (PERIODIC) call check_periodic_one_RBDY2(ibdyty)
    if (BOUNDS)  call out_of_bounds_one_RBDY2(ibdyty)

  end subroutine comp_dof_one_RBDY2
!!!------------------------------------------------------------------------  
  subroutine comp_V_RBDY2

    implicit none 
    integer :: ibdyty
                             !1234567890123
    character(len=13) :: IAM='RBDY2::comp_V'
   
    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    

          if( .not. bdyty(ibdyty)%visible) cycle

          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%Ireac

          if ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle

          call apply_vlocy_driven_dof(ibdyty,iV____)
       
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    
    case DEFAULT

       call FATERR(IAM, 'INTEGRATOR NOT SUPPORTED YET!')
       
    end select

  end subroutine comp_V_RBDY2
!!!------------------------------------------------------------------------
  subroutine comp_X_RBDY2

    implicit none 
    integer :: ibdyty
                             !1234567890123
    character(len=13) :: IAM='RBDY2::comp_X'
 
    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    

          if( .not. bdyty(ibdyty)%visible) cycle

          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + & 
                            H*(((1.d0-THETA)*bdyty(ibdyty)%Vbegin) + (THETA*bdyty(ibdyty)%V)) 

       end do
       !$OMP END DO
       !$OMP END PARALLEL
    
    case DEFAULT

       call FATERR(IAM, 'INTEGRATOR NOT SUPPORTED YET!')
       
    end select

    if (PERIODIC) call check_periodic
    if (BOUNDS)  call out_of_bounds_RBDY2

  end subroutine comp_X_RBDY2
!!!------------------------------------------------------------------------  
  subroutine check_periodic

    implicit none
    integer :: ibdyty

    do ibdyty=1,nb_RBDY2
       bdyty(ibdyty)%periode = 0
       if(.not.bdyty(ibdyty)%visible) cycle
       if( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) > periode )then
          bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) - periode
          bdyty(ibdyty)%periode = 1
       else if( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) < 0.D0 ) then
          bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) + periode
          bdyty(ibdyty)%periode =-1
       end if
    end do
    
  end subroutine check_periodic
!!!------------------------------------------------------------------------  
!!!------------------------------------------------------------------------  
  subroutine check_periodic_one_RBDY2(ibdyty)

    implicit none
    integer, intent(in) :: ibdyty

    bdyty(ibdyty)%periode = 0
    if (bdyty(ibdyty)%visible) then
       if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) > periode )then
          bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) - periode
          bdyty(ibdyty)%periode = 1
       else if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) < 0.D0 ) then
          bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) + periode
          bdyty(ibdyty)%periode =-1
       end if
    end if
    
  end subroutine check_periodic_one_RBDY2
!!!------------------------------------------------------------------------  
  subroutine apply_vlocy_driven_dof( ibdyty, storage )

    implicit none 

    integer( kind = 4 ) :: ibdyty
    integer             :: storage

                                   !123456789012345678901234567890123
    character( len = 33 ) :: IAM = 'mod_RBDY2::apply_vlocy_driven_dof'
    integer               :: ivd
    integer               :: iccdof

    ! computing driven dof
    
    select case( storage )
    case( iV____ )
      do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof

        if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

        iccdof = dofnb_of_a_driven_dof( bdyty( ibdyty )%vlocy_driven_dof( ivd ) )

        bdyty( ibdyty )%V( iccdof ) = bdyty( ibdyty )%Vdriv( ivd )
        bdyty( ibdyty )%X( iccdof ) = bdyty( ibdyty )%Xdriv( ivd )
      end do

    case( iVfree )
      do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof

        if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle
          
        iccdof = dofnb_of_a_driven_dof( bdyty( ibdyty )%vlocy_driven_dof( ivd ) )

        bdyty( ibdyty )%Vfree( iccdof ) = bdyty(ibdyty)%Vdriv( ivd )
      end do

    case default
      call FATERR( IAM, 'error vlocy type not known in mod_RBDY2' )

    end select

  end subroutine apply_vlocy_driven_dof
!!!------------------------------------------------------------------------   
  subroutine nullify_vlocy_driven_dof(ibdyty,storage)

    implicit none 

    integer( kind = 4 ) :: ibdyty
    integer             :: storage

                                   !12345678901234567890123456789012345
    character( len = 35 ) :: IAM = 'mod_RBDY2::nullify_vlocy_driven_dof'
    integer               :: ivd,iccdof

    select case( storage )
    case ( iV____ )
      ! nullifying V for velocity driven dof
      do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof

        if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

        iccdof = dofnb_of_a_driven_dof( bdyty( ibdyty )%vlocy_driven_dof( ivd ) )

        bdyty( ibdyty )%V( iccdof ) = 0.D0
      end do

    case ( iVaux_ )
      ! nullifying Vaux for velocity driven dof
      do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof

        if ( .not. bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active ) cycle

        iccdof = dofnb_of_a_driven_dof( bdyty( ibdyty )%vlocy_driven_dof( ivd ) )

        bdyty( ibdyty )%Vaux( iccdof ) = 0.D0
      end do

    case default
      call FATERR( IAM, 'error nullifying vlocy for a driven dof in mod_RBDY2' )

    end select

  end subroutine nullify_vlocy_driven_dof
!!!------------------------------------------------------------------------  
!vv: On peut sans doutes (comme en 3D) faire rentrer ces case(s) dans 
!    nullify_vlocy_driven_dof. J'ai modifie le nom de la fonction
!    (de "nullify_reac_driven_dof" a "nullify_reac_of_vlocy_driven_dof_RBDY2")
!    pour qu'il soit plus explicite (=> of_vlocy_) et pour l'appel (=> _RBDY2) 
  subroutine nullify_reac_of_vlocy_driven_dof_RBDY2(ibdyty,storage)

    implicit none 

    integer           :: ivd,iccdof
    integer           :: storage
    character(len=34) :: IAM='mod_RBDY2::nullify_reac_driven_dof'
    integer(kind=4)   :: ibdyty

    select case(storage)
    case (iIreac)
       ! nullifying Reac for velocity driven dof  
       do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
          iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%Ireac(iccdof)=0.D0   
       end do
    case (iIaux_)
       ! nullifying Raux for velocity driven dof  
       do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
          iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%Iaux(iccdof)=0.D0   
       end do
    case default
       call FATERR(IAM,'error nullifying reac for a driven dof in mod_RBDY2')
    end select
    
  end subroutine nullify_reac_of_vlocy_driven_dof_RBDY2
!!!------------------------------------------------------------------------  
  function itest_bdyty(clin)
    
    implicit none

    character(len=103) :: clin
    integer            :: itest_bdyty
    
    if (clin(2:6) == 'RBDY2') then
       itest_bdyty = ifound
    else
       itest_bdyty = isskip
    end if
    
  end function itest_bdyty
!!!------------------------------------------------------------------------
  function itest_blmty(clin,ibdyty)
    
    implicit none

    integer            :: ibdyty    
    character(len=103) :: clin
    integer            :: itest_blmty
    character(len=80)  :: cout 
    character(len=21)  :: IAM='mod_RBDY2::test_blmty'
    
    select case(clin(2:6))
    case('PLAIN','NULLx')
       itest_blmty = ifound
    case('     ')
       itest_blmty = isskip
    case('nodty','$$$$$')
       itest_blmty = inomor
    case default
       write(cout,'(A7,A5,A18,I7)')' blmty ',clin(2:6),' unknown in RBDY2 ',ibdyty
       call FATERR(IAM,cout)
    end select
    
  end function itest_blmty
!!!------------------------------------------------------------------------
  function itest_nodty(clin,ibdyty)

    implicit none

    integer            :: ibdyty    
    character(len=103) :: clin
    integer            :: itest_nodty
    character(len=80)  :: cout 
    character(len=22)  :: IAM='mod_RBDY2::itest_nodty'

    if (is_a_nodty(clin(2:6))) then
       itest_nodty = ifound
       return
    end if

    select case(clin(2:6))
    case('     ')
       itest_nodty = isskip
    case('tacty','$$$$$')
       itest_nodty = inomor
    case default
       write(cout,'(A7,A5,A18,I7)')' nodty ',clin(2:6),' unknown in RBDY2 ',ibdyty
       call FATERR(IAM,cout)
    end select

  end function itest_nodty
!!!------------------------------------------------------------------------
  function itest_tacty(clin,ibdyty) 

    implicit none

    integer :: ibdyty
    character(len=103) :: clin
    integer            :: itest_tacty
    character(len=80)  :: cout 
    character(len=22)  :: IAM='mod_RBDY2::itest_tacty'

    select case(clin(2:6))
    case('DISKx','xKSID','JONCx','POLYG','PT2Dx','DISKb')
       itest_tacty = ifound
    case('     ') 
       itest_tacty = isskip
    case('$$$$$')
       itest_tacty = inomor
    case default
       write(cout,'(A7,A5,A18,I7)')' tacty ',clin(2:6),' unknown in RBDY2 ',ibdyty
       call FATERR(IAM,cout)
    end select

  end function itest_tacty
!!!------------------------------------------------------------------------
  integer function get_periode(ibdyty)
    
    implicit none
    
    integer,intent(in)    :: ibdyty
    
    get_periode = bdyty(ibdyty)%periode
    
  end function get_periode
!!!------------------------------------------------------------------------ 
  subroutine comp_mass_one_body(ibdyty)
    implicit none
    integer(KIND=4), intent(IN) :: ibdyty
    !
    integer      :: iblmty,itacty,ibehav,iccdof,ivd,i
    real(kind=8) :: Ummass,mass1,mass2,mass3,mass4,avr_radiuss,gyr_radiuss,radius0

    character(len=80)  :: cout 

    do iblmty=1,size(bdyty(ibdyty)%blmty)
       ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb
       select case(bdyty(ibdyty)%blmty(iblmty)%blmID)
       case('PLAIN')
          Ummass=get_rho(ibehav)
          avr_radiuss=bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius
          gyr_radiuss=bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius
       case default
          call LOGMES('you try to compute the mass of an unknown blmty')
       end select
    end do


    mass1=Ummass*avr_radiuss*avr_radiuss*PI_g
    mass2=mass1
    mass3=mass1*gyr_radiuss*gyr_radiuss


    do iccdof=1,size(bdyty(ibdyty)%V)

       ! computing ordinary rigid body masses
       if (iccdof == 1) bdyty(ibdyty)%mass(iccdof)=mass1
       if (iccdof == 2) bdyty(ibdyty)%mass(iccdof)=mass2
       if (iccdof == 3) bdyty(ibdyty)%mass(iccdof)=mass3
       ! computing extra degrees of freedom masses
       if (iccdof == 4) then
          select case(get_nodID(bdyty(ibdyty)%nodty))
          case(i_NO4xx)
             !mj NO4Px, pneumatic node.
             !
             !mj J'etais parti pour, "The definition of the fourth degree of freedom X(4), the radius deformation is:
             !mj   X(4)=(radius-radius0)/radius0, 
             !mj where radius is the radius of the contactor in the actual configuration 
             !mj and radius0 the radius in the reference configuration."
             !mj
             !mj J'ai change d'avis. 
             !mj
             !mj Dans le cas precedent, j'ai essaye, cela fonctionne. Mais la variable duale est un peu bidon 
             !mj si on veut que le produit de la variable primale et de la variable duale soit un travail.
             !mj C'est un moment, ou une pression multipliee par deux fois le volume du contacteur. 
             !mj Un autre choix possible est de prendre comme variable primale le volume enferme 
             !mj par le contacteur et comme variable duale la pression exercee sur le contacteur. Ainsi 
             !mj volume x pression = travail. L'inconvenient de ce choix est que l'on traine partout des
             !mj constantes et que l'on s'empetre dans l'interpretation des donnees et des resulats,
             !mj car il faut reconstituer le rayon a partir du volume, ce qui en soit est elementaire,
             !mj mais un peu acrobatique.
             !mj
             !mj Dans ma version ancienne, le rayon etait choisi comme variable irenterne et bdry_radius n'etait pas lu.
             !mj
             !mj J'en arrive a mon dernier choix. Je choisis comme variable primale (variable interne)
             !mj la difference entre le rayon (variable) et bdry_radius (fixe). Ainsi
             !mj cooref(4) est l'ecart par rapport a bdry_radius dans la configuration de reference,
             !mj X(4) est la variation depuis cooref(4), de sorte que le rayon (variable) du contacteur est
             !mj bdry_radius+cooref(4), dans la configuration de reference,
             !mj bdry_radius+cooref(4)+X(4)=bdry_radius+coor(4), dans la configuration actuelle.
             !mj La variable duale est une force, pression divisee par le rayon.
             !mj Par rapport au modele ancien utilise au Cemagref, il y a tres peu de choses a modifier 
             !mj dans BODIES.DAT ET DOF.INI.
             !mj Dans DRV_DOF.DAT, on impose une force, ce qui n'est pas exactement la meme chose qu'imposer 
             !mj une pression.

             !
             ! The fourth degree of freedom defines a variable radius (an internal variable) in the following way:
             ! bdry_radius is a fixed data, some radius of the dispx.
             ! The radius in the reference configuration is defined by
             !   radius0=cooref(4)+bdry_radius
             ! The radius in the actual configuration is defined by
             !   radius=cooref(4)+X(4)+bdry_radius=coor(4)+X(4)+bdry_radius
             ! The chosen dynamic law for the extra ddl is:
             !   V(4)-Vbegin(4)=mass4*H*F,
             ! where F stands for the sum of normal applied forces on the boundary, a pressure if multiplied by 
             ! 2*3.14159*radius*Lu, (Lu unit length). Beware, imposing a force on the internal variable, 
             ! (change in radius) is not exactly the same than imposing a pressure on the boundary. 
             ! mass4 should be some mass concentrated on the boundary for example,
             !   mass1=mass2, or
             !   mass3/(radius*radius), or
             !   mass3/(radius0*radius0), or
             !   mass3/(bdry_radius*bdry_radius), or
             !   mass3/(gyr_radiuss*gyr_radiuss)=mass1=mass2
             ! where mass1=mass2, is the masss of the body to which is attached the contactor, mass3
             ! is the inertia moment of the body.
             ! In these circumstances,it will be clever to choose bdary_rad equal to the gyration radius.
             ! Our final choice is,
             !   mass4=mass3/(gyr_radiuss*gyr_radiuss), so that with a boundary radius equal to the 
             ! gyration radius,
             !   mass4=mass1=mass2.
             !       
             ! Though it might be possible to come into complications, it is recommanded that when 
             ! using contactors DISPx or xPSID attached to a body ibdyty, none other contactor
             ! should be attached to this body.

             if (size(bdyty(ibdyty)%tacty) .gt. 1) then
                call logmes('DISPx or xPSID and some other contactors are attached to a same body.')
                call logmes('Extension not supported, DISPx or xPSID should be unique boundary of body.')
                write(cout,'(A15,I7)')'Faulty body is ',ibdyty
                call faterr('RBDY2::comp_mass_one_body',cout)
             end if
             mass4=mass1
             bdyty(ibdyty)%mass(iccdof)=mass4 
          case default
             call LOGMES('you try to compute the extra mass of an unknown nodty')
          end select
       end if

       if (bdyty(ibdyty)%mass(iccdof) > 1.D-20) then
          bdyty(ibdyty)%inv_mass(iccdof)=1.D0/bdyty(ibdyty)%mass(iccdof)
       else
          do i=1, size(bdyty(ibdyty)%tacty)
             if (bdyty(ibdyty)%tacty(i)%tacID/='PT2Dx') then  
                call LOGMES('WARNING: Very small mass term')
                write(cout,'(A6,1X,I5,A5,1X,I5,A6,1X,D14.7)') 'rbdy2: ',ibdyty,' ddl:',iccdof,' mass:',bdyty(ibdyty)%mass(iccdof)
                call LOGMES(cout)
                call LOGMES('arbitrary value is taken for inv_mass (1.d+20)')
             end if
          end do
          bdyty(ibdyty)%inv_mass(iccdof) = 1.D20

       endif

    end do

    if (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) return

    ! nullifying inv_mass where degrees of freedom are driven
    do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
       iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
       bdyty(ibdyty)%inv_mass(iccdof)=0.D0
    end do

  end subroutine comp_mass_one_body

  subroutine comp_mass_RBDY2
    implicit none
    integer :: ibdyty

    do ibdyty = 1, nb_RBDY2
      call comp_mass_one_body(ibdyty)
    end do

  end subroutine comp_mass_RBDY2

!!!------------------------------------------------------------------------ 
  subroutine comp_Fext_RBDY2

    implicit none

    integer      :: ifd,ibdyty,iccdof
    real(kind=8) :: Febegin,Fe

    if (nb_RBDY2 == 0) return

    select case(M_INTEGRATOR_ID)

    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,iccdof,ifd,Febegin,Fe)

       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    
          ! initializing
          bdyty(ibdyty)%Fext=0.D0
          ! gravity forces 
          do iccdof=1,size(bdyty(ibdyty)%V)
             if (iccdof == 1) bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%mass(1)*grav1
             if (iccdof == 2) bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%mass(2)*grav2
          end do
          ! driven forces
          do ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             
             call comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)
             
             bdyty(ibdyty)%Fdriv(ifd)=(1.D0-THETA)*Febegin+THETA*Fe     
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))
             
             bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+bdyty(ibdyty)%Fdriv(ifd) 

          end do
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_GEAR)  
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,iccdof,ifd,Febegin,Fe)

       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    
          ! initializing
          bdyty(ibdyty)%Fext=0.D0
          ! gravity forces 
          do iccdof=1,size(bdyty(ibdyty)%V)
             if (iccdof == 1) bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%mass(1)*grav1
             if (iccdof == 2) bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%mass(2)*grav2
          end do
          ! driven forces
          do ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             
             call comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)
             
             bdyty(ibdyty)%Fdriv(ifd) = 0.5*(Febegin+Fe) 
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))
             
             bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+bdyty(ibdyty)%Fdriv(ifd) 
             
          end do
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_VERLET)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,iccdof,ifd,Febegin,Fe)

       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    
          ! initializing
          bdyty(ibdyty)%Fext=0.D0
          ! gravity forces 
          do iccdof=1,size(bdyty(ibdyty)%V)
             if (iccdof == 1) bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%mass(1)*grav1
             if (iccdof == 2) bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%mass(2)*grav2
          end do
          ! driven forces
          do ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             
             call comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)
             
!bof pourquoi moyenne ?
             bdyty(ibdyty)%Fdriv(ifd) = 0.5*(Febegin+Fe) 
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))
             
             bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+bdyty(ibdyty)%Fdriv(ifd) 
             
          end do
       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case default

       call faterr('RBDY2::comp_Fext','Integrator not supported')

    end select

  end subroutine comp_Fext_RBDY2
!!!------------------------------------------------------------------------  
  subroutine comp_Fint_RBDY2

    implicit none

    integer :: ibdyty


    if (nb_RBDY2 == 0) return

    do ibdyty=1,nb_RBDY2
       bdyty(ibdyty)%Fint=0.d0
    end do

  end subroutine comp_Fint_RBDY2
!!!------------------------------------------------------------------------
  subroutine read_PLAIN(rbody,iblmty)

    implicit none

    character(len=103) ::  clin
    integer            ::  iblmty
    type(T_RBDY2)      :: rbody

    read(G_clin( 2: 6),'(A5)')    rbody%blmty(iblmty)%blmID
    read(G_clin(23:27),'(A5)')    rbody%blmty(iblmty)%behav
    read(G_clin(35:48),'(D14.7)') rbody%blmty(iblmty)%PLAIN%avr_radius 
    read(G_clin(56:69),'(D14.7)') rbody%blmty(iblmty)%PLAIN%gyr_radius

    nullify(rbody%blmty(iblmty)%PLAIN%inc_dilat,rbody%blmty(iblmty)%PLAIN%dilat,rbody%blmty(iblmty)%PLAIN%cooref)

  end subroutine read_PLAIN
!!!------------------------------------------------------------------------
  subroutine write_PLAIN(nfich,rbody,iblmty)

    implicit none

    integer       :: iblmty,nfich
    type(T_RBDY2) :: rbody

    write(nfich,102)rbody%blmty(iblmty)%blmID,iblmty,      &
         'behav',rbody%blmty(iblmty)%behav,             &
         'avrd=',rbody%blmty(iblmty)%PLAIN%avr_radius,  &
         'gyrd=',rbody%blmty(iblmty)%PLAIN%gyr_radius
    
102 format(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
    
  end subroutine write_PLAIN
!!!------------------------------------------------------------------------
  subroutine write_PLAIN_cleared(nfich,rbody,iblmty)

    implicit none

    integer       :: iblmty,nfich
    type(T_RBDY2) :: rbody

    write(nfich,102)rbody%blmty(iblmty)%blmID,iblmty,      &
         'behav',rbody%blmty(iblmty)%behav,             &
         'avrd=',0.d0,  &
         'gyrd=',0.d0

102 format(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))

  end subroutine write_PLAIN_cleared
!!!------------------------------------------------------------------------
  subroutine read_NULLx(rbody,iblmty)

    implicit none

    character(len=103) ::  clin
    integer            ::  iblmty
    type(T_RBDY2)      :: rbody
    
    rbody%blmty(iblmty)%blmID='NULLx'
    
  end subroutine read_NULLx
!!!------------------------------------------------------------------------
  subroutine write_NULLx(nfich,rbody,iblmty)

    implicit none

    integer       ::  nfich,iblmty
    type(T_RBDY2) :: rbody

    write(nfich,'(1x,A5)') 'NULLx'
    
  end subroutine write_NULLx
!!!------------------------------------------------------------------------
!!! PUBLIC ....
!!!------------------------------------------------------------------------ 
  subroutine add_reac(ibdyty,xxccdof,xxreac,storage)

    !
    ! called by injj 
    !

    integer            :: ibdyty,iccdof
    integer            :: storage
    character(len=19)  :: IAM='mod_RBDY2::add_reac'
    
    integer,     dimension(size(bdyty(ibdyty)%Ireac)) :: xxccdof
    real(kind=8),dimension(size(bdyty(ibdyty)%Ireac)) :: xxreac

    select case(storage)
    case (iIreac)

!       print*,'corps',ibdyty
!       print*,'avant',bdyty(ibdyty)%Ireac
       
       do iccdof=1,size(bdyty(ibdyty)%Ireac)
          bdyty(ibdyty)%Ireac(xxccdof(iccdof))=  &
               bdyty(ibdyty)%Ireac(xxccdof(iccdof))+xxreac(xxccdof(iccdof))
       end do

!       print*,'apres',bdyty(ibdyty)%Ireac
!       print*,'&&&&&'

    case (iIaux_)
       do iccdof=1,size(bdyty(ibdyty)%Iaux)
          bdyty(ibdyty)%Iaux(xxccdof(iccdof))=   &
               bdyty(ibdyty)%Iaux(xxccdof(iccdof))+xxreac(xxccdof(iccdof))
       end do

    !vv: pour la DDM enrichie version P.A.
    case (iVaux_)
       do iccdof=1,size(bdyty(ibdyty)%Vaux)
          bdyty(ibdyty)%Vaux(xxccdof(iccdof))=   &
               bdyty(ibdyty)%Vaux(xxccdof(iccdof))+xxreac(xxccdof(iccdof))
       end do
    case default
       call FATERR(IAM,'error adding reac')
    end select

  end subroutine add_reac
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  subroutine nullify_reac(ibdyty,storage)

    !
    ! called by vitrad
    !

    implicit none 
    integer :: ibdyty,iccdof
    integer :: storage
    !12345678901234567890123
    character(len=23)  :: IAM='mod_RBDY2::nullify_reac'

    select case(storage)
    case (iIreac)
       do iccdof=1,size(bdyty(ibdyty)%Ireac)
          bdyty(ibdyty)%Ireac(iccdof)=0.d0
       end do
    case (iIaux_)
       do iccdof=1,size(bdyty(ibdyty)%Iaux)
          bdyty(ibdyty)%Iaux(iccdof)=0.d0
       end do
    case default
       call FATERR(IAM,'error nullifying reac')
    end select

  end subroutine nullify_reac
!!!------------------------------------------------------------------------ 
  subroutine comp_vlocy(ibdyty,storage)

    !
    ! called by vitrad
    !
    !am: also called by DDM modules

    implicit none 
    integer :: ibdyty,iccdof
    integer :: storage
    !123456789012345678901
    character(len=21)  :: IAM='mod_RBDY2::comp_vlocy'
    select case(storage) 
    case ( iV____e_invM_t_Ireac )
       do iccdof=1,size(bdyty(ibdyty)%V)
          bdyty(ibdyty)%V(iccdof)=bdyty(ibdyty)%Ireac(iccdof)*bdyty(ibdyty)%inv_mass(iccdof)                
       end do
    case ( iVaux_e_invM_t_Ireac )
       do iccdof=1,size(bdyty(ibdyty)%V)
          bdyty(ibdyty)%Vaux(iccdof)=bdyty(ibdyty)%Ireac(iccdof)*bdyty(ibdyty)%inv_mass(iccdof)                
       end do
    case ( iVaux_e_invM_t_Iaux_ )
       do iccdof=1,size(bdyty(ibdyty)%V)
          bdyty(ibdyty)%Vaux(iccdof)=bdyty(ibdyty)%Iaux(iccdof)*bdyty(ibdyty)%inv_mass(iccdof)               
       end do
    case ( iVaux_e_invM_t_Iaux_p_Vfree )
       ! compute : Vaux = M^-1 Raux
       do iccdof=1,size(bdyty(ibdyty)%V)
          bdyty(ibdyty)%Vaux(iccdof)=bdyty(ibdyty)%Iaux(iccdof)*bdyty(ibdyty)%inv_mass(iccdof)               
       end do

       ! nullify velocity driven dof for Vaux
       if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
          call nullify_vlocy_driven_dof(ibdyty, iVaux_)

       ! compute : Vaux = Vaux + Vfree
       ! N.B. velocity driven dof are already applied to Vfree => Vaux = Vdriv
       bdyty(ibdyty)%Vaux = bdyty(ibdyty)%Vaux + bdyty(ibdyty)%Vfree
    case default
       call FATERR(IAM,'error computing velocy')
    end select

  end subroutine comp_vlocy
!!!------------------------------------------------------------------------ 
  subroutine nullify_vlocy(ibdyty,storage)

    !
    ! called SDL solver
    !

    implicit none 
    integer :: ibdyty,iccdof
    integer :: storage
    !123456789012345678901234
    character(len=24)  :: IAM='mod_RBDY2::nullify_vlocy'

    select case(storage)
    case (iVaux_)
       do iccdof=1,size(bdyty(ibdyty)%Vaux)
          bdyty(ibdyty)%Vaux(iccdof)=0.D0
       end do
    case default
       call FATERR(IAM,'error nullifying vlocy')
    end select

  end subroutine nullify_vlocy
!!!------------------------------------------------------------------------ 
  subroutine init_source_point_RBDY2(nbfirst,radius,Xshift,Yshift)

    implicit none

    integer      :: i,nbfirst,itacty
    real(kind=8) :: radius,Xshift,Yshift

    itacty      = 1
    sp_radius   = radius
    first_RBDY2 = nbfirst
    sp_shift_x  = Xshift
    sp_shift_y  = Yshift

    do i=1,first_RBDY2
       bdyty(i)%visible = .true. 
    end do

    nb_falling_RBDY2 = first_RBDY2

    bdyty(nb_falling_RBDY2)%Xbegin(1) = sp_shift_x
    bdyty(nb_falling_RBDY2)%Xbegin(2) = sp_shift_y

    do i = first_RBDY2+1,nb_RBDY2
       if ( (bdyty(i)%tacty(itacty)%tacID == 'DISKx').or. &
            (bdyty(i)%tacty(itacty)%tacID == 'POLYG'))then
          
          if(bdyty(i)%tacty(itacty)%color=='BASEx') then 
             bdyty(i)%visible=.true.
             cycle
          end if

          bdyty(i)%visible=.false.

          bdyty(i)%Vbegin(1:3) = 0.D0
          bdyty(i)%V(1:3)      = 0.D0
          bdyty(i)%Xbegin(1:2) = 0.D0
          bdyty(i)%X(1:2)      = 0.D0
          
       end if
    end do

  end subroutine init_source_point_RBDY2
!!!----------------------------------------------------------
  subroutine check_source_point_RBDY2

    implicit none

    character(len=5)          :: color,tacID
    integer                   :: ibdy,itacty,iblmty=1
    real(kind=8)              :: dist,rayon
    character(len=30)         :: cout

!    print*,"==================="
!    print*,nb_falling_RBDY2,nb_RBDY2


    if (nb_falling_RBDY2 .ge. nb_RBDY2) return

    itacty = 1

    if (nb_falling_RBDY2 + 1 < nb_RBDY2) then
       
       tacID=bdyty(nb_falling_RBDY2+1)%tacty(itacty)%tacID

       if ( (tacID .ne.'JONCx') .and. (tacID .ne. 'xKSID') .and. (tacID .ne. 'xPSID') ) then

!          print*,bdyty(nb_falling_RBDY2)%X

          dist = dsqrt( &
               ((bdyty(nb_falling_RBDY2)%X(1)-sp_shift_x)**2) + &
               ((bdyty(nb_falling_RBDY2)%X(2)-sp_shift_y)**2) )
       
          rayon = 0.D0 

          if (bdyty(nb_falling_RBDY2)%tacty(itacty)%tacID=='DISKx') &
               rayon = bdyty(nb_falling_RBDY2)%blmty(iblmty)%PLAIN%avr_radius

          if (bdyty(nb_falling_RBDY2+1)%tacty(itacty)%tacID=='DISKx') &
               rayon = rayon + bdyty(nb_falling_RBDY2+1)%blmty(iblmty)%PLAIN%avr_radius
          
          if (bdyty(nb_falling_RBDY2)%tacty(itacty)%tacID=='POLYG') &
               rayon = bdyty(nb_falling_RBDY2)%blmty(iblmty)%PLAIN%avr_radius
          
          if (bdyty(nb_falling_RBDY2+1)%tacty(itacty)%tacID=='POLYG') &
               rayon = rayon + bdyty(nb_falling_RBDY2+1)%blmty(iblmty)%PLAIN%avr_radius
          
!          print*,'rayon: ',rayon,' dist: ',dist

          if (dist > rayon) then
!             IF (nb_falling_RBDY2+1<nb_RBDY2) THEN
                nb_falling_RBDY2 = nb_falling_RBDY2 + 1
                bdyty(nb_falling_RBDY2)%visible=.true.
!!
                bdyty(nb_falling_RBDY2)%Xbegin(1) = sp_shift_x
                bdyty(nb_falling_RBDY2)%Xbegin(2) = sp_shift_y
!! depuis la v2 on travaille dans X
                bdyty(nb_falling_RBDY2)%X(1) = sp_shift_x
                bdyty(nb_falling_RBDY2)%X(2) = sp_shift_y

!                print*,bdyty(nb_falling_RBDY2)%Xbegin
!                print*,bdyty(nb_falling_RBDY2)%X
!                print*,bdyty(nb_falling_RBDY2)%Vbegin
!                print*,bdyty(nb_falling_RBDY2)%V


!             END IF
             write(cout,'(A24,1X,I5)') ' @ FALLING RBDY2 NUMBER:',nb_falling_RBDY2
             call LOGMES(cout)
          end if
       end if
    end if

  end subroutine check_source_point_RBDY2
!!!------------------------------------------------------------------------   
  subroutine set_init_boundary_RBDY2(ibound,linf)

    implicit none

    integer      :: ibound
    real(kind=8) :: linf

    select case(ibound)
    case(1)
       limit_inf = linf
    case(2)
       limit_sup = linf
    case(3)
       limit_left = linf
    case(4)
       limit_right = linf
    end select

    BOUNDS=.true.

  end subroutine set_init_boundary_RBDY2
!!!------------------------------------------------------------------------   
  subroutine out_of_bounds_RBDY2

    implicit none

    integer :: ibdyty

    do ibdyty=1,nb_RBDY2
       if (.not.bdyty(ibdyty)%visible) cycle
       if ( (bdyty(ibdyty)%X(2)+bdyty(ibdyty)%cooref(2) ) .lt. limit_inf )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(2)    = limit_inf-bdyty(ibdyty)%cooref(2)
          cycle
       end if
       if ( (bdyty(ibdyty)%X(2)+bdyty(ibdyty)%cooref(2) ) .gt. limit_sup )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(2)    = limit_sup-bdyty(ibdyty)%cooref(2)
          cycle
       end if
       if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) .lt. limit_left )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(1)    = limit_left-bdyty(ibdyty)%cooref(1)
          cycle
       end if
       if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) .gt. limit_right )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(1)    = limit_right-bdyty(ibdyty)%cooref(1)
          cycle
       end if
    end do

  end subroutine out_of_bounds_RBDY2
!!!------------------------------------------------------------------------   
!!!------------------------------------------------------------------------   
  subroutine out_of_bounds_one_RBDY2(ibdyty)

    implicit none

    integer, intent(IN) :: ibdyty

    if (bdyty(ibdyty)%visible) then
       if ( (bdyty(ibdyty)%X(2)+bdyty(ibdyty)%cooref(2) ) .lt. limit_inf )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(2)    = limit_inf-bdyty(ibdyty)%cooref(2)
       else if ( (bdyty(ibdyty)%X(2)+bdyty(ibdyty)%cooref(2) ) .gt. limit_sup )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(2)    = limit_sup-bdyty(ibdyty)%cooref(2)
       else if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) .lt. limit_left )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(1)    = limit_left-bdyty(ibdyty)%cooref(1)
       else if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) .gt. limit_right )then
          bdyty(ibdyty)%visible =.false.
          bdyty(ibdyty)%Vbegin  = 0.D0
          bdyty(ibdyty)%V       = 0.D0
          bdyty(ibdyty)%X(1)    = limit_right-bdyty(ibdyty)%cooref(1)
       end if
    end if

  end subroutine out_of_bounds_one_RBDY2
!!!------------------------------------------------------------------------   
!!! PUBLIC RECUP ....
!!!------------------------------------------------------------------------   
  subroutine get_vlocy(ibdyty,storage,vlocy)

    implicit none

    integer :: i
    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%V)),intent(out) :: vlocy
    integer :: storage
    !123456789012345678901
    character(len=20)  :: IAM='mod_RBDY2::get_vlocy'

    select case(storage)
    case( iV____ )
       do i=1,size(bdyty(ibdyty)%V)
          vlocy(i)=bdyty(ibdyty)%V(i)
       end do
    case( iVbeg_ )
       do i=1,size(bdyty(ibdyty)%V)
          vlocy(i)=bdyty(ibdyty)%Vbegin(i)
       end do
    case( iVfree )
       do i=1,size(bdyty(ibdyty)%V)
          vlocy(i)=bdyty(ibdyty)%Vfree(i)
       end do
    case( iVaux_ )
       do i=1,size(bdyty(ibdyty)%V)
          vlocy(i)=bdyty(ibdyty)%Vaux(i)
       end do
    case default
       call FATERR(IAM,'error computing velocy')
    end select

  end subroutine get_vlocy
!!!------------------------------------------------------------------------
  function get_V(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%V)) :: get_V

    get_V = bdyty(ibdyty)%V

  end function get_V
!!!------------------------------------------------------------------------
  function get_visible(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    logical :: get_visible

    get_visible = bdyty(ibdyty)%visible

  end function get_visible
!!!------------------------------------------------------------------------
  function get_r2m(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    logical :: get_r2m

    get_r2m = bdyty(ibdyty)%r2m

  end function get_r2m
!!!------------------------------------------------------------------------
  subroutine put_r2m(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    logical :: r2m

    bdyty(ibdyty)%r2m=.true.

  end subroutine put_r2m
!!!------------------------------------------------------------------------
  function get_Vbegin(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%Vbegin)) :: get_Vbegin

    get_Vbegin = bdyty(ibdyty)%Vbegin

  end function get_Vbegin
!!!------------------------------------------------------------------------
  function get_Vfree(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%Vfree)) :: get_Vfree

    get_Vfree = bdyty(ibdyty)%Vfree

  end function get_Vfree
!!!------------------------------------------------------------------------
  function get_Vaux(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%Vaux)) :: get_Vaux

    get_Vaux = bdyty(ibdyty)%Vaux

  end function get_Vaux
!!!------------------------------------------------------------------------
  function get_Xbegin(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%Xbegin)) :: get_Xbegin

    get_Xbegin = bdyty(ibdyty)%Xbegin 

  end function get_Xbegin
!!!------------------------------------------------------------------------
  function get_X(ibdyty)

    implicit none

    integer,intent(in)    :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%X)) :: get_X

    get_X = bdyty(ibdyty)%X

  end function get_X
!!!------------------------------------------------------------------------
  function get_coor(ibdyty,itacty)

    implicit none

    integer,intent(in)    :: ibdyty,itacty
    real(kind=8),dimension(size(bdyty(ibdyty)%cooref)) :: get_coor

    real(kind=8),dimension(2) :: shift
    real(kind=8) :: a,c,s

    !fd le 2/11/09 a clarifier
    get_coor=bdyty(ibdyty)%cooref+bdyty(ibdyty)%X 
    ! get_coor=bdyty(ibdyty)%coor
       
    if (itacty.ne.0) then

       !fd le 2/11/09 a clarifier
       a =  bdyty(ibdyty)%cooref(3) + bdyty(ibdyty)%X(3)
       ! a =  bdyty(ibdyty)%coor(3)

       c=cos(a);s=sin(a)

       shift(1)=  c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
                - s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
       shift(2)=  s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
                + c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)

       get_coor(1:2) = get_coor(1:2) + shift(1:2)
    end if

  end function get_coor
!!!------------------------------------------------------------------------
  function get_gyr_radius(ibdyty)

    implicit none

    integer,intent(in) :: ibdyty
    real(kind=8)       :: get_gyr_radius

    get_gyr_radius=bdyty(ibdyty)%blmty(1)%PLAIN%gyr_radius

  end function get_gyr_radius
!!!------------------------------------------------------------------------
  function get_avr_radius(ibdyty)

    implicit none

    integer,intent(in) :: ibdyty
    real(kind=8)       :: get_avr_radius

    get_avr_radius=bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius

  end function get_avr_radius
!!!------------------------------------------------------------------------
  real(kind=8) function get_avr_radius_tacty(ibdyty,itacty)

    implicit none
    integer,intent(in) :: ibdyty,itacty

    get_avr_radius_tacty=bdyty(ibdyty)%tacty(itacty)%BDARY%rdg

  end function get_avr_radius_tacty
!!!------------------------------------------------------------------------
  function get_coorTT(ibdyty,itacty)

    ! This subroutine computes predicted coordinates of bodies at time (1.D0-THETA)*H
    ! from the beginning of the time step. They will be used to select prox tactors and
    ! compute gaps, see comments in tactors moduli, mod_DKDKx.f90, mod_DKJCx.f90,... 

    implicit none

    integer,intent(in)    :: ibdyty,itacty
    real(kind=8),dimension(size(bdyty(ibdyty)%cooref)) :: get_coorTT
    real(kind=8) :: a,c,s
    real(kind=8),dimension(2) :: shiftTT

    !fd a clarifier
    get_coorTT =  bdyty(ibdyty)%cooref &
         + bdyty(ibdyty)%Xbegin &
         + (1.D0-THETA)*H*bdyty(ibdyty)%Vbegin
    !get_coorTT =  bdyty(ibdyty)%coor &
    !     + (1.D0-THETA)*H*bdyty(ibdyty)%Vbegin

    if (itacty.ne.0) then
       a =  get_coorTT(3)

       c=cos(a);s=sin(a)

       shiftTT(1) =  c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
            - s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)

       shiftTT(2) =  s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
            + c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
       
       get_coorTT(1) = get_coorTT(1) + shiftTT(1)
       get_coorTT(2) = get_coorTT(2) + shiftTT(2) 

    end if

  end function get_coorTT
!!!------------------------------------------------------------------------
  function get_cooref(ibdyty,itacty)

    implicit none

    integer,intent(in)    :: ibdyty,itacty
    real(kind=8),dimension(size(bdyty(ibdyty)%cooref)) :: get_cooref
    real(kind=8) :: a,c,s
    real(kind=8),dimension(2) :: shift

    get_cooref =  bdyty(ibdyty)%cooref 

    if (itacty.ne.0) then

       a =  get_cooref(3)

       c=cos(a);s=sin(a)

       shift(1) =  c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
            - s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
       shift(2) =  s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
            + c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
       
       get_cooref(1) = get_cooref(1) + shift(1)
       get_cooref(2) = get_cooref(2) + shift(2) 

    end if

  end function get_cooref
!!!------------------------------------------------------------------------
  subroutine put_cooref(ibdyty,XC)

    implicit none

    integer,intent(in)                                 :: ibdyty
    real(kind=8),dimension(size(bdyty(ibdyty)%cooref)) :: XC

    bdyty(ibdyty)%cooref=XC

  end subroutine put_cooref
!!!------------------------------------------------------------------------
  subroutine get_behav(ibdyty,iblmty,behav)

    implicit none

    integer,intent(in)       :: ibdyty,iblmty
    character(len=5)         :: behav

    behav=bdyty(ibdyty)%blmty(iblmty)%behav

  end subroutine get_behav
!!!------------------------------------------------------------------------
  integer function get_bulk_behav_number_RBDY2(ibdyty,iblmty)

    implicit none
    integer,intent(in) :: ibdyty,iblmty

    get_bulk_behav_number_RBDY2 = bdyty(ibdyty)%blmty(iblmty)%lawnb

  end function get_bulk_behav_number_RBDY2
!!!------------------------------------------------------------------------
  subroutine indent_behav(ibdyty,iblmty,ind_color)

    implicit none

    !fd achtung c'est du mjean sert au mailleur de tensegrite
    !mj ya wol, a virer dans un more 

    integer,intent(in)       :: ibdyty,iblmty
    character(len=3)         :: ind_color

    bdyty(ibdyty)%blmty(iblmty)%behav=ind_color//bdyty(ibdyty)%blmty(iblmty)%behav(4:5)

  end subroutine indent_behav
!!!------------------------------------------------------------------------
  subroutine get_data(ibdyty,itacty,data)

    implicit none

    integer,intent(in)    :: ibdyty,itacty
    !mj   real(kind=8),intent(out),dimension(:):: data
    real(kind=8),intent(out),dimension(size(bdyty(ibdyty)%tacty(itacty)%BDARY%data)):: data
    !1234567890123456789012
    character(len=19)  :: IAM='mod_RBDY2::get_data'

    select case(bdyty(ibdyty)%tacty(itacty)%tacID)
    case('DISKx','xKSID','JONCx','POLYG','DISKb')

       !fd  il faudrait verifier que la taille du data en entree est celle stockee !
       !fd  voir avec des pointeurs ...
       !mj  je propose real(kind=8),intent(out),dimension(size(bdyty(ibdyty)%tacty(itacty)%BDARY%data)):: data
       !mj  sinon, ca ne marche pas bien

       data=bdyty(ibdyty)%tacty(itacty)%BDARY%data
    case default  
       call FATERR(IAM,'boundary type unknown in get_data')
    end select

  end subroutine get_data
!!!------------------------------------------------------------------------
  subroutine get_idata(ibdyty,itacty,idata)

    implicit none

    integer,intent(in)                 :: ibdyty,itacty
    integer,intent(inout),dimension(:) :: idata
    character(len=20)  :: IAM='mod_RBDY2::get_idata'

    select case(bdyty(ibdyty)%tacty(itacty)%tacID)
    case('POLYG')
       idata=bdyty(ibdyty)%tacty(itacty)%BDARY%idata
    case default  
       call FATERR(IAM,'boundary type unknown in get_idata')
    end select

  end subroutine get_idata
!!!------------------------------------------------------------------------
  function get_shiftTT(ibdyty,itacty)

    ! This subroutine computes predicted coordinates of bodies at time (1.D0-THETA)*H
    ! from the beginning of the time step. They will be used to select prox tactors and
    ! compute gaps, see comments in tactors moduli, mod_DKDKx.f90, mod_DKJCx.f90,... 

    implicit none

    integer,intent(in)    :: ibdyty,itacty
    real(kind=8),dimension(2) :: get_shiftTT
    real(kind=8) :: a,c,s

    a =  bdyty(ibdyty)%cooref(3) &
         + bdyty(ibdyty)%Xbegin(3) &
         + (1.D0-THETA)*H*bdyty(ibdyty)%Vbegin(3)

    c=cos(a);s=sin(a)

    get_shiftTT(1)=  c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
         - s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
    get_shiftTT(2)=  s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
         + c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)

  end function get_shiftTT
!!!------------------------------------------------------------------------
  integer function get_nb_RBDY2(fantome)

    implicit none

    integer,optional  :: fantome

    get_nb_RBDY2=nb_RBDY2

  end function get_nb_RBDY2
!!!------------------------------------------------------------------------
  character(len=5) function get_bdyID(ibdyty)

    implicit none

    integer  :: ibdyty

    get_bdyID=bdyty(ibdyty)%bdyID

  end function get_bdyID
!!!------------------------------------------------------------------------
  integer function get_nb_tacty(ibdyty)

    implicit none

    integer  :: ibdyty

    get_nb_tacty=size(bdyty(ibdyty)%tacty)

  end function get_nb_tacty
!!!------------------------------------------------------------------------
  character(len=5) function get_tacID(ibdyty,itacty)

    implicit none

    integer  :: ibdyty,itacty

    get_tacID=bdyty(ibdyty)%tacty(itacty)%tacID

  end function get_tacID
!!!------------------------------------------------------------------------
  character(len=5) function get_color(ibdyty,itacty)

    implicit none

    integer  :: ibdyty,itacty

    get_color = bdyty(ibdyty)%tacty(itacty)%color 

  end function get_color
!!!------------------------------------------------------------------------
  subroutine set_color_RBDY2(ibdyty,itacty,color)

    implicit none

    integer  :: ibdyty,itacty
    character(len=5) :: color

    bdyty(ibdyty)%tacty(itacty)%color = color

  end subroutine set_color_RBDY2
!!!------------------------------------------------------------------------
  subroutine indent_color(ibdyty,itacty,ind_color)

    !
    ! fd achtung c'est du mjean sert au mailleur
    !
    implicit none
    integer  :: ibdyty,itacty
    character(len=3) :: ind_color

    bdyty(ibdyty)%tacty(itacty)%color=ind_color//bdyty(ibdyty)%tacty(itacty)%color(4:5) 

  end subroutine indent_color
!!!------------------------------------------------------------------------
  function get_reac(ibdyty)

    implicit none

    integer,intent(in)                    :: ibdyty
    real(kind=8),dimension(3) :: get_reac

    if (smooth_method) then
      get_reac(1:3)=bdyty(ibdyty)%Ireac(1:3)
    else
      get_reac(1:3)=bdyty(ibdyty)%Ireac(1:3)/H    
    endif
  end function get_reac
!!!------------------------------------------------------------------------ 
   function get_Fext(ibdyty)

    implicit none

    integer,intent(in)        :: ibdyty
    real(kind=8),dimension(3) :: get_Fext

    get_Fext = bdyty(ibdyty)%Fext(1:3)

  end function get_Fext
!!!------------------------------------------------------------------------ 
   function get_Fint(ibdyty)

    implicit none

    integer,intent(in)        :: ibdyty
    real(kind=8),dimension(3) :: get_Fint

    get_Fint = bdyty(ibdyty)%Fint(1:3)

  end function get_Fint
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_area(ibdyty)

    implicit none

    integer,intent(in)       :: ibdyty

    get_area = bdyty(ibdyty)%blmty(1)%PLAIN%area 

  end function get_area
!!!!------------------------------------------------------------------------ 
  real(kind=8) function get_area_tacty(ibdyty,itacty)

    implicit none

    integer,intent(in)       :: ibdyty,itacty

    get_area_tacty = bdyty(ibdyty)%tacty(itacty)%BDARY%area

  end function get_area_tacty
!!!------------------------------------------------------------------------
  function get_mass(ibdyty)

    implicit none

    integer       :: ibdyty
    real(kind=8)  :: get_mass

    get_mass=bdyty(ibdyty)%mass(1)

  end function get_mass
!!!------------------------------------------------------------------------
  function get_ptr_mass(ibdyty)

    implicit none

    integer :: ibdyty
    real(kind=8), dimension(:), pointer :: get_ptr_mass

    get_ptr_mass=>bdyty(ibdyty)%mass

  end function get_ptr_mass
!!!------------------------------------------------------------------------
  function get_ptr_fint(ibdyty)

    implicit none

    integer :: ibdyty
    real(kind=8), dimension(:), pointer :: get_ptr_fint

    get_ptr_fint=>bdyty(ibdyty)%fint

  end function get_ptr_fint
!!!------------------------------------------------------------------------
  function get_ptr_fext(ibdyty)

    implicit none

    integer :: ibdyty
    real(kind=8), dimension(:), pointer :: get_ptr_fext

    get_ptr_fext=>bdyty(ibdyty)%fext

  end function get_ptr_fext
!!!------------------------------------------------------------------------
  function get_ptr_vbeg(ibdyty)

    implicit none

    integer :: ibdyty
    real(kind=8), dimension(:), pointer :: get_ptr_vbeg

    get_ptr_vbeg=>bdyty(ibdyty)%Vbegin

  end function get_ptr_vbeg
!!!------------------------------------------------------------------------
  function get_ptr_coor(ibdyty)

    implicit none

    integer :: ibdyty
    real(kind=8), dimension(:), pointer :: get_ptr_coor

    get_ptr_coor=>bdyty(ibdyty)%coor

  end function get_ptr_coor
!!!------------------------------------------------------------------------
  function get_inv_mass(ibdyty)

    implicit none

    integer                   :: ibdyty
    real(kind=8),dimension(3) :: get_inv_mass

    get_inv_mass=bdyty(ibdyty)%inv_mass

  end function get_inv_mass
!!!------------------------------------------------------------------------
  subroutine get_nb_outline(ibdyty,nb_outline)

    implicit none

    integer,intent(in)       :: ibdyty
    integer,intent(out)      :: nb_outline

    nb_outline = size(bdyty(ibdyty)%tacty)

  end subroutine get_nb_outline
!!!------------------------------------------------------------------------
  integer function get_entity_RBDY2(ibdyty)

    implicit none

    integer,intent(in)       :: ibdyty

    get_entity_RBDY2 = nb_existing_entities + ibdyty

  end function get_entity_RBDY2
!!!------------------------------------------------------------------------ 
!!! Multi-physics applications
!!!------------------------------------------------------------------------ 
  real(KIND=8) function get_elec_cond(ibdyty)
    implicit none
    integer,intent(in) :: ibdyty

    get_elec_cond = bdyty(ibdyty)%ECond

  end function get_elec_cond
!!!------------------------------------------------------------------------ 
  real(KIND=8) function get_elec_condini(ibdyty)
    implicit none
    integer,intent(in) :: ibdyty

    get_elec_condini = bdyty(ibdyty)%ECondini

  end function get_elec_condini
!!!------------------------------------------------------------------------ 
  subroutine put_elec_cond(ibdyty,cond)
    implicit none
    integer,intent(in) :: ibdyty
    real(kind=8)       :: cond

    bdyty(ibdyty)%ECond = cond
    
  end subroutine  put_elec_cond
!!!------------------------------------------------------------------------ 
  real(KIND=8) function get_electric_potentiel(ibdyty)
    implicit none
    integer,intent(in) :: ibdyty

    get_electric_potentiel = bdyty(ibdyty)%EPot

  end function get_electric_potentiel
!!!------------------------------------------------------------------------ 
  subroutine  put_electric_potentiel(ibdyty,EPot)
    implicit none
    integer,intent(in)       :: ibdyty
    real(kind=8),intent(in)  :: EPot
    
    bdyty(ibdyty)%EPot = EPot

  end subroutine put_electric_potentiel
!!!------------------------------------------------------------------------ 
  real(KIND=8) function get_electric_current(ibdyty)
    implicit none
    integer,intent(in) :: ibdyty

    get_electric_current = bdyty(ibdyty)%ECur

  end function get_electric_current
!!!------------------------------------------------------------------------ 
  subroutine  put_electric_current(ibdyty,ECur)
    implicit none
    integer,intent(in)       :: ibdyty
    real(kind=8),intent(in)  :: ECur
    
    bdyty(ibdyty)%ECur = ECur

  end subroutine put_electric_current
!!!------------------------------------------------------------------------ 
!!! Multi-physics applications
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_betai(ibdyty,itacty)
    implicit none
    integer(kind=4) :: ibdyty,itacty

    get_betai = bdyty(ibdyty)%tacty(itacty)%BDARY%betai 

  end function get_betai
!!!------------------------------------------------------------------------ 
  subroutine add_betai(ibdyty,itacty,betai)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty
    real(kind=8),intent(in)    :: betai
    !print*,ibdyty,itacty,betai
    bdyty(ibdyty)%tacty(itacty)%BDARY%betai = betai    

  end subroutine add_betai
!!!------------------------------------------------------------------------ 
  integer(kind=4) function get_thermal_ID(ibdyty,itacty)
    implicit none
    integer(kind=4) :: ibdyty,itacty

    get_thermal_ID = bdyty(ibdyty)%tacty(itacty)%BDARY%IdTh

  end function get_thermal_ID
!!!------------------------------------------------------------------------ 
  subroutine put_thermal_ID(ibdyty,itacty,ID)
    implicit none
    integer(kind=4) :: ibdyty,itacty,ID

    bdyty(ibdyty)%tacty(itacty)%BDARY%IdTh = ID

  end subroutine put_thermal_ID
!!!------------------------------------------------------------------------ 
  real(kind=8) function  get_thermal_value(ibdyty,itacty)
    implicit none
    integer(kind=4) :: ibdyty,itacty

    get_thermal_value = bdyty(ibdyty)%tacty(itacty)%BDARY%T

  end function get_thermal_value
!!!------------------------------------------------------------------------ 
  subroutine  put_thermal_value(ibdyty,itacty,T)
    implicit none
    integer(kind=4) :: ibdyty,itacty
    real(kind=8)    :: T

    bdyty(ibdyty)%tacty(itacty)%BDARY%T = T

  end subroutine put_thermal_value
!!!------------------------------------------------------------------------
  real(kind=8) function get_therm_condini(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty

    get_therm_condini = bdyty(ibdyty)%tacty(itacty)%BDARY%TCondini
    
  end function get_therm_condini
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_therm_cond(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty

    get_therm_cond = bdyty(ibdyty)%tacty(itacty)%BDARY%TCond
    
  end function get_therm_cond
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_therm_pcond(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty

    get_therm_pcond = bdyty(ibdyty)%tacty(itacty)%BDARY%PTCond
    
  end function get_therm_pcond
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_therm_scond(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty

    get_therm_scond = bdyty(ibdyty)%tacty(itacty)%BDARY%STCond
    
  end function get_therm_scond
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_therm_xcond(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty

    get_therm_xcond = bdyty(ibdyty)%tacty(itacty)%BDARY%Tnx
    
  end function get_therm_xcond
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_therm_ycond(ibdyty,itacty)
    implicit none
    integer,intent(in) :: ibdyty,itacty

    get_therm_ycond = bdyty(ibdyty)%tacty(itacty)%BDARY%Tny
    
  end function get_therm_ycond
!!!------------------------------------------------------------------------ 
  subroutine get_ani_therm_cond(ibdyty,itacty,PTcond,STcond,Tnx,Tny)
    implicit none
    real(kind=8)               :: PTcond,STcond,Tnx,Tny
    integer(kind=4),intent(in) :: ibdyty,itacty

    PTCond = bdyty(ibdyty)%tacty(itacty)%BDARY%PTCond
    STCond = bdyty(ibdyty)%tacty(itacty)%BDARY%STCond
    Tnx    = bdyty(ibdyty)%tacty(itacty)%BDARY%Tnx   
    Tny    = bdyty(ibdyty)%tacty(itacty)%BDARY%Tny

  end subroutine get_ani_therm_cond
!!!------------------------------------------------------------------------
real(KIND=8) function get_therm_sheat(ibdyty,itacty)
!jr
    implicit none
    integer,intent(in) :: ibdyty,itacty

    get_therm_sheat = bdyty(ibdyty)%tacty(itacty)%BDARY%Hspe
    
  end function get_therm_sheat
!!!------------------------------------------------------------------------
  real(KIND=8) function get_therm_diffu_alpha_rtheta(ibdyty,itacty)
!jr
    implicit none
    integer,intent(in) :: ibdyty,itacty

    get_therm_diffu_alpha_rtheta = bdyty(ibdyty)%tacty(itacty)%BDARY%alpha_rtheta
    
  end function get_therm_diffu_alpha_rtheta
!!!------------------------------------------------------------------------
  real(KIND=8) function get_therm_diffu_alpha_z(ibdyty,itacty)
!jr
    implicit none
    integer,intent(in) :: ibdyty,itacty

    get_therm_diffu_alpha_z = bdyty(ibdyty)%tacty(itacty)%BDARY%alpha_z
    
  end function get_therm_diffu_alpha_z
!!!------------------------------------------------------------------------
  subroutine put_therm_sheat(ibdyty,itacty,Hspe)
!jr
    implicit none
    integer,intent(in) :: ibdyty,itacty
    real(kind=8)       :: Hspe

    bdyty(ibdyty)%tacty(itacty)%BDARY%Hspe = Hspe
    
  end subroutine put_therm_sheat

!!!------------------------------------------------------------------------ 
  subroutine put_therm_diffu(ibdyty,itacty,alpha_rtheta,alpha_z)
!jr
    implicit none
    integer,intent(in) :: ibdyty,itacty
    real(kind=8)       :: alpha_rtheta, alpha_z

    bdyty(ibdyty)%tacty(itacty)%BDARY%alpha_rtheta = alpha_rtheta
    bdyty(ibdyty)%tacty(itacty)%BDARY%alpha_z = alpha_z

  end subroutine put_therm_diffu

!!!------------------------------------------------------------------------ 
  subroutine put_therm_cond(ibdyty,itacty,cond)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty
    real(kind=8)               :: cond

    bdyty(ibdyty)%tacty(itacty)%BDARY%TCond = cond
    
  end subroutine  put_therm_cond
!!!------------------------------------------------------------------------ 
  real(kind=8) function get_thermal_coefficient(ibdyty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty

    get_thermal_coefficient = bdyty(ibdyty)%Talpha

  end function get_thermal_coefficient
!!!------------------------------------------------------------------------ 
!!! get_WS : Recup surface energy of contactor itacty of body ibdyty
  !------------------------------------------------------------------------ 
  subroutine initialize_status_sectors
    implicit none
    integer(kind=4) :: ibdyty,itacty,isect

    do ibdyty=1,nb_RBDY2
       do itacty=1,size(bdyty(ibdyty)%tacty)
          do isect=1,nb_WSsect
!             bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect) = i_undefined
!mr&vhn pb pas d evolution possible avec undefined
             bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect) = i_free
          end do
       end do
    end do

  end subroutine initialize_status_sectors

!!vhn - initialize energie surface of all sector = WSmax ou WSmin
  subroutine initialize_WS_sectors(deltaWS)
    implicit none
    
    integer(kind=4) :: ibdyty,itacty,isect,iblmty,ibehav
    real(kind=8)    :: WS,WSmin,WSmax,deltaWS,ContTime,ActIner
    
    do ibdyty=1,nb_RBDY2
       iblmty = 1
       ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
       do itacty=1,size(bdyty(ibdyty)%tacty)
          
          call get_surface_energy_WS(ibehav,WSmax,WSmin,ContTime,ActIner)
             
          bdyty(ibdyty)%tacty(itacty)%BDARY%WStime(1:nb_WSsect) = ContTime
          bdyty(ibdyty)%tacty(itacty)%BDARY%WS(1:nb_WSsect)    = WSmin + deltaWS*(WSmax-WSmin)
          bdyty(ibdyty)%tacty(itacty)%BDARY%WSini(1:nb_WSsect) = WSmin + deltaWS*(WSmax-WSmin)
     
       end do
    end do

  end subroutine initialize_WS_sectors

  !> brief
  subroutine update_status_sector(ibdyty,itacty,isect,status)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty,isect,status
    integer(kind=4)            :: statusik

    bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect) = status !MR&VHN

  end subroutine update_status_sector

  !> brief
  real(kind=8) function get_WS(ibdyty,itacty,isect)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty,isect
    
    get_WS = bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect)
    
  end function get_WS

  real(kind=8) function get_WStime(ibdyty,itacty,isect)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty,isect
    
    get_WStime = bdyty(ibdyty)%tacty(itacty)%BDARY%WStime(isect)
    
  end function get_WStime

  real(kind=8) function get_WSstatus(ibdyty,itacty,isect)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty,isect
    
    get_WSstatus = bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect)
    
  end function get_WSstatus

  !> brief
  real(kind=8) function get_average_WS(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty
    integer(kind=4)            :: isect

    get_average_WS = 0.d0
    
    do isect=1,nb_WSsect
       get_average_WS = get_average_WS + bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect)
    end do

    get_average_WS = get_average_WS/real(nb_WSsect,8)

  end function get_average_WS
  !vhn
  real(kind=8) function get_average_WS_active(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty
    integer(kind=4)            :: isect,isect_active

    get_average_WS_active = 0.d0
    isect_active = 0
    do isect=1,nb_WSsect
       IF ((bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect).EQ. 2 ).OR. &
            (bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect).EQ. 3 )) THEN
          get_average_WS_active = get_average_WS_active + bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect)
          isect_active = isect_active +1
       END IF

    end do
    IF (isect_active .EQ. 0) isect_active = 1
    get_average_WS_active = get_average_WS_active/real(isect_active,8)

  end function get_average_WS_active

  real(kind=8) function get_max_WS(ibdyty,itacty)
    implicit none
    integer(kind=4),intent(in) :: ibdyty,itacty
    integer(kind=4)            :: isect

    get_max_WS = 0.d0
    
    do isect=1,nb_WSsect
       IF (bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect) .GE. get_max_WS) THEN
          get_max_WS = bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect)
       END IF
    end do

  end function get_max_WS
!!!------------------------------------------------------------------------
  subroutine update_WSvsT_RBDY2
    implicit none
    integer(kind=4) :: ibdyty,itacty,iblmty,ibehav,isect
    real(kind=8)    :: WS,T,WSini

    iblmty = 1

    do ibdyty=1,nb_RBDY2
       ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
       do itacty=1,size(bdyty(ibdyty)%tacty) 
          do isect=1,nb_WSsect
             WSini = bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect)
             bdyty(ibdyty)%tacty(itacty)%BDARY%WSini(isect) = WSini
             T     = bdyty(ibdyty)%tacty(itacty)%BDARY%T
             call compute_WSvsT(ibehav,WS,T)
             bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect) = WS
          end do
       end do
    end do

  end subroutine update_WSvsT_RBDY2
!!!------------------------------------------------------------------------ 
!!! vhn & mr
  subroutine update_WSvsTime_RBDY2
    implicit none
    integer(kind=4) :: ibdyty,itacty,iblmty,ibehav,isect,WSstatus
    real(kind=8)    :: WS,WStime,WSini

    do ibdyty=1,nb_RBDY2
       iblmty = 1
       ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
       do itacty=1,size(bdyty(ibdyty)%tacty) 

          do isect=1,nb_WSsect
             WSini    = bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect)
             WStime   = bdyty(ibdyty)%tacty(itacty)%BDARY%WStime(isect)
             WSstatus = bdyty(ibdyty)%tacty(itacty)%BDARY%WSstatus(isect)

             call compute_WSvsTime(ibehav,WSstatus,WS,WStime,WSini)

             bdyty(ibdyty)%tacty(itacty)%BDARY%WS(isect) = WS
             bdyty(ibdyty)%tacty(itacty)%BDARY%WStime(isect) = WStime

          end do

       end do

    end do

  end subroutine update_WSvsTime_RBDY2
!!!------------------------------------------------------------------------ 
!!! END MULTI-PHYSICS APPLICATIONS
!!!------------------------------------------------------------------------ 
  subroutine resize_RBDY2(homo)

    implicit none
    integer            :: ibdyty,iblmty,itacty,nbdof
    character(len=23)  :: IAM='mod_RBDY2::resize_RBDY2'
    character(len=103) :: cout
    real(kind=8)       :: homo

    do ibdyty=1,nb_RBDY2       

       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       bdyty(ibdyty)%cooref(1:nbdof)=homo*bdyty(ibdyty)%cooref(1:nbdof)

       do iblmty=1,size(bdyty(ibdyty)%blmty)
          select case(bdyty(ibdyty)%blmty(iblmty)%blmID)
          case('PLAIN')
             bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius=homo*bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius
             bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius=homo*bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius
          end select
       end do
       do itacty=1,size(bdyty(ibdyty)%tacty) 
          select case(bdyty(ibdyty)%tacty(itacty)%tacID)
          case('DISKx','xKSID')           
             bdyty(ibdyty)%tacty(itacty)%BDARY%data(1)=homo*bdyty(ibdyty)%tacty(itacty)%BDARY%data(1)
          case('JONCx','PT2Dx')
             bdyty(ibdyty)%tacty(itacty)%BDARY%data(1)=homo*bdyty(ibdyty)%tacty(itacty)%BDARY%data(1)
             bdyty(ibdyty)%tacty(itacty)%BDARY%data(2)=homo*bdyty(ibdyty)%tacty(itacty)%BDARY%data(2)
          case('POLYG')            
             bdyty(ibdyty)%tacty(itacty)%BDARY%data(:)=homo*bdyty(ibdyty)%tacty(itacty)%BDARY%data(:)
             bdyty(ibdyty)%tacty(itacty)%BDARY%shift  =homo*bdyty(ibdyty)%tacty(itacty)%BDARY%shift
          case('DISKb')
             bdyty(ibdyty)%tacty(itacty)%BDARY%data(1)=homo*bdyty(ibdyty)%tacty(itacty)%BDARY%data(1)
             bdyty(ibdyty)%tacty(itacty)%BDARY%shift  =homo*bdyty(ibdyty)%tacty(itacty)%BDARY%shift
          case default
             write(cout,'(A6,A5,A8)') 'tacty ',bdyty(ibdyty)%tacty(itacty)%tacID,' unknown'
             !123456                                     12345678
             call FATERR(IAM,cout)
          end select
       end do
    end do

  end subroutine resize_RBDY2
!!!------------------------------------------------------------------------   
  subroutine nullify_X_dof_RBDY2

    implicit none 

    integer :: ibdyty

    ! imposing a vanishing begin velocity

    do ibdyty=1,nb_RBDY2
       bdyty(ibdyty)%Xbegin= 0.D0
       bdyty(ibdyty)%X     = 0.D0                                
    end do

  end subroutine nullify_X_dof_RBDY2
!!!------------------------------------------------------------------------
  subroutine nullify_V_dof_RBDY2

    implicit none 

    integer :: ibdyty

    ! imposing a vanishing begin velocity

    do ibdyty=1,nb_RBDY2
       bdyty(ibdyty)%Vbegin= 0.D0
       bdyty(ibdyty)%V     = 0.D0                                
    end do

  end subroutine nullify_V_dof_RBDY2
!!!------------------------------------------------------------------------
  subroutine print_info_RBDY2(ibdyty)

    implicit none

    integer           :: ibdyty,iblmty=1
    character(len=80) :: cout

    write(cout,1) ibdyty
    call LOGMES(cout,.TRUE.)

    write(cout,2) bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius,bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius
    call LOGMES(cout,.TRUE.)
    
    write(cout,3) bdyty(ibdyty)%inv_mass(1),bdyty(ibdyty)%inv_mass(2),bdyty(ibdyty)%inv_mass(3)
    call LOGMES(cout,.TRUE.)
    
1   format(1X,'RBDY2:',1x,I5)
2   format(1X,'avrd :',1x,D14.7,1X,'gyrd :',1x,D14.7)
3   format(1X,'1/m1 :',1x,D14.7,1X,'1/m2 :',1x,D14.7,1X,'1/m3 :',1x,D14.7)
    
  end subroutine print_info_RBDY2
!!!------------------------------------------------------------------------ 
  subroutine ghost2invisible_RBDY2

    implicit none

    integer :: ibdyty

    do ibdyty=1,nb_RBDY2
       if (bdyty(ibdyty)%blmty(1)%behav == 'ghost') bdyty(ibdyty)%visible = .false. 
    end do

  end subroutine ghost2invisible_RBDY2
!!!------------------------------------------------------------------------ 
!!!mj This command allows to change BODIES.DAT ref coordinates into 
!!!mj BODIES.DAT refcoordinates + DOF.INI displacements
!!!mj This command is used to change reference configuration
!!!------------------------------------------------------------------------ 
  subroutine add_dof2bodies_RBDY2

    implicit none

    integer :: ibdyty,nbdof
    
    do ibdyty=1,nb_RBDY2       
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       bdyty(ibdyty)%cooref(1:nbdof)=bdyty(ibdyty)%cooref(1:nbdof)+bdyty(ibdyty)%Xbegin(1:nbdof)
    end do

  end subroutine add_dof2bodies_RBDY2
!!!------------------------------------------------------------------------ 
  logical function get_write_Rnod_RBDY2(fantome)

    implicit none
    integer,optional :: fantome

    get_write_Rnod_RBDY2 = write_Rnod

  end function get_write_Rnod_RBDY2
!!!------------------------------------------------------------------------ 
  logical function get_write_DOF_RBDY2(fantome)

    implicit none
    integer,optional :: fantome

    get_write_DOF_RBDY2 = write_DOF

  end function get_write_DOF_RBDY2

!!!------------------------------------------------------------------------ 
  function get_T_RBDY2(ibdyty)
    !
    ! routine destinee a recuperer la temperature d'une brique
    !
    implicit none
    integer      :: ibdyty
    real(kind=8) :: get_T_RBDY2

    get_T_RBDY2 = bdyty(ibdyty)%T

  end function get_T_RBDY2
!!!------------------------------------------------------------------------ 
  subroutine put_invmass_RBDY2(ibdyty,W,nbdof)
    implicit none
    integer :: ibdyty,nbdof
    real(kind=8),dimension(nbdof) :: W
    
    if (size(bdyty(ibdyty)%inv_mass) /= nbdof) then
     call faterr('RBDY2::put_invmass','taille non concordante')
    endif

    bdyty(ibdyty)%inv_mass = W

  end subroutine
!!!------------------------------------------------------------------------ 
  subroutine put_precon_W_RBDY2(ibdyty,idof,InvMass)
    implicit none
    integer :: ibdyty,idof
    real(kind=8) :: InvMass
    
    if (size(bdyty(ibdyty)%inv_mass) < idof) then
     call faterr('RBDY2::put_precon_W','taille non concordante')
    endif

    bdyty(ibdyty)%inv_mass(idof) = InvMass

  end subroutine
!------------------------------------------------------------------------
 subroutine put_vector_RBDY2(id_vect,ibdyty,vect,nbdof)
 implicit none

   integer :: ibdyty,nbdof
   real(kind=8),dimension(nbdof) :: vect
   character(len=5) :: id_vect

   if (nb_RBDY2 == 0) return

   if (nbdof /= size(bdyty(ibdyty)%V) ) then
     call faterr('RBDY2::put_vector','nbdof non concordant')
   endif

   select case(id_vect)
    case('X____')
     bdyty(ibdyty)%X=vect
    case('Xbeg_')
     bdyty(ibdyty)%Xbegin=vect
    case('V____')
     bdyty(ibdyty)%V=vect
    case('Vbeg_')
     bdyty(ibdyty)%Vbegin=vect
    case('Vaux_')
     bdyty(ibdyty)%Vaux=vect
    case('Reac_')
     bdyty(ibdyty)%Ireac=vect*H
    case('Raux_')
     bdyty(ibdyty)%Iaux=vect*H
    case('Ireac')
     bdyty(ibdyty)%Ireac=vect
    case('Iaux_')
     bdyty(ibdyty)%Iaux=vect
    case('Vfree')
     bdyty(ibdyty)%Vfree=vect
    case('Fext_')
     bdyty(ibdyty)%Fext=bdyty(ibdyty)%Fext+vect
    !am : ajout de la possibilite de modifier directement les 
    ! coordonnees au debut ('Coorb") ou a la fin ('Coor_') du
    ! pas de temps
    case('Coor_')
     call put_coor(ibdyty, vect)
    case('Coorb')
     call put_coor_begin(ibdyty, vect)
    case('Coor0')
     call put_cooref(ibdyty, vect)

    case DEFAULT
     call faterr('RBDY2::put_vector','Sorry unknown id: '//id_vect)
   end select

 end subroutine put_vector_RBDY2
!!!------------------------------------------------------------------------
 subroutine compute_window_forces

   implicit none
   integer :: ibdyty

   do ibdyty=1,nb_RBDY2
      if (.not.bdyty(ibdyty)%visible) cycle
      if ( bdyty(ibdyty)%blmty(1)%behav .ne. CW_BEHAV ) cycle
      if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) < CW_XMIN ) cycle
      if ( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) > CW_XMAX ) cycle

      bdyty(ibdyty)%Fext(1) = CW_CV__
   end do

 end subroutine compute_window_forces
!!!------------------------------------------------------------------------
 subroutine increment_window_velocity

   implicit none
   integer :: ibdyty

   do ibdyty=1,nb_RBDY2
      if(.not.bdyty(ibdyty)%visible) cycle
      if( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) < CW_XMIN ) cycle
      if( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) > CW_XMAX ) cycle

      if ( bdyty(ibdyty)%blmty(1)%behav .eq. CW_BEHAV ) then
         bdyty(ibdyty)%Vbegin(1) = CW_CV__
         bdyty(ibdyty)%V(1)      = CW_CV__
      end if

   end do

 end subroutine increment_window_velocity
!!!------------------------------------------------------------------------
 subroutine compute_window_velocity

   implicit none
   integer :: ibdyty

   do ibdyty=1,nb_RBDY2
      if(.not.bdyty(ibdyty)%visible) cycle
      if( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) < CW_XMIN ) cycle
      if( (bdyty(ibdyty)%X(1)+bdyty(ibdyty)%cooref(1) ) > CW_XMAX ) cycle

      if ( bdyty(ibdyty)%blmty(1)%behav .eq. CW_BEHAV ) then
         bdyty(ibdyty)%V(1) = CW_CV__
         !bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) + THETA*H*bdyty(ibdyty)%V(1)
      end if

   end do

 end subroutine compute_window_velocity
!!!------------------------------------------------------------------------
 subroutine set_init_cnstrt_window(Xmin,Xmax,CV,BEHAV)

   implicit none
   real(kind=8)     :: Xmin,Xmax,cv
   character(len=5) :: BEHAV
   CW_XMIN = Xmin
   CW_XMAX = Xmax
   CW_CV__ = CV
   CW_BEHAV = BEHAV

 end subroutine set_init_cnstrt_window
!!!------------------------------------------------------------------------
 subroutine get_vector_RBDY2(id_vect,ibdyty,vect,nbdof)
 implicit none

   integer, intent(in) :: ibdyty,nbdof
   real(kind=8),dimension(nbdof), intent(out) :: vect
   character(len=5), intent(in) :: id_vect
   character(len=80) :: cout
   character(len=18) :: IAM
         !123456789012345678
   IAM = 'RBDY2::get_vector'

   if (nb_RBDY2 == 0) return

   if (id_vect(1:4) == 'Coor') then
     if(nbdof /= size(bdyty(ibdyty)%cooref) ) then
       write(cout,'(A,I0,1x,I0)') 'nbdof Coor non concordant: ', nbdof,size(bdyty(ibdyty)%cooref)
       call faterr(IAM,cout)
     endif
   else
     if (nbdof /= size(bdyty(ibdyty)%V) ) then
       write(cout,'(A,I0,1x,I0)') 'nbdof ddl non concordant: ', nbdof,size(bdyty(ibdyty)%cooref)
       call faterr(IAM,cout)
     endif
   endif

   select case(id_vect)
    case('Coor0')
     vect=bdyty(ibdyty)%cooref(1:nbdof)
    case('Coor_')
     vect=get_coor(ibdyty,0)
    case('Coorb')
     vect=get_coor_begin(ibdyty,0)
    case('Coorm')
     vect=get_coorTT(ibdyty,0)
    case('X____')
     vect=bdyty(ibdyty)%X
    case('Xbeg_')
     vect=bdyty(ibdyty)%Xbegin
    case('V____')
     vect=bdyty(ibdyty)%V
    case('Vbeg_')
     vect=bdyty(ibdyty)%Vbegin
    case('Vfree')
     vect=bdyty(ibdyty)%Vfree
    case('Vaux_')
     vect=bdyty(ibdyty)%Vaux
    case('Reac_')
     vect=bdyty(ibdyty)%Ireac/H
    case('Raux_')
     vect=bdyty(ibdyty)%Iaux/H
    case('Ireac')
     vect=bdyty(ibdyty)%Ireac
    case('Iaux_')
     vect=bdyty(ibdyty)%Iaux
    !am: ajout de la recuperation de la moyenne sur le pas de temps des 
    !    efforts exterieurs
    case('Fext_')
     vect=bdyty(ibdyty)%Fext
    !rm: ajout de la recuperation de Fint
    case('Fint_')
     vect=bdyty(ibdyty)%Fint
    case DEFAULT
     call faterr(IAM,'Sorry unknown id: '//id_vect)
   end select

 end subroutine get_vector_RBDY2

 function get_ptr_vector_RBDY2(id_vect,ibdyty)
 implicit none

   integer :: ibdyty
   real(kind=8),dimension(:),pointer :: get_ptr_vector_RBDY2
   character(len=5) :: id_vect

   if (nb_RBDY2 == 0) return

   select case(id_vect)
    case('Coor0')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%cooref
    case('X____')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%X
    case('Xbeg_')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%Xbegin
    case('V____')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%V
    case('Vbeg_')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%Vbegin
    case('Vaux_')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%Vaux
    case('Ireac')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%Ireac
    case('Iaux_')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%Iaux
    !am: ajout de la recuperation la moyenne sur le pas de temps des 
    !    efforts exterieurs
    case('Fext_')
     get_ptr_vector_RBDY2 => bdyty(ibdyty)%Fext
    case DEFAULT
     call faterr('RBDY2::get_ptr_vector','Sorry unknown id: '//id_vect)
   end select

 end function get_ptr_vector_RBDY2
!------------------------------------------------------------------------  
! am: debut des routine supplementaires
 
  ! fonction qui calcule les grandeurs necessaires pour tester si on a atteint l'etat d'equilibre :
  !   * Qnorm : 1/N*sum(<|v_i|>), ou |v_i| est la norme euclidienne du vecteur vitesse v_i
  !   * Mnorm : sup(<|v_i|>) 
  ! pour les corps dont l'ordonnee est situe entre alt_min et alt_max
  subroutine compute_partial_equilibrium_state_RBDY2(alt_min, alt_max, Qnorm, Mnorm)

     implicit none

     ! variables d'entree :
     real(kind=8), intent(in) :: alt_min, alt_max
       ! le calcul n'est effectue que pour les corps dont l'ordonnee
       ! est entre alt_min et alt_max

     ! variables de sortie :
     real(kind=8), intent(out) :: Qnorm, Mnorm 
        !   * Qnorm : 1/N*sum(<|v_i|>), ou |v_i| est la norme euclidienne du vecteur vitesse v_i
        !   * Mnorm : sup(<|v_i|>)

     ! variables locales       1234567890123456789012345678901234567890
     character(len=40) :: IAM='RBDY2::compute_partial_equilibrium_state'
     integer           :: ibdyty
     real(kind=8)      :: norm

     integer :: nb_RBDY2_used ! nombre de RBDY2 utilies pour le calcul
                              ! de la norme, i.e. le nombre de corps dont
                              ! l'abscisse est comprise entre 0 et 
                              ! abs_max, si on l'a donne

     ! si l'intervalle [alt_min, alt_max] n'est pas defini ou degenere
     if (alt_min >= alt_max) then
        ! on a leve une erreur fatale!
        call logmes('Error '//IAM//': intervalle d altitudes non defini, ou degenere!')
     end if

     ! on initialise le nombre de RBDY2 utilises poir le calcul de la norme
     nb_RBDY2_used = 0

     Qnorm = 0.D0
     Mnorm =-1.D20
    
     do ibdyty=1,nb_RBDY2
        ! l'ordonnee du grain doit etre
        ! comprise entre alt_min et alt_max
        if (bdyty(ibdyty)%cooref(2) + bdyty(ibdyty)%Xbegin(2) >= alt_min .and. &
            bdyty(ibdyty)%cooref(2) + bdyty(ibdyty)%Xbegin(2) <= alt_max) &
        then
           ! on incremente le nombre de RBDY2 utilises pour le calcul de la norme
           nb_RBDY2_used = nb_RBDY2_used + 1
    
           norm = dsqrt(&
                  bdyty(ibdyty)%Vbegin(1)*bdyty(ibdyty)%Vbegin(1) + &
                  bdyty(ibdyty)%Vbegin(2)*bdyty(ibdyty)%Vbegin(2) + &
                  bdyty(ibdyty)%Vbegin(3)*bdyty(ibdyty)%Vbegin(3))
       
           Qnorm = Qnorm + norm
           Mnorm= max (Mnorm,norm)
        end if
     end do

    ! si le nombre de RBDY2 utilises pour le calcul de la norme est nul
    if (nb_RBDY2_used == 0) then
       ! on affiche un warning
       call logmes(IAM//':Warning: Aucun corps n a ete detecte dans la zone indiquee!')
       ! on met a 0 les normes
       Qnorm = 0.d0
       Mnorm= 0.d0
    ! sinon,
    else
       ! on moyenne la norme quadratique en utilisant le nombre de RBDY2
       ! utilises pour la calculer
       Qnorm = Qnorm / real(nb_RBDY2,8)  
    end if  

  end subroutine compute_partial_equilibrium_state_RBDY2

  ! fonction qui teste si une partie de l'achantillon est en equilibre
  ! les corps ayant leur abscisse comprise entre 0 et abs_max
  ! elle sert dans le cas des silosn pour decider si on active la 
  ! recherche d'arches
  ! precondition:
  !    - alt_min: altitude (ordonnee) minimale definissant les corps pris en compte
  !    - alt_max: altitude (ordonnee) maximale definissant les corps pris en compte
  ! postcondition:
  !    - info: vaut "vrai" ssi l'equilibre a ete atteint
  subroutine check_partial_equilibrium_state_RBDY2(info, alt_min, alt_max)

    implicit none 

    ! variable d'entree
    real(kind=8), intent(in) :: alt_min, alt_max 
       ! le calcul n'est effectue que pour les corps dont l'ordonnee
       ! est entre alt_min et alt_max
    
    ! variables de sortie
    logical, intent(out) :: info ! vaut "vrai" ssi la norme calculee est plus petite que la tolerance

    ! variables locales
    integer           :: ibdyty
    real(kind=8)      :: norm,Qnorm,Mnorm
    character(len=60) :: cout
    character(len=60) :: IAM='RBDY2::check_partial_equilibrium_state'
    integer :: nb_RBDY2_used ! nombre de RBDY2 utilies pour le calcul
                             ! de la norme, i.e. le nombre de corps dont
                             ! l'abscisse est comprise entre 0 et 
                             ! abs_max, si on l'a donne
   
    ! on calcule les deux normes 
    call compute_partial_equilibrium_state_RBDY2(alt_min, alt_max, Qnorm, Mnorm)

    ! on affiche le rapport norme/tolerance, pour chacune 
    write(cout,'(1X,A3,2(3X,A12,D10.3,1X))') &
         ' @ ','Qnorm / tol=',Qnorm/eqs_tol,'Mnorm / tol=',Mnorm/eqs_tol 
    call LOGMES(cout)

    info = .false.

    ! en fonction du type de norme choisi pour decider que l'equilibre est atteint
    ! on compare Qnorm ou Mnorm a la tolerance
    select case(eqs_ichecktype)
    case(iQvlcy)
       if (Qnorm <= eqs_tol) info = .true.
    case(iMvlcy)
       if (Mnorm <= eqs_tol) info = .true.
    end select
    
    ! on affiche la valeur de la tolerance et le resultat
    print*, 'tol=', eqs_tol
    print*, 'equilibre atteint=', info
    
  end subroutine check_partial_equilibrium_state_RBDY2

  ! fonction qui permet de savoir si on impose une condition a la limite periodique
  ! precondition:
  !   - rien
  ! postcondition:
  !   - renvoie 'vrai' ssi on impose une condition a la limite periodique
  function is_periodic_RBDY2()

    implicit none
    
    ! variable de retour
    logical :: is_periodic_RBDY2

    is_periodic_RBDY2 = PERIODIC

  end function is_periodic_RBDY2

  function get_xperiode_RBDY2()
    implicit none
    real(kind=8) :: get_xperiode_RBDY2

    get_xperiode_RBDY2 = periode

  end function get_xperiode_RBDY2

  ! fonction qui permet de modifier les coordonnees a la fin du pas de temps d'un corps
  ! precondition:
  !    - ibdyty: numero du corps auquel on veut imposer de nouvelles coordonnees
  !    - new_coor: nouvelles coordonnees du corps
  ! postcondition:
  !    - les coordonnees a la fin du pas de temps du corps ibdyty sont celles stockees dans
  !      new_coor
  subroutine put_coor(ibdyty, new_coor)

    implicit none 

    ! variables d'entree:
    integer, intent(in) :: ibdyty ! numero du corps on veut imposer de nouvelles coordonnees
    real(kind=8), dimension(size(bdyty(ibdyty)%cooref)) :: new_coor ! nouvelles coordonnees du corps

    ! on modifie les deplacements a la fin du pas de temps, du corps, de sorte que ses nouvelles coordonnees
    ! soit celles stockees dans new_coor
    bdyty(ibdyty)%X = new_coor - bdyty(ibdyty)%cooref    
       
  end subroutine put_coor

  ! fonction qui permet de modifier les coordonnees au debut du pas de temps d'un corps
  ! precondition:
  !    - ibdyty: numero du corps auquel on veut imposer de nouvelles coordonnees
  !    - new_coor: nouvelles coordonnees du corps
  ! postcondition:
  !    - les coordonnees a la fin du pas de temps du corps ibdyty sont celles stockees dans
  !      new_coor
  subroutine put_coor_begin(ibdyty, new_coor)

    implicit none 

    ! variables d'entree:
    integer, intent(in) :: ibdyty ! numero du corps on veut imposer de nouvelles coordonnees
    real(kind=8), dimension(size(bdyty(ibdyty)%cooref)) :: new_coor ! nouvelles coordonnees du corps

    ! on modifie les deplacements au debut du pas de temps, du corps, de sorte que ses nouvelles coordonnees
    ! soit celles stockees dans new_coor
    bdyty(ibdyty)%Xbegin = new_coor - bdyty(ibdyty)%cooref 
       
  end subroutine put_coor_begin
  
  ! fonction qui permet de recuperer les coordonnees au debut du pas de temps d'un corps
  ! precondition:
  !    - ibdyty: numero du corps dont on veut imposer de nouvelles coordonnees
  !    - itacty (en OPTION): pour agir sur un certain conacteur attache au corps (am: est-ce utile?)
  ! postcondition:
  !    - le vecteur retourne contient les coordonnees du corps au debut du pas de temps

  !mr pas d'accent dans les fichiers
  !mr cette fct n'est pas utile est fait redondance avec get_coor

  function get_coor_begin(ibdyty,itacty)

    implicit none

    ! variables d'entree:
    integer, intent(in) :: ibdyty ! numero du corps on veut imposer de nouvelles coordonnees
    integer, intent(in), optional :: itacty ! pour agir sur un certain conacteur attache au corps (au cas ou...)
    
    ! variable de retour
    real(kind=8),dimension(size(bdyty(ibdyty)%cooref)) :: get_coor_begin ! coordonnees au debut du pas de temps

    ! variables locales:
    real(kind=8) :: a_begin ! angle de rotation du corps autour de l'axe (Oz)
                      ! au debut du pas de temps (deplacement du corps dans
                      ! le repere entraine)
    real(kind=8) :: c, s ! respectivement le cosinus et le sinus de l'angle
                         ! precedent
    real(kind=8), dimension(2) :: shift_begin ! vecteur permettant de passer de
                         ! la position du centre d'inertie, au debut du pas de
                         ! temps, a la position du contacteur passe en argument
                         ! (si celui-ci est bien defini),  au debut du pas de                            ! temps

    ! on calcule les coordonnees au debut du pas de temps et on les stocke dans la variable de retour
    get_coor_begin = bdyty(ibdyty)%cooref + bdyty(ibdyty)%Xbegin 
       
    ! si on donne un numero de contacteur attache au corps bien defini 
    if (itacty .ne. 0) then
        
       ! on calcule la rotation au debut du pas de temps 
       a_begin =  bdyty(ibdyty)%cooref(3) + bdyty(ibdyty)%Xbegin(3)

       ! on en deduit le cosinus et le sinus de cet angle
       c=cos(a_begin)
       s=sin(a_begin)

       ! ce qui permet de construire le vecteur permettant de passer
       ! de la position du centre d'inertie du corps a la position 
       ! du centre d'inertie du contacteur
       shift_begin(1)=  c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
                      - s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
       shift_begin(2)=  s*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
                      + c*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)

       ! on en deduit la position du centre d'inertie du contacteur dans
       ! le repere global 
       get_coor_begin(1:2) = get_coor_begin(1:2) + shift_begin(1:2)

    end if

  end function get_coor_begin
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
  subroutine biaxial_loading(num_down,f_down,num_right,f_right,num_up,f_up,num_left,f_left)

 ! routine de chargement en contrainte d'un boite rectangulaire
 ! on donne les numeros des corps qui composent la boite (sens trigo)
 ! 1: down, 2:right, 3:up, 4:left 
 ! chaque chargement est constant dans le temps

 implicit none
 integer                                    :: num_down,num_right
 integer                                    :: num_up,num_left
 real(kind=8)                               :: f_down,f_right,f_up,f_left
 real(kind=8),dimension(3,4)                :: coor
 real(kind=8),dimension(4),save             :: ep
 real(kind=8),dimension(4),save             :: L0
 real(kind=8),dimension(4)                  :: L
 logical                                    :: is_first_time=.true.

 character(len=80) :: cout

 coor(:,1) = bdyty(num_down )%cooref+bdyty(num_down )%Xbegin
 coor(:,2) = bdyty(num_right)%cooref+bdyty(num_right)%Xbegin
 coor(:,3) = bdyty(num_up   )%cooref+bdyty(num_up   )%Xbegin
 coor(:,4) = bdyty(num_left )%cooref+bdyty(num_left )%Xbegin

 if (is_first_time) then
   ep(1) = bdyty(num_down )%tacty(1)%BDARY%DATA(2)
   ep(2) = bdyty(num_right)%tacty(1)%BDARY%DATA(2)
   ep(3) = bdyty(num_up   )%tacty(1)%BDARY%DATA(2)
   ep(4) = bdyty(num_left )%tacty(1)%BDARY%DATA(2)

   L0(1) = (coor(1,2)-ep(2))-(coor(1,4)+ep(4)) !x
   L0(2) = (coor(2,3)-ep(3))-(coor(2,1)+ep(1)) !y
   L0(3) = (coor(1,2)-ep(2))-(coor(1,4)+ep(4)) !x
   L0(4) = (coor(2,3)-ep(3))-(coor(2,1)+ep(1)) !y
  
   is_first_time = .false.
 end if
 L(1) = (coor(1,2)-ep(2))-(coor(1,4)+ep(4))
 L(2) = (coor(2,3)-ep(3))-(coor(2,1)+ep(1))
 L(3) = (coor(1,2)-ep(2))-(coor(1,4)+ep(4))
 L(4) = (coor(2,3)-ep(3))-(coor(2,1)+ep(1))

 bdyty(num_down)%Fext(2)  = f_down * L(1)
 write(cout,'(A,I0,A,D12.4)') '-> F(',num_down,') = ', bdyty(num_down)%Fext(2)
 call logmes(cout)

 bdyty(num_right)%Fext(1) = -f_right * L(2)
 write(cout,'(A,I0,A,D12.4)') '-> F(',num_right,') = ', bdyty(num_right)%Fext(1)

 bdyty(num_up)%Fext(2)    = -f_up * L(3)
 write(cout,'(A,I0,A,D12.4)') '-> F(',num_up,') = ', bdyty(num_up)%Fext(2)

 bdyty(num_left)%Fext(1)  = f_left * L(4)
 write(cout,'(A,I0,A,D12.4)') '-> F(',num_left,') = ', bdyty(num_left)%Fext(1)

 end subroutine biaxial_loading
!------------------------------------------------------------------------  
 subroutine Oedemetric_test(num_up,num_right,num_left,Finitial,Ffinal,DeltaT)

 ! routine de chargement en contrainte de la parois superieure d'une boite
 ! on donne les numeros des corps qui composent la parois sup et 
 ! les parois droite et gauches
 ! le chargement sur la parois superieure evolue cycliquement et par palier entre Fini et Ffin
 ! avec des actualisation tous les deltaT
 ! Fini donne aussi la valeur du palier

   implicit none
   integer                                 :: num_up,num_right,num_left
   real(kind=8)                            :: Finitial,Ffinal,DeltaT
   real(kind=8),save                       :: cpt_1,FORCE
   logical                                 :: is_first_time=.true.
   real(kind=8)                            :: sens=1.d0, F_applied=0.d0
   real(kind=8),dimension(3,2)             :: coor
   real(kind=8),dimension(2)               :: ep
   real(kind=8)                            :: L
 
   character(len=80) :: cout

   if (is_first_time) then
     FORCE =  - Finitial
     cpt_1 = 0.D0
     is_first_time = .false.
     F_applied = FORCE
   end if

   coor(:,1) =   bdyty(num_right)%cooref+bdyty(num_right)%Xbegin
   coor(:,2) =   bdyty(num_left )%cooref+bdyty(num_left )%Xbegin
   ep(1)     =   bdyty(num_right)%tacty(1)%BDARY%DATA(2)
   ep(2)     =   bdyty(num_left )%tacty(1)%BDARY%DATA(2)
   L         =   abs( (coor(1,1)-ep(2))-(coor(1,2)+ep(2)) )

   cpt_1 = cpt_1 + H
   if (cpt_1 > DeltaT) then
     if (sens > 0.d0) then  ! charge
       F_applied = F_applied + Force
       if (abs(F_applied) >= Ffinal) then
          F_applied = -Ffinal
          sens = -1.d0      ! on change de sens
       endif
     else                   ! decharge
       F_applied = F_applied - Force
       if (abs(F_applied) <= Finitial) then
          F_applied = -Finitial
          sens = 1.d0       ! on change de sens
       endif
     endif
     cpt_1 = 0.d0
   end if
 
   write(cout,'(A,D12.4,A,D12.4)') '--> Fmax = ',Ffinal,' Fmin = ',Finitial
   call logmes(cout)
 
   !fd jolie ce test !! 
   !if (sens)       bdyty(num_up)%Fext(2)    = FORCE * 2**real(cpt_2,8) * L
   !if (.not.sens)  bdyty(num_up)%Fext(2)    = FORCE * 2**real(cpt_2,8) * L
   ! donc on supprime
   bdyty(num_up)%Fext(2)    = F_applied * L

   write(cout,'(A,D12.4)') '-> L = ',L
   call logmes(cout)
   write(cout,'(A,I0,A,D12.4)') '-> F(',num_up,') = ', bdyty(num_up)%Fext(2)
   call logmes(cout)
   write(cout,'(A,I0,A,D12.4)') '-> S(',num_up,') = ', bdyty(num_up)%Fext(2) / L
   call logmes(cout)
   write(cout,'(A,I0,A,D12.4)') '-> time = ',cpt_1,' time step = ', DeltaT
   call logmes(cout)

 end subroutine Oedemetric_test
 
!!!------------------------------------------------------------------------  
  subroutine Biaxial_def_walls(num_up,num_down,epaisseur,sigma,membrana_color_Right,membrana_color_Left)

    ! chargement bi-axiale avec pseudo membrane
    ! on donne parois up et down
    ! on donne l'epaisseur de grains qui entre dans la membrane de chaque cote
    ! on donne la contrainte qu'on souhaite appliquer
    ! on donne des couleurs qu'on va affecter aux grains des membranes droites et gauche 
    
    implicit none
    integer                                 :: num_up,num_down
    real(kind=8)                            :: epaisseur
    real(kind=8)                            :: sigma
    character(len=5)                        :: membrana_color_Right,membrana_color_Left
    logical                                 :: first_time_step=.true.
    real(kind=8)                            :: L
    integer                                 :: i
    real(kind=8),save                       :: Sum_R,Sum_L
    real(kind=8)                            :: Sum_FR,Sum_FL
    real(kind=8)                            :: xmax,xmin
    !
    character(len=80) :: cout
 
    if (first_time_step) then

       xmax  =-1.D20
       xmin  = 1.D20
       Sum_L = 0.D0
       Sum_R = 0.D0
   
       do i=1,nb_RBDY2
          if(.not.bdyty(i)%visible) cycle
          if (i == num_up .or. i == num_down) cycle
          xmax = max(xmax,bdyty(i)%cooref(1)+bdyty(i)%Xbegin(1))
          xmin = min(xmin,bdyty(i)%cooref(1)+bdyty(i)%Xbegin(1))
       end do
   
       do i=1,nb_RBDY2
          if(.not.bdyty(i)%visible) cycle
          if (i == num_up .or. i == num_down) cycle

          if (bdyty(i)%cooref(1)+bdyty(i)%Xbegin(1) < xmin+epaisseur) then
             Sum_L = Sum_L + bdyty(i)%blmty(1)%PLAIN%avr_radius
             bdyty(i)%blmty(1)%behav = membrana_color_Left
             bdyty(i)%blmty(1)%lawnb = get_bulk_behav_nb(bdyty(i)%blmty(1)%behav)
          end if

          if (bdyty(i)%cooref(1)+bdyty(i)%Xbegin(1) > xmax-epaisseur) then
             Sum_R = Sum_R + bdyty(i)%blmty(1)%PLAIN%avr_radius
             bdyty(i)%blmty(1)%behav = membrana_color_Right
             bdyty(i)%blmty(1)%lawnb = get_bulk_behav_nb(bdyty(i)%blmty(1)%behav)
          end if

       end do
       first_time_step = .false.

    end if
    
    L      = 0.D0
    Sum_FR = 0.D0
    Sum_FL = 0.D0
    
    L = (bdyty(num_up  )%cooref(2)+bdyty(num_up  )%Xbegin(2) - bdyty(num_up  )%tacty(1)%BDARY%DATA(2)) - &
        (bdyty(num_down)%cooref(2)+bdyty(num_down)%Xbegin(2) + bdyty(num_down)%tacty(1)%BDARY%DATA(2))

    do i=1,nb_RBDY2
       if(.not.bdyty(i)%visible) cycle
       if (i == num_up .or. i == num_down) cycle
       if (bdyty(i)%blmty(1)%behav == membrana_color_Left) then
          bdyty(i)%Fext(1) =   sigma * L * bdyty(i)%blmty(1)%PLAIN%avr_radius / Sum_L
          Sum_FL = Sum_FL + bdyty(i)%Fext(1)
       end if
       if (bdyty(i)%blmty(1)%behav == membrana_color_Right) then
          bdyty(i)%Fext(1) = - sigma * L * bdyty(i)%blmty(1)%PLAIN%avr_radius / Sum_R
          Sum_FR = Sum_FR + bdyty(i)%Fext(1)
       end if
    end do
 
    ! print*,xmin,xmax
    ! print*, Sum_FR,Sum_FL, SUM_R,Sum_L
    write(cout,'(A,D12.4)') '--> Sigma(right) = ', Sum_FR/L
    call logmes(cout)
    write(cout,'(A,D12.4)') '--> Sigma(left)  = ', Sum_FL/L
    call logmes(cout)
 
  end subroutine Biaxial_def_walls
!!!---------------------------------------------------------------------------
!!!---------------------------------------------------------------------------
   !> \brief set visibility for a given body
   subroutine set_visibility(ibdyty, visibility)

      implicit none

      integer, intent(in) :: ibdyty ! indice du corps considere
      logical, intent(in) :: visibility ! visibilite du corps considere

      ! affectation de sa visibilite au corps considere
      bdyty(ibdyty)%visible = visibility

   end subroutine set_visibility

   !> \brief set visibility for a given list of bodies
   subroutine set_visibility_list(indices, visibilities, nb_bodies)

      implicit none

      ! variables d'entree
      integer, intent(in) :: nb_bodies ! nombre de corps consideres
      integer, dimension(nb_bodies), intent(in) :: indices ! liste des indices des corps
         ! a qui on va atttibuer une visibilite
      logical, dimension(nb_bodies), intent(in) :: visibilities ! visibilite a
         ! attribuer a chaque corps considere

      ! variables locales       12345678901234567890123456
      character(len=26) :: IAM='RBDY2::set_visibility_list'
      integer :: i ! indice de boucle

      ! verification de la coherence de la liste d'indices
     
      ! si les indices commencent avant 1 ou finissent apres nb_RBDY2
      if (minval(indices) < 1 .or. maxval(indices) > nb_RBDY2) then
         ! on affiche un message d'erreur
         call logmes('Error :'//IAM//': inconsistent body indices list!') 
      end if

      ! pour chaque corps de la liste
      do i=1, nb_bodies
         ! on affecte sa visibilite au corps courant
         bdyty(indices(i))%visible = visibilities(i)
      end do

   end subroutine set_visibility_list

  subroutine set_invisible(nb_bdy,list_bdy)
    
    implicit none
    integer,intent(in) :: nb_bdy
    integer,dimension(nb_bdy),intent(in) :: list_bdy
    integer :: i
    !                          123456789012345678901234 
    character(len=24)  :: IAM='mod_RBDY2::set_invisible'

    ! on rend invisibles tous les corps de la liste
    call set_visibility_list(list_bdy, (/ (.false., i=1, nb_bdy) /), nb_bdy)

  end subroutine set_invisible

  subroutine set_visible(nb_bdy,list_bdy)

     implicit none

    integer,intent(in) :: nb_bdy
    integer,dimension(nb_bdy),intent(in) :: list_bdy
    integer :: i
    !                          1234567890123456789012 
    character(len=22)  :: IAM='mod_RBDY2::set_visible'

    ! on rend visibles tous les corps de la liste
    call set_visibility_list(list_bdy, (/ (.true., i=1, nb_bdy) /), nb_bdy)

  end subroutine
!!!------------------------------------------------------------------------  
  subroutine shear_def_walls(thickness,lenght,sigma,mcolor)

    ! Sheared sample using a pseudo membrane
    
    implicit none
    integer                :: ibdyty
    real(kind=8)           :: sigma,thickness,lenght
    character(len=5)       :: mcolor
    logical                :: first_time_step=.true.
    real(kind=8),save      :: MSUM
    real(kind=8)           :: YMAX,Yi
 
    if (first_time_step) then

       YMAX =-1.D20
       MSUM = 0.D0

       do ibdyty = 1,nb_RBDY2
          Yi = bdyty(ibdyty)%cooref(2)+bdyty(ibdyty)%Xbegin(2)
          YMAX = max(YMAX,Yi)
       end do
   
       do ibdyty = 1,nb_RBDY2

          if (bdyty(ibdyty)%cooref(2)+bdyty(ibdyty)%Xbegin(2) < YMAX - thickness ) cycle

          MSUM = MSUM + bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius
          bdyty(ibdyty)%blmty(1)%behav = mcolor

       end do

       first_time_step = .false.

    end if
    
    do ibdyty=1,nb_RBDY2
 
       if (bdyty(ibdyty)%blmty(1)%behav .ne. mcolor ) cycle
       
       bdyty(ibdyty)%Fext(2) =   - sigma * lenght * bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius / MSUM

    end do
 
  end subroutine Shear_def_walls
!!!---------------------------------------------------------------------------


  !> fixing some geometric quantities for interpolation
  subroutine set_vcooref_RBDY2(ibdyty,nbv,cooref)
    !
    ! routine destinee a deposer les vertex d un polyg
    !
    implicit none
    integer      :: ibdyty,nbv,i
    real(kind=8),dimension(2,nbv) :: cooref
    real(kind=8) :: Lx,Ly
    character(len=80) :: cout
    character(len=18) :: IAM
          !123456789012345678
    IAM = 'RBDY2::set_vcooref'

    if (nbv /=1 .and. nbv /= 4) then
      write(cout,'(A,1x,I0)') 'unexpected number of vertex',nbv
      call faterr(IAM,cout)
    endif

    if (associated(bdyty(ibdyty)%blmty(1)%PLAIN%cooref) .or. &
        associated(bdyty(ibdyty)%blmty(1)%PLAIN%dilat)      .or. &
        associated(bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat)) then
      call faterr(IAM,'thermal array already allocated !?')
    end if    

    allocate(bdyty(ibdyty)%blmty(1)%PLAIN%cooref(2,nbv), &
             bdyty(ibdyty)%blmty(1)%PLAIN%dilat(nbv),        &
             bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(nbv))


    Lx = 0.d0; Ly = 0.d0

    if (nbv > 1) then
      do i=2,nbv
        Lx = max(Lx,abs(cooref(1,i) - cooref(1,1)))
        Ly = max(Ly,abs(cooref(2,i) - cooref(2,1)))
      enddo
      if (Lx == 0.d0 .or. Ly == 0.d0) then
        write(cout,'(A,1x,I0,2(1x,D14.7))') 'improbable size of the brick', ibdyty, Lx, Ly
        call faterr(IAM,cout)
      endif
      bdyty(ibdyty)%blmty(1)%PLAIN%ihalflx = 2.d0/Lx
      bdyty(ibdyty)%blmty(1)%PLAIN%ihalfly = 2.d0/Ly
    endif

    bdyty(ibdyty)%blmty(1)%PLAIN%cooref  = cooref
    bdyty(ibdyty)%blmty(1)%PLAIN%dilat  = 0.d0 
    bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat = 0.d0 

  end subroutine set_vcooref_RBDY2


 ! oldies for constant T

!!!------------------------------------------------------------------------ 
!!$  SUBROUTINE get_Vth(ibdyty,P_cooref,V)
!!$    !
!!$    ! routine destinee a calculer la vitesse
!!$    ! d'un point de coordonnees initiales P_cooref 
!!$    ! due a l'increment de dilatation thermique
!!$    !
!!$    IMPLICIT NONE
!!$    INTEGER      :: ibdyty
!!$    REAL(kind=8) :: V0x,V0y,a,c,s
!!$    REAL(kind=8),DIMENSION(2):: P_cooref,V
!!$
!!$    ! calcul de la vitesse dans le repere initial
!!$
!!$    V0x = bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat*P_cooref(1)/H
!!$    V0y = bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat*P_cooref(2)/H
!!$
!!$    ! calcul de la rotation du solide
!!$
!!$    a = bdyty(ibdyty)%cooref(3) + & 
!!$        bdyty(ibdyty)%Xbegin(3) + &
!!$        (1.D0-THETA)*H*bdyty(ibdyty)%Vbegin(3)
!!$
!!$    c=COS(a);s=SIN(a)
!!$
!!$    ! on tourne la vitesse dans le repere actuel
!!$
!!$    V(1) =  c*V0x - s*V0y
!!$    V(2) =  s*V0x + c*V0y
!!$
!!$  END SUBROUTINE get_Vth
!!$!!!------------------------------------------------------------------------ 
!!$  SUBROUTINE get_Xth(ibdyty,P_cooref,Xth)
!!$    !
!!$    ! routine destinee a calculer le deplacement total 
!!$    ! d'un point de coordonnees initiales P_cooref
!!$    ! du a l'increment de dilatation thermique
!!$    !
!!$    IMPLICIT NONE
!!$    INTEGER      :: ibdyty
!!$    REAL(kind=8),DIMENSION(2):: P_cooref,Xth
!!$
!!$    Xth(1) = bdyty(ibdyty)%blmty(1)%PLAIN%dilat*P_cooref(1)
!!$    Xth(2) = bdyty(ibdyty)%blmty(1)%PLAIN%dilat*P_cooref(2)
!!$
!!$  END SUBROUTINE get_Xth

  !------------------------------------------------------------------------ 
  subroutine get_Vth(ibdyty,P_cooref,V)
    !
    ! routine destinee a calculer la vitesse
    ! d'un point de coordonnees initiales P_cooref 
    ! due a l'increment de dilatation thermique
    !
    implicit none
    integer      :: ibdyty
    real(kind=8) :: V0x,V0y,a,c,s
    real(kind=8),dimension(2):: P_cooref,V

    real(kind=8) :: xi,eta,X,Y,Tp,Tm

    if (.not. associated(bdyty(ibdyty)%blmty(1)%PLAIN%dilat)) then
      V=0.d0
      return
    endif

    !fd pas de vertex on a une T constante

    if (size(bdyty(ibdyty)%blmty(1)%PLAIN%dilat) == 1) then 

      ! calcul de la vitesse dans le repere initial

      V0x = bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(1)*P_cooref(1)/H
      V0y = bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(1)*P_cooref(2)/H

    else

    !fd travail de porc ... c'est necessairement un polyg a 4 sommets 
    ! avec la numerotation :  4 - 3    
    !                         |   |
    !                         1 - 2

      X   = P_cooref(1)  !! - bdyty(ibdyty)%cooref(1) 
      Y   = P_cooref(2)  !! - bdyty(ibdyty)%cooref(2)
      xi  = X*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLx
      eta = Y*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLy

!      write(6,'(I5,4(1x,D14.7))'),ibdyty,bdyty(ibdyty)%blmty(1)%PLAIN%dT(1:4)
!      write(6,'(2(1x,D14.7))') xi,eta 

      Tm =  (0.5 * (1.d0 -eta) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(1)) + &
            (0.5 * (1.d0 +eta) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(4))
      Tp =  (0.5 * (1.d0 -eta) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(2)) + &
            (0.5 * (1.d0 +eta) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(3))

      V0x = ((0.5 * Tm *(X -(0.5*X*X*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLx))) + &
             (0.5 * Tp *(X +(0.5*X*X*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLx))))/H

!      write(6,'(3(1x,A2,1x,D14.7))') 'T-',Tm,'T+',Tp,'Vx',V0x 


      Tm =  (0.5 * (1.d0 -xi) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(1)) + &
            (0.5 * (1.d0 +xi) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(2))
      Tp =  (0.5 * (1.d0 -xi) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(4)) + &
            (0.5 * (1.d0 +xi) * bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat(3))

      V0y = ((0.5 * Tm *(Y -(0.5*Y*Y*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLy))) + &
             (0.5 * Tp *(Y +(0.5*Y*Y*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLy))))/H

!      write(6,'(3(1x,A2,1x,D14.7))') 'T-',Tm,'T+',Tp,'Vy',V0y 

    endif

    ! calcul de la rotation du solide

    a = bdyty(ibdyty)%cooref(3) + & 
        bdyty(ibdyty)%Xbegin(3) + &
        (1.D0-THETA)*H*bdyty(ibdyty)%Vbegin(3)

    c=cos(a);s=sin(a)

    ! on tourne la vitesse dans le repere actuel

    V(1) =  c*V0x - s*V0y
    V(2) =  s*V0x + c*V0y

!    write(6,'(2(1x,D14.7))') V(1),V(2)

  end subroutine get_Vth
  !------------------------------------------------------------------------ 
  !------------------------------------------------------------------------ 
  subroutine get_Xth(ibdyty,P_cooref,Xth)
    !
    ! routine destinee a calculer le deplacement total 
    ! d'un point de coordonnees initiales P_cooref (dans le repere du centre d'inertie)
    ! du a l'increment de dilatation thermique
    !
    implicit none
    integer      :: ibdyty
    real(kind=8),dimension(2):: P_cooref,Xth

    real(kind=8) :: xi,eta,X,Y,Tp,Tm


    if (.not. associated(bdyty(ibdyty)%blmty(1)%PLAIN%dilat)) then
      Xth=0.d0
      return
    endif


    !fd pas de vertex on a une T constante

    if (size(bdyty(ibdyty)%blmty(1)%PLAIN%dilat) == 1) then 

      Xth(1) = bdyty(ibdyty)%blmty(1)%PLAIN%dilat(1)*P_cooref(1)
      Xth(2) = bdyty(ibdyty)%blmty(1)%PLAIN%dilat(1)*P_cooref(2)
   else
    !fd travail de porc ... c'est necessairement un polyg a 4 sommets 
    ! avec la numerotation :  4 - 3    
    !                         |   |
    !                         1 - 2

!      print*,'brick: ',ibdyty
 
      X   = P_cooref(1) !! - bdyty(ibdyty)%cooref(1) 
      Y   = P_cooref(2) !! - bdyty(ibdyty)%cooref(2)

!      print*,'coordonnees du pc dans RG',X,Y
    
      xi  = X*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLx
      eta = Y*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLy

!      print*,'coordonnees reduites du pc dans RG',xi,eta

      Tm =  (0.5 * (1.d0 -eta) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(1)) + &
            (0.5 * (1.d0 +eta) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(4))
      Tp =  (0.5 * (1.d0 -eta) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(2)) + &
            (0.5 * (1.d0 +eta) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(3))

!      print*,'suivant x temperatures pour l''interpolation',Tm,Tp


      Xth(1) = (0.5 * Tm *(X -(0.5*X*X*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLx))) + &
               (0.5 * Tp *(X +(0.5*X*X*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLx)))

!      print*,'suivant x deplacement',Xth(1)


      Tm =  (0.5 * (1.d0 -xi) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(1)) + &
            (0.5 * (1.d0 +xi) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(2))
      Tp =  (0.5 * (1.d0 -xi) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(4)) + &
            (0.5 * (1.d0 +xi) * bdyty(ibdyty)%blmty(1)%PLAIN%dilat(3))

!      print*,'suivant y temperatures pour l''interpolation',Tm,Tp

      Xth(2) = (0.5 * Tm *(Y -(0.5*Y*Y*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLy))) + &
               (0.5 * Tp *(Y +(0.5*Y*Y*bdyty(ibdyty)%blmty(1)%PLAIN%iHalfLy)))

!      print*,'suivant y deplacement',Xth(2)
   endif

  end subroutine get_Xth
  !------------------------------------------------------------------------ 
  subroutine update_dilatation
    implicit none 
    integer :: ibdyty

    do ibdyty=1,nb_RBDY2

      if ( .not. associated(bdyty(ibdyty)%blmty(1)%PLAIN%dilat) ) cycle

      bdyty(ibdyty)%blmty(1)%PLAIN%dilat=bdyty(ibdyty)%blmty(1)%PLAIN%dilat + & 
                                         bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat

    end do
  end subroutine update_dilatation
  !------------------------------------------------------------------------ 
  subroutine set_dilatation_increment(ibdyty,dT,inc_dilat)
    implicit none 
    integer      :: ibdyty !< body number
    real(kind=8) :: dT !< mean tempertaure over the body
    real(kind=8) :: inc_dilat(:) !< thermal dilatation increment

    if (size(inc_dilat) /= size(bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat)) then
      call faterr('RBDY2::set_dilatation_increment','non conforming shapes')
    endif
    bdyty(ibdyty)%blmty(1)%PLAIN%inc_dilat = inc_dilat

    bdyty(ibdyty)%T = bdyty(ibdyty)%T + dT

  end subroutine set_dilatation_increment

  !------------------------------------------------------------------------  
  subroutine get_drv_vlocy_RBDY2(ibdyty, indices, values)
  !permet d'extraire les vitesses imposees pour chaque corps ibdytyd 
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4), dimension(:), pointer :: indices
    real(kind=8)   , dimension(:), pointer :: values
    !
    integer(kind=4) :: i, nb_drvdof
    real(kind=8)    :: Vbegind

    if( nb_RBDY2<1 ) return

    nb_drvdof = bdyty(ibdyty)%nb_vlocy_driven_dof
    if( nb_drvdof==0 ) return

    allocate( indices(nb_drvdof), values(nb_drvdof) )

    do i = 1, nb_drvdof
      indices(i) = bdyty(ibdyty)%vlocy_driven_dof(i)%dofnb
      call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(i),Vbegind,values(i))
    end do

  end subroutine

  subroutine comp_drv_vlocy_RBDY2(ibdyty, values)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    real(kind=8), dimension(:)  :: values
    !
    integer(kind=4)   :: i
    real(kind=8)      :: Vbegind
    character(len=27) :: IAM
    !      123456789012345678901234567
    IAM = 'RBDY2::comp_drv_vlocy_RBDY2'

    if( size(values) /= bdyty(ibdyty)%nb_vlocy_driven_dof ) then
      call faterr(IAM,'wrong size')
    endif

    do i=1, size(values)
      call comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(i), Vbegind,values(i))
    end do
  end subroutine

!!!------------------------------------------------------------------------

  !> compute the new position of grains for a given velocity
  subroutine comp_coor_4all_RBDY2

    implicit none 
    integer :: ibdyty
    character(len=34) :: IAM
          !123456789012345678901234
    IAM = 'RBDY2::compute_coor_4all'
    
    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    

          if( .not. bdyty(ibdyty)%visible) cycle

          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin +  &
                            (1.d0-THETA)*H*bdyty(ibdyty)%Vbegin +  &
                            THETA*H*bdyty(ibdyty)%V 

       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_GEAR)
       call faterr(IAM,'method not supported with this INTEGRATOR')

    case(INTEGRATOR_VERLET)    

       call faterr(IAM,'method not supported with this INTEGRATOR')

    case DEFAULT
       call faterr(IAM,'INTEGRATOR NOT SUPPORTED YET!')
       
    end select

    if (PERIODIC) call check_periodic
    if (BOUNDS)  call out_of_bounds_RBDY2

  end subroutine 

  !> compute the new velocity of grains for a given contact reaction
  subroutine comp_vlocy_4all_RBDY2

    implicit none 
    integer :: ibdyty
    character(len=35) :: IAM
          !1234567890123456789012345
    IAM = 'RBDY2::compute_vlocy_4all'
    
    select case(M_INTEGRATOR_ID)
    case(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       do ibdyty=1,nb_RBDY2    

          if( .not. bdyty(ibdyty)%visible) cycle

          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + &
                            bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%Ireac

          if ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) cycle

          call apply_vlocy_driven_dof(ibdyty,iV____)

       end do
       !$OMP END DO
       !$OMP END PARALLEL
    case(INTEGRATOR_GEAR)
       call faterr(IAM,'method not supported with this INTEGRATOR')

    case(INTEGRATOR_VERLET)    

       call faterr(IAM,'method not supported with this INTEGRATOR')

    case DEFAULT
       call faterr(IAM,'INTEGRATOR NOT SUPPORTED YET!')
       
    end select

    if (PERIODIC) call check_periodic
    if (BOUNDS)  call out_of_bounds_RBDY2

  end subroutine comp_vlocy_4all_RBDY2
!!!-------------------------------------------------------------------------------
!!! mr experimental
!!!
  subroutine get_stress_RBDY2(ibdyty,SIGMA)

    implicit none
    integer(kind=4)             :: ibdyty
    real(kind=8),dimension(2,2) :: SIGMA

    SIGMA = bdyty(ibdyty)%SIGMA

  end subroutine get_stress_RBDY2
!!!-------------------------------------------------------------------------------
  subroutine initialize_stress()
    implicit none
    integer(kind=4) :: ibdyty

    do ibdyty=1,nb_RBDY2
       bdyty(ibdyty)%SIGMA = 0.d0
    end do

  end subroutine initialize_stress
!!!-------------------------------------------------------------------------------
  subroutine add_stress(ibdyty,SIGMA)

    implicit none
    integer(kind=4)             :: ibdyty
    real(kind=8),dimension(2,2) :: SIGMA

    bdyty(ibdyty)%SIGMA = bdyty(ibdyty)%SIGMA + SIGMA

  end subroutine add_stress

! vv: to skip invisible bodies in write_bodies and write_out_dof
!!!------------------------------------------------------------------------ 
  SUBROUTINE set_skip_invisible_RBDY2

    IMPLICIT NONE

    skip_invisible = .TRUE.

  END SUBROUTINE set_skip_invisible_RBDY2
!!!------------------------------------------------------------------------ 
!!!-------------------------------------------------------------------------------
!fd TODO clarifier cette merde
!fd j'imagine que ces belles fonctions non documentees 
!   sont la pour rechercher les grains les plus hauts 
!   appartenant a une bande horizontale discretisee regulierement
!   ca doit servir pour le calcul de la convection avec les modeles thermiques. 
!   CECI DIT je me demande ce que ca fait la. Apres tout ca pourrait etre gere dans le module MP.
!!!-------------------------------------------------------------------------------
  logical function IS_IN_THE_FREE_BOUNDARY(ibdyty,itacty)
    
    implicit none
    integer(kind=4) :: ibdyty,itacty

    IS_IN_THE_FREE_BOUNDARY = bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY

  end function IS_IN_THE_FREE_BOUNDARY
!!!-------------------------------------------------------------------------------
  subroutine init_free_boundary_RBDY2(XMIN,XMAX,DX)

    implicit none
    integer(kind=4)           :: ibdyty,itacty,ix,ifb
    integer(kind=4)           :: nb_tacty
    real(kind=8)              :: XMIN,XMAX,DX,DEN
    real(kind=8),dimension(2) :: coor

    FREE_BOUNDARY = .true.

    FB_NX = 1+int((XMAX-XMIN)/DX)

    FB_XMIN = XMIN
    FB_XMAX = XMAX
    FB_DX   = DX

    DEN = 1.0/(XMAX-XMIN)

    if(allocated(FREE_SURFACE)) deallocate(FREE_SURFACE)
    allocate(FREE_SURFACE(FB_NX))

    do ix = 1,FB_NX
       FREE_SURFACE(ix)%ID_RBDY2 = 0
       FREE_SURFACE(ix)%ID_TACTY = 0
       FREE_SURFACE(ix)%YMAX     =-1D+24
       FREE_SURFACE(ix)%ACTIVE   = .true.
    end do

    do ibdyty=1,nb_RBDY2

       nb_tacty = size(bdyty(ibdyty)%tacty)

       do itacty=1,nb_tacty
          coor = get_coor(ibdyty,itacty)

          ifb = int(FB_NX*(coor(1)-XMIN)*DEN) + 1

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY = .false.

          if( FREE_SURFACE(ifb)%YMAX .gt. coor(2) ) cycle

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY = .true.

          FREE_SURFACE(ifb)%YMAX     = coor(2)
          FREE_SURFACE(ifb)%ID_RBDY2 = ibdyty
          FREE_SURFACE(ifb)%ID_TACTY = itacty
          FREE_SURFACE(ifb)%ACTIVE   = .true.
       end do

    end do

  end subroutine init_free_boundary_RBDY2
!-------------------------------------------------------------------------------
  subroutine free_boundary_computation()

    implicit none
    integer(kind=4)           :: itacty,ibdyty,ifb
    real(kind=8)              :: DEN
    integer(kind=4)           :: nb_tacty
    real(kind=8),dimension(2) :: coor
    
    DEN = 1.0/(FB_XMAX-FB_XMIN)

    do ifb = 1,FB_NX
       FREE_SURFACE(ifb)%YMAX = -1D+24
       FREE_SURFACE(ifb)%ID_RBDY2 = 0
       FREE_SURFACE(ifb)%ID_TACTY = 0
       FREE_SURFACE(ifb)%ACTIVE   = .true.
    end do

    do ibdyty=1,nb_RBDY2

       nb_tacty = size(bdyty(ibdyty)%tacty)

       do itacty=1,nb_tacty

          coor = get_coor(ibdyty,itacty)
          ifb = int(FB_NX*(coor(1)-FB_XMIN)*DEN) + 1

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY = .false.

          if( FREE_SURFACE(ifb)%YMAX .gt. coor(2) ) cycle

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY  = .true.

          FREE_SURFACE(ifb)%YMAX     = coor(2)
          FREE_SURFACE(ifb)%ID_RBDY2 = ibdyty
          FREE_SURFACE(ifb)%ID_TACTY = itacty
          FREE_SURFACE(ifb)%ACTIVE   = .true.

       end do

    end do

  end subroutine free_boundary_computation
!!!------------------------------------------------------------------------
  subroutine set_surface_sectors(nb)
    implicit none
    integer(kind=4),intent(in) :: nb !> nb is the number of sector divided by 2

    nb_WSsect = 2*nb

  end subroutine set_surface_sectors
!!!------------------------------------------------------------------------
  integer(kind=4) function get_surface_sectors()
    implicit none

    get_surface_sectors = nb_WSsect

  end function get_surface_sectors
!!!------------------------------------------------------------------------
  subroutine get_density(ibdyty, density)
    implicit none
    integer      :: ibdyty
    real(kind=8) :: density

    density = get_rho( bdyty(ibdyty)%blmty(1)%lawnb )
  end subroutine

  subroutine set_mass(ibdyty, mass)

    implicit none
    integer(KIND=4), intent(IN) :: ibdyty

    real(KIND=8), intent(IN) :: mass
    !
    integer      :: iblmty,itacty,ibehav,iccdof,ivd,i
    real(kind=8) :: Ummass,mass1,mass2,mass3,mass4,avr_radiuss,gyr_radiuss,radius0

    character(len=80)  :: cout 

    do iblmty=1,size(bdyty(ibdyty)%blmty)
       !ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb
       select case(bdyty(ibdyty)%blmty(iblmty)%blmID)
       case('PLAIN')
          !Ummass=get_rho(ibehav)
          avr_radiuss=bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius
          gyr_radiuss=bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius
       case default
          call LOGMES('you try to compute the mass of an unknown blmty')
       end select
    end do

    mass1=mass
    mass2=mass1
    mass3=mass1*gyr_radiuss*gyr_radiuss

    do iccdof=1,size(bdyty(ibdyty)%V)

       ! computing ordinary rigid body masses
       if (iccdof == 1) bdyty(ibdyty)%mass(iccdof)=mass1
       if (iccdof == 2) bdyty(ibdyty)%mass(iccdof)=mass2
       if (iccdof == 3) bdyty(ibdyty)%mass(iccdof)=mass3
       ! computing extra degrees of freedom masses
       if (iccdof == 4) then
          select case(get_nodID(bdyty(ibdyty)%nodty))
          case(i_NO4xx)
             !mj NO4Px, pneumatic node.
             !
             !am: cf comp_mass_RBDY2 pour la suite des propos de mj
             if (size(bdyty(ibdyty)%tacty) .gt. 1) then
                call LOGMES('DISPx or xPSID and some other contactors are attached to a same body.')
                call LOGMES('Extension not supported, DISPx or xPSID should be unique boundary of body.')
                write(cout,'(A15,I7)')'Faulty body is ',ibdyty
                call faterr('RBDY2::set_mass',cout)
             end if
             mass4=mass1
             bdyty(ibdyty)%mass(iccdof)=mass4 
          case default
             call LOGMES('you try to compute the extra mass of an unknown nodty')
          end select
       end if

       if (bdyty(ibdyty)%mass(iccdof) > 1.D-20) then
          bdyty(ibdyty)%inv_mass(iccdof)=1.D0/bdyty(ibdyty)%mass(iccdof)
       else
          do i=1, size(bdyty(ibdyty)%tacty)
             if (bdyty(ibdyty)%tacty(i)%tacID/='PT2Dx') then  
                call LOGMES('WARNING: Very small mass term')
                write(cout,'(A6,1X,I5,A5,1X,I5,A6,1X,D14.7)') 'rbdy2: ',ibdyty,' ddl:',iccdof,' mass:',bdyty(ibdyty)%mass(iccdof)
                call LOGMES(cout)
                call LOGMES('arbitrary value is taken for inv_mass (1.d+20)')
             end if
          end do
          bdyty(ibdyty)%inv_mass(iccdof) = 1.D20

       endif

    end do

    if (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) return

    ! nullifying inv_mass where degrees of freedom are driven
    do ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
       iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
       bdyty(ibdyty)%inv_mass(iccdof)=0.D0
    end do

  end subroutine set_mass

!------------------------------------------------------------------------
!am : nouvelles fonctions pour rendre possible un calcul multi-domaines sequentiel
!     dans le cadre de l'architecture actuelle
!------------------------------------------------------------------------

   !> \brief copy several times read bodies and store copied bodies at the end of the bodies container
   !>        Warning: the number of RBDY2 is altered by this subroutine
   subroutine copy_bodies_RBDY2(factor)

      implicit none

      ! variable d'entree
      integer, intent(in) :: factor !< [in] total number of copies in the bodies container after
                                    !< the calling, i.e. each body is duplicated factor - 1 times

      ! variables locales
      integer :: nb_copies ! nombfe de copies a realiser
      integer :: new_ibdyty ! indice ou stocker la copie courante du corps courant
      integer :: nb_bulks  ! nombre de bulks du corps courant
      integer :: nb_tacts  ! nombre de contacteurs du corps courant
      integer :: size_data ! taille du tableau data du contacteur courant
      integer :: size_idata ! taille du tableau idata du contacteur courant
      integer :: nbdof ! nombre de dof pour un noeud
      integer :: errare ! pour recuperer le code d'erreur renvoye par une llocation memoire
      integer :: icopy ! indice de boucle sur les copies
      integer :: ibdyty ! indice de boucle sur les corps
      integer :: iblmty ! indice de boucle sur les bulks
      integer :: itacty ! indice de boucle sur les contacteurs

      !                         123456789012345678         
      character(len=18) :: IAM='RBDY2::copy_bodies'

      ! verification de la coherence des donnees :

      ! si le facteur est plus petit que 1
      if (factor < 1) then
         ! on affiche un message d'erreur
         call logmes('Errror '//IAM// ": factor must be reater then one!")
      end if

      ! si le tableau des corps ne peut contenir toutes les copies
      if (nb_RBDY2*factor > size(bdyty)) then
         ! on affiche un message d'erreur
         call logmes('Error '//IAM// ": bdyty is too small!")
      end if

      ! on calcule le nombre de copies a realiser
      nb_copies = factor - 1

      ! pour chaque copie a realiser
      do icopy=1, nb_copies
         ! pour chaque corps
         do ibdyty=1, nb_RBDY2
            ! on calcule l'indice ou stocker la copie courante du corps courant
            new_ibdyty = icopy*nb_RBDY2 + ibdyty
            ! on copie l'identifiant du corps courant
            bdyty(new_ibdyty)%bdyID = bdyty(ibdyty)%bdyID
 
            !
            ! copie des bulks
            ! 

            ! on recupere le nombre de bulks du corps courant
            nb_bulks = size(bdyty(ibdyty)%blmty)
            ! on alloue l'espace memoire pour stocker les bulks de la copie
            ! du corps courant
            allocate(bdyty(new_ibdyty)%blmty(nb_bulks), stat=errare)
            ! si l'allocation a echoue
            if (errare /= 0) then
               ! on affiche un message d'erreur
               call logmes('Error '//IAM//': error allocating bdyty%blmty')
            end if

            ! pour chaque bulk du corps courant 
            do iblmty=1, nb_bulks
               ! on copie :
               !   * l'identifiant du bulk 
               bdyty(new_ibdyty)%blmty(iblmty)%blmID = bdyty(ibdyty)%blmty(iblmty)%blmID
               !   * le surnom du materiau volumique
               bdyty(new_ibdyty)%blmty(iblmty)%behav = bdyty(ibdyty)%blmty(iblmty)%behav
               ! si le bulk est de type PLAIN
               if (bdyty(new_ibdyty)%blmty(iblmty)%blmID == 'PLAIN') then
                  ! on copie :
                  !   * l'average radius
                  bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%avr_radius = bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius
                  !   * le gyration radius
                  bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%gyr_radius = bdyty(ibdyty)%blmty(iblmty)%PLAIN%gyr_radius
                  ! on initialise a nul les pointeurs
                  nullify(bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%inc_dilat, &
                          bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%dilat, &
                          bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%cooref)
               ! sinon
               else
                  ! on affiche un message d'erreur
                  call logmes('Error '//IAM//': error only PLAIN bulks are supported!')
               end if
            end do

            !
            ! copie du noeud
            ! 

            ! on copie l'identifiant du noeud du corps courant
            call new_nodty(bdyty(new_ibdyty)%nodty, get_nodNAME(bdyty(ibdyty)%nodty))

            ! on recupere le nombre dof en fonction du type de neoud
            nbdof = nbdof_a_nodty(bdyty(new_ibdyty)%nodty)

            ! on alloue l'espace memoire pour stocker la copie des champs porte par le neoud du corps courant
            !   * les coordonnes de reference
            allocate(bdyty(new_ibdyty)%cooref(nbdof), stat=errare)
            !   * les coordonnes a la fin du pas de temps
            allocate(bdyty(new_ibdyty)%coor(nbdof), stat=errare)
            !   * le vecteur deplacement au debut du pas
            allocate(bdyty(new_ibdyty)%Xbegin(nbdof), stat=errare)
            !   * le vecteur deplacement a la fin du pas
            allocate(bdyty(new_ibdyty)%X(nbdof), stat=errare)
            !   * le vecteur vitesse au debut du pas
            allocate(bdyty(new_ibdyty)%Vbegin(nbdof), stat=errare)
            !   * le vecteur vitesse a la fin du pas
            allocate(bdyty(new_ibdyty)%V(nbdof), stat=errare)
            !   * le vecteur vitesse libre
            allocate(bdyty(new_ibdyty)%Vfree(nbdof), stat=errare)
            !   * le vecteur vitesse auxiliaire
            allocate(bdyty(new_ibdyty)%Vaux(nbdof), stat=errare)
            !   * le torseur des forces exterieures
            allocate(bdyty(new_ibdyty)%Fext(nbdof), stat=errare)
            !   * le torseur des forces interieures
            allocate(bdyty(new_ibdyty)%Fint(nbdof), stat=errare)
            !   * le torseur des impulsions de contact sur le pas de temps
            allocate(bdyty(new_ibdyty)%Ireac(nbdof), stat=errare)
            !   * le torseur des impulsions de contact auxiliaire
            allocate(bdyty(new_ibdyty)%Iaux(nbdof), stat=errare)
            !   * la matrice de masse (stockee comme une matrice diagonale)
            allocate(bdyty(new_ibdyty)%mass(nbdof), stat=errare)
            !   * la matrice de masse (stockee comme une matrice diagonale)
            allocate(bdyty(new_ibdyty)%inv_mass(nbdof), stat=errare)
            ! si on utilise la methode Smooth DEM
            if (smooth_method) then
               ! on alloue les champs ad hoc
               allocate(bdyty(new_ibdyty)%A(nbdof), stat=errare)
               allocate(bdyty(new_ibdyty)%Abegin(nbdof), stat=errare)
               allocate(bdyty(new_ibdyty)%B(nbdof), stat=errare)
               allocate(bdyty(new_ibdyty)%Bbegin(nbdof), stat=errare)
               allocate(bdyty(new_ibdyty)%C(nbdof), stat=errare)
               allocate(bdyty(new_ibdyty)%Cbegin(nbdof), stat=errare)
            ! sinon,
            else
               ! on initialise les pointeurs a nul
               nullify(bdyty(new_ibdyty)%A)
               nullify(bdyty(new_ibdyty)%Abegin)
               nullify(bdyty(new_ibdyty)%B)
               nullify(bdyty(new_ibdyty)%Bbegin)
               nullify(bdyty(new_ibdyty)%C)
               nullify(bdyty(new_ibdyty)%Cbegin)
            end if
            ! si l'allocation a echoue
            if (errare /= 0) then
               ! on affiche un message d'erreur
               call logmes('Error '//IAM// ': error allocating bdyty%cooref, bdyty%coor, bdyty%X, bdyty%V, ...')
            end if

            ! on copie :
            !   * les coordonnees du noeud
            bdyty(new_ibdyty)%cooref = bdyty(ibdyty)%cooref
            bdyty(new_ibdyty)%coor   = bdyty(ibdyty)%coor
            !   * les deplacements
            bdyty(new_ibdyty)%Xbegin = bdyty(ibdyty)%Xbegin
            bdyty(new_ibdyty)%X      = bdyty(ibdyty)%X
            !   * les vitesses
            bdyty(new_ibdyty)%Vbegin = bdyty(ibdyty)%Vbegin
            bdyty(new_ibdyty)%V      = bdyty(ibdyty)%V
            bdyty(new_ibdyty)%Vfree  = bdyty(ibdyty)%Vfree
            bdyty(new_ibdyty)%Vaux   = bdyty(ibdyty)%Vaux
            !   * les forces exterieures
            bdyty(new_ibdyty)%Fext   = bdyty(ibdyty)%Fext
            !   * les forces interieures
            bdyty(new_ibdyty)%Fint   = bdyty(ibdyty)%Fint
            !   * les impulsions de contact
            bdyty(new_ibdyty)%Ireac  = bdyty(ibdyty)%Ireac
            bdyty(new_ibdyty)%Iaux   = bdyty(ibdyty)%Iaux
            !   * la matrice de masse
            bdyty(new_ibdyty)%mass   = bdyty(ibdyty)%mass
            !   * l'inverse de la matrice de masse
            bdyty(new_ibdyty)%inv_mass = bdyty(ibdyty)%inv_mass
            ! si on utilise la methode Smooth DEM
            if (smooth_method) then
               ! on copie les champs ad hoc
               bdyty(new_ibdyty)%Abegin = bdyty(ibdyty)%Abegin
               bdyty(new_ibdyty)%A      = bdyty(ibdyty)%A
               bdyty(new_ibdyty)%Bbegin = bdyty(ibdyty)%Bbegin
               bdyty(new_ibdyty)%B      = bdyty(ibdyty)%B
               bdyty(new_ibdyty)%Cbegin = bdyty(ibdyty)%Cbegin
               bdyty(new_ibdyty)%C      = bdyty(ibdyty)%C
            end if

            ! gestion des C.L. imposees
            ! N.B.: les C.I. ont deja ete prises en commpte via Xbegin et Vbegin

            ! on copie le nombre de dof imposes :
            !   * en vitesse
            bdyty(new_ibdyty)%nb_vlocy_driven_dof = bdyty(ibdyty)%nb_vlocy_driven_dof
            !   * en force
            bdyty(new_ibdyty)%nb_force_driven_dof = bdyty(ibdyty)%nb_force_driven_dof

            ! s'il ya des conditions limites en vitesse
            if (bdyty(new_ibdyty)%nb_vlocy_driven_dof > 0) then
               ! on alloue l'esapce memoire pour stocker
               !   * les indices des dofs imposes en vitesse
               allocate(bdyty(new_ibdyty)%vlocy_driven_dof(bdyty(new_ibdyty)%nb_vlocy_driven_dof))
               !   * les vitesses imposees
               allocate(bdyty(new_ibdyty)%Vdriv(bdyty(new_ibdyty)%nb_vlocy_driven_dof))
               !   * les deplacements imposes
               allocate(bdyty(new_ibdyty)%Xdriv(bdyty(new_ibdyty)%nb_vlocy_driven_dof))
               ! si l'allocation a echoue
               if (errare /= 0) then
                  ! on affiche un message d'erreur
                  call logmes('Error '//IAM// ': error allocating bdyty%Vdriv, bdyty%Xdriv, ...')
               end if

               ! on copie les C.L. en vitesse
               bdyty(new_ibdyty)%vlocy_driven_dof = bdyty(ibdyty)%vlocy_driven_dof
               bdyty(new_ibdyty)%Vdriv            = bdyty(ibdyty)%Vdriv
               bdyty(new_ibdyty)%Xdriv            = bdyty(ibdyty)%Xdriv
            ! sinon,
            else
               ! on initialise les pointeurs a nul
               nullify(bdyty(new_ibdyty)%vlocy_driven_dof)
               nullify(bdyty(new_ibdyty)%Vdriv)
               nullify(bdyty(new_ibdyty)%Xdriv)
            end if
            ! s'il ya des conditions limites en force
            if (bdyty(new_ibdyty)%nb_force_driven_dof > 0) then
               ! on alloue l'esapce memoire pour stocker
               !   * les indices des dofs imposes en force
               allocate(bdyty(new_ibdyty)%force_driven_dof(bdyty(new_ibdyty)%nb_force_driven_dof))
               !   * les forces imposees
               allocate(bdyty(new_ibdyty)%Fdriv(bdyty(new_ibdyty)%nb_force_driven_dof))

               ! si l'allocation a echoue
               if (errare /= 0) then
                  ! on affiche un message d'erreur
                  call logmes('Error '//IAM// ': error allocating bdyty%Fdriv, ...')
               end if

               ! on copie les C.L. en force
               bdyty(new_ibdyty)%force_driven_dof = bdyty(ibdyty)%force_driven_dof
               bdyty(new_ibdyty)%Fdriv            = bdyty(ibdyty)%Fdriv
            ! sinon,
            else
               ! on initialise les pointeurs a nul
               nullify(bdyty(new_ibdyty)%force_driven_dof)
               nullify(bdyty(new_ibdyty)%Fdriv)
            end if
            ! si l'allocation a echoue
            if (errare /= 0) then
               ! on affiche un message d'erreur
               call logmes('Error '//IAM// ': error allocating bdyty%Vdriv, bdyty%Fdriv, ...')
            end if

            ! gestion des autres champs (scalaires)
            !am : j'en oublie surement d'autres... 

            ! on copie 
            !   * visibilite (initialement, tous invisibles)
            bdyty(new_ibdyty)%visible = .false.
            !   * autres
            bdyty(new_ibdyty)%r2m     = bdyty(ibdyty)%r2m
            bdyty(new_ibdyty)%SIGMA   = bdyty(ibdyty)%SIGMA

            !
            ! copie des contacteurs
            ! 

            ! on recupere le nombre de contacteurs du corps courant
            nb_tacts = size(bdyty(ibdyty)%tacty)
            ! on alloue l'espace memoire pour stocker les contacteurs de la copie
            ! du corps courant
            allocate(bdyty(new_ibdyty)%tacty(nb_tacts), stat=errare)
            ! si l'allocation a echoue
            if (errare /= 0) then
               ! on affiche un message d'erreur
               call logmes('Error '//IAM// ': error allocating bdyty%tacty')
            end if

            ! pour chaque contacteur du corps courant 
            do itacty=1, nb_tacts
               ! on copie :
               !   * l'identifiant du contacteur
               bdyty(new_ibdyty)%tacty(itacty)%tacID = bdyty(ibdyty)%tacty(itacty)%tacID
               !   * la couleur du contacteur
               bdyty(new_ibdyty)%tacty(itacty)%color = bdyty(ibdyty)%tacty(itacty)%color

               ! on rempli la structure BDARY du contacteur courant

               ! si le tableau data du contacteur courant a ete alloue
               if (associated(bdyty(ibdyty)%tacty(itacty)%BDARY%data)) then
                  ! on recupere la table du tableau data du contacteur courant
                  size_data = size(bdyty(ibdyty)%tacty(itacty)%BDARY%data)

                  ! on alloue l'esapce memoire pour stocker le tableau data
                  allocate(bdyty(new_ibdyty)%tacty(itacty)%BDARY%data(size_data))

                  ! on copie le tableau data
                  bdyty(new_ibdyty)%tacty(itacty)%BDARY%data = bdyty(ibdyty)%tacty(itacty)%BDARY%data
               ! sinon,
               else
                  ! on initialise le pointeur a nul
                  nullify(bdyty(new_ibdyty)%tacty(itacty)%BDARY%data)
               end if
               ! si le tableau idata du contacteur courant a ete alloue
               if (associated(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)) then
                  ! on recupere la table du tableau idata du contacteur courant
                  size_idata = size(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)

                  ! on alloue l'esapce memoire pour stocker le tableau idata
                  allocate(bdyty(new_ibdyty)%tacty(itacty)%BDARY%idata(size_idata))

                  ! on copie le tableau idata
                  bdyty(new_ibdyty)%tacty(itacty)%BDARY%idata = bdyty(ibdyty)%tacty(itacty)%BDARY%idata
               ! sinon,
               else
                  ! on initialise le pointeur a nul
                  nullify(bdyty(new_ibdyty)%tacty(itacty)%BDARY%idata)
               end if
               ! on copie :
               !   * la surface
               bdyty(new_ibdyty)%tacty(itacty)%BDARY%area  = bdyty(ibdyty)%tacty(itacty)%BDARY%area
               !   * le rdg
               bdyty(new_ibdyty)%tacty(itacty)%BDARY%rdg   = bdyty(ibdyty)%tacty(itacty)%BDARY%rdg
               !   * le grdg
               bdyty(new_ibdyty)%tacty(itacty)%BDARY%grdg  = bdyty(ibdyty)%tacty(itacty)%BDARY%grdg
               !   * le shift
               bdyty(new_ibdyty)%tacty(itacty)%BDARY%shift = bdyty(ibdyty)%tacty(itacty)%BDARY%shift
            end do
         end do
      end do

      ! on modifie le nombre de RBDY2
      nb_RBDY2 = factor*nb_RBDY2

      !am : je pense que cette mise a jour des entites craint, si on a eu le mauvais gout de charger
      !   d'autres type de corps entre la creation des RBDY2 et cette copie... mais je n'ai rien de 
      !   mieux a proposer pour l'instant

      ! si on n'a pas encore mis a jour la liste d'entites
      if (nb_existing_entities == 0) then
         ! on le fait
         call update_existing_entities_RBDY2
      ! sinon,
      else
         ! on met a jour le nombre d'entites, en ajoutant les corps copies
         call add_nb_ENTITY(nb_copies*nb_RBDY2)
      end if

   end subroutine copy_bodies_RBDY2


   !> \brief set visibility for all bodies
   subroutine set_visibility_4all_RBDY2(visibilities, size_visibilities)

      implicit none

      ! variable d'entree
      integer, intent(in) :: size_visibilities ! taille de tableau visibilities
      logical, dimension(size_visibilities), intent(in) :: visibilities ! tableau 
         ! indiquant la visibilite de chaque corps

      ! variables locales       12345678901234567890123456
      character(len=26) :: IAM='RBDY2::set_visibility_4all'
      integer :: ibdyty ! indice de boucle sur les corps
      integer :: i ! indice de boucle anonyme

      ! si la taille du tabelau n'est pas egale au nombre de corps
      if (size_visibilities /= nb_RBDY2) then
         ! on affiche un message d'erreur
         call logmes('Error '//IAM// ': the number of visibilities icompatible with the number of bodies!')
      end if

      ! on affecte sa visibilite a chaque corps en affactant les visibilites
      ! a une liste de corps egale a la liste de tous les corps
      call set_visibility_list( (/ (i, i=1, nb_RBDY2) /) , visibilities, nb_RBDY2)

   end subroutine set_visibility_4all_RBDY2

!!!------------------------------------------------------------------------
  character(len=5) function get_bulk_behav_ID_RBDY2(ibdyty,iblmty)

    implicit none
    integer,intent(in) :: ibdyty,iblmty

    get_bulk_behav_ID_RBDY2 = bdyty(ibdyty)%blmty(iblmty)%behav

  end function get_bulk_behav_ID_RBDY2

!-------------------------------------------------------------------------------
! melimelo spirit
!------------------------------------------------------------------------------

 !> \brief Initialize the module
 subroutine set_nb_RBDY2(nb)
    implicit none
    integer(kind=4), intent(in) :: nb !< [in] number of RBDY2 in the simulation
    !
    integer(kind=4)   :: errare,ibdyty
    character(len=80) :: cout
    character(len=12) :: IAM='set_nb_RBDY2'

    nb_RBDY2=nb

    if( allocated(bdyty) ) then
      write (*,*) 'bdyty of RBDY2 already allocated'
      deallocate(bdyty)
    end if
    allocate(bdyty(nb_RBDY2),stat=errare)
    if (errare /= 0) then
       call FATERR(IAM,'error allocating bdyty')
    end if

    do ibdyty = 1, nb_RBDY2
      bdyty(ibdyty)%bdyID = 'RBDY2'
      if( associated(bdyty(ibdyty)%blmty) ) nullify(bdyty(ibdyty)%blmty)
      ! we are sure there will be no more than 1 bulk
      allocate( bdyty(ibdyty)%blmty(1), stat=errare )
      if( errare /= 0 ) then
        write (cout,'(A,I0)') 'Problem while allocating blmty of RBDY2 : ', ibdyty
        call faterr(IAM,cout)
      end if

      if( associated(bdyty(ibdyty)%mass) ) nullify(bdyty(ibdyty)%mass)
      allocate( bdyty(ibdyty)%mass(3), stat=errare)
      if( errare /= 0 ) then
        write (cout,'(A,I0)') 'Problem while allocating mass of RBDY2 : ', ibdyty
        call faterr(IAM,cout)
      end if

    end do

 end subroutine

 !> \brief Set the bulk of body
 subroutine set_bulk_of_RBDY2(nb_dof, rigid_data, r8_vector)
    implicit none
    integer(kind=4), intent(out) :: nb_dof !< [out] the number of dof of the added bulk
    real(kind=8), dimension(:), allocatable, intent(inout) :: rigid_data !< [inout] array storing computed bulk data
    real(kind=8), dimension(:), intent(in) :: r8_vector                  !< [in] average and gyration radius
    !
    character(len=17) :: IAM='set_bulk_of_RBDY2'
    integer(kind=4) :: errare

    nb_dof = 3

    if( allocated(rigid_data) ) then
      call FATERR(IAM,'rigid_data already allocated')
    end if

    allocate(rigid_data(6), stat=errare)
    if( errare /= 0 ) then
      call FATERR(IAM,'error allocating rigid_data')
    end if

    ! average_radius and gyration radius respectively
    rigid_data(1:2) = r8_vector(1:2)

    ! initial inertia frame
    if( size(r8_vector) < 6 ) then
      rigid_data(3  ) = 1.D0
      rigid_data(4:5) = 0.D0
      rigid_data(6  ) = 1.D0
    else
      rigid_data(3:6) = r8_vector(3:6)
    end if
    !nullify(bdyty(ibdy)%blmty(iblmty)%PLAIN%inc_dilat, &
    !        bdyty(ibdy)%blmty(iblmty)%PLAIN%dilat, &
    !        bdyty(ibdy)%blmty(iblmty)%PLAIN%cooref)

 end subroutine

!---------------------------------------------------------------

  !> \brief Compute mass matrix of a RBDY2
  subroutine comp_mass_one_body_RBDY2(M_elem, i_behav, rigid_data)
    implicit none
    real(kind=8), dimension(3), intent(inout) :: M_elem     !< [in,out] elementary matrix in which to store the mass
    integer(kind=4) ,           intent(in)    :: i_behav    !< [in] behaviour index
    real(kind=8), dimension(2), intent(in)    :: rigid_data !< [in] average and gyration radii
    !
    integer      :: ibehav,iccdof
    real(kind=8) :: Umass,mass1,mass2,avr_radius,gyr_radius

    Umass = get_rho(i_behav)

    avr_radius = rigid_data(1)
    gyr_radius = rigid_data(2)

    M_elem(1) = Umass*avr_radius*avr_radius*PI_g
    M_elem(2) = M_elem(1)
    M_elem(3) = M_elem(1)*gyr_radius*gyr_radius

    !if( bdyty(ibdyty)%mass(iccdof)<1.D-20) THEN
    !   PRINT*,'MASS',bdyty(ibdyty)%mass(iccdof),'iccdof',iccdof
    !   PRINT*,'RBDY2',ibdyty
    !END IF
       
  end subroutine comp_mass_one_body_RBDY2

  !> \brief Compute the external forces on a RBDY2
  subroutine comp_Fext_one_body_RBDY2(mass, fext)
    implicit none
    real(kind=8), dimension(:), intent(in)  :: mass  !< [in] elementary mass matrix
    real(kind=8), dimension(:), intent(out) :: fext  !< [out] the external forces
    !
    integer(kind=4) :: i_dof
    real(kind=8), dimension(3) :: grav

    i_dof = get_idof_RBDY2()
    call paranoid_check_r8_size('RBDY2::comp_Fext_one_body', fext, i_dof)

    grav(1) = grav1
    grav(2) = grav2
    grav(3) = 0.D0

    fext(1:i_dof) =  mass(1:i_dof) * grav(1:i_dof)

  end subroutine comp_Fext_one_body_RBDY2

  !> \brief Compute the internal forces of a RBDY2
  subroutine comp_Bulk_one_body_RBDY2(fint)
    implicit none
    real(kind=8), dimension(:), pointer :: fint !< [in] state array of the internal forces

    fint = 0.d0

  end subroutine comp_Bulk_one_body_RBDY2

  !> \brief Get the number of degrees of freedom
  function get_idof_RBDY2()
    implicit none
    integer(kind=4) :: get_idof_RBDY2 !< [return] number of degrees of freedom for a RBDY2

    get_idof_RBDY2 = 3

  end function get_idof_RBDY2

  !> \brief Get field map of a body
  subroutine get_ccfield_RBDY2(ccfield)
    implicit none
    integer(kind=4), dimension(:), pointer :: ccfield !< [inout] field indices map for a RBDY2 (null)

    ccfield => null()
  end subroutine get_ccfield_RBDY2

  !> \brief Add mass matrix of a body in a G_matrix
  subroutine add_mass_to_matrix_RBDY2(matrix, ibdyty, edof2gdof)
    implicit none
    integer(kind=4), intent(in) :: ibdyty !< [in] index of the body in bdyty array
    integer(kind=4), dimension(:), intent(in) :: edof2gdof !< [in] map
    type(G_matrix), intent(inout) :: matrix !< [inout] matrix
    !
    integer(kind=4) :: i
    real(kind=8), dimension(3,3) :: full_mass

    full_mass = 0.D0
    do i = 1, 3
      full_mass(i,i) = bdyty(ibdyty)%mass(i)
    end do
    call G_assemb(matrix, full_mass, edof2gdof)

  end subroutine add_mass_to_matrix_RBDY2

  !> \brief Disable or enable a velocity constraint for a given body
  !>        in a given dimension
  subroutine switch_vlocy_driven_dof( ibdyty, iccdof, ival )

    implicit none

    integer, intent( in ) :: ibdyty
    integer, intent( in ) :: iccdof
    integer, intent( in ) :: ival

    ! ****
    integer :: ivd
    logical :: found_dof_vlocy
                                  !123456789012345678901234567890
    character( len =30 ) :: IAM = 'RBDY2::switch_vlocy_driven_dof'
    character( len =80 ) :: cout

    found_dof_vlocy = .FALSE.

    ! nullifying inv_mass where degrees of freedom are driven
    do ivd = 1, bdyty( ibdyty )%nb_vlocy_driven_dof
      if ( iccdof == dofnb_of_a_driven_dof( bdyty( ibdyty )%vlocy_driven_dof( ivd ) ) ) then

        if ( ival == 0 ) then
          bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active = .FALSE.
          bdyty( ibdyty )%inv_mass( iccdof )                = 1.d0 / bdyty( ibdyty )%mass( iccdof )
        else
          bdyty( ibdyty )%vlocy_driven_dof( ivd )%is_active = .TRUE.
          bdyty( ibdyty )%inv_mass( iccdof )                = 0.d0
        endif

        found_dof_vlocy = .TRUE.
        exit

      endif
    end do

    if ( .not. found_dof_vlocy ) then
      write( cout, '("body ",I0," ddl ",I0)' ) ibdyty, iccdof
      call logmes( cout )
      call faterr( IAM, 'Error while trying to switch a velocy driven dof status' )
    end if

  end subroutine switch_vlocy_driven_dof


  subroutine clean_memory_RBDY2()
    implicit none
    integer(kind=4) :: i, i_bdyty, i_blmty, i_tacty
    
    nb_RBDY2 = 0

    if( allocated(bdyty) ) then
      do i_bdyty = 1, size(bdyty)

        if( associated(bdyty(i_bdyty)%blmty) ) then
          do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%dilat) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%dilat)
              nullify(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%dilat)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%inc_dilat) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%inc_dilat)
              nullify(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%inc_dilat)
            end if
            if( associated(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%cooref) ) then
              deallocate(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%cooref)
              nullify(bdyty(i_bdyty)%blmty(i_blmty)%PLAIN%cooref)
            end if
          end do
          deallocate(bdyty(i_bdyty)%blmty)
          nullify(bdyty(i_bdyty)%blmty)
        end if

        if( associated(bdyty(i_bdyty)%tacty) ) then
          do i_tacty = 1, size(bdyty(i_bdyty)%tacty)
            if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data) ) then
              deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data)
              nullify(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data)
            end if
            if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata) ) then
              deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata)
              nullify(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata)
            end if
            if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WS) ) then
              deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WS)
              nullify(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WS)
            end if
            if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WSini) ) then
              deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WSini)
              nullify(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WSini)
            end if
            if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WStime) ) then
              deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WStime)
              nullify(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WStime)
            end if
            if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WSstatus) ) then
              deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WSstatus)
              nullify(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%WSstatus)
            end if
          end do
          deallocate(bdyty(i_bdyty)%tacty)
          nullify(bdyty(i_bdyty)%tacty)
        end if

        if( associated(bdyty(i_bdyty)%cooref  ) ) deallocate(bdyty(i_bdyty)%cooref  )
        if( associated(bdyty(i_bdyty)%coor    ) ) deallocate(bdyty(i_bdyty)%coor    )
        if( associated(bdyty(i_bdyty)%Vbegin  ) ) deallocate(bdyty(i_bdyty)%Vbegin  )
        if( associated(bdyty(i_bdyty)%Xbegin  ) ) deallocate(bdyty(i_bdyty)%Xbegin  )
        if( associated(bdyty(i_bdyty)%V       ) ) deallocate(bdyty(i_bdyty)%V       )
        if( associated(bdyty(i_bdyty)%X       ) ) deallocate(bdyty(i_bdyty)%X       )
        if( associated(bdyty(i_bdyty)%Vfree   ) ) deallocate(bdyty(i_bdyty)%Vfree   )
        if( associated(bdyty(i_bdyty)%Vaux    ) ) deallocate(bdyty(i_bdyty)%Vaux    )
        if( associated(bdyty(i_bdyty)%Fext    ) ) deallocate(bdyty(i_bdyty)%Fext    )
        if( associated(bdyty(i_bdyty)%Fint    ) ) deallocate(bdyty(i_bdyty)%Fint    )
        if( associated(bdyty(i_bdyty)%mass    ) ) deallocate(bdyty(i_bdyty)%mass    )
        if( associated(bdyty(i_bdyty)%Ireac   ) ) deallocate(bdyty(i_bdyty)%Ireac   )
        if( associated(bdyty(i_bdyty)%Iaux    ) ) deallocate(bdyty(i_bdyty)%Iaux    )
        if( associated(bdyty(i_bdyty)%inv_mass) ) deallocate(bdyty(i_bdyty)%inv_mass)

        nullify(bdyty(i_bdyty)%cooref  )
        nullify(bdyty(i_bdyty)%coor    )
        nullify(bdyty(i_bdyty)%Vbegin  )
        nullify(bdyty(i_bdyty)%Xbegin  )
        nullify(bdyty(i_bdyty)%V       )
        nullify(bdyty(i_bdyty)%X       )
        nullify(bdyty(i_bdyty)%Vfree   )
        nullify(bdyty(i_bdyty)%Vaux    )
        nullify(bdyty(i_bdyty)%Fext    )
        nullify(bdyty(i_bdyty)%Fint    )
        nullify(bdyty(i_bdyty)%mass    )
        nullify(bdyty(i_bdyty)%Ireac   )
        nullify(bdyty(i_bdyty)%Iaux    )
        nullify(bdyty(i_bdyty)%inv_mass)

        if( associated(bdyty(i_bdyty)%vlocy_driven_dof) ) then
          do i = 1, size(bdyty(i_bdyty)%vlocy_driven_dof)
            if( associated(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x) ) then
              deallocate(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x)
              nullify(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x)
            end if
            if( associated(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx) ) then
              deallocate(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx)
              nullify(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx)
            end if
          end do

          deallocate(bdyty(i_bdyty)%vlocy_driven_dof)
          nullify(bdyty(i_bdyty)%vlocy_driven_dof)
        end if

        if( associated(bdyty(i_bdyty)%force_driven_dof) ) then
          do i = 1, size(bdyty(i_bdyty)%force_driven_dof)
            if( associated(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x) ) then
              deallocate(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x)
              nullify(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x)
            end if
            if( associated(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx) ) then
              deallocate(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx)
              nullify(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx)
            end if
          end do
          deallocate(bdyty(i_bdyty)%force_driven_dof)
          nullify(bdyty(i_bdyty)%force_driven_dof)
        end if

        if( associated(bdyty(i_bdyty)%Vdriv) ) deallocate(bdyty(i_bdyty)%Vdriv)
        if( associated(bdyty(i_bdyty)%Xdriv) ) deallocate(bdyty(i_bdyty)%Xdriv)
        if( associated(bdyty(i_bdyty)%Fdriv) ) deallocate(bdyty(i_bdyty)%Fdriv)

        if( associated(bdyty(i_bdyty)%A     ) ) deallocate(bdyty(i_bdyty)%A     )
        if( associated(bdyty(i_bdyty)%B     ) ) deallocate(bdyty(i_bdyty)%B     )
        if( associated(bdyty(i_bdyty)%C     ) ) deallocate(bdyty(i_bdyty)%C     )
        if( associated(bdyty(i_bdyty)%Abegin) ) deallocate(bdyty(i_bdyty)%Abegin)
        if( associated(bdyty(i_bdyty)%Bbegin) ) deallocate(bdyty(i_bdyty)%Bbegin)
        if( associated(bdyty(i_bdyty)%Cbegin) ) deallocate(bdyty(i_bdyty)%Cbegin)

        nullify(bdyty(i_bdyty)%Vdriv )
        nullify(bdyty(i_bdyty)%Xdriv )
        nullify(bdyty(i_bdyty)%Fdriv )
        nullify(bdyty(i_bdyty)%A     )
        nullify(bdyty(i_bdyty)%B     )
        nullify(bdyty(i_bdyty)%C     )
        nullify(bdyty(i_bdyty)%Abegin)
        nullify(bdyty(i_bdyty)%Bbegin)
        nullify(bdyty(i_bdyty)%Cbegin)

      end do
      deallocate(bdyty)
    end if

    !nb_existing_entities = 0
    !nb_falling_RBDY2     = 0
    !first_RBDY2          = 0

    !BOUNDS = .false.
    !limit_inf   = -1.D+24
    !limit_sup   =  1.D+24 
    !limit_left  = -1.D+24
    !limit_right =  1.D+24 

    !sp_radius
    !sp_shift_x
    !sp_shift_y

    !eqs_tol
    !eqs_ichecktype

    !CWx0,CWxf,CWxi,CWy0,CWyf,CWyi
    !CWnx,CWny
    first_construct=.false.

    PERIODIC = .false.
    !periode
    !CW_XMIN = 0.D0, CW_XMAX = 0.D0, CW_CV__ = 0.D0
    !CW_BEHAV

    !skip_invisible = .false.

    if( allocated(free_surface) ) deallocate(free_surface)

    !FB_NX
    !FB_XMIN,FB_XMAX,FB_DX
    !FREE_BOUNDARY = .false.

    !nb_WSsect = 1

  end subroutine clean_memory_RBDY2

end module RBDY2

