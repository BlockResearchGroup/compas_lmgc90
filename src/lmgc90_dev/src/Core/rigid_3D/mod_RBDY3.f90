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
MODULE RBDY3                                       

  !!****h* LMGC90.CORE/RBDY3
  !! NAME
  !!  module RBDY3
  !! PURPOSE
  !!  RBDY3 module defines generically a type T_RBDY3 for 3 dimensional rigid bodies, with 6 degrees of freedom.
  !!  The first two degrees of freedom are related to the motion of the center of gravity, and the third to the 
  !!  rotation of the body. All data are stored in a single list bdyty.
  !!  
  !!  The standard bulk properties are defined in a bulk element called PLAIN.
  !!  These properties are the mass and the inertia moment. They are computed from:
  !!  the specific mass defined in BULK_BEHAVIOUR.DAT from RIGID law called by its nickname behav;
  !!  the average radius and the inertia moment, data from the blmty PLAIN, see below.
  !!
  !!  The standard nodty is NO6xx, with 6 degrees of freedom as introduced above.
  !! 
  !!  The feature used as a customized property distinguishing a rigid body from another body is the boundary.
  !!  These boundaries,for examples tacty DISKx, JONCx, POLYG,  are managed in moduli such as mod_DISKx, mod_JONCx, 
  !!  mod_POLYG. Boundary datas, radius of DISKx, half axes ax1, ax2,of JONCx, etc., are stored in a T_BDARY type in  
  !!  some array data, the meaning of it being described in some class a_BDARY_DISKx, a_BDARY_JONCx, a_BDARY_POLY. 
  !!
  !!  It happens that the abstract class RBDY3 may be extented to some other applications, for instance the tacty
  !!  POINT, which is a 3 dimensional material particle, with 3 degrees of freedom. Another example is xPSID, which means
  !!  a reverse pneumatic disk. A pneumatic disk, is a 2 dimensional rigid body wiyh boundary such as DISKx but equipped 
  !!  with a variable boundary radius.This boundary radius is then a fourth degree of freedom and not anymore a data. 
  !!  The nodty used for this body is NO6xx. Reverse, means that the free region is the inner disk. 
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/BULK_BEHAVIOUR
  !!  LMGC90.CORE/A_DOF
  !!****
  
  !fd Integration des rotations avec theta-schema 
  ! 2 approches sont disponibles: 
  !  new_rotation_scheme == .false.
  !     integration linearisee + Graam-Schmidt
  !     Increment fait _ini -> _TT
  !     comp_dof  fait _TT -> _end
  !     update_dof fait _ini = _end
  !   new_rotation_scheme == .true.
  !     approche Hughes-Winget
  !     on ne travaille vraiment qu'avec les TT, _ini contient le _TT du pas precedent
  !     Increment fait _ini -> _TT 
  !     comp_dof fait _end = _TT
  !     update_dof fait _ini = _end
  !
  ! TODO clarifier les choses ...


  USE utilities
  USE overall
  USE bulk_behaviour
  USE a_matrix
  USE a_DOF
  USE ALGEBRA
  USE RigidKinematic

  USE a_BDARY_SPHER
  USE a_BDARY_PLANx  
  USE a_BDARY_DNLYC
  USE a_BDARY_CYLND
  USE a_BDARY_POLYR
  USE a_BDARY_PT3Dx
  
  USE paranoid_checks

  use RBDY3_type, only : T_RBDY3

  IMPLICIT NONE

  PRIVATE

  !------------------------------------------------------------------------ 

  TYPE(T_RBDY3),DIMENSION(:),POINTER :: bdyty => NULL( )
  
  integer(kind=4) :: nb_RBDY3 = 0
  integer(kind=4) :: nb_existing_entities

  ! managing rotation with Hughes Winget scheme
  LOGICAL      :: new_rotation_scheme = .FALSE.

  ! write out flags
  LOGICAL :: skip_invisible = .FALSE., keep_ini_dof_order=.FALSE.
  LOGICAL :: renum_visible_bodies=.FALSE. !pta

!!! --------------------------------------------------------

  ! bounds definition
  LOGICAL      :: BOUNDS=.FALSE.
  REAL(kind=8),DIMENSION(3) :: MinBound =-1.D+24
  REAL(kind=8),DIMENSION(3) :: MaxBound =+1.D+24
  
  ! periodicity
  LOGICAL      :: XPERIODIC=.FALSE.,YPERIODIC=.FALSE.
  REAL(kind=8) :: xperiode,yperiode

  ! parametres permettant de stopper un calcul si la vitesse est stabilisee.
  !
  real(kind=8)      :: eqs_tol
  integer           :: eqs_ichecktype
  integer,parameter :: iQvlcy = 1 , iMvlcy = 2
  
  !
  !mr T_FREE_BOUNDARY TYPE
  !mr use to determine the particles composing the free surface of a sample
  !
  TYPE T_FREE_BOUNDARY

     INTEGER(kind=4) :: ID_RBDY3,ID_TACTY
     REAL(KIND=8)    :: ZMAX
     LOGICAL         :: ACTIVE

  END TYPE T_FREE_BOUNDARY

  TYPE(T_FREE_BOUNDARY),DIMENSION(:,:),ALLOCATABLE  :: FREE_SURFACE

  INTEGER(kind=4) :: FB_NX,FB_NY
  REAL(KIND=8)    :: FB_XMIN,FB_XMAX,FB_YMIN,FB_YMAX,FB_DX
  LOGICAL         :: FREE_BOUNDARY = .FALSE.
  !
  LOGICAL         :: GIVEN_THREAD_NETWORK = .FALSE.

  !
  PUBLIC &
       increment_RBDY3, &
       set_vlocy_drvdof_RBDY3, &
       is_dof_driven_RBDY3, &
       out_of_bounds_RBDY3, &
       fatal_damping_RBDY3, &
       partial_damping_RBDY3, &
       comp_Fext_RBDY3, &
       comp_Fint_RBDY3, &
       comp_free_vlocy_RBDY3, &
       comp_dof_RBDY3, &
       update_dof_RBDY3, &
       write_xxx_dof_RBDY3, &
       write_xxx_Rnod_RBDY3, &
       write_out_bodies_RBDY3, &
       write_out_driven_dof_RBDY3, &
       read_in_bodies_RBDY3, &
       update_existing_entities_RBDY3, &
       read_in_dof_RBDY3, &
       read_in_driven_dof_RBDY3, &
       read_behaviours_RBDY3, &
       comp_mass_RBDY3, &
       set_new_rotation_scheme_RBDY3, &
       set_init_boundary_RBDY3, &
       set_xperiodic_data_RBDY3, &
       set_yperiodic_data_RBDY3, &
       get_write_DOF_RBDY3, &
       get_write_Rnod_RBDY3, &
       read_mp_behaviours_RBDY3, &
       update_WS_RBDY3, &
       without_rotation_of_RBDY3, &
       set_skip_invisible_RBDY3, &
       set_keep_ini_dof_order_RBDY3, &
       membrane_RBDY3, &
       triaxial_loading, &
       put_vector_RBDY3, get_vector_RBDY3, &
       put_matrix_RBDY3, get_matrix_RBDY3, &
       get_all_rdata_RBDY3 , &
       get_ptr_vector_RBDY3, &
       get_drv_vlocy_RBDY3 , &
       comp_drv_vlocy_RBDY3, &
       comp_coor_4all_RBDY3, & ! <- rm: fonctions pour binding avec peligriff
       get_density, &
       comp_glob_inert_RBDY3,&
       get_ptr_mass, &
       set_nb_RBDY3               , & !<- pour module modelization
       add_one_RBDY3              , &
       set_one_tactor_RBDY3       , &
       set_blmty_RBDY3            , &
       add_predef_driven_dof_RBDY3, &
       add_evol_driven_dof_RBDY3  , &
       get_idof_RBDY3, &
       get_ccfield_RBDY3, &
       comp_mass_one_body_RBDY3, &
       comp_Fext_one_body_RBDY3, &
       comp_Bulk_one_body_RBDY3, &
       add_mass_to_matrix_RBDY3, &
       !am DDM : declaration des fonctions necessaires a la DDM 
       copy_bodies_RBDY3, &
       set_visibility_4all_RBDY3, &
       set_invisible, &
       set_mass_RBDY3, &
       comp_dof_one_RBDY3, &
       comp_free_vlocy_one_RBDY3, &
       set_bdyty2tacty_RBDY3, &
       get_ptr_bdyty2tacty_RBDY3, &
       ! vv & am 
       nullify_vlocy_driven_dof, &
       comp_V_RBDY3, &
       comp_X_localFrame_RBDY3, &
       check_bounds_RBDY3, &
       set_invisible_small_objects,&
       add_dof2bodies_RBDY3,&
       compute_configurationTT_RBDY3, &
       switch_vlocy_driven_dof, &
       print_info_RBDY3, &
       set_data_equilibrium_RBDY3, &
       check_equilibrium_state_RBDY3, &
       get_dofstatus_RBDY3


  PUBLIC &
       load_thread_network_RBDY3, &
       is_RBDY3_same_THREAD, &
       get_thread

  PUBLIC &
       add_reac    , nullify_reac, nullify_vlocy, comp_vlocy  , get_vlocy, &
       get_coor    , get_coorb    , get_cooref  , get_coorTT   , get_Xbegin  , get_X    , &
       get_V       , get_Vbegin  , get_tacID    , get_color   , get_behav, &
       get_idata   , get_reac    , get_data_sz  , get_idata_sz, get_bdyID, &
       get_nb_tacty, get_volume  , get_mass     , get_data    , get_inertia_tensor, &
       get_nb_RBDY3, get_visible , put_data     , get_shiftTT , get_entity_RBDY3, &
       get_Fext    , set_visible, get_avr_radius, put_coor, put_V, add_ext_Fext,&
       get_inertia_frameIni, get_inertia_frameTT, get_inertia_frame, &
       get_elec_cond,get_elec_condini,put_elec_cond,get_electric_potentiel,put_electric_potentiel,&
       get_therm_cond,get_therm_condini,put_therm_cond,get_thermal_value,put_thermal_value, &
       get_thermal_coefficient,&
       is_Xperiodic_RBDY3,is_Yperiodic_RBDY3, get_xperiode_RBDY3,get_yperiode_RBDY3,&
       !get_xperiode,get_yperiode,&
       get_avr_radius_tacty, &
       get_WS,put_WS,get_bulk_behav_number_RBDY3, &
       get_embeded_frame,put_embeded_frame, &
       get_Finertia, &
       renum_visible_RBDY3, get_visibleID, &
       set_color_RBDY3

  PUBLIC &
       free_boundary_computation, &
       IS_IN_THE_FREE_BOUNDARY, &
       init_free_boundary_RBDY3, &
       clean_memory_RBDY3

  public get_bdyty_RBDY3

CONTAINS

!------------------------------------------------------------------------

  subroutine get_bdyty_RBDY3( arg_bdyty )

    implicit none

    type( T_RBDY3 ), dimension( : ), pointer :: arg_bdyty

    arg_bdyty => bdyty

  end subroutine get_bdyty_RBDY3

!!!------------------------------------------------------------------------
!!! In/Out Subroutine interface
!!!------------------------------------------------------------------------
  SUBROUTINE read_in_bodies_RBDY3(ilog, factor)

    IMPLICIT NONE
    INTEGER, INTENT(in) :: ilog
    ! variable d'entree optionelle
    INTEGER, INTENT(in), OPTIONAL :: factor ! on surdimenssionne le
       ! tabelau des corps en multipliant le nombre de corps lus par
       ! ce facteur

    G_nfich = get_io_unit()
    SELECT CASE(ilog)
    CASE(1)
       OPEN(unit=G_nfich,file=TRIM(location(in_bodies(:))))
    CASE(2)
       NULLIFY(bdyty)
       OPEN(unit=G_nfich,file=TRIM(location(out_bodies(:))))
    CASE default
       CALL faterr('RBDY3::read_in_bodies',' @ Could not read BODIES file')
    END SELECT

    CALL read_bodies(factor)
    CLOSE(G_nfich)
    
  END SUBROUTINE read_in_bodies_RBDY3
!!!------------------------------------------------------------------------
  subroutine read_in_dof_RBDY3(step)
    implicit none
    integer(kind=4), intent(in) :: step
    
    G_nfich = get_io_unit()

    if( step==0 ) then
      open(unit=G_nfich,file=TRIM(location(in_dof(:))))
    else if( step>0 ) then
      open(unit=G_nfich,file=TRIM(location(out_dof(:))))
    else
      open(unit=G_nfich,file=TRIM(location(last_dof(:))))
    end if

    call read_in_dof
    close(G_nfich)
    
  end subroutine read_in_dof_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE read_in_driven_dof_RBDY3

    IMPLICIT NONE
    
    G_nfich = get_io_unit()
    OPEN(unit=G_nfich,file=TRIM(location(in_driven_dof(:))))
    CALL read_driven_dof
    CLOSE(G_nfich)
    
  END SUBROUTINE read_in_driven_dof_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_bodies_RBDY3(ilog)

    IMPLICIT NONE

    INTEGER :: nfich,ilog

    nfich = get_io_unit()
    SELECT CASE(ilog)
    CASE(1)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_bodies(:))))
    CASE(2)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(in_bodies(:))))
    CASE default
       
    END SELECT
    CALL write_bodies(nfich)
    CLOSE(nfich)

  END SUBROUTINE write_out_bodies_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_driven_dof_RBDY3

    IMPLICIT NONE
    
    INTEGER :: nfich

    nfich = get_io_unit()

    OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_driven_dof(:))))
    CALL write_driven_dof(nfich)
    CLOSE(nfich)
    
  END SUBROUTINE write_out_driven_dof_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_dof_RBDY3(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_dof)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_dof(1:lc))))
       CALL write_out_dof(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_dof)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_dof(1:lc))))
       CALL write_out_dof(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_dof(6,ifrom,ito)
    END SELECT

  END SUBROUTINE write_xxx_dof_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Rnod_RBDY3(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc))))
       CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc))))
       CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_Rnod(6,ifrom,ito)
    END SELECT

  END SUBROUTINE write_xxx_Rnod_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies(factor)

    IMPLICIT NONE
    
    ! variable d'entree optionelle
    INTEGER, INTENT(in), OPTIONAL :: factor ! on surdimenssionne le
       ! tabelau des corps en multipliant le nombre de corps lus par
       ! ce facteur

    INTEGER                     :: i,j,k
    INTEGER                     :: ibdyty,iblmty,inodty,itacty,iccdof,idof,nbdof
    INTEGER                     :: errare,itest
    CHARACTER(len=5)            :: tacID
    CHARACTER(len=27)           :: IAM ='mod_RBDY3::read_bodies'
    CHARACTER(len=103)          :: cout
    LOGICAL                     :: comp_blmty,comp_nodty
    REAL(kind=8)                :: IT(3),I1i,I2i,I3i,vT,Vi,norm
    REAL(kind=8),DIMENSION(3,3) :: mat_IT,mat_Ii,mat_aux,localframe

    REAL(kind=8),DIMENSION(3)   :: OG,v_aux,d
    REAL(kind=8),DIMENSION(6)   :: cooref

    REAL(kind=8) :: avrd

    INTEGER :: size_bdyty ! taille du tableau des corps
    real(kind=8), parameter :: untiers = 1.d0/3.d0

    ! verification de la coherence des donnees :

    ! si on adonne un facteur
    IF (PRESENT(factor)) THEN
       ! si le facteur est plus petit que 1
       IF (factor < 1) THEN
          ! on affiche un message d'erreur
          CALL logmes('Error '//IAM// ": factor must be greater than one!")
       END IF
    END IF

    ! first reading: sizing array of bodies bdyty  

    ibdyty=0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin)
       IF (itest.NE.ifound) CYCLE
       ibdyty=ibdyty+1
    END DO
    REWIND(G_nfich)

    WRITE(cout,'(I0,1x,A)') ibdyty,'RBDY3 found'
    CALL LOGMES(cout)
    CALL LOGMES('--')
    
    nb_RBDY3 = ibdyty
    
    IF (nb_RBDY3==0) RETURN

    ! si le tableau des corps doit etre surdimensionne
    IF (PRESENT(factor)) THEN
       ! la taille du tableau des corps est le nombre de corps lus
       ! multiplie par le facteur
       size_bdyty = factor*nb_RBDY3
    ! sinon,
    ELSE
       ! la taille du tableau des corps est directement le nombre de corps lus
       size_bdyty = nb_RBDY3
    END IF

    ALLOCATE(bdyty(size_bdyty),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating bdyty')
    END IF

    ! second reading: 
    ! sizing list of bulk elements
    ! sizing list of nodes 
    ! sizing list of contactors
    
    ibdyty = 0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6).NE.'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin)
       IF (itest.NE.ifound) CYCLE
       ibdyty = ibdyty + 1
          
       READ(G_clin(2:6),'(A5)') bdyty(ibdyty)%bdyID
                
       iblmty = 0
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) .NE. 'blmty') CYCLE               ! fishing for the keyword 'blmty'
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_blmty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) iblmty=iblmty+1
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)
       
       IF (iblmty.NE.0) THEN
          ALLOCATE(bdyty(ibdyty)%blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty')
          END IF
       ELSE
          NULLIFY(bdyty(ibdyty)%blmty)
          WRITE(cout,'(A27,I7)') 'warning: RBDY3 without bulk',ibdyty
          CALL LOGMES(cout)
       END IF

       inodty = 0
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6).NE.'nodty') CYCLE                ! fishing for the keyword 'nodty'
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_nodty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) inodty=inodty+1
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)
       
       IF (inodty /= 1) THEN
          WRITE(cout,'(A33,I7)') 'warning: RBDY3 should have 1 node',ibdyty
          CALL FATERR(IAM,cout)
       END IF

       itacty = 0
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6).NE.'tacty') CYCLE                ! fishing for the keyword 'tacty'			 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_tacty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) itacty=itacty+1
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)
       
       IF (itacty /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%tacty(itacty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%tacty')
          END IF

          ALLOCATE(bdyty(ibdyty)%bdyty2tacty(2,itacty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%bdyty2tacty')
          END IF

          DO itacty=1,SIZE(bdyty(ibdyty)%tacty)
             NULLIFY(bdyty(ibdyty)%tacty(itacty)%BDARY%data)
             NULLIFY(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
             bdyty(ibdyty)%tacty(itacty)%BDARY%I1=0.D0
             bdyty(ibdyty)%tacty(itacty)%BDARY%I2=0.D0
             bdyty(ibdyty)%tacty(itacty)%BDARY%I3=0.D0
          END DO

          bdyty(ibdyty)%bdyty2tacty = 0

       ELSE 
          NULLIFY(bdyty(ibdyty)%tacty)
          NULLIFY(bdyty(ibdyty)%bdyty2tacty)
          !123456789   01234   567890123456789012
          WRITE(cout,'(A32,I7)') 'warning: RBDY3 without contactor',ibdyty
          CALL LOGMES(cout)
       END IF
       CYCLE
    END DO
    REWIND(G_nfich)   
    
    ! third reading: filling types

    ibdyty=0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin)
       IF (itest .NE. ifound) CYCLE
       ibdyty=ibdyty+1
              
       iblmty=0
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE                ! fishing for the keyword 'blmty'		 
          DO
             IF( .NOT. read_G_clin()) EXIT
             
             itest=itest_blmty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN
                iblmty=iblmty+1
                READ(G_clin(2:6),'(A5)') bdyty(ibdyty)%blmty(iblmty)%blmID
             END IF
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

       inodty=0
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty'			 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_nodty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN
                IF (G_clin(2:6) /= 'NO6xx') THEN
                   CALL FATERR(IAM,'nodty in BODIES.DAT should be a NO6xx')
                END IF
                call new_nodty(bdyty(ibdyty)%nodty,G_clin(2:6))
             END IF
             CYCLE
          END DO
          EXIT       
       END DO
       BACKSPACE(G_nfich)   

       itacty=0
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'tacty') CYCLE                ! fishing for the keyword 'tacty'
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_tacty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) itacty = itacty + 1
             CYCLE
          END DO
          EXIT
       END DO
       CYCLE
    END DO
    REWIND(G_nfich)   

    ! fourth reading: filling in data
    
    ibdyty=0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin)
       IF (itest.NE.ifound) CYCLE
       ibdyty=ibdyty+1

       iblmty = 0
       DO  
          IF( .NOT. read_G_clin()) EXIT  
          IF (G_clin(2:6) /= 'blmty') CYCLE                ! fishing for the keyword 'blmty'	
          
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_blmty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN
                iblmty=iblmty+1
                ! check here all bulk elements selected to build body RBDY3
                SELECT CASE(G_clin(2:6))
                CASE('PLAIN')
                   CALL read_PLAIN(bdyty(ibdyty),iblmty)
                   !case('BLMXX')    
                   !call read_BLMXX(ibdyty,iblmty,G_clin)
                CASE default  
                END SELECT
                EXIT
             END IF
             CYCLE
          END DO
          EXIT
       END DO
       BACKSPACE(G_nfich)

       

       inodty = 0
       DO    
          IF( .NOT. read_G_clin()) EXIT  
          IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty'			 
          DO
             IF( .NOT. read_G_clin()) EXIT  
             itest=itest_nodty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN     
                CALL G_read_a_nodty(cooref,G_clin(2:6))

                call init_bdyty_( bdyty(ibdyty) )
                call init_nodty_( bdyty(ibdyty), cooref )

             END IF
             CYCLE
          END DO
          EXIT       
       END DO
       BACKSPACE(G_nfich)   
       
       itacty = 0
       DO    
          IF( .NOT. read_G_clin()) EXIT  
          IF (G_clin(2:6) /= 'tacty') CYCLE                ! fishing for the keyword 'tacty'
          DO
             IF( .NOT. read_G_clin()) EXIT  
             itest=itest_tacty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT
             IF (itest == ifound) THEN
                itacty=itacty+1

                !fd&mr 11/10/07 fait au dessus
                !
                READ(G_clin( 2: 6),'(A5)') tacID
                READ(G_clin(23:27),'(A5)') bdyty(ibdyty)%tacty(itacty)%color
                bdyty(ibdyty)%tacty(itacty)%tacID = get_contactor_id_from_name(tacID)

                !NULLIFY(bdyty(ibdyty)%tacty(itacty)%BDARY%data)
                !NULLIFY(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
                bdyty(ibdyty)%tacty(itacty)%BDARY%shift = 0.D0
                
                IF (bdyty(ibdyty)%blmty(1)%PLAIN%I1 /= 0.D0 .OR. &
                    bdyty(ibdyty)%blmty(1)%PLAIN%I2 /= 0.D0 .OR. &
                    bdyty(ibdyty)%blmty(1)%PLAIN%I3 /= 0.D0 ) THEN

                  !fd a voir pour le multi corps.
                  ! on met qq chose dedans pour court-circuiter certains
                  ! calculs des a_bdary. 
                  ! si ces valeurs sont non nulles ca veut dire que le contacteur est exprime dans
                  ! le rep d'inertie et que le dof.ini doit exister ....

                  bdyty(ibdyty)%tacty(itacty)%BDARY%I1 = bdyty(ibdyty)%blmty(1)%PLAIN%I1
                  bdyty(ibdyty)%tacty(itacty)%BDARY%I2 = bdyty(ibdyty)%blmty(1)%PLAIN%I2
                  bdyty(ibdyty)%tacty(itacty)%BDARY%I3 = bdyty(ibdyty)%blmty(1)%PLAIN%I3

                ENDIF


                SELECT CASE(bdyty(ibdyty)%tacty(itacty)%tacID)
                CASE(i_spher)
                   CALL read_BDARY_SPHER(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe)
                CASE(i_planx)
                   CALL read_BDARY_PLANx(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededFrame, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
                CASE(i_dnlyc)
                   CALL read_BDARY_DNLYC(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe)
                CASE(i_cylnd)
                   CALL read_BDARY_CYLND(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
                CASE(i_polyr)

                   !print*,"======================================="
                   !print*,'corps: ',ibdyty,' contacteur: ',itacty

                   CALL read_BDARY_POLYR(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)


                CASE(i_polyf)

                   !print*,"======================================="
                   !PRINT*,'corps: ',ibdyty,' contacteur: ',itacty

                   CALL read_BDARY_POLYF(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

                CASE(i_polyo)

                   !print*,"======================================="
                   !print*,'corps: ',ibdyty,' contacteur: ',itacty

                   CALL read_BDARY_POLYO(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)

                CASE(i_pt3dx)
                   
                   !fd par defaut les PT3Dx ont un rayon de avrd, si il vaut zero on prend 1.

                   IF (bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius == 0.D0) THEN
                     bdyty(ibdyty)%tacty(itacty)%BDARY%volume = 1.D0
                   ELSE
                     bdyty(ibdyty)%tacty(itacty)%BDARY%volume = bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius
                   ENDIF 

                   CALL read_BDARY_PT3Dx(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe,&
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
                   !bdyty(ibdyty)%tacty(itacty)%BDARY%volume = 0.D0

                CASE(i_spheb)
                   CALL read_BDARY_SPHER(bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%volume, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I1, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I2, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%I3, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                         bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
                CASE default
                   CALL FATERR(IAM,'unknown bdary')
                END SELECT
             END IF
             CYCLE
          END DO
          EXIT
       END DO
       CYCLE
    END DO
    
    CLOSE(G_nfich)

    iblmty = 1

    DO ibdyty = 1,nb_RBDY3

       SELECT CASE(bdyty(ibdyty)%blmty(iblmty)%blmID)
       CASE('PLAIN')
         call comp_blmty_(bdyty(ibdyty))
       CASE default  
          call faterr(IAM,'Unknown model')
       END SELECT

    END DO

  END SUBROUTINE read_bodies

!!!------------------------------------------------------------------------
  SUBROUTINE load_thread_network_RBDY3()

    IMPLICIT NONE
    INTEGER	:: nfich,ERR
    INTEGER	:: ibdyty,ithread

    given_thread_network = .TRUE.

    DO ibdyty = 1, nb_RBDY3
       bdyty(ibdyty)%thread = 0       
    END DO
    
    nfich=get_io_unit()
    OPEN(unit=nfich,file='DATBOX/FIL_NETWORK.DAT',status='OLD')

    ERR = 0

    DO 
       
       READ(nfich,'(I7,1X,I7)',IOSTAT=ERR) ibdyty, ithread
       IF (ERR.NE.0) EXIT
       bdyty(ibdyty)%thread = ithread

    END DO

    CLOSE(nfich)

  END SUBROUTINE load_thread_network_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE write_bodies(nfich)

    IMPLICIT NONE

    INTEGER            :: nfich
    INTEGER            :: ibdyty,iblmty,itacty,nbdof
    character(len=5)   :: tacID
    CHARACTER(len=23)  :: IAM='mod_RBDY3::write_bodies'
    CHARACTER(len=103) :: cout

    INTEGER :: k

    DO ibdyty=1,nb_RBDY3
       
       IF (skip_invisible .AND. .NOT. bdyty(ibdyty)%visible) CYCLE

       WRITE(nfich,'(A6)') '$bdyty'
       !pta: old fashion WRITE(nfich,101) bdyty(ibdyty)%bdyID,ibdyty
       WRITE(nfich,101) bdyty(ibdyty)%bdyID,get_visibleID(ibdyty)
       
       WRITE(nfich,'(A6)') '$blmty'
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          SELECT CASE(bdyty(ibdyty)%blmty(iblmty)%blmID)
          CASE('PLAIN')
             CALL write_PLAIN(nfich,bdyty(ibdyty),iblmty)                   
          CASE default  
             WRITE(cout,'(A7,A5,A18,I5)')' blmty ',bdyty(ibdyty)%blmty(iblmty)%blmID,' unknown in RBDY3 ',ibdyty
             !1234567                                     123456789012   34567   8
             CALL FATERR(IAM,cout)
          END SELECT
       END DO
       WRITE(nfich,'(A6)') '$nodty'
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty),1,bdyty(ibdyty)%cooref,'coo',nfich)

       WRITE(nfich,'(A6)') '$tacty'
       DO itacty=1,SIZE(bdyty(ibdyty)%tacty)

!          print*,ibdyty,itacty,bdyty(ibdyty)%tacty(itacty)%tacID
           tacID = get_contactor_name_from_id( bdyty(ibdyty)%tacty(itacty)%tacID )

          SELECT CASE(bdyty(ibdyty)%tacty(itacty)%tacID)
          CASE(i_spher)
             CALL write_BDARY_SPHER(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%data)
          CASE(i_planx)
             CALL write_BDARY_PLANx(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          CASE(i_dnlyc)
             CALL write_BDARY_DNLYC(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%data)
          CASE(i_cylnd)
             CALL write_BDARY_CYLND(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          CASE(i_polyr, i_polyf, i_polyo)
             CALL write_BDARY_POLYR(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%idata, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          CASE(i_pt3dx)
             CALL write_BDARY_PT3Dx(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          CASE(i_spheb)
             CALL write_BDARY_SPHER(nfich,itacty,tacID,                     &
                                    bdyty(ibdyty)%tacty(itacty)%color,      &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%data, &
                                    bdyty(ibdyty)%tacty(itacty)%BDARY%shift)
          CASE default
             WRITE(cout,'(A6,A5,A8)') 'tacty ',tacID,' unknown'
             CALL FATERR(IAM,cout)
          END SELECT
       END DO
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
    END DO
     
101 FORMAT(1X,A5,2X,I7)            
    
  END SUBROUTINE write_bodies
!!!------------------------------------------------------------------------   
  SUBROUTINE read_behaviours_RBDY3

    IMPLICIT NONE

    INTEGER            :: ibdyty,iblmty,ibehav,itest
    CHARACTER(len=26)  :: IAM='mod_RBDY3::read_behaviours'
    CHARACTER(len=103) :: cout

    DO ibdyty=1,nb_RBDY3
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          
          bdyty(ibdyty)%blmty(iblmty)%lawnb = &
               get_bulk_behav_nb(bdyty(ibdyty)%blmty(iblmty)%behav)
          
          IF (bdyty(ibdyty)%blmty(iblmty)%lawnb == 0) THEN
             
             WRITE(cout,'(A9,A5,A9,I7,A17)') 'nickname ',bdyty(ibdyty)%blmty(iblmty)%behav,& 
                  ' body nb ',ibdyty,' unknown in lawty'
             !123456789          12345678901234567        
             CALL LOGMES('check BODIES.DAT in DATBOX')
             CALL LOGMES('check BEHAVIOURS.DAT in DATBOX')
             CALL FATERR(IAM,cout)
          END IF
       END DO
       ! CYCLE
    END DO
    
  END SUBROUTINE read_behaviours_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE read_mp_behaviours_RBDY3(disper)
    !!****f* RBDY3/read_mp_behaviours_RBDY3
    !! NAME
    !!  read_mp_behaviours_RBDY3
    !! SYNPOSIS
    !!  read_mp_behaviours_RBDY3(disper)
    !! INPUTS
    !!  REAL(kind=8) : disper   variable dispersion 
    !! PURPOSE
    !!  Initialize electrical and thermal variables according to the description given
    !!  in the file BULK_BEHAV.DAT. The [disper] variable allows to give a variation in
    !!  electrical and thermal properties.
    !!****
    IMPLICIT NONE
    
    INTEGER            :: ibdyty,iblmty,ibehav,itest,itacty,nb_tacty
    REAL(kind=8)       :: X,ECond,TCond,vol,avrd,disper,WS,WSini
    CHARACTER(len=32)  :: IAM
    CHARACTER(len=103) :: cout

    !    12345678901234567890123456789012
    IAM='mod_RBDY3::read_mp_behaviours'

     DO ibdyty=1,nb_RBDY3

       !mr : we assume that iblmty = 1
       iblmty = 1
       ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb
    
       nb_tacty = SIZE(bdyty(ibdyty)%tacty)

       DO itacty=1,nb_tacty

          CALL RANDOM_NUMBER(X)

          ECond = (1+(2*X-1)*disper)*get_ECond(ibehav)
          TCond = (1+(2*X-1)*disper)*get_TCond(ibehav)

          ECond = bdyty(ibdyty)%tacty(itacty)%BDARY%volume*ECond

          bdyty(ibdyty)%tacty(itacty)%BDARY%ECond    = ECond
          bdyty(ibdyty)%tacty(itacty)%BDARY%ECondini = ECond
          bdyty(ibdyty)%tacty(itacty)%BDARY%EPot     = 0.D0

          bdyty(ibdyty)%tacty(itacty)%BDARY%TCond    = TCond
          bdyty(ibdyty)%tacty(itacty)%BDARY%TCondini = TCond
          bdyty(ibdyty)%tacty(itacty)%BDARY%T        = 0.D0
          
          CALL compute_WSvsT(ibehav,WS,0.D0)

          bdyty(ibdyty)%tacty(itacty)%BDARY%WS    = WS
          bdyty(ibdyty)%tacty(itacty)%BDARY%WSini = WS

       END DO

    END DO

  END SUBROUTINE read_mp_behaviours_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE read_in_dof

    IMPLICIT NONE

    INTEGER            :: i,ibdyty,inodty,nbdof,itest
    CHARACTER(len=5)   :: chnod
    INTEGER            :: errare
    CHARACTER(len=22)  :: IAM='mod_RBDY3::read_in_dof'
    CHARACTER(len=103) :: cout
    REAL(kind=8),DIMENSION(3) :: vect
    
    INTEGER :: l_ibdyty 

    DO ibdyty = 1,nb_RBDY3
       bdyty(ibdyty)%Xbegin =0.D0
       bdyty(ibdyty)%X      =0.D0
       bdyty(ibdyty)%Vbegin =0.D0
       bdyty(ibdyty)%V      =0.D0
    END DO

    l_ibdyty = 0
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin)
       IF (itest == ifound) THEN
          l_ibdyty = l_ibdyty + 1
!          READ(G_clin(9:15),'(I7)') ibdyty
          READ(G_clin(9:15),*) ibdyty

          IF (keep_ini_dof_order) ibdyty=l_ibdyty

          IF (ibdyty <= 0 .OR. ibdyty > nb_RBDY3) THEN
             WRITE(cout,'(A12,I7,A60)') 'body number ',ibdyty,' does not belong to collection'
             !123456789012          123456789012345678901234567890
             CALL FATERR(IAM,cout)
          END IF
       ELSE
          CYCLE
       END IF
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty'			 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_nodty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT	                      
             IF (itest == ifound) THEN
                READ(G_clin(9:13),'(I5)') inodty
                IF (inodty /= 1) THEN 
                   WRITE(cout,'(A12,I5,A25,I5,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                   !123456789012          1234567890123456789012345
                   CALL FATERR(IAM,cout)
                END IF
                       
                IF (get_node_id_from_name(G_clin(2:6)) > nbdof_a_nodty(bdyty(ibdyty)%nodty)) THEN 
                   WRITE(cout,'(A5,I5,A22,I5)') G_clin(2:6),inodty,' is not nodty of body ', ibdyty
                   !1234567890123456789012
                   CALL FATERR(IAM,cout)
                END IF
                
                nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
                
                !fd
                !Gs&Fd  on lit d'abord la translation du centre c'est un NO6xxx
                !       et ensuite les valeurs de la rotation doivent être nulle
                ! 
                chnod='NO6xx'
                CALL G_read_a_nodty(bdyty(ibdyty)%Xbegin,chnod)
                
                !fd
                !fd on lit les vitesses
                !fd    
                IF( .NOT. read_G_clin()) THEN
                   call faterr(IAM,'error reading Vbegin')
                END IF
                
                CALL G_read_a_nodty(bdyty(ibdyty)%Vbegin(1:nbdof),chnod)
                                
                !fd
                !fd on lit alpha
                !fd
                chnod='NO3xx'
                IF( .NOT. read_G_clin()) THEN
                   call faterr(IAM,'error reading alpha')
                END IF
                CALL G_read_a_nodty(bdyty(ibdyty)%LocalFrameIni(1:3,1),chnod)
                !fd
                !fd on lit beta
                !fd
                IF( .NOT. read_G_clin()) THEN
                   call faterr(IAM,'error reading beta')
                END IF
                CALL G_read_a_nodty(bdyty(ibdyty)%LocalFrameIni(1:3,2),chnod)
                !fd
                !fd on lit gamma
                !fd
                IF( .NOT. read_G_clin()) THEN
                   call faterr(IAM,'error reading gamma')
                END IF
                CALL G_read_a_nodty(bdyty(ibdyty)%LocalFrameIni(1:3,3),chnod)
                
                bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin     
                bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin 
                bdyty(ibdyty)%LocalFrameRef(1:3,1:3)=bdyty(ibdyty)%LocalFrameIni(1:3,1:3)
                bdyty(ibdyty)%LocalFrame(1:3,1:3)=bdyty(ibdyty)%LocalFrameIni(1:3,1:3)
                bdyty(ibdyty)%LocalFrameTT(1:3,1:3)=bdyty(ibdyty)%LocalFrameIni(1:3,1:3)
                
             END IF
             CYCLE
          END DO
          EXIT       
       END DO
       CYCLE
    END DO


    !fd test
    IF (XPERIODIC .OR. YPERIODIC) CALL check_periodic(.TRUE.)
    IF (BOUNDS) CALL out_of_bounds_RBDY3
    
  END SUBROUTINE read_in_dof
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_dof(nfich,ifrom,ito)
    
    IMPLICIT NONE
    INTEGER :: ibdyty,nfich,ifrom,ito,nbdof
    INTEGER :: lc 
    
    DO ibdyty = ifrom,ito
       
       IF (skip_invisible .AND. .NOT. bdyty(ibdyty)%visible) CYCLE

       WRITE(nfich,'(A6)') '$bdyty'
       !pta: old fashion WRITE(nfich,101) bdyty(ibdyty)%bdyID,ibdyty
       WRITE(nfich,101) bdyty(ibdyty)%bdyID,get_visibleID(ibdyty)
       WRITE(nfich,'(A6)') '$nodty'
       
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       
       CALL write_a_nodty('NO6xx',1, bdyty(ibdyty)%X,'X  ',nfich)
       CALL write_a_nodty('NO6xx',1, bdyty(ibdyty)%V(1:nbdof),'V  ',nfich)

       CALL write_a_nodty('alpha',1, &
            bdyty(ibdyty)%LocalFrame(1:3,1),'   ',nfich)
       CALL write_a_nodty('beta ',1, &
            bdyty(ibdyty)%LocalFrame(1:3,2),'   ',nfich)
       CALL write_a_nodty('gamma',1, &
            bdyty(ibdyty)%LocalFrame(1:3,3),'   ',nfich)
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
       
    END DO
                          !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
    
101 FORMAT(1X,A5,2X,I7)            
    
  END SUBROUTINE write_out_dof
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_Rnod(nfich,ifrom,ito)
    
    IMPLICIT NONE
    INTEGER :: ibdyty,nfich,ifrom,ito,nbdof
    INTEGER  :: lc 
    
    DO ibdyty = ifrom,ito
       
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)bdyty(ibdyty)%bdyID,ibdyty
       WRITE(nfich,'(A6)') '$nodty'
       
       nbdof = nbdof_a_nodty(bdyty(ibdyty)%nodty)
       
       CALL write_a_nodty('NO6xx',1,bdyty(ibdyty)%Ireac(:)/H,'R/H',nfich)
       
       WRITE(nfich,'(A6)') '$$$$$$'
       WRITE(nfich,'(A1)') ' '
    END DO
    
101 FORMAT(1X,A5,2X,I7)            
           
  END SUBROUTINE write_out_Rnod

  subroutine reset_driven_dof_(bdy)
    implicit none
    type(T_RBDY3), intent(inout) :: bdy

    if( associated(bdy%vlocy_driven_dof) ) deallocate(bdy%vlocy_driven_dof)
    if( associated(bdy%Vdriv)            ) deallocate(bdy%Vdriv)
    if( associated(bdy%Xdriv)            ) deallocate(bdy%Xdriv)
    if( associated(bdy%force_driven_dof) ) deallocate(bdy%force_driven_dof)
    if( associated(bdy%Fdriv)            ) deallocate(bdy%Fdriv)
    nullify(bdy%vlocy_driven_dof)
    nullify(bdy%Vdriv)
    nullify(bdy%Xdriv)
    nullify(bdy%force_driven_dof)
    nullify(bdy%Fdriv)
    bdy%nb_vlocy_driven_dof=0
    bdy%nb_force_driven_dof=0

  end subroutine

  subroutine set_nb_vdriv_(bdy, nb_driv)
    implicit none
    type(T_RBDY3), intent(inout) :: bdy
    integer      , intent(in)    :: nb_driv

    bdy%nb_vlocy_driven_dof = 0
    if( nb_driv > 0 ) then
      allocate( bdy%vlocy_driven_dof(nb_driv) )
      allocate( bdy%Vdriv(nb_driv) )
      allocate( bdy%Xdriv(nb_driv) )
    end if

  end subroutine

  subroutine set_nb_fdriv_(bdy, nb_driv)
    implicit none
    type(T_RBDY3), intent(inout) :: bdy
    integer      , intent(in)    :: nb_driv

    bdy%nb_force_driven_dof = 0
    if( nb_driv > 0 ) then
      allocate( bdy%force_driven_dof(nb_driv) )
      allocate( bdy%Fdriv(nb_driv) )
    end if

  end subroutine

!!!------------------------------------------------------------------------   
  SUBROUTINE read_driven_dof

    IMPLICIT NONE
    INTEGER :: ivd,ifd,ibdyty,inodty,dofnb,itest
    integer           :: chnod
    INTEGER           :: errare
    CHARACTER(len=26) :: IAM='mod_RBDY3::read_driven_dof'
    CHARACTER(len=72) :: cout
    
    do ibdyty = 1, nb_RBDY3
      call reset_driven_dof_( bdyty(ibdyty) )
      bdyty(ibdyty)%thread = ibdyty
    end do

    IF (nb_rbdy3 == 0) RETURN

    ! first reading: sizing array vlocy_driven_dof  

    DO    
 
       ivd=0
       ifd=0
       
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin) 
       IF (itest /= ifound) CYCLE
!       READ(G_clin(9:15),'(I7)')ibdyty
       READ(G_clin(7:15),*) ibdyty

       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty'			 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_nodty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT                  
             IF (itest == ifound) THEN
                DO
                   IF( .NOT. read_G_clin()) EXIT
                   IF (G_clin(2:6) /= 'dofty') CYCLE          ! fishing for the keyword 'dofty'
                   DO
                      IF( .NOT. read_G_clin()) EXIT
                      SELECT CASE(G_clin(2:6))
                      CASE('vlocy') 
                         ivd=ivd+1
                      CASE('force') 
                         ifd=ifd+1
                      CASE default
                         BACKSPACE(G_nfich)
                         EXIT
                      END SELECT
                      CYCLE
                   END DO
                   EXIT
                END DO
             END IF
             CYCLE
          END DO
          EXIT
       END DO

       call set_nb_vdriv_( bdyty(ibdyty), ivd )
       call set_nb_fdriv_( bdyty(ibdyty), ifd )

       CYCLE
    END DO
    
    IF (SUM(bdyty(1:nb_RBDY3)%nb_vlocy_driven_dof) == 0) CALL LOGMES('warning: no RBDY3 with vlocy_driven_dof')
    IF (SUM(bdyty(1:nb_RBDY3)%nb_force_driven_dof) == 0) CALL LOGMES('warning: no RBDY3 with force_driven_dof')
    
    ! second reading: filling in data

    REWIND(G_nfich)

    DO
       ivd=0
       ifd=0
       
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty' 
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty(G_clin)                      
       IF (itest /= ifound) CYCLE
!       READ(G_clin(9:15),'(I7)') ibdyty
       READ(G_clin(7:15),*) ibdyty
       IF (ibdyty <= 0 .OR. ibdyty > nb_RBDY3) THEN
          
          WRITE(cout,'(A12,I5,A30)') 'body number ',ibdyty,' does not belong to collection'
          !123456789012          123456789012345678901234567890
          CALL FATERR(IAM,cout)
       END IF
       DO    
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty'			 
          DO
             IF( .NOT. read_G_clin()) EXIT
             itest=itest_nodty(G_clin,ibdyty)
             IF (itest == isskip) CYCLE
             IF (itest == inomor) EXIT                      
             IF (itest == ifound) THEN
                chnod= get_node_id_from_name( G_clin(2:6) )
                READ(G_clin(9:13),'(I5)') inodty
                IF (inodty /= 1 ) THEN 
                   
                   WRITE(cout,'(A12,I5,A25,I5)') 'node number ',inodty,' does not belong to body ',ibdyty
                   !123456789012          1234567890123456789012345
                   CALL FATERR(IAM,cout)
                ENDIF
                DO
                   IF( .NOT. read_G_clin()) EXIT
                   IF (G_clin(2:6) /= 'dofty') CYCLE          ! fishing for the keyword 'dofty'
                   DO
                      IF( .NOT. read_G_clin()) EXIT
                      SELECT CASE(G_clin(2:6))
                      CASE('vlocy') 
                         ivd=ivd+1
                         
                         READ(G_clin( 9: 13),'(I5)   ')dofnb
                         
                         IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty)) THEN
                            
                            WRITE(cout,'(A11,I5,A25,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                            !12345678901          1234567890123456789012345
                            CALL FATERR(IAM,cout)
                         ENDIF

                         CALL read_a_driven_dof(ibdyty,chnod,inodty,G_clin,bdyty(ibdyty)%vlocy_driven_dof(ivd))
                         ! rm: must now update count of driven dof when adding
                         bdyty(ibdyty)%nb_vlocy_driven_dof = ivd
                         
                      CASE('force')
                         ifd=ifd+1
                         
                         READ(G_clin( 9: 13),'(I5)   ')dofnb
                         
                         IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty)) THEN
                            WRITE(cout,'(A11,I5,A25,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                            !12345678901          1234567890123456789012345
                            CALL FATERR(IAM,cout)
                         ENDIF
                         
                         CALL read_a_driven_dof(ibdyty,chnod,inodty,G_clin,bdyty(ibdyty)%force_driven_dof(ifd))
                         ! rm: must now update count of driven dof when adding
                         bdyty(ibdyty)%nb_force_driven_dof = ifd
                         
                      CASE default
                         BACKSPACE(G_nfich)
                         EXIT
                      END SELECT
                      CYCLE
                   END DO
                   EXIT
                END DO
             END IF
             CYCLE
          END DO
          EXIT
       END DO
       CYCLE
    END DO
    
  END SUBROUTINE read_driven_dof
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  SUBROUTINE write_driven_dof(nfich)

    IMPLICIT NONE

    INTEGER :: nfich
    INTEGER :: ivd,ifd,ibdyty,iccdof

    DO ibdyty=1,nb_RBDY3
       IF ((bdyty(ibdyty)%nb_vlocy_driven_dof /= 0 ) .OR. (bdyty(ibdyty)%nb_force_driven_dof /= 0 )) THEN

          !pta
          IF (skip_invisible .AND. .NOT. bdyty(ibdyty)%visible) CYCLE

          WRITE(nfich,'(A6)') '$bdyty'
          !pta old fashion WRITE(nfich,101) bdyty(ibdyty)%bdyID,ibdyty
          WRITE(nfich,101) bdyty(ibdyty)%bdyID,get_visibleID(ibdyty)
          
          WRITE(nfich,'(A6)') '$nodty'
          WRITE(nfich,101) get_nodNAME(bdyty(ibdyty)%nodty),1
          CALL write_a_driven_dof(nfich)
          
          DO iccdof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty)
             
             DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
                IF (is_a_driven_dof(iccdof,bdyty(ibdyty)%vlocy_driven_dof(ivd))) THEN
                   CALL write_a_driven_dof(nfich,'vlocy',bdyty(ibdyty)%vlocy_driven_dof(ivd))
                END IF
             END DO
             
             DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
                IF (is_a_driven_dof(iccdof,bdyty(ibdyty)%force_driven_dof(ifd))) THEN
                   CALL write_a_driven_dof(nfich,'force',bdyty(ibdyty)%force_driven_dof(ifd))
                END IF
             END DO
             
          END DO
          WRITE(nfich,'(A6)')'$$$$$$'
          WRITE(nfich,'(A1)')' '
       ENDIF
    END DO
    
!pta old fashion 101   FORMAT(1X,A5,2X,I7)    
101   FORMAT(1X,A5,I7)    

  END SUBROUTINE write_driven_dof
!!!------------------------------------------------------------------------  

!!!------------------------------------------------------------------------  
  !fd this subroutine computes the detection configuration, i.e. coorTT and LocalFrameTT
  subroutine compute_configurationTT_RBDY3

     implicit none

     ! locals
     integer :: ibdyty
     real(kind=8), dimension(3) :: spin

     if (nb_RBDY3 == 0) return

     if (.not. is_contactdetectionconfiguration_defined) then
       vw_b = 1.d0 - theta
       vw_e = 0.
       !call logmes('RBDY3::default contact configuration is used')
     endif

     do ibdyty=1, nb_RBDY3
        if (.not. bdyty(ibdyty)%visible) cycle

        bdyty(ibdyty)%coorTT(1:3) = bdyty(ibdyty)%cooref(1:3) + bdyty(ibdyty)%Xbegin(1:3) + &
                                    (H*(vw_b*bdyty(ibdyty)%Vbegin(1:3) + vw_e*bdyty(ibdyty)%V(1:3)))

        ! compute LocalFrameTT
        if ( new_rotation_scheme) then

          !spin(1:3) = bdyty(ibdyty)%Vbegin(4:6)
          !call update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrameTT)

          ! pas bon voir Integration.pdf
          !spin(1:3) = bdyty(ibdyty)%V(4:6)
          !call update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrameTT,theta)

          spin(1:3) = vw_b*bdyty(ibdyty)%Vbegin(4:6) + vw_e*bdyty(ibdyty)%V(4:6)
          call update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrameTT)

        else 

          spin(1:3) = (1.d0 - theta)*bdyty(ibdyty)%Vbegin(4:6)

          call update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameIni,bdyty(ibdyty)%LocalFrameTT)

        endif              

     end do

  end subroutine compute_configurationTT_RBDY3
!!!------------------------------------------------------------------------  

!!!------------------------------------------------------------------------  
  !> TODO la gestion des rotations pour tous les integrateurs car ca ne marche correctement qu avec des spheres 
  SUBROUTINE increment_RBDY3
    
    IMPLICIT NONE 
    INTEGER                     :: ibdyty,ivd,iccdof,k
    REAL(kind=8)                :: Vdrivenbegin,Vdriven,Xdrivenbegin,Xdriven,UMTTH,TTH
    REAL(kind=8),DIMENSION(6)   :: XC
    REAL(kind=8),DIMENSION(3)   :: spin    

    IF (nb_RBDY3 == 0) RETURN
    
    SELECT CASE(M_INTEGRATOR_ID)
    !!!-----------------------------------------------------------------------------------------------
    CASE(INTEGRATOR_MOREAU)
    
       UMTTH =  (1.D0-THETA)*H
         TTH =         THETA*H


       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ivd,iccdof,Xdrivenbegin,Xdriven,Vdrivenbegin,Vdriven)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3
    
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          
          bdyty(ibdyty)%Ireac = 0.d0
          bdyty(ibdyty)%Iaux  = 0.d0
          
          ! initialisation des variables courantes du schema
          bdyty(ibdyty)%V      = bdyty(ibdyty)%Vbegin
          bdyty(ibdyty)%X(1:3) = bdyty(ibdyty)%Xbegin(1:3)
          
          ! bdyty(ibdyty)%X(1:3) = bdyty(ibdyty)%Xbegin(1:3) + &
          !                        UMTTH*bdyty(ibdyty)%Vbegin(1:3) + &
          !                        TTH*bdyty(ibdyty)%V(1:3)

          ! computing driven dof
          
          DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
             
             CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
             
             Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
             Xdriven      = Xdrivenbegin + UMTTH*Vdrivenbegin+TTH*Vdriven
             bdyty(ibdyty)%Vdriv(ivd) = Vdriven
             bdyty(ibdyty)%Xdriv(ivd) = Xdriven 
          
          END DO
          
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
       END DO

       !$OMP END DO
       !$OMP END PARALLEL
 
    !!!-----------------------------------------------------------------------------------------------
    CASE(INTEGRATOR_GEAR)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ivd,iccdof,Xdrivenbegin,Xdriven,Vdrivenbegin,Vdriven)
       !$OMP DO SCHEDULE(RUNTIME)

        DO ibdyty=1,nb_RBDY3
       
          IF (.NOT. bdyty(ibdyty)%visible) CYCLE

          bdyty(ibdyty)%Ireac = 0.d0
          bdyty(ibdyty)%Iaux  = 0.d0
                 
          bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin + &
                        c1*bdyty(ibdyty)%Vbegin + c2*bdyty(ibdyty)%Abegin + &
                        c3*bdyty(ibdyty)%Bbegin + c4*bdyty(ibdyty)%Cbegin
          bdyty(ibdyty)%V= bdyty(ibdyty)%Vbegin + &
                        c1*bdyty(ibdyty)%Abegin + c2*bdyty(ibdyty)%Bbegin + &
                        c3*bdyty(ibdyty)%Cbegin
          bdyty(ibdyty)%A= bdyty(ibdyty)%Abegin + &
                        c1*bdyty(ibdyty)%Bbegin + c2*bdyty(ibdyty)%Cbegin
          bdyty(ibdyty)%B= bdyty(ibdyty)%Bbegin + &
                        c1*bdyty(ibdyty)%Cbegin               
       
          IF (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
       
          ! computing driven dof
       
          DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
          
             CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)
          
             iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          
             Xdrivenbegin             = bdyty(ibdyty)%Xbegin(iccdof)
             Xdriven                  = Xdrivenbegin + c1*Vdrivenbegin
             bdyty(ibdyty)%Vdriv(ivd) = Vdriven
             bdyty(ibdyty)%Xdriv(ivd) = Xdriven 
             
          END DO
       
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    !!!-----------------------------------------------------------------------------------------------
    CASE(INTEGRATOR_VERLET)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ivd,iccdof,Xdrivenbegin,Xdriven,Vdrivenbegin,Vdriven)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3
          
          IF (.NOT. bdyty(ibdyty)%visible) CYCLE
          
          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + H*bdyty(ibdyty)%Vbegin + 0.5*H*H*bdyty(ibdyty)%Abegin
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin + 0.5*H*bdyty(ibdyty)%Abegin

          IF (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
          
          ! computing driven dof
          
          DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
          
             CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),Vdrivenbegin,Vdriven)
             
             iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
             
             Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
             Xdriven      = Xdrivenbegin + H*Vdrivenbegin + 0.5*H*(Vdriven-Vdrivenbegin)
             bdyty(ibdyty)%Vdriv(ivd) = Vdriven
             bdyty(ibdyty)%Xdriv(ivd) = Xdriven 
             
          END DO
          
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
       END DO

       !$OMP END DO
       !$OMP END PARALLEL
    !!!-----------------------------------------------------------------------------------------------
    CASE DEFAULT

       CALL faterr('RBDY3::increment','INTEGRATOR NOT SUPPORTED YET!')

    END SELECT
    
  END SUBROUTINE increment_RBDY3
!!!-----------------------------------------------------------------------------------------------

  logical function is_dof_driven_RBDY3(ibdyty)
    implicit none
    integer, intent(in) :: ibdyty
    !
    integer :: ivd

    is_dof_driven_RBDY3 = .false.

    if( bdyty(ibdyty)%nb_vlocy_driven_dof /= 6 ) return

    is_dof_driven_RBDY3 = .true.
    do ivd = 1, bdyty(ibdyty)%nb_vlocy_driven_dof

      if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active ) then
        is_dof_driven_RBDY3 = .false.
        exit
      end if

    end do

  end function is_dof_driven_RBDY3

!!!-----------------------------------------------------------------------------------------------
  !> Set the value of a velocity driven dof
  SUBROUTINE set_vlocy_drvdof_RBDY3(ibdyty, idrvdof, Vdriven)
    IMPLICIT NONE 
    INTEGER(kind=4), INTENT(in) :: ibdyty       !< body number
    INTEGER(kind=4), INTENT(in) :: idrvdof      !< driven dof index
    REAL(kind=8),    INTENT(in) :: Vdriven      !< Vdriv at end of time step
    !
    INTEGER(kind=4)   :: ivd, iccdof, i
    REAL(kind=8)      :: Vdrivenbegin, Xdrivenbegin, Xdriven
    !                           1234567890123456789012
    CHARACTER(len=22) :: IAM = 'set_vlocy_drvdof_RBDY3'

    IF ( ibdyty < 1 .or. ibdyty > nb_RBDY3 ) CALL faterr(IAM,'wrong RBDY3 index')

    ivd = 0
    DO i = 1, bdyty(ibdyty)%nb_vlocy_driven_dof
      IF( bdyty(ibdyty)%vlocy_driven_dof(i)%dofnb == idrvdof ) ivd=i
    END DO

    Vdrivenbegin = bdyty(ibdyty)%Vbegin(idrvdof)

    IF( ivd == 0 ) CALL faterr(IAM,'driven dof index not found')

    SELECT CASE(M_INTEGRATOR_ID)
    !!!-----------------------------------------------------------------------------------------------
    CASE(INTEGRATOR_MOREAU)

      IF( .NOT. bdyty(ibdyty)%visible ) RETURN
      
      iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))

      Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
      Xdriven      = Xdrivenbegin + (1.D0-THETA)*H*Vdrivenbegin+THETA*H*Vdriven
      bdyty(ibdyty)%Vdriv(ivd) = Vdriven
      bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

      CALL apply_vlocy_driven_dof(ibdyty,iV____)

    !!!-----------------------------------------------------------------------------------------------
    CASE(INTEGRATOR_GEAR)

      IF( .NOT. bdyty(ibdyty)%visible ) RETURN
      
      iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
      
      Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
      Xdriven      = Xdrivenbegin + c1*Vdrivenbegin
      bdyty(ibdyty)%Vdriv(ivd) = Vdriven
      bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

      CALL apply_vlocy_driven_dof(ibdyty,iV____)

    !!!-----------------------------------------------------------------------------------------------
    CASE(INTEGRATOR_VERLET)

      IF( .NOT. bdyty(ibdyty)%visible ) RETURN
    
      iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
      
      Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
      Xdriven      = Xdrivenbegin + H*Vdrivenbegin
      bdyty(ibdyty)%Vdriv(ivd) = Vdriven
      bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

      CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
    !!!-----------------------------------------------------------------------------------------------
    CASE default

      CALL FATERR(IAM,'INTEGRATOR NOT SUPPORTED YET!')

    END SELECT

  END SUBROUTINE set_vlocy_drvdof_RBDY3
!!!------------------------------------------------------------------------  

!!!------------------------------------------------------------------------ 
  SUBROUTINE partial_damping_RBDY3(iv,Vmax)
    implicit none
    !input
    integer, intent(in) :: iv
    real(kind=8), intent(in) :: Vmax   
    !dummy
    integer :: ibdyty
    real(kind=8) :: normV,ratio   
    
    if (nb_RBDY3 == 0) return
    IF (MODULO(Nstep,iv) .NE.0) RETURN

    ! imposing a vanishing begin velocity
    DO ibdyty=1,nb_RBDY3
       if (.not. bdyty(ibdyty)%visible) cycle
       normV = dsqrt((bdyty(ibdyty)%Vbegin(1)**2)+ &
                     (bdyty(ibdyty)%Vbegin(2)**2)+ &
                     (bdyty(ibdyty)%Vbegin(3)**2))
       ratio = normV/Vmax
       if ( ratio > 1.D0 ) then
          bdyty(ibdyty)%Vbegin(1:3)= bdyty(ibdyty)%Vbegin(1:3)/ratio
          bdyty(ibdyty)%V(1:3)     = bdyty(ibdyty)%Vbegin(1:3)
       end if
    END DO
    
  END SUBROUTINE partial_damping_RBDY3
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  subroutine fatal_damping_RBDY3(ibdyty)
    implicit none 
    integer(kind=4), intent(in),optional :: ibdyty
    !
    integer(kind=4)   :: i
    character(len=40) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = 'RBDY3:fatal_damping'

    if (nb_RBDY3 == 0) return

    if (present(ibdyty)) then
      if( .not. bdyty(ibdyty)%visible ) return
    
      if( ibdyty < 1 .or. ibdyty > nb_RBDY3 ) then
        write(cout,'(A,1x,I0)') 'Unknown body:', ibdyty
        call faterr(IAM,cout)
      end if
    
      bdyty(ibdyty)%Vbegin= 0.d0
      bdyty(ibdyty)%V=0.D0

    else
      do i = 1,nb_RBDY3
        if( .not. bdyty(i)%visible ) cycle
        bdyty(i)%Vbegin=0.D0
        bdyty(i)%V=0.D0                                
      enddo
    endif   
  end subroutine fatal_damping_RBDY3
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  SUBROUTINE without_rotation_of_RBDY3

    IMPLICIT NONE 
    INTEGER :: ibdyty
    
    DO ibdyty=1,nb_RBDY3
       bdyty(ibdyty)%Vbegin(4:6)= 0.D0    
       bdyty(ibdyty)%V(4:6)     = 0.D0    
    END DO
    
  END SUBROUTINE without_rotation_of_RBDY3
!!!------------------------------------------------------------------------    

!!!------------------------------------------------------------------------    
  SUBROUTINE update_dof_RBDY3

    IMPLICIT NONE 
    INTEGER :: ibdyty
    
    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          bdyty(ibdyty)%Vbegin        = bdyty(ibdyty)%V
          bdyty(ibdyty)%Xbegin        = bdyty(ibdyty)%X
          bdyty(ibdyty)%LocalFrameIni = bdyty(ibdyty)%LocalFrame
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE(INTEGRATOR_GEAR)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          bdyty(ibdyty)%Xbegin = bdyty(ibdyty)%X 
          bdyty(ibdyty)%Vbegin = bdyty(ibdyty)%V                
          bdyty(ibdyty)%Abegin = bdyty(ibdyty)%A
          bdyty(ibdyty)%Bbegin = bdyty(ibdyty)%B
          bdyty(ibdyty)%Cbegin = bdyty(ibdyty)%C
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE(INTEGRATOR_VERLET)    

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          bdyty(ibdyty)%Xbegin = bdyty(ibdyty)%X 
          bdyty(ibdyty)%Vbegin = bdyty(ibdyty)%V                
          bdyty(ibdyty)%Abegin = bdyty(ibdyty)%A
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE DEFAULT

       CALL faterr('RBDY3::update_dof','INTEGRATOR NOT SUPPORTED YET!')

    END SELECT

  END SUBROUTINE update_dof_RBDY3
!!!------------------------------------------------------------------------  
  SUBROUTINE comp_free_vlocy_RBDY3
  
    IMPLICIT NONE
    INTEGER :: ibdyty

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP PRIVATE(ibdyty)
    !$OMP DO SCHEDULE(RUNTIME)

    DO ibdyty=1,nb_RBDY3
       IF(.NOT.bdyty(ibdyty)%visible) CYCLE       
       bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vbegin &
                          + H*bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Fext & 
                          + bdyty(ibdyty)%Fint)
       
       IF ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
       
       CALL apply_vlocy_driven_dof(ibdyty,iVfree)

       if (bdyty(ibdyty)%nb_vlocy_driven_dof == 6) call set_clamped_status_ENTITY(get_entity_RBDY3(ibdyty))
       
    END DO

    !$OMP END DO
    !$OMP END PARALLEL

    !fd si rien n'est fait on calcule avec des valeurs par defaut
    if (.not. is_contactdetectionconfiguration_defined) then

       call logmes('RBDY3::contact configuration implicitely computed')

       call compute_configurationTT_RBDY3

    endif


  END SUBROUTINE comp_free_vlocy_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE comp_dof_RBDY3
    IMPLICIT NONE 
    INTEGER                     :: ibdyty,k,l
    REAL(kind=8)                :: TTH,UMTTH
    REAL(kind=8),DIMENSION(6)   :: corr,acce

    REAL(kind=8),DIMENSION(3) :: spin

    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)

       TTH   = THETA*H
       UMTTH = (1.d0-THETA)*H

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,spin)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3

          IF (.NOT. bdyty(ibdyty)%visible) CYCLE

          ! Reac(1:3) resultante dans R0
          ! Reac(4:6) moment dans RG_M
          !
          ! V(1:3) vitesse de translation dans R0
          ! V(4:6) vitesse de rotation dans RG_M
          
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%Ireac

          ! actualisation de la translation X

          !am : pour pouvoir faire plusieurs compute_dof sans cumuler les corrections

          bdyty(ibdyty)%X(1:3) = bdyty(ibdyty)%Xbegin(1:3) + & 
                                 UMTTH*bdyty(ibdyty)%Vbegin(1:3) + &
                                 TTH*bdyty(ibdyty)%V(1:3) 
          !mr useless
          bdyty(ibdyty)%X(4:6) = 0.D0
       
          ! actualisation du repere principal d'inertie
       
          IF ( new_rotation_scheme ) THEN
          
             !bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT

             spin(1:3) = bdyty(ibdyty)%V(4:6)
             CALL update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameini,bdyty(ibdyty)%LocalFrame)
          
          ELSE

             spin(1:3) = theta*bdyty(ibdyty)%V(4:6)
             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)

          END IF


 
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
       
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
       
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE(INTEGRATOR_GEAR) 

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,acce,corr,spin)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3
       
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE

          acce = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac/H + bdyty(ibdyty)%Fext)
          corr = acce - bdyty(ibdyty)%A
          
          bdyty(ibdyty)%X=bdyty(ibdyty)%X + cr*corr 
          bdyty(ibdyty)%V=bdyty(ibdyty)%V + cv*corr
          bdyty(ibdyty)%A=acce
          bdyty(ibdyty)%B=bdyty(ibdyty)%B + cb*corr
          bdyty(ibdyty)%C=bdyty(ibdyty)%C + cc*corr

          !!!mr pas correct voir plus tard

          IF ( new_rotation_scheme ) THEN
          
             bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT
          
          ELSE

             spin(1:3) = bdyty(ibdyty)%V(4:6)

             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)

             !mr useless
             bdyty(ibdyty)%X(4:6) = 0.D0

          END IF
          
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
          
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE(INTEGRATOR_VERLET)    

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,spin)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3

          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
       
          bdyty(ibdyty)%A = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%IReac/H+bdyty(ibdyty)%Fext)
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin + 0.5*H*(bdyty(ibdyty)%A+bdyty(ibdyty)%Abegin)
       
          IF ( new_rotation_scheme ) THEN
          
             bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT
          
          ELSE

             spin(1:3) = bdyty(ibdyty)%V(4:6) + 0.5*H*bdyty(ibdyty)%A(4:6)

             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)

          END IF
          
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
       
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
       
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE DEFAULT

       CALL faterr('RBDY3::comp_dof','INTEGRATOR NOT SUPPORTED YET!')

    END SELECT

    !IF (XPERIODIC .OR. YPERIODIC) CALL check_periodic
    IF (BOUNDS) CALL out_of_bounds_RBDY3

  END SUBROUTINE comp_dof_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE comp_V_RBDY3
    
    IMPLICIT NONE 
    INTEGER                     :: ibdyty

                                     !1234567890123
    CHARACTER(len=13)         :: IAM='RBDY3::comp_V'

    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3

          IF (.NOT. bdyty(ibdyty)%visible) CYCLE

          ! Reac(1:3) resultante dans R0
          ! Reac(4:6) moment dans RG_M
          !
          ! V(1:3) vitesse de translation dans R0
          ! V(4:6) vitesse de rotation dans RG_M
          
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%Ireac
 
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE
       
          CALL apply_vlocy_driven_dof(ibdyty,iV____)
       
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE DEFAULT

       CALL FATERR(IAM,'INTEGRATOR NOT SUPPORTED YET!')

    END SELECT

  END SUBROUTINE comp_V_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE comp_X_localFrame_RBDY3
    
    IMPLICIT NONE 
    INTEGER                     :: ibdyty

    REAL(kind=8),DIMENSION(3) :: spin
                                     !123456789012345678901234
    CHARACTER(len=24)         :: IAM='RBDY3::comp_X_localFrame'

    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,spin)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3

          IF (.NOT. bdyty(ibdyty)%visible) CYCLE

          ! Reac(1:3) resultante dans R0
          ! Reac(4:6) moment dans RG_M

          ! actualisation de la translation X

          !am : pour pouvoir faire plusieurs compute_dof sans cumuler les corrections
          bdyty(ibdyty)%X(1:3) = bdyty(ibdyty)%Xbegin(1:3) + & 
                                 H*(((1.d0-THETA)*bdyty(ibdyty)%Vbegin(1 : 3)) + &
                                           (THETA*bdyty(ibdyty)%V(1 : 3))) 
       
          !mr useless
          bdyty(ibdyty)%X(4:6) = 0.D0


          ! actualisation du repere principal d'inertie
       
          IF ( new_rotation_scheme ) THEN
          
             !bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT

             spin(1:3) = bdyty(ibdyty)%V(4:6)
             CALL update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameini,bdyty(ibdyty)%LocalFrame)
          
          ELSE

             spin(1:3) = theta*bdyty(ibdyty)%V(4:6)

             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)


          END IF
 
       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE DEFAULT

       CALL FATERR(IAM,'INTEGRATOR NOT SUPPORTED YET!')

    END SELECT

    !IF (XPERIODIC .OR. YPERIODIC) CALL check_periodic
    IF (BOUNDS) CALL out_of_bounds_RBDY3

  END SUBROUTINE comp_X_localFrame_RBDY3
!!!------------------------------------------------------------------------  
  SUBROUTINE check_periodic(iwantbegin)

    IMPLICIT NONE
    INTEGER :: ibdyty

    character(len=80) :: cout
    logical           :: begin
    logical,optional  :: iwantbegin
    real(kind=8)      :: coor_

    begin=.FALSE.
    if (present(iwantbegin)) begin = iwantbegin
    
    !fd dgb
    !print*,'on check periodic'

    IF( XPERIODIC ) THEN
       DO ibdyty=1,nb_RBDY3
          !bdyty(ibdyty)%xperiode = 0
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE

          if (begin) then
            IF( (bdyty(ibdyty)%cooref(1) + bdyty(ibdyty)%Xbegin(1) ) >= xperiode )THEN
               !pta CALL LOGMES('reaches X+'); PRINT*,ibdyty
               write(cout,'("body ",I0)') ibdyty
               CALL LOGMES('reaches X+ : ');
               CALL LOGMES(cout); 
               !print*,bdyty(ibdyty)%cooref(1)+bdyty(ibdyty)%X(1)
               bdyty(ibdyty)%Xbegin(1) = bdyty(ibdyty)%Xbegin(1) - xperiode
               !bdyty(ibdyty)%xperiode = 1
               !print*,bdyty(ibdyty)%X(1)
            ELSE IF( (bdyty(ibdyty)%cooref(1) + bdyty(ibdyty)%Xbegin(1) ) < 0.D0 ) THEN
               !pta CALL LOGMES('reaches X-'); PRINT*,ibdyty
               write(cout,'("body ",I0)') ibdyty
               CALL LOGMES('reaches X- : ');
               CALL LOGMES(cout); 
               bdyty(ibdyty)%Xbegin(1) = bdyty(ibdyty)%Xbegin(1) + xperiode
               !bdyty(ibdyty)%xperiode =-1
            END IF
          else

            call faterr('check_periodic','disabled') 
             
            coor_ = modulo(bdyty(ibdyty)%cooref(1) + bdyty(ibdyty)%X(1),xperiode)          
            bdyty(ibdyty)%X(1) = bdyty(ibdyty)%cooref(1) - coor_
              
            ! IF( (bdyty(ibdyty)%cooref(1) + bdyty(ibdyty)%X(1) ) >= xperiode )THEN
            !    !pta CALL LOGMES('reaches X+'); PRINT*,ibdyty
            !    write(cout,'("body ",I0)') ibdyty
            !    CALL LOGMES('reaches X+ : ');
            !    CALL LOGMES(cout); 
            !    !print*,bdyty(ibdyty)%cooref(1)+bdyty(ibdyty)%X(1)
            !    bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) - xperiode
            !    !bdyty(ibdyty)%xperiode = 1
            !    !print*,bdyty(ibdyty)%X(1)
            ! ELSE IF( (bdyty(ibdyty)%cooref(1) + bdyty(ibdyty)%X(1) ) < 0.D0 ) THEN
            !    !pta CALL LOGMES('reaches X-'); PRINT*,ibdyty
            !    write(cout,'("body ",I0)') ibdyty
            !    CALL LOGMES('reaches X- : ');
            !    CALL LOGMES(cout); 
            !    bdyty(ibdyty)%X(1) = bdyty(ibdyty)%X(1) + xperiode
            !    !bdyty(ibdyty)%xperiode =-1
            ! END IF
          endif
        END DO
    END IF

    IF( YPERIODIC ) THEN
       DO ibdyty=1,nb_RBDY3
          bdyty(ibdyty)%yperiode = 0
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          if (begin) then
            IF( (bdyty(ibdyty)%Xbegin(2)+bdyty(ibdyty)%cooref(2) ) >= yperiode )THEN
               bdyty(ibdyty)%Xbegin(2) = bdyty(ibdyty)%Xbegin(2) - yperiode
               bdyty(ibdyty)%yperiode = 1
               !pta CALL LOGMES('reaches Y+'); PRINT*,ibdyty
               write(cout,'("body ",I0)') ibdyty
               CALL LOGMES('reaches Y+ : ');
               CALL LOGMES(cout); 
            ELSE IF( (bdyty(ibdyty)%Xbegin(2)+bdyty(ibdyty)%cooref(2) ) < 0.D0 ) THEN
               bdyty(ibdyty)%Xbegin(2) = bdyty(ibdyty)%Xbegin(2) + yperiode
               bdyty(ibdyty)%yperiode =-1
               !pta CALL LOGMES('reaches Y-'); PRINT*,ibdyty
               write(cout,'("body ",I0)') ibdyty
               CALL LOGMES('reaches Y- : '); 
               CALL LOGMES(cout); 
            END IF
             
          else

            call faterr('check_periodic','disabled')
             
            coor_ = modulo(bdyty(ibdyty)%cooref(2) + bdyty(ibdyty)%X(2),yperiode)          
            bdyty(ibdyty)%X(2) = bdyty(ibdyty)%cooref(2) - coor_
            
            ! IF( (bdyty(ibdyty)%X(2)+bdyty(ibdyty)%cooref(2) ) >= yperiode )THEN
            !    bdyty(ibdyty)%X(2) = bdyty(ibdyty)%X(2) - yperiode
            !    bdyty(ibdyty)%yperiode = 1
            !    !pta CALL LOGMES('reaches Y+'); PRINT*,ibdyty
            !    write(cout,'("body ",I0)') ibdyty
            !    CALL LOGMES('reaches Y+ : ');
            !    CALL LOGMES(cout); 
            ! ELSE IF( (bdyty(ibdyty)%X(2)+bdyty(ibdyty)%cooref(2) ) < 0.D0 ) THEN
            !    bdyty(ibdyty)%X(2) = bdyty(ibdyty)%X(2) + yperiode
            !    bdyty(ibdyty)%yperiode =-1
            !    !pta CALL LOGMES('reaches Y-'); PRINT*,ibdyty
            !    write(cout,'("body ",I0)') ibdyty
            !    CALL LOGMES('reaches Y- : '); 
            !    CALL LOGMES(cout); 
            ! END IF
          endif
        END DO
    END IF

  END SUBROUTINE check_periodic
!!!------------------------------------------------------------------------  
  SUBROUTINE apply_vlocy_driven_dof(ibdyty,storage)

    IMPLICIT NONE 
    
    INTEGER         :: ivd,iccdof
    INTEGER         :: storage
    
    INTEGER(KIND=4) :: ibdyty

    IF(.NOT.bdyty(ibdyty)%visible) RETURN

    DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
  
       if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active) cycle

       iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
       SELECT CASE(storage)
       CASE(iV____)
          bdyty(ibdyty)%V(iccdof)=bdyty(ibdyty)%Vdriv(ivd)    
          bdyty(ibdyty)%X(iccdof)=bdyty(ibdyty)%Xdriv(ivd)
       CASE(iVfree)
          bdyty(ibdyty)%Vfree(iccdof)=bdyty(ibdyty)%Vdriv(ivd)
       CASE default
          CALL faterr('RBDY3::apply_vlocy_driven_dof','error vlocy type not known')
       END SELECT
    END DO

  END SUBROUTINE apply_vlocy_driven_dof
!!!------------------------------------------------------------------------   
  SUBROUTINE nullify_vlocy_driven_dof(ibdyty,storage)

    IMPLICIT NONE 

    INTEGER         :: ivd,iccdof
    INTEGER         :: storage
    
    INTEGER(KIND=4) :: ibdyty

    IF(.NOT.bdyty(ibdyty)%visible) RETURN

    SELECT CASE(storage)
    CASE (iIreac)
       ! nullifying Reac for velocity driven dof  
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active) cycle

          iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%Ireac(iccdof)=0.D0   
       END DO
    CASE (iIaux_)      
       ! nullifying Iaux for velocity driven dof  
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active) cycle

          iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%Iaux(iccdof)=0.D0   
       END DO
    CASE (iV____)
       ! nullifying V for velocity driven dof  
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active) cycle

          iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%V(iccdof)=0.D0   
       END DO
    CASE (iVaux_)      
       ! nullifying Vaux for velocity driven dof  
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

          if (.not. bdyty(ibdyty)%vlocy_driven_dof(ivd)%is_active) cycle

          iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%Vaux(iccdof)=0.D0   
       END DO
    END SELECT
    
  END SUBROUTINE nullify_vlocy_driven_dof
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_bdyty(clin)
    
    IMPLICIT NONE
    
    CHARACTER(len=103) :: clin
    
    IF (clin(2:6) == 'RBDY3') THEN
       itest_bdyty = ifound
    ELSE
       itest_bdyty = inomor
    END IF
  
  END FUNCTION itest_bdyty
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_blmty(clin,ibdyty)

    IMPLICIT NONE

    INTEGER            :: ibdyty    
    CHARACTER(len=103) :: clin
    CHARACTER(len=80)  :: cout 
    CHARACTER(len=21)  :: IAM='mod_RBDY3::test_blmty'

    SELECT CASE(clin(2:6))
    CASE('PLAIN')
       itest_blmty = ifound
    CASE('     ')
       itest_blmty = isskip
    CASE('nodty')
       itest_blmty = inomor
    CASE('$$$$$')
       itest_blmty = inomor
    CASE default
       WRITE(cout,'(A7,A5,A18,I5)')' blmty ',clin(2:6),' unknown in RBDY3 ',ibdyty
       CALL FATERR(IAM,cout)
    END SELECT
  
  END FUNCTION itest_blmty
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_nodty(clin,ibdyty)

    IMPLICIT NONE
    
    INTEGER            :: ibdyty    
    CHARACTER(len=103) :: clin
    CHARACTER(len=80)  :: cout 
    CHARACTER(len=21)  :: IAM='mod_RBDY3::test_nodty'

    IF (is_a_nodty(clin(2:6))) THEN
       itest_nodty = ifound
       RETURN
    END IF

    SELECT CASE(clin(2:6))
    CASE('     ')
       itest_nodty = isskip
    CASE('tacty')
       itest_nodty = inomor
    CASE('$$$$$')
       itest_nodty = inomor
    CASE default
       WRITE(cout,'(A7,A5,A18,I5)')' nodty ',clin(2:6),' unknown in RBDY3 ',ibdyty
       CALL FATERR(IAM,cout)
    END SELECT
  
  END FUNCTION itest_nodty
!!!------------------------------------------------------------------------
  INTEGER FUNCTION itest_tacty(clin,ibdyty) 

    IMPLICIT NONE

    INTEGER            :: ibdyty
    CHARACTER(len=103) :: clin
    CHARACTER(len=80)  :: cout 
    CHARACTER(len=21)  :: IAM='mod_RBDY3::test_tacty'

    SELECT CASE(clin(2:6))
    CASE('SPHER','SPHEb','POINT','DNLYC','CYLND','PLANx','POLYR','POLYF','POLYO','PT3Dx','PLANb')
       itest_tacty = ifound
    CASE('     ') 
       itest_tacty = isskip
    CASE('$$$$$')
       itest_tacty = inomor
    CASE default
       WRITE(cout,'(A7,A5,A18,I5)')' tacty ',clin(2:6),' unknown in RBDY3 ',ibdyty
       CALL FATERR(IAM,cout)
    END SELECT

  END FUNCTION itest_tacty
!!!------------------------------------------------------------------------
  SUBROUTINE comp_Fext_RBDY3 

    IMPLICIT NONE

    INTEGER      :: ifd,ibdyty,iccdof
    REAL(kind=8) :: Febegin,Fe

    IF (nb_RBDY3 == 0) RETURN

    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ifd,iccdof,Febegin,Fe)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3    
          ! initializing
          bdyty(ibdyty)%Fext(1:6)=0.D0
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE

          !
          bdyty(ibdyty)%Fext(1) = bdyty(ibdyty)%mass(1)*grav1
          bdyty(ibdyty)%Fext(2) = bdyty(ibdyty)%mass(2)*grav2
          bdyty(ibdyty)%Fext(3) = bdyty(ibdyty)%mass(3)*grav3
       
          !write(*,'(I0,3(1x,D12.5))')ibdyty,bdyty(ibdyty)%Vbegin(4:6) 

          bdyty(ibdyty)%Fext(4)=-(bdyty(ibdyty)%mass(6) - bdyty(ibdyty)%mass(5))*bdyty(ibdyty)%Vbegin(5)*bdyty(ibdyty)%Vbegin(6)
          bdyty(ibdyty)%Fext(5)=-(bdyty(ibdyty)%mass(4) - bdyty(ibdyty)%mass(6))*bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(6)
          bdyty(ibdyty)%Fext(6)=-(bdyty(ibdyty)%mass(5) - bdyty(ibdyty)%mass(4))*bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(5)
       
          !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%mass(4:6)
          !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Fext(4:6) 

          ! driven forces
          DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             
             CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)
          
             bdyty(ibdyty)%Fdriv(ifd)=(1.D0-THETA)*Febegin+THETA*Fe     
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))
          
             bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+bdyty(ibdyty)%Fdriv(ifd) 
          
          END DO

       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE(INTEGRATOR_GEAR)  

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ifd,iccdof,Febegin,Fe)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3    
          ! initializing
          bdyty(ibdyty)%Fext(1:6)=0.D0
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          !
          bdyty(ibdyty)%Fext(1) = bdyty(ibdyty)%mass(1)*grav1
          bdyty(ibdyty)%Fext(2) = bdyty(ibdyty)%mass(2)*grav2
          bdyty(ibdyty)%Fext(3) = bdyty(ibdyty)%mass(3)*grav3
       
          ! terme de correction des inerties
          !
          !bdyty(ibdyty)%Fext(4)=-(bdyty(ibdyty)%I(3) - bdyty(ibdyty)%I(2))*bdyty(ibdyty)%Vbegin(5)*bdyty(ibdyty)%Vbegin(6)
          !
          !bdyty(ibdyty)%Fext(5)=-(bdyty(ibdyty)%I(1) - bdyty(ibdyty)%I(3))*bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(6)
          !
          !bdyty(ibdyty)%Fext(6)=-(bdyty(ibdyty)%I(2) - bdyty(ibdyty)%I(1))*bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(5)
       
          ! driven forces
          DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             
             CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)
          
             bdyty(ibdyty)%Fdriv(ifd) = (Febegin + Fe )*0.5     
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))
          
             bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+bdyty(ibdyty)%Fdriv(ifd) 
          
          END DO

       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE(INTEGRATOR_VERLET)

       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty,ifd,iccdof,Febegin,Fe)
       !$OMP DO SCHEDULE(RUNTIME)

       DO ibdyty=1,nb_RBDY3    
          ! initializing
          bdyty(ibdyty)%Fext(1:6)=0.D0
          IF(.NOT.bdyty(ibdyty)%visible) CYCLE
          !
          bdyty(ibdyty)%Fext(1) = bdyty(ibdyty)%mass(1)*grav1
          bdyty(ibdyty)%Fext(2) = bdyty(ibdyty)%mass(2)*grav2
          bdyty(ibdyty)%Fext(3) = bdyty(ibdyty)%mass(3)*grav3
       
          ! terme de correction des inerties
          !
          !bdyty(ibdyty)%Fext(4)=-(bdyty(ibdyty)%I(3) - bdyty(ibdyty)%I(2))*bdyty(ibdyty)%Vbegin(5)*bdyty(ibdyty)%Vbegin(6)
          !
          !bdyty(ibdyty)%Fext(5)=-(bdyty(ibdyty)%I(1) - bdyty(ibdyty)%I(3))*bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(6)
          !
          !bdyty(ibdyty)%Fext(6)=-(bdyty(ibdyty)%I(2) - bdyty(ibdyty)%I(1))*bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(5)
       
          ! driven forces
          DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof
             
             CALL comp_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd),Febegin,Fe)
          
             bdyty(ibdyty)%Fdriv(ifd) = (Febegin + Fe )*0.5     
             
             iccdof=dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))
          
             bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+bdyty(ibdyty)%Fdriv(ifd) 
          
          END DO

       END DO

       !$OMP END DO
       !$OMP END PARALLEL

    CASE default
       CALL faterr('RBDY3::comp_Fext','Integrator not supported')
    END SELECT

  END SUBROUTINE comp_Fext_RBDY3
!!!------------------------------------------------------------------------   
  SUBROUTINE comp_Fint_RBDY3 
    
    IMPLICIT NONE
    INTEGER :: ibdyty
    
    DO ibdyty=1,nb_RBDY3
       bdyty(ibdyty)%Fint=0.d0
    END DO
    
  END SUBROUTINE comp_Fint_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE read_PLAIN(bdyty,iblmty)

    IMPLICIT NONE
    INTEGER           :: iblmty
    CHARACTER(len=21) :: IAM='mod_RBDY3::read_PLAIN'
    TYPE(T_RBDY3)     :: bdyty
    
    READ(G_clin( 2: 6),'(A5)')    bdyty%blmty(iblmty)%blmID
    READ(G_clin(23:27),'(A5)')    bdyty%blmty(iblmty)%behav
    READ(G_clin(35:48),'(D14.7)') bdyty%blmty(iblmty)%PLAIN%avr_radius 
    
    IF( .NOT. read_G_clin()) THEN
       CALL FATERR(iam,'we expect a line in order to read inertia !')      
    END IF
    
    IF (G_clin(30:31) /= 'I1') THEN
       !CALL LOGMES('We skip reading inertia terms')
       bdyty%blmty(iblmty)%PLAIN%I1 = 0.D0
       bdyty%blmty(iblmty)%PLAIN%I2 = 0.D0
       bdyty%blmty(iblmty)%PLAIN%I3 = 0.D0
    ELSE
       READ(G_clin(35:48),'(D14.7)') bdyty%blmty(iblmty)%PLAIN%I1
       READ(G_clin(56:69),'(D14.7)') bdyty%blmty(iblmty)%PLAIN%I2
       READ(G_clin(77:90),'(D14.7)') bdyty%blmty(iblmty)%PLAIN%I3
    END IF
    
  END SUBROUTINE read_PLAIN
!!!------------------------------------------------------------------------
  SUBROUTINE write_PLAIN(nfich,bdyty,iblmty)

    IMPLICIT NONE

    INTEGER       :: iblmty,nfich
    TYPE(T_RBDY3) :: bdyty

    WRITE(nfich,102) bdyty%blmty(iblmty)%blmID,iblmty, &
                     'behav',bdyty%blmty(iblmty)%behav, &
                     'avrd=',bdyty%blmty(iblmty)%PLAIN%avr_radius 

    WRITE(nfich,132) 'I1  =',bdyty%blmty(iblmty)%PLAIN%I1, & 
                     'I2  =',bdyty%blmty(iblmty)%PLAIN%I2, & 
                     'I3  =',bdyty%blmty(iblmty)%PLAIN%I3
   
102 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,(2X,A5,D14.7))
132 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

  END SUBROUTINE write_PLAIN
!!!----------------------------------------------------------------------------------------
  SUBROUTINE add_reac(ibdyty,xxccdof,xxreac,storage)

    IMPLICIT NONE
    INTEGER                    :: ibdyty,iccdof,storage
    INTEGER,     DIMENSION(6)  :: xxccdof
    REAL(kind=8),DIMENSION(6)  :: xxreac

    IF(.NOT.bdyty(ibdyty)%visible) RETURN
    
    SELECT CASE(storage)
    CASE (iIreac)

       bdyty(ibdyty)%Ireac=bdyty(ibdyty)%Ireac+xxreac

    CASE (iIaux_)

       bdyty(ibdyty)%Iaux=bdyty(ibdyty)%Iaux+xxreac

    END SELECT
    
  END SUBROUTINE add_reac
!!!------------------------------------------------------------------------  
  SUBROUTINE nullify_reac(ibdyty,storage)
    
    IMPLICIT NONE 
    INTEGER :: ibdyty,iccdof
    INTEGER :: storage
    
    SELECT CASE(storage)
    CASE (iIreac)
       bdyty(ibdyty)%Ireac(1:6) = 0.d0
    CASE (iIaux_)
       bdyty(ibdyty)%Iaux(1:6) = 0.d0
    END SELECT
    
  END SUBROUTINE nullify_reac
!!!------------------------------------------------------------------------
  SUBROUTINE comp_vlocy(ibdyty,storage)
    
    IMPLICIT NONE 
    INTEGER :: ibdyty
    INTEGER :: storage
    
    IF(.NOT.bdyty(ibdyty)%visible) RETURN

    SELECT CASE(storage) 
    CASE (iV____e_invM_t_Ireac )

       bdyty(ibdyty)%V = bdyty(ibdyty)%Ireac*bdyty(ibdyty)%inv_mass

    CASE (iVaux_e_invM_t_Ireac )

       bdyty(ibdyty)%Vaux = bdyty(ibdyty)%Ireac*bdyty(ibdyty)%inv_mass

    CASE (iVaux_e_invM_t_Iaux_ )

       bdyty(ibdyty)%Vaux = bdyty(ibdyty)%Iaux*bdyty(ibdyty)%inv_mass

    CASE ( iVaux_e_invM_t_Iaux_p_Vfree )
       ! compute : Vaux = M^-1 Iaux
       bdyty(ibdyty)%Vaux = bdyty(ibdyty)%Iaux*bdyty(ibdyty)%inv_mass

       ! nullify velocity driven dof for Vaux
       IF ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
          call nullify_vlocy_driven_dof(ibdyty, iVaux_)

       ! compute : Vaux = Vaux + Vfree
       ! N.B. velocity driven dof are already applied to Vfree => Vaux = Vdriv
       bdyty(ibdyty)%Vaux = bdyty(ibdyty)%Vaux + bdyty(ibdyty)%Vfree

    END SELECT
  
  END SUBROUTINE comp_vlocy
!!!------------------------------------------------------------------------------------------------------- 

!!!------------------------------------------------------------------------------------------------------- 
  SUBROUTINE nullify_vlocy(ibdyty,storage)

    IMPLICIT NONE 
    INTEGER           :: ibdyty,iccdof
    INTEGER           :: storage
    CHARACTER(len=24) :: IAM='mod_RBDY3::nullify_vlocy'
    
    SELECT CASE(storage)
    CASE (iVaux_)
        bdyty(ibdyty)%Vaux=0.D0
    CASE default
       CALL FATERR(IAM,'error nullifying vlocy')
    END SELECT
    
  END SUBROUTINE nullify_vlocy
!!!------------------------------------------------------------------------------------------------------- 

!!!------------------------------------------------------------------------------------------------------- 
  FUNCTION get_avr_radius(ibdyty)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: ibdyty
    REAL(kind=8)       :: get_avr_radius

    get_avr_radius=bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius

  END FUNCTION get_avr_radius
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_avr_radius_tacty(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER,INTENT(in) :: ibdyty,itacty

    get_avr_radius_tacty=bdyty(ibdyty)%tacty(itacty)%BDARY%rdg

  END FUNCTION get_avr_radius_tacty
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  FUNCTION get_inertia_frameTT(ibdyty)

    IMPLICIT NONE
    INTEGER                     :: ibdyty
    REAL(kind=8),DIMENSION(3,3) :: get_inertia_frameTT
   
    get_inertia_frameTT = bdyty(ibdyty)%LocalFrameTT

  END FUNCTION get_inertia_frameTT
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  FUNCTION get_inertia_frameIni(ibdyty)

    IMPLICIT NONE
    INTEGER                      :: ibdyty
    REAL(kind=8),DIMENSION(3,3)  :: get_inertia_frameIni
   
    get_inertia_frameIni =bdyty(ibdyty)%LocalFrameIni
    
  END FUNCTION get_inertia_frameIni
!!!------------------------------------------------------------------------
  FUNCTION get_inertia_frame(ibdyty)

    IMPLICIT NONE
    INTEGER                      :: ibdyty
    REAL(kind=8),DIMENSION(3,3)  :: get_inertia_frame
   
    get_inertia_frame= bdyty(ibdyty)%LocalFrame

  END FUNCTION get_inertia_frame
!!! ------------------------------------------------------------------------
!!! mr
!!! FUNCTION get_embeded_frame
!!! INPUT
!!!  [ibdyty,itacty]
!!! PURPOSE
!!!  give the embeded frame of each tactor
!!!
  FUNCTION get_embeded_frame(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER                     :: ibdyty,itacty
    REAL(kind=8),DIMENSION(3,3) :: get_embeded_frame

    get_embeded_frame = bdyty(ibdyty)%tacty(itacty)%BDARY%EmbededFrame

  END FUNCTION get_embeded_frame
!!!---------------------------------------------------------------------------------------------
  SUBROUTINE put_embeded_frame(ibdyty,itacty,EmbededFrame)

    IMPLICIT NONE
    INTEGER                     :: ibdyty,itacty
    REAL(kind=8),DIMENSION(3,3) :: EmbededFrame

    bdyty(ibdyty)%tacty(itacty)%BDARY%EmbededFrame = EmbededFrame

  END SUBROUTINE put_embeded_frame
!!!---------------------------------------------------------------------------------------------
  !> returns the contact configuration coordinate,
  !> if itacty = 0 returns the center of inertia,
  !> if itacty /= 0 returns the barycenter of the contactor of local rank itacty 
  
  FUNCTION get_coorTT(ibdyty,itacty,updated)

    IMPLICIT NONE
    INTEGER                   :: ibdyty,itacty
    REAL(kind=8),DIMENSION(3) :: get_coorTT
    logical,optional          :: updated

    logical                   :: keep

    keep = .TRUE.
    if (present(updated)) keep=updated

    get_coorTT = bdyty(ibdyty)%coorTT

    if (keep) then
      if (XPERIODIC) get_coorTT(1) = modulo(get_coorTT(1),xperiode)
      if (YPERIODIC) get_coorTT(2) = modulo(get_coorTT(2),yperiode)    
    endif
   
    IF ( itacty .NE. 0 ) then
       get_coorTT = get_coorTT + get_shiftTT(ibdyty,itacty)
    endif
  END FUNCTION get_coorTT
!!!------------------------------------------------------------------------
  FUNCTION get_shiftTT(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER                   :: i,j,ibdyty,itacty
    REAL(kind=8),DIMENSION(3) :: get_shiftTT
    
    get_shiftTT = 0.d0
    DO j=1,3
      DO i=1,3
        get_shiftTT(j) = get_shiftTT(j) &
                       + (bdyty(ibdyty)%tacty(itacty)%BDARY%shift(i)*bdyty(ibdyty)%LocalFrameTT(j,i))
      END DO
    END DO

  END FUNCTION get_shiftTT
!!!------------------------------------------------------------------------
  FUNCTION get_vlocy(ibdyty,storage)

    IMPLICIT NONE
    INTEGER                   :: ibdyty,storage
    REAL(kind=8),DIMENSION(6) :: get_vlocy

    SELECT CASE(storage)
    CASE(iV____)
       get_vlocy = bdyty(ibdyty)%V
    CASE(iVbeg_)
       get_vlocy = bdyty(ibdyty)%Vbegin
    CASE(iVfree)
       get_vlocy = bdyty(ibdyty)%Vfree
    CASE(iVaux_)
       get_vlocy = bdyty(ibdyty)%Vaux
    CASE default
    END SELECT

  END FUNCTION get_vlocy
!!!------------------------------------------------------------------------
  FUNCTION get_Xbegin(ibdyty)

    IMPLICIT NONE
    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(3) :: get_Xbegin

    get_Xbegin = bdyty(ibdyty)%Xbegin(1:3) 
    
  END FUNCTION get_Xbegin
!!!------------------------------------------------------------------------
  FUNCTION get_X(ibdyty)

    IMPLICIT NONE
    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(3) :: get_X

    get_X = bdyty(ibdyty)%X(1:3)

  END FUNCTION get_X
!!!------------------------------------------------------------------------
  FUNCTION get_coor(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER                   :: i,j,ibdyty,itacty
    REAL(kind=8),DIMENSION(3) :: get_coor,shift

    get_coor = 0.D0

    get_coor(1:3) = bdyty(ibdyty)%cooref(1:3) + bdyty(ibdyty)%X(1:3)

    IF (itacty.NE.0) THEN
       DO j=1,3
          shift(j) = 0.D0
          DO i=1,3
             shift(j) = shift(j) &
                  + (bdyty(ibdyty)%tacty(itacty)%BDARY%shift(i)*bdyty(ibdyty)%LocalFrame(j,i))
          END DO
      END DO

      get_coor(1:3) = get_coor(1:3) + shift(1:3)
    
    END IF

    if (XPERIODIC) then
       get_coor(1) = modulo(get_coor(1),xperiode)
       if ( get_coor(1) < 0. .or. get_coor(1) > xperiode ) call faterr('get_coor','x perio wrong')
    endif
    
    if (YPERIODIC) then
      get_coor(2) = modulo(get_coor(2),yperiode)
      if ( get_coor(2) < 0. .or. get_coor(2) > yperiode ) call faterr('get_coor','y perio wrong')
    endif  
  END FUNCTION get_coor
!!!------------------------------------------------------------------------
  FUNCTION get_coorb(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER                   :: i,j,ibdyty,itacty
    REAL(kind=8),DIMENSION(3) :: get_coorb,shift

    get_coorb = 0.D0

    get_coorb(1:3) = bdyty(ibdyty)%cooref(1:3) + bdyty(ibdyty)%Xbegin(1:3)

    IF (itacty.NE.0) THEN
       DO j=1,3
          shift(j) = 0.D0
          DO i=1,3
             shift(j) = shift(j) &
                  + (bdyty(ibdyty)%tacty(itacty)%BDARY%shift(i)*bdyty(ibdyty)%LocalFrameIni(j,i))
          END DO
      END DO

      get_coorb(1:3) = get_coorb(1:3) + shift(1:3)
    
    END IF

    if (XPERIODIC) then
       get_coorb(1) = modulo(get_coorb(1),xperiode)
       if ( get_coorb(1) < 0. .or. get_coorb(1) > xperiode ) call faterr('get_coorb','x perio wrong')
    endif
    
    if (YPERIODIC) then
      get_coorb(2) = modulo(get_coorb(2),yperiode)
      if ( get_coorb(2) < 0. .or. get_coorb(2) > yperiode ) call faterr('get_coorb','y perio wrong')
    endif  
  END FUNCTION get_coorb
!!!------------------------------------------------------------------------
  FUNCTION get_cooref(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER    :: i,j,ibdyty,itacty
    REAL(kind=8),DIMENSION(3) :: get_cooref,shift

    get_cooref = 0.D0
    shift = 0.D0    

    IF (itacty.NE.0) THEN
       DO j=1,3
          shift(j) = 0.D0
          DO i=1,3
             shift(j) = shift(j) &
                  + (bdyty(ibdyty)%tacty(itacty)%BDARY%shift(i)*bdyty(ibdyty)%LocalFrameIni(j,i))
          END DO
       END DO
    END IF
       
    get_cooref(1:3) = bdyty(ibdyty)%cooref(1:3) + shift(1:3)

  END FUNCTION get_cooref
!!$!!!------------------------------------------------------------------------
!!$  FUNCTION get_behav(ibdyty,itacty)
!!$
!!$    IMPLICIT NONE
!!$    INTEGER          :: ibdyty,itacty
!!$    CHARACTER(len=5) :: get_behav
!!$
!!$    get_behav = bdyty(ibdyty)%blmty(1)%behav 
!!$
!!$  END FUNCTION get_behav
!!$!!!------------------------------------------------------------------------ 
  SUBROUTINE get_data(ibdyty,itacty,DATA)
    
    IMPLICIT NONE
    INTEGER                   :: ibdyty,itacty
    REAL(kind=8),DIMENSION(:) :: DATA
    
    DATA = bdyty(ibdyty)%tacty(itacty)%BDARY%data
    
  END SUBROUTINE get_data
!!!------------------------------------------------------------------------
  SUBROUTINE put_data(ibdyty,itacty,DATA)
    
    IMPLICIT NONE
    INTEGER                                                              :: ibdyty,itacty
    REAL(kind=8),DIMENSION(SIZE(bdyty(ibdyty)%tacty(itacty)%BDARY%data)) :: DATA
    
    bdyty(ibdyty)%tacty(itacty)%BDARY%data = DATA
    
  END SUBROUTINE put_data
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_data_sz(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER :: ibdyty,itacty
    
    get_data_sz = SIZE(bdyty(ibdyty)%tacty(itacty)%BDARY%data)
    
  END FUNCTION get_data_sz
!!!------------------------------------------------------------------------
  SUBROUTINE get_idata(ibdyty,itacty,idata)

    IMPLICIT NONE
    INTEGER              :: ibdyty,itacty
    INTEGER,DIMENSION(:) :: idata

    idata = bdyty(ibdyty)%tacty(itacty)%BDARY%idata

  END SUBROUTINE get_idata
!!!------------------------------------------------------------------------
  INTEGER FUNCTION  get_idata_sz(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER :: ibdyty,itacty
   
    get_idata_sz = SIZE(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
    
  END FUNCTION get_idata_sz
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_bdyID(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty
   
    get_bdyID = bdyty(ibdyty)%bdyID

  END FUNCTION get_bdyID
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_tacty(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty
    
    get_nb_tacty = SIZE(bdyty(ibdyty)%tacty)
    
  END FUNCTION get_nb_tacty
!!!------------------------------------------------------------------------
  integer FUNCTION get_tacID(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER :: ibdyty,itacty
   
    get_tacID = bdyty(ibdyty)%tacty(itacty)%tacID

  END FUNCTION get_tacID
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_nb_RBDY3(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL  :: fantome
   
    get_nb_RBDY3 = nb_RBDY3

  END FUNCTION get_nb_RBDY3
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_behav(ibdyty)

    IMPLICIT NONE
    INTEGER  :: ibdyty

    get_behav = bdyty(ibdyty)%blmty(1)%behav 

  END FUNCTION get_behav
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_color(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER  :: ibdyty,itacty

    get_color = bdyty(ibdyty)%tacty(itacty)%color 

  END FUNCTION get_color
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------  
  subroutine set_color_RBDY3(ibdyty,itacty,color)

    implicit none

    integer  :: ibdyty,itacty
    character(len=5) :: color

    ! if tacty is specified... set only this one
    if( itacty /= 0 ) then
      bdyty(ibdyty)%tacty(itacty)%color = color
      return
    end if

    ! otherwise... everything !
    do itacty = 1, size(bdyty(ibdyty)%tacty)
      bdyty(ibdyty)%tacty(itacty)%color = color
    end do

  end subroutine set_color_RBDY3
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE put_color(ibdyty,itacty,color)

    IMPLICIT NONE
    INTEGER  :: ibdyty,itacty
    CHARACTER(len=5) :: color

    bdyty(ibdyty)%tacty(itacty)%color = color 

  END SUBROUTINE put_color
!!!------------------------------------------------------------------------
  FUNCTION get_V(ibdyty)
    
    IMPLICIT NONE
    INTEGER                  :: ibdyty
    REAL(kind=8),DIMENSION(6):: get_V
    
    get_V = bdyty(ibdyty)%V

  END FUNCTION get_V
!!!------------------------------------------------------------------------
  FUNCTION get_Vbegin(ibdyty)

    IMPLICIT NONE
    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(6) :: get_Vbegin
    
    get_Vbegin = bdyty(ibdyty)%Vbegin
    
  END FUNCTION get_Vbegin
!!!------------------------------------------------------------------------
  FUNCTION get_Finertia(ibdyty)

    IMPLICIT NONE

    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(6) :: get_Finertia

    !write(*,'(6(1x,D12.5))') bdyty(ibdyty)%mass(1:6)
    !write(*,'(6(1x,D12.5))') bdyty(ibdyty)%V(1:6)
    !write(*,'(6(1x,D12.5))') bdyty(ibdyty)%Vbegin(1:6)
    !print*,H 

    get_Finertia(1:6)=bdyty(ibdyty)%mass(1:6)*(bdyty(ibdyty)%V(1:6)-bdyty(ibdyty)%Vbegin(1:6))/H 

  END FUNCTION get_Finertia
!!!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
  FUNCTION get_reac(ibdyty)

    IMPLICIT NONE

    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(6) :: get_reac
    IF (smooth_method) THEN
      get_reac = bdyty(ibdyty)%Ireac
    ELSE
      get_reac(1:6)=bdyty(ibdyty)%Ireac(1:6)/H  
    ENDIF
  END FUNCTION get_reac
!!!------------------------------------------------------------------------
  FUNCTION get_Fext(ibdyty)

    IMPLICIT NONE

    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(6) :: get_Fext
    
    get_Fext = bdyty(ibdyty)%Fext
 
  END FUNCTION get_Fext
!!!------------------------------------------------------------------------
  FUNCTION get_inertia_tensor(ibdyty)

    IMPLICIT NONE
    INTEGER                   :: ibdyty,iblmty
    REAL(kind=8),DIMENSION(3) :: get_inertia_tensor

    iblmty = 1
    get_inertia_tensor(1) = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1
    get_inertia_tensor(2) = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2
    get_inertia_tensor(3) = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3

  END FUNCTION get_inertia_tensor
!!!------------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_volume(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty,iblmty

    iblmty = 1
    get_volume = bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume
    
  END FUNCTION get_volume
!!!------------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_mass(ibdyty)
    
    IMPLICIT NONE
    INTEGER      :: ibdyty

    get_mass = bdyty(ibdyty)%mass(1)
 
  END FUNCTION get_mass
!!!------------------------------------------------------------------------
  LOGICAL FUNCTION get_visible(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty

    get_visible = bdyty(ibdyty)%visible
    
  END FUNCTION get_visible
!!!------------------------------------------------------------------------
  SUBROUTINE  set_visible(ibdyty,FLAG)

    IMPLICIT NONE
    INTEGER :: ibdyty
    LOGICAL :: FLAG

    bdyty(ibdyty)%visible = FLAG
    
  END SUBROUTINE set_visible
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_bulk_behav_number_RBDY3(ibdyty,iblmty)

    IMPLICIT NONE
    INTEGER,INTENT(in) :: ibdyty,iblmty

    get_bulk_behav_number_RBDY3 = bdyty(ibdyty)%blmty(iblmty)%lawnb

  END FUNCTION get_bulk_behav_number_RBDY3
!!!!---------------------------------------------------------------------------------------
  SUBROUTINE comp_mass_RBDY3 

    IMPLICIT NONE
    INTEGER                     :: ibdyty,iblmty,ibehav,iccdof,ivd
    REAL(kind=8)                :: Ummass,mass1,mass2,mass3,vol,I1,I2,I3
    character(len=80) :: cout
    
    DO ibdyty=1,nb_RBDY3
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb
          SELECT CASE(bdyty(ibdyty)%blmty(iblmty)%blmID)
          CASE('PLAIN')
             
             Ummass = get_rho(ibehav)
             
             I1  = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1
             I2  = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2
             I3  = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3
             vol = bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume

          CASE default
             CALL LOGMES('you try to compute the mass of an unknown blmty')
          END SELECT
       END DO
       
       mass1  = Ummass*vol
       mass2  = mass1
       mass3  = mass1
       iblmty = 1            
       
       I1 = I1*Ummass
       I2 = I2*Ummass
       I3 = I3*Ummass
       
       DO iccdof=1,SIZE(bdyty(ibdyty)%V)
          
          IF (iccdof == 1) bdyty(ibdyty)%mass(iccdof)=mass1
          IF (iccdof == 2) bdyty(ibdyty)%mass(iccdof)=mass2
          IF (iccdof == 3) bdyty(ibdyty)%mass(iccdof)=mass3
          IF (iccdof == 4) bdyty(ibdyty)%mass(iccdof)=I1
          IF (iccdof == 5) bdyty(ibdyty)%mass(iccdof)=I2
          IF (iccdof == 6) bdyty(ibdyty)%mass(iccdof)=I3
          
          IF (bdyty(ibdyty)%mass(iccdof)>0.D0) THEN
             bdyty(ibdyty)%inv_mass(iccdof)=1.D0/bdyty(ibdyty)%mass(iccdof)
          ELSE
             write(cout,'(A,1x,I0)') 'RBDY3:',ibdyty
             write(cout,'(A,1x,D14.7,A,1x,I0)') 'MASS:',bdyty(ibdyty)%mass(iccdof),' iccdof:',iccdof
             call faterr('RBDY3::comp_mass',cout)
          END IF
       END DO
       
!!$       print*,'corps: ',ibdyty
!!$       print*,'geo: '
!!$       print*,bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume,bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1, &
!!$              bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2,bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3 
!!$       print*,'masse: '
!!$       print*,bdyty(ibdyty)%mass(1:6)
!!$       print*,'inv masse: '
!!$       print*,bdyty(ibdyty)%inv_mass(1:6)
!!$       print*,'xxxxxxxxxxxxxxxxx'

       IF (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) CYCLE 
       
       ! nullifying inv_mass where degrees of freedom are driven
       DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
          iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
          bdyty(ibdyty)%inv_mass(iccdof) = 0.D0
       END DO

!!$      print*,'inv masse avec ddl'
!!$      print*,bdyty(ibdyty)%inv_mass(1:6)
!!$      print*,'xxxxxxxxxxxxxxxxx'

    END DO

  END SUBROUTINE comp_mass_RBDY3
!!!------------------------------------------------------------------------ 
  SUBROUTINE out_of_bounds_RBDY3

    IMPLICIT NONE
    INTEGER :: ibdy,i

    !am: modified criterion taking into account the case of a body lying ON the boundary
    real(kind=8) :: dist
    real(kind=8) :: tol = epsilon(1.d0)
    
    DO ibdy=1,nb_RBDY3
       IF(.NOT.bdyty(ibdy)%visible) CYCLE
       DO i = 1,3
          !IF((bdyty(ibdy)%X(i)+bdyty(ibdy)%cooref(i)).LT.MinBound(i)) THEN
          dist = (bdyty(ibdy)%X(i)+bdyty(ibdy)%cooref(i)) - MinBound(i)
          IF(dist < 0.d0 .or. abs(dist) < tol) THEN
             bdyty(ibdy)%visible=.FALSE.
             bdyty(ibdy)%Vbegin = 0.D0
             bdyty(ibdy)%V      = 0.D0
             bdyty(ibdy)%X(i)   = MinBound(i) - bdyty(ibdy)%cooref(i)
             EXIT
          END IF
          !IF((bdyty(ibdy)%X(i)+bdyty(ibdy)%cooref(i)).GT.MaxBound(i)) THEN
          dist = (bdyty(ibdy)%X(i)+bdyty(ibdy)%cooref(i)) - MaxBound(i)
          IF(dist > 0.d0 .or. abs(dist) < tol) THEN
             bdyty(ibdy)%visible=.FALSE.
             bdyty(ibdy)%Vbegin = 0.D0
             bdyty(ibdy)%V      = 0.D0
             bdyty(ibdy)%X(i)   = MaxBound(i) - bdyty(ibdy)%cooref(i)
             EXIT
          END IF
       END DO
    END DO
    
  END SUBROUTINE out_of_bounds_RBDY3
!!!------------------------------------------------------------------------   
  !am DDM: this function return "true" iff bounds are defined
  function check_bounds_RBDY3()

     implicit none

     logical :: check_bounds_RBDY3

     check_bounds_RBDY3 = BOUNDS

  end function check_bounds_RBDY3
!!!------------------------------------------------------------------------   
  SUBROUTINE set_init_boundary_RBDY3(ibound,linf)

    IMPLICIT NONE

    INTEGER      :: ibound
    REAL(kind=8) :: linf

    SELECT CASE(ibound)
    CASE(1)
       MinBound(1) = linf
    CASE(2)
       MaxBound(1) = linf
    CASE(3)
       MinBound(2) = linf
    CASE(4)
       MaxBound(2) = linf
    CASE(5)
       MinBound(3) = linf
    CASE(6)
       MaxBound(3) = linf
    END SELECT
    
    BOUNDS=.TRUE.

  END SUBROUTINE set_init_boundary_RBDY3
!!!------------------------------------------------------------------------
  INTEGER FUNCTION get_entity_RBDY3(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty

    get_entity_RBDY3 = nb_existing_entities + ibdyty
    
  END FUNCTION get_entity_RBDY3
!!!------------------------------------------------------------------------ 
  SUBROUTINE update_existing_entities_RBDY3

    IMPLICIT NONE

    nb_existing_entities = get_nb_ENTITY()
    CALL add_nb_ENTITY(nb_RBDY3)

  END SUBROUTINE update_existing_entities_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE set_new_rotation_scheme_RBDY3

    IMPLICIT NONE

    new_rotation_scheme = .TRUE.

  END SUBROUTINE set_new_rotation_scheme_RBDY3
!!!------------------------------------------------------------------------
  ! fonction qui permet de savoir si on impose une condition a la limite periodique
  ! precondition:
  !   - rien
  ! postcondition:
  !   - renvoie 'vrai' ssi on impose une condition a la limite periodique
  function is_Xperiodic_RBDY3()
    implicit none
    logical :: is_Xperiodic_RBDY3

    is_Xperiodic_RBDY3 = XPERIODIC

  end function is_Xperiodic_RBDY3

  function is_Yperiodic_RBDY3()
    implicit none
    logical :: is_Yperiodic_RBDY3

    is_Yperiodic_RBDY3 = YPERIODIC

  end function is_Yperiodic_RBDY3
!!!------------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_xperiode_RBDY3()
    IMPLICIT NONE
    get_xperiode_RBDY3 = xperiode
    
  END FUNCTION get_xperiode_RBDY3
!!!------------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_yperiode_RBDY3()
    IMPLICIT NONE    
    get_yperiode_RBDY3 = yperiode
    
  END FUNCTION get_yperiode_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE set_xperiodic_data_RBDY3(periode)

    IMPLICIT NONE
    REAL(kind=8) :: periode


    xperiode  = periode
    XPERIODIC = .TRUE.

  END SUBROUTINE set_xperiodic_data_RBDY3
!!!------------------------------------------------------------------------
  SUBROUTINE set_yperiodic_data_RBDY3(periode)

    IMPLICIT NONE
    REAL(kind=8) :: periode

    yperiode  = periode
    YPERIODIC = .TRUE.

  END SUBROUTINE set_yperiodic_data_RBDY3

!!!------------------------------------------------------------------------ 
!!!mj This command allows to change BODIES.DAT ref coordinates into 
!!!mj BODIES.DAT refcoordinates + DOF.INI displacements
!!!mj This command is used to change reference configuration
!!!------------------------------------------------------------------------ 

  subroutine add_dof2bodies_RBDY3

    implicit none

    integer :: ibdyty,nbdof
   
    ! vv: ne fonctionne que pour les spheres. Pour les autres contacteurs, il faudrait updater les
    ! parametres inertiels I1, I2 et I3 presents dans le BODIES. et ajouter l'orientation du
    ! repere principal d'inertie (present uniquement dans le DOF. :).
    do ibdyty=1,nb_RBDY3       
       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty)
       bdyty(ibdyty)%cooref(1:nbdof)=bdyty(ibdyty)%cooref(1:nbdof)+bdyty(ibdyty)%Xbegin(1:nbdof)
    end do

  end subroutine add_dof2bodies_RBDY3
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Rnod_RBDY3()

    IMPLICIT NONE

    get_write_Rnod_RBDY3 = write_Rnod

  END FUNCTION get_write_Rnod_RBDY3
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_DOF_RBDY3()

    IMPLICIT NONE

    get_write_DOF_RBDY3 = write_DOF

  END FUNCTION get_write_DOF_RBDY3
!!!------------------------------------------------------------------------ 
!!! Multi-physics applications
!!!------------------------------------------------------------------------ 
  REAL(KIND=8) FUNCTION get_elec_cond(ibdyty,itacty)
    
    IMPLICIT NONE

    INTEGER,INTENT(in)       :: ibdyty,itacty

    get_elec_cond = bdyty(ibdyty)%tacty(itacty)%BDARY%ECond

  END FUNCTION get_elec_cond
!!!------------------------------------------------------------------------ 
  REAL(KIND=8) FUNCTION get_elec_condini(ibdyty,itacty)
    
    IMPLICIT NONE

    INTEGER,INTENT(in)       :: ibdyty,itacty

    get_elec_condini = bdyty(ibdyty)%tacty(itacty)%BDARY%ECondini

  END FUNCTION get_elec_condini
!!!------------------------------------------------------------------------ 
  SUBROUTINE put_elec_cond(ibdyty,itacty,cond)

    IMPLICIT NONE

    INTEGER,INTENT(in)       :: ibdyty,itacty
    REAL(kind=8)             :: cond

    bdyty(ibdyty)%tacty(itacty)%BDARY%ECond = cond
    
  END SUBROUTINE  put_elec_cond
!!!------------------------------------------------------------------------ 
  REAL(KIND=8) FUNCTION get_electric_potentiel(ibdyty,itacty)
    
    IMPLICIT NONE
    INTEGER,INTENT(in) :: ibdyty,itacty

    get_electric_potentiel = bdyty(ibdyty)%tacty(itacty)%BDARY%EPot

  END FUNCTION get_electric_potentiel
!!!------------------------------------------------------------------------ 
  SUBROUTINE  put_electric_potentiel(ibdyty,itacty,EPot)

    IMPLICIT NONE
    INTEGER,INTENT(in)       :: ibdyty,itacty
    REAL(kind=8),INTENT(in)  :: EPot
    
    bdyty(ibdyty)%tacty(itacty)%BDARY%EPot = EPot

  END SUBROUTINE put_electric_potentiel
!!!------------------------------------------------------------------------ 
!!!
!!!------------------------------------------------------------------------ 
  REAL(KIND=8) FUNCTION get_therm_cond(ibdyty,itacty)
    
    IMPLICIT NONE
    INTEGER,INTENT(in) :: ibdyty,itacty

    get_therm_cond = bdyty(ibdyty)%tacty(itacty)%BDARY%TCond
    
  END FUNCTION get_therm_cond
!!!------------------------------------------------------------------------ 
  SUBROUTINE put_therm_cond(ibdyty,itacty,cond)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: ibdyty,itacty
    REAL(kind=8)       :: cond

    bdyty(ibdyty)%tacty(itacty)%BDARY%TCond = cond
    
  END SUBROUTINE  put_therm_cond
!!!------------------------------------------------------------------------ 
  REAL(KIND=8) FUNCTION  get_thermal_value(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER :: ibdyty,itacty

    get_thermal_value = bdyty(ibdyty)%tacty(itacty)%BDARY%T

  END FUNCTION get_thermal_value
!!!------------------------------------------------------------------------ 
  SUBROUTINE  put_thermal_value(ibdyty,itacty,T)

    IMPLICIT NONE
    INTEGER      :: ibdyty,itacty
    REAL(kind=8) :: T

    bdyty(ibdyty)%tacty(itacty)%BDARY%T = T

  END SUBROUTINE put_thermal_value
!!!------------------------------------------------------------------------
  REAL(KIND=8) FUNCTION get_therm_condini(ibdyty,itacty)
    
    IMPLICIT NONE
    INTEGER,INTENT(in) :: ibdyty,itacty

    get_therm_condini = bdyty(ibdyty)%tacty(itacty)%BDARY%TCondini
    
  END FUNCTION get_therm_condini
!!!------------------------------------------------------------------------ 
  REAL(KIND=8) FUNCTION get_WS(ibdyty,itacty)

    IMPLICIT NONE
    INTEGER,INTENT(in) :: ibdyty,itacty
    
    get_WS = bdyty(ibdyty)%tacty(itacty)%BDARY%WS
    
  END FUNCTION get_WS
!!!------------------------------------------------------------------------ 
  SUBROUTINE put_WS(ibdyty,itacty,WS)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: ibdyty,itacty
    REAL(kind=8)       :: WS

    bdyty(ibdyty)%tacty(itacty)%BDARY%WS = WS

  END SUBROUTINE put_WS
!!!------------------------------------------------------------------------
  SUBROUTINE update_WS_RBDY3

    IMPLICIT NONE
    INTEGER      :: ibdyty,itacty,iblmty,ibehav
    REAL(kind=8) :: WS,T,WSini

    iblmty = 1

    DO ibdyty=1,nb_RBDY3
       ibehav = bdyty(ibdyty)%blmty(iblmty)%lawnb

       DO itacty=1,SIZE(bdyty(ibdyty)%tacty) 

          WSini = bdyty(ibdyty)%tacty(itacty)%BDARY%WSini
          T     = bdyty(ibdyty)%tacty(itacty)%BDARY%T

          CALL compute_WSvsT(ibehav,WS,T)

          bdyty(ibdyty)%tacty(itacty)%BDARY%WS = WS
          
       END DO
          
    END DO

  END SUBROUTINE update_WS_RBDY3
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_thermal_coefficient(ibdyty)

    IMPLICIT NONE
    INTEGER,INTENT(in)       :: ibdyty

    get_thermal_coefficient = bdyty(ibdyty)%Talpha

  END FUNCTION get_thermal_coefficient
!!!------------------------------------------------------------------------ 
!!! END MULTI-PHYSICS APPLICATIONS
! !!!------------------------------------------------------------------------ 
!   INTEGER FUNCTION get_xperiode(ibdyty)
    
!     IMPLICIT NONE
!     INTEGER,INTENT(in)    :: ibdyty
    
!     get_xperiode = bdyty(ibdyty)%xperiode
    
!   END FUNCTION get_xperiode
! !!!------------------------------------------------------------------------
!   INTEGER FUNCTION get_yperiode(ibdyty)
    
!     IMPLICIT NONE
!     INTEGER,INTENT(in)    :: ibdyty
    
!     get_yperiode = bdyty(ibdyty)%yperiode
    
!   END FUNCTION get_yperiode
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------
  SUBROUTINE put_coor(ibdyty,coor)

    IMPLICIT NONE
    INTEGER    :: ibdyty
    REAL(kind=8),DIMENSION(3) :: coor
    
    bdyty(ibdyty)%cooref(1:3) = coor(1:3)
    bdyty(ibdyty)%X = 0.D0
    bdyty(ibdyty)%Xbegin = 0.D0

  END SUBROUTINE put_coor
!!!------------------------------------------------------------------------
  SUBROUTINE put_V(ibdyty,V)

    IMPLICIT NONE
    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(6) :: V
    
    bdyty(ibdyty)%Vbegin(1:6) = V(1:6)
    bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin

  END SUBROUTINE put_V
!!!------------------------------------------------------------------------
  SUBROUTINE add_ext_Fext(ibdyty,eFext)

    IMPLICIT NONE

    INTEGER                   :: ibdyty
    REAL(kind=8),DIMENSION(6) :: eFext
    
    bdyty(ibdyty)%Fext = bdyty(ibdyty)%Fext + eFext
    !PRINT*,'eFext:',eFext(1),eFext(2),eFext(3)

  END SUBROUTINE add_ext_Fext
!!!------------------------------------------------------------------------
  SUBROUTINE set_skip_invisible_RBDY3

    IMPLICIT NONE

    skip_invisible = .TRUE.

  END SUBROUTINE set_skip_invisible_RBDY3
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  SUBROUTINE set_keep_ini_dof_order_RBDY3

    IMPLICIT NONE

    keep_ini_dof_order = .TRUE.

  END SUBROUTINE set_keep_ini_dof_order_RBDY3
!!!------------------------------------------------------------------------ 
  SUBROUTINE membrane_RBDY3(sigma_min,sigma_max,DT_ini,DT_load,DT_relax,centre,ep,& 
                            nb_grain,up_grain,is_first_time,is_up_actif,nfich)

   ! routine qui applique le chargement equivalent a une membrane
   !
   ! sigma_min,sigma_max,DT_ini,DT_load <- application du chargement
   !                        on commence par appliquer sigma_min jusqu'a TPSbegin+DT_ini 
   !                        On monte a sigma_max de TPSbegin+DT_ini a TPSbegin+DT_ini+DT_load    
   !                        on maintient sigma_max
   ! DT_relax pas utilise

   ! centre: coordonnee x,y de l'axe dans le plan z=0
   ! ep: epaisseur de la membrane
   ! nb_grain : les solides dans la membrane sont de 1:nb_grain
   ! up_grain,is_up_actif : si on veut piloter le plan superieur (num up_grain) 
   !                        pour mettre en charge hydrostatique is_up_actif=true
   ! is_first_time : premier appel de la routine
   ! nfich : le num du fichier ou on doit ecrire

   IMPLICIT NONE

   INTEGER                                        :: nb_grain,nfich,up_grain
   REAL(kind=8)                                   :: sigma_min,sigma_max,T1,T2,T3,ep,sigma
   REAL(kind=8)                                   :: DT_ini,DT_load,DT_relax
   LOGICAL                                        :: is_first_time,is_up_actif

   REAL(kind=8),DIMENSION(2)                      :: centre

   INTEGER                                        :: i,j,cpt_nbgrain 
 
   REAL(kind=8)                                   :: rayon,rmax,hmax,hmin,XF,YF,rd,cs,cs_approx,Vsonde,Rsonde,&
                                                     Vbdy_sonde,Eps,Hcyl, &
                                                     phi,dilatance,fs

   REAL(kind=8),PARAMETER                         :: untiers=1.D0/3.D0

   REAL(kind=8),SAVE                              :: Hcyl_0,Rcyl_0,phi_0,Rd_max,Vbdy,phi_tranche_0

   REAL(kind=8),DIMENSION(10)                     :: Rtranche,fs_tranche
   INTEGER     ,DIMENSION(10)                     :: cpt_tranche
   REAL(kind=8)                                   :: Htranche,Vcyl_tranche,phi_tranche,dilatance_tranche,xmin,xmax,Vc
   
   CHARACTER(len=5)                               :: name

   TYPE T_grain
    LOGICAL :: is_ok
    REAL(kind=8):: r,rd
    INTEGER ::Itranche 
   END TYPE T_grain

   TYPE(T_grain),ALLOCATABLE,DIMENSION(:),SAVE :: grain

   character(len=80) :: cout
   character(len=15) :: IAM
         !123456789012345
   IAM = 'RBDY3::membrane'

   IF (is_first_time) THEN 

     T1=TPSbegin+DT_ini
     T2=T1+DT_load
     T3=T2+DT_relax

     ALLOCATE(grain(nb_grain))

     Rd_max=0.d0
     Vbdy=0.d0
     DO i=1,nb_grain

       grain(i)%rd=(0.75d0*bdyty(i)%tacty(1)%BDARY%volume/PI_g)**untiers
       Rd_max=MAX(Rd_max,grain(i)%rd)
!       print*,i,grain(i)%rd

       Vbdy=Vbdy+bdyty(i)%tacty(1)%BDARY%volume

     ENDDO

     PRINT*,'rd_max',rd_max
     PRINT*,'V bodies',Vbdy

   ENDIF

   IF (TPS < T1) THEN
     sigma = sigma_min
   ELSE IF (T1 <= TPS .AND. TPS < T2) THEN
     sigma = sigma_min + (((sigma_max - sigma_min)/(T2-T1))*(TPS-T1))
   ELSE
     sigma = sigma_max
   ENDIF

 !------------------------------------------------------------------------
 ! on suppose que le cylindre est aligné sur l'axe des z
 ! il faut trouver son rayon rmax
 !----------------------------------------------------------------------

   rmax= 0.D0
   hmin= 1.d20
   hmax=-1.d20

! calcul distance a l'axe, hauteur cyl

   DO i=1,nb_grain
     grain(i)%r=SQRT((bdyty(i)%cooref(1)+bdyty(i)%Xbegin(1) -centre(1))**2+ &
                     (bdyty(i)%cooref(2)+bdyty(i)%Xbegin(2)-centre(2))**2)
     grain(i)%is_ok=.FALSE.
     rmax=MAX(rmax,grain(i)%r+grain(i)%rd)
     hmin=MIN(hmin,bdyty(i)%cooref(3)+bdyty(i)%Xbegin(3)-grain(i)%rd)
     hmax=MAX(hmax,bdyty(i)%cooref(3)+bdyty(i)%Xbegin(3)+grain(i)%rd)
   END DO

   Hcyl =  hmax-hmin

   IF (is_first_time) THEN
     Rcyl_0 = rmax
     Hcyl_0= Hcyl
   END IF

! calcul grains dans les tranches

   Htranche =  Hcyl/REAL(10,8)
   Rtranche = 0.d0
   DO i=1,nb_grain
     j=FLOOR((bdyty(i)%cooref(3)+bdyty(i)%Xbegin(3)-hmin)/Htranche)+1
!     print*,(bdyty(i)%cooref(3)+bdyty(i)%Xbegin(3))/Htranche,j
     grain(i)%Itranche=j
     IF (j<1 .OR. j>10) THEN
       write(cout,'(A,1x,I0)') 'on est en dehors des tranches:',j
       call faterr(IAM,cout)
     ENDIF
     Rtranche(j)=MAX(Rtranche(j),(grain(i)%r+grain(i)%rd))
   END DO

   cs=2.d0*PI_g*rmax*Hcyl

   PRINT*,'Cylindre constant:'
   PRINT*,'Rmax= ',rmax,' Hcyl= ',Hcyl
   PRINT*,'Surf cyl =',cs


   !------------------------------------------------------------------------
   ! --- Puis on cherche combien de grains seront dans la bande ----
   ! --- Puis on calcule le rayon apparent de chaque grain ----
   !------------------------------------------------------------------------

   cpt_nbgrain=0
   cpt_tranche=0
   DO i=1,nb_grain
     rayon=grain(i)%r+grain(i)%rd
     j=grain(i)%Itranche
     IF (rayon>Rtranche(j)-ep) THEN
       cpt_nbgrain=cpt_nbgrain+1
       grain(i)%is_ok=.TRUE. 
       cpt_tranche(j)=cpt_tranche(j)+1  
     END IF
   ENDDO

   fs = sigma*cs/REAL(cpt_nbgrain,8)

   Vcyl_tranche=0.d0
   cs_approx=0.d0

   DO j=1,10
     IF (cpt_tranche(j) == 0) THEN
       call faterr(IAM,'tranche vide')
     ENDIF
     fs_tranche(j) = sigma*2.d0*PI_g*Rtranche(j)*Htranche/REAL(cpt_tranche(j),8)
     Vcyl_tranche = Vcyl_tranche + (Htranche * PI_g* (Rtranche(j)**2))
     cs_approx=cs_approx+(2.d0*PI_g*Rtranche(j)*Htranche)
   ENDDO

   Vbdy_sonde=0.d0
   Rsonde = Rcyl_0-(3.d0*Rd_max)
   DO i=1,nb_grain
     bdyty(i)%blmty(1)%behav='zobxx'   

     rayon=grain(i)%r
     IF (grain(i)%is_ok) THEN
       j=grain(i)%Itranche
       name='PLEXs'
       WRITE(name(5:5),'(I1)') j-1

       bdyty(i)%blmty(1)%behav=name

       XF= (centre(1)-bdyty(i)%cooref(1)-bdyty(i)%Xbegin(1))/rayon
       YF= (centre(2)-bdyty(i)%cooref(2)-bdyty(i)%Xbegin(2))/rayon
       bdyty(i)%Fext(1)=bdyty(i)%Fext(1)+(fs_tranche(j)*XF)
       bdyty(i)%Fext(2)=bdyty(i)%Fext(2)+(fs_tranche(j)*YF)
     END IF

     IF (rayon - grain(i)%rd < Rsonde) THEN
       bdyty(i)%blmty(1)%behav='PLEXi'  
       IF ( rayon + grain(i)%rd <= Rsonde ) THEN
         Vbdy_sonde=Vbdy_sonde+bdyty(i)%tacty(1)%BDARY%volume
       ELSE 
!on aura ca
!if ( Rsonde < rayon + grain(i)%rd            .and. &
!                          rayon - grain(i)%rd < Rsonde  ) then

!fd calcul du volume coupe

           xmin = -grain(i)%rd
           xmax = Rsonde - rayon
           Vc = PI_g*((grain(i)%rd**2)*(xmax-xmin) + ((xmin**3)*untiers) - ((xmax**3)*untiers))
          
           Vbdy_sonde=Vbdy_sonde+Vc

       ENDIF
     ENDIF

   END DO

   Vsonde = Hcyl * PI_g* ((Rcyl_0-(3.d0*Rd_max))**2)
   Eps = (Hcyl_0 - Hcyl)/Hcyl_0

   phi = Vbdy_sonde/Vsonde 
   phi_tranche = Vbdy/Vcyl_tranche 

   PRINT*,'surface des tranches ',cs_approx
   PRINT*,'sonde:',Vsonde,Vbdy_sonde
   PRINT*,'tranches:',Vcyl_tranche,Vbdy

   IF (is_first_time) THEN 
     phi_0=phi
     phi_tranche_0=phi_tranche
   ENDIF
 
   dilatance = (phi_0 - phi)/phi
   dilatance_tranche = (phi_tranche_0 - phi_tranche)/phi_tranche

!   print*,'Nombre de grains dans la membrane',cpt_nbgrain


   IF (is_up_actif) THEN
     bdyty(up_grain)%Fext(3)=bdyty(up_grain)%Fext(3)-(sigma*(PI_g*(rmax**2)))
!     print*,'effort applique ',bdyty(up_grain)%Fext(3)
   ENDIF

   WRITE(nfich,'(8(1x,D14.7))') TPS,rmax,Hcyl,eps,phi,dilatance,phi_tranche,dilatance_tranche

 END SUBROUTINE membrane_RBDY3
!------------------------------------------------------------------------
  LOGICAL FUNCTION IS_IN_THE_FREE_BOUNDARY(ibdyty,itacty)
    
    IMPLICIT NONE
    INTEGER(kind=4) :: ibdyty,itacty

    IS_IN_THE_FREE_BOUNDARY = bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY

  END FUNCTION IS_IN_THE_FREE_BOUNDARY
!-------------------------------------------------------------------------------
  SUBROUTINE init_free_boundary_RBDY3(XMIN,XMAX,YMIN,YMAX,DX)

    IMPLICIT NONE
    INTEGER(kind=4)           :: ibdyty,itacty,ix,iy,ifbx,ifby
    INTEGER(kind=4)           :: nb_tacty
    REAL(kind=8)              :: XMIN,XMAX,YMIN,YMAX,DX,DENX,DENY
    REAL(kind=8),DIMENSION(3) :: coor

    FREE_BOUNDARY = .TRUE.

    FB_NX = 1+INT((XMAX-XMIN)/DX)
    FB_NY = 1+INT((YMAX-YMIN)/DX)

    FB_XMIN = XMIN
    FB_XMAX = XMAX
    FB_YMIN = YMIN
    FB_YMAX = YMAX

    FB_DX   = DX

    DENX = 1.0/(XMAX-XMIN)
    DENY = 1.0/(YMAX-YMIN)

    IF(ALLOCATED(FREE_SURFACE)) DEALLOCATE(FREE_SURFACE)
    ALLOCATE(FREE_SURFACE(FB_NX,FB_NY))

    DO ix = 1,FB_NX
       DO iy = 1,FB_NY
          FREE_SURFACE(ix,iy)%ID_RBDY3 = 0
          FREE_SURFACE(ix,iy)%ID_TACTY = 0
          FREE_SURFACE(ix,iy)%ZMAX     =-1D+24
          FREE_SURFACE(ix,iy)%ACTIVE   = .TRUE.
       END DO
    END DO

    DO ibdyty=1,nb_RBDY3

       nb_tacty = SIZE(bdyty(ibdyty)%tacty)

       DO itacty=1,nb_tacty
          coor = get_coor(ibdyty,itacty)

          ifbx = INT(FB_NX*(coor(1)-XMIN)*DENX) + 1
          ifby = INT(FB_NY*(coor(2)-YMIN)*DENY) + 1

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY = .FALSE.

          IF( FREE_SURFACE(ifbx,ifby)%ZMAX .GT. coor(3) ) CYCLE

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY = .TRUE.

          FREE_SURFACE(ifbx,ifby)%ZMAX     = coor(3)
          FREE_SURFACE(ifbx,ifby)%ID_RBDY3 = ibdyty
          FREE_SURFACE(ifbx,ifby)%ID_TACTY = itacty
          FREE_SURFACE(ifbx,ifby)%ACTIVE   = .TRUE.
       END DO

    END DO

  END SUBROUTINE init_free_boundary_RBDY3
!-------------------------------------------------------------------------------
    SUBROUTINE free_boundary_computation()

    IMPLICIT NONE
    INTEGER(kind=4)           :: itacty,ibdyty,ifbx,ifby
    REAL(kind=8)              :: DENX,DENY
    INTEGER(kind=4)           :: nb_tacty
    REAL(kind=8),DIMENSION(3) :: coor
    
    DENX = 1.0/(FB_XMAX-FB_XMIN)
    DENY = 1.0/(FB_YMAX-FB_YMIN)

    DO ifbx = 1,FB_NX
       DO ifby = 1,FB_NY
          FREE_SURFACE(ifbx,ifby)%ZMAX = -1D+24
          FREE_SURFACE(ifbx,ifby)%ID_RBDY3 = 0
          FREE_SURFACE(ifbx,ifby)%ID_TACTY = 0
          FREE_SURFACE(ifbx,ifby)%ACTIVE   = .TRUE.
       END DO
    END DO

    DO ibdyty=1,nb_RBDY3

       nb_tacty = SIZE(bdyty(ibdyty)%tacty)

       DO itacty=1,nb_tacty

          coor = get_coor(ibdyty,itacty)
          
          ifbx = INT(FB_NX*(coor(1)-FB_XMIN)*DENX) + 1
          ifby = INT(FB_NY*(coor(2)-FB_YMIN)*DENY) + 1

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY = .FALSE.

          IF( FREE_SURFACE(ifbx,ifby)%ZMAX .GT. coor(3) ) CYCLE

          bdyty(ibdyty)%tacty(itacty)%BDARY%BOUNDARY  = .TRUE.

          FREE_SURFACE(ifbx,ifby)%ZMAX     = coor(3)
          FREE_SURFACE(ifbx,ifby)%ID_RBDY3 = ibdyty
          FREE_SURFACE(ifbx,ifby)%ID_TACTY = itacty
          FREE_SURFACE(ifbx,ifby)%ACTIVE   = .TRUE.

       END DO

    END DO

  END SUBROUTINE free_boundary_computation
!-------------------------------------------------------------------------------

  !------------------------------------------------------------------------  
  SUBROUTINE get_drv_vlocy_RBDY3(ibdyty, indices, values)
  !permet d'extraire les vitesses imposees pour chaque corps ibdyty
    IMPLICIT NONE
    INTEGER(kind=4), INTENT(in) :: ibdyty
    INTEGER(kind=4), DIMENSION(:), POINTER :: indices
    REAL(kind=8)   , DIMENSION(:), POINTER :: values
    !
    INTEGER(kind=4) :: i, nb_drvdof
    REAL(kind=8)    :: Vbegind

    if( nb_RBDY3<1 ) return

    nb_drvdof = bdyty(ibdyty)%nb_vlocy_driven_dof
    IF( nb_drvdof==0 ) RETURN

    ALLOCATE( indices(nb_drvdof), values(nb_drvdof) )

    DO i = 1, nb_drvdof
      indices(i) = bdyty(ibdyty)%vlocy_driven_dof(i)%dofnb
      CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(i),Vbegind,values(i))
    END DO

  END SUBROUTINE

  SUBROUTINE comp_drv_vlocy_RBDY3(ibdyty, values)
    IMPLICIT NONE
    INTEGER(kind=4), INTENT(in) :: ibdyty
    REAL(kind=8), DIMENSION(:)  :: values
    !
    INTEGER(kind=4)   :: i
    REAL(kind=8)      :: Vbegind
    CHARACTER(len=27) :: IAM
    !      123456789012345678901234567
    IAM = 'RBDY3::comp_drv_vlocy_RBDY3'


    IF( SIZE(values) /= bdyty(ibdyty)%nb_vlocy_driven_dof ) THEN
      CALL faterr(IAM,'wrong size')
    ENDIF

    DO i=1, SIZE(values)
      CALL comp_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(i), Vbegind,values(i))
    END DO
  END SUBROUTINE

!!!------------------------------------------------------------------------

!----------------------------------------------------------
! for siconos and peligriff wrap
!----------------------------------------------------------

  !> compute the new position of grains for a given velocity
  SUBROUTINE comp_coor_4all_RBDY3

    IMPLICIT NONE 
    INTEGER :: ibdyty
    character(len=21) :: IAM
          !123456789012345678901
    IAM = 'RBDY3::comp_coor_4all'
    
    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)
       !$OMP PARALLEL DEFAULT(SHARED) &
       !$OMP PRIVATE(ibdyty)
       !$OMP DO SCHEDULE(RUNTIME)
       DO ibdyty=1,nb_RBDY3    

          IF( .NOT. bdyty(ibdyty)%visible) CYCLE

          bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin +  &
                            (1.d0-THETA)*H*bdyty(ibdyty)%Vbegin +  &
                            THETA*H*bdyty(ibdyty)%V 

       END DO
       !$OMP END DO
       !$OMP END PARALLEL
    CASE(INTEGRATOR_GEAR)
       call faterr(IAM,'method not supported with this INTEGRATOR')

    CASE(INTEGRATOR_VERLET)    

       call faterr(IAM,'method not supported with this INTEGRATOR')

    CASE DEFAULT
       call faterr(IAM,'method not supported with this INTEGRATOR')
       
    END SELECT

    IF (XPERIODIC .OR. YPERIODIC) CALL check_periodic
    IF (BOUNDS) CALL out_of_bounds_RBDY3

  END SUBROUTINE 

!!!------------------------------------------------------------------------
  SUBROUTINE get_density(ibdyty, density)
    IMPLICIT NONE
    INTEGER      :: ibdyty
    REAL(kind=8) :: density

    density = get_rho( bdyty(ibdyty)%blmty(1)%lawnb )
  END SUBROUTINE

!!!------------------------------------------------------------------------
  FUNCTION get_ptr_mass(ibdyty)

    IMPLICIT NONE

    INTEGER :: ibdyty
    REAL(kind=8), DIMENSION(:), POINTER :: get_ptr_mass

    get_ptr_mass=>bdyty(ibdyty)%mass

  END FUNCTION get_ptr_mass
!!!------------------------------------------------------------------------
  !vt 
  !compute the components of inertie in the global frame
  SUBROUTINE comp_glob_inert_RBDY3(ibdyty,GlobalInertia)

    IMPLICIT NONE 
    
    INTEGER :: ibdyty,i,err
    REAL(kind=8),DIMENSION(3,3), INTENT(inout):: GlobalInertia
    REAL(kind=8),DIMENSION(3,3):: Mat_IP,Inv_P
    character(len=90) :: cout 
    
    !We want the globalInertia J such as J=P-1 I P, with P the local Frame and I the local Inertia.
    
    !First we calculate the product A=IP 
    DO i=1,3
    Mat_IP(i,:)=bdyty(ibdyty)%mass(i+3)*bdyty(ibdyty)%LocalFrame(i,:)    
    END DO
    
    !Second we inverse the matrix P
    Inv_P=bdyty(ibdyty)%LocalFrame
    
    call inverse33(Inv_P, err)
    if (err == 1) then 
      write(cout,'("Inertia non invertible for RBDY3 ",I0 )') ibdyty
      call faterr('RBDY3::comp_glob_inert',trim(cout)) 
    endif   
    !Finally we calculate the last product P-1 A
    GlobalInertia=MATMUL(Inv_P,Mat_IP)

  END SUBROUTINE
!-------------------------------------------------------------------------------
!! routine developpee par E. Azema pour le pilotage d une cellule tri-tri en contrainte
!! num_xxx est le id des rbdy3 portant du plan xxx
!! loads(nb_loads,1) contient le rang du plan concerne (1:down,2:right,etc)
!! loads(nb_loads,2) contient la forces appliquee
 SUBROUTINE triaxial_loading(num_down,num_right,num_up,num_left,num_av,num_der, &
                              nb_loads,loads)
  
  IMPLICIT NONE
  INTEGER                                  :: num_down,num_right,num_up,num_left, &
                                              num_av,num_der,inb_loads
  INTEGER                                  :: nb_loads
  REAL(kind=8),DIMENSION(2,nb_loads)       :: loads  
  REAL(kind=8),DIMENSION(6,6)              :: coor
  REAL(kind=8),DIMENSION(6),SAVE           :: ep
  REAL(kind=8),DIMENSION(3),SAVE           :: L0
  REAL(kind=8),DIMENSION(6)                :: L
  LOGICAL                                  :: is_first_time=.TRUE.
  character(len=80) :: cout

  coor(:,1) = bdyty(num_down )%cooref+bdyty(num_down )%Xbegin
  coor(:,2) = bdyty(num_right)%cooref+bdyty(num_right)%Xbegin
  coor(:,3) = bdyty(num_up   )%cooref+bdyty(num_up   )%Xbegin
  coor(:,4) = bdyty(num_left )%cooref+bdyty(num_left )%Xbegin
  coor(:,5) = bdyty(num_av   )%cooref+bdyty(num_av   )%Xbegin
  coor(:,6) = bdyty(num_der  )%cooref+bdyty(num_der  )%Xbegin

  IF (is_first_time) THEN
    ep(1) = bdyty(num_down )%tacty(1)%BDARY%DATA(3)
    ep(2) = bdyty(num_right)%tacty(1)%BDARY%DATA(3)
    ep(3) = bdyty(num_up   )%tacty(1)%BDARY%DATA(3)
    ep(4) = bdyty(num_left )%tacty(1)%BDARY%DATA(3)
    ep(5) = bdyty(num_av   )%tacty(1)%BDARY%DATA(3)
    ep(6) = bdyty(num_der  )%tacty(1)%BDARY%DATA(3)

    L0(1) = ABS((coor(2,2)-ep(2))-(coor(2,4)+ep(4)))
    L0(2) = ABS((coor(1,5)-ep(5))-(coor(1,6)+ep(6)))
    L0(3) = ABS((coor(3,3)-ep(3))-(coor(3,1)+ep(1)))

    is_first_time=.FALSE.
  ENDIF

  L(1) = ABS((coor(2,2)-ep(2))-(coor(2,4)+ep(4)))              ! distance gauche/droite
  L(2) = ABS((coor(1,5)-ep(5))-(coor(1,6)+ep(6)))              ! distance avant/arriere
  L(3) = ABS((coor(3,3)-ep(3))-(coor(3,1)+ep(1)))              ! distance haut/bas

  DO inb_loads = 1, nb_loads
    SELECT CASE (INT(loads(1,inb_loads)))
    CASE (1) 
      bdyty(num_down)%Fext(3)=(loads(2,inb_loads))*(L(1)*L(2))
      PRINT*, '-> F(',num_down,') = ', bdyty(num_down)%Fext(3)
    CASE (2)
      bdyty(num_right)%Fext(2)=-(loads(2,inb_loads))*(L(2)*L(3))
      PRINT*, '-> F(',num_right,') = ', bdyty(num_right)%Fext(2)
    CASE (3)
      bdyty(num_up)%Fext(3)=-(loads(2,inb_loads))*L(1)*L(2)
      PRINT*, '-> F(',num_up,') = ', bdyty(num_up)%Fext(3)
    CASE (4)
      bdyty(num_left)%Fext(2)=(loads(2,inb_loads))*L(2)*L(3)
      PRINT*, '-> F(',num_left,') = ', bdyty(num_left)%Fext(2)
    CASE (5)
      bdyty(num_av)%Fext(1)=-(loads(2,inb_loads))*L(1)*L(3)
      PRINT*, '-> F(',num_av,') = ', bdyty(num_av)%Fext(1)
    CASE (6)
      bdyty(num_der)%Fext(1)=(loads(2,inb_loads))*L(1)*L(3)
      PRINT*, '-> F(',num_der,') = ', bdyty(num_der)%Fext(1)
    CASE default
       write(cout,'(A)') 'unsuported number in triaxial loading should 1|2|3|4|5|6'
       write(cout,'(I0)') loads(inb_loads,1)
       call faterr('RBDY3::triaxial_loading',cout)
    END SELECT
  END DO

END SUBROUTINE triaxial_loading

 SUBROUTINE put_vector_RBDY3(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect

                            !12345678901234567
   CHARACTER(len=18) :: IAM='RBDY3::put_vector'


   IF (nb_RBDY3 == 0) RETURN

   IF (nbdof /= SIZE(bdyty(ibdyty)%V) ) THEN
     call FATERR(IAM,'nbdof non concordant')
   ENDIF

   SELECT CASE(id_vect)
   CASE('X____')
     bdyty(ibdyty)%X=vect
   CASE('Xbeg_')
     bdyty(ibdyty)%Xbegin=vect
   CASE('V____')
     bdyty(ibdyty)%V=vect
   CASE('Vbeg_')
     bdyty(ibdyty)%Vbegin=vect
   CASE('Reac_')
     bdyty(ibdyty)%Ireac=vect*H
   CASE('Raux_')
     bdyty(ibdyty)%Iaux=vect*H
   CASE('Ireac')
     bdyty(ibdyty)%Ireac=vect
   CASE('Iaux_')
     bdyty(ibdyty)%Iaux=vect
   CASE('Vfree')
     bdyty(ibdyty)%Vfree=vect
   CASE('Fext_')
     bdyty(ibdyty)%Fext=bdyty(ibdyty)%Fext+vect
   !CASE('Coor_')
     !burk bdyty(ibdyty)%X = vect - bdyty(ibdyty)%cooref    
   !CASE('Coorb')
     !burk bdyty(ibdyty)%Xbegin = vect - bdyty(ibdyty)%cooref    
   CASE('Coor0')
     bdyty(ibdyty)%cooref = vect
   CASE DEFAULT
     call faterr('RBDY3::put_vector','unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE put_vector_RBDY3

 SUBROUTINE get_vector_RBDY3(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect

                            !12345678901234567
   CHARACTER(len=18) :: IAM='RBDY3::get_vector'


   IF (nb_RBDY3 == 0) RETURN

   IF (id_vect(1:4) == 'Coor') THEN
     IF(nbdof /= SIZE(bdyty(ibdyty)%cooref) ) THEN
       PRINT*,'cooref',nbdof,SIZE(bdyty(ibdyty)%cooref)
       call FATERR(IAM,'nbdof non concordant') 
     ENDIF
   ELSE
     IF (nbdof /= SIZE(bdyty(ibdyty)%V) ) THEN
       PRINT*,'V',nbdof,SIZE(bdyty(ibdyty)%V)
       call FATERR(IAM,'nbdof non concordant') 
     ENDIF
   ENDIF

   SELECT CASE(id_vect)
    CASE('Coor0')
     vect=bdyty(ibdyty)%cooref
    CASE('Coor_')
     vect=get_coor(ibdyty,0)
    CASE('Coorm')
     vect=get_coorTT(ibdyty,0)
    CASE('Coorb')
       vect= bdyty(ibdyty)%cooref + bdyty(ibdyty)%Xbegin
       if (XPERIODIC) vect(1) = modulo(vect(1),xperiode)
       if (YPERIODIC) vect(2) = modulo(vect(2),yperiode)
    CASE('X____')
     vect=bdyty(ibdyty)%X
    CASE('Xbeg_')
     vect=bdyty(ibdyty)%Xbegin
    CASE('V____')
     vect=bdyty(ibdyty)%V
    CASE('Vbeg_')
     vect=bdyty(ibdyty)%Vbegin
    CASE('Vfree')
     vect=bdyty(ibdyty)%Vfree
    CASE('Vaux_')
     vect=bdyty(ibdyty)%Vaux
    CASE('Raux_')
     vect=bdyty(ibdyty)%Iaux/H
    CASE('Reac_')
     vect=bdyty(ibdyty)%Ireac/H
    CASE('Iaux_')
     vect=bdyty(ibdyty)%Iaux
    CASE('Ireac')
     vect=bdyty(ibdyty)%Ireac
    !am: ajout de la recuperation la moyenne sur le pas de temps des 
    !    efforts exterieurs
    CASE('Fext_')
     vect=bdyty(ibdyty)%Fext
    CASE('Fint_')
     vect=bdyty(ibdyty)%Fint
    CASE DEFAULT
     call faterr('RBDY3::get_vector','unknown id: '//id_vect)
   END SELECT

 END SUBROUTINE get_vector_RBDY3

 FUNCTION get_ptr_vector_RBDY3(id_vect,ibdyty)
 IMPLICIT NONE

   INTEGER :: ibdyty
   REAL(kind=8),DIMENSION(:),POINTER :: get_ptr_vector_RBDY3
   CHARACTER(len=5) :: id_vect

   IF (nb_RBDY3 == 0) RETURN

   SELECT CASE(id_vect)
    CASE('Coor0')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%cooref
    CASE('X____')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%X
    CASE('Xbeg_')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%Xbegin
    CASE('V____')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%V
    CASE('Vbeg_')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%Vbegin
    CASE('Vaux_')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%Vaux
    CASE('Ireac')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%Ireac
    CASE('Iaux_')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%Iaux
    !am: ajout de la recuperation la moyenne sur le pas de temps des 
    !    efforts exterieurs
    CASE('Fext_')
     get_ptr_vector_RBDY3 => bdyty(ibdyty)%Fext
    CASE DEFAULT
     call faterr('RBDY3::get_ptr_vector','unknown id: '//id_vect)
   END SELECT

 END FUNCTION get_ptr_vector_RBDY3

 !> \brief Set a matrix of a RBDY3
 !> currently only inertia frame matrices
 SUBROUTINE put_matrix_RBDY3(id_mat,ibdyty,mat,nbdof)
   IMPLICIT NONE
   CHARACTER(len=5) :: id_mat !< [in] which matrix to set
   INTEGER(kind=4)  :: ibdyty !< [in] body number
   INTEGER(kind=4)  :: nbdof  !< [in] order of the matrix to set
   REAL(kind=8), DIMENSION(nbdof,nbdof) :: mat !< [in] new values of the matrix
   !
   CHARACTER(len=80) :: cout
   CHARACTER(len=17) :: IAM
   !      12345678901234567
   IAM = 'RBDY3::put_matrix'

   IF( nb_RBDY3 == 0 ) RETURN

   !am : fait planter gfortran
   !if( any(shape(mat) /= shape(bdyty(ibdyty)%localFrame)) ) then
   IF( nbdof /= 3 ) THEN
     CALL faterr(IAM, 'nbdof non concordant')
   END IF

   SELECT CASE(id_mat)
    CASE('IFbeg')
      bdyty(ibdyty)%localFrameIni(1:nbdof,1:nbdof) = mat(1:nbdof,1:nbdof)
    CASE('IFTT_')
      bdyty(ibdyty)%localFrameTT(1:nbdof,1:nbdof)  = mat(1:nbdof,1:nbdof)
    CASE('IF___')
      bdyty(ibdyty)%localFrame(1:nbdof,1:nbdof)    = mat(1:nbdof,1:nbdof)
    CASE default
      WRITE(cout,'(A,1x,A)') 'Sorry unknown id:',id_mat
      CALL faterr(IAM,cout)
   END SELECT

 END SUBROUTINE put_matrix_RBDY3

 !> \brief Get a matrix of a RBDY3
 !> currently only inertia frame matrices
 SUBROUTINE get_matrix_RBDY3(id_mat,ibdyty,mat,nbdof)
   IMPLICIT NONE
   CHARACTER(len=5) :: id_mat !< [in] which matrix to get
   INTEGER(kind=4)  :: ibdyty !< [in] body number
   INTEGER(kind=4)  :: nbdof  !< [in] order of the matrix to get
   REAL(kind=8), DIMENSION(nbdof,nbdof) :: mat !< [out] values of the matrix
   !
   CHARACTER(len=80) :: cout
   CHARACTER(len=17) :: IAM
   !      12345678901234567
   IAM = 'RBDY3::get_matrix'

   IF( nb_RBDY3 == 0 ) RETURN

   !am : fait planter gfortran
   !if( any(shape(mat) /= shape(bdyty(ibdyty)%localFrame)) ) then
   IF( nbdof /= 3 ) THEN
     CALL faterr(IAM, 'nbdof non concordant')
   END IF

   SELECT CASE(id_mat)
    CASE('IFref')
      mat = bdyty(ibdyty)%localFrameRef
    CASE('IFbeg')
      mat = get_inertia_frameIni(ibdyty)
    CASE('IFTT_')
      mat = get_inertia_frameTT(ibdyty)
    CASE('IF___')
      mat = get_inertia_frame(ibdyty)
    CASE default
      WRITE(cout,'(A,1x,A)') 'Sorry unknown id:',id_mat
      CALL faterr(IAM,cout)
   END SELECT

 END SUBROUTINE get_matrix_RBDY3

 subroutine get_all_rdata_RBDY3(rdata, nb_bodies, nb_fields)
   implicit none
   integer, intent(in) :: nb_bodies !< [in] number of rbdy3
   integer, intent(in) :: nb_fields !< [in] number of components
   real(kind=8), dimension(nb_fields,nb_bodies) :: rdata !< [out] values of the matrix
   !
   integer :: ibdyty
   character(len=80) :: cout
   character(len=20) :: IAM
   !      12345678901234567890
   IAM = 'RBDY3::get_all_rdata'

   if( nb_RBDY3 == 0 ) return

   if( nb_bodies /= NB_RBDY3 ) then
     call faterr(IAM, 'wrong number of bodies')
   end if

   do ibdyty = 1, nb_rbdy3
     rdata( 1: 3,ibdyty) = bdyty(ibdyty)%cooref(1:3) + bdyty(ibdyty)%X(1:3)
     rdata( 4: 9,ibdyty) = bdyty(ibdyty)%V(1:6)
     rdata(10:15,ibdyty) = bdyty(ibdyty)%Fext(1:6)
     rdata(16:21,ibdyty) = bdyty(ibdyty)%Ireac(1:6)
     rdata(22:30,ibdyty) = reshape(bdyty(ibdyty)%LocalFrame, (/9/))
   end do

 end subroutine get_all_rdata_RBDY3

!------------------------------------------------------------------------
!am : nouvelles fonctions pour rendre possible un calcul multi-domaines sequentiel
!     dans le cadre de l'architecture actuelle
!------------------------------------------------------------------------

 !> \brief copy several times read bodies and store copied bodies at the end of the bodies container
 !>        Warning: the number of RBDY3 is altered by this subroutine
 SUBROUTINE copy_bodies_RBDY3(factor)

    IMPLICIT NONE

    ! variable d'entree
    INTEGER, INTENT(in) :: factor !< [in] total number of copies in the bodies container after
                                  !< the calling, i.e. each body is duplicated factor - 1 times

    ! variables locales
    INTEGER :: nb_copies ! nombfe de copies a realiser
    INTEGER :: new_ibdyty ! indice ou stocker la copie courante du corps courant
    INTEGER :: nb_bulks  ! nombre de bulks du corps courant
    INTEGER :: nb_tacts  ! nombre de contacteurs du corps courant
    INTEGER :: size_data ! taille du tableau data du contacteur courant
    INTEGER :: size_idata ! taille du tableau idata du contacteur courant
    INTEGER :: nbdof ! nombre de dof pour un noeud
    INTEGER :: errare ! pour recuperer le code d'erreur renvoye par une llocation memoire
    INTEGER :: icopy ! indice de boucle sur les copies
    INTEGER :: ibdyty ! indice de boucle sur les corps
    INTEGER :: iblmty ! indice de boucle sur les bulks
    INTEGER :: itacty ! indice de boucle sur les contacteurs

    CHARACTER(len=103) :: cout
    !                         123456789012345678         
    CHARACTER(len=18) :: IAM='RBDY3::copy_bodies'

    ! verification de la coherence des donnees :

    ! si le facteur est plus petit que 1
    IF (factor < 1) THEN
       ! on affiche un message d'erreur
       CALL logmes('Errror '//IAM// ": factor must be reater then one!")
    END IF

    ! si le tableau des corps ne peut contenir toutes les copies
    IF (nb_RBDY3*factor > SIZE(bdyty)) THEN
       ! on affiche un message d'erreur
       CALL logmes('Error '//IAM// ": bdyty is too small!")
    END IF

    ! on calcule le nombre de copies a realiser
    nb_copies = factor - 1

    ! pour chaque copie a realiser
    DO icopy=1, nb_copies
       ! pour chaque corps
       DO ibdyty=1, nb_RBDY3
          ! on calcule l'indice ou stocker la copie courante du corps courant
          new_ibdyty = icopy*nb_RBDY3 + ibdyty
          ! on copie l'identifiant du corps courant
          bdyty(new_ibdyty)%bdyID = bdyty(ibdyty)%bdyID
 
          !
          ! copie des bulks
          ! 

          ! on recupere le nombre de bulks du corps courant
          nb_bulks = SIZE(bdyty(ibdyty)%blmty)
          ! on alloue l'espace memoire pour stocker les bulks de la copie
          ! du corps courant
          ALLOCATE(bdyty(new_ibdyty)%blmty(nb_bulks), stat=errare)
          ! si l'allocation a echoue
          IF (errare /= 0) THEN
             ! on affiche un message d'erreur
             CALL logmes('Error '//IAM//': error allocating bdyty%blmty')
          END IF

          ! pour chaque bulk du corps courant 
          DO iblmty=1, nb_bulks
             ! on copie :
             !   * l'identifiant du bulk 
             bdyty(new_ibdyty)%blmty(iblmty)%blmID = bdyty(ibdyty)%blmty(iblmty)%blmID
             !   * le surnom du materiau volumique
             bdyty(new_ibdyty)%blmty(iblmty)%behav = bdyty(ibdyty)%blmty(iblmty)%behav
             ! si le bulk est de type PLAIN
             IF (bdyty(new_ibdyty)%blmty(iblmty)%blmID == 'PLAIN') THEN
                ! on copie :
                !   * l'average radius
                bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%avr_radius = bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius
                !   * le volume
                bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%volume = bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume
                !   * les inerties principales
                bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%I1 = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1
                bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%I2 = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2
                bdyty(new_ibdyty)%blmty(iblmty)%PLAIN%I3 = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3
             ! sinon
             ELSE
                ! on affiche un message d'erreur
                CALL logmes('Error '//IAM//': error only PLAIN bulks are supported!')
             END IF
          END DO

          !
          ! copie du noeud
          ! 

          ! on copie l'identifiant du noeud du corps courant
          call new_nodty(bdyty(new_ibdyty)%nodty,get_nodNAME(bdyty(ibdyty)%nodty))

          ! on recupere le nombre dof en fonction du type de neoud
          nbdof = nbdof_a_nodty(bdyty(new_ibdyty)%nodty)

          ! N.B.: pas d'allocation memoire necessaire pour stocker la copie des champs porte par le neoud du corps courant
          !       puisque les champs sont des tableaux de taille fixe et pas des pointeurs!

          ! on copie :
          !   * les coordonnees du noeud
          bdyty(new_ibdyty)%cooref = bdyty(ibdyty)%cooref
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
          IF (smooth_method) THEN
             ! on copie les champs ad hoc
             bdyty(new_ibdyty)%Abegin = bdyty(ibdyty)%Abegin
             bdyty(new_ibdyty)%A      = bdyty(ibdyty)%A
             bdyty(new_ibdyty)%Bbegin = bdyty(ibdyty)%Bbegin
             bdyty(new_ibdyty)%B      = bdyty(ibdyty)%B
             bdyty(new_ibdyty)%Cbegin = bdyty(ibdyty)%Cbegin
             bdyty(new_ibdyty)%C      = bdyty(ibdyty)%C
          END IF

          ! dans tous les cas, on stocke l'orientation (i.e. l'expression du repere principal d'inertie dans le repere global) :
          !    * initiale
          bdyty(new_ibdyty)%LocalFrameIni = bdyty(ibdyty)%LocalFrameIni
          !    * courante
          bdyty(new_ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrame
          !    * dans la configuration de detection
          bdyty(new_ibdyty)%LocalFrameTT = bdyty(ibdyty)%LocalFrameTT

          ! gestion des C.L. imposees
          ! N.B.: les C.I. ont deja ete prises en commpte via Xbegin et Vbegin

          ! on copie le nombre de dof imposes :
          !   * en vitesse
          bdyty(new_ibdyty)%nb_vlocy_driven_dof = bdyty(ibdyty)%nb_vlocy_driven_dof
          !   * en force
          bdyty(new_ibdyty)%nb_force_driven_dof = bdyty(ibdyty)%nb_force_driven_dof

          ! s'il ya des conditions limites en vitesse
          IF (bdyty(new_ibdyty)%nb_vlocy_driven_dof > 0) THEN
             ! on alloue l'esapce memoire pour stocker
             !   * les indices des dofs imposes en vitesse
             ALLOCATE(bdyty(new_ibdyty)%vlocy_driven_dof(bdyty(new_ibdyty)%nb_vlocy_driven_dof))
             !   * les vitesses imposees
             ALLOCATE(bdyty(new_ibdyty)%Vdriv(bdyty(new_ibdyty)%nb_vlocy_driven_dof))
             !   * les deplacements imposes
             ALLOCATE(bdyty(new_ibdyty)%Xdriv(bdyty(new_ibdyty)%nb_vlocy_driven_dof))
             ! si l'allocation a echoue
             IF (errare /= 0) THEN
                ! on affiche un message d'erreur
                CALL logmes('Error '//IAM// ': error allocating bdyty%Vdriv, bdyty%Xdriv, ...')
             END IF

             ! on copie les C.L. en vitesse
             bdyty(new_ibdyty)%vlocy_driven_dof = bdyty(ibdyty)%vlocy_driven_dof
             bdyty(new_ibdyty)%Vdriv            = bdyty(ibdyty)%Vdriv
             bdyty(new_ibdyty)%Xdriv            = bdyty(ibdyty)%Xdriv
          ! sinon,
          ELSE
             ! on initialise les pointeurs a nul
             NULLIFY(bdyty(new_ibdyty)%vlocy_driven_dof)
             NULLIFY(bdyty(new_ibdyty)%Vdriv)
             NULLIFY(bdyty(new_ibdyty)%Xdriv)
          END IF
          ! s'il ya des conditions limites en force
          IF (bdyty(new_ibdyty)%nb_force_driven_dof > 0) THEN
             ! on alloue l'esapce memoire pour stocker
             !   * les indices des dofs imposes en force
             ALLOCATE(bdyty(new_ibdyty)%force_driven_dof(bdyty(new_ibdyty)%nb_force_driven_dof))
             !   * les forces imposees
             ALLOCATE(bdyty(new_ibdyty)%Fdriv(bdyty(new_ibdyty)%nb_force_driven_dof))

             ! si l'allocation a echoue
             IF (errare /= 0) THEN
                ! on affiche un message d'erreur
                CALL logmes('Error '//IAM// ': error allocating bdyty%Fdriv, ...')
             END IF

             ! on copie les C.L. en force
             bdyty(new_ibdyty)%force_driven_dof = bdyty(ibdyty)%force_driven_dof
             bdyty(new_ibdyty)%Fdriv            = bdyty(ibdyty)%Fdriv
          ! sinon,
          ELSE
             ! on initialise les pointeurs a nul
             NULLIFY(bdyty(new_ibdyty)%force_driven_dof)
             NULLIFY(bdyty(new_ibdyty)%Fdriv)
          END IF
          ! si l'allocation a echoue
          IF (errare /= 0) THEN
             ! on affiche un message d'erreur
             CALL logmes('Error '//IAM// ': error allocating bdyty%Vdriv, bdyty%Fdriv, ...')
          END IF

          ! gestion des autres champs (scalaires)
          !am : j'en oublie surement d'autres... 

          ! on copie 
          !   * visibilite (initialement, tous invisibles)
          bdyty(new_ibdyty)%visible = .FALSE.
          !   * autres
          bdyty(new_ibdyty)%area     = bdyty(ibdyty)%area
          !bdyty(new_ibdyty)%xperiode = bdyty(ibdyty)%xperiode
          !bdyty(new_ibdyty)%yperiode = bdyty(ibdyty)%yperiode
          bdyty(new_ibdyty)%T        = bdyty(ibdyty)%T
          bdyty(new_ibdyty)%Talpha   = bdyty(ibdyty)%Talpha

          !
          ! copie des contacteurs
          ! 

          ! on recupere le nombre de contacteurs du corps courant
          nb_tacts = SIZE(bdyty(ibdyty)%tacty)
          ! si l'objet a au moins un contacteur
          IF (nb_tacts /= 0) THEN
             ! on alloue l'espace memoire pour stocker les contacteurs de la copie
             ! du corps courant
             ALLOCATE(bdyty(new_ibdyty)%tacty(nb_tacts), stat=errare)
             ! si l'allocation a echoue
             IF (errare /= 0) THEN
                ! on affiche un message d'erreur
                CALL logmes('Error '//IAM// ': error allocating bdyty%tacty')
             END IF

             ! on alloue l'espace memoire pour stocker la map bdyty2tacty
             ALLOCATE(bdyty(new_ibdyty)%bdyty2tacty(2, nb_tacts), stat=errare)
             ! si l'allocation a echoue
             IF (errare /= 0) THEN
                ! on affiche un message d'erreur
                CALL logmes('Error '//IAM// ': error allocating bdyty%bdyty2tacty')
             END IF

             ! pour chaque contacteur du corps courant 
             DO itacty=1, nb_tacts
                ! on copie :
                !   * l'identifiant du contacteur
                bdyty(new_ibdyty)%tacty(itacty)%tacID = bdyty(ibdyty)%tacty(itacty)%tacID
                !   * la couleur du contacteur
                bdyty(new_ibdyty)%tacty(itacty)%color = bdyty(ibdyty)%tacty(itacty)%color

                ! on rempli la structure BDARY du contacteur courant

                ! si le tableau data du contacteur courant a ete alloue
                IF (ASSOCIATED(bdyty(ibdyty)%tacty(itacty)%BDARY%data)) THEN
                   ! on recupere la table du tableau data du contacteur courant
                   size_data = SIZE(bdyty(ibdyty)%tacty(itacty)%BDARY%data)

                   ! on alloue l'esapce memoire pour stocker le tableau data
                   ALLOCATE(bdyty(new_ibdyty)%tacty(itacty)%BDARY%data(size_data))

                   ! on copie le tableau data
                   bdyty(new_ibdyty)%tacty(itacty)%BDARY%data = bdyty(ibdyty)%tacty(itacty)%BDARY%data
                ! sinon,
                ELSE
                   ! on initialise le pointeur a nul
                   NULLIFY(bdyty(new_ibdyty)%tacty(itacty)%BDARY%data)
                END IF
                ! si le tableau idata du contacteur courant a ete alloue
                IF (ASSOCIATED(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)) THEN
                   ! on recupere la table du tableau idata du contacteur courant
                   size_idata = SIZE(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)

                   ! on alloue l'esapce memoire pour stocker le tableau idata
                   ALLOCATE(bdyty(new_ibdyty)%tacty(itacty)%BDARY%idata(size_idata))

                   ! on copie le tableau idata
                   bdyty(new_ibdyty)%tacty(itacty)%BDARY%idata = bdyty(ibdyty)%tacty(itacty)%BDARY%idata
                ! sinon,
                ELSE
                   ! on initialise le pointeur a nul
                   NULLIFY(bdyty(new_ibdyty)%tacty(itacty)%BDARY%idata)
                END IF
                ! on copie :
                !   * le volume
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%volume     = bdyty(ibdyty)%tacty(itacty)%BDARY%volume
                !   * le rdg
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%rdg        = bdyty(ibdyty)%tacty(itacty)%BDARY%rdg
                !   * les inerties principales
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%I1         = bdyty(ibdyty)%tacty(itacty)%BDARY%I1
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%I2         = bdyty(ibdyty)%tacty(itacty)%BDARY%I2
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%I3         = bdyty(ibdyty)%tacty(itacty)%BDARY%I3
                !   * le localframe 
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%embededframe = bdyty(ibdyty)%tacty(itacty)%BDARY%embededframe
                !   * le shift
                bdyty(new_ibdyty)%tacty(itacty)%BDARY%shift      = bdyty(ibdyty)%tacty(itacty)%BDARY%shift

                !am: on ne copie pas les champs utilises pour le multi-physique... pour l'instant!
             END DO
          !sinon,
          ELSE
             ! on initialise les tableaux (pointeurs) propres aux contacteurs a nul
             NULLIFY(bdyty(new_ibdyty)%tacty)
             NULLIFY(bdyty(new_ibdyty)%bdyty2tacty)
             ! on affiche un warning (parce que c'est quand meme louche un rigide sans contacteur...)
             !123456789   01234   567890123456789012
             WRITE(cout,'(A32,I7)') 'warning: RBDY3 without contactor', new_ibdyty
             CALL logmes(cout)
          END IF
       END DO
    END DO

    ! on modifie le nombre de RBDY3
    nb_RBDY3 = factor*nb_RBDY3

    !am : je pense que cette mise a jour des entites craint, si on a eu le mauvais gout de charger
    !   d'autres type de corps entre la creation des RBDY3 et cette copie... mais je n'ai rien de 
    !   mieux a proposer pour l'instant

    ! si on n'a pas encore mis a jour la liste d'entites
    IF (nb_existing_entities == 0) THEN
       ! on le fait
       CALL update_existing_entities_RBDY3
    ! sinon,
    ELSE
       ! on met a jour le nombre d'entites, en ajoutant les corps copies
       CALL add_nb_ENTITY(nb_copies*nb_RBDY3)
    END IF

 END SUBROUTINE copy_bodies_RBDY3

 !> \brief set visibility for a given list of bodies
 SUBROUTINE set_visibility_list(indices, visibilities, nb_bodies)

    IMPLICIT NONE

    ! inputs
    INTEGER, INTENT(in) :: nb_bodies ! considered number od bodies
    INTEGER, DIMENSION(nb_bodies), INTENT(in) :: indices ! body indices list
       ! to which set the visibility
    LOGICAL, DIMENSION(nb_bodies), INTENT(in) :: visibilities ! visibilities
       ! to be set

    ! local variables         12345678901234567890123456
    CHARACTER(len=26) :: IAM='RBDY3::set_visibility_list'
    INTEGER :: i ! some index

    ! consistancy check of the inidces list
   
    ! if at least one index is not defined, the prgram is stopped
    IF (MINVAL(indices) < 1 .OR. MAXVAL(indices) > nb_RBDY3) THEN
       CALL faterr(IAM, ': inconsistent body indices list!') 
    END IF

    ! for each considered body
    DO i=1, nb_bodies
       ! visibility of the current body is set
       bdyty(indices(i))%visible = visibilities(i)
    END DO

 END SUBROUTINE set_visibility_list

  !> \brief set visibility for all bodies
 SUBROUTINE set_visibility_4all_RBDY3(visibilities, size_visibilities)

    IMPLICIT NONE

    ! inouts
    INTEGER, INTENT(in) :: size_visibilities ! size of the array visibilities
    LOGICAL, DIMENSION(size_visibilities), INTENT(in) :: visibilities ! array
       ! giving visibility for each body

    ! local variables         12345678901234567890123456
    CHARACTER(len=26) :: IAM='RBDY3::set_visibility_4all'
    INTEGER :: i ! some index

    ! if the size of vsibilities is not equal to the number of bodies, the programm is stopped
    IF (size_visibilities /= nb_RBDY3) THEN
       CALL faterr(IAM, 'the number of visibilities icompatible with the number of bodies!')
    END IF

    ! visibility of each body is set using the set_visibility_list subroutine
    ! (using the list of all body indices)
    CALL set_visibility_list( (/ (i, i=1, nb_RBDY3) /) , visibilities, nb_RBDY3)

 END SUBROUTINE set_visibility_4all_RBDY3

  subroutine set_invisible(nb_bdy,list_bdy)
    
    implicit none
    integer,intent(in) :: nb_bdy
    integer,dimension(nb_bdy),intent(in) :: list_bdy
    integer :: i
    !                          123456789012345678901234 
    character(len=24)  :: IAM='mod_RBDY3::set_invisible'

    ! on rend invisibles tous les corps de la liste
    call set_visibility_list(list_bdy, (/ (.false., i=1, nb_bdy) /), nb_bdy)

  end subroutine set_invisible

  !> \brief set mass of a given RBDY3
  SUBROUTINE set_mass_RBDY3(ibdyty, mass) 

    IMPLICIT NONE

    ! inputs
    INTEGER, INTENT(in)      :: ibdyty ! the considered RBDY3
    REAL(kind=8), INTENT(in) :: mass    ! the mass to be set

    INTEGER                     :: iblmty,ibehav,iccdof,ivd
    REAL(kind=8)                :: Ummass,mass1,mass2,mass3,vol,I1,I2,I3
    character(len=80) :: cout
    
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
       !ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb
       SELECT CASE(bdyty(ibdyty)%blmty(iblmty)%blmID)
       CASE('PLAIN')
          
          !Ummass = get_rho(ibehav)
          
          I1  = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1
          I2  = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2
          I3  = bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3
          vol = bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume

       CASE default
          CALL LOGMES('you try to compute the mass of an unknown blmty')
       END SELECT
    END DO
    
    ! the given mass is set to mass terms
    mass1  = mass
    mass2  = mass1
    mass3  = mass1
    iblmty = 1            

    ! the density coresponding to the actual mass is computed in order to
    ! compute inertia terms
    Ummass = mass/vol    

    I1 = I1*Ummass
    I2 = I2*Ummass
    I3 = I3*Ummass
    
    DO iccdof=1,SIZE(bdyty(ibdyty)%V)
       
       IF (iccdof == 1) bdyty(ibdyty)%mass(iccdof)=mass1
       IF (iccdof == 2) bdyty(ibdyty)%mass(iccdof)=mass2
       IF (iccdof == 3) bdyty(ibdyty)%mass(iccdof)=mass3
       IF (iccdof == 4) bdyty(ibdyty)%mass(iccdof)=I1
       IF (iccdof == 5) bdyty(ibdyty)%mass(iccdof)=I2
       IF (iccdof == 6) bdyty(ibdyty)%mass(iccdof)=I3
       
       IF (bdyty(ibdyty)%mass(iccdof)>0.D0) THEN
          bdyty(ibdyty)%inv_mass(iccdof)=1.D0/bdyty(ibdyty)%mass(iccdof)
       ELSE
          write(cout,'(A,1x,I0)') 'RBDY3:',ibdyty
          write(cout,'(A,1x,D14.7,A,1x,I0)') 'MASS:',bdyty(ibdyty)%mass(iccdof),', iccdof:',iccdof
          call faterr('RBDY3::set_mass',cout)
       END IF
    END DO
    
!!$    print*,'corps: ',ibdyty
!!$    print*,'geo: '
!!$    print*,bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume,bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1, &
!!$           bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2,bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3 
!!$    print*,'masse: '
!!$    print*,bdyty(ibdyty)%mass(1:6)
!!$    print*,'inv masse: '
!!$    print*,bdyty(ibdyty)%inv_mass(1:6)
!!$    print*,'xxxxxxxxxxxxxxxxx'

    IF (bdyty(ibdyty)%nb_vlocy_driven_dof == 0) RETURN 
    
    ! nullifying inv_mass where degrees of freedom are driven
    DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
       iccdof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))
       bdyty(ibdyty)%inv_mass(iccdof) = 0.D0
    END DO

!!$   print*,'inv masse avec ddl'
!!$   print*,bdyty(ibdyty)%inv_mass(1:6)
!!$   print*,'xxxxxxxxxxxxxxxxx'

  END SUBROUTINE set_mass_RBDY3

!!!------------------------------------------------------------------------
  ! TODO DDM : ajouter une fonction qui ne calculerait que la vitesse a la fin du pas
  SUBROUTINE comp_dof_one_RBDY3(ibdyty)
    
    IMPLICIT NONE 

    INTEGER, INTENT(in)         :: ibdyty

    INTEGER                     :: k,l
    REAL(kind=8)                :: TTH
    REAL(kind=8),DIMENSION(6)   :: corr,acce

    REAL(kind=8),DIMENSION(3) :: spin

    SELECT CASE(M_INTEGRATOR_ID)
    CASE(INTEGRATOR_MOREAU)

       TTH = THETA*H

       IF (bdyty(ibdyty)%visible) THEN

          ! Reac(1:3) resultante dans R0
          ! Reac(4:6) moment dans RG_M
          !
          ! V(1:3) vitesse de translation dans R0
          ! V(4:6) vitesse de rotation dans RG_M
          
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vfree + bdyty(ibdyty)%inv_mass*bdyty(ibdyty)%IReac
             
          ! actualisation de la translation X

          !am : pour pouvoir faire plusieurs compute_dof sans cumuler les corrections
          !bdyty(ibdyty)%X(1:3) = bdyty(ibdyty)%X(1:3) + TTH*bdyty(ibdyty)%V(1:3)
          bdyty(ibdyty)%X(1 : 3) = bdyty(ibdyty)%Xbegin(1 : 3) + & 
                                   H*(((1.d0-THETA)*bdyty(ibdyty)%Vbegin(1 : 3)) + &
                                             (THETA*bdyty(ibdyty)%V(1 : 3))) 
          !mr useless
          bdyty(ibdyty)%X(4:6) = 0.D0
          
          ! actualisation du repere principal d'inertie
          
          IF ( new_rotation_scheme ) THEN
          
             !bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT

             spin(1:3) = bdyty(ibdyty)%V(4:6)
             CALL update_inertia_frame33(2,H,spin,bdyty(ibdyty)%LocalFrameini,bdyty(ibdyty)%LocalFrame)
          
          ELSE

             spin(1:3) = theta*bdyty(ibdyty)%V(4:6)
             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)

          END IF
 
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
             CALL apply_vlocy_driven_dof(ibdyty,iV____)

       END IF
       
    CASE(INTEGRATOR_GEAR) 
       IF (bdyty(ibdyty)%visible) THEN
       
          acce = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%IReac/H + bdyty(ibdyty)%Fext)
          corr = acce - bdyty(ibdyty)%A
          
          bdyty(ibdyty)%X=bdyty(ibdyty)%X + cr*corr 
          bdyty(ibdyty)%V=bdyty(ibdyty)%V + cv*corr
          bdyty(ibdyty)%A=acce
          bdyty(ibdyty)%B=bdyty(ibdyty)%B + cb*corr
          bdyty(ibdyty)%C=bdyty(ibdyty)%C + cc*corr

          !!!mr pas correct voir plus tard

          IF ( new_rotation_scheme ) THEN
          
             bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT
          
          ELSE

             spin(1:3) = bdyty(ibdyty)%V(4:6)

             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)

             !mr useless
             bdyty(ibdyty)%X(4:6) = 0.D0

          END IF
          
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
             CALL apply_vlocy_driven_dof(ibdyty,iV____)
          
       END IF

    CASE(INTEGRATOR_VERLET)    
       IF (bdyty(ibdyty)%visible) THEN
       
          bdyty(ibdyty)%A = bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Ireac/H+bdyty(ibdyty)%Fext)
          bdyty(ibdyty)%V = bdyty(ibdyty)%Vbegin + 0.5*H*(bdyty(ibdyty)%A+bdyty(ibdyty)%Abegin)
       
          IF ( new_rotation_scheme ) THEN
          
             bdyty(ibdyty)%LocalFrame = bdyty(ibdyty)%LocalFrameTT
          
          ELSE

             spin(1:3) = bdyty(ibdyty)%V(4:6) + 0.5*H*bdyty(ibdyty)%A(4:6)

             CALL update_inertia_frame33(1,H,spin,bdyty(ibdyty)%LocalFrameTT,bdyty(ibdyty)%LocalFrame)

          END IF
          
          IF ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) &
             CALL apply_vlocy_driven_dof(ibdyty,iV____)
       
       END IF
    CASE DEFAULT

       CALL faterr('RBDY3::comp_dof_one','INTEGRATOR NOT SUPPORTED YET!')

    END SELECT

    IF (XPERIODIC .OR. YPERIODIC) CALL check_periodic
    IF (BOUNDS) CALL out_of_bounds_RBDY3

  END SUBROUTINE comp_dof_one_RBDY3

!!!------------------------------------------------------------------------  
  SUBROUTINE comp_free_vlocy_one_RBDY3(ibdyty)
  
    IMPLICIT NONE
    INTEGER, INTENT(in) :: ibdyty

    if (bdyty(ibdyty)%visible) then    
       bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vbegin &
            + H*bdyty(ibdyty)%inv_mass*(bdyty(ibdyty)%Fext & 
            + bdyty(ibdyty)%Fint)
       
       if ( bdyty(ibdyty)%nb_vlocy_driven_dof /= 0) & 
          CALL apply_vlocy_driven_dof(ibdyty,iVfree)
    end if

  END SUBROUTINE comp_free_vlocy_one_RBDY3

!-------------------------------------------------------------------------------
! melimelo spirit
!------------------------------------------------------------------------------

 !> \brief Initialize the module
 subroutine set_nb_RBDY3(nb)
    IMPLICIT NONE
    INTEGER(kind=4), INTENT(in) :: nb !< [in] number of RBDY3 in the simulation
    !
    INTEGER(kind=4) :: errare,ibdyty
    CHARACTER(len=80)::cout
    CHARACTER(len=12)::IAM='RBDY3::set_nb'


    if( associated(bdyty) ) then
      write( cout, *) "["//IAM//"] Huuummm, bdyty of RBDY3 already associated... cleaning the module"
      call logmes(cout,.true.)
      call clean_memory()
    end if

    nb_RBDY3=0
    allocate(bdyty(nb))

 end subroutine

 subroutine init_bdyty_(bdy)
   implicit none
   type(T_RBDY3), intent(inout) :: bdy

   bdy%bdyID = 'RBDY3'

   bdy%cooref(1:6) = 0.d0
   bdy%coorTT(1:3) = 0.d0

   bdy%Xbegin  = 0.d0
   bdy%Vbegin  = 0.d0
   bdy%X       = 0.d0
   bdy%V       = 0.d0
   bdy%Vfree   = 0.d0
   bdy%Ireac   = 0.d0
   bdy%Iaux    = 0.d0
   bdy%Fext    = 0.d0
   bdy%Fint    = 0.d0
   bdy%visible = .true.

   if( smooth_method ) then
      bdy%Abegin = 0.d0
      bdy%Bbegin = 0.d0
      bdy%Cbegin = 0.d0
      bdy%A      = 0.d0
      bdy%B      = 0.d0
      bdy%C      = 0.d0
   end if

   bdy%area = 0.d0

 end subroutine init_bdyty_

 subroutine init_nodty_(bdy, cooref)
   implicit none
   type(T_RBDY3)             , intent(inout) :: bdy
   real(kind=8), dimension(3), intent(in)    :: cooref

   !bdy%nodty = i_NO6xx
   call new_nodty(bdy%nodty,'NO6xx')

   bdy%cooref(1:3) = cooref(1:3)
   bdy%cooref(4:6) = 0.d0

 end subroutine

 subroutine add_one_RBDY3(cooref, nb_tacty, nb_v_drvdof, nb_f_drvdof)
   implicit none
   real(kind=8), dimension(3), intent(in) :: cooref
   integer                   , intent(in) :: nb_tacty
   integer                   , intent(in) :: nb_v_drvdof
   integer                   , intent(in) :: nb_f_drvdof
   !
   integer :: i_bdyty, i_blmty, i_tacty, nb_blmty
   !                           12345678901234
   character(len=14) :: IAM = "RBDY3::add_one"

   if( .not. associated(bdyty)  ) call faterr(IAM, 'bdyty not allocated')
   if( nb_RBDY3+1 > size(bdyty) ) call faterr(IAM, 'bdyty allocated too small')

   nb_blmty = 1

   i_bdyty = nb_RBDY3 + 1

   ! null initialization
   call init_bdyty_(bdyty(i_bdyty))

   ! nodty part
   call init_nodty_(bdyty(i_bdyty), cooref)

   ! drvdof part
   call reset_driven_dof_(bdyty(i_bdyty))
   bdyty(i_bdyty)%thread = i_bdyty
   call set_nb_vdriv_( bdyty(i_bdyty), nb_v_drvdof )
   call set_nb_fdriv_( bdyty(i_bdyty), nb_f_drvdof )

   ! blmty part
   ! first allocate
   allocate(bdyty(i_bdyty)%blmty(nb_blmty))
   ! filling is done with set_blmty

   ! tacty part
   allocate(bdyty(i_bdyty)%tacty(nb_tacty)        )
   allocate(bdyty(i_bdyty)%bdyty2tacty(2,nb_tacty))
   do i_tacty = 1, nb_tacty
     bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data  => null()
     bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata => null()
     bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I1 = 0.d0
     bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I2 = 0.d0
     bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I3 = 0.d0
   end do
   bdyty(i_bdyty)%bdyty2tacty(:,:) = 0

   nb_RBDY3 = i_bdyty

 end subroutine

 subroutine set_one_tactor_RBDY3(i_bdyty, i_tacty, tacID, color, vol, inertia, frame, shift, idata, rdata)
   implicit none
   integer                     , intent(in) :: i_bdyty, i_tacty, tacID
   character(len=5)            , intent(in) :: color
   real(kind=8)                , intent(in) :: vol
   real(kind=8), dimension(3)  , intent(in) :: inertia, shift
   real(kind=8), dimension(3,3), intent(in) :: frame
   integer     , dimension(:)  , pointer    :: idata ! duplicated on our side
   real(kind=8), dimension(:)  , pointer    :: rdata ! duplicated on our side

   bdyty(i_bdyty)%tacty(i_tacty)%tacID              = tacID
   bdyty(i_bdyty)%tacty(i_tacty)%color              = color

   bdyty(i_bdyty)%tacty(i_tacty)%BDARY%volume       = vol
   bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I1           = inertia(1)
   bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I2           = inertia(2)
   bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I3           = inertia(3)
   bdyty(i_bdyty)%tacty(i_tacty)%BDARY%embededframe = frame(1:3,1:3)
   bdyty(i_bdyty)%tacty(i_tacty)%BDARY%shift        = shift(1:3)

   allocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata, source=idata)
   allocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data , source=rdata)

   select case( tacID )
   case( i_polyr )
     call set_BDARY_POLYR( bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data        , &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata       , &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%volume      , &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I1          , &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I2          , &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%I3          , &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%embededframe, &
                           bdyty(i_bdyty)%tacty(i_tacty)%BDARY%shift         )
   end select

 end subroutine set_one_tactor_RBDY3

 subroutine comp_blmty_(bdy)
   implicit none
   type(T_RBDY3), intent(inout) :: bdy
   !
   logical :: update_tacty_config
   integer :: i_tacty, i_node, j_node
   real(kind=8)                 :: vT, vi, avrd
   real(kind=8), dimension(3)   :: OG, d, inertia, IT
   real(kind=8), dimension(3,3) :: mat_IT, mat_aux, localframe
   !
   real(kind=8), parameter :: untiers = 1.d0/3.d0

   if( size(bdy%tacty) == 1 ) then
     i_tacty = 1
     vT = bdy%tacty(i_tacty)%BDARY%volume
     OG = bdy%tacty(i_tacty)%BDARY%shift(1:3)
     bdy%tacty(i_tacty)%BDARY%shift(1:3) = 0.d0
     localframe = bdy%tacty(i_tacty)%BDARY%embededframe(1:3,1:3)
     bdy%tacty(i_tacty)%BDARY%embededframe(1:3,1:3) = Id33(1:3,1:3)

     ! compute volume or reset average radius value
     if( bdy%blmty(1)%PLAIN%avr_radius /= 0.d0 ) then
       avrd = bdy%blmty(1)%PLAIN%avr_radius
       bdy%blmty(1)%PLAIN%volume = 4.d0 * untiers * PI_g * avrd**3
     else
       bdy%blmty(1)%PLAIN%volume = vT
       bdy%blmty(1)%PLAIN%avr_radius = (0.75d0*vT/PI_g)**(untiers)
     end if

     bdy%blmty(1)%PLAIN%I1 = bdy%tacty(i_tacty)%BDARY%I1
     bdy%blmty(1)%PLAIN%I2 = bdy%tacty(i_tacty)%BDARY%I2
     bdy%blmty(1)%PLAIN%I3 = bdy%tacty(i_tacty)%BDARY%I3

     update_tacty_config = .false.

   else

     OG = 0.d0 ! center of gravity
     vT = 0.d0 ! Total volume

     ! use tacty list to compute blmty
     do i_tacty = 1, size(bdy%tacty)
       vi = bdy%tacty(i_tacty)%BDARY%volume
       bdy%tacty(i_tacty)%BDARY%rdg = (0.75d0*PI_g*vi)**(untiers)

       OG = OG + (vi*bdy%tacty(i_tacty)%BDARY%shift)
       vT = vT + vi
     end do
     OG = OG/vT

     ! compute volume or reset average radius value
     if( bdy%blmty(1)%PLAIN%avr_radius /= 0.d0 ) then
       avrd = bdy%blmty(1)%PLAIN%avr_radius
       bdy%blmty(1)%PLAIN%volume = 4.d0 * untiers * PI_g * avrd**3
     else
       bdy%blmty(1)%PLAIN%volume = vT
       bdy%blmty(1)%PLAIN%avr_radius = (0.75d0*vT/PI_g)**(untiers)
     end if

     do i_tacty = 1, size(bdy%tacty)
       bdy%tacty(i_tacty)%BDARY%shift = bdy%tacty(i_tacty)%BDARY%shift - OG
     end do

     ! if needed, compute inertia
     if( bdy%blmty(1)%PLAIN%I1 == 0.d0 .and. &
         bdy%blmty(1)%PLAIN%I2 == 0.d0 .and. &
         bdy%blmty(1)%PLAIN%I3 == 0.d0 ) then

       mat_IT = 0.d0
       do i_tacty = 1, size(bdy%tacty)
         inertia = (/ bdy%tacty(i_tacty)%BDARY%I1, &
                      bdy%tacty(i_tacty)%BDARY%I2, &
                      bdy%tacty(i_tacty)%BDARY%I3  /)
         mat_aux(1:3,1:3) = transpose33(bdy%tacty(i_tacty)%BDARY%embededframe)
         do i_node = 1, 3
           mat_aux(i_node,1:3) = inertia(i_node) * mat_aux(i_node,1:3)
         end do
         mat_IT(1:3,1:3) = mat_IT(1:3,1:3) + matmul( bdy%tacty(i_tacty)%BDARY%embededframe, mat_aux )
         do i_node = 1, 3
           d(1:3)    = bdy%tacty(i_tacty)%BDARY%shift(1:3)
           d(i_node) = 0.d0
           mat_IT(i_node,i_node) = mat_IT(i_node,i_node) + ( bdy%tacty(i_tacty)%BDARY%volume * dot_product(d,d) )
         end do
         d = bdy%tacty(i_tacty)%BDARY%shift
         do i_node = 1, 3
           do j_node = 1, 3
             if( j_node == i_node )  cycle
             vi = bdy%tacty(i_tacty)%BDARY%volume
             mat_IT(i_node,j_node) = mat_IT(i_node,j_node) - vi*d(i_node)*d(j_node)
           end do
         end do

       end do

       call diagonalise33(mat_IT,IT,localframe)

       bdy%blmty(1)%PLAIN%I1 = IT(1)
       bdy%blmty(1)%PLAIN%I2 = IT(2)
       bdy%blmty(1)%PLAIN%I3 = IT(3)

       update_tacty_config = .true.

     else

       localframe(1:3,1:3) = Id33(1:3,1:3)
       update_tacty_config = .false.

     end if

   end if ! size(tacty) == 1

   bdy%cooref(1:3) = bdy%cooref(1:3) + OG(1:3)
   bdy%coorTT(1:3) = bdy%cooref(1:3)

   bdy%LocalFrameRef(1:3,1:3) = localframe(1:3,1:3)
   bdy%LocalFrameIni(1:3,1:3) = localframe(1:3,1:3)
   bdy%LocalFrameTT(1:3,1:3)  = localframe(1:3,1:3)
   bdy%LocalFrame(1:3,1:3)    = localframe(1:3,1:3)

   if( update_tacty_config ) call update_tacty_frame_(bdy, localframe)

 end subroutine

 subroutine update_tacty_frame_(bdy, localframe)
   implicit none
   type(T_RBDY3), intent(inout) :: bdy
   real(kind=8), dimension(3,3), intent(in) :: localframe
   !
   integer :: i_tacty, k
   real(kind=8), dimension(3) :: v_aux

   do i_tacty = 1, size(bdy%tacty)

     ! on passe le shift du repere global au repere d'inertie
     v_aux(1:3) = matmul( transpose(localframe), bdy%tacty(i_tacty)%BDARY%shift )
     bdy%tacty(i_tacty)%BDARY%shift = v_aux

     ! pour les polyr on exprime les vertex ref dans le repere d'inertie du corps
     select case ( bdy%tacty(i_tacty)%tacID )
     case (i_polyr, i_polyf, i_polyo)

       do k = 1, bdy%tacty(i_tacty)%BDARY%idata(1)

         !rm: from tacty frame to global frame
         v_aux(1:3) = matmul( bdy%tacty(i_tacty)%BDARY%embededframe(1:3,1:3), &
                              bdy%tacty(i_tacty)%BDARY%data(3*k-2:3*k)        )


         !fd on passe du global au inertie
         bdy%tacty(i_tacty)%BDARY%data(3*k-2:3*k) = matmul(localframe(1:3,1:3),v_aux(1:3))

       end do

       ! on remet le embeded frame a identity
       bdy%tacty(i_tacty)%BDARY%embededframe = Id33

     case(i_planx)

       !* mapping of the PLANx embeded frame in the local frame

       bdy%tacty(i_tacty)%BDARY%embededframe = matmul( transpose(localframe), &
                                                       bdy%tacty(i_tacty)%BDARY%embededframe )

     !case(i_cylnd)

     !   bdy%tacty(itacty)%BDARY%embededframe = matmul(transpose33(localframe), &
     !                                                 bdy%tacty(i_tacty)%BDARY%embededframe)

     end select

   end do

 end subroutine update_tacty_frame_

 !> \brief Set the bulk of body
 subroutine set_blmty_RBDY3(i_bdyty, behav, avrd, inertia, frame)
   implicit none
   integer                     , intent(in) :: i_bdyty
   character(len=5)            , intent(in) :: behav
   real(kind=8)                , intent(in) :: avrd
   real(kind=8), dimension(3)  , intent(in) :: inertia
   real(kind=8), dimension(3,3), intent(in) :: frame
   !                         1234567890123456
   character(len=16) :: IAM='RBYD3::set_blmty'

   ! paranoid ?
   if( .not. associated(bdyty) ) then
     call faterr(IAM,'body container not allocated')
   end if
   if( i_bdyty < 1 .or. i_bdyty > nb_RBDY3 ) then
     call faterr(IAM,'wrong index of bdyty')
   end if

   ! avr_radius, I1, I2, I3 and volume
   bdyty(i_bdyty)%blmty(1)%blmID            = 'PLAIN'
   bdyty(i_bdyty)%blmty(1)%behav            = behav
   bdyty(i_bdyty)%blmty(1)%PLAIN%avr_radius = avrd

   bdyty(i_bdyty)%blmty(1)%PLAIN%I1 = inertia(1)
   bdyty(i_bdyty)%blmty(1)%PLAIN%I2 = inertia(2)
   bdyty(i_bdyty)%blmty(1)%PLAIN%I3 = inertia(3)

   bdyty(i_bdyty)%blmty(1)%PLAIN%volume = 0.d0

   call comp_blmty_(bdyty(i_bdyty))
   !bdyty(i_bdyty)%LocalFrameRef(1:3,1:3) = frame(1:3,1:3)
   !bdyty(i_bdyty)%LocalFrameIni(1:3,1:3) = frame(1:3,1:3)
   !bdyty(i_bdyty)%LocalFrameTT(1:3,1:3)  = frame(1:3,1:3)
   !bdyty(i_bdyty)%LocalFrame(1:3,1:3)    = frame(1:3,1:3)


 end subroutine

  subroutine add_predef_driven_dof_RBDY3(i_bdyty, vlocy, i_dof, values)
    implicit none
    integer                   , intent(in) :: i_bdyty
    logical                   , intent(in) :: vlocy
    integer                   , intent(in) :: i_dof
    real(kind=8), dimension(6), intent(in) :: values
    !
    integer :: i_driv

    ! internal increment of number of driven dof

    if( vlocy ) then
      i_driv = bdyty(i_bdyty)%nb_vlocy_driven_dof + 1
      call set_a_driven_dof(bdyty(i_bdyty)%vlocy_driven_dof(i_driv), i_bdyty, i_no6xx, 1, i_dof, values)
      bdyty(i_bdyty)%nb_vlocy_driven_dof = i_driv
    else
      i_driv = bdyty(i_bdyty)%nb_force_driven_dof + 1
      call set_a_driven_dof(bdyty(i_bdyty)%force_driven_dof(i_driv), i_bdyty, i_no6xx, 1, i_dof, values)
      bdyty(i_bdyty)%nb_force_driven_dof = i_driv
    end if

  end subroutine

  subroutine add_evol_driven_dof_RBDY3(i_bdyty, vlocy, i_dof, values)
    implicit none
    integer                     , intent(in) :: i_bdyty
    logical                     , intent(in) :: vlocy
    integer                     , intent(in) :: i_dof
    real(kind=8), dimension(:,:), pointer    :: values
    !
    integer :: i_driv

    ! internal increment of number of driven dof

    if( vlocy ) then
      i_driv = bdyty(i_bdyty)%nb_vlocy_driven_dof + 1
      call set_a_driven_dof(bdyty(i_bdyty)%vlocy_driven_dof(i_driv), i_bdyty, i_no6xx, 1, i_dof, values)
      bdyty(i_bdyty)%nb_vlocy_driven_dof = i_driv
    else
      i_driv = bdyty(i_bdyty)%nb_force_driven_dof + 1
      call set_a_driven_dof(bdyty(i_bdyty)%force_driven_dof(i_driv), i_bdyty, i_no6xx, 1, i_dof, values)
      bdyty(i_bdyty)%nb_force_driven_dof = i_driv
    end if

  end subroutine

  !> \brief Get the number of degrees of freedom
  FUNCTION get_idof_RBDY3()
    IMPLICIT NONE
    INTEGER(kind=4) :: get_idof_RBDY3 !< [return] number of degrees of freedom for a RBDY3

    get_idof_RBDY3 = 6

  END FUNCTION

  !> \brief Get field map of a body
  SUBROUTINE get_ccfield_RBDY3(ccfield)
    IMPLICIT NONE
    INTEGER(kind=4), DIMENSION(:), POINTER :: ccfield !< [inout] field indices map for a RBDY3 (null)

    ccfield => NULL()
  END SUBROUTINE

  !> \brief Compute mass matrix of a RBDY3
  subroutine comp_mass_one_body_RBDY3(M_elem, i_behav, rigid_data)
    implicit none
    real(kind=8), dimension(6), intent(inout) :: M_elem     !< [in,out] elementary matrix in which to store the mass
    integer(kind=4) ,           intent(in)    :: i_behav    !< [in] behaviour index
    real(kind=8), dimension(5), intent(in)    :: rigid_data !< [in] average and gyration radii
    !
    integer(kind=4) :: iblmty,ibehav,iccdof,ivd
    real(kind=8)    :: Umass,mass1,mass2,mass3,vol,I1,I2,I3
    
    Umass = get_rho(i_behav)

    I1  = rigid_data(2)
    I2  = rigid_data(3)
    I3  = rigid_data(4)

    vol = rigid_data(5)

    M_elem(1) = Umass*vol
    M_elem(2) = M_elem(1)
    M_elem(3) = M_elem(1)
       
    M_elem(4) = I1*Umass
    M_elem(5) = I2*Umass
    M_elem(6) = I3*Umass
    
    !if( bdyty(ibdyty)%mass(iccdof)<1.D-20) THEN
    !   PRINT*,'MASS',bdyty(ibdyty)%mass(iccdof),'iccdof',iccdof
    !   PRINT*,'RBDY3',ibdyty
    !END IF
       
  END SUBROUTINE comp_mass_one_body_RBDY3
!!!------------------------------------------------------------------------ 

  !> \brief Compute the external forces on a RBDY3
  subroutine comp_Fext_one_body_RBDY3(mass, fext)
    implicit none
    real(kind=8), dimension(:), intent(in)  :: mass  !< [in] elementary mass matrix
    real(kind=8), dimension(:), intent(out) :: fext  !< [out] the external forces
    !
    integer(kind=4) :: i_dof
    real(kind=8), dimension(6) :: grav

    i_dof = get_idof_RBDY3()
    CALL paranoid_check_r8_size('RBDY3::comp_Fext_one_body', fext, i_dof)

    grav(1)   = grav1
    grav(2)   = grav2
    grav(3)   = grav3
    grav(4:6) = 0.D0

    fext(1:i_dof) = mass(1:i_dof) * grav(1:i_dof)

  END SUBROUTINE comp_Fext_one_body_RBDY3

  !> \brief Compute the internal forces of a RBDY3
  SUBROUTINE comp_Bulk_one_body_RBDY3(fint)
    IMPLICIT NONE
    REAL(kind=8), DIMENSION(:), POINTER :: fint !< [in] state array of the internal forces

    fint = 0.d0

  END SUBROUTINE comp_Bulk_one_body_RBDY3

  !> \brief Add mass matrix of a body in a G_matrix
  SUBROUTINE add_mass_to_matrix_RBDY3(matrix, ibdyty, edof2gdof)
    IMPLICIT NONE
    INTEGER(kind=4), INTENT(in) :: ibdyty !< [in] index of the body in bdyty array
    INTEGER(kind=4), DIMENSION(:), INTENT(in) :: edof2gdof !< [in] map
    TYPE(G_matrix), INTENT(inout) :: matrix !< [inout] matrix
    !
    INTEGER(kind=4) :: i
    REAL(kind=8), DIMENSION(6,6) :: full_mass

    full_mass = 0.D0
    DO i = 1, 6
      full_mass(i,i) = bdyty(ibdyty)%mass(i)
    END DO
    CALL G_assemb(matrix, full_mass, edof2gdof)

  END SUBROUTINE

!!!------------------------------------------------------------------------

  SUBROUTINE set_bdyty2tacty_rbdy3(ibdyty,itacty,id,rank)
    IMPLICIT NONE 

    INTEGER :: ibdyty,itacty,id,rank
    bdyty(ibdyty)%bdyty2tacty(1:2,itacty) = (/ id, rank /) 

  END SUBROUTINE

  FUNCTION get_ptr_bdyty2tacty_rbdy3(ibdyty)
    IMPLICIT NONE
    INTEGER :: ibdyty
    INTEGER, DIMENSION(:,:), POINTER  :: get_ptr_bdyty2tacty_rbdy3
 
    get_ptr_bdyty2tacty_rbdy3 => bdyty(ibdyty)%bdyty2tacty

  END FUNCTION
!pb
  LOGICAL FUNCTION is_RBDY3_same_THREAD(ibdycd,ibdyan)

    IMPLICIT NONE
    INTEGER :: ibdycd,ibdyan
    
    is_RBDY3_same_THREAD = .FALSE.
    
    IF ( bdyty(ibdycd)%thread .EQ. bdyty(ibdyan)%thread ) is_RBDY3_same_THREAD = .TRUE.
    
  END FUNCTION is_RBDY3_same_THREAD

!!!-----------------------------------------

  INTEGER FUNCTION get_thread(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty

    get_thread = bdyty(ibdyty)%thread

  END FUNCTION get_thread

!!!-----------------------------------------
  subroutine set_invisible_small_objects(val)
    implicit none
    real(kind=8) :: val
    integer      :: ibdyty
    do ibdyty=1,nb_RBDY3
      if( bdyty(ibdyty)%blmty(1)%PLAIN%avr_radius < val ) bdyty(ibdyty)%visible = .FALSE. 
    enddo
  end subroutine 

!!!-----------------------------------------
  subroutine switch_vlocy_driven_dof( ibdyty, iccdof, ival )

    implicit none

    integer, intent( in ) :: ibdyty
    integer, intent( in ) :: iccdof
    integer, intent( in ) :: ival

    ! ****
    integer :: ivd
    logical :: found_dof_vlocy
                             !123456789012345678901234567890
    character(len=30) :: IAM='RBDY3::switch_vlocy_driven_dof'
    character(len=80) :: cout
    
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
      call logmes( cout, .TRUE. )
      call faterr( IAM, 'Error while trying to switch a velocy driven dof status' )
    end if
    write( cout, '("changing body ",I0," ddl ",I0)' ) ibdyty, iccdof
    call logmes( cout, .TRUE. )

  end subroutine

!!!-PTA-------------------------------------------------------------------- 
  SUBROUTINE renum_visible_RBDY3

    IMPLICIT NONE

    INTEGER           :: ibdyty, ibdyty_vis 

    renum_visible_bodies = .TRUE.
   
    ! maping pour la renumerotation
    ibdyty_vis = 0
    DO ibdyty=1,nb_RBDY3
       IF (bdyty(ibdyty)%visible) THEN
          ibdyty_vis = ibdyty_vis + 1
          bdyty(ibdyty)%visibleID = ibdyty_vis
       ELSE
          bdyty(ibdyty)%visibleID = 0
       END IF
    END DO 

  END SUBROUTINE renum_visible_RBDY3
!!!-PTA-------------------------------------------------------------------- 
!!!-PTA--------------------------------------------------------------------
  INTEGER FUNCTION get_visibleID(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty

    IF (renum_visible_bodies) THEN
       get_visibleID = bdyty(ibdyty)%visibleID
    ELSE
       get_visibleID = ibdyty
    END IF
    
  END FUNCTION get_visibleID
!!!-PTA--------------------------------------------------------------------

  subroutine clean_memory_RBDY3()
    implicit none
    integer(kind=4)   :: i_bdyty, i_tacty, i

    if( .not. associated(bdyty) ) return

    nb_RBDY3 = -1

    do i_bdyty = 1, size(bdyty)
     
      if( associated(bdyty(i_bdyty)%blmty) ) deallocate(bdyty(i_bdyty)%blmty)

      if( associated(bdyty(i_bdyty)%tacty) ) then
        do i_tacty = 1, size(bdyty(i_bdyty)%tacty)
          if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata) ) then
            deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%idata)
          end if
          if( associated(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data) ) then
            deallocate(bdyty(i_bdyty)%tacty(i_tacty)%BDARY%data)
          end if
        end do
        deallocate(bdyty(i_bdyty)%tacty)
      end if

      if( associated(bdyty(i_bdyty)%bdyty2tacty) ) deallocate(bdyty(i_bdyty)%bdyty2tacty)

      if( associated(bdyty(i_bdyty)%vlocy_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%vlocy_driven_dof)
          if( associated(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%x)
          end if

          if( associated(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%vlocy_driven_dof(i)%time_evolution%fx)
          end if
        end do
        deallocate(bdyty(i_bdyty)%vlocy_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%force_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%force_driven_dof)
          if( associated(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%x)
          end if

          if( associated(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%force_driven_dof(i)%time_evolution%fx)
          end if
        end do
        deallocate(bdyty(i_bdyty)%force_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%Vdriv) ) deallocate(bdyty(i_bdyty)%Vdriv)
      if( associated(bdyty(i_bdyty)%Xdriv) ) deallocate(bdyty(i_bdyty)%Xdriv)
      if( associated(bdyty(i_bdyty)%Fdriv) ) deallocate(bdyty(i_bdyty)%Fdriv)

    end do

    deallocate(bdyty)
    bdyty => NULL( )

    if( allocated(free_surface) ) deallocate(free_surface)

  end subroutine

  subroutine print_info_RBDY3(ibdyty)

    implicit none

    integer           :: ibdyty,iblmty=1
    character(len=80) :: cout

    write(cout,1) ibdyty
    call LOGMES(cout)

    write(cout,2) bdyty(ibdyty)%blmty(iblmty)%PLAIN%avr_radius,bdyty(ibdyty)%blmty(iblmty)%PLAIN%volume
    call LOGMES(cout)

    write(cout,3) bdyty(ibdyty)%blmty(iblmty)%PLAIN%I1,bdyty(ibdyty)%blmty(iblmty)%PLAIN%I2,bdyty(ibdyty)%blmty(iblmty)%PLAIN%I3
    call LOGMES(cout)
    
    write(cout,4) bdyty(ibdyty)%inv_mass(1),bdyty(ibdyty)%inv_mass(2),bdyty(ibdyty)%inv_mass(3)
    call LOGMES(cout)
    
    write(cout,5) bdyty(ibdyty)%inv_mass(4),bdyty(ibdyty)%inv_mass(5),bdyty(ibdyty)%inv_mass(6)
    call LOGMES(cout)

1   format(1X,'RBDY3:',1x,I5)
2   format(1X,'avrd :',1x,D14.7,1X,'vol  :',1x,D14.7)
3   format(1X,'I1   :',1x,D14.7,1X,'I2   :',1x,D14.7,1X,'I3   :',1x,D14.7)
4   format(1X,'1/m1 :',1x,D14.7,1X,'1/m2 :',1x,D14.7,1X,'1/m3 :',1x,D14.7)
5   format(1X,'1/I1 :',1x,D14.7,1X,'1/I2 :',1x,D14.7,1X,'1/I3 :',1x,D14.7)    

  end subroutine print_info_RBDY3

!!!------------------------------------------------------------------------
  subroutine set_data_equilibrium_RBDY3(checktype,tol)

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
       call faterr('RBDY3::seT_data_equilibrium',' @ WARNING: unknown checktype: '//checktype)
    end select

  end subroutine set_data_equilibrium_RBDY3
!!!------------------------------------------------------------------------
  subroutine check_equilibrium_state_RBDY3(info)

    implicit none 

    integer           :: ibdyty
    real(kind=8)      :: norm,Qnorm,Mnorm
    logical           :: info
    character(len=80) :: cout

    Qnorm = 0.D0
    Mnorm =-1.D20

    do ibdyty=1,nb_RBDY3
       if(.not.bdyty(ibdyty)%visible) cycle
       norm = dsqrt(&
            bdyty(ibdyty)%Vbegin(1)*bdyty(ibdyty)%Vbegin(1) + &
            bdyty(ibdyty)%Vbegin(2)*bdyty(ibdyty)%Vbegin(2) + &
            bdyty(ibdyty)%Vbegin(3)*bdyty(ibdyty)%Vbegin(3) + &
            bdyty(ibdyty)%Vbegin(4)*bdyty(ibdyty)%Vbegin(4) + &
            bdyty(ibdyty)%Vbegin(5)*bdyty(ibdyty)%Vbegin(5) + &
            bdyty(ibdyty)%Vbegin(6)*bdyty(ibdyty)%Vbegin(6))
       
       Qnorm = Qnorm + norm
       Mnorm= max (Mnorm,norm)                                
    end do

    Qnorm = Qnorm / real(nb_RBDY3,8)  
    
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

    
  end subroutine check_equilibrium_state_RBDY3

!------------------------------------------------------------------------
 SUBROUTINE get_dofstatus_RBDY3(ibdyty,val)
   IMPLICIT NONE

   ! variables d'entree
   INTEGER, INTENT(in) :: ibdyty  ! numero de corps
   ! variable de sortie
   ! vecteur contenant le status du noeud
   integer(kind=4), INTENT(out) :: val 
      
   integer(kind=4)   :: ivd,ifd,idof
   
                            !123456789012345678901
   CHARACTER(len=20) :: IAM='RBDY3::get_dofstatus'

   val = 0
   
   IF (nb_RBDY3 == 0) RETURN

   DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof

     idof = dofnb_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd))

     ! en x 1, y 10, z 100
     
     val = val + (10**(idof-1))  

   enddo

   DO ifd=1,bdyty(ibdyty)%nb_force_driven_dof

     idof = dofnb_of_a_driven_dof(bdyty(ibdyty)%force_driven_dof(ifd))

     ! en x 2, y 20, z 200
     val = val + (2*(10**(idof-1)))  

  ENDDO

 END SUBROUTINE    
  
END MODULE RBDY3
