# 1 "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/Core/contactor_3D/mod_POLYR.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/Core/contactor_3D/mod_POLYR.f90"
! fd test
! je fais apparaitre la fonction is_a_POLYD pour court circuiter le cablage en dur sur RBDY3
! dans PRPR mais c'est a reprendre en entier ... 
! 

! 25/11/12 pour le moment ce module ne gere pas la visibilite pour les autres ! 


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
MODULE POLYR                                       

  !!****h* LMGC90.CORE/POLYR
  !! NAME
  !!  module POLYR
  !! AUTHORS
  !!  Saussine, Dubois
  !! FUNCTION
  !!  Modelize generic polyedra
  !!****

  USE algebra
  USE utilities
  USE overall
  USE DiscreteGeometry
  USE bulk_behaviour
  USE a_DOF
  USE parameters
  
  use RBDY3, get_coorTT_RBDY3          => get_coorTT         , &
             get_inertia_frameTT_RBDY3 => get_inertia_frameTT, &
             get_coor_RBDY3            => get_coor           , &
             get_inertia_frame_RBDY3   => get_inertia_frame  , &
             get_inertia_frameIni_RBDY3 => get_inertia_frameIni , &             
             get_X_RBDY3               => get_X              , &
             get_Xbegin_RBDY3          => get_Xbegin         , &
             get_V_RBDY3               => get_V              , &
             get_reac_RBDY3            => get_reac           , &
             get_behav_RBDY3           => get_behav          , &
             get_visible_RBDY3         => get_visible
!rm : 15/09/2016 : this should be better than the above
!                  but does not work with pgf90 v15.9-0
!  USE RBDY3,ONLY: &
!       get_nb_RBDY3, &
!       get_nb_tacty, &
!       get_tacid, &
!       get_color, &
!       get_data, get_data_sz, &
!       get_idata_sz,get_idata,&
!       get_coorTT_RBDY3 => get_coorTT, &
!       get_inertia_frameTT_RBDY3 => get_inertia_frameTT, &
!       get_vlocy, &
!       get_shiftTT, &
!       add_reac, &
!       comp_vlocy, &
!       nullify_reac,  &
!       nullify_vlocy,  &
!       get_entity_rbdy3, &
!       !fd todo virer ces fonctions implicites
!
!       get_coor_RBDY3 => get_coor, &
!       get_inertia_frame_RBDY3 => get_inertia_frame, &
!       get_X_RBDY3 => get_X, &
!       get_Xbegin_RBDY3 => get_Xbegin, &
!       get_V_RBDY3 => get_V, &
!       get_reac_RBDY3 => get_reac, &
!       get_behav_RBDY3 => get_behav, &
!       get_visible_RBDY3 => get_visible, &
!       get_inertia_tensor, &
!       set_bdyty2tacty_RBDY3, &
!       get_visibleID, & !pta
!       print_info_RBDY3

  USE MAILx
  USE mecaMAILx

  use mbs3d, only : get_nb_MBS3D              => get_nb             , &
                    get_nb_tacty_MBS3D        => get_nb_tacty       , &
                    get_tacID_MBS3D           => get_tacID          , &
                    get_color_MBS3D           => get_color          , &
                    get_ptr_idata_MBS3D       => get_ptr_idata      , &
                    get_ptr_rdata_MBS3D       => get_ptr_rdata      , &
                    get_coor_MBS3D            => get_coor           , &
                    get_coorTT_MBS3D          => get_coorTT         , &
                    get_inertia_frame_MBS3D   => get_inertia_frame  , &
                    get_inertia_frameTT_MBS3D => get_inertia_frameTT, &
                    get_entity_MBS3D          => get_entity         , &
                    add_reac_MBS3D            => add_reac           , &
                    comp_vlocy_MBS3D          => comp_vlocy         , &
                    nullify_vlocy_MBS3D       => nullify_vlocy      , &
                    nullify_reac_MBS3D        => nullify_reac       , &
                    get_vlocy_MBS3D           => get_vlocy          , &
                    get_shiftTT_MBS3D         => get_shiftTT

  IMPLICIT NONE

  PRIVATE


  TYPE, PUBLIC :: T_POLYR
    ! rank of the polyr in its array
    integer                                  :: id          
    !
    INTEGER                                  ::  nb_vertex
    ! vertex coordinates in TT configuration
    REAL(kind=8),DIMENSION(:,:),POINTER      ::  vertex => null()

    ! vertex coordinates in principal local frame 
    ! Rq: utilise pour les rigides pour calculer vertex
    REAL(kind=8),DIMENSION(:)  ,POINTER      ::  vertex_ref => null()

    INTEGER                                  ::  nb_faces
    INTEGER,DIMENSION(:,:),POINTER           ::  face => null()

    ! les normales TT exprimees dans le repere global
    REAL(kind=8),DIMENSION(:,:),POINTER      ::  normal => null()

    ! les normales exprimees dans le repere principal d'inertie
    !fd utilise pour les rigides pour calculer la normale
    REAL(kind=8),DIMENSION(:,:),POINTER      ::  normal_ref => null()

    ! surface de chaque element
    REAL(kind=8),DIMENSION(:),POINTER        ::  areas => null()

    !fd map entre la numerotation locale des sommets des faces et la numerotation sous jacente des noeuds du maillage 
    !fd necessaire lorsqu'on a un modele defo sous jacent
    integer,dimension(:),pointer             :: l2g => null()

    ! position du centre d'inertie du polyr (conf TT)
    REAL(kind=8),DIMENSION(3)                ::  center            
    ! distance max (centre,vertex)
    REAL(kind=8)                             ::  radius            
    ! plus petit rayon inscrit 
    REAL(kind=8)                             ::  inner_radius      
    ! espece de rayons de la face calcules avec barycentre des points composant la face
    REAL(kind=8)                             ::  min_radius_face,max_radius_face  
                                                                                  
    ! projection sur les axes globaux (AABB)
    REAL(kind=8),DIMENSION(3)                ::  minpos,maxpos     

    ! fonction support (une valeur pas face):
    !  * le vertex retenue comme etant le plus favorable 
    INTEGER(kind=8),DIMENSION(:),POINTER     ::  vsupport_face => null()
    !  * distance entre l'origine et le vertex le plus favorable       
    REAL(kind=8),DIMENSION(:),POINTER        ::  val_support => null()

     
    !fd mon merdier a moi pour la gestion de la dilatation. Par temperature on entend Treelle-Tref

    REAL(kind=8),DIMENSION(:),POINTER  :: T => null()     ! (T -Tref) aux vertex au debut du pas de temps
    REAL(kind=8),DIMENSION(:),POINTER  :: inc_T  => null()! increments de T aux vertex pendant le pas de temps courant
    REAL(kind=8)                       :: alpha  ! coefficient de dilatation thermique dans le POLYR
    REAL(kind=8)                       :: hLx,hLy,hLz ! les half length dans chaque direction
    LOGICAL                            :: with_thermal_dilatation=.FALSE.,is_T_constant = .TRUE.

    !!!-----------------------------------------------------------------------
    !!! Structure Half Edge
    !!!-----------------------------------------------------------------------

    type(T_HE_Hdl) :: HE_Hdl

    ! face topologique
    ! tableau de listes de faces qui composent le set
    type(G_i_list), dimension(:), pointer :: f2f_set  => null()
    ! tableau de listes de noeuds qui composent les elements du contour d'un set
    ! a chaque fois il y debut et fin
    type(G_i_list), dimension(:), pointer :: f2f_contour => null()
    ! tableau de listes des sommets qui composent le contour d'un set
    type(G_i_list), dimension(:), pointer :: f2f_sommetofcontour => null()
    ! listes de noeuds sommets (appartenant a plus de 2 contours)
    integer       , dimension(:), pointer :: f2f_sommet => null()
    ! listes de edge qui sont des couples de sommets
    integer       , dimension(:,:), pointer :: f2f_edge => null()
    ! tableau de statut d'un set (flat ou non)
    integer       , dimension(:), pointer :: f2f_status=> null()

 END TYPE T_POLYR
 
  TYPE(T_POLYR),DIMENSION(:),ALLOCATABLE :: S_POLYR

  ! ----------------------------------------------------------------------
  ! POLYR2bdyty(1,itac) : serial number of body RBDY2 to which is attached
  !                       the contactor POLYR numbered itac in the list of 
  !                       all contactors POLYR 
  ! POLYR2bdyty(2,itac) : serial number of contactor POLYR itac in the list
  !                       of contactors POLYR attached to a body (generally 1)
  ! POLYR2bdyty(3,itac) : kind of underlying model i_rbdy3:rigid, i_mailx:MAILx, i_mbs3:MBS3D
  ! ----------------------------------------------------------------------
  ! INTEGER,DIMENSION(:,:),ALLOCATABLE,TARGET :: POLYR2bdyty
  integer( kind = 4 ), dimension( : , : ), pointer :: POLYR2bdyty

  INTEGER      :: nb_POLYR,nb_MAILx
  REAL(kind=8) :: max_ray,min_ray

  !!INTEGER,DIMENSION(:),ALLOCATABLE,PUBLIC     :: face_cd,face_an
  INTEGER,DIMENSION(:),ALLOCATABLE,PUBLIC     :: big_POLYR
  INTEGER,PUBLIC                              :: nb_big_POLYR = 0
  real(kind=8)                                :: threshold_big_POLYR = 4.0
  logical                                     :: skiptopo_big_POLYR=.FALSE.

  REAL(kind=8) :: radius_variation=1.D0

  logical :: bavard=.FALSE.  

  logical :: assume_good_orientation=.false.,skip_HE=.false.

  integer,dimension(:),allocatable ::  to_flip

  ! angle maxi entre 2 elements successifs d une face(par defaut 5 deg max)
  real(kind=8) :: topo_angle=5.0
  ! angle qui defini si une surface est plane (par defaut 1 deg max)
  real(kind=8) :: flatness_angle=1.0

  integer(kind=4) :: nbsf=15
  integer(kind=4),dimension(:),  pointer :: nb_point_outlines_POLYR => null() 
  integer(kind=4),dimension(:),  pointer :: all_connectivities => null()
  real(kind=8),   dimension(:,:),pointer :: outlines_POLYR => null()
  real(kind=8),   dimension(:),  pointer :: scalarfields_POLYR => null()

  PUBLIC &
       read_bodies_POLYR, &
       MOVE_POLYR, &
       save_vertex_POLYR, &
       set_radius_correction_POLYR, &
       assume_good_orientation_POLYR, &
       skip_HE_POLYR, &
       flip_orientation_POLYR, & 
       get_mdl_POLYR, &
       is_a_POLYD, &
       get_wireframe_POLYR, &  
       set_threshold_big_POLYR, &
       set_nb_big_POLYR, &       
       set_big_POLYR, &
       skip_topo_big_POLYR
  PUBLIC &
       POLYR2bdyty, &
       get_POLYR, &
       get_nb_POLYR,&
       get_mean_radius_POLYR,&
       get_behav_POLYR, &
       get_coor_POLYR, &
       get_vlocy_POLYR, &
       S_POLYR, &
       nullify_reac_POLYR, &
       nullify_vlocy_POLYR, &
       get_inertia_frame_POLYR, &
       get_inertia_frameIni_POLYR, &       
       get_inertia_frameTT_POLYR, &
       comp_vlocy_POLYR,&
       add_reac_POLYR,&
       get_coorTT_POLYR, &
       get_color_POLYR, &
       get_max_radius_POLYR, &
       get_min_radius_POLYR, &
       get_radius_POLYR, &
       get_ENT_POLYR, &
       get_visible_POLYR,&
!       get_cooref_POLYR, &
       get_X_POLYR, &
       get_Vtherm_POLYR,&
       get_Vtherm_POLYR_ref,&
       get_Vtherm_POLYR_face,&
       get_Xtherm_POLYR,&
       get_T_POLYR, &
       is_POLYR_same_RBDY3, &
       get_shiftTT_POLYR, &
       get_centres_faces, &
       get_surfaces_faces, &
       set_topo_angle, set_flatness_angle, get_flatness_angle, &
       get_visibleID_POLYR, & !pta
       print_info_POLYR
        !,get_data

  PUBLIC get_vertex_POLYR      , & !<- rm added
         get_ptr_vertexTT_POLYR , &
         get_ptr_normalTT_POLYR , & 
         get_POLYR2BDYTY       , &
         get_ptr_POLYR2BDYTY   , &
         get_nb_point_outlines_POLYR,&
         init_outlines_POLYR        ,&
         get_nb_scalarfields_POLYR  ,&
         init_scalarfields_POLYR    ,&
         update_postdata_POLYR      ,&
         get_ptr_connectivity_POLYR ,&
         get_all_connectivities_POLYR, &
         get_ptr_vertex_ref_POLYR, &
         get_topo_data_POLYR, &
         loc2glob_frame_POLYR, & ! for f2f vtk visu...
         glob2loc_frame_POLYR

  public clean_memory_POLYR

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_POLYR

    IMPLICIT NONE
    INTEGER           :: ibdyty,itacty,errare,nb_RBDY3,nb_MBS3D
    CHARACTER(len=18) :: IAM='POLYR::read_bodies'

    INTEGER                                 :: i,id,i_vertex,i_face,j,k,f
    INTEGER                                 :: nb_face_rel_vertex
    INTEGER,DIMENSION(20)                   :: tmp_faces
    INTEGER                                 :: tmp_arete_nb
    INTEGER,DIMENSION(60)                   :: tmp_arete
    REAL(kind=8),DIMENSION(3)               :: normal,tmp_center,vv
    REAL(kind=8),DIMENSION(3,3)             :: triangle
    REAL(kind=8)                            :: surface,radius,tmp_radius,inner_radius,p1,p2,p3,dist,scal,norm
    INTEGER,DIMENSION(:),pointer            :: idata
    INTEGER                                 :: idata_sz,data_sz
    INTEGER                                 :: vertex_id,i_vertex1,i_vertex2,i_vertex3
    INTEGER                                 :: nb_max_vertex,average_nb_face,average_nb_vertex
    CHARACTER(len=120)                      :: cout 

    integer :: i4_vec(3),vd,vf

    !fd visu

    REAL(kind=8),dimension(:,:),allocatable :: v_coor,v_coor_ptc,coor
    REAL(kind=8),DIMENSION(3,3)             :: v_localframe
    REAL(kind=8),DIMENSION(3)               :: v_translation
    REAL(kind=8)                            :: rd,vol,p

    !fd f2f

    !integer,dimension(:),allocatable :: f2f_skip,f2f_belong
    !integer :: nb_f2f_set,fi,fj

! tol sur le defaut d'alignement de normales
! 8.1 deg    real(kind=8) :: tol=1.d-2
! 2.5 deg    real(kind=8) :: tol=1.d-3
! 0.8 deg    real(kind=8) :: tol=1.d-4

    real(kind=8) :: tol_topology,tol_flatness

    integer :: n1,n2
    real(kind=8),dimension(3)::v1,v2

    ! 
    integer :: nb_l2g
    integer,dimension(:,:),allocatable :: connec

    ! mettre a .TRUE. si on veut etre prevenu quand un objet n'est pas convexe.
    logical :: warning_convexity = .FALSE.
    logical :: is_polyo


    integer(kind=4) :: err_

    integer :: if,iv

    real(kind=8) :: tt, tf
    
    idata => null()

    tol_topology = cos(topo_angle * (PI_g/180.d0))
    tol_flatness = 1.d0 - cos(flatness_angle * (PI_g/180.d0))

    print*,'================'
    print*,'topo tol= ',tol_topology
    print*,'flatness tol= ',tol_flatness
    print*,'================'

    nb_POLYR = 0
    nb_RBDY3 = get_nb_RBDY3()

    ! first scan for computing vector size

    DO ibdyty=1,nb_RBDY3   
      DO itacty=1,get_nb_tacty(ibdyty)
        id = get_tacID(ibdyty,itacty)
        if ( id/=i_polyr .and. id/=i_polyf .and. id/=i_polyo ) cycle
        nb_POLYR = nb_POLYR + 1
        call set_bdyty2tacty_rbdy3(ibdyty,itacty,id,nb_POLYR)
      END DO
    END DO

    nb_MAILx=get_nb_MAILx()

    DO ibdyty=1,nb_MAILx   
       DO itacty=1,get_nb_tacty_MAILx(ibdyty)
          IF( get_tacID_MAILx(ibdyty,itacty) /= i_polyd) cycle
          nb_POLYR = nb_POLYR + 1
       END DO
    END DO
    
    nb_MBS3D =get_nb_MBS3D()

    do ibdyty = 1,nb_MBS3D   
       do itacty = 1, get_nb_tacty_MBS3D(ibdyty)
          id = get_tacID_MBS3D(ibdyty,itacty)
          if ( id/=i_polyr .and. id/=i_polyf .and. id/=i_polyo ) cycle
          nb_POLYR = nb_POLYR + 1
       end do
    end do
    
    WRITE(cout,'(A,A,A,1x,I0,1X,A)') '[',IAM,']:',nb_POLYR,'POLYR found'
    CALL LOGMES(cout)

    IF (nb_POLYR == 0) RETURN
  
    ! allocation of contactor -> body map

    ! IF (ALLOCATED(POLYR2bdyty)) DEALLOCATE(POLYR2bdyty)
    if ( associated( POLYR2bdyty ) ) deallocate( POLYR2bdyty )
    allocate(POLYR2bdyty(3,nb_POLYR),stat=errare)
    POLYR2bdyty = 0
     
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating POLYR2bdyty')
    END IF

    ! filling of contactor -> body map

    nb_POLYR = 0

    DO ibdyty=1,nb_RBDY3   
       DO itacty=1,get_nb_tacty(ibdyty)
          id = get_tacID(ibdyty,itacty)
          if ( id/=i_polyr .and. id/=i_polyf .and. id/=i_polyo ) cycle
          nb_POLYR=nb_POLYR+1
          POLYR2bdyty(1,nb_POLYR) = ibdyty
          POLYR2bdyty(2,nb_POLYR) = itacty
          POLYR2bdyty(3,nb_POLYR) = i_rbdy3
       END DO
    END DO
    
    DO ibdyty=1,nb_MAILx   
       DO itacty=1,get_nb_tacty_MAILx(ibdyty)
          IF( get_tacID_MAILx(ibdyty,itacty) /= i_polyd) cycle
          nb_POLYR = nb_POLYR + 1
          POLYR2bdyty(1,nb_POLYR) = ibdyty
          POLYR2bdyty(2,nb_POLYR) = itacty
          POLYR2bdyty(3,nb_POLYR) = i_mailx
       END DO
    END DO

    do ibdyty = 1, nb_MBS3D
       do itacty = 1, get_nb_tacty_MBS3D(ibdyty)
          id = get_tacID_MBS3D(ibdyty,itacty)
          if ( id/=i_polyr .and. id/=i_polyf .and. id/=i_polyo ) cycle
          nb_POLYR = nb_POLYR + 1
          POLYR2bdyty(1,nb_POLYR) = ibdyty
          POLYR2bdyty(2,nb_POLYR) = itacty
          POLYR2bdyty(3,nb_POLYR) = i_mbs3
       end do
    end do

    ! allocation of local contactor database

    ALLOCATE(S_POLYR(nb_POLYR))

    ! ... and some information

    nb_max_vertex     = 0
    average_nb_face   = 0
    average_nb_vertex = 0
    
    ! filling local contactor database 

    DO id=1,nb_POLYR

       !fd pour savoir qui je suis au fin fond d'un calcul
       S_POLYR(id)%id=id

       if (POLYR2bdyty(3,id) == i_rbdy3) then

         !print*,id,' is rigid'

         !fd@@@ idata(1):nb_vertex,idata(2):nb_face,idata(3:*):connectivite des faces

         idata_sz = get_idata_sz(POLYR2bdyty(1,id),POLYR2bdyty(2,id))
         ALLOCATE(idata(idata_sz),stat=errare)
         IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating idata')
         END IF
         CALL get_idata(POLYR2bdyty(1,id),POLYR2bdyty(2,id),idata)

         !fd nouveau: remise de toutes les normales dans le meme sens pour un objet
         ! on part d'un germe qui est la normale de la premiere facette

         allocate(connec(3,idata(2)))

         !print*,idata(2),2+3*idata(2),size(idata)
 
         connec = reshape(idata(3 : 2+3*idata(2)), (/3,idata(2)/))

         ! on oriente la face grace au premier triangle

         !print*,idata(1),idata(2)

         call set_orientation_surface_T3(idata(1),idata(2),connec,err_)
         if (err_ > 0) then
            write(cout,'("POLYR ",I0)') id
            call logmes(cout)
           call faterr(IAM,'unexpected problem with orientation') 
         endif   
         if (err_ < 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           call logmes('warning while building orientation') 
         endif   

         !do i_face=1,idata(2)
         !  print*,'avant: ',idata(2+3*i_face-2),idata(2+3*i_face-1),idata(2+3*i_face)
         !  print*,'apres: ',connec(1,i_face),connec(2,i_face),connec(3,i_face)
         !enddo

         idata(3 : 2+3*idata(2)) = pack(connec,mask=.TRUE.)

         deallocate(connec)

         nullify(S_POLYR(id)%l2g)

       else if (POLYR2bdyty(3,id) == i_mailx) then

         !print*,id,' is defo'

         !fd permet de recuperer la taille vraie sans le terme masque en 0
         CALL get_idata_sz_MAILx(POLYR2bdyty(1,id),POLYR2bdyty(2,id),idata_sz)
         allocate(idata(idata_sz),stat=errare)
         IF (errare /= 0) THEN
            print*,idata_sz
            CALL FATERR(IAM,'error allocating idata')
         END IF

         !fd recupere le tableau sans le terme masque en 0
         CALL get_idata_MAILx(POLYR2bdyty(1,id),POLYR2bdyty(2,id),idata(1:idata_sz))

         !print*,'On charge un POLYD',idata(1),idata(2)

         !fd pas sur que ce soit utile ici. Ce travail est fait cote EF !?

         !fd nouveau: remise de toutes les normales dans le meme sens pour un objet
         ! on part d'un germe qui est la normale de la premiere facette

         allocate(connec(3,idata(2)))

         !print*,idata(2),2+3*idata(2),size(idata)
 
         connec = reshape(idata(3 : 2+3*idata(2)), (/3,idata(2)/))

         ! on oriente la face grace au premier triangle

         !print*,id,idata(1),idata(2)

         call set_orientation_surface_T3(idata(1),idata(2),connec, err_)          
         if (err_ > 0) then
            write(cout,'("POLYR ",I0)') id
            call logmes(cout)
           call faterr(IAM,'unexpected problem with orientation') 
         endif   
         if (err_ < 0) then
            write(cout,'("POLYR ",I0)') id
            call logmes(cout)
           call logmes('warning while building orientation') 
         endif   

         !do i_face=1,idata(2)
         !  print*,'avant: ',idata(2+3*i_face-2),idata(2+3*i_face-1),idata(2+3*i_face)
         !  print*,'apres: ',connec(1,i_face),connec(2,i_face),connec(3,i_face)
         !enddo

         idata(3 : 2+3*idata(2)) = pack(connec,mask=.TRUE.)

         deallocate(connec)

         !fd construction de l2g la map entre les indexes des noeuds de la peau et les
         !fd indexes des noeuds du maillage

         allocate(S_POLYR(id)%l2g(idata(1)))
         S_POLYR(id)%l2g = 0
         nb_l2g = 0
         do f=1,idata(2)
           do i=1,3
             if (count(S_POLYR(id)%l2g == idata(2+(3*(f-1)+i))) /= 0 ) then
                j=minloc(array=S_POLYR(id)%l2g, &
                         mask=S_POLYR(id)%l2g(:)==idata(2+(3*(f-1)+i)), &
                         dim=1)
                idata(2+(3*(f-1)+i)) = j
             else 
               nb_l2g = nb_l2g + 1
               if (nb_l2g > idata(1) ) then
                 write(6,'(I0,1x,I0)') idata(1),nb_l2g
                 CALL FATERR(IAM,'error building l2g map')
               endif
               S_POLYR(id)%l2g(nb_l2g) = idata(2+(3*(f-1)+i))
               idata(2+(3*(f-1)+i)) = nb_l2g
             endif
           enddo
         enddo

         ! fd paranoiac prints
         !write(*,'(3(1x,I0))') idata(3:size(idata))
         !write(*,'(3(1x,I0))') S_POLYR(id)%l2g(idata(3:size(idata)))

       else if (POLYR2bdyty(3,id) == i_mbs3) then

         idata => get_ptr_idata_MBS3D(POLYR2bdyty(1,id),POLYR2bdyty(2,id))

         allocate(connec(3,idata(2)))

         !print*,idata(2),2+3*idata(2),size(idata)
 
         connec = reshape(idata(3 : 2+3*idata(2)), (/3,idata(2)/))

         ! on oriente la face grace au premier triangle

         !print*,idata(1),idata(2)

         call set_orientation_surface_T3(idata(1),idata(2),connec, err_)          
         if (err_ > 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           call faterr(IAM,'unexpected problem with orientation') 
         endif   
         if (err_ < 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           call logmes('warning while building orientation') 
         endif   

         !do i_face=1,idata(2)
         !  print*,'avant: ',idata(2+3*i_face-2),idata(2+3*i_face-1),idata(2+3*i_face)
         !  print*,'apres: ',connec(1,i_face),connec(2,i_face),connec(3,i_face)
         !enddo

         idata(3 : 2+3*idata(2)) = pack(connec,mask=.TRUE.)

         deallocate(connec)

         nullify(S_POLYR(id)%l2g)

       else
         write(6,'(A,1x,I0,1x,A,1x,I0)') 'POLYR',id,'model',POLYR2bdyty(3,id)
         call LOGMES('Error '//IAM//': Unsuported underlying model')
       endif

       S_POLYR(id)%nb_vertex = idata(1)
       S_POLYR(id)%nb_faces  = idata(2)

       nb_max_vertex     = MAX(nb_max_vertex,S_POLYR(id)%nb_vertex)
       average_nb_face   = average_nb_face   +  S_POLYR(id)%nb_faces
       average_nb_vertex = average_nb_vertex +  S_POLYR(id)%nb_vertex
    
       ALLOCATE(S_POLYR(id)%face(3,S_POLYR(id)%nb_faces))

       ALLOCATE(S_POLYR(id)%vsupport_face(S_POLYR(id)%nb_faces))
       S_POLYR(id)%vsupport_face=0

       ALLOCATE(S_POLYR(id)%val_support(S_POLYR(id)%nb_faces))
       S_POLYR(id)%val_support=0.d0

       ALLOCATE(S_POLYR(id)%vertex(3,S_POLYR(id)%nb_vertex))
       S_POLYR(id)%vertex=0.d0

       ALLOCATE(S_POLYR(id)%normal(3,S_POLYR(id)%nb_faces))
       S_POLYR(id)%normal     = 0.D0

       ALLOCATE(S_POLYR(id)%areas(S_POLYR(id)%nb_faces))
       S_POLYR(id)%areas     = 0.D0

       nullify(S_POLYR(id)%f2f_set,S_POLYR(id)%f2f_contour,S_POLYR(id)%f2f_sommet, &
               S_POLYR(id)%f2f_sommetofcontour,S_POLYR(id)%f2f_edge,S_POLYR(id)%f2f_status)


       if (POLYR2bdyty(3,id) == i_rbdy3 .or. POLYR2bdyty(3,id) == i_mbs3) then

         !fd pour les polyr supportes par des rigides on 
         ! cree des structures de donnees de reference qui par 
         ! simple rotation permettront de construire des structures de donnees 
         ! dans le repere globale

         if( polyr2bdyty(3,id) == i_rbdy3) then
           ALLOCATE(S_POLYR(id)%vertex_ref(nbdime*S_POLYR(id)%nb_vertex))
           CALL get_data(POLYR2bdyty(1,id),POLYR2bdyty(2,id),S_POLYR(id)%vertex_ref)
           is_polyo = get_tacID( polyr2bdyty(1,id), polyr2bdyty(2,id) ) == i_polyo
         else
           is_polyo = get_tacID_MBS3D( polyr2bdyty(1,id), polyr2bdyty(2,id) ) == i_polyo
           allocate(S_POLYR(id)%vertex_ref(nbdime*S_POLYR(id)%nb_vertex))
           S_POLYR(id)%vertex_ref(:) = get_ptr_rdata_MBS3D(POLYR2bdyty(1,id),POLYR2bdyty(2,id))
         end if

         S_POLYR(id)%vertex_ref = S_POLYR(id)%vertex_ref * radius_variation

         ALLOCATE(S_POLYR(id)%normal_ref(3,S_POLYR(id)%nb_faces)) 
         S_POLYR(id)%normal_ref = 0.D0

         S_POLYR(id)%min_radius_face = 1.D20 
         S_POLYR(id)%max_radius_face = 0.d0

         DO i_face=1,idata(2)
            vertex_id = idata(2+3*i_face-2)
            i_vertex1 = vertex_id

            triangle(1,1) = S_POLYR(id)%vertex_ref(3*vertex_id-2)
            triangle(2,1) = S_POLYR(id)%vertex_ref(3*vertex_id-1)
            triangle(3,1) = S_POLYR(id)%vertex_ref(3*vertex_id)

            vertex_id = idata(2+3*i_face-1)
            i_vertex2 = vertex_id

            triangle(1,2) = S_POLYR(id)%vertex_ref(3*vertex_id-2)
            triangle(2,2) = S_POLYR(id)%vertex_ref(3*vertex_id-1)
            triangle(3,2) = S_POLYR(id)%vertex_ref(3*vertex_id)

            vertex_id = idata(2+3*i_face)
            i_vertex3 = vertex_id
            triangle(1,3) = S_POLYR(id)%vertex_ref(3*vertex_id-2)
            triangle(2,3) = S_POLYR(id)%vertex_ref(3*vertex_id-1)
            triangle(3,3) = S_POLYR(id)%vertex_ref(3*vertex_id)

            err_ = compute_info_triangle(triangle,surface=surface,outer_radius=tmp_radius,normal=normal)
            
            if (err_ == 1)  then
              write(cout,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
              call logmes(cout,.true.)
              write(cout,*) "--> triangle degenerated: 3 merged vertices"
              call logmes(cout,.true.)
              write(cout,*) "    POLYR number    : ", id
              call logmes(cout,.true.)
              write(cout,*) "    Face number     : ", i_face
              call logmes(cout,.true.)
            else if (err_==2) then
              write(cout,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
              call logmes(cout,.true.)
              write(cout,*) "--> triangle degenerated: 3 aligned vertices"
              call logmes(cout,.true.)
              write(cout,*) "    POLYR number    : ", id
              call logmes(cout,.true.)
              write(cout,*) "    Face number     : ", i_face
              call logmes(cout,.true.)
            endif

            S_POLYR(id)%areas(i_face)  = surface
            
            S_POLYR(id)%min_radius_face=MIN(S_POLYR(id)%min_radius_face,tmp_radius)
            S_POLYR(id)%max_radius_face=MAX(S_POLYR(id)%max_radius_face,tmp_radius)

            if (assume_good_orientation .or. is_polyo) then

               if (flip(id)) then
                 idata(2+3*i_face-2)=i_vertex1       
                 idata(2+3*i_face-1)=i_vertex3       
                 idata(2+3*i_face  )=i_vertex2       

                 S_POLYR(id)%normal_ref(1:3,i_face)=-normal
               else
                 S_POLYR(id)%normal_ref(1:3,i_face)= normal
               endif
            else

              ! le centre du polyhedre est en 0,0,0 
              ! donc triangle(:,1) est le vecteur qui va du centre au sommet 1 de la face

              IF (DOT_PRODUCT(triangle(:,1),normal) > 0.D0) THEN  !normale exterieure             

                S_POLYR(id)%normal_ref(1:3,i_face)=normal

                !print*,'on garde'
               !print*,S_POLYR(id)%normal_ref(1:3,i_face)

              ELSE
                idata(2+3*i_face-2)=i_vertex1       
                idata(2+3*i_face-1)=i_vertex3       
                idata(2+3*i_face  )=i_vertex2       
        
                S_POLYR(id)%normal_ref(1:3,i_face)=-normal

                !print*,'on swap face',i_face,' corps ',id
                !print*,S_POLYR(id)%normal_ref(1:3,i_face)

              ENDIF
            endif


            S_POLYR(id)%face(1,i_face)=idata(2+3*i_face-2)
            S_POLYR(id)%face(2,i_face)=idata(2+3*i_face-1)
            S_POLYR(id)%face(3,i_face)=idata(2+3*i_face) 

            !print*,'objet: ',id,'face: ',i_face
            !print*,S_POLYR(id)%face(:,i_face)
            !print*,S_POLYR(id)%normal_ref(1:3,i_face)
            !print*,dot_product(S_POLYR(id)%normal_ref(:,i_face),tmp_center)

  
         ENDDO

         S_POLYR(id)%center=0.D0

         ! determination du rayon d'encombrement: norme de la distance d'un vertex au centre d'inertie
         ! qui est en 0 0 0 suite aux manipulations lors de la lecture

         radius= 0.d0
         DO k=1,S_POLYR(id)%nb_vertex
           p1=S_POLYR(id)%vertex_ref(3*k-2)*S_POLYR(id)%vertex_ref(3*k-2)
           p2=S_POLYR(id)%vertex_ref(3*k-1)*S_POLYR(id)%vertex_ref(3*k-1)
           p3=S_POLYR(id)%vertex_ref(3*k)*S_POLYR(id)%vertex_ref(3*k)
           radius=MAX(radius,dsqrt(p1+p2+p3))
         ENDDO

         IF (radius<1.D-8) THEN
           write(cout,'(2(A,1x,I0))') ' POLYR:',id,' RBDY3:',POLYR2bdyty(1,id)
           write(cout,'(A,1x,D14.7)') ' radius is :',radius
           call faterr(IAM,cout)
         ENDIF

         S_POLYR(id)%radius=radius
       
         !fd determination de vsupport qui represente la distance au centre d'inertie la plus grande 
         !fd dans la direction de la normale a une face. cette distance devrait etre la distance de
         !fd la face elle meme mais pour identifier les cas degeneres on teste tout.
         !fd ca permet aussi de calculer la plus petite distance qui nous donnera min_radius  

         radius=1.d+20

         DO k=1,S_POLYR(id)%nb_faces
            normal=S_POLYR(id)%normal_ref(1:3,k)
            dist=-1.D20
            DO j=1,S_POLYR(id)%nb_vertex
               scal=S_POLYR(id)%vertex_ref(3*j-2)*normal(1)+ &
                    S_POLYR(id)%vertex_ref(3*j-1)*normal(2)+ &
                    S_POLYR(id)%vertex_ref(3*j)*normal(3)
               IF (scal > dist) THEN
                  S_POLYR(id)%vsupport_face(k) = j
                  S_POLYR(id)%val_support(k)   = scal
                  dist=scal
               ENDIF
            ENDDO

            radius = min(radius,dist)

            !fd pas utile
          
            if (warning_convexity) then

              if (S_POLYR(id)%vsupport_face(k) /= S_POLYR(id)%face(1,k) .and. & 
                  S_POLYR(id)%vsupport_face(k) /= S_POLYR(id)%face(2,k) .and. &
                  S_POLYR(id)%vsupport_face(k) /= S_POLYR(id)%face(3,k)) then  


                write(cout,*) '----------------------------------------------------'
                call logmes(cout,.true.)
                write(cout,*) 'WARNING : corps ', id,'  non strictement convexe'
                call logmes(cout,.true.)
                write(cout,*) ' face ',k,' suspecte avec la valeur ',S_POLYR(id)%val_support(k)
                call logmes(cout,.true.)

                j=S_POLYR(id)%face(1,k)
                scal=S_POLYR(id)%vertex_ref(3*j-2)*normal(1)+ &
                     S_POLYR(id)%vertex_ref(3*j-1)*normal(2)+ &
                     S_POLYR(id)%vertex_ref(3*j)*normal(3)

                write(cout,*) ' valeur attendue ',scal
                call logmes(cout,.true.)
                write(cout,*) ' normale utilisee ',normal
                call logmes(cout,.true.)
                write(cout,*) '--'
                call logmes(cout,.true.)
                write(cout,*) ' info sur la face suspecte '
                call logmes(cout,.true.)
                write(cout,*) ' vertex de la face',S_POLYR(id)%face(:,k)
                call logmes(cout,.true.)
                write(cout,*) ' vertex concerne ',S_POLYR(id)%vsupport_face(k)
                call logmes(cout,.true.)
                write(cout,*) ' normale a la face',S_POLYR(id)%normal_ref(1:3,k)
                call logmes(cout,.true.)
                write(cout,*) '----------------------------------------------------'
                call logmes(cout,.true.)

                !fd dangereux          stop

              endif
            endif
         ENDDO
       
         !fd on conserve le plus petit rayon inscrit

         S_POLYR(id)%inner_radius=radius

       else if (POLYR2bdyty(3,id) == i_mailx) then

         !fd pour les polyr supportes par des deformables les structures de 
         ! donnees dans le repere global seront calculees a partir des noeuds support

         nullify(S_POLYR(id)%vertex_ref,S_POLYR(id)%normal_ref)

         radius=0.d0
         inner_radius=1.d20
         tmp_center = get_RcoorTT_mecaMAILx(POLYR2bdyty(1,id),POLYR2bdyty(2,id))

         do i=1,S_POLYR(id)%nb_vertex
           S_POLYR(id)%vertex(:,i)=get_coorbegin_nodty_mecaMAILx(POLYR2bdyty(1,id),S_POLYR(id)%l2g(i))
           !write(*,'(I0,1x,I0,3(1x,E12.5))'),i,(S_POLYR(id)%l2g(i),S_POLYR(id)%vertex(:,i)-tmp_center) 
           tmp_radius=dsqrt(dot_product(S_POLYR(id)%vertex(:,i)-tmp_center,S_POLYR(id)%vertex(:,i)-tmp_center))
           !write(*,'(E12.5)') tmp_radius
           radius=MAX(radius,tmp_radius)
           inner_radius=min(inner_radius,tmp_radius)
         enddo

         S_POLYR(id)%radius=radius
         S_POLYR(id)%inner_radius=inner_radius

         S_POLYR(id)%min_radius_face = 1.D20 
         S_POLYR(id)%max_radius_face = 0.d0

         do i=1,S_POLYR(id)%nb_faces

            S_POLYR(id)%face(1,i)=idata(2+3*i-2)
            S_POLYR(id)%face(2,i)=idata(2+3*i-1)
            S_POLYR(id)%face(3,i)=idata(2+3*i) 

            triangle(:,1) = S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,i))
            triangle(:,2) = S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,i))
            triangle(:,3) = S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,i))

            err_ = compute_info_triangle(triangle,surface=surface, outer_radius=tmp_radius, normal=normal)

            if (err_ == 1) then
              write(cout,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
              call logmes(cout,.true.)
              write(cout,*) "--> triangle degenerated: 3 merged vertices"
              call logmes(cout,.true.)
              write(cout,*) "    POLYR number    : ", id,', of MAILx:',POLYR2bdyty(1,id),', local polyr id:',POLYR2bdyty(2,id)
              call logmes(cout,.true.)
              write(cout,*) "    Face number     : ", i
              call logmes(cout,.true.)
            else if (err_==2) then
              write(cout,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
              call logmes(cout,.true.)
              write(cout,*) "--> triangle degenerated: 3 aligned vertices"
              call logmes(cout,.true.)
              write(cout,*) "    POLYR number    : ", id,', of MAILx:',POLYR2bdyty(1,id),', local polyr id:',POLYR2bdyty(2,id)
              call logmes(cout,.true.)
              write(cout,*) "    Face number     : ", i
              call logmes(cout,.true.)
            endif

            S_POLYR(id)%areas(i) = surface

            S_POLYR(id)%min_radius_face=MIN(S_POLYR(id)%min_radius_face,tmp_radius)
            S_POLYR(id)%max_radius_face=MAX(S_POLYR(id)%max_radius_face,tmp_radius)

            S_POLYR(id)%normal(:,i) = normal

         enddo
       endif

       ! rm: should harmonize get_idata in all modules...
       if (POLYR2bdyty(3,id) == i_rbdy3 .or. POLYR2bdyty(3,id) == i_mailx) then
         deallocate(idata)
       end if
       nullify(idata)
       
       !fd @@@ for thermal part

       ALLOCATE(S_POLYR(id)%T(S_POLYR(id)%nb_vertex),S_POLYR(id)%inc_T(S_POLYR(id)%nb_vertex))
       
       S_POLYR(id)%T     =0.d0
       S_POLYR(id)%inc_T =0.d0
       S_POLYR(id)%alpha =0.d0
       S_POLYR(id)%hLx   =0.d0
       S_POLYR(id)%hLy   =0.d0
       S_POLYR(id)%hLz   =0.d0
       
    ENDDO
    
    min_ray =  1.D+20
    max_ray = -1.D+20
    tmp_radius=0.D0


    !print*,'calcul big_polyr' 
    
    DO id=1,nb_POLYR

       !print*, id,S_POLYR(id)%inner_radius,S_POLYR(id)%radius

       tmp_radius=tmp_radius+S_POLYR(id)%radius 
       !fd min_ray=MIN(min_ray,S_POLYR(id)%inner_radius)
       min_ray=MIN(min_ray,S_POLYR(id)%radius)       
       max_ray=MAX(max_ray,S_POLYR(id)%radius)
    ENDDO
    tmp_radius = tmp_radius/REAL(nb_POLYR,8)

    !print*,'rayon moyen : ',tmp_radius,' rayon limite : ',threshold_big_POLYR*tmp_radius
    
    if (.not. allocated(big_POLYR)) then
      !fd ca devient une variable de module nb_big_POLYR=0
      DO id=1,nb_POLYR
         IF (S_POLYR(id)%radius>threshold_big_POLYR*tmp_radius) nb_big_POLYR=nb_big_POLYR+1
      ENDDO
      IF (nb_big_POLYR > 0) THEN
        ALLOCATE(big_POLYR(nb_big_POLYR))
        nb_big_POLYR=0
        DO id=1,nb_POLYR
          IF (S_POLYR(id)%radius>threshold_big_POLYR*tmp_radius) THEN
             nb_big_POLYR=nb_big_POLYR+1
             big_POLYR(nb_big_POLYR)=id
          ENDIF
        ENDDO
      ENDIF
    ELSE
      call logmes('Big POLYR arbitrarly defined')
      write(cout,'("Number of big POLYR= ",I0)') nb_big_POLYR
      call logmes(cout)
      do id=1,nb_big_POLYR
         write(cout,'(I0," is a big POLYR")') big_POLYR(id)
         call logmes(cout)
         if (POLYR2bdyty(3,big_POLYR(id)) /= i_rbdy3) call faterr(IAM,'it must be a rigid object')
      enddo   

    ENDIF

    WRITE(*,*) ' !-------------------------------------!'
    WRITE(*,*) ' ! MAX RADIUS POLYR',max_ray
    WRITE(*,*) ' ! MIN RADIUS POLYR',min_ray
    WRITE(*,*) ' ! MEAN RADIUS POLYR',tmp_radius
    WRITE(*,*) ' ! NB BIG POLYR',nb_big_POLYR
    WRITE(*,*) ' ! Average vertex number',average_nb_vertex/REAL(nb_POLYR,8)
    WRITE(*,*) ' ! NB tot vertex',average_nb_vertex
    WRITE(*,*) ' ! Average face number',average_nb_face/REAL(nb_POLYR,8)
    WRITE(*,*) ' ! NB tot faces',average_nb_face
    WRITE(*,*) ' !-------------------------------------!'


    if (nb_big_POLYR == nb_POLYR) then
      ! call faterr(IAM,'nb_big_POLYR equal to nb_POLYR')
      call logmes('Special case: nb_big_POLYR equal to nb_POLYR')       
    endif   
    
    IF (nb_big_POLYR > 0) THEN
       min_ray = 1.D+20
       max_ray = -1.D+20
       tmp_radius=0.D0
       DO id=1,nb_POLYR
          k=0 
          DO i=1,nb_big_POLYR
             IF (big_POLYR(i)==id) THEN
                k=1
                EXIT
             ENDIF
          ENDDO
          IF (k==1) CYCLE
          tmp_radius=tmp_radius+S_POLYR(id)%radius 
          !fd min_ray=MIN(min_ray,S_POLYR(id)%inner_radius)
          min_ray=MIN(min_ray,S_POLYR(id)%radius)          
          max_ray=MAX(max_ray,S_POLYR(id)%radius)
       ENDDO
       tmp_radius = tmp_radius/REAL(nb_POLYR,8)
    ENDIF
!
    ! !fd on essaie de visualiser les polyr, leur centre, le centre d'inertie

    ! if (bavard) then

    !    DO id=1,nb_POLYR

    !      if (POLYR2bdyty(3,id) == i_rbdy3) then

    !        !fd attention le local frame est bidon car dof ini pas lu
    !        v_localframe = get_inertia_frame_POLYR(id)
    !        v_translation= get_coor_POLYR(id)

    !        !print*,'read solide',id
    !        !print*,v_translation  
    !        !print*,v_localframe(:,1)
    !        !print*,v_localframe(:,2)
    !        !print*,v_localframe(:,3)

    !        allocate (v_coor(S_POLYR(id)%nb_vertex,3),v_coor_ptc(5,3))

    !        rd = S_POLYR(id)%radius*0.1

    !        do i=1,S_POLYR(id)%nb_vertex

    !           v_coor(i,1) = S_POLYR(id)%vertex_ref(3*i-2)*v_localframe(1,1)  &
    !                       + S_POLYR(id)%vertex_ref(3*i-1)*v_localframe(1,2)  &
    !                       + S_POLYR(id)%vertex_ref(3*i  )*v_localframe(1,3)  &
    !                       + v_translation(1)

    !           v_coor(i,2) = S_POLYR(id)%vertex_ref(3*i-2)*v_localframe(2,1)  &
    !                       + S_POLYR(id)%vertex_ref(3*i-1)*v_localframe(2,2)  &
    !                       + S_POLYR(id)%vertex_ref(3*i  )*v_localframe(2,3)  &
    !                       + v_translation(2)

    !           v_coor(i,3) = S_POLYR(id)%vertex_ref(3*i-2)*v_localframe(3,1)  &
    !                       + S_POLYR(id)%vertex_ref(3*i-1)*v_localframe(3,2)  &
    !                       + S_POLYR(id)%vertex_ref(3*i  )*v_localframe(3,3)  &
    !                       + v_translation(3)

    !        enddo

    !        !do i=1,S_POLYR(id)%nb_faces
    !        !   s1(1:3) = v_coor(S_POLYR(id)%face(2,i),1:3) - v_coor(S_POLYR(id)%face(1,i),1:3)
    !        !   s2(1:3) = v_coor(S_POLYR(id)%face(3,i),1:3) - v_coor(S_POLYR(id)%face(1,i),1:3)
    !        !   norm = cross_product(s1,s2)
    !        !   print*,'read',i
    !        !   print*,norm/dsqrt(DOT_PRODUCT(norm,norm))
    !        !enddo

    !        v_coor_ptc(1,:)=v_translation(:)
    !        v_coor_ptc(2,:)=get_coor_POLYR(id)
    !        v_coor_ptc(3,:)=get_coor_POLYR(id) + (rd*v_localframe(:,1))
    !        v_coor_ptc(4,:)=get_coor_POLYR(id) + (rd*v_localframe(:,2))
    !        v_coor_ptc(5,:)=get_coor_POLYR(id) + (rd*v_localframe(:,3))

    !        call gmv_draw_onePOLYR(id,S_POLYR(id)%nb_vertex,v_coor,S_POLYR(id)%nb_faces,S_POLYR(id)%face,5,v_coor_ptc)

    !        deallocate (v_coor,v_coor_ptc)

    !      endif
    !    ENDDO

    ! end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !fd construction structure HE

    if (.not. skip_HE) then

      DO id=1,nb_POLYR

        !print*,'POLYR ',id

        !! on sait que 3 vertex par face, max_adj_face = 30 !!
        S_POLYR(id)%HE_Hdl = new_HE_Hdl(S_POLYR(id)%nb_vertex,S_POLYR(id)%nb_faces,30)
               
        do f=1,S_POLYR(id)%nb_faces
          i4_vec(:)=S_POLYR(id)%face(:,f)
          call settle_HE_Hdl(S_POLYR(id)%HE_Hdl,i4_vec,err_)
          if (err_ > 0) then
            cout=''
            write(cout,'("POLYR ",I0)') id 
            call logmes(cout)
            call faterr(IAM,'unexpected error in settle_HE_Hdl')
          endif   

       enddo
 
        call build_HE_Hdl(S_POLYR(id)%HE_Hdl,err_)

        if (err_ > 0) then
          write(cout,*) 'Error: impossible to create the HE structure'
          call logmes(cout, .true.)
          write(cout,*) 'Polyr : ',id,' corps: ',polyr2bdyty(1,id)
          call logmes(cout, .true.)
        endif

        if (associated(S_POLYR(id)%HE_Hdl%B2opHE)) then
          write(cout,'(A)')       'boundaries has been found when building the HE structure'
          call logmes(cout, .true.)
          write(cout,'(2(A,I0))') 'Polyr: ',id,' body: ',polyr2bdyty(1,id)
          call logmes(cout, .true.)
          write(cout,'(A,I0)')    'Number of boundaries: ',size(S_POLYR(id)%HE_Hdl%B2opHE)
          call logmes(IAM//cout, .true.)
        endif

      ENDDO

    else
      ! on cree un objet vide
      DO id=1,nb_POLYR
        S_POLYR(id)%HE_Hdl = new_HE_Hdl(0,0,0)
      ENDDO
    endif

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !fd construction du f2f_set

   DO id=1,nb_POLYR

      if (associated(S_POLYR(id)%f2f_set) .or. &
          associated(S_POLYR(id)%f2f_contour) .or. &
          associated(S_POLYR(id)%f2f_sommet) .or. &
          associated(S_POLYR(id)%f2f_sommetofcontour) .or. &
          associated(S_POLYR(id)%f2f_edge) .or. &
          associated(S_POLYR(id)%f2f_status)) then
        print*,'POLYR ',id
        call FATERR(IAM,'f2f values already associated')
      endif

      nullify(S_POLYR(id)%f2f_set)

      if (POLYR2bdyty(3,id) == i_rbdy3 .or. POLYR2bdyty(3,id) == i_mbs3) then

        !print*,'POLYR ',id


        tt = tol_topology
        tf = tol_flatness
        if (skiptopo_big_POLYR) then         
        if (nb_big_polyr /= 0) then  
           if (any(big_polyr == id) ) then
              tt = cos(PI_g)
              tf = 1. - cos(PI_g)
           endif   
        endif   
        endif
        
        call build_topology_surface_T3(S_POLYR(id)%nb_vertex,S_POLYR(id)%nb_faces, &
                                       S_POLYR(id)%face, &
                                       S_POLYR(id)%normal_ref, &
                                       tt, tf, &
                                       S_POLYR(id)%f2f_set, &
                                       S_POLYR(id)%f2f_contour, &
                                       S_POLYR(id)%f2f_sommet, &
                                       S_POLYR(id)%f2f_edge, &
                                       S_POLYR(id)%f2f_status, err_)         
         if (err_ > 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           !call faterr(IAM,'unexpected problem while building topology')
           call logmes('unexpected problem while building topology')            
         endif   
         if (err_ < 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           call logmes('warning while building topology')
           call logmes('  ')            
         endif   

        !print*,'POLYR: ',id,' nb sommet: ',size(S_POLYR(id)%f2f_sommet), &
        !                    ' nb edge: ',size(S_POLYR(id)%f2f_edge,dim=2), &
        !                    ' nb face: ',size(S_POLYR(id)%f2f_contour)
        !do i=1,size(S_POLYR(id)%f2f_sommet)
        !  print*,S_POLYR(id)%f2f_sommet(i)
        !enddo
        !do i=1,size(S_POLYR(id)%f2f_edge,dim=2)
        !  print*,S_POLYR(id)%f2f_edge(1:2,i)
        !enddo

      else if (POLYR2bdyty(3,id) == i_mailx) then
        call build_topology_surface_T3(S_POLYR(id)%nb_vertex,S_POLYR(id)%nb_faces, &
                                       S_POLYR(id)%face,S_POLYR(id)%normal, &
                                       tol_topology, tol_flatness, &
                                       S_POLYR(id)%f2f_set, &
                                       S_POLYR(id)%f2f_contour, &
                                       S_POLYR(id)%f2f_sommet, &
                                       S_POLYR(id)%f2f_edge, &
                                       S_POLYR(id)%f2f_status, err_)
         if (err_ > 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           call faterr(IAM,'unexpected problem with topology') 
         endif   

         if (err_ > 0) then
           write(cout,'("POLYR ",I0)') id
           call logmes(cout)
           call logmes('warning while building topology')
           call logmes('  ') 
         endif
         
      else
        call FATERR(IAM,'Impossible')
      endif

      !fd print*,'POLYR: ',id,' nb tological faces: ',size(S_POLYR(id)%f2f_set)
 
      !do f=1,size(S_POLYR(id)%f2f_set)
      ! print*,'elements face: ',f
      ! write(*,*) S_POLYR(id)%f2f_set(f)%G_i
      !enddo


      !do f=1,size(S_POLYR(id)%f2f_contour)
      ! print*,'contour face: ',f
      ! write(*,'(2(1x,I0))') S_POLYR(id)%f2f_contour(f)%G_i
      !enddo
 
      
      allocate(S_POLYR(id)%f2f_sommetofcontour(size(S_POLYR(id)%f2f_contour)))

      do f=1,size(S_POLYR(id)%f2f_contour)
        k=0 
        do i=1,size(S_POLYR(id)%f2f_contour(f)%G_i) 
          if ( modulo(i,2) == 0 ) cycle   ! on ne prend que les impaires
          if ( count(S_POLYR(id)%f2f_sommet == S_POLYR(id)%f2f_contour(f)%G_i(i)) /= 0 ) k = k + 1
        enddo 
        allocate( S_POLYR(id)%f2f_sommetofcontour(f)%G_i(k) ) 
        k=0
        do i=1,size(S_POLYR(id)%f2f_contour(f)%G_i)  
          if ( modulo(i,2) == 0 ) cycle   
          if ( count(S_POLYR(id)%f2f_sommet == S_POLYR(id)%f2f_contour(f)%G_i(i)) /= 0 ) then
            k = k + 1
            S_POLYR(id)%f2f_sommetofcontour(f)%G_i(k) = S_POLYR(id)%f2f_contour(f)%G_i(i)
          endif
        enddo 
      enddo
     


   enddo


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !fd construction du volume et flip/flap si c est negatif

   DO id=1,nb_POLYR

      ! skip all this if POLYO
      if (POLYR2bdyty(3,id) == i_rbdy3 ) then
        if( get_tacID( POLYR2bdyty(1,id), POLYR2bdyty(2,id) ) == i_polyo ) cycle
      end if

      if (POLYR2bdyty(3,id) == i_rbdy3 .or. POLYR2bdyty(3,id) == i_mbs3) then


        allocate(coor(3,S_POLYR(id)%nb_vertex))

        coor = reshape(S_POLYR(id)%vertex_ref, (/3,S_POLYR(id)%nb_vertex/))


        call compute_volume_surface_T3(S_POLYR(id)%nb_vertex,S_POLYR(id)%nb_faces,S_POLYR(id)%face, &
                                       coor,S_POLYR(id)%normal_ref,vol)         
 
        if (vol < 0.d0 .and. .not. assume_good_orientation) S_POLYR(id)%normal_ref= -S_POLYR(id)%normal_ref

        deallocate(coor) 

      else if (POLYR2bdyty(3,id) == i_mailx) then

        call compute_volume_surface_T3(S_POLYR(id)%nb_vertex,S_POLYR(id)%nb_faces,S_POLYR(id)%face, &
                                       S_POLYR(id)%vertex,S_POLYR(id)%normal,vol)         

        if (vol < 0.d0) S_POLYR(id)%normal= -S_POLYR(id)%normal

      else
        call FATERR(IAM,'Impossible')
      endif

      !fd print*,'POLYR: ',id,' volume ', vol

      if (vol < 0.d0 .and. .not. assume_good_orientation) then
        do if=1,S_POLYR(id)%nb_faces
          iv=S_POLYR(id)%face(2,if)          
          S_POLYR(id)%face(2,if)=S_POLYR(id)%face(3,if)                    
          S_POLYR(id)%face(3,if)=iv                     
        enddo
      endif

   enddo

   call move_polyr()

  END SUBROUTINE READ_BODIES_POLYR
!!!--------------------------------------------------------------------- 
  SUBROUTINE MOVE_POLYR

    IMPLICIT NONE

    INTEGER                                 :: id,k,i_vertex,ki
    REAL(kind=8),DIMENSION(3,3)             :: localframe
    REAL(kind=8),DIMENSION(3)               :: translation,vertex_ref,vertex,normal_ref,normal
    REAL(kind=8)                            :: U_alpha,U_beta,U_gamma,min1,min2,min3,max1,max2,max3
    CHARACTER(len=18)                       :: IAM='POLYR::MOVE_POLYR'
    REAL(kind=8),DIMENSION(3)               :: Xtherm=0.d0

    REAL(kind=8),DIMENSION(3)               :: s1,s2,norm
    integer :: i

    ! on conserve le pas auquel on a fait l'update
    integer :: last_step_updt=-1

    character(len=90)::cout
    integer :: err_
    
    !fd si on a deja appele cette fonction a ce pas on sort direct
    if (nstep == last_step_updt) return

    last_step_updt = nstep

    !print*,"MOVE POLYR"


    ! Chargement de l'orientation du repère principal d'inertie et du déplacement du centre de masse

    DO id=1,nb_POLYR

       localframe = get_inertia_frameTT_POLYR(id)

       translation= get_coorTT_POLYR(id)  

       !print*,'------------------'
       !print*,'move solid ',id
       !print*,translation
       !print*,localframe(:,1)
       !print*,localframe(:,2)
       !print*,localframe(:,3)

       S_POLYR(id)%center=translation

       if (POLYR2bdyty(3,id) == i_rbdy3 .or. polyr2bdyty(3,id) == i_mbs3) then

         ! Calcul des positions des vertex dans le repere absolu pour la configuration courante
         ! Les vertex de reference sont connus dans le repère principal d'inertie (alpha,beta,gamma)
         ! ces vecteurs etant eux meme exprimes dans le repère absolu. 
         ! La position des sommets dans le repère absolu est donc: s1_abs=(a*alpha+b*beta+c*gamma)+translation
       
         min1=1.D20;min2=1.D20;min3=1.D20
         max1=-1.D20;max2=-1.D20;max3=-1.D20

         DO k=1,S_POLYR(id)%nb_vertex
            vertex_ref(1:3)= S_POLYR(id)%vertex_ref(3*k-2:3*k)
 
            !print*,'vertex ',k
            !print*,vertex_ref(1:3)
         
            !fd @@@ prise en compte du deplacement des vertex du a la dilatation des briques
          
!            Xtherm = get_Xtherm_POLYR(id,vertex_ref(1:3))
            Xtherm =0.d0
          
            vertex_ref(1:3) = vertex_ref(1:3) + Xtherm(1:3)

            !fd @@@
          
            vertex(1)= vertex_ref(1)*localframe(1,1)  &
                     + vertex_ref(2)*localframe(1,2)  &
                     + vertex_ref(3)*localframe(1,3) + translation(1)
      
            vertex(2)= vertex_ref(1)*localframe(2,1)  &
                     + vertex_ref(2)*localframe(2,2)  &
                     + vertex_ref(3)*localframe(2,3) + translation(2)
 
            vertex(3)= vertex_ref(1)*localframe(3,1)  &
                     + vertex_ref(2)*localframe(3,2)  &
                     + vertex_ref(3)*localframe(3,3) + translation(3)

            S_POLYR(id)%vertex(1:3,k)= vertex(1:3)

            IF (vertex(1)>max1) max1=vertex(1)
            IF (vertex(1)<min1) min1=vertex(1)
            IF (vertex(2)>max2) max2=vertex(2)
            IF (vertex(2)<min2) min2=vertex(2)
            IF (vertex(3)>max3) max3=vertex(3)
            IF (vertex(3)<min3) min3=vertex(3)

         ENDDO

         S_POLYR(id)%minpos(1)=min1;S_POLYR(id)%minpos(2)=min2;S_POLYR(id)%minpos(3)=min3
         S_POLYR(id)%maxpos(1)=max1;S_POLYR(id)%maxpos(2)=max2;S_POLYR(id)%maxpos(3)=max3
       
         !do i=1,S_POLYR(id)%nb_faces
         !  s1(1:3) = S_POLYR(id)%vertex(1:3,S_POLYR(id)%face(2,i)) - S_POLYR(id)%vertex(1:3,S_POLYR(id)%face(1,i))
         !  s2(1:3) = S_POLYR(id)%vertex(1:3,S_POLYR(id)%face(3,i)) - S_POLYR(id)%vertex(1:3,S_POLYR(id)%face(1,i))
         !  norm = cross_product(s1,s2)
         !  print*,'move calcul',i
         !  print*,norm/dsqrt(DOT_PRODUCT(norm,norm))

         !  center(1:3) = (0.33333333*(S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,i)) + &
         !                            S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,i)) + &
         !                            S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,i)))) - translation

         !  print*,center
         !  print*,dot_product(center,norm/dsqrt(DOT_PRODUCT(norm,norm)))

         !enddo

         ! Rotation des normales

         !print*,' move tourne polyr'

         DO k=1,S_POLYR(id)%nb_faces

           !print*,'surface',k
           !print*, S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,k))
           !print*, S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,k))
           !print*, S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,k))


           !<fd 24/09/06 je ne comprends pas a quoi correspond ce vsupport_face !!!
           !        i_vertex=S_POLYR(id)%vsupport_face(k)
           !        print*,S_POLYR(id)%vsupport_face(k)

           i_vertex = S_POLYR(id)%face(1,k)

           !fd>

            normal_ref(1:3)=S_POLYR(id)%normal_ref(1:3,k)
  
            normal(1) = normal_ref(1)*localframe(1,1) &
                      + normal_ref(2)*localframe(1,2) &
                      + normal_ref(3)*localframe(1,3)

            normal(2) = normal_ref(1)*localframe(2,1) &
                      + normal_ref(2)*localframe(2,2) &
                      + normal_ref(3)*localframe(2,3)

            normal(3) = normal_ref(1)*localframe(3,1) &
                      + normal_ref(2)*localframe(3,2) &
                      + normal_ref(3)*localframe(3,3)

            S_POLYR(id)%normal(1:3,k)=normal(1:3)
            S_POLYR(id)%val_support(k)=  S_POLYR(id)%vertex(1,i_vertex)*normal(1) + &
                                         S_POLYR(id)%vertex(2,i_vertex)*normal(2) + & 
                                         S_POLYR(id)%vertex(3,i_vertex)*normal(3)

            !print*,S_POLYR(id)%normal(1:3,k)

         ENDDO

       else if (POLYR2bdyty(3,id) == i_mailx) then

         !print*,'objet: ',id

         do i=1,S_POLYR(id)%nb_vertex

           S_POLYR(id)%vertex(:,i)=get_coorTT_nodty_mecaMAILx(POLYR2bdyty(1,id),S_POLYR(id)%l2g(i))

          
         enddo

         S_POLYR(id)%minpos(1)=minval(S_POLYR(id)%vertex(1,:))
         S_POLYR(id)%minpos(2)=minval(S_POLYR(id)%vertex(2,:))
         S_POLYR(id)%minpos(3)=minval(S_POLYR(id)%vertex(3,:))
         S_POLYR(id)%maxpos(1)=maxval(S_POLYR(id)%vertex(1,:))
         S_POLYR(id)%maxpos(2)=maxval(S_POLYR(id)%vertex(2,:))
         S_POLYR(id)%maxpos(3)=maxval(S_POLYR(id)%vertex(3,:))

         do i=1,S_POLYR(id)%nb_faces

            normal = cross_product(S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,i)) - &
                                   S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,i)),  &
                                   S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,i)) - &
                                   S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,i)))

            norm = length3(normal)

            S_POLYR(id)%normal(:,i) = normal/norm

         enddo


       endif

       ! new 
       ! a voir pb TT 
       !print*,' update HE_hdl'

       if (.not. skip_HE) then
         call update_HE_Hdl(S_POLYR(id)%HE_Hdl,S_POLYR(id)%vertex,err_,S_POLYR(id)%normal)
         if (err_ > 0) then
           write(cout,'(I0)') id
           call logmes(cout) 
           call faterr('POLYR::MOVE','impossible to update HE') 
         endif
       endif

       !print*,'AABB'
       !print*,'Xmin ',S_POLYR(id)%minpos(1),'Xmax ',S_POLYR(id)%maxpos(1)
       !print*,'Ymin ',S_POLYR(id)%minpos(2),'Ymax ',S_POLYR(id)%maxpos(2)
       !print*,'Zmin ',S_POLYR(id)%minpos(3),'Zmax ',S_POLYR(id)%maxpos(3)

    ENDDO
    
  END SUBROUTINE MOVE_POLYR

  subroutine loc2glob_frame_POLYR(itact, points)
    implicit none
    integer, intent(in) :: itact
    real(kind=8), dimension(:,:), intent(inout) :: points
    !
    integer :: i_point, err
    real(kind=8), dimension(3)   :: center, new_point
    real(kind=8), dimension(3,3) :: lf

    lf     = get_inertia_frameTT_POLYR(itact)
    center = get_coorTT_POLYR(itact)

    do i_point = 1, size(points,2)
      new_point(1:3) = center + matmul(lf, points(:,i_point) )
      points(1:3,i_point) = new_point(1:3)
    end do

    ! should do something regarding dilatation ?

  end subroutine loc2glob_frame_POLYR

  subroutine glob2loc_frame_POLYR(itact, points)
    implicit none
    integer, intent(in) :: itact
    real(kind=8), dimension(:,:), intent(inout) :: points
    !
    integer :: i_point, err
    real(kind=8), dimension(3)   :: center, new_point
    real(kind=8), dimension(3,3) :: lf

    lf     = get_inertia_frameTT_POLYR(itact)
    center = get_coorTT_POLYR(itact)

    call inverse33(lf, err)
    if( err /= 0 ) then
      call faterr('POLYR:loc2blog_frame', 'non invertible rotation matrix !!!')
    end if

    do i_point = 1, size(points,2)
      new_point(1:3) = matmul(lf, points(:,i_point)-center )
      points(1:3,i_point) = new_point(1:3)
    end do

    ! should do something regarding dilatation ?

  end subroutine glob2loc_frame_POLYR
 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_POLYR(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome
  
    get_nb_POLYR = nb_POLYR
  
  END FUNCTION get_nb_POLYR
!!!------------------------------------------------------------------------   
  TYPE(T_POLYR) FUNCTION get_POLYR(itact)
    
    IMPLICIT NONE
    INTEGER :: itact
     
    get_POLYR = S_POLYR(itact)
  
  END FUNCTION get_POLYR
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_max_radius_POLYR(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome

    get_max_radius_POLYR = max_ray

  END FUNCTION get_max_radius_POLYR
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_min_radius_POLYR(fantome)

    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
     
    get_min_radius_POLYR = min_ray

  END FUNCTION get_min_radius_POLYR
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_radius_POLYR(itact)

    IMPLICIT NONE   
    INTEGER :: itact
 
    get_radius_POLYR = S_POLYR(itact)%radius
    
  END FUNCTION get_radius_POLYR
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_mean_radius_POLYR(fantome)
    
    IMPLICIT NONE   
    INTEGER               :: id
    REAL(kind=8),OPTIONAL :: fantome
    REAL(kind=8)          :: mean_radius

    mean_radius = 0.D0
    IF (nb_POLYR /= 0) THEN
       DO id=1,nb_POLYR
          mean_radius = mean_radius + S_POLYR(id)%radius
       END DO
       mean_radius = mean_radius/REAL(nb_POLYR,8)
    END IF

    get_mean_radius_POLYR = mean_radius
    
  END FUNCTION get_mean_radius_POLYR
!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_POLYR(itact)

    IMPLICIT NONE
    INTEGER :: itact 
   
    if (POLYR2bdyty(3,itact) == i_rbdy3) then

      get_color_POLYR = get_color(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))
   
    else if (POLYR2bdyty(3,itact) == i_mailx) then

     get_color_POLYR = get_color_mecaMAILx(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    else if (POLYR2bdyty(3,itact) == i_mbs3) then

     get_color_POLYR = get_color_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    endif

  END FUNCTION get_color_POLYR
!!!------------------------------------------------------------------------
  !fd virer cette merde
  
  character(len=5) function get_mdl_POLYR(itac)
     implicit none
     integer :: itac
     get_mdl_POLYR = get_body_model_name_from_id( polyr2bdyty(3,itac) )
  end function
!!!------------------------------------------------------------------------ 
  logical function is_a_POLYD(itac)
     implicit none
     integer :: itac
     select case(polyr2bdyty(3,itac)) 
     case(i_rbdy3,i_mbs3)
       is_a_POLYD = .FALSE.
     case(i_mailx)
       is_a_POLYD = .TRUE.
     case default
       call faterr('POLYR::is_a_POLYD','unknown model type')
     end select
  end function
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_visible_POLYR(itact)

    IMPLICIT NONE
    INTEGER :: itact 
 
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  

       get_visible_POLYR = get_visible_RBDY3(POLYR2bdyty(1,itact))
     
    else

       get_visible_POLYR = .true.
   
    endif

  END FUNCTION get_visible_POLYR

  character(len=5) function get_behav_POLYR(itact)
    implicit none
    integer(kind=4), intent(in)  :: itact

    get_behav_POLYR = 'nknow'

    if (POLYR2bdyty(3,itact) == i_rbdy3 ) then  
      get_behav_POLYR = get_behav_RBDY3(polyr2bdyty(1,itact))
    end if

  end function get_behav_POLYR
  
  function get_Xbegin_POLYR(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_Xbegin_POLYR

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_Xbegin_POLYR = get_Xbegin_RBDY3(POLYR2bdyty(1,itact))       
     
    else if (POLYR2bdyty(3,itact) == i_mailx .or. POLYR2bdyty(3,itact) == i_mbs3) then

      get_Xbegin_POLYR = 0.d0
   
    endif

  end function

  function get_X_POLYR(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_X_POLYR

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_X_POLYR = get_X_RBDY3(POLYR2bdyty(1,itact))       
     
    else if (POLYR2bdyty(3,itact) == i_mailx .or. POLYR2bdyty(3,itact) == i_mbs3) then

      get_X_POLYR = 0.d0
   
    endif

  end function

  function get_V_POLYR(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(6) :: get_V_POLYR

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_V_POLYR = get_V_RBDY3(POLYR2bdyty(1,itact))       
     
    else if (POLYR2bdyty(3,itact) == i_mailx .or. POLYR2bdyty(3,itact) == i_mbs3) then

      get_V_POLYR = 0.d0
   
    endif

  end function

  function get_reac_POLYR(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(6) :: get_reac_POLYR

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_reac_POLYR = get_reac_RBDY3(POLYR2bdyty(1,itact))       
     
    else if (POLYR2bdyty(3,itact) == i_mailx .or. POLYR2bdyty(3,itact) == i_mbs3) then

      get_reac_POLYR = 0.d0
   
    endif

  end function

  function get_coor_POLYR(itact)
    implicit none
    integer(kind=4), intent(in) :: itact
    real(kind=8), dimension(3)  :: get_coor_POLYR

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_coor_POLYR = get_coor_RBDY3(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_coor_POLYR = get_RcoorTT_mecaMAILx(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))
   
    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_coor_POLYR = get_coor_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    endif

  end function

  function get_inertia_frame_POLYR(itact)
    implicit none
    integer(kind=4)             :: itact
    real(kind=8),dimension(3,3) :: get_inertia_frame_POLYR
   
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_inertia_frame_POLYR = get_inertia_frame_RBDY3(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_inertia_frame_POLYR = get_Rinertia_frameTT_mecaMAILx(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_inertia_frame_POLYR = get_inertia_frame_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    endif
  end function get_inertia_frame_POLYR

!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------ 
  FUNCTION get_Vtherm_POLYR(itacty,coor)
    !
    ! routine destinee a calculer la vitesse
    ! d'un point de coordonnees actuelle coor
    ! due a l'increment de dilatation thermique
    !
    ! ceci utilise l'ancienne methode sans detection explicite des contacts
    ! avec cette approche on ne peut prendre que T constant
    !
    IMPLICIT NONE

    INTEGER                     :: itacty
    REAL(kind=8),DIMENSION(3)   :: coor,coor0,V0,get_Vtherm_POLYR
    REAL(kind=8),DIMENSION(3,3) :: localframe
    REAL(kind=8)                :: dilat,inc_dilat
    
    if (POLYR2bdyty(3,itacty) == i_mailx .or. POLYR2bdyty(3,itacty) == i_mbs3) then
       get_Vtherm_POLYR = 0.d0
       return
    endif

    dilat     = S_POLYR(itacty)%alpha*(SUM(    S_POLYR(itacty)%T(:))/S_POLYR(itacty)%nb_vertex)
    inc_dilat = S_POLYR(itacty)%alpha*(SUM(S_POLYR(itacty)%inc_T(:))/S_POLYR(itacty)%nb_vertex)
    
    !fd @@@ calcul de la coordonnee dans le repere d'inertie de reference
    !fd @@@ comme ((coor - coor0) / coor0) = dilat
    !fd @@@ alors  coor = coor0 (1 + dilat)
    !fd @@@ alors  coor0 = coor/(1 + dilat)
    
    coor0 = coor / (1.d0 + dilat)
    
    !fd @@@ calcul de la vitesse dans le repere d'inertie
    
    V0 = inc_dilat*coor0/H
    
    !fd @@@ passage de cette vitesse dans le repere global
    
    localframe = get_inertia_frameTT_POLYR(itacty)
    
    get_Vtherm_POLYR(1) = &
         V0(1)*localframe(1,1) + &
         V0(2)*localframe(1,2) + &
         V0(3)*localframe(1,3)
    
    get_Vtherm_POLYR(2) = &
         V0(1)*localframe(2,1) + &
         V0(2)*localframe(2,2) + &
         V0(3)*localframe(2,3)
    
    get_Vtherm_POLYR(3) = &
         V0(1)*localframe(3,1) + &
         V0(2)*localframe(3,2) + &
         V0(3)*localframe(3,3)
    
  END FUNCTION get_Vtherm_POLYR
!!!------------------------------------------------------------------------ 
  FUNCTION get_Vtherm_POLYR_ref(itacty,coor0)
    !
    ! routine destinee a calculer la vitesse
    ! d'un point de coordonnees de reference coor0
    ! due a l'increment de dilatation thermique
    !
    IMPLICIT NONE

    INTEGER      :: itacty
    REAL(kind=8),DIMENSION(3)   :: coor0,V0,get_Vtherm_POLYR_ref
    REAL(kind=8),DIMENSION(3,3) :: localframe
    REAL(kind=8)                :: inc_dilat
    
    REAL(kind=8)                :: iT1x,iT2x,iT1y,iT2y,iT1z,iT2z
    REAL(kind=8),DIMENSION(8)   :: temp
    

    if (POLYR2bdyty(3,itacty) == i_mailx .or. POLYR2bdyty(3,itacty) == i_mbs3) then
       get_Vtherm_POLYR_ref = 0.d0
       return
    endif

    IF (S_POLYR(itacty)%is_T_constant) THEN
       
       inc_dilat = S_POLYR(itacty)%alpha*(SUM(S_POLYR(itacty)%inc_T(:))/S_POLYR(itacty)%nb_vertex)
       
       !fd @@@ calcul de la vitesse dans le repere d'inertie
       
       V0 = inc_dilat*coor0/H
       
    ELSE
       
       !fd on procede direction par direction:
       !fd 1/ on calcule les increments de temperatures aux sommets du segment 
       !fd    de la direction donnee par interpolation
       !fd 2/ on calcule la vitesse
       
       
       iT1x=interpolate_H8(-1.d0,coor0(2),coor0(3),S_POLYR(itacty)%inc_T(1:8))
       iT2x=interpolate_H8( 1.d0,coor0(2),coor0(3),S_POLYR(itacty)%inc_T(1:8))
       
       V0(1) = S_POLYR(itacty)%hLx * S_POLYR(itacty)%alpha * 0.5 * &
            (iT1x*(coor0(1) - (coor0(1)*coor0(1)*0.5)) + iT2x*(coor0(1) + (coor0(1)*coor0(1)*0.5))) 
       
       iT1y=interpolate_H8(coor0(1),-1.d0,coor0(3),S_POLYR(itacty)%inc_T(1:8))
       iT2y=interpolate_H8(coor0(1), 1.d0,coor0(3),S_POLYR(itacty)%inc_T(1:8))
       
       V0(2) = S_POLYR(itacty)%hLy*S_POLYR(itacty)%alpha*0.5* &
            (iT1y*(coor0(2) - (coor0(2)*coor0(2)*0.5))+iT2y*(coor0(2) + (coor0(2)*coor0(2)*0.5))) 
       
       iT1z=interpolate_H8(coor0(1),coor0(2),-1.d0,S_POLYR(itacty)%inc_T(1:8))
       iT2z=interpolate_H8(coor0(1),coor0(2), 1.d0,S_POLYR(itacty)%inc_T(1:8))
       
       V0(3) = S_POLYR(itacty)%hLz*S_POLYR(itacty)%alpha*0.5* &
            (iT1z*(coor0(3) - (coor0(3)*coor0(3)*0.5))+iT2z*(coor0(3) + (coor0(3)*coor0(3)*0.5))) 
       
       V0 = V0/H
       
    ENDIF
    
    !fd @@@ passage de cette vitesse dans le repere global
    
    localframe = get_inertia_frameTT_POLYR(itacty)
    
    get_Vtherm_POLYR_ref(1) = &
         V0(1)*localframe(1,1) + &
         V0(2)*localframe(1,2) + &
         V0(3)*localframe(1,3)
    
    get_Vtherm_POLYR_ref(2) = &
         V0(1)*localframe(2,1) + &
         V0(2)*localframe(2,2) + &
         V0(3)*localframe(2,3)
    
    get_Vtherm_POLYR_ref(3) = &
         V0(1)*localframe(3,1) + &
         V0(2)*localframe(3,2) + &
         V0(3)*localframe(3,3)
    
  END FUNCTION get_Vtherm_POLYR_ref
!!!------------------------------------------------------------------------ 
  FUNCTION get_Vtherm_POLYR_face(itacty,i_face,coor_face)
    !
    ! routine destinee a calculer la vitesse due a l'increment de dilatation thermique
    ! d'un point de coordonnees de reference coor_face dans la face id_face
    !
    ! on recupere la vitesse libre des noeuds du maillage sous-jacent 

    IMPLICIT NONE

    INTEGER      :: itacty,i_face
    REAL(kind=8),DIMENSION(3) :: coor_face,get_Vtherm_POLYR_face
    !
    REAL(kind=8),DIMENSION(3)   :: v1,v2,v3,V0
    REAL(kind=8),DIMENSION(3,3) :: localframe
    
    if (POLYR2bdyty(3,itacty) == i_rbdy3 .or. POLYR2bdyty(3,itacty) == i_mbs3) then
       get_Vtherm_POLYR_face = 0.d0
       return
    endif

    call get_nodal_vector_mecaMAILx('Vfree',polyr2bdyty(1,itacty),S_POLYR(itacty)%l2g(S_POLYR(itacty)%face(1,i_face)),v1,3)
    call get_nodal_vector_mecaMAILx('Vfree',polyr2bdyty(1,itacty),S_POLYR(itacty)%l2g(S_POLYR(itacty)%face(2,i_face)),v2,3)
    call get_nodal_vector_mecaMAILx('Vfree',polyr2bdyty(1,itacty),S_POLYR(itacty)%l2g(S_POLYR(itacty)%face(3,i_face)),v3,3)

    V0 = coor_face(1)*v1 + coor_face(2)*v2 + coor_face(3)*v3

    !fd @@@ passage de cette vitesse dans le repere global
    
    localframe = get_inertia_frameTT_POLYR(itacty)
    
    get_Vtherm_POLYR_face(1) = &
         V0(1)*localframe(1,1) + &
         V0(2)*localframe(1,2) + &
         V0(3)*localframe(1,3)
    
    get_Vtherm_POLYR_face(2) = &
         V0(1)*localframe(2,1) + &
         V0(2)*localframe(2,2) + &
         V0(3)*localframe(2,3)
    
    get_Vtherm_POLYR_face(3) = &
         V0(1)*localframe(3,1) + &
         V0(2)*localframe(3,2) + &
         V0(3)*localframe(3,3)
    
  END FUNCTION get_Vtherm_POLYR_face

  FUNCTION get_Xtherm_POLYR(itacty,coor0)
    !
    ! routine destinee a calculer le deplacement total 
    ! d'un point de coordonnees initiales coor0
    ! du a la dilatation thermique
    !
    IMPLICIT NONE

    INTEGER                   :: itacty
    REAL(kind=8),DIMENSION(3) :: coor0,X0,get_Xtherm_POLYR
    
    REAL(kind=8)              :: dilat
    
    REAL(kind=8)              :: iT1x,iT2x,iT1y,iT2y,iT1z,iT2z
    

    
    if (POLYR2bdyty(3,itacty) == i_mailx .or. POLYR2bdyty(3,itacty) == i_mbs3) then
       get_Xtherm_POLYR = 0.d0
       return
    endif

    IF (S_POLYR(itacty)%is_T_constant) THEN
       
       dilat = S_POLYR(itacty)%alpha*(SUM(S_POLYR(itacty)%T(:))/S_POLYR(itacty)%nb_vertex)
       
       !fd c'est bien le coor0 initial 
       
       X0 = dilat*coor0
       
    ELSE
       
       !fd on procede direction par direction:
       !fd 1/ on calcule les increments de temperatures aux sommets du segment 
       !fd    de la direction donnee par interpolation
       !fd 2/ on calcule la vitesse
       
       iT1x=interpolate_H8(-1.d0,coor0(2),coor0(3),S_POLYR(itacty)%T(1:8))
       iT2x=interpolate_H8( 1.d0,coor0(2),coor0(3),S_POLYR(itacty)%T(1:8))
       
       X0(1) = S_POLYR(itacty)%hLx*S_POLYR(itacty)%alpha*0.5* &
            (iT1x*(coor0(1) - (coor0(1)*coor0(1)*0.5)) + iT2x*(coor0(1) + (coor0(1)*coor0(1)*0.5))) 
       
       iT1y=interpolate_H8(coor0(1),-1.d0,coor0(3),S_POLYR(itacty)%T(1:8))
       iT2y=interpolate_H8(coor0(1), 1.d0,coor0(3),S_POLYR(itacty)%T(1:8))
       
       X0(2) = S_POLYR(itacty)%hLy*S_POLYR(itacty)%alpha*0.5* &
            (iT1y*(coor0(2) - (coor0(2)*coor0(2)*0.5))+iT2y*(coor0(2) + (coor0(2)*coor0(2)*0.5))) 
       
       iT1z=interpolate_H8(coor0(1),coor0(2),-1.d0,S_POLYR(itacty)%T(1:8))
       iT2z=interpolate_H8(coor0(1),coor0(2), 1.d0,S_POLYR(itacty)%T(1:8))
       
       X0(3) = S_POLYR(itacty)%hLz*S_POLYR(itacty)%alpha*0.5* &
            (iT1z*(coor0(3) - (coor0(3)*coor0(3)*0.5))+iT2z*(coor0(3) + (coor0(3)*coor0(3)*0.5))) 
       
    ENDIF
    
    !fd je pense qu'il faudrait utiliser les localframe aussi
    
    get_Xtherm_POLYR = X0
    
  END FUNCTION get_Xtherm_POLYR
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_T_POLYR(itacty)
    !
    ! routine destinee a recuperer la temperature en un vertex
    ! dans l' immediat ca ne rend qu'une temperature moyenne sur la brique 
    !
    IMPLICIT NONE
    INTEGER      :: itacty
    
    get_T_POLYR = SUM(S_POLYR(itacty)%T(:))/S_POLYR(itacty)%nb_vertex
    
  END FUNCTION get_T_POLYR
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_dT_POLYR(itacty)
    !
    ! routine destinee a recuperer la temperature en un vertex
    ! dans l' immediat ca ne rend qu'une temperature moyenne sur la brique 
    !
    IMPLICIT NONE
    INTEGER      :: itacty
    REAL(kind=8) :: dT
    
    get_dT_POLYR = 0.25*( &
          (S_POLYR(itacty)%T(2)+S_POLYR(itacty)%T(3) &
         + S_POLYR(itacty)%T(7)+S_POLYR(itacty)%T(6)) &
         -(S_POLYR(itacty)%T(1)+S_POLYR(itacty)%T(4) &
         + S_POLYR(itacty)%T(8)+S_POLYR(itacty)%T(5)))

  END FUNCTION get_dT_POLYR
!!!------------------------------------------------------------------------ 
  SUBROUTINE save_vertex_POLYR

    IMPLICIT NONE
    INTEGER       :: id,k,itacty
    REAL(kind=8),DIMENSION(3) :: I,vect
    CHARACTER(len=5)                    :: tacID,color,clin
    CHARACTER(len=48)                   :: clin4
    
    
    OPEN(unit=1,status='REPLACE',file='VERTEX_POLYR.DAT')
    WRITE(1,*) nb_POLYR
    DO id=1,nb_POLYR
       WRITE(1,*) S_POLYR(id)%nb_vertex
       DO k=1,S_POLYR(id)%nb_vertex
          WRITE(1,*) S_POLYR(id)%vertex(1,k),S_POLYR(id)%vertex(2,k),S_POLYR(id)%vertex(3,k)
       ENDDO
       WRITE(1,*) S_POLYR(id)%nb_faces
       DO k=1,S_POLYR(id)%nb_faces
          WRITE(1,*) S_POLYR(id)%face(1,k),S_POLYR(id)%face(2,k),S_POLYR(id)%face(3,k)
       ENDDO
       I = get_inertia_tensor(POLYR2bdyty(1,id))
       WRITE(1,*) I(1),I(2),I(3)
       WRITE(1,'(A6)') '$$$$$$'
    ENDDO
    CLOSE(1)
    
    OPEN(unit=1,status='REPLACE',file='BODIES_POLYR.DAT')
    vect=0.D0
    clin='NO6xx'
    tacID='POLYR'
    color='BLEUx'
    itacty=1
    DO id=1,nb_POLYR  
       WRITE(1,'(A6)') '$bdyty'
       WRITE(1,'(1X,A5,I7)') 'RBDY3',id 
       WRITE(1,'(A6)') '$blmty'  
       !123456789012345678901234567890123456789012345678'
       clin4(1:48)=' PLAIN      1  behav  PLEX1  avrd= 0.0000000D+01'
       WRITE(clin4(23:28),'(A5)')'PLEXx'
       WRITE(1,'(A48)') clin4
       WRITE(1,'(A6)') '$nodty'
       WRITE(1,105) clin,itacty,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
       WRITE(1,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
       WRITE(1,'(A6)') '$tacty'
       WRITE(1,104) tacID,itacty,'color',color,'nb_vertex=',S_POLYR(id)%nb_vertex,'nb_faces=',S_POLYR(id)%nb_faces
       DO k=1,S_POLYR(id)%nb_vertex
          WRITE(1,131) 'coo1=',S_POLYR(id)%vertex(1,k),'coo2=',S_POLYR(id)%vertex(2,k),'coo3=',S_POLYR(id)%vertex(3,k)
       ENDDO
       DO k=1,S_POLYR(id)%nb_faces
          WRITE(1,132) 'ver1=',S_POLYR(id)%face(1,k),'ver2=',S_POLYR(id)%face(2,k),'ver3=',S_POLYR(id)%face(3,k)
       ENDDO
       WRITE(1,'(A6)') '$$$$$$'
    ENDDO
    CLOSE(1)
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7,4X,A9,I7)
131 FORMAT(27X,3(2X,A5,D14.7))
132 FORMAT(27X,3(2X,A5,I7,7X))
105 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,D14.7))
101 FORMAT(1X,A5,2X,I5)
103 FORMAT(27X,3(2X,A5,D14.7))

  END SUBROUTINE save_vertex_POLYR
!!!------------------------------------------------------------------------
  FUNCTION interpolate_H8(ksi,eta,zet,vec)

    IMPLICIT NONE
    REAL(kind=8) :: interpolate_H8
    REAL(kind=8) :: ksi,eta,zet
    REAL(kind=8),DIMENSION(8) :: N
    REAL(kind=8),DIMENSION(8) :: vec
    REAL(kind=8) :: UPK,UPE,UPZ,UMK,UMZ,UME
    REAL(kind=8),PARAMETER :: US8=1.d0/8.d0
    INTEGER :: i
    
    !   print*,ksi,eta,zet 
    !   do i=1,8
    !     print*,vec(:,i)
    !   enddo
    
    UPK=1.d0+KSI  ;  UPE=1.d0+ETA  ;  UPZ=1.d0+ZET
    UMK=1.d0-KSI  ;  UME=1.d0-ETA  ;  UMZ=1.d0-ZET
    
    N=RESHAPE((/  US8*UMK *UME *UMZ       , &
         US8*UPK *UME *UMZ       , &
         US8*UPK *UPE *UMZ       , &
         US8*UMK *UPE *UMZ       , &
         US8*UMK *UME *UPZ       , &
         US8*UPK *UME *UPZ       , &
         US8*UPK *UPE *UPZ       , &
         US8*UMK *UPE *UPZ       /),(/SIZE(N)/))
    
    interpolate_H8 = 0.d0
    DO i=1,8   
       interpolate_H8 = interpolate_H8 + (vec(i)*N(i))   
    ENDDO
    
  END FUNCTION interpolate_H8
!!!------------------------------------------------------------------------
  FUNCTION is_POLYR_same_RBDY3(itact1,itact2)

    IMPLICIT NONE

    INTEGER :: itact1,itact2 
    LOGICAL :: is_POLYR_same_RBDY3
   
    is_POLYR_same_RBDY3 = .FALSE.

    ! fd old - changer le nom de la fonction
    !IF (polyr2bdyty(3,itact1) /= i_rbdy3 .or. polyr2bdyty(3,itact2) /= i_rbdy3) return
    !IF(polyr2bdyty(1,itact1) == polyr2bdyty(1,itact2)) is_POLYR_same_RBDY3=.TRUE.

    IF ((polyr2bdyty(3,itact1) == polyr2bdyty(3,itact2)) .and. &
        (polyr2bdyty(1,itact1) == polyr2bdyty(1,itact2))) is_POLYR_same_RBDY3=.TRUE.
        
 END FUNCTION is_POLYR_same_RBDY3
!!!------------------------------------------------------------------------ 
  SUBROUTINE set_radius_correction_POLYR(val)

    IMPLICIT NONE
    REAL(kind=8) :: val

    radius_variation = val

  END SUBROUTINE set_radius_correction_POLYR

!!!------------------------------------------------------------------------ 

  SUBROUTINE set_threshold_big_POLYR(threshold)

    IMPLICIT NONE
    REAL(kind=8) :: threshold
                             !123456789012345678901234567890    
    character(len=30) :: IAM='POLYR::set_threshold_big_POLYR'

    if (allocated(big_POLYR)) then
       call faterr(IAM,'Big POLYR are already defined, define threshold before readings')
    endif
    
    threshold_big_POLYR = threshold

  END SUBROUTINE set_threshold_big_POLYR

!!!------------------------------------------------------------------------ 
  
  SUBROUTINE set_nb_big_POLYR(nb)

    IMPLICIT NONE
    integer(kind=4) :: nb
                             !12345678901234567890123    
    character(len=23) :: IAM='POLYR::set_nb_big_POLYR'
    !***
    character(len=80) :: cout

    call logmes('Arbitrarly defining big POLYR')
    
    if (allocated(big_POLYR)) then
       call faterr(IAM,'Big POLYR are already defined, set them before readings')
    endif

    nb_big_POLYR=0
    
    allocate(big_POLYR(nb))
    big_POLYR=0

    write(cout,'(A,1x,I0)') 'Maximum number of big POLYR set to',nb
    call logmes(cout)

    
  END SUBROUTINE set_nb_big_POLYR

!!!------------------------------------------------------------------------ 
  
  SUBROUTINE set_big_POLYR(itacty)

    IMPLICIT NONE
    integer(kind=4) :: itacty
    !***
                             !12345678901234567890    
    character(len=20) :: IAM='POLYR::set_big_POLYR'

    character(len=80) :: cout
       
    nb_big_POLYR=nb_big_POLYR+1

    if (nb_big_POLYR > size(big_POLYR)) then
      call faterr(IAM,'too many big POLYR defined, set a greater number')
    endif  
      
    write(cout,'(I0,1x,A,1x,I0)') itacty,'defined as a big POLYR'
    call logmes(cout)
       
    big_POLYR(nb_big_POLYR)=itacty 


  END SUBROUTINE set_big_POLYR
!!!------------------------------------------------------------------------ 
  
  SUBROUTINE skip_topo_big_POLYR

    IMPLICIT NONE

    skiptopo_big_POLYR=.TRUE.

  END SUBROUTINE skip_topo_big_POLYR
  
!!!------------------------------------------------------------------------

  subroutine get_centres_faces(id,centres)
    implicit none
    integer :: id
    real(kind=8),dimension(:,:),pointer :: centres
    !***
    integer :: if 
    real(kind=8),parameter :: un_tiers=1.d0/3.d0 

    if (associated(centres)) deallocate(centres)
    allocate(centres(3,S_POLYR(id)%nb_faces))
    do if=1,S_POLYR(id)%nb_faces

      centres(:,if) = un_tiers*(S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,if)) + &
                                S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,if)) + &
                                S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,if)))

    enddo

  end subroutine
!!!------------------------------------------------------------------------
  subroutine get_surfaces_faces(id,surfaces)
    implicit none
    integer :: id
    real(kind=8),dimension(:),pointer :: surfaces
    !***
    integer :: if 
    real(kind=8) :: a,b,c,p,tmp2(2),tmp3(3)

    !print*,'corps ',id
    
    if (associated(surfaces)) deallocate(surfaces)
    allocate(surfaces(S_POLYR(id)%nb_faces))

    surfaces(:) =  S_POLYR(id)%areas(:)

    ! do if=1,S_POLYR(id)%nb_faces

      ! tmp3 = S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,if)) - S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,if)) 
      ! a = dsqrt( dot_product(tmp3,tmp3) )
      ! tmp3 = S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,if)) - S_POLYR(id)%vertex(:,S_POLYR(id)%face(2,if)) 
      ! b = dsqrt( dot_product(tmp3,tmp3) )
      ! tmp3 = S_POLYR(id)%vertex(:,S_POLYR(id)%face(1,if)) - S_POLYR(id)%vertex(:,S_POLYR(id)%face(3,if))
      ! c = dsqrt( dot_product(tmp3,tmp3) )

      ! p = 0.5D0 * (a+b+c)

      ! tmp3 = (/p-a,p-b,p-c/)      
      ! if ( maxval(tmp3) == 0.d0 .or. minval(tmp3)/maxval(tmp3) < 1.d-3 )  then
      !   ! triangle degenerated: 3 merged vertices or 3 aligned vertices
      !   surfaces(if) = 0.d0
      ! else
      !   ! triangle ok
      !   surfaces(if) = dsqrt(p * (p-a) * (p-b) * (p-c))
      ! endif

      !print *,if,p,a,b,c,surfaces(if)

    ! enddo

  end subroutine
!!!------------------------------------------------------------------------
  subroutine assume_good_orientation_POLYR
     implicit none

     assume_good_orientation=.true. 

  end subroutine
!!!------------------------------------------------------------------------
  subroutine skip_HE_POLYR
     implicit none

     skip_HE=.true. 

  end subroutine
!!!------------------------------------------------------------------------
  subroutine flip_orientation_POLYR(nb,liste)
     implicit none
     integer :: nb,liste(nb)

     allocate(to_flip(nb))
     to_flip=liste

  end subroutine
!!!------------------------------------------------------------------------
  logical function flip(i)
     implicit none
     integer::i

     if (allocated(to_flip)) then
       if (count(to_flip(:) == i) /= 0) flip=.TRUE.
     else

       flip=.FALSE.

     end if

  end function
!!!------------------------------------------------------------------------
  subroutine add_reac_POLYR(itact,xxccdof,xxreac,storage)
    implicit none
    integer :: itact,storage
    INTEGER,     DIMENSION(6)  :: xxccdof
    REAL(kind=8),DIMENSION(6)  :: xxreac

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
    
      !print*,'add reac',storage,itact
      !write(*,'(6(1x,D12.5))') xxreac

      call add_reac(POLYR2bdyty(1,itact),xxccdof,xxreac,storage)       
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

      call add_Rreac_mecaMAILx(POLYR2bdyty(1,itact),xxccdof,xxreac,storage)
   
    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      call add_reac_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact),xxccdof,xxreac,storage)
       
    endif

  end subroutine  
!!!------------------------------------------------------------------------
  FUNCTION get_coorTT_POLYR(itact)
    implicit none
    integer :: itact
    REAL(kind=8),DIMENSION(3) :: get_coorTT_POLYR

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_coorTT_POLYR = get_coorTT_RBDY3(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))       
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_coorTT_POLYR = get_RcoorTT_mecaMAILx(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))
   
    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_coorTT_POLYR = get_coorTT_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    endif

  end function
!!!------------------------------------------------------------------------
  FUNCTION get_inertia_frameTT_POLYR(itact)

    IMPLICIT NONE
    INTEGER                     :: itact
    REAL(kind=8),DIMENSION(3,3) :: get_inertia_frameTT_POLYR
   
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_inertia_frameTT_POLYR = get_inertia_frameTT_RBDY3(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_inertia_frameTT_POLYR = get_Rinertia_frameTT_mecaMAILx(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_inertia_frameTT_POLYR = get_inertia_frameTT_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    endif
  END FUNCTION get_inertia_frameTT_POLYR
!!!------------------------------------------------------------------------
  FUNCTION get_inertia_frameIni_POLYR(itact)

    IMPLICIT NONE
    INTEGER                     :: itact
    REAL(kind=8),DIMENSION(3,3) :: get_inertia_frameIni_POLYR
   
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_inertia_frameIni_POLYR = get_inertia_frameTT_RBDY3(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mailx) then

      ! get_inertia_frameIni_POLYR = get_Rinertia_frameIni_mecaMAILx(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      ! get_inertia_frameIni_POLYR = get_inertia_frameIni_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))

    endif
  END FUNCTION get_inertia_frameIni_POLYR
!!!------------------------------------------------------------------------
  subroutine comp_vlocy_POLYR(itact,storage)  
    implicit none
    integer :: itact,storage

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  

      call comp_vlocy(POLYR2bdyty(1,itact),storage)
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

     call comp_vlocy_mecaMAILx(POLYR2bdyty(1,itact),storage)
   
    else if (POLYR2bdyty(3,itact) == i_mbs3) then

     call comp_vlocy_MBS3D(POLYR2bdyty(1,itact),storage)
       
    endif

  end subroutine  
!!!------------------------------------------------------------------------
  subroutine nullify_vlocy_POLYR(itact,storage)  
    implicit none
    integer :: itact,storage

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  

      call nullify_vlocy(POLYR2bdyty(1,itact),storage)
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

     call nullify_vlocy_mecaMAILx(POLYR2bdyty(1,itact),storage)

    else if (POLYR2bdyty(3,itact) == i_mbs3) then
     
     call nullify_vlocy_MBS3D(POLYR2bdyty(1,itact),storage)
       
    endif

  end subroutine  
!!!------------------------------------------------------------------------
  subroutine nullify_reac_POLYR(itact,storage)  
    implicit none
    integer :: itact,storage

    if (POLYR2bdyty(3,itact) == i_rbdy3) then  

      call nullify_reac(POLYR2bdyty(1,itact),storage)
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

      call nullify_reac_mecaMAILx(POLYR2bdyty(1,itact),storage)
   
   else if (POLYR2bdyty(3,itact) == i_mbs3) then

      call nullify_reac_MBS3D(POLYR2bdyty(1,itact),storage)
      
   endif

  end subroutine  
!!!------------------------------------------------------------------------
  FUNCTION get_vlocy_POLYR(itact,istate)
    implicit none
    integer :: itact,istate
    REAL(kind=8),DIMENSION(6) :: get_vlocy_POLYR
 
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_vlocy_POLYR = get_vlocy(POLYR2bdyty(1,itact),istate)       

      !print*,'get vlocy',istate,itact
      !write(*,'(6(1x,D12.5))') get_vlocy_POLYR
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_vlocy_POLYR = get_Rvlocy_mecaMAILx(POLYR2bdyty(1,itact),istate)
   
   else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_vlocy_POLYR = get_vlocy_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact),istate)
      
   endif

  end function
!!!------------------------------------------------------------------------
  FUNCTION get_shiftTT_POLYR(itact)
    implicit none
    integer :: itact
    REAL(kind=8),DIMENSION(3) :: get_shiftTT_POLYR
 
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_shiftTT_POLYR = get_shiftTT(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))       
     
    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_shiftTT_POLYR = 0.d0
   
   else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_shiftTT_POLYR = get_shiftTT_MBS3D(POLYR2bdyty(1,itact),POLYR2bdyty(2,itact))       

   endif

  end function
!!!------------------------------------------------------------------------
  subroutine set_topo_angle(angle)
    implicit none
    real(kind=8) :: angle
    ! ***
    !                         123456789012345678901
    CHARACTER(len=21) :: IAM='POLYR::set_topo_angle'

    if (angle < 0.d0 .or. angle > 180.d0) &
      call Faterr(IAM,' angle must be between 0 and 180')  
    
    topo_angle = angle

  end subroutine
!!!------------------------------------------------------------------------
  subroutine set_flatness_angle(angle)
    implicit none
    real(kind=8) :: angle
    ! ***
    !                         1234567890123456789012345
    CHARACTER(len=25) :: IAM='POLYR::set_flatness_angle'

    if (angle < 0.d0 .or. angle > 180.d0) &
      call Faterr(IAM,' angle must be between 0 and 180')  

    flatness_angle = angle

  end subroutine
!!!------------------------------------------------------------------------
  function get_flatness_angle()
    implicit none
    real(kind=8) :: get_flatness_angle
    ! ***
    !                         1234567890123456789012345
    CHARACTER(len=25) :: IAM='POLYR::get_flatness_angle'

    get_flatness_angle = flatness_angle

  end function
!!!------------------------------------------------------------------------
  function get_topo_data_POLYR()
    implicit none
    integer, dimension(:,:), pointer :: get_topo_data_POLYR
    !
    integer :: i_tacty, nb_ele, i_ele, i_f, idx

    ! first count total number of faces:
    nb_ele = 0
    do i_tacty = 1, nb_POLYR
      nb_ele = nb_ele + S_POLYR(i_tacty)%nb_faces
    end do

    allocate(get_topo_data_POLYR(4,nb_ele))
    get_topo_data_POLYR(:,:) = -1

    nb_ele = 0
    do i_tacty = 1,nb_POLYR

      !id = itacty
      do i_f = 1, size(S_POLYR(i_tacty)%f2f_set)
        do i_ele = 1, size(S_POLYR(i_tacty)%f2f_set(i_f)%G_i)
          idx = S_POLYR(i_tacty)%f2f_set(i_f)%G_i(i_ele)
          get_topo_data_POLYR(1,nb_ele+idx) = i_tacty
          get_topo_data_POLYR(2,nb_ele+idx) = i_f
          get_topo_data_POLYR(3,nb_ele+idx) = idx
          get_topo_data_POLYR(4,nb_ele+idx) = S_POLYR(i_tacty)%f2f_status(i_f)
        end do
      end do

      nb_ele = nb_ele + S_POLYR(i_tacty)%nb_faces

    end do

  end function get_topo_data_POLYR
!!!------------------------------------------------------------------------
  integer function get_ENT_POLYR(itact)
    implicit none
    integer :: itact
    
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  
      
      get_ent_POLYR = get_entity_rbdy3(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mailx) then

      get_ENT_POLYR = get_entity_mecaMAILx(POLYR2bdyty(1,itact))

    else if (POLYR2bdyty(3,itact) == i_mbs3) then

      get_ent_POLYR = get_entity_MBS3D(POLYR2bdyty(1,itact))

    endif

  end function
!!!------------------------------------------------------------------------
  subroutine get_wireframe_POLYR(id,angle,coor,connectivity)
    implicit none
    integer :: id
    real(kind=8) :: angle
    ! coordonnees des noeuds : 3,nbn
    real(kind=8),dimension(:,:),pointer :: coor 
    ! connectivite des faces a plat: nbf,nbnf_b,idnf_b,...,idnf_e, ...., nbnf_e,idnf_b,...,idnf_e
    integer,dimension(:),pointer :: connectivity
    ! ***
    integer :: i,sz,nb_f2f,j,nbn,idx
    type(G_i_list),dimension(:),allocatable :: tmp 
    integer,dimension(:),allocatable :: map
    real(kind=8) :: tol    

    tol = 1.d0 - cos(angle*PI_g/90.d0)

    allocate(map(S_POLYR(id)%nb_vertex))
    map=0

    nb_f2f = size(S_POLYR(id)%f2f_contour)

    allocate(tmp(nb_f2f))
    sz=1 + nb_f2f 

    !> on recupere les contours et on tasse la numerotation
    idx=0
    do i=1,nb_f2f
      !print*,'xxxxxxxxxxx'
      !print*,i
      !print*,'avant ',S_POLYR(id)%f2f_contour(i)%G_i
      call get_corners_of_contour(S_POLYR(id)%vertex,S_POLYR(id)%f2f_contour(i)%G_i,tmp(i)%G_i,tol)
      !print*,'apres ',tmp(i)%G_i
      !print*,'xxxxxxxxxxx'
      sz = sz + size(tmp(i)%G_i)
      do j=1,size(tmp(i)%G_i)
        if (map(tmp(i)%G_i(j)) == 0) then
          idx= idx + 1
          map(tmp(i)%G_i(j)) = idx
        endif
      enddo
    enddo

    allocate(connectivity(sz))

    connectivity(1)=nb_f2f
    sz=1
    do i=1,nb_f2f
      sz = sz+1
      connectivity(sz) = size(tmp(i)%G_i)
      do j=1,size(tmp(i)%G_i)
        connectivity(sz+j)=map(tmp(i)%G_i(j))
      enddo
      sz=sz+connectivity(sz)
      deallocate(tmp(i)%G_i)
    enddo

    nbn=count(map>0)
    allocate(coor(3,nbn))
    do i=1,nbn
      j = minloc(map,dim=1,mask=map==i)
      coor(1:3,i) = S_POLYR(id)%vertex(1:3,j)
    enddo
    deallocate(tmp,map)
  end subroutine
!!!------------------------------------------------------------------------
 subroutine get_vertex_POLYR(itacty, coor, dime, nb_vertices)
   implicit none
   integer(kind=4), intent(in)           :: itacty
   real(kind=8), dimension(:,:), pointer :: coor
   integer(kind=4), intent(out)          :: dime, nb_vertices
   !
   integer(kind=4) :: k
   real(kind=8), dimension(3)   :: X, vertex_ref
   real(kind=8), dimension(3,3) :: frame
   
   CHARACTER(len=80) :: cout
   CHARACTER(len=23) :: IAM
   !      12345678901234567890123
   IAM = "POLYR::get_vertex_POLYR"

   if(associated(coor) ) nullify(coor)

   if( itacty > nb_polyr .or. itacty < 1 ) return

   dime = 3
   nb_vertices = S_POLYR(itacty)%nb_vertex

   if( nb_vertices > 0 ) then

     allocate(coor(dime,nb_vertices))

     if (POLYR2bdyty(3,itacty) == i_rbdy3) then

       ! rm: attention a la roucoulade... get_inertia_frame rend localFrameTT !

       X     = get_coor_POLYR(itacty)
       frame = get_inertia_frame_POLYR(itacty)

       do k = 1, nb_vertices
          vertex_ref(1:3)= S_POLYR(itacty)%vertex_ref(3*k-2:3*k)
        
          coor(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
          coor(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
          coor(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

       end do

       !fd debile coor(1:dime,1:nb_vertices) = S_POLYR(itacty)%vertex(1:dime,1:nb_vertices)

     else if (POLYR2bdyty(3,itacty) == i_mailx) then

       do k = 1, nb_vertices
         coor(:,k) = get_coor_nodty_mecaMAILx(POLYR2bdyty(1,itacty),S_POLYR(itacty)%l2g(k))
       end do
       
    else if (POLYR2bdyty(3,itacty) == i_mbs3) then
      WRITE(cout,'(A)') "Function not implemented with mbs body."
      CALL faterr(IAM,cout)
       
    end if

   end if

 end subroutine
!!!------------------------------------------------------------------------
 function get_ptr_vertexTT_POLYR(itacty)
   implicit none
   integer(kind=4), intent(in)           :: itacty
   real(kind=8), dimension(:,:), pointer :: get_ptr_vertexTT_POLYR
   !
   integer(kind=4) :: nb_vertices

   nullify(get_ptr_vertexTT_POLYR)

   if( itacty > nb_polyr .or. itacty < 1 ) return

   nb_vertices = S_POLYR(itacty)%nb_vertex

   if( nb_vertices > 0 ) then

     get_ptr_vertexTT_POLYR => S_POLYR(itacty)%vertex(1:3,1:nb_vertices)

   end if

 end function

!!!------------------------------------------------------------------------
 function get_ptr_normalTT_POLYR(itacty)
   implicit none
   integer(kind=4), intent(in)           :: itacty
   real(kind=8), dimension(:,:), pointer :: get_ptr_normalTT_POLYR
   !
   integer(kind=4) :: nb_faces

   nullify(get_ptr_normalTT_POLYR)

   if( itacty > nb_polyr .or. itacty < 1 ) return

   nb_faces = S_POLYR(itacty)%nb_faces

   if( nb_faces > 0 ) then

     get_ptr_normalTT_POLYR => S_POLYR(itacty)%normal(1:3,1:nb_faces)

   end if

 end function

!!!------------------------------------------------------------------------
 function get_ptr_vertex_ref_POLYR(itacty)
   implicit none
   integer(kind=4), intent(in) :: itacty
   real(kind=8), dimension(:), pointer :: get_ptr_vertex_ref_POLYR

   nullify(get_ptr_vertex_ref_POLYR)

   if( itacty > nb_polyr .or. itacty < 1 ) return

   get_ptr_vertex_ref_POLYR => S_POLYR(itacty)%vertex_ref

 end function get_ptr_vertex_ref_POLYR

!!!------------------------------------------------------------------------
 subroutine get_POLYR2BDYTY(map, size1, size2)
   implicit none
   integer(kind=4), dimension(:,:), pointer :: map
   integer(kind=4), intent(out)             :: size1, size2
   !
   if(associated(map) ) nullify(map)

   size1 = 3
   size2 = nb_POLYR

   if( nb_POLYR > 0 ) then

     allocate(map(size1,size2))

     map(1:size1,1:size2) = POLYR2bdyty(1:size1,1:size2)

   end if

 end subroutine

 function get_ptr_POLYR2BDYTY()
   implicit none
   integer(kind=4), dimension(:,:), pointer :: get_ptr_POLYR2BDYTY

   nullify(get_ptr_POLYR2BDYTY)

   if( nb_POLYR > 0 ) then

     get_ptr_POLYR2BDYTY => POLYR2bdyty

   end if

 end function

!!! for vtk visu !!!
!------------------------------------------------------------------------
 function init_outlines_POLYR()
   implicit none
   integer(kind=4) :: itacty,sz,i_f
   real(kind=8),dimension(:,:),pointer :: init_outlines_POLYR

   if ( nb_POLYR .eq. 0 ) then
     init_outlines_POLYR => null()
     return
   endif 

   if (associated(nb_point_outlines_POLYR)) deallocate(nb_point_outlines_POLYR)
   allocate(nb_point_outlines_POLYR(nb_POLYR+1))
   nb_point_outlines_POLYR(1) = 0
   do itacty = 1, nb_POLYR
     nb_point_outlines_POLYR(itacty+1) = &
          nb_point_outlines_POLYR(itacty) + S_POLYR(itacty)%nb_vertex
   end do

   sz =  nb_point_outlines_POLYR(nb_POLYR+1)

   if (associated(outlines_POLYR)) deallocate(outlines_POLYR)
   allocate(outlines_POLYR(3,sz)) 

   outlines_POLYR(1:3,1:sz) = 0.d0

   init_outlines_POLYR => outlines_POLYR

   !storing connectivities of each polyr
   ! 1/ sizing
   sz = 0
   do itacty = 1, nb_POLYR
     sz = sz + S_POLYR(itacty)%nb_faces*4 + 1
   end do

   if( associated(all_connectivities) ) deallocate(all_connectivities)
   allocate(all_connectivities(sz))

   ! 2/ filling
   sz = 1
   do itacty = 1, nb_POLYR
     all_connectivities(sz) = S_POLYR(itacty)%nb_faces
     sz = sz+1
     do i_f = 1, S_POLYR(itacty)%nb_faces
       all_connectivities(sz) = 3
       all_connectivities(sz+1:sz+3) = S_POLYR(itacty)%face(1:3,i_f)
       sz = sz+4
     end do
   end do

 end function init_outlines_POLYR

 !!!------------------------------------------------------------------------
 subroutine updt_outline_POLYR(itacty,outline)

   implicit none
   integer(kind=4) :: itacty,k,nb_vertices
   real(kind=8),dimension(3)       :: X, vertex_ref
   real(kind=8),dimension(3,3)     :: frame
   real(kind=8),dimension(3,S_POLYR(itacty)%nb_vertex) :: outline

   nb_vertices = S_POLYR(itacty)%nb_vertex
   if (POLYR2bdyty(3,itacty) == i_rbdy3) then

     X     = get_coor_POLYR(itacty)
     frame = get_inertia_frame_POLYR(itacty)

     do k = 1, nb_vertices
        vertex_ref(1:3)= S_POLYR(itacty)%vertex_ref(3*k-2:3*k)
 
        outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
        outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
        outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

     end do

   else if (POLYR2bdyty(3,itacty) == i_mailx) then


     do k = 1, nb_vertices
       outline(:,k) = get_coor_nodty_mecaMAILx(POLYR2bdyty(1,itacty),S_POLYR(itacty)%l2g(k))
     end do


   else if (POLYR2bdyty(3,itacty) == i_mbs3) then

     X     = get_coor_MBS3D(polyr2bdyty(1,itacty),polyr2bdyty(2,itacty))
     frame = get_inertia_frame_MBS3D(polyr2bdyty(1,itacty),polyr2bdyty(2,itacty))

     do k = 1, nb_vertices
        vertex_ref(1:3)= S_POLYR(itacty)%vertex_ref(3*k-2:3*k)
 
        outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
        outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
        outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

     end do

   end if

 end subroutine updt_outline_POLYR
 
 !------------------------------------------------------------------------
 function get_nb_point_outlines_POLYR()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_POLYR

   get_nb_point_outlines_POLYR => nb_point_outlines_POLYR

 end function get_nb_point_outlines_POLYR

!------------------------------------------------------------------------
  function get_nb_scalarfields_POLYR()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_POLYR

   get_nb_scalarfields_POLYR = nbsf

 end function get_nb_scalarfields_POLYR

!------------------------------------------------------------------------
 subroutine updt_scalarfield_POLYR(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3)   = get_X_POLYR(itacty)
   scalarfield(4:9)   = get_V_POLYR(itacty)
   scalarfield(10:15) = get_REAC_POLYR(itacty)

 end subroutine updt_scalarfield_POLYR

!------------------------------------------------------------------------
 function init_scalarfields_POLYR()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_POLYR

   if ( nb_POLYR .eq. 0 ) then
     init_scalarfields_POLYR => null()
     return
   endif 

   sz = nbsf * nb_POLYR

   if (associated(scalarfields_POLYR)) deallocate(scalarfields_POLYR)
   allocate(scalarfields_POLYR(sz)) 

   scalarfields_POLYR(1:sz) = 0.d0

   init_scalarfields_POLYR => scalarfields_POLYR

 end function init_scalarfields_POLYR

 !------------------------------------------------------------------------
 subroutine update_postdata_POLYR()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf,szo,nbpto

   if (nb_POLYR == 0) return

   if (.not. associated(outlines_POLYR) ) call faterr('POLYR::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_POLYR) ) call faterr('POLYR::update_postdata','init_scalarfields is mandatory')

   iszo = 0
   iszsf = 0
   do itacty=1,nb_POLYR
     nbpto = S_POLYR(itacty)%nb_vertex
     call updt_outline_POLYR(itacty,outlines_POLYR(1:3,iszo+1:iszo+nbpto))
     iszo = iszo + nbpto
     call updt_scalarfield_POLYR(itacty,scalarfields_POLYR(iszsf+1:iszsf+nbsf))
     iszsf = iszsf + nbsf
   enddo    

 end subroutine update_postdata_POLYR

 !------------------------------------------------------------------------ 
 function get_ptr_connectivity_POLYR(itacty)
   implicit none
   integer(kind=4), intent(in) :: itacty
   integer(kind=4), dimension(:,:), pointer :: get_ptr_connectivity_POLYR

   get_ptr_connectivity_POLYR => S_POLYR(itacty)%face

 end function 

 !------------------------------------------------------------------------ 
 function get_all_connectivities_POLYR()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_all_connectivities_POLYR

   get_all_connectivities_POLYR => all_connectivities

 end function get_all_connectivities_POLYR

!!!-PTA----------------------------------------------------------------------- 
  INTEGER FUNCTION get_visibleID_POLYR(itact)

    IMPLICIT NONE
    INTEGER :: itact 
    
    CHARACTER(len=80) :: cout
    CHARACTER(len=26) :: IAM
    !      12345678901234567890123456
    IAM = "POLYR::get_visibleID_POLYR"
 
    if (POLYR2bdyty(3,itact) == i_rbdy3) then  ! cas RBDY3
       get_visibleID_POLYR = get_visibleID(POLYR2bdyty(1,itact))
    else if (POLYR2bdyty(3,itact) == i_mailx) then ! cas mecaMAILx
       get_visibleID_POLYR = get_visibleID(POLYR2bdyty(1,itact))
    else if (POLYR2bdyty(3,itact) == i_mbs3) then ! cas MBS3D
       get_visibleID_POLYR = POLYR2bdyty(1,itact)
    endif

  END FUNCTION get_visibleID_POLYR
!!!-PTA----------------------------------------------------------------------- 

 subroutine print_info_POLYR(itact)
   implicit none
   integer(kind=4), intent(in) :: itact

   if( polyr2bdyty(3,itact) == i_rbdy3 ) then
     call print_info_RBDY3(polyr2bdyty(1,itact))
   end if

 end subroutine

 subroutine clean_memory_POLYR()
   implicit none
   integer(kind=4) :: i, j

   nb_POLYR = 0
   nb_MAILx = 0

   if( allocated(S_POLYR) ) then
     do i = 1, size(S_POLYR)
       if( associated(S_POLYR(i)%vertex)        ) deallocate(S_POLYR(i)%vertex)
       if( associated(S_POLYR(i)%vertex_ref)    ) deallocate(S_POLYR(i)%vertex_ref)
       if( associated(S_POLYR(i)%face)          ) deallocate(S_POLYR(i)%face)
       if( associated(S_POLYR(i)%normal)        ) deallocate(S_POLYR(i)%normal)
       if( associated(S_POLYR(i)%normal_ref)    ) deallocate(S_POLYR(i)%normal_ref)
       if( associated(S_POLYR(i)%areas)         ) deallocate(S_POLYR(i)%areas)
       if( associated(S_POLYR(i)%l2g)           ) deallocate(S_POLYR(i)%l2g)
       if( associated(S_POLYR(i)%vsupport_face) ) deallocate(S_POLYR(i)%vsupport_face)
       if( associated(S_POLYR(i)%val_support)   ) deallocate(S_POLYR(i)%val_support)
       if( associated(S_POLYR(i)%T)             ) deallocate(S_POLYR(i)%T)
       if( associated(S_POLYR(i)%inc_T)         ) deallocate(S_POLYR(i)%inc_T)

       !> \todo rm: this is rather ugly, it would be better to initialize all pointers
       !>           to null() inside DiscreteGeometry module
       if (.not. skip_HE) then
         call erase_HE_Hdl(S_POLYR(i)%HE_Hdl)
       end if

       if( associated(S_POLYR(i)%f2f_set) ) then
         do j = 1, size(S_POLYR(i)%f2f_set)
           if( associated(S_POLYR(i)%f2f_set(j)%G_i) ) deallocate(S_POLYR(i)%f2f_set(j)%G_i)
         end do
         deallocate(S_POLYR(i)%f2f_set)
       end if
       if( associated(S_POLYR(i)%f2f_contour) ) then
         do j = 1, size(S_POLYR(i)%f2f_contour)
           if( associated(S_POLYR(i)%f2f_contour(j)%G_i) ) deallocate(S_POLYR(i)%f2f_contour(j)%G_i)
         end do
         deallocate(S_POLYR(i)%f2f_contour)
       end if
       if( associated(S_POLYR(i)%f2f_sommetofcontour) ) then
         do j = 1, size(S_POLYR(i)%f2f_sommetofcontour)
           if( associated(S_POLYR(i)%f2f_sommetofcontour(j)%G_i) ) then
             deallocate(S_POLYR(i)%f2f_sommetofcontour(j)%G_i)
           end if
         end do
         deallocate(S_POLYR(i)%f2f_sommetofcontour)
       end if
       if( associated(S_POLYR(i)%f2f_sommet) ) deallocate(S_POLYR(i)%f2f_sommet)
       if( associated(S_POLYR(i)%f2f_edge  ) ) deallocate(S_POLYR(i)%f2f_edge)
       if( associated(S_POLYR(i)%f2f_status) ) deallocate(S_POLYR(i)%f2f_status)
     end do
     deallocate(S_POLYR)
   end if

   ! if( allocated(polyr2bdyty) ) deallocate(polyr2bdyty)
   if( associated( polyr2bdyty ) ) deallocate( polyr2bdyty )

   if( allocated(big_POLYR) ) deallocate(big_POLYR)
   nb_big_POLYR = 0

   if( allocated(to_flip) ) deallocate(to_flip)

   if( associated(nb_point_outlines_POLYR) ) then
     deallocate(nb_point_outlines_POLYR)
     nullify(nb_point_outlines_POLYR)
   end if

   if( associated(all_connectivities) ) then
     deallocate(all_connectivities)
     nullify(all_connectivities)
   end if

   if( associated(outlines_POLYR) ) then
     deallocate(outlines_POLYR)
     nullify(outlines_POLYR)
   end if

   if( associated(scalarfields_POLYR) ) then
     deallocate(scalarfields_POLYR)
     nullify(scalarfields_POLYR)
   end if

 end subroutine



END MODULE POLYR

