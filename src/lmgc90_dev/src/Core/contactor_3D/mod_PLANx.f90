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
MODULE PLANx                                       

  !!****h* LMGC90.CORE/PLANx
  !! NAME
  !!  module PLANx
  !! AUTHORS
  !!  Community
  !! FUNCTION
  !!  Modelize an empty cylinder  
  !!****

  USE utilities
  USE overall
  USE bulk_behaviour
  USE a_DOF
  USE algebra
  USE parameters

  USE RBDY3,ONLY: &
       get_nb_RBDY3,get_nb_tacty,get_TacID,get_data, &
       get_color, &
       get_coorTT_RBDY3 => get_coorTT, &
       get_coor_RBDY3   => get_coor  , &
       get_cooref_RBDY3 => get_cooref, & 
       get_inertia_frameTT_RBDY3  => get_inertia_frameTT, & 
       get_inertia_frame_RBDY3    => get_inertia_frame, & ! Permet de récupérer le repère d'inertie en fin de pas(visu)
       get_inertia_frameIni_RBDY3 => get_inertia_frameIni, & !for md 
       add_reac_RBDY3   => add_reac  , &
       get_Xbegin_RBDY3 => get_Xbegin, &
       get_vlocy_RBDY3  => get_vlocy , &
       comp_vlocy_RBDY3 => comp_vlocy, &
       get_WS_RBDY3     => get_WS    , &
       nullify_reac_RBDY3  => nullify_reac , &       
       nullify_vlocy_RBDY3 => nullify_vlocy, &       
       get_behav_RBDY3     => get_behav    , & 
       get_ENT_RBDY3     => get_entity_RBDY3, &
       get_shiftTT_RBDY3 => get_shiftTT     , &
       get_embeded_frame_RBDY3 => get_embeded_frame, &
       put_embeded_frame_RBDY3 => put_embeded_frame, &
       get_X_RBDY3             => get_X            , &
       get_V_RBDY3             => get_V            , &  
       get_reac_RBDY3          => get_reac         , &
       set_bdyty2tacty_RBDY3, &
       get_visible, & 
       get_visibleID !pta
  
  use mbs3d, only : get_nb_MBS3D              => get_nb             , &
                    get_nb_tacty_MBS3D        => get_nb_tacty       , &
                    get_tacID_MBS3D           => get_tacID          , &
                    get_ptr_rdata_MBS3D       => get_ptr_rdata      , &
                    get_color_MBS3D           => get_color          , &
                    get_coor_MBS3D            => get_coor           , &
                    get_coorTT_MBS3D          => get_coorTT         , &
                    get_inertia_frame_MBS3D   => get_inertia_frame  , &
                    get_inertia_frameTT_MBS3D => get_inertia_frameTT, &
                    get_embeded_frame_MBS3D   => get_embeded_frame  , &
                    get_vlocy_MBS3D           => get_vlocy          , &
                    get_ENT_MBS3D             => get_entity         , &
                    get_shiftTT_MBS3D         => get_shiftTT        , &
                    add_reac_MBS3D            => add_reac           , &
                    comp_vlocy_MBS3D          => comp_vlocy         , &
                    nullify_vlocy_MBS3D       => nullify_vlocy      , &
                    nullify_reac_MBS3D        => nullify_reac
  
  
  IMPLICIT NONE

  PRIVATE

  TYPE,PUBLIC ::  T_PLANx
     REAL(kind=8) :: ax1,ax2,ax3
  END TYPE T_PLANx
 
  ! ----------------------------------------------------------------------
  ! planx2bdyty
  ! planx2bdyty(1,itac) : serial number of body RBDY3 to which is attached
  !                       the contactor PLANx numbered itac in the list of
  !                       all contactors PLANx 
  ! planx2bdyty(2,itac) : serial number of contactor PLANx itac in the list 
  !                       of contactors PLANx attached to a body(Generically
  !                       1 except if the underlying model is "i_mbs3")
  ! planx2bdyty(3,itac) : kind of underlying model i_rbdy3:rigid, i_mbs3:MBS
  ! ----------------------------------------------------------------------
  INTEGER,DIMENSION(:,:),POINTER  ::  planx2bdyty

  INTEGER,PRIVATE      :: nb_PLANx = 0
  REAL(kind=8),PRIVATE :: mean_radius_PLANx
  REAL(kind=8),PRIVATE :: max_radius,min_radius
 
  integer(kind=4) :: nbpto = 8 ! nb points describing the contactor outline
  real(kind=8), dimension(3,8) :: unit_plan
  integer(kind=4) :: nbsf=15
  integer(kind=4),dimension(:),  pointer :: nb_point_outlines_PLANx => null() 
  integer(kind=4),dimension(:),  pointer :: all_connectivities      => null()
  real(kind=8),   dimension(:,:),pointer :: outlines_PLANx          => null()
  real(kind=8),   dimension(:),  pointer :: scalarfields_PLANx      => null()

  PUBLIC planx2bdyty

  PUBLIC &
       read_bodies_PLANx

  PUBLIC &
       !get_behav_planx,
       get_color_planx,get_mean_radius_planx,     &
       get_coorTT_PLANx,get_coor_PLANx,         &
       get_inertia_frame_PLANx,add_reac_PLANx,    &
       get_Xbegin_PLANx,get_vlocy_PLANx,          &
       get_V_PLANx,get_reac_PLANx,                & 
       comp_vlocy_PLANx,nullify_reac_PLANx,       &
       get_nb_PLANx,nullify_vlocy_PLANx,          &
       get_inertia_frameTT_PLANx,               &
       get_max_radius_PLANx,get_min_radius_PLANx, &
       get_ENT_PLANx,get_inertia_frameIni_PLANx,&
       get_X_PLANx,get_cooref_PLANx,get_axes_PLANx, & !,get_data
       get_WS_PLANx,get_shiftTT_PLANx, &
       get_embeded_frame_PLANx,put_embeded_frame_PLANx, &
       set_data_PLANx,&
       get_visible_PLANx, & 
       get_visibleID_PLANx !pta

  public &! <- rm: visu vtk
       get_ptr_PLANx2BDYTY        ,&
       get_nb_point_outlines_PLANx,&
       init_outlines_PLANx        ,&
       get_nb_scalarfields_PLANx  ,&
       init_scalarfields_PLANx    ,&
       update_postdata_PLANx      ,&
       get_all_connectivities_PLANx

  public clean_memory_PLANx

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_PLANx

    IMPLICIT NONE

    INTEGER                           :: ibdyty,itacty,errare,nb_RBDY3,nb_MBS3D,ID_RBDY3,ID_TACTY,id
    REAL(kind=8)                      :: ax1,ax2,ax3 
    REAL(kind=8),DIMENSION(3)         :: DATA
    REAL(kind=8),DIMENSION(:),pointer :: rdata
    REAL(kind=8)                      :: radius,mean_radius 
    CHARACTER(len=18)                 :: IAM='PLANx::read_bodies'
    CHARACTER(len=80)                 :: cout 
    REAL(kind=8),DIMENSION(3,3)       :: EmbededFrame,localframe

    nb_PLANx = 0
    nb_RBDY3 = get_nb_RBDY3()

    DO ibdyty=1,nb_RBDY3   
      DO itacty=1,get_nb_tacty(ibdyty)
        IF ( get_tacID(ibdyty,itacty) =='PLANx') then
          nb_PLANx = nb_PLANx+1
          id=get_contactor_id_from_name('PLANx')
          call set_bdyty2tacty_rbdy3(ibdyty,itacty,id,nb_PLANx) 
        endif
       END DO
    END DO
    
    nb_MBS3D =get_nb_MBS3D()
    DO ibdyty=1,nb_MBS3D   
      DO itacty=1,get_nb_tacty_MBS3D(ibdyty)
        IF ( get_tacID_MBS3D(ibdyty,itacty) /= i_planx) cycle
        nb_PLANx = nb_PLANx+1
      END DO
    END DO

    WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_PLANx,'PLANx found'
    CALL LOGMES(cout)

    IF (nb_PLANx.EQ.0) RETURN

    allocate(planx2bdyty(3,nb_PLANx),stat=errare)

    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating planx2bdyty')
    END IF

    nb_PLANx = 0
    DO ibdyty=1,nb_RBDY3   
       DO itacty=1,get_nb_tacty(ibdyty)
          IF ( get_tacID(ibdyty,itacty).EQ.'PLANx') THEN
             nb_PLANx = nb_PLANx + 1
             planx2bdyty(1,nb_PLANx) = ibdyty
             planx2bdyty(2,nb_PLANx) = itacty
             planx2bdyty(3,nb_PLANx) = i_rbdy3
          END IF
       END DO
    END DO
    
    DO ibdyty = 1, nb_MBS3D
       DO itacty = 1, get_nb_tacty_MBS3D(ibdyty)
          IF( get_tacID_MBS3D(ibdyty,itacty) /= i_planx) cycle
          nb_PLANx = nb_PLANx + 1
          planx2bdyty(1,nb_PLANx) = ibdyty
          planx2bdyty(2,nb_PLANx) = itacty
          planx2bdyty(3,nb_PLANx) = i_mbs3
       END DO
    END DO

    min_radius = 1.D20
    max_radius =-1.D20
    mean_radius= 0.D0
    
    IF (nb_PLANx.EQ.0) RETURN

    DO ibdyty=1,nb_PLANx
       !case of PLANx is carried by a MBS
       IF (planx2bdyty(3,ibdyty) == i_mbs3) THEN
          !case of PLANx is carried by a RBDY3
          rdata => get_ptr_rdata_MBS3D(planx2bdyty(1,ibdyty),planx2bdyty(2,ibdyty))
          ax1 = rdata(1)
          ax2 = rdata(2)
          ax3 = rdata(3)

       ELSE
          !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
          ID_RBDY3 = planx2bdyty(1,ibdyty)
          ID_TACTY = planx2bdyty(2,ibdyty)

          !print*,'plan ',ibdyty,ID_RBDY3,ID_TACTY

          CALL get_data(ID_RBDY3,ID_TACTY,DATA)

          ax1 = DATA(1)
          ax2 = DATA(2)
          ax3 = DATA(3)
       END IF
       
       !* pseudo radius

       radius = SQRT(ax1*ax1+ax2*ax2+ax3*ax3)

       min_radius  = MIN(radius,min_radius)
       max_radius  = MAX(radius,max_radius)

       mean_radius = mean_radius + radius

       !!* Definition of the PLANx embeded frame

       !EmbededFrame = get_embeded_frame_PLANx(ibdyty)
       !localframe   = get_inertia_frameIni_PLANx(ibdyty)

       !EmbededFrame = MATMUL(transpose33(localframe),EmbededFrame)

       !CALL put_embeded_frame_PLANx(ibdyty,EmbededFrame)

       !write(*,'(3(1x,D12.5))') localframe
       !print*,'---'
       !write(*,'(3(1x,D12.5))') embededframe

    END DO

    mean_radius = mean_radius/REAL(nb_PLANx,8)
   
  END SUBROUTINE read_bodies_PLANx
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_PLANx(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome
      
    get_nb_PLANx = nb_PLANx
  
  END FUNCTION get_nb_PLANx
!!!------------------------------------------------------------------------   
  REAL(kind=8) FUNCTION get_mean_radius_PLANx(fantome)

    IMPLICIT NONE 
    REAL(kind=8),OPTIONAL     :: fantome

    get_mean_radius_PLANx = mean_radius_PLANx

  END FUNCTION get_mean_radius_PLANx
!!!------------------------------------------------------------------------   
  REAL(kind=8) FUNCTION get_max_radius_PLANx(fantome)
    
    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_max_radius_PLANx = max_radius

  END FUNCTION get_max_radius_PLANx
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_min_radius_PLANx(fantome)
    
    IMPLICIT NONE   
    REAL(kind=8),OPTIONAL :: fantome
 
    get_min_radius_PLANx=min_radius

  END FUNCTION get_min_radius_PLANx
!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_PLANx(itact)

    IMPLICIT NONE
    INTEGER      :: itact 

    IF (planx2bdyty(3,itact) == i_mbs3) THEN
       !case of PLANx is carried by a MBS
       get_color_PLANx = get_color_MBS3D(planx2bdyty(1,itact),planx2bdyty(2,itact))
    ELSE
       !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
       get_color_PLANx = get_color(planx2bdyty(1,itact),planx2bdyty(2,itact))
    END IF

  END FUNCTION get_color_PLANx
!!!------------------------------------------------------------------------

  !fd
  ! cette fonction n'a rien a foutre la
  ! il faut utiliser planx2bdyty + get_behav_RBDY3
  
  ! CHARACTER(len=5) FUNCTION get_behav_PLANx(itact)
  !   implicit none
  !   integer(kind=4)  :: itacty
  !   CHARACTER(len=80):: cout
    
  !   get_behav_PLANx = 'nknow'
    
  !   IF (planx2bdyty(3,itact) /= i_mbs3) THEN
  !      !case of PLANx is NOT carried by a MBS
  !      get_behav_PLANx = get_behav_RBDY3(planx2bdyty(1,itact))
    
  !   ELSE
  !      !case of PLANx is carried by a MBS
  !      WRITE(cout,'(A)') "PLANx::get_behav_PLANx on a mbs return 'uknow' behav"
  !      CALL LOGMES(cout)
  !   END IF
    
  ! END FUNCTION
  
!!!------------------------------------------------------------------------   
  FUNCTION get_axes_PLANx(itact)

    IMPLICIT NONE
    INTEGER                   :: itact
    REAL(kind=8),DIMENSION(3) :: get_axes_PLANx
    
    REAL(kind=8),DIMENSION(:),pointer :: rdata
    
    IF (planx2bdyty(3,itact) == i_mbs3) THEN
       !case of PLANx is carried by a MBS
       rdata => get_ptr_rdata_MBS3D(planx2bdyty(1,itact),planx2bdyty(2,itact))
       get_axes_PLANx(1) = rdata(1)
       get_axes_PLANx(2) = rdata(2)
       get_axes_PLANx(3) = rdata(3)

    ELSE
       !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
       CALL get_data(planx2bdyty(1,itact),planx2bdyty(2,itact),get_axes_PLANx)
    END IF
    

  END FUNCTION get_axes_PLANx
!!!------------------------------------------------------------------------   

  !> \brief use anonymous data to initialize a contactor
  subroutine set_data_PLANx(idata, rdata, node_list, nb_support, support, Brd)
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

    ! 8 points defines the PLANx
    nb_support = 8
    allocate( support(nbDIME,nb_support) )

    support(1,1) = -rdata(1) ; support(2,1) = -rdata(2) ; support(3,1) = -rdata(3)
    support(1,2) =  rdata(1) ; support(2,2) = -rdata(2) ; support(3,2) = -rdata(3)
    support(1,3) =  rdata(1) ; support(2,3) =  rdata(2) ; support(3,3) = -rdata(3)
    support(1,4) = -rdata(1) ; support(2,4) =  rdata(2) ; support(3,4) = -rdata(3)
    support(1,5) = -rdata(1) ; support(2,5) = -rdata(2) ; support(3,5) =  rdata(3)
    support(1,6) =  rdata(1) ; support(2,6) = -rdata(2) ; support(3,6) =  rdata(3)
    support(1,7) =  rdata(1) ; support(2,7) =  rdata(2) ; support(3,7) =  rdata(3)
    support(1,8) = -rdata(1) ; support(2,8) =  rdata(2) ; support(3,8) =  rdata(3)

    ! when we will want to have a cluster of planx...
    !if( size(data) == 6 ) then !there is a shift
    !  support(1,:) = rdata(4) + support(1,:)
    !  support(2,:) = rdata(5) + support(2,:)
    !  support(3,:) = rdata(6) + support(3,:)
    !else if( size(data) == 12 ) then !there is a frame
    !  do i = 1, nb_support
    !    support(1:3,i) = matmul( reshape(rdata(4:12),shape=(/3,3/)), support(1:3,i) )
    !  end do
    !else if( size(data) == 15 ) then !there is a shift and a frame
    !  do i = 1, nb_support
    !    support(1:3,i) = rdata(4:6) + matmul( reshape(rdata(7:15),shape=(/3,3/)), support(1:3,i) )
    !  end do
    !end if

    Brd = maxval(rdata(1:3))

  end subroutine

!!! for vtk visu !!!

 function get_ptr_PLANx2BDYTY()
   implicit none

   integer(kind=4),dimension(:,:),pointer :: get_ptr_PLANx2BDYTY

   get_ptr_PLANx2BDYTY => planx2bdyty

 end function get_ptr_PLANx2BDYTY
!------------------------------------------------------------------------
 integer function get_ENT_PLANx(itacty)
    implicit none
    integer(kind=4) :: itacty
    
    IF (planx2bdyty(3,itacty) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        get_ENT_PLANx = get_ENT_MBS3D(planx2bdyty(1,itacty))
        
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_ENT_PLANx = get_ENT_RBDY3(planx2bdyty(1,itacty))
    END IF
 
 end function

 !------------------------------------------------------------------------
 function get_X_PLANx(itact)
   implicit none
   integer(kind=4) :: itact
   real(kind=8), dimension(3) :: get_X_PLANx

   IF (planx2bdyty(3,itact) == i_mbs3) THEN
      !case of PLANx is carried by a MBS
      get_X_PLANx = 0.0
     
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_X_PLANx = get_X_RBDY3(planx2bdyty(1,itact))
   END IF
   
 end function
!------------------------------------------------------------------------
 function get_cooref_PLANx(itacty)
    IMPLICIT NONE
    INTEGER    :: itacty
    REAL(kind=8),DIMENSION(3) :: get_cooref_PLANx
    
    CHARACTER(len=80) :: cout
    CHARACTER(len=23) :: IAM
    !      12345678901234567890123
    IAM = "PLANx::get_cooref_PLANx"
    
    IF (planx2bdyty(3,itacty) == i_mbs3) THEN
      !case of PLANx is carried by a MBS
      WRITE(cout,'(A)') "Function not implemented with mbs body."
      CALL faterr(IAM,cout)
     
    ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_cooref_PLANx = get_cooref_RBDY3(planx2bdyty(1,itacty), planx2bdyty(2,itacty))
    END IF
    
 end function
!------------------------------------------------------------------------
 function get_V_PLANx(itact)
   implicit none
   integer(kind=4) :: itact
   real(kind=8), dimension(6) :: get_V_PLANx

   IF (planx2bdyty(3,itact) == i_mbs3) THEN
      !case of PLANx is carried by a MBS
      get_V_PLANx = 0.0
     
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_V_PLANx = get_V_RBDY3(planx2bdyty(1,itact))
   END IF

 end function
!------------------------------------------------------------------------
 function get_vlocy_PLANx(itacty,istate)
    implicit none
    integer :: itacty,istate
    REAL(kind=8),DIMENSION(6) :: get_vlocy_PLANx
    
    IF (planx2bdyty(3,itacty) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        get_vlocy_PLANx = get_vlocy_MBS3D(planx2bdyty(1,itacty), planx2bdyty(2,itacty), istate)
     
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_vlocy_PLANx = get_vlocy_RBDY3(planx2bdyty(1,itacty), istate)
   END IF
    
 end function
!------------------------------------------------------------------------
 function get_reac_PLANx(itact)
   implicit none
   integer(kind=4) :: itact
   real(kind=8), dimension(6) :: get_reac_PLANx

   IF (planx2bdyty(3,itact) == i_mbs3) THEN
      !case of PLANx is carried by a MBS
      get_reac_PLANx = 0.0
     
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_reac_PLANx = get_reac_RBDY3(planx2bdyty(1,itact))
   END IF

 end function
!------------------------------------------------------------------------
 function get_Xbegin_PLANx(itact)
   implicit none
   integer(kind=4) :: itact
   real(kind=8), dimension(3) :: get_Xbegin_PLANx
   
   IF (planx2bdyty(3,itact) == i_mbs3) then
      !case of PLANx is carried by a MBS
      get_Xbegin_PLANx = 0.0
      
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_Xbegin_PLANx = get_Xbegin_RBDY3(planx2bdyty(1,itact))
   END IF
 end function
!------------------------------------------------------------------------
 function get_coor_PLANx(itacty)
   implicit none
   integer(kind=4) :: itacty, ibdy, itact
   real(kind=8), dimension(3) :: get_coor_PLANx
   
   ibdy  = planx2bdyty(1,itacty)
   itact = planx2bdyty(2,itacty)
   IF (planx2bdyty(3,itacty) == i_mbs3) then
      !case of PLANx is carried by a MBS
      get_coor_PLANx = get_coor_MBS3D(ibdy,itact)
      
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_coor_PLANx = get_coor_RBDY3(ibdy, itact)
   END IF
 
 end function
!------------------------------------------------------------------------
 function get_coorTT_PLANx(itact)
   implicit none
   integer(kind=4) :: itact
   real(kind=8), dimension(3) :: get_coorTT_PLANx
   
   IF (planx2bdyty(3,itact) == i_mbs3) then
      !case of PLANx is carried by a MBS
      get_coorTT_PLANx = get_coorTT_MBS3D(planx2bdyty(1,itact),planx2bdyty(2,itact))
      
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_coorTT_PLANx = get_coorTT_RBDY3(planx2bdyty(1,itact),planx2bdyty(2,itact))
   END IF

 end function
!------------------------------------------------------------------------
 function get_inertia_frame_PLANx(itacty)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,3) :: get_inertia_frame_PLANx
   
   IF (planx2bdyty(3,itacty) == i_mbs3) then
      !case of PLANx is carried by a MBS
      get_inertia_frame_PLANx = get_inertia_frame_MBS3D(planx2bdyty(1,itacty),planx2bdyty(2,itacty))
      
   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_inertia_frame_PLANx = get_inertia_frame_RBDY3(planx2bdyty(1,itacty))
   END IF
   
 end function
!------------------------------------------------------------------------
 function get_inertia_frameTT_PLANx(itacty)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,3) :: get_inertia_frameTT_PLANx
   
   IF (planx2bdyty(3,itacty) == i_mbs3) then
      !case of PLANx is carried by a MBS
      get_inertia_frameTT_PLANx = get_inertia_frameTT_MBS3D(planx2bdyty(1,itacty),planx2bdyty(2,itacty))

   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_inertia_frameTT_PLANx = get_inertia_frameTT_RBDY3(planx2bdyty(1,itacty))
   END IF

 end function
!------------------------------------------------------------------------
 function get_inertia_frameIni_PLANx(itacty)
    implicit none
    integer(kind=4) :: itacty
    real(kind=8),dimension(3,3) :: get_inertia_frameIni_PLANx
    
    CHARACTER(len=80) :: cout
    CHARACTER(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = "PLANx::get_inertia_frameIni_PLANx"
   
    IF (planx2bdyty(3,itacty) == i_mbs3) then
        !case of PLANx is carried by a MBS
        WRITE(cout,'(A)') "Function not implemented with mbs body."
        CALL faterr(IAM,cout)
        
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_inertia_frameIni_PLANx = get_inertia_frameIni_RBDY3(planx2bdyty(1,itacty))
    END IF
   
 end function
!------------------------------------------------------------------------
 function get_embeded_frame_PLANx(itacty)
    implicit none
    integer(kind=4) :: itacty
    real(kind=8), dimension(3,3) :: get_embeded_frame_PLANx
    
    IF (planx2bdyty(3,itacty) == i_mbs3) then
        !case of PLANx is carried by a MBS
        get_embeded_frame_PLANx = get_embeded_frame_MBS3D(planx2bdyty(1,itacty), planx2bdyty(2,itacty))

    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_embeded_frame_PLANx = get_embeded_frame_RBDY3(planx2bdyty(1,itacty), planx2bdyty(2,itacty))
    END IF

 end function
!------------------------------------------------------------------------
 function get_WS_PLANx(itacty)
    implicit none
    integer(kind=4) :: itacty
    real(kind=8)    :: get_WS_PLANx
    
    CHARACTER(len=80) :: cout
    CHARACTER(len=19) :: IAM
    !      1234567890123456789
    IAM = "PLANx::get_WS_PLANx"
    
    IF (planx2bdyty(3,itacty) == i_mbs3) then
        !case of PLANx is carried by a MBS
        WRITE(cout,'(A)') "Function not implemented with mbs body."
        CALL faterr(IAM,cout)
        
   ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_WS_PLANx = get_WS_RBDY3(planx2bdyty(1,itacty), planx2bdyty(2,itacty))
   END IF
    
 end function
!------------------------------------------------------------------------
 function get_shiftTT_PLANx(itacty)
    implicit none
    integer(kind=4) :: itacty
    real(kind=8), dimension(3) :: get_shiftTT_PLANx
    
    IF (planx2bdyty(3,itacty) == i_mbs3) then
        !case of PLANx is carried by a MBS
        get_shiftTT_PLANx = get_shiftTT_MBS3D(planx2bdyty(1,itacty), planx2bdyty(2,itacty))
        
   ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_shiftTT_PLANx = get_shiftTT_RBDY3(planx2bdyty(1,itacty), planx2bdyty(2,itacty))
   END IF
    
 end function


 ! !------------------------------------------------------------------------
 ! integer function get_nb_point_outline_PLANx(fantome)

 !   implicit none
 !   integer(kind=4),optional :: fantome

 !   get_nb_point_outline_PLANx = nbpto

 ! end function get_nb_point_outline_PLANx 

 !------------------------------------------------------------------------ 
 subroutine updt_outline_PLANx(itacty,outline)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(3,nbpto) :: outline
   !
   integer(kind=4)             :: k
   real(kind=8),dimension(3)   :: X, vertex_ref
   real(kind=8),dimension(3,3) :: frame, lframe
   real(kind=8),dimension(3)   :: data
   
   REAL(kind=8),DIMENSION(:),pointer :: rdata

   !fd    
   !fd get_coor_PLANx gere le shift
   !fd 
   IF (planx2bdyty(3,itacty) == i_mbs3) THEN
      !case of PLANx is carried by a MBS
      X     = get_coor_MBS3D(planx2bdyty(1,itacty),planx2bdyty(2,itacty))
      frame = get_inertia_frame_MBS3D(planx2bdyty(1,itacty),planx2bdyty(2,itacty))
      lframe= get_embeded_frame_MBS3D(planx2bdyty(1,itacty),planx2bdyty(2,itacty))
      
      frame = matmul(frame,lframe)
      
      rdata => get_ptr_rdata_MBS3D(planx2bdyty(1,itacty),planx2bdyty(2,itacty))
      data(1) = rdata(1)
      data(2) = rdata(2)
      data(3) = rdata(3)

   ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      X     = get_coor_PLANx(itacty)
      frame = get_inertia_frame_PLANx(itacty)
      lframe= get_embeded_frame_PLANx(itacty)

      frame = matmul(frame,lframe)

      call get_data(planx2bdyty(1,itacty),planx2bdyty(2,itacty),data)
   
   END IF

   do k = 1, nbpto
      vertex_ref(1:3) = unit_plan(1:3,k) * data(1:3)
 
      outline(1,k) = dot_product(vertex_ref(1:3),frame(1,1:3)) + X(1)
      outline(2,k) = dot_product(vertex_ref(1:3),frame(2,1:3)) + X(2)
      outline(3,k) = dot_product(vertex_ref(1:3),frame(3,1:3)) + X(3)

   end do

 end subroutine updt_outline_PLANx

 !------------------------------------------------------------------------
 function get_nb_point_outlines_PLANx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_nb_point_outlines_PLANx

   get_nb_point_outlines_PLANx => nb_point_outlines_PLANx

 end function get_nb_point_outlines_PLANx

 !------------------------------------------------------------------------
 function init_outlines_PLANx()
   implicit none
   integer(kind=4) :: itacty,sz
   real(kind=8),dimension(:,:),pointer :: init_outlines_PLANx

   if ( nb_PLANx .eq. 0 ) then
     init_outlines_PLANx => null()
     return
   endif 

   if (associated(nb_point_outlines_PLANx)) deallocate(nb_point_outlines_PLANx)
   allocate(nb_point_outlines_PLANx(nb_PLANx+1)) 
   nb_point_outlines_PLANx(1) = 0
   do itacty = 1, nb_PLANx
     nb_point_outlines_PLANx(itacty+1) = nb_point_outlines_PLANx(itacty) + nbpto
   end do

   sz =  nb_point_outlines_PLANx(nb_PLANx+1)

   if (associated(outlines_PLANx)) deallocate(outlines_PLANx)
   allocate(outlines_PLANx(3,sz)) 

   outlines_PLANx(1:3,1:sz) = 0.d0

   init_outlines_PLANx => outlines_PLANx

   !storing connectivities of each planx
   ! 1/ sizing : 6 quad 
   !sz = 0
   !do itacty = 1, nb_CYLND
   !  sz = sz + nb_cells*5 + 1
   !end do
   sz = nb_PLANx * ( 6*5 + 1 )

   if( associated(all_connectivities) ) deallocate(all_connectivities)
   allocate(all_connectivities(sz))

   ! 2/ filling
   sz = 1
   do itacty = 1, nb_PLANx
     !bottom
     all_connectivities(sz) = 6
     sz = sz+1
     all_connectivities(sz) = 4
     all_connectivities(sz+1:sz+4) = (/1,2,3,4/)
     sz = sz+5
     !bottom
     all_connectivities(sz) = 4
     all_connectivities(sz+1:sz+4) = (/5,6,7,8/)
     sz = sz+5
     !left
     all_connectivities(sz) = 4
     all_connectivities(sz+1:sz+4) = (/1,2,6,5/)
     sz = sz+5
     !right
     all_connectivities(sz) = 4
     all_connectivities(sz+1:sz+4) = (/3,4,8,7/)
     sz = sz+5
     !front
     all_connectivities(sz) = 4
     all_connectivities(sz+1:sz+4) = (/2,3,7,6/)
     sz = sz+5
     !rear
     all_connectivities(sz) = 4
     all_connectivities(sz+1:sz+4) = (/1,4,8,5/)
     sz = sz+5
   end do

   !coordinates of reference plan
   unit_plan(1:3,1) = (/-1.,-1.,-1./)
   unit_plan(1:3,2) = (/ 1.,-1.,-1./)
   unit_plan(1:3,3) = (/ 1., 1.,-1./)
   unit_plan(1:3,4) = (/-1., 1.,-1./)
   unit_plan(1:3,5) = (/-1.,-1., 1./)
   unit_plan(1:3,6) = (/ 1.,-1., 1./)
   unit_plan(1:3,7) = (/ 1., 1., 1./)
   unit_plan(1:3,8) = (/-1., 1., 1./)

 end function init_outlines_PLANx

 !------------------------------------------------------------------------
 function get_nb_scalarfields_PLANx()
   implicit none
   integer(kind=4) :: get_nb_scalarfields_PLANx

   get_nb_scalarfields_PLANx = nbsf

 end function get_nb_scalarfields_PLANx
 !------------------------------------------------------------------------
 function init_scalarfields_PLANx()
   implicit none
   integer(kind=4) :: sz
   real(kind=8),dimension(:),pointer :: init_scalarfields_PLANx

   if ( nb_PLANx .eq. 0 ) then
     init_scalarfields_PLANx => null()
     return
   endif 

   sz = nbsf * nb_PLANx

   if (associated(scalarfields_PLANx)) deallocate(scalarfields_PLANx)
   allocate(scalarfields_PLANx(sz)) 

   scalarfields_PLANx(1:sz) = 0.d0

   init_scalarfields_PLANx => scalarfields_PLANx

 end function init_scalarfields_PLANx
 
 !------------------------------------------------------------------------
 subroutine updt_scalarfield_PLANx(itacty,scalarfield)
   implicit none
   integer(kind=4) :: itacty
   real(kind=8),dimension(nbsf) :: scalarfield
   
   !mr : displacement and velocity of the center of mass
   scalarfield(1:3)   = get_X_PLANx(itacty)
   scalarfield(4:9)   = get_V_PLANx(itacty)
   scalarfield(10:15) = get_REAC_PLANx(itacty)

 end subroutine updt_scalarfield_PLANx
 

 !------------------------------------------------------------------------
 subroutine update_postdata_PLANx()
   implicit none
   integer(kind=4) :: itacty,iszo,iszsf

   if (nb_PLANx == 0) return

   if (.not. associated(outlines_PLANx) ) call faterr('PLANx::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_PLANx) ) call faterr('PLANx::update_postdata','init_scalarfields is mandatory')
   
   iszo = 0
   iszsf = 0
   do itacty=1,nb_PLANx
     call updt_outline_PLANx(itacty,outlines_PLANx(1:3,iszo+1:iszo+nbpto))
     iszo = iszo + nbpto
     call updt_scalarfield_PLANx(itacty,scalarfields_PLANx(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   enddo    

 end subroutine update_postdata_PLANx

 !------------------------------------------------------------------------ 
 function get_all_connectivities_PLANx()
   implicit none
   integer(kind=4), dimension(:), pointer :: get_all_connectivities_PLANx

   get_all_connectivities_PLANx => all_connectivities

 end function

!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_visible_PLANx(itact)

    IMPLICIT NONE
    INTEGER :: itact 
    
    IF (planx2bdyty(3,itact) == i_mbs3) THEN
      !case of PLANx is carried by a MBS
      get_visible_PLANx = .true.
     
    ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      get_visible_PLANx = get_visible(planx2bdyty(1,itact))    
    END IF    

  END FUNCTION get_visible_PLANx
!!!------------------------------------------------------------------------ 

!!!-PTA----------------------------------------------------------------------- 
  INTEGER FUNCTION get_visibleID_PLANx(itact)

    IMPLICIT NONE
    INTEGER :: itact
    
    CHARACTER(len=80) :: cout
    CHARACTER(len=26) :: IAM
    !      12345678901234567890123456
    IAM = "PLANx::get_visibleID_PLANx"
    
    IF (planx2bdyty(3,itact) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        get_visibleID_PLANx = planx2bdyty(1,itact)
     
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        get_visibleID_PLANx = get_visibleID(planx2bdyty(1,itact))
    END IF

  END FUNCTION get_visibleID_PLANx
!!!-PTA-----------------------------------------------------------------------
  subroutine put_embeded_frame_PLANx(itacty,EmbededFrame)
    implicit none
    integer(kind=4)             :: itacty
    real(kind=8),dimension(3,3) :: EmbededFrame
    
    CHARACTER(len=80) :: cout
    CHARACTER(len=30) :: IAM
    !      123456789012345678901234567890
    IAM = "PLANx::put_embeded_frame_PLANx"
    
    if (planx2bdyty(3,itacty) == i_mbs3) then
      !case of PLANx is carried by a MBS
      WRITE(cout,'(A)') "Function not implemented with mbs body."
      CALL faterr(IAM,cout)
     
    ELSE
      !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
      CALL put_embeded_frame_RBDY3(planx2bdyty(1,itacty), planx2bdyty(2,itacty),EmbededFrame)
    END IF
    
  end subroutine
!------------------------------------------------------------------------
  subroutine add_reac_PLANx(itact,xxccdof,xxreac,storage)
    implicit none
    integer :: itact,storage
    INTEGER,     DIMENSION(6)  :: xxccdof
    REAL(kind=8),DIMENSION(6)  :: xxreac

    IF (planx2bdyty(3,itact) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        CALL add_reac_MBS3D(planx2bdyty(1,itact), planx2bdyty(2,itact),xxccdof,xxreac,storage)
        
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        CALL add_reac_RBDY3(planx2bdyty(1,itact), xxccdof,xxreac,storage)
    END IF

  end subroutine  
!------------------------------------------------------------------------
  subroutine comp_vlocy_PLANx(itact,storage)  
    implicit none
    integer :: itact,storage

    IF (planx2bdyty(3,itact) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        CALL comp_vlocy_MBS3D(planx2bdyty(1,itact),storage)
        
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        CALL comp_vlocy_RBDY3(planx2bdyty(1,itact),storage)
       
    END IF

  end subroutine 
!------------------------------------------------------------------------
  subroutine nullify_vlocy_PLANx(itact,storage)  
    implicit none
    integer :: itact,storage
    
    IF (planx2bdyty(3,itact) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        CALL nullify_vlocy_MBS3D(planx2bdyty(1,itact),storage)
        
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        CALL nullify_vlocy_RBDY3(planx2bdyty(1,itact),storage)

    END IF

  end subroutine  
!------------------------------------------------------------------------
  subroutine nullify_reac_PLANx(itact,storage)  
    implicit none
    integer :: itact,storage
    
    IF (planx2bdyty(3,itact) == i_mbs3) THEN
        !case of PLANx is carried by a MBS
        CALL nullify_reac_MBS3D(planx2bdyty(1,itact),storage)
        
    ELSE
        !Other case: I don't know if i_rbdy3 is the only possibility, so I don't check it.
        CALL nullify_reac_RBDY3(planx2bdyty(1,itact),storage)

    END IF

  end subroutine  
!------------------------------------------------------------------------
 subroutine clean_memory_PLANx()
   implicit none

   nb_PLANx = 0

   if( associated(planx2bdyty) ) then
     deallocate(planx2bdyty)
     nullify(planx2bdyty)
   end if

   if( associated(nb_point_outlines_PLANx) ) then
     deallocate(nb_point_outlines_PLANx)
     nullify(nb_point_outlines_PLANx)
   end if

   if( associated(all_connectivities) ) then
     deallocate(all_connectivities)
     nullify(all_connectivities)
   end if

   if( associated(outlines_PLANx) ) then
     deallocate(outlines_PLANx)
     nullify(outlines_PLANx)
   end if

   if( associated(scalarfields_PLANx) ) then
     deallocate(scalarfields_PLANx)
     nullify(scalarfields_PLANx)
   end if

 end subroutine

END MODULE PLANx

