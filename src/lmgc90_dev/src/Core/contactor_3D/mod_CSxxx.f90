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
 !> manages candidate nodes on a 3D surface on a mecaMAILx
 MODULE CSxxx                                       

 USE utilities

 USE overall
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx
  
 use algebra
 USE DiscreteGeometry
 USE a_EF
 USE xSpxx

 IMPLICIT NONE
 
 PRIVATE

 !fd new 24/01/2012
 ! In order to keep the topology of contact zones
 ! the internal data structure rely on xSpxx patches of element which
 ! is binded to the interaction (private).
 ! a public "fictitious" list of contact nodes is proposed for managing interactions.

 ! a voir si interp/weight ne doivent pas s'appuyer sur tous les noeuds du patch comme
 ! ca serait le cas avec les mortar

 type :: T_CSxxx

   ! id of CSpxx it belongs
   integer                              :: iCSpxx
   ! index of CS inside CSp
   integer                              :: iCSsci
   ! if /=0 id of the corresponding mesh node, if == 0 is located on an element
   integer                              :: inode
   ! if /=0 id of the corresponding element (xSpxx%face), if == 0 is located on a node
   integer                              :: ielement
   ! the corresponding local node (1 if attach to node, 3 or 4 if attached to element)
   integer,dimension(:),pointer         :: tnode
   ! interpolation on nodes mesh node
   real(kind=8),dimension(:),pointer    :: interp
   ! derive interpolation on nodes mesh node
   real(kind=8),dimension(:,:),pointer  :: d_interp
   ! integration weight of node
   real(kind=8)                         :: weight
   ! surf of node
   real(kind=8)                         :: surf
   ! tosi
   integer                              :: iblmty, igp

 end type T_CSxxx

 INTEGER :: nb_CSxxx

 TYPE(T_CSxxx), DIMENSION(:), ALLOCATABLE :: l_CSxxx

 integer, dimension( : , : ), allocatable, &
      target, public :: csxxx2bdyty  ! csxxx2bdyty(1,itac): 
                                     ! serial number of body MAILx to which is attached the contactor 
                                     ! CSpxx numbered itac in the list of all contactors CSpxx 
                                     ! csxxx2bdyty(2,itac): 
                                     ! serial number of contactor CSpxx in the list of contactors for the fictitious itac CSxxx  
                                     ! of any kind attached to a body cspxx2bdyty(1,itac)


 ! private date structures

 INTEGER :: nb_CSpxx

 TYPE(T_xSpxx), DIMENSION(:), ALLOCATABLE :: l_CSpxx

!fd type permettant de stocker les valeurs en un point de gauss de:
! les fonctions de forme de l'element de bord
! les derivees des fonctions de forme de l'element volumique s'adossant a l'element surfacique  
! ces dernieres valeurs sont necessaires au calcul d'intégrales de bord (calcul du jacobien) 
type, private :: T_interp
  ! nbn
  real(kind=8),dimension(:),pointer :: N
  ! dim,nbn
  real(kind=8),dimension(:,:),pointer :: DN
end type T_interp


 !  
 ! public routines
 PUBLIC load_tactors_CSxxx, &
        set_precon_node_CSxxx

 PUBLIC get_nb_CSxxx, &
        get_sci_CSxxx, &
        get_ENT_CSxxx, &
        get_color_CSxxx,&
        get_visible_CSxxx,&
        nullify_reac_CSxxx, &
        nullify_vlocy_CSxxx, &
        comp_vlocy_CSxxx,&
        add_reac_CSxxx, &
        get_vlocy_CSxxx, &
        get_coorTT_CSxxx, &
        get_normalTT_CSxxx, &
        get_surf_CSxxx, &
        get_weight_CSxxx, &
        flip_orientation_CSxxx, &
        flip_orientation_one_CSpxx, &
        set_shrink_CSxxx, &
        increment_CSpxx, &
        apply_pressure_CSpxx, &
        interpolate_reac_CSpxx, &
        get_nodes_CSxxx, &
        get_dof_CSxxx, &
        get_interp_CSxxx, &
        apply_surface_load_CSpxx, &
        get_connec_CSpxx  , &
        get_all_data_CSpxx, &
        all_dof_driven_CSxxx, &
        ! xper
        get_bulk_strain_triaxiality_csxxx, &
        get_bulk_stress_triaxiality_csxxx

 public clean_memory_CSxxx

!        get_Vwear_CSxxx, &
!        put_Vwear_CSxxx, &

 ! ?
 real(kind=8) :: CS_shrink = 1.d0

!TODO le pb du restart 

CONTAINS

!!!------------------------------------------------------------------------

  SUBROUTINE load_tactors_CSxxx

    IMPLICIT NONE

                                      !1234567890123456789
    CHARACTER(len=19),parameter:: IAM='CSxxx::load_tactors'

    INTEGER :: nb_MAILx
    INTEGER :: ibdyty,iblmty,itacty,igp,errare
    CHARACTER(len=103) :: cout

    integer,dimension(:,:),allocatable :: cspxx2bdyty

    !fd les coordonnees des noeuds d'un element de bord (dim,nbn)
    real(kind=8),target :: coor_3(3,3),coor_4(3,4),coor_8(3,8),coor_6(3,6)

    ! gp coordinates and weights
    integer                             :: nbgp_3,nbgp_4,nbgp_8,nbgp_6, nbgp
    real(kind=8),dimension(:,:),pointer ::  gpc_3, gpc_4, gpc_8, gpc_6, gpc, coor
    real(kind=8),dimension(:)  ,pointer ::  gpw_3, gpw_4, gpw_8, gpw_6, gpw

    !> tableau pour tous les points de gauss de l'element de bord
    type(T_interp),dimension(:), pointer:: interp_3,interp_4,interp_8,interp_6, interp

    integer      :: i,j,id,iCSpxx,ixSxxx,iCSxxx,nbv_bf,quad_id, i_sci

    real(kind=8),dimension(:),allocatable :: nodal_surface

    real(kind=8) :: coefint,detj
    real(kind=8), dimension(3) :: v1, v2, normal, coor_i

    integer(kind=4), dimension(3), target  :: support_3 = (/1,2,3/)
    integer(kind=4), dimension(6), target  :: support_6 = (/1,2,3,4,5,6/)
    integer(kind=4), dimension(4), target  :: support_4 = (/1,2,3,4/)
    integer(kind=4), dimension(8), target  :: support_8 = (/1,2,3,4,9,10,11,12/)
    integer(kind=4), dimension(:), pointer :: support, inodes

    interp => null()

    nb_MAILx=get_nb_MAILx()

    ! public
    nb_CSxxx=0    

    ! private
    nb_CSpxx=0    

    IF (nb_MAILx == 0) RETURN
   
    !fd cuisine interne

    DO ibdyty=1,nb_MAILx   
       DO itacty=1,get_nb_tacty_MAILx(ibdyty)
          IF( get_tacID_MAILx(ibdyty,itacty) == 'CSpxx' .or. &
              get_tacID_MAILx(ibdyty,itacty) == 'CSpx0' .or. &
              get_tacID_MAILx(ibdyty,itacty) == 'CSpx1' .or. &
              get_tacID_MAILx(ibdyty,itacty) == 'CSpx2')  nb_CSpxx=nb_CSpxx+1
       END DO
    END DO
    
    WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_CSpxx,'CSpxx found'
    CALL LOGMES(cout)
    
    IF (nb_CSpxx == 0) RETURN
    
    ALLOCATE(l_CSpxx(nb_CSpxx),stat=errare)
    
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating l_CSpxx')
    END IF

    allocate(cspxx2bdyty(2,nb_CSpxx),stat=errare)
    
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating cspxx2bdyty')
    END IF
    
    ! print*,'on commence a charger ...' 

    if( .not. allocated(M2meca) ) then
      call faterr(IAM,'Please call LoadModels before LoadTactors')
    end if
    call load_tactors_xSpxx('C',l_CSpxx,cspxx2bdyty)

    ! print*,'...ok'
    ! cuisine pour l'externe

    do iCSpxx=1,nb_CSpxx
      if( l_CSpxx(iCSpxx)%quadrature < 0 ) then
        ! contact aux noeudx
        nb_CSxxx = nb_CSxxx + l_CSpxx(iCSpxx)%nb_vertex
      else

        do ixSxxx = 1, l_CSpxx(iCSpxx)%nb_xSxxx
          select case( l_CSpxx(iCSpxx)%nb_vertex_bf(ixSxxx) )
          case(3, 6)
            select case( l_CSpxx(iCSpxx)%quadrature )
            case(0)
              quad_id = i_tr01
            case(1)
              quad_id = i_tr03
            case(2)
              quad_id = i_tr06
            case default
            end select
          case(4, 8)
            select case( l_CSpxx(iCSpxx)%quadrature )
            case(0)
              quad_id = i_q1x1
            case(1)
              quad_id = i_q2x2
            case(2)
              quad_id = i_q3x3
            case default
            end select
          case default
            call FATERR(IAM,'wrong number of nodes of the xSxxx element')
          end select

          nb_CSxxx = nb_CSxxx + get_nb_points_from_quadrature(quad_id)

        end do

      end if
    end do

    write(cout,*) 'nb_CSxxx',nb_CSxxx
    call logmes(cout)

    allocate(l_CSxxx(nb_CSxxx),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating l_CSxxx')
    END IF

    ALLOCATE(csxxx2bdyty(3,nb_CSxxx),stat=errare)
    
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating csxxx2bdyty')
    END IF

    nullify(gpc_3,gpw_3,gpc_6,gpw_6,gpc_4,gpw_4,gpc_8,gpw_8)
    nullify(interp_3,interp_6,interp_4,interp_8)

    j=0
    do iCSpxx=1,nb_CSpxx

      ! cas des CSxxx portes par les noeuds
      if( l_CSpxx(iCSpxx)%quadrature < 0 ) then
        allocate(nodal_surface(l_CSpxx(iCSpxx)%nb_vertex))
        call compute_nodal_resultant_of_constant_field(l_CSpxx(iCSpxx),1.d0,nodal_surface)
        
        !print*,nodal_surface

        do i=1,l_CSpxx(iCSpxx)%nb_vertex   
          iCSxxx = j + i
          l_CSxxx(iCSxxx)%iCSpxx = iCSpxx
          l_CSxxx(iCSxxx)%iCSsci = i
          l_CSxxx(iCSxxx)%inode  = l_CSpxx(iCSpxx)%vertex2node(i)
          l_CSxxx(iCSxxx)%ielement = 0
          allocate(l_CSxxx(iCSxxx)%interp(1))
          l_CSxxx(iCSxxx)%interp = 1.d0
          l_CSxxx(iCSxxx)%surf = nodal_surface(i)
          !!!!
          ! si pression
          ! l_CSxxx(iCSxxx)%weight = nodal_surface(i)
          ! si force 
          l_CSxxx(iCSxxx)%weight = 1.d0
          !!!!
          l_CSxxx(iCSxxx)%iblmty = 0
          l_CSxxx(iCSxxx)%igp    = 0
          !
          csxxx2bdyty(1,iCSxxx) = cspxx2bdyty(1,iCSpxx) 
          csxxx2bdyty(2,iCSxxx) = cspxx2bdyty(2,iCSpxx)
        enddo

        j = j + l_CSpxx(iCSpxx)%nb_vertex

        deallocate(nodal_surface)

      ! cas des CSxxx portes par les elements
      else

        ! compute form functions only if quadrature changed
        select case( l_CSpxx(iCSpxx)%quadrature )
        case(0)
          call compute_inter(i_T_P1,i_tr01,nbgp_3,gpc_3,gpw_3,interp_3)
          call compute_inter(i_T_P2,i_tr01,nbgp_6,gpc_6,gpw_6,interp_6)
          call compute_inter(i_Q_P1,i_q1x1,nbgp_4,gpc_4,gpw_4,interp_4)
          call compute_inter(i_Q_P2,i_q1x1,nbgp_8,gpc_8,gpw_8,interp_8)
        case(1)
          call compute_inter(i_T_P1,i_tr03,nbgp_3,gpc_3,gpw_3,interp_3)
          call compute_inter(i_T_P2,i_tr03,nbgp_6,gpc_6,gpw_6,interp_6)
          call compute_inter(i_Q_P1,i_q2x2,nbgp_4,gpc_4,gpw_4,interp_4)
          call compute_inter(i_Q_P2,i_q2x2,nbgp_8,gpc_8,gpw_8,interp_8)
        case(2)
          call compute_inter(i_T_P1,i_tr06,nbgp_3,gpc_3,gpw_3,interp_3)
          call compute_inter(i_T_P2,i_tr06,nbgp_6,gpc_6,gpw_6,interp_6)
          call compute_inter(i_Q_P1,i_q3x3,nbgp_4,gpc_4,gpw_4,interp_4)
          call compute_inter(i_Q_P2,i_q3x3,nbgp_8,gpc_8,gpw_8,interp_8)
        end select

        ibdyty = l_CSpxx(iCSpxx)%ibdyty
        i_sci = 0
        do ixSxxx=1,l_CSpxx(iCSpxx)%nb_xSxxx

          nbv_bf = l_CSpxx(iCSpxx)%nb_vertex_bf(ixSxxx)

          ! selecting form functions
          select case( nbv_bf )
          case(3)
            nbgp    =  nbgp_3
            gpc     => gpc_3
            gpw     => gpw_3
            coor    => coor_3
            interp  => interp_3
            support => support_3
          case(6)
            nbgp    =  nbgp_6
            gpc     => gpc_6
            gpw     => gpw_6
            coor    => coor_6
            interp  => interp_6
            support => support_6
          case(4)
            nbgp    =  nbgp_4
            gpc     => gpc_4
            gpw     => gpw_4
            coor    => coor_4
            interp  => interp_4
            support => support_4
          case(8)
            nbgp    =  nbgp_8
            gpc     => gpc_8
            gpw     => gpw_8
            coor    => coor_8
            interp  => interp_8
            support => support_8
          end select

          inodes => l_CSpxx(iCSpxx)%face(:,ixSxxx)
          do i = 1, nbv_bf
            coor(:,i)=get_cooref_nodty_mecaMAILx(ibdyty,inodes(i))
          end do

          ! rm: look for blmty support
          !     here the identity of ibdyty/M_ibdyty is relied on
          !     ... not such a good idea
          call get_blmty_from_nodes_MAILx(ibdyty, inodes(1:nbv_bf), iblmty)

          do i = 1, nbgp
            iCSxxx = j + i
            i_sci = i_sci + 1 ! number of vertex in patch
            l_CSxxx(iCSxxx)%iCSpxx = iCSpxx
            l_CSxxx(iCSxxx)%iCSsci = i_sci
            l_CSxxx(iCSxxx)%inode  = 0
            l_CSxxx(iCSxxx)%ielement = ixSxxx
            allocate(l_CSxxx(iCSxxx)%interp(nbv_bf))
            l_CSxxx(iCSxxx)%interp = interp(i)%N
            allocate(l_CSxxx(iCSxxx)%d_interp(2,nbv_bf))
            l_CSxxx(iCSxxx)%d_interp(1,:) = interp(i)%DN(1,support)
            l_CSxxx(iCSxxx)%d_interp(2,:) = interp(i)%DN(2,support)
            !support = 1:3, 1:4, [1:4,9:12], :

            ! on calcule :
            !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
            !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
            v1=0.d0
            v2=0.d0
            !calcul des composantes de v1 & v2 (calcule sur la face du bas 1:3)
            do id = 1, 3
              v1(id) = dot_product(l_CSxxx(iCSxxx)%d_interp(1,:), coor(id,:))
              v2(id) = dot_product(l_CSxxx(iCSxxx)%d_interp(2,:), coor(id,:))
            end do

            ! on en deduit det(J) = J_S = | v1 x v2 |
            detJ = length3(cross_product(v1, v2)) 

            ! on peut alors calculer : w_PG*det(J)

            ! print*,'< ',IAM
            ! print*,'gp     ',i
            ! print*,'v1     ',v1
            ! print*,'v2     ',v2
            ! print*,'detj   ',detj
            ! print*,'w_pg   ',gpw(i)
            ! print*,'interp ',l_CSxxx(iCSxxx)%interp
            ! print*,IAM,' >'

            ! contact surface
            l_CSxxx(iCSxxx)%surf   = DETJ*gpw(i)

            !!!!
            ! si pression 
            ! l_CSxxx(iCSxxx)%weight = DETJ*gpw(i)
            ! si force
            l_CSxxx(iCSxxx)%weight = 1.d0
            !!!!
            l_CSxxx(iCSxxx)%iblmty = iblmty
            do id = 1, 3
              coor_i(id) = dot_product( interp(i)%N, coor(id,:) )
            end do
            call get_nearest_gp_mecaMAILx(ibdyty,iblmty,coor_i,igp)
            l_CSxxx(iCSxxx)%igp   = igp

            csxxx2bdyty(1,iCSxxx) = cspxx2bdyty(1,iCSpxx) 
            csxxx2bdyty(2,iCSxxx) = cspxx2bdyty(2,iCSpxx)
          end do
          j = j + nbgp

        end do
      end if
    end do 

    if (j /= nb_CSxxx) then
      call FATERR(IAM,'mismatch in counting nb_CSxxx')
    endif

    csxxx2bdyty(3,:) = i_mailx

    if( associated(interp) ) then
      do i=1,nbgp_3    
        deallocate(interp_3(i)%N,interp_3(i)%DN)
      enddo
      deallocate (interp_3,gpc_3,gpw_3)
      
      do i=1,nbgp_6    
        deallocate(interp_6(i)%N,interp_6(i)%DN)
      enddo
      deallocate (interp_6,gpc_6,gpw_6)

      do i=1,nbgp_4    
        deallocate(interp_4(i)%N,interp_4(i)%DN)
      enddo
      deallocate (interp_4,gpc_4,gpw_4)
      
      do i=1,nbgp_8    
        deallocate(interp_8(i)%N,interp_8(i)%DN)
      enddo
      deallocate (interp_8,gpc_8,gpw_8)
      nullify(gpc_3,gpw_3,gpc_6,gpw_6,gpc_4,gpw_4,gpc_8,gpw_8)
      nullify(interp_3,interp_6,interp_4,interp_8)
      nullify(gpc,gpw,interp)
    end if

    ! do iCSxxx=1,nb_CSxxx
    !  write(*,'(I0,1x,I0,1x,I0)') iCSxxx,l_CSxxx(iCSxxx)%iCSpxx,l_CSpxx(l_CSxxx(iCSxxx)%iCSpxx)%ibdyty
    !  if (l_CSxxx(iCSxxx)%inode /= 0) then
    !    write(*,'(I0)') l_CSxxx(iCSxxx)%inode
    !  else
    !    write(*,'(4(I0,1x))') l_CSpxx(l_CSxxx(iCSxxx)%iCSpxx)%face(:,l_CSxxx(iCSxxx)%ielement)
    !  endif
    !  write(*,'(4(1x,D12.5))') l_CSxxx(iCSxxx)%interp
    !  write(*,'(1x,D12.5)') l_CSxxx(iCSxxx)%weight
    ! enddo

    do iCSxxx=1,nb_CSxxx
      iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
      ibdyty=l_CSpxx(iCSpxx)%ibdyty
      if (l_CSxxx(iCSxxx)%inode /= 0) then
        allocate(l_CSxxx(iCSxxx)%tnode(1))
        l_CSxxx(iCSxxx)%tnode=minloc(l_CSpxx(iCSpxx)%vertex2node,mask=l_CSpxx(iCSpxx)%vertex2node==l_CSxxx(iCSxxx)%inode)
      else
        allocate(l_CSxxx(iCSxxx)%tnode(l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)))
        do i=1,l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
          l_CSxxx(iCSxxx)%tnode(i:i)=minloc(l_CSpxx(iCSpxx)%vertex2node, &
                                            mask=l_CSpxx(iCSpxx)%vertex2node==l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement))
        enddo
      endif
    enddo

    deallocate(cspxx2bdyty)


  END SUBROUTINE load_tactors_CSxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE nullify_reac_CSxxx (iCSxxx,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iCSxxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_CSpxx(l_CSxxx(iCSxxx)%iCSpxx)%ibdyty

   CALL nullify_reac_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_reac_CSxxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE nullify_vlocy_CSxxx (iCSxxx,storage)
  !
  ! called by SDL solver
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iCSxxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_CSpxx(l_CSxxx(iCSxxx)%iCSpxx)%ibdyty

   CALL nullify_vlocy_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_vlocy_CSxxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE comp_vlocy_CSxxx(iCSxxx,storage,need_full_vlocy)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iCSxxx
   INTEGER            :: storage  
   logical :: need_full_vlocy

   ! new by fd
   INTEGER :: ibdyty,iCSpxx,isize

   iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
   ibdyty=l_CSpxx(iCSpxx)%ibdyty

   if (l_CSpxx(iCSpxx)%is_precon .and. .not. need_full_vlocy ) then
     !print*,'precon'
     if (l_CSxxx(iCSxxx)%inode /= 0) then 
       CALL comp_vlocy_bynode_mecaMAILx(ibdyty,(/ l_CSxxx(iCSxxx)%inode /),storage)
     else
       isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
       CALL comp_vlocy_bynode_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%face(1:isize,l_CSxxx(iCSxxx)%ielement),storage)
     endif
   else
     CALL comp_vlocy_mecaMAILx(ibdyty,storage)
   endif

END SUBROUTINE comp_vlocy_CSxxx

!------------------------------------------------------------------------

SUBROUTINE add_reac_CSxxx(iCSxxx,reac,storage)
   IMPLICIT NONE 

   INTEGER     ,INTENT(in)              :: iCSxxx
   REAL(kind=8),DIMENSION(3),INTENT(in) :: reac
   INTEGER                              :: storage

   INTEGER ::  iCSpxx,ibdyty

   REAL(kind=8),DIMENSION(3) :: l_reac

   INTEGER :: i

                                      !123456789012345
   CHARACTER(len=15),parameter :: IAM='CSxxx::add_reac'

   
   iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
   ibdyty=l_CSpxx(iCSpxx)%ibdyty


   !if (storage == iIaux_) then
   !   print*,'entree corps',ibdyty
   !   print*,reac(:)
   !   print*,l_CSpxx(iCSpxx)%face(:,l_CSxxx(iCSxxx)%ielement)
   !   print*,l_CSxxx(iCSxxx)%interp
   !endif

   
   if (l_CSxxx(iCSxxx)%inode /= 0) then 
      l_reac = l_CSxxx(iCSxxx)%weight*l_CSxxx(iCSxxx)%interp(1)*reac
      CALL add_reac_nodty_mecaMAILx(ibdyty,l_CSxxx(iCSxxx)%inode,l_reac,storage)
   else
     do i=1,l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
       l_reac = l_CSxxx(iCSxxx)%weight*l_CSxxx(iCSxxx)%interp(i)*reac
       !if (storage == iIaux_) then
        ! print*,'< ',IAM 
       !  print*,' add Iaux'
        ! print*,i,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement)
        ! print*,l_CSxxx(iCSxxx)%weight*l_CSxxx(iCSxxx)%interp(i)
        ! print*,'l_reac ',l_reac
        ! print*,IAM,' >'
       !endif
        
       CALL add_reac_nodty_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement),l_reac,storage)
     enddo
   endif

END SUBROUTINE add_reac_CSxxx

!------------------------------------------------------------------------ 

SUBROUTINE get_vlocy_CSxxx(iCSxxx,storage,vlocy)
  IMPLICIT NONE

  INTEGER :: icsxxx
  INTEGER :: storage  
  REAL(kind=8),DIMENSION(3)   :: vlocy

                                     !1234567890123456
  CHARACTER(len=16),parameter :: IAM='CSxxx::get_vlocy'


  integer,DIMENSION(8) :: num
  INTEGER :: ibdyty,iCSpxx, i, isize

  iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
  ibdyty=l_CSpxx(iCSpxx)%ibdyty

  if (l_CSxxx(iCSxxx)%inode /= 0) then 
    isize = 1
    num(1)= l_CSxxx(iCSxxx)%inode
  else
    isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
    num(1:isize)= l_CSpxx(iCSpxx)%face(1:isize,l_CSxxx(iCSxxx)%ielement)
  endif

  vlocy = 0.d0
  SELECT CASE(storage)
    CASE(iV____)
        do i=1,isize
          vlocy = vlocy + l_CSxxx(iCSxxx)%interp(i)*get_V_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE(iVbeg_)
        do i=1,isize
          vlocy = vlocy + l_CSxxx(iCSxxx)%interp(i)*get_Vbegin_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE(iVfree)
        do i=1,isize
          vlocy = vlocy + l_CSxxx(iCSxxx)%interp(i)*get_Vfree_nodty_mecaMAILx(ibdyty,num(i))
          !write(*,'(I0,1x,I0,3(1x,D12.5))') ibdyty,num(i),get_Vfree_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE(iVaux_)
        do i=1,isize
          vlocy = vlocy + l_CSxxx(iCSxxx)%interp(i)*get_Vaux_nodty_mecaMAILx(ibdyty,num(i))
        enddo

        !print*,'< ',IAM
        !write(*,'(A,3(1x,D12.5))') 'Vaux ',vlocy
        !print*,IAM,' >'

    CASE(iVddm_)
        do i=1,isize
          vlocy = vlocy + l_CSxxx(iCSxxx)%interp(i)*get_Vddm_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE default
        call faterr('CSxxx::get_vlocy','unknown storage')
  END SELECT

!  print*,vlocy
!  print*,'--'
  !if (storage == 69) then 
  ! print*,'get Vfree'
  ! write(*,'(4(1x,I0))') num(1:isize)
  ! write(*,'(4(1x,D12.5))')  l_CSxxx(iCSxxx)%interp(1:isize)
  ! write(*,'(3(1x,D12.5))') vlocy
  !endif
END SUBROUTINE get_vlocy_CSxxx

!------------------------------------------------------------------------ 

FUNCTION get_coorTT_CSxxx(icsxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(3) :: get_coorTT_CSxxx
  INTEGER                   :: icsxxx

  INTEGER ::   ibdyty,iCSpxx

  INTEGER :: i, isize

  INTEGER :: M_ibdyty,M_itacty
  real(kind=8),dimension(3) :: vec_perio

  iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
  ibdyty=l_CSpxx(iCSpxx)%ibdyty

  get_coorTT_CSxxx= 0.d0
  do i=1,size(l_CSxxx(iCSxxx)%tnode)
     get_coorTT_CSxxx(:) = get_coorTT_CSxxx(:) + l_CSxxx(iCSxxx)%interp(i)*l_CSpxx(iCSpxx)%tcoor(:,l_CSxxx(iCSxxx)%tnode(i))
  enddo

!fp prise en cpte du periodique
  M_ibdyty = csxxx2bdyty(1,icsxxx)
  M_itacty = csxxx2bdyty(2,icsxxx)

  call get_periodic_MAILx(M_ibdyty,M_itacty,vec_perio)
  get_coorTT_CSxxx(:)=get_coorTT_CSxxx(:)+vec_perio(:)

END FUNCTION get_coorTT_CSxxx

!------------------------------------------------------------------------ 

FUNCTION get_normalTT_CSxxx(icsxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(3) :: get_normalTT_CSxxx
  INTEGER :: icsxxx
  ! ***
  REAL(kind=8),DIMENSION(3) :: tmp,tmp1,tmp2
  INTEGER :: ibdyty,iCSpxx
  INTEGER :: M_ibdyty,M_itacty
  REAL(kind=8) :: norm
  INTEGER,dimension(8) :: num
  INTEGER :: i, isize

  iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
  ibdyty=l_CSpxx(iCSpxx)%ibdyty

  if (l_CSxxx(iCSxxx)%inode /= 0) then 
    if (l_CSpxx(iCSpxx)%well_oriented) then
       get_normalTT_CSxxx =  l_CSpxx(iCSpxx)%tnormal(:,l_CSxxx(iCSxxx)%tnode(1))
    else
       get_normalTT_CSxxx = -l_CSpxx(iCSpxx)%tnormal(:,l_CSxxx(iCSxxx)%tnode(1))
    endif
  else

    !fd on pourrait calculer une interpolation des normales nodales mais on garde la normale a la face
    ! pas bon car ele pas plan et maintenant quadratiques

    isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
    num(1:isize)= l_CSpxx(iCSpxx)%face(1:isize,l_CSxxx(iCSxxx)%ielement)

    tmp1 = get_coorTT_nodty_mecaMAILx(ibdyty,num(2)) - &
           get_coorTT_nodty_mecaMAILx(ibdyty,num(1))

    tmp2 = get_coorTT_nodty_mecaMAILx(ibdyty,num(isize)) - &
           get_coorTT_nodty_mecaMAILx(ibdyty,num(1))

    tmp(1) = (tmp1(2)*tmp2(3)) - (tmp1(3)*tmp2(2))
    tmp(2) = (tmp1(3)*tmp2(1)) - (tmp1(1)*tmp2(3))
    tmp(3) = (tmp1(1)*tmp2(2)) - (tmp1(2)*tmp2(1))

    norm = dsqrt(dot_product(tmp,tmp))

    if (l_CSpxx(iCSpxx)%well_oriented) then
      get_normalTT_CSxxx = tmp/norm
    else
      get_normalTT_CSxxx = -tmp/norm
    endif
  endif

END FUNCTION get_normalTT_CSxxx

!------------------------------------------------------------------------ 

FUNCTION get_normal2TT_CSxxx(icsxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(3) :: get_normal2TT_CSxxx
  INTEGER :: icsxxx
  ! ***
  REAL(kind=8),DIMENSION(3) :: tmp,tmp1,tmp2
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor
  INTEGER :: ibdyty,iCSpxx,id
  INTEGER :: M_ibdyty,M_itacty
  REAL(kind=8) :: norm
  INTEGER,dimension(8) :: num
  INTEGER :: i, isize

  iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
  ibdyty=l_CSpxx(iCSpxx)%ibdyty

  if (l_CSxxx(iCSxxx)%inode /= 0) then 
    if (l_CSpxx(iCSpxx)%well_oriented) then
       get_normal2TT_CSxxx =  l_CSpxx(iCSpxx)%tnormal(:,l_CSxxx(iCSxxx)%tnode(1))
    else
       get_normal2TT_CSxxx = -l_CSpxx(iCSpxx)%tnormal(:,l_CSxxx(iCSxxx)%tnode(1))
    endif
  else
    !fd on pourrait calculer une interpolation des normales nodales mais on garde la normale a la face
    isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
    num(1:isize)= l_CSpxx(iCSpxx)%face(1:isize,l_CSxxx(iCSxxx)%ielement)

    allocate(coor(3,isize))
    do i=1,isize
       coor(:,i)=get_coorTT_nodty_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement))
    enddo 

    !da on utilise plutot les derivees des fonctions de formes aux points de gauss
    
    ! on calcule :
    !   * v1 = dx/d(s1), i.e. v1 = dNi/d(ksi) xi
    !   * v2 = dx/d(s2), i.e. v2 = dNi/d(eta) xi
    tmp1=0.d0
    tmp2=0.d0
    !calcul des composantes de v1 & v2 (calcule sur la face du bas 1:4)
    !print *,'Coor du calcul de la normale : ',coor(1,:)
    !print *,'                             : ',coor(2,:)
    !print *,'                             : ',coor(3,:)
    
    !print *,'Visu des derivee de fonction forme : ',l_CSxxx(iCSxxx)%d_interp(1,:)
    !print *,'                                   : ',l_CSxxx(iCSxxx)%d_interp(2,:)
    
    
    do id=1,3
       tmp1(id) = dot_product(l_CSxxx(iCSxxx)%d_interp(1,:), coor(id,:))
       tmp2(id) = dot_product(l_CSxxx(iCSxxx)%d_interp(2,:), coor(id,:))
    end do
    
    !print *,'Coor du calcul de tmp1 : ',tmp1/dsqrt(dot_product(tmp1,tmp1))
    !print *,'Coor du calcul de tmp2 : ',tmp2/dsqrt(dot_product(tmp2,tmp2))
    
    ! on en deduit det(J) = J_S = | v1 x v2 |
    ! Attention dans la normale il y a detJs local 
    tmp = cross_product(tmp1, tmp2)
    norm = dsqrt(dot_product(tmp,tmp))
    
    tmp = tmp/norm 

    if (l_CSpxx(iCSpxx)%well_oriented) then
      get_normal2TT_CSxxx = tmp
    else
      get_normal2TT_CSxxx = -tmp
    endif
    
    deallocate(coor)
    
  endif

END FUNCTION get_normal2TT_CSxxx

!!!------------------------------------------------------------------------ 

 REAL(kind=8) FUNCTION get_surf_CSxxx(icsxxx)
  IMPLICIT NONE

  INTEGER ::   icsxxx

                          !12345678901234
  CHARACTER(LEN=14):: IAM='CSxx::get_surf'

  ! si pression
  ! get_surf_CSxxx = 1.d0
  ! si force
  get_surf_CSxxx = l_CSxxx(iCSxxx)%surf
  
END FUNCTION get_surf_CSxxx

!!!------------------------------------------------------------------------ 

 REAL(kind=8) FUNCTION get_weight_CSxxx(icsxxx)
  IMPLICIT NONE

  INTEGER ::   icsxxx

                          !12345678901234567
  CHARACTER(LEN=17):: IAM='CSxxx::get_weight'

  get_weight_CSxxx = l_CSxxx(iCSxxx)%weight
  
END FUNCTION get_weight_CSxxx

!!!------------------------------------------------------------------------ 

 FUNCTION get_nb_CSxxx(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_CSxxx
  
   get_nb_CSxxx = nb_CSxxx
  
 END FUNCTION get_nb_CSxxx

!!!------------------------------------------------------------------------

 function get_sci_CSxxx(icsxxx)
   implicit none
   !
   integer, intent(in) :: icsxxx
   !
   integer :: get_sci_CSxxx

   get_sci_CSxxx = l_CSxxx(icsxxx)%iCSsci

 end function get_sci_CSxxx

!!!------------------------------------------------------------------------ 

 FUNCTION get_ENT_CSxxx(icsxxx)

   IMPLICIT NONE
   INTEGER :: icsxxx
   INTEGER :: get_ENT_CSxxx
 
   integer :: iCSpxx
 
   iCSpxx=l_CSxxx(iCSxxx)%iCSpxx

   get_ENT_CSxxx = get_entity_mecaMAILx(l_CSpxx(iCSpxx)%ibdyty)

 END FUNCTION get_ENT_CSxxx

!!!------------------------------------------------------------------------ 

 logical function all_dof_driven_CSxxx(icsxxx)
   implicit none
   integer, intent(in) :: icsxxx
   !
   integer :: iCSpxx, ibdyty, inode, ielem

   iCSpxx = l_CSxxx(iCSxxx)%iCSpxx
   ibdyty = l_CSpxx(iCSpxx)%ibdyty

   inode  = l_CSxxx(iCSxxx)%inode
   ielem  = l_CSxxx(iCSxxx)%ielement

   ! rm: there really should not be any other case...
   if( inode /= 0 ) then
     all_dof_driven_CSxxx = is_node_dof_driven_mecaMAILx(ibdyty, inode)
   else 
     all_dof_driven_CSxxx = is_elem_dof_driven_mecaMAILx(ibdyty, ielem)
   end if

 end function all_dof_driven_CSxxx
!!!------------------------------------------------------------------------ 

 subroutine set_precon_node_CSxxx
   implicit none 
   integer :: nb_MAILx
   integer :: ibdyty,iCSpxx,i

   nb_MAILx=get_nb_MAILx()

   if (nb_MAILx == 0) return

   if (nb_CSpxx == 0) return

   do iCSpxx=1,nb_CSpxx
     ibdyty = l_CSpxx(iCSpxx)%ibdyty
     do i=1,l_CSpxx(iCSpxx)%nb_vertex
       call set_precon_node_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%vertex2node(i))
       l_CSpxx(icspxx)%is_precon = .true.          
     end do 
   end do

 end subroutine set_precon_node_CSxxx

!!!------------------------------------------------------------------------ 

 real(kind=8) function surf_tri(ibdyty,i1,i2,i3)
   IMPLICIT NONE
   integer                   :: ibdyty,i1,i2,i3
   real(kind=8),dimension(3) :: coor1,coor2,coor3,v
   real(kind=8)               :: p,l1,l2,l3

   coor1 =  get_coorTT_nodty_mecaMAILx(ibdyty,i1)
   coor2 =  get_coorTT_nodty_mecaMAILx(ibdyty,i2)
   coor3 =  get_coorTT_nodty_mecaMAILx(ibdyty,i3)

   v = coor2 - coor1
   l3 = dsqrt(dot_product(v,v))

   v = coor3 - coor2
   l1 = dsqrt(dot_product(v,v))

   v = coor1 - coor3
   l2 = dsqrt(dot_product(v,v))

   p = 0.5*(l1+l2+l3)

   surf_tri = dsqrt(p*(p-l1)*(p-l2)*(p-l3))


 END FUNCTION surf_tri

!!!------------------------------------------------------------------------ 

  CHARACTER(len=5) FUNCTION get_color_CSxxx(itact)

    IMPLICIT NONE
    INTEGER :: itact 
   
    get_color_CSxxx = get_color_MAILx(CSxxx2bdyty(1,itact),CSxxx2bdyty(2,itact))
 
  END FUNCTION get_color_CSxxx

!!!------------------------------------------------------------------------ 

  FUNCTION get_visible_CSxxx(icsxxx)

    IMPLICIT NONE

    ! variables d'entree
    ! indice de CSxxx, dans la numerotation des CSxxx
    INTEGER :: iCSxxx

    ! valeur de retour
    ! vaut "vrai" ssi le CSxxx considere correspond a un mecaMAILx visible
    logical :: get_visible_CSxxx 

    ! variables locales
    ! indice du modele mecaMAILx, dans la numerotation des mecaMAILx, correspondant au CSxxx considere   
    integer :: ibdyty,iCSpxx

    ! on recupere l'indice du modele mecaMAILx, dans la numerotation des
    ! mecaMAILx, correspondant au CSxxx considere 

    iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
    ibdyty=l_CSpxx(iCSpxx)%ibdyty

    ! le CSxxx est visible ssi le modele mecaMAILx correspondant est visible
    get_visible_CSxxx = get_visible_mecaMAILx(ibdyty)
 
  END FUNCTION get_visible_CSxxx

!!!------------------------------------------------------------------------ 

  SUBROUTINE flip_orientation_CSxxx(ibdyty)
     implicit none
     integer :: ibdyty,i,count

     print*,'flip all CSxxx of object ',ibdyty

     count=0
     do i=1,nb_CSpxx
       if (l_CSpxx(i)%ibdyty == ibdyty) then
         l_CSpxx(i)%well_oriented=.FALSE.  
         count = count + 1
       endif
     enddo

     print*,count,' CSpxx flipped'

  end subroutine 

!!!------------------------------------------------------------------------ 

  SUBROUTINE flip_orientation_one_CSpxx(ibdyty,icspxx)
     implicit none
     integer :: ibdyty,icspxx,i,count
     character(len=120) :: mes
     logical :: found

     write(mes,'(A,1x,I0,1x,A,1x,I0)') 'flip CSpxx',icspxx,'of object ',ibdyty
     call logmes(mes)

     count=0
     found=.false.
     do i=1,nb_CSpxx
       if (l_CSpxx(i)%ibdyty == ibdyty) then
         count = count + 1
         if (count == icspxx) then
           l_CSpxx(i)%well_oriented=.FALSE.  
           found=.true.
           exit
         endif
       endif
     enddo

     if (.not. found) then
       call FATERR('CSxxx::flip_orientation_one_CSxxx','unable to find CSxxx')
     endif
  end subroutine 

!!!------------------------------------------------------------------------ 

  subroutine set_shrink_CSxxx(shrink)
    implicit none
    real(kind=8) :: shrink

    call faterr('CSxxx::set_shrink','obsolete function')
    CS_shrink = shrink

  end subroutine

!!!------------------------------------------------------------------------ 

  ! mise a jour de la structure CSxxx
  SUBROUTINE increment_CSpxx(reset)
    IMPLICIT NONE
    integer, intent(in), optional :: reset

    ! on conserve le pas auquel on a fait l'update
    integer :: last_step_updt=-1
    integer :: iCSpxx,ibdyty,i

    if( present(reset) ) then
        last_step_updt = -1
        return
    end if

    !fd si on a deja appele cette fonction a ce pas on sort direct
    if (nstep == last_step_updt) return
    last_step_updt = nstep

     
    ! on pousse les positions des points de contact
    ! on actualise les normales 
    do iCSpxx=1,nb_CSpxx
      ibdyty=l_CSpxx(iCSpxx)%ibdyty       
      do i=1,l_CSpxx(iCSpxx)%nb_vertex
         l_CSpxx(iCSpxx)%tcoor(:,i) = get_coorTT_nodty_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%vertex2node(i))      
      end do 
      if (l_CSpxx(iCSpxx)%quadrature < 0) then
        call increment_xSpxx(l_CSpxx(iCSpxx),.FALSE.)
      else 
        call increment_xSpxx(l_CSpxx(iCSpxx),.TRUE.)
      endif
    enddo

  END SUBROUTINE increment_CSpxx

!!!------------------------------------------------------------------------ 

  ! ajout d'une pression sur un patch 
  SUBROUTINE apply_pressure_CSpxx(iCSpxx,pressure)
    IMPLICIT NONE
    integer :: iCSpxx
    real(kind=8) :: pressure
    ! ****
    integer                                 :: iCSxxx,i,ibdyty
    real(kind=8),dimension(nbdime)          :: l_reac,normal

    ! print*,'----'
    
    ibdyty=l_CSpxx(iCSpxx)%ibdyty

    ! print *,'pressure ', pressure
    ! print *,'Nombre de CSxxx : ',size(l_CSxxx)

    do iCSxxx=1,size(l_CSxxx)
      if (l_CSxxx(iCSxxx)%iCSpxx /= iCSpxx) cycle
      normal = get_normal2TT_CSxxx(icsxxx)
      ! print *,'Numero du CSxxx : ',iCSxxx
      ! print *,'Normale : ',normal
      
      if (l_CSxxx(iCSxxx)%inode /= 0) then 

         l_reac = l_CSxxx(iCSxxx)%surf*l_CSxxx(iCSxxx)%interp(1)*pressure*normal
         CALL add_reac_nodty_mecaMAILx(ibdyty,l_CSxxx(iCSxxx)%inode,l_reac,iFext_)

      else
        do i=1,l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
          
          ! print *,'l_CSxxx(iCSxxx)%weight : ',l_CSxxx(iCSxxx)%weight
          ! print *,'l_CSxxx(iCSxxx)%interp(i) : ',l_CSxxx(iCSxxx)%interp(i)
          ! print *,'normale : ',normal
          
          l_reac = l_CSxxx(iCSxxx)%surf*l_CSxxx(iCSxxx)%interp(i)*pressure*normal
          
          ! print *,'Application des forces nodales sur body : ',ibdyty,' aux noeuds : ' & 
          !       ,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement),  ' de valeur : ',l_reac
          
          CALL add_reac_nodty_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement),l_reac,iFext_)
        enddo
      endif
    enddo 


    ! print*,l_reac
    ! print*,'----'
    
  end subroutine

!!!------------------------------------------------------------------------ 

  ! ajout d'une force surfacique sur un patch 
  SUBROUTINE apply_surface_load_CSpxx(iCSpxx,load)
    IMPLICIT NONE
    integer :: iCSpxx
    real(kind=8),dimension(3)              :: load
    ! ****
    integer                                 :: iCSxxx,i,ibdyty
    real(kind=8),dimension(nbdime)          :: l_reac

    ibdyty=l_CSpxx(iCSpxx)%ibdyty

    !print *,'Taille du nombre de CSxxx : ',size(l_CSxxx)

    do iCSxxx=1,size(l_CSxxx)
      if (l_CSxxx(iCSxxx)%iCSpxx /= iCSpxx) cycle

      !print *,'Numero du CSxxx : ',iCSxxx
      !print *,'Normale : ',normal
      if (l_CSxxx(iCSxxx)%inode /= 0) then 

         l_reac = l_CSxxx(iCSxxx)%surf*l_CSxxx(iCSxxx)%interp(1)*load
         CALL add_reac_nodty_mecaMAILx(ibdyty,l_CSxxx(iCSxxx)%inode,l_reac,iFext_)
 
     else
        do i=1,l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
          
          
          !print *,'l_CSxxx(iCSxxx)%weight : ',l_CSxxx(iCSxxx)%weight
          !print *,'l_CSxxx(iCSxxx)%interp(i) : ',l_CSxxx(iCSxxx)%interp(i)
          !print *,'normale : ',normal
          
          l_reac = l_CSxxx(iCSxxx)%surf*l_CSxxx(iCSxxx)%interp(i)*load
          
          !print *,'Application des forces nodales sur body : ',ibdyty,' aux noeuds : ' & 
          !       ,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement),  ' de valeur : ',l_reac
          
          CALL add_reac_nodty_mecaMAILx(ibdyty,l_CSpxx(iCSpxx)%face(i,l_CSxxx(iCSxxx)%ielement),l_reac,iFext_)
        enddo
      endif
    enddo 

  end subroutine

!!!------------------------------------------------------------------------ 

  ! calcul une interpolation au point de contact de reac 
  SUBROUTINE interpolate_reac_CSpxx(iCSxxx,r,storage)
    IMPLICIT NONE
    integer :: iCSxxx,storage
    real(kind=8),dimension(nbdime) :: r
    ! ****
    integer                          :: iCSpxx,i,ibdyty,isize
    integer,DIMENSION(8)             :: num

    iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
    ibdyty=l_CSpxx(iCSpxx)%ibdyty

    if (l_CSxxx(iCSxxx)%inode /= 0) then 
      isize = 1
      num(1)= l_CSxxx(iCSxxx)%inode
    else
      isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
      num(1:isize)= l_CSpxx(iCSpxx)%face(1:isize,l_CSxxx(iCSxxx)%ielement)
    endif

    !print*,iCSxxx,l_CSxxx(iCSxxx)%ielement

    r = 0.d0
    do i=1,isize

          !print*,num(i),get_Reac_nodty_mecaMAILx(ibdyty,num(i),storage)

          r = r + l_CSxxx(iCSxxx)%interp(i)*get_Reac_nodty_mecaMAILx(ibdyty,num(i),storage)
    enddo

    ! on refait une pressions
    r = r / l_CSxxx(iCSxxx)%weight

  end subroutine

  !!!------------------------------------------------------------------------ 

  !> on recupere le tableau des noeuds supports au CSxxx
  !> ca peut etre un noeud comme un element a 3 ou 4 noeuds
  function get_nodes_CSxxx(iCSxxx)
      IMPLICIT NONE

    INTEGER :: iCSpxx,icsxxx, isize
    integer(kind=4),dimension(:),pointer :: get_nodes_CSxxx
    
    INTEGER(kind=4) :: i
    
    iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
    isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)

    allocate(get_nodes_CSxxx(isize)) 
    get_nodes_CSxxx(1:isize) = l_CSpxx(iCSpxx)%face(1:isize,l_CSxxx(iCSxxx)%ielement)
  
  END function get_nodes_CSxxx
    
!!!------------------------------------------------------------------------ 

  !> on recupere le tableau des ddl supports au CSxxx
  !> ca peut etre un noeud comme un element a 3 ou 4 noeuds
  function get_dof_CSxxx(iCSxxx)
    IMPLICIT NONE

    INTEGER :: icsxxx
    integer(kind=4),dimension(:),pointer :: get_dof_CSxxx

    integer(kind=4),DIMENSION(8) :: iccdof,nbdof
    INTEGER(kind=4)              :: ibdyty,iCSpxx, in, i, isize, icc


    iCSpxx=l_CSxxx(iCSxxx)%iCSpxx
    ibdyty=l_CSpxx(iCSpxx)%ibdyty

    iccdof=0
    nbdof=0

    if (l_CSxxx(iCSxxx)%inode /= 0) then 
      call get_dof_node_mecamailx(ibdyty,l_CSxxx(iCSxxx)%inode,iccdof(1),nbdof(1))
      allocate(get_dof_CSxxx(nbdof(1)))
      get_dof_CSxxx(1:nbdof(1)) = (/ ( iccdof(1)+i, i=1, nbdof(1) ) /)
    else
      isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
      do in=1,isize
        call get_dof_node_mecamailx(ibdyty,l_CSxxx(iCSxxx)%tnode(in),iccdof(in),nbdof(in))
      enddo
      allocate(get_dof_CSxxx(sum(nbdof)))
      icc=0
      do in=1,isize
        get_dof_CSxxx(icc+1:icc+nbdof(in)) = (/ ( iccdof(in)+i, i=1, nbdof(in) ) /)
        icc = icc + nbdof(in)
      enddo
    endif

  END function get_dof_CSxxx

!!!------------------------------------------------------------------------ 

  !> on recupere l interpolation sur le/les noeuds supports
  !> \todo faire un getPtr 
  function get_interp_CSxxx(iCSxxx)
    INTEGER :: icsxxx
    real(kind=8),dimension(:),pointer :: get_interp_CSxxx

    !***
    integer(kind=4) :: isize,iCSpxx  
    
    if (l_CSxxx(iCSxxx)%inode /= 0) then 
      allocate(get_interp_CSxxx(1)) 

      get_interp_CSxxx(1) = l_CSxxx(iCSxxx)%interp(1)

    else
  
      iCSpxx=l_CSxxx(iCSxxx)%iCSpxx

      isize = l_CSpxx(iCSpxx)%nb_vertex_bf(l_CSxxx(iCSxxx)%ielement)
      allocate(get_interp_CSxxx(isize)) 

      get_interp_CSxxx(1:isize)= l_CSxxx(iCSxxx)%interp(1:isize)

    endif

  end function

!!!------------------------------------------------------------------------
  
  !> \brief Compute quadrature points position and weight and interpolation
  !> The computation is done for all support element, but only if quadrature changed
  subroutine compute_inter(interp_id,quad_id,nbgp,gpc,gpw,interp)
    implicit none
    !> interpolation type
    integer(kind=4), intent(in) :: interp_id
    !> quadrature rule
    integer(kind=4), intent(in) :: quad_id
    !> number of points in quadrature
    integer(kind=4), intent(inout) :: nbgp
    !> quadrature points position
    real(kind=8)   , dimension(:,:), pointer :: gpc
    !> quadrature points weights
    real(kind=8)   , dimension(:)  , pointer :: gpw
    !> form functions of quadrature points
    type(T_interp) , dimension(:)  , pointer:: interp
    !
    integer(kind=4) :: interp_id2, i
    real(kind=8)    :: zeta

    ! check if computation is necessary
    if( associated(gpc) ) then
      if( size(gpw) == get_nb_points_from_quadrature(quad_id) ) then
        return
      end if
    end if

    if(associated(gpc)) then
      deallocate(gpc,gpw)
      do i = 1, size(interp)
        deallocate(interp(i)%N)
        deallocate(interp(i)%DN)
      end do
      deallocate(interp)
    end if

    call pos_gauss(quad_id,gpc,gpw)

    nbgp = size(gpw)

    select case( interp_id )
    case( i_T_P1 )
      interp_id2 = i_TEP1
      zeta = 0.d0
      ! face du bas (/ 1, 2, 3 /)
    case( i_T_P2 )
      interp_id2 = i_TEP2
      zeta = 0.d0
      ! face du bas (/ 1, 2, 3, 5, 6, 7/)
    case( i_Q_P1 )
      interp_id2 = i_H_P1
      zeta =-1.d0
      ! face du bas (/ 1, 2, 3, 4/)
    case( i_Q_P2 )
      interp_id2 = i_H_P2
      zeta =-1.d0
      ! face du bas (/ 1, 2, 3, 4, 9, 10, 11, 12/)
    end select

    allocate(interp(nbgp))
    do i = 1, nbgp
      nullify(interp(i)%N,interp(i)%DN)   
      call fonct_forme(interp_id,gpc(:,i),interp(i)%N)
      call derive_forme(interp_id2, (/ gpc(1, i), gpc(2, i), zeta /), interp(i)%DN)
    enddo

  end subroutine compute_inter
  
!!!------------------------------------------------------------------------   
 subroutine get_bulk_strain_triaxiality_csxxx(id_csxxx,value)
   implicit none

   integer(kind=4) :: id_csxxx
   real(kind=8)    :: value

    if( l_CSxxx(id_CSxxx)%igp < 1 ) call faterr('CSxxx::get_bulk_strain_triaxiality', 'no nearest Gauss point found, the contact law is not usable in this configuration')

    call get_gp_strain_triaxiality_mecaMAILx(csxxx2bdyty(1,id_CSxxx),l_CSxxx(id_CSxxx)%iblmty,l_CSxxx(id_CSxxx)%igp,value)

  end subroutine get_bulk_strain_triaxiality_csxxx
  
 subroutine get_bulk_stress_triaxiality_csxxx(id_csxxx,value)
   implicit none

   integer(kind=4) :: id_csxxx
   real(kind=8)    :: value

    if( l_CSxxx(id_CSxxx)%igp < 1 ) call faterr('CSxxx::get_bulk_stress_triaxiality', 'no nearest Gauss point found, the contact law is not usable in this configuration')

    call get_gp_stress_triaxiality_mecaMAILx(csxxx2bdyty(1,id_CSxxx),l_CSxxx(id_CSxxx)%iblmty,l_CSxxx(id_CSxxx)%igp,value)

 end subroutine get_bulk_stress_triaxiality_csxxx
  


!!!------------------------------------------------------------------------

  function get_connec_CSpxx()
    implicit none
    integer, dimension(:), pointer :: get_connec_CSpxx

    get_connec_CSpxx => null()
    if( nb_CSpxx == 0 ) return

    get_connec_CSpxx => get_connec_xSpxx(l_CSpxx)

  end function

  subroutine get_all_data_CSpxx(idata, rdata)
    implicit none
    integer     , dimension(:,:), pointer :: idata
    real(kind=8), dimension(:,:), pointer :: rdata

    idata => null()
    rdata => null()

    if( nb_CSpxx == 0 ) return

    call get_all_data_xSpxx(l_CSpxx, idata, rdata, .true.)

  end subroutine

!!!------------------------------------------------------------------------   

  subroutine clean_memory_CSxxx
    implicit none
    integer(kind=4) :: i

    nb_CSxxx = 0
    if( allocated(l_CSxxx) ) then
      do i = 1, size(l_CSxxx)
        if( associated(l_CSxxx(i)%tnode)  ) deallocate(l_CSxxx(i)%tnode)
        if( associated(l_CSxxx(i)%interp) ) deallocate(l_CSxxx(i)%interp)
      end do
      deallocate(l_CSxxx)
    end if

    if( allocated(csxxx2bdyty) ) deallocate(csxxx2bdyty)

    nb_CSpxx = 0
    if( allocated(l_CSpxx) ) then
      do i = 1, size(l_CSpxx)
        call erase_xSpxx(l_CSpxx(i))
      end do
      deallocate(l_CSpxx)
    end if

    ! reset increment step internal step numbering
    call increment_CSpxx(1)
  end subroutine

 END MODULE CSxxx
