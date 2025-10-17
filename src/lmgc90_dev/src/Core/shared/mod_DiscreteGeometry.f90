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

!> DicreteGeometry contains a set of function and subroutine
!> usefull to manage discretized geometry 
!>@author f dubois
MODULE DiscreteGeometry

!  .. "Use Statements"
  use utilities, only : logmes, faterr, &
                        G_i_list

  use algebra, only : cross_product, &
                      determinant  , &
                      diagonalise33, &
                      inverse33    , &
                      inverse22    , &
                      length3

  use a_EF, only : i_T_P1, i_Q_P1, i_Q_P2  , &
                   i_H_P1, i_H_P2, i_Q2x2  , &
                   i_TR01, i_TR03, i_TR04  , &
                   get_gp_coor , &
                   fonct_forme , &
                   derive_forme, &
                   second_forme, &
                   integrate_field

  use clipper, only : polygones_intersection, &
                      clipper_free

  use overall, only: pi_g

!  USE LA_PRECISION, ONLY: WP => DP
!  USE F95_LAPACK

  implicit none
  private
  public node_triangle_projection, &
         convex_hull, &
         segments_intersection, &
         segments_intersection_wp, &
         polytopes_intersection_wc, &         
         display_polytope, &
         comp_rep, &
         ! 
         new_HE_Hdl, &
         settle_HE_Hdl, &
         build_HE_Hdl, &
         info_HE_Hdl, &         
         erase_HE_Hdl, &
         next_HE, &
         previous_HE, &
         update_HE_Hdl, &
         node_HE_Hdl_proximity, &
         new_node_HE_Hdl_proximity, &         
         node_HE_Hdl_rough_proximity, &
         get_face_normals_HE_Hdl, &
         get_nodal_normals_HE_Hdl, &
         get_nodal_radius_HE_Hdl, &
         set_orientation_surface_T3, &
         compute_vol_tetrahedron, &
         compute_inertia_tetrahedron, &
         compute_info_triangle, & 
         build_topology_surface_T3, &
         compute_volume_surface_T3, &
         compute_mass_center_surface_T3, &
         compute_inertia_surface_T3, &
         compute_mechanical_properties_surface_T3, &
         compute_volume_inertia_global_frame_surface_T3, &
         identify_entities_surface_T3, &
         compute_volume_inertia_global_frame_surface_Q4, &
         compute_node_reduced_coor_in_linear_quadrangle, &
         compute_node_reduced_coor_in_quadratic_quadrangle, &         
         !identity, &
         !my_func, &
         get_corners_of_contour, &
         edgetoedge_distance, &
         nodetoedge_distance, &
         nodetonode_distance, &
         nodetoface_distance, &
         edgetoface_distance, &
         edgetoface_distance_wp, &
         pinball_HE_Hdl_rough_proximity, &
         node_with_ppp_HE_Hdl_proximity, &
         !
         find_proximal
         ! rm: RIP 21 aout 2024 
         !polytopes_intersection, &    
         !polytopes_intersection_wp, &

  !fd structure half-edge

  type, PUBLIC :: T_HE   ! half edge object 
     integer :: i,f      ! index local vertex dans la face (1,2,3) , num face
  end type T_HE          ! if bord (0,bord)     

  type, PUBLIC :: T_HE_Hdl   ! half edge handler for triangle 
    !
    integer                                   :: max_adj_faces,nb_vertex,nb_faces
    ! description des faces
    integer       ,dimension(:,:)    ,pointer :: faces    
    !fd
    !fd un bord tourne dans le meme sens que le HE oppose 
    !fd le contour est oriente comme les faces
    !fd avec v2HE si v est sur le bord on a tjs le HE de bord et ce bord part du vertex
    !fd
    ! pour un vertex (global) retourne un half_edge HE=(i,f) 
    type(T_HE)    , dimension(:)     ,pointer :: V2HE     
    ! pour un HE==(i,f) retourne le HE oppose ou (0,bord) 
    type(T_HE)    , dimension(:,:)   ,pointer :: HE2opHE  
    ! pour un bord retourne le HE oppose
    type(T_HE)    , dimension(:)     ,pointer :: B2opHE   
    !
    ! map bord contour
    integer       , dimension(:)     ,pointer :: b2cnt    
    ! tableau des contour, liste fermee et orientee de bords
    type(G_i_list), dimension(:)     ,pointer :: cnts     
    !
    ! position des vertex pour la recherche de contact 
    real(kind=8)  ,dimension(:,:)    ,pointer :: vertex   

    ! normal aux faces pour la recherche de contact   
    real(kind=8)  ,dimension(:,:)    ,pointer :: normal   
    ! is normal allocated only here, or reference another array
    logical                                   :: is_normal_mine

    ! a vertex positionnes donne le vecteur orientation d un HE
    real(kind=8)  ,dimension(:,:,:)  ,pointer :: HEorien  
    ! 
    ! auxiliary paranoiac value, correspond to the real number of faces added to the HE_hdl
    integer                                   :: if       
    !
    logical :: reverse=.false.
  end type T_HE_Hdl

  logical :: itchatche =.FALSE.

  
contains

!> create an empty HE_hdl
!> max_adj_faces est le nombre de triangles arrivant sur un noeud
type(T_HE_Hdl) function new_HE_Hdl(nb_vertex,nb_faces,max_adj_faces)
  implicit none
  integer :: nb_vertex,nb_faces,max_adj_faces,i
  type(T_HE) :: HE_null

  new_HE_Hdl%nb_vertex = nb_vertex
  new_HE_Hdl%nb_faces = nb_faces
  new_HE_Hdl%max_adj_faces = max_adj_faces
  new_HE_Hdl%if = 0

  if (nb_faces == 0) then
    nullify(new_HE_Hdl%faces, &
            new_HE_Hdl%HE2opHE, &
            new_HE_Hdl%HEorien, &
            new_HE_Hdl%normal )
  else
    allocate(new_HE_Hdl%faces(3,nb_faces))
    new_HE_Hdl%faces=0
    allocate(new_HE_Hdl%HE2opHE(3,nb_faces))
    allocate(new_HE_Hdl%HEorien(3,3,nb_faces), &
             new_HE_Hdl%normal(3,nb_faces))
    new_HE_Hdl%is_normal_mine = .true.

  endif

  if (nb_vertex == 0) then
    nullify(new_HE_Hdl%V2HE)
  else           
    allocate(new_HE_Hdl%V2HE(nb_vertex))
    do i=1,nb_vertex
      new_HE_Hdl%V2HE(i)%i=0
      new_HE_Hdl%V2HE(i)%f=0       
    enddo   
  endif

  nullify(new_HE_Hdl%vertex,new_HE_Hdl%cnts,new_HE_Hdl%b2cnt)

end function

!> add a face to a HE_Hdl
subroutine settle_HE_Hdl(HE_Hdl,face,err)
  implicit none
  type(T_HE_Hdl) :: HE_Hdl
  integer :: face(:)
  integer :: err
  ! ***
  integer :: nbn
  character(len=90) :: cout
                          !1234567890123456789012345678901
  character(len=31):: IAM='DiscreteGeometry::settle_HE_Hdl'

  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err= 1
    return
  endif

  nbn = size(face)         
  if (nbn /= 3) then
    call LOGMES(IAM//' number of nodes of the face different than 3',.true.)
    err= 1
    return
  endif

  HE_Hdl%if = HE_Hdl%if + 1
  if ( HE_Hdl%if > HE_Hdl%nb_faces) then
    cout=''
    write(cout,'("face index ",I0," number of faces ",I0)') HE_Hdl%if,HE_Hdl%nb_faces
    call LOGMES(IAM//' number of faces higher than max value',.true.)
    err= 1
    return
  endif

  HE_Hdl%faces(:,HE_Hdl%if) = face(:)

end subroutine

!> once all the faces are added it builds the data structure of the HE_hdl
subroutine build_HE_Hdl(HE_Hdl,err)
  implicit none
  type(T_HE_Hdl) :: HE_Hdl
  integer        :: err, err_
  ! ***
  type(T_HE)                            :: HE_aux,HE
  type(T_HE),dimension(:,:),allocatable :: tempo     ! vecteur de travail 
  integer   ,dimension(:)  ,allocatable :: idx_tempo ! vecteur de travail 
  integer   ,dimension(:,:),allocatable :: tag       ! vecteur de travail 
  integer   ,dimension(:),allocatable   :: contour   ! vecteur de travail 
  
  ! ***
  integer :: f,i,nb_bord,v,vd,vf,vd_op,vf_op,iop
  logical :: found_opHE,itchatche =.false.

  !**
  integer :: nb_cnt,bi,bj,v_i,v_d,v_f,c,n,idx,ic,b
  logical :: start,closed,empty_loop
 
                           !123456789012345678901234567890
  character(len=30) :: IAM='DiscreteGeometry::build_HE_Hdl'
  character(len=120):: cout

  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err= 1
    return
  endif
  
  HE_aux%i=0;HE_aux%f=0

  allocate(tempo(HE_Hdl%nb_vertex,HE_Hdl%max_adj_faces), &
       idx_tempo(HE_Hdl%nb_vertex))

  ! pour stocker les HE adjacents au noeud ; ici on initialise
  tempo = HE_aux

  ! pour connaitre le nombre de triangles adjacents 
  idx_tempo=0

  ! necessary to detect boundary
  ! at the end if tag == 0 its a boundary edge 
  allocate(tag(3,HE_Hdl%nb_faces))
  tag=0

  !fd test a la con
  do f=1,HE_Hdl%nb_faces
    !if (itchatche) print*,'face ', f
    do i=1,3
      vd=HE_Hdl%faces(i,f)
      !if (itchatche) print*,'sommet ',i,' vertex ',vd

      !on incremente le nombre de triangles auquel appartient le vertex
      idx_tempo(vd) = idx_tempo(vd) + 1
      if (idx_tempo(vd) > HE_Hdl%max_adj_faces) then
        call LOGMES('Error '//IAM//': number of adjacent faces to a node reaches max_adj_faces',.true.)
        err= 1
        return
      endif 

      !on stocke le HE (rang dans la face, num face) adjacent au noeud
      tempo(vd,idx_tempo(vd))%i=i ; tempo(vd,idx_tempo(vd))%f=f

      ! on cherche le vertex suivant
      HE_aux=next_HE(tempo(vd,idx_tempo(vd)),err_)
      if (err_ > 0) then
        !call logmes() 
        err= 1
        return
      endif  

      vf=HE_Hdl%faces(HE_aux%i,HE_aux%f)

      !if (itchatche) print*,'vertex suivant ',vf

      ! --- construction de HE2opHE ---
      ! on fait a la vole donc si par encore peuple ca se fera quand le voisin sera peuple
      
      ! on cherche un HE avec les meme num en sens oppose
      vd_op = vf 
      found_opHE = .FALSE.
      do iop=1,idx_tempo(vd_op)
        HE_aux=next_HE(tempo(vd_op,iop),err_)
        if (err_ > 0) then
          !call logmes() 
          err= 1
          return
        endif  
        vf_op = HE_Hdl%faces(HE_aux%i,HE_aux%f)

        if (vf_op == vd) then
          ! on a trouve l'HE en face 
          HE_aux=tempo(vd_op,iop)
          HE_Hdl%HE2opHE(i,f)=HE_aux
          !if (itchatche) then
          !  print*,'possede un oppose'
          !  print*,'HE op',HE_aux%i,HE_aux%f,HE_Hdl%faces(HE_aux%i,HE_aux%f)
          !endif
          HE_aux%i=i;HE_aux%f=f
          HE_Hdl%HE2opHE(HE_Hdl%HE2opHE(i,f)%i,HE_Hdl%HE2opHE(i,f)%f)=HE_aux
          found_opHE = .TRUE.
          exit
        endif
      enddo

      
      if (.not. found_opHE) then
        !print*,'ne possede pas encore d oppose'

        !on garde ceux qui n'ont pas encore d'oppose (evite de se faire chier avec des tests)
        !on testera apres pour virer les bords 
        HE_Hdl%V2HE(vd)=tempo(vd,idx_tempo(vd))
      else 
        if (tag(i,f)== 0) then
          tag(i,f) = 1
        else
          call LOGMES('Error '//IAM//': Huummm HE already tagged, it is not possible',.true.)
          err= 1
          return
        endif
        HE_aux = HE_Hdl%HE2opHE(i,f)
        if (tag(HE_aux%i,HE_aux%f)== 0) then
          tag(HE_aux%i,HE_aux%f) = 1
        else
          call LOGMES('Error '//IAM//': Huummm HE already tagged, it is not possible',.true.)
          err= 1
          return
        endif
      end if
    enddo
  enddo

  
  ! construction des bords

  nb_bord=0
  do f=1,HE_Hdl%nb_faces
   do i=1,3
     if (tag(i,f) == 0) then
       !print*,'bord: face ',f,' rang ',i
       nb_bord=nb_bord+1
       !vd=He_Hdl%faces(i,f)
       !HE_aux%i=i; HE_aux%f=f
       !print*,'HE beg ',HE_aux%i,HE_aux%f
       !HE_aux=next_HE(HE_aux)
       !print*,'HE end ',HE_aux%i,HE_aux%f
       !vf=HE_Hdl%faces(HE_aux%i,HE_aux%f)
       !print*,'face ',f,' bord libre',vd,vf
     endif
   enddo
  enddo

  if (nb_bord == 0) then
    nullify(HE_Hdl%B2opHE)
    nullify(HE_Hdl%B2cnt)
    nullify(HE_Hdl%cnts)

    ! utiliser info_he_hdl
    !
    ! if (itchatche) then
    !   do c=1,HE_Hdl%nb_vertex
    !     write(cout,*) '=================='
    !     call logmes(cout, .true.)
    !     write(cout,*) 'HE linked to vertex ',c
    !     call logmes(cout, .true.)
    !     HE=HE_Hdl%V2HE(c)
    !     write(cout,*) 'rank: ',HE%i,' face ',HE%f
    !     call logmes(cout, .true.)
    !     if (HE%i == 0) then
    !       write(cout,*) 'HE opposed to boundary'
    !       call logmes(cout, .true.)
    !       HE_aux = HE_Hdl%B2opHE(HE%f)
    !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !       call logmes(cout, .true.)
    !       write(cout,*) 'starting vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
    !       call logmes(cout, .true.)
    !       HE_aux = next_HE(HE_aux,err_)
    !       write(cout,*) 'ending vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
    !       call logmes(cout, .true.)
    !     else
    !       write(cout,*) 'HE opposed to HE'
    !       call logmes(cout, .true.)
    !       HE_aux = HE_Hdl%HE2opHE(HE%i,HE%f)
    !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !       call logmes(cout, .true.)
    !     endif
    !     !
    !     if (HE%i == 0) then
    !       HE_aux = HE_Hdl%B2opHE(HE%f)
    !       HE_aux = previous_HE(HE_aux,err_)
    !       write(cout,*) 'previous HE'
    !       call logmes(cout, .true.)
    !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !       call logmes(cout, .true.)
    !       if (HE_aux%i == 0) then
    !         call LOGMES('Error '//IAM//': not possible',.true.)
    !         call logmes(cout, .true.)
    !         err= 1
    !         return
    !       endif
    !       write(cout,*) 'opposed HE'
    !       call logmes(cout, .true.)
    !       HE_aux = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f)
    !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !       call logmes(cout, .true.)
    !     else
    !       HE_aux = previous_HE(HE,err_)
    !       write(cout,*) 'previous HE'
    !       call logmes(cout, .true.)
    !       write(cout,*) 'rang: ',HE_aux%i,' face ',HE_aux%f
    !       call logmes(cout, .true.)
    !       if (HE_aux%i == 0) then
    !         call LOGMES('Error '//IAM//': not possible',.true.)
    !         call logmes(cout, .true.)
    !         err= 1
    !         return
    !       endif
    !       write(cout,*) 'opposed HE'
    !       call logmes(cout, .true.)
    !       HE_aux = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f)
    !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !       call logmes(cout, .true.)
    !     endif
    !   enddo

    ! end if

    return
  else
    allocate(HE_Hdl%B2opHE(nb_bord))
    allocate(HE_Hdl%B2cnt(nb_bord))
  endif

  nb_bord=0
  do f=1,HE_Hdl%nb_faces
    do i=1,3
      if (tag(i,f) == 0) then
        v = HE_Hdl%faces(i,f)
        nb_bord=nb_bord+1
        HE_Hdl%HE2opHE(i,f)%i=0;HE_Hdl%HE2opHE(i,f)%f=nb_bord
        HE_Hdl%V2HE(v)%i=0;HE_Hdl%V2HE(v)%f=nb_bord
        HE_Hdl%B2opHE(nb_bord)%i=i ; HE_Hdl%B2opHE(nb_bord)%f=f
      endif
    enddo
  enddo

  ! construction des contours (liste fermees de bords)
  allocate(contour(nb_bord))
  HE_Hdl%B2cnt=0
  nb_cnt=0
  do 
   !construction d'un nouveau contour
   start = .true.
   closed = .false.
   do bi=1,nb_bord

     !si bord deja dans un contour on passe 
     if (HE_Hdl%B2cnt(bi) /= 0) cycle

     !on cree un nouveau contour
     if (start) then
       !print*,'on cree un nouveau contour' 
       nb_cnt = nb_cnt + 1 
       HE_Hdl%B2cnt(bi) = nb_cnt
       !print*,'le bord ',bi,' en fait partie'
       HE = HE_Hdl%B2opHE(bi)
       !vertex de depart du bord
       v_i = HE_Hdl%faces(HE%i,HE%f)
       
       HE = next_HE(HE,err_)
       if (err_ > 0 ) then
         !call logmes() 
         err= 1
         return
       endif  
       ! vertex de fin du bord
       v_f = HE_Hdl%faces(HE%i,HE%f)     

       !print*,'depart ',v_i,' arrivee ', v_f
       start = .false.
     endif
     !on cherche les bords suivants
     do
       empty_loop = .true.
       do bj=1,nb_bord

         !si bord deja dans un contour on passe  
         if (HE_Hdl%B2cnt(bj) /= 0) cycle
          
         HE_aux = HE_Hdl%B2opHE(bj)    
         v_d = HE_Hdl%faces(HE_aux%i,HE_aux%f)
         
         !si on a trouve le bord suivant
         if (v_d == v_f) then               
           !print*,'le bord ',bj,' en fait partie'
           HE_Hdl%B2cnt(bj) = nb_cnt        
           HE = next_HE(HE_aux,err_)
           if (err_ > 0) then
             !call logmes() 
             err= 1
             return
           endif  
           !vertex de fin du bord
           v_f = HE_Hdl%faces(HE%i,HE%f)
           !print*,'depart ',v_d,' arrivee ', v_f

           empty_loop = .false.
           !si on est revenu au point de depart           
           if (v_f == v_i) closed = .true.  
           exit         
         endif
       enddo
      
       !si le contour est ferme on sort de la boucle de recherche 
       if (closed) exit

       !si on a tout teste et que le contour est ouvert on a une erreur (non-manifold ?)
       if (empty_loop) then
         call LOGMES('Error '//IAM//': Humm contour not closed impossible',.true.)
         err= 1
         return
       endif
     enddo
   enddo
   if (start) exit ! on n'a pas trouve de nouveau contour a creer
  enddo

  allocate(HE_Hdl%cnts(nb_cnt))
  do c=1,nb_cnt
    n = count(HE_Hdl%B2cnt(:) == c) 
    if (n < 2) then
      call LOGMES(IAM//' Humm strange contour',.true.)
      err= 1
      return
    endif
    allocate(HE_Hdl%cnts(c)%G_i(n))
  enddo

  HE_Hdl%B2cnt = 0
  do c=1,nb_cnt
    start = .true.
    closed = .false.
    idx = 0
    do bi=1,nb_bord
      if (HE_Hdl%B2cnt(bi) /= 0) cycle
      if (start) then
        HE_Hdl%B2cnt(bi) = c
        !
        idx = idx + 1
        HE_Hdl%cnts(c)%G_i(idx)=bi
        !
        HE = HE_Hdl%B2opHE(bi)
        ! point de depart
        v_i = HE_Hdl%faces(HE%i,HE%f)   
        !
        HE = next_HE(HE,err_)
        if (err_ > 0) then
          !call logmes() 
          err= 1
          return
        endif  

        v_f = HE_Hdl%faces(HE%i,HE%f)   ! le suivant
        start = .false.
      endif
      !
      do 
        empty_loop = .true.
        do bj=1,nb_bord
          if (HE_Hdl%B2cnt(bj) /= 0) cycle
          HE_aux = HE_Hdl%B2opHE(bj)    
          v_d = HE_Hdl%faces(HE_aux%i,HE_aux%f)          
          if (v_d == v_f) then
            HE_Hdl%B2cnt(bj) = c
            !
            idx = idx + 1
            HE_Hdl%cnts(c)%G_i(idx)=bj
            !
            HE = next_HE(HE_aux,err_)
            if (err_ > 0) then
              !call logmes() 
              err= 1
              return
            endif  

            v_f = HE_Hdl%faces(HE%i,HE%f)    ! le suivant
            empty_loop = .false.
            if (v_f == v_i) closed = .true.  ! on est revenu au point de depart
            exit         
          endif
        enddo
        if (closed) exit
        if (empty_loop) then
          call LOGMES('Error '//IAM//': Humm contour not closed impossible',.true.)
          err= 1
          return
        endif
      enddo
    enddo
    if (start) then
      call LOGMES('Error '//IAM//': Humm strange',.true.)
      err= 1
      return
    endif
  enddo

  ! on corrige V2HE pour ne pas avoir de bord
  do c=1,size(HE_Hdl%cnts)
    do ic=1,size(HE_Hdl%cnts(c)%G_i)
      b = HE_Hdl%cnts(c)%G_i(ic)
      vd = HE_Hdl%faces(HE_Hdl%B2opHE(b)%i,HE_Hdl%B2opHE(b)%f) 
      if (HE_Hdl%V2HE(vd)%i == 0) then
        HE_Hdl%V2HE(vd)=HE_Hdl%B2opHE(b)
      endif
    enddo
  enddo

  ! utiliser info_he_hdl
  !
  ! if (itchatche) then
  !   do c=1,size(HE_Hdl%cnts)
  !     write(cout,*) '=================='
  !     call logmes(cout, .true.)
  !     write(cout,*) 'contour ',c
  !     call logmes(cout, .true.)
  !     do ic=1,size(HE_Hdl%cnts(c)%G_i)
  !       b = HE_Hdl%cnts(c)%G_i(ic)
  !       write(cout,*) 'rank ',ic,' borded ',b,' vertex ',HE_Hdl%faces(HE_Hdl%B2opHE(b)%i,HE_Hdl%B2opHE(b)%f)
  !       call logmes(cout, .true.)
  !     enddo
  !   enddo

  !   do c=1,HE_Hdl%nb_vertex
  !     write(cout,*) '=================='
  !     call logmes(cout, .true.)
  !     write(cout,*) 'HE linked to vertex ',c
  !     call logmes(cout, .true.)
  !     HE=HE_Hdl%V2HE(c)
  !     write(cout,*) 'rank: ',HE%i,' face ',HE%f
  !     call logmes(cout, .true.)
  !     if (HE%i == 0) then
  !       write(cout,*) 'HE opposed to boundary'
  !       call logmes(cout, .true.)
  !       HE_aux = HE_Hdl%B2opHE(HE%f)
  !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
  !       call logmes(cout, .true.)
  !       write(cout,*) 'staring vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
  !       call logmes(cout, .true.)
  !       HE_aux = next_HE(HE_aux,err_)
  !       write(cout,*) 'ending vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
  !       call logmes(cout, .true.)
  !     else
  !       write(cout,*) 'HE opposed to HE'
  !       call logmes(cout, .true.)
  !       HE_aux = HE_Hdl%HE2opHE(HE%i,HE%f)
  !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
  !       call logmes(cout, .true.)
  !     endif
  !     !
  !     if (HE%i == 0) then
  !       HE_aux = HE_Hdl%B2opHE(HE%f)
  !       HE_aux = previous_HE(HE_aux,err_)
  !       write(cout,*) 'previous HE'
  !       call logmes(cout, .true.)
  !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
  !       call logmes(cout, .true.)
  !       if (HE_aux%i == 0) then
  !         call LOGMES('Error '//IAM//': not possible',.true.)
  !         err=1
  !         return
  !       endif
  !       write(cout,*) 'opposed HE'
  !       call logmes(cout, .true.)
  !       HE_aux = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f)
  !       write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
  !       call logmes(cout, .true.)
  !     else
  !      HE_aux = previous_HE(HE,err_)
  !      write(cout,*) 'previous HE'
  !      call logmes(cout, .true.)
  !      write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
  !      call logmes(cout, .true.)
  !      if (HE_aux%i == 0) then
  !         call LOGMES('Error '//IAM//': not possible',.true.)
  !         err= 1
  !         return
  !      endif
  !      write(cout,*) 'opposed HE'
  !      call logmes(cout, .true.)
  !      HE_aux = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f)
  !      write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
  !      call logmes(cout, .true.)
  !     endif
  !   enddo

  ! end if

  deallocate(tempo,idx_tempo,tag,contour)
  
end subroutine


subroutine info_HE_Hdl(HE_Hdl)
  implicit none
  type(T_HE_Hdl)    :: HE_Hdl
  !
  integer(kind=4)   :: iv,ic,ib,err_,err,b
  type(T_HE)        :: HE_aux,HE
  logical           :: found

                           !12345678901234567890123456789
  character(len=29) :: IAM='DiscreteGeometry::info_HE_Hdl'

  character(len=120):: cout

  write(cout,*) '=================='
  call logmes(cout, .true.)
  
  print*,'nb vertex  ',HE_Hdl%nb_vertex
  print*,'nb faces   ',HE_Hdl%nb_faces
  print*,'nb contours',size(HE_Hdl%cnts)
  

  do iv=1,HE_Hdl%nb_vertex

     print*,'   '
     print*,'** HE'
     print*,'   '     
     
     write(cout,*) 'HE linked to vertex ',iv
     call logmes(cout, .true.)
     
     HE=HE_Hdl%V2HE(iv)
     write(cout,*) 'rank: ',HE%i,' face ',HE%f
     call logmes(cout, .true.)

     if (HE%i == 0) then
        write(cout,*) 'by construction a vertex can not be related to a bord'        
        call FATERR(IAM,cout)
     endif
        
     write(cout,*) 'starting vertex ',HE_Hdl%faces(HE%i,HE%f)
     call logmes(cout, .true.)

     print*,'   '
     print*,'*** concerning opposite HE'
     print*,'   '
     
     HE_aux = HE_Hdl%HE2opHE(HE%i,HE%f)
     write(cout,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
     call logmes(cout, .true.)

     if (HE_aux%i /= 0) then
       write(cout,*) 'HE opposed to HE'
       call logmes(cout, .true.)
     
       write(cout,*) 'starting vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
       call logmes(cout, .true.)
       
       HE_aux = next_HE(HE_aux,err_)
       write(cout,*) 'ending vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
       call logmes(cout, .true.)

     else
       write(cout,*) 'HE opposed to a bord'
       call logmes(cout, .true.)

       print*,'assumed contour ',HE_Hdl%b2cnt(HE_aux%f)
       
       found=.FALSE.
       do ic=1,size(HE_Hdl%cnts)
         do ib=1,size(HE_Hdl%cnts(ic)%G_i)
           b = HE_Hdl%cnts(ic)%G_i(ib)
           if (b == HE_aux%f) then
             found =.TRUE.
             exit
           endif  
         enddo
         if (found) exit  
       enddo

       if (found) then
          write(cout,*) 'rank ',ib,' bord ',b,' vertex ',HE_Hdl%faces(HE_Hdl%B2opHE(b)%i,HE_Hdl%B2opHE(b)%f)
          call logmes(cout, .true.)
       else
          call faterr(IAM,'cant find bord')          
       endif
     endif  
     print*,'   '     
     print*,'*** concerning nodes of the face of the HE'
     print*,'   '

     HE_aux = next_HE(HE,err_)
       
     if (HE_aux%i == 0) then
       call LOGMES('Error '//IAM//': next is a bord which is not possible',.true.)
       call logmes(cout, .true.)
       err= 1
       return
     endif

     write(cout,*) 'next vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
     call logmes(cout, .true.)
       
     HE_aux = next_HE(HE_aux,err_)

     if (HE_aux%i == 0) then
       call LOGMES('Error '//IAM//': next next is a bord which is not possible',.true.)
       call logmes(cout, .true.)
       err= 1
       return
     endif
     
     write(cout,*) 'next next vertex ',HE_Hdl%faces(HE_aux%i,HE_aux%f)
     call logmes(cout, .true.)
       
  enddo

  print*,'   '
  print*,'** contours'
  print*,'   '
  
  do ic=1,size(HE_Hdl%cnts)

    write(cout,*) 'contour ',ic
    call logmes(cout, .true.)
    
    do ib=1,size(HE_Hdl%cnts(ic)%G_i)
       b = HE_Hdl%cnts(ic)%G_i(ib)
       He%i=HE_Hdl%B2opHE(b)%i
       He%f=HE_Hdl%B2opHE(b)%f
       He_aux=next_HE(He,err_)
       write(cout,*) 'rank ',ib,' bord ',b,' vertex ',HE_Hdl%faces(He%i,He%f),HE_Hdl%faces(HE_aux%i,HE_aux%f)
       call logmes(cout, .true.)
    enddo
  enddo

end subroutine
  
!> erasing HE
subroutine erase_HE_Hdl(HE_Hdl)
  implicit none
  type(T_HE_Hdl) :: HE_Hdl
  !
  integer(kind=4) :: i

  HE_Hdl%max_adj_faces = 0
  HE_Hdl%nb_vertex     = 0
  HE_Hdl%nb_faces      = 0

  if( associated(HE_Hdl%faces) ) then
    deallocate(HE_Hdl%faces)
    nullify(HE_Hdl%faces)
  end if

  if( associated(HE_Hdl%V2HE) ) then
    deallocate(HE_Hdl%V2HE)
    nullify(HE_Hdl%V2HE)
  end if
  if( associated(HE_Hdl%HE2opHE) ) then
    deallocate(HE_Hdl%HE2opHE)
    nullify(HE_Hdl%HE2opHE)
  end if
  if( associated(HE_Hdl%B2opHE) ) then
    deallocate(HE_Hdl%B2opHE)
    nullify(HE_Hdl%B2opHE)
  end if

  if( associated(HE_Hdl%b2cnt) ) then
    deallocate(HE_Hdl%b2cnt)
    nullify(HE_Hdl%b2cnt)
  end if

  if( associated(HE_Hdl%cnts) ) then
    do i = 1, size(HE_Hdl%cnts)
      if( associated(HE_Hdl%cnts(i)%G_i) ) deallocate(HE_Hdl%cnts(i)%G_i)
    end do
    deallocate(HE_Hdl%cnts)
    nullify(HE_Hdl%cnts)
  end if

  if( associated(HE_Hdl%vertex) ) then
    !rm done in xSpxx
    !deallocate(HE_Hdl%vertex)
    nullify(HE_Hdl%vertex)
  end if
  if( associated(HE_Hdl%normal) ) then
    if( HE_Hdl%is_normal_mine ) deallocate(HE_Hdl%normal)
    nullify(HE_Hdl%normal)
  end if
  if( associated(HE_Hdl%HEorien) ) then
    deallocate(HE_Hdl%HEorien)
    nullify(HE_Hdl%HEorien)
  end if

  HE_Hdl%if = 0
  HE_Hdl%reverse = .false.

end subroutine

!> updates vertices positions, edges orientations and faces normal 
subroutine update_HE_Hdl(HE_Hdl,vertex,err,normal)
  implicit none
  type(T_HE_Hdl) :: HE_Hdl
  real(kind=8),pointer   :: vertex(:,:)
  real(kind=8),pointer,optional   :: normal(:,:)
  integer :: err
  ! ***
  real(kind=8) :: vv(3),d1(3),d2(3)
  integer :: f,i,vd,vf

                          !1234567890123456789012345678901
  character(len=31):: IAM='DiscreteGeometry::update_HE_Hdl'

  err=0

  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err=1
    return
  endif

  HE_Hdl%vertex => vertex

  do f=1,He_Hdl%nb_faces

    do i=1,3
      vd = HE_Hdl%faces(i,f)
      vf = HE_Hdl%faces(modulo(i,3)+1,f)
      vv(:) = HE_Hdl%vertex(:,vf) - HE_Hdl%vertex(:,vd)
      HE_Hdl%HEorien(:,i,f)= vv(:) / length3(vv)
    enddo

    if (present(normal) ) then
      if(HE_Hdl%is_normal_mine) then
        deallocate(HE_Hdl%normal)
        HE_Hdl%is_normal_mine = .false.
      end if
      HE_Hdl%normal => normal
    else
      d1(:) = vertex(:,HE_Hdl%faces(2,f)) - vertex(:,HE_Hdl%faces(1,f))
      d2(:) = vertex(:,HE_Hdl%faces(3,f)) - vertex(:,HE_Hdl%faces(1,f))

      vv = cross_product(d1,d2)

      HE_Hdl%normal(:,f) = vv/length3(vv)

    endif

    if (HE_Hdl%reverse) HE_Hdl%normal(:,f) = -HE_Hdl%normal(:,f)

!    print*,f
!    print*,d1
!    print*,d2
!    print*,HE_Hdl%normal(:,f)



  enddo

end subroutine


!> computes nodes normals 
!> \todo: ponderer la moyenne par la surface des elements 
subroutine get_nodal_normals_HE_Hdl(HE_Hdl,normal,err)
  implicit none
  type(T_HE_Hdl)       :: HE_Hdl
  real(kind=8),pointer :: normal(:,:)
  integer              :: err,err_
  ! ***
  real(kind=8) :: vv(3)
  integer :: f,vd,f0,face0,nb_vv,cnt_b,idx_b,b
  type(T_HE) :: HE,HE_aux

  logical :: itchatche = .false.
  logical :: bord,go_inside,au_bord,bord0

                          !123456789012345678901234567890123456789012
  character(len=42):: IAM='DiscreteGeometry::get_nodal_normals_HE_Hdl'
   
  err = 0

  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
     call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
     err = 1
     return
  endif

  if (associated(normal)) deallocate(normal)
  allocate(normal(3,HE_Hdl%nb_vertex))

  if (itchatche) print*,IAM

  DO vd=1,HE_Hdl%nb_vertex

    if (itchatche) then 
      print*,'========='
      print*,'vertex ',vd
    endif

    HE = HE_Hdl%V2HE(vd)

    f0 = HE%f

    if (itchatche) then 
      print*,'face  ',f0
      print*,'sommet', HE%i
    endif

    
    if (HE%i /= 0) then
      !on commence par une face normale
      bord0 = .false.
      face0 = f0
    else
      !on commence par un bord
      bord0=.true.
      HE_aux=HE_Hdl%b2opHE(f0)
      face0 = HE_aux%f

      !print*,HE_aux%i,HE_aux%f

    endif

    ! parcours des arretes autour de vd

    bord = .false.
    go_inside = .true.
    au_bord=.false.

    vv=0.d0
    nb_vv=0

    do   
      if (HE%i == 0) then
        ! je suis un bord

        if (itchatche) print*,'je suis un bord'

        bord = .true.
        au_bord=.true.

        ! on ne fait rien

      else
        vv = vv + HE_Hdl%normal(:,HE%f)
        nb_vv = nb_vv + 1
      endif

      ! on passe au suivant

      if (.not. bord) then
        ! on arrive par un HE 

        HE_aux=previous_HE(HE,err_)

        if (err_ > 0) then
          !call logmes() 
          err= 1
          return
        endif  
         
        HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
      
        if (itchatche) then
          print*,'previous HE',HE_aux%i,HE_aux%f,HE_Hdl%faces(HE_aux%i,HE_aux%f)
          print*,'He suivant ',HE%i,HE%f
        endif

        go_inside = .false.

      else

        ! on arrive par un bord

        if (go_inside) then
          ! on rentre dans les faces
          ! car on commence ou on vient d'un autre bord

          if (itchatche) then
            print*,'on entre dans la matiere'
            print*,'par ',HE%i,HE%f
          endif

          !%< 28/02/2012
          !fd modif ca me semble faux 
          !en fait on doit prendre le HE juste de l'autre cote du bord
          HE  = HE_Hdl%B2opHE(HE%f)


          ! ce truc saute celui juste de l'autre cote pour aller chercher le suivant !?
          !HE_aux = HE_Hdl%B2opHE(HE%f)
          !HE_aux=previous_HE(HE_aux)
          !HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
          !%< 28/02/2012

          if (itchatche) print*,'He suivant ',HE%i,HE%f

          go_inside = .false.

        else
         ! on va chercher le bord suivant dans le contour
         ! car on vient de la matiere 

          if (itchatche) then
            print*,'on sort de la matiere'
            print*,'on arrive du bord',HE%f
          endif 

          cnt_b = HE_Hdl%b2cnt(HE%f)
          do idx_b=1,size(HE_Hdl%cnts(cnt_b)%G_i)
            if (HE_Hdl%cnts(cnt_b)%G_i(idx_b) == HE%f) exit
          enddo
          if (idx_b > size(HE_Hdl%cnts(cnt_b)%G_i)) then
            call logmes(IAM//' Humm boundary not in contour',.true.)
            err= 1
            return
          endif
          b = HE_Hdl%cnts(cnt_b)%G_i(modulo(idx_b,size(HE_Hdl%cnts(cnt_b)%G_i))+1)

          !print*,'on passe au bord suivant',b
      
          HE%i=0; HE%f=b
          go_inside = .true.

          if (itchatche) print*,'bord suivant ',HE%i,HE%f

        endif
      endif

      bord = .false.

      if (((He%i==0 .and. bord0) .or. (He%i /=0 .and. .not. bord0)) .and. &
          (HE%f == f0)) then
         if (itchatche) print*,'on a fait le tour'
        exit ! on a fait le tour
      endif
    enddo

    if (nb_vv /= 0) then
      normal(:,vd)=vv/length3(vv)
    else
      !cout=''
      !write(cout,'(I0,1x,I0)') vd,HE_Hdl%nb_vertex
      !call logmes(cout)
      !write(*,'(3(1x,I0))') HE_Hdl%faces    
      call logmes('Error in '//IAM//' node without neighbours - impossible',.true.)
      err= 1
      return
    endif
  enddo

end subroutine

!> computes faces normals 
subroutine get_face_normals_HE_Hdl(HE_Hdl,normal,err)
  implicit none
  type(T_HE_Hdl)       :: HE_Hdl
  real(kind=8),pointer :: normal(:,:)
  integer              :: err
  ! ***
  integer :: f

                          !12345678901234567890123456789012345678901
  character(len=41):: IAM='DiscreteGeometry::get_face_normals_HE_Hdl'

  err=0
   
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err=1
    return
  endif

  if (.not. associated(HE_Hdl%normal)) then
    call LOGMES(IAM//' normals not available you should update the HE_Hdl',.true.)
    err=1
    return
  endif

  if (associated(normal)) deallocate(normal)
  allocate(normal(3,HE_Hdl%nb_faces))

  do f=1,He_Hdl%nb_faces
    normal(:,f) =  HE_Hdl%normal(:,f)  
  enddo
end subroutine

!> computes nodes radius 
subroutine get_nodal_radius_HE_Hdl(HE_Hdl,radius,err)
  implicit none
  type(T_HE_Hdl)       :: HE_Hdl
  real(kind=8),pointer :: radius(:)
  integer              :: err,err_
  
  ! ***
  real(kind=8) :: vv(3)
  integer :: f,vd,vf,f0,face0,nb_vv,cnt_b,idx_b,b
  type(T_HE) :: HE,HE_aux

  logical :: itchatche = .false.
  logical :: bord,go_inside,au_bord,bord0

                          !123456789012345678901234567890123456789012
  character(len=42):: IAM='DiscreteGeometry::get_nodal_radius_HE_Hdl'

  err=0
   
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err=1
    return
  endif

  if (associated(radius)) deallocate(radius)
  allocate(radius(HE_Hdl%nb_vertex))
  radius=0.d0

  !itchatche=.true.

  if (itchatche) print*,IAM

  DO vd=1,HE_Hdl%nb_vertex

    if (itchatche) then 
      print*,'========='
      print*,'vertex ',vd
    endif

    HE = HE_Hdl%V2HE(vd)

    f0 = HE%f

    if (HE%i /= 0) then
      !on commence par une face interieure
      bord0 = .false.
      face0 = f0
    else
      !on commence par un bord
      bord0=.true.
      HE_aux=HE_Hdl%b2opHE(f0)
      face0 = HE_aux%f

      !print*,HE_aux%i,HE_aux%f

    endif

    ! parcours des arretes autour de vd

    bord = .false.
    go_inside = .true.
    au_bord=.false.

    vv=0.d0

    do   
      if (HE%i == 0) then
        ! je suis un bord

        if (itchatche) print*,'je suis un bord'

        bord = .true.
        au_bord=.true.

        ! on ne fait rien

      else

        !i = HE_Hdl%faces(HE_aux%i,HE_aux%f)

        vf = HE_Hdl%faces(modulo(He%i,3)+1,HE%f)
        vv(:) = HE_Hdl%vertex(:,vf) - HE_Hdl%vertex(:,vd)
        
        radius(vd) = max(radius(vd),length3(vv))

      endif

      ! on passe au suivant

      if (.not. bord) then
        ! on arrive par un HE 

        HE_aux=previous_HE(HE,err_)
         
        if (err_ > 0) then
          !call logmes() 
          err=1
          return
        endif  
         
        HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
      
        if (itchatche) then
          print*,'previous HE',HE_aux%i,HE_aux%f,HE_Hdl%faces(HE_aux%i,HE_aux%f)
          print*,'He suivant ',HE%i,HE%f
        endif

        go_inside = .false.

      else

        ! on arrive par un bord

        if (go_inside) then
          ! on rentre dans les faces
          ! car on commence ou on vient d'un autre bord

          if (itchatche) then
            print*,'on entre dans la matiere'
            print*,'par ',HE%i,HE%f
          endif

          !%< 28/02/2012
          !fd modif ca me semble faux 
          !en fait on doit prendre le HE juste de l'autre cote du bord
          HE  = HE_Hdl%B2opHE(HE%f)


          ! ce truc saute celui juste de l'autre cote pour aller chercher le suivant !?
          !HE_aux = HE_Hdl%B2opHE(HE%f)
          !HE_aux=previous_HE(HE_aux)
          !HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
          !%< 28/02/2012

          if (itchatche) print*,'He suivant ',HE%i,HE%f

          go_inside = .false.

        else
         ! on va chercher le bord suivant dans le contour
         ! car on vient de la matiere 

          if (itchatche) then
            print*,'on sort de la matiere'
            print*,'on arrive du bord',HE%f
          endif 

          cnt_b = HE_Hdl%b2cnt(HE%f)
          do idx_b=1,size(HE_Hdl%cnts(cnt_b)%G_i)
            if (HE_Hdl%cnts(cnt_b)%G_i(idx_b) == HE%f) exit
          enddo
          if (idx_b > size(HE_Hdl%cnts(cnt_b)%G_i)) then
            call logmes(IAM//' Humm boundary not in contour',.true.)
            err=1
            return
          endif
          b = HE_Hdl%cnts(cnt_b)%G_i(modulo(idx_b,size(HE_Hdl%cnts(cnt_b)%G_i))+1)

          !print*,'on passe au bord suivant',b
      
          HE%i=0; HE%f=b
          go_inside = .true.

          if (itchatche) print*,'bord suivant ',HE%i,HE%f

        endif
      endif

      bord = .false.

      if (((He%i==0 .and. bord0) .or. (He%i /=0 .and. .not. bord0)) .and. &
          (HE%f == f0)) then
         if (itchatche) print*,'on a fait le tour'
        exit ! on a fait le tour
      endif
    enddo

    !if (nb_vv /= 0) then
    !  !normal(:,vd)=vv/real(nb_vv)
    !else
    !  write(*,'(I0,1x,I0)'),vd,HE_Hdl%nb_vertex
    !  write(*,'(3(1x,I0))'),HE_Hdl%faces    
    !  call FATERR(IAM,' node without neighbours - impossible') 
    !endif
  enddo

end subroutine

!> one looks for the proximal point in an HE_Hdl to a given point
!> HE_Hdl     the surface stored in a half hedge handler
!> cd_coor    the given node
!> halo       a given distance to reduce ppp search
!> dir        a given orientation to exclude faces
!> do_trim    triming contact on HE surface
!> ppp        I if /=0 ppp_ini O ppp
!> gap        distance
!> point      contact point
!> t n s      the local framework
!> f_out      face number
!> weight_out reduced coordinates in the face
!> bavard     log level
!> err        err level 0: ok 1: something wrong in the data
!> good_nodes reduce searching nodes
!> node_HE_Hdl_proximity= -99 pas ppp, =1 noeud, =2 arete, =3 face
!>
integer function node_HE_Hdl_proximity(HE_Hdl,cd_coor,halo,dir,do_trim,ppp,gap,point,t,n,s,f_out,weight_out,bavard,err,trim_angle,good_nodes)
  implicit none
  type(T_HE_Hdl)        :: HE_Hdl
  real(kind=8)          :: cd_coor(3),halo,dir(3)
  logical               :: do_trim
  real(kind=8)          :: gap,point(3),t(3),n(3),s(3)
  integer               :: ppp,f_out
  real(kind=8)          :: weight_out(3)
  logical               :: bavard
  real(kind=8),optional :: trim_angle
  integer,optional      :: good_nodes(:)
  integer               :: err,err_

  ! ***
  integer               :: i,j,k,f,min_vert,vd,f0,fj,ibad_k,iff  !,vf,vd_aux
  real(kind=8)          :: min_dist,dist,norm,vec(3),vec_aux(3),orien(3),vv(3),&
                           apab,pf(3,3),sens,weight(3),halo2
  type(T_HE)            :: min_HE,HE,HE_aux,HE_rust
  logical               :: itchatche,is_inside

  !***
  logical :: bord0,bord,go_inside,au_bord
  integer :: b,idx_b,cnt_b
  !***
  integer :: ppp_ini,face0
  !***
  ! mean normal
  real(kind=8),dimension(3) :: mean_normal

  !87.13 deg
  real(kind=8) :: tol_trim = 0.05
  
                          !123456789012345678901234567890123456789
  character(len=39):: IAM='DiscreteGeometry::node_HE_Hdl_proximity'


  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err=1
    return
  endif

  if (present(trim_angle)) then
      tol_trim=cos(trim_angle*PI_G/180.d0)
  endif   
  
  itchatche = bavard

  ! if (itchatche) then
    ! print*,"-----------------------------------"
    ! print*,"on entre dans node_HE_Hdl_proximity"
    ! print*,"cd_coor ",cd_coor
    ! print*,"cd_dir  ",dir
    ! print*,"ppp     ",ppp
  ! endif
  
  ! a voir 
  weight_out = 0.d0
  f_out=0
  !
  halo2=halo*halo 

  ppp_ini=0

  if (ppp /= 0 ) then
     
    ! si on a un point le plus proche
    ppp_ini=ppp
     
    ! on cherche autour pour voir si pas mieux
     
    ! on rentre avec un ppp connu

    vec(:) = cd_coor(:) - HE_Hdl%vertex(:,ppp)
    dist = dot_product(vec,vec)  ! pas length() pour economiser la sqrt

    !print*,'vertex an: ',i
    !print*,HE_Hdl%vertex(:,i)
    !print*,dist,halo2

    ! on garde la distance comme reference meme si pas dans le halo
    min_dist = dist
    min_vert = ppp
    
    ! on recherche un nouveau ppp de proche en proche
    ! la boucle interieure tourne autour du dernier ppp pour chercher un min_vert 
    ! on continue tant qu'on trouve un point plus proche
    do
      ! on repete la recherche jusqu'a ce que ça ne change plus 
      HE = HE_Hdl%V2HE(ppp)
       
      ! print*,'HE du ppp ',HE%i,HE%f
       

      bord = .false.
      go_inside = .true.
      
      ! recherche d'un vertex le plus proche
      ! il faut faire attention dans les parcours
      !   un HE part du vertex pp
      !   un bord est oriente dans le meme que son opHE
      !

      f0 = HE%f
      
      do ! parcours des arretes autour de min_vert avec prev+opHE ; si bord next + opHE

         bord = .false.
         if (HE%i == 0) then
           ! on est sur un bord
           bord = .true. 
           ! on va chercher le HE oppose pour avoir acces aux num de vertex
           HE = HE_Hdl%B2opHE(HE%f)

           if (HE_Hdl%faces(HE%i,HE%f) /= ppp) then
             ! cas bord arrivant sur min_vert 
             ! rien a faire le vertex est l'origine de ce HE               
             HE_aux = HE
           else
             ! cas bord partant de min_vert 
             HE_aux = next_HE(HE,err_)
             if (err_>0) then
               !call logmes() 
                err=1
                return
             endif  
              
           endif
         else
           HE_aux = next_HE(HE,err_)
           if (err_ > 0) then
             !call logmes() 
             err=1
             return
           endif

           ! print*,'HE next ',HE_aux%i,HE_aux%f
         endif

         i = HE_Hdl%faces(HE_aux%i,HE_aux%f)
         vec(:) = cd_coor(:) - HE_Hdl%vertex(:,i)
         dist = dot_product(vec,vec)  ! pas length() pour economiser la sqrt

         !print*,'vertex an: ',i
         !print*,HE_Hdl%vertex(:,i)
         !print*,dist,halo2

         ! stupide if (dist > halo2) cycle
         if (dist < min_dist) then
           min_dist = dist
           min_vert = i
         endif

         !recherche du suivant

         if (.not. bord) then    
           ! on arrive par un HE

           ! print*,'on arrive de',HE%i,HE%f,HE_Hdl%faces(HE%i,HE%f)
            
           HE_aux=previous_HE(HE,err_)
           if (err_>0) then
             !call logmes() 
             err=1
             return
           endif  

           ! print*,'previous HE',HE_aux%i,HE_aux%f,HE_Hdl%faces(HE_aux%i,HE_aux%f)
  
           HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
      
           ! print*,'new HE',HE%i,HE%f,HE_Hdl%faces(HE%i,HE%f)

           go_inside = .false.

         else
           ! on est sur un bord
           ! on se remet sur le bord 
           HE = HE_Hdl%HE2opHE(HE%i,HE%f)
            
           if (go_inside) then    
             ! on vient d'un autre bord, on rentre dans les faces

             ! print*,'on entre dans la matiere'
             ! print*,'depuis ',HE%i,HE%f

             HE_aux = HE_Hdl%B2opHE(HE%f)

             ! print*,'dans ',HE_aux%i,HE_aux%f

             if (HE_aux%f == f0) then
               ! print*,'on a fait le tour'
               exit ! on a fait le tour
             endif

             HE_aux=previous_HE(HE_aux,err_)
             if (err_>0) then
               !call logmes() 
               err=1
               return
             endif  

             HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 

             ! print*,'nouveau HE',HE%i,HE%f
             
             go_inside = .false.

           else
             ! on va chercher le bord suivant dans le contour
             ! car on vient de sortir de la matiere 

             ! print*,'on arrive du bord',HE%i,HE%f

             cnt_b = HE_Hdl%b2cnt(HE%f)
             do idx_b=1,size(HE_Hdl%cnts(cnt_b)%G_i)
               if (HE_Hdl%cnts(cnt_b)%G_i(idx_b) == HE%f) exit
             enddo
             if (idx_b > size(HE_Hdl%cnts(cnt_b)%G_i)) then
               print*,'contour ',cnt_b,' bord ',He%f,' rank ',He%i
               call logmes(IAM//' Humm boundary not in contour',.true.)
               err=1
               return
             endif
             b = HE_Hdl%cnts(cnt_b)%G_i(modulo(idx_b,size(HE_Hdl%cnts(cnt_b)%G_i))+1)

             ! print*,'on passe au bord suivant',b
      
             HE%i=0; HE%f=b

             go_inside = .true.

           endif
         endif

         if (HE%f == f0) then
           !print*,'on a fait le tour'
           exit ! on a fait le tour
         endif

      enddo

      if (min_vert == ppp) then
        exit ! on n'a pas trouve mieux on sort
      else
        ppp = min_vert
        cycle
      endif
    enddo

    if (min_dist <= halo2) then
       ! on a trouve qqch qui convient
       ppp_ini = ppp
    else
       ! ca ne convient pas on force une recherche totale
       ppp_ini = 0
    endif
    
    min_vert = ppp_ini
    
  endif
  
  if (ppp_ini == 0 ) then
    ! si ppp_ini = 0 on recherche un point le plus proche (ppp)
    !
    min_dist = 1.d20
    min_vert = 0
  
    DO i=1,HE_Hdl%nb_vertex

       ! au cas ou on ne cherche pas sur tout
       if (present(good_nodes)) then
         if (good_nodes(i) == 0) cycle
       end if

       vec(:) = cd_coor(:) - HE_Hdl%vertex(:,i)
       dist = dot_product(vec,vec)  ! pas length() pour economiser la sqrt

       ! print*,'vertex an: ',i
       ! print*,HE_Hdl%vertex(:,i)
       !print*,dist,halo2

       if (dist > halo2) cycle
       if (dist < min_dist) then
          min_dist = dist
          min_vert =i
       endif
    ENDDO
  endif

  ! on n'a pas trouve de point le plus proche on arrete
  !
  if (min_vert == 0) then
    node_HE_Hdl_proximity=-99
    if (itchatche) print*,' pas de ppp dans ',halo2
    return
  endif

  ppp = min_vert

  min_HE%i=0; min_HE%f=0

  vec(:) = cd_coor(:) - HE_Hdl%vertex(:,min_vert)

  vd = min_vert
  HE = HE_Hdl%V2HE(vd)
  f0 = HE%f

  if (HE%i /= 0) then
    bord0 = .false.
    face0 = f0
  else
    bord0=.true.
    HE_aux=HE_Hdl%b2opHE(f0)
    face0 = HE_aux%f
  endif

  if (itchatche) then
     print*,'<- debut info He_hdl_proximity'
     print*,'cd_coor ', cd_coor(:)
     if (HE%i /= 0 ) then 
       print*,'vertex pp ',min_vert,' face liee ',f0
     else
       print*,'vertex pp ',min_vert,' bord lie ',f0,' face liee ',face0
     endif
     print*,'coor pp   ', HE_Hdl%vertex(:,min_vert)
     print*,'HE        ', He%i,He%f
     print*,'dist      ', min_dist,'halo ',halo
  endif

  min_dist = 1.d20
  
  bord = .false.
  go_inside = .true.
  au_bord=.false.

  mean_normal=0.d0

  ! recherche de l'arete la plus proche
  ! il faut faire attention dans le parcours
  !   un HE part du vertex pp
  !   un bord arrive sur le vertex pp
  !
  ! new 19/11/2010 
  ! lorque un noeud est sur un bord on le declare au_bord
  ! ca va permettre de prendre la normale a une face comme normale
  !
  do   ! parcours des arretes autour de vd
   
     ! si on est entre sur un bord
     if (HE%i == 0) then

       if (itchatche) print*,'on arrive sur un bord'

       HE = HE_Hdl%B2opHE(HE%f)
       bord = .true.
       au_bord=.true.
     endif

     orien(:) = HE_Hdl%HEorien(:,HE%i,HE%f)

     if (bord) then 
       if (HE_Hdl%faces(HE%i,HE%f) /= min_vert) orien = - orien
       HE = HE_Hdl%HE2opHE(HE%i,HE%f) ! on remet les choses a leur place
     endif

     ! coordonnee reduite sur l'arrete
     apab=dot_product(vec,orien)
     vv(:) = vec(:) - apab*orien(:)

     dist= length3(vv)

     if (itchatche) then
       print*,'orien ',orien
       print*,'dist a orien',dist
       print*,'apab sur orien',apab
     endif

     ! si apab < 0 on n'est pas sur l'arete 
     ! comme on part du ppp on ne cherche pas si ca sort a l'autre bout             
     !fd oups: en plus comme orien est unitaire apab n'est pas une coordonnee reduite ...

     if (apab .ge. 0.d0 .and. dist < min_dist) then
        min_dist = dist
        min_HE = HE
     endif

     ! recherche de l'arrete suivante
     if (.not. bord) then
       ! on arrive par un HE 

       HE_aux=previous_HE(HE,err_)
       if (err_ > 0) then
         !call logmes() 
         err = 1
         return
       endif  

       ! if (itchatche) print*,'previous HE',HE_aux%i,HE_aux%f,HE_Hdl%faces(HE_aux%i,HE_aux%f)
  
       HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
      
       if (itchatche) print*,'He suivant ',HE%i,HE%f

       go_inside = .false.

     else
       ! on arrive par un bord

       if (go_inside) then
         ! on rentre dans les faces
         ! car on commence ou on vient d'un autre bord

         if (itchatche) then
           print*,'on entre dans la matiere'
           print*,'par ',HE%i,HE%f
         endif

         !fd foireux !!
         
         ! HE_aux = HE_Hdl%B2opHE(HE%f)

         ! HE_aux=previous_HE(HE_aux, err_)
         ! if (err_ > 0) then
         !   !call logmes() 
         !   err= 1
         !   return
         !  endif  

         ! HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 

         HE= HE_Hdl%B2opHE(HE%f)

         if (itchatche) print*,'He suivant ',HE%i,HE%f

         go_inside = .false.

       else
        ! on va chercher le bord suivant dans le contour
        ! car on vient de la matiere 

         if (itchatche) then
           print*,'on sort de la matiere'
           print*,'on arrive du bord',HE%f
         endif 

         cnt_b = HE_Hdl%b2cnt(HE%f)
         do idx_b=1,size(HE_Hdl%cnts(cnt_b)%G_i)
           if (HE_Hdl%cnts(cnt_b)%G_i(idx_b) == HE%f) exit
         enddo
         if (idx_b > size(HE_Hdl%cnts(cnt_b)%G_i)) then
           call logmes(IAM//' Humm boundary not in contour',.true.)
           err= 1
           return
         endif
         b = HE_Hdl%cnts(cnt_b)%G_i(modulo(idx_b,size(HE_Hdl%cnts(cnt_b)%G_i))+1)

         !print*,'on passe au bord suivant',b
      
         HE%i=0; HE%f=b
         go_inside = .true.

         if (itchatche) print*,'bord suivant ',HE%i,HE%f

       endif
     endif

     if (He%i/=0) then
       mean_normal = mean_normal + HE_Hdl%normal(:,He%f)
     endif
        
     bord = .false.

     if (((He%i==0 .and. bord0) .or. (He%i /=0 .and. .not. bord0)) .and. &
         (HE%f == f0)) then
        if (itchatche) print*,'on a fait le tour'
       exit ! on a fait le tour
     endif


     !if (itchatche) 
     !  print*,'on continue' 
     !  if (HE%i /= 0) then
     !    print*,'opposite HE is a face ',HE%i,HE%f,HE_Hdl%faces(HE%i,HE%f)
     !  else
     !    print*,'opposite HE is a bord',HE%i,HE%f
     !  endif 
     !endif
  enddo

  if (min_HE%f == 0) then  ! le test min_HE%i = 0 est nul a cause des bords

     !TODO prendre une normale au vertex an comme moyenne des faces adjacentes  

     if (itchatche) print* ,'on voit la pointe'

     vec(:) = cd_coor(:) - HE_Hdl%vertex(:,min_vert)
     norm=length3(vv)
     if (norm /= 0.d0) then
       n = vv/norm
       if (itchatche) print*,'normale',n 
       if (do_trim .and. dot_product(dir,-n) < tol_trim ) then           
         if (itchatche) then
           print*,'orientation noeud(cd)-noeud(an) non compatible avec dir'
           print*,'on trim la pointe'
           print*,dir
           print*,n
         endif
         node_HE_Hdl_proximity = -99
         n=0.d0
         return
       endif   
     else
       if (itchatche) print*,'on est sur la pointe'          
     endif


     
     ! la surface forme une pointe
     ! on est sur le vertex
     ! on prend comme normale vec norme

     ! est on dedans (-1) ou dehors (1)

     if (au_bord) then

      ! on prend comme normale celle du HE qui porte le ppp

      !TODO calculer une normale au point comme moyenne 
      !     des normales des faces adjacentes29/03/16  pour ejecter ce test 


        
       !< fd
       ! avant ...
       ! n = HE_Hdl%normal(:,face0)
       ! ... apres  

       !todo: voir pourquoi dans certains cas dist == 0 en 3D
       !      j'ai colle une rustine avec l'ancienne methode
        
       dist=length3(mean_normal)
       if (dist /= 0.d0) then
         n=mean_normal/dist
       else
         n = HE_Hdl%normal(:,face0)
       endif

       !fd >
       
       gap=dot_product(vec,n)
     else
       !< fd
       ! avant ... 
       !sens=-sign(1.d0,dot_product(dir,vec))
       !
       !dist = sens*length3(vec)                  
       ! 
       !gap=dist
       !if (dist .ne. 0.d0) then
       !   n = vec/dist 
       !else
       !   n = -dir/length3(dir)
       !endif
       !
       ! ... apres
       dist=length3(mean_normal)
       if (dist == 0.d0) then
         call logmes('Error in '//IAM//' fucking dist -- case 2',.true.)
         err = 1
         return
       endif
      
       n=mean_normal/dist
       gap = dot_product(vec,n)
       !fd >
       
     endif

     ! test sur les normales 
     if (do_trim .and. dot_product(dir,-n) < tol_trim ) then           
       if (itchatche) print *,' on trim la pointe'
       node_HE_Hdl_proximity=-99
       return
     endif
     
     call comp_rep(t,n,s)

     point = HE_Hdl%vertex(:,min_vert) 

     f_out = face0 ! on prend comme face celle du HE qui porte le ppp
     do iff=1,3
       if (HE_Hdl%faces(iff,f_out) == min_vert) then 
         weight_out(iff) =1.d0
       else
         weight_out(iff) =0.d0
       endif
     enddo

     node_HE_Hdl_proximity=1

  else

     if (itchatche) then 
       if (min_HE%i /= 0) then
         HE_aux = next_HE(min_HE,err_)
         if (err_ > 0) then
           !call logmes() 
           err=1
           return
         endif  

         print*,' arete la plus proche',  &
         HE_Hdl%faces(min_HE%i,min_HE%f), &
         HE_Hdl%faces(HE_aux%i,HE_aux%f)
       
         HE_aux = HE_Hdl%HE2opHE(min_HE%i,min_HE%f)
         print*,' portee par faces ',min_HE%f,HE_aux%f
       else
         ! a faire pour les bords
       endif
     endif 

     ! on a trouve une arrete 
     ! on regarde les faces de chaques cotes

     min_dist = 1.d20

     HE = min_HE
     HE_aux = HE
     
     is_inside = .FALSE.

     min_HE%i=0;min_HE%f=0

     au_bord=.false.

     fj=0
     
     mean_normal = 0.d0
     
     !print*,'nouveau test'
     !print*,HE%i,HE%f
     do j=1,2
        !print*,'j: ',j
        if (j==2) then 
          if (HE_aux%i == 0) then
            HE_aux = HE_Hdl%B2opHE(HE%f)
          else
            HE_aux = HE_Hdl%HE2opHE(HE%i,HE%f)
            !print*,HE_aux%i,HE_aux%f
          endif
          if (HE_aux%i == 0) then
            au_bord=.true.
            !print*,j,' est un bord' 
            cycle  ! on est sur bord y a qu une face
          endif 
        else
          if (HE_aux%i == 0) then
            au_bord=.true.
            !print*,j,' est un bord'
            cycle  ! on est sur bord y a qu une face
          endif
        endif

        fj = HE_aux%f

        n = HE_Hdl%normal(:,fj)

        if (itchatche) then 
          print *,' face ',fj
          print *,' normale face ',n
          print *,' normale au point',dir
        endif

        if (do_trim .and. dot_product(dir,-n) < tol_trim ) then           
           if (itchatche) print *,' on trim la face'
           cycle
        endif

        mean_normal = mean_normal + n
        
        do k=1,3
           pf(:,k) = HE_Hdl%vertex(:,HE_Hdl%faces(k,fj))
        enddo

        if (itchatche) then 
          print*,'sommets de la face'
          write(*,'(3(1x,D12.5))') pf
        endif

        is_inside = node_triangle_normal_projection(pf,cd_coor,-n,dist,weight,ibad_k,itchatche)

        !print*,'on sort par', ibad_k

        !fd je vire la rustine
        ibad_k = 0

        if (is_inside) then
          ! ouf on est dedans

          if (itchatche) then 
            print*,'distance face ',dist
            print*,'dist min',min_dist 
          endif 
          if (dist < min_dist) then
            min_dist=dist
            min_HE=HE_aux
            ! pour dire qu'on y est arrive sans rustine
            ! fd a quoi ca sert ? 
            ! vd_aux=0

            ! construction du point sur la face
            point(:) = 0.d0
            do k=1,3
              point(:) = point(:) + weight(k)*pf(:,k)
            enddo

            ! on sort de la boucle
            exit 
          endif
        else

          ! rustine a la fred dub
          ! on teste la face par ou ca sort au cas ou 
          ! aberration geometrique 
          ! on est sortie par le cote k
          if (ibad_k /= 0 ) then

            if (itchatche) print*,'rustine'

            HE_rust = HE_Hdl%HE2opHE(ibad_k,fj)

            if (HE_rust%i /= 0) then
              fj = HE_rust%f
              n = HE_Hdl%normal(:,fj)

              do k=1,3
                pf(:,k) = HE_Hdl%vertex(:,HE_Hdl%faces(k,fj))
              enddo

              is_inside = node_triangle_normal_projection(pf,cd_coor,-n,dist,weight,ibad_k,itchatche)

              if (is_inside) then
                ! on est dedans
                if (dist < min_dist) then
                  ! construction du point sur la face
                  point(:) = 0.d0
                  do k=1,3
                    point(:) = point(:) + weight(k)*pf(:,k)
                  enddo

                  min_dist=dist
                  min_HE=HE_rust

                  !fd a quoi ca sert ?
                  !vd_aux=vd

                  if (itchatche) then
                     print*,'rustine'
                     print *,HE_rust%i,HE_rust%f
                  endif

                  ! on sort de la boucle !?  -
                  !exit 

                endif
              endif
            endif
          endif
        endif
     enddo

     bord=.false.

     if (min_HE%f == 0) then
        ! on est sur l'arete  

        if (itchatche) print*,'on voit une arete'

        ! HE contient le HE de depart, on s'en sert

        ! si on est entre sur un bord
        if (HE%i == 0) then
          HE = HE_Hdl%B2opHE(HE%f)
          bord = .true.
        endif

        orien(:) = HE_Hdl%HEorien(:,HE%i,HE%f)
 
        if (bord) then 
          if (HE_Hdl%faces(HE%i,HE%f) /= min_vert) orien = - orien
          ! orien = - orien                ! c'est forcement un bord 
          ! HE = HE_Hdl%HE2opHE(HE%i,HE%f) ! on ne remet pas les choses a leur place car on en a besoin
        endif

        apab = dot_product(vec,orien)
        vv = vec - apab*orien
        norm = length3(vv)
        
        if (norm /= 0.d0) then
          n = vv/norm
          if (itchatche) print*,'normale',n 
          if (do_trim .and. dot_product(dir,-n) < tol_trim ) then           
            if (itchatche) then
               print*,'orientation noeud(cd)-arete(an) non compatible avec dir'
               print*,' on trim l arete'
               print*,dir
               print*,n
             endif
             node_HE_Hdl_proximity = -99
             n=0.d0
             return
          endif   
        else
          if (itchatche) print*,'on est sur l"arete'          
        endif

        
        !fd 19/11/2010 pour eviter le pb des normales un peu bizarre sur un bord.
        !
        if (au_bord) then
          if (itchatche) print*,'cas au bord'           
          fj = HE%f
          n = HE_Hdl%normal(:,fj)
        else
          if (norm .ne. 0.d0) then
             !< fd
             ! avant ...
             !n = vv/norm
             ! on fait attention que la normale soit sortante
             !n=sign(1.d0,dot_product(HE_Hdl%normal(:,HE%f),n))*n
             ! ... apres
             dist=length3(mean_normal)
             if (dist == 0.d0) then
                
               if (itchatche) print*,'merdasse faces et aretes '
                   
               node_HE_Hdl_proximity = -99
               n=0.d0
               return
             endif
      
             n=mean_normal/dist
             
             ! fd >
          else
            fj = HE%f
            n = HE_Hdl%normal(:,fj)
          endif
        endif
        !fd si la normale trouvee n'est pas bonne par rapport 
        !fd a la dir de recherche on jarte
        if (do_trim .and. dot_product(dir,-n) < tol_trim ) then           
           if (itchatche) then
             print*,'orientation noeud(cd)-arete(an) non compatible avec dir'
             print*,' on trim la face'
             print*,dir
             print*,n
           endif

           node_HE_Hdl_proximity = -99
           n=0.d0
           return
        endif

        !fd point(:)=HE_Hdl%vertex(:,HE_Hdl%faces(HE%i,HE%f)) + apab*orien(:)
        point(:)=HE_Hdl%vertex(:,min_vert) + apab*orien(:)

        dist = dot_product(vec,n)
        gap = dist

        f_out = HE%f
        ! weight
        weight_out =0.d0
        !fd si bord dans le bon sens
        if (HE_Hdl%faces(He%i,He%f) == min_vert) then 
             weight_out(He%i) = apab
             weight_out(modulo(He%i,3)+1) = 1.d0 - apab
        else
             weight_out(He%i) = 1.d0 - apab
             weight_out(modulo(He%i+1,3)+1) = apab
        endif

        node_HE_Hdl_proximity=2

     else
        ! on est dans une face         

        if (itchatche) then 
          print *,' on est dans la face'
          print *,'face ',min_HE%f
          print *,'dist ',min_dist
          
        endif

        gap=min_dist

        fj = min_HE%f

        n = HE_Hdl%normal(:,fj) 

        !fd a quoi ca sert ?
        !if (vd_aux /=0) then
        !   vec_aux = cd_coor(:) - HE_Hdl%vertex(:,vd_aux)
        !   point = HE_Hdl%vertex(:,vd_aux) + vec_aux - min_dist*n
        !else
        point = HE_Hdl%vertex(:,min_vert) + vec - min_dist*n
        !endif

        !print *,'xxx'
        !print *,'n   ',n

        !print*,fj, HE_Hdl%faces(:,fj)

        !print*,HE_Hdl%vertex(:,HE_Hdl%faces(1,fj))
        !print*,HE_Hdl%vertex(:,HE_Hdl%faces(2,fj))
        !print*,HE_Hdl%vertex(:,HE_Hdl%faces(3,fj))
        !print *,'xxx'

        f_out = fj
        weight_out = weight

        node_HE_Hdl_proximity=3

     endif

     !if (itchatche) then
     !   print *,'ptc ',point(:)
     !   print *,'n   ',n
     !endif

     call comp_rep(t,n,s)

  endif

  if (itchatche) print*,' fin info He_hdl_proximity ->'


end function node_HE_Hdl_proximity

!> one looks for the proximal point in an HE_Hdl to a given point
!> HE_Hdl     the surface stored in a half hedge handler
!> cd_coor    the given node
!> halo       a given distance to reduce ppp search
!> dir        a given orientation to exclude faces
!> do_trim    triming contact on HE surface
!> ppp        I if /=0 ppp_ini O ppp
!> gap        distance
!> point      contact point
!> t n s      the local framework
!> f_out      face number
!> weight_out reduced coordinates in the face
!> bavard     log level
!> err        err level 0: ok 1: something wrong in the data
!> good_nodes reduce searching nodes
!> node_HE_Hdl_proximity= -99 pas ppp, =1 noeud, =2 arete, =3 face
!>
integer function new_node_HE_Hdl_proximity(HE_Hdl,cd_coor,halo,dir,do_trim,ppp,gap,point, &
                                           t,n,s,f_out,weight_out,bavard,err,trim_angle,good_nodes,with_extend)
  implicit none
  type(T_HE_Hdl)        :: HE_Hdl
  real(kind=8)          :: cd_coor(3),halo,dir(3)
  logical               :: do_trim
  real(kind=8)          :: gap,point(3),t(3),n(3),s(3)
  integer               :: ppp,f_out
  real(kind=8)          :: weight_out(3)
  logical               :: bavard
  integer               :: err,err_  
  real(kind=8),optional :: trim_angle
  integer,optional      :: good_nodes(:)
  logical,optional      :: with_extend

  ! ***
  integer               :: i,j,k,f,min_vert,vd,vf,f0,fj,ibad_k,iff,iout,nb_HE,min_f  !,vf,vd_aux
  real(kind=8)          :: min_dist,dist,min_dist2,dist2,norm,vec(3),vec_aux(3),orien(3),vv(3),&
                           apab,pf(3,3),sens,min_weight(3),fweight(3),fdist,halo2,edge(3),ww,proj
  type(T_HE)            :: min_HE,HE,HE_aux,HE_rust
  logical               :: itchatche,is_inside,extend
  
  
  type(T_HE),DIMENSION(100) :: liste_HE

  
  !***
  logical :: bord0,bord,go_inside,au_bord
  integer :: b,idx_b,cnt_b
  !***
  integer :: ppp_ini,face0
  !***
  ! mean normal
  real(kind=8),dimension(3) :: mean_normal

  !87.13 deg
  real(kind=8) :: tol_trim = 0.05
  
                          !123456789012345678901234567890123456789
  character(len=39):: IAM='DiscreteGeometry::node_HE_Hdl_proximity'

  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err=1
    return
  endif

  if (present(trim_angle)) then
      tol_trim=cos(trim_angle*PI_G/180.d0)
  endif
   
  if (present(with_extend)) then
     extend=with_extend
  else
     extend=.FALSE.
  endif   
  
  if (bavard) then
    print*,"-----------------------------------"
    print*,"on entre dans node_HE_Hdl_proximity"
    print*,"cd_coor ",cd_coor
    print*,"cd_dir  ",dir
    print*,"ppp     ",ppp
  endif
  
  ! a voir 
  weight_out = 0.d0
  f_out=0
  !
  halo2=halo*halo 

  ppp_ini=0

  ! si on propose un point le plus proche
  ! on cherche autour pour voir si pas mieux
  if (ppp /= 0 ) then

    ! on rentre avec un ppp connu

    vec(:) = cd_coor(:) - HE_Hdl%vertex(:,ppp)
    dist2 = dot_product(vec,vec)

    !print*,'vertex an: ',i
    !print*,HE_Hdl%vertex(:,i)
    !print*,dist,halo2

    ! on garde la distance comme reference meme si pas dans le halo
    min_dist2 = dist2
    min_vert = ppp

    if (present(good_nodes)) then 
      call update_proximal_point_HE_Hdl(HE_Hdl,cd_coor,dir,halo2,min_vert,min_dist2,extend,tol_trim,bavard,good_nodes)
    else
      call update_proximal_point_HE_Hdl(HE_Hdl,cd_coor,dir,halo2,min_vert,min_dist2,extend,tol_trim,bavard)
    endif

    ppp_ini = min_vert
    
  endif
  
  if (ppp_ini == 0 ) then
    ! si ppp_ini = 0 on recherche du point le plus proche (ppp) sur l'antagoniste
    !
    call get_proximal_point_HE_Hdl(HE_Hdl,cd_coor,halo,min_vert,min_dist,good_nodes)

  endif

  ! on n'a pas trouve de point le plus proche on arrete
  !
  if (min_vert == 0) then
    new_node_HE_Hdl_proximity=-99
    if (bavard) print*,' pas de ppp dans ',halo2
    return
  endif

  if (bavard) then
    print*,'min_vert', min_vert
    write(*,'("[",3(1x,E12.5,","),"],")') HE_Hdl%vertex(:,min_vert)
  endif   
  
  ! jusque la, le point le plus proche est un vertex
  new_node_HE_Hdl_proximity = 1

  min_HE%i=0; min_HE%f=0

  vec(:) = cd_coor(:) - HE_Hdl%vertex(:,min_vert)
  
  ! recupere tous les HE qui partent du vertex min_vert
  call get_all_HE_around_vertex(min_vert,HE_Hdl,extend,nb_HE,liste_HE)

  if (bavard) print*,'nb_HE ', nb_HE
  
  ! parcourt tous les HE autour de min_vert en passant les bords
  do i = 1,nb_HE

     if (bavard) print*,'[xxxxx index ', i
     
     HE = liste_HE(i)
     if (HE%i == 0) then
        !on evacue le cas ou on est sur un bord
        if (bavard) print*,'un bord'
        cycle
     endif   
     dist=1e20
     if ( extend ) then
        ! si la liste des HE est etendue aux voisins qui ferment le polygone
        ! c'est plus lourd car le point de depart des HE n'est pas tjs le meme

        if (bavard) print*,'extend'

        ! vertex de debut du edge
        vd       = HE_Hdl%faces(HE%i,HE%f)            
        
        mean_normal =  mean_normal + HE_Hdl%normal(:,HE%f)

        ! vertex de fin du edge
        HE_aux   = next_HE(HE,err_)
        if (err_ > 0) then
          !call logmes() 
          err= 1
          return
        endif  
        vf       = HE_Hdl%faces(HE_aux%i,HE_aux%f)    
        
        ! on verifie que les 2 extremites de ce HE appartiennent aux good nodes
        if (present(good_nodes)) then
           if ( good_nodes(vd) == 0 .or. good_nodes(vf) == 0 ) cycle
        endif

        if (bavard) then
          print*,'vd' 
          write(*,'("[",3(1x,E12.5,","),"],")') HE_Hdl%vertex(:,vd)
          print*,'vf'
          write(*,'("[",3(1x,E12.5,","),"],")') HE_Hdl%vertex(:,vf)           
        endif
        
        vec(:)   = cd_coor(:) - HE_Hdl%vertex(:,vd)
        edge(:)  = HE_Hdl%vertex(:,vf) - HE_Hdl%vertex(:,vd)
        orien(:) = HE_Hdl%HEorien(:,HE%i,HE%f)
        proj     = dot_product(vec,orien)

        if ( proj > 0. .and. proj < length3(edge) ) then
           ! calcul de la distance normale a l'arete
           vv(:) = vec(:) - proj*orien(:)
           dist  = length3(vv)
           if (dist < min_dist) then
              ! le point le plus proche est sur un edge
              new_node_HE_Hdl_proximity = 2
              min_dist                 = dist
              min_HE                   = HE
              if (bavard) print*,'dans l arete' 
           endif
        else
          if (bavard) print*,'pas dans l arete' 
        endif
     else
        ! sinon, on est sur que tous les HE partent du ppp, c'est plus simple
        orien(:)    =  HE_Hdl%HEorien(:,HE%i,HE%f)
        mean_normal =  mean_normal + HE_Hdl%normal(:,HE%f)

        ! on verifie que le bout de ce HE appartient aux good nodes
        if (present(good_nodes)) then
           HE_aux = next_HE(HE,err_)
           if (err_ > 0) then
             !call logmes() 
             err= 1
             return
           endif  
           ! vertex de fin du edge
           vf     = HE_Hdl%faces(HE_aux%i,HE_aux%f)    
           if ( good_nodes(vf) == 0 ) cycle
        endif

        if (bavard) then
           HE_aux = next_HE(HE,err_)
           vf     = HE_Hdl%faces(HE_aux%i,HE_aux%f)    ! vertex de fin du edge
           print*,'vf'
           write(*,'("[",3(1x,E12.5,","),"],")') HE_Hdl%vertex(:,vf)           
        endif
        
        ! projection de vec sur l'arrete
        proj  = dot_product(vec,orien)

        ! si apab < 0, on n'est pas sur l'arete 
        ! mais comme on part du ppp, on ne cherche pas si ca sort a l'autre bout             
        if (proj .ge. 0.d0) then
           ! calcul de la distance normale a l'arete
           vv(:) = vec(:) - proj*orien(:)
           dist  = length3(vv)
           if (dist < min_dist) then
              ! le point le plus proche est sur un edge
              new_node_HE_Hdl_proximity = 2
              min_dist                 = dist
              min_HE                   = HE
           endif
        endif
        
     endif

     if (bavard) then
       print*,'  --> orien = ',orien
       print*,'  --> proj  = ',proj
       if (extend) print*,'  --> length= ',length3(edge)
       print*,'  --> dist  = ',dist
       print*,'xxxxxx]'
     endif
        
  enddo

  if (min_HE%f /= 0) then
     ! on a trouve une arrete,
     ! donc on regarde les 2 faces de chaque cote de cette arete

     if (bavard) then
       print*,'on a trouve une arete'
       print*,'  min_dist = ',min_dist
     endif

     HE = min_HE

     ! on reinitialise la normale moyenne
     mean_normal = 0.d0

     ! par construction le min_HE ne peut pas etre un bord
     if (min_HE%i == 0) call faterr(IAM,'it can be a bord')
     
     do j = 1,2

        if (j == 2) then
          ! au 2eme passage, on prend le HE oppose pour regarder l'autre face
          HE = HE_Hdl%HE2opHE(HE%i,HE%f)
        endif
        
        ! si c'est un HE de bord, il n'y a pas de face adjacente
        if (HE%i == 0) cycle

        fj = HE%f
        n  = HE_Hdl%normal(:,fj)

        ! si l'angle entre les normales sort de 100-260 deg on ejecte
        if (do_trim .and. dot_product(dir,n) > -0.05 ) then
           if (bavard) print *,' on trim la face'
           cycle
        endif

        ! on ajoute la normale de la face
        mean_normal = mean_normal + n

        ! les 3 sommets de la face
        do k = 1,3
           pf(:,k) = HE_Hdl%vertex(:,HE_Hdl%faces(k,fj))
        enddo        
        
        ! est ce que le pt se projete dans la face
        !fd debile ...
        ! is_inside = node_triangle_normal_projection(pf,cd_coor,-dir,fdist,fweight,iout,bavard)

        is_inside = node_triangle_normal_projection(pf,cd_coor,n,fdist,fweight,iout,bavard)

        if (is_inside) then
           ! on recalcule vec pour etre sur
           vd     = HE_Hdl%faces(HE%i,HE%f)
           vec(:) = cd_coor(:) - HE_Hdl%vertex(:,vd)
           ! calcul de la distance a la face
           dist = dot_product(vec,n)
           
           if (dist < min_dist) then
              ! le point le plus proche est sur une face
              new_node_HE_Hdl_proximity = 3
              min_dist                 = dist
              min_f                    = fj
              min_weight               = fweight
           endif
        endif
        
        if (bavard) then
           print*,'side ',j
           print*,'  --> is_inside = ',is_inside
           print*,'  -->  min_dist = ',min_dist
        endif

     enddo
  endif

  ! print*,'------'
  ! print*,new_node_HE_Hdl_proximity,min_HE%i,min_HE%f,min_dist,min_f,min_weight
  ! print*,'------'
  
  !---------------------------------------------------------------------
  ! si contact avec un vertex
  if (new_node_HE_Hdl_proximity == 1) then

     if (bavard) print* ,'on est sur une pointe'

     ! ! le contact sur le bord d'un patch n'est pas conserve
     ! if (au_bord) then
     !    if (bavard) then
     !       print*,'contact sur un vertex du bord du patch'
     !       print *,' --> on supprime le contact'
     !    endif

     !    FC_node_HE_Hdl_proximity = -99
     !    n                        = 0.d0
     !    return

     ! endif

     ! on prend la normale moyenne au vertex
     dist = length3(mean_normal)
     if (dist == 0.d0) then
        new_node_HE_Hdl_proximity = -99
        n                        = 0.d0
        return
     endif
     n = mean_normal/dist

     !fd si la normale trouvee n'est pas bonne par rapport 
     !fd a la dir de recherche on jarte
     if (do_trim .and. dot_product(dir,n) > -0.05 ) then
        if (bavard) then
           print*,'orientation noeud(cd)-arete(an) non compatible avec dir'
           print *,' on trim la face'
           print*,dir
           print*,n
        endif

        new_node_HE_Hdl_proximity = -99
        n                        = 0.d0
        return
     endif
     
     call comp_rep(t,n,s)

     point = HE_Hdl%vertex(:,min_vert) 
     gap   = dot_product(vec,n)

     ! pour calculer les weight, on prend comme face celle du HE qui porte le ppp
     ! comme on n'est pas sur d'être au bord ou pas on gere
     
     HE               = HE_Hdl%V2HE(min_vert)

     weight_out       = 0.d0
     if (HE%i /= 0) then
       f_out            = HE%f
       weight_out(HE%i) = 1.d0
     else
       HE_aux=HE_Hdl%b2opHE(HE%f)
       f_out = HE_aux%f
       do iff=1,3
         if (HE_Hdl%faces(iff,f_out) == min_vert) then 
           weight_out(iff) =1.d0
         else
           weight_out(iff) =0.d0
         endif
       enddo
     endif  

  !---------------------------------------------------------------------
  ! si contact avec un edge
  else if (new_node_HE_Hdl_proximity == 2) then

     if (bavard) print *,'on est sur une arrete'

     ! le contact sur le bord d'un patch n'est pas conserve
     if (is_boundary(HE_Hdl,min_HE)) then
        if (bavard) then
           print*,'contact sur un edge du bord du patch'
           print *,' --> on supprime le contact'
        endif

        new_node_HE_Hdl_proximity = -99
        n                        = 0.d0
        return
        
     endif

     ! la normale est definie comme la moyenne des faces adjacentes
     dist = length3(mean_normal)
     if (dist == 0.d0) then
        new_node_HE_Hdl_proximity = -99
        n                        = 0.d0
        return
     endif

     n = mean_normal / dist
        
     !fd si la normale trouvee n'est pas bonne par rapport 
     !fd a la dir de recherche on jarte
     if (do_trim .and. dot_product(dir,n) > -0.05 ) then
        if (bavard) then
           print*,'orientation noeud(cd)-arete(an) non compatible avec dir'
           print *,' on trim la face'
           print*,dir
           print*,n
        endif

        new_node_HE_Hdl_proximity = -99
        n                        = 0.d0
        return
     endif

     call comp_rep(t,n,s)

     ! on redefinit vec car on a peut etre pris le HE oppose plus haut
     HE_aux     = next_HE(min_HE,err_)
     if (err_ > 0) then
        call logmes('zob') 
        err= 1
        return
     endif  
     
     vd         = HE_Hdl%faces(min_HE%i,min_HE%f)    ! vertex de debut du edge
     vf         = HE_Hdl%faces(HE_aux%i,HE_aux%f)    ! vertex de fin du edge
     vec(:)     = cd_coor(:) - HE_Hdl%vertex(:,vd)
     edge(:)    = HE_Hdl%vertex(:,vf) - HE_Hdl%vertex(:,vd)
     
     orien(:)   = HE_Hdl%HEorien(:,min_HE%i,min_HE%f)
     proj       = dot_product(vec,orien)
     point(:)   = HE_Hdl%vertex(:,vd) + proj*orien(:)
     gap        = dot_product(vec,n)

     f_out      = min_HE%f
     weight_out = 0.d0
     ! longueur de l'arete pour adimensionner la projection
     ww         = proj / length3(edge)

     ! affectation des poids aux 2 extremites du edge
     weight_out(min_HE%i)             = 1.d0 - ww
     weight_out(modulo(min_HE%i,3)+1) = ww

  !---------------------------------------------------------------------
  ! si contact avec une face
  else if (new_node_HE_Hdl_proximity == 3) then

     if (bavard) then 
        print *,' on est dans la face numero ',min_f
     endif

     f_out      = min_f
     weight_out = min_weight   

     ! construction du point sur la face
     point = 0.d0
     do i = 1,3
        pf(:,i)  = HE_Hdl%vertex(:,HE_Hdl%faces(i,f_out))
        point(:) = point(:) + weight_out(i)*pf(:,i)
     enddo

     ! repere local associe au point de contact
     n      = HE_Hdl%normal(:,min_f) 
     call comp_rep(t,n,s)

     ! on redefinit vec car on a peut etre pris le HE oppose plus haut
     vec(:) = cd_coor(:) - point(:)
     gap    = dot_product(vec,n)

  endif

  if (bavard) print*,' fin info He_hdl_proximity ->'


end function new_node_HE_Hdl_proximity


!> one looks for the proximal point in a HE_Hdl for a given point
!> HE_Hdl   the surface stored in a half hedge handler
!> cd_coor  the given node
!> dir      a given orientation to exclude faces
!> gap      distance
!> ppp  
!> point    contact point
!> t n s    the local framework
!> f        face number
!> weight   reduced coordinates in the face
!> node_HE_Hdl_proximity= -99 pas contact, =1 noeud, =2 arete, =3 face
!>
integer function node_with_ppp_HE_Hdl_proximity(HE_Hdl,cd_coor,dir,ppp,gap,point,t,n,s,f_out,weight_out,bavard,err,good_nodes)
  implicit none
  type(T_HE_Hdl)   :: HE_Hdl
  real(kind=8)     :: cd_coor(3),halo,dir(3)
  real(kind=8)     :: gap,point(3),t(3),n(3),s(3)
  integer          :: ppp, f_out
  real(kind=8)     :: weight_out(3)
  logical          :: bavard
  integer,optional :: good_nodes(:)
  integer          :: err,err_

  ! ***
  integer        :: i,j,k,f,min_vert,f0,fj,ibad_k,iff
  real(kind=8)   :: min_dist,dist,norm,vec(3),vec_aux(3),orien(3),vv(3),&
                    apab,pf(3,3),sens,weight(3)
  type(T_HE)     :: min_HE,HE,HE_aux,HE_rust
  logical        :: itchatche,is_inside

  !***
  logical :: bord0,bord,go_inside,au_bord
  integer :: b,idx_b,cnt_b
  !***
  integer :: face0

                          !123456789012345678901234567890123456789012345678
  character(len=48):: IAM='DiscreteGeometry::node_with_ppp_HE_Hdl_proximity'

  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    err = 1
    return 
  endif

  itchatche = bavard

 ! print*,"-----------------------------------"
 ! print*,"on entre dans node_with_ppp_HE_Hdl_proximity"

  ! a voir 
  weight_out = 0.d0
  f_out=0
  !

  min_HE%i=0; min_HE%f=0
  min_dist = 1.d20

  vec(:) = cd_coor(:) - HE_Hdl%vertex(:,ppp)

  HE = HE_Hdl%V2HE(ppp)
  f0 = HE%f

  if (HE%i /= 0) then
    bord0 = .false.
    face0 = f0
  else
    bord0=.true.
    HE_aux=HE_Hdl%b2opHE(f0)
    face0 = HE_aux%f
  endif

  if (itchatche) then
     print* ,'<- debut info detection'
     print* ,'cd_coor ', cd_coor(:)
     if (HE%i /= 0 ) then 
       print* ,'vertex pp ',ppp,' face liee ',f0
     else
       print* ,'vertex pp ',ppp,' bord lie ',f0,' face liee ',face0
     endif
     print* ,'coor pp   ', HE_Hdl%vertex(:,ppp)
     print* ,'HE        ', He%i,He%f
  endif

  bord = .false.
  go_inside = .true.
  au_bord=.false.

  ! recherche de l'arete la plus proche
  ! il faut faire attention dans le parcours
  !   un HE part du vertex pp
  !   un bord arrive sur le vertex pp
  !
  ! new 19/11/2010 
  ! lorque un noeud est sur un bord on le declare au_bord
  ! ca va permettre de prendre la normale a une face comme normale
  !
  do   ! parcours des arretes autour de ppp
   
     ! si on est entre sur un bord
     if (HE%i == 0) then

       if (itchatche) print*,'on arrive sur un bord'

       HE = HE_Hdl%B2opHE(HE%f)
       bord = .true.
       au_bord=.true.
     endif

     orien(:) = HE_Hdl%HEorien(:,HE%i,HE%f)

     if (bord) then 
       if (HE_Hdl%faces(HE%i,HE%f) /= ppp) orien = - orien
       !if (.not. start) orien = - orien   ! quand c est un bord qui part de ppp
       HE = HE_Hdl%HE2opHE(HE%i,HE%f) ! on remet les choses a leur place
     endif

     ! coordonnee reduite sur l'arrete
     apab=dot_product(vec,orien)
     vv(:) = vec(:) - apab*orien(:)

     dist= length3(vv)

     if (itchatche) then
       print*,'dir ',orien
       print*,'dist a dir',dist
       print*,'apab sur dir',apab
     endif

     ! si apab < 0 on n'est pas sur l'arete 
     ! comme on part du ppp on ne cherche pas si ca sort a l'autre bout             

     !fd oups: en plus comme orien est unitaire apab n'est pas une coordonnee reduite ...

     if (apab .ge. 0.d0 .and. dist < min_dist) then
        min_dist = dist
        min_HE = HE
     endif

     if (.not. bord) then
       ! on arrive par un HE 

       HE_aux=previous_HE(HE,err_)
       if (err_ > 0) then
         !call logmes() 
         err= 1
         return
       endif  

       ! if (itchatche) print*,'previous HE',HE_aux%i,HE_aux%f,HE_Hdl%faces(HE_aux%i,HE_aux%f)
  
       HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 
      
       if (itchatche) print*,'He suivant ',HE%i,HE%f

       go_inside = .false.

     else
       ! on arrive par un bord

       if (go_inside) then
         ! on rentre dans les faces
         ! car on commence ou on vient d'un autre bord

         if (itchatche) then
           print*,'on entre dans la matiere'
           print*,'par ',HE%i,HE%f
         endif

         HE_aux = HE_Hdl%B2opHE(HE%f)

         HE_aux=previous_HE(HE_aux,err_)
         if (err_ > 0) then
           !call logmes() 
           err= 1
           return
         endif  

         HE = HE_Hdl%HE2opHE(HE_aux%i,HE_aux%f) 

         if (itchatche) print*,'He suivant ',HE%i,HE%f

         go_inside = .false.

       else
        ! on va chercher le bord suivant dans le contour
        ! car on vient de la matiere 

         if (itchatche) then
           print*,'on sort de la matiere'
           print*,'on arrive du bord',HE%f
         endif 

         cnt_b = HE_Hdl%b2cnt(HE%f)
         do idx_b=1,size(HE_Hdl%cnts(cnt_b)%G_i)
           if (HE_Hdl%cnts(cnt_b)%G_i(idx_b) == HE%f) exit
         enddo
         if (idx_b > size(HE_Hdl%cnts(cnt_b)%G_i)) then
           call logmes(IAM//' Humm boundary not in contour',.true.)
           err = 1
           return 
         endif
         b = HE_Hdl%cnts(cnt_b)%G_i(modulo(idx_b,size(HE_Hdl%cnts(cnt_b)%G_i))+1)

         !print*,'on passe au bord suivant',b
      
         HE%i=0; HE%f=b
         go_inside = .true.

         if (itchatche) print*,'bord suivant ',HE%i,HE%f

       endif
     endif

     bord = .false.

     if (((He%i==0 .and. bord0) .or. (He%i /=0 .and. .not. bord0)) .and. &
         (HE%f == f0)) then
        if (itchatche) print*,'on a fait le tour'
       exit ! on a fait le tour
     endif


     !if (itchatche) 
     !  print*,'on continue' 
     !  if (HE%i /= 0) then
     !    print*,'opposite HE is a face ',HE%i,HE%f,HE_Hdl%faces(HE%i,HE%f)
     !  else
     !    print*,'opposite HE is a bord',HE%i,HE%f
     !  endif 
     !endif
  enddo

  if (min_HE%f == 0) then  ! le test min_HE%i = 0 est nul a cause des bords

     !TODO prendre une normale au vertex an comme moyenne des faces adjacentes  


     if (itchatche) print* ,'on est sur une pointe'

     ! la surface forme une pointe
     ! on est sur le vertex
     ! on prend comme normale vec norme

     ! est on dedans (-1) ou dehors (1)

     if (au_bord) then

      ! on prend comme normale celle du HE qui porte le ppp

      !TODO calculer une normale au point comme moyenne 
      !     des normales des faces adjacentes pour ejecter ce test 

       n = HE_Hdl%normal(:,face0)
       gap=dot_product(vec,n)
     else
       sens=-sign(1.d0,dot_product(dir,vec))

       dist = sens*length3(vec)                  

       gap=dist
       if (dist .ne. 0.d0) then
          n = vec/dist 
       else
          n = -dir/length3(dir)
       endif

     endif

     call comp_rep(t,n,s)

     point = HE_Hdl%vertex(:,ppp) 

     f_out = face0 ! on prend comme face celle du HE qui porte le ppp
     do iff=1,3
       if (HE_Hdl%faces(iff,f_out) == ppp) then 
         weight_out(iff) =1.d0
       else
         weight_out(iff) =0.d0
       endif
     enddo

     node_with_ppp_HE_Hdl_proximity=1

  else

     if (itchatche) then 
       if (min_HE%i /= 0) then
         HE_aux = next_HE(min_HE,err_)
         if (err_ > 0) then
           !call logmes() 
           err= 1
           return
         endif  

         print*,' arete la plus proche',  &
         HE_Hdl%faces(min_HE%i,min_HE%f), &
         HE_Hdl%faces(HE_aux%i,HE_aux%f)
       
         HE_aux = HE_Hdl%HE2opHE(min_HE%i,min_HE%f)
         print*,' portee par faces ',min_HE%f,HE_aux%f
       else
         ! a faire pour les bords
       endif
     endif 

     ! on a trouve une arrete 
     ! on regarde les faces de chaques cotes

     min_dist = 1.d20

     HE = min_HE

     is_inside = .FALSE.

     min_HE%i=0;min_HE%f=0
     HE_aux = HE

     au_bord=.false.

     !print*,'nouveau test'
     !print*,HE%i,HE%f
     do j=1,2
        !print*,'j: ',j
        if (j==2) then 
          if (HE_aux%i == 0) then
            HE_aux = HE_Hdl%B2opHE(HE%f)
          else
            HE_aux = HE_Hdl%HE2opHE(HE%i,HE%f)
            !print*,HE_aux%i,HE_aux%f
          endif
          if (HE_aux%i == 0) then
            au_bord=.true.
            !print*,j,' est un bord' 
            cycle  ! on est sur bord y a qu une face
          endif 
        else
          if (HE_aux%i == 0) then
            au_bord=.true.
            !print*,j,' est un bord'
            cycle  ! on est sur bord y a qu une face
          endif
        endif

        fj = HE_aux%f

        n = HE_Hdl%normal(:,fj)

        if (itchatche) then 
          print *,' face ',fj
          print *,' normale face ',n
          print *,' normale au point',dir
        endif

        ! si l'angle entre les normales sort de 100-260 deg on ejecte

        if (dot_product(dir,n) > -0.05 ) then
           if (itchatche) print *,' on trim la face'
           cycle
        endif

        do k=1,3
           pf(:,k) = HE_Hdl%vertex(:,HE_Hdl%faces(k,fj))
        enddo

        if (itchatche) then 
          print*,'sommets de la face'
          write(*,'(3(1x,D12.5))') pf
        endif

        is_inside = node_triangle_normal_projection(pf,cd_coor,-n,dist,weight,ibad_k,itchatche)

        !print*,'on sort par', ibad_k

        !fd je vire la rustine
        ibad_k = 0

        if (is_inside) then
          ! ouf on est dedans
          if (dist < min_dist) then
            min_dist=dist
            min_HE=HE_aux

            ! construction du point sur la face
            point(:) = 0.d0
            do k=1,3
              point(:) = point(:) + weight(k)*pf(:,k)
            enddo

            ! on sort de la boucle
            exit 
          endif
        else
          ! rustine a la fred dub
          ! on teste la face par ou ca sort au cas ou 
          ! aberration geometrique 
          ! on est sortie par le cote k

          if (ibad_k /= 0 ) then

            if (itchatche) print*,'rustine'

            HE_rust = HE_Hdl%HE2opHE(ibad_k,fj)

            if (HE_rust%i /= 0) then
              fj = HE_rust%f
              n = HE_Hdl%normal(:,fj)

              do k=1,3
                pf(:,k) = HE_Hdl%vertex(:,HE_Hdl%faces(k,fj))
              enddo

              is_inside = node_triangle_normal_projection(pf,cd_coor,-n,dist,weight,ibad_k,itchatche)

              if (is_inside) then
                ! on est dedans
                if (dist < min_dist) then
                  ! construction du point sur la face
                  point(:) = 0.d0
                  do k=1,3
                    point(:) = point(:) + weight(k)*pf(:,k)
                  enddo

                  min_dist=dist
                  min_HE=HE_rust

                  if (itchatche) then
                     print*,'rustine'
                     print *,HE_rust%i,HE_rust%f
                  endif

                  ! on sort de la boucle !?  -
                  !exit 

                endif
              endif
            endif
          endif
        endif
     enddo

     bord=.false.

     if (min_HE%f == 0) then
        ! on est sur l'arete  

        !if (itchatche) print *,'on est sur l arrete'


        ! HE contient le HE de depart, on s'en sert

        ! si on est entre sur un bord
        if (HE%i == 0) then
          HE = HE_Hdl%B2opHE(HE%f)
          bord = .true.
        endif

        orien(:) = HE_Hdl%HEorien(:,HE%i,HE%f)
 
        if (bord) then 
          if (HE_Hdl%faces(HE%i,HE%f) /= ppp) orien = - orien
          ! orien = - orien                ! c'est forcement un bord 
          ! HE = HE_Hdl%HE2opHE(HE%i,HE%f) ! on ne remet pas les choses a leur place car on en a besoin
        endif

        apab = dot_product(vec,orien)
        vv = vec - apab*orien
        norm = length3(vv)

        !fd 19/11/2010 pour eviter le pb des normales un peu bizarre sur un bord.
        !
        if (au_bord) then
          fj = HE%f
          n = HE_Hdl%normal(:,fj)
        else
          if (norm .ne. 0.d0) then           
             n = vv/norm
             ! on fait attention que la normale soit sortante
             n=sign(1.d0,dot_product(HE_Hdl%normal(:,HE%f),n))*n
          else
            fj = HE%f
            n = HE_Hdl%normal(:,fj)
          endif
        endif
        !fd si la normale trouvee n'est pas bonne par rapport 
        !fd a la dir de recherche on jarte
        if (dot_product(dir,n) > -0.05 ) then
           if (itchatche) then
             print*,'orientation noeud(cd)-arete(an) non compatible avec dir'
             print *,' on trim la face'
             print*,dir
             print*,n
           endif

           node_with_ppp_HE_Hdl_proximity = -99
           n=0.d0
           return
        endif

        !fd point(:)=HE_Hdl%vertex(:,HE_Hdl%faces(HE%i,HE%f)) + apab*orien(:)
        point(:)=HE_Hdl%vertex(:,ppp) + apab*orien(:)

        dist = dot_product(vec,n)
        gap = dist

        f_out = HE%f
        ! weight
        weight_out =0.d0
        !fd si bord dans le bon sens
        if (HE_Hdl%faces(He%i,He%f) == ppp) then 
             weight_out(He%i) = apab
             weight_out(modulo(He%i,3)+1) = 1.d0 - apab
        else
             weight_out(He%i) = 1.d0 - apab
             weight_out(modulo(He%i+1,3)+1) = apab
        endif

        node_with_ppp_HE_Hdl_proximity=2

     else
        ! on est dans une face         

        if (itchatche) then 
          print *,' on est dans la face'
          print *,'face ',min_HE%f
        endif

        gap=min_dist

        fj = min_HE%f

        n = HE_Hdl%normal(:,fj) 

        point = HE_Hdl%vertex(:,ppp) + vec - min_dist*n

        !print *,'xxx'
        !print *,'n   ',n

        !print*,fj, HE_Hdl%faces(:,fj)

        !print*,HE_Hdl%vertex(:,HE_Hdl%faces(1,fj))
        !print*,HE_Hdl%vertex(:,HE_Hdl%faces(2,fj))
        !print*,HE_Hdl%vertex(:,HE_Hdl%faces(3,fj))
        !print *,'xxx'

        f_out = fj
        weight_out = weight

        node_with_ppp_HE_Hdl_proximity=3

     endif

     !if (itchatche) then
     !   print *,'ptc ',point(:)
     !   print *,'n   ',n
     !endif

     call comp_rep(t,n,s)

  endif

  !if (itchatche) print*,' fin info detection ->'

end function node_with_ppp_HE_Hdl_proximity

!> one looks roughly for the (unique) proximal point in a HE_Hdl for a given point
!> HE_Hdl      the surface stored in a half hedge handler
!> cd_coor     the given node
!> halo        a given distance to reduce ppp search
!> dir         a given orientation to exclude faces
!> min_vert    ppp
!> min_dist    gap
!> bavard      log level
!> err         err level 0:ok 1:problem
!> good_nodes  to reduce concerned nodes   
!> node_HE_Hdl_rough_proximity= 0 pas possible, =1 ok
!>
integer function node_HE_Hdl_rough_proximity(HE_Hdl,cd_coor,halo,dir,min_vert,min_dist, &
                                             bavard,err,trim_angle,good_nodes,with_extend)
  implicit none
  type(T_HE_Hdl)        :: HE_Hdl
  real(kind=8)          :: cd_coor(3),halo,dir(3),min_dist
  integer               :: min_vert
  logical               :: bavard
  integer               :: err
  real(kind=8),optional :: trim_angle
  integer,optional      :: good_nodes(:)
  logical,optional      :: with_extend 

  ! ***
  integer        :: c,ic,b,v_d,i
  real(kind=8)   :: dist2,vec(3),n(3),t(3),s(3),halo2
  type(T_HE)     :: HE_aux
  logical        :: is_good

  real(kind=8),pointer :: normal(:,:)

  ! 87.13 deg
  real(kind=8) :: tol_trim = 0.05

  ! fd
  logical                   :: au_bord,extend,is_inside
  integer                   :: nb_HE,i_HE,iout,k
  type(T_HE),DIMENSION(100) :: liste_HE
  real(kind=8)              :: pf(3,3),fweight(3),fdist

  
                          !123456789012345678901234567890123456789012345
  character(len=45):: IAM='DiscreteGeometry::node_HE_Hdl_rough_proximity'

  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call logmes('Error in '//IAM//' Uninitialized He_Hdl',.true.)
    ERR = 1
    return 
  endif

  if (bavard) then 
    print*,"-----------------------------------"
    print*,"on entre dans node_HE_Hdl_rough_proximity"
  endif

  if (present(trim_angle)) then
      tol_trim=cos(trim_angle*PI_G/180.d0)
  endif   

  if (present(with_extend)) then
     extend=with_extend
  else
     extend=.FALSE.
  endif
  
  normal => null()
  
  !
  halo2=halo*halo 

  is_good=.true.  
  min_vert = 0

  ! on teste que le ppp soit dans le halo
  min_dist=1.d20

  call get_nodal_normals_HE_Hdl(HE_Hdl,normal,err)

  if (err == 1) call faterr(IAM,'wtf')

  ! print*,'-------------------'
  ! print*,'vertex cd'
  ! print*,'coor ',cd_coor
  ! print*,'normale ',dir

  
  DO i=1,HE_Hdl%nb_vertex

    ! au cas ou on ne cherche pas sur tout
    if (present(good_nodes)) then
      if (good_nodes(i) == 0) cycle
    end if

    vec(:) = cd_coor(:) - HE_Hdl%vertex(:,i)
    dist2 = dot_product(vec,vec)  ! pas length() pour economiser la sqrt

    if (bavard) then
      print*,'vertex an: ',i
      print*,'coor', HE_Hdl%vertex(:,i)
      print*,'dist**2 ',dist2,' halo**2 ',halo2
    endif
    if (dist2 > halo2) cycle
    if (dot_product(dir,-normal(:,i)) < tol_trim) cycle    
    if (dist2 < min_dist) then

      ! print*,'vertex an: ',i
      ! print*,'coor', HE_Hdl%vertex(:,i)
      ! print*,'dist**2 ',dist,' halo**2 ',halo2
      ! 
       
      ! recupere tous les HE qui partent du vertex
      call get_all_HE_around_vertex(i,HE_Hdl,extend,nb_HE,liste_HE)

      ! print*,nb_HE,size(HE_Hdl%faces,dim=2)
      ! do i_HE=1,nb_HE
      !   HE_aux=liste_HE(i_HE)
      !   print*,i_HE,HE_aux%i,HE_aux%f
      ! enddo
     
      do i_HE=1,nb_HE
        HE_aux=liste_HE(i_HE)
        if (HE_aux%i == 0)  cycle 
        ! print*,i_HE,HE_aux%i,HE_aux%f
        do k=1,3
          pf(:,k) = HE_Hdl%vertex(:,HE_Hdl%faces(k,HE_aux%f))
        enddo

        ! write(*,'(3(1x,e12.5))') pf
        
        is_inside = node_triangle_normal_projection(pf,cd_coor,-dir,fdist,fweight,iout,bavard)
        
        ! print*,is_inside,fdist,fweight
        ! print*,fweight(1)*pf(:,1)+fweight(2)*pf(:,2)+fweight(3)*pf(:,3)

        !fd  je rajoute un test sur fdist (distance négative !!) pour les cas trop interpenetres         
        if (is_inside .and. -fdist > -halo*0.25) then
          ! print*,'fdist= ',fdist,' tol ',-halo*0.25
           
          min_dist = dist2
          min_vert =i
          exit
        endif
      enddo 
    endif
  ENDDO

  deallocate(normal)
  
  ! on n'a pas trouve de point le plus proche on arrete
  !
  if (min_vert == 0) then
    node_HE_Hdl_rough_proximity=0
    return
  endif

  node_HE_Hdl_rough_proximity=1

  
  return

end function node_HE_Hdl_rough_proximity

!> one looks roughly for the proximal points in a HE_Hdl for a given point
!> using a pinball stategy
!> HE_Hdl      the surface stored in a half hedge handler
!> cd_coor     the given node
!> halo        a given distance to reduce ppp search - radius
!> vertices    O which nodes are seen by cd_coor
!> pinball_HE_Hdl_rough_proximity= 0 not possible, =1 ok
!
!fd une bete recherche dans des halos
integer function pinball_HE_Hdl_rough_proximity(HE_Hdl,radius,cd_coor,cd_radius,halo,vertices,bavard,err,good_nodes)
  implicit none
  type(T_HE_Hdl)            :: HE_Hdl
  real(kind=8),dimension(:) :: radius
  real(kind=8)              :: cd_coor(3),cd_radius,halo
  integer                   :: vertices(:)
  logical                   :: bavard
  integer                   :: err
  integer,optional          :: good_nodes(:)
  
  ! ***
  integer        :: vd
  real(kind=8)   :: dist,vec(3)

                          !123456789012345678901234567890123456789012345
  character(len=45):: IAM='DiscreteGeometry::pinball_HE_Hdl_rough_proximity'

  err = 0
  
  if (HE_Hdl%nb_vertex == 0 .or. HE_Hdl%nb_faces == 0) then
    call LOGMES(IAM//' Uninitialized He_Hdl',.true.)
    ERR= 1
    return
  endif

  ! print*,"-----------------------------------"
  ! print*,"on entre dans pinball_HE_Hdl_rough_proximity"

  !
  vertices = 0
  pinball_HE_Hdl_rough_proximity=0

  DO vd=1,HE_Hdl%nb_vertex

     ! au cas ou, on ne cherche pas sur tout
     if (present(good_nodes)) then
        if ( good_nodes(vd) == 0 ) cycle
     endif

    vec(:) = cd_coor(:) - HE_Hdl%vertex(:,vd)
    dist = length3(vec)

    if (dist < radius(vd)+cd_radius+halo) then
      vertices(vd)=1
      pinball_HE_Hdl_rough_proximity = pinball_HE_Hdl_rough_proximity+1
    endif

  enddo

end function pinball_HE_Hdl_rough_proximity


type(T_HE) function next_HE(HE,err)
  implicit none
  ! on sait face a 3 noeuds
  integer    :: m=3
  type(T_HE) :: HE
  integer    :: err
                          !12345678901234567890123456789
  character(len=29):: IAM='DiscreteGeometry::next_HE_Hdl'

  err = 0
  
  if (HE%i == 0) then
    call LOGMES(IAM//' unable to find a free edge',.true.)
    err= 1
    return
  endif    
  next_HE%f=HE%f
  next_HE%i=modulo(HE%i,m)+1
end function

type(T_HE) function previous_HE(HE,err)
  implicit none
  ! on sait face de 3 noeuds
  integer    :: m=3
  type(T_HE) :: HE
  integer    :: err
                          !123456789012345678901234567890123
  character(len=33):: IAM='DiscreteGeometry::previous_HE_Hdl'

  err = 0
  
  if (HE%i == 0) then
    call LOGMES('Error in '//IAM//' unable on free edge',.true.)
    err = 1
    return
  endif    
    

  previous_HE%f=HE%f
  previous_HE%i=modulo(HE%i-2,m)+1
end function


! check if a HE is a boundary
logical function is_boundary(HE_Hdl,HE)
  implicit none
  type(T_HE)        :: HE,HE_aux
  type(T_HE_Hdl)    :: HE_Hdl
                           !12345678901234567890123456789
  character(len=29) :: IAM='DiscreteGeometry::is_boundary'
  
  is_boundary = .false.
  
  if (HE%i==0) then
     is_boundary = .true.
  else
     HE_aux = HE_Hdl%HE2opHE(HE%i,HE%f)
     if (HE_aux%i==0) is_boundary = .true.
  endif
  
end function is_boundary

! check if a vertex is on a boundary
logical function is_vertex_on_boundary(HE_Hdl,vd)
  implicit none
  integer           :: vd,i0,f0
  type(T_HE)        :: HE
  type(T_HE_Hdl)    :: HE_Hdl
  integer           :: err_
  
                           !123456789012345678901234567890123456789
  character(len=39) :: IAM='DiscreteGeometry::is_vertex_on_boundary'
  
  is_vertex_on_boundary = .false.
  
  HE = HE_Hdl%V2HE(vd)
  i0 = HE%i
  f0 = HE%f
  do
    if (is_boundary(HE_Hdl,HE)) then
      ! si le HE est un bord, alors le vertex aussi
      is_vertex_on_boundary = .true.
      exit
    else
      HE = previous_HE(HE,err_)
      HE = HE_Hdl%HE2opHE(HE%i,HE%f)
      ! on a fait le tour du vertex, on sort
      if (HE%i == i0 .and. HE%f == f0) exit
    endif
  enddo
  
end function is_vertex_on_boundary

! get all HE starting from a vertex (avec les bords !!)
subroutine get_all_HE_around_vertex(vd,HE_Hdl,extend,nb_HE,liste_HE)
  implicit none
  integer                             :: vd,i0,f0,nb_HE
  logical                             :: extend
  type(T_HE_Hdl)                      :: HE_Hdl
  type(T_HE)                          :: HE,HE_aux
  integer, parameter                  :: nb_max = 100
  type(T_HE),DIMENSION(nb_max)        :: liste_HE
  integer                             :: err_

                                !1234567890123456789012345678901234567890
  character(len=40)      :: IAM='DiscreteGeometry::get_all_HE_from_vertex'


  ! initialisation et stockage du HE de depart (pas un bord)
  nb_HE   = 0
  HE      = HE_Hdl%V2HE(vd)
  i0      = HE%i
  f0      = HE%f

  if (i0 == 0) call faterr(IAM,"it should not start with a bord")
  
  ! parcours les HE autour de vd (clockwise op+next)
  do    
     ! on stocke le HE
     nb_HE = nb_HE + 1
     if ( nb_HE > nb_max ) call faterr(IAM,"il faut augmenter le nombre de HE around")
     liste_HE(nb_HE) = HE

     ! on commence a aller chercher le suivant (etape op)
     if (HE%i == 0) then
        ! si bord on prend le HE oppose
        HE = HE_Hdl%B2opHE(HE%f)
     else
        ! si on etend aux voisins, on prend le HE qui ferme la face
        if ( extend ) then
           nb_HE = nb_HE + 1
           if ( nb_HE > nb_max ) call faterr(IAM,"il faut augmenter le nombre de HE around")
           liste_HE(nb_HE) = next_HE(HE,err_)
        endif
       
        ! si on arrive par un HE normal
        HE = HE_Hdl%HE2opHE(HE%i,HE%f)
     endif

     ! on poursuit (etape next)
     if (HE%i == 0) then
        ! si on se retrouve sur un bord, on rembobine
        HE      = HE_Hdl%B2opHE(HE%f)
        do
           HE = previous_HE(HE,err_)
           HE = HE_Hdl%HE2opHE(HE%i,HE%f)
           ! si on retrouve a nouveau un bord, on a fini le rembobinage
           if (HE%i == 0) exit
        end do
     else
        ! si on n est pas sur un bord, on prend le suivant normalement
        HE = next_HE(HE,err_)
     endif
     
     ! si on retrouve le HE de depart, on a fait le tour du vertex et on sort
     if ((HE%f == f0) .and. (HE%i == i0)) exit
     
  enddo

end subroutine get_all_HE_around_vertex

! get all the antagonist vertices included inside the halo
!     and the proximal point
subroutine get_proximal_point_HE_Hdl(HE_Hdl,cd_coor,halo,min_vert,min_dist,good_nodes)
  implicit none
  type(T_HE_Hdl)     :: HE_Hdl
  real(kind=8)       :: cd_coor(3),halo,min_dist
  integer            :: min_vert
  integer,optional   :: good_nodes(:)
  ! ***
  integer            :: iv
  real(kind=8)       :: dist,vec(3),halo2

                            !123456789012345678901234567890123456
  character(len=36)  :: IAM='DiscreteGeometry::get_proximal_point'

  
  ! recherche des points dans le halo
  halo2    = halo*halo
  min_vert = 0
  min_dist = 1.d20
  
  do iv = 1,HE_Hdl%nb_vertex

     ! au cas ou, on ne cherche pas sur tout
     if (present(good_nodes)) then
        if ( good_nodes(iv) == 0 ) cycle
     endif

     vec(:) = cd_coor(:) - HE_Hdl%vertex(:,iv)
     dist   = dot_product(vec,vec)  ! pas length() pour economiser la sqrt

     if (dist > halo2) cycle
     if (dist < min_dist) then
        min_dist = dist
        min_vert = iv
     endif
        
  enddo

  if (min_vert > 0) then
     vec(:)   = cd_coor(:) - HE_Hdl%vertex(:,min_vert)
     min_dist = length3(vec)
  endif
  
end subroutine get_proximal_point_HE_Hdl

! knowing a ppp verify if it is still a ppp or if a surounding vertex is not better 
subroutine update_proximal_point_HE_Hdl(HE_Hdl,cd_coor,dir,halo2,min_vert,min_dist2,extend,tol_trim,bavard,good_nodes)
  implicit none
  type(T_HE_Hdl)       :: HE_Hdl
  real(kind=8)         :: cd_coor(3),dir(3),halo2,min_dist2
  integer              :: min_vert
  logical              :: extend
  real(kind=8)         :: tol_trim
  logical              :: bavard
  integer,optional     :: good_nodes(:)
  
  ! ***
  integer              :: k,vv,err
  real(kind=8)         :: vec(3),dist2
  real(kind=8),pointer :: normal(:,:)
  
  type(T_HE),DIMENSION(100) :: liste_HE
  type(T_HE)                :: HE_aux
  integer                   :: nb_HE,i_HE
  
                            !123456789012345678901234567890123456789
  character(len=39)  :: IAM='DiscreteGeometry::update_proximal_point'


  call get_nodal_normals_HE_Hdl(HE_Hdl,normal,err)
  
  ! recupere tous les HE qui partent du vertex
  call get_all_HE_around_vertex(min_vert,HE_Hdl,extend,nb_HE,liste_HE)

  do i_HE=1,nb_HE
    HE_aux=liste_HE(i_HE)
    if (HE_aux%i == 0)  cycle 
    do k=1,3
      vv = HE_Hdl%faces(k,HE_aux%f )
      if (vv == min_vert) cycle 
      vec(:) = cd_coor(:) - HE_Hdl%vertex(:,vv)
      dist2 = dot_product(vec,vec)  ! pas length() pour economiser la sqrt

      if (bavard) then
        print*,'vertex an: ',vv
        print*,'coor', HE_Hdl%vertex(:,vv)
        print*,'dist**2 ',dist2,' halo**2 ',halo2
      endif
      if (dist2 > halo2) cycle
      if (dot_product(dir,-normal(:,vv)) < tol_trim) cycle    
      if (dist2 < min_dist2) then
         min_dist2 = dist2
         min_vert = vv
      endif   
    enddo
  enddo

  deallocate(normal)
  
end subroutine update_proximal_point_HE_Hdl


!!!!!!!!!!!!!!!!!!!! end of HE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> based on G. Van Den Bergen p.85
!> true if the projection is inside the triangle
!>
!> p vertices of triangle
!> s vertex to project
!> dir direction of projection
!> lambda distance between node and triangle along n
!> weight barycentric coordinates
!> bavard verbosity level

logical function node_triangle_projection(p,s,dir,lambda,weight,bavard)
   implicit none
   real(kind=8) :: p(3,3),s(3),dir(3),lambda,weight(3)
   logical :: bavard
   ! internal variables
   real(kind=8) :: d1(3),d2(3),n(3),delta,b(3),u(3),tol=1.d-08
   !
   node_triangle_projection = .false.
   lambda = 0.d0
   weight = 0.d0
   !
   d1(:) = p(:,2) - p(:,1)
   d2(:) = p(:,3) - p(:,1)
   n = cross_product(d1,d2)
   !
   delta = - dot_product(dir,n)
   if (delta == 0.d0) then
     if (bavard) print*,'projection vecteur parallel to the triangle'
     return
   endif
   delta = 1.d0/delta
 
   b(:) = s(:) - p(:,1)
   lambda = dot_product(b,n)*delta
   u = cross_product(b,dir)
   weight(2) = dot_product(d2,u)*delta       
   weight(3) = - dot_product(d1,u)*delta
   weight(1) = 1.d0 - weight(2) - weight(3)

   if (weight(2) < -tol .or. &
       weight(3) < -tol .or. &
       weight(1) < -tol ) then

     if (bavard) then
       print*,'out of triangle'
       print*,'s ',s(:)
       print*,'dir',dir(:)
       print*,'p1',p(:,1)
       print*,'p2',p(:,2)
       print*,'p3',p(:,3)
       print*,'w',weight
       print*,'h',lambda
       print*,'delta',delta
       print*,'d1',d1
       print*,'d2',d2
       print*,'u',u
     endif 
     lambda = 0.d0
     weight = 0.d0
     return
   endif

   node_triangle_projection = .true.

end function


!> based on G. Van Den Bergen p.85
!>
!> special case given dir and face normal are the same
!>
!> true if the projection is inside the triangle
!>
!> p vertices of triangle
!> s vertex to project
!> dir direction of projection
!> lambda distance between node and triangle along n
!> weight barycentric coordinates
!> if outside gives the side
!> bavard verbosity level

logical function node_triangle_normal_projection(p,s,dir,lambda,weight,iout,bavard)
   implicit none
   real(kind=8) :: p(3,3),s(3),dir(3),lambda,weight(3)
   logical :: bavard
   integer :: iout
   !***
   ! internal variables
   real(kind=8) :: d1(3),d2(3),n(3),delta,b(3),u(3),tol=1.d-08
   !
   node_triangle_normal_projection = .false.
   !
   lambda = 0.d0
   weight = 0.d0
   iout=0
   !
   d1(:) = p(:,2) - p(:,1)
   d2(:) = p(:,3) - p(:,1)
   n = cross_product(d1,d2)
   !
   delta = - dot_product(dir,n)
   
   if (delta == 0.d0) then
     if (bavard) print*,'projection vecteur parallel to the triangle'
     return
   endif

   delta = 1.d0/delta
 
   b(:) = s(:) - p(:,1)
   lambda = dot_product(b,n)*delta
   
   u = cross_product(b,dir)
   weight(2) = dot_product(d2,u)*delta       
   weight(3) = - dot_product(d1,u)*delta
   weight(1) = 1.d0 - weight(2) - weight(3)

   if (weight(2) < -tol .or. &
       weight(3) < -tol .or. &
       weight(1) < -tol ) then

     if (bavard) then
       print*,'out of triangle'
       print*,'s ',s(:)
       print*,'dir',dir(:)
       print*,'p1',p(:,1)
       print*,'p2',p(:,2)
       print*,'p3',p(:,3)
       print*,'w',weight
       print*,'h',lambda
       print*,'delta',delta
       print*,'d1',d1
       print*,'d2',d2
       print*,'u',u
       print*,'==============='
     endif 

     if (weight(1) < -tol ) iout=1
     if (weight(2) < -tol) iout=2
     if (weight(3) < -tol) iout=3

     lambda = 0.d0
     weight = 0.d0

     return
   endif

   node_triangle_normal_projection = .true.

end function


!> compute the convex hull of a set of coplanar vertices (3D)
!> vl (I): total vertex list 
!> ivl (O): list of indexes of the vertices that belong to the convex hull (1:n, n-1 <=size(vl), ivl(n)=ivl(1))    
!> vertexes are stored anti-clockwise
!> ref_size (I): ref distance for the tests 
subroutine convex_hull(vl,ivl,ref_size,err)
  implicit none
  real(kind=8) :: vl(:,:),ref_size
  integer      :: ivl(:)
  integer      :: err
  !
  integer :: nb_vertex,i_max,i,k,inc,ii,j,foireux
  real(kind=8) :: theta,dir(2),dt,max_theta,zero,norm,rot(2,2)
  integer,allocatable,dimension(:) :: vec_i4,is_ok
  !real(kind=8),allocatable,dimension(:) :: tmp1,tmp2
                          !12345678901234567890123457890
  character(len=30):: IAM='DiscreteGeometry::convex_hull'
  character(len=90):: cout
  character(len=10):: ctmp
  integer          :: idx,isz

  err = 0
  
  ivl = 0
  nb_vertex = size(vl,dim=2)

  !print*,'->qh ',nb_vertex
  !do i=1,nb_vertex
  !print*,vl(:,i)
  !enddo

  IF (nb_vertex > 2 ) THEN

    dt = -0.01d0*pi_g ! tol angle
    rot(1,1) = cos(dt) ; rot(1,2)=-sin(dt)  
    rot(2,1) = sin(dt) ; rot(2,2)= cos(dt)

    zero = 1.d-14

    allocate(vec_i4(size(vl,dim=1)),is_ok(nb_vertex)) !,tmp1(nb_vertex),tmp2(nb_vertex))

    ! pour savoir les points deja sur l'enveloppe convexe
    ! on ne met pas le premier car on veut pouvoir fermer le contour

    is_ok  = 0  

    ! On cherche tout d'abord l'index du sommet d'abcisse la plus grande
    ! on cherche le maxloc (en x et y)
    vec_i4 = maxloc(vl,dim=2)

    !print*,'maxloc ',vec_i4

    ! on garde celui avec le plus grand x 
    inc=1
    ivl(inc)=vec_i4(1)

    !print*,'on garde'
    !print*,inc,ivl(inc)

    ! On cherche ensuite le sommet qui forme un angle avec la verticale
    ! qui soit le plus petit (c'est a dire on maximise le produit scalaire)
    ! on incline un peu a cause des erreurs d'arrondie

    dir = (/ cos((pi_g*0.5)) , sin((pi_g*0.5)) /)

    theta=0.d0

    ! on construit le contour 
    DO k=1,nb_vertex   

       !print*,'pour ',ivl(inc),' ...'
       !print*,'dir ',dir

       max_theta=-1.D+20
       i_max=0

       dir = matmul(rot,dir)

       DO i=1,nb_vertex

         ! si deja traite on passe
         IF (is_ok(i) == 1) CYCLE

         ! afin d eviter les points de meme coordonnee
         norm=(vl(2,i)-vl(2,ivl(inc)))**2+(vl(1,i)-vl(1,ivl(inc)))**2

         IF (norm > ref_size*zero) THEN

           ii = max(1,inc)

           if (i == ivl(ii)) cycle

           theta=(dir(1)*(vl(1,i)-vl(1,ivl(ii)))) +  &
                 (dir(2)*(vl(2,i)-vl(2,ivl(ii))))

           norm=sqrt((vl(2,i)-vl(2,ivl(ii)))**2+(vl(1,i)-vl(1,ivl(ii)))**2)
           theta = theta/norm

           !print*,'recherche du suivant'
           !print*,i,theta,ivl(ii)
           !print*,vl(:,ivl(ii))
           !print*,vl(:,ivl(inc))
           !print*,vl(:,ivl(ii)) + dir(:)
           !print*,vl(:,i)

           IF (theta>max_theta) THEN
             max_theta =theta
             i_max=i
           ENDIF
        else
           
           !print*,k,inc,i
           !print*,ivl(inc),vl(:,ivl(inc))
           !print*,i,vl(:,i)
           !print*,i,' et ',ivl(inc),' indentiques'
           !print*,'tolerance ',ref_size*zero

           ! il est au meme endroit qu'un autre donc on l'ejecte sauf si c'est le premier
           if (i /= ivl(1)) is_ok(i) = 1
         ENDIF
       ENDDO  

       if (i_max == 0) then
         ! cas ou le qh se reduit a un point ...
         if (inc == 1) then  
           foireux=1
           DO j=1,nb_vertex
             if (j==ivl(1)) cycle 
             if (is_ok(j) /= 1) then
               foireux=0
               exit
             endif  
           ENDDO
           if (foireux == 1) then
             ivl(2) = ivl(1)
             exit            
           endif
         endif
        
         ! sinon ca merde
         cout=''
         write(cout,'("dim ",I0," nb vertices ",I0)') size(vl,dim=1), size(vl,dim=2)
         call LOGMES(cout,.true.)
         !
         call LOGMES('coordinates ',.true.)
         do j=1, size(vl,dim=2)
           cout=''         
           write(cout,'(3(1x,D15.7))') vl(:,j)
           call LOGMES(cout,.true.)
         enddo
         !
         call LOGMES('kept vertices',.true.)
         cout=''
         idx=0
         do j=1, size(vl,dim=2)
            ctmp=''
            write(ctmp,'(I0)') ivl(j)
            isz = len( trim( ctmp ) )
            cout(idx+1:idx+isz) = ctmp(1:isz)
            idx = idx + isz +1
         enddo
         call LOGMES(cout,.true.)
         !         
         call LOGMES('managed vertices',.true.)
         cout=''
         idx=0
         do j=1, size(vl,dim=2)
            ctmp=''
            write(ctmp,'(I0)') is_ok(j)
            isz = len( trim( ctmp ) )
            cout(idx+1:idx+isz) = ctmp(1:isz)
            idx = idx + isz +1
         enddo
         call LOGMES(cout,.true.)
         !
         cout=''         
         write(cout,'("ref_size ",I0," zero ",I0)')  ref_size, zero
         call LOGMES(cout,.true.)
         !         
         call LOGMES(IAM//' unable to build convex-hull',.true.)
         err = 1
         return
       endif

       !print*,'... le suivant est ',i_max

       inc=inc+1

       is_ok(i_max) = 1

       ivl(inc)=i_max
       
       !print*,'on garde'
       !print*,inc,ivl(inc)

       ! Test d'arret: on a ferme le contour
       IF (ivl(inc) == ivl(1)) exit

       ! construction nouvelle direction 
       ! horizontal
       if (abs(vl(2,ivl(inc)) - vl(2,ivl(inc-1))) < zero) then
         if (vl(1,ivl(inc)) - vl(1,ivl(inc-1)) > 0.d0) then
           theta = 0.
         else
           theta = pi_g
         endif
         dir = (/ cos(theta) , sin(theta) /)
       else 
         !vertical
         if (abs(vl(1,ivl(inc)) - vl(1,ivl(inc-1))) < zero) then
           if (vl(2,ivl(inc)) - vl(2,ivl(inc-1)) > 0.d0) then
            theta = (pi_g*0.5)
           else
            theta = (1.5*pi_g)
           endif
           dir = (/ cos(theta) , sin(theta) /)
         else
           norm=(vl(1,ivl(inc)) - vl(1,ivl(inc-1)))**2+(vl(2,ivl(inc)) - vl(2,ivl(inc-1)))**2
           norm = 1.d0/sqrt(norm)
           dir = (/ (vl(1,ivl(inc))-vl(1,ivl(inc-1)))*norm , (vl(2,ivl(inc)) - vl(2,ivl(inc-1)))*norm /)
         endif
       endif

    ENDDO

    deallocate(vec_i4,is_ok) 
  ELSE

    ! Soit le nombre de vertex de PRan projetes est inferieur ou egal a 3 
    !fd c'est incomprehensible pourquoi en local ?
          
    DO i=1,nb_vertex
        
      ivl(i) = i

    ENDDO

    ivl(nb_vertex+1) = ivl(1) 

  endif

  !print*,'vertex du contour : '
  !do i=1,nb_vertex+1
  !  if (ivl(i) /= 0) print*,ivl(i),vl(:,ivl(i))
  !enddo

end subroutine

! me semble avoir des "trous"
!> -1 //, 2 on passe par le bout, 1 on passe par le centre, 0 on coupe en dehors
integer function segments_intersection(a,b,c,d,ref_size,p)
  use predicates
  implicit none
  real(kind=8),dimension(2) :: a,b,c,d,p
  real(kind=8) :: denom,num,ss,tt,ref_size,tol,zero

  segments_intersection = -99

  ! pas facile tol = ref_size*zero
  zero= f_exactinit()
  tol = 1.000001*zero

  ! Equation parametrique des segments du contour

  denom=a(1)*(d(2)-c(2)) + &
        b(1)*(c(2)-d(2)) + &
        d(1)*(b(2)-a(2)) + &
        c(1)*(a(2)-b(2))

  IF (dabs(denom) < tol) THEN
    segments_intersection = -1
    return
  ELSE
    ! Parametres ss et tt

    num=a(1)*(d(2)-c(2)) + &
        c(1)*(a(2)-d(2)) + &
        d(1)*(c(2)-a(2))

    if (dabs(num) < tol .or. dabs(num - denom) < tol) segments_intersection=2 
          
    ss=num/denom

    num=-(a(1)*(c(2)-b(2)) +  &
          b(1)*(a(2)-c(2)) + &
          c(1)*(b(2)-a(2)))

    if (dabs(num) < tol .or. dabs(num - denom) < tol) segments_intersection=2  

    tt=num/denom

    ! les parametres ss et tt sont compris entre 0 et 1

    IF ((zero < ss  .AND. ss < (1.d0 - zero))  .AND. &
        (zero < tt  .AND. tt < (1.d0 - zero)))   THEN
      segments_intersection=1
    else if ( -zero > ss .OR. ss > (1.d0 + zero) .OR. &
              -zero > tt .OR. tt > (1.d0 + zero)) THEN
      segments_intersection=0
    endif

    p(:)=a(:) + ss* (b(:)-a(:))

    return
  endif
end function


!> \brief Compute the intersection of 2 coplanar polytopes (with clipper => wc)
subroutine polytopes_intersection_wc(vl1,ivl1,vl2,ivl2,points,nb_points,shrink1,shrink2,delta,area,bavard,err)
  implicit none
  !> first array of vertices of shape (2,1:m1) (stored anti-clockwise)
  real(kind=8), dimension(:,:), intent(in) :: vl1
  !> second array of vertices of shape (2,1:m2) (stored anti-clockwise)
  real(kind=8), dimension(:,:), intent(in) :: vl2
  !> list of valid vertices in first polygon (1:n,n-1 <=m1, ivl(1) == ivl(n))
  integer     , dimension(:)  , intent(in) :: ivl1
  !> list of valid vertices (1:n,n-1 <=m2, ivl(1) == ivl(n))
  integer     , dimension(:)  , intent(in) :: ivl2
  !> number of intersection points
  integer     , dimension(:)  , pointer :: nb_points
  !> the intersection (no double)
  real(kind=8), dimension(:,:), pointer :: points
  !> the shrink value to use in clipper on first polygon
  real(kind=8), intent(in) :: shrink1
  !> the shrink value to use in clipper on second polygon
  real(kind=8), intent(in) :: shrink2
  !> the delta value to use in clipper (use to simplify polygons)
  real(kind=8), intent(in) :: delta
  !> the area of the intersection computed by clipper
  real(kind=8), intent(out):: area
  !> log level of the function
  logical :: bavard
  !> return code : 0 if successful
  integer :: err
  !
  real(kind=8), dimension(:,:), pointer :: polyg1, polyg2, cpoints
  integer     , dimension(:)  , pointer :: csizes, sizes1, sizes2
  real(kind=8), dimension(:)  , pointer :: carea
  !
  integer :: nb_vertex1, nb_vertex2, i, ii, nb_pts, nb_pol
  character(len=120) :: cout
                                      !123456789012345678901234567890123456789012
  character(len=42), parameter :: IAM='DiscreteGeometry_polytopes_intersection_wc'

  err = 0

  nb_points => null()

  cpoints => null()
  csizes  => null()
  carea   => null()

  if( bavard ) call logmes('['//IAM//'] polyg1', .true.)
  nb_vertex1=0
  do i=1,size(ivl1) 
    if (ivl1(i) <= 0) cycle
    nb_vertex1 = nb_vertex1 + 1
  enddo
  ! on vire le dernier qui est le premier
  nb_vertex1=nb_vertex1-1
  if (bavard) then
    write(cout,'(A,A,A,1X,I0)') '[',IAM,'] nb vertex:',nb_vertex1
    call logmes(cout, .true.)
  end if
  
  allocate(polyg1(2,nb_vertex1))

  ii=0
  do i=1,size(ivl1) 
    if (ivl1(i) <= 0) cycle
    ii = ii + 1
    polyg1(:,ii) = vl1(:,ivl1(i))
    if (bavard) then
      write(cout,'(A,A,A,2(1x,D14.7))') '[',IAM,']', polyg1(:,ii)
      call logmes(cout, .true.)
    end if
    if (ii == nb_vertex1) exit
  enddo
     
  if( bavard ) call logmes('['//IAM//'] polyg2', .true.)
  nb_vertex2=0
  do i=1,size(ivl2) 
    if (ivl2(i) <= 0) cycle
    nb_vertex2 = nb_vertex2 + 1
  enddo
  ! on vire le dernier qui est le premier
  nb_vertex2=nb_vertex2-1  
  if (bavard) then
    write(cout,'(A,A,A,1X,I0)') '[',IAM,'] nb vertex:',nb_vertex2
    call logmes(cout, .true.)
  end if
  
  allocate(polyg2(2,nb_vertex2))

  ii=0
  do i=1,size(ivl2) 
    if (ivl2(i) <= 0) cycle
    ii = ii + 1
    polyg2(:,ii) = vl2(:,ivl2(i))
    if (bavard) then
      write(cout,'(A,A,A,2(1x,D14.7))') '[',IAM,']', polyg2(:,ii)
      call logmes(cout, .true.)
    end if
    if (ii == nb_vertex2) exit
  enddo

  !
  allocate(sizes1(1),sizes2(1))
  sizes1(1) = size(polyg1, dim=2)
  sizes2(1) = size(polyg2, dim=2)
  call polygones_intersection(polyg1, sizes1, polyg2, sizes2, shrink1, shrink2, delta, cpoints, csizes, carea)
  !

  if (.not. associated(cpoints)) then
    !err = 1
    if( bavard ) call logmes('['//IAM//'] no intersection points', .true.)
    return
  end if
  
  if (bavard) then
    write(cout,*) ' '
    call logmes(cout, .true.)
    write(cout,*) 'cpoints: ', cpoints
    call logmes(cout, .true.)
    write(cout,*) 'csizes: ' , csizes
    call logmes(cout, .true.)
    write(cout,*) 'carea: '  , carea
    call logmes(cout, .true.)
    write(cout,*) ' '
    call logmes(cout, .true.)
  endif

  allocate( nb_points, source=csizes )
  nb_pts = sum(nb_points)
  nb_pol = size(nb_points)
  area   = sum(carea(1:nb_pol))

  allocate(points(2,nb_pts))
  !print*,nb_points
  do i=1,nb_pts
    points(:,i)=cpoints(:,i) 
  enddo   
  !print*,points
 
  deallocate(sizes1,polyg1)
  deallocate(sizes2,polyg2)
  call clipper_free(cpoints)
  call clipper_free(csizes)
  call clipper_free(carea)
  
end subroutine

subroutine display_polytope(nb_vertex,vl,ivl)
  implicit none
  integer      :: nb_vertex
  real(kind=8) :: vl(:,:)
  integer      :: ivl(:)
  !***
  integer      :: iii

  print*,'-------------------------------------------'
  print*,'nb_vertex= ',nb_vertex
  print*,'----'
  do iii=1,nb_vertex
    print "(2(1x,E12.5))",vl(:,ivl(iii))
  enddo

end subroutine display_polytope

  
!> 0 on ne coupe pas
!> 1xxxx on coupe: 
!> 2xxxx on est // et on passe donc par deux points
!> un x pour a,b,c et d, la valeur du x=1 si on passe par ce point 
!> (p,q) points d intersection
integer function segments_intersection_wp(a,b,c,d,p,q,err)
  use predicates
  implicit none
  real(kind=8),dimension(2) :: a,b,c,d,p,q
  integer                   :: err

  !***
  real(kind=8) :: denom,num,ss,zero !,tt

  real(kind=8)    :: x1,x2,x3,x4
  logical         :: croise
  integer(kind=4) :: i
  !                        123456789012345678901234567890123456789012
  character(len=42):: IAM='DiscreteGeometry::segments_intersection_wp'

  err = 0
  
  p = 0.d0 ; q = 0.d0
  segments_intersection_wp = -99

  ! definition de tol en utilisant la valeur fournie par predicates
  zero= f_exactinit()

  ! on teste si intersection
  x1 = f_orient2d(c, d, a)
  x2 = f_orient2d(c, d, b)  

  croise=.false.
  if (x1*x2 <= 0.d0) then
    x3 = f_orient2d(a, b, c)
    x4 = f_orient2d(a, b, d)  
    if (x3*x4 <= 0.d0) then
      croise=.true.
    endif
  endif

  ! ne croise pas on sort
  if (.not. croise) then
     segments_intersection_wp = 0
     return
  endif

  ! cas //
  ! si les 2 segments sont dans le meme sens c est trivial a,c -> q & b,d ->p
  ! si les segments sont en sens inverse il faut gerer contacts ac et bd 
  if (x1 == 0.d0 .and. x2 == 0.d0) then
    ! parano 
    if (x3 == 0.d0 .and. x4 == 0.d0) then
      segments_intersection_wp = 20000
      i=0
      
      if (between(c,d,a)) then
        q = a 
        i = 1
        segments_intersection_wp=segments_intersection_wp+1000
      endif  

      if (between(a,b,c)) then
        if (i == 1) then
          ! car a est deja dans q    
          p = c 
        else
          q = c
        endif
        segments_intersection_wp=segments_intersection_wp+10
      endif

      if (between(a,b,d)) then
        p=d 
        i = 10
        segments_intersection_wp=segments_intersection_wp+1
      endif  

      if (between(c,d,b)) then
        if (i == 10) then 
          q=b
        else 
          p=b 
        endif
        segments_intersection_wp=segments_intersection_wp+100
      endif

      return

    else
      call logmes(IAM//' something wrong in management of parallel segment',.true.)
      err = 1
      return
    endif  
  endif

  ! noeuds extremites
  !
  ! cas degenere ou on passe par 1 ou 2 sommets
  if (x2 == 0.d0) then
    if (x3 == 0.d0) then  
      segments_intersection_wp = 10110
    else if (x4 == 0.d0) then
      segments_intersection_wp = 10101
    else
      segments_intersection_wp = 10100
    endif
    p=b
    return
  endif
  if (x4 == 0.d0) then
    if (x1 == 0.d0) then  
      segments_intersection_wp = 11001
    else
      segments_intersection_wp = 10001
    endif
    p=d
    return
  endif
  if (x1 == 0.d0) then
    if (x3 == 0.d0) then  
      segments_intersection_wp = 11010
    else
      segments_intersection_wp = 11000
    endif   
    p=a
    return
  endif
  if (x3 == 0.d0) then
    segments_intersection_wp = 10010
    p=c
    return
  endif

  ! cas classique
  ! Equation parametrique des segments du contour

  denom=a(1)*(d(2)-c(2)) + &
        b(1)*(c(2)-d(2)) + &
        d(1)*(b(2)-a(2)) + &
        c(1)*(a(2)-b(2))

  IF (dabs(denom) < zero) THEN
    call logmes(IAM//' there should be an intersection',.true.)
    err = 1
    return
  ELSE
    segments_intersection_wp = 10000
    ! Parametres ss et tt

    num=a(1)*(d(2)-c(2)) + &
        c(1)*(a(2)-d(2)) + &
        d(1)*(c(2)-a(2))

    ss=num/denom

    !num=-(a(1)*(c(2)-b(2)) +  &
    !      b(1)*(a(2)-c(2)) + &
    !      c(1)*(b(2)-a(2)))
    ! 
    !tt=num/denom

    p(:)=a(:) + ss* (b(:)-a(:))

    return
 endif
 if (segments_intersection_wp < 0) then
   call logmes(IAM//' cannot return without status',.true.)
   err= 1
   return
 endif     
end function

!> is c between a and b
logical function between(a,b,c) 
   implicit none
   real(kind=8) :: a(2),b(2),c(2)
   between=.FALSE.
   if (a(1) == b(1) ) then
     if ((a(2) <= c(2) .and. c(2) <= b(2)) .or. &  
         (a(2) >= c(2) .and. c(2) >= b(2))) then
       between = .TRUE.
     endif 
   else  
     if ((a(1) <= c(1) .and. c(1) <= b(1)) .or. &  
         (a(1) >= c(1) .and. c(1) >= b(1))) then
       between = .TRUE.
     endif 
   endif
end function between

!> check if a set of nodes is in a polytop 
!> coor (I) : coordinates of the node
!> vl (I): array of vertices (1:m)
!> ivl (I): list of valid vertices (1:n,n-1 <=m, ivl(1) == ivl(n))
!> vertices are stored anti-clockwise
!> ref_size (I): ref distance for the tests 
!> node_in_polytope (O): 0=out, 1=in
integer function node_in_polytope(coor,vlc,ivlc,ref_size,bavard,err)
  implicit none
  real(kind=8) :: coor(2),vlc(:,:),ref_size
  integer      :: ivlc(:)
  logical      :: bavard
  integer      :: err
  
  !***
  integer :: nbc ! number of points of the contour
  real(kind=8) :: tol,x1(2),x2(2)
  integer :: j,nints

  err= 0

  node_in_polytope=0

  tol = 1d-14 * ref_size

  if (minval(ivlc) == 0) then
    nbc = minloc(ivlc,dim=1) - 2
  else
    nbc = size(ivlc) - 1
  endif

  if (nbc < 3) then
    call logmes('DiscreteGeometry::node_in_polytope polytope has too few nodes',.true.)
    err = 1
    return
  endif

  ! pour le point on cherche combien la ligne horizontale partant de lui vers +infini
  ! coupe de segments si le nb est impair le point est dans le contour  

  nints=0 ! nombre de segment croise 
  do j=1,nbc

    ! recherche d un segment concerne
!fd ce teste avec des tolerance saute si ne noeud est pile au niveau d'un vertex
! car dans ce cas il voit les 2 segments
!    if ( coor(2) > min(vlc(2,ivlc(j)),vlc(2,ivlc(j+1))) - tol  .and. &
!         coor(2) < max(vlc(2,ivlc(j)),vlc(2,ivlc(j+1))) + tol ) then

!fd ce teste devrait mieux marcher sur un cote; sur une pointe c'est pas sur
    if ( coor(2) >= min(vlc(2,ivlc(j)),vlc(2,ivlc(j+1)))   .and. &
         coor(2) < max(vlc(2,ivlc(j)),vlc(2,ivlc(j+1)))  ) then
 
      if (bavard) print*,'noeud voit segment', ivlc(j),ivlc(j+1)

      ! trop loin degage
      if (coor(1) > max(vlc(1,ivlc(j)),vlc(1,ivlc(j+1))) + tol) cycle 

      ! bien avant ok           
      if (coor(1) < min(vlc(1,ivlc(j)),vlc(1,ivlc(j+1))) - tol) then
        nints = nints + 1

        if (bavard) print*,'et noeud croise'

        cycle 
      endif
       
      ! entre 2 on teste 
      x1 = vlc(:,ivlc(j+1)) - vlc(:,ivlc(j))
      x2 = coor - vlc(:,ivlc(j))

!fd ne faut il pas mettre >= et =< ?

      if (vlc(2,ivlc(j+1)) > vlc(2,ivlc(j))) then
        ! segment montant le produit vectoriel est > 0 
        if ( (x1(1) * x2(2)) - (x1(2) * x2(1)) > 0.d0) nints = nints + 1 
        if (bavard) print*,'et noeud croise (montant)'
      else
        ! segment descendant le produit vectoriel est < 0 
        if ( (x1(1) * x2(2)) - (x1(2) * x2(1)) < 0.d0) nints = nints + 1 
        if (bavard) print*,'et noeud croise (descendant)'
      endif
    endif
  enddo

  if (mod(nints,2) /= 0) node_in_polytope=1

end function

function comp_normal_to_triangle(p)
   implicit none
   real(kind=8) :: p(3,3)
   real(kind=8),dimension(3):: comp_normal_to_triangle
   ! internal variables
   real(kind=8) :: d1(3),d2(3),n(3)
   !
   d1(:) = p(:,2) - p(:,1)
   d2(:) = p(:,3) - p(:,1)
   n = cross_product(d1,d2)
   comp_normal_to_triangle = n/length3(n)
end function


SUBROUTINE comp_rep(t,n,s)
IMPLICIT NONE
REAL(kind=8),DIMENSION(3) :: n,t,s

IF (dabs(n(1)) < 1.d-15) THEN

! fd on est dans le plan y-z

 t(1) =  n(1)
 t(2) = -n(3)
 t(3) =  n(2)

 t = t/length3(t)

 s=cross_product(t,n)

 RETURN

ENDIF

IF (dabs(n(2)) < 1.d-15) THEN

! fd on est dans le plan z-x


 t(2) =  n(2)

 t(1) = -n(3)
 t(3) =  n(1)

 t = t/length3(t)

 s=cross_product(t,n)

 RETURN

ENDIF

IF (dabs(n(3)) < 1.d-15) THEN

! fd on est dans le plan x-y


 t(1) = -n(2)
 t(2) =  n(1)
 t(3) =  n(3)

 t = t/length3(t)

 s=cross_product(t,n)

 RETURN

ENDIF

!fd cas general 

!fd on genere un premier vecteur perpendiculaire
t(1) = 1.; t(2) = 0. ; t(3) = 0.
s = cross_product(t,n)
t=s/length3(s)

s=cross_product(t,n)


END SUBROUTINE comp_rep


  !> routine servant a orienter dans le meme sens toutes les faces d une surface triangulee
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> adjac liste des ele adjacents a un ele , si 0 c'est un bord
  subroutine set_orientation_surface_T3(nbnode,nbele,connec,err)
     implicit none

     integer :: nbnode,nbele
     integer :: connec(3,nbele)
     integer :: err

     !*****
     integer :: ie,is,in,ic,node_begin,node_end
     integer :: iadj,je,i,j,ideb,ifin,jdeb,jfin,isens,jsens,itmp
     integer,allocatable :: tag(:)
     integer,allocatable,target :: aux1(:),aux2(:)  
     integer,pointer :: to_search(:),next_search(:),flipflap(:)

     ! construction de la liste des ele adjacents a un noeud 
     integer,dimension(:,:),allocatable :: adj_ele_2_node
     integer,dimension(:)  ,allocatable :: idx_adj_ele_2_node
     integer :: max_adj_ele_2_node = 200

     ! pour manipuler les surfaces des objets
     integer,dimension(:,:),allocatable :: adj_ele_2_ele
     integer,dimension(:) ,allocatable :: idx_adj_ele_2_ele
     integer :: max_adj_ele_2_ele = 3

     !***
     logical :: is_found

     character(len=108) :: cout
     character(len=44)  :: IAM
           !12345678901234567890123456789012345678901234
     IAM = 'DiscreteGeometry::set_orientation_surface_T3' 

     err = 0

     ! on prepare la liste des elements adjacents a un noeud
     ! on travaille avec un tableau 2D surdimensionne et 
     ! un vecteur qui donne la taille reelle 

     allocate(adj_ele_2_node(max_adj_ele_2_node,nbnode), &
              idx_adj_ele_2_node(nbnode))

     adj_ele_2_node = 0
     idx_adj_ele_2_node = 0

     do ie=1,nbele
       do ic=1,3
         in = connec(ic,ie)
         idx_adj_ele_2_node(in) = idx_adj_ele_2_node(in) + 1 
         if ( idx_adj_ele_2_node(in) > max_adj_ele_2_node) then
           write(cout,'(A)') 'Error: max_adj_ele has been reached'
           call logmes(cout,.true.) 
           write(cout,'(A,I0,A,I0)') 'for node ',in,' value ',idx_adj_ele_2_node(in)
           call logmes(cout,.true.)
           call logmes('Error in '//IAM//' while computing adjacent elements to a node',.true.)
           err = 1
           return
         endif
         adj_ele_2_node(idx_adj_ele_2_node(in),in)=ie
       enddo
     enddo


     ! on prepare la liste des elements adjacents a un element
     ! on travaille avec un tableau 2D surdimensionne et 
     ! un vecteur qui donne la taille reelle 

     allocate(adj_ele_2_ele(max_adj_ele_2_ele,nbele), &
             idx_adj_ele_2_ele(nbele))

     adj_ele_2_ele =0
     idx_adj_ele_2_ele=0

     ! construction de la liste des elements adjacents a chaque element
     do ie=1,nbele
       ! on va parcourir les cotes
       do ic=1,3

         node_begin=connec(ic,ie)
         node_end  =connec(mod(ic,3)+1,ie)

         is_found=.false.
         do iadj=1,idx_adj_ele_2_node(node_begin) 
           je=adj_ele_2_node(iadj,node_begin)
           if (je == ie) cycle
           if (count(connec(:,je) == node_end) /=0 ) then
             idx_adj_ele_2_ele(ie) = idx_adj_ele_2_ele(ie) + 1  
             if ( idx_adj_ele_2_ele(ie) > max_adj_ele_2_ele) then
               write(cout,'(A)') 'Error: max_adj_ele_2_ele has been reached'
               call logmes(cout,.true.) 
               write(cout,'(A,I0,A,I0)') 'for ele ',in,' value ',idx_adj_ele_2_ele(ie)
               call logmes(cout,.true.)
               call logmes('Error in '//IAM//' while computing adjacent elements to an element',.true.)
               err= 1
               return
             endif
             adj_ele_2_ele(idx_adj_ele_2_ele(ie),ie) = je
             is_found=.true.
             exit
           endif
         enddo

         !if (.not. is_found) then
         !  print*,'element: ',ie
         !  print*,'side ',node_begin,node_end,' without adjacent'
         !endif
       enddo
     enddo

     deallocate(adj_ele_2_node,idx_adj_ele_2_node)

     !print*,'orientation des surfaces'

     ! les faces traitees (la 1 va bien !)
     allocate(tag(nbele))
     tag = 0
     tag(1) = 1

     ! tableaux intermediaires pour la manipulation des voisins 
     allocate(aux1(nbele), aux2(nbele))  
     to_search=>aux1
     next_search=>aux2

     !on initialise le processus de recherche
     to_search=0
     to_search(1)=1

     do 
       !on construit une nouvelle liste de recherche
       next_search=0
       !on parcourt ce qui a ete pose dans to_search et on tag les adj 
       in=0
       do is=1,nbele
         if (to_search(is) == 0) exit
         ie = to_search(is)

         do iadj=1,3
           ! print*,tag(adj_ele_2_ele(iadj,ie))
           ! si deja tagge ou deja dans la liste on cycle
           if (adj_ele_2_ele(iadj,ie) == 0) cycle
           if (tag(adj_ele_2_ele(iadj,ie)) /= 0 .or. & 
               count(next_search(:) == adj_ele_2_ele(iadj,ie)) /= 0 ) cycle      
           in = in + 1
           next_search(in)=adj_ele_2_ele(iadj,ie)
         enddo
       enddo

       !print*,in
       !print*,next_search

       !plus rien a trouver
       if (in==0) exit

       flipflap => to_search
       to_search => next_search
       next_search => flipflap

       ! on teste les ele dans la liste de recherche
       do is=1,nbele
         if (to_search(is) == 0) exit
         ie = to_search(is)
         !print*,'======'
         !print*,'on teste l element ',ie

         do iadj=1,3
           if (adj_ele_2_ele(iadj,ie) == 0) cycle
           !print*,'etat de l adjacent ',tag(adj_ele_2_ele(iadj,ie))
           ! si deja tagge ou deja dans la liste on cycle
           if (tag(adj_ele_2_ele(iadj,ie)) /= 0) then      
             je = adj_ele_2_ele(iadj,ie)
             ideb=0;ifin=0
             do i=1,3
               do j=1,3
                 if (connec(i,ie) == connec(j,je)) then 
                   ! on a trouve le premier
                   if (ideb == 0) then
                     ideb = i
                     jdeb = j
                     exit
                   else
                     ifin=i
                     jfin=j
                     exit
                   endif
                 endif
               enddo 
               if (ifin /= 0) exit
             enddo
             if (ifin == 0) then
                write(cout,'(A,I0,A,I0)') 'impossible to find a common edge for elements ',ie, ' and ',je
                call logmes(cout,.true.)
                call logmes('Error in '//IAM,.true.)
                err = 1
                return
             endif

             !fd on teste le sens
             isens = ifin - ideb 
             jsens = jfin - jdeb

             if (jsens == 1 .or. jsens == -2) then 
               ! on change le sens
               if (isens == 1 .or. isens == -2) then
                 itmp=connec(ideb,ie)
                 connec(ideb,ie)=connec(ifin,ie)
                 connec(ifin,ie)=itmp
                 !print*,'flip',connec(ifin,ie),connec(ideb,ie)
               endif
             else
               if (isens ==-1 .or. isens == 2) then
                 itmp=connec(ideb,ie)
                 connec(ideb,ie)=connec(ifin,ie)
                 connec(ifin,ie)=itmp
                 !print*,'flip',connec(ifin,ie),connec(ideb,ie)
               endif
             endif

             tag(ie)=1
             exit
           endif
         enddo
         if (tag(ie) == 0) then
           write(cout,'(A,I0)') 'impossible to find tagged neighboor for element', ie
           call logmes(cout,.true.) 
           call logmes('Error in '//IAM,.true.)
           err = 1
           return
         endif
       enddo
     enddo

     deallocate(adj_ele_2_ele,idx_adj_ele_2_ele)
     deallocate(tag,aux1,aux2)

  end subroutine



  !> routine servant a calculer des faces topologiques dans un maillage
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> adjac liste des ele adjacents a un ele , si 0 c'est un bord
  !> err : 0 ok, -1 warning, 1 error
  subroutine build_topology_surface_T3(nbnode,nbele,connec,normal,tol_topology,tol_flatness, &
                                       topo_face,contour_face,sommet,cote,status_face,err)
     implicit none

     integer                             :: nbnode,nbele
     integer                             :: connec(3,nbele)
     real(kind=8)                        :: normal(3,nbele),tol_topology,tol_flatness
     type(G_i_list),dimension(:),pointer :: topo_face
     type(G_i_list),dimension(:),pointer :: contour_face 
     integer,dimension(:)  , pointer     :: sommet
     integer,dimension(:,:), pointer     :: cote
     integer,dimension(:)  , pointer     :: status_face  ! 0 flat, 1 curved
     integer                             :: err
     
     !*****
     integer :: ie,is,in,ic,node_begin,node_end,nb_bord
     integer :: iadj,je,i,j,nb_topo_face,jadj,ke
     integer,allocatable :: skip(:),belong(:)
     integer,allocatable,target :: aux1(:),aux2(:)  
     integer,pointer :: to_search(:),next_search(:),flipflap(:)

     ! construction de la liste des ele adjacents a un noeud 
     integer,dimension(:,:),allocatable :: adj_ele_2_node
     integer,dimension(:)  ,allocatable :: idx_adj_ele_2_node
     integer :: max_adj_ele_2_node = 200

     ! pour manipuler les surfaces des objets
     integer,dimension(:,:),allocatable :: adj_ele_2_ele
     integer,dimension(:) ,allocatable :: idx_adj_ele_2_ele
     integer :: max_adj_ele_2_ele = 3

     ! pour le statut
     real(kind=8) :: dist

     ! a la peche aux cote
     integer :: ibeg,iend,nb_cote,jj,j_first,idx_cote,k, i_zero, nb_zeros
     ! le nb de cotes entre 2 faces du maillage de peau
     ! si le maillage est irregulier dans sa courbure c'est le nombre de edge du maillage 
     integer,parameter :: max_nb_cote = 500000
     integer           :: max_cote(2,max_nb_cote) 

     !***
     logical :: is_found,bavard=.false., look_for_end, keep

     !
     character(len=108) :: cout
                            !1234567890123456789012345678901234567890123
     character(len=43)::IAM='DiscreteGeometry::build_topology_surface_T3'

     err = 0

     !print*,'Starting Building Tolopogy'
     !print*,'tol pro sca normales: ',tol_topology
     !print*,'tol flatness: ',tol_flatness


     !print*, 'build topo'
     !print*,nbnode,nbele
     !do ie=1,nbele
     !  print*, '  ',ie,'->', connec(:,ie)
     !enddo

     !!! Phase preliminaire on cree ce qu'il faut pour parcourir la surface !!!!


     ! on prepare la liste des elements adjacents a un noeud
     ! on travaille avec un tableau 2D surdimensionne et 
     ! un vecteur qui donne la taille reelle 

     allocate(adj_ele_2_node(max_adj_ele_2_node,nbnode), &
              idx_adj_ele_2_node(nbnode))

     adj_ele_2_node = 0
     idx_adj_ele_2_node = 0

     do ie=1,nbele
       do ic=1,3
         in = connec(ic,ie)
         idx_adj_ele_2_node(in) = idx_adj_ele_2_node(in) + 1 
         if ( idx_adj_ele_2_node(in) > max_adj_ele_2_node) then
           cout='' 
           write(cout,'(A)') 'you reach max_adj_ele'
           call logmes(cout,.true.)
           cout=''
           write(cout,'(A,I0,A,I0)') 'for node ',in,' value ',idx_adj_ele_2_node(in)
           call logmes(cout,.true.)
           call logmes('Error in '//IAM,.true.)
           err = 1
           return
         endif
         adj_ele_2_node(idx_adj_ele_2_node(in),in)=ie
       enddo
     enddo


     ! on prepare la liste des elements adjacents par les bords a un element
     ! on travaille avec un tableau 2D surdimensionne et 
     ! un vecteur qui donne la taille reelle 

     allocate(adj_ele_2_ele(max_adj_ele_2_ele,nbele), &
             idx_adj_ele_2_ele(nbele))

     adj_ele_2_ele =0
     idx_adj_ele_2_ele=0

     ! construction de la liste des elements adjacents a chaque element
     do ie=1,nbele
       ! on va parcourir les cotes
       do ic=1,3

         node_begin=connec(ic,ie)
         node_end  =connec(mod(ic,3)+1,ie)

         is_found=.false.
         do iadj=1,idx_adj_ele_2_node(node_begin) 
           je=adj_ele_2_node(iadj,node_begin)
           if (je == ie) cycle
           if ( any( connec(:,je) == node_end ) ) then
             idx_adj_ele_2_ele(ie) = idx_adj_ele_2_ele(ie) + 1  
             if ( idx_adj_ele_2_ele(ie) > max_adj_ele_2_ele) then
               cout='' 
               write(cout,'(A)') 'you reach max_adj_ele_2_ele'
               call logmes(cout,.true.)
               cout=''
               write(cout,'(A,I0,A,I0)') 'for ele ',ie,' value ',idx_adj_ele_2_ele(ie)
               call logmes(cout,.true.)
               call logmes('Error in '//IAM,.true.)
               err = 1              
               return
             endif
             adj_ele_2_ele(idx_adj_ele_2_ele(ie),ie) = je
             is_found=.true.
             exit
           endif
         enddo

         !if (.not. is_found) then
         !  print*,'element: ',ie
         !  print*,'side ',node_begin,node_end,' without adjacent'
         !endif
       enddo
     enddo

     !do ie=1,nbele
     !  print*,'ele ',ie,' adjacents'
     !  print*,adj_ele_2_ele(1:idx_adj_ele_2_ele(ie),ie)        
     !enddo

     !!!! Construction des faces topologiques

     ! tableaux intermediaires pour la manipulation des voisins 
     allocate(aux1(nbele), aux2(nbele))  
     to_search=>aux1
     next_search=>aux2

     nb_topo_face = 0
     allocate(skip(nbele),belong(nbele))
     skip   = 0
     belong = (/ (i,i=1,nbele) /)

     ! search a free element
     do ie=1,nbele-1
       if (skip(ie) == 1) cycle
       nb_topo_face = nb_topo_face + 1

       !print*,'====='
       !print*,'on ajoute la face',ie

       !on initialise le processus de recherche
       to_search=0
       to_search(1)=ie

       skip(ie) = 1

       do 
         !on construit une nouvelle liste de recherche
         next_search=0
         !on parcourt ce qui a ete pose dans to_search et on tag les adj 
         in=0

         do is=1,nbele
           if (to_search(is) == 0) exit 

           je = to_search(is)

           !print*,'voisin de ',je
           !print*,adj_ele_2_ele(:,je)

           ! on regarde les elements adjacents
           ! TODO utiliser HE_hdl
           do iadj = 1, idx_adj_ele_2_ele(je)
             ! si deja tagge ou deja dans la liste on cycle
             ! print*,skip(adj_ele_2_ele(iadj,je))
             if (skip(adj_ele_2_ele(iadj,je)) == 1 .or. & 
                 any( next_search(:) == adj_ele_2_ele(iadj,je) ) ) cycle      
             
             !print*,je,dot_product(normal(:,je),normal(:,adj_ele_2_ele(iadj,je)))

             if (dot_product(normal(:,je),normal(:,adj_ele_2_ele(iadj,je))) > tol_topology) then

               !print*,'on ajoute l element ',adj_ele_2_ele(iadj,je),' dans la face' 

               in = in + 1
               next_search(in) = adj_ele_2_ele(iadj,je)

               skip(adj_ele_2_ele(iadj,je)) = 1    ! on tag 
               belong(adj_ele_2_ele(iadj,je)) = ie ! on affecte
             !else
               !print*,'angle pas bon pour ',adj_ele_2_ele(iadj,je) 
               !print*,je,normal(:,je)
               !print*,adj_ele_2_ele(iadj,je),normal(:,adj_ele_2_ele(iadj,je))

             endif
           enddo
         enddo

         !print*,in
         !if (in /= 0) print*,next_search(1:in)

         !plus rien a trouver
         if (in==0) exit

         flipflap => to_search
         to_search => next_search
         next_search => flipflap

       enddo
     enddo

     ! au cas ou la derniere face soit la seule dans un groupe de faces
     if (skip(nbele) == 0) nb_topo_face = nb_topo_face + 1 

     allocate(topo_face(nb_topo_face), &
              contour_face(nb_topo_face), &
              status_face(nb_topo_face))

     nb_topo_face = 0

     do ie = 1, nbele
       if (belong(ie) /= ie) cycle  
       nb_topo_face = nb_topo_face + 1
       i = count(belong == ie) 

       if (nb_topo_face > size(topo_face)) then
         !print *,nbele
         !print *,'nb de groupe de faces : ',size(topo_face)
         !do i=1,size(topo_face)
         !   print *,'composees des faces : ', topo_face(i)%G_i(:)
         !enddo
         !print *,'Ou mettre les ',i,' faces numero',ie
         !print *,'tableau belong ',belong
          call logmes('Error in '//IAM//' : pb with topo_face',.true.)
          err = 1
          return
       endif

       allocate(topo_face(nb_topo_face)%G_i(i))
       i=0
       do je=ie,nbele
         if (belong(je) /= belong(ie) ) cycle     
         i = i+1
         topo_face(nb_topo_face)%G_i(i) = je
       enddo
     end do

     !do i=1,nb_topo_face
     !  print*,'face ',i  
     !  do j=1,size(topo_face(i)%G_i)
     !    print*,'ele ',topo_face(i)%G_i(j)
     !    print*,'connectivite ',connec(:,topo_face(i)%G_i(j))
     !  enddo
     !enddo 

     ! construction du statut des faces topo (flat|curved)
     ! identification des elements ayant un bord (pas d'ele adjacent dans la meme face topo)
     ! construction des contours

     !fd on se ressert du tableau pour compter a 
     !   combien de contour un noeud appartient
     idx_adj_ele_2_node = 0

     status_face = 0 
     do i=1,nb_topo_face

       ! ecart moyen en orientation au premier element
       dist = 0.d0

       ! nombre d'elements de bord de la face
       nb_bord=0
       do j=1,size(topo_face(i)%G_i)
         ie = topo_face(i)%G_i(j)

         !fd on somme ce qui s'ecarte de l'alignement         
         dist = dist + dabs(dot_product(normal(:,topo_face(i)%G_i(1)),normal(:,ie)) - 1.d0) 

         !rm counting open edges (of the geometrie)
         nb_bord = nb_bord + (3-idx_adj_ele_2_ele(ie))
         !fd on compte les bords libres (de face topo)
         do iadj=1,idx_adj_ele_2_ele(ie)
           je=adj_ele_2_ele(iadj,ie)
           if ( all(topo_face(i)%G_i /= je) ) then
             
             !print*,'l element ',ie,' a 1 bord de plus'
             nb_bord = nb_bord + 1
           endif
         enddo
       enddo

       !print*,dist/size(topo_face(i)%G_i(:)),1e-6
       if ((dist/size(topo_face(i)%G_i(:))) > tol_flatness) status_face(i) = 1
       !print*,status_face(i)

       ! un contour est une bete liste de 2 points successifs
       allocate(contour_face(i)%G_i(2*nb_bord))

       !fd on rempli le contour avec les bords
       !rm: a small problem, when looking for the nodes of the open edges,
       !    because there is no adjacent element of another topo face to compare node index.
       !    Thus what's must be tested, is that there are NO neighbour to this edge
       !    which is even more of a pain in case of 2 open edges (because the test is heavy,
       !    and if not careful, the same edge may be found twice...).
       nb_bord=0
       do j=1,size(topo_face(i)%G_i)
         ie=topo_face(i)%G_i(j)
         !rm : must check all edges, not only those with adjacent element
         !do iadj = 1, idx_adj_ele_2_ele(ie)
         nb_zeros = 3-idx_adj_ele_2_ele(ie)
         do iadj = 1, 3
           je=adj_ele_2_ele(iadj,ie)
           is_found=.false.
           if ( je/=0 .and. any(topo_face(i)%G_i == je) ) cycle
           ! on cherche l'arrete de l element
           i_zero = 0
           do ic=1,3
             node_begin=connec(ic,ie)
             node_end=connec(mod(ic,3)+1,ie)
             if( je/=0 ) then
               ! arrete de l'element ie en commun avec je
               if( any(connec(:,je)==node_begin) .and. any(connec(:,je)==node_end) ) then
                 is_found = .true.
                 exit
               endif
             else
               ! look for open edge, which is not an edge of the current element with a neighbour
               is_found = .true.
               do jadj = 1, idx_adj_ele_2_ele(ie)
                 ke = adj_ele_2_ele(jadj,ie)
                 is_found = is_found .and. .not.( any(connec(:,ke)==node_begin) .and. any(connec(:,ke)==node_end) )
               end do
               if( .not. is_found ) cycle
               i_zero = i_zero + 1
               if( i_zero < nb_zeros ) cycle
               nb_zeros = nb_zeros - 1
               exit
             endif
           enddo
           ! on sauve les noeuds
           if (is_found) then
             contour_face(i)%G_i(2*nb_bord+1)=node_begin
             contour_face(i)%G_i(2*nb_bord+2)=node_end
             nb_bord = nb_bord + 1
             !print*,'ajout du bord ',nb_bord
             !fd on incremente le compteur de contour du noeud
             idx_adj_ele_2_node(node_begin) = idx_adj_ele_2_node(node_begin) + 1
           endif
         enddo
       enddo

       ! on ordonne les bords
       j_first = 1
       do j=1,nb_bord-1
         is_found=.false.
         !print*,'contour avant',contour_face(i)%G_i

         do iadj=j+1,nb_bord       
           if (contour_face(i)%G_i(2*(j-1)+2) == contour_face(i)%G_i(2*(iadj-1)+1)) then
             is_found=.true.
             if (iadj > j+1) then
               ! le bord suivant est plus loin dans la liste il faut qu on permute
               node_begin=contour_face(i)%G_i(2*(iadj-1)+1)
               node_end=contour_face(i)%G_i(2*(iadj-1)+2)
               contour_face(i)%G_i(2*(iadj-1)+1:2*(iadj-1)+2) = contour_face(i)%G_i(2*j+1:2*j+2)
               contour_face(i)%G_i(2*j+1) = node_begin
               contour_face(i)%G_i(2*j+2) = node_end
             endif
             exit
           endif
         enddo

         if (.not. is_found) then
           !fd on est peut etre tombe sur un cas avec un trou on saute le raccord

           !fd todo arriver a gerer les faces topo avec des trous ...

           !print*,contour_face(i)%G_i(1),contour_face(i)%G_i(2*(j-1)+2)
           if (nb_bord > 2) then   
             if (contour_face(i)%G_i(2*(j_first-1)+1) == contour_face(i)%G_i(2*(j-1)+2)) then
               write(cout,'(A,A,A,I0,A)') 'Warning in ',IAM,' : Topological face ',i,' has a hole'    
               call logmes(cout,.true.)
               err=-1
               j_first = j+1
               cycle  
             endif  
           else
             if (contour_face(i)%G_i(1) == contour_face(i)%G_i(2*(nb_bord-1)+2)) then
               write(cout,'(A,A,A,I0,A)') 'Warning in ',IAM,' : Topological face ',i,' is strange'
               call logmes(cout,.true.)
               err=-1
               !print*,'nb bords ',nb_bord
               !print*,'contour:'
               !print*,contour_face(i)%G_i(:)
               !print*,'nb ele ',size(topo_face(i)%G_i)
               cycle  
             endif  
           endif 
           !print*,'on n arrive pas a ordonner le contour de la face ',i
           !print*,'nb ele ',size(topo_face(i)%G_i)
           !print*,'nb bords ',nb_bord
           !print*,'contour:'
           !print*,contour_face(i)%G_i(:)
           call logmes('Error in '//IAM//' : unable to determine the boundary of the face',.true.)
           err=1
           return
         endif
       enddo
     enddo

     !fd recherche des sommets, i.e. noeuds a la jonction de plusieurs faces
     !rm: can be 0...
     i = count(idx_adj_ele_2_node > 2) 
     allocate(sommet(i))
     j=0
     do i=1,nbnode
       if (idx_adj_ele_2_node(i) > 2) then
         j = j + 1
         sommet(j) = i
       endif
     enddo

     !fd recherche des cote, i.e. partie de contour partagee par 2 faces. 
     !fd les extremitees d un cote sont 2 sommets

     look_for_end = .FALSE.
     idx_cote=0
     max_cote=0

     do i=1,nb_topo_face

       !print*,'->'
       
       j_first=0

       ! on prevoit de faire 2 fois le tour
       ! a cause du fait que le debut du 1er cote n'est pas le element de bord
       do jj=1,size(contour_face(i)%G_i)  

         ! on calcul le rang de l'ele de bord 
         j = modulo(jj-1,size(contour_face(i)%G_i)/2)+1            
         !print*,'j= ',j

         ! on sort quand on tombe sur le 1er
         if (j == j_first) exit 

         if (.not. look_for_end) then
           ibeg = contour_face(i)%G_i(2*(j-1)+1)
           if (count(sommet==ibeg) /= 0) then   
             if (j_first == 0) then
               j_first=j
               !print*,'j_first ',j
             endif
             look_for_end = .TRUE.            

             !print*,'ibeg= ',ibeg
           endif
         endif 
         if (look_for_end) then
           iend = contour_face(i)%G_i(2*(j-1)+2)
           if (count(sommet==iend) /= 0) then   

             ! c'est un cote licite              
             ! on teste si le cote existe
             keep = .TRUE.
             do k=1,idx_cote
               if (max_cote(1,k) == min(ibeg,iend) .and. &
                   max_cote(2,k) == max(ibeg,iend) ) then
                 keep=.FALSE.
                 exit
               endif
             enddo                  

             if (keep) then
            
               idx_cote = idx_cote + 1

               if (idx_cote > max_nb_cote) then
                 call logmes('Error in '//IAM//' : maximum size of boundary array reached',.true.)
                 err=1
                 return
               endif  

               max_cote(1,idx_cote) = min(ibeg,iend)
               max_cote(2,idx_cote) = max(ibeg,iend)

               !print*,idx_cote,max_cote(:,idx_cote)

             endif

             look_for_end = .FALSE.            
             !print*,'iend= ',iend
           endif
         endif
       enddo  
       if (look_for_end) then
         call logmes('Error in '//IAM//' : unable to build the boundary',.true.)
         err=1
         return
       endif 
     enddo

     allocate(cote(2,idx_cote)) 
     do i=1,idx_cote
       cote(:,i) = max_cote(:,i)
     enddo     

     if (bavard) then
       print *,'nb de groupe de faces : ',size(topo_face)
       do i=1,size(topo_face)
          print*,'face ',i
          print *,'elements : '
          write(*,*) topo_face(i)%G_i(:)
          !print*,'Noeud porteur : '       ,connec(1,topo_face(i)%G_i(1))
          !print*,'Normale (rep inertie) : ',normal(:,topo_face(i)%G_i(1))
          print*,'Contour:'
          write(*,'(2(1x,I0))') contour_face(i)%G_i
       enddo
     endif

     nullify(to_search,next_search,flipflap)

     deallocate(adj_ele_2_node,idx_adj_ele_2_node)
     deallocate(skip,belong)
     deallocate(adj_ele_2_ele,idx_adj_ele_2_ele)
     deallocate(aux1,aux2)


  end subroutine


  !> routine servant a affecter un numero aux composantes connexes d'un ensemble de faces triangulaires
  subroutine identify_entities_surface_T3(nbnode, nbele, connec, max_adj_ele_2_node, ele2entity, err)

     implicit none

     ! variables d'entree
     integer, intent(in) :: nbnode ! nombre de noeuds
     integer, intent(in) :: nbele ! nombre d'elements
     integer, intent(in) :: connec(3, nbele) ! connectivite des elements
     integer, intent(in) :: max_adj_ele_2_node ! nombre maximal d'elements adjacents a un noeud

     ! variable de sortie
     integer, intent(out) :: ele2entity(nbele) ! tableau donnant le numero d'entite affecte a chaque element

     integer              :: err

     ! variables locales
     integer :: ie ! indice de boucle sur les elements
     integer :: in ! pour recuperer l'indice d'un noeud
     integer :: ic ! inidce de boucle sur les cotes d'un element
     integer :: node_begin, node_end ! indice des noeuds d'un cote d'une element
     integer :: je ! pour recuperer un numero d'element
     integer :: iadj ! indice de boucle sur les elements adjacents a un noeud
     integer, allocatable, target :: aux1(:), aux2(:) ! tableaux cibles necessaires au parcours elements adjacents
     integer, pointer :: to_search(:), next_search(:), flipflap(:) ! pointeurs de tableau necessaires au parcours des
        ! elements adjacents :
        !   to_search : liste des elements a prcourir durant l'iteration courante
        !   next_search : liste des elements a prcourir pendant la prochaine iteration
        !   fliflap : variable intermediaire permettant d'intervertir to_search et next_search
     logical :: is_found=.false. ! vaut "vrai" ssi on a trouve l'element adjacent a l'element courant
     integer :: entity ! numero d'entite courant
     integer :: ideb ! numero du premier element qui n'appartient a aucune entite
     integer :: inext ! indice de la prochaine case disponible dans next_search 
     integer :: is ! indices de boucle pour parcourir to_search

     ! construction de la liste des ele adjacents a un noeud 
     integer, dimension(:, :), allocatable :: adj_ele_2_node ! tableau surdimensionne stockant la liste des elements adjacents
        ! a un noeud
     integer, dimension(:), allocatable :: idx_adj_ele_2_node ! nombre d'elements adjacents a un noeud (taille reelle de la
        ! liste d'elements dajacents a un noeud)

     ! pour manipuler les surfaces des objets
     integer, parameter :: max_adj_ele_2_ele = 3 ! nombre maximal d'elements adjacents a des elements (i.e. 3 pour un maillage en triangles) 
     integer, dimension(:, :), allocatable :: adj_ele_2_ele ! tableau surdimensionne stockant la liste des elements adjacents
        ! a un element
     integer, dimension(:), allocatable :: idx_adj_ele_2_ele ! nombre d'elements adjacents a un noeud (taille reelle de la
        ! liste d'elements dajacents a un noeud)

     character(len=108) :: cout
                            !1234567890123456789012345678901234567890123456
     character(len=46)::IAM='DiscreteGeometry::identify_entities_surface_T3'

     !do ie=1, nbele
     !  print*, 'element ', ie, ' connec '
     !  print*, connec(:, ie)
     !end do

     err = 0

     ! si le nombre maximum d'elements adjacents a un noeud propose n'a pas de sens
     if (max_adj_ele_2_node <= 0) then
        ! on quitte le programme
        call logmes('Error in '//IAM//' : the given maximal number of adjacent elements per node must be non-negative!',.true.)
        err = 1
        return
     end if

     !!! Phase preliminaire on cree ce qu'il faut pour parcourir la surface !!!!

     ! on prepare la liste des elements adjacents a un noeud
     ! on travaille avec un tableau 2D surdimensionne et 
     ! un vecteur qui donne la taille reelle 

     ! allocation memoire des tableaux
     allocate(adj_ele_2_node(max_adj_ele_2_node, nbnode), &
              idx_adj_ele_2_node(nbnode))

     ! initialisation a 0 des tabeaux
     adj_ele_2_node = 0
     idx_adj_ele_2_node = 0

     ! construction de la liste des elements adjacents a chaque noeud

     ! pour chaque element
     do ie=1, nbele
        ! pour chaque noeud de l'element
        do ic=1, 3
           ! on recupere le numero du noeud courant
           in = connec(ic,ie)
           ! on incremente le nombre d'elements adjacents au noeud courant
           idx_adj_ele_2_node(in) = idx_adj_ele_2_node(in) + 1 
           ! si le nombre d'elements adjacents au noeud courant est plus grand
           ! que le nombre maximal d'elements adajacents a un noeud
           if ( idx_adj_ele_2_node(in) > max_adj_ele_2_node) then 
              ! on affiche un message d'erreur
              write(cout,'(A)') 'Error: max_adj_ele reached'
              call logmes(cout,.true.)
              write(cout,'(A,I0,A,I0)') 'for node ', in ,' value ', idx_adj_ele_2_node(in)
              call logmes(cout,.true.)
              ! on quitte le programme
              call logmes('Error in '//IAM,.true.)
              err= 1
              return
           end if
           ! si tout va bien, on stocke le numero de l'element courant dans la liste des
           ! elements adjacents au noeud courant
           adj_ele_2_node(idx_adj_ele_2_node(in), in) = ie
       end do
     end do

     ! on prepare la liste des elements adjacents a un element
     ! on travaille avec un tableau 2D surdimensionne et 
     ! un vecteur qui donne la taille reelle 

     ! allocation memoire des tableaux
     allocate(adj_ele_2_ele(max_adj_ele_2_ele,nbele), &
              idx_adj_ele_2_ele(nbele))

     ! initialisation a 0 des tabeaux
     adj_ele_2_ele =0
     idx_adj_ele_2_ele=0

     ! construction de la liste des elements adjacents a chaque noeud

     ! pour chaque element
     do ie=1, nbele
        ! pour chaque cote de l'element
        do ic=1, 3
           ! on recupere le numero des noeuds du cote de l'element courant
           node_begin = connec(ic, ie)
           node_end   = connec(mod(ic, 3) + 1, ie)
  
           ! on indique qu'on a pas encore trouve l'element adjacent a l'element courant
           ! par le cote courant
           is_found=.false.
           ! pour chaque element adjacent au premier noeud du cote de l'element courant
           do iadj=1, idx_adj_ele_2_node(node_begin) 
              ! on recupere l'element, adjacent au premier noeud de l'element courant, courant
              je=adj_ele_2_node(iadj, node_begin)
              ! s'il coincide avec l'element courant, on passe au suivant
              if (je == ie) cycle
              ! si la connectivite de cet element contient le deuxieme noeud de l'element courant
              if (count(connec(:, je) == node_end) /=0 ) then
                 ! on a trouve l'element adjacent a l'element courant, par le cote courant

                 ! on incremente le nombre d'elements adjacents a l'element courant 
                 idx_adj_ele_2_ele(ie) = idx_adj_ele_2_ele(ie) + 1  
                 ! si le nombre d'elements adjacents a l'element courant est plus grand
                 ! que le nombre maximal d'elements adajacents a un element
                 if ( idx_adj_ele_2_ele(ie) > max_adj_ele_2_ele) then
                    ! on affiche un message d'erreur
                    ! on quitte le programme
                    write(cout,'(A)') 'Error: max_adj_ele_2_ele reached'
                    call logmes(cout,.true.)
                    write(cout,'(A,I0,A,I0)') 'for ele ', ie, ' value ', idx_adj_ele_2_ele(ie)
                    call logmes(cout,.true.)
                    call logmes('Error in'//IAM,.true.)
                 end if
                 ! si tout va bien, on stocke le numero du nouvel element adjacent a l'element courant,
                 adj_ele_2_ele(idx_adj_ele_2_ele(ie), ie) = je
                 ! on indique qu'on a trouve l'element adjacent a l'element courant par le cote
                 ! courant
                 is_found=.true.
                 ! et on quitte la boucle sur les elements adjacents au premeier noeud du cote courant 
                 ! de l'element courant
                 exit
              end if
           end do
 
           ! si on a pas trouve le noeud adjacent a l'element courant par le cote courant
           !if (.not. is_found) then
           !   ! on l'indique a l'utilisateur
           !   print*, 'element: ', ie
           !   print*, 'side ', node_begin, node_end, ' without adjacent'
           !end if
        end do
     end do
 
     ! on desalloue les tableaux servant au stockage des listes d'elements adjacents a chaque noeuds, devenus
     ! inutiles
     deallocate(adj_ele_2_node, idx_adj_ele_2_node)

     !do ie=1, nbele
     !  print*, 'ele ', ie, ' adjacents'
     !  print*, adj_ele_2_ele(1:idx_adj_ele_2_ele(ie), ie)        
     !end do

     !!!! Affectation d'un numero d'entite a chaque composante connexe

     ! allocation memoire pour les tableaux intermediaires pour le parcours des elements adjacents
     allocate(aux1(nbele), aux2(nbele))  

     ! association d'un tableau alloue au pointeur
     !   * donnant la liste des elements a parcourir durant l'iteration courante
     to_search=>aux1
     !   * donnant la liste des elements a parcourir durant la prochaine iteration
     next_search=>aux2

     ! on indique qu'initialement aucune entite n'est definie pour aucune element
     ele2entity=0
     ! on initialise le numero d'entite a 0
     entity=0

     ! tant qu'il reste potentiellement des elements n'apparteant a aucune entite,
     ! on cherche le premier d'entre eux

     ! pour chaque element
     do ideb=1, nbele
        ! si l'element courant appartient deja a une entite, on passe au suivant
        if (ele2entity(ideb) /= 0) cycle

        ! ici, on sait qu'il reste des elements n'appartenant a aucune entite et ideb est
        ! le germe de la prochaine entite

        ! on passe au numero d'entite suivant
        entity = entity + 1
        ! on donne son numero d'entite au premier element de la nouvelle entite
        ele2entity(ideb) = entity

        ! on initialise a vide la liste des elements a parcourir durant l'iteration courante
        to_search=0
        ! on ajoute le premier element de la nouvelle entite a la liste des elements a 
        ! parcourir durant l'iteration courante
        to_search(1)=ideb

        ! tant qu'il reste des elements a parcourir durant l'iteration courante
        do 
           ! on initialise a vide la liste des elements a parcourir durant la prochaine
           ! iteration
           next_search=0
           ! on initialise a 0 l'inidce de la nouvelle cas disponible dans next_search 
           inext=0
           ! pour chaque element de la liste des elements a parcourir durant l'iteration 
           ! courante
           do is=1, nbele
              ! si on atteint la fin de la liste des elements a parcourir durant l'iteration
              ! courante, on sort de la boucle
              if (to_search(is) == 0) exit 
  
              ! on recupere le numero d'element de l'element courant de la liste des elements
              ! a parcourir durant l'iteration courante 
              ie = to_search(is)
              ! pour chaque element adjacent a l'element courant
              do iadj=1, idx_adj_ele_2_ele(ie)
                 ! si l'element adjacent courant appartient deja a une entite, on passe au 
                 ! suivant
                 if (ele2entity(adj_ele_2_ele(iadj, ie)) /= 0) cycle      
                 ! sinon, on incremente l'indice de la nouvelle case disponible dans next_search 
                 inext = inext + 1
                 ! on y stocke l'element adjacent courant, pour pouvoir le parcourir lors de la 
                 ! prochaine iteration
                 next_search(inext)=adj_ele_2_ele(iadj, ie)
                 ! on indique que l'element adajcent courant appartient a la meme entite que
                 ! l'element courant (et en cours de construction)
                 ele2entity(adj_ele_2_ele(iadj, ie))=entity
              end do
           end do
 
           !print*,inext
           !print*,next_search

           ! si la liste des elements a parcourir lors de la prochiane iteration est vide, alors
           ! tous les elements appartiennent a une entite et on peut quitter la boucle
           if (inext == 0) exit

           ! sinon, on echange les roles des listes to_search et next_search de sorte que
           ! la liste des elements a parcourir lors de la prochaine iteration, devienne la liste
           ! des elements a parourir lors de l'iteration courante, quand on sera passe a la prochaine 
           ! iteration 
           flipflap => to_search
           to_search => next_search
           next_search => flipflap 
        end do
     end do

     !print *, 'nb entities=', entity

     ! on deasalloue l'espace memoire occuppe par :
     !   * les liste d'adjacences
     deallocate(adj_ele_2_ele, idx_adj_ele_2_ele)
     !   * les tableaux utilises lors de l'affectation des entites
     deallocate(aux1, aux2)

  end subroutine identify_entities_surface_T3

  !> routine servant a calculer le volume contenue dans une surface triangulee 
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> adjac liste des ele adjacents a un ele , si 0 c'est un bord
  subroutine compute_volume_surface_T3(nbnode,nbele,connec,coor,normal,vol)
     implicit none

     integer,intent(in) :: nbnode,nbele
     integer,intent(in) :: connec(3,nbele)

     real(kind=8),intent(in) :: normal(3,nbele),coor(3,nbnode)
     real(kind=8) :: vol

     !*****
     integer :: ie,ic

     logical :: bavard=.false.
                            !1234567890123456789012345678901234567890
     character(len=40)::IAM='DiscreteGeometry::compute_volume_face_T3'

     real(kind=8) :: X(3,3), value

     integer :: ig
     real(kind=8),pointer :: gp_coor(:,:)
     real(kind=8),allocatable :: gp_field(:)

     vol=0.d0

     do ie=1,nbele

       if (normal(1,ie) /= 0.d0) then

         ! recuperation des coordonnees du triangle courant
         X(:, 1) = coor(:, connec(1, ie))
         X(:, 2) = coor(:, connec(2, ie))
         X(:, 3) = coor(:, connec(3, ie))

         !stored_normal = normal(:, ie)

         nullify(gp_coor)
         call get_gp_coor(3, i_T_P1, i_TR01, 3, X, gp_coor)

         allocate(gp_field(size(gp_coor,dim=2)))
         do ig=1,size(gp_coor,dim=2)
           gp_field(ig) = f_vol(gp_coor(:,ig),normal(:, ie))
         enddo

         call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR01,3,.FALSE., X, value)

         ! ajout au volume
         vol = vol + value

         deallocate(gp_coor,gp_field)
       endif

     enddo

  end subroutine

  !> routine servant a calculer les coordonnees du centre d'inertie d'un objet delimite par une surface triangulee 
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> coor coordonnees des noeuds
  !> normal normale pour chaque element
  !> x_G coordonnees du centre d'inertie
  subroutine compute_mass_center_surface_T3(nbnode, nbele, connec, coor, normal, vol, x_G)
     implicit none

     ! variables d'entree
     integer, intent(in) :: nbnode, nbele
     integer, intent(in) :: connec(3, nbele)
     real(kind=8), intent(in) :: normal(3, nbele), coor(3, nbnode)
     real(kind=8), intent(in) :: vol ! volume de l'objet

     ! variable de sortie
     real(kind=8), dimension(3), intent(out) :: x_G ! coordonnees du centre d'inertie

     ! variables locales
     integer :: ie
                              !1234567890123456789012345678901234567890
     character(len=40) :: IAM='DiscreteGeometry::compute_mass_center_face_T3'

     ! coordonnees des sommets l'element courant, dans le plan de l'element courant, dont l'origine est 
     ! le premier sommet de l'element courant
     real(kind=8) :: X(3, 3) 

     ! pour stocker la contribution de l'element courant a chaque coordonnee        
     ! du centre d'inertie
     real(kind=8), dimension(3) :: value 
        

     integer :: ig
     real(kind=8),pointer :: gp_coor(:,:)
     real(kind=8),allocatable :: gp_field(:)

     ! on initialise la position du centre d'inertie a 0
     x_G=0.d0

     ! pour chaque element
     do ie=1,nbele

        ! recuperation des coordonnees du triangle courant
        X(:, 1) = coor(:, connec(1, ie))
        X(:, 2) = coor(:, connec(2, ie))
        X(:, 3) = coor(:, connec(3, ie))

        !! stockage de la normale a l'element courant (pour f_xG, f_yG et f_zG)
        !stored_normal = normal(:, ie)


        nullify(gp_coor)
        call get_gp_coor(3, i_T_P1, i_TR03, 3, X, gp_coor)

        allocate(gp_field(size(gp_coor,dim=2)))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_xG(gp_coor(:,ig),normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR03, 3, .FALSE., X, value(1))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_yG(gp_coor(:,ig),normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR03, 3, .FALSE., X, value(2))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_zG(gp_coor(:,ig),normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR03, 3, .FALSE., X, value(3))

        ! ajout de la contribution de l'element courant au coordonees du centre d'inertie
        x_G(:) = x_G(:) + value(:)

        deallocate(gp_coor,gp_field)
     end do
    
     ! on divise les coordonnees obtenues par le volume de l'objet, pour obtenir les coordonnees du centre d'inertie
     x_G = x_G/vol

  end subroutine compute_mass_center_surface_T3

  !> routine servant a calculer l'inertie d'un objet delimite par une surface triangulee 
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> coor coordonnees des noeuds
  !> normal normale pour chaque element
  !> I matrice d'inertie
  subroutine compute_inertia_surface_T3(nbnode, nbele, connec, coor, normal, x_G, I)
     implicit none

     ! variables d'entree
     integer, intent(in) :: nbnode, nbele
     integer, intent(in) :: connec(3, nbele)
     real(kind=8), intent(in):: normal(3, nbele), coor(3, nbnode)
     real(kind=8), dimension(3), intent(in) :: x_G ! coordonnees du centre d'inertie

     ! variable de sortie
     real(kind=8), dimension(3, 3), intent(out) :: I ! matrice d'inertie stockee sous la forme d'une matrice pleine

     ! variables locales
     integer :: ie
                              !1234567890123456789012345678901234567890
     character(len=40) :: IAM='DiscreteGeometry::compute_inertie_face_T3'

     real(kind=8) :: X(3, 3) ! coordonnees des sommets l'element courant, dans le plan de l'element courant, dont l'otigine est 
        ! le premier sommet de l'element courant
     !real(kind=8) :: dsurf ! surface de l'element courant
     real(kind=8), dimension(6) :: I_vect ! matrice d'inertie stockee sous la forme d'un vecteur (notations de voigt)
     real(kind=8), dimension(6) :: value ! pour stocker la contribution de l'element courant a chaque composante de la
     integer :: ig
     real(kind=8),pointer :: gp_coor(:,:)
     real(kind=8),allocatable :: gp_field(:)

     !! stockage du centre d'inertie de l'objet (pour f_I11, fI22, f_I33, f_I32, f_I31 et f_I21) 
     !stored_xG = x_G

     ! on initialise la matrice d'inertie a 0
     I_vect=0.d0

     ! pour chaque element
     do ie=1,nbele

        ! recuperation des coordonnees du triangle courant
        X(:, 1) = coor(:, connec(1, ie))
        X(:, 2) = coor(:, connec(2, ie))
        X(:, 3) = coor(:, connec(3, ie))

        !! stockage de la normale a l'element courant (pour f_I11, fI22, f_I33, f_I32, f_I31 et f_I21)
        !stored_normal = normal(:, ie)

        nullify(gp_coor)
        call get_gp_coor(3, i_T_P1, i_TR04, 3, X, gp_coor)

        allocate(gp_field(size(gp_coor,dim=2)))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I11(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR04, 3, .FALSE., X, value(1))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I22(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR04, 3, .FALSE., X, value(2))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I33(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR04, 3, .FALSE., X, value(3))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I32(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR04, 3, .FALSE., X, value(4))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I31(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR04, 3, .FALSE., X, value(5))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I21(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 3, i_T_P1, i_TR04, 3, .FALSE., X, value(6))

        ! ajout de la contribution de l'element courant a la matrice d'inertie
        I_vect(:) = I_vect(:) + value(:)

        deallocate(gp_coor,gp_field)

     end do
    
     ! on stocke la matrice d'inertie sous la forme d'une matrice pleine
     I(1, 1) = I_vect(1)
     I(2, 2) = I_vect(2)
     I(3, 3) = I_vect(3)
     I(3, 2) = I_vect(4) ; I(2, 3) = I_vect(4)
     I(3, 1) = I_vect(5) ; I(1, 3) = I_vect(5)
     I(2, 1) = I_vect(6) ; I(1, 2) = I_vect(6)

  end subroutine compute_inertia_surface_T3

  !am : fonction qui calcule le volume, le centre d'inertie eti, eventuellement, la matrice d'inertie dans le repere global 
  subroutine compute_volume_inertia_global_frame_surface_T3(nbnode, nbele, connec, coor, comp_inertia, vol, x_G, I, err)
     implicit none

     ! variables d'entree
     ! nombre de noeuds
     integer, intent(in)                        :: nbnode
     ! nombre d'elements
     integer, intent(in)                        :: nbele
     ! coordonnees des noeuds
     real(kind=8), intent(in)                   :: coor(3, nbnode)
     ! vaut "vrai" ssi on doit calculer la matrice d'inertie     
     logical, intent(in)                        :: comp_inertia 

     ! variables d'entree-sortie
     ! connectivites des elements
     integer, intent(inout)                     :: connec(3, nbele) 

     ! variables de sortie
     ! volume de l'objet
     real(kind=8), intent(out)                  :: vol
     ! centre d inertie
     real(kind=8), dimension(3), intent(out)    :: x_G
     ! matrice d'inertie stockee sous la forme d'une matrice pleine
     ! N.B.: si comp_ineria vaut "faux", I est nulle
     real(kind=8), dimension(3, 3), intent(out) :: I
     
     integer                                    :: err,err_
     character(len=120)                         :: cout

     ! variablees locales
     real(kind=8) :: normal(3, nbele) ! normales aux elements
     real(kind=8) :: triangle(3,3)    ! pour stocker les coords de l'element courant

     integer :: swap ! pour stocker un numero de noeud pendant qu'on retourne la connectivite d'un element
     integer :: ie ! indice de boucle sur les elements

     err = 0
     
     !fd on met toutes les faces dans le meme sens
     call set_orientation_surface_T3(nbnode, nbele, connec,err_)
     
     if ( err_ > 0) then
       call logmes('Error in DiscreteGeometry::compute_volume_inertia_global_frame_surface_T3',.true.) 
       err = 1
       return 
     else if ( err_ < 0) then
       call logmes('Warning in DiscreteGeometry::compute_volume_inertia_global_frame_surface_T3',.true.) 
     endif   

     !fd calcul des normales
     do ie=1, nbele
       triangle(:,1) = coor(:, connec(1,ie))
       triangle(:,2) = coor(:, connec(2,ie))
       triangle(:,3) = coor(:, connec(3,ie))
       err_ = compute_info_triangle(triangle, normal=normal(:,ie)) 
       if (err_ == 1)  then
          err = 2
          write(cout,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          call logmes(cout,.true.)
          write(cout,*) "--> triangle degenerated: 3 merged vertices"
          call logmes(cout,.true.)
          write(cout,*) "    Element number  : ", ie
          call logmes(cout,.true.)
       else if (err_==2) then
          err = 3
          write(cout,*) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          call logmes(cout,.true.)
          write(cout,*) "--> triangle degenerated: 3 aligned vertices"
          call logmes(cout,.true.)
          write(cout,*) "    Element number  : ", ie
          call logmes(cout,.true.)
       endif
     end do

     !print*,'vol'

     !fd on calcule le volume
     call compute_volume_surface_T3(nbnode,nbele,connec,coor,normal,vol)

     !fd si le volume est negatif on retourne tout
     if (vol < 0.d0) then
       !call logmes('Warning in DiscreteGeometry::compute_volume_inertia_global_frame_surface_T3 : surface orientation was swapped',.true.)  
       vol = dabs(vol)
       normal= -normal
       do ie=1, nbele
          swap=connec(2, ie)          
          connec(2, ie)=connec(3, ie)                    
          connec(3, ie)=swap                     
       enddo
     endif

     !print*,'xg'

     !fd calcul du centre d'inertie
     call compute_mass_center_surface_T3(nbnode, nbele, connec, coor, normal, vol, x_G)


     !print*,'inertia'

     ! si on a demande le calcul de l'inertie
     if (comp_inertia) then
        ! on la calcule
        !fd calcul de la matrice d'inertie
        call compute_inertia_surface_T3(nbnode, nbele, connec, coor, normal, x_G, I)
     ! sinon,
     else
        ! on renvoie une matrice nulle
        I = 0.d0
     endif

     !print*,'ok'

  end subroutine compute_volume_inertia_global_frame_surface_T3

  subroutine compute_mechanical_properties_surface_T3(nbnode, nbele, connec, coor, vol, x_G, I1,I2,I3, localframe,err)
     implicit none

     integer, intent(in)                       :: nbnode,nbele
     integer, intent(inout)                    :: connec(3,nbele)
     real(kind=8), intent(inout)               :: coor(3,nbnode)  
     ! volume de l'objet
     real(kind=8), intent(out)                 :: vol        
     ! centre d inertie
     real(kind=8), dimension(3), intent(out)   :: x_G        
     ! inerties principales
     real(kind=8), intent(inout)               :: I1,I2,I3   
     ! repere principal d inertie
     real(kind=8), dimension(3,3), intent(out) :: localframe 
     ! 
     integer                                   :: err,err_
     
     !***
     ! vaut "vrai" ssi on a besoin de recalculer l'inertie
     logical :: comp_inertia 
     real(kind=8) :: tmp(3)
     ! matrice d'inertie stockee sous la forme d'une matrice pleine
     real(kind=8), dimension(3, 3) :: I 

     integer :: in,ie

     err = 0
     
     ! on doit recalculer la matrice d'inertie ssi les inerties principales sont toutes nulles
     comp_inertia = I1 == 0.D0 .and. I2 == 0.D0 .and. I3 == 0.D0

     ! on calcule le volume, le centre d'inertie et eventuellement la matrice d'inertie dans le repere global
     call compute_volume_inertia_global_frame_surface_T3(nbnode, nbele, connec, coor, comp_inertia, vol, x_G, I,err_)

     if (err_ > 0) then
       call logmes('Error in DiscreteGeomeytry::compute_mechanical_properties_surface_T3',.true.)
       err= 1
       return
     endif


     if (vol < 0.d0) then
       call logmes('Error in DiscreteGeomeytry::compute_mechanical_properties_surface_T3 bad orientation of the mesh',.true.)
       err= 1
       return
     endif
    
     ! si on a recalcule la matrice d'inertie dans le repere global
     if (comp_inertia) then
       ! on la diagonalise pour calculer les inerties principales et le repere principal d'inertie

       !fd diagonalisation => inertie principales et repere principale d'inertie

       call diagonalise33(I, tmp, localframe)
  
       I1  = tmp(1)
       I2  = tmp(2)
       I3  = tmp(3)
     ! sinon,
     else
       ! on initialise le repere principal d'inertie en utilisant le repere global
       localframe=0.d0
       localframe(1,1)=1.d0
       localframe(2,2)=1.d0
       localframe(3,3)=1.d0

     endif

     !fd on calcule les coordonnees par rapport au centre d'inertie tjs dans le rep global
     DO in=1,nbnode
       coor(:,in) = coor(:,in) - x_G(:) 
     END DO

     !fd on ecrit les coordonnees dans le repere principale d'inertie
     DO in=1,nbnode
       tmp(:)=coor(:,in)     
       coor(1,in) = DOT_PRODUCT(localframe(1:3,1),tmp(1:3))
       coor(2,in) = DOT_PRODUCT(localframe(1:3,2),tmp(1:3))
       coor(3,in) = DOT_PRODUCT(localframe(1:3,3),tmp(1:3))
     END DO

  end subroutine

  !fd %<----

  !> routine servant a calculer le volume contenue dans une surface fermee composee de Q4 
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> David Eberly, Geometric Tools, Redmond WA 98052
  !> https://www.geometrictools.com/Documentation/PolyhedralMassProperties.pdf
  subroutine compute_volume_surface_Q4(nbnode,nbele,connec,coor,normal,vol)
     implicit none

     integer,intent(in) :: nbnode,nbele
     integer,intent(in) :: connec(4,nbele)

     real(kind=8),intent(in) :: normal(3,nbele),coor(3,nbnode)
     real(kind=8) :: vol

     !*****
     integer :: ie,ic

     logical :: bavard=.false.
                            !1234567890123456789012345678901234567890
     character(len=40)::IAM='DiscreteGeometry::compute_volume_face_Q4'

     real(kind=8) :: X(3,4), value

     integer :: ig
     real(kind=8),pointer :: gp_coor(:,:)
     real(kind=8),allocatable :: gp_field(:)

     vol=0.d0

     do ie=1,nbele

       if (normal(1,ie) /= 0.d0) then

         ! recuperation des coordonnees du triangle courant
         X(:, 1) = coor(:, connec(1, ie))
         X(:, 2) = coor(:, connec(2, ie))
         X(:, 3) = coor(:, connec(3, ie))
         X(:, 4) = coor(:, connec(4, ie))

         !! stockage de la normale a l'element courant (pour f_vol)
         !stored_normal = normal(:, ie)

         nullify(gp_coor)
         call get_gp_coor(4, i_Q_P1, i_Q2x2, 3, X, gp_coor)

         allocate(gp_field(size(gp_coor,dim=2)))
         do ig=1,size(gp_coor,dim=2)
           gp_field(ig) = f_vol(gp_coor(:,ig),normal(:, ie))
         enddo

         call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value)

         ! ajout au volume
         vol = vol + value

         deallocate(gp_coor,gp_field)
       endif

     enddo

  end subroutine

  !> routine servant a calculer les coordonnees du centre d'inertie d'un objet delimite par une surface triangulee 
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> coor coordonnees des noeuds
  !> normal normale pour chaque element
  !> x_G coordonnees du centre d'inertie
  subroutine compute_mass_center_surface_Q4(nbnode, nbele, connec, coor, normal, vol, x_G)
     implicit none

     ! variables d'entree
     integer, intent(in) :: nbnode, nbele
     integer, intent(in) :: connec(4, nbele)
     real(kind=8), intent(in) :: normal(3, nbele), coor(3, nbnode)
     real(kind=8), intent(in) :: vol ! volume de l'objet

     ! variable de sortie
     real(kind=8), dimension(3), intent(out) :: x_G ! coordonnees du centre d'inertie

     ! variables locales
     integer :: ie
                              !123456789012345678901234567890123456789012345
     character(len=45) :: IAM='DiscreteGeometry::compute_mass_center_face_Q4'

     real(kind=8) :: X(3, 4) ! coordonnees des sommets l'element courant, dans le plan de l'element courant, dont l'origine est 
        ! le premier sommet de l'element courant
     real(kind=8), dimension(3) :: value ! pour stocker la contribution de l'element courant a chaque coordonnee
        ! du centre d'inertie

     integer :: ig
     real(kind=8),pointer :: gp_coor(:,:)
     real(kind=8),allocatable :: gp_field(:)

     ! on initialise la position du centre d'inertie a 0
     x_G=0.d0

     ! pour chaque element
     do ie=1,nbele

        ! recuperation des coordonnees du triangle courant
        X(:, 1) = coor(:, connec(1, ie))
        X(:, 2) = coor(:, connec(2, ie))
        X(:, 3) = coor(:, connec(3, ie))
        X(:, 4) = coor(:, connec(4, ie))

        !! stockage de la normale a l'element courant (pour f_xG, f_yG et f_zG)
        !stored_normal = normal(:, ie)

        nullify(gp_coor)
        call get_gp_coor(4, i_Q_P1, i_Q2x2, 3, X, gp_coor)

        allocate(gp_field(size(gp_coor,dim=2)))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_xG(gp_coor(:,ig),normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(1))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_yG(gp_coor(:,ig),normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(2))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_zG(gp_coor(:,ig),normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(3))

        ! ajout de la contribution de l'element courant au coordonees du centre d'inertie
        x_G(:) = x_G(:) + value(:)

        deallocate(gp_coor,gp_field)
     end do
    
     ! on divise les coordonnees obtenues par le volume de l'objet, pour obtenir les coordonnees du centre d'inertie
     x_G = x_G/vol

  end subroutine compute_mass_center_surface_Q4

  !> routine servant a calculer l'inertie d'un objet delimite par une surface triangulee 
  !> nbele le nb d'elements
  !> connec table de connectivite des ele
  !> coor coordonnees des noeuds
  !> normal normale pour chaque element
  !> I matrice d'inertie
  subroutine compute_inertia_surface_Q4(nbnode, nbele, connec, coor, normal, x_G, I)
     implicit none

     ! variables d'entree
     integer, intent(in) :: nbnode, nbele
     integer, intent(in) :: connec(4, nbele)
     real(kind=8), intent(in):: normal(3, nbele), coor(3, nbnode)
     real(kind=8), dimension(3), intent(in) :: x_G ! coordonnees du centre d'inertie

     ! variable de sortie
     real(kind=8), dimension(3, 3), intent(out) :: I ! matrice d'inertie stockee sous la forme d'une matrice pleine

     ! variables locales
     integer :: ie
                              !1234567890123456789012345678901234567890
     character(len=40) :: IAM='DiscreteGeometry::compute_inertie_face_Q4'

     real(kind=8) :: X(3, 4) ! coordonnees des sommets l'element courant, dans le plan de l'element courant, dont l'otigine est 
        ! le premier sommet de l'element courant
     !real(kind=8) :: dsurf ! surface de l'element courant
     real(kind=8), dimension(6) :: I_vect ! matrice d'inertie stockee sous la forme d'un vecteur (notations de voigt)
     real(kind=8), dimension(6) :: value ! pour stocker la contribution de l'element courant a chaque composante de la
     integer :: ig
     real(kind=8),pointer :: gp_coor(:,:)
     real(kind=8),allocatable :: gp_field(:)

     !! stockage du centre d'inertie de l'objet (pour f_I11, fI22, f_I33, f_I32, f_I31 et f_I21) 
     !stored_xG = x_G

     ! on initialise la matrice d'inertie a 0
     I_vect=0.d0

     ! pour chaque element
     do ie=1,nbele

        ! recuperation des coordonnees du triangle courant
        X(:, 1) = coor(:, connec(1, ie))
        X(:, 2) = coor(:, connec(2, ie))
        X(:, 3) = coor(:, connec(3, ie))
        X(:, 4) = coor(:, connec(4, ie))

        !! stockage de la normale a l'element courant (pour f_I11, fI22, f_I33, f_I32, f_I31 et f_I21)
        !stored_normal = normal(:, ie)

        nullify(gp_coor)
        call get_gp_coor(4, i_Q_P1, i_Q2x2, 3, X, gp_coor)

        allocate(gp_field(size(gp_coor,dim=2)))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I11(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(1))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I22(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(2))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I33(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(3))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I32(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(4))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I31(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(5))

        do ig=1,size(gp_coor,dim=2)
          gp_field(ig) = f_I21(gp_coor(:,ig),x_G,normal(:, ie))
        enddo
        call INTEGRATE_field(gp_field, 4, i_Q_P1, i_Q2x2, 3, .FALSE., X, value(6))

        ! ajout de la contribution de l'element courant a la matrice d'inertie
        I_vect(:) = I_vect(:) + value(:)

        deallocate(gp_coor,gp_field)

     end do
    
     ! on stocke la matrice d'inertie sous la forme d'une matrice pleine
     I(1, 1) = I_vect(1)
     I(2, 2) = I_vect(2)
     I(3, 3) = I_vect(3)
     I(3, 2) = I_vect(4) ; I(2, 3) = I_vect(4)
     I(3, 1) = I_vect(5) ; I(1, 3) = I_vect(5)
     I(2, 1) = I_vect(6) ; I(1, 2) = I_vect(6)

  end subroutine compute_inertia_surface_Q4

  !am : fonction qui calcule le volume, le centre d'inertie et, eventuellement, la matrice d'inertie dans le repere global 
  subroutine compute_volume_inertia_global_frame_surface_Q4(nbnode, nbele, connec, coor, comp_inertia, vol, x_G, I, err)
     implicit none

     ! variables d'entree
     ! nombre de noeuds
     integer, intent(in)                        :: nbnode
     ! nombre d'elements
     integer, intent(in)                        :: nbele
     ! coordonnees des noeuds
     real(kind=8), intent(in)                   :: coor(3, nbnode)
     ! vaut "vrai" ssi on doit calculer la matrice d'inertie
     logical, intent(in)                        :: comp_inertia 

     ! variables d'entree-sortie
     ! connectivites des elements
     integer, intent(inout)                     :: connec(4, nbele) 

     ! variables de sortie
     ! volume de l'objet
     real(kind=8), intent(out)                  :: vol
     ! centre d inertie
     real(kind=8), dimension(3), intent(out)    :: x_G
     ! matrice d'inertie stockee sous la forme d'une matrice pleine
     ! N.B.: si comp_ineria vaut "faux", I est nulle
     real(kind=8), dimension(3, 3), intent(out) :: I

     integer                                    :: err
                                                     

     ! variablees locales
     real(kind=8) :: normal(3, nbele) ! normales aux elements
     real(kind=8) :: norm ! pour stocker la norme du vecteur 12^13, pour l'element courant

     integer :: swap ! pour stocker un numero de noeud pendant qu'on retourne la connectivite d'un element
     integer :: ie ! indice de boucle sur les elements

     character(len=64) :: IAM
           !1234567890123456789012345678901234567890123456789012345678901234
     IAM = 'DiscreteGeometry::compute_volume_inertia_global_frame_surface_Q4'

     err = 0

     ! on ne fait pas ca  
     !fd on met toutes les faces dans le meme sens
     !call set_orientation_surface_T3(nbnode, nbele, connec) 

     !fd calcul des normales
     do ie=1, nbele
       normal(:, ie)=cross_product(coor(:, connec(2,ie)) - coor(:, connec(1,ie)), &
                                   coor(:, connec(4,ie)) - coor(:, connec(1,ie)))
       norm = length3(normal(1:3,ie))
       if (norm == 0.d0) then
          call logmes('Error in '//IAM//' : normal length is equal to zero',.true.)
          err = 1
          return
       endif 
       normal(:, ie) = normal(:, ie)/norm
     end do

     !print*,'vol'

     !fd on calcule le volume
     call compute_volume_surface_Q4(nbnode,nbele,connec,coor,normal,vol)

     !if (vol < 0.d0) then
     !  !fd si le volume est negatif on retourne tout
     !  vol = dabs(vol)
     !  normal= -normal
     !  do ie=1, nbele
     !     swap=connec(2, ie)          
     !     connec(2, ie)=connec(3, ie)                    
     !     connec(3, ie)=swap                     
     !  enddo
     !endif

     !fd calcul du centre d'inertie
     call compute_mass_center_surface_Q4(nbnode, nbele, connec, coor, normal, vol, x_G)

     !print*,'inertia'

     ! si on a demande le calcul de l'inertie
     if (comp_inertia) then
        ! on la calcule
        !fd calcul de la matrice d'inertie
        call compute_inertia_surface_Q4(nbnode, nbele, connec, coor, normal, x_G, I)
     ! sinon,
     else
        ! on renvoie une matrice nulle
        I = 0.d0
     endif

     !print*,'ok'

  end subroutine compute_volume_inertia_global_frame_surface_Q4



  !fd ---->%


  subroutine compute_vol_tetrahedron(p1,p2,p3,p4,vol)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: p1,p2,p3,p4,c1,c2,c3
    real(kind=8),parameter    :: un_6=1.d0/6.d0
    real(kind=8)              :: vol
    integer                   :: err

    err = 0
    
    c1 = p2 - p1
    c2 = p3 - p1
    c3 = p4 - p1

    vol = determinant(c1,c2,c3)*un_6

  end subroutine

  !> sortie de tonon (attention b' et c' sont permutes dans le papier)
  SUBROUTINE compute_inertia_tetrahedron(p1,p2,p3,p4,I0)

    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3)   :: p1,p2,p3,p4
    REAL(kind=8),DIMENSION(3,3) :: I0
    REAL(kind=8)                :: x1,y1,z1
    REAL(kind=8)                :: x2,y2,z2
    REAL(kind=8)                :: x3,y3,z3
    REAL(kind=8)                :: x4,y4,z4
    REAL(kind=8)                :: detJ
    REAL(kind=8),parameter      :: inv60 = 1.d0/60.d0,inv120 = 1.d0/120.d0


    detj = dabs(jacobien(p1,p2,p3,p4))

    x1 = p1(1) ; y1 = p1(2) ; z1 = p1(3)
    x2 = p2(1) ; y2 = p2(2) ; z2 = p2(3)
    x3 = p3(1) ; y3 = p3(2) ; z3 = p3(3)
    x4 = p4(1) ; y4 = p4(2) ; z4 = p4(3)

    ! xx
    I0(1,1) = detJ*inv60* &
         (y1*y1 + y1*y2 + y2*y2 + y1*y3 + y2*y3 + &     
          y3*y3 + y1*y4 + y2*y4 + y3*y4 + y4*y4 + &
          z1*z1 + z1*z2 + z2*z2 + z1*z3 + z2*z3 + &
          z3*z3 + z1*z4 + z2*z4 + z3*z4 + z4*z4)
    ! yy
    I0(2,2) = detJ*inv60* &
         (x1*x1 + x1*x2 + x2*x2 + x1*x3 + x2*x3 + &     
          x3*x3 + x1*x4 + x2*x4 + x3*x4 + x4*x4 + &
          z1*z1 + z1*z2 + z2*z2 + z1*z3 + z2*z3 + &
          z3*z3 + z1*z4 + z2*z4 + z3*z4 + z4*z4)
    ! zz
    I0(3,3) = detJ*inv60* &
         (x1*x1 + x1*x2 + x2*x2 + x1*x3 + x2*x3 + &     
          x3*x3 + x1*x4 + x2*x4 + x3*x4 + x4*x4 + &
          y1*y1 + y1*y2 + y2*y2 + y1*y3 + y2*y3 + &     
          y3*y3 + y1*y4 + y2*y4 + y3*y4 + y4*y4)
          
    ! yz

    I0(2,3)=  -detJ*inv120* &
          (2.d0*y1*z1 +      y2*z1 +      y3*z1 +      y4*z1 + & 
                y1*z2 + 2.d0*y2*z2 +      y3*z2 +      y4*z2 + & 
                y1*z3 +      y2*z3 + 2.d0*y3*z3 +      y4*z3 + &
                y1*z4 +      y2*z4 +      y3*z4 + 2.d0*y4*z4)

    I0(3,2) = I0(2,3)

    ! xz

    I0(1,3)=  -detJ*inv120* &
          (2.d0*x1*z1 +      x2*z1 +      x3*z1 +      x4*z1 + & 
                x1*z2 + 2.d0*x2*z2 +      x3*z2 +      x4*z2 + & 
                x1*z3 +      x2*z3 + 2.d0*x3*z3 +      x4*z3 + &
                x1*z4 +      x2*z4 +      x3*z4 + 2.d0*x4*z4)

    I0(3,1) = I0(1,3)

    ! xy

    I0(1,2) =  -detJ*inv120* &
          (2.d0*x1*y1 +      x2*y1 +      x3*y1 +      x4*y1 + & 
                x1*y2 + 2.d0*x2*y2 +      x3*y2 +      x4*y2 + & 
                x1*y3 +      x2*y3 + 2.d0*x3*y3 +      x4*y3 + &
                x1*y4 +      x2*y4 +      x3*y4 + 2.d0*x4*y4)

    I0(2,1)=I0(1,2)

  END SUBROUTINE

  REAL(kind=8) FUNCTION jacobien(p1,p2,p3,p4)

    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3)   :: p1,p2,p3,p4,c1,c2,c3

    c1 = p2 - p1
    c2 = p3 - p1
    c3 = p4 - p1

    jacobien = determinant(c1,c2,c3)

  END FUNCTION

  !> calcul des infos concernant un triangle ; 0 si ok, 1 si points confondus, 2 si points alignes
  integer function compute_info_triangle(s,surface,inner_radius,outer_radius,normal) 
    implicit none
    real(kind=8), dimension(:,:) :: s  ! taille (dim,3)
    real(kind=8), optional ::  surface,inner_radius,outer_radius,normal(3)
    ! ***
    real(kind=8),dimension(3) :: l,c
    real(kind=8):: hp,surf
    real(kind=8),dimension(3) :: v1,v2,v3
    integer :: dim
    real(kind=8)::tol

    tol=1d-12

    ! a priori, pas de probleme
    compute_info_triangle = 0

    ! on recupere la dimension du pb
    dim = size(s,dim=1)

    ! calcul longueur des cotes
    v1=0.d0
    v1(1:dim) = s(1:dim,2)-s(1:dim,3)
    l(1) = length3(v1)

    v2=0.d0
    v2(1:dim) = s(1:dim,1)-s(1:dim,3)
    l(2) = length3(v2)

    v3=0.d0
    v3(1:dim) = s(1:dim,2)-s(1:dim,1)
    l(3) = length3(v3)

    !demi perimetre 
    hp = 0.5d0*(l(1)+l(2)+l(3))

    ! formule de Heron
    surf = sqrt(hp*(hp-l(1))*(hp-l(2))*(hp-l(3)))

    ! 3 merged vertices
    if (hp < tol .and. surf < tol*tol)  then
       compute_info_triangle = 1
       if (present(surface))      surface      = 0.d0
       if (present(inner_radius)) inner_radius = 0.d0
       if (present(outer_radius)) outer_radius = 0.d0
       if (present(normal))       normal(:)    = 0.00
       
    ! 3 aligned vertices
    else if ( hp > tol .and. surf < tol*tol) then
       compute_info_triangle = 2
       if (present(surface))      surface      = 0.d0
       if (present(inner_radius)) inner_radius = 0.d0
       if (present(outer_radius)) outer_radius = hp
       if (present(normal))       normal(:)    = 0.00
       
    ! normal triangle 
    else
       if (present(surface))      surface      = surf
       if (present(inner_radius)) inner_radius = surf/hp
       if (present(outer_radius)) then
           c(1) = DOT_PRODUCT(-v2, v3)
           c(2) = DOT_PRODUCT(-v3,-v1)
           c(3) = DOT_PRODUCT( v1, v2)
           ! triangle obtusangle
           if (MINVAL(c) < 0D0) then
              outer_radius = 0.5*MAXVAL(l)
           ! triangle acutangle
           else
              outer_radius = l(1)*l(2)*l(3)/(4*surf)
           endif
       endif
       if (present(normal)) then
          if (dim == 2) then
            normal = (/ 0.d0, 0.d0, 1.d0 /) 
          else
            normal = cross_product(v1,v3)
            normal = normal / length3(normal)
          endif      
       endif   
    endif

  end function

!  real(kind=8) function identity(coor)
!    implicit none
!    real(kind=8) :: coor(:)
!
!    identity = 1.d0
!    return
!  end function 

!  real(kind=8) function my_func(coor)
!     implicit none
!
!     real(kind=8), intent(in) :: coor(:)
!   
!     !my_func=coor(1)*coor(1)*coor(1)
!     my_func=coor(1)
!     return
!
!  end function my_func

  ! 
  subroutine compute_node_reduced_coor_in_linear_quadrangle(coorn,coorq4,weight,err)
    implicit none
    real(kind=8) :: coorn(3),coorq4(3,4),weight(4)
    integer      :: err,err_
    !***
    real(kind=8) :: frame(3,3),norm,coorn_loc(2),coorq4_loc(2,4),cooref(2),tmp(3)

    real(kind=8), dimension(:), pointer :: N   
          
    character(len=108) :: cout
    character(len=64)  :: IAM
          !1234567890123456789012345678901234567890123456789012345678901234
    IAM = 'DiscreteGeometry::compute_node_reduced_coor_in_linear_quadrangle'


    err = 0
    
    !print*,'coordonnees globales'
    !write(*,'(3(1x,D12.5))') coorn
    !write(*,'(3(1x,D12.5))') coorq4

    frame(:,1) = coorq4(:,2) - coorq4(:,1)
    norm = length3(frame(1:3,1))
    if (norm < 1d-10) then
      write(cout,'(A)') 'la longueur de l axe 1 est trop petite'
      call logmes(cout,.true.) 
      write(cout,'(D14.7)') norm
      call logmes(cout,.true.)       
      write(cout,'(3(D14.7))') coorq4(:,2)
      call logmes(cout,.true.)       
      write(cout,'(3(D14.7))') coorq4(:,1)
      call logmes(cout,.true.) 
      call logmes('Error in'//IAM)
      err = 1
      return
    endif 
    frame(:,1) = frame(:,1)/norm

    frame(:,3)=cross_product(frame(:,1), &
                            coorq4(:,4) - coorq4(:,1))
    norm = length3(frame(1:3,3))
    if (norm < 1d-10) then
      write(cout,'(A)') 'la longueur de l axe 2 est trop petite'
      call logmes(cout,.true.)  
      write(cout,'(D14.7)') norm
      call logmes(cout,.true.)
      write(cout,'(3(D14.7))') coorq4(:,4)
      call logmes(cout,.true.)
      write(cout,'(3(D14.7))') coorq4(:,1)
      call logmes(cout,.true.) 
      call logmes('Error in'//IAM,.true.)
      err= 1
      return
    endif 
    frame(:,3) = frame(:,3)/norm

    frame(:,2)=cross_product(frame(:,3),frame(:,1))

    !tous les pts sont dans le plan de la face donc on se vire la partie normale
    !on exprime tout dans le repere positionne en coorq4(:,1)
    coorq4_loc(:,1) = 0.d0

    tmp=coorq4(:,2)-coorq4(:,1)     
    coorq4_loc(:,2) = (/ dot_product(tmp,frame(:,1)), 0.d0 /)  

    tmp=coorq4(:,3)-coorq4(:,1)     
    coorq4_loc(:,3) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  

    tmp=coorq4(:,4)-coorq4(:,1)     
    coorq4_loc(:,4) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  

    tmp = coorn - coorq4(:,1)
    !au cas ou on vire une composante normale
    tmp = tmp - (dot_product(tmp,frame(:,3))*frame(:,3))

    coorn_loc =  (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  

    !print*,'coordonnees locales'
    !write(*,'(2(1x,D12.5))') coorn_loc
    !write(*,'(2(1x,D12.5))') coorq4_loc

    call compute_coor_ref_ele(i_Q_P1,coorq4_loc,4,2,coorn_loc,10,1d-10,(/0.d0, 0.d0/),cooref,err_)

    if (err_ >0) then
      call logmes('Error in '//IAM//' : unexpected result in compute_coor_ref !',.true.)
      err = 1
      return
    endif
   
    !print*,'parametrage '
    !write(*,'(2(1x,D12.5))') cooref

    nullify(N)
    call FONCT_FORME(i_Q_P1, cooref, N)
    weight=N
    deallocate(N)

    !print*,'poids q4'
    !print*,weight

  end subroutine

  ! DA : Ajout pour les antagoniste quadrangle 8 noeuds ou quadratiques
  subroutine compute_node_reduced_coor_in_quadratic_quadrangle(coorn,coorq8,weight,err)
    implicit none
    real(kind=8) :: coorn(3),coorq8(3,8),weight(8)
    integer      :: err,err_
    !***
    real(kind=8) :: frame(3,3),norm,coorn_loc(2),coorq8_loc(2,8),cooref(2),tmp(3)

    real(kind=8), dimension(:), pointer :: N   
          
    character(len=108) :: cout
    character(len=67)  :: IAM
          !1234567890123456789012345678901234567890123456789012345678901234567
    IAM = 'DiscreteGeometry::compute_node_reduced_coor_in_quadratic_quadrangle'

    err = 0
    
    !print*,'coordonnees globales'
    !write(*,'(3(1x,D12.5))') coorn
    !write(*,'(3(1x,D12.5))') coorq4

    frame(:,1) = coorq8(:,2) - coorq8(:,1)
    norm = length3(frame(1:3,1))
    if (norm < 1d-10) then
      write(cout,'(A)') 'la longueur de l axe 1 est trop petite'
      call logmes(cout,.true.) 
      write(cout,'(D14.7)') norm
      call logmes(cout,.true.) 
      write(cout,'(3(D14.7))') coorq8(:,2)
      call logmes(cout,.true.) 
      write(cout,'(3(D14.7))') coorq8(:,1)
      call logmes(cout,.true.) 
      call logmes('Error in'//IAM,.true.)
      err = 1
      return
    endif 
    frame(:,1) = frame(:,1)/norm

    frame(:,3)=cross_product(frame(:,1), &
                            coorq8(:,4) - coorq8(:,1))
    norm = length3(frame(1:3,3))
    if (norm < 1d-10) then
      write(cout,'(A)') 'la longueur de l axe 2 est trop petite'
      call logmes(cout,.true.) 
      write(cout,'(D14.7)') norm
      call logmes(cout,.true.) 
      write(cout,'(3(D14.7))') coorq8(:,4)
      call logmes(cout,.true.) 
      write(cout,'(3(D14.7))') coorq8(:,1)
      call logmes(cout,.true.) 
      call logmes('Error in '//IAM,.true.)
      err = 1
      return
    endif
    frame(:,3) = frame(:,3)/norm

    frame(:,2)=cross_product(frame(:,3),frame(:,1))

    !print*,'s ',frame(:,1)
    !print*,'t ',frame(:,2)
    !print*,'n ',frame(:,3)
    
    !tous les pts sont dans le plan de la face donc on se vire la partie normale
    !on exprime tout dans le repere positionne en coorq4(:,1)
    coorq8_loc(:,1) = 0.d0
    
    tmp=coorq8(:,2)-coorq8(:,1)     
    coorq8_loc(:,2) = (/ dot_product(tmp,frame(:,1)), 0.d0 /)  
    
    tmp=coorq8(:,3)-coorq8(:,1)     
    coorq8_loc(:,3) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  
    
    tmp=coorq8(:,4)-coorq8(:,1)     
    coorq8_loc(:,4) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  

    tmp=coorq8(:,5)-coorq8(:,1)     
    coorq8_loc(:,5) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  
    tmp=coorq8(:,6)-coorq8(:,1)     
    coorq8_loc(:,6) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  
    tmp=coorq8(:,7)-coorq8(:,1)     
    coorq8_loc(:,7) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  
    tmp=coorq8(:,8)-coorq8(:,1)     
    coorq8_loc(:,8) = (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  

    tmp = coorn - coorq8(:,1)
    !au cas ou on vire une composante normale
    tmp = tmp - (dot_product(tmp,frame(:,3))*frame(:,3))

    coorn_loc =  (/ dot_product(tmp,frame(:,1)), dot_product(tmp,frame(:,2)) /)  
    
    ! fd c'est quoi cette merde ?
    !coorq8_loc(1,:) = coorq8(1,:)
    !coorq8_loc(2,:) = coorq8(2,:)
    !coorn_loc(1) = coorn(1)
    !coorn_loc(2) = coorn(2)
    
    !print*,'coordonnees locales du point'
    !write(*,'(2(1x,D12.5))') coorn_loc
    !print*,'coordonnees locales de l element'
    !write(*,'(2(1x,D12.5))') coorq8_loc
    !print*,'----'

    call compute_coor_ref_ele(i_Q_P2,coorq8_loc,8,2,coorn_loc,10,1d-10,(/0.d0, 0.d0/),cooref,err_)

    if (err_ > 0 ) then
      call logmes('Error in '//IAM//' : unexpected result of compute_coor_ref !',.true.)
      err = 1
      return
    endif


    !print*,'parametrage '
    !write(*,'(2(1x,D12.5))') cooref

    nullify(N)
    call FONCT_FORME(i_Q_P2, cooref, N)
    weight=N
    deallocate(N)

    !print*,'poids q4'
    !print*,weight

  end subroutine

  
!    - compute_coor_ref_ele : calcul des coordonnees reduite d'un point dans un element
!         - G_ele : calcul du residu a minimiser
!         - dG_ele : calcul de le gradient du residu
!         - solve_linear_pb : surcouche qui choisi automatiquement entre inverse22 et inverse33
!              pour resoudre le systeme lineaire, de l'iteration courante
!
!    - modules a importer : a_EF, algebra et overall

   ! fonction qui calcule les coordonnees, dans l'element de reference, 
   ! d'un point appartenant a un element deforme
   subroutine compute_coor_ref_ele(T_Fonc_Forme, coor_ele, NbNo, dim, &
                                   coor, nb_iter_max, tol, &
                                   coor_ref0, coor_ref,err)
   
      implicit none
      
      ! variables d'entree :
      !
      ! type de la fonction de forme de l'element (dans la nomenclature de a_EF) :
      ! * i_T_P1: pour un triangle a trois noeuds
      ! * i_Q_P1: pour un quadrangle a quatre noeuds
      integer(kind=4), intent(in) :: T_Fonc_Forme 
      ! nombre de noeuds dans l'element
      integer :: NbNo , dim
      ! coordonnees des noeuds de l'element reel
      ! coor_ele(:,i) : coordonnees du noeud i
      real(kind=8), dimension(dim, NbNo), intent(in) :: coor_ele 
      ! coordonnees d'un point dans l'element reel
      ! coor = (x, y), en 2D
      !        (x, y, z), en 3D
      real(kind=8), dimension(dim), intent(in) :: coor 
      ! coordonnees dans l'element de reference, servant de point de depart pour
      ! la recherche des coordonnees reduites de l'element courant
      ! coor_ref0 = (ksi, eta), en 2D
      !             (ksi, eta, zeta), en 3D
      real(kind=8), dimension(dim), intent(in) :: coor_ref0 
      ! nombre maximal d'iteration
      integer :: nb_iter_max 
      ! tolerance relative pour determiner si l'algo a converge
      real(kind=8) :: tol 
      ! ***
      ! variable de sortie :
      !
      ! coordonnees corsepondant, dans l'element de reference, au point repere par
      ! coor dans l'element deforme
      ! coor_ref = (ksi, eta), en 2D
      !            (ksi, eta, zeta), en 3D
      real(kind=8), dimension(dim), intent(out) :: coor_ref 
      ! ***      
      ! variables locales :
      !
      ! pour stocker la nouvelle valeur des coordonnees de reference 
      ! construites par la methode de Newton
      real(kind=8), dimension(dim) :: coor_ref_new
      ! pour stocker l'increment entre les coordonnees de references actuelles
      ! et la prochaine aproxiamtion
      real(kind=8), dimension(dim) :: delta_coor_ref 
      ! pour calculer le residu, de façon generique
      real(kind=8), dimension(dim) :: G_ 
      ! pour calculer la derivee du residu de façon generique
      real(kind=8), dimension(dim, dim) :: dG_ 
      ! pour stocker le second membre du systeme
      real(kind=8), dimension(dim) :: RHS 
      ! pour compter les iterations effectuees
      integer :: k 
      ! pour l'affichage de dG_
      integer :: i, j 
      integer :: err, err_
      
      err = 0
      
      ! on part du point de depart passe en argument
      coor_ref = coor_ref0
      
      ! on initialise la nombre d'iterations
      k = 0

      ! tant qu'on pas converge
      do
      
         ! on incremente le nombre d'iterations effectuees
         k = k + 1
         
         ! on calcule le residu pour les coordonnees de reference
         ! courantes
         call G_ele(T_Fonc_Forme, coor_ele, NbNo, dim, coor, coor_ref, G_)
         
         !print*, 'G_=', G_
         
         ! on en deduit le second membre pour la methode de Newton
         RHS = -G_
         
         ! on calcule la derivee du residu pour les coordonnees de
         ! reference courantes
         call dG_ele(T_Fonc_Forme, coor_ele, NbNo, dim, coor_ref, dG_)
         
         !print*, 'dG_='
         !do i=1, 2
         !   do j=1, 2
         !      write(*, '(D14.7,1X,A1)', advance='no') &
         !         dG_(i, j), ' '
         !   end do
         !   write(*, *)
         !end do
         
         ! on resoud le systeme :
         ! dG_*delta_coor_ref = RHS
         ! pour obtenir l'increment entre les nouvelles coordonnees
         ! et les coordonnees courantes
         call solve_linear_pb(dim, dG_, RHS, delta_coor_ref,err_)

         if (err_ > 0) then
           call logmes('Error in DiscreteGeometry::compute_coor_ref_ele : non invertible system !',.true.)
           err = 1
           return
         endif
         
         ! on calcule les nouvelles coordonnees de reference
         coor_ref_new = coor_ref + delta_coor_ref
         
         ! si on converge ou que le nombre maximal d'iterations 
         ! a ete atteint

         
         !fd le 10/08/2012 j'ai vire la quantite de reference dans le test car elle n'avait aucun sens

         if (dot_product(delta_coor_ref, delta_coor_ref) < &
             tol*tol .or. &
             k > nb_iter_max) then
             
             ! on sort de la boucle
             exit
             
         end if
         
         ! sinon, on passe a l'itaration suivante
         coor_ref = coor_ref_new

      end do

      ! on recupere les coordonnees de references finales
      coor_ref = coor_ref_new
      
      ! on affiche un warning si la method n'a pas converge
      if (k > nb_iter_max) then
      
         call logmes('Error in DiscreteGeometry::compute_coor_ref_ele : Newton did not converge !',.true.)
         err = 1
         return
      
      end if
      
      
      !print*, 'k=', k
      
   end subroutine compute_coor_ref_ele
   
   ! fonction qui calcule le residu :
   ! en 2D: G(ksi, eta) = (x, y) - F(ksi, eta)
   ! en 3D: G(ksi, eta, zeta) = (x, y, z) - F(ksi, eta, zeta)
   ! ou, F est la fonction qui calcule les cordonnees dans
   ! un element deforme, a partir des coordonnees dans l'element
   ! de reference 
   subroutine G_ele(T_Fonc_Forme, coor_ele, NbNo, dim, coor, coor_ref, G_)
   
      implicit none
      
      ! variables d'entree : 
      !
      ! type de la fonction de forme de l'element (dans la nmenclature de a_EF) :
      ! * i_T_P1: pour un triangle a trois noeuds
      ! * i_Q_P1: pour un quadrangle a quatre noeuds
      integer(kind=4), intent(in) :: T_Fonc_Forme 
      ! nombre de noeuds dans l'element
      integer :: NbNo,dim
      ! coordonnees des noeuds de l'element reel
      ! coor_ele(i, :) : coordonnees du noeud i
      real(kind=8), dimension(dim,nbno), intent(in) :: coor_ele 
      ! coordonnees d'un point dans l'element reel
      ! coor = (x, y), en 2D
      !        (x, y, z), en 3D
      real(kind=8), dimension(dim), intent(in) :: coor 
      ! coordonnees du point dans l'element de reference, pour lequel on cherche la
      ! valeur du residu
      ! coor_ref = (ksi, eta), en 2D
      !            (ksi, eta, zeta), en 3D
      real(kind=8), dimension(dim), intent(in) :: coor_ref 
      !***   
      ! variable de sortie :
      !
      ! valeur du residu
      ! au point de coordonnees coor_ref dans l'element de reference
      real(kind=8), dimension(dim), intent(out) :: G_ 
      !***   
      ! variables locales :
      !
      ! pour recuperer 
      ! les valeurs des fonctions de formes, en coor_ref
      real(kind=8), dimension(:), pointer :: N 
      ! indice de boucle
      integer :: i 
   
      ! on initialise le pointeur N
      nullify(N)
      
      ! on recupere les valeurs des fonctions de formes, en coor_ref
      call FONCT_FORME(T_Fonc_Forme, coor_ref, N)
   
      ! on en deduit la valeur du residu au point de coordonnees
      ! coor_ref, dans l'element de reference
      
      ! on initilaise le residu aux coordonnees du point dans 
      ! l'element reel
      G_ = coor
      
      ! pour chaque dimension
      do i=1, dim    
   
         ! on ajoute la contribution du noeud courant
         G_(i) = G_(i) - dot_product(N(:),coor_ele(i, :))
      
      end do
   
      ! on desalloue l'espace memoire alloue pour le calcul des 
      ! fonctions de forme
      deallocate(N)
      nullify(N)
   
   end subroutine G_ele
   
   ! fonction qui calcule la derivee du residu :
   ! en 2D: G(ksi, eta) = (x, y) - F(ksi, eta)
   ! en 3D: G(ksi, eta, zeta) = (x, y, z) - F(ksi, eta, zeta)
   ! ou, F est la fonction qui calcule les cordonnees dans
   ! un element deforme, a partir des coordonnees dans l'element
   ! de reference 
   subroutine dG_ele(T_Fonc_Forme, coor_ele, NbNo, dim, coor_ref, dG_)
   
      ! variables d'entree : 
      !
      ! type de la fonction de forme de l'element (dans la nmenclature de a_EF) :
      ! * i_T_P1: pour un triangle a trois noeuds
      ! * i_Q_P1: pour un quadrangle a quatre noeuds
      integer(kind=4), intent(in) :: T_Fonc_Forme
      ! nombre de noeuds dans l'element
      integer :: NbNo ,dim
      ! coordonnees des noeuds de l'element reel
      ! coor_ele(i, :) : coordonnees du noeud i
      real(kind=8), dimension(dim,NbNo), intent(in) :: coor_ele 
      ! coordonnees du point dans l'element de reference, pour lequel on cherche la
      ! valeur du residu
      ! coor_ref = (ksi, eta), en 2D
      !            (ksi, eta, zeta), en 3D
      real(kind=8), dimension(dim), intent(in) :: coor_ref 
      ! ***   
      ! variable de sortie :
      !
      ! valeur de la derivee du residu au point de coordonnees
      ! coor_ref dans l'element de reference
      real(kind=8), dimension(dim, dim), intent(out) :: dG_ 
      ! ***   
      ! variables locales :
      !
      ! pour recuperer 
      ! les valeurs des derivees des fonctions de formes, en coor_ref
      real(kind=8), dimension(:, :), pointer :: DN 
      ! indices de boucle
      integer :: i, j 
   
      ! on initialise le pointeur DN
      nullify(DN)
   
      ! on calcule les derivees des fonctions de forme, au point
      ! de coordonees coor_ref, dans l'element de reference
      call DERIVE_FORME(T_Fonc_Forme, coor_ref, DN)
      
      ! on initilaise la derivee du residu a 0
      dG_ = 0.d0
      
      ! pour chaque composante du residu
      do i=1, dim    
         ! pour chaque composante des coordonnees de reference
         do j=1, dim
            
            ! on ajoute la contribution des noeuds
            dG_(i, j) = dG_(i, j) - &
                        dot_product(DN(j, :), coor_ele(i, :))  
      
         end do
      
      end do
      
      ! Rq: dG serait-il l'oppose du transpose de la jacobienne?
   
      ! on desalloue l'espace memoire alloue pour le calcul des 
      ! fonctions de forme
      deallocate(DN)
      nullify(DN)
   
   end subroutine dG_ele
   
   ! fonction qui resoud un systeme lineaire de taille 2 ou 3
   subroutine solve_linear_pb(dim, A, b, x, err)
   
      implicit none
      
      ! variables d'entree : 
      !
      integer :: dim
      ! matrice du syteme
      real(kind=8), dimension(dim, dim), intent(in) :: A 
      ! second membre
      real(kind=8), dimension(dim), intent(in) :: b 


      ! ***      
      ! variables de sortie :
      !
      ! solution
      real(kind=8), dimension(dim), intent(out) :: x
      integer :: err
      
      ! ***   
      ! variables locales :     123456789012345678901234567890123
      !
      character(len=33) :: IAM='mod_a_projection::solve_linear_pb'
      ! determinant de A
      real(kind=8) :: det_A 
      ! inverse de A
      real(kind=8), dimension(dim, dim) :: inv_A 
      ! pour appeler la fonction d'inversion de matrice
      integer :: err_ 

      err = 0
      
      ! on copie A dans inv_A, pour que l'appel de la fonction d'inversion
      ! de matrice ne touche pas A
      inv_A = A
      
      ! on appelle la routine d'inversion de matrice 
      
      ! en fonction de la dimension
      select case(dim)
      
         case(2) ! cas 2D
      
            call inverse22(inv_A,err_)
            if (err_ == 1) then
              call logmes('Error in '//IAM//' non invertible matrix!',.true.)
              err = 1
              return
            endif

         case(3) ! cas 3D
         
            call inverse33(inv_A,err_)
            if (err_ == 1) then
              call logmes('Error in '//IAM//' non invertible matrix!',.true.)
              err = 1
              return
            endif
           
         ! si on ne rconnait pas la dimension
         case default
         
            ! on affiche un message d'erreur
            call logmes('Error in '//IAM//' unknown space dimension!',.true.)
            err = 1
            return
            
      end select
        
      ! on en deduit la solution du systeme
      x = matmul(inv_A, b)
   
   end subroutine solve_linear_pb

  !> evaluates (x, 0, 0) . n
  real(kind=8) function f_vol(coor,normal)
     implicit none

     real(kind=8), intent(in) :: coor(:),normal(:)
   
     f_vol=coor(1)*normal(1)

     return

  end function f_vol

  !> evaluates (x^2/2, 0, 0) . n
  real(kind=8) function f_xG(coor,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),normal(:)
        
     f_xG=0.5*coor(1)*coor(1)*normal(1)
     return

  end function f_xG

  !> evaluates (0, y^2/2, 0) . n
  real(kind=8) function f_yG(coor,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),normal(:)
        
     f_yG=0.5*coor(2)*coor(2)*normal(2)
     return

  end function f_yG

  !> evaluates (0, 0, z^2/2) . n
  real(kind=8) function f_zG(coor,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),normal(:)
        
     f_zG=0.5*coor(3)*coor(3)*normal(3)
     return

  end function f_zG

  ! evaluates (0, (y - y_G)^3/3, (z - z_G)^3/3) . n
  real(kind=8) function f_I11(coor,coorG,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),coorG(:),normal(:)
     real(kind=8), dimension(3) :: coor_bary
     real(kind=8), parameter :: untiers=1.d0/3.d0 

     ! on calcule les coordonnees du point dans le referentiel barycentrique de l'objet
     coor_bary(1) = coor(1) - coorG(1)
     coor_bary(2) = coor(2) - coorG(2)
     coor_bary(3) = coor(3) - coorG(3)
        
     f_I11= (coor_bary(2)*coor_bary(2)*coor_bary(2)*normal(2) + &
             coor_bary(3)*coor_bary(3)*coor_bary(3)*normal(3))*untiers
     return

  end function f_I11

  ! evaluates  ((x - x_G)^3/3, 0, (z - z_G)^3/3) . n
  real(kind=8) function f_I22(coor,coorG,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),coorG(:),normal(:)
     real(kind=8), dimension(3) :: coor_bary
     real(kind=8), parameter :: untiers=1.d0/3.d0 

     ! on calcule les coordonnees du point dans le referentiel barycentrique de l'objet
     coor_bary(1) = coor(1) - coorG(1)
     coor_bary(2) = coor(2) - coorG(2)
     coor_bary(3) = coor(3) - coorG(3)
        
     f_I22= (coor_bary(1)*coor_bary(1)*coor_bary(1)*normal(1) + &
             coor_bary(3)*coor_bary(3)*coor_bary(3)*normal(3))*untiers
     return

  end function f_I22

  ! evaluates ((x - x_G)^3/3, (y - y_G)^3/3, 0) . n
  real(kind=8) function f_I33(coor,coorG,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),coorG(:),normal(:)
     real(kind=8), dimension(3) :: coor_bary
     real(kind=8), parameter :: untiers=1.d0/3.d0 

     ! on calcule les coordonnees du point dans le referentiel barycentrique de l'objet
     coor_bary(1) = coor(1) - coorG(1)
     coor_bary(2) = coor(2) - coorG(2)
     coor_bary(3) = coor(3) - coorG(3)
        
     f_I33= (coor_bary(1)*coor_bary(1)*coor_bary(1)*normal(1) + &
             coor_bary(2)*coor_bary(2)*coor_bary(2)*normal(2))*untiers
     return

  end function f_I33

  ! evaluates -(0, (y - y_G)^2(z - z_G)/2, 0) . n
  real(kind=8) function f_I32(coor,coorG,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),coorG(:),normal(:)
     real(kind=8), dimension(3) :: coor_bary

     ! on calcule les coordonnees du point dans le referentiel barycentrique de l'objet
     coor_bary(1) = coor(1) - coorG(1)
     coor_bary(2) = coor(2) - coorG(2)
     coor_bary(3) = coor(3) - coorG(3)
        
     f_I32= -0.5*coor_bary(2)*coor_bary(2)*coor_bary(3)*normal(2)
     return

  end function f_I32

  !> evaluates -((x - x_G)^2(z - z_G)/2, 0, 0) . n
  real(kind=8) function f_I31(coor,coorG,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),coorG(:),normal(:)
     real(kind=8), dimension(3) :: coor_bary

     ! on calcule les coordonnees du point dans le referentiel barycentrique de l'objet
     coor_bary(1) = coor(1) - coorG(1)
     coor_bary(2) = coor(2) - coorG(2)
     coor_bary(3) = coor(3) - coorG(3)
        
     f_I31= -0.5*coor_bary(1)*coor_bary(1)*coor_bary(3)*normal(1)
     return

  end function f_I31

  ! evaluates -((x - x_G)^2(y - y_G)/2, 0, 0) . n
  real(kind=8) function f_I21(coor,coorG,normal)
     implicit none
        
     real(kind=8), intent(in) :: coor(:),coorG(:),normal(:)
     real(kind=8), dimension(3) :: coor_bary

     ! on calcule les coordonnees du point dans le referentiel barycentrique de l'objet
     coor_bary(1) = coor(1) - coorG(1)
     coor_bary(2) = coor(2) - coorG(2)
     coor_bary(3) = coor(3) - coorG(3)
        
     f_I21= -0.5*coor_bary(1)*coor_bary(1)*coor_bary(2)*normal(1)
     return

  end function f_I21

  !> routine qui calcule les coins d'un contour (liste de couple de noeuds (debut/fin) orientee)
  subroutine get_corners_of_contour(vertex,contour,corners,tol)
     implicit none
     real(kind=8),dimension(:,:) :: vertex
     integer,dimension(:) :: contour
     integer,dimension(:),pointer :: corners
     real(kind=8),optional :: tol
     ! ***
     integer :: in,nbn,ic,nbc
     integer,dimension(:),allocatable :: i4_vec
     real(kind=8) :: norm1,norm2
     real(kind=8),dimension(3)::vec1,vec2

     if (.not. present(tol)) then 
       tol = 1.d-2 ! 4 deg (90*acos(1-tol)/pi)
     endif


     ! combien de segments
     nbn = size(contour)/2
     ! etat du premier noeud du segment
     allocate(i4_vec(nbn))
     i4_vec=0
     
     ! si on a plus que 3 sommets on voir si on vire les doublons
     if (nbn > 3) then

       !print*,'-------'
       !print*,contour
       !print*,1,' : ',contour(2),contour(1)
       vec1(:)= vertex(:,contour(2)) - vertex(:,contour(1))
       norm1=length3(vec1)
       vec1=vec1/norm1

       do in=2,nbn 
         !print*,in,' : ',contour(2*in),contour(2*in-1)
         vec2(:)= vertex(:,contour(2*in)) - vertex(:,contour(2*in-1))       
         norm2=length3(vec2)
         vec2=vec2/norm2
         !print*,vec1
         !print*,vec2
         !print*,dot_product(vec1,vec2),1.d0 - tol

         ! si alignes on tag
         !fd c'est oriente je vire dabs : if (dabs(dot_product(vec1,vec2)) > 1.d0 - tol) i4_vec(in)=1
         if (dot_product(vec1,vec2) > 1.d0 - tol) i4_vec(in)=1
         !
         vec1=vec2
       enddo
    
       vec2(:)= vertex(:,contour(2)) - vertex(:,contour(1))
       norm2=length3(vec2)
       vec2=vec2/norm2
       !fd c'est oriente je vire dabs : if (dabs(dot_product(vec1,vec2)) > 1.d0 - tol) i4_vec(1)=1
       if (dot_product(vec1,vec2) > 1.d0 - tol) i4_vec(1)=1

     endif
     ! on remonte les coins
     nbc=count(i4_vec == 0)
     allocate(corners(nbc))
     ic=0
     do in=1,nbn
       if (i4_vec(in) == 0) then
         ic=ic+1
         corners(ic)=contour(2*in-1)
       endif
     enddo
     deallocate(i4_vec)

  end subroutine


  !fd routines de calcul de contact utilisees par common plane
  !  *ntr fourni une orientation de reference indispensable savoir la bonne direction des contacts
  !   les routines gerent la reorientation automatiquement.
  !   Une routine qui en appelle une autre va donc deleguer cette operation 
  !  *attention au cas ou on permute les roles cd/an il ne faut pas changer le signe de dist, normal apres coup c'est automatique 

  !> computes signed distance between 2 edges
  !> we assume here that nodes 1 are cd and nodes 2 are an ; ntr is the cd-an orientation  
  !> edgetoedge_distance = 0 no contact, =-1 1 contact point par extremite, = 1 contact point (not //), = 2 contact points (//) 
  integer function edgetoedge_distance(coor1b,coor1e,coor2b,coor2e,ntr,nbc,ptc,normal,dist,err)
     implicit none
     !> ends of edges
     real(kind=8),dimension(3)   :: coor1b,coor1e,coor2b,coor2e
     !> reference normal
     real(kind=8),dimension(3)   :: ntr
     !> number of contacts
     integer(kind=4)             :: nbc
     !> contact point
     real(kind=8),dimension(3,2) :: ptc
     !> normal vector
     real(kind=8),dimension(3,2) :: normal
     !> gap
     real(kind=8),dimension(2)   :: dist
     !
     integer                     :: err
     ! ***
                              !1234567890123456789012345678901234567
     character(len=37) :: IAM='DiscreteGeometry::edgetoedge_distance' 
     !> curvilinear coordinate of contact support node on edge 
     real(kind=8),dimension(2,2) :: weights
     !
     real(kind=8),dimension(3)   :: e1,e2,tmp
     real(kind=8)                :: l1,l2,norm,x1,x2
     real(kind=8)                :: mat(2,2),vec(2), a12,b1,b2,delta
     integer(kind=4)             :: err_                            
     ! tolerance sur le parallelisme
     real(kind=8)                :: tol=1e-6
     !
     character(len=90)           :: cout

     err = 0
     
     edgetoedge_distance = 0
     nbc     = 0       
     weights = 0.d0
     ptc     = 0.d0 
     normal  = 0.d0
     dist    = 0.d0

     ! calcul des axes des cotes 
     e1 = coor1e - coor1b     
     l1 = length3(e1)
     if (l1 == 0.d0) then
       write(cout,'(E12.5)') coor1b
       call logmes(cout,.true.)
       write(cout,'(E12.5)') coor1e
       call logmes(cout,.true.)
       call logmes('Error in '//IAM//' : ends of edge 1 are the same',.true.)
       err = 1
       return
     endif
     e1 = e1/l1

     e2 = coor2e - coor2b     
     l2 = length3(e2)
     if (l2 == 0.d0) then 
       write(cout,'(E12.5)') coor2b
       call logmes(cout,.true.)
       write(cout,'(E12.5)') coor2e
       call logmes(cout,.true.)
       call logmes('Error in '//IAM//' : ends of edge 2 are the same',.true.)
       err = 1
       return
     endif
     e2 = e2/l2

     norm = dot_product(e1,e2)

     if (dabs(norm) > (1.d0 - tol)) then

       !print*,"cas //"

       ! cas // 
     
       ! on teste 1 sur 2 

       tmp = coor1b - coor2b
       x1 = dot_product(tmp,e2)

       tmp = coor1e - coor2b
       x2 = dot_product(tmp,e2)
     
       if ( norm > 0.d0 ) then

         !print*,'meme sens'

         ! edge dans le meme sens

         if (x1 > l2) then

           edgetoedge_distance =-1

           !1          b   e
           !           x---x
           !    x----x  
           !2   b    e
           ! 
           ! noeud b - noeud e 

           nbc = nbc + 1

           weights(1,nbc) = 0. 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(coor1b + coor2e)
           normal(:,nbc) = coor1b - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)
                
             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else if ((x1 >= 0.d0 .and. x1 <=l2) .and. &
                   x2 > l2 ) then

           edgetoedge_distance = 2

           !1        b     e
           !         x-----x
           !    x-------x  
           !2   b       e
           ! 

           ! noeud b - milieu

           nbc=nbc+1

           weights(1,nbc) = 0. 
           weights(2,nbc) = x1/l2

           ptc(:,nbc) =  0.5*(coor1b + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1b - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr 
           endif

           ! milieu  - noeud e

           tmp = coor2e - coor1b    !fd zarbi
           x2 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x2/l1 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2e)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif
  
           return 

         else if ((x1 >= 0.d0 .and. x1 <=l2) .and. &
                  (x2 >= 0.d0 .and. x2 <=l2)  ) then

           edgetoedge_distance = 2

           !1        b     e
           !         x-----x
           !      x-----------x  
           !2     b           e
           ! 

           ! noeud b - milieu

           nbc=nbc+1

           weights(1,nbc) = 0. 
           weights(2,nbc) = x1/l2

           ptc(:,nbc) =  0.5*(coor1b + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1b - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           ! noeud e - milieu

           nbc=nbc+1

           weights(1,nbc) = 1.
           weights(2,nbc) = x2/l2 

           ptc(:,nbc) =  0.5*(coor1e + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1e - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif
  
           return 


         else if (x1 < 0.d0 .and. x2 > l2) then
           edgetoedge_distance = 2

           !1    b            e
           !     x------------x
           !         x-----x  
           !2        b     e
           ! 

           ! milieu - noeud b
            
            
           tmp = coor2b - coor1b
           x1 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x1/l1 
           weights(2,nbc) = 0.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2b)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2b
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           ! milieu - noeud e

           tmp = coor2e - coor1b
           x2 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x2/l1 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2e)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else if (x1 < 0.d0 .and. &
                 (x2 >= 0.d0 .and. x2 <=l2)  ) then

           edgetoedge_distance = 2

           !1    b       e
           !     x-------x
           !         x-----x  
           !2        b     e
           ! 

           ! milieu - noeud b 

           tmp = coor2b - coor1b
           x1 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x1/l1 
           weights(2,nbc) = 0.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2b)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2b
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif


           ! noeud e - milieu

           nbc=nbc+1

           weights(1,nbc) = 1.
           weights(2,nbc) = x2/l2 

           ptc(:,nbc) =  0.5*(coor1e + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1e - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif
  
           return 

         else if (x2 < 0.d0) then 

           edgetoedge_distance =-1

           !1  b     e
           !   x-----x
           !            x-----x  
           !2           b     e
           ! 

           ! noeud e - noeud b 

           nbc = nbc + 1
           weights(1,nbc) = 1. 
           weights(2,nbc) = 0.

           ptc(:,nbc) =  0.5*(coor1e + coor2b)
           normal(:,nbc) = coor1e - coor2b
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else 

           write(cout,'(4(E12.5,1x))') x1,x2,l1,l2
           call logmes(cout,.true.)
           call logmes('Error in '//IAM//' : unexpected case',.true.)
           err = 1
           return
 
         endif   

       else 

         !print*,'sens oppose'

         ! edge en sens opposes

         if (x2 > l2) then

           edgetoedge_distance =-1

           !1          e   b
           !           x---x
           !    x----x  
           !2   b    e
           ! 
           ! noeud e - noeud e 

           nbc = nbc + 1

           weights(1,nbc) = 1. 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(coor1e + coor2e)
           normal(:,nbc) = coor1e - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else if ((x2 >= 0.d0 .and. x2 <=l2) .and. &
                   x1 > l2 ) then

           edgetoedge_distance = 2

           !1        e     b
           !         x-----x
           !    x-------x  
           !2   b       e
           ! 

           ! noeud e - milieu

           nbc=nbc+1

           weights(1,nbc) = 1. 
           weights(2,nbc) = x2/l2

           ptc(:,nbc) =  0.5*(coor1e + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1e - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           ! milieu  - noeud e

           tmp = coor2e - coor1b
           x1 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x1/l1 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2e)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif
  
           return 

         else if ((x2 >= 0.d0 .and. x2 <=l2) .and. &
                  (x1 >= 0.d0 .and. x1 <=l2) ) then

           edgetoedge_distance = 2

           !1        e     b
           !         x-----x
           !      x-----------x  
           !2     b           e
           ! 

           ! noeud b - milieu

           nbc=nbc+1

           weights(1,nbc) = 1. 
           weights(2,nbc) = x2/l2

           ptc(:,nbc) =  0.5*(coor1e + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1e - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           ! noeud e - milieu

           nbc=nbc+1

           weights(1,nbc) = 0.
           weights(2,nbc) = x1/l2 

           ptc(:,nbc) =  0.5*(coor1e + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1e - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif
  
           return 

         else if (x2 < 0.d0 .and. x1 > l2) then

           edgetoedge_distance = 2

           !1    e            b
           !     x------------x
           !         x-----x  
           !2        b     e
           ! 

           ! milieu - noeud b 

           tmp = coor2e - coor1b
           x1 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x1/l1 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2e)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           ! milieu - noeud e

           tmp = coor2b - coor1b
           x2 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x2/l1 
           weights(2,nbc) = 0.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2b)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2b
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else if (x2 < 0.d0 .and. &
                 (x1 >= 0.d0 .and. x1 <=l2) ) then

           edgetoedge_distance = 2
            
           !1    e      b
           !     x------x
           !         x-----x  
           !2        b     e
           ! 

           ! milieu - noeud b 

           tmp = coor2e - coor1b
           x1 = dot_product(tmp,e1)

           nbc=nbc+1

           weights(1,nbc) = x1/l1 
           weights(2,nbc) = 1.

           ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) + coor2e)
           normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) - coor2e
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           ! noeud b - milieu

           nbc=nbc+1

           weights(1,nbc) = 1. 
           weights(2,nbc) = x2/l2

           ptc(:,nbc) =  0.5*(coor1e + ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))
           normal(:,nbc) = coor1e - ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else if (x1 < 0.d0) then 

           edgetoedge_distance =-1

           !1  e     b
           !   x-----x
           !            x-----x  
           !2           b     e
           ! 

           ! noeud e - noeud b 

           nbc = nbc + 1

           weights(1,nbc) = 0. 
           weights(2,nbc) = 0.

           ptc(:,nbc) =  0.5*(coor1b + coor2b)
           normal(:,nbc) = coor1b - coor2b
           dist(nbc) = length3(normal(:,nbc)) 

           if (dist(nbc) /= 0.d0) then
             normal(:,nbc) = normal(:,nbc) / dist(nbc)

             if (dot_product(normal(:,nbc),ntr) < 0.d0) then
               normal(:,nbc) = -normal(:,nbc)      
               dist(nbc) = -dist(nbc)
             endif

           else           
             normal(:,nbc) = ntr
           endif

           return

         else 

           write(cout,'(4(E12.5,1x))') x1,x2,l1,l2
           call logmes(cout,.true.) 
           call logmes('Error in '//IAM//' : unexpected case',.true.)
           err = 1
           return

         endif

       endif

     else

       !print*,'cas non //'

       ! cas non //

       ! ! calcul abcisse curviligne du point d'intersection des 
       ! ! droites passant par coorxb et de direction ex (x=1,2) 

       ! mat(1,1) = 1.d0 ; mat(1,2) = -norm
       ! mat(2,1) = norm ; mat(2,2) = -1.d0

       ! call inverse22(mat,err_)

       ! if (err_ == 1) then
       !   call logmes('Error in '//IAM//' : non invertible matrix',.true.)
       !   err = 1
       !   return
       ! endif

       ! tmp = coor2b - coor1b

       ! vec = (/ dot_product(tmp,e1), dot_product(tmp,e2) /)
       
       ! x1 = mat(1,1)*vec(1) + mat(1,2)*vec(2)
       ! x2 = mat(2,1)*vec(1) + mat(2,2)*vec(2)

       ! following Eberly 3D Game Engine Design p 642 ; decalage notation 0->1 et 1->2

       tmp = coor1b - coor2b
       b1  = dot_product(e1,tmp)         
       b2  =-dot_product(e2,tmp)         
       a12 =-norm 

       delta=1.d0-(a12*a12)

       if (delta < tol*tol) then
         call logmes('Error in '//IAM//' : non invertible matrix',.true.)
         err = 1
         return
       endif

       x1 = ((a12*b2) - b1)/delta 
       x2 = ((a12*b1) - b2)/delta
        
       nbc = nbc + 1

       if ((x1 >= 0.d0 .and. x1 <= l1) .and. &   
           (x2 >= 0.d0 .and. x2 <= l2)) then   

         edgetoedge_distance = 1

         !        2
         !      e x
         !    b   |   e
         ! 1  x - | - x
         !        |
         !      b x
         ! 
         ! milieu - milieu 

         weights(1,nbc) = x1/l1 
         weights(2,nbc) = x2/l2

       elseif ( x1 < 0.d0 .and. &   
               (x2 >= 0.d0 .and. x2 <= l2)) then   

         edgetoedge_distance = -1

         !        2
         !      e x
         !        |  b       e
         ! 1      |  x - - - x
         !        |
         !      b x

         ! noeud b - milieu

         weights(1,nbc) = 0. 
         weights(2,nbc) = x2/l2

       elseif ( x1 < 0.d0 .and. x2 < 0.d0 ) then   

         edgetoedge_distance = -1

         !        2
         !     e  x
         !        |  
         !        |  
         !        |
         !     b  x  b       e
         ! 1         x - - - x

         ! noeud b - noeud b

         weights(1,nbc) = 0. 
         weights(2,nbc) = 0.

       elseif ( x1 < 0.d0 .and. x2 > l2) then   

         edgetoedge_distance = -1

         !        2
         !           b       e
         ! 1         x - - - x
         !     e  x
         !        |  
         !        |  
         !        |
         !     b  x  

         ! noeud b - noeud e

         weights(1,nbc) = 0. 
         weights(2,nbc) = 1.

       elseif ( x1 > l1 .and. &   
               (x2 >= 0.d0 .and. x2 <= l2)) then   

         edgetoedge_distance = -1

         ! noeud e - milieu

         !                 2
         !               e x
         !   b       e     |  
         ! 1 x - - - x     |  
         !                 |
         !               b x

         ! noeud b - milieu

         weights(1,nbc) = 1. 
         weights(2,nbc) = x2/l2

       elseif ( x1 > l1 .and. x2 < 0.d0 ) then   

         edgetoedge_distance = -1

         !                        2
         !                     e  x
         !                        |  
         !                        |  
         !                        |
         !           b       e b  x
         ! 1         x - - - x
         
         ! noeud e - noeud b

         weights(1,nbc) = 1. 
         weights(2,nbc) = 0.

       elseif ( x1 > l1 .and. x2 > l2) then   

         edgetoedge_distance = -1

         !                      2
         !           b       e
         ! 1         x - - - x
         !                   e  x
         !                      |  
         !                      |  
         !                      |
         !                   b  x  

         ! noeud e - noeud e

         weights(1,nbc) = 1. 
         weights(2,nbc) = 1.

       elseif ((x1 >= 0.d0 .and. x1 <= l1) .and. &   
                x2 < 0.d0 ) then   

         edgetoedge_distance = -1

         !               2
         !             e x
         !               |  
         !               |  
         !             b x
         !           b       e 
         ! 1         x - - - x
         !

         ! milieu - noeud b 
         weights(1,nbc) = x1/l1 
         weights(2,nbc) = 0.

       elseif ((x1 >= 0.d0 .and. x1 <= l1) .and. &   
                x2 > l2 ) then   

         edgetoedge_distance = -1

         !               2
         !           b       e
         ! 1         x - - - x
         !            e  x
         !               |  
         !               |  
         !               |
         !            b  x  

         ! mlieu - noeud e           

         weights(1,nbc) = x1/l1 
         weights(2,nbc) = 1.

       else 

           write(cout,'(4(E12.5,1x))') x1,x2,l1,l2
           call logmes(cout,.true.)
           call logmes('Error in '//IAM//' : unexpected case',.true.)
           err = 1
           return

       endif

       ! print*,'---'

       ! print*,'weights ',weights(:,nbc)

       
       ptc(:,nbc) =  0.5*(((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) +  &
                          ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e))

       normal(:,nbc) = ((1.d0 - weights(1,nbc))*coor1b + weights(1,nbc)*coor1e) -  &
                       ((1.d0 - weights(2,nbc))*coor2b + weights(2,nbc)*coor2e)

       dist(nbc) = length3(normal(:,nbc)) 

       if (dist(nbc) /= 0.d0) then
         normal(:,nbc) = normal(:,nbc) / dist(nbc)

         ! print*,normal(:,nbc)
         ! print*,dist(nbc)
         
         if (dot_product(normal(:,nbc),ntr) < 0.d0) then
            normal(:,nbc) = -normal(:,nbc)      
            dist(nbc) = -dist(nbc)
          endif

       else           
         normal(:,nbc) = ntr(:)
       endif

       ! print*,normal(:,nbc)
       ! print*,dist(nbc)       
       ! print*,'---'

       return

     endif

  end function 


  !> computes signed distance between point and point
  !> we assume here that node 1 is cd and node 2 is an ; ntr is the cd-an orientation 
  subroutine nodetonode_distance(coor1,coor2,ntr,ptc,normal,dist)
     implicit none
     !> nodes
     real(kind=8),dimension(3) :: coor1,coor2
     !> reference normal
     real(kind=8),dimension(3) :: ntr
     !> contact point, normal and distance
     real(kind=8)  :: ptc(3),normal(3),dist

     ! ***
                              !1234567890123456789012345678901234567
     character(len=37) :: IAM='DiscreteGeometry::nodetonode_distance' 

     ptc = 0.5*(coor1 + coor2)

     normal = coor1 - coor2

     dist = length3(normal) 

     if (dist /= 0.d0) then
       normal = normal / dist

       if (dot_product(normal,ntr) < 0.d0) then
         normal = -normal      
         dist = -dist
       endif

     else           
       normal = ntr
     endif
  end subroutine     

  
  !> computes signed distance between point and edge
  !> we assume here that node 1 is cd and nodes 2 are an ; ntr is the cd-an orientation   
  !> nodetoedge_distance = 0 no contact, = -1 node-end , = 1  node-edge
  integer function nodetoedge_distance(coor1,coor2b,coor2e,ntr,ptc,normal,dist,err)
     implicit none
     !> nodes
     real(kind=8),dimension(3) :: coor1,coor2b,coor2e
     !> reference normal
     real(kind=8),dimension(3) :: ntr
     !> contact point, normal and distance
     real(kind=8)               :: ptc(3),normal(3),dist
     !
     integer                    :: err
     ! ***
                              !1234567890123456789012345678901234567
     character(len=37) :: IAM='DiscreteGeometry::nodetoedge_distance' 
     real(kind=8),dimension(3) :: e2,tmp
     real(kind=8)              :: weight,x1,l2
     character(len=90)         :: cout
     
     err= 0

     !> calcul des axes des cotes 

     e2 = coor2e - coor2b     
     l2 = length3(e2)
     if (l2 == 0) then
       write(cout,'(3(E12.5,1x))') coor2b 
       call logmes(cout,.true.)
       write(cout,'(3(E12.5,1x))') coor2e 
       call logmes(cout,.true.)

       call logmes('Error in '//IAM//' : ends of edge are the same',.true.)
       err = 1
       return
     endif
     e2 = e2/l2

     tmp = coor1 - coor2b
     x1 = dot_product(tmp,e2)

     if ( x1 >l2 ) then 

        !print*,'node - edge node e'

        nodetoedge_distance = -1

        weight = 1. 

        ptc = 0.5*(coor1 + coor2e)

        normal = coor1 - coor2e

        dist = length3(normal) 

        if (dist /= 0.d0) then
          normal = normal / dist
  
          if (dot_product(normal,ntr) < 0.d0) then
            normal = -normal      
            dist = -dist
           endif

        else           
          normal = ntr
        endif

     elseif (x1 >= 0.d0 .and. x1 <= l2) then  
        nodetoedge_distance = 1

        !print*,'node - edge'

        weight = x1/l2 

        ptc = 0.5*(coor1 + ((1. - weight)*coor2b + weight*coor2e))

        normal = coor1 - ((1. - weight)*coor2b + weight*coor2e)

        dist = length3(normal) 

        if (dist /= 0.d0) then
          normal = normal / dist

          if (dot_product(normal,ntr) < 0.d0) then
            normal = -normal      
            dist = -dist
           endif

        else           
          normal = ntr
        endif

     elseif (x1 < 0.d0) then 
        nodetoedge_distance = -1   

        !print*,'node - edge node b'

        weight = 0. 

        ptc = 0.5*(coor1 + coor2b)

        normal = coor1 - coor2b

        dist = length3(normal) 

        if (dist /= 0.d0) then
          normal = normal / dist

          if (dot_product(normal,ntr) < 0.d0) then
            normal = -normal      
            dist = -dist
           endif

        else           
          normal = ntr
        endif

     else 
 
       call logmes('Error in '//IAM//' : unexpected case',.true.)
       err = 1
       return
       
     endif

   end function


  !> computes signed distance between point and face
  !> we assume here that node 1 is cd and nodes 2 are an ; ntr is the cd-an orientation
  !> nodetoface_distance = 0 no contact, =-1 node-end, =-2 node-edge , =1 node-face
  integer function nodetoface_distance(coor1,coor2,ntr,ptc,normal,dist,err)
     implicit none
     !> nodes
     real(kind=8),dimension(3)   :: coor1
     !> face
     real(kind=8),dimension(:,:) :: coor2
     !> reference normal
     real(kind=8),dimension(3)   :: ntr
     !> contact point
     real(kind=8)                :: ptc(3)
     !> normal and distance
     real(kind=8)                :: normal(3)
     !> distance
     real(kind=8)                :: dist
     !
     integer                     :: err, err_

     ! ***
                              !1234567890123456789012345678901234567
     character(len=37) :: IAM='DiscreteGeometry::nodetoface_distance' 

     real(kind=8),dimension(:,:),allocatable :: e  
     real(kind=8),dimension(:),allocatable :: l  
     integer,dimension(:),allocatable :: signe
     integer :: nbs,ie,icheck,status
     real(kind=8) :: tmp(3),t(3),x,x1,xprev

     character(len=90) :: cout
     
     err = 0
     
     nbs = size(coor2,dim=2)

     normal = ntr
     
     !fd on reconstruit les vecteurs tangents
     allocate(e(size(coor2,dim=1),nbs),l(nbs))     
     do ie = 1, nbs
       e(:,ie) = coor2(:,modulo(ie,nbs)+1) - coor2(:,ie)
       l(ie) = length3(e(:,ie))
       if (l(ie) /= 0.d0) then 
         e(:,ie) = e(:,ie)/l(ie) 

         !print*,ie,e(:,ie)

       else
         write(cout,'(I0,1x,I0)') ie,nbs
         call logmes(cout,.true.)
         write(cout,'(3(E12.5,1x))') coor2(:,ie)
         call logmes(cout,.true.)
         write(cout,'(3(E12.5,1x))') coor2(:,modulo(ie,nbs)+1)
         call logmes(cout,.true.)
         call logmes('Error in '//IAM//' : two vertices at same place',.true.)
         err = 1
         return
       endif  
     enddo

     !fd recherche du point de contact  
     tmp = coor1 - coor2(:,1)  
     dist = dot_product(tmp,normal)  

     ptc = coor1 - dist*normal 

     !print*,'ptc',ptc

     ! test si dedans 
     allocate(signe(nbs))
     do ie = 1, nbs

       t = cross_product(normal,e(:,ie)) 
       tmp = ptc - coor2(:,ie)

       signe(ie) = 0
       if (dot_product(t,tmp) < 0.d0) signe(ie) = 1 

       !print*,'signe(',ie,')=',signe(ie)

     enddo

     status=sum(signe)      
     if (status > 1 .and. nbs >3) then 
       ! a la peche au cas degenere (comme le trapeze)
       xprev = -1.d0
       x1= 1.d0
       do ie = 1, nbs
         if (signe(ie) == 0) then
           xprev=-1.d0
           cycle
         endif
         tmp = ptc - coor2(:,ie)
         x = dot_product(tmp,e(:,ie))
         if (ie==1) x1=x
         if (x>=0.d0 .and. x<=l(ie)) then 
           ! on se projete bien sur un edge et pas sur un sommet
           signe=0
           signe(ie)=1
           status=1
           exit
         else if ( x > l(ie)) then
           xprev = x
           if (ie == nbs .and. x1 < 0.d0) then
             signe=0
             signe(nbs)=1
             signe(1)=1
             status=2
             exit
           endif
         else if (x < 0.d0) then
           if (xprev > 0.d0) then                     
             signe=0
             signe(ie-1)=1
             signe(ie)=1
             status=2
             exit
           endif 
         endif
       enddo
     endif

     select case (status)
     case(0)
       ! dans la face
       nodetoface_distance = 1

       ! if (dot_product(normal,ntr) < 0.) then
       !   normal = -normal      
       !   dist = -dist
       ! endif

     case(1)
       ! sur un cote
       nodetoface_distance =-2

       do ie=1, nbs-1
         if (signe(ie) == 0) cycle  
         icheck = nodetoedge_distance(coor1,coor2(:,ie),coor2(:,modulo(ie,nbs)+1),ntr,ptc,normal,dist,err_)

         if (err_ > 0) then
           call logmes('Error in '//IAM//' : unexpected case in nodetoedge distance computation',.true.)
           err = 1
           return
         endif    

         exit
       enddo
       
     case(2)
       ! sur un sommet
       nodetoface_distance =-1

       do ie=1, nbs
         if (signe(ie) == 0) cycle  
         if (ie == 1 .and. signe(ie+1) == 0) then 
           call nodetonode_distance(coor1,coor2(:,ie),ntr,ptc,normal,dist)
         else 
           call nodetonode_distance(coor1,coor2(:,modulo(ie,nbs)+1),ntr,ptc,normal,dist)
         endif 
         exit
       enddo

     case default
       ! bidon

       call logmes('Error in '//IAM//' : unexpected case',.true.)
       err = 1
       return
       
     end select

     deallocate(e,signe,l)

  end function


  !> computes signed distance between edge and face
  !> we assume here that nodes 1 are cd and nodes 2 are an ; ntr is the cd-an orientation
  !> edgetoface_distance = 0 no contact, = -1 node-end, = -2 node-edge , =2 edge-face
  integer function edgetoface_distance(coor1b,coor1e,coor2,ntr,nb_ptc,ptc,normal,dist,err)
     implicit none
     !> first node of the edge
     real(kind=8),dimension(3)   :: coor1b
     !> second node of the edge
     real(kind=8),dimension(3)   :: coor1e
     !> face
     real(kind=8),dimension(:,:) :: coor2
     !> reference normal
     real(kind=8),dimension(3)   :: ntr
     !> number of contact point ; maximum 2
     integer                     :: nb_ptc
     !> contact points
     real(kind=8)                :: ptc(3,2)
     !> normals
     real(kind=8)                :: normal(3,2)
     !> gaps
     real(kind=8)                :: dist(2)
     !
     integer                     :: err,err_

     ! ***
                              !1234567890123456789012345678901234567
     character(len=37) :: IAM='DiscreteGeometry::edgetoface_distance' 

     real(kind=8),dimension(:,:),allocatable :: e  
     real(kind=8),dimension(:),allocatable :: l  
     integer,dimension(:),allocatable :: signeb,signee
     integer :: nbs,ie,icheck,l_nb_ptc,status
     real(kind=8) :: tmp(3),t(3),x,xprev,x1,l_ptc(3,2),l_n(3,2),l_dist(2)
     character(len=90) :: cout

     integer :: imin

     
     err = 0
     
     ! nombre de sommets du contour
     nbs = size(coor2,dim=2)     

     ! on stocke l'orientation de la matiere 
     normal(:,1) = ntr 
     
     !fd on reconstruit les vecteurs tangents
     allocate(e(size(coor2,dim=1),nbs),l(nbs))     
     do ie = 1, nbs
       e(:,ie) = coor2(:,modulo(ie,nbs)+1) - coor2(:,ie)
       l(ie) = length3(e(:,ie))
       if (l(ie) /= 0.d0) then 
         e(:,ie) = e(:,ie)/l(ie) 
       else
         write(cout,'(I0)') ie
         call logmes(cout,.true.)
         write(cout,'(3(1x,D12.5))') coor2
         call logmes(cout,.true.)
         call logmes('Error in '//IAM//' : two vertices at same place',.true.)
         err = 1
         return
       endif  
     enddo

     !fd recherche du point de contact  
     tmp = coor1b - coor2(:,1)  
     dist(1) = dot_product(tmp,normal(:,1))  
     ptc(:,1) = coor1b - dist(1)*normal(:,1) 

     tmp = coor1e - coor2(:,1)  
     dist(2) = dot_product(tmp,normal(:,1))  
     ptc(:,2) = coor1e - dist(2)*normal(:,1) 

     ! test si dedans ; =0 dedans =1 dehors
     ! soit les deux points dans le contour et c'est termine
     ! soit on doit gerer les cas pourris:
     !  extremitees dehors,
     !  tangent a un bord,
     !  qui coupe un sommet par arrete
     !  qui coupe un sommet par extremitee
     
     allocate(signeb(nbs),signee(nbs))     
     do ie = 1, nbs

       t = cross_product(normal(:,1),e(:,ie)) 

       tmp = ptc(:,1) - coor2(:,ie)
       signeb(ie) = 0
       if (dot_product(t,tmp) < 0.d0) signeb(ie) = 1 

       tmp = ptc(:,2) - coor2(:,ie)
       signee(ie) = 0
       if (dot_product(t,tmp) < 0.d0) signee(ie) = 1 

     enddo

     nb_ptc=2

     status=sum(signeb)
     ! le noeud b (begin) est dehors
     if (status > 1 .and. nbs >3) then 
       ! a la peche au cas degenere (genre trapeze)
       xprev = -1.d0
       x1= 1.d0
       do ie = 1, nbs
         if (signeb(ie) == 0) then
           xprev=-1.d0
           cycle
         endif
         tmp = ptc(:,1) - coor2(:,ie)
         x = dot_product(tmp,e(:,ie))
         if (ie==1) x1=x
         if (x>=0.d0 .and. x<=l(ie)) then 
           ! on se projete bien sur un edge et pas sur un sommet
           signeb=0
           signeb(ie)=1
           status=1
           exit
         else if ( x > l(ie)) then
           xprev = x
           if (ie == nbs .and. x1 < 0.d0) then
             signeb=0
             signeb(nbs)=1
             signeb(1)=1
             status=2
             exit
           endif
         else if (x < 0.d0) then
           if (xprev > 0.d0) then                     
             signeb=0
             signeb(ie-1)=1
             signeb(ie)=1
             status=2
             exit
           endif 
         endif
       enddo
     endif

     select case (status)
     case(0)
       ! dans la face
       edgetoface_distance = 1

       !print*,'noeud b dans la face'

       ! if (dot_product(normal(:,1),ntr) < 0.d0) then
       !   normal(:,1) = -normal(:,1)      
       !   dist(1) = -dist(1)
       ! endif


     case(1)
       ! sur un cote
       edgetoface_distance =-2

       !print*,' noeud b sort face par edge'

       do ie=1, nbs
         if (signeb(ie) == 0) cycle  

         icheck = edgetoedge_distance(coor1b,coor1e, &
                                      coor2(:,ie),coor2(:,modulo(ie,nbs)+1), &
                                      ntr,l_nb_ptc,l_ptc,l_n,l_dist,err_)

         if (err_ > 0) then
           call logmes('Error in '//IAM//' : unexpected problem in edgetoedge distance computation',.true.)
           err = 1
           return
         endif   

         
         if (l_nb_ptc == 2) then

           !> \todo : est-ce qu'on doit faire autre chose ?
           call logmes('cas degenere : on garde le point milieu')

           ! print*,'xxxx'
           ! print*,l_ptc(:,1)
           ! print*,l_dist(1)
           ! print*,l_n(:,1)
           ! !--
           ! print*,l_ptc(:,2)
           ! print*,l_dist(2)
           ! print*,l_n(:,2)
           ! print*,'xxxx'

           ! ptc(:,1) = 0.5d0*(l_ptc(:,1)+l_ptc(:,2))
           ! dist(1)  = 0.5d0*(l_dist(1)+l_dist(2))
           ! normal(:,1) = 0.5d0*(l_n(:,1) + l_n(:,2))

           imin = minloc(l_dist(:),dim=1)           
           ptc(:,1) = l_ptc(:,imin)
           dist(1)  = l_dist(imin)
           normal(:,1) = l_n(:,imin)

         else

           ptc(:,1) = l_ptc(:,1)
           dist(1)  = l_dist(1)
           normal(:,1) = l_n(:,1)

         endif

         exit
       enddo

       
     case(2)
       ! sur un sommet
       edgetoface_distance =-1

       !print*,' noeud b sort face par sommet'

       do ie=1, nbs
         if (signeb(ie) == 0) cycle  

         !print*,coor2(:,modulo(ie,nbs)+1)
         !print*,coor1b
         !print*,coor1e

         
         if (ie == 1 .and. signeb(ie+1) == 0) then 
           ! si c'est le premier l'autre est le dernier 
           icheck = nodetoedge_distance(coor2(:,ie),&
                                        coor1b,coor1e,-ntr,l_ptc(:,1),l_n(:,1),l_dist(1),err_)
         else 
           ! sinon c'est le suivant   
           icheck = nodetoedge_distance(coor2(:,modulo(ie,nbs)+1), &
                                        coor1b,coor1e,-ntr,l_ptc(:,1),l_n(:,1),l_dist(1),err_)
         endif 

         if (err_ > 0) then
           call logmes('Error in '//IAM//' : unexpected problem in nodetoedge distance computation',.true.)
           err = 1
           return
         endif   
         
         ptc(:,1) = l_ptc(:,1)
         normal(:,1) = -l_n(:,1)
         dist(1) = l_dist(1) 

         exit
       enddo

     case default
       ! bidon

       !print*,sum(signeb)
       !print*,sum(signeb)
       !print*,signeb

       !print*,coor1b
       !print*,coor1e 
       !print*,ptc(:,1)
       !write(*,'(3(1xD12.5))') coor2

       call logmes('Error in '//IAM//' : unexpected case with b and c nodes',.true.)
       err = 1
       return
      
     end select


     status=sum(signee)      
     if (status > 1 .and. nbs >3) then 
       ! a la peche au cas degenere (genre trapeze)
       xprev = -1.d0
       x1= 1.d0
       do ie = 1, nbs
         if (signee(ie) == 0) then
           xprev=-1.d0
           cycle
         endif
         tmp = ptc(:,2) - coor2(:,ie)
         x = dot_product(tmp,e(:,ie))
         if (ie==1) x1=x
         if (x>=0.d0 .and. x<=l(ie)) then 
           ! on se projete bien sur un edge et pas sur un sommet
           signee=0
           signee(ie)=1
           status=1
           exit
         else if ( x > l(ie)) then
           xprev = x
           if (ie == nbs .and. x1 < 0.d0) then
             signee=0
             signee(nbs)=1
             signee(1)=1
             status=2
             exit
           endif
         else if (x < 0.d0) then
           if (xprev > 0.d0) then                     
             signee=0
             signee(ie-1)=1
             signee(ie)=1
             status=2
             exit
           endif 
         endif
       enddo
     endif


     select case (status)
     case(0)
       ! dans la face

       edgetoface_distance = 1

       !print*,'noeud e dans la face'

       ! if (dot_product(normal(:,2),ntr) < 0.d0) then
       !   normal(:,2) = -normal(:,2)      
       !   dist(2) = -dist(2)
       ! endif

     case(1)
       ! sur un cote

       edgetoface_distance =-2

       !print*,' noeud e sort face par edge'


       do ie=1, nbs
         if (signee(ie) == 0) cycle  

         icheck = edgetoedge_distance(coor1b,coor1e, &
                                     coor2(:,ie),coor2(:,modulo(ie,nbs)+1), &
                                     ntr,l_nb_ptc,l_ptc,l_n,l_dist,err_)

        if (err_ > 0) then
          call logmes('Error in '//IAM//' : unexpected problem in edgetoedge distance computation',.true.)
          err = 1
          return
  
        endif 

         if (l_nb_ptc == 2) then

           !> \todo : est-ce qu'on doit faire autre chose ?
           call logmes('degenerated case: middle point kept')

           ptc(:,2) = 0.5d0*(l_ptc(:,1)+l_ptc(:,2))
           dist(2)  = 0.5d0*(l_dist(1)+l_dist(2))
           normal(:,2) = 0.5d0*(l_n(:,1) + l_n(:,2))

         else

           ptc(:,2) = l_ptc(:,1)
           dist(2)  = l_dist(1)
           normal(:,2) = l_n(:,1)

         endif

         exit
       enddo

     case(2)
       ! sur un sommet

       edgetoface_distance =-1

       !print*,' noeud e sort face par sommet'

       do ie=1, nbs
         if (signee(ie) == 0) cycle  
         if (ie == 1 .and. signee(ie+1) == 0 ) then
           icheck = nodetoedge_distance(coor2(:,ie),coor1b,coor1e, &
                                       -ntr,l_ptc(:,2),l_n(:,2),l_dist(2),err_)
         else
           icheck = nodetoedge_distance(coor2(:,modulo(ie,nbs)+1),coor1b,coor1e, &
                                       -ntr,l_ptc(:,2),l_n(:,2),l_dist(2),err_)
         endif 

         if (err_ > 0) then
           call logmes('Error in '//IAM//' : unexpected problem in edgetoedge distance computation',.true.)
           err = 1
           return
  
         endif 
         
         ptc(:,2) = l_ptc(:,2)
         normal(:,2) = -l_n(:,2)
         dist(2) = l_dist(2) 

         exit
       enddo

     case default
       ! bidon
       !print*,sum(signee)
       !print*,signee

       !print*,coor1b
       !print*,coor1e 
       !print*,ptc(:,2)
       !write(*,'(3(1xD12.5))') coor2

       call logmes('Error in '//IAM//' : unexpected case with e and c nodes',.true.)
       err = 1
       return
      
     end select

     deallocate(e,signeb,signee,l)

  end function

  !fd new 24052015
  !> computes signed distance between edge and face
  !> we assume here that nodes 1 are cd and nodes 2 are an ; ntr is the cd-an orientation  
  !> edgetoface_distance = 0 no contact, = -1 node-end, = -2 node-edge , = 1 edge-face
  integer function edgetoface_distance_wp(coor1b,coor1e,coor2,ntr,nb_ptc,ptc,normal,dist,err,bavard)
     use predicates
     implicit none
     !> first node of the edge
     real(kind=8),dimension(3)   :: coor1b
     !> second node of the edge
     real(kind=8),dimension(3)   :: coor1e
     !> face
     real(kind=8),dimension(:,:) :: coor2
     !> reference normal
     real(kind=8),dimension(3)   :: ntr
     !> number of contact point ; maximum 2
     integer                     :: nb_ptc
     !> contact points
     real(kind=8)                :: ptc(3,2)
     !> normals
     real(kind=8)                :: normal(3,2)
     !> gaps
     real(kind=8)                :: dist(2)
     !
     integer                     :: err, err_
     logical                     :: bavard 

     ! ***
                               !1234567890123456789012345678901234567890
     character(len=40)  :: IAM='DiscreteGeometry::edgetoface_distance_wp'
     character(len=120) :: cout

     real(kind=8),dimension(3)   :: pcoor1b,pcoor1e,normal_in,tmp
     real(kind=8)                :: dist1b,dist1e
     integer                     :: nbs,ie,status,nb_cross,ie_para
     real(kind=8)                :: zero,b_check,e_check,b2_check,e2_check
     logical                     :: b_inside,e_inside,keep

     ! pour les tests en plus
     integer                     :: icheck,l_nb_ptc
     real(kind=8)                :: l_ptc(3,2),l_n(3,2),l_dist(2)

     integer                     :: imin
     
     err = 0
     
     zero= f_exactinit()

     ! nombre de sommets du contour
     nbs = size(coor2,dim=2)     
     normal_in=ntr
     normal=0.d0
     
     ! la normale est fourni en entree; a corriger quand on aura toute la structure de donnees
     !fd calcul des points dans le plan de la face (ca reste 3D !!)
     
     tmp = coor1b - coor2(:,1)  
     dist1b = dot_product(tmp,normal_in)
     !print*,dist1b
     pcoor1b = coor1b - dist1b*normal_in
     !print*,pcoor1b

     tmp = coor1e - coor2(:,1)  
     dist1e = dot_product(tmp,normal_in)
     !print*,dist1e
     pcoor1e = coor1e - dist1e*normal_in 
     !print*,pcoor1e
     
     ! test si les noeuds sont dans le contour ou si le edge intersecte la face

     edgetoface_distance_wp = 0
     nb_ptc = 0
     ptc=0.d0
     
     b_inside = .True.
     e_inside = .True.
     nb_cross = 0

     if ( bavard ) then
       write(cout,*) 'test intersection edge-bord face'
       call logmes(cout, .true.)
     end if
     
     do ie = 1, nbs
       if ( bavard ) then
         write(cout,*) 'bord number ',ie
         call logmes(cout, .true.)
       end if
        
       tmp = coor2(:,ie) - normal_in(:)
        
       !print*,coor2(:,ie), tmp, coor2(:,modulo(ie,nbs)+1), pcoor1b
       b_check = f_orient3d(coor2(:,ie), tmp, coor2(:,modulo(ie,nbs)+1), pcoor1b)

       !fd de quel cote est le point
       if (b_check < 0.d0) b_inside = .False.

       !print*,coor2(:,ie), tmp, coor2(:,modulo(ie,nbs)+1), pcoor1e
       e_check = f_orient3d(coor2(:,ie), tmp, coor2(:,modulo(ie,nbs)+1), pcoor1e)
       
       !fd de quel cote est le point
       if (e_check < 0.d0) e_inside = .False.

       !print*,b_check,e_check
       
       !fd si ca peut croiser on teste le dual
       if ((b_check <  0.d0 .and. e_check >= 0.d0) .or. &      
           (b_check >= 0.d0 .and. e_check <  0.d0)) then

         tmp = pcoor1b(:) - normal_in(:)
          
         b2_check = f_orient3d(pcoor1b, tmp, pcoor1e, coor2(:,ie))
         e2_check = f_orient3d(pcoor1b, tmp, pcoor1e, coor2(:,modulo(ie,nbs)+1))

         !fd ca coupe
         if ((b2_check <  0.d0 .and. e2_check >= 0.d0) .or. &      
             (b2_check >= 0.d0 .and. e2_check <  0.d0)) then

           if ( bavard ) then
              write(cout,*) 'ca coupe'
              call logmes(cout, .true.)
              write(cout,*) coor1b
              call logmes(cout, .true.)
              write(cout,*) coor1e
              call logmes(cout, .true.)
              do imin=1,nbs
                write(cout,*) coor2(:,imin)
                call logmes(cout, .true.)
              enddo
           endif

            
           !fd on delegue le calcul des intersections
           ! icheck = edgetoedge_distance(coor1b,coor1e, &
           !                              coor2(:,ie),coor2(:,modulo(ie,nbs)+1), &
           !                              ntr,l_nb_ptc,l_ptc,l_n,l_dist,err_)

           icheck = edgetoedge_distance(coor1b,coor1e, &
                                        coor2(:,ie),coor2(:,modulo(ie,nbs)+1), &
                                        normal_in,l_nb_ptc,l_ptc,l_n,l_dist,err_)

           !print*,'icheck = ', icheck

           if ( icheck == 0 .or. err_ > 0) then
              call logmes('Error in '//IAM//' : intersection expected',.true.)
              err = 1
              return
           endif   
           
           ! on fait un test geo pour verifier que le point n'existe pas deja  
           ! a ne faire que si on a deja 1 point
           keep = .True.
           if (nb_cross == 1) then
             if (l_nb_ptc == 2) then
               tmp = 0.5d0*(l_ptc(:,1)+l_ptc(:,2))
             else
               tmp = l_ptc(:,1)
             endif

             if (length3(ptc(:,1) - tmp(:)) < 1d-14) keep = .False.
             
           else if (nb_cross == 2) then
             keep = .False.  
           endif

           if (keep) then 
             nb_cross = nb_cross + 1
              
             if (l_nb_ptc == 2) then
               !> \todo : est-ce qu'on doit faire autre chose ?
               call logmes('cas degenere : bords paralleles')

               ! print*,'yyyy'
               ! print*,l_ptc(:,1)
               ! print*,l_dist(1)
               ! print*,l_n(:,1)
               ! !--
               ! print*,l_ptc(:,2)
               ! print*,l_dist(2)
               ! print*,l_n(:,2)
               ! print*,'yyyy'
 

               ! ptc(:,nb_cross) = 0.5d0*(l_ptc(:,1)+l_ptc(:,2))
               ! dist(nb_cross)  = 0.5d0*(l_dist(1)+l_dist(2))
               ! normal(:,nb_cross) = 0.5d0*(l_n(:,1) + l_n(:,2))

               imin = minloc(l_dist(:),dim=1)           
               ptc(:,nb_cross) = l_ptc(:,imin)
               dist(nb_cross)  = l_dist(imin)
               normal(:,nb_cross) = l_n(:,imin)
               
               
             else
  
               ptc(:,nb_cross) = l_ptc(:,1)
               dist(nb_cross)  = l_dist(1)
               normal(:,nb_cross) = l_n(:,1)

             endif

             !fd non !? normal(:,nb_cross) = normal(:,nb_cross)*sign(1.d0,dot_product(normal(:,nb_cross),ntr(:)))
            
             if (bavard) then
               write(cout,*)  icheck,'-1 extermite, 1 intersection, 2 parallele'
               call logmes(cout, .true.)
               write(cout,*) 'ok on garde intersection',nb_cross
               call logmes(cout, .true.)
               write(cout,*) 'ptc  ',ptc(:,nb_cross)
               call logmes(cout, .true.)
               write(cout,*) 'dist ',dist(nb_cross)
               call logmes(cout, .true.)
               write(cout,*) 'n    ',normal(:,nb_cross)
               call logmes(cout, .true.)
             endif  
           endif

         endif
       endif
      
       !fd cas //
       if (b_check == 0.d0 .and. e_check == 0.d0) then
         if (bavard) then
           write(cout,*) 'edge - bord face paralleles'
           call logmes(cout, .true.)
         end if
         nb_cross = -1
         ie_para = ie
         exit 
       endif
     enddo

     if (nb_cross == 2) then
       edgetoface_distance_wp = 1
       nb_ptc = 2 
     else if (nb_cross == 1) then
       if (b_inside) then

         edgetoface_distance_wp = 1
         nb_ptc = 2   
         ptc(:,2) = pcoor1b(:)
         dist(2) = dist1b
         normal(:,2) = normal_in
         
         ! if (dot_product(normal(:,2),normal_in) < 0.d0) then
         !   dist(2) = -dist(2) 
         !   normal(:,2) = -normal_in
         ! endif

         if (bavard) then
            write(cout,*) ' b inside'
            call logmes(cout, .true.)
            write(cout,*) 'ptc  ',ptc(:,2)
            call logmes(cout, .true.)
            write(cout,*) 'dist ',dist(2)
            call logmes(cout, .true.)
            write(cout,*) 'n    ',normal(:,2)
            call logmes(cout, .true.)
         endif  
        
       else if (e_inside) then

         edgetoface_distance_wp = 1
         nb_ptc = 2   
         ptc(:,2) = pcoor1e(:)
         dist(2) = dist1e
         normal(:,2) = normal_in                  

         ! if (dot_product(normal(:,1),normal_in) < 0.d0) then
         !   dist(2) = -dist(2)
         !   normal(:,2) = -normal_in      
         ! endif

         if (bavard) then
           write(cout,*) ' e inside'
           call logmes(cout, .true.)
           write(cout,*) 'ptc  ',ptc(:,2)
           call logmes(cout, .true.)
           write(cout,*) 'dist ',dist(2)
           call logmes(cout, .true.)
           write(cout,*) 'n    ',normal(:,2)
           call logmes(cout, .true.)
         endif  
         
       else
         ! cas ou  ca passe par un coin  
         edgetoface_distance_wp = -1
         nb_ptc = 1   
       endif

    else if (nb_cross == -1) then
       ! cas bord //
       edgetoface_distance_wp = -2
       nb_ptc = 2    
       !fd on delegue le calcul des intersections

       ! cafouillage dans la gestion des signes
       ! 
       
       ! icheck = edgetoedge_distance(coor1b,coor1e, &
       !                              coor2(:,ie_para),coor2(:,modulo(ie_para,nbs)+1), &
       !                              ntr,l_nb_ptc,l_ptc,l_n,l_dist,err_)
       
       icheck = edgetoedge_distance(coor1b,coor1e, &
                                    coor2(:,ie_para),coor2(:,modulo(ie_para,nbs)+1), &
                                    normal_in,l_nb_ptc,l_ptc,l_n,l_dist,err_)
       
       if ( icheck /= 2 .or. err_ > 0) then
          call logmes('Error in '//IAM//' : parallel intersection expected',.true.)
          err= 1
          return
       endif   
          
       ptc(:,1) = l_ptc(:,1)
       dist(1)  = l_dist(1)
       normal(:,1) = l_n(:,1)
       !normal(:,1) = sign(l_n(:,1),dot_product(l_n(:,1),ntr))       

       ptc(:,2) = l_ptc(:,2)
       dist(2)  = l_dist(2)
       normal(:,2) = l_n(:,2)
       !normal(:,2) = sign(l_n(:,2),dot_product(l_n(:,2),ntr))       

       if (bavard) then
         write(cout,*) 'edge - bord //'
         call logmes(cout, .true.)
         write(cout,*) 'ptc  ',ptc(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'dist ',dist(1)
         call logmes(cout, .true.)
         write(cout,*) 'n    ',normal(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'ptc  ',ptc(:,2)
         call logmes(cout, .true.)
         write(cout,*) 'dist ',dist(2)
         call logmes(cout, .true.)
         write(cout,*) 'n    ',normal(:,2)
         call logmes(cout, .true.)
       endif      
     endif

     !fd dedans
     
     if (b_inside .and. e_inside) then

       edgetoface_distance_wp = 1
       nb_ptc = 2
       ptc(:,1) = pcoor1b(:)
       dist(1) = dist1b
       normal(:,1)=normal_in

       ! if (dot_product(normal(:,1),ntr) < 0.d0) then
       !   dist(1) = -dist(1)
       !   normal(:,1) = -normal(:,1)      
       ! endif

       if (bavard) then
         write(cout,*) ' b inside'
         call logmes(cout, .true.)
         write(cout,*) 'ptc  ',ptc(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'dist ',dist(1)
         call logmes(cout, .true.)
         write(cout,*) 'n    ',normal(:,1)
         call logmes(cout, .true.)
       endif  

       ptc(:,2) = pcoor1e(:)
       dist(2) = dist1e
       normal(:,2)=normal_in

       ! if (dot_product(normal(:,2),ntr) < 0.d0) then
       !   dist(2) = -dist(2)
       !   normal(:,2) = -normal(:,2)      
       ! endif

       if (bavard) then 
         write(cout,*) ' e inside'
         call logmes(cout, .true.)
         write(cout,*) 'ptc  ',ptc(:,2)
         call logmes(cout, .true.)
         write(cout,*) 'dist ',dist(2)
         call logmes(cout, .true.)
         write(cout,*) 'n    ',normal(:,2)
         call logmes(cout, .true.)
       endif  

    endif

  end function

  !> \brief Look for projection of point on the face of an element using form functions
  subroutine find_proximal(i_interp,iface,elem,point,itmax,tol,proj,iter,norm,err)
    implicit none
    !> interpolation id
    integer(kind=4), intent(in) :: i_interp
    !> index of face on which to project
    integer(kind=4), intent(in) :: iface
    !> element coordinates (absolut frame)
    real(kind=8), dimension(:,:), intent(in) :: elem
    !> point cooridnates (absolute frame)
    real(kind=8), dimension(3)   , intent(in) :: point
    !> maximum number of iterationin Newton procedure
    integer(kind=4), intent(in) :: itmax
    !> maximum error allowed
    real(kind=8)   , intent(in) :: tol
    !> coordinate of projection found (absolute frame)
    real(kind=8), dimension(3), intent(out) :: proj
    !> number of iteration done
    integer(kind=4), intent(out):: iter
    !> value of the norm
    real(kind=8)   , intent(out):: norm
    !
    integer                     :: err
    !
    logical ::  conv
    integer(kind=4) :: i, j, k, ij
    real(kind=8) :: det, kval ! ca va vite
    real(kind=8), dimension(3) :: l, d1, d2, dd1, dd2, d1d2, lhs, rproj
    real(kind=8), dimension(2) :: rhs, delta, sol
    real(kind=8), dimension(:)  , pointer :: N   => null()
    real(kind=8), dimension(:,:), pointer :: DN  => null()
    real(kind=8), dimension(:,:), pointer :: DDN => null()

    err = 0
    
    ! solution of minimization problem
    sol = 0.d0

    select case(i_interp)
    case(i_H_P1,i_H_P2)
      select case(iface)
      case(1) !rear (x-)
        i = 2; j = 3; k = 1; ij = 6
        kval = -1.d0
      case(2) !front (x+)
        i = 2; j = 3; k = 1; ij = 6
        kval =  1.d0
      case(3) !left (y-)
        i = 1; j = 3; k = 2; ij = 5
        kval = -1.d0
      case(4) !right (y+)
        i = 1; j = 3; k = 2; ij = 5
        kval =  1.d0
      case(5) !bottom (z-)
        i = 1; j = 2; k = 3; ij = 4
        kval = -1.d0
      case(6) !top (z+)
        i = 1; j = 2; k = 3; ij = 4
        kval =  1.d0
      case default
      end select
    end select

    iter = 0
    conv = .false.
    do while(.not. conv)
 
      iter = iter + 1

      ! putting sol in the projection point in reference element
      rproj(i) = sol(1)
      rproj(j) = sol(2)
      rproj(k) = kval

      ! computing corresponding form functions and derivatives
      call  fonct_forme(i_interp, rproj, N)
      call derive_forme(i_interp, rproj, DN)
      call second_forme(i_interp, rproj, DDN)

      ! compute lhs
      l    = point - matmul(elem,N)
      d1   = matmul(elem,DN(i,:))
      d2   = matmul(elem,DN(j,:))
      dd1  = matmul(elem,DDN(:,i))
      dd2  = matmul(elem,DDN(:,j))
      d1d2 = matmul(elem,DDN(:,ij))

      lhs(1) = dot_product(l,dd1)  - dot_product(d1,d1)
      lhs(2) = dot_product(l,dd2)  - dot_product(d2,d2)
      lhs(3) = dot_product(l,d1d2) - dot_product(d1,d2)
      
      ! compute rhs
      rhs(1) = -dot_product(l,d1)
      rhs(2) = -dot_product(l,d2)

      ! solve system (since it is a 2x2 system, done by hand)
      det = lhs(1)*lhs(2) - lhs(3)*lhs(3)
      
      if( abs( det ) < 1.e-12 ) then
        call logmes('Error in DiscreteGeometry::find_proximal : non invertible system',.true.)
        err = 1
        return
      end if
      delta(1) = ( lhs(2)*rhs(1) - lhs(3)*rhs(2) ) / ( det )
      delta(2) = ( lhs(1)*rhs(2) - lhs(3)*rhs(1) ) / ( det )

      !\todo : out of bounds management !

      sol = sol + delta
      norm = dot_product( delta, delta )

      if( norm  < tol   ) conv = .true.
      if( iter > itmax ) exit

    end do

    if ( .not. conv ) then
      call logmes('Error in DiscreteGeometry::find_proximal : Newton not converged when computing projection on element',.true.)
      err = 1
      return
    end if

    ! putting sol in the point in reference element
    rproj(i) = sol(1)
    rproj(j) = sol(2)
    rproj(k) = kval

    ! getting projection point in reference element in absolute frame
    call fonct_forme(i_interp, rproj, N)
    proj = matmul(elem,N)

    if( associated(N) ) then
      deallocate(N)
      nullify(N)
    end if

    if( associated(DN) ) then
      deallocate(DN)
      nullify(DN)
    end if

    if( associated(DN) ) then
      deallocate(DDN)
      nullify(DDN)
    end if

  end subroutine

end module DiscreteGeometry
