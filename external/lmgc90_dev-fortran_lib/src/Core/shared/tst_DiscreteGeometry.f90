program test

  use DiscreteGeometry

  use algebra

  implicit none

  ! number of wrong test
  integer(kind=4) :: nb_err

  nb_err = 0

  call test_proj

  call test_convexhull

  call test_HE

  call test_node_surface

  call test_node_surface_new

  call test_nodetonode

  call test_nodetoedge

  call test_nodetoface

  call test_edgetoedge

  call test_edgetoface

  call test_edgetoface_wp

  call test_segments_intersection_wp

  !rm: RIP 21 aout 2024
  !call test_polytopes_intersection
  !call test_polytopes_intersection_wp

  call test_find_proximal(nb_err)

  if( nb_err > 0 ) stop 1

contains     

  subroutine test_proj
    implicit none

    real(kind=8) :: vl1(3,3),dir(3),p(3),norme,pp(3)
    real(kind=8) :: lambda,weight(3)
    integer :: i
    logical :: is_inside

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de node_triangle_projection'


    vl1(:,1) = (/0.d0, 0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.75d0, 0.d0 /)
    vl1(:,3) = (/0.75d0, 1.d0, 0.d0 /)

    dir = (/ 0.1, 0., -1. /)  
    p   = (/ 0.5, 0.5, 0.3 /)  

    norme = length3(dir)
    dir = dir / norme

    is_inside=node_triangle_projection(vl1,p,dir,lambda,weight,.true.)

    if (is_inside) then
       print*,' '
       print*,'dedans'
       print*,'distance ',lambda
       print*,'poids    ',weight

       pp(:) = weight(1)*vl1(:,1) + weight(2)*vl1(:,2) + weight(3)*vl1(:,3) 

       print*,pp

       do i=1,3
          print*, ((vl1(1,modulo(i,3)+1) - vl1(1,i))*(pp(2) - vl1(2,i))) - &
               ((vl1(2,modulo(i,3)+1) - vl1(2,i))*(pp(1) - vl1(1,i)))
       enddo

    else
       print*,' '
       print*,'dehors'
    endif


    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine test_proj

  subroutine test_convexhull
    implicit none

    real(kind=8) :: vl1(2,10),ref_size
    integer      :: ivl1(10),i
    
    real(kind=8) :: vl2(2,6)
    integer      :: ivl2(6)

    integer      :: err

    !real(kind=8),dimension(:,:),pointer :: points

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de convexhull'


    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,10) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/0.5d0,5.d0 /)
    vl1(:,7) = (/1.d0, 1.d0 /)
    vl1(:,5) = (/0.5d0, 0.75d0 /)
    vl1(:,6) = (/0.5d0, 0.75d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)
    vl1(:,8) = (/0.00001d0, 1.d0 /)
    vl1(:,9) = (/0.1d0, 0.75d0 /)
    vl1(:,2) = (/0.1d0, 0.75d0 /)

    ivl1 = 0

    call convex_hull(vl1,ivl1,0.001d0,err)

    if (err > 0) then
      print*,'Error in convex hull'
      stop 
    endif   

    do i=1,10
       if (ivl1(i) == 0) cycle
       print*,ivl1(i),vl1(:,ivl1(i))
    enddo

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'new test de convexhull'

    vl2(:,1) = (/ -0.2360662D+00,  0.3705738D+00 /)
    vl2(:,2) = (/  0.0000000D+00, -0.2360662D+00 /)
    vl2(:,3) = (/  0.3705738D+00,  0.0000000D+00 /)
    vl2(:,4) = (/ -0.2360662D+00,  0.3705738D+00 /)
    vl2(:,5) = (/  0.0000000D+00, -0.2360662D+00 /)
    vl2(:,6) = (/  0.3705738D+00,  0.0000000D+00 /)

    ivl2 = 0

    !call convex_hull(vl2,ivl2,0.001d0)
    call convex_hull(vl2,ivl2,3.1078783461667373D-003,err)
    
    if (err > 0) then
      print*,'Error in convex hull'
      stop 
    endif   

    
    do i=1,6
       if (ivl2(i) == 0) cycle
       print*,ivl2(i),vl2(:,ivl2(i))
    enddo


    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'    
    
  end subroutine test_convexhull

  subroutine test_HE
    implicit none
    real(kind=8),pointer :: vertex(:,:)
    integer :: face(3),b
    type(T_HE_Hdl) :: HE_Hdl,HE_Hdl2  
    type(T_HE) :: HE,HE_aux
    integer :: err

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de gestion HE         '

    ! un tetraedre

    print*,'on charge un tetraedre'

    allocate(vertex(3,4))
    vertex(:,1) = (/ 0.3830166D+04, -0.1562217D+05, 0.1225000D+04/)
    vertex(:,2) = (/ 0.3895424D+04, -0.1553386D+05, 0.1225000D+04/)
    vertex(:,3) = (/ 0.3963252D+04, -0.1559586D+05, 0.1132322D+04/)
    vertex(:,4) = (/ 0.3869883D+04, -0.1559636D+05, 0.1143730D+04/)

    HE_Hdl = new_HE_Hdl(4,4,4)

    face = (/ 3 ,  2 , 1 /)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 1 ,  2 , 4 /)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 2 ,  3 , 4 /)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 3 ,  1 , 4 /)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    call build_HE_Hdl(HE_Hdl,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   
 
    if (.not. associated(HE_Hdl%B2opHE)) then
       print*,'pas de bords'
    else
       print*,size(HE_Hdl%B2opHE),' bords'
    endif


    ! un carre avec un trou carre

    print*,'on charge un carre troue'

    deallocate(vertex)
    allocate(vertex(3,8))
    vertex(:,1) = (/ 0.d0, 0.d0, 0.d0 /)
    vertex(:,2) = (/ 1.d0, 0.d0, 0.d0 /)
    vertex(:,3) = (/ 1.d0, 1.d0, 0.d0 /)
    vertex(:,4) = (/ 0.d0, 1.d0, 0.d0 /)
    vertex(:,5) = (/ 0.3d0, 0.3d0, 0.d0 /)
    vertex(:,6) = (/ 0.7d0, 0.3d0, 0.d0 /)
    vertex(:,7) = (/ 0.7d0, 0.7d0, 0.d0 /)
    vertex(:,8) = (/ 0.3d0, 0.7d0, 0.d0 /)

    HE_Hdl2 = new_HE_Hdl(8,8,4)

    face = (/ 1 ,  6 , 5 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1 ,  2 , 6 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 2 ,  7 , 6 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 2 ,  3 , 7 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 3 ,  4 , 8 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 3 ,  8 , 7 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 4 ,  1 , 5 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 4 ,  5 , 8 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    call build_HE_Hdl(HE_Hdl2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   
 
    if (.not. associated(HE_Hdl2%B2opHE)) then
       print*,'pas de bord'
    else
       print*,size(HE_Hdl2%B2opHE),' bords'
       do b=1,size(HE_Hdl2%B2opHE)
       
         HE_aux=next_HE(HE_Hdl2%B2opHE(b), err)
         if (err > 0) then
           print*,'failed'
           stop
         endif   

         print*,'bord ',b,'appartient face',HE_Hdl2%B2opHE(b)%f 
         print*,'appuye sur ',HE_Hdl2%faces(HE_Hdl2%B2opHE(b)%i,HE_Hdl2%B2opHE(b)%f), &
                ' et '       ,HE_Hdl2%faces(HE_aux%i,HE_aux%f) 

       enddo

    endif

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine

  subroutine test_node_surface
    implicit none
    real(kind=8),pointer :: vertex(:,:)
    real(kind=8)         :: cd(3),dir(3),gap,point(3),t(3),n(3),s(3),ori(3),weight(3)
    integer :: face(3),status,ppp
    type(T_HE_Hdl) :: HE_Hdl, HE_Hdl2
    type(T_HE) :: HE,HE_aux
    integer :: i,f,c,b,ic
    integer :: err,err_
    character(len=7):: IAM='test_node_surface'

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de detection node face'

    ! un cube

    print*,'***test sur un cube***'

    ori = (/ 0.5, 0.5, 0.5 /)

    allocate(vertex(3,8))

    vertex(:,1) = (/ 0. , 0. , 0. /)  
    vertex(:,2) = (/ 1. , 0. , 0. /)  
    vertex(:,3) = (/ 1. , 1. , 0. /)  
    vertex(:,4) = (/ 0. , 1. , 0. /)  
    vertex(:,5) = (/ 0. , 0. , 1. /)  
    vertex(:,6) = (/ 1. , 0. , 1. /)  
    vertex(:,7) = (/ 1. , 1. , 1. /)  
    vertex(:,8) = (/ 0. , 1. , 1. /)  

    HE_Hdl = new_HE_Hdl(8,12,10)

    face = (/ 1, 2,  3 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 3,  4 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 1, 2,  6 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 6,  5 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 2, 3,  7 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 2, 7,  6 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 4,  8 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 8,  5 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 3, 4,  8 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 3, 8,  7 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 5, 7,  8 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif
   
    face = (/ 5, 6,  7 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    call build_HE_Hdl(HE_Hdl,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    call update_HE_Hdl(HE_Hdl,vertex,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   


    ! *** noeud - face
    print*,'****noeud face'
    cd = (/ 0.5, 0.5 , 1.5 /)
    dir = (/ 0. , 0., -1. /)

    ppp = 0

    status = node_HE_Hdl_proximity(HE_Hdl,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status
    print*,gap
    print*,point
    print*,t
    print*,n
    print*,s

    ! *** noeud - arete
    print*,'****noeud arete'
    cd = (/ 1.5, 0.5 , 1.5 /)
    dir = (/ -1. , 0., -1. /)

    ppp = 0

    status = node_HE_Hdl_proximity(HE_Hdl,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status
    print*,gap
    print*,point
    print*,t
    print*,n
    print*,s

    ! *** noeud - coin
    print*,'****noeud coin'
    cd = (/ 1.5, 1.5 , 1.5 /)
    dir = (/ -1. , -1., -1. /)

    ppp = 0

    status = node_HE_Hdl_proximity(HE_Hdl,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status
    print*,gap
    print*,point
    print*,t
    print*,n
    print*,s

    ! un carre avec un trou carre

    print*,'***test carre troue***'

    deallocate(vertex)
    allocate(vertex(3,8))
    vertex(:,1) = (/ 0.d0, 0.d0, 0.d0 /)
    vertex(:,2) = (/ 1.d0, 0.d0, 0.d0 /)
    vertex(:,3) = (/ 1.d0, 1.d0, 0.d0 /)
    vertex(:,4) = (/ 0.d0, 1.d0, 0.d0 /)
    vertex(:,5) = (/ 0.3d0, 0.3d0, 0.d0 /)
    vertex(:,6) = (/ 0.7d0, 0.3d0, 0.d0 /)
    vertex(:,7) = (/ 0.7d0, 0.7d0, 0.d0 /)
    vertex(:,8) = (/ 0.3d0, 0.7d0, 0.d0 /)

    HE_Hdl2 = new_HE_Hdl(8,8,4)

    face = (/ 1 ,  6 , 5 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1 ,  2 , 6 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 2 ,  7 , 6 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 2 ,  3 , 7 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 3 ,  4 , 8 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 3 ,  8 , 7 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 4 ,  1 , 5 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 4 ,  5 , 8 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    call build_HE_Hdl(HE_Hdl2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   
 
    call update_HE_Hdl(HE_Hdl2,vertex,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'orientation HE'
    do f=1,8
      do i=1,3
        print*,HE_hdl2%HEorien(:,i,f)  
      enddo
    enddo

    !fd si besoin de comprendre comment c est fichu
    
    ! print*,'contours'
    ! print*,size(HE_Hdl2%cnts)
    ! do c=1,size(HE_Hdl2%cnts)
    !   print*,'=================='
    !   write(6,*) 'contour ',c
    !   do ic=1,size(HE_Hdl2%cnts(c)%G_i)
    !     b = HE_Hdl2%cnts(c)%G_i(ic)
    !     write(6,*) 'rank ',ic,' borded ',b,' vertex ',HE_Hdl2%faces(HE_Hdl2%B2opHE(b)%i,HE_Hdl2%B2opHE(b)%f)
    !   enddo
    ! enddo

    ! do c=1,HE_Hdl2%nb_vertex
    !   write(6,*) '=================='
    !   write(6,*) 'HE linked to vertex ',c
    !   print*,HE_Hdl2%vertex(:,c)
    !   HE=HE_Hdl2%V2HE(c)
    !   write(6,*) 'rank: ',HE%i,' face ',HE%f
    !   print*,HE_Hdl2%faces(:,HE%f)
    !   if (HE%i == 0) then
    !     write(6,*) 'HE opposed to boundary'
    !     HE_aux = HE_Hdl2%B2opHE(HE%f)
    !     write(6,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !     write(6,*) 'staring vertex ',HE_Hdl2%faces(HE_aux%i,HE_aux%f)
    !     HE_aux = next_HE(HE_aux,err_)
    !     write(6,*) 'ending vertex ',HE_Hdl2%faces(HE_aux%i,HE_aux%f)
    !   else
    !     write(6,*) 'HE opposed to HE'
    !     HE_aux = HE_Hdl2%HE2opHE(HE%i,HE%f)
    !     write(6,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !     print*,HE_Hdl2%faces(:,HE_aux%f)
    !   endif
    !   !
    !   if (HE%i == 0) then
    !     HE_aux = HE_Hdl2%B2opHE(HE%f)
    !     HE_aux = previous_HE(HE_aux,err_)
    !     write(6,*) 'previous HE'
    !     write(6,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !     if (HE_aux%i == 0) then
    !       write(6,*) 'Error '//IAM//': not possible'
    !       err=1
    !       return
    !     endif
    !     write(6,*) 'opposed HE'
    !     HE_aux = HE_Hdl2%HE2opHE(HE_aux%i,HE_aux%f)
    !     write(6,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !   else
    !    HE_aux = previous_HE(HE,err_)
    !    write(6,*) 'previous HE'
    !    write(6,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !    if (HE_aux%i == 0) then
    !       write(6,*) 'Error '//IAM//': not possible'
    !       err= 1
    !       return
    !    endif
    !    write(6,*) 'opposed HE'
    !    HE_aux = HE_Hdl2%HE2opHE(HE_aux%i,HE_aux%f)
    !    write(6,*) 'rank: ',HE_aux%i,' face ',HE_aux%f
    !   endif
    ! enddo


    
    ! *** noeud - face
    print*,'****noeud dedans'
    cd = (/ 0.2d0, 0.2d0 , 1.5d0 /)
    dir = (/ 0.d0 , 0.d0, -1.d0 /)
    ppp = 0

    status = node_HE_Hdl_proximity(HE_Hdl2,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   
    
    print*,'point de depart: ',cd
    print*,'statut: ',status
    print*,'gap : ',gap
    print*,'point sur la surface : ',point
    print*,'t : ',t
    print*,'n : ',n
    print*,'s : ',s
    print*,' '

    print*,'****noeud trou'
    cd = (/ 0.4d0, 0.4d0 , 1.5d0 /)
    dir = (/ 0.d0 , 0.d0, -1.d0 /)
    ppp = 0

    status = node_HE_Hdl_proximity(HE_Hdl2,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'point de depart: ',cd
    print*,'statut: ',status
    print*,'gap : ',gap
    print*,'point sur la surface : ',point
    print*,'t : ',t
    print*,'n : ',n
    print*,'s : ',s
    print*,' '
    
    print*,'****noeud dehors'
    cd = (/-0.4d0,-0.4d0 , 1.5d0 /)
    dir = (/ 0.d0 , 0.d0, -1.d0 /)
    ppp = 0

    status = node_HE_Hdl_proximity(HE_Hdl2,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'point de depart: ',cd
    print*,'statut: ',status
    print*,'gap : ',gap
    print*,'point sur la surface : ',point
    print*,'t : ',t
    print*,'n : ',n
    print*,'s : ',s

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine test_node_surface
  
  subroutine test_node_surface_new
    implicit none
    real(kind=8),pointer :: vertex(:,:)
    real(kind=8)         :: cd(3),dir(3),gap,point(3),t(3),n(3),s(3),ori(3),weight(3)
    integer :: face(3),status,ppp
    type(T_HE_Hdl) :: HE_Hdl, HE_Hdl2  
    integer :: i,f
    integer :: err

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de detection node face'

    ! un cube

    print*,'***test sur un cube***'

    ori = (/ 0.5, 0.5, 0.5 /)

    allocate(vertex(3,8))

    vertex(:,1) = (/ 0. , 0. , 0. /)  
    vertex(:,2) = (/ 1. , 0. , 0. /)  
    vertex(:,3) = (/ 1. , 1. , 0. /)  
    vertex(:,4) = (/ 0. , 1. , 0. /)  
    vertex(:,5) = (/ 0. , 0. , 1. /)  
    vertex(:,6) = (/ 1. , 0. , 1. /)  
    vertex(:,7) = (/ 1. , 1. , 1. /)  
    vertex(:,8) = (/ 0. , 1. , 1. /)  

    HE_Hdl = new_HE_Hdl(8,12,10)

    face = (/ 1, 2,  3 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 3,  4 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 1, 2,  6 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 6,  5 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 2, 3,  7 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 2, 7,  6 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 4,  8 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1, 8,  5 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 3, 4,  8 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 3, 8,  7 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 5, 7,  8 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif
   
    face = (/ 5, 6,  7 /)
    call orien_face(ori,face,vertex)
    call settle_HE_Hdl(HE_Hdl,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    call build_HE_Hdl(HE_Hdl,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    call update_HE_Hdl(HE_Hdl,vertex,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   


    ! *** noeud - face
    print*,'****noeud face'
    cd = (/ 0.5, 0.5 , 1.5 /)
    dir = (/ 0. , 0., -1. /)

    ppp = 0

    status = new_node_HE_Hdl_proximity(HE_Hdl,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status
    print*,gap
    print*,point
    print*,t
    print*,n
    print*,s
    print*,' '

    ! *** noeud - arete
    print*,'****noeud arete'
    cd = (/ 1.5, 0.5 , 1.5 /)
    dir = (/ -1. , 0., -1. /)

    ppp = 0

    status = new_node_HE_Hdl_proximity(HE_Hdl,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status
    print*,gap
    print*,point
    print*,t
    print*,n
    print*,s
    print*,' '

    ! *** noeud - coin
    print*,'****noeud coin'
    cd = (/ 1.5, 1.5 , 1.5 /)
    dir = (/ -1. , -1., -1. /)

    ppp = 0

    status = new_node_HE_Hdl_proximity(HE_Hdl,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status
    print*,gap
    print*,point
    print*,t
    print*,n
    print*,s
    print*,' '
    
    ! un carre avec un trou carre

    print*,'***test carre troue***'

    deallocate(vertex)
    allocate(vertex(3,8))
    vertex(:,1) = (/ 0.d0, 0.d0, 0.d0 /)
    vertex(:,2) = (/ 1.d0, 0.d0, 0.d0 /)
    vertex(:,3) = (/ 1.d0, 1.d0, 0.d0 /)
    vertex(:,4) = (/ 0.d0, 1.d0, 0.d0 /)
    vertex(:,5) = (/ 0.3d0, 0.3d0, 0.d0 /)
    vertex(:,6) = (/ 0.7d0, 0.3d0, 0.d0 /)
    vertex(:,7) = (/ 0.7d0, 0.7d0, 0.d0 /)
    vertex(:,8) = (/ 0.3d0, 0.7d0, 0.d0 /)

    HE_Hdl2 = new_HE_Hdl(8,8,4)

    face = (/ 1 ,  6 , 5 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   
    
    face = (/ 1 ,  2 , 6 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 2 ,  7 , 6 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 2 ,  3 , 7 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 3 ,  4 , 8 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 3 ,  8 , 7 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 4 ,  1 , 5 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    face = (/ 4 ,  5 , 8 /)
    call settle_HE_Hdl(HE_Hdl2,face,err)
    if (err > 0) then
      print*,'Error in settle_HE_Hdl'
      stop 
    endif   

    call build_HE_Hdl(HE_Hdl2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   
 
    call update_HE_Hdl(HE_Hdl2,vertex,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'orientation HE'
    do f=1,8
      do i=1,3
        print*,HE_hdl2%HEorien(:,i,f)  
      enddo
    enddo

    
    ! *** noeud - face
    print*,'****noeud dedans'
    cd = (/ 0.2d0, 0.2d0 , 1.5d0 /)
    dir = (/ 0.d0 , 0.d0, -1.d0 /)
    ppp = 0

    status = new_node_HE_Hdl_proximity(HE_Hdl2,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   
    
    print*,'point de depart: ',cd
    print*,'statut: ',status
    print*,'gap : ',gap
    print*,'point sur la surface : ',point
    print*,'t : ',t
    print*,'n : ',n
    print*,'s : ',s
    print*,' '

    print*,'****noeud trou'
    cd = (/ 0.4d0, 0.4d0 , 1.5d0 /)
    dir = (/ 0.d0 , 0.d0, -1.d0 /)
    ppp = 0

    status = new_node_HE_Hdl_proximity(HE_Hdl2,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'point de depart: ',cd
    print*,'statut: ',status
    print*,'gap : ',gap
    print*,'point sur la surface : ',point
    print*,'t : ',t
    print*,'n : ',n
    print*,'s : ',s
    print*,' '

    print*,'****noeud dehors'
    cd = (/-0.4d0,-0.4d0 , 1.5d0 /)
    dir = (/ 0.d0 , 0.d0, -1.d0 /)
    ppp = 0

    status = new_node_HE_Hdl_proximity(HE_Hdl2,cd,10.d0,dir,.true.,ppp,gap,point,t,n,s,f,weight,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'point de depart: ',cd
    print*,'statut: ',status
    print*,'gap : ',gap
    print*,'point sur la surface : ',point
    print*,'t : ',t
    print*,'n : ',n
    print*,'s : ',s

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine test_node_surface_new

  subroutine orien_face(center,face,vertex)
    implicit none
    real(kind=8) :: vertex(:,:)
    real(kind=8) :: center(3),d1(3),d2(3),vv(3)   
    integer :: face(3),itmp

    d1(:) = vertex(:,face(2)) - vertex(:,face(1))
    d2(:) = vertex(:,face(3)) - vertex(:,face(1))

    vv = cross_product(d1,d2)

    d1(:) =  vertex(:,face(1)) - center(:)
    
    if (dot_product(vv,d1) < 0.d0) then
      print*,'on permute'
      itmp = face(2) 
      face(2)=face(3)
      face(3)=itmp
    endif
  end subroutine


  subroutine test_nodetonode

    implicit none
    real(kind=8) :: coor1(3),coor2(3)
    real(kind=8) :: ntr(3),normal(3),ptc(3),dist
    integer (kind=4) :: status

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de nodetonode_distance'

    print*,'loin' 

    coor1 = (/ 0., 0., 1. /)      
    coor2 = (/ -1., -1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 

    call nodetonode_distance(coor1,coor2,ntr,ptc,normal,dist)

    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist

    print*,'colle' 

    coor1 = (/ -2., 0., 1. /)      
    coor2 = (/ -2., 0. , 1. /) 

    ntr = (/ 0., 0. , 1. /) 

    call nodetonode_distance(coor1,coor2,ntr,ptc,normal,dist)

    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
  end subroutine 

  subroutine test_nodetoedge

    implicit none
    real(kind=8) :: coor1(3),coor2(3,2)
    real(kind=8) :: ntr(3),normal(3),ptc(3),dist
    integer (kind=4) :: status
    integer :: err

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de nodetoedge_distance'

    print*,'au centre du edge' 

    coor1 = (/ 0., 0., 1. /)      
    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal = (/ 0., 0. , 1. /) 

    status = nodetoedge_distance(coor1,coor2(:,1),coor2(:,2),ntr,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist

    print*,'au bout du edge' 

    coor1 = (/ -2., 0., 1. /)      
    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal = (/ 0., 0. , 1. /) 

    status = nodetoedge_distance(coor1,coor2(:,1),coor2(:,2),ntr,ptc,normal,dist, err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist


    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
  end subroutine 


  subroutine test_edgetoedge
    implicit none

    real(kind=8) :: coor1b(3),coor1e(3),coor2b(3),coor2e(3)
    real(kind=8) :: normal(3,2),ptc(3,2),dist(2)
    integer (kind=4) :: status,nbc,i
    real(kind=8) :: l1,e1(3),l2,e2(3),ntr(3)
    integer :: err
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de edgetoedge_distance'


    ntr = (/ 0., 0., -1. /)

    coor1b = (/ 0., 0. , 0. /) 
    e1 = (/ 1., 0. , 0. /) 
    l1 = 1.
    coor1e = coor1b + l1*e1


    ! edge edge 1 point aux bouts

    coor2b = (/ 0., 0. , 1. /) 
    e2 = (/ 0., 1. , 0. /) 
    l2 = 3.
    coor2e = coor2b + l2*e2

    status = edgetoedge_distance(coor1b,coor1e,coor2b,coor2e,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
      print*,'failed'
      stop
    endif   

    print*,' edge edge '  
    print*,status,nbc 
    do i=1,nbc
      print*,i
      print*,'ptc',ptc(:,i)
      print*,'n  ',normal(:,i)
      print*,'d  ',dist(i)
    enddo

    ! edge - edge 1 point aux milieux

    coor2b = (/ 0.5, -0.5 , 1. /) 
    e2 = (/ 0., 1. , 0. /) 
    l2 = 3.
    coor2e = coor2b + l2*e2

    status = edgetoedge_distance(coor1b,coor1e,coor2b,coor2e,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,' edge edge '
    print*,status,nbc 
    do i=1,nbc
      print*,i
      print*,'ptc',ptc(:,i)
      print*,'n  ',normal(:,i)
      print*,'d  ',dist(i)
    enddo

    ! edge - edge 2 point

    coor2b = (/-0.5, -0.5 , 1. /) 
    e2 = e1
    l2 = 3.
    coor2e = coor2b + l2*e2

    status = edgetoedge_distance(coor1b,coor1e,coor2b,coor2e,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,' edge edge '
    print*,status,nbc 
    do i=1,nbc
      print*,i
      print*,'ptc',ptc(:,i)
      print*,'n  ',normal(:,i)
      print*,'d  ',dist(i)
    enddo

    ! node node

    coor2b = (/ -1., 0. , 1. /) 
    e2 = (/ 0., 1. , 0. /) 
    l2 = 3.
    coor2e = coor2b + l2*e2

    status = edgetoedge_distance(coor1b,coor1e,coor2b,coor2e,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,' node node '
    print*,status,nbc 
    do i=1,nbc
      print*,i
      print*,'ptc',ptc(:,i)
      print*,'n  ',normal(:,i)
      print*,'d  ',dist(i)
    enddo

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
  end subroutine 

  subroutine test_nodetoface

    real(kind=8) :: coor1(3),coor2(3,4)
    real(kind=8) :: ntr(3),normal(3),ptc(3),dist
    integer (kind=4) :: status
    integer :: err 
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de nodetoface_distance'

    print*,'au centre de la face' 

    coor1 = (/ 0., 0., 1. /)      
    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal = (/ 0., 0. , 1. /) 

    status = nodetoface_distance(coor1,coor2,ntr,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,' node face '
    print*,status 
    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist

    print*,'sur le bord de la face'

    coor1 = (/ 1.1, 0., 1. /)      
    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal = (/ 0., 0. , 1. /) 

    status = nodetoface_distance(coor1,coor2,ntr,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,' node face '
    print*,status 
    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist


    print*,'sur un coin de la face'

    coor1 = (/ 1.1, 1.1, 1. /)      
    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal = (/ 0., 0. , 1. /) 

    status = nodetoface_distance(coor1,coor2,ntr,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,' node face '
    print*,status 
    print*,'ptc',ptc
    print*,'n  ',normal
    print*,'d  ',dist


    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine

  subroutine test_edgetoface

    real(kind=8)     :: coor1(3,2),coor2(3,4)
    real(kind=8)     :: ntr(3),normal(3,2),ptc(3,2),dist(2)
    integer (kind=4) :: status,nbc
    integer          :: err

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de edgetoface_distance'

    print*,'-----------------------------'
    print*,'dans la face' 

    coor1(:,1) = (/ 0., 0., 1. /)      
    coor1(:,2) = (/ 0.5, 0., 1. /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' 0. 0. 0.'
    print*,' 0.5 0. 0.'

    print*,'-----------------------------'
    print*,'coupe un bord de la face'

    coor1(:,1) = (/ 0., 0., 1. /)      
    coor1(:,2) = (/ 1.5, 0., 1. /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /)     

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' 0. 0. 0.'
    print*,' 1. 0. 0.'
    
    print*,'-----------------------------'
    print*,'coupe deux bords de la face'

    coor1(:,1) = (/ -1.5, 0., 1. /)      
    coor1(:,2) = (/ 1.5, 0., 1. /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,'-1. 0. 0.'
    print*,' 1. 0. 0.'

    print*,'-----------------------------'
    print*,'coupe deux bords de la face'

    coor1(:,1) = (/  0.d0, 1.2d0, 1.d0 /)      
    coor1(:,2) = (/ 1.2d0, 0.d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /)     

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    !print*,'doit etre : '
    !print*,'-1. 0. 0.'
    !print*,' 1. 0. 0.'

    print*,'-----------------------------'
    print*,'coupe un noeud de la face'

    coor1(:,1) = (/ 0., 0., 1. /)      
    coor1(:,2) = (/ 1.1d0, 1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' 0. 0. 0.'
    print*,' 1. 1. 0.'

    print*,'-----------------------------'
    print*,'coupe deux noeuds de la face'

    coor1(:,1) = (/ -1.1d0, -1.1d0, 1.d0 /)      
    coor1(:,2) = (/ 1.1d0, 1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' -1. -1. 0.'
    print*,' 1. 1. 0.'
    
    print*,'-----------------------------'
    print*,'coupe deux noeuds de la face'

    coor1(:,1) = (/ -1.1d0, 1.1d0, 1.d0 /)      
    coor1(:,2) = (/ 1.1d0, -1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' -1. 1. 0.'
    print*,' 1. -1. 0.'

    print*,'-----------------------------'
    print*,'parallele au bord de la face'

    coor1(:,1) = (/ -1.d0, 1.1d0, 1.d0 /)      
    coor1(:,2) = (/ -1.d0, -1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err)

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' -1. 1. 0.'
    print*,' -1. -1. 0.'

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine


  subroutine test_edgetoface_wp

    real(kind=8) :: coor1(3,2),coor2(3,4)
    real(kind=8) :: ntr(3),normal(3,2),ptc(3,2),dist(2)
    integer (kind=4) :: status,nbc
    integer :: err

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test de edgetoface_distance_wp'

    print*,'-----------------------------'
    print*,'dans la face' 

    coor1(:,1) = (/ 0., 0., 1. /)      
    coor1(:,2) = (/ 0.5, 0., 1. /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /)     

    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' 0. 0. 0.'
    print*,' 0.5 0. 0.'

    print*,'-----------------------------'
    print*,'coupe un bord de la face'

    coor1(:,1) = (/ 0., 0., 1. /)      
    coor1(:,2) = (/ 1.5, 0., 1. /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /)     

    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' 0. 0. 0.'
    print*,' 1. 0. 0.'
   
    print*,'-----------------------------'
    print*,'coupe deux bords de la face'

    coor1(:,1) = (/ -1.5, 0., 1. /)      
    coor1(:,2) = (/ 1.5, 0., 1. /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /)     

    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,'-1. 0. 0.'
    print*,' 1. 0. 0.'

    print*,'-----------------------------'
    print*,'coupe deux bords de la face'

    coor1(:,1) = (/  0.d0, 1.2d0, 1.d0 /)      
    coor1(:,2) = (/ 1.2d0, 0.d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /)
    normal(:,2) = (/ 0., 0. , 1. /)     

    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    !print*,'doit etre : '
    !print*,'-1. 0. 0.'
    !print*,' 1. 0. 0.'

    print*,'-----------------------------'
    print*,'coupe un noeud de la face'

    coor1(:,1) = (/ 0., 0., 1. /)      
    coor1(:,2) = (/ 1.1d0, 1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 
    normal(:,2) = (/ 0., 0. , 1. /)
    
    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' 0. 0. 0.'
    print*,' 1. 1. 0.'
    
    print*,'-----------------------------'
    print*,'coupe deux noeuds de la face'

    coor1(:,1) = (/ -1.1d0, -1.1d0, 1.d0 /)      
    coor1(:,2) = (/ 1.1d0, 1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 
    normal(:,2) = (/ 0., 0. , 1. /)
    
    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)
    
    print*,'doit etre : '
    print*,' -1. -1. 0.'
    print*,' 1. 1. 0.'

    print*,'-----------------------------'
    print*,'coupe deux noeuds de la face'

    coor1(:,1) = (/ -1.1d0, 1.1d0, 1.d0 /)      
    coor1(:,2) = (/ 1.1d0, -1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 
    normal(:,2) = (/ 0., 0. , 1. /) 

    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' -1. 1. 0.'
    print*,' 1. -1. 0.'
    
    print*,'-----------------------------'
    print*,'parallele au bord de la face'

    coor1(:,1) = (/ -1.d0, 1.1d0, 1.d0 /)      
    coor1(:,2) = (/ -1.d0, -1.1d0, 1.d0 /)      

    coor2(:,1) = (/ -1., -1. , 0. /) 
    coor2(:,2) = (/  1., -1. , 0. /) 
    coor2(:,3) = (/  1.,  1. , 0. /) 
    coor2(:,4) = (/ -1.,  1. , 0. /) 

    ntr = (/ 0., 0. , 1. /) 
    normal(:,1) = (/ 0., 0. , 1. /) 
    normal(:,2) = (/ 0., 0. , 1. /)
    
    status = edgetoface_distance_wp(coor1(:,1),coor1(:,2),coor2,ntr,nbc,ptc,normal,dist,err,.true.)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,status 
    print*,'ptc 1',ptc(:,1)
    print*,'n 1 ',normal(:,1)
    print*,'d 1 ',dist(1)

    print*,'ptc 2',ptc(:,2)
    print*,'n 2 ',normal(:,2)
    print*,'d 2 ',dist(2)

    print*,'doit etre : '
    print*,' -1. 1. 0.'
    print*,' -1. -1. 0.'
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine

  
  subroutine test_segments_intersection_wp
    use predicates
    implicit none
    real(kind=8), dimension(2) :: p2a, p2b, p2c, p2d, p2, q2
    real(kind=8) :: tol
    integer(kind=4) :: code
    integer :: err
    
    tol = 1.0000001*f_exactinit()

    print*,'---------------------------------------'
    print*,' test_segments_intersection_wp         '
    print*,'---------------------------------------'    
    
    ! axes qui se croisent
       
    print*,'-----'
    print*,'intersection franche' 

    p2a = (/0.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
    p2c = (/0.5d0,-1.0d0  /); p2d = (/0.5d0,1.d0    /)

    print*,'new'
    code = segments_intersection_wp(p2a,p2b,p2c,p2d,p2,q2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'code = ',code,' prevue = 10000'
    print*,'point = ',p2,'prevue = 0.5 0.'

    print*,'old'
    code = segments_intersection(p2a,p2b,p2c,p2d,0.d0,p2)
    
    print*,'code = ',code,' prevue = 1'
    print*,'point = ',p2,'prevue = 0.5 0.'


    ! axes qui se croisent au bout

    print*,'-----'
    print*,'intersection autour de la tolerance et qui passe par un sommet'  
    
    p2a = (/0.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
    p2c = (/-10.0d0,-tol  /); p2d = (/10.d0,+tol    /)

    print*,'new'
    code = segments_intersection_wp(p2a,p2b,p2c,p2d,p2,q2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'code = ',code,' prevue = 11000'
    print*,'point = ',p2,'prevue = 0. 0.'
    
    print*,'old'
    code = segments_intersection(p2a,p2b,p2c,p2d,0.d0,p2)

    print*,'code = ',code,' prevue = 2'
    print*,'point = ',p2,'prevue = 0. 0.'
    
    ! axes qui ne se croisent pas

    print*,'-----'
    print*,'pas d intersection mais dans la tolerance'
    
    p2a = (/0.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
    p2c = (/-10.0d0,-tol  /); p2d = (/10.d0,-tol    /)

    print*,'new'
    code = segments_intersection_wp(p2a,p2b,p2c,p2d,p2,q2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'code = ',code,' prevue = 0'
    print*,'point = ',p2
    
    print*,'old'
    code = segments_intersection(p2a,p2b,p2c,p2d,0.d0,p2)


    print*,'code = ',code,' prevue = 0'
    print*,'point = ',p2

    ! axes //
    
    print*,'-----'
    print*,'parallele'
    
    p2a = (/0.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
    p2c = (/-10.0d0,0.d0  /); p2d = (/10.d0,0.d0    /)

    print*,'new'
    code = segments_intersection_wp(p2a,p2b,p2c,p2d,p2,q2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'code = ',code,' prevue = 21100'
    print*,'point = ',p2,'prevue = 0. 0.'
    print*,'point = ',q2
    
    print*,'old'
    code = segments_intersection(p2a,p2b,p2c,p2d,0.d0,p2)

    print*,'code = ',code,' prevue = -1'
    print*,'point = ',p2,'prevue = 0. 0.'

    ! axes //

    print*,'-----'
    print*,'parallele'   
    p2a = (/-11.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
    p2c = (/-10.0d0,0.d0  /); p2d = (/10.d0,0.d0    /)

    print*,'new'
    code = segments_intersection_wp(p2a,p2b,p2c,p2d,p2,q2,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,'code = ',code,' prevue = 20110'
    print*,'point = ',p2,'prevue = -10. 0.'
    print*,'point = ',q2
    
    print*,'old'
    code = segments_intersection(p2a,p2b,p2c,p2d,0.d0,p2)


    print*,'code = ',code,' prevue = -1'
    print*,'point = ',p2,'prevue = -10. 0.'
    

  end subroutine test_segments_intersection_wp

  subroutine test_find_proximal(nb_err)
    use a_EF, only : i_H_P1, i_H_P2
    implicit none
    !
    integer(kind=4), intent(inout) :: nb_err
    !
    real(kind=8)    :: norm
    integer(kind=4) :: iface, iter
    ! a quadratic hexahedron
    real(kind=8), dimension(3,20) :: hexa
    ! test points, projections and reference results (one for each face)
    real(kind=8), dimension(3,6)  :: point, proj, ref

    integer :: err

    ! linear nodes
    hexa(:,1) = (/ 0.d0, 0.d0, 0.d0 /)
    hexa(:,2) = (/ 1.d0, 0.d0, 0.d0 /)
    hexa(:,3) = (/ 1.d0, 1.d0, 0.d0 /)
    hexa(:,4) = (/ 0.d0, 1.d0, 0.d0 /)
    hexa(:,5) = (/ 0.d0, 0.d0, 1.d0 /)
    hexa(:,6) = (/ 1.d0, 0.d0, 1.d0 /)
    hexa(:,7) = (/ 1.d0, 1.d0, 1.d0 /)
    hexa(:,8) = (/ 0.d0, 1.d0, 1.d0 /)

    ! quadratic nodes
    hexa(:, 9) = ( hexa(:,1) + hexa(:,2) ) / 2.d0
    hexa(:,10) = ( hexa(:,2) + hexa(:,3) ) / 2.d0
    hexa(:,11) = ( hexa(:,3) + hexa(:,4) ) / 2.d0
    hexa(:,12) = ( hexa(:,4) + hexa(:,1) ) / 2.d0

    hexa(:,13) = ( hexa(:,5) + hexa(:,6) ) / 2.d0
    hexa(:,14) = ( hexa(:,6) + hexa(:,7) ) / 2.d0
    hexa(:,15) = ( hexa(:,7) + hexa(:,8) ) / 2.d0
    hexa(:,16) = ( hexa(:,8) + hexa(:,5) ) / 2.d0

    hexa(:,17) = ( hexa(:,1) + hexa(:,5) ) / 2.d0
    hexa(:,18) = ( hexa(:,2) + hexa(:,6) ) / 2.d0
    hexa(:,19) = ( hexa(:,3) + hexa(:,7) ) / 2.d0
    hexa(:,20) = ( hexa(:,4) + hexa(:,8) ) / 2.d0

    point(:,1) = (/-2.d0, 0.2d0, 0.2d0/)
    point(:,2) = (/ 2.d0, 0.3d0, 0.3d0/)
    point(:,3) = (/0.4d0, -2.d0, 0.4d0/)
    point(:,4) = (/0.4d0,  2.d0, 0.6d0/)
    point(:,5) = (/0.6d0, 0.6d0, -2.d0/)
    point(:,6) = (/0.7d0, 0.7d0,  2.d0/)

    ref = point
    ref(1,1) =  0.d0
    ref(1,2) =  1.d0
    ref(2,3) =  0.d0
    ref(2,4) =  1.d0
    ref(3,5) =  0.d0
    ref(3,6) =  1.d0

    ! linear
    do iface = 1, 6
      call find_proximal(i_H_P1, iface, hexa(:,1:8),point(:,iface),10,1.d-12,proj(:,iface),iter, norm, err)
      !print *, 'face : ', iface, iter, err
      if( any( abs(proj(:,iface)-ref(:,iface))>1.e-12 ) ) then
        print *, '[ERROR] Linear, wrong projections on face: ', iface, proj(:,iface), '/', ref(:,iface)
        nb_err = 1
      end if
    end do

    ! quadratic
    do iface = 1, 6
      call find_proximal(i_H_P2, iface, hexa(:,:),point(:,iface),10,1.d-12,proj(:,iface),iter, norm, err)
      !print *, 'face : ', iface, iter, err
      if( any( abs(proj(:,iface)-ref(:,iface))>1.e-12 ) ) then
        print *, '[ERROR] Quadratic, wrong projections on face: ', iface, proj(:,iface), '/', ref(:,iface)
        nb_err = 1
      end if
    end do

  end subroutine

end program test
