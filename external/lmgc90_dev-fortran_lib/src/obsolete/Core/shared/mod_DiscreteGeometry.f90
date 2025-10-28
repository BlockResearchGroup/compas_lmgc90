
!> compute the intersection of 2 coplanar polytopes
!> vl1,vl2 (I): array of vertices (1:m)
!> ivl1,ivl2 (I): list of valid vertices (1:n,n-1 <=m, ivl(1) == ivl(n))
!> vertices are stored anti-clockwise
!> ref_size (I): ref distance for the tests 
!> points the intersection (no double)
!> nb_ptc number of intersection points 
subroutine polytopes_intersection(vl1,ivl1,vl2,ivl2,ref_size,points,nb_ptc,bavard,err)
  implicit none
  real(kind=8)                        :: vl1(:,:),vl2(:,:),ref_size
  integer                             :: ivl1(:),ivl2(:)
  real(kind=8),dimension(:,:),pointer :: points
  integer                             :: err,err_ 
  !
  logical :: is_good
  integer :: nb_vertex1,nb_vertex2,inc,nb_ptc,k,m,i,check
  real(kind=8) :: listsup(2),scal1,lvec(2),tt,ss,denom,num, &
                  xcd_min,xcd_max,ycd_min,ycd_max, &
                  xan_min,xan_max,yan_min,yan_max, &
                  zero,ref_size2

  logical :: bavard

!fd ca me desespere de voir une MERDE pareille !!

  err = 0
  
  !fd tolerance pour declarer que 2 points sont au meme endroit
  !trop stricte zero=1.d-14
  zero=1.d-4

  ref_size2=ref_size*ref_size

  if (minval(ivl1) == 0) then
    nb_vertex1 = minloc(ivl1,dim=1) - 2
  else
    nb_vertex1 = size(ivl1) - 1
  endif

  if (minval(ivl2) == 0) then  
    nb_vertex2 = minloc(ivl2,dim=1) - 2
  else
    nb_vertex2 = size(ivl2) - 1 
  endif
  !
  !print*,nb_vertex1,nb_vertex2
  !
  if (associated(points)) deallocate(points)
  allocate(points(size(vl1,dim=1),size(vl1,dim=2)+size(vl2,dim=2)))
  nb_ptc=0

  !fd le cas 1 | 1 est vire ...
  IF (nb_vertex1 /= 1 .or. nb_vertex2 /= 1 ) then

    ! -->ici on ne fait que l intersections entre segments !! <---
    ! recherche de l'intersection entre les segments des deux contours
    ! tres tres nul car n**2 <- ajout test grossier pour essayer de virer des cas triviaux

    !print*,'vertex 1',nb_vertex1
    !print*,ivl1

    !print*,'vertex 2',nb_vertex2
    !print*,ivl2

    DO  k=1,nb_vertex2

      xan_min=min(vl2(1,ivl2(k)),vl2(1,ivl2(k+1))); xan_max=max(vl2(1,ivl2(k)),vl2(1,ivl2(k+1)))
      yan_min=min(vl2(2,ivl2(k)),vl2(2,ivl2(k+1))); yan_max=max(vl2(2,ivl2(k)),vl2(2,ivl2(k+1)))
     
      !print*,'segment de 2: ',ivl2(k),ivl2(k+1) 
      !print*,'x ',xan_min,xan_max
      !print*,'y ',yan_min,yan_max

      DO m=1,nb_vertex1

        !print*,'segment de 1: ',ivl1(m),ivl2(m+1) 

        ! test grossier boite englobante segments
        !
        xcd_min=min(vl1(1,ivl1(m)),vl1(1,ivl1(m+1))); xcd_max=max(vl1(1,ivl1(m)),vl1(1,ivl1(m+1)))
        if (xcd_max < xan_min .or. xcd_min > xan_max) cycle

        !print*,'x ',xcd_min,xcd_max

        ycd_min=min(vl1(2,ivl1(m)),vl1(2,ivl1(m+1))); ycd_max=max(vl1(2,ivl1(m)),vl1(2,ivl1(m+1)))
        if (ycd_max < yan_min .or. ycd_min > yan_max) cycle

        !print*,'y ',ycd_min,ycd_max
        
        check = segments_intersection(vl2(:,ivl2(k)),vl2(:,ivl2(k+1)), &
                                       vl1(:,ivl1(m)),vl1(:,ivl1(m+1)), &
                                       ref_size,listsup)
        !print*,'check=',check        

        if (check > 0) then
          is_good=.true.
          do i=1,nb_ptc
            lvec(1:2)=listsup(:) - points(:,i)
            if (dot_product(lvec,lvec) < zero*ref_size2) then 
               is_good=.false.
               exit
            endif
          enddo
          if (is_good) then
            nb_ptc=nb_ptc+1              
            points(:,nb_ptc)= listsup(:)
            !print*,nb_ptc,listsup
          endif
        endif
      ENDDO
    ENDDO
  ENDIF

  if (bavard) print*,'avec intersection segments ',nb_ptc,' contacts'

  ! recherche des sommets vl1 appartenant a l'enveloppe des vl2
  ! dedans si tous les produits vectoriels de meme signe 

  if (bavard) print*,'recherche 1 dans 2'
  DO m=1,nb_vertex1
    
    if (bavard) print*,'vertex ',m

    !fd old inc=0 

    !Soit nombre de points de l'enveloppe 2 est superieur ou egal a 3
    IF (nb_vertex2 >= 3) THEN
      !fd new 
      inc = node_in_polytope(vl1(:,ivl1(m)),vl2,ivl2,ref_size,bavard,err_)

      if (err_ > 0) then
        err= 1
        return 
      endif   
       
      if (inc==1) then    
        nb_ptc = nb_ptc+1
        points(:,nb_ptc)=vl1(:,ivl1(m))
        if (bavard) print*,'+ with 1 in 2 ',nb_ptc,' contacts'
      endif


      !fd old
!!$      DO  k=1,nb_vertex2
!!$
!!$        scal1 = (vl2(1,ivl2(k+1))-vl2(1,ivl2(k)))*(vl1(2,ivl1(m))-vl2(2,ivl2(k))) - &
!!$                (vl2(2,ivl2(k+1))-vl2(2,ivl2(k)))*(vl1(1,ivl1(m))-vl2(1,ivl2(k)))
!!$
!!$        if (bavard) print*,'produit vectoriel',scal1
!!$
!!$        ! Soit le produit vectoriel est positif
!!$
!!$        IF (scal1 >= -1.D-05 ) inc=inc+1
!!$
!!$        ! si on a fait le tour et tout est positif
!!$        ! on est dedans 
!!$        IF (inc==nb_vertex2) THEN
!!$          is_good=.true.
!!$          do i=1,nb_ptc
!!$            lvec(1:2)=vl1(:,ivl1(m)) - points(:,i)
!!$            if (dot_product(lvec,lvec) < zero*ref_size2) then 
!!$               is_good=.false.
!!$               exit
!!$            endif
!!$          enddo
!!$          if (is_good) then
!!$
!!$            !print*,'dedans sens +'
!!$            nb_ptc=nb_ptc+1
!!$            points(:,nb_ptc) = vl1(:,ivl1(m))
!!$          endif
!!$
!!$        ENDIF
!!$
!!$        if (bavard) print*,'+ avec 1 dans 2 ',nb_ptc,' contacts'
!!$
!!$      ENDDO

    ELSE
      !Soit nombre de points de l'enveloppe  est inferieur a 3
      ! i.e segment ou point

      DO  k=1,nb_vertex2

        lvec(1:2)=vl1(1:2,ivl1(m)) - vl2(1:2,ivl2(k))

        if (dot_product(lvec,lvec) < zero*ref_size2) THEN

          ! Recuperation de la liste des points candidats appartenant au contour de l'antagoniste

          is_good=.true.
          do i=1,nb_ptc
            lvec(1:2)=vl1(:,ivl1(m)) - points(:,i)
            if (dot_product(lvec,lvec) < zero*ref_size2) then 
               is_good=.false.
               exit
            endif
          enddo

          if (is_good) then
            nb_ptc=nb_ptc+1
            points(:,nb_ptc) = vl1(:,ivl1(m))
            !print*,'dedans sens -'
          endif
        ENDIF
      ENDDO

      !print*,'2 dans 1 same',nb_ptc

    ENDIF
  ENDDO
  ! Test dans l'autre sens
  ! pan appartenant a l'enveloppe des pcd

  if (bavard) print*,'recherche 2 dans 1'

  DO  k=1,nb_vertex2

    if (bavard) print*,'vertex ',k

    !fd new
    if (nb_vertex1 >= 3) then
      inc = node_in_polytope(vl2(:,ivl2(k)),vl1,ivl1,ref_size,bavard,err_)
      if (err_ > 0) then
        err= 1
        return
      endif  
      if (inc==1) then    
        nb_ptc = nb_ptc+1
        points(:,nb_ptc)=vl2(:,ivl2(k))
        if (bavard) print*,'+ avec 2 dans 1 ',nb_ptc,' contacts'
      endif

    endif
    !fd old
!!$    inc=0 
!!$    !Soit nombre de points de l'enveloppe est superieur ou egal a 3
!!$    IF (nb_vertex1 >= 3) THEN
!!$
!!$      DO m=1,nb_vertex1
!!$
!!$        scal1 = (vl1(1,ivl1(m+1))-vl1(1,ivl1(m)))*(vl2(2,ivl2(k))-vl1(2,ivl1(m))) - &
!!$                (vl1(2,ivl1(m+1))-vl1(2,ivl1(m)))*(vl2(1,ivl2(k))-vl1(1,ivl1(m)))
!!$
!!$        if (bavard) print*,'produit vectoriel ',scal1
!!$
!!$        ! Soit le produit vectoriel est positif
!!$        IF (scal1>=-1.D-05 ) inc=inc+1
!!$
!!$        ! si on a fait le tour et tout est positif
!!$        ! on est dedans 
!!$
!!$        IF (inc==nb_vertex1) THEN
!!$
!!$          is_good=.true.
!!$          do i=1,nb_ptc
!!$            lvec(1:2)=vl2(:,ivl2(k)) - points(:,i)
!!$            if (dot_product(lvec,lvec) < zero*ref_size2) then 
!!$               is_good=.false.
!!$               exit
!!$            endif
!!$          enddo
!!$          if (is_good) then
!!$
!!$           !print*,'dedans sens +'
!!$            nb_ptc=nb_ptc+1
!!$            points(:,nb_ptc)=vl2(:,ivl2(k))
!!$
!!$          endif
!!$
!!$        ENDIF
!!$
!!$        if (bavard) print*,'+ avec 2 dans 1 ',nb_ptc,' contacts'
!!$
!!$      ENDDO
!!$    ENDIF

  ENDDO

  !print*,'all points',nb_ptc

end subroutine

!> Compute the intersection of 2 coplanar convex polytopes (with predicates => wp)
!> Algorithm proposed in the book of O'Rourke, Computational Geometry
!> vl1,vl2 (I): array of vertices (1:m)
!> ivl1,ivl2 (I): list of valid vertices (1:n,n-1 <=m, ivl(1) == ivl(n))
!> vertices are stored anti-clockwise
!> points the intersection (no double)
!> nb_ptc number of intersection points 
subroutine polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_points,bavard,err)
  use predicates
  implicit none
  real(kind=8)                        :: vl1(:,:),vl2(:,:)
  integer                             :: ivl1(:),ivl2(:),nb_points
  real(kind=8),dimension(:,:),pointer :: points
  logical                             :: bavard
  integer                             :: err,err_
  !
  logical :: is_good
  ! nb vertex, id vertex courant, id vertex precedent,increment
  integer :: nb_vertex1,nb_vertex2,id1,id2,idp1,idp2,idn1,idn2,i1,i2,i,iii,sz_points
  real(kind=8) :: v1(2),v2(2),p(2),q(2),cross,c1Hc2,c2Hc1,zero
  integer(kind=4) :: code, inflag
  logical :: firstpoint,keep

                           !123456789012345678901234567890123456789012
  character(len=42) :: IAM='DiscreteGeometry_polytopes_intersection_wp'
  character(len=120):: cout

  err = 0
  
  if (minval(ivl1) == 0) then
    nb_vertex1 = minloc(ivl1,dim=1) - 2
  else
    nb_vertex1 = size(ivl1) - 1
  endif

  if (minval(ivl2) == 0) then  
    nb_vertex2 = minloc(ivl2,dim=1) - 2
  else
    nb_vertex2 = size(ivl2) - 1 
  endif
 
  ! on alloue en surdimensionnant 
  if (associated(points)) deallocate(points)
  sz_points=size(vl1,dim=2)+size(vl2,dim=2)
  allocate(points(size(vl1,dim=1),sz_points))

  ! le nombre de points de l'intersection
  nb_points=0

  !fd le cas 1 vertex | 1 vertex est vire ...
  IF (nb_vertex1 == 1 .and. nb_vertex2 == 1 ) return

  !
  if (bavard) then
    print*,'-------------------------------------------'
    print*,nb_vertex1,nb_vertex2
  endif
  !
  zero=f_exactinit()
  inflag=0
  firstpoint=.TRUE.
  !fd rang de l'arete du polytope 
  i1=0 ; i2=0
  !fd rang du sommet 
  id1=1 ; id2=1
  i=0

  !fd on tourne autour de chaque contour jusqu'a etre revenu au point de depart de chacun
  do
     if (bavard) print*,'------ it = ',i
     !wtf inflag=0     
     keep = .false.
     
     ! on recupere les points de depart
     idp1 = modulo(id1+nb_vertex1-2, nb_vertex1) + 1
     idp2 = modulo(id2+nb_vertex2-2, nb_vertex2) + 1

     if (bavard) print*,idp1,id1,idp2,id2

     ! on construit les aretes idp1->id1 et idp2->id2
     v1 = vl1(:,ivl1(id1)) - vl1(:,ivl1(idp1))
     v2 = vl2(:,ivl2(id2)) - vl2(:,ivl2(idp2))

     ! on va avancer sur le contour ayant l'extremite de l'arete courante en dehors du contour de l'arete  
     ! avec le predicat isleft ca veut dire < 0.d0
     ! c1 isleft bord c2
     c1Hc2=f_orient2d(vl2(:,ivl2(idp2)),vl2(:,ivl2(id2)),vl1(:,ivl1(id1)))
     ! c2 isleft bord c1
     c2Hc1=f_orient2d(vl1(:,ivl1(idp1)),vl1(:,ivl1(id1)),vl2(:,ivl2(id2)))

     ! on calcule ca pour gerer les cas foireux v1^v2
     cross = (v1(1)*v2(2)) - (v1(2)*v2(1))
     
     ! Calcul intersection entre 2 segments si 
     ! code = 0            ne coupe pas
     ! 1000 <= code < 2000 on coupe en 1 point ; retourne p
     ! 2000 <= code        les 2 segments sont // ; on retourne p,q les extremites de la zone de recouvrement 
     code = segments_intersection_wp(vl1(:,ivl1(idp1)),vl1(:,ivl1(id1)), &
                                   vl2(:,ivl2(idp2)),vl2(:,ivl2(id2)), &
                                   p,q,err_)
    
     if (err_ > 0) then
       call logmes('Error in '//IAM//' : segments_intersection_wp',.true.)
       err = 1
       return
     endif

     
     if (bavard) then
        print*,"code = ",code
        print*,"v1.v2 = ",dot_product(v1,v2)," cross = ",cross
        print*,"c1Hc2 = ",c1Hc2," c2Hc1 = ",c2Hc1
     endif
     
     ! ca coupe: franchement
     if (code == 10000) then
       !print*,'ca coupe'
        
       ! si on est au tout debut on ne garde pas               
       if (inflag == 0 .and. firstpoint) then
         ! on n incremente pas
         i1=0 ; i2=0  
         firstpoint=.FALSE.
       endif
      
       if (c1Hc2 > 0.d0) then

         ! si c1 est dedans
         inflag = 1
          
       else if (c2Hc1 > 0.d0) then

        ! si c2 est dedans
        inflag = 2
          
       else

        ! on ne fait rien car on ne sait pas
          
       endif
       
       ! on est revenu au premier point
       if (nb_points > 0) then
         if (length2(points(:,1)-p) <= zero) exit
       endif                

       if (bavard) then
         write(cout,*) 'intersection point ',p
         call logmes(cout, .true.)
       end if
       
       nb_points = nb_points + 1

       if (nb_points > sz_points) then
         ! on essaie de rajouter un point alors que ca n est pas possible            
         call display_polytope(nb_vertex1,vl1,ivl1)
         call display_polytope(nb_vertex2,vl2,ivl2)          

         write(cout,*) '----'
         call logmes(cout, .true.)
         write(cout,"(2(1x,E12.5))") points
         call logmes(cout, .true.)

         call logmes('Error in '//IAM//' : Unexpected situation',.true.)
         err = 1
         return

      endif
       
       points(:,nb_points) = p
     endif 

     ! bords non // qui coupent par un ou deux sommets (confondus) 
         
     if (code == 10100 .or. code == 10110) then
       ! si on est au tout debut                
       if (inflag == 0 .and. firstpoint) then
         ! on n incremente pas
         i1=0 ; i2=0  
         firstpoint=.FALSE.
       endif
        
       idn1 = modulo(id1-1+1, nb_vertex1) + 1
       if (f_orient2d(vl2(:,ivl2(idp2)),vl2(:,ivl2(id2)),vl1(:,ivl1(idn1))) >= 0.d0) then
         ! vu comme un point interieur
         inflag = 1 
       else
         ! vu comme une intersection  
         inflag = 2
         keep = .true.
      endif

     endif

     ! bords non // qui coupent par un ou deux sommets (confondus) 
     
     if (code == 10001 .or. code == 11001) then
       ! si on est au tout debut                
       if (inflag == 0 .and. firstpoint) then
         ! on n incremente pas
         i1=0 ; i2=0  
         firstpoint=.FALSE.
       endif

       idn2 = modulo(id2-1+1, nb_vertex2) + 1
       if (f_orient2d(vl1(:,ivl1(idp1)),vl1(:,ivl1(id1)),vl2(:,ivl2(idn2))) >= 0.d0) then
         ! vu comme un point interieur 
         inflag = 2
       else
         ! vu comme une intersection  
         inflag = 1
         keep = .true.
       endif
      
     end if

     ! // ca touche juste sur un bord
     if (code > 20000 .and. dot_product(v1,v2) > 0.d0) then
       ! si on est au tout debut                
       if (inflag == 0 .and. firstpoint) then
         ! on n incremente pas
         i1=0 ; i2=0  
         firstpoint=.FALSE.
       endif
      
       select case (code)
       case(21100,20110)    
          inflag=1
       case(20011,21001)
          inflag=2
       end select
       
       if (nb_points > 0) then       
         if (length2(points(:,1)-p) <= zero) exit
       endif
      
       !nb_points=nb_points+1
       !points(:,nb_points) = p
     endif

     ! Regles d'avancement
     
     ! // ca touche juste sur un bord oriente en sens oppose
     if (code > 20000 .and. dot_product(v1,v2) < 0.d0) then
        
       if( nb_points > 0 ) then
         if (length2(points(:,nb_points)-p) > zero) then
           if (bavard) then
             write(cout,*) ' oppose++'
             call logmes(cout, .true.)
           end if
            
           nb_points=nb_points+1
           
           if (nb_points > sz_points) then
             ! on essaie de rajouter un point alors que ca n est pas possible
             call display_polytope(nb_vertex1,vl1,ivl1)
             call display_polytope(nb_vertex2,vl2,ivl2)          
              
             write(cout,*) '----'
             call logmes(cout, .true.)
             write(cout,"(2(1x,E12.5))") points
             call logmes(cout, .true.)
             
             call logmes('Error in '//IAM//' : Unexpected situation',.true.)
             err= 1
             return
           endif

           points(:,nb_points) = p
         endif   
       end if
      
       if (length2(p-q) > zero) then
         if (bavard) then
           write(cout,*) ' oppose++'
           call logmes(cout, .true.)
         end if
         nb_points=nb_points+1
         if (nb_points > sz_points) then
           call display_polytope(nb_vertex1,vl1,ivl1)
           call display_polytope(nb_vertex2,vl2,ivl2)          
            
           write(cout,*) '----'
           call logmes(cout, .true.)
           write(cout,"(2(1x,E12.5))") points
           call logmes(cout, .true.)

           call logmes('Error in '//IAM//' : Unexpected situation',.true.)
           err = 1
           return
         endif

         points(:,nb_points) = q
       endif
      
       exit
       
     endif

     ! on prend le plus a gauche de c1 et c2 ...
     ! ... c'est c1 
     if ( cross >= 0.d0 ) then

       ! si c2 est a gauche de c1   
       if (c2Hc1 > 0.d0) then

         if (code >= 0 .and. inflag == 1) then
             if (bavard) then
               write(cout,*) 'c1++ ',vl1(:,ivl1(id1))
               call logmes(cout, .true.)
             end if
            
             nb_points = nb_points + 1
             
             if (nb_points > sz_points) then
               call display_polytope(nb_vertex1,vl1,ivl1)
               call display_polytope(nb_vertex2,vl2,ivl2)          

               write(cout,*) '----'
               call logmes(cout, .true.)
               write(cout,"(2(1x,E12.5))") points
               call logmes(cout, .true.)

               call logmes('Error in '//IAM//' : Unexpected situation',.true.)
               err = 1
               return
             endif
             
             points(:,nb_points) = vl1(:,ivl1(id1))
         endif
          
         if ( keep ) then
           if (bavard) then
             write(cout,*) 'c2x ',vl2(:,ivl2(id2))
             call logmes(cout, .true.)
           end if
            
           nb_points = nb_points + 1

           if (nb_points > sz_points) then
             call display_polytope(nb_vertex1,vl1,ivl1)
             call display_polytope(nb_vertex2,vl2,ivl2)          
              
             write(cout,*) '----'
             call logmes(cout, .true.)
             write(cout,"(2(1x,E12.5))") points
             call logmes(cout, .true.)
             
             call logmes('Error in '//IAM//' : Unexpected situation',.true.)
             err = 1
             return
           endif
           
           points(:,nb_points) = vl2(:,ivl2(id2))
         endif    

         ! on avance dans c1
         id1 = modulo(id1-1 + 1, nb_vertex1) + 1 
         i1 = i1 + 1
         
       ! si c2 n est pas a gauche de c1        
       else

         if (code >= 0 .and. inflag == 2) then
           if (bavard) then
             write(cout,*) 'c2++ ',vl2(:,ivl2(id2))
             call logmes(cout, .true.)
           end if
            
           nb_points = nb_points + 1
           
           if (nb_points > sz_points) then
             call display_polytope(nb_vertex1,vl1,ivl1)
             call display_polytope(nb_vertex2,vl2,ivl2)          
              
             write(cout,*) '----'
             call logmes(cout, .true.)
             write(cout,"(2(1x,E12.5))") points
             call logmes(cout, .true.)

             call logmes('Error in '//IAM//' : Unexpected situation',.true.)
             err = 1
             return
           endif
           
           points(:,nb_points) = vl2(:,ivl2(id2))  
         endif

         ! on avance dans c2
         id2 = modulo(id2-1 + 1, nb_vertex2) + 1 
         i2 = i2 + 1

       endif             

     ! ... c'est c2 le plus a gauche
     else 
       if (c1Hc2 > 0.d0) then

         if (code >= 0 .and. inflag == 2) then    
           if (bavard) then
             write(cout,*) 'c2++ ',vl2(:,ivl2(id2))
             call logmes(cout, .true.)
           end if
           nb_points = nb_points + 1
           
           if (nb_points > sz_points) then
             call display_polytope(nb_vertex1,vl1,ivl1)
             call display_polytope(nb_vertex2,vl2,ivl2)          
              
             write(cout,*) '----'
             call logmes(cout, .true.)
             write(cout,"(2(1x,E12.5))") points
             call logmes(cout, .true.)

             call logmes('Error in '//IAM//' : Unexpected situation',.true.)
             err = 1
             return
           endif
           
           points(:,nb_points) = vl2(:,ivl2(id2))
         endif

         if (keep) then
           if (bavard) then
             write(cout,*) 'c1x ',vl1(:,ivl1(id1))
             call logmes(cout, .true.)
           end if
            
           nb_points = nb_points + 1

           if (nb_points > sz_points) then
             call display_polytope(nb_vertex1,vl1,ivl1)
             call display_polytope(nb_vertex2,vl2,ivl2)          
              
             write(cout,*) '----'
             call logmes(cout, .true.)
             write(cout,"(2(1x,E12.5))") points
             call logmes(cout, .true.)

             call logmes('Error in '//IAM//' : Unexpected situation',.true.)
             err = 1
             return
           endif
           
           points(:,nb_points) = vl1(:,ivl1(id1))
         endif

         ! on avance dans c2
         id2 = modulo(id2-1 + 1, nb_vertex2) + 1 
         i2 = i2 + 1
         
       else

         if (code >= 0 .and. inflag == 1) then
           if (bavard) then
             write(cout,*) 'c1++ ',vl1(:,ivl1(id1))
             call logmes(cout, .true.)
           end if

           nb_points = nb_points + 1

           if (nb_points > sz_points) then
             call display_polytope(nb_vertex1,vl1,ivl1)
             call display_polytope(nb_vertex2,vl2,ivl2)          
              
             write(cout,*) '----'
             call logmes(cout, .true.)
             write(cout,"(2(1x,E12.5))") points
             call logmes(cout, .true.)

             call logmes('Error in '//IAM//' : Unexpected situation',.true.)
             err = 1
             return
           endif
           
           points(:,nb_points) = vl1(:,ivl1(id1))
         endif
            
         ! on avance dans c1
         id1 = modulo(id1-1 + 1, nb_vertex1) + 1 
         i1 = i1 + 1

       endif
     endif   

     if (bavard) then
       write(cout,*) 'i1 = ',i1,'i2 = ',i2
       call logmes(cout, .true.)
       write(cout,*) 'inflag = ',inflag
       call logmes(cout, .true.)
     end if
     i=i+1
     
     if ((i1 < nb_vertex1 .or. i2 < nb_vertex2) .and. &
         (i1 < 2*nb_vertex1) .and. (i2 < 2*nb_vertex2)) cycle
     
     exit
  enddo

  if (inflag == 0 .and. nb_points == 0) then

     ! on teste cas interieurs
     ! 2 dans 1
     id2=1
     i = 0 
     do id1=1,nb_vertex1
       idp1 = modulo(id1+nb_vertex1-2, nb_vertex1) + 1 
       if (f_orient2d(vl1(:,ivl1(idp1)),vl1(:,ivl1(id1)),vl2(:,ivl2(1))) > 0.d0) then     
         i=i+1
       else
         exit
       endif   
     enddo
     if (i == nb_vertex1) then
       do id2=1,nb_vertex2
         nb_points = nb_points + 1

         if (nb_points > sz_points) then
           call display_polytope(nb_vertex1,vl1,ivl1)
           call display_polytope(nb_vertex2,vl2,ivl2)          
              
           print*,'----'
           print "(2(1x,E12.5))",points

           call logmes('Error in '//IAM//' : Unexpected situation',.true.)
           err = 1
           return
         endif
         
         points(:,nb_points) = vl2(:,ivl2(id2))
       enddo
       return
     endif 
     ! 1 dans 2     
     id1=1
     do id2=1,nb_vertex2
       idp2 = modulo(id2+nb_vertex2-2, nb_vertex2) + 1
       if (f_orient2d(vl2(:,ivl2(idp2)),vl2(:,ivl2(id2)),vl1(:,ivl1(1))) > 0.d0) then     
         i=i+1
       else
         exit
       endif   
     enddo
     if (i == nb_vertex2) then
       do id1=1,nb_vertex1
         nb_points = nb_points + 1

         if (nb_points > sz_points) then
           call display_polytope(nb_vertex1,vl1,ivl1)
           call display_polytope(nb_vertex2,vl2,ivl2)          
              
           print*,'----'
           print "(2(1x,E12.5))",points

           call logmes('Error in '//IAM//' : Unexpected situation',.true.)
           err = 1
           return
         endif
         
         points(:,nb_points) = vl1(:,ivl1(id1))
       enddo
       return
    endif
    
     if (bavard) print*,' contours disjoints'

  endif
  
end subroutine

