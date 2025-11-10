
  subroutine test_polytopes_intersection
    implicit none

    real(kind=8) :: vl1(2,8),vl2(2,7),ref_size
    integer      :: ivl1(9),ivl2(8),nb_ptc,i
    real(kind=8),dimension(:,:),pointer :: points
    integer :: err
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection       '

    nullify(points)

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/0.9d0, 1.1d0 /)
    vl1(:,4) = (/-1.d0, 1.05d0 /)
    vl1(:,5) = (/4.d0, 4.d0 /)
    vl1(:,6) = (/2.d0, 0.d0 /)
    vl1(:,7) = (/5.d0, 3.d0 /)
    vl1(:,8) = (/9.d0, 0.d0 /)

    ivl1 = (/ 1,2,3,4,1, 0, 0, 0, 0 /)

    vl2(:,1) = (/0.d0, 1.d0 /)
    vl2(:,2) = (/1.d0, 1.d0 /)
    vl2(:,3) = (/1.d0, 2.d0 /)
    vl2(:,4) = (/0.d0, 2.d0 /)
    vl2(:,5) = (/0.d0, 1.d0 /)
    vl1(:,6) = (/20.d0, 0.d0 /)
    vl1(:,7) = (/5.d0, 30.d0 /)

    ivl2 = (/ 1,2,3,4,1,0,0,0 /)

    call polytopes_intersection(vl1,ivl1,vl2,ivl2,1.d0,points,nb_ptc,.true.,err)
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

  end subroutine test_polytopes_intersection

  subroutine test_polytopes_intersection_wp
    implicit none

    real(kind=8) :: vl1(2,8),vl2(2,7),ref_size
    integer      :: ivl1(9),ivl2(8),nb_ptc,i
    real(kind=8),dimension(:,:),pointer :: points
    integer :: err

    print*,'---------------------------------------'
    print*,' test_polytopes_intersection_wp         '
    print*,'---------------------------------------'    
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection 4 points '

    nullify(points)

    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.5d0, 0.5d0 /)
    vl2(:,2) = (/1.5d0, 0.5d0 /)
    vl2(:,3) = (/1.5d0, 1.5d0 /)
    vl2(:,4) = (/0.5d0, 1.5d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true.,err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection: 1 point exterieur '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/1.d0, 1.d0 /)
    vl2(:,2) = (/2.d0, 1.d0 /)
    vl2(:,3) = (/2.d0, 2.d0 /)
    vl2(:,4) = (/1.d0, 2.d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)
    
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection: 3 points interieurs '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.5d0, 0.3d0 /)
    vl2(:,2) = (/1.d0, 0.5d0 /)
    vl2(:,3) = (/0.5d0, 0.8d0 /)

    ivl2(1:4) = (/ 1,2,3,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)
    
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection: 3 points interieurs reverse '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl2(:,1) = (/0.d0, 0.d0 /)
    vl2(:,2) = (/1.d0, 0.d0 /)
    vl2(:,3) = (/1.d0, 1.d0 /)
    vl2(:,4) = (/0.d0, 1.d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    vl1(:,1) = (/0.5d0, 0.3d0 /)
    vl1(:,2) = (/1.d0, 0.5d0 /)
    vl1(:,3) = (/0.5d0, 0.8d0 /)

    ivl1(1:4) = (/ 1,2,3,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true.,err)
    
    if (err > 0) then
       print*,'failed'
       stop
    endif   
    
    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

    print*,'test d intersection 2 points exterieurs '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/1.d0, 0.d0 /)
    vl2(:,2) = (/2.d0, 0.d0 /)
    vl2(:,3) = (/2.d0, 1.d0 /)
    vl2(:,4) = (/1.d0, 1.d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

    print*,'test d intersection 4 points interieurs avec 1 bord commun '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.1d0, 0.1d0 /)
    vl2(:,2) = (/1.d0, 0.1d0 /)
    vl2(:,3) = (/1.d0, 0.9d0 /)
    vl2(:,4) = (/0.1d0, 0.9d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

    print*,'test d intersection 4 points interieurs avec 1 bord commun - reverse'

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl2(:,1) = (/0.d0, 0.d0 /)
    vl2(:,2) = (/1.d0, 0.d0 /)
    vl2(:,3) = (/1.d0, 1.d0 /)
    vl2(:,4) = (/0.d0, 1.d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    vl1(:,1) = (/0.1d0, 0.1d0 /)
    vl1(:,2) = (/1.d0, 0.1d0 /)
    vl1(:,3) = (/1.d0, 0.9d0 /)
    vl1(:,4) = (/0.1d0, 0.9d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

    print*,'test d intersection 4 points'

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/-1.d0, 0.1d0 /)
    vl2(:,2) = (/2.d0, 0.1d0 /)
    vl2(:,3) = (/2.d0, 0.9d0 /)
    vl2(:,4) = (/-1.d0, 0.9d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   
    
    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'

    print*,'test d intersection 4 points interieurs'

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.5d0, 0.d0 /)
    vl2(:,2) = (/1.d0, 0.5d0 /)
    vl2(:,3) = (/0.5d0, 1.d0 /)
    vl2(:,4) = (/0.d0, 0.5d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true.,err)

    if (err > 0) then
       print*,'failed'
       stop
    endif
    
    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection contours superposes'

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.d0, 0.d0 /)
    vl2(:,2) = (/1.d0, 0.d0 /)
    vl2(:,3) = (/1.d0, 1.d0 /)
    vl2(:,4) = (/0.d0, 1.d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection 2 dans 1   '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.1d0, 0.1d0 /)
    vl2(:,2) = (/0.9d0, 0.1d0 /)
    vl2(:,3) = (/0.9d0, 0.9d0 /)
    vl2(:,4) = (/0.1d0, 0.9d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true., err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)
    
    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection 1 dans 2   '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl2(:,1) = (/0.d0, 0.d0 /)
    vl2(:,2) = (/1.d0, 0.d0 /)
    vl2(:,3) = (/1.d0, 1.d0 /)
    vl2(:,4) = (/0.d0, 1.d0 /)

    ivl2(1:5) = (/ 1,2,3,4,1 /)

    vl1(:,1) = (/0.1d0, 0.1d0 /)
    vl1(:,2) = (/0.9d0, 0.1d0 /)
    vl1(:,3) = (/0.9d0, 0.9d0 /)
    vl1(:,4) = (/0.1d0, 0.9d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true.,err)

    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)


    print*,'xxxxxxxxxxxxxxxxxxxxxxxxxxx'
    print*,'test d intersection: 3 points interieurs coins '

    nullify(points)
    
    vl1 = 0.d0 ; ivl1 = 0
    vl2 = 0.d0 ; ivl2 = 0

    vl1(:,1) = (/0.d0, 0.d0 /)
    vl1(:,2) = (/1.d0, 0.d0 /)
    vl1(:,3) = (/1.d0, 1.d0 /)
    vl1(:,4) = (/0.d0, 1.d0 /)

    ivl1(1:5) = (/ 1,2,3,4,1 /)

    vl2(:,1) = (/0.5d0, 0.3d0 /)
    vl2(:,2) = (/1.d0, 0.d0 /)
    vl2(:,3) = (/0.5d0, 0.8d0 /)

    ivl2(1:4) = (/ 1,2,3,1 /)

    call polytopes_intersection_wp(vl1,ivl1,vl2,ivl2,points,nb_ptc,.true.,err)
    
    if (err > 0) then
       print*,'failed'
       stop
    endif   

    print*,nb_ptc
    do i=1,nb_ptc
       print*,points(:,i)
    enddo

    deallocate(points)

    
  end subroutine test_polytopes_intersection_wp

  
