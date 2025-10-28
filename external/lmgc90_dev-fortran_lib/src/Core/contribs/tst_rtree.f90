program test_rtree

  use rtree, only: f_reset_tree2, &
                   f_add2tree2  , &
                   f_search_in_tree2

  implicit none

  real(kind=8) :: ti,tf
  integer :: is_good, nb_err

  logical :: ok

  ! ce programme de test peut etre utilise de 2 facons:
  !  - standard depuis le rep de test ; une serie de tests de base seront alors lancés
  !  - autre depuis le rep d'exemple LMGC90v2_Examples/Tests/Benchmarks/Detection/DKDK/ ; 
  !    une comparaison entre detection classique et rtree sera alors faite 

  inquire(file='sample.txt', exist=ok)

  nb_err = 0

  if (ok) then
    print*,'-------------------'
    is_good = realsample_test() 
    nb_err = nb_err + is_good
    print*,'-------------------'
  else
    print*,'-------------------'
    CALL cpu_time(ti)  
    is_good = unit_test()
    nb_err = nb_err + is_good
    CALL cpu_time(tf)  
    print*,'elapsed ',tf-ti
    print*,'-------------------'
    CALL cpu_time(ti)  
    is_good = regular_packing_test()
    nb_err = nb_err + is_good
    CALL cpu_time(tf)  
    print*,'elapsed ',tf-ti
    print*,'-------------------'
    is_good = random_test() 
    nb_err = nb_err + is_good
    print*,'-------------------'
  endif

  if( nb_err > 0 ) then
    stop 1
  end if

  contains

  integer function unit_test()

    use iso_c_binding

    integer(kind=4), parameter :: nb_rec = 5
    ! a list of rectangles
    real(c_double), dimension(4,nb_rec) :: rects
    ! a list of candidate boxes
    real(c_double), dimension(6,nb_rec) :: cd
    ! a list of intersecting body
    integer(c_int), dimension(:), pointer :: icdan

    integer(kind=4) :: i

    print '(A)','unit test' 
    unit_test = 0

    allocate(icdan(nb_rec)) 

    rects(:,1) = (/0. ,0. ,2.,2./)
    rects(:,2) = (/5. ,5. ,7.,7./)
    rects(:,3) = (/1. ,1. ,3.,3./)
    rects(:,4) = (/2.1,2.1,6.,6./)
    rects(:,5) = (/5. ,2. ,6.,3./)

    call f_reset_tree2()

    ! adding first rectangle
    call f_add2tree2(rects(1:2,1),rects(3:4,1),1)


    do i = 2, nb_rec
      icdan = 0
      call f_search_in_tree2(rects(1:2,i),rects(3:4,i),c_loc(icdan(1)))
      print '(A,I0,A,I0,A)', 'rec ', i, ' intersects with ', icdan(1), ' others'
      if ( icdan(1) > 0 ) print *,  icdan(2:icdan(1)+1)
      call f_add2tree2(rects(1:2,i),rects(3:4,i),i)

      if ( i == 2 .and. icdan(1) /= 0 ) unit_test=1
      if ( i == 3 .and. icdan(1) /= 1 ) unit_test=1
      if ( i == 4 .and. icdan(1) /= 2 ) unit_test=1
      if ( i == 5 .and. icdan(1) /= 1 ) unit_test=1
    end do

    if (unit_test == 0) then
      print '(A)','passed' 
    else
      print '(A)','failed' 
    endif

  end function unit_test

  integer function regular_packing_test()

    use iso_c_binding

    integer(kind=4), parameter :: nb_b = 1000, nb_s=nb_b-1, nb_l=1000, nb_rec=nb_b+nb_l*(nb_b+nb_s) 

    integer(c_int) :: id
    !
    real(c_double) :: tx,ty,x,y,tol
    ! a list of intersecting body
    integer(c_int), dimension(:), pointer :: icdan
    !
    !
    integer(kind=4) :: i,nb,j
  
    print '(A,I0)','regular packing test ',nb_rec 

    regular_packing_test=0

    !allocate(icdan(nb_rec)) 
    allocate(icdan(10)) 

    ! taille de la bb
    x=1. 
    y=1. 
    tol=0.

    call f_reset_tree2()

    nb=0
    ! adding first rectangle
    id = 1
    call f_add2tree2((/tx,ty/),(/tx+x,ty+y/),id)


    ! first raw
    ty=0.
    tx=0.
    do i = 2, nb_b
      id = id +1
      icdan = 0
      tx = tx + x + tol
      call f_search_in_tree2((/tx,ty/),(/tx+x,ty+y/),c_loc(icdan(1)))
      !print '(A,I0,A,I0,A)', 'rec ', id, ' intersects with ', icdan(1), ' others'
      !if( icdan(1) > 0 ) print *,  icdan(2:icdan(1)+1)
      nb = nb + icdan(1)
      call f_add2tree2((/tx,ty/),(/tx+x,ty+y/),id)
    end do

    ! 2 more raws
    do j=1,nb_l
      ty = ty + y + tol
      tx = -(x + tol)*0.5
      do i = 1, nb_s
        id = id +1
        icdan = 0
        tx = tx + x + tol
        call f_search_in_tree2((/tx,ty/),(/tx+x,ty+y/),c_loc(icdan(1)))
        !print '(A,I0,A,I0,A)', 'rec ', id, ' intersects with ', icdan(1), ' others'
        !if( icdan(1) > 0 ) print *,  icdan(2:icdan(1)+1)
        nb = nb + icdan(1)
        call f_add2tree2((/tx,ty/),(/tx+x,ty+y/),id)
      end do
      ty = ty + y + tol
      tx = -x
      do i = 1, nb_b
        id = id +1
        icdan = 0
        tx = tx + x + tol
        call f_search_in_tree2((/tx,ty/),(/tx+x,ty+y/),c_loc(icdan(1)))
        !print '(A,I0,A,I0,A)', 'rec ', id, ' intersects with ', icdan(1), ' others'
        !if( icdan(1) > 0 ) print *,  icdan(2:icdan(1)+1)
        nb = nb + icdan(1)
        call f_add2tree2((/tx,ty/),(/tx+x,ty+y/),id)
      end do
    enddo

    print '(A,I0)', 'total nb of intersections  ', nb
    print '(A,I0)', 'theoretical value          ', nb_b - 1 + nb_l*((nb_s - 1) + 2*nb_s + &
                                                                  (nb_b - 1) + 2*(nb_b-2) + 2 )

    if (nb /= nb_b - 1 + nb_l*((nb_s - 1) + 2*nb_s + (nb_b - 1) + 2*(nb_b-2) + 2 )) regular_packing_test=1

    if (regular_packing_test == 0) then
      print '(A)','passed' 
    else
      print '(A)','failed' 
    endif

    deallocate(icdan)

  end function regular_packing_test

  integer function random_test()

    use iso_c_binding

    integer(kind=4), parameter :: nb_rec = 100000
    ! a list of rectangles
    real(c_double), dimension(4,nb_rec) :: rects
    ! lists of random numbers
    real(c_double),dimension(nb_rec) :: x,y,l   
    ! a list of intersecting body
    integer(c_int), dimension(:), pointer :: icdan
    !
    real(c_double) :: ratio=0.001
    !
    integer(kind=4) :: i,nb
  
    print '(A,I0)','irregular packing test ',nb_rec 

    allocate(icdan(100)) 

    call random_number(x)
    call random_number(y)
    call random_number(l)

    print*,minval(l),maxval(l)

    do i=1,nb_rec
     rects(:,i) = (/ x(i)-ratio*l(i), y(i)-ratio*l(i), &
                     x(i)+ratio*l(i), y(i)+ratio*l(i)  /)
    enddo

    CALL cpu_time(ti)  

    call f_reset_tree2()

    ! adding first rectangle
    call f_add2tree2(rects(1:2,1),rects(3:4,1),1)

    nb=0
    do i = 2, nb_rec
      icdan = 0
      call f_search_in_tree2(rects(1:2,i),rects(3:4,i),c_loc(icdan(1)))
      !print '(A,I0,A,I0,A)', 'rec ', i, ' intersects with ', icdan(1), ' others'
      !if( icdan(1) > 0 ) print *,  icdan(2:icdan(1)+1)
      if ( icdan(1) == 100 ) print*,'wtf' 
      nb = nb + icdan(1)
      call f_add2tree2(rects(1:2,i),rects(3:4,i),i)
    end do

    CALL cpu_time(tf)  
    print*,'elapsed ',tf-ti


    print '(A,I0)', ' total nb of intersections', nb

    open(unit=10)
    do i=1,nb_rec
      write(10,'(2(1x,D12.5))') x(i),y(i)
    enddo
    close(10)

    random_test=0

    deallocate(icdan)
    

  end function random_test

  integer function realsample_test()

    use iso_c_binding

    integer(kind=4) :: nb_rec
    ! a list of rectangles
    real(c_double), dimension(:,:),allocatable :: rects
    ! lists of random numbers
    real(c_double),dimension(:),allocatable :: x,y,l   
    ! a list of intersecting body
    integer(c_int), dimension(:), pointer :: icdan
    !
    real(c_double) :: tol
    !
    integer(kind=4) :: i,nb

    ! lecture du fichier crache par le pre

    open(unit=10,file='sample.txt')

    read(10,*) nb_rec,tol

    allocate(x(nb_rec),y(nb_rec),l(nb_rec),rects(4,nb_rec))

    do i=1,nb_rec
      read(10,*) x(i),y(i),l(i)
      rects(:,i) = (/ x(i)-(l(i)+tol), y(i)-(l(i)+tol), &
                      x(i)+(l(i)+tol), y(i)+(l(i)+tol)  /)
    enddo

    close(10)

    ! c est parti

    allocate(icdan(100)) 
  
    CALL cpu_time(ti)  

    call f_reset_tree2()

    ! adding first rectangle
    call f_add2tree2(rects(1:2,1),rects(3:4,1),1)

    nb=0
    do i = 2, nb_rec
      icdan = 0
      call f_search_in_tree2(rects(1:2,i),rects(3:4,i),c_loc(icdan(1)))
      !print '(A,I0,A,I0,A)', 'rec ', i, ' intersects with ', icdan(1), ' others'
      !if( icdan(1) > 0 ) print *,  icdan(2:icdan(1)+1)

      if ( icdan(1) == 100 ) then
        print*,'wtf !' 
        print*,'max number of neighbours is reached'
        stop 1
      endif

      nb = nb + icdan(1)
      call f_add2tree2(rects(1:2,i),rects(3:4,i),i)
    end do

    CALL cpu_time(tf)  
    print*,'elapsed ',tf-ti

    print '(A,I0)', ' total nb of intersections ', nb

    deallocate(icdan)
    deallocate(x,y,l,rects)    

    realsample_test=0


  end function realsample_test

end program test_rtree
