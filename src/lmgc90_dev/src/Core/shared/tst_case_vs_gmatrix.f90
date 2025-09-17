program test_case_vs_gmatrix

   use a_matrix
   use utilities

   USE timer, ONLY: &
        initialize_utimer, &
        write_utimer, &
        get_new_utimer_ID, &
        start_utimer,stop_utimer

   
   implicit none
  
   type(G_matrix) :: A
   integer :: i, j, k, n, info
   integer :: bw
   real(kind=8) :: residu
   integer, dimension(1) :: perm_fake ! tableau de permutation bidon utilise pour delcarer la G_matrix 
   real(kind=8), dimension(:), allocatable :: b_ci, b_co, b_g

   ! Coefficients des matrices L^-1
   ! Pour nb_liens=2
   real(kind=8), parameter :: d2_3=(2.d0/3.d0), d1_3=(1.d0/3.d0)
   ! Pour nb_liens=3
   real(kind=8), parameter :: d3_4=0.75d0, d1_2=0.5d0, d1_4=0.25d0 ! 1=1.d0
   ! Pour nb_liens=4
   real(kind=8), parameter :: d4_5=0.8d0, d3_5=0.6d0, d2_5=0.4d0, d1_5=0.2d0, d6_5=1.2d0
   ! Pour nb_liens=5
   real(kind=8), parameter :: d5_6=(5.d0/6.d0), d1_6=(1.d0/6.d0), d4_3=(4.d0/3.d0), d3_2=1.5d0
   ! Pour nb_liens=6
   real(kind=8), parameter :: d6_7=(6.d0/7.d0), d5_7=(5.d0/7.d0), d4_7=(4.d0/7.d0), d3_7=(3.d0/7d0), d2_7=(2.d0/7.d0), &
                              d1_7=(1.d0/7.d0), d10_7=(10.d0/7.d0), d8_7=(8.d0/7.d0), d12_7=(12.d0/7.d0),              &
                              d9_7=(9.d0/7.d0)
   ! Pour nb_liens=7
   real(kind=8), parameter :: d7_8=0.875d0, d5_8=0.625d0, d3_8=0.375d0, d1_8=0.125d0, d5_4=1.25d0, d15_8=1.875d0, d9_8=1.125d0
   ! Indices des timers
   integer :: id_gmatrix, id_case, id_inutile


   !                         1234567890123
   character(len=13) :: IAM='test_a_MATRIX'

   ! Initialisation des timers
   CALL initialize_utimer
                                    !12345678901234567890
   id_case       = get_new_utimer_ID('RESOL DIRECT        ')
   id_gmatrix    = get_new_utimer_ID('RESOL GMATRIX       ')
   id_inutile    = get_new_utimer_ID('AUTRES              ')

   !read (5,*), n
   n = 10
   !do n = 2,2

      call start_utimer(id_inutile)
      ! allocate memory for the right hand side
      allocate(b_ci(6*n))
      allocate(b_co(6*n))
      allocate(b_g(6*n))
      call stop_utimer(id_inutile)

      do j = 1,1000!0

         ! compute the columns of the inverse of the matrix
         do i=1, 6*n
            call start_utimer(id_inutile)

            !print *, "-----------new n---------"
            !print *, "n =", i

            ! compute the right hand side
            b_ci=0.d0
            b_ci(i) = 1.d0
            b_g=0.d0
            b_g(i) = 1.d0
            call stop_utimer(id_inutile)

            ! on choisi la methode de resolution selon le nombre de liens
            !if (n < 3) then

               call start_utimer(id_case)

               SELECT CASE(n)

               CASE(1)
                  b_co(1:6) = 0.5d0*b_ci(1:6)

               CASE(2)
                  b_co(1:6)  =  ( d2_3*b_ci(1:6) + d1_3*b_ci(7:12) )
                  b_co(7:12) =  ( d1_3*b_ci(1:6) + d2_3*b_ci(7:12) )
               CASE(3)
                  b_co(1:6)  =  ( d3_4*b_ci(1:6) + d1_2*b_ci(7:12) + d1_4*b_ci(13:18) )
                  b_co(7:12) =  ( d1_2*b_ci(1:6) + 1.d0*b_ci(7:12) + d1_2*b_ci(13:18) )
                  b_co(13:18)=  ( d1_4*b_ci(1:6) + d1_2*b_ci(7:12) + d3_4*b_ci(13:18) )
               CASE(4)
                  b_co(1:6)  =  ( d4_5*b_ci(1:6) + d3_5*b_ci(7:12) + d2_5*b_ci(13:18) + d1_5*b_ci(19:24) )
                  b_co(7:12) =  ( d3_5*b_ci(1:6) + d6_5*b_ci(7:12) + d4_5*b_ci(13:18) + d2_5*b_ci(19:24) )
                  b_co(13:18)=  ( d2_5*b_ci(1:6) + d4_5*b_ci(7:12) + d6_5*b_ci(13:18) + d3_5*b_ci(19:24) )
                  b_co(19:24)=  ( d1_5*b_ci(1:6) + d2_5*b_ci(7:12) + d3_5*b_ci(13:18) + d4_5*b_ci(19:24) )
               CASE(5)
                  b_co(1:6)  =  ( d5_6*b_ci(1:6) + d2_3*b_ci(7:12) + d1_2*b_ci(13:18) + d1_3*b_ci(19:24) + &
                                                 d1_6*b_ci(25:30)                                                              )
                  b_co(7:12) =  ( d2_3*b_ci(1:6) + d4_3*b_ci(7:12) + 1.d0*b_ci(13:18) + d2_3*b_ci(19:24) + &
                                                 d1_3*b_ci(25:30)                                                              )
                  b_co(13:18)=  ( d1_2*b_ci(1:6) + 1.d0*b_ci(7:12) + d3_2*b_ci(13:18) + 1.d0*b_ci(19:24) + &
                                                 d1_2*b_ci(25:30)                                                              )
                  b_co(19:24)=  ( d1_3*b_ci(1:6) + d2_3*b_ci(7:12) + 1.d0*b_ci(13:18) + d4_3*b_ci(19:24) + &
                                                 d2_3*b_ci(25:30)                                                              )
                  b_co(25:30)=  ( d1_6*b_ci(1:6) + d1_3*b_ci(7:12) + d1_2*b_ci(13:18) + d2_3*b_ci(19:24) + &
                                                 d5_6*b_ci(25:30)                                                              )
               CASE(6)
                  b_co(1:6)  =  ( d6_7*b_ci(1:6) + d5_7*b_ci(7:12) + d4_7*b_ci(13:18) + d3_7*b_ci(19:24) + &
                                                 d2_7*b_ci(25:30)+d1_7*b_ci(31:36)                                           )
                  b_co(7:12) =  ( d5_7*b_ci(1:6) +d10_7*b_ci(7:12)+ d8_7*b_ci(13:18) + d6_7*b_ci(19:24) +  &
                                                 d4_7*b_ci(25:30)+d2_7*b_ci(31:36)                                           )
                  b_co(13:18)=  ( d4_7*b_ci(1:6) + d8_7*b_ci(7:12) +d12_7*b_ci(13:18) + d9_7*b_ci(19:24) + &
                                                 d6_7*b_ci(25:30)+d3_7*b_ci(31:36)                                           )
                  b_co(19:24)=  ( d3_7*b_ci(1:6) + d6_7*b_ci(7:12) + d9_7*b_ci(13:18) + d12_7*b_ci(19:24)+ &
                                                 d8_7*b_ci(25:30)+d4_7*b_ci(31:36)                                           )
                  b_co(25:30)=  ( d2_7*b_ci(1:6) + d4_7*b_ci(7:12) + d6_7*b_ci(13:18) + d8_7*b_ci(19:24) + &
                                                d10_7*b_ci(25:30)+d5_7*b_ci(31:36)                                           )
                  b_co(31:36)=  ( d1_7*b_ci(1:6) + d2_7*b_ci(7:12) + d3_7*b_ci(13:18) + d4_7*b_ci(19:24) + &
                                                 d5_7*b_ci(25:30)+d6_7*b_ci(31:36)                                           )
               CASE(7)
                  b_co(1:6)  =  ( d7_8*b_ci(1:6) + d3_4*b_ci(7:12) + d5_8*b_ci(13:18) + d1_2*b_ci(19:24) + &
                                                 d3_8*b_ci(25:30)+d1_4*b_ci(31:36)+ d1_8*b_ci(37:42)                       )
                  b_co(7:12) =  ( d3_4*b_ci(1:6) + d3_2*b_ci(7:12) + d5_4*b_ci(13:18) + 1.d0*b_ci(19:24) + &
                                                 d3_4*b_ci(25:30)+d1_2*b_ci(31:36)+ d1_4*b_ci(37:42)                       )
                  b_co(13:18)=  ( d5_8*b_ci(1:6) + d5_4*b_ci(7:12) +d15_8*b_ci(13:18) + d3_2*b_ci(19:24) + &
                                                 d9_8*b_ci(25:30)+d3_4*b_ci(31:36)+ d3_8*b_ci(37:42)                       )
                  b_co(19:24)=  ( d1_2*b_ci(1:6) + 1.d0*b_ci(7:12) + d3_2*b_ci(13:18) + 2.d0*b_ci(19:24)+ &
                                                 d3_2*b_ci(25:30)+1.d0*b_ci(31:36)+ d1_2*b_ci(37:42)                       )
                  b_co(25:30)=  ( d3_8*b_ci(1:6) + d3_4*b_ci(7:12) + d9_8*b_ci(13:18) + d3_2*b_ci(19:24) + &
                                                d15_8*b_ci(25:30)+d5_4*b_ci(31:36)+ d5_8*b_ci(37:42)                       )
                  b_co(31:36)=  ( d1_4*b_ci(1:6) + d1_2*b_ci(7:12) + d3_4*b_ci(13:18) + 1.d0*b_ci(19:24) + &
                                                 d5_4*b_ci(25:30)+d3_2*b_ci(31:36)+ d3_4*b_ci(37:42)                       )
                  b_co(37:42)=  ( d1_8*b_ci(1:6) + d1_4*b_ci(7:12) + d3_8*b_ci(13:18) + d1_2*b_ci(19:24) + &
                                                 d5_8*b_ci(25:30)+d3_4*b_ci(31:36)+ d7_8*b_ci(37:42)                       )
               END SELECT

               call stop_utimer(id_case)

               !print*, b

            !else

               call start_utimer(id_gmatrix)

               ! matrice bande symetrique
               bw=7

               ! declaration de la matrice
               call G_declare(A, 'sym_band', 6*n, .false., perm_fake, perm_fake) 

               ! definition de la largeur de bande de la matrice
               call G_settle(A, (/ 1, bw /))

               ! construction de la G_matrix (et initialisation a 0)
               call G_build(A)

               ! remplissage de la matrice

               ! dans tous les cas : partie diagonale
               do k=1, 6*n
                  call G_add(A, 2.d0, k, k)
               end do
               ! si on a plusieurs liens : l'extra diagonale non nulle
               do k=7, 6*n
                  call G_add(A, -1.d0, k - bw + 1, k)
               end do

               ! remplissage du tableau intermediraire utilise par la resolution
               call G_store(A)

               ! compute the columns of the inverse of the matrix
               call G_solve_linear_system(A, b_g, info)

               !print*, b

               ! some paranoid test...
               if (info /= 0) then
                  call FATERR(IAM, 'No solution')
               end if
               
               ! destruction de la matrice
               call G_free(A)

               call stop_utimer(id_gmatrix)

            !end if

            call start_utimer(id_inutile)
            ! Test que les deux méthodes donnent bien le même résultat
            residu = abs(sum(b_co - b_g))

            !print *, "-----------------------------"
            !print *, "b_c=", b_co
            !print *, "-----------------------------"
            !print *, "b_g=", b_g

            call stop_utimer(id_inutile)

         end do
      end do

      if (residu > 0.0001) print *, "residu > 0.0001"

      call start_utimer(id_inutile)
      ! deallocate the memory used by the system
      deallocate(b_ci)
      deallocate(b_co)
      deallocate(b_g)
      call stop_utimer(id_inutile)
   !end do

   !CALL write_utimer

end program test_case_vs_gmatrix
