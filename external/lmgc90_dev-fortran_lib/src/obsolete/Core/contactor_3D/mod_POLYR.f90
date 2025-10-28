
! !!!------------------------------------------------------------------------
!   subroutine gmv_draw_onePOLYR(ibdy, &
!        nb_pt,coor,nb_face,face, &
!        nb_ptc,coor_ptc)

!     implicit none

!     integer :: ibdy
!     integer :: nb_pt,nb_face,nb_ptc
!     real(kind=8),dimension(nb_pt,3)   :: coor
!     integer     ,dimension(3,nb_face) :: face
!     real(kind=8),dimension(nb_ptc,3)     :: coor_ptc   

!     integer :: nfich
!     integer :: i

!     character(len=11) :: nom

!     nfich=get_io_unit()

!     nom =' '
!     write(nom,'("_bdy_",I0)') ibdy
!     open(nfich,file='visu'//trim(nom)//'.txt')

!     write(nfich,'("gmvinput ascii")')
!     !
!     write(nfich,'("nodes ",I0)') nb_ptc
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,1)
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,2)
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,3)
!     !
!     write(nfich,'("cells 0")')
!     !
!     write(nfich,'("polygons")')
!     do i=1,nb_face
!        write(nfich,'(2(1x,I5))') ibdy,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor(face(:,i),1)
!        write(nfich,'(8(1x,E12.5))') coor(face(:,i),2)
!        write(nfich,'(8(1x,E12.5))') coor(face(:,i),3)
!     enddo
!     write(nfich,'("endpoly")')
!     !
! !!$  write(nfich,'("tracers",1x,I1)') nb_ptc
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,1)
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,2)
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,3)
! !!$  write(nfich,'("rien")')
! !!$  write(nfich,'(8(1x,I5))') (i-1,i=1,nb_ptc)
! !!$  write(nfich,'("endtrace")')
!     !
!     write(nfich,'("endgmv")')  
!     !
!     close(nfich)
!   end subroutine gmv_draw_onePOLYR
