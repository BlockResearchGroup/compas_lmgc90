module ExternalDetection

use overall, only : faterr

!use utilites
!use polyr

implicit none

! Xper
! integer(kind=4), parameter :: nbcpf = 2
! integer(kind=4),dimension(:,:),allocatable :: interface2cd, interface2an
! integer(kind=4) :: nb_contacts




contains

! la routine de detection qui va bien
subroutine detection_indian(id1,id2,adist,nb_ctc,PT_CTC,overlap,n)!,t,s)

integer :: id1, id2, nb_ctc
real(kind=8) :: adist
real(kind=8), dimension(:,:), pointer :: PT_CTC, n
real(kind=8), dimension(:),   pointer :: overlap

  call faterr('ExternalDetection::detection_indian','not implemented')

end subroutine detection_indian

function externalDetection_get_nb()
  implicit none
  integer(kind=4) :: externalDetection_get_nb

  ! Xper
  ! !
  ! integer(kind=4) :: nb_MAILX, nb_cd_interfaces, nb_an_interfaces, nb_contacts, nb_interfaces, nbnodes_bdy, nbsides
  ! integer(kind=4) :: ibdy, icd, ian

  ! integer :: is_perio
  ! real(kind=8),dimension(3) :: vec_perio

  ! integer(kind=4) :: isref,icheck

  ! integer(kind=4) :: i,j,k
  ! integer(kind=4) :: id
  ! integer,dimension(:),allocatable :: ids_cand
  ! integer,dimension(:),allocatable :: tempo,tempo1,tempo2

  call faterr('ExternalDetection::get_nb','not implemented')

  ! Xper
  ! call PELnbinterfacesCandidate(nb_interfaces)

  ! ! 2 = nbcpf dans Xper_wrap_user : le nombre de points par CL
  ! external_detection_get_nb = nbcpf * nb_interfaces

  ! ! if no refinement, return
  ! call PELisRefinement( isref )
  ! call PELcheckRefinement( icheck )
  ! if( isref == 1 .and. icheck == 0 ) then
  !   return
  ! end if

  ! nb_contacts = external_detection_get_nb

  ! ! TODO: attention il faut initialiser avec le nbsides total venant de Xper !!!! 
  ! if (allocated(interface2cd)) deallocate(interface2cd)
  ! allocate( interface2cd( 2, nb_contacts   ) )
  ! if (allocated(interface2an)) deallocate(interface2an)
  ! allocate( interface2an( 3, nb_interfaces ) )

  ! call PELnbBodies(nb_MAILx)
  ! allocate(tempo(nb_MAILx))

  ! tempo = 0
  
  ! call PELnbInterfacesCandidate( nb_cd_interfaces )

  ! if(allocated(ids_cand)) deallocate(ids_cand)
  ! allocate( ids_cand(nb_cd_interfaces) )

  ! do i = 1, nb_cd_interfaces
    
  !   call PELinterfaceIDsideCandidate(i,icd)
  !   call PELinterfaceCandidate(i, ibdy, is_perio, vec_perio)

  !   ids_cand( i ) = icd

  !   do k = 1, nbcpf

  !      tempo(ibdy) = tempo(ibdy) + 1

  !      interface2cd(1, nbcpf*(i-1) + k) = ibdy
  !      interface2cd(2, nbcpf*(i-1) + k) = tempo(ibdy)

  !   end do

  ! end do

  ! call PELnbInterfacesAntagonist( nb_an_interfaces )

  ! do i = 1, nb_an_interfaces

  !    call PELinterfaceAntagonist(i,ibdy)

  !    call PELinterfacenbNodesAntagonist(i,nbnodes_bdy)
  !    if (allocated(tempo1)) deallocate(tempo1)
  !    allocate(tempo1(nbnodes_bdy))
  !    call PELinterfaceNbSidesAntagonist(i,nbsides)
  !    if (allocated(tempo2)) deallocate(tempo2)
  !    allocate(tempo2(nbsides))         
  !    call PELinterfacelistnodesAntagonistTwoD(i,tempo1,tempo2)

  !    tempo(ibdy) = tempo(ibdy) + 1

  !    do j = 1, nbsides

  !       call PELinterfaceIDSideAntagonist(i,j,ian)

  !       do id = 1, nb_cd_interfaces

  !           if( ids_cand( id ) == ian ) then

  !              interface2an(1, id) = ibdy
  !              interface2an(2, id) = tempo(ibdy)
  !              interface2an(3, id) = 1+tempo2(j)

  !              exit
  !           end if
  !       end do

  !    end do
     
  !    deallocate(tempo1)
  !    deallocate(tempo2)

  ! end do

end function externalDetection_get_nb

subroutine externalDetection_get_contact(icdan, icdbdy, icdtact, ianbdy, iantact, inode)
  implicit none
  !>
  integer(kind=4), intent(in) :: icdan
  integer(kind=4), intent(out) :: icdbdy
  integer(kind=4), intent(out) :: icdtact
  integer(kind=4), intent(out) :: ianbdy
  integer(kind=4), intent(out) :: iantact
  integer(kind=4), intent(out) :: inode
  
  call faterr('ExternalDetection::get_contact','not implemented')

  ! Xper
  ! icdbdy  = interface2cd( 1, icdan )
  ! icdtact = interface2cd( 2, icdan )
  ! ianbdy  = interface2an( 1, (icdan+1) / nbcpf )
  ! iantact = interface2an( 2, (icdan+1) / nbcpf )
  ! inode   = interface2an( 3, (icdan+1) / nbcpf )
  
end subroutine externalDetection_get_contact

end module ExternalDetection

