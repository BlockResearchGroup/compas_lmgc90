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

!> module managing "antagoniste polyline" contactor
 MODULE ALpxx                                       

 USE overall
 USE utilities
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx
  
 use algebra, only : length2
!$ use timer

 IMPLICIT NONE

 PRIVATE


 ! alpxx2bdyty(1,itac): 
 !   serial number of body MAILx to which is attached the contactor 
 !   ALpxx numbered itac in the list of all contactors ALpxx 
 ! alpxx2bdyty(2,itac): 
 !   serial number of contactor ALpxx itac in the list of contactors 
 !   of any kind attached to a body  alpxx2bdyty(1,itac)
 ! clxxx2bdyty(3,itac):
 !   kind of model i_mailx, etc
 
 integer, dimension( : , : ), allocatable, target, public :: alpxx2bdyty  
 INTEGER :: nb_ALpxx

 TYPE,PUBLIC :: T_ALpxx
   ! serial number of body mecaMAILx
   INTEGER                       :: ibdyty
   ! serial number of nodes in the body ibdyty    
   INTEGER,DIMENSION(:),POINTER  :: idata => null()
   logical                       :: precon

   INTEGER,DIMENSION(:),POINTER  :: iblmty => null()
   INTEGER,DIMENSION(:),POINTER  :: iedge  => null()
   
 END TYPE T_ALpxx

 TYPE(T_ALpxx), DIMENSION(:), ALLOCATABLE :: l_ALpxx

 PUBLIC l_ALpxx

 PUBLIC :: read_bodies_ALPxx,&
           set_precon_node_ALpxx, &
           get_connec_ALpxx , &
           get_all_data_ALpxx

 PUBLIC :: nullify_reac_ALpxx,nullify_vlocy_ALpxx,comp_vlocy_Alpxx,add_reac_ALpxx, &
           get_vlocy_ALpxx,get_coorTT_ALpxx,get_nb_ALpxx, get_coor_support_ALxxx, &
           get_nb_node_ALpxx, &
           get_Vwear_ALpxx,put_Vwear_ALpxx, & 
           get_ENT_ALpxx, &
           set_data_ALpxx, &
           get_nodes_ALxxx, &
           get_visible_ALpxx, &
           all_dof_driven_ALxxx

 public :: clean_memory_ALpxx

 CONTAINS

!!!------------------------------------------------------------------------
 SUBROUTINE read_bodies_ALpxx

   IMPLICIT NONE

   INTEGER :: nb_MAILx,idata_sz
   INTEGER :: M_ibdyty,itacty,errare,id
   INTEGER, DIMENSION(2) :: ALidata
                            !123456789012345678
   CHARACTER(len=18) :: IAM='ALpxx::read_bodies'
   CHARACTER(len=80) :: cout

   nb_MAILx=get_nb_MAILx()

   IF (nb_MAILx == 0) RETURN

   nb_ALpxx=0

   DO M_ibdyty=1,nb_MAILx   
     DO itacty=1,get_nb_tacty_MAILx(M_ibdyty)
         IF( get_tacID_MAILx(M_ibdyty,itacty) == 'ALpxx')  nb_ALpxx=nb_ALpxx+1
     END DO 
   END DO

   WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_ALpxx,'ALpxx found'
   CALL LOGMES(cout)

   IF (nb_ALpxx == 0) RETURN

   allocate(alpxx2bdyty(3,nb_ALpxx),stat=errare)

   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating alpxx2bdyty')
   END IF

   ALLOCATE(l_ALpxx(nb_ALpxx),stat=errare)

   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating l_ALpxx')
   END IF

   if( .not. allocated(M2meca) ) then
     call faterr(IAM,'Please call LoadModels before LoadTactors')
   end if

   nb_ALpxx=0

   DO M_ibdyty=1,nb_MAILx
      DO itacty=1,get_nb_tacty_MAILx(M_ibdyty)
       IF (get_tacID_MAILx(M_ibdyty,itacty) == 'ALpxx') THEN
          nb_ALpxx=nb_ALpxx+1

          !alpxx2bdyty(1,itac) : serial number of body MAILx to which is attached the 
          !                      contactor ALpxx numbered itac in the list of all 
          !                      contactors ALpxx 
          alpxx2bdyty(1,nb_ALpxx)=M_ibdyty

          !alpxx2bdyty(2,itac) : serial number of contactor ALxxx itac in the list of 
          !                      contactors of any kind attached to body alxxx2bdyty(1,itac)          
          alpxx2bdyty(2,nb_ALpxx)=itacty

          !alpxx2bdyty(3,itac) : type of body the contactor is attached to
          alpxx2bdyty(3,nb_ALpxx)=i_mailx 
          
          l_ALpxx(nb_ALpxx)%ibdyty = M2meca(M_ibdyty)%bdyty

          CALL get_idata_sz_MAILx(M_ibdyty,itacty,idata_sz)

          ALLOCATE(l_ALpxx(nb_ALpxx)%idata(idata_sz+1),stat=errare)

          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating l_ALpxx%idata')
          END IF

          CALL get_idata_MAILx(M_ibdyty,itacty,l_ALpxx(nb_ALpxx)%idata(1:idata_sz+1))

          l_ALpxx(nb_ALpxx)%precon  = .false.

          allocate(l_ALpxx(nb_ALpxx)%iblmty(idata_sz),l_ALpxx(nb_ALpxx)%iedge(idata_sz))

          do id=1,idata_sz
             call get_edge_MAILx(M_ibdyty,l_ALpxx(nb_ALpxx)%idata(id),l_ALpxx(nb_ALpxx)%idata(id+1), &
                                 l_ALpxx(nb_ALpxx)%iblmty(id),l_ALpxx(nb_ALpxx)%iedge(id))
             !print *,'ALpxx attached to avatar ', M_ibdyty, &
             !        ' ALxxx ', id, &
             !        ' attached to element ',l_ALpxx(nb_ALpxx)%iblmty(id), &
             !        ' edge ',l_ALpxx(nb_ALpxx)%iedge(id)  

             
          enddo
          
       END IF
     END DO 
   END DO

 END SUBROUTINE read_bodies_ALPxx
!------------------------------------------------------------------------ 

 function get_connec_ALpxx()
   implicit none
   integer, dimension(:), pointer :: get_connec_ALpxx
   !
   integer :: nb_al, i_alp, i_al, idx

   get_connec_ALpxx => null()
   if( nb_ALpxx == 0 ) return

   ! counting
   nb_al = 1
   do i_alp = 1, nb_ALpxx
     nb_al = nb_al + 3*size(l_ALpxx(i_alp)%idata)
   end do

   allocate(get_connec_ALpxx(nb_al))

   nb_al =  0
   idx = 2
   do i_alp = 1, nb_ALpxx
     do i_al = 2, size(l_ALpxx(i_alp)%idata)
       get_connec_ALpxx(idx)   = 2
       get_connec_ALpxx(idx+1) = l_ALpxx(i_alp)%idata(i_al-1)
       get_connec_ALpxx(idx+2) = l_ALpxx(i_alp)%idata(i_al  )
       idx = idx + 3
       nb_al = nb_al + 1
     end do
   end do
   get_connec_ALpxx(1) = nb_al

 end function

 function get_normal_(ibdyty, inoda, inodb)
   implicit none
   integer, intent(in) :: ibdyty, inoda, inodb
   real(kind=8), dimension(2) :: get_normal_
   !
   real(kind=8), dimension(2) :: vec
   real(kind=8) :: norm, tmp

   get_normal_ = get_cooref_nodty_mecaMAILx(ibdyty, inodb) &
                -get_cooref_nodty_mecaMAILx(ibdyty, inoda)

   norm = length2(get_normal_)
   if( norm > 0.d0 ) get_normal_ = get_normal_ / norm
   tmp = get_normal_(1)
   get_normal_(1) = -get_normal_(2)
   get_normal_(2) = tmp

 end function

 subroutine get_all_data_ALpxx(idata, rdata)
   implicit none
   integer     , dimension(:,:), pointer :: idata
   real(kind=8), dimension(:,:), pointer :: rdata
   !
   integer :: nb_al, i_alp, i_al, idx

   idata => null()
   rdata => null()

   if( nb_ALpxx == 0 ) return

   ! counting
   nb_al = 0
   do i_alp = 1, nb_ALpxx
     nb_al = nb_al + size(l_ALpxx(i_alp)%idata)
   end do

   allocate(idata(3,nb_al))
   allocate(rdata(3,nb_al))
   idata = 0
   rdata = 0.d0

   idx = 1
   do i_alp = 1, nb_ALpxx
     do i_al = 2, size(l_ALpxx(i_alp)%idata)
       idata(1,idx) = l_ALpxx(i_alp)%ibdyty
       idata(2,idx) = i_alp
       idata(3,idx) = i_al

       rdata(1:2,idx) = get_normal_( l_ALpxx(i_alp)%ibdyty       ,&
                                     l_ALpxx(i_alp)%idata(i_al-1),&
                                     l_ALpxx(i_alp)%idata(i_al  ) &
                                   )
       idx = idx + 1
     end do
   end do

 end subroutine

!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES 
!
!------------------------------------------------------------------------ 
SUBROUTINE nullify_reac_ALpxx(ialpxx,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: ialpxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_alpxx(ialpxx)%ibdyty

   CALL nullify_reac_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_reac_ALpxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE nullify_vlocy_ALpxx(ialpxx,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: ialpxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_alpxx(ialpxx)%ibdyty

   CALL nullify_vlocy_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_vlocy_ALpxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE comp_vlocy_Alpxx(ialpxx,ialxxx,storage,need_full_vlocy)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: ialpxx,ialxxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  
   logical            :: need_full_vlocy

   ibdyty=l_ALpxx(ialpxx)%ibdyty

   if (l_ALpxx(ialpxx)%precon .and. .not. need_full_vlocy ) then
     CALL comp_vlocy_bynode_mecaMAILx(ibdyty,(/l_ALpxx(ialpxx)%idata(ialxxx),l_ALpxx(ialpxx)%idata(ialxxx+1)/),storage)
   else
     CALL comp_vlocy_mecaMAILx(ibdyty,storage)
   endif

END SUBROUTINE comp_vlocy_Alpxx
!------------------------------------------------------------------------
SUBROUTINE add_reac_ALpxx(ialpxx,ialxxx,reac,apab,storage)
   IMPLICIT NONE 
   INTEGER     ,INTENT(in) :: ialxxx,ialpxx

   REAL(kind=8),DIMENSION(2) :: reac,tmp

   INTEGER :: storage  
   INTEGER :: ibdyty,numnoda,numnodb
   INTEGER ::M_ibdyty,M_itacty 

   REAL(kind=8) :: apab

   ibdyty =l_ALpxx(ialpxx)%ibdyty

   numnoda=l_ALpxx(ialpxx)%idata(ialxxx)
   numnodb=l_ALpxx(ialpxx)%idata(ialxxx+1)

   M_ibdyty = alpxx2bdyty(1,ialpxx)
   M_itacty = alpxx2bdyty(2,ialpxx)

   tmp = (1.d0-apab)*reac
   CALL add_reac_nodty_mecaMAILx(ibdyty,numnoda,tmp,storage)

   tmp = apab*reac
   CALL add_reac_nodty_mecaMAILx(ibdyty,numnodb,tmp,storage)

END SUBROUTINE add_reac_ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_vlocy_ALpxx(ialpxx,ialxxx,storage,vlocy,apab)
  IMPLICIT NONE
  INTEGER :: ialpxx,ialxxx

  INTEGER :: storage  

  INTEGER ::   ibdyty,  itacty,  numnoda,  numnodb
  !INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8) :: apab

  REAL(kind=8),DIMENSION(2) :: vlocy,vlocya,vlocyb

  !M_ibdyty = alpxx2bdyty(1,ialpxx)
  !M_itacty = alpxx2bdyty(2,ialpxx)

  ibdyty= l_alpxx(ialpxx)%ibdyty
  numnoda=l_alpxx(ialpxx)%idata(ialxxx)
  numnodb=l_alpxx(ialpxx)%idata(ialxxx+1)

  SELECT CASE(storage)
    CASE(iV____)
        vlocya=get_V_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_V_nodty_mecaMAILx(ibdyty,numnodb)

        vlocy = (1.d0 -apab)*vlocya + apab*vlocyb
    CASE(iVbeg_)
        vlocya=get_Vbegin_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vbegin_nodty_mecaMAILx(ibdyty,numnodb)

        vlocy = (1.d0 -apab)*vlocya + apab*vlocyb
    CASE(iVfree)
        vlocya=get_Vfree_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vfree_nodty_mecaMAILx(ibdyty,numnodb)

        vlocy = (1.d0 -apab)*vlocya + apab*vlocyb
    CASE(iVaux_)
        vlocya=get_Vaux_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vaux_nodty_mecaMAILx(ibdyty,numnodb)

        vlocy = (1.d0 -apab)*vlocya + apab*vlocyb
    CASE(iVddm_)
        vlocya=get_Vddm_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vddm_nodty_mecaMAILx(ibdyty,numnodb)

        vlocy = (1.d0 -apab)*vlocya + apab*vlocyb
    CASE default
  END SELECT

END SUBROUTINE get_vlocy_ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_coorTT_ALpxx(ialpxx,nb_node_ALPxx,coor_ALpxx)
  IMPLICIT NONE
  integer(kind=4), intent(in) :: ialpxx, nb_node_ALpxx
  real(kind=8), dimension(2,nb_node_ALpxx),intent(out) :: coor_ALpxx
  !
  integer(kind=4) :: ibdyty, ialxxx, numnod

  ibdyty  = l_ALpxx(ialpxx)%ibdyty

  DO ialxxx=1,nb_node_ALpxx

    numnod = l_ALpxx(ialpxx)%idata(ialxxx)
  
    coor_ALpxx(1:2,ialxxx) = get_coorTT_nodty_mecaMAILx(ibdyty,numnod)

  ENDDO

END SUBROUTINE get_coorTT_ALpxx
!------------------------------------------------------------------------ 
subroutine get_coor_support_ALxxx(ialpxx,ialxxx,coor_support)
  implicit none
  integer(kind=4), intent(in) :: ialpxx, ialxxx
  real(kind=8), dimension(2,2), intent(out) :: coor_support
  !
  integer(kind=4) :: ibdyty, numnod

  ibdyty  = l_ALpxx(ialpxx)%ibdyty

  numnod = l_ALpxx(ialpxx)%idata(ialxxx)
  coor_support(1:2,1) = get_coorTT_nodty_mecaMAILx(ibdyty,numnod)

  numnod = l_ALpxx(ialpxx)%idata(ialxxx+1)
  coor_support(1:2,2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnod)

end subroutine get_coor_support_ALxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_ALpxx(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_ALpxx
  
   get_nb_ALpxx = nb_ALpxx
  
 END FUNCTION get_nb_ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_node_ALpxx(ialpxx)

   IMPLICIT NONE
   INTEGER :: ialpxx
   INTEGER :: get_nb_node_ALpxx
  
   get_nb_node_ALpxx = SIZE(l_Alpxx(ialpxx)%idata)

 END FUNCTION get_nb_node_ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE get_Vwear_ALpxx(ialpxx,ialxxx,vlocy,apab)
  IMPLICIT NONE
  INTEGER :: ialpxx,ialxxx

  INTEGER ::   ibdyty,  itacty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8) :: apab

  REAL(kind=8),DIMENSION(2) :: vlocy,vlocya,vlocyb

  M_ibdyty = alpxx2bdyty(1,ialpxx)
  M_itacty = alpxx2bdyty(2,ialpxx)

  ibdyty= l_alpxx(ialpxx)%ibdyty
  numnoda=l_alpxx(ialpxx)%idata(ialxxx)
  numnodb=l_alpxx(ialpxx)%idata(ialxxx+1)

  vlocya=get_Vwear_nodty_mecaMAILx(ibdyty,numnoda)
  vlocyb=get_Vwear_nodty_mecaMAILx(ibdyty,numnodb)
  vlocy = (1.d0 -apab)*vlocya + apab*vlocyb

END SUBROUTINE get_Vwear_ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE put_Vwear_ALpxx(ialpxx,ialxxx,anvwear,apab)
  IMPLICIT NONE 
   INTEGER     ,INTENT(in) :: ialxxx,ialpxx

   REAL(kind=8),DIMENSION(2) :: anvwear

   INTEGER :: ibdyty,numnoda,numnodb
   INTEGER ::M_ibdyty,M_itacty 

   REAL(kind=8) :: apab

   ibdyty =l_ALpxx(ialpxx)%ibdyty

   numnoda=l_ALpxx(ialpxx)%idata(ialxxx)
   numnodb=l_ALpxx(ialpxx)%idata(ialxxx+1)

   M_ibdyty = alpxx2bdyty(1,ialpxx)
   M_itacty = alpxx2bdyty(2,ialpxx)

   CALL put_Vwear_nodty_mecaMAILx(ibdyty,numnoda,anvwear)

   CALL put_Vwear_nodty_mecaMAILx(ibdyty,numnodb,anvwear)

END SUBROUTINE put_Vwear_ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_ENT_ALpxx(ialpxx)

   IMPLICIT NONE
   INTEGER :: ialpxx
   INTEGER :: get_ENT_ALpxx
  
   get_ENT_ALpxx = get_entity_mecaMAILx(l_Alpxx(ialpxx)%ibdyty)

 END FUNCTION get_ENT_ALpxx
!------------------------------------------------------------------------

!!!----------------------------------------------------------------------
 logical function all_dof_driven_ALxxx(iALpxx, i_vert_a, i_vert_b)
    implicit none
    integer, intent(in) :: iALpxx, i_vert_a, i_vert_b
    !
    logical :: ddof
    integer :: ibdyty, i_node

    ibdyty = l_Alpxx(ialpxx)%ibdyty
    i_node = l_ALpxx(iALpxx)%idata(i_vert_a)

    ddof = is_node_dof_driven_mecaMAILx(ibdyty, i_node)
    i_node = l_ALpxx(iALpxx)%idata(i_vert_b)

    all_dof_driven_ALxxx = is_node_dof_driven_mecaMAILx(ibdyty, i_node)

    all_dof_driven_ALxxx = all_dof_driven_ALxxx .and. ddof
 end function
!!!-----------------------------------------------------------------------



 subroutine set_precon_node_ALpxx
   implicit none

   integer :: nb_MAILx
   integer :: ibdyty,itacty,ialpxx,ialxxx,numnod

   nb_MAILx=get_nb_MAILx()

   if (nb_MAILx == 0) return

   ialpxx = 0
   do ibdyty=1,nb_MAILx
     do itacty=1,get_nb_tacty_MAILx(ibdyty)
       if (get_tacID_MAILx(ibdyty,itacty) == 'ALpxx') then
          iALpxx=iALpxx+1
          do ialxxx=1,size(l_Alpxx(ialpxx)%idata)
            numnod = l_ALpxx(ialpxx)%idata(ialxxx)
            call set_precon_node_mecaMAILx(ibdyty,numnod)  
          enddo
          l_ALpxx(ialpxx)%precon = .true.          
       endif
     enddo
   enddo

 end subroutine

 !> \brief use anonymous data to initialize a contactor
 subroutine set_data_ALpxx(idata, rdata, node_list, nb_support, support, Brd)
   implicit none
   !> [in] idata: anonymous integer data (nb_al, [id_a, id_b]*nb_al)
   integer(kind=4), dimension(:), pointer :: idata
   !> [in] rdata: anonymous real data (NULL)
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

   nb_support = 2*idata(1)

   allocate(node_list(nb_support))
   node_list(1:nb_support) = idata(2:nb_support+1)
   
   if( size(idata) /= nb_support+1 ) then
     call faterr('ALpxx::set_data_ALpxx','inconsistent idata size')
   end if
 
   allocate( support(nbDIME,nb_support) )

   support = 0.d0
   
   Brd = 0.D0

 end subroutine

 function get_nodes_ALxxx(ialpxx,ialxxx)
   implicit none
   integer(kind=4), intent(in)   :: ialpxx
   integer(kind=4), intent(in)   :: ialxxx
   integer(kind=4), dimension(2) :: get_nodes_ALxxx

   get_nodes_ALxxx(1:2) = l_ALpxx(ialpxx)%idata(ialxxx:ialxxx+1)
 end function

 function get_visible_ALpxx(itact)

   implicit none

   integer(kind=4) :: itact 
   logical :: get_visible_ALpxx
   
   get_visible_ALpxx = get_visible_mecaMAILx(l_ALpxx(itact)%ibdyty)
 
 end function get_visible_ALpxx  

 subroutine clean_memory_ALpxx()
   implicit none
   integer(kind=4) :: i

   nb_ALpxx = 0

   if( allocated(alpxx2bdyty) ) deallocate(alpxx2bdyty)

   if( allocated(l_ALpxx) ) then
     do i = 1, size(l_ALpxx)
       if( associated(l_ALpxx(i)%idata) ) then
           deallocate(l_ALpxx(i)%idata)
           nullify(l_ALpxx(i)%idata)
       end if
       if( associated(l_ALpxx(i)%iblmty) ) then
           deallocate(l_ALpxx(i)%iblmty)
           nullify(l_ALpxx(i)%iblmty)
       end if
       if( associated(l_ALpxx(i)%iedge) ) then
           deallocate(l_ALpxx(i)%iedge)
           nullify(l_ALpxx(i)%iedge)
       end if
     end do
     deallocate(l_ALpxx)
   end if

 end subroutine

 END MODULE ALpxx

