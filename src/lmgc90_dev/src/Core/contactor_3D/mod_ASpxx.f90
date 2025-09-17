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
 MODULE ASpxx                                       

 USE overall
 USE utilities
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx
  
 USE DiscreteGeometry
 USE xSpxx

!$ use timer

 IMPLICIT NONE

 PRIVATE

 integer, dimension( : , : ), allocatable, &
      target, public :: aspxx2bdyty  ! aspxx2bdyty(1,itac): 
                                     ! serial number of body MAILx to which is attached the contactor 
                                     ! Aspxx numbered itac in the list of all contactors Aspxx 
                                     ! aspxx2bdyty(2,itac): 
                                     ! serial number of contactor Aspxx itac in the list of contactors
                                     ! of any kind attached to a body  aspxx2bdyty(1,itac)


 INTEGER :: nb_ASpxx

 TYPE(T_xSpxx), DIMENSION(:), ALLOCATABLE :: l_ASpxx

 PUBLIC l_ASpxx

 PUBLIC :: load_tactors_ASpxx,&
           set_precon_node_ASpxx, &
           get_connec_ASpxx, &
           get_all_data_ASpxx

 PUBLIC :: get_nb_ASpxx, &
           get_ENT_ASpxx, & 
           get_nb_vertex_ASpxx, &
           get_HE_Hdl_ASpxx, &
           get_color_ASpxx, &
           get_visible_ASpxx, &
           get_coorTT_ASpxx, &
           nullify_reac_ASpxx, &
           nullify_vlocy_ASpxx, &
           comp_vlocy_ASpxx, &
           add_reac_ASxxx, &
           get_vlocy_ASxxx, &
           all_dof_driven_ASxxx, &
           !           
           get_nb_ASxxx, &       ! attention ces fonctions sont relatives aux ASxxt 
           is_singleton_ASpxx, & ! pour savoir si un ASpxx contient 1 seul element ASxxx 
           get_dof_ASpxx, &
           get_interp_ASpxx

 public :: clean_memory_ASpxx

!get_Vwear_Aspxx,put_Vwear_Aspxx, & 


 CONTAINS

!!!------------------------------------------------------------------------
 SUBROUTINE load_tactors_ASpxx

!fd attention les ASpxx sont des conteneurs de ASxxx !!

   IMPLICIT NONE

   ! ***
                            !1234567890123456789
   CHARACTER(len=19) :: IAM='ASpxx::load_tactors'

   ! ***

   INTEGER            :: nb_MAILx
   INTEGER            :: ibdyty,itacty,errare
   CHARACTER(len=103) :: cout

   nb_MAILx=get_nb_MAILx()

   !fd ici on compte les containers de Asxxx 
   nb_ASpxx=0

   IF (nb_MAILx == 0) RETURN

   DO ibdyty=1,nb_MAILx   
     DO itacty=1,get_nb_tacty_MAILx(ibdyty)
       IF( get_tacID_MAILx(ibdyty,itacty) == 'ASpxx')  then
         nb_ASpxx=nb_ASpxx+1
       ENDIF
     END DO 
   END DO

   WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_ASpxx,'ASpxx found'
   CALL LOGMES(cout)

   IF (nb_ASpxx == 0) RETURN

   allocate(aspxx2bdyty(3,nb_ASpxx),stat=errare)

   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating aspxx2bdyty')
   END IF

   ALLOCATE(l_ASpxx(nb_ASpxx),stat=errare)

   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating l_ASpxx')
   END IF

   if( .not. allocated(M2meca) ) then
     call faterr(IAM,'Please call LoadModels before LoadTactors')
   end if

   call load_tactors_xSpxx('A',l_ASpxx,aspxx2bdyty)
   aspxx2bdyty(3,:) = i_mailx

 END SUBROUTINE load_tactors_ASpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE nullify_reac_ASpxx(iASpxx,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iaspxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_aspxx(iaspxx)%ibdyty

   CALL nullify_reac_mecaMAILx(ibdyty,storage)

   !  if (storage == 2) print*,'nullify Ireac'
   !  if (storage == 100) print*,'nullify Iaux'
   !  if (storage == 70) print*,'nullify Rfree'

END SUBROUTINE nullify_reac_Aspxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE nullify_vlocy_ASpxx(iASpxx,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iaspxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_aspxx(iaspxx)%ibdyty

   CALL nullify_vlocy_mecaMAILx(ibdyty,storage)

  !if (storage == 0) print*,'nullify Vbeg'
  !if (storage == 99) print*,'nullify Vaux'
  !if (storage == 69) print*,'nullify Vfree'

END SUBROUTINE nullify_vlocy_Aspxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE comp_vlocy_ASpxx(iASpx,iASxt,storage,need_full_vlocy)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iASpx,iASxt
   INTEGER            :: storage  
   logical            :: need_full_vlocy

   integer            :: ibdyty, iASxx, num(8), isize

   ibdyty=l_ASpxx(iASpx)%ibdyty

   call get_num_xSPxx(l_ASpxx(iASpx),iASxt,num,isize)

   if (l_ASpxx(iASpx)%is_precon .and. .not. need_full_vlocy ) then
     !fd on ne calcule que pour les noeuds de la face qui nous concerne
     call comp_vlocy_bynode_mecaMAILx(ibdyty,num(1:isize),storage)
   else
     !fd on calcule pour tous les noeuds
     CALL comp_vlocy_mecaMAILx(ibdyty,storage)
   end if

  !if (storage == 1) print*,'comp V=M^-1 Ireac'
  !if (storage == 2) print*,'comp Vaux=M^-1 Ireac'
  !if (storage == 3) print*,'comp Vaux=M^-1 Iaux'

END SUBROUTINE comp_vlocy_ASpxx
!------------------------------------------------------------------------
SUBROUTINE add_reac_ASxxx(iaspx,iasxt,tweight,reac,storage)
   IMPLICIT NONE 
   INTEGER     ,INTENT(in)   :: iASpx,iASxt
   REAL(kind=8),dimension(3) :: tweight
   REAL(kind=8),DIMENSION(3) :: reac

   INTEGER :: storage  
   INTEGER :: ibdyty

   INTEGER     ,dimension(8) :: num
   REAL(kind=8),DIMENSION(3) :: lreac
   REAL(kind=8),dimension(8) :: interp

   INTEGER :: i, isize

                                      !123456789012345
   CHARACTER(len=15),parameter :: IAM='ASpxx::add_reac'

   ibdyty =l_Aspxx(iaspx)%ibdyty

   call get_num_xSPxx(l_ASpxx(iASpx),iASxt,num,isize)

   call get_interp_xSPxx(l_ASpxx(iASpx),iASxt,tweight,interp,isize)

   !if (storage == iIaux_) then
   !   print*,'entree corps',ibdyty
   !   print*,reac(:)
   !   print*,num
   !   print*,interp
   !endif
      
   do i=1,isize
     lreac(:) = interp(i)* reac(:)

     !  if (storage == 2) print*,'add Ireac'
     !  if (storage == 70) print*,'add Rfree'
     !if (storage == iIaux_) then
     !  print*,'< ',IAM
     !  print*,' add Iaux'
     !  print*,ibdyty, iaspx         
     !  print*,i,num(i)
     !  print*,interp(i)
     !  print*,'sortie ',lreac
     !  print*,IAM,' >'
     !endif

     CALL add_reac_nodty_mecaMAILx(ibdyty,num(i),lreac,storage)
   enddo

END SUBROUTINE add_reac_ASxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_vlocy_ASxxx(iASpx,iASxt,tweight,storage,vlocy)

   IMPLICIT NONE

   INTEGER                     :: iASpx,iASxt
   REAL(kind=8),dimension(3)   :: tweight
   INTEGER                     :: storage  
   REAL(kind=8),DIMENSION(3)   :: vlocy

                                      !1234567890123456
   CHARACTER(len=16),parameter :: IAM='ASpxx::get_vlocy'


   INTEGER                     :: ibdyty
   REAL(kind=8),dimension(8)   :: interp
   INTEGER     ,dimension(8)   :: num
   INTEGER                     :: i, isize

   ibdyty= l_ASpxx(iASpx)%ibdyty

   ! print*,'----'
   ! print*,'get_vlocy_ASxxx'
   ! print*,iASpx,ibdyty,iASxt   
   
   call get_num_xSPxx(l_ASpxx(iASpx),iASxt,num,isize)
   
   call get_interp_xSPxx(l_ASpxx(iASpx),iASxt,tweight,interp,isize)

   vlocy=0.d0
   SELECT CASE(storage)
    CASE(iV____)
        do i=1,isize
          vlocy(:) = vlocy(:) + interp(i)*get_V_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE(iVbeg_)
        do i=1,isize
          vlocy(:) = vlocy(:) + interp(i)*get_Vbegin_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE(iVfree)
        do i=1,isize
          vlocy(:) = vlocy(:) + interp(i)*get_Vfree_nodty_mecaMAILx(ibdyty,num(i))
          !write(*,'(I0,1x,I0,3(1x,D12.5))') ibdyty,num(i),get_Vfree_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE(iVaux_)
        do i=1,isize
           vlocy(:) = vlocy(:) + interp(i)*get_Vaux_nodty_mecaMAILx(ibdyty,num(i))
        enddo
        !print*,'< ',IAM
        !print*,ibdyty, iaspx
        !write(*,'(A,3(1x,D12.5))') 'Vaux ',vlocy
        !print*,IAM,' >'
    CASE(iVddm_)
        do i=1,isize
          vlocy(:) = vlocy(:) + interp(i)*get_Vddm_nodty_mecaMAILx(ibdyty,num(i))
        enddo
    CASE default
        call FATERR(IAM,'Unexpected value for storage')
  END SELECT

  !if (storage == 0) print*,'get Vbeg'
  !if (storage == 99) print*,'get Vaux'
  !if (storage == 69) then 
  ! print*,'get Vfree'
  ! write(*,'(4(1x,I0))') num(1:isize)
  ! write(*,'(4(1x,D12.5))') interp(1:isize)
  ! write(*,'(3(1x,D12.5))') vlocy
  !endif
END SUBROUTINE get_vlocy_ASxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
subroutine get_coorTT_ASpxx(iASpxx,coorTT_ASxxx)
  IMPLICIT NONE

  INTEGER :: iASpxx
  REAL(kind=8),DIMENSION(:,:) :: coorTT_ASxxx


  call get_coorTT_xSpxx(l_ASpxx(iASpxx),coorTT_ASxxx)


END subroutine
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_ASpxx(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_ASpxx
  
   get_nb_ASpxx = nb_ASpxx
  
 END FUNCTION get_nb_ASpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_ASxxx(iASpxx)

   IMPLICIT NONE
   INTEGER :: iASpxx
   INTEGER :: get_nb_ASxxx
  
   get_nb_ASxxx = get_nb_xSxxx(l_ASpxx(iASpxx))

 END FUNCTION get_nb_Asxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_vertex_ASpxx(iaspxx)

   IMPLICIT NONE
   INTEGER :: iaspxx
   INTEGER :: get_nb_vertex_ASpxx
  
   get_nb_vertex_ASpxx = get_nb_vertex_xSpxx(l_ASpxx(iASpxx))

 END FUNCTION get_nb_vertex_ASpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_ENT_Aspxx(iaspxx)

   IMPLICIT NONE
   INTEGER :: iaspxx
   INTEGER :: get_ENT_Aspxx
  
   get_ENT_Aspxx = get_entity_mecaMAILx(l_Aspxx(iaspxx)%ibdyty)

 END FUNCTION get_ENT_Aspxx
!------------------------------------------------------------------------ 
 FUNCTION is_singleton_ASpxx(iASpxx)

   IMPLICIT NONE
   INTEGER :: iASpxx
   logical :: is_singleton_ASpxx
 
   is_singleton_ASpxx = is_singleton_xSpxx(l_ASpxx(iASpxx)) 


 END FUNCTION is_singleton_Aspxx
!------------------------------------------------------------------------ 
 logical function all_dof_driven_ASxxx(iASpxx, f_out)
   implicit none
   integer, intent(in) :: iASpxx, f_out
   !
   logical :: ddof
   integer :: ibdyty, i_vert, i_node, iASxxx

   ibdyty = l_ASpxx(iASpxx)%ibdyty
   iASxxx = l_ASpxx(iASpxx)%tface2face(f_out)

   all_dof_driven_ASxxx = .true.

   do i_vert = 1, l_ASpxx(iASpxx)%nb_vertex_bf(iASxxx)
     i_node = l_ASpxx(iASpxx)%face(i_vert,iASxxx)
     ddof   = is_node_dof_driven_mecaMAILx(ibdyty, i_node)
     all_dof_driven_ASxxx = all_dof_driven_ASxxx .and. ddof
   end do

 end function
 !------------------------------------------------------------------------ 
 subroutine set_precon_node_ASpxx
   implicit none

   integer          :: nb_MAILx
   integer          :: ibdyty,itacty,iASpxx,iASxxx,i
   character(len=5) :: tacID

   nb_MAILx=get_nb_MAILx()

   if (nb_MAILx == 0) return

   iASpxx = 0
   do ibdyty=1,nb_MAILx
     do itacty=1,get_nb_tacty_MAILx(ibdyty)
       tacID = get_tacID_MAILx(ibdyty,itacty)

       if (tacID == 'ASpxx') then
         iASpxx=iASpxx+1

         !print*,'bdy ',ibdyty,' tac ',itacty
         !print*,'asp ',iaspxx,' contient ',l_Aspxx(iaspxx)%nb_ASxxx

         do iASxxx=1,l_Aspxx(iASpxx)%nb_xSxxx
              
           !print*,'as ',iasxxx,' nb n by face ',l_Aspxx(iaspxx)%nb_vertex_bf(iasxxx)

           do i=1,l_ASpxx(iASpxx)%nb_vertex_bf(iASxxx)
        
             call set_precon_node_mecaMAILx(ibdyty,l_ASpxx(iASpxx)%face(i,iASxxx))  

           enddo

           l_ASpxx(iASpxx)%is_precon = .true.          

          enddo
       endif
     enddo
   enddo

 end subroutine

!------------------------------------------------------------------------ 
 FUNCTION get_HE_Hdl_Aspxx(iASpxx)

   IMPLICIT NONE
   INTEGER :: iASpxx
   type(T_HE_Hdl),pointer :: get_HE_Hdl_ASpxx
  
   get_HE_Hdl_Aspxx => get_HE_Hdl_xSpxx(l_ASpxx(iASpxx))

 END FUNCTION get_HE_Hdl_Aspxx
!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  CHARACTER(len=5) FUNCTION get_color_ASpxx(iASpxx)

    IMPLICIT NONE
    INTEGER :: iASpxx
   
    get_color_ASpxx = get_color_MAILx(ASpxx2bdyty(1,iASpxx),ASpxx2bdyty(2,iASpxx))
 
  END FUNCTION get_color_ASpxx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_visible_ASpxx(iASpxx)

    IMPLICIT NONE
    INTEGER :: iASpxx
   
    get_visible_ASpxx = .TRUE.
! a ajouter  get_visible_MAILx(CSxxx2bdyty(1,iASpxx))
 
  END FUNCTION get_visible_ASpxx
!!!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
  function get_dof_ASpxx(iASpx,iASxt)
    IMPLICIT NONE

    INTEGER :: iASpx,iASxt
    integer(kind=4),dimension(:),pointer :: get_dof_ASpxx

    integer(kind=4),DIMENSION(8) :: num,iccdof,nbdof
    INTEGER(kind=4)              :: ibdyty,iASpxx, in, i, isize, icc

    ibdyty= l_ASpxx(iASpx)%ibdyty

    iccdof=0
    nbdof=0

    call get_num_xSPxx(l_ASpxx(iASpx),iASxt,num,isize)

    do in=1,isize
        call get_dof_node_mecamailx(ibdyty,num(in),iccdof(in),nbdof(in))
    enddo
    allocate(get_dof_ASpxx(sum(nbdof)))
    icc=0
    do in=1,isize
      get_dof_ASpxx(icc+1:icc+nbdof(in)) = (/ ( iccdof(in)+i, i=1, nbdof(in) ) /)
      icc = icc + nbdof(in)
    enddo

  END function get_dof_ASpxx

  !> on recupere l interpolation sur le/les noeuds supports
  !> \todo faire un getPtr 
  function get_interp_ASpxx(iASpx,iASxt,tweight)
    INTEGER :: iASpx,iASxt
    REAL(kind=8),dimension(3)   :: tweight
    real(kind=8),dimension(:),pointer :: get_interp_ASpxx

    !***
    integer(kind=4) :: isize
    !integer(kind=4),DIMENSION(8) :: num
    real(kind=8),dimension(8)::interp 

    !call get_num_xSPxx(l_ASpxx(iASpx),iASxt,num,isize)

    call get_interp_xSPxx(l_ASpxx(iASpx),iASxt,tweight,interp,isize)

    allocate(get_interp_ASpxx(isize)) 
    get_interp_ASpxx(1:isize) = interp(1:isize)

  end function

  function get_connec_ASpxx()
    implicit none
    integer, dimension(:), pointer :: get_connec_ASpxx

    get_connec_ASpxx => null()
    if( nb_ASpxx == 0 ) return

    get_connec_ASpxx => get_connec_xSpxx(l_ASpxx)

  end function

  subroutine get_all_data_ASpxx(idata, rdata)
    implicit none
    integer     , dimension(:,:), pointer :: idata
    real(kind=8), dimension(:,:), pointer :: rdata

    idata => null()
    rdata => null()

    if( nb_ASpxx == 0 ) return

    call get_all_data_xSpxx(l_ASpxx, idata, rdata, .false.)

  end subroutine

  subroutine clean_memory_ASpxx
    implicit none
    integer(kind=4) :: i

    if( allocated(aspxx2bdyty) ) deallocate(aspxx2bdyty)

    nb_ASpxx = 0
    if( allocated(l_ASpxx) ) then
      do i = 1, size(l_ASpxx)
        call erase_xSpxx(l_ASpxx(i))
      end do
      deallocate(l_ASpxx)
    end if

  end subroutine

 END MODULE ASpxx

