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

!> module managing "candidate node on a line" contactor
 MODULE CLxxx                                       

 USE utilities

 USE overall
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx
  
 use algebra, only : length2

!$ use timer

 IMPLICIT NONE
 
 PRIVATE

 ! clxxx2bdyty(1,itac): 
 !   serial number of body MAILx (not mecaMAILx !!) to which is attached the contactor 
 !   CLxxx numbered itac in the list of all contactors CLxxx 
 ! clxxx2bdyty(2,itac): 
 !   serial number of contactor CLxxx itac in the list of contactors
 !   of any kind attached to a body  clxxx2bdyty(1,itac)
 ! clxxx2bdyty(3,itac):
 !   kind of model i_mailx, etc
 integer, dimension( : , : ), allocatable, target, public :: clxxx2bdyty  

 INTEGER :: nb_CLxxx

 TYPE,PUBLIC :: T_CLxxx
   ! serial number of body mecaMAILx
   INTEGER :: ibdyty
   ! serial number of nodes in the body ibdyty  
   integer :: numnoda,numnodb
   logical :: precon
   !fd support element, edge number, nearest gauss point
   integer :: iblmty, iedge, igp
 END TYPE T_CLxxx

 TYPE(T_CLxxx), DIMENSION(:), ALLOCATABLE :: l_CLxxx

 PUBLIC l_CLxxx

 INTEGER :: NbNodesByClxxx=2

 ! public routines
 PUBLIC read_bodies_CLxxx, &
        set_precon_node_CLxxx, &
        get_connec_CLpxx , &
        get_all_data_CLpxx

 PUBLIC get_nb_CLxxx, &
        nullify_reac_CLxxx,nullify_vlocy_CLxxx,comp_vlocy_CLxxx,&
        add_reac_CLxxx, &
        get_vlocy_CLxxx,get_coorTT_CLxxx, get_coor_support_CLxxx, &
        get_Vwear_CLxxx,put_Vwear_CLxxx,get_normalTT_CLxxx, &
        get_ENT_CLxxx, get_length_CLxxx, set_NbNodesByCLxxx, &
        set_data_CLxxx , &
        get_nodes_CLxxx, & ! rm
        get_apab_CLxxx, &
        get_visible_CLxxx, &
        get_bulk_strain_clxxx, &        
        get_bulk_stress_clxxx, &
        get_bulk_strain_triaxiality_clxxx, &
        get_bulk_stress_triaxiality_clxxx, &
        all_dof_driven_CLxxx

 public clean_memory_CLxxx

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_bodies_CLxxx

    IMPLICIT NONE

    INTEGER :: nb_MAILx
    INTEGER :: M_ibdyty,itacty,errare

    INTEGER, DIMENSION(2) :: CLidata
    CHARACTER(len=18)     :: IAM='CLxxx::read_bodies'
    CHARACTER(len=80) :: cout

    REAL(kind=8) :: rdata(1)
    real(kind=8), dimension(2) :: coor,coora,coorb     
    
    nb_MAILx=get_nb_MAILx()
    
    IF (nb_MAILx == 0) RETURN
    
    nb_CLxxx=0
    
    DO M_ibdyty=1,nb_MAILx   
       DO itacty=1,get_nb_tacty_MAILx(M_ibdyty)
          IF( get_tacID_MAILx(M_ibdyty,itacty) == 'CLxxx')  nb_CLxxx=nb_CLxxx+1
       END DO
    END DO
    
    WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_CLxxx,'CLxxx found'
    CALL LOGMES(cout)
    
    IF (nb_CLxxx == 0) RETURN
    
    allocate(clxxx2bdyty(3,nb_CLxxx))
    
    ALLOCATE(l_CLxxx(nb_CLxxx),stat=errare)
    
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating l_CLxxx')
    END IF
    
    if( .not. allocated(M2meca) ) then
      call faterr(IAM,'Please call LoadModels before LoadTactors')
    end if

    nb_CLxxx=0
    
    DO M_ibdyty=1,nb_MAILx
       DO itacty=1,get_nb_tacty_MAILx(M_ibdyty)
          IF (get_tacID_MAILx(M_ibdyty,itacty) == 'CLxxx') THEN
             nb_CLxxx=nb_CLxxx+1
             
             !clxxx2bdyty(1,itac) : serial number of body MAILx to which is attached the
             !                      contactor CLxxx numbered itac in the list of all
             !                      contactors CLxxx
             clxxx2bdyty(1,nb_CLxxx)=M_ibdyty
             
             !clxxx2bdyty(2,itac) : serial number of contactor CLxxx itac in the list of
             !                      contactors of any kind attached to body clxxx2bdyty(1,itac)
             clxxx2bdyty(2,nb_CLxxx)=itacty
             
             !clxxx2bdyty(3,itac) : type of body the contactor is attached to
             clxxx2bdyty(3,nb_CLxxx)=i_mailx 
             
             l_CLxxx(nb_CLxxx)%ibdyty = M2meca(M_ibdyty)%bdyty
             
             CALL get_idata_MAILx(M_ibdyty,itacty,CLidata)
             
             l_CLxxx(nb_CLxxx)%numnoda = CLidata(1)
             l_CLxxx(nb_CLxxx)%numnodb = CLidata(2)

             l_CLxxx(nb_CLxxx)%precon  = .false.

             call get_edge_MAILx(M_ibdyty, l_CLxxx(nb_CLxxx)%numnoda, l_CLxxx(nb_CLxxx)%numnodb, &
                                 l_CLxxx(nb_CLxxx)%iblmty, l_CLxxx(nb_CLxxx)%iedge)

             !print *,'CLxxx attached to avatar ',l_CLxxx(nb_CLxxx)%ibdyty, &
             !        ' element ',l_CLxxx(nb_CLxxx)%iblmty, &
             !        ' edge ',l_CLxxx(nb_CLxxx)%iedge  


             ! if edge really exists
             if (l_CLxxx(nb_CLxxx)%iedge /= 0) then  
             
               call get_rdata_MAILx(M_ibdyty,itacty,rdata)
               coora(1:2) = get_cooref_nodty_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty,l_CLxxx(nb_CLxxx)%numnoda)
               coorb(1:2) = get_cooref_nodty_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty,l_CLxxx(nb_CLxxx)%numnodb)
               coor = (1.d0 - rdata(1))*coora+rdata(1)*coorb

               call get_nearest_gp_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty,l_CLxxx(nb_CLxxx)%iblmty,coor,l_CLxxx(nb_CLxxx)%igp)
             else
               l_CLxxx(nb_CLxxx)%igp = 0
             endif  
             !print *,'nearest gauss point ',l_CLxxx(nb_CLxxx)%igp
             
          END IF
       END DO
    END DO
    
    !fd call get_edge(ibdyty, l_CLxxx(nb_CLxxx)%numnoda, l_CLxxx(nb_CLxxx)%numnodb)
    

  END SUBROUTINE read_bodies_CLxxx
!------------------------------------------------------------------------ 

  function get_connec_CLpxx()
    implicit none
    integer, dimension(:), pointer :: get_connec_CLpxx
    !
    integer :: nb_cl, i_clp, idx

    get_connec_CLpxx => null()
    if( nb_CLxxx == 0 ) return

    ! counting
    nb_cl = 3*nb_CLxxx+1
    allocate(get_connec_CLpxx(nb_cl))

    nb_cl =  0
    idx = 2
    do i_clp = 1, nb_CLxxx
      get_connec_CLpxx(idx)   = 2
      get_connec_CLpxx(idx+1) = l_CLxxx(i_clp)%numnoda
      get_connec_CLpxx(idx+2) = l_CLxxx(i_clp)%numnodb
      idx = idx + 3
      nb_cl = nb_cl + 1
    end do
    get_connec_CLpxx(1) = nb_cl

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

  subroutine get_all_data_CLpxx(idata, rdata)
    implicit none
    integer     , dimension(:,:), pointer :: idata
    real(kind=8), dimension(:,:), pointer :: rdata
    !
    integer :: nb_cl, i_clp, idx

    idata => null()
    rdata => null()

    if( nb_CLxxx == 0 ) return

    allocate(idata(4,nb_CLxxx))
    allocate(rdata(3,nb_CLxxx))
    idata = 0
    rdata = 0.d0

    idx = 1
    do i_clp = 1, nb_CLxxx
      idata(1,idx) = l_CLxxx(i_clp)%ibdyty
      idata(2,idx) = i_clp
      idata(3,idx) = 1
      idata(4,idx) = NbNodesByCLxxx

      rdata(1:2,idx) = get_normal_( l_CLxxx(i_clp)%ibdyty , &
                                    l_CLxxx(i_clp)%numnoda, &
                                    l_CLxxx(i_clp)%numnodb  &
                                  )
      idx = idx + 1
    end do

  end subroutine


!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES 
!
!------------------------------------------------------------------------ 
SUBROUTINE nullify_reac_CLxxx (iclxxx,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iclxxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_clxxx(iclxxx)%ibdyty

   CALL nullify_reac_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_reac_CLxxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE nullify_vlocy_CLxxx (iclxxx,storage)
  !
  ! called by SDL solver
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iclxxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_clxxx(iclxxx)%ibdyty

   CALL nullify_vlocy_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_vlocy_CLxxx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE comp_vlocy_CLxxx(iclxxx,storage,need_full_vlocy)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: iclxxx
   INTEGER            :: ibdyty
   INTEGER            :: storage  
   logical            :: need_full_vlocy

   ibdyty=l_clxxx(iclxxx)%ibdyty

   if (l_CLxxx(iclxxx)%precon  .and. .not. need_full_vlocy ) then
     CALL comp_vlocy_bynode_mecaMAILx(ibdyty,(/l_CLxxx(iclxxx)%numnoda,l_CLxxx(iclxxx)%numnodb/),storage)
   else
     CALL comp_vlocy_mecaMAILx(ibdyty,storage)
   end if

END SUBROUTINE comp_vlocy_CLxxx
!------------------------------------------------------------------------
SUBROUTINE add_reac_CLxxx(iclxxx,reac,storage)
   IMPLICIT NONE 
   INTEGER     ,INTENT(in) :: iclxxx

   REAL(kind=8),DIMENSION(2) :: reac,tmp
   INTEGER :: storage

   INTEGER :: ibdyty,numnoda,numnodb
   INTEGER ::M_ibdyty,M_itacty 

   REAL(kind=8) :: rdata(1),apab

   ibdyty =l_CLxxx(iclxxx)%ibdyty

   numnoda=l_CLxxx(iclxxx)%numnoda
   numnodb=l_CLxxx(iclxxx)%numnodb

   M_ibdyty = clxxx2bdyty(1,iclxxx)
   M_itacty = clxxx2bdyty(2,iclxxx)

   CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

   apab = rdata(1)
   tmp = (1.d0-apab)*reac
   CALL add_reac_nodty_mecaMAILx(ibdyty,numnoda,tmp,storage)
   tmp = apab*reac
   CALL add_reac_nodty_mecaMAILx(ibdyty,numnodb,tmp,storage)

END SUBROUTINE add_reac_CLxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_vlocy_CLxxx(iclxxx,storage,vlocy)
  IMPLICIT NONE
  INTEGER :: iclxxx
  INTEGER :: storage  

  INTEGER ::   ibdyty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab

  REAL(kind=8),DIMENSION(2) :: vlocy,vlocya,vlocyb

  M_ibdyty = clxxx2bdyty(1,iclxxx)
  M_itacty = clxxx2bdyty(2,iclxxx)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty=l_clxxx(iclxxx)%ibdyty
  numnoda=l_clxxx(iclxxx)%numnoda
  numnodb=l_clxxx(iclxxx)%numnodb

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
        call faterr('CLxxx::get_vlocy','unknown storage')
  END SELECT

END SUBROUTINE get_vlocy_CLxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
FUNCTION get_coorTT_CLxxx(iclxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(2) :: get_coorTT_CLxxx
  INTEGER :: iclxxx
  INTEGER :: ibdyty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab

  real(kind=8),dimension(3) :: vec_perio
 
  M_ibdyty = clxxx2bdyty(1,iclxxx)
  M_itacty = clxxx2bdyty(2,iclxxx)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty  = l_CLxxx(iclxxx)%ibdyty
  numnoda = l_CLxxx(iclxxx)%numnoda
  numnodb = l_CLxxx(iclxxx)%numnodb

  get_coorTT_CLxxx = (1.d0 - apab)*get_coorTT_nodty_mecaMAILx(ibdyty,numnoda) + &
                              apab*get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)

  !fd a voir
  call get_periodic_MAILx(M_ibdyty,M_itacty,vec_perio)
  get_coorTT_CLxxx(1)=get_coorTT_CLxxx(1)+vec_perio(1)
  get_coorTT_CLxxx(2)=get_coorTT_CLxxx(2)+vec_perio(2)

END FUNCTION get_coorTT_CLxxx
!------------------------------------------------------------------------ 
subroutine get_coor_support_CLxxx(iclxxx,coor_support)
  implicit none
  integer(kind=4), intent(in) :: iclxxx
  real(kind=8), dimension(2,2), intent(out) :: coor_support
  !
  integer(kind=4) :: ibdyty, numnod

  ibdyty  = l_CLxxx(iclxxx)%ibdyty

  numnod = l_CLxxx(iclxxx)%numnoda
  coor_support(1:2,1) = get_coorTT_nodty_mecaMAILx(ibdyty,numnod)

  numnod = l_CLxxx(iclxxx)%numnodb
  coor_support(1:2,2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnod)

end subroutine get_coor_support_CLxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 REAL(kind=8) FUNCTION get_length_CLxxx(iclxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(2) :: coorA,coorB
  INTEGER ::   iclxxx
  INTEGER ::   ibdyty,  numnoda,  numnodb
  INTEGER ::   M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab,r

  ibdyty  = l_CLxxx(iclxxx)%ibdyty
  numnoda = l_CLxxx(iclxxx)%numnoda
  numnodb = l_CLxxx(iclxxx)%numnodb

  coorA(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnoda)
  coorB(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)

  get_length_CLxxx = DSQRT((coorB(1)-coorA(1))**2+(coorB(2)-coorA(2))**2)/REAL(NbNodesByClxxx)

  IF (dime_mod /= i_2D_axisym) RETURN

  M_ibdyty = clxxx2bdyty(1,iclxxx)
  M_itacty = clxxx2bdyty(2,iclxxx)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  r = (1.d0 - apab)*coorA(1) + apab*coorB(1)

  get_length_CLxxx = get_length_CLxxx * 6.2831853 * r


END FUNCTION get_length_CLxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
FUNCTION get_normalTT_CLxxx(iclxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(2) :: get_normalTT_CLxxx,tmp
  INTEGER :: iclxxx
  INTEGER ::   ibdyty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: norm

  ibdyty  = l_CLxxx(iclxxx)%ibdyty
  numnoda = l_CLxxx(iclxxx)%numnoda
  numnodb = l_CLxxx(iclxxx)%numnodb

  tmp = get_coorTT_nodty_mecaMAILx(ibdyty,numnodb) - &
        get_coorTT_nodty_mecaMAILx(ibdyty,numnoda)

  norm = dsqrt(tmp(1)**2 + tmp(2)**2)

  tmp = tmp /norm

  get_normalTT_CLxxx(1) = -tmp(2)
  get_normalTT_CLxxx(2) =  tmp(1)

END FUNCTION get_normalTT_CLxxx
!------------------------------------------------------------------------ 


!------------------------------------------------------------------------ 
 FUNCTION get_nb_CLxxx(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_CLxxx
  
   get_nb_CLxxx = nb_CLxxx
  
 END FUNCTION get_nb_CLxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_Vwear_CLxxx(iclxxx,vlocy)
  IMPLICIT NONE
  INTEGER :: iclxxx
  INTEGER :: storage  

  INTEGER ::   ibdyty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab

  REAL(kind=8),DIMENSION(2) :: vlocy,vlocya,vlocyb

  M_ibdyty = clxxx2bdyty(1,iclxxx)
  M_itacty = clxxx2bdyty(2,iclxxx)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty =l_clxxx(iclxxx)%ibdyty
  numnoda=l_clxxx(iclxxx)%numnoda
  numnodb=l_clxxx(iclxxx)%numnodb


  vlocya=get_Vwear_nodty_mecaMAILx(ibdyty,numnoda)
  vlocyb=get_Vwear_nodty_mecaMAILx(ibdyty,numnodb)

  vlocy = (1.d0 -apab)*vlocya + apab*vlocyb

END SUBROUTINE get_Vwear_CLxxx
!------------------------------------------------------------------------ 
SUBROUTINE put_Vwear_CLxxx(iclxxx,cdvwear)
   IMPLICIT NONE 
   INTEGER     ,INTENT(in) :: iclxxx

   REAL(kind=8),DIMENSION(2) :: cdvwear

   INTEGER :: ibdyty,numnoda,numnodb
   INTEGER ::M_ibdyty,M_itacty 

   REAL(kind=8) :: rdata(1),apab

   ibdyty =l_CLxxx(iclxxx)%ibdyty

   numnoda=l_CLxxx(iclxxx)%numnoda
   numnodb=l_CLxxx(iclxxx)%numnodb

   M_ibdyty = clxxx2bdyty(1,iclxxx)
   M_itacty = clxxx2bdyty(2,iclxxx)

   CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

   apab = rdata(1)
   
   CALL put_Vwear_nodty_mecaMAILx(ibdyty,numnoda,((1.d0-apab)*cdvwear))

   CALL put_Vwear_nodty_mecaMAILx(ibdyty,numnodb,apab*cdvwear)

END SUBROUTINE put_Vwear_CLxxx

!------------------------------------------------------------------------ 
 FUNCTION get_ENT_CLxxx(iclxxx)

   IMPLICIT NONE
   INTEGER :: iclxxx
   INTEGER :: get_ENT_CLxxx
  
   get_ENT_CLxxx = get_entity_mecaMAILx(l_CLxxx(iclxxx)%ibdyty)

 END FUNCTION get_ENT_CLxxx
!------------------------------------------------------------------------

!!!----------------------------------------------------------------------

 logical function all_dof_driven_CLxxx(iCLxxx)
   implicit none
   integer, intent(in) :: iCLxxx
   !
   integer :: ibdyty, inode, ielem

   ibdyty = l_CLxxx(iCLxxx)%ibdyty

   if (get_apab_CLxxx(iCLxxx) /= 1.d0) then 
     inode = l_CLxxx(iCLxxx)%numnoda
     all_dof_driven_CLxxx = is_node_dof_driven_mecaMAILx(ibdyty, inode)
     !  print * ,iCLxxx, inode, all_dof_driven_CLxxx
     if (.not. all_dof_driven_CLxxx) return
   end if

   if (get_apab_CLxxx(iCLxxx) /= 0.d0) then 
     inode = l_CLxxx(iCLxxx)%numnodb
     all_dof_driven_CLxxx = is_node_dof_driven_mecaMAILx(ibdyty, inode)
     !  print * ,iCLxxx, inode, all_dof_driven_CLxxx
   end if


 end function all_dof_driven_CLxxx

!!!---------------------------------------------------------------------

 subroutine set_precon_node_CLxxx
   implicit none 
   integer :: nb_MAILx
   integer :: ibdyty,itacty,iclxxx

   nb_MAILx=get_nb_MAILx()

   if (nb_MAILx == 0) return

   iclxxx=0
   do ibdyty=1,nb_MAILx
     do itacty=1,get_nb_tacty_MAILx(ibdyty)
       if (get_tacID_MAILx(ibdyty,itacty) == 'CLxxx') then
          iCLxxx=iCLxxx+1

          call set_precon_node_mecaMAILx(ibdyty,l_CLxxx(iCLxxx)%numnoda)
          call set_precon_node_mecaMAILx(ibdyty,l_CLxxx(iCLxxx)%numnodb)

          l_CLxxx(iclxxx)%precon = .true.          

       end if
     end do 
   end do


 end subroutine set_precon_node_CLxxx
!!!------------------------------------------------------------------------ 
SUBROUTINE set_NbNodesByCLxxx(ivalue)
  IMPLICIT NONE
  INTEGER :: ivalue  
  
  NbNodesByCLxxx = ivalue

END SUBROUTINE set_NbNodesByCLxxx

 !> \brief use anonymous data to initialize a contactor
 subroutine set_data_CLxxx(idata, rdata, node_list, nb_support, support, Brd)
   implicit none
   !> [in] idata: anonymous integer data (nb_cl, [id_a,id_b]*nb_cl)
   integer(kind=4), dimension(:), pointer :: idata
   !> [in] rdata: anonymous real data ([weight]*nb_cl)
   real(kind=8),    dimension(:), pointer :: rdata
   !> [out] node_list: list of nodes of the model_handle to use
   integer(kind=4), dimension(:),   allocatable, intent(inout) :: node_list
   !> [out] nb_support: number of support nodes to the contactor
   integer(kind=4),                              intent(out)   :: nb_support
   !> [out] support: support nodes to the contactor
   real(kind=8),    dimension(:,:), allocatable, intent(inout) :: support
   !> [out] Brd: boundary radius
   real(kind=8),    intent(out) :: Brd
 
   if( allocated(node_list) ) deallocate(node_list)
   if( allocated(support)   ) deallocate(support)
 
   nb_support  = 2*idata(1)
   allocate(node_list(nb_support))
   node_list(1:nb_support) = idata(2:nb_support+1)

   if( size(idata) /= nb_support+1 ) then
     call faterr('CLxxx::set_data_CLxxx','inconsistent idata size')
   end if
 
   allocate( support(nbDIME,nb_support) )

   support = 0.d0
   
   Brd = 0.1 !kind of halo ?
 
 end subroutine

 function get_nodes_CLxxx(iclxxx)
   implicit none
   integer(kind=4), intent(in)   :: iclxxx
   integer(kind=4), dimension(2) :: get_nodes_CLxxx

   get_nodes_CLxxx(1) = l_CLxxx(iclxxx)%numnoda
   get_nodes_CLxxx(2) = l_CLxxx(iclxxx)%numnodb
 end function

 function get_apab_CLxxx(iclxxx)
   integer(kind=4), intent(in) :: iclxxx
   real(kind=8) :: get_apab_CLxxx
   !
   real(kind=8) :: rdata(1)

   call get_rdata_MAILx(clxxx2bdyty(1,iclxxx),clxxx2bdyty(2,iclxxx),rdata)

   get_apab_CLxxx = rdata(1)

 end function

 function get_visible_CLxxx(iclxxx)

   implicit none

   integer(kind=4) :: iclxxx
   logical :: get_visible_CLxxx
   
   get_visible_CLxxx = get_visible_mecaMAILx(l_CLxxx(iclxxx)%ibdyty)
 
 end function get_visible_CLxxx

 subroutine get_bulk_strain_clxxx(nb_clxxx,vec)
   implicit none

   integer(kind=4) :: nb_clxxx
   real(kind=8)    :: vec(:)

   REAL(kind=8),dimension(:),pointer  :: field
   
   if( l_CLxxx(nb_CLxxx)%igp < 1 ) call faterr('CLxxx::get_bulk_strain', 'no nearest Gauss point found, the contact law is not usable in this configuration')

   field => get_gp_strain_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty, l_CLxxx(nb_CLxxx)%iblmty, l_CLxxx(nb_CLxxx)%igp)

   vec=field

 end subroutine get_bulk_strain_clxxx
 
 subroutine get_bulk_stress_clxxx(nb_clxxx,vec)
   implicit none

   integer(kind=4) :: nb_clxxx
   real(kind=8)    :: vec(:)

   REAL(kind=8),dimension(:),pointer  :: field

   field => get_gp_stress_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty, l_CLxxx(nb_CLxxx)%iblmty, l_CLxxx(nb_CLxxx)%igp)

   vec=field

 end subroutine get_bulk_stress_clxxx

 subroutine get_bulk_strain_triaxiality_clxxx(nb_clxxx,value)
   implicit none

   integer(kind=4) :: nb_clxxx
   real(kind=8)    :: value

    if( l_CLxxx(nb_CLxxx)%igp < 1 ) call faterr('CLxxx::get_bulk_strain_triaxiality', 'no nearest Gauss point found, the contact law is not usable in this configuration')

    call get_gp_strain_triaxiality_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty,l_CLxxx(nb_CLxxx)%iblmty,l_CLxxx(nb_CLxxx)%igp,value)

 end subroutine get_bulk_strain_triaxiality_clxxx
 
 
 subroutine get_bulk_stress_triaxiality_clxxx(nb_clxxx,value)
   implicit none

   integer(kind=4) :: nb_clxxx
   real(kind=8)    :: value

    if( l_CLxxx(nb_CLxxx)%igp < 1 ) call faterr('CLxxx::get_bulk_stress_triaxiality', 'no nearest Gauss point found, the contact law is not usable in this configuration')

    call get_gp_stress_triaxiality_mecaMAILx(l_CLxxx(nb_CLxxx)%ibdyty,l_CLxxx(nb_CLxxx)%iblmty,l_CLxxx(nb_CLxxx)%igp,value)

 end subroutine get_bulk_stress_triaxiality_clxxx
 


 subroutine clean_memory_clxxx()
   implicit none

   nb_CLxxx = 0

   if( allocated(clxxx2bdyty) ) deallocate(clxxx2bdyty)

   if( allocated(l_CLxxx) ) then
     deallocate(l_CLxxx)
   end if

 end subroutine

END MODULE CLxxx
