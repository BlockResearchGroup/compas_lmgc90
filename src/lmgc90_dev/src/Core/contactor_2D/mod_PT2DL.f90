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
 MODULE PT2DL                                       

  !!****h* LMGC90.CORE/PT2DL
  !! NAME
  !!  module PT2DL
  !! PURPOSE
  !!  2D point on a MAILx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/BULK_BEHAVIOUR
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/a_DOF
  !!  LMGC90.CORE/MAILx
  !!  LMGC90.CORE/mecaMAILx
  !!  LMGC90.CORE/therMAILx
  !!****

 USE utilities
 USE overall
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx

 ! > B.o.B.o.R >
 USE therMAILx
 ! < B.o.B.o.R <
  
!$ use timer
 IMPLICIT NONE
 
 PRIVATE

 integer, dimension( : , : ), allocatable, &
      target, public :: pt2dl2bdyty  ! pt2dl2bdyty(1,itac): 
                                     ! serial number of body MAILx to which is attached the contactor 
                                     ! PT2DL numbered itac in the list of all contactors PT2DL 
                                     ! pt2dl2bdyty(2,itac): 
                                     ! serial number of contactor PT2DL itac in the list of contactors 
 INTEGER :: nb_PT2DL

 TYPE,PUBLIC :: T_PT2DL
   INTEGER :: ibdyty,numnoda,numnodb
   logical :: precon
 END TYPE T_PT2DL

 TYPE(T_PT2DL), DIMENSION(:), ALLOCATABLE,PUBLIC :: l_PT2DL

 !!! > B.o.B.o.R >
 !!! gestion des conditions de convection, radiation, ....

 INTEGER,DIMENSION(:,:),ALLOCATABLE,PUBLIC  ::  pt2tl2bdyty   ! pt2tl2bdyty(1,itac): 
                                                              ! serial number of body MAILx to which is attached the contactor 
                                                              ! PT2DL numbered itac in the list of all contactors PT2DL 
                                                              ! pt2dl2bdyty(2,itac): 
                                                              ! serial number of contactor PT2DL itac in the list of contactors 
                                                              ! of any kind attached to a body pt2dl2bdyty(1,itac)
                                                              ! of any kind attached to a body pt2dl2bdyty(1,itac)
 INTEGER :: nb_pt2tl

 TYPE,PUBLIC :: T_pt2tl
   INTEGER                              :: ibdyty,numnoda,numnodb
   REAL(kind=8)                         :: hconv
   REAL(kind=8)                         :: Tconv
   REAL(kind=8),DIMENSION(2,2)          :: convection_matrix 
   REAL(kind=8),DIMENSION(2)            :: convection_rhs,convection_rhs_old
   REAL(kind=8)                         :: coeffconvection = 200.D+0
   REAL(kind=8)                         :: tempconvection = 20.D+0
   REAL(kind=8)                         :: epaisseur = 1.
 END TYPE T_pt2tl

 TYPE(T_pt2tl), DIMENSION(:), ALLOCATABLE, PUBLIC :: l_pt2tl

!!! < B.o.B.o.R <



! public routines
 PUBLIC read_bodies_PT2DL, &
        set_precon_node_PT2DL, &
        compute_convection_matrix_PT2DL, &
        compute_convection_RHS_PT2DL, &
        set_hconv_PT2TL, &
        set_temp_PT2TL, &
        get_nb_PT2TL, & 
        get_body_PT2TL, &
        add_convection2KT_PT2DL, &
        add_convection2RHS_PT2DL

 PUBLIC get_nb_PT2DL, &
        nullify_reac_PT2DL,nullify_vlocy_PT2DL,comp_vlocy_PT2DL,&
        add_reac_PT2DL, &
        get_vlocy_PT2DL,get_coorTT_PT2DL,get_coorefTT_PT2DL, &
        get_ENT_PT2DL, &
        set_Tconv_PT2TL 

 public clean_memory_PT2DL

CONTAINS

!!!--------------------------------------------------------------------------
  SUBROUTINE read_bodies_PT2DL

    IMPLICIT NONE

    INTEGER :: nb_MAILx
    INTEGER :: ibdyty,itacty,errare
    INTEGER, DIMENSION(2) :: PTidata
    CHARACTER(len=18)     :: IAM='PT2DL::read_bodies'
    CHARACTER(len=172) :: cout

    nb_MAILx=get_nb_MAILx()

    IF (nb_MAILx == 0) RETURN
    
    nb_PT2DL=0
!!! > B.o.B.o.R >
    nb_pt2tl = 0
!!! < B.o.B.o.R <

    DO ibdyty=1,nb_MAILx   
       DO itacty=1,get_nb_tacty_MAILx(ibdyty)
          IF( get_tacID_MAILx(ibdyty,itacty) == 'PT2DL')  nb_PT2DL=nb_PT2DL+1
!!! > B.o.B.o.R >
          IF( get_tacID_MAILx(ibdyty,itacty) == 'PT2TL')  nb_pt2tl=nb_pt2tl+1
!!! < B.o.B.o.R <
          
       END DO
    END DO
    
    cout=' '
    WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_PT2DL,'PT2DL found'
    CALL LOGMES(trim(cout))
    
!!! > B.o.B.o.R >                         !12345678901234567890123
    cout=' '
    WRITE(cout,'(1X,I5,1X,A23)') nb_pt2tl,'thermal contactor found'
    CALL LOGMES(trim(cout))
!!! < B.o.B.o.R <
    
    IF (nb_PT2DL == 0 .AND. nb_pt2tl == 0 ) RETURN
    
    IF (nb_PT2DL /= 0) THEN
       
       allocate(pt2dl2bdyty(3,nb_PT2DL),stat=errare)
       
       IF (errare /= 0) THEN
          CALL FATERR(IAM,'error allocating pt2dl2bdyty')
       END IF
       
       ALLOCATE(l_PT2DL(nb_PT2DL),stat=errare)
       
       IF (errare /= 0) THEN
          CALL FATERR(IAM,'error allocating l_PT2DL')
       END IF
       
       nb_PT2DL=0
       
       if( .not. allocated(M2meca) ) then
         call faterr(IAM,'Please call LoadModels before LoadTactors')
       end if

       DO ibdyty=1,nb_MAILx
          DO itacty=1,get_nb_tacty_MAILx(ibdyty)
             IF (get_tacID_MAILx(ibdyty,itacty) == 'PT2DL') THEN
                nb_PT2DL=nb_PT2DL+1
                pt2dl2bdyty(1,nb_PT2DL)=ibdyty  !   pt2dl2bdyty(1,itac) : serial number of body MAILx to which is attached the 
                !                         contactor PT2DL numbered itac in the list of all 
                !                         contactors PT2DL 
                pt2dl2bdyty(2,nb_PT2DL)=itacty  !   pt2dl2bdyty(2,itac) : serial number of contactor PT2DL itac in the list of 
                !                         contactors of any kind attached to body pt2dl2bdyty(1,itac)
                pt2dl2bdyty(3,nb_PT2DL)=i_mailx !   pt2dl2bdyty(3,itac): type of body the contactor is attached to
                
                l_PT2DL(nb_PT2DL)%ibdyty = M2meca(ibdyty)%bdyty
                CALL get_idata_MAILx(ibdyty,itacty,PTidata)
                
                l_PT2DL(nb_PT2DL)%numnoda = PTidata(1)
                l_PT2DL(nb_PT2DL)%numnodb = PTidata(2)

                l_pt2dl(nb_pt2dl)%precon  = .false.
                
             END IF
          END DO
       END DO
    ENDIF

!!! < B.o.B.o.R <
    IF (nb_pt2tl /= 0) THEN
   
       ALLOCATE(pt2tl2bdyty(2,nb_pt2tl),stat=errare)

       IF (errare /= 0) THEN
          CALL FATERR(IAM,'error allocating pt2tl2bdyty')
       END IF
       
       ALLOCATE(l_pt2tl(nb_PT2TL),stat=errare)
       
       IF (errare /= 0) THEN
          CALL FATERR(IAM,'error allocating l_pt2tl')
       END IF
       
       nb_PT2TL=0
       
       DO ibdyty=1,nb_MAILx
          DO itacty=1,get_nb_tacty_MAILx(ibdyty)
             IF (get_tacID_MAILx(ibdyty,itacty) == 'PT2TL') THEN
                
                nb_pt2tl=nb_pt2tl+1

                pt2tl2bdyty(1,nb_pt2tl)=ibdyty    !   pt2dl2bdyty(1,itac) : serial number of body MAILx to which is attached the 
                !                         contactor PT2DL numbered itac in the list of all 
                !                         contactors PT2DL 

                pt2tl2bdyty(2,nb_pt2tl)=itacty    !   pt2dl2bdyty(2,itac) : serial number of contactor PT2DL itac in the list of 
                !                         contactors of any kind attached to body pt2dl2bdyty(1,itac)
 
                l_pt2tl(nb_pt2tl)%ibdyty = M2therm(ibdyty)%bdyty
 
                CALL get_idata_MAILx(ibdyty,itacty,PTidata)

                l_pt2tl(nb_pt2tl)%numnoda = PTidata(1)
                l_pt2tl(nb_pt2tl)%numnodb = PTidata(2)


             END IF
          END DO
       END DO
    ENDIF
  
!!! < B.o.B.o.R <
    
  END SUBROUTINE read_bodies_PT2DL
!!!------------------------------------------------------------------------ 
!!! PUBLIC ROUTINES 
!!!------------------------------------------------------------------------ 
SUBROUTINE nullify_reac_PT2DL (ipt2dl,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: ipt2dl
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_pt2dl(ipt2dl)%ibdyty

   CALL nullify_reac_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_reac_PT2DL
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE nullify_vlocy_PT2DL (ipt2dl,storage)
  !
  ! called by SDL solver
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: ipt2dl
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_pt2dl(ipt2dl)%ibdyty

   CALL nullify_vlocy_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_vlocy_PT2DL
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE comp_vlocy_PT2DL(ipt2dl,storage,need_full_vlocy)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: ipt2dl
   INTEGER            :: ibdyty
   INTEGER            :: storage  
   logical            :: need_full_vlocy

   ibdyty=l_pt2dl(ipt2dl)%ibdyty

   if (l_PT2DL(ipt2dl)%precon .and. .not. need_full_vlocy ) then
     CALL comp_vlocy_bynode_mecaMAILx(ibdyty,(/l_PT2DL(ipt2dl)%numnoda,l_PT2DL(ipt2dl)%numnodb/),storage)
   else
     CALL comp_vlocy_mecaMAILx(ibdyty,storage)
   endif

END SUBROUTINE comp_vlocy_PT2DL
!------------------------------------------------------------------------
SUBROUTINE add_reac_PT2DL(ipt2dl,reac,storage)
   IMPLICIT NONE 
   INTEGER     ,INTENT(in) :: ipt2dl

   REAL(kind=8),DIMENSION(2) :: reac
   INTEGER :: storage

   INTEGER :: ibdyty,numnoda,numnodb
   INTEGER ::M_ibdyty,M_itacty 

   REAL(kind=8) :: rdata(1),apab

   ibdyty =l_PT2DL(ipt2dl)%ibdyty

   numnoda=l_PT2DL(ipt2dl)%numnoda
   numnodb=l_PT2DL(ipt2dl)%numnodb

   M_ibdyty = pt2dl2bdyty(1,ipt2dl)
   M_itacty = pt2dl2bdyty(2,ipt2dl)

   CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

   apab = rdata(1)
   
   CALL add_reac_nodty_mecaMAILx(ibdyty,numnoda,((1.d0-apab)*reac),storage)

   CALL add_reac_nodty_mecaMAILx(ibdyty,numnodb,(apab*reac),storage)

END SUBROUTINE add_reac_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_vlocy_PT2DL(ipt2dl,storage,vlocy)
  IMPLICIT NONE
  INTEGER :: ipt2dl
  INTEGER :: storage  

  INTEGER ::   ibdyty,  itacty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab

  REAL(kind=8),DIMENSION(2) :: vlocy,vlocya,vlocyb

  M_ibdyty = pt2dl2bdyty(1,ipt2dl)
  M_itacty = pt2dl2bdyty(2,ipt2dl)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty=l_pt2dl(ipt2dl)%ibdyty
  numnoda=l_pt2dl(ipt2dl)%numnoda
  numnodb=l_pt2dl(ipt2dl)%numnodb

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

    CASE default
        call faterr('PT2DL::get_vlocy','unknown storage')
  END SELECT

END SUBROUTINE get_vlocy_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
FUNCTION get_coorTT_PT2DL(ipt2dl)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(2) :: get_coorTT_PT2DL
  INTEGER :: ipt2dl
  INTEGER ::   ibdyty,  itacty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab

  M_ibdyty = pt2dl2bdyty(1,ipt2dl)
  M_itacty = pt2dl2bdyty(2,ipt2dl)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty  = l_PT2DL(ipt2dl)%ibdyty
  numnoda = l_PT2DL(ipt2dl)%numnoda
  numnodb = l_PT2DL(ipt2dl)%numnodb

  get_coorTT_PT2DL = (1.d0 - apab)*get_coorTT_nodty_mecaMAILx(ibdyty,numnoda) + &
                              apab*get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)

END FUNCTION get_coorTT_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
FUNCTION get_coorefTT_PT2DL(ipt2dl)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(2) :: get_coorefTT_PT2DL
  INTEGER :: ipt2dl
  INTEGER ::   ibdyty,  itacty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab

  M_ibdyty = pt2dl2bdyty(1,ipt2dl)
  M_itacty = pt2dl2bdyty(2,ipt2dl)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty  = l_PT2DL(ipt2dl)%ibdyty
  numnoda = l_PT2DL(ipt2dl)%numnoda
  numnodb = l_PT2DL(ipt2dl)%numnodb

  get_coorefTT_PT2DL = (1.d0 - apab)*get_coorTT_nodty_mecaMAILx(ibdyty,numnoda) + &
                              apab*get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)

END FUNCTION get_coorefTT_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_PT2DL(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_PT2DL
  
   get_nb_PT2DL = nb_PT2DL
  
 END FUNCTION get_nb_PT2DL
!! > B.o.B.o.R >
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_PT2TL(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_PT2TL
  
   get_nb_PT2TL = nb_PT2TL
  
 END FUNCTION get_nb_PT2TL

!! < B.o.B.o.R <
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_ENT_PT2DL(ipt2dl)

   IMPLICIT NONE
   INTEGER :: ipt2dl
   INTEGER :: get_ENT_PT2DL
  
   get_ENT_PT2DL = get_entity_mecaMAILx(l_PT2DL(ipt2dl)%ibdyty)

 END FUNCTION get_ENT_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine set_precon_node_PT2DL
   implicit none 
   integer :: nb_MAILx
   integer :: ibdyty,itacty,ipt2dl

   nb_MAILx=get_nb_MAILx()

   if (nb_MAILx == 0) return

   ipt2dl=0
   do ibdyty=1,nb_MAILx
     do itacty=1,get_nb_tacty_MAILx(ibdyty)
       if (get_tacID_MAILx(ibdyty,itacty) == 'PT2DL') then
          ipt2dl=ipt2dl+1

          !fd on pourrait faire plus simple ... en utilisant le ibdyty stocke dans l_pt2dl

          call set_precon_node_mecaMAILx(ibdyty,l_PT2DL(ipt2dl)%numnoda)
          call set_precon_node_mecaMAILx(ibdyty,l_PT2DL(ipt2dl)%numnodb)

          l_pt2dl(nb_pt2dl)%precon  = .true.

       end if
     end do 
   end do


 end subroutine set_precon_node_PT2DL
!!!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 REAL(kind=8) FUNCTION get_length_PT2DL(iclxxx)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(2) :: coorA,coorB
  INTEGER ::   iclxxx
  INTEGER ::   ibdyty,  numnoda,  numnodb

  REAL(kind=8),DIMENSION(1) :: rdata

  REAL(kind=8) :: apab,r

  ibdyty  = l_pt2tl(iclxxx)%ibdyty
  numnoda = l_pt2tl(iclxxx)%numnoda
  numnodb = l_pt2tl(iclxxx)%numnodb

  coorA(1:2) = get_coor_nodty_MAILx(ibdyty,numnoda)
  coorB(1:2) = get_coor_nodty_MAILx(ibdyty,numnodb)

  get_length_PT2DL= DSQRT((coorB(1)-coorA(1))**2+(coorB(2)-coorA(2))**2)
END FUNCTION get_length_PT2DL
!------------------------------------------------------------------------ 
!!! > B.o.B.o.R > 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
! FUNCTION get_nb_PT2TL(fantome)

!   IMPLICIT NONE
!   INTEGER,OPTIONAL :: fantome
!   INTEGER :: get_nb_PT2TL
  
!   get_nb_PT2TL = nb_PT2TL
  
! END FUNCTION get_nb_PT2TL
!------------------------------------------------------------------------ 
subroutine set_hconv_pt2tl(itact,hconv)
  implicit none
  integer :: itact 
  real(kind=8) :: hconv

  l_pt2tl(itact)%coeffconvection = hconv

end subroutine
!------------------------------------------------------------------------ 
subroutine set_Tconv_pt2tl(itact,Tconv)
  implicit none
  integer :: itact 
  real(kind=8) :: Tconv

  l_pt2tl(itact)%tempconvection = Tconv

end subroutine
!------------------------------------------------------------------------ 
SUBROUTINE compute_convection_matrix_PT2DL
  IMPLICIT NONE
  INTEGER   :: iPT2D
  REAL(kind=8)               :: CL
  REAL(kind=8)               :: matrice_base(2,2)=RESHAPE((/2.,1.,1.,2./),SHAPE=(/2,2/))

  DO iPT2D = 1,nb_pt2tl
     
     CL             = get_length_PT2DL(iPT2D)
     l_pt2tl(iPT2D)%convection_matrix=(l_pt2tl(iPT2D)%coeffconvection*l_pt2tl(iPT2D)%epaisseur*CL/6.)*matrice_base
  ENDDO

END SUBROUTINE compute_convection_matrix_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE compute_convection_RHS_PT2DL
  IMPLICIT NONE
  INTEGER      :: iPT2D
  REAL(kind=8) :: CL,TF
  REAL(kind=8) :: vecteur_base(2)  = (/1.,1./)  
 
  DO iPT2D = 1,nb_pt2tl
     CL              = get_length_PT2DL(iPT2D)
     l_pt2tl(iPT2D)%convection_rhs_old  =  l_pt2tl(iPT2D)%convection_rhs
     l_pt2tl(iPT2D)%convection_rhs      =  l_pt2tl(iPT2D)%coeffconvection*l_pt2tl(iPT2D)%epaisseur*CL/2* &
                                            l_pt2tl(iPT2D)%tempconvection*vecteur_base
  ENDDO
END SUBROUTINE compute_convection_RHS_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE add_convection2KT_PT2DL
  IMPLICIT NONE
  INTEGER   :: iPT2D,ibdyty
  INTEGER, DIMENSION(2) :: l_dof
 
  DO iPT2D = 1,nb_pt2tl

     ibdyty=pt2tl2bdyty(1,iPT2D)

     l_dof(1)=l_PT2TL(iPT2D)%numnoda
     l_dof(2)=l_PT2TL(iPT2D)%numnodb

     CALL add_convection2KT_therMAILx(ibdyty,l_dof,l_PT2TL(iPT2D)%convection_matrix)

  ENDDO

END SUBROUTINE add_convection2KT_PT2DL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE add_convection2RHS_PT2DL
  IMPLICIT NONE
  INTEGER      :: iPT2D,ibdyty
  INTEGER, DIMENSION(2) :: l_dof

  DO iPT2D = 1,nb_pt2tl

     ibdyty=pt2tl2bdyty(1,iPT2D)

     l_dof(1)=l_PT2TL(iPT2D)%numnoda
     l_dof(2)=l_PT2TL(iPT2D)%numnodb

     CALL add_convection2RHS_therMAILx(ibdyty,l_dof,l_PT2TL(iPT2D)%convection_matrix,l_PT2TL(iPT2D)%convection_rhs)

  ENDDO

END SUBROUTINE add_convection2RHS_PT2DL
!SUBROUTINE set_hconv_PT2TL(iPT2D,hconv)
! IMPLICIT NONE
! INTEGER :: iPT2D
! REAL(KIND=8) :: hconv
! l_PT2TL(iPT2D)%hconv = hconv
!END SUBROUTINE set_hconv_PT2TL
SUBROUTINE set_temp_PT2TL(iPT2D,temp)
 IMPLICIT NONE
 INTEGER :: iPT2D
 REAL(KIND=8) :: temp
 l_PT2TL(iPT2D)%Tconv = temp
END SUBROUTINE set_temp_PT2TL
FUNCTION get_body_PT2TL(itacty)
implicit none
integer :: itacty
integer :: get_body_PT2TL
get_body_PT2TL =  pt2dl2bdyty(1,itacty)
END FUNCTION get_body_PT2TL
!------------------------------------------------------------------------ 
!!! < B.o.B.o.R <
!------------------------------------------------------------------------ 

  subroutine clean_memory_PT2DL()
    implicit none
    integer(kind=4) :: i
  
    nb_pt2dl = 0
  
    if( allocated(pt2dl2bdyty) ) deallocate(pt2dl2bdyty)
  
    if( allocated(l_PT2DL) ) then
      deallocate(l_PT2DL)
    end if
  
    nb_pt2tl = 0
  
    if( allocated(pt2tl2bdyty) ) deallocate(pt2tl2bdyty)
  
    if( allocated(l_PT2TL) ) then
      deallocate(l_PT2TL)
    end if
  
  end subroutine

END MODULE PT2DL

