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

!> manage "antagoniste disk on a line" contactor
 MODULE DISKL                                       

 !!  2D antagoniste disque on a MAILx

 USE utilities
 USE overall
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx

 IMPLICIT NONE
 
 PRIVATE

 INTEGER :: nb_diskL

 TYPE,PUBLIC :: T_DISKL
   INTEGER :: ibdyty,numnoda,numnodb
   logical :: precon
 END TYPE T_DISKL

 TYPE(T_DISKL), DIMENSION(:), ALLOCATABLE :: l_DISKL

 PUBLIC l_DISKL

 ! diskll2bdyty(1,itac): serial number of body MAILx to which is attached the contactor 
 !                       DISKL numbered itac in the list of all contactors DISKL 
 ! diskl2bdyty(2,itac):  serial number of contactor DISKL itac in the list of contactors 
 !                       of any kind attached to a body diskl2bdyty(1,itac)
 integer, dimension( : , : ), allocatable, target, public :: diskl2bdyty  


 REAL(kind=8) :: min_radius,max_radius,mean_radius

 ! visu
 INTEGER :: nbpto=10 ! nb points describing the conntactor outline
 INTEGER :: nbsf=6 
 REAL(kind=8),DIMENSION(:,:),POINTER :: outlines_DISKL => NULL()
 REAL(kind=8),DIMENSION(:),POINTER :: scalarfields_DISKL => NULL()

 ! public routines

 PUBLIC read_bodies_DISKL, &
        set_precon_node_DISKL

 PUBLIC get_nb_DISKL, &
        get_radius_DISKL, get_mean_radius_DISKL, get_max_radius_DISKL, get_min_radius_DISKL, &
        nullify_reac_DISKL, nullify_vlocy_DISKL, comp_vlocy_DISKL,&
        add_reac_DISKL, &
        get_vlocy_DISKL,get_coorTT_DISKL, &
        get_ENT_DISKL, &
        clean_memory_DISKL, &
        get_visible_DISKL


 CONTAINS

!!!------------------------------------------------------------------------
 SUBROUTINE read_bodies_DISKL

   IMPLICIT NONE
   INTEGER :: nb_MAILx
   INTEGER :: ibdyty,itacty,itact,errare
   INTEGER, DIMENSION(2) :: DKidata
   REAL(kind=8),DIMENSION(2) :: DATA
                            !123456789012345678
   CHARACTER(len=18) :: IAM='DISKL::read_bodies'
   CHARACTER(len=80) :: cout

   nb_MAILx=get_nb_MAILx()

   IF (nb_MAILx == 0) RETURN

   nb_DISKL=0

   DO ibdyty=1,nb_MAILx   
     DO itacty=1,get_nb_tacty_MAILx(ibdyty)
         IF( get_tacID_MAILx(ibdyty,itacty) == 'DISKL')  nb_DISKL=nb_DISKL+1
     END DO 
   END DO

   !WRITE(cout,'(1X,I5,1X,A5)') nb_DISKL,'found'
   WRITE(cout,'(A,A,A,1x,I0,1x,A)') '[',IAM,']:',nb_DISKL,'DISKL found'
   CALL LOGMES(cout)

   IF (nb_DISKL == 0) RETURN

   allocate(diskl2bdyty(3,nb_DISKL),stat=errare)

   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating diskl2bdyty')
   END IF

   ALLOCATE(l_DISKL(nb_DISKL),stat=errare)

   IF (errare /= 0) THEN
     CALL FATERR(IAM,'error allocating l_DISKL')
   END IF

   if( .not. allocated(M2meca) ) then
     call faterr(IAM,'Please call LoadModels before LoadTactors')
   end if

   nb_DISKL=0

   DO ibdyty=1,nb_MAILx
     DO itacty=1,get_nb_tacty_MAILx(ibdyty)
       IF (get_tacID_MAILx(ibdyty,itacty) == 'DISKL') THEN
          nb_DISKL=nb_DISKL+1
          diskl2bdyty(1,nb_DISKL)=ibdyty  !   diskl2bdyty(1,itac) : serial number of body MAILx to which is attached the 
                                          !                         contactor DISKL numbered itac in the list of all 
                                          !                         contactors DISKL 
          diskl2bdyty(2,nb_DISKL)=itacty  !   diskl2bdyty(2,itac) : serial number of contactor DISKL itac in the list of 
                                          !                         contactors of any kind attached to body diskl2bdyty(1,itac)
          diskl2bdyty(3,nb_DISKL)=i_mailx !   diskl2bdyty(3,itac) : type of body the contactor is attached to
          
          l_DISKL(nb_DISKL)%ibdyty = M2meca(ibdyty)%bdyty
          CALL get_idata_MAILx(ibdyty,itacty,DKidata)

          l_DISKL(nb_DISKL)%numnoda = DKidata(1)
          l_DISKL(nb_DISKL)%numnodb = DKidata(2)

          l_DISKL(nb_DISKL)%precon  = .false.

       END IF
     END DO 
   END DO

   min_radius = 1.D20
   max_radius = 0.D0
   mean_radius= 0.D0

   DO itact=1,nb_DISKL
     min_radius  = MIN(get_radius_DISKL(itact),min_radius)
     max_radius  = MAX(get_radius_DISKL(itact),max_radius)
     mean_radius = mean_radius + get_radius_DISKL(itact)
   END DO 
   mean_radius=mean_radius/nb_DISKL

 END SUBROUTINE read_bodies_DISKL
!!!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
!
! PUBLIC ROUTINES 
!
!------------------------------------------------------------------------ 
SUBROUTINE nullify_reac_DISKL (idiskl,storage)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: idiskl
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_diskl(idiskl)%ibdyty

   CALL nullify_reac_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_reac_DISKL
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE nullify_vlocy_DISKL (idiskl,storage)
  !
  ! called by SDL solver
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: idiskl
   INTEGER            :: ibdyty
   INTEGER            :: storage  

   ibdyty=l_diskl(idiskl)%ibdyty

   CALL nullify_vlocy_mecaMAILx(ibdyty,storage)

END SUBROUTINE nullify_vlocy_DISKL
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE comp_vlocy_DISKL(idiskl,storage,need_full_vlocy)
  !
  ! called by vitrad
  !
   IMPLICIT NONE
   INTEGER,INTENT(in) :: idiskl
   INTEGER            :: ibdyty
   INTEGER            :: storage  
   logical            :: need_full_vlocy

   ibdyty=l_diskl(idiskl)%ibdyty

   if (l_DISKL(idiskl)%precon .and. .not. need_full_vlocy ) then
     CALL comp_vlocy_bynode_mecaMAILx(ibdyty,(/l_DISKL(idiskl)%numnoda,l_DISKL(idiskl)%numnodb/),storage)
   else
     CALL comp_vlocy_mecaMAILx(ibdyty,storage)
   end if

END SUBROUTINE comp_vlocy_DISKL
!------------------------------------------------------------------------
SUBROUTINE add_reac_DISKL(idiskl,reac,storage)
   IMPLICIT NONE 
   INTEGER     ,INTENT(in)   :: idiskl

   REAL(kind=8),DIMENSION(3) :: reac
   REAL(kind=8),DIMENSION(3) :: rdata
   REAL(kind=8),DIMENSION(2) :: coorA,coorB,Fa,Fb
   REAL(kind=8),DIMENSION(2) :: t,n   
   INTEGER :: storage

   INTEGER :: ibdyty,numnoda,numnodb
   INTEGER ::M_ibdyty,M_itacty 

   REAL(kind=8) :: apab,normAB
   REAL(kind=8) :: Ft,Fn,Mz,Fat,Fan,Fbt,Fbn

!!$   print*,'reac de add_reac_diskl 1 : ',reac(1)
!!$   print*,'reac de add_reac_diskl 2 : ',reac(2)
!!$   print*,'reac de add_reac_diskl 3 : ',reac(3)

   ibdyty =l_DISKL(idiskl)%ibdyty

   numnoda=l_DISKL(idiskl)%numnoda
   numnodb=l_DISKL(idiskl)%numnodb

   M_ibdyty = diskl2bdyty(1,idiskl)
   M_itacty = diskl2bdyty(2,idiskl)

   CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

   apab = rdata(1)

   !!!Creation du repere local

   coorA(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnoda)
   coorB(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)
   normAB = dsqrt((coorB(1)-coorA(1))**2+(coorB(2)-coorA(2))**2)
 
   t(1) = coorB(1)-coorA(1)
   t(2) = coorB(2)-coorA(2)
   t = t/normAB
   n(1) = -t(2)
   n(2) =  t(1)

!!$   print*,'t',t
!!$   print*,'n',n

   !!!Passage des forces dans le repere local a l'arrete

   Ft = t(1)*reac(1)+t(2)*reac(2)
   Fn = n(1)*reac(1)+n(2)*reac(2)
   Mz = reac(3)

   !! on ramene au point P sur l'arrete

   Mz = Mz - (Ft*(get_radius_DISKL(idiskl) - rdata(2)))

   Fan = ((1.D0-apab)*Fn) - (Mz/normAB)
   Fbn = Fn - Fan

   Fat = (1.D0-apab)*Ft
   Fbt = apab*Ft

   !!!Passage dans le repere global

   Fa(1) = t(1)*Fat+n(1)*Fan
   Fa(2) = t(2)*Fat+n(2)*Fan
   Fb(1) = t(1)*Fbt+n(1)*Fbn
   Fb(2) = t(2)*Fbt+n(2)*Fbn  

!!$   print*,'Fa1 : ',Fa(1)
!!$   print*,'Fa2 : ',Fa(2)
!!$   print*,'Fb1 : ',Fb(1)
!!$   print*,'Fb2 : ',Fb(2)  
         
   CALL add_reac_nodty_mecaMAILx(ibdyty,numnoda,Fa,storage)

   CALL add_reac_nodty_mecaMAILx(ibdyty,numnodb,Fb,storage)

END SUBROUTINE add_reac_DISKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_vlocy_DISKL(idiskl,storage,vlocy)
  IMPLICIT NONE
  INTEGER :: idiskl
  INTEGER :: storage  

  INTEGER ::   ibdyty,  itacty,  numnoda,  numnodb
  INTEGER :: M_ibdyty,M_itacty

  REAL(kind=8),DIMENSION(3) :: rdata

  REAL(kind=8) :: apab,normAB

  REAL(kind=8),DIMENSION(2) :: vlocya,vlocyb
  REAL(kind=8),DIMENSION(3) :: lvlocy,vlocy
  REAL(kind=8),DIMENSION(2) :: coorA,coorB
  REAL(kind=8),DIMENSION(2) :: t,n   
  REAL(kind=8) :: Vat,Van,Vbt,Vbn

  M_ibdyty = diskl2bdyty(1,idiskl)
  M_itacty = diskl2bdyty(2,idiskl)

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  apab = rdata(1)

  ibdyty=l_diskl(idiskl)%ibdyty
  numnoda=l_diskl(idiskl)%numnoda
  numnodb=l_diskl(idiskl)%numnodb

  coorA(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnoda)
  coorB(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)
  normAB = dsqrt((coorB(1)-coorA(1))**2+(coorB(2)-coorA(2))**2)
  t(1) = coorB(1)-coorA(1)
  t(2) = coorB(2)-coorA(2)
  t = t/normAB
  n(1) = -t(2)
  n(2) =  t(1)

  SELECT CASE(storage)
    CASE(iV____)
        vlocya=get_V_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_V_nodty_mecaMAILx(ibdyty,numnodb)
    CASE(iVbeg_)
        vlocya=get_Vbegin_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vbegin_nodty_mecaMAILx(ibdyty,numnodb)
    CASE(iVfree)
        vlocya=get_Vfree_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vfree_nodty_mecaMAILx(ibdyty,numnodb)
    CASE(iVaux_)
        vlocya=get_Vaux_nodty_mecaMAILx(ibdyty,numnoda)
        vlocyb=get_Vaux_nodty_mecaMAILx(ibdyty,numnodb)
    CASE default
        call faterr('DISKL::get_vlocy','unknown storage')
  END SELECT

  !!!Passage des vitesses dans le repere local a l'arrete

  Vat = t(1)*vlocya(1)+t(2)*vlocya(2)
  Van = n(1)*vlocya(1)+n(2)*vlocya(2)

  Vbt = t(1)*vlocyb(1)+t(2)*vlocyb(2)
  Vbn = n(1)*vlocyb(1)+n(2)*vlocyb(2)

  lvlocy(1) = (1.d0 -apab)*Vat + apab*Vbt
  lvlocy(2) = (1.d0 -apab)*Van + apab*Vbn
  lvlocy(3) = (vlocyb(2) - vlocya(2))/normAB

  ! on ramene au centre du disque

  lvlocy(2) = lvlocy(2) - (lvlocy(3)*(get_radius_DISKL(idiskl) - rdata(2))) 

  !!!Passage dans le repere global

  vlocy(1) = lvlocy(1)*t(1)+lvlocy(2)*n(1)
  vlocy(2) = lvlocy(1)*t(2)+lvlocy(2)*n(2)
  vlocy(3) = lvlocy(3)

END SUBROUTINE get_vlocy_DISKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
FUNCTION get_coorTT_DISKL(idiskl)
  IMPLICIT NONE
  REAL(kind=8),DIMENSION(3) :: get_coorTT_DISKL
  INTEGER                   :: idiskl
  INTEGER                   :: ibdyty, itacty, numnoda, numnodb
  INTEGER                   :: M_ibdyty,M_itacty
  REAL(kind=8),DIMENSION(2) :: coorA,coorB,coorM,CM
  REAL(kind=8),DIMENSION(2) :: t,n   
  REAL(kind=8),DIMENSION(3) :: rdata
  REAL(kind=8) :: apab
  REAL(kind=8) :: normAB,normCM

  M_ibdyty = diskl2bdyty(1,idiskl)
  M_itacty = diskl2bdyty(2,idiskl)

  ibdyty =l_DISKL(idiskl)%ibdyty
  numnoda=l_DISKL(idiskl)%numnoda
  numnodb=l_DISKL(idiskl)%numnodb

  CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)

  !!!Creation du repere local
  apab=rdata(1)

  coorA(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnoda)
  coorB(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)

  !hpc bug point always in the middle of the segment
  !hpc correction to respect apab ratio
  !coorM(1:2) = (coorA + coorB)/2.D0

  coorM(1:2) = (1.d0 - apab)*coorA(1:2)+apab*coorB(1:2)

  normAB = dsqrt((coorB(1)-coorA(1))**2+(coorB(2)-coorA(2))**2)
  normCM = get_radius_DISKL(idiskl) - rdata(2)

  t(1) = coorB(1)-coorA(1)
  t(2) = coorB(2)-coorA(2)
  t = t/normAB
  n(1) = -t(2)
  n(2) =  t(1)

  CM(1) = normCM*n(1)
  CM(2) = normCM*n(2)

  apab = rdata(1)

  get_coorTT_DISKL(1:2) = coorM(1:2) - CM(1:2)
  get_coorTT_DISKL(3) = 0.D0

!  if (ibdyty == 1) print*,'X center1 ',get_coorTT_DISKL 
!  if (ibdyty == 2) print*,'X center2 ',get_coorTT_DISKL 

END FUNCTION get_coorTT_DISKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_DISKL(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   INTEGER :: get_nb_DISKL
  
   get_nb_DISKL = nb_DISKL
  
 END FUNCTION get_nb_DISKL

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_mean_radius_DISKL(fantome)

   IMPLICIT NONE   
   REAL(kind=8),OPTIONAL :: fantome
   REAL(kind=8)  :: get_mean_radius_DISKL
 
   get_mean_radius_DISKL=mean_radius

 END FUNCTION get_mean_radius_DISKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_max_radius_DISKL(fantome)

   IMPLICIT NONE   
   REAL(kind=8),OPTIONAL :: fantome
   REAL(kind=8)  :: get_max_radius_DISKL
 
   get_max_radius_DISKL=max_radius

 END FUNCTION get_max_radius_DISKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_min_radius_DISKL(fantome)

   IMPLICIT NONE   
   REAL(kind=8),OPTIONAL :: fantome
   REAL(kind=8)  :: get_min_radius_DISKL
 
   get_min_radius_DISKL=min_radius

 END FUNCTION get_min_radius_DISKL
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 FUNCTION get_radius_DISKL(idiskl)
   IMPLICIT NONE
   INTEGER                   :: idiskl,ibdyty,numnoda,numnodb
   REAL(kind=8),DIMENSION(2) :: coorA,coorB
   REAL(kind=8)              :: get_radius_DISKL,normAB,apab,brpm
   REAL(kind=8),DIMENSION(3) :: rdata

   REAL(kind=8)              :: distAM,distMB
   REAL(kind=8),DIMENSION(2) :: coorAM,coorMB,coorM

   ibdyty =l_DISKL(idiskl)%ibdyty

   numnoda=l_DISKL(idiskl)%numnoda
   numnodb=l_DISKL(idiskl)%numnodb
   
   CALL get_rdata_MAILx(diskl2bdyty(1,idiskl),diskl2bdyty(2,idiskl),rdata)

   coorA(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnoda)
   coorB(1:2) = get_coorTT_nodty_mecaMAILx(ibdyty,numnodb)

   !hpc modification for multi-DISKL
   apab = rdata(1)

   brpm = rdata(3)
   
   coorM(1:2) = (1.d0 - apab)*coorA(1:2)+apab*coorB(1:2)
   coorAM = coorM-coorA
   coorMB = coorB-coorM

   distAM = SQRT( coorAM(1)*coorAM(1) + coorAM(2)*coorAM(2) )
   distMB = SQRT( coorMB(1)*coorMB(1) + coorMB(2)*coorMB(2) )

   !hpc position of DISKL points on the mesh
   coorA = coorM - rdata(3)*coorAM/distAM
   coorB = coorM + rdata(3)*coorMB/distMB

   normAB = (coorB(1)-coorA(1))**2 + (coorB(2)-coorA(2))**2
   get_radius_DISKL = (rdata(2)**2 + rdata(3)**2)/(2.D0 * rdata(2))

!   if (ibdyty == 2) print*,'Radius2 ',get_radius_DISKL
!   if (ibdyty == 1) print*,'Radius1 ',get_radius_DISKL


 END FUNCTION get_radius_DISKL  
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 FUNCTION get_ENT_DISKL(idiskl)

   IMPLICIT NONE
   INTEGER :: idiskl
   INTEGER :: get_ENT_DISKL
  
   get_ENT_DISKL = get_entity_mecaMAILx(l_DISKL(idiskl)%ibdyty)

 END FUNCTION get_ENT_DISKL
!------------------------------------------------------------------------
 SUBROUTINE set_precon_node_DISKL
   IMPLICIT NONE 
   INTEGER :: nb_MAILx
   INTEGER :: ibdyty,itacty,iDISKL

   nb_MAILx=get_nb_MAILx()

   IF (nb_MAILx == 0) RETURN

   iDISKL=0
   DO ibdyty=1,nb_MAILx
      DO itacty=1,get_nb_tacty_MAILx(ibdyty)
         IF (get_tacID_MAILx(ibdyty,itacty) == 'CLxxx') THEN
            iDISKL=iDISKL+1
            
            CALL set_precon_node_mecaMAILx(ibdyty,l_DISKL(iDISKL)%numnoda)
            CALL set_precon_node_mecaMAILx(ibdyty,l_DISKL(iDISKL)%numnodb)
            
            l_DISKL(idiskL)%precon = .true.          

         END IF
      END DO
   END DO


 END SUBROUTINE set_precon_node_DISKL
!------------------------------------------------------------------------
 function get_visible_DISKL(itact)

   implicit none

   integer(kind=4) :: itact 
   logical :: get_visible_DISKL
   
   get_visible_DISKL = get_visible_mecaMAILx(diskl2bdyty(1,itact))
 
 end function get_visible_DISKL
 
 !------------------------------------------------------------------------ 
 INTEGER FUNCTION get_nb_point_outline_DISKL(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome

   get_nb_point_outline_DISKL = nbpto

 END FUNCTION get_nb_point_outline_DISKL 

 !------------------------------------------------------------------------ 
 SUBROUTINE get_outline_DISKL(idiskl,outline)

   IMPLICIT NONE
   INTEGER                           :: idiskl,k
   INTEGER                           :: M_ibdyty,M_itacty,ibdyty,numnoda,numnodb
   REAL(kind=8),DIMENSION(3)         :: rdata
   REAL(kind=8)                      :: r,DPI,apab,normAB,normCM,CACB,theta
   REAL(kind=8),DIMENSION(3)         :: X
   REAL(kind=8),DIMENSION(2,0:nbpto) :: outline
   REAL(kind=8),DIMENSION(2)         :: coorA,coorB,coorM,CM,XA,XB,coorAref,coorBref
   REAL(kind=8),DIMENSION(2)         :: CA,CB,CK,CKl
   REAL(kind=8),DIMENSION(2)         :: t,n   

   REAL(kind=8)                      :: distAM,distMB
   REAL(kind=8),DIMENSION(2)         :: coorAM,coorMB


   M_ibdyty = diskl2bdyty(1,idiskl)
   M_itacty = diskl2bdyty(2,idiskl)   

   CALL get_rdata_MAILx(M_ibdyty,M_itacty,rdata)
   apab=rdata(1)

   !fd on recupere le rayon : distance entre le cercle et le cote de l'element
   r=get_radius_DISKL(idiskl)

   ibdyty  = l_DISKL(idiskl)%ibdyty
   numnoda = l_DISKL(idiskl)%numnoda
   numnodb = l_DISKL(idiskl)%numnodb

   !coorAref(1:2) = get_cooref_nodty_MAILx(M_ibdyty,numnoda)
   !coorBref(1:2) = get_cooref_nodty_MAILx(M_ibdyty,numnodb)
   
   !XA(1:2) = get_X_nodty_mecaMAILx(ibdyty,numnoda)
   !XB(1:2) = get_X_nodty_mecaMAILx(ibdyty,numnodb)

   !!fd les coordonnees actuelles
   !coorA(1:2) = coorAref(1:2) + XA(1:2)
   !coorB(1:2) = coorBref(1:2) + XB(1:2)
   
   coorA(1:2) = get_coor_nodty_MAILx(M_ibdyty,numnoda)
   coorB(1:2) = get_coor_nodty_MAILx(M_ibdyty,numnodb)

   !hpc coordinate of DISKL center
   coorM(1:2) = (1.d0 - apab)*coorA(1:2)+apab*coorB(1:2)
   coorAM = coorM-coorA
   coorMB = coorB-coorM

   distAM = SQRT( coorAM(1)*coorAM(1) + coorAM(2)*coorAM(2) )
   distMB = SQRT( coorMB(1)*coorMB(1) + coorMB(2)*coorMB(2) )
  
   !hpc position of DISKL points on the mesh
   coorA = coorM - rdata(3)*coorAM/distAM
   coorB = coorM + rdata(3)*coorMB/distMB

   normCM     = r - rdata(2)

   !fd vecteur temporaire
   t(1) = coorB(1)-coorA(1)
   t(2) = coorB(2)-coorA(2)
   
   normAB     = SQRT(t(1)*t(1)+ t(2)*t(2))
   t = t/normAB
   
   n(1) = -t(2)
   n(2) =  t(1)

   !fd vecteur centre-point milieu
   CM(1) = normCM*n(1)
   CM(2) = normCM*n(2)

   !fd position du centre
   X(1:2) = coorM(1:2) - CM(1:2)
   
   !fd on pose dans outline
   outline(1,0) = coorM(1)
   outline(2,0) = coorM(2)
   X(3)=0.D0

   !fd vecteur centre-A et centre-B 
   CA(1:2) = coorA(1:2) - X(1:2)
   CB(1:2) = coorB(1:2) - X(1:2)
   CACB = CA(1)*CB(1)+CA(2)*CB(2)

   !bof   theta = acos(CACB/(r**2))
   theta = ACOS(CACB/(DOT_PRODUCT(CA,CA)+DOT_PRODUCT(CB,CB)))

!!$   !!! on passe dans la base (t,n) de centre C !!!
!!$   
!!$   CAl(1) = t(1)*CA(1)+t(2)*CA(2)
!!$   CAl(2) = n(1)*CA(1)+n(2)*CA(2)
!!$   CBl(1) = t(1)*CA(1)+t(2)*CA(2)
!!$   CBl(2) = n(1)*CA(1)+n(2)*CA(2)

   outline(1,1) = coorB(1)
   outline(2,1) = coorB(2)   
   outline(1,nbpto) = coorA(1)
   outline(2,nbpto) = coorA(2)   

   DPI=theta/(nbpto-1)

   DO k=2,nbpto-1

     CKl(1) = r*COS((PI_g/2.D0)-(theta/2.D0)+X(3)+(k-1)*DPI)
     CKl(2) = r*SIN((PI_g/2.D0)-(theta/2.D0)+X(3)+(k-1)*DPI)

     CK(1) = t(1)*CKl(1)+n(1)*CKl(2)
     CK(2) = t(2)*CKl(1)+n(2)*CKl(2)

     outline(1,k) = X(1) + CK(1)
     outline(2,k) = X(2) + CK(2)

   END DO

 END SUBROUTINE get_outline_DISKL

 
 !------------------------------------------------------------------------
 FUNCTION init_outlines_DISKL(sz)
   IMPLICIT NONE
   INTEGER :: sz
   REAL(kind=8),DIMENSION(:,:),POINTER :: init_outlines_DISKL

   IF (sz == 0 .OR. nb_DISKL == 0 .OR. nbpto*nb_DISKL /= sz) THEN
     init_outlines_DISKL => NULL()
     RETURN
   ENDIF 

   IF (ASSOCIATED(outlines_DISKL)) DEALLOCATE(outlines_DISKL)
   ALLOCATE(outlines_DISKL(2,sz)) 

   outlines_DISKL(1:2,1:sz) = 0.d0

   init_outlines_DISKL => outlines_DISKL

 END FUNCTION init_outlines_DISKL
 
 !------------------------------------------------------------------------
 FUNCTION init_scalarfields_DISKL(sz)
   IMPLICIT NONE
   INTEGER :: sz
   REAL(kind=8),DIMENSION(:),POINTER :: init_scalarfields_DISKL

   IF (sz == 0 .OR. nb_DISKL == 0 .OR. nbsf*nb_DISKL /= sz) THEN
     init_scalarfields_DISKL => NULL()
     RETURN
   ENDIF 

   IF (ASSOCIATED(scalarfields_DISKL)) DEALLOCATE(scalarfields_DISKL)
   ALLOCATE(scalarfields_DISKL(sz)) 

   scalarfields_DISKL(1:sz) = 0.d0

   init_scalarfields_DISKL => scalarfields_DISKL

 END FUNCTION init_scalarfields_DISKL
 
 !------------------------------------------------------------------------  
 SUBROUTINE updt_scalarfield_DISKL(itacty,scalarfield)

   IMPLICIT NONE
   INTEGER :: itacty
   REAL(kind=8),DIMENSION(nbsf) :: scalarfield

   scalarfield = 0.d0
   
   !scalarfield(1:3) = get_coor_DISKL(diskl2bdyty(1,itacty),diskl2bdyty(2,itacty))
   !scalarfield(4:6) = get_V_DISKL(diskl2bdyty(1,itacty))

 END SUBROUTINE updt_scalarfield_DISKL

 !------------------------------------------------------------------------
 SUBROUTINE update_postdata_DISKL()
   IMPLICIT NONE

   !
   INTEGER :: itacty,iszo,iszsf
   REAL(kind=8) :: outline(2,0:nbpto)

   if (nb_DISKL == 0) return

   if (.not. associated(outlines_DISKL)) call faterr('DISKL::update_postdata','init_outlines is mandatory')
   if (.not. associated(scalarfields_DISKL)) call faterr('DISKL::update_postdata','init_scalarfields is mandatory')

   iszo = 0
   iszsf = 0
   DO itacty=1,nb_DISKL
     CALL get_outline_DISKL(itacty,outline) 
     ! attention la routine get_outline a ete ecrite pour gmv et necessite un contour ferme
     ! mj pas touche ...
     outlines_DISKL(:,iszo+1:iszo+nbpto-1) = outline(:,1:nbpto-1)    
     iszo = iszo + nbpto-1
     CALL updt_scalarfield_DISKL(itacty,scalarfields_DISKL(iszsf+1:iszsf+nbsf))
     iszsf = iszsf +nbsf
   ENDDO    

 END SUBROUTINE update_postdata_DISKL
!------------------------------------------------------------------------

 subroutine clean_memory_DISKL()
   implicit none
   integer(kind=4) :: i

   nb_diskl = 0

   if( allocated(diskl2bdyty) ) deallocate(diskl2bdyty)

   if( allocated(l_DISKL) ) then
     deallocate(l_DISKL)
   end if

   if( associated(outlines_DISKL) ) then
     deallocate(outlines_DISKL)
     nullify(outlines_DISKL)
   end if

   if( associated(scalarfields_DISKL) ) then
     deallocate(scalarfields_DISKL)
     nullify(scalarfields_DISKL)
   end if

 end subroutine

END MODULE DISKL

