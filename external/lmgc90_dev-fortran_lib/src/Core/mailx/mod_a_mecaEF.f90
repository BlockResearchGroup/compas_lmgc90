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
MODULE a_mecaEF

!  generic front-end to the mechanical Elements  

USE utilities

USE a_EF

USE a_mecaEF_iso
USE a_mecaEF_shb
USE a_mecaEF_shell
USE a_mecaEF_bar
USE a_mecaEF_discrete
USE a_mecaEF_joint

IMPLICIT NONE

! pour construire une liste de tous les elements disponibles
! on a un type utilisateur + un tableau

TYPE T_mecaEF
  integer          :: ID   ! kind of element formulations i_iso, i_bar, i_shell, etc
  integer          :: nb   ! rank in the elements of a formulation (ID) 
  character(len=5) :: name ! nickname of the element : DKTxx,...
END TYPE T_mecaEF


INTEGER :: nb_mecaEF=0
TYPE(T_mecaEF),DIMENSION(:),ALLOCATABLE :: mecaEF

CONTAINS

SUBROUTINE init_mecaEF
  IMPLICIT NONE

  INTEGER :: i,j,errare
  !                         123456789012345678901
  CHARACTER(len=21) :: IAM='a_mecaEF::init_mecaEF'

  !rm : skip if already loaded ...
  if( nb_mecaEF > 0 ) return

  !
  ! initialisation des types
  ! 
  CALL init_mecaEF_bar
  CALL init_mecaEF_shell
  CALL init_mecaEF_iso
  CALL init_mecaEF_shb
  CALL init_mecaEF_discrete
  CALL init_mecaEF_joint  
  !
  ! construction de la map 
  !
  nb_mecaEF=nb_mecaEF+get_nb_ele_bar()
  nb_mecaEF=nb_mecaEF+get_nb_ele_shell()
  nb_mecaEF=nb_mecaEF+get_nb_ele_iso()
  nb_mecaEF=nb_mecaEF+get_nb_ele_shb()
  nb_mecaEF=nb_mecaEF+get_nb_ele_discrete()
  nb_mecaEF=nb_mecaEF+get_nb_ele_joint()  

  ALLOCATE(mecaEF(nb_mecaEF),stat=errare)
  IF (errare /= 0) THEN
    CALL FATERR(IAM,'error allocating meca_ef')
  END IF

  j=0
  DO i=1,get_nb_ele_bar()
    mecaEF(i+j)%ID= i_bar
    mecaEF(i+j)%nb=i
    mecaEF(i+j)%NAME=get_NAME_mecaEF_bar(i)
  ENDDO
  j=j+get_nb_ele_bar()
  !
  DO i=1,get_nb_ele_shell()
    mecaEF(i+j)%ID= i_she
    mecaEF(i+j)%nb=i
    mecaEF(i+j)%NAME=get_NAME_mecaEF_shell(i)
  ENDDO
  j=j+get_nb_ele_shell()
  !
  DO i=1,get_nb_ele_iso()
    mecaEF(i+j)%ID= i_iso
    mecaEF(i+j)%nb=i
    mecaEF(i+j)%NAME=get_NAME_mecaEF_iso(i)
  ENDDO
  j=j+get_nb_ele_iso()
!
  DO i=1,get_nb_ele_shb()
    mecaEF(i+j)%ID= i_shb
    mecaEF(i+j)%nb=i
    mecaEF(i+j)%NAME=get_NAME_mecaEF_shb(i)
  ENDDO
  j=j+get_nb_ele_shb()

  DO i=1,get_nb_ele_discrete()
    mecaEF(i+j)%ID= i_dis
    mecaEF(i+j)%nb=i
    mecaEF(i+j)%NAME=get_NAME_mecaEF_discrete(i)
  ENDDO
  j=j+get_nb_ele_discrete()
  
  DO i=1,get_nb_ele_joint()
    mecaEF(i+j)%ID= i_joint
    mecaEF(i+j)%nb=i
    mecaEF(i+j)%NAME=get_NAME_mecaEF_joint(i)
  ENDDO
  j=j+get_nb_ele_joint()
  
END SUBROUTINE init_mecaEF



logical FUNCTION is_ele_mecaEF(blmnb,name)
  !
  ! fonction qui dit si l'element est du type name (iso|bar|shell|discrete|joint|shb)
  !
  IMPLICIT NONE
  INTEGER :: blmnb
  character(len=8) :: name
  !                         12345678901234567890123
  CHARACTER(len=23) :: IAM='a_mecaEF::is_ele_mecaEF'

  is_ele_mecaEF = .FALSE. 

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    if (index(name,'iso') /= 0)      is_ele_mecaEF = .TRUE.
  CASE( i_shb )
    if (index(name,'shb') /= 0)      is_ele_mecaEF = .TRUE.
  CASE( i_bar )
    if (index(name,'bar') /= 0)      is_ele_mecaEF = .TRUE.
  CASE( i_she )
    if (index(name,'shell') /= 0)    is_ele_mecaEF = .TRUE.
  CASE( i_dis )
    if (index(name,'discrete') /= 0) is_ele_mecaEF = .TRUE.
  CASE( i_joint )
    if (index(name,'joint') /= 0)    is_ele_mecaEF = .TRUE.
  CASE DEFAULT
    call FATERR(IAM,'unknown name')
  END SELECT 

END FUNCTION is_ele_mecaEF

!=======================================================================

INTEGER FUNCTION get_nb_in_mecaEF(NAME)
  !
  ! fonction qui retourne l'indice dans le tableau mecaEF 
  ! de l'element de nom NAME 
  !
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i
  !                          12345678901234567890123456
  CHARACTER(len=26)  :: IAM='a_mecaEF::get_nb_in_mecaEF'
  CHARACTER(len=30)  :: cout

  DO i=1,SIZE(mecaEF)

    IF (mecaEF(i)%NAME == NAME) THEN
      get_nb_in_mecaEF=i
      RETURN
    ENDIF

  ENDDO
                            !1234567        12345678
  WRITE(cout,'(A7,A5,A8)') 'mecaEF ',NAME,' unknown'
  CALL FATERR(IAM,cout)

END FUNCTION get_nb_in_mecaEF

!=======================================================================

INTEGER FUNCTION get_nb_in_mecaEFmodel(i)
  !
  ! fonction qui retourne l'indice de l'element d'indice i dans le tableau mecaEF d'un type d'element 
  !   
  !
  IMPLICIT NONE
  INTEGER          :: i
  !                         1234567890123456789012345678901
  CHARACTER(len=31) :: IAM='a_mecaEF::get_nb_in_mecaEFmodel'
  CHARACTER(len=30) :: cout
  !
  get_nb_in_mecaEFmodel=mecaEF(i)%nb

  !                            !1234567        12345678
  !  write(cout,'(A7,A5,A8)') 'mecaEF ',NAME,' unknown'
  !  call FATERR(IAM,cout)

  RETURN
  
END FUNCTION get_nb_in_mecaEFmodel

!=======================================================================

integer function ele_is(nb)
  !
  ! fonction qui retourne l'ID (iso/bar/shell) de l'element 
  ! d'indice nb dans mecaEF
  !
  implicit none
  integer, intent(in) :: nb

  ele_is = mecaEF(nb)%ID

end function ele_is

!=======================================================================

INTEGER FUNCTION get_bw_mecaEF(i,nodes)
  !
  ! fonction qui retourne la largueur de bande de l'element
  ! d'indice i dans mecaEF
  !
  IMPLICIT NONE
  INTEGER :: nb,i
  INTEGER,DIMENSION(:) :: nodes
  !                         12345678901234567890123
  CHARACTER(len=23) :: IAM='a_mecaEF::get_bw_mecaEF'

  nb=get_nb_in_mecaEFmodel(i)

  SELECT CASE (ele_is(i))
  CASE( i_iso )
    get_bw_mecaEF=get_bw_mecaEF_iso(nb,nodes)
  CASE( i_shb )
    get_bw_mecaEF=get_bw_mecaEF_shb(nb,nodes)
  CASE( i_bar )
    get_bw_mecaEF=get_bw_mecaEF_bar(nb,nodes)
  CASE( i_she )
    get_bw_mecaEF=get_bw_mecaEF_shell(nb,nodes)
  CASE( i_dis )
    get_bw_mecaEF=get_bw_mecaEF_discrete(nb,nodes)
  CASE( i_joint )
    get_bw_mecaEF=get_bw_mecaEF_joint(nb,nodes)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

  !!  print*,'element de type: ',ele_is(i)
  !!  print*,'numero dans le type:',nb
  !!  print*,'connectivite   : ',nodes
  !!  print*,'largeur de bande:',get_bw_mecaEF

END FUNCTION get_bw_mecaEF

!============ compute elementary stiffness matrix ===

SUBROUTINE compute_elementary_bulk(blmnb,ppsnb,ibdyty,iblmty,dt,coor_ele,dep_ele, &
                                   Fint,compute_Fint,                             &
                                   K,compute_stiffness,                           &
                                   push_fields)       
  !
  ! fonction qui calcule la matrice de rigidite elementaire et le second membre d'un
  ! element ayant l'indice blmnb dans mecaEF, de comportement mdlnb, de loi lawnb
  ! et de position coor_ele
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,dep_ele
  real(kind=8) :: dt
  REAL(kind=8),DIMENSION(:)   :: Fint
  REAL(kind=8),DIMENSION(:,:) :: K
  logical :: compute_fint,compute_stiffness,push_fields

  integer,dimension(:) :: ppsnb

  !                         123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF::compute_elementary_bulk'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL compute_elementary_bulk_ISO(nb,ppsnb,ibdyty,iblmty,dt,coor_ele,dep_ele, &
                                    fint,compute_Fint,                           &
                                    K,compute_stiffness,                         &
                                    push_fields)
  case( i_bar )
    CALL compute_elementary_bulk_bar(nb,ppsnb,dt,coor_ele,dep_ele,ibdyty,iblmty, &
                                    fint,compute_Fint,                           &
                                    K,compute_stiffness,                         &
                                    push_fields)

  !case( i_she )
  !  call RIG_ELA_DKT(nb,coor_ele,K)
    
  CASE( i_shb )
    CALL compute_elementary_bulk_shb(nb,ppsnb,dt,coor_ele,dep_ele,ibdyty,iblmty, &
                                    fint,compute_Fint,                           &
                                    K,compute_stiffness,                         &
                                    push_fields)
  CASE( i_dis )
    CALL compute_elementary_bulk_discrete(nb,ppsnb,coor_ele,dep_ele,ibdyty,iblmty,fint,K)

  CASE( i_joint )

    CALL compute_elementary_bulk_JOINT(nb,ppsnb,ibdyty,iblmty,dt,coor_ele,dep_ele, &
                                    fint,compute_Fint,                           &
                                    K,compute_stiffness,                         &
                                    push_fields)    
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  endselect

END SUBROUTINE compute_elementary_bulk

!=======================================================================

! SUBROUTINE compute_elementary_fields(blmnb,ppsnb,ibdyty,iblmty,dt,coor_ele,dep_ele)       
!   !
!   ! fonction qui calcule contraintes et variables internes d'un
!   ! element ayant l'indice blmnb dans mecaEF, de comportement mdlnb, de loi lawnb
!   ! et de position coor_ele
!   !  
!   IMPLICIT NONE
!   INTEGER :: blmnb,nb
!   INTEGER :: ibdyty,iblmty
!   REAL(kind=8),DIMENSION(:,:) :: coor_ele,dep_ele
!   real(kind=8) :: dt

!   integer,dimension(:) :: ppsnb

!   !                         12345678901234567890123456789012345
!   CHARACTER(len=35) :: IAM='a_mecaEF::compute_elementary_fields'

!   nb=get_nb_in_mecaEFmodel(blmnb)

!   SELECT CASE (ele_is(blmnb))
!   CASE( i_iso )
!      CALL compute_elementary_fields_ISO(nb,ppsnb,ibdyty,iblmty,dt,coor_ele,dep_ele)

!   case( i_bar )
!      CALL compute_elementary_fields_bar(nb,ppsnb,dt,coor_ele,dep_ele,ibdyty,iblmty)

!   !case( i_she )

!   case( i_shb )
!      CALL compute_elementary_fields_SHB(nb,ppsnb,dt,coor_ele,dep_ele,ibdyty,iblmty)

!   CASE( i_dis )
!      ! rien a faire

!   CASE( i_joint )
!      CALL compute_elementary_fields_JOINT(nb,ppsnb,ibdyty,iblmty,dt,coor_ele,dep_ele)

     
!   CASE DEFAULT
!     call FATERR(IAM,'unknown elementary model')
!   end select

! END SUBROUTINE compute_elementary_fields

!=======================================================================

SUBROUTINE compute_elementary_mass(blmnb,ppsnb,ibdyty,iblmty,coor_ele,M)       
  !
  ! fonction qui calcule la matrice de rigidite elementaire d'un
  ! element ayant l'indice blmnb dans mecaEF, de comportement mdlnb, de loi lawnb
  ! et de position coor_ele
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,ibdyty,iblmty
  integer,dimension(:) :: ppsnb
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:,:) :: M
  !                         123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF::compute_elementary_mass'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL compute_elementary_mass_iso(nb,ppsnb,ibdyty,iblmty,coor_ele,M)

  CASE( i_shb )
    CALL compute_elementary_mass_shb(nb,ppsnb,coor_ele,M)

  !CASE( i_she )
  !   call MASS_DKT(nb,coor_ele,M)

  CASE( i_bar )
    call compute_elementary_mass_bar(nb,ppsnb,coor_ele,M,ibdyty,iblmty)

  CASE( i_dis )
     CALL compute_elementary_mass_discrete(nb,ppsnb,coor_ele,M)
     
  CASE( i_joint )
      !nada

  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE compute_elementary_mass

!=======================================================================

!> \brief Compute an internal forces
SUBROUTINE compute_elementary_internal_force(blmnb,ppsnb,ibdyty,iblmty,coor_ele,Fint)       
  !
  ! fonction qui calcule la force interieure elementaire
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:)   :: Fint

  integer,dimension(:) :: ppsnb

  !                         1234567890123456789012345678901234567890123
  CHARACTER(len=43) :: IAM='a_mecaEF::compute_elementary_internal_force'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL Stress2Fint_ISO(nb,ppsnb,ibdyty,iblmty,coor_ele,fint)

  CASE( i_shb )
    CALL Stress2Fint_SHB(nb,ppsnb,coor_ele,ibdyty,iblmty,fint)

  case( i_bar )
    !fd todo

  !case( i_she )
  !  call RIG_ELA_DKT(nb,coor_ele,K)

  CASE( i_dis )
    Fint=0.d0

  CASE( i_joint )
    CALL Stress2Fint_JOINT(nb,ppsnb,ibdyty,iblmty,coor_ele,fint)
    
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE compute_elementary_internal_force

!=======================================================================

SUBROUTINE compute_elementary_volume(blmnb,coor_ele,volume)       
!
! fonction qui calcule le volume d un element
!  
  IMPLICIT NONE
  INTEGER                     :: blmnb,nb
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8)                :: volume

!                           12345678901234567890123456789012345
  CHARACTER(len=35) :: IAM='a_mecaEF::compute_elementary_volume'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
      CALL compute_elementary_volume_iso(nb,coor_ele,volume)
!  CASE( i_dkt )
!     call MASS_DKT(nb,coor_ele,M)
  CASE( i_bar )
    volume=0.d0
  CASE( i_dis )
    volume = 0.d0
  CASE( i_joint )
    volume = 0.d0
 CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE compute_elementary_volume

!=======================================================================

SUBROUTINE compute_elementary_jacobian(blmnb,ppsnb,ibdyty,iblmty,jacobian)       
  !
  ! fonction qui calcule le volume d un element
  !  
  IMPLICIT NONE
  INTEGER                     :: blmnb,nb,ppsnb(:),ibdyty,iblmty
  REAL(kind=8)                :: jacobian

  !                         1234567890123456789012345678901234567
  CHARACTER(len=37) :: IAM='a_mecaEF::compute_elementary_jacobian'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
      CALL compute_elementary_jacobian_iso(nb,ppsnb,ibdyty,iblmty,jacobian)

  !CASE( i_dkt )
  !     

  !CASE( i_bar )
  !

  !CASE( i_dis )
  !
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')

  END SELECT

END SUBROUTINE compute_elementary_jacobian

!=======================================================================

SUBROUTINE compute_elementary_center(blmnb,coor_ele,center)       
  !
  ! fonction qui calcule la poistion du centre d'un element
  !  
  IMPLICIT NONE
  INTEGER                     :: blmnb,nb
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),dimension(:)   :: center

  !                         12345678901234567890123456789012345
  CHARACTER(len=35) :: IAM='a_mecaEF::compute_elementary_center'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
      CALL compute_elementary_center_iso(nb,coor_ele,center)

  !CASE( i_dkt )
  !

  !CASE( i_bar )
  !

  !CASE( i_dis )
  !

  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE compute_elementary_center

!=======================================================================

SUBROUTINE compute_elementary_energy(blmnb,ppsnb,ibdyty,iblmty, &
                                     coor_ele,E_def)       
  !
  ! compute deformation energy of element 
  ! 
  IMPLICIT NONE
  INTEGER :: blmnb,ppsnb(:),nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8)                :: E_def

  !                         12345678901234567890123456789012345
  CHARACTER(len=35) :: IAM='a_mecaEF::compute_elementary_energy'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL ENERGY_ISO (nb,ppsnb,ibdyty,iblmty,coor_ele,E_def)

  !case( i_she )
  !

  !case( i_bar )
  !

  !CASE( i_dis )
  !

  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE compute_elementary_energy

!=======================================================================

SUBROUTINE compute_elementary_power(blmnb,mdlnb,lawnb,ibdyty,iblmty, &
                                     coor_ele,V_ele,P_def)       
  !
  ! compute power of element
  ! 
  IMPLICIT NONE
  INTEGER :: blmnb,mdlnb,lawnb,nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:)   :: V_ele
  REAL(kind=8)                :: P_def
  !                         1234567890123456789012345678901234
  CHARACTER(len=34) :: IAM='a_mecaEF::compute_elementary_power'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL POWER_ISO (nb,mdlnb,lawnb,ibdyty,iblmty,coor_ele,V_ele,P_def)

  !case( i_she )
  !

  !case( i_bar )
  !

  !CASE( i_dis )
  !
    
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')

  END SELECT

END SUBROUTINE compute_elementary_power

!=======================================================================

! subroutine get_coor_gp(blmnb,coor_ele,coor_pg)
!   !
!   ! fonction qui calcule les coordonnees des points de gauss 
!   ! d'element ayant l'indice blmnb dans mecaEF, 
!   ! de property set  ppsnb
!   ! et de position coor_ele
!   !  
!   implicit none
!   integer :: blmnb,nb
!   real(kind=8),dimension(:,:) :: coor_ele
!   real(kind=8),dimension(:,:) :: coor_pg
!   !                         123456789012345678901
!   CHARACTER(len=21) :: IAM='a_mecaEF::get_coor_gp'

!   nb=get_nb_in_mecaEFmodel(blmnb)

!   select case (ele_is(blmnb))
!   case( i_iso )
!     call get_coor_pg_ISO(nb,coor_ele,coor_pg)

!   case( i_shb )
!     call get_coor_pg_SHB(nb,coor_ele,coor_pg)

!   !case( i_she )

!   !case( i_bar )

!   !CASE( i_dis )

!   case( i_joint )
!     call get_coor_pg_joint(nb,coor_ele,coor_pg)

    
!   CASE DEFAULT
!     call FATERR(IAM,'unknown elementary model')
!   endselect

! end subroutine get_coor_gp

!=======================================================================

subroutine interpolate_node2gp(blmnb,valnoe,valpg)
  !
  ! fonction qui interpole au pg des valeurs connues aux noeuds
  !  
  implicit none
  integer :: blmnb,nb
  real(kind=8),dimension(:) :: valnoe
  real(kind=8),dimension(:) :: valpg
  !                         12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_mecaEF::interpolate_node2gp'

  nb=get_nb_in_mecaEFmodel(blmnb)

  select case (ele_is(blmnb))
  case( i_iso )
    call   interpolate_node2pg_ISO(nb,valnoe,valpg)

  case( i_shb )
    call   interpolate_node2pg_SHB(nb,valnoe,valpg)

  !case( i_she )

  !case( i_bar )

  !CASE( i_dis )

  case( i_joint )
    call interpolate_node2pg_JOINT(nb,valnoe,valpg)
    
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')

  endselect

end subroutine interpolate_node2gp

!=======================================================================

SUBROUTINE gpv2node_2D(blmnb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)       
  !
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,required_field,nbfields,nbnodes_stored,mdlnb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: nodalvalues

  !                         123456789012345678901
  CHARACTER(len=21) :: IAM='a_mecaEF::gpv2node_2D'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL gpv2node_2D_iso(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)

  !case( i_she )
  !

  case( i_bar )
    CALL gpv2node_2D_bar(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        

  !case ( i_dis )
  !
  case( i_joint )
    
     NodalValues = 0.d0
     NbNodes_stored = 0


  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')

  END SELECT

END SUBROUTINE gpv2node_2D

!=======================================================================

SUBROUTINE gpv2node_3D(blmnb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)       
  !
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,required_field,nbfields,nbnodes_stored,mdlnb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: nodalvalues

  !                         123456789012345678901
  CHARACTER(len=21) :: IAM='a_mecaEF::gpv2node_3D'


  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL gpv2node_3D_iso(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        

  CASE( i_shb )
    CALL gpv2node_3D_shb(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)   

  !case( i_she )
  !

  case( i_bar )
    CALL gpv2node_3D_bar(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        

  case ( i_dis )
     NodalValues = 0.d0
     NbNodes_stored = 0

  case( i_joint)

    CALL gpv2node_3D_joint(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)             
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE gpv2node_3D

!=======================================================================

SUBROUTINE gpv2element_3D(blmnb,mdlnb,ibdyty,iblmty,required_field,Field,NbFields)       
  !
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,mdlnb,nb,required_field,nbfields
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:) :: Field

  !                         123456789012345678901234
  CHARACTER(len=24) :: IAM='a_mecaEF::gpv2element_3D'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL gpv2element_3D_iso(nb,mdlnb,ibdyty,iblmty,required_field,Field,NbFields)        

  CASE( i_shb )
    Field = 0.d0    

  case( i_she )
    Field = 0.d0    

  case( i_bar )
    Field = 0.d0    

  case ( i_dis )
     Field = 0.d0

  case( i_joint)
    CALL gpv2element_3D_joint(nb,mdlnb,ibdyty,iblmty,required_field,Field,NbFields)             
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE gpv2element_3D

!===================================================
!============= low level routines ==================
!===================================================

INTEGER FUNCTION get_N_NODE_mecaEF(NAME)
  !
  ! fonction qui retourne le nombre de noeuds 
  ! de l'element de nom NAME
  !
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb
  !                         123456789012345678901234567
  CHARACTER(len=27) :: IAM='a_mecaEF::get_N_NODE_mecaEF'

  i=get_nb_in_mecaEF(NAME)  
  nb=get_nb_in_mecaEFmodel(i)

  SELECT CASE (ele_is(i))
  CASE( i_iso )
      get_N_NODE_mecaEF=get_N_NODE_mecaEF_iso(nb)
  CASE( i_shb )
      get_N_NODE_mecaEF=get_N_NODE_mecaEF_shb(nb)
  CASE( i_bar )
      get_N_NODE_mecaEF=get_N_NODE_mecaEF_bar(nb)
  CASE( i_she )
      get_N_NODE_mecaEF=get_N_NODE_mecaEF_shell(nb)
  CASE( i_dis )
      get_N_NODE_mecaEF=get_N_NODE_mecaEF_discrete(nb)
  CASE( i_joint )
      get_N_NODE_mecaEF=get_N_NODE_mecaEF_joint(nb)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END FUNCTION get_N_NODE_mecaEF

!=======================================================================

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF(NAME)
  !
  ! fonction qui retourne le nombre de degres de liberte aux noeuds
  ! de l'element de nom NAME
  !
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb
  !                         1234567890123456789012345678901234
  CHARACTER(len=34) :: IAM='a_mecaEF::get_N_DOF_by_NODE_mecaEF'

  i=get_nb_in_mecaEF(NAME)  
  nb=get_nb_in_mecaEFmodel(i)

  SELECT CASE (ele_is(i))
  CASE( i_iso )
      get_N_DOF_by_NODE_mecaEF=get_N_DOF_by_NODE_mecaEF_iso(nb)
  CASE( i_bar )
      get_N_DOF_by_NODE_mecaEF=get_N_DOF_by_NODE_mecaEF_bar(nb)
  CASE( i_she )
      get_N_DOF_by_NODE_mecaEF=get_N_DOF_by_NODE_mecaEF_shell(nb)
  CASE( i_shb )
      get_N_DOF_by_NODE_mecaEF=get_N_DOF_by_NODE_mecaEF_shb(nb)
  CASE( i_dis )
      get_N_DOF_by_NODE_mecaEF=get_N_DOF_by_NODE_mecaEF_discrete(nb)
  CASE( i_joint )
      get_N_DOF_by_NODE_mecaEF=get_N_DOF_by_NODE_mecaEF_joint(nb)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END FUNCTION get_N_DOF_by_NODE_mecaEF

!=======================================================================

INTEGER FUNCTION get_N_GP_mecaEF(NAME)
  !
  ! fonction qui retourne le nombre de points de gauss
  ! de l'element de nom NAME
  !
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb
  !                         1234567890123456789012345
  CHARACTER(len=25) :: IAM='a_mecaEF::get_N_GP_mecaEF'

  i=get_nb_in_mecaEF(NAME)  
  nb=get_nb_in_mecaEFmodel(i)

  SELECT CASE (ele_is(i))
  CASE( i_iso )
    get_N_GP_mecaEF=get_N_GP_mecaEF_iso(nb)
  case( i_bar )
    get_N_GP_mecaEF=get_N_GP_mecaEF_bar(nb)
  case( i_she )
    get_N_GP_mecaEF=get_N_GP_mecaEF_shell(nb)
  case( i_shb )
    get_N_GP_mecaEF=get_N_GP_mecaEF_shb(nb)
  case( i_dis )
     get_N_GP_mecaEF=0
  CASE( i_joint )
    get_N_GP_mecaEF=get_N_GP_mecaEF_joint(nb)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END FUNCTION get_N_GP_mecaEF

!=======================================================================

SUBROUTINE get_ele_gp_coor(blmnb,ppsnb,ibdyty,iblmty,coor_ele,coor_pg)
  !
  ! fonction qui retourne la position des points de gauss d'un element
  !
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,coor_pg

  integer,dimension(:) :: ppsnb

  !                         1234567890123456789012345
  CHARACTER(len=25) :: IAM='a_mecaEF::get_ele_gp_coor'


  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     call get_coor_pg_ISO(nb,coor_ele,coor_pg)

  !case( i_she )

  CASE( i_shb )
     call get_coor_pg_SHB(nb,coor_ele,coor_pg)
  case( i_bar )
     call get_coor_pg_BAR(nb,coor_ele,coor_pg)
  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  CASE( i_joint )
     call get_coor_pg_JOINT(nb,coor_ele,coor_pg)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 

!=======================================================================

SUBROUTINE get_ele_gp_fields(blmnb,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)
  !
  ! fonction qui retourne les champs (defo/contrainte) aux points de gauss d'un element
  !
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  integer :: required_Field,FieldSize
  REAL(kind=8),DIMENSION(:,:) :: field

  integer,dimension(:) :: ppsnb

  !                         123456789012345678901234567
  CHARACTER(len=27) :: IAM='a_mecaEF::get_ele_gp_fields'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     if (nbdime == 3 .or. nbDIME == 2) then
       call gpv_iso(nb,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

  !case( i_she )

  CASE( i_shb )
     if (nbdime == 3) then
       call gpv_3D_shb(nb,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

  case( i_bar )
       call gpv_bar(nb,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)

  case ( i_dis )

     call FATERR(IAM,'impossible for this mecaEF model')

  CASE( i_joint )
     if (nbdime == 3 .or. nbDIME == 2) then
       call gpv_field_joint(nb,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)
     else
       call faterr(IAM,'Unimplemented function')
     endif   
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 


SUBROUTINE get_joint_gp_vec(blmnb,ppsnb,ibdyty,iblmty,required_vec,vec)
  !
  ! fonction qui retourne les champs (defo/contrainte) aux points de gauss d'un element
  !
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  integer :: required_vec
  REAL(kind=8),DIMENSION(:,:) :: vec

  integer,dimension(:) :: ppsnb

  !                         12345678901234567890123456
  CHARACTER(len=27) :: IAM='a_mecaEF::get_joint_gp_vec'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     call faterr(IAM,'Unimplemented function')

  case( i_she )
    call faterr(IAM,'Unimplemented function')

  CASE( i_shb )
     call faterr(IAM,'Unimplemented function')

  case( i_bar )
     call faterr(IAM,'Unimplemented function')     

  case ( i_dis )
     call FATERR(IAM,'Unimplemented function')

  CASE( i_joint )
     if (nbdime == 3 .or. nbDIME == 2) then
       call gpv_vec_joint(nb,ppsnb,ibdyty,iblmty,required_vec,vec)
     else
       call faterr(IAM,'Unimplemented function')
     endif   
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 


SUBROUTINE get_joint_gp_frame(blmnb,ppsnb,ibdyty,iblmty,Field,FieldSize)
  !
  ! fonction qui retourne le repere local aux points de gauss d'un element
  !
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  integer :: FieldSize
  REAL(kind=8),DIMENSION(:,:) :: field

  integer,dimension(:) :: ppsnb

  !                         1234567890123456789012345678
  CHARACTER(len=26) :: IAM='a_mecaEF::get_joint_gp_frame'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    call faterr(IAM,'Unimplemented function')

  case( i_she )
    call faterr(IAM,'Unimplemented function')
     
  CASE( i_shb )
    call faterr(IAM,'Unimplemented function')

  case( i_bar )
    call faterr(IAM,'Unimplemented function')

  case ( i_dis )
    call faterr(IAM,'Unimplemented function')

  CASE( i_joint )
    if (nbdime == 3 .or. nbDIME == 2) then
      call gpv_frame_joint(nb,ppsnb,ibdyty,iblmty,Field,FieldSize)
    else
      call faterr(IAM,'Unimplemented function')
    endif   
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 

!=======================================================================

SUBROUTINE get_joint_gp_endo(blmnb,ppsnb,ibdyty,iblmty,endo)
  !
  ! fonction qui retourne le repere local aux points de gauss d'un element
  !
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty

  REAL(kind=8),DIMENSION(:) :: endo

  integer,dimension(:) :: ppsnb

  !                         123456789012345678901234567
  CHARACTER(len=26) :: IAM='a_mecaEF::get_joint_gp_endo'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    call faterr(IAM,'Unimplemented function')

  case( i_she )
    call faterr(IAM,'Unimplemented function')
     
  CASE( i_shb )
    call faterr(IAM,'Unimplemented function')

  case( i_bar )
    call faterr(IAM,'Unimplemented function')

  case ( i_dis )
    call faterr(IAM,'Unimplemented function')

  CASE( i_joint )
    if (nbdime == 3 .or. nbDIME == 2) then
      call gpv_endo_joint(nb,ppsnb,ibdyty,iblmty,endo)
    else
      call faterr(IAM,'Unimplemented function')
    endif   
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 

!=======================================================================

SUBROUTINE get_ele_gp_all_internal(blmnb,ppsnb,ibdyty,iblmty,Field,FieldSize)
  !
  ! fonction qui retourne les internal au points de gauss
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  integer :: FieldSize
  REAL(kind=8),DIMENSION(:,:),pointer :: field

  integer,dimension(:) :: ppsnb

  !                         12345678901234567890123456789
  CHARACTER(len=27) :: IAM='a_mecaEF::get_ele_gp_internal'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     if (nbdime == 3 .or. nbDIME == 2) then
       call gpv_all_internal_iso(nb,ppsnb,ibdyty,iblmty,Field,FieldSize)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

  CASE( i_shb )
     call faterr(IAM,'impossible for this mecaEF model')

  case( i_bar )
     call faterr(IAM,'impossible for this mecaEF model')

  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')

  CASE( i_joint )
     if (nbdime == 3 .or. nbDIME == 2) then
       call gpv_internal_joint(nb,ppsnb,ibdyty,iblmty,Field,FieldSize)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')

  end select

END SUBROUTINE 

!=======================================================================

SUBROUTINE get_ele_internal_integral(blmnb,ppsnb,ibdyty,iblmty,X,i,val)
  !
  ! fonction qui retourne l inegrale d'un internal
  !  
  IMPLICIT NONE
  INTEGER                     :: blmnb, ibdyty, iblmty, i
  REAL(kind=8),DIMENSION(:,:) :: X
  real(kind=8)                :: val
  integer,dimension(:)        :: ppsnb

  !                         12345678901234567890123456789
  CHARACTER(len=27) :: IAM='a_mecaEF::get_ele_gp_internal'

  integer :: nb
  
  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     if (nbdime == 3 .or. nbDIME == 2) then
       call element_internal_iso(nb, ppsnb, ibdyty, iblmty, X, i, val)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

  CASE( i_shb )
     call faterr(IAM,'impossible for this mecaEF model')


  case( i_bar )
     call faterr(IAM,'impossible for this mecaEF model')

  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')


  case ( i_joint )
     call FATERR(IAM,'impossible for this mecaEF model')

     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 

!=======================================================================

SUBROUTINE get_ele_gp_external_fields(blmnb,ppsnb,ibdyty,iblmty,Field)
  !
  ! fonction qui retourne les champs externes aux points de gauss d'un element 
  !
  !  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: field

  integer,dimension(:) :: ppsnb

  !                         123456789012345678901234567890123456
  CHARACTER(len=36) :: IAM='a_mecaEF::get_ele_gp_external_fields'


  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     if (nbdime == 3) then
       call gp_external_field_3D_iso(nb,ppsnb,ibdyty,iblmty,Field)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

  CASE( i_shb )
     if (nbdime == 3) then     
       call gp_external_field_3D_shb(nb,ppsnb,ibdyty,iblmty,Field)
     else
       call faterr(IAM,'Unimplemented function')
     endif   

  case( i_bar )
    call gp_external_field_bar(nb,ppsnb,ibdyty,iblmty,Field)

  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')


  CASE( i_joint )
     if (nbdime == 3) then
       call gp_external_field_3D_joint(nb,ppsnb,ibdyty,iblmty,Field)
     else
       call faterr(IAM,'Unimplemented function')
     endif   
     
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE 

!=======================================================================

SUBROUTINE compute_elementary_ortho_frame(blmnb,ppsnb,ibdyty,iblmty,coor_ele)
  !
  ! fd experimental
  ! fonction qui calculent des choses via le module user
  !  
  IMPLICIT NONE
  INTEGER :: ibdyty,iblmty,blmnb,nb
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  integer,dimension(:) :: ppsnb

  !                         1234567890123456789012345678901234567890
  CHARACTER(len=40) :: IAM='a_mecaEF::compute_elementary_ortho_frame'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
      CALL compute_elementary_ortho_frame_iso(nb,ppsnb,ibdyty,iblmty,coor_ele)

  !CASE( i_dkt )

  !CASE( i_bar )

  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  case ( i_joint )
     call FATERR(IAM,'impossible for this mecaEF model')
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE 

!=======================================================================

SUBROUTINE compute_elementary_field(blmnb,ppsnb,time,dt)       
  IMPLICIT NONE
  INTEGER              :: blmnb,nb
  integer,dimension(:) :: ppsnb
  real(kind=8)         :: time,dt

!                           1234567890123456789012345678901234
  CHARACTER(len=34) :: IAM='a_mecaEF::compute_elementary_field'

  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL compute_elementary_field_iso(nb,ppsnb,time,dt)
  !CASE( i_dkt )

  CASE( i_bar )
    !fd CALL compute_elementary_field_bar(nb,ppsnb,time,dt)    
  case( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  CASE( i_joint )
    CALL compute_elementary_field_joint(nb,ppsnb,time,dt)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE 

!=======================================================================

SUBROUTINE get_elementary_field(blmnb,ppsnb,name,time,dt,coor_ele,field_ele)       
  IMPLICIT NONE
  INTEGER                     :: blmnb,nb
  integer,dimension(:)        :: ppsnb
  CHARACTER(len=30)           :: name          ! nom du field
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:)   :: field_ele
  real(kind=8)                :: time,dt

  !                         123456789012345678901234567890
  CHARACTER(len=30) :: IAM='a_mecaEF::get_elementary_field'


  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    CALL get_elementary_field_iso(nb,ppsnb,name,time,dt,coor_ele,field_ele)
  !CASE( i_dkt )

  !CASE( i_bar )
  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  CASE( i_joint )
    CALL get_elementary_field_joint(nb,ppsnb,name,time,dt,coor_ele,field_ele)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE 

!=======================================================================

subroutine compute_elementary_field_divergence_mecaEF(blmnb, coor_ele, vField_ele, Fext_ele)       

   implicit none
  
   ! variables d'entree :
   ! indice dans mecaEF
   integer, intent(in) :: blmnb                          
   ! coordonnees des sommets de l'element
   real(kind=8), dimension(:, :), intent(in) :: coor_ele, vField_ele

   ! variable de sortie :
   real(kind=8), dimension(:), intent(out) :: Fext_ele
  
   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_mecaEFmodel(blmnb)

   ! on appelle la routine de calcul adaptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
     
         call compute_elementary_field_divergence_ISO(nb, coor_ele, vfield_ele, Fext_ele)
    
   end select

end subroutine compute_elementary_field_divergence_mecaEF

!=======================================================================

!> \brief get the number of dofs in the element
function get_N_DOF_mecaEF(name)
  implicit none
  character(len=5), intent(in) :: name !< [in] name of the element
  integer(kind=4) :: get_N_DOF_mecaEF  !< [return] number of dof in the element
  !
  integer(kind=4)   :: i,nb
  character(len=26) :: IAM
  !       12345678901234567890123456
  IAM = 'a_mecaEF::get_N_DOF_mecaEF'

  i  = get_nb_in_mecaEF(NAME)  
  nb = get_nb_in_mecaEFmodel(i)

  select case (ele_is(i))
  case( i_iso )
      get_N_DOF_mecaEF = get_N_DOF_mecaEF_iso(nb)
  case( i_shb )
      get_N_DOF_mecaEF = get_N_DOF_mecaEF_shb(nb)
  !case( i_bar )
  !    get_N_DOF_mecaEF = get_N_DOF_mecaEF_bar(nb)
  !case( i_she )
  !    get_N_DOF_mecaEF = get_N_DOF_mecaEF_shell(nb)
  !case( i_dis )
  !    get_N_DOF_mecaEF = get_N_DOF_mecaEF_discrete(nb)
  case( i_joint )
      get_N_DOF_mecaEF = get_N_DOF_mecaEF_joint(nb)
  case default
    call faterr(IAM,'unknown elementary model')
  end select
end function

!=======================================================================

!> \brief get the number of dofs of a node of the element
function get_N_DOF_of_NODE_mecaEF(name, i_node)
  implicit none
  character(len=5), intent(in) :: name        !< [in] name of the element
  integer(kind=4),  intent(in) :: i_node      !< [in] index of the node
  integer(kind=4) :: get_N_DOF_of_NODE_mecaEF !< [return] number of dof of the node
  !
  integer(kind=4)   :: i,nb
  character(len=33) :: IAM
  !       123456789012345678901234567890123
  IAM = 'a_mecaEF::get_N_DOF_of_NODE_mecaEF'

  i  = get_nb_in_mecaEF(NAME)  
  nb = get_nb_in_mecaEFmodel(i)

  select case (ele_is(i))
  case( i_iso )
      get_N_DOF_of_NODE_mecaEF = get_N_DOF_of_NODE_mecaEF_iso(nb, i_node)
  case( i_shb )
      get_N_DOF_of_NODE_mecaEF = get_N_DOF_of_NODE_mecaEF_shb(nb, i_node)
  !case( i_bar )
  !    get_N_DOF_of_NODE_mecaEF = get_N_DOF_of_NODE_mecaEF_bar(nb, i_node)
  !case( i_she )
  !    get_N_DOF_of_NODE_mecaEF = get_N_DOF_of_NODE_mecaEF_shell(nb, i_node)
  !case( i_dis )
  !    get_N_DOF_of_NODE_mecaEF = get_N_DOF_of_NODE_mecaEF_discrete(nb, i_node)
  case( i_joint )
      get_N_DOF_of_NODE_mecaEF = get_N_DOF_of_NODE_mecaEF_joint(nb, i_node)
  case default
    call faterr(IAM,'unknown elementary model')
  end select
end function

!=== NEW ARCH -- RIP ===========

! rm's work for new arch -- unplugged by fd 03/06/2015

!============ compute elementary stiffness matrix ===

!> \brief Compute an elementary bulk element
!> Identical to compute_elementary_bulk but with new arch API
subroutine compute_elementary_bulk2(blmnb,ppsnb,dt,coor_ele,dep_ele,fields,gauss_map, gauss_names,Fint,K)       
  implicit none
  integer(kind=4),                   intent(in)    :: blmnb       !< [in] element model number
  integer(kind=4),   dimension(:)  , intent(in)    :: ppsnb       !< [in] property set
  real(kind=8)                                     :: dt          !< [in] time step
  integer(kind=4),   dimension(:,:), intent(in)    :: gauss_map   !< [in] map to acces gauss point fields
  character(len=30), dimension(:)  , intent(in)    :: gauss_names !< [in] map to get gauss fields name
  real(kind=LONG),   dimension(:)  , intent(inout) :: fields      !< [in] fields values at gauss points
  real(kind=LONG),   dimension(:,:), intent(in)    :: coor_ele    !< [in] reference coordinates of the nodes
  real(kind=LONG),   dimension(:)  , intent(in)    :: dep_ele     !< [in] displacement of the nodes
  real(kind=LONG),   dimension(:)  , intent(out)   :: Fint        !< [out] elementary internal forces vector
  real(kind=LONG),   dimension(:,:), intent(out)   :: K           !< [out] elementary rigidity matrix
  !
  integer(kind=4) :: nb

  nb=get_nb_in_mecaEFmodel(blmnb)

  select case (ele_is(blmnb))
    case( i_iso )

       !unplugged by fd call compute_elementary_bulk_iso2(nb,ppsnb,dt,coor_ele,dep_ele,fields,gauss_map,gauss_names,Fint,K)

!    case( i_she )
!      call RIG_ELA_DKT(nb,coor_ele,K)
!    case( i_bar )
!      call RIG_ELA_BAR(nb,coor_ele,K)
!    case( i_dis )
!        call compute_elementary_bulk_discrete(nb,ppsnb,coor_ele,M)
    case default
      call faterr('a_mecaEF::compute_elementary_bulk2','unknown elementary model')
  endselect

end subroutine compute_elementary_bulk2

!> \brief Compute stress and internal variables of an element
subroutine compute_elementary_fields2(blmnb,ppsnb,dt,coor_ele,dep_ele,fields_old,fields_new,gauss_map,gauss_names)
  implicit none
  integer(kind=4),                   intent(in)  :: blmnb       !< [in] element model number
  integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb       !< [in] property set
  real(kind=8)                                   :: dt          !< [in] time step
  integer(kind=4),   dimension(:,:), intent(in)  :: gauss_map   !< [in] map to acces gauss point fields
  character(len=30), dimension(:)  , intent(in)  :: gauss_names !< [in] map to get gauss fields name
  real(kind=LONG),   dimension(:)  , intent(in)  :: fields_old  !< [in] fields values at gauss points
  real(kind=LONG),   dimension(:)  , intent(out) :: fields_new  !< [out] new fields values at gauss points
  real(kind=LONG),   dimension(:,:), intent(in)  :: coor_ele    !< [in] reference coordinates of the nodes
  real(kind=LONG),   dimension(:)  , intent(in)  :: dep_ele     !< [in] displacement of the nodes
  !
  integer(kind=4)   :: nb
  character(len=36) :: IAM
  !     123456789012345678901234567890123456
  IAM ='a_mecaEF::compute_elementary_fields2'


  nb=get_nb_in_mecaEFmodel(blmnb)

  select case (ele_is(blmnb))
  case( i_iso )
     !unplugged by fd call compute_elementary_fields_iso2(nb,ppsnb,dt,coor_ele,dep_ele,fields_old,fields_new,gauss_map,gauss_names)
!  case( i_bar )
!
!  case( i_she )
!
!  case( i_dis )
!     ! rien a faire
  case default
    call faterr(IAM,'unknown elementary model')
  end select

end subroutine compute_elementary_fields2

!> \brief interpolate a field values from gauss point to nodes
subroutine gpv2node2(blmID,mdlnb,field_index,fields,field_map,field_names,nodal_values,dime)
  implicit none
  integer(kind=4),                 intent(in)    :: blmID        !< [in] finite element type id
  integer(kind=4),                 intent(in)    :: mdlnb        !< [in] model id
  integer(kind=4),                 intent(in)    :: field_index  !< [in] index of the field to compute
  real(kind=8)   , dimension(:)  , intent(in)    :: fields       !< [in] fields values at gauss points
  integer(kind=4), dimension(:,:), intent(in)    :: field_map    !< [in] map to acces gauss point fields
  character(len=30), dimension(:), intent(in)    :: field_names  !< [in] map to get gauss fields name
  real(kind=8)   , dimension(:,:), intent(inout) :: nodal_values !< [out] values of the field computed on nodes 
  integer(kind=4),                 intent(in)    :: dime         !< [in] dimension
  !
  integer(kind=4)   :: nb
  character(len=19) :: IAM
!      1234567890123456789
  IAM='a_mecaEF::gpv2node2'


  nb=get_nb_in_mecaEFmodel(blmID)

  select case( ele_is(blmID) )
    case( i_iso )
      !unplugged by fd call gpv2node_iso2(nb,mdlnb,field_index,fields,field_map,field_names,nodal_values,dime)
!      case( i_she )
!
!      case( i_bar )
!
    case( i_dis )
      nodal_values = 0.d0
    case DEFAULT
      call faterr(IAM,'unknown elementary model')
  end select

end subroutine gpv2node2

!=======================================================================

subroutine get_ele_ptr_mecaEF(blmnb,coor_ele,primal_ele,dual_ele,operator_ele)
  implicit none 
  integer :: blmnb,nb
  real(kind=8), pointer :: coor_ele(:),primal_ele(:),dual_ele(:),operator_ele(:,:)

  character(len=28) :: IAM
  !      1234567890123456789012345678
  IAM = 'a_mecaEF::get_ele_ptr_mecaEF'


  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
    call get_ele_ptr_mecaEF_iso(nb,coor_ele,primal_ele,dual_ele,operator_ele)

  !CASE( i_dkt )

  !CASE( i_bar )
  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  CASE( i_joint )
    call get_ele_ptr_mecaEF_joint(nb,coor_ele,primal_ele,dual_ele,operator_ele)
  CASE DEFAULT
    call FATERR(IAM,'unknown model')
  END SELECT

end subroutine

subroutine get_nearest_gp_mecaEF(blmnb,coor_ele,coor,gpid)
  implicit none
  ! le numero de l'element dans la liste locale
  integer, intent(in) :: blmnb       
  ! coordonnees des sommets
  real(kind=LONG), dimension(:,:), intent(in)  :: coor_ele
  ! coordonnees du point
  real(kind=LONG), dimension(:)  , intent(out) :: coor
  ! gp id
  integer, intent(out) :: gpid

  !***
  integer                         :: nb
  character(len=31) :: IAM
  !      1234567890123456789012345678901
  IAM = 'a_mecaEF::get_nearest_gp_mecaEF'

  nb=get_nb_in_mecaEFmodel(blmnb)

  select case (ele_is(blmnb))
  case( i_iso )
    call get_nearest_gp_mecaEF_iso(nb,coor_ele,coor,gpid)

  !case( i_dkt )

  !case( i_bar )
  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  case( i_joint )
    call get_nearest_gp_mecaEF_joint(nb,coor_ele,coor,gpid)
  case default
    call FATERR(IAM,'unknown model')
  end select

end subroutine

subroutine check_elementary_ppset(blmnb,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  INTEGER              :: blmnb
  INTEGER              :: ibdyty,iblmty
  integer,dimension(:) :: ppsnb
  integer              :: nb
  !                         123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF::check_ppset'
  
  nb=get_nb_in_mecaEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     call check_elementary_ppset_iso(nb,ppsnb,ibdyty,iblmty)
  case(i_shb)
     call check_elementary_ppset_shb(nb,ppsnb,ibdyty,iblmty)     
  case(i_bar,i_she,i_dis)
  CASE( i_joint )
     call check_elementary_ppset_joint(nb,ppsnb,ibdyty,iblmty)
  case default
     call FATERR(IAM,'unknown elementary model')
  endselect
  
end subroutine check_elementary_ppset
  
END MODULE a_mecaEF
