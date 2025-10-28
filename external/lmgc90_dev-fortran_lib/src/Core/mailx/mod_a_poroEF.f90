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


!> generic API to poroelastic finite element 
MODULE a_poroEF

USE utilities

USE a_EF

USE a_poroEF_iso

IMPLICIT NONE

TYPE T_poroEF
  integer          :: ID      ! iso, bar, shell
  integer          :: nb      ! number of ID
  CHARACTER(len=5) :: NAME    ! nickname DKTxx,...
END TYPE T_poroEF

INTEGER :: nb_poroEF=0
TYPE(T_poroEF),DIMENSION(:),ALLOCATABLE :: poroEF

CONTAINS

SUBROUTINE init_poroEF
  IMPLICIT NONE

  INTEGER :: i,j,errare
!                           12345678901234567890123
  CHARACTER(len=23) :: IAM='a_poroEF::init_poroEF'
!
  !rm : for reload...
  if( nb_poroEF > 0 ) return

! initialisation des types
! 
  CALL init_poroEF_iso
!
! construction de la table de hachage 
!
  nb_poroEF=nb_poroEF+get_nb_ele_iso()

  ALLOCATE(poroEF(nb_poroEF),stat=errare)
  IF (errare /= 0) THEN
    CALL FATERR(IAM,'error allocating porom_ef%point_gauss')
  END IF
!
  j=0
!
  DO i=1,get_nb_ele_iso()
    poroEF(i+j)%ID= i_iso
    poroEF(i+j)%nb=i
    poroEF(i+j)%NAME=get_NAME_poroEF_iso(i)
  ENDDO
  j=j+get_nb_ele_iso()
  
END SUBROUTINE init_poroEF
!=======================================================================
INTEGER FUNCTION get_nb_in_poroEF(NAME)
!
! fonction qui retourne l'indice dans le tableau poroEF 
! de l'element de nom NAME 
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i
!                            1234567890123456789012345678
  CHARACTER(len=28)  :: IAM='a_poroEF::get_nb_in_poroEF'
  CHARACTER(len=30)  :: cout

  DO i=1,SIZE(poroEF)
 
   IF (poroEF(i)%NAME == NAME) THEN
     get_nb_in_poroEF=i
     RETURN
   ENDIF

  ENDDO
                           !12345678        12345678
  WRITE(cout,'(A8,A5,A8)') 'poroEF ',NAME,' unknown'
  CALL FATERR(IAM,cout)


END FUNCTION get_nb_in_poroEF
!=======================================================================
INTEGER FUNCTION get_nb_in_poroEFmodel(i)
!
! fonction qui retourne l'indice dans le tableau poroEFmodel 
! de l'element d'indice i 
!
  IMPLICIT NONE
  INTEGER          :: i
!                           123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_poroEF::get_nb_in_poroEFmodel'
  CHARACTER(len=30) :: cout
!
  get_nb_in_poroEFmodel=poroEF(i)%nb
  RETURN

!                            !1234567        12345678
!  write(cout,'(A7,A5,A8)') 'poroEF ',NAME,' unknown'
!  call FATERR(IAM,cout)
!  stop

END FUNCTION get_nb_in_poroEFmodel
!=======================================================================
integer function ele_is(nb)
!
! fonction qui retourne l'ID (iso/bar/shell) de l'element 
! d'indice nb dans poroEF
!
  implicit none
  integer, intent(in) :: nb

  ele_is = poroEF(nb)%ID
!
end function ele_is
!=======================================================================
INTEGER FUNCTION get_bw_poroEF(i,nodes)
!
! fonction qui retourne la largueur de bande de l'element
! d'indice i dans poroEF
!
  IMPLICIT NONE
  INTEGER :: nb,i
  INTEGER,DIMENSION(:) :: nodes
!
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_bw_poroEF=get_bw_poroEF_iso(nb,nodes)
  END SELECT

!!  print*,'element de type: ',ele_is(i)
!!  print*,'numero dans le type:',nb
!!  print*,'connectivite   : ',nodes
!!  print*,'largeur de bande:',get_bw_poroEF

END FUNCTION get_bw_poroEF

!=================================================================
!============= Calcul des matrices elementaires ==================
!=================================================================

!============ compute elementary mass matrix ===

SUBROUTINE compute_elementary_mass(blmnb,ppsnb,dt,coor_ele,U_ele,P_ele,ibdyty,iblmty,M)
!
! fonction qui calcule la matrice de rigidite elementaire d'un
! element ayant l'indice blmnb dans mecaEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,ibdyty,iblmty
  integer,dimension(:) :: ppsnb
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,U_ele
  REAL(kind=8),DIMENSION(:)   :: P_ele
  REAL(kind=8),DIMENSION(:,:) :: M
  REAL(kind=8)                :: dt

  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
      CALL compute_elementary_mass_ISO(nb,ppsnb,dt,coor_ele,U_ele,P_ele,ibdyty,iblmty,M)
  END SELECT

END SUBROUTINE compute_elementary_mass

!============ compute elementary capacity matrix ===


SUBROUTINE compute_elementary_damping(blmnb,ppsnb,dt,coor_ele,U_ele,V_ele,P_ele,ibdyty,iblmty,C)

! fonction qui calcule la matrice de rigidite elementaire d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE

  INTEGER                     :: blmnb,nb
  INTEGER                     :: ibdyty,iblmty
  real(kind=8)                :: dt

  REAL(kind=8),DIMENSION(:,:) :: coor_ele,U_ele,V_ele
  REAL(kind=8),DIMENSION(:)   :: P_ele
  REAL(kind=8),DIMENSION(:,:) :: C
  integer,dimension(:) :: ppsnb
  
  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL compute_elementary_damping_ISO(nb,ppsnb,dt,coor_ele,U_ele,V_ele,P_ele,ibdyty,iblmty,C)
  ENDSELECT

END SUBROUTINE compute_elementary_damping

!============ compute elementary stiffness matrix ===

SUBROUTINE compute_elementary_bulk(blmnb,ppsnb,dt,coor_ele,U_ele,ibdyty,iblmty,Fint,K)       
!
! fonction qui calcule la matrice de rigidite elementaire et le second membre d'un
! element ayant l'indice blmnb dans poroEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  real(kind=8)                :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,U_ele
  REAL(kind=8),DIMENSION(:)   :: Fint
  REAL(kind=8),DIMENSION(:,:) :: K

  integer,dimension(:) :: ppsnb

  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
       CALL compute_elementary_bulk_ISO(nb,ppsnb,dt,coor_ele,U_ele, &
                                        ibdyty,iblmty,Fint,K)

   endselect

END SUBROUTINE compute_elementary_bulk

!===================================================
!============= low level routines ==================
!===================================================
INTEGER FUNCTION get_N_NODE_THER_poroEF(NAME)
!
! fonction qui retourne le nombre de noeud 
! de l'element de nom NAME
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_NODE_THER_poroEF=get_N_NODE_THER_poroEF_iso(nb)
  END SELECT

END FUNCTION get_N_NODE_THER_poroEF
!===================================================
INTEGER FUNCTION get_N_NODE_MECA_poroEF(NAME)
!
! fonction qui retourne le nombre de noeud 
! de l'element de nom NAME
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_NODE_MECA_poroEF=get_N_NODE_MECA_poroEF_iso(nb)
  END SELECT

END FUNCTION get_N_NODE_MECA_poroEF
!===================================================
INTEGER FUNCTION get_N_NODE_poroEF(NAME)
!
! fonction qui retourne le nombre de noeud 
! de l'element de nom NAME
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_NODE_poroEF=get_N_NODE_poroEF_iso(nb)
  END SELECT

END FUNCTION get_N_NODE_poroEF
!=======================================================================
INTEGER FUNCTION get_N_DOF_by_NODE_THER_poroEF(NAME)

! fonction qui retourne le nombre de points de gauss
! de l'element de nom NAME

  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_DOF_by_NODE_THER_poroEF=get_N_DOF_by_NODE_THER_poroEF_iso(nb)
  END SELECT

END FUNCTION get_N_DOF_by_NODE_THER_poroEF
!=======================================================================
INTEGER FUNCTION get_N_DOF_by_NODE_MECA_poroEF(NAME)

! fonction qui retourne le nombre de points de gauss
! de l'element de nom NAME

  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_DOF_by_NODE_MECA_poroEF=get_N_DOF_by_NODE_MECA_poroEF_iso(nb)
  END SELECT

END FUNCTION get_N_DOF_by_NODE_MECA_poroEF
!=======================================================================
INTEGER FUNCTION get_N_DOF_by_NODE_poroEF(NAME, i_node)

! fonction qui retourne le nombre de points de gauss
! de l'element de nom NAME

  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb,i_node

  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_DOF_by_NODE_poroEF=get_N_DOF_by_NODE_poroEF_iso(nb, i_node)
  END SELECT

END FUNCTION get_N_DOF_by_NODE_poroEF
!=======================================================================
INTEGER FUNCTION get_N_GP_poroEF(NAME, phys)

! pour retrouver le nb de pg  et le nb de val par pg

  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb
  CHARACTER(len=4)             :: phys          ! type de physique demande
  
  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_GP_poroEF=get_N_GP_poroEF_iso(nb, phys)
   CASE default
      get_N_GP_poroEF=0
      PRINT*,'get_N_GP_poroEF non implemante pour ce type d element'
  END SELECT

END FUNCTION get_N_GP_poroEF

!=======================================================================
INTEGER FUNCTION get_MECA_to_PORO(NAME, i_dof)

! pour retrouver le dof poro a partir du dof mec

  IMPLICIT NONE
  INTEGER          :: i,i_dof,nb,blmnb
  CHARACTER(len=5) :: NAME
  
  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_MECA_to_PORO=get_MECA_to_PORO_iso(nb, i_dof)
   CASE default
      get_MECA_to_PORO=0
      PRINT*,'get_MECA_to_PORO non implemante pour ce type d element'
  END SELECT

END FUNCTION get_MECA_to_PORO

!=======================================================================
INTEGER FUNCTION get_THER_to_PORO(NAME, i_dof)

! pour retrouver le dof poro a partir du dof mec

  IMPLICIT NONE
  INTEGER          :: i,i_dof,nb,blmnb
  CHARACTER(len=5) :: NAME
  
  i=get_nb_in_poroEF(NAME)  
  nb=get_nb_in_poroEFmodel(i)
  
  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_THER_to_PORO=get_THER_to_PORO_iso(nb, i_dof)
   CASE default
      get_THER_to_PORO=0
      PRINT*,'get_THER_to_PORO non implemante pour ce type d element'
  END SELECT

END FUNCTION get_THER_to_PORO
!******************************************************************************
subroutine get_coor_pg(blmnb,coor_ele,coor_pg,phys)
!
! fonction qui calcule les coordonnees des points de gauss 
! d'element ayant l'indice blmnb dans mecaEF, 
! de property set  ppsnb
! et de position coor_ele
!  
  implicit none
  integer :: blmnb,nb
  real(kind=8),dimension(:,:) :: coor_ele
  real(kind=8),dimension(:,:) :: coor_pg
  CHARACTER(len=4)             :: phys          ! type de physique demande

  nb=get_nb_in_poroEFmodel(blmnb)

  select case (ele_is(blmnb))
    case( i_iso )
!     mdlnb = ppset(ppsnb)%mdlnb 
!     lawnb = ppset(ppsnb)%lawnb
     call   get_coor_pg_ISO(nb,coor_ele,coor_pg,phys)
!    case('shell')  
!    case('bar  ')  
  endselect

end subroutine get_coor_pg
!******************************************************************************

!******************************************************************************
subroutine interpolate_node2pg(blmnb,valnoe,valpg,phys)
!
! fonction qui calcule les valeurs au pg en partant des valeurs aux noeuds
!  
  implicit none
  integer :: blmnb,nb
  real(kind=8),dimension(:) :: valnoe
  real(kind=8),dimension(:) :: valpg
  CHARACTER(len=4)             :: phys          ! type de physique demande
  
  nb=get_nb_in_poroEFmodel(blmnb)

  select case (ele_is(blmnb))
    case( i_iso )
!     mdlnb = ppset(ppsnb)%mdlnb 
!     lawnb = ppset(ppsnb)%lawnb
     call   interpolate_node2pg_ISO(nb,valnoe,valpg,phys)
!    case('shell')  
!    case('bar  ')  
  endselect

end subroutine interpolate_node2pg
!******************************************************************************
SUBROUTINE compute_elementary_fields(blmnb,ppsnb,dt,coor_ele,dep_ele,vit_ele,P_ele,ibdyty,iblmty)       
!
! fonction qui calcule contraintes et variables internes d'un
! element ayant l'indice blmnb dans poroEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  real(kind=8) :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,dep_ele,vit_ele
  REAL(kind=8),DIMENSION(:)   :: P_ele

  integer,dimension(:) :: ppsnb

  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )

       CALL compute_elementary_fields_ISO (nb,ppsnb,dt,coor_ele,dep_ele,vit_ele,P_ele,ibdyty,iblmty)

!    case('shell')  
!
!    case('bar  ')  
!
  endselect

END SUBROUTINE compute_elementary_fields

SUBROUTINE gpv2node_2D(blmnb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)       
!
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,required_field,nbfields,nbnodes_stored,mdlnb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: nodalvalues

!                           123456789012345678901
  CHARACTER(len=21) :: IAM='a_poroEF::gpv2node_2D'

  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     CALL gpv2node_2D_iso(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        
!    case('shell')  
!
!    case('bar  ')  
!
  case ( i_dis )
     call FATERR(IAM,'impossible for this mecaEF model')
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE gpv2node_2D

SUBROUTINE gpv2node_3D(blmnb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)       
!
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,required_field,nbfields,nbnodes_stored,mdlnb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: nodalvalues

!                           123456789012345678901
  CHARACTER(len=21) :: IAM='a_poroEF::gpv2node_3D'


  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL gpv2node_3D_iso(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        
!    case('shell')  
!
!    case('bar  ')  
!
  case ( i_dis )
     NodalValues = 0.d0
     NbNodes_stored = 0
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE gpv2node_3D

SUBROUTINE gpv2nodeP_3D(blmnb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)       
!
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,required_field,nbfields,nbnodes_stored,mdlnb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: nodalvalues

!                           1234567890123456789012
  CHARACTER(len=22) :: IAM='a_poroEF::gpv2nodeP_3D'


  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
      
      CALL gpv2nodeP_3D_iso(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        
!    case('shell')  
!
!    case('bar  ')  
    case ( i_dis )
     NodalValues = 0.d0
     NbNodes_stored = 0
    CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE gpv2nodeP_3D

!=======================================================================
INTEGER FUNCTION get_local_connectivity_edge_poroEF(i,nodes,num)
!
! fonction qui retourne le noeud de la edge
!
  IMPLICIT NONE
  INTEGER :: num,i,nodes,nb
!
  nb=get_nb_in_poroEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_local_connectivity_edge_poroEF = get_local_connectivity_edge_poroEF_iso(nb,nodes,num)
  END SELECT

END FUNCTION get_local_connectivity_edge_poroEF

!============ compute elementary div(Vecteur) matrix ===

SUBROUTINE add_elementary_field_load(blmnb,ppsnb,dt,coor_ele,U_ele,Vec_ele,ibdyty,iblmty,Fint, ideriv)       
!
! fonction qui ajoute une charge locale ou une derive de cette charge
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty,ideriv
  real(kind=8)                :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,U_ele,Vec_ele
  REAL(kind=8),DIMENSION(:)   :: Fint

  integer,dimension(:) :: ppsnb

  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
       CALL add_elementary_load_ISO(nb,ppsnb,dt,coor_ele,U_ele,Vec_ele,ibdyty,iblmty,Fint, ideriv)

   endselect

END SUBROUTINE add_elementary_field_load

subroutine check_elementary_ppset(blmnb,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  INTEGER              :: blmnb
  INTEGER              :: ibdyty,iblmty
  integer,dimension(:) :: ppsnb
  integer              :: nb
  !                         123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF::check_ppset'
  
  nb=get_nb_in_poroEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     call check_elementary_ppset_iso(nb,ppsnb,ibdyty,iblmty)
  case default
     call FATERR(IAM,'unknown elementary model')
  endselect
  
end subroutine check_elementary_ppset


END MODULE a_poroEF
