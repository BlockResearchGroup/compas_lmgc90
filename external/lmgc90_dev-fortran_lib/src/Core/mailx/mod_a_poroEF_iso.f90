
!todo

! redescendre la gestion des valeurs au pg ici ou dans models ?

! clarifier ce qui est gere par ce module (interpolation, integration) et 
! par models (calculs en 1 pg)
! est il logique d'avoir le test isext ici ? 
!  => ca devrait plutot se trouver dans models

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
MODULE a_poroEF_iso
! class Finite Element for mecanical problems
! Basic computations on Finite Elements
! this class defines data type and methods and type
!
! DOES NOT CONTAIN ANY DATA

use parameters

USE utilities
USE algebra
USE a_MATRIX

USE a_EF

USE a_mecaEF_iso, ONLY : init_mecaEF_iso, &
                         get_T_FONC_FORME_mecaEF, &
                         get_id_mecaef_iso, &
                         get_N_GP_mecaEF_iso, &
                         get_SCH_GAUSS_RIG_mecaEF, &
                         get_N_PG_MAS_mecaEF, &
                         get_N_PG_RIG_mecaEF, &
                         get_N_NODE_mecaEF_iso, & 
                         get_N_DOF_by_NODE_mecaEF_iso, &
                         get_bw_mecaEF_iso, &
                         get_gp_ptr_mecaEF_iso, &
                         get_coor_pg_ISO_meca => get_coor_pg_ISO, &
                         GRADIENT_ISO_MECA    => GRADIENT_ISO, &
                         Bl_ISO_MECA          => Bl_ISO, &
                         B_ISO_MECA           => B_ISO, &
                         Id_ISO_MECA          => Id_ISO, &
                         interpolate_node2pg_ISO_MECA => interpolate_node2pg_ISO, &
                         MASS_ISO_MECA        => compute_elementary_mass_iso, &
                         BULK_GD_ISO_MECA   => BULK_GD_ISO, &
                         BULK_HPP_ISO_MECA   => BULK_HPP_ISO ,&
                         gpv2node_3D_iso_meca => gpv2node_3D_iso ,&
                         gpv2node_2D_iso_meca => gpv2node_2D_iso, &
                         fields_hpp_iso_meca => fields_hpp_iso, &
                         fields_gd_iso_meca =>fields_gd_iso
                         
                                                  
USE a_therEF_iso, ONLY : init_therEF_iso, &
                         get_T_FONC_FORME_therEF_iso, &
                         get_id_theref_iso, &
                         get_N_GP_therEF_iso, &
                         get_SCH_GAUSS_therEF, &
                         get_N_DOF_by_NODE_therEF_iso, &
                         get_N_NODE_therEF_iso, &
                         get_bw_therEF_iso, &
                         get_gp_ptr_therEF_iso, &
                         get_coor_pg_ISO_THER => get_coor_pg_ISO, &
                         GRADIENT_ISO_THER    => GRADIENT_ISO, &
                         Bl_ISO_THER          => Bl_ISO, &
                         interpolate_node2pg_ISO_THER => interpolate_node2pg_ISO, &
                         CAPACITY_ISO_THER        => CAPACITY_ISO, &
                         CONDUCTIVITY_ISO_THER => CONDUCTIVITY_ISO ,&
                         CONDUCTIVITY_ISO_GD_THER => CONDUCTIVITY_GD_ISO, &
                         MEAN_PRESSURE_ISO_THER => MEAN_PRESSURE_ISO, &
                         gpv2node_iso_ther => gpv2node_iso ,&
                         fields_hpp_iso_ther => fields_hpp_iso ,&
                         fields_gd_iso_ther => fields_gd_iso

USE bulk_behaviour

USE models

USE ExternalModelsHandler

USE MAILx

USE user


IMPLICIT NONE

TYPE T_poroEF_iso
   PRIVATE
   CHARACTER(len=5)          :: NAME
   CHARACTER(len=5)          :: mecaNAME
   CHARACTER(len=5)          :: therNAME
   INTEGER                   :: mecaID
   INTEGER                   :: therID
   INTEGER                   :: nbr_node
   INTEGER, ALLOCATABLE      :: dof_by_node(:)
   INTEGER, ALLOCATABLE      :: meca_2_poro(:)
   INTEGER, ALLOCATABLE      :: ther_2_poro(:)
   INTEGER                   :: meca_DOF
   INTEGER                   :: ther_DOF
   TYPE(T_PT_GAUSS), POINTER :: PG_THER(:)
   TYPE(T_PT_GAUSS), POINTER :: PG_MECA(:)
   TYPE(T_PT_GAUSS), POINTER :: PG_THER_ON_MECA(:)
   INTEGER, ALLOCATABLE      :: edge_2_vertex(:,:)
   
END TYPE T_poroEF_iso 

TYPE(T_poroEF_iso),DIMENSION(8),PRIVATE :: poroEF


CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_iso(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_iso=SIZE(poroEF)

END FUNCTION get_nb_ele_iso

SUBROUTINE init_poroEF_iso
  IMPLICIT NONE
  logical :: is_initialize = .false.
  INTEGER :: itempo,i,j,k,p,errare,n_dof_meca, n_dof_ther,n_node,n_node_meca,n_node_ther
  INTEGER :: mm,tt
  REAL(kind=long),POINTER :: CG(:,:),POIDS_ELE(:)
  !                         12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_poroEF_iso::init_poroEF_iso'
  
  if( is_initialize ) return

  ! Initilisation des element de type MECA et THER  
  CALL init_mecaEF_iso
  CALL init_therEF_iso

  ! Declaration des elements finis
  ! EF 2D
  !
  ! T3
  poroEF(6)%NAME          ='T33xx'
  poroEF(6)%mecaNAME      ='T3xxx'
  poroEF(6)%therNAME      ='T3xxx'
  poroEF(6)%nbr_node      = 3
  CALL get_id_mecaEF_iso(poroEF(6)%mecaNAME, poroEF(6)%mecaID)
  CALL get_id_therEF_iso(poroEF(6)%therNAME, poroEF(6)%therID)
  ALLOCATE(poroEF(6)%edge_2_vertex(3,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for T33xx')
  END IF
  poroEF(6)%edge_2_vertex = 0

  ! Q4
  poroEF(5)%NAME          ='Q44xx'
  poroEF(5)%mecaNAME      ='Q4xxx'
  poroEF(5)%therNAME      ='Q4xxx'
  poroEF(5)%nbr_node      = 4
  CALL get_id_mecaEF_iso(poroEF(5)%mecaNAME, poroEF(5)%mecaID)
  CALL get_id_therEF_iso(poroEF(5)%therNAME, poroEF(5)%therID)
  ALLOCATE(poroEF(5)%edge_2_vertex(4,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for Q44xx')
  END IF
  poroEF(5)%edge_2_vertex = 0
  
  ! Q8
  poroEF(1)%NAME          ='Q84xx'
  poroEF(1)%mecaNAME      ='Q8xxx'
  poroEF(1)%therNAME      ='Q4xxx'
  poroEF(1)%nbr_node      = 8
  CALL get_id_mecaEF_iso(poroEF(1)%mecaNAME, poroEF(1)%mecaID)
  CALL get_id_therEF_iso(poroEF(1)%therNAME, poroEF(1)%therID)
  ALLOCATE(poroEF(1)%edge_2_vertex(4,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for Q84xx')
  END IF
  poroEF(1)%edge_2_vertex = 0
  poroEF(1)%edge_2_vertex(1,1) = 1
  poroEF(1)%edge_2_vertex(1,2) = 2
  poroEF(1)%edge_2_vertex(2,1) = 2
  poroEF(1)%edge_2_vertex(2,2) = 3
  poroEF(1)%edge_2_vertex(3,1) = 3
  poroEF(1)%edge_2_vertex(3,2) = 4
  poroEF(1)%edge_2_vertex(4,1) = 4
  poroEF(1)%edge_2_vertex(4,2) = 1
  
  ! T6
  poroEF(2)%NAME          ='T63xx'
  poroEF(2)%mecaNAME      ='T6xxx'
  poroEF(2)%therNAME      ='T3xxx'
  poroEF(2)%nbr_node      = 6
  CALL get_id_mecaEF_iso(poroEF(2)%mecaNAME, poroEF(2)%mecaID)
  CALL get_id_therEF_iso(poroEF(2)%therNAME, poroEF(2)%therID)
  ALLOCATE(poroEF(2)%edge_2_vertex(3,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for T63xx')
  END IF
  poroEF(2)%edge_2_vertex = 0
  poroEF(2)%edge_2_vertex(1,1) = 1
  poroEF(2)%edge_2_vertex(1,2) = 2
  poroEF(2)%edge_2_vertex(2,1) = 2
  poroEF(2)%edge_2_vertex(2,2) = 3
  poroEF(2)%edge_2_vertex(3,1) = 3
  poroEF(2)%edge_2_vertex(3,2) = 1
  
  ! EF 3D

  ! H20
  poroEF(3)%NAME          ='H208x'
  poroEF(3)%mecaNAME      ='H20xx'
  poroEF(3)%therNAME      ='H8xxx'
  poroEF(3)%nbr_node      = 20
  CALL get_id_mecaEF_iso(poroEF(3)%mecaNAME, poroEF(3)%mecaID)
  CALL get_id_therEF_iso(poroEF(3)%therNAME, poroEF(3)%therID)
  ALLOCATE(poroEF(3)%edge_2_vertex(12,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for H208x')
  END IF
  poroEF(3)%edge_2_vertex = 0
  poroEF(3)%edge_2_vertex(1,1) = 1
  poroEF(3)%edge_2_vertex(1,2) = 2
  poroEF(3)%edge_2_vertex(2,1) = 2
  poroEF(3)%edge_2_vertex(2,2) = 3
  poroEF(3)%edge_2_vertex(3,1) = 3
  poroEF(3)%edge_2_vertex(3,2) = 4
  poroEF(3)%edge_2_vertex(4,1) = 4
  poroEF(3)%edge_2_vertex(4,2) = 1
  
  poroEF(3)%edge_2_vertex(5,1) = 5
  poroEF(3)%edge_2_vertex(5,2) = 6
  poroEF(3)%edge_2_vertex(6,1) = 6
  poroEF(3)%edge_2_vertex(6,2) = 7
  poroEF(3)%edge_2_vertex(7,1) = 7
  poroEF(3)%edge_2_vertex(7,2) = 8
  poroEF(3)%edge_2_vertex(8,1) = 8
  poroEF(3)%edge_2_vertex(8,2) = 5
  
  poroEF(3)%edge_2_vertex(9,1) = 1
  poroEF(3)%edge_2_vertex(9,2) = 5
  poroEF(3)%edge_2_vertex(10,1) = 2
  poroEF(3)%edge_2_vertex(10,2) = 6
  poroEF(3)%edge_2_vertex(11,1) = 3
  poroEF(3)%edge_2_vertex(11,2) = 7
  poroEF(3)%edge_2_vertex(12,1) = 4
  poroEF(3)%edge_2_vertex(12,2) = 8  
  
  ! H8
  poroEF(8)%NAME          ='H88xx'
  poroEF(8)%mecaNAME      ='H8xxx'
  poroEF(8)%therNAME      ='H8xxx'
  poroEF(8)%nbr_node      = 8
  CALL get_id_mecaEF_iso(poroEF(8)%mecaNAME, poroEF(8)%mecaID)
  CALL get_id_therEF_iso(poroEF(8)%therNAME, poroEF(8)%therID)
  ALLOCATE(poroEF(8)%edge_2_vertex(8,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for H88xx')
  END IF
  poroEF(8)%edge_2_vertex = 0
  
  ! TE10
  poroEF(4)%NAME          ='TE104'
  poroEF(4)%mecaNAME      ='TE10x'
  poroEF(4)%therNAME      ='TE4xx'
  poroEF(4)%nbr_node      = 10
  CALL get_id_mecaEF_iso(poroEF(4)%mecaNAME, poroEF(4)%mecaID)
  CALL get_id_therEF_iso(poroEF(4)%therNAME, poroEF(4)%therID)
  ALLOCATE(poroEF(4)%edge_2_vertex(6,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for TE104')
  END IF
  poroEF(4)%edge_2_vertex(1,1) = 1
  poroEF(4)%edge_2_vertex(1,2) = 2
  poroEF(4)%edge_2_vertex(2,1) = 2
  poroEF(4)%edge_2_vertex(2,2) = 3
  poroEF(4)%edge_2_vertex(3,1) = 3
  poroEF(4)%edge_2_vertex(3,2) = 1
  poroEF(4)%edge_2_vertex(4,1) = 1
  poroEF(4)%edge_2_vertex(4,2) = 4
  poroEF(4)%edge_2_vertex(5,1) = 2
  poroEF(4)%edge_2_vertex(5,2) = 4
  poroEF(4)%edge_2_vertex(6,1) = 3
  poroEF(4)%edge_2_vertex(6,2) = 4
  
  ! TE4
  poroEF(7)%NAME          ='TE44x'
  poroEF(7)%mecaNAME      ='TE4xx'
  poroEF(7)%therNAME      ='TE4xx'
  poroEF(7)%nbr_node      = 4
  CALL get_id_mecaEF_iso(poroEF(7)%mecaNAME, poroEF(7)%mecaID)
  CALL get_id_therEF_iso(poroEF(7)%therNAME, poroEF(7)%therID)
  ALLOCATE(poroEF(7)%edge_2_vertex(4,2),stat=errare)
  IF (errare/=0) THEN
    CALL FATERR(IAM,'error allocating edge_2_vertex for TE44x')
  END IF
  poroEF(7)%edge_2_vertex = 0
  
  ! Fin de Declaration des elements finis
  NULLIFY(CG,POIDS_ELE)

  DO i=1,SIZE(poroEF)
      ! Allocation des tables index
      n_dof_meca = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
      n_dof_ther = get_N_DOF_by_NODE_THER_poroEF_iso(i)
      n_node_meca = get_N_NODE_MECA_poroEF_iso(i)
      n_node_ther = get_N_NODE_THER_poroEF_iso(i)
      
      ALLOCATE(poroEF(i)%meca_2_poro(n_dof_meca*n_node_meca))
      ALLOCATE(poroEF(i)%ther_2_poro(n_dof_ther*n_node_ther))
      ALLOCATE(poroEF(i)%dof_by_node(poroEF(i)%nbr_node))
      ! Creation de la table de dof
      DO j = 1,poroEF(i)%nbr_node
         IF (j <= n_node_ther) THEN
            poroEF(i)%dof_by_node(j) = n_dof_meca + n_dof_ther
         ELSE
            poroEF(i)%dof_by_node(j) = n_dof_meca
         ENDIF
      ENDDO
      !Creation des tables d index
      p = 0
      mm = 0
      tt = 0
      DO j = 1,poroEF(i)%nbr_node
         ! Boucle sur les noeuds
         IF (poroEF(i)%dof_by_node(j) == n_dof_meca) THEN
             ! Seulement des dof de meca
             DO k = 1,n_dof_meca
                p = p + 1
                mm = mm + 1
                poroEF(i)%meca_2_poro(mm) = p
             ENDDO
         ENDIF
         IF (poroEF(i)%dof_by_node(j) == n_dof_meca + n_dof_ther) THEN
             ! dof de meca et dof de ther
             DO k = 1,n_dof_meca
                p = p + 1
                mm = mm + 1
                poroEF(i)%meca_2_poro(mm) = p
             ENDDO
             DO k = 1,n_dof_ther
                p = p + 1
                tt = tt + 1
                poroEF(i)%ther_2_poro(tt) = p
             ENDDO
         ENDIF
      ENDDO
      
      !Creation des pinteurs vers les points de gauss de l'element
     
      ALLOCATE(poroEF(i)%PG_MECA(get_N_GP_poroEF_iso(i, 'MECA')),stat=errare)
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating poro_ef%PG_MECA')
      END IF
      
      ALLOCATE(poroEF(i)%PG_THER(get_N_GP_poroEF_iso(i, 'THER')),stat=errare)
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating poro_ef%PG_THER')
      END IF
      
      ALLOCATE(poroEF(i)%PG_THER_ON_MECA(get_N_GP_poroEF_iso(i, 'MECA')),stat=errare)
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating poro_ef%PG_MECA')
      END IF
      
      CALL pos_gauss(get_SCH_GAUSS_poroEF(i,'MECA'),CG,POIDS_ELE)
      
      ! Pointeur du champ de thermique
      DO j=1,get_N_GP_poroEF_iso(i, 'THER')

         poroEF(i)%PG_THER(j) = get_gp_ptr_therEF_iso(poroEF(i)%therID,j)

      ENDDO
      ! Pointeur du champ de meca
      
      !print *,'CG : ',CG
      !print *,'Poids : ',POIDS_ELE
      
      DO j=1,get_N_GP_poroEF_iso(i, 'MECA')

         poroEF(i)%PG_MECA(j) = get_gp_ptr_mecaEF_iso(poroEF(i)%mecaID,j)
         
         
         poroEF(i)%PG_THER_ON_MECA(j)%POIDS=POIDS_ELE(j)   
         
         NULLIFY(poroEF(i)%PG_THER_ON_MECA(j)%N)
         CALL fonct_forme(get_T_FONC_FORME_poroEF(i,'THER'),CG(:,j),poroEF(i)%PG_THER_ON_MECA(j)%N)
         
         NULLIFY(poroEF(i)%PG_THER_ON_MECA(j)%DN)
         CALL derive_forme(get_T_FONC_FORME_poroEF(i,'THER'),CG(:,j),poroEF(i)%PG_THER_ON_MECA(j)%DN)

      ENDDO
      
  ENDDO

  if( associated(CG) ) deallocate(CG)
  if( associated(POIDS_ELE) ) deallocate(POIDS_ELE)

  is_initialize = .true.

END SUBROUTINE init_poroEF_iso

CHARACTER(len=5) FUNCTION get_NAME_poroEF_iso(nb)
  IMPLICIT NONE
  INTEGER :: nb
  
  get_NAME_poroEF_iso = poroEF(nb)%NAME

END FUNCTION get_NAME_poroEF_iso

INTEGER FUNCTION get_N_NODE_poroEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_poroEF_iso = get_N_NODE_MECA_poroEF_iso(nb)
 

END FUNCTION get_N_NODE_poroEF_iso

INTEGER FUNCTION get_N_NODE_MECA_poroEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_MECA_poroEF_iso = get_N_NODE_mecaEF_iso(poroEF(nb)%mecaID)

END FUNCTION get_N_NODE_MECA_poroEF_iso

INTEGER FUNCTION get_N_NODE_THER_poroEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_THER_poroEF_iso = get_N_NODE_therEF_iso(poroEF(nb)%therID)

END FUNCTION get_N_NODE_THER_poroEF_iso

INTEGER FUNCTION get_N_DOF_by_NODE_poroEF_iso(nb, i_node)
  IMPLICIT NONE
  INTEGER          :: nb, i_node
    
    get_N_DOF_by_NODE_poroEF_iso = poroEF(nb)%dof_by_node(i_node)

END FUNCTION get_N_DOF_by_NODE_poroEF_iso

INTEGER FUNCTION get_N_DOF_by_NODE_MECA_poroEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_MECA_poroEF_iso = get_N_DOF_by_NODE_mecaEF_iso(poroEF(nb)%mecaID)

END FUNCTION get_N_DOF_by_NODE_MECA_poroEF_iso

INTEGER FUNCTION get_N_DOF_by_NODE_THER_poroEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_THER_poroEF_iso = get_N_DOF_by_NODE_therEF_iso(poroEF(nb)%therID)

END FUNCTION get_N_DOF_by_NODE_THER_poroEF_iso

INTEGER FUNCTION get_N_GP_poroEF_iso(nb, phys)
  IMPLICIT NONE
  INTEGER          :: nb
  CHARACTER(len=4) :: phys

  SELECT CASE(phys)
    CASE('MECA')
         get_N_GP_poroEF_iso = get_N_PG_RIG_mecaEF(poroEF(nb)%mecaID)
    CASE('THER')
         get_N_GP_poroEF_iso = get_N_GP_therEF_iso(poroEF(nb)%therID)
    CASE DEFAULT
         call faterr('mod_a_poroEF_iso::get_N_GP','Mauvaise physique demandee '//phys)
  END SELECT

END FUNCTION get_N_GP_poroEF_iso

!=========================================================
INTEGER FUNCTION get_MECA_to_PORO_iso(nb, i_dof)
  IMPLICIT NONE
  INTEGER          :: nb, i_dof
 
 get_MECA_to_PORO_iso = poroEF(nb)%meca_2_poro(i_dof)

END FUNCTION get_MECA_to_PORO_iso

!=========================================================
INTEGER FUNCTION get_THER_to_PORO_iso(nb, i_dof)
  IMPLICIT NONE
  INTEGER          :: nb, i_dof

 get_THER_to_PORO_iso = poroEF(nb)%ther_2_poro(i_dof)

END FUNCTION get_THER_to_PORO_iso
!=========================================================

SUBROUTINE N_P_ISO(N_NE,N,B)

! computes the N matrix (fonct form of pressure)  at a gauss point

!------------------------------------------------------------------------------!
!                                                                              !
!   B  est de la forme                                                         !
!                                                                              !
!  EN 2D : BI  = [     N1        N2  ...                                       !
!  EN AXI              N1        N2  ...  ]                                    !
!                                                                              !
!  EN 3D : BlI = [     N1        N2  ...                                       !
!                      N1        N2  ...                                       !
!                      N1        N2  ...  ]                                    !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF

SELECT CASE(DIME_mod)

 CASE(i_2D_strain)
   ALLOCATE(B(2,N_NE))
   DO I=1,N_NE
      B(1,I) = N(I)
      B(2,I) = N(I)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(2,N_NE))
   DO I=1,N_NE
      B(1,I) = N(I)
      B(2,I) = N(I)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(3,N_NE))
   DO I=1,N_NE
      B(1,I) = N(I)
      B(2,I) = N(I)
      B(3,I) = N(I)
   ENDDO
 CASE DEFAULT
   call faterr('a_poroEF_iso::N_P_ISO','dime not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE N_P_ISO
!=========================================================

SUBROUTINE N_U_ISO(N_NE,N,B)

! computes the N matrix (fonct form of pressure)  at a gauss point

!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BI  = [     NI        0                                             !
!  EN AXY               0        NI    ]                                       !
!                                                                              !
!  EN 3D : BlI = [     NI         0          0                                 !
!                      0          NI         0                                 !
!                      0          0          NI ]                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF

SELECT CASE(DIME_mod)

 CASE(i_2D_strain)
   ALLOCATE(B(2,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  N(I)  ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/  ZERO  ,   N(I)      /)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(2,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  N(I)  ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/  ZERO  ,   N(I)      /)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(3,3*N_NE))
   DO I=1,N_NE
      B(1,3*(I-1)+1:3*I) = (/  N(I)  ,   ZERO    ,   ZERO  /)
      B(2,3*(I-1)+1:3*I) = (/  ZERO  ,   N(I)    ,   ZERO  /)
      B(3,3*(I-1)+1:3*I) = (/  ZERO  ,   ZERO    ,   N(I)  /)
   ENDDO
 CASE DEFAULT
   call faterr('a_poroEF_iso::N_U_ISO','dime not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE N_U_ISO
! ------------------------------------------------------------------------------

SUBROUTINE DIV_U_ISO(N_NE,N,DNX,R,B)

! computes the DIV_U matrix (divergence of displacement)  at a gauss point

!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BI  = [ dNI / dX       0                                            !
!                      0      dNI / dY                                         !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                      0      dNI / dY       0                                 !
!                      0          0      dNI / dZ ]                            !
!                                                                              !
!  EN AXI :BI  = [ dNI / dX + NI / R        0                                  !
!                           0           dNI / dY  ]                            !
!                                                                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: DNX(:,:)
REAL(KIND=LONG)                :: R
REAL(KIND=LONG), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF
SELECT CASE(DIME_mod)
 
 CASE(i_2D_strain)
   ALLOCATE(B(2,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(2,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  DNX(1,I) + (N(I)/R)   ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/   ZERO                 ,  DNX(2,I)   /)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(3,3*N_NE))
   DO I=1,N_NE
      B(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
      B(2,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
      B(3,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
   ENDDO
 CASE DEFAULT
   call faterr('a_poroEF_iso::DIV_U_ISO','dime not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE DIV_U_ISO

! ------------------------------------------------------------------------------

SUBROUTINE DIV_U_GD_ISO(N_NE,N,DNX,R,B)

! computes the DIV_U matrix (divergence of displacement)  at a gauss point

!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BI  = [ dNI / dX       0                                            !
!                      0      dNI / dX                                         !
!                  dNI / dY       0                                            ! 
!                      0      dNI / dY  ]                                      !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                      0      dNI / dX       0                                 !
!                      0          0      dNI / dX ]                            !
!                  dNI / dY       0          0                                 !
!                      0      dNI / dY       0                                 !
!                      0          0      dNI / dY ]                            !
!                  dNI / dZ       0          0                                 !
!                      0      dNI / dZ       0                                 !
!                      0          0      dNI / dZ ]                            !
!                                                                              !
!  EN AXI :BI  = Description a Faire                                           ! 
!                                                                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: DNX(:,:)
REAL(KIND=LONG)                :: R
REAL(KIND=LONG), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF
SELECT CASE(DIME_mod)
 
 CASE(i_2D_strain)
   ALLOCATE(B(4,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(1,I)   /)
      B(3,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,   ZERO      /)
      B(4,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(4,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  DNX(1,I) + (N(I)/R)   ,   ZERO                 /)
      B(2,2*(I-1)+1:2*I) = (/   ZERO                 ,  DNX(1,I) + (N(I)/R)   /)
      B(3,2*(I-1)+1:2*I) = (/  DNX(2,I)              ,   ZERO                 /)
      B(4,2*(I-1)+1:2*I) = (/   ZERO                 ,  DNX(2,I)              /)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(9,3*N_NE))
   DO I=1,N_NE
      B(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
      B(2,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(1,I)   ,   ZERO      /)
      B(3,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(1,I)   /)
      B(4,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,   ZERO      ,   ZERO      /)
      B(5,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
      B(6,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(2,I)   /)
      B(7,3*(I-1)+1:3*I) = (/  DNX(3,I)  ,   ZERO      ,   ZERO      /)
      B(8,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(3,I)   ,   ZERO      /)
      B(9,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
   ENDDO
 CASE DEFAULT
   call faterr('a_poroEF_iso::DIV_U_GD_ISO','dime not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE DIV_U_GD_ISO

! ------------------------------------------------------------------------------

SUBROUTINE GRADIENT_ISO(phys,i,ig,X,DNX,COEFINT,R)

 IMPLICIT NONE

 INTEGER        , INTENT(IN)  :: i, ig

 REAL(KIND=LONG), INTENT(IN)  :: X(:,:)

 REAL(KIND=LONG), POINTER     :: DNX(:,:)
 REAL(KIND=LONG), INTENT(OUT) :: COEFINT
 REAL(KIND=LONG), INTENT(OUT) :: R

!                           12345678901234567890123456
  CHARACTER(len=26) :: IAM='a_poroEF_iso::gradient_iso'
  CHARACTER(len=4 ) :: phys

  SELECT CASE(phys)
    CASE('MECA')
    CALL GRADIENT_ISO_MECA(get_N_NODE_MECA_poroEF_iso(i), &
                           poroEF(i)%PG_MECA(ig)%N, poroEF(i)%PG_MECA(ig)%DN ,poroEF(i)%PG_MECA(ig)%POIDS , &
                           X,DNX,COEFINT,R)
    CASE('THER')
    
    CALL GRADIENT_ISO_THER(get_N_NODE_THER_poroEF_iso(i), &
                           poroEF(i)%PG_THER(ig)%N, poroEF(i)%PG_THER(ig)%DN ,poroEF(i)%PG_THER(ig)%POIDS , &
                           X,DNX,COEFINT,R)
    CASE DEFAULT
       call faterr('a_poroEF_iso::GRADIENT_ISO','Mauvaise physique demandee: '//phys)
  END SELECT

END SUBROUTINE GRADIENT_ISO

!------------------------------------------------------------------------------!
!    Calcul de la matrice de couplage  elementaire  Kup                        !
!------------------------------------------------------------------------------!
SUBROUTINE COUPLAGE_HPP_ISO(i,ppsnb,ibdyty,iblmty,dt,X,C)


  IMPLICIT NONE

  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty       ! le numero de l'element dans la liste locale
  INTEGER                     :: IG
  real(kind=long)             :: dt         !
  REAL(KIND=LONG)             :: X(:,:)     ! coordonnees des sommets
  REAL(KIND=LONG)             :: C(:,:)     ! matrice de couplage. elem. poro.
  REAL(KIND=LONG)             :: COEFINT,R
  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:), & !   
                                     B(:,:) , &  ! matrice B
                                     N(:,:)      ! matrice N
  !REAL(KIND=LONG), ALLOCATABLE    :: P(:)        ! vecteur de test
                          !123456789012345678901234567
  character(len=27):: IAM='a_poro_EF_iso::Couplage_iso'
  
  REAL(kind=long) :: biot

  logical :: is_biot_field
  integer :: rank,mdlnb,lawnb
  
  integer,dimension(:) :: ppsnb
  
  is_biot_field=.false. 
  
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  
  if (get_biot_type(lawnb) == 0) then
     biot = get_biot(lawnb)
  else
     is_biot_field=.true.
  endif
  
  ! Pour tous les points de Gauss
  DO IG=1,get_N_PG_RIG_poroEF(i)
     
     if (is_biot_field) then 
         rank = get_poro_field_rank_MAILx(ibdyty,iblmty,'BIOT')
         CALL get_poro_field_MAILx(ibdyty,iblmty,ig,rank,biot)

     endif
     
     ! Initialisation a vide des pointeurs
     NULLIFY(N,B,DNX)
     ! Appel aux gradient des fonctions de formes de la meca
     CALL GRADIENT_ISO('MECA',i,ig,X,DNX,COEFINT,R)
     ! Construction de la matrice divergence
     CALL DIV_U_ISO(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,B)
     
     ! Construction
     ! Construction de la matrice des fonctions de forme thermique
     CALL N_P_ISO(get_N_NODE_THER_poroEF_iso(i),poroEF(i)%PG_THER_ON_MECA(ig)%N,N)

     
     C = C +  MATMUL(TRANSPOSE(N),B)*COEFINT*biot
     
     DEALLOCATE(N,B,DNX); NULLIFY(N,B,DNX)

  ENDDO

END SUBROUTINE  COUPLAGE_HPP_ISO

!------------------------------------------------------------------------------!
!    Calcul de la matrice de couplage non lineaire elementaire  Kup            !
!------------------------------------------------------------------------------!
SUBROUTINE COUPLAGE_GD_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U,C)


  IMPLICIT NONE

  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty       ! le numero de l'element dans la liste locale
  INTEGER                     :: IG
  real(kind=long)             :: dt            !
  REAL(KIND=LONG)             :: X(:,:),U(:,:) ! coordonnees des sommets
  REAL(KIND=LONG)             :: C(:,:)        ! matrice de couplage. elem. poro.
  REAL(KIND=LONG)             :: COEFINT,R
  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:), &  !   
                                     DIV(:,:) , & ! matrice DIV
                                     N(:,:)       ! matrice N
                                     
  REAL(KIND=LONG), POINTER        :: F_T(:,:)
                                     
  !REAL(KIND=LONG), ALLOCATABLE    :: P(:)        ! vecteur de test
                          !123456789012345678901234567
  character(len=27):: IAM='a_poro_EF_iso::Couplage_iso'
  
  REAL(kind=long) :: biot,J

  logical :: is_biot_field
  integer :: rank,mdlnb,lawnb
  
  integer,dimension(:) :: ppsnb
  
  is_biot_field=.false. 
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  
  if (get_biot_type(lawnb) == 0) then
     biot = get_biot(lawnb)
  else
     is_biot_field=.true.
  endif
  
  ! Pour tous les points de Gauss
  DO IG=1,get_N_PG_RIG_poroEF(i)
     
     if (is_biot_field) then 
         rank = get_poro_field_rank_MAILx(ibdyty,iblmty,'BIOT')
         CALL get_poro_field_MAILx(ibdyty,iblmty,ig,rank,biot)

     endif
     ! Initialisation a vide des pointeurs
     NULLIFY(N,DNX,F_T,DIV)
     ! Appel aux gradient des fonctions de formes de la meca
     CALL GRADIENT_ISO('MECA',i,ig,X,DNX,COEFINT,R)
     ! Construction de la matrice divergence
     CALL  DIV_U_GD_ISO(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,DIV)
     ! Calcul du gradient de la transformation F = I + grad U
     CALL  GRAD_TRANSFORMATION(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,U,F_T,J)
     ! Construction de la matrice des fonctions de forme thermique
     CALL  N_P_ISO(get_N_NODE_THER_poroEF_iso(i),poroEF(i)%PG_THER_ON_MECA(ig)%N,N)
     
     C = C +  MATMUL(TRANSPOSE(N),MATMUL(F_T,DIV))*COEFINT*biot*J
     
     DEALLOCATE(N,F_T,DIV,DNX); NULLIFY(N,F_T,DIV,DNX)

  ENDDO

END SUBROUTINE  COUPLAGE_GD_ISO

SUBROUTINE GRAD_TRANSFORMATION(N_NE,N,DNX,R,U,F_T,J)

! computes the inv F matrix (inverse de la Jacobienne de la transformation)

!------------------------------------------------------------------------------!
!                                                                              !
!  EN 3D : inv F = [ dX / dx   dX / dy    dX / dz                              !
!                    dY / dx   dY / dy    dY / dz                              !
!                    dZ / dx   dZ / dy    dZ / dz   ]                          !
!                                                                              !
!------------------------------------------------------------------------------!


IMPLICIT NONE

INTEGER                      :: N_NE
REAL(KIND=LONG)              :: R,J,Ur
REAL(KIND=LONG), POINTER     :: DNX(:,:),N(:),B(:,:),Id(:)
REAL(KIND=LONG)              :: U(:,:)
REAL(KIND=LONG),DIMENSION(:,:),POINTER :: F_T
REAL(KIND=LONG),DIMENSION(:),  ALLOCATABLE :: GRAD_F
REAL(KIND=LONG),DIMENSION(:,:),ALLOCATABLE :: inv_F
  
INTEGER :: inull
                        !1234567890123456789012345678901234
CHARACTER(len=34):: IAM='a_poro_EF_iso::grad_transformation'

nullify(Id,B)

IF(ASSOCIATED(F_T)) THEN ; DEALLOCATE(F_T) ; NULLIFY(F_T) ; ENDIF

SELECT CASE(DIME_mod)

    CASE(i_3D)
        ALLOCATE(inv_F(3,3))
        inv_F(:,:) = 0.d0
        ALLOCATE(GRAD_F(9))
        GRAD_F(:) = 0.d0
        ALLOCATE(F_T(3,9))
        F_T(:,:) = 0.d0
        
        CALL B_ISO_MECA(N_NE,N,DNX,R,B)
        
        GRAD_F = MATMUL(B,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
        CALL Id_ISO_MECA(Id)
        GRAD_F = Id + GRAD_F
     
        inv_F = reshape(GRAD_F,(/3,3/))
        J   = determinant33(inv_F)
        call inverse33(inv_F,inull)
        if (inull == 1) then
          call faterr(IAM,'non invertible matrix')
        endif   
       
        ! dUx / dx
        F_T(1,1) = inv_F(1,1)
        F_T(1,4) = inv_F(1,2)
        F_T(1,7) = inv_F(1,3)
        ! dUy / dy
        F_T(2,2) = inv_F(2,1)
        F_T(2,5) = inv_F(2,2)
        F_T(2,8) = inv_F(2,3)
        ! dUz / dz
        F_T(3,3) = inv_F(3,1)
        F_T(3,6) = inv_F(3,2)
        F_T(3,9) = inv_F(3,3)
        
    CASE(i_2D_strain)

        ALLOCATE(inv_F(2,2))
        inv_F(:,:) = 0.d0
        ALLOCATE(GRAD_F(4))
        GRAD_F(:) = 0.d0
        ALLOCATE(F_T(2,4))
        F_T(:,:) = 0.d0
        
         CALL B_ISO_MECA(N_NE,N,DNX,R,B)
        
        GRAD_F = MATMUL(B,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
        CALL Id_ISO_MECA(Id)
        GRAD_F = Id + GRAD_F
        
        inv_F = reshape(GRAD_F,(/2,2/))
        J     = inv_F(1,1)*inv_F(2,2)-inv_F(1,2)*inv_F(2,1)
        inv_F(1,:)=(/  inv_F(2,2)/J ,-inv_F(1,2)/J /)
        inv_F(2,:)=(/ -inv_F(2,1)/J , inv_F(1,1)/J /)
        
        ! dUx / dx
        F_T(1,1) = inv_F(1,1)
        F_T(1,3) = inv_F(1,2)
        ! dUy / dy
        F_T(2,2) = inv_F(2,1)
        F_T(2,4) = inv_F(2,2)
    
    CASE(i_2D_axisym)
    
        ALLOCATE(inv_F(2,2))
        inv_F(:,:) = 0.d0
        ALLOCATE(GRAD_F(4))
        GRAD_F(:) = 0.d0
        ALLOCATE(F_T(2,4))
        F_T(:,:) = 0.d0
        
         CALL B_ISO_MECA(N_NE,N,DNX,R,B)
        
        GRAD_F = MATMUL(B,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
        CALL Id_ISO_MECA(Id)
        GRAD_F = Id + GRAD_F
        
        Ur = DOT_PRODUCT(N,U(1,:))
        
        inv_F = reshape(GRAD_F,(/2,2/))
        J     = (inv_F(1,1)*inv_F(2,2)-inv_F(1,2)*inv_F(2,1))*(1.D0 + Ur/R)
        inv_F(1,:)=(/  inv_F(2,2)/J ,-inv_F(1,2)/J /)
        inv_F(2,:)=(/ -inv_F(2,1)/J , inv_F(1,1)/J /)
        
        ! dUx / dx
        F_T(1,1) = inv_F(1,1)
        F_T(1,3) = inv_F(1,2)
        ! dUy / dy
        F_T(2,2) = inv_F(2,1)
        F_T(2,4) = inv_F(2,2)
    CASE DEFAULT
        CALL FATERR(IAM,'Unsupported dimension: '//get_dime_mode_name_from_id(dime_mod))
END SELECT

DEALLOCATE(inv_F,GRAD_F)
DEALLOCATE(Id, B); NULLIFY(Id, B)

END SUBROUTINE GRAD_TRANSFORMATION

!------------------------------------------------------------------------------!
!    Calcul de la matrice de masse elementaire  [Me]=[M  ;  0 ] [dV/dt]            !
!                                                    [0  ;  C ] [dP/dt]            !
!------------------------------------------------------------------------------!
SUBROUTINE MASS_ISO_SOLID(i,ppsnb,dt,X,P,ibdyty,iblmty,M)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i,ibdyty,iblmty! le numero de l'element dans la liste locale
integer        ,dimension(:):: ppsnb
REAL(KIND=LONG)             :: X(:,:)     ! coordonnees des sommets
REAL(KIND=LONG)             :: P(:)       ! pressions aux sommets
REAL(KIND=LONG)             :: M(:,:)     ! matrice de mass. elem. poro.
INTEGER                     :: Nb_node_meca,Nb_node_ther, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE:: M_meca(:,:)     ! matrice de mass. elem. de la meca
REAL(KIND=LONG), ALLOCATABLE:: C_ther(:,:)     ! matrice de mass. elem. de la ther
REAL(KIND=LONG), ALLOCATABLE:: F_ther(:)       ! vecteur de mass. elem. de la ther
REAL(KIND=LONG)             :: dt

! Recuperation des infos de l element fini poro mecanique
Nb_node_meca     = get_N_NODE_MECA_poroEF_iso(i)
Nb_node_ther     = get_N_NODE_THER_poroEF_iso(i)
Nb_dof_meca = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de masse mecanique
ALLOCATE(M_meca(Nb_node_meca * Nb_dof_meca,Nb_node_meca * Nb_dof_meca))
M_meca(:,:) = 0.d0

CALL MASS_ISO_MECA(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,X,M_meca)

! Creation de la matrice masse elementaire d unb element poro mecanique
DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_meca * Nb_dof_meca
       M(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%meca_2_poro(j_dof)) = M_meca(i_dof,j_dof)
    ENDDO
ENDDO

! Allocation de la matrice de capacite thermique
ALLOCATE(C_ther(Nb_node_ther * Nb_dof_ther,Nb_node_ther * Nb_dof_ther))
C_ther(:,:) = 0.d0

ALLOCATE(F_ther(Nb_node_ther * Nb_dof_ther))
F_ther(:) = 0.d0

CALL CAPACITY_ISO_THER(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),P,ibdyty,iblmty,F_ther,C_ther)


! Creation de la matrice de capacite elementaire d un element poro mecanique
DO i_dof=1,Nb_node_ther * Nb_dof_ther
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       M(poroEF(i)%ther_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) = C_ther(i_dof,j_dof)
    ENDDO
    !done in assemb_RHS
    !F(poroEF(i)%ther_2_poro(i_dof)) = F_ther(i_dof)
ENDDO

DEALLOCATE(C_ther,M_meca,F_ther)

END SUBROUTINE MASS_ISO_SOLID

!------------------------------------------------------------------------------!
!    Calcul de la matrice de masse elementaire  [Me]=[M  ;  0 ] [dV/dt]            !
!                                                    [0  ;  0 ] [dP/dt]           !
!------------------------------------------------------------------------------!
SUBROUTINE MASS_ISO_FLUID(i,ppsnb,dt,X,ibdyty,iblmty,M)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i,ibdyty,iblmty! le numero de l'element dans la liste locale
integer        ,dimension(:):: ppsnb
REAL(KIND=LONG)             :: X(:,:)     ! coordonnees des sommets
REAL(KIND=LONG)             :: M(:,:)     ! matrice de mass. elem. poro.
INTEGER                     :: Nb_node, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE:: M_meca(:,:)     ! matrice de mass. elem. de la meca
REAL(KIND=LONG)             :: dt

M = 0.d0

! Recuperation des infos de l element fini poro mecanique
Nb_node     = get_N_NODE_MECA_poroEF_iso(i)
Nb_dof_meca = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de masse mecanique
ALLOCATE(M_meca(Nb_node * Nb_dof_meca,Nb_node * Nb_dof_meca))
M_meca(:,:) = 0.d0

CALL MASS_ISO_MECA(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,X,M_meca)

! Creation de la matrice masse elementaire d unb element poro mecanique
DO i_dof=1,Nb_node * Nb_dof_meca
    DO j_dof=1,Nb_node * Nb_dof_meca
       M(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%meca_2_poro(j_dof)) = M_meca(i_dof,j_dof)
    ENDDO
ENDDO

DEALLOCATE(M_meca)

END SUBROUTINE MASS_ISO_FLUID

!------------------------------------------------------------------------------!
!    Calcul de la matrice de masse elementaire  Muu                            !
!------------------------------------------------------------------------------!
SUBROUTINE MASS_NAVIER_STOKES_ISO(i,ppsnb,ibdyty,iblmty,dt,X,V,C)

  IMPLICIT NONE

  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty       ! le numero de l'element dans la liste locale
  INTEGER                     :: IG
  REAL(KIND=LONG)             :: X(:,:),V(:,:)     ! coordonnees des sommets
  REAL(KIND=LONG)             :: C(:,:)     ! matrice d'acceleration du fluide permanente
  REAL(KIND=LONG)             :: COEFINT,R
  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:), & !   
                                     B(:,:)  ! matrice B
  
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE:: V_x_pg(:)   ! vecteur au PG de la vitesse
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE:: V_y_pg(:)   ! vecteur au PG de la vitesse
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE:: V_z_pg(:)   ! vecteur au PG de la vitesse
                                    
                            !1234567890123456789012345678901234567
  character(len=37):: IAM='a_poro_EF_iso::Mass_Navier_Stokes_iso'
 
  ! zone de stockage: gradient,flux,internal,operateur tangent
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: N_SUPG, NN, Mat_Vpg, GradVo
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRADV
   
  REAL(kind=long) :: vol,lenght,rho,NormV,T1_SUPG,Pe,mu,dt
  
  REAL(kind=8),DIMENSION(21) :: elas_coeff
  
  logical :: is_char=.false.
  logical :: is_supg=.false.
  
  integer :: rank,mdlnb,lawnb,idime,j,anisotropie
  
  integer,dimension(:) :: ppsnb
  
  ! Debut du calcul
  is_char=.false.
  is_supg=.false.
  
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)
  IF (get_eleop_value(mdlnb,'adve_') == 'supg_' ) is_supg = .true.
  IF (get_eleop_value(mdlnb,'adve_') == 'char_' ) is_char = .true.
  
  CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)
   
  ! Recherche du coefficient de diffusion pour evaluation du Peclet local
  rho = get_rho(lawnb)
  vol = element_volume_ISO(i, X)
  mu  = elas_coeff(1)
    
  SELECT CASE(DIME_mod)
      CASE(i_2D_strain,i_2D_stress,i_2D_axisym )
           lenght = sqrt(vol/PI_g)
           ALLOCATE(N_SUPG(get_N_NODE_MECA_poroEF_iso(i)*nbDIME,2))
           ALLOCATE(NN(get_N_NODE_MECA_poroEF_iso(i)*nbDIME,2))
           ALLOCATE(GradVo(2,2))
           ALLOCATE(GradV(5))
           NN     = 0.D0
           GradVo = 0.D0
           GradV  = 0.D0
           ! Vitesse aux PG
           ALLOCATE(V_x_pg(get_N_PG_RIG_poroEF(i)))
           V_x_pg = 0.D0
           ALLOCATE(V_y_pg(get_N_PG_RIG_poroEF(i)))
           V_y_pg = 0.D0
           ALLOCATE(V_z_pg(get_N_PG_RIG_poroEF(i)))
           V_z_pg = 0.D0
           CALL interpolate_node2pg_ISO(i,V(1,:),V_x_pg,'MECA')
           CALL interpolate_node2pg_ISO(i,V(2,:),V_y_pg,'MECA')
                      
      CASE(i_3D)
           lenght = SIGN(ABS((3.0d0*vol)/(4.0d0*PI_g))**(1.0/3.0),(3.0d0*vol)/(4.0d0*PI_g))
           ALLOCATE(N_SUPG(get_N_NODE_MECA_poroEF_iso(i)*nbDIME,3))
           ALLOCATE(NN(get_N_NODE_MECA_poroEF_iso(i)*nbDIME,3))
           ALLOCATE(GradVo(3,3))
           ALLOCATE(GradV(9))
           NN     = 0.D0
           GradVo = 0.D0
           GradV  = 0.D0
           ! Vitesse aux PG
           ALLOCATE(V_x_pg(get_N_PG_RIG_poroEF(i)))
           V_x_pg = 0.D0
           ALLOCATE(V_y_pg(get_N_PG_RIG_poroEF(i)))
           V_y_pg = 0.D0
           ALLOCATE(V_z_pg(get_N_PG_RIG_poroEF(i)))
           V_z_pg = 0.D0
           CALL interpolate_node2pg_ISO(i,V(1,:),V_x_pg,'MECA')
           CALL interpolate_node2pg_ISO(i,V(2,:),V_y_pg,'MECA')
           CALL interpolate_node2pg_ISO(i,V(3,:),V_z_pg,'MECA')
           
  END SELECT
  
  
  
  ! Initialisation a vide des pointeurs
  NULLIFY(B,DNX) 
  
  ! Pour tous les points de Gauss de la mecanique
  DO IG=1,get_N_PG_RIG_poroEF(i)
     
     ! Appel aux gradient des fonctions de formes de la meca
     CALL GRADIENT_ISO('MECA',i,ig,X,DNX,COEFINT,R)
     ! Construction de la matrice gradient
     CALL B_ISO_MECA(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,B)
     ! Recherche de la vitesse pour le Point de Gauss IG
     CALL MAT_U(V_x_pg(ig),V_y_pg(ig),V_z_pg(ig),Mat_Vpg,NormV)
     ! Construction de la matrice des fonctions de formes MECA
     DO idime = 1, nbDIME
        DO j=1,get_N_NODE_MECA_poroEF_iso(i) ! Boucle sur les noeuds
           NN(j*nbDIME-1 + idime - 1,idime) =  poroEF(i)%PG_MECA(ig)%N(j)
        ENDDO
     ENDDO
     
     !! Calcul de la matrice U.grad U soit Nt.U.B
     !C = C +  MATMUL(NN,MATMUL(Mat_Vpg,B))*COEFINT*rho
     ! Calcul du grad V au point de Gauss
     ! DA : ne marche pad en Axy
     GRADV = 0.D0
     GRADV = MATMUL(B,RESHAPE(source=V,shape=(/ SIZE(V) /)))
     GradVo = 0.D0
     GradVo = RESHAPE(source = GRADV, shape = (/nbDIME, nbDIME/) ,order = (/2,1/))

     ! DA : Dans le cas axisymmetrique on ajoute la contribution du rayon
     IF (DIME_mod == i_2D_axisym) THEN
         GradVo(1,1) = GradVo(1,1) + GRADV(5)
     ENDIF

     ! Calcul de la matrice U.grad U soit Nt.U.B
     C = C +  MATMUL(NN,MATMUL(Mat_Vpg,B))*COEFINT*rho + MATMUL(MATMUL(NN, GradVo),TRANSPOSE(NN))*COEFINT*rho

     ! appel a la stabilisation SUPG
     if (is_supg) then 
        
        ! Calcul du Peclet local
        if( NormV < 1.d-18 ) then
          T1_SUPG = 0.D0
        else
          Pe = lenght * NormV * rho / mu
          T1_SUPG = ((1.0 - 1.0/((Pe/2.0) + 1.0)) * lenght)/(2.0*NormV)
        end if
        ! Creation de la fonction de ponderation methode SUPG
        N_SUPG = T1_SUPG*transpose(matmul(Mat_Vpg,B))
        
        C = C + MATMUL(N_SUPG,MATMUL(Mat_Vpg,B))*COEFINT*rho + MATMUL(MATMUL(N_SUPG, GradVo),TRANSPOSE(N_SUPG))*COEFINT*rho

      endif

    DEALLOCATE(Mat_Vpg)


  ENDDO

DEALLOCATE(V_x_pg,V_y_pg,V_z_pg,N_SUPG, NN, GradVo, GRADV)
DEALLOCATE(B,DNX) ;  NULLIFY(B,DNX)

END SUBROUTINE  MASS_NAVIER_STOKES_ISO

! Creation de la matrice des vitesses pour navier stokes
SUBROUTINE MAT_U(Ux,Uy,Uz,Mat_Vpg,NormV)

IMPLICIT NONE
REAL(KIND=LONG), ALLOCATABLE:: Mat_Vpg(:,:)
REAL(KIND=LONG)             :: NormV
REAL(KIND=LONG), INTENT(IN) :: Ux,Uy,Uz

! Recherche de la vitesse pour le Point de Gauss IG
SELECT CASE(DIME_mod)

  CASE(i_2D_strain,i_2D_stress,i_2D_axisym )

       ALLOCATE(Mat_Vpg(2,5))
       Mat_Vpg=RESHAPE((/  Ux   ,  Uy     , ZERO , ZERO , ZERO ,&
                           ZERO ,  ZERO   , Ux   ,  Uy  , ZERO  /),(/2,5/))
       
       NormV = sqrt(Ux*Ux + Uy*Uy)
       
       !print *,'Vx : ',Ux
       !print *,'Vy : ',Uy
       
       !print *,'Mat Vpg : ',Mat_Vpg
       
       
  CASE(i_3D)
       ALLOCATE(Mat_Vpg(3,9))
       Mat_Vpg=RESHAPE((/  Ux   ,  Uy     , Uz   , ZERO , ZERO , ZERO , ZERO , ZERO , ZERO, &
                           ZERO ,  ZERO   , ZERO , Ux   , Uy   , Uz   , ZERO , ZERO , ZERO, &
                           ZERO ,  ZERO   , ZERO , ZERO , ZERO , ZERO,  Ux   , Uy   , Uz /),(/3,9/))
       
       NormV = sqrt(Ux*Ux + Uy*Uy + Uz*Uz)

END SELECT

END SUBROUTINE MAT_U

!------------------------------------------------------------------------------!
!    Calcul de la matrice de rigidite elementaire  [Ke]=[K  ;  0 ] [U]         !
!                                                       [0  ;  0 ] [int P]         !
!------------------------------------------------------------------------------!
SUBROUTINE BULK_GD_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty,F,K)

IMPLICIT NONE

INTEGER        , INTENT(IN)     :: i ! le numero de l'element dans la liste locale
INTEGER                         :: ibdyty,iblmty,mdlnb,lawnb
integer,dimension(:),intent(in) :: ppsnb
real(kind=8)                    :: dt
REAL(KIND=LONG),DIMENSION(:,:)  :: X,U ! coordonnees de depart,deplacement total
REAL(KIND=LONG),DIMENSION(:,:)  :: K     ! matrice de rig. elem.
REAL(KIND=LONG),DIMENSION(:)    :: F
INTEGER                         :: Nb_node_ther, Nb_node_meca, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE    :: K_meca(:,:)     ! matrice de rigidite. elem. de la meca
REAL(KIND=LONG), ALLOCATABLE    :: F_meca(:)       ! vecteur de force. interne. de la meca

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Recuperation des infos de l element fini poro mecanique
Nb_node_ther = get_N_NODE_THER_poroEF_iso(i)
Nb_node_meca = get_N_NODE_MECA_poroEF_iso(i)
Nb_dof_meca  = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther  = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de rigidite mecanique
ALLOCATE(K_meca(Nb_node_meca * Nb_dof_meca,Nb_node_meca * Nb_dof_meca))
K_meca(:,:) = 0.d0
! Allocation des efforts internes
ALLOCATE(F_meca(Nb_node_meca * Nb_dof_meca))
F_meca(:) = 0.d0

! Appel de la partie mecanique
CALL BULK_GD_ISO_MECA(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,dt,X,U, &
                      F_meca,.TRUE., &
                      K_meca,.TRUE.,&
                      .FALSE.)

! Partie concernant la mecanique structurelle
DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_meca * Nb_dof_meca
       K(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%meca_2_poro(j_dof)) = K_meca(i_dof,j_dof)
    ENDDO
    F(poroEF(i)%meca_2_poro(i_dof)) = F_meca(i_dof)
ENDDO

DEALLOCATE(K_meca,F_meca)

END SUBROUTINE BULK_GD_ISO

!------------------------------------------------------------------------------!
!    Calcul de la matrice de rigidite elementaire  [Ke]=[K    ;  0 ] [U]         !
!                                                       [0    ;  0 ] [int P]         !
!------------------------------------------------------------------------------!
SUBROUTINE BULK_HPP_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty,F,K)


IMPLICIT NONE

INTEGER        , INTENT(IN)     :: i ! le numero de l'element dans la liste locale
INTEGER                         :: ibdyty,iblmty,mdlnb,lawnb
integer,dimension(:),intent(in) :: ppsnb
real(kind=8)                    :: dt
REAL(KIND=LONG),DIMENSION(:,:)  :: X,U ! coordonnees de depart,deplacement total
REAL(KIND=LONG),DIMENSION(:,:)  :: K     ! matrice de rig. elem.
REAL(KIND=LONG),DIMENSION(:)    :: F
INTEGER                         :: Nb_node_ther, Nb_node_meca, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE    :: K_meca(:,:)     ! matrice de rigidite. elem. de la meca
REAL(KIND=LONG), ALLOCATABLE    :: F_meca(:)       ! vecteur de force. interne. de la meca

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Recuperation des infos de l element fini poro mecanique
Nb_node_ther = get_N_NODE_THER_poroEF_iso(i)
Nb_node_meca = get_N_NODE_MECA_poroEF_iso(i)
Nb_dof_meca  = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther  = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de rigidite mecanique
ALLOCATE(K_meca(Nb_node_meca * Nb_dof_meca,Nb_node_meca * Nb_dof_meca))
K_meca(:,:) = 0.d0
! Allocation des efforts internes
ALLOCATE(F_meca(Nb_node_meca * Nb_dof_meca))
F_meca(:) = 0.d0

! Appel de la partie mecanique
CALL BULK_HPP_ISO_MECA(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,dt,X,U, &
                       F_meca,.TRUE.,&
                       K_meca,.TRUE.,&
                       .FALSE.)

! Partie concernant la mecanique structurelle
DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_meca * Nb_dof_meca
       K(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%meca_2_poro(j_dof)) = K_meca(i_dof,j_dof)
    ENDDO
    F(poroEF(i)%meca_2_poro(i_dof)) = F_meca(i_dof)
ENDDO

DEALLOCATE(K_meca,F_meca)

END SUBROUTINE BULK_HPP_ISO

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_bulk_ISO(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,K)

   implicit none
   INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   real(kind=long)                 :: dt
   REAL(KIND=LONG)                 :: X(:,:),U(:,:) ! coordonnees des sommets
   REAL(KIND=LONG)                 :: K(:,:)        ! matrice de rig. elem.
   REAL(KIND=LONG),DIMENSION(:)    :: Fint 
                           !1234567890123456789
   character(len=19):: IAM='a_poro_EF_iso::bulk'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)
   
   select case(get_eleop_value(mdlnb,'type_'))
   case('solid')
       select case(get_eleop_value(mdlnb,'kine_'))
       case('small')
         call bulk_hpp_iso(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,K)
       case('large')
         call bulk_gd_iso(i,ppsnb,dt,X,U,ibdyty,iblmty,Fint,K)
       case default
         call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
         call FATERR(IAM,'kinematic type unknown (small | large)')
       end select
   case('fluid')
   
   
   case default
     call logMES('type=='//get_eleop_value(mdlnb,'type_'))
     call FATERR(IAM,'type unknown (solid | fluid)')
   end select


END SUBROUTINE


!============= low level private routines ==================

INTEGER FUNCTION get_N_PG_RIG_poroEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_RIG_poroEF=get_N_PG_RIG_mecaEF(poroEF(nb)%mecaID)

END FUNCTION get_N_PG_RIG_poroEF

INTEGER FUNCTION get_N_PG_MAS_poroEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_MAS_poroEF=get_N_PG_MAS_mecaEF(poroEF(nb)%mecaID)

END FUNCTION get_N_PG_MAS_poroEF

INTEGER FUNCTION get_N_PG_DIF_poroEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_DIF_poroEF=get_N_GP_therEF_iso(poroEF(nb)%therID)

END FUNCTION get_N_PG_DIF_poroEF

INTEGER FUNCTION get_N_PG_CAP_poroEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_CAP_poroEF=get_N_GP_therEF_iso(poroEF(nb)%therID)

END FUNCTION get_N_PG_CAP_poroEF


integer(kind=4) FUNCTION get_T_FONC_FORME_poroEF(nb,phys)
  IMPLICIT NONE
  INTEGER          :: nb
  CHARACTER(len=4) :: phys

  SELECT CASE(phys)
    CASE('MECA')
         get_T_FONC_FORME_poroEF = get_T_FONC_FORME_mecaEF(poroEF(nb)%mecaID)
    CASE('THER')
         get_T_FONC_FORME_poroEF = get_T_FONC_FORME_therEF_iso(poroEF(nb)%therID)
    CASE DEFAULT
         call faterr('a_poroEF_iso::get_T_FONC_FORME','Mauvaise physique demandee: '//phys)
  END SELECT
  
END FUNCTION get_T_FONC_FORME_poroEF

integer(kind=4) FUNCTION get_SCH_GAUSS_poroEF(nb,phys)
  IMPLICIT NONE
  INTEGER          :: nb
  CHARACTER(len=4) :: phys

  SELECT CASE(phys)
    CASE('MECA')
         get_SCH_GAUSS_poroEF = get_SCH_GAUSS_RIG_mecaEF(poroEF(nb)%mecaID)
    CASE('THER')
         get_SCH_GAUSS_poroEF = get_SCH_GAUSS_therEF(poroEF(nb)%therID)
    CASE DEFAULT
         call faterr('a_poroEF_iso::get_SCH_GAUSS','Mauvaise physique demandee: '//phys)
  END SELECT
  
END FUNCTION get_SCH_GAUSS_poroEF

!*************************************************************************
SUBROUTINE get_coor_pg_ISO(i,X,coor_pg,phys)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i             ! le numero de l'element
REAL(KIND=LONG)              :: X(:,:)        ! coordonnees des sommets
REAL(KIND=LONG)              :: coor_pg(:,:)  ! coordonnees des points de Gauss
INTEGER                      :: ig
CHARACTER(len=4)             :: phys          ! type de physique demande

SELECT CASE(phys)
    CASE('MECA')
         CALL get_coor_pg_ISO_meca(poroEF(i)%mecaID,X,coor_pg)
    CASE('THER')
         CALL get_coor_pg_ISO_ther(poroEF(i)%therID,X,coor_pg)
    CASE DEFAULT
         call faterr('a_poroEF_iso::get_coor_pg','Mauvaise physique demandee: '//phys)
  END SELECT

END SUBROUTINE get_coor_pg_ISO
!*************************************************************************
!*************************************************************************
SUBROUTINE interpolate_node2pg_ISO(i,valnoe,valpg,phys)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i           ! le numero de l'element
REAL(KIND=LONG)              :: valnoe(:)   ! valeurs aux sommets
REAL(KIND=LONG)              :: valpg(:)    ! valeurs aux points de Gauss
INTEGER                      :: ig
CHARACTER(len=4)             :: phys          ! type de physique demande

  SELECT CASE(phys)
    CASE('MECA')
         CALL interpolate_node2pg_ISO_MECA(poroEF(i)%mecaID,valnoe,valpg)
    CASE('THER')
         CALL interpolate_node2pg_ISO_THER(poroEF(i)%therID,valnoe,valpg)
    CASE DEFAULT
         call faterr('a_poroEF_iso::interpolate_node2pg','Mauvaise physique demandee: '//phys)
  END SELECT
        

END SUBROUTINE interpolate_node2pg_ISO
!*************************************************************************
!*************************************************************************
INTEGER FUNCTION get_bw_poroEF_iso(nb,nodes)

  IMPLICIT NONE
  INTEGER :: nb, bw_meca, bw_ther
  INTEGER,DIMENSION(:) :: nodes
  
  bw_meca = get_bw_mecaEF_iso(poroEF(nb)%mecaID,nodes)
  bw_ther = get_bw_therEF_iso(poroEF(nb)%therID,nodes)
  
  get_bw_poroEF_iso = bw_meca + bw_ther
  
END FUNCTION get_bw_poroEF_iso

!*************************************************************************
!*************************************************************************
! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_fields_ISO(i,ppsnb,dt,X,U,V,P,ibdyty,iblmty)

implicit none
INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
integer,dimension(:),intent(in) :: ppsnb
INTEGER        , INTENT(IN)     :: ibdyty,iblmty
real(kind=8)                    :: dt
REAL(KIND=LONG)                 :: X(:,:),U(:,:),V(:,:) ! coordonnees des sommets
REAL(KIND=LONG)                 :: P(:)
                        !12345678901234567890123456789012345678901234
character(len=44):: IAM='a_poro_EF_iso::compute_elementary_fiedls_iso'
integer :: mdlnb,inull

CALL get_ppset_value(ppsnb(1),mdlnb,inull)
    select case(get_eleop_value(mdlnb,'type_'))
      case('solid')
        select case(get_eleop_value(mdlnb,'kine_'))
        case('small')
            call fields_hpp_iso_meca(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,dt,X,U)
            call fields_hpp_iso_ther(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),P,ibdyty,iblmty)
        case('large')
            call fields_gd_iso_meca(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,dt,X,U)
            call fields_gd_iso_ther(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),&
                                    U(:,1:get_N_NODE_THER_poroEF_iso(i)), & 
                                    P,ibdyty,iblmty)
        case default
            call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
        call FATERR(IAM,'kinematic type unknown (small | large)')
        end select
     case('fluid')
        select case(get_eleop_value(mdlnb,'kine_'))
        case('small')
            call fields_hpp_iso_meca(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,dt,X,V)
            call fields_hpp_iso_ther(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),P,ibdyty,iblmty)
        case('large')
            call logMES('this option - large - is not implemented for fluid model')
        case default
            call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
        call FATERR(IAM,'kinematic type unknown (small | large)')
        end select
     
     case default
        call logMES('type=='//get_eleop_value(mdlnb,'type_'))
        call FATERR(IAM,'type unknown (solid | fluid)')
     end select

END SUBROUTINE
!----------------------------------------------------------------------------!
! fonction qui calule le volume d'un element
function element_volume_ISO(i, X)

   implicit none

   ! variables d'entree
   integer, intent(in)      :: i ! numero de l'element dans la liste locale
   real(kind=8), intent(in) :: X(:,:) ! coordonnees des sommets de l'element

   ! valeur de retour
   real(kind=8)             :: element_volume_ISO ! volume de l'element
   
   ! variables locales   
   real(kind=8)             :: V ! pour calculer le volume de l'element
   integer                  :: iG ! indice de boucle

   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT ! pour recuperer le produit du poids associe
                                       ! au point de Gauss courant (pour l'integration)
                                       ! et de la valeur du jacobien au point de Gauss 
                                       ! courant (changement de variable pour integrer
                                       ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         !point de Gauss courant
   
   ! Initialisation a vide des pointeurs
   NULLIFY(DNX)
 
   ! on initialise a 0 le volume de la maille
   V=0.d0

   ! Pour tous les points de Gauss
   do IG=1,get_N_PG_RIG_poroEF(i)  

      ! on recupere la valeur du produit du poids associe au point de Gauss courant
      ! et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
      CALL GRADIENT_ISO('MECA',i,ig,X,DNX,COEFINT,R)

      ! on ajoute la contribution du point de Gauss courant au calcul du volume
      ! de l'element
      ! N.B.: le volume de l'element est la somme pour tous les points de Gauss
      !       de l'element iG de w(iG)*det(J)(x_iG)
      V = V + COEFINT

   end do

   ! on libere l'espace memoire occupe par
   deallocate(dnx)

   ! on renvoie le volume de l'element
   element_volume_ISO = V

end function element_volume_ISO

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes modelisations PoroElastique, NavierStokes
!
SUBROUTINE compute_elementary_mass_ISO(i,ppsnb,dt,X,U,P,ibdyty,iblmty,K)

   implicit none
   INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   real(kind=long)                 :: dt
   REAL(KIND=LONG)                 :: X(:,:),U(:,:),P(:)   ! coordonnees des sommets
   REAL(KIND=LONG)                 :: K(:,:)        ! matrice de rig. elem.
                           !1234567890123456789
   character(len=19):: IAM='a_poro_EF_iso::bulk'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)
   
   select case(get_eleop_value(mdlnb,'type_'))
   case('solid')
     call MASS_ISO_SOLID(i,ppsnb,dt,X,P,ibdyty,iblmty,K)
   case('fluid')
     call MASS_ISO_FLUID(i,ppsnb,dt,X + U,ibdyty,iblmty,K)
   case default
     call logMES('type=='//get_eleop_value(mdlnb,'type_'))
     call FATERR(IAM,'type unknown (solid | fluid)')
   end select

END SUBROUTINE compute_elementary_mass_ISO

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes modelisations PoroElastique, NavierStokes
!
SUBROUTINE compute_elementary_damping_ISO(i,ppsnb,dt,X,U,V,P,ibdyty,iblmty,K)

   implicit none
   INTEGER        , INTENT(IN)     :: i       ! le numero de l'element dans la liste locale
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   real(kind=long)                 :: dt
   REAL(KIND=LONG)                 :: X(:,:),U(:,:),V(:,:) ! coordonnees des sommets
   REAL(KIND=LONG)                 :: P(:)
   REAL(KIND=LONG)                 :: K(:,:)        ! matrice de rig. elem.
                           !1234567890123456789
   character(len=19):: IAM='a_poro_EF_iso::bulk'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)
   
   select case(get_eleop_value(mdlnb,'type_'))
   case('solid')
     call DAMPING_ISO_SOLID(i,ppsnb,dt,X,U,P,ibdyty,iblmty,K)
   case('fluid')
     call DAMPING_ISO_FLUID(i,ppsnb,dt,X + U,U,V,P,ibdyty,iblmty,K)
   case default
     call logMES('type=='//get_eleop_value(mdlnb,'type_'))
     call FATERR(IAM,'type unknown (solid | fluid)')
   end select

END SUBROUTINE compute_elementary_damping_ISO

!------------------------------------------------------------------------------!
!    Calcul de la matrice de NavierStockes elementaire  [Ke]=[ K    ; -C       ] [V]         !
!                                                            [ C^T  ;  epsilon ] [P]         !
!------------------------------------------------------------------------------!
SUBROUTINE DAMPING_ISO_FLUID(i,ppsnb,dt,X,U,V,P,ibdyty,iblmty,K)


IMPLICIT NONE

INTEGER        , INTENT(IN)     :: i ! le numero de l'element dans la liste locale
INTEGER                         :: ibdyty,iblmty,mdlnb,lawnb,anisotropie
REAL(kind=8),DIMENSION(21)       :: elas_coeff 
integer,dimension(:),intent(in) :: ppsnb
REAL(KIND=LONG),DIMENSION(:,:)  :: X,U,V ! coordonnees de depart,deplacement total
REAL(KIND=LONG),DIMENSION(:,:)  :: K     ! matrice de rig. elem.
REAL(KIND=LONG),DIMENSION(:)    :: P     ! Pression nodale
REAL(KIND=LONG)                 :: dt, epsilon_parameter
INTEGER                         :: Nb_node_ther, Nb_node_meca, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE    :: K_meca(:,:)     ! matrice de rigidite. elem. de la meca
REAL(KIND=LONG), ALLOCATABLE    :: M_meca(:,:)     ! matrice de rigidite. elem. de la meca
REAL(KIND=LONG), ALLOCATABLE    :: C_ther(:,:)     ! matrice de capa. elem. de la ther

REAL(KIND=LONG), ALLOCATABLE    :: F_meca(:)       ! vecteur de force. interne. de la meca
REAL(KIND=LONG), ALLOCATABLE    :: F_ther(:)       ! vecteur de force. interne. de la meca
REAL(KIND=LONG), ALLOCATABLE    :: C_couplage(:,:) ! matrice de couplage Structure Pression

K = 0.d0

! Debut de construction de la matrice elementaire
CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Recuperation des infos de l element fini poro mecanique
Nb_node_ther = get_N_NODE_THER_poroEF_iso(i)
Nb_node_meca = get_N_NODE_MECA_poroEF_iso(i)
Nb_dof_meca  = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther  = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de rigidite mecanique
ALLOCATE(K_meca(Nb_node_meca * Nb_dof_meca,Nb_node_meca * Nb_dof_meca))
K_meca(:,:) = 0.d0
! Allocation de la matrice de couplage
ALLOCATE(C_couplage(Nb_node_ther * Nb_dof_ther,Nb_node_meca * Nb_dof_meca))
C_couplage(:,:) = 0.d0
! Allocation des efforts internes mecanique
ALLOCATE(F_meca(Nb_node_meca * Nb_dof_meca))
F_meca(:) = 0.d0
! Allocation des efforts internes thermique
ALLOCATE(F_ther(Nb_node_ther * Nb_dof_ther))
F_ther(:) = 0.d0
ALLOCATE(C_ther(Nb_node_ther * Nb_dof_ther,Nb_node_ther * Nb_dof_ther))
C_ther(:,:) = 0.d0
! Allocation de la matrice de masse mecanique
ALLOCATE(M_meca(Nb_node_meca * Nb_dof_meca,Nb_node_meca * Nb_dof_meca))
M_meca(:,:) = 0.d0

CALL MASS_NAVIER_STOKES_ISO(i,ppsnb,ibdyty,iblmty,dt,X,V,M_meca)
! Appel de la partie mecanique avec Poisson = 0.0 et E = mu / 2.0
CALL BULK_HPP_ISO_MECA(poroEF(i)%mecaID,ppsnb,ibdyty,iblmty,dt,X,V, &
                       F_meca, .false., K_meca, .true., .false.)
! Appel de la partie couplage Biot = 1.0
CALL COUPLAGE_HPP_ISO(i,ppsnb,ibdyty,iblmty,dt,X,C_couplage)

! Partie concernant la mecanique structurelle
DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_meca * Nb_dof_meca
       K(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%meca_2_poro(j_dof)) = K_meca(i_dof,j_dof) + M_meca(i_dof,j_dof)
    ENDDO
ENDDO

! Appel de la partie incompressibilite
CALL CAPACITY_ISO_THER(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),P,ibdyty,iblmty,F_ther,C_ther)

! Partie concernant la condition incompressibilite
DO i_dof=1,Nb_node_ther * Nb_dof_ther
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       K(poroEF(i)%ther_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) =   C_ther(i_dof,j_dof)
    ENDDO
ENDDO

! Partie concernant couplage

DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       K(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) = - C_couplage(j_dof,i_dof)
       K(poroEF(i)%ther_2_poro(j_dof), poroEF(i)%meca_2_poro(i_dof)) =   C_couplage(j_dof,i_dof)
    ENDDO
ENDDO

DEALLOCATE(M_meca,F_meca,C_couplage,K_meca,F_ther,C_ther)

END SUBROUTINE DAMPING_ISO_FLUID


!------------------------------------------------------------------------------!
!    Calcul de la matrice de damping elementaire        [Ke]=[0    ; -C ] [V]         !
!                                                            [C^T  ;  D ] [P]         !
!------------------------------------------------------------------------------!
SUBROUTINE DAMPING_ISO_SOLID(i,ppsnb,dt,X,U,P,ibdyty,iblmty,K)


IMPLICIT NONE

INTEGER        , INTENT(IN)     :: i ! le numero de l'element dans la liste locale
INTEGER                         :: ibdyty,iblmty,mdlnb,lawnb
integer,dimension(:),intent(in) :: ppsnb
REAL(KIND=LONG),DIMENSION(:,:)  :: X,U ! coordonnees de depart,deplacement total
REAL(KIND=LONG),DIMENSION(:)    :: P
REAL(KIND=LONG),DIMENSION(:,:)  :: K     ! matrice de rig. elem.
REAL(KIND=LONG)                 :: dt
INTEGER                         :: Nb_node_ther, Nb_node_meca, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE    :: D_ther(:,:)     ! matrice de conduction. elem. de la ther
REAL(KIND=LONG), ALLOCATABLE    :: F_ther(:)       ! matrice de conduction. elem. de la ther
REAL(KIND=LONG), ALLOCATABLE    :: C_couplage(:,:) ! matrice de couplage Structure Pression

                        !12345678901234567890123456789012
CHARACTER(len=32):: IAM='a_poro_EF_iso::damping_iso_solid'

K = 0.d0

! Debut de construction de la matrice elementaire
CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Recuperation des infos de l element fini poro mecanique
Nb_node_ther = get_N_NODE_THER_poroEF_iso(i)
Nb_node_meca = get_N_NODE_MECA_poroEF_iso(i)
Nb_dof_meca  = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther  = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de conduction thermique
ALLOCATE(D_ther(Nb_node_ther * Nb_dof_ther,Nb_node_ther * Nb_dof_ther))
D_ther(:,:) = 0.d0
! Allocation des flux internes
ALLOCATE(F_ther(Nb_node_ther * Nb_dof_ther))
F_ther(:) = 0.d0
! Allocation de la matrice de couplage
ALLOCATE(C_couplage(Nb_node_ther * Nb_dof_ther,Nb_node_meca * Nb_dof_meca))
C_couplage(:,:) = 0.d0

! Appel de la partie diffusive 

SELECT CASE(get_eleop_value(mdlnb,'kine_'))
       CASE('small')
          CALL CONDUCTIVITY_ISO_THER(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),&
                                     P,ibdyty,iblmty, &
                                     F_ther, .false., D_ther, .true., .false.)
       CASE('large')
         CALL CONDUCTIVITY_ISO_GD_THER(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)), &
                                       U(:,1:get_N_NODE_THER_poroEF_iso(i)),P,ibdyty,iblmty, &
                                       F_ther, .false., D_ther, .true., .false.)
       CASE default
            CALL logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
            CALL FATERR(IAM,'kinematic type unknown (small | large)')
END SELECT


! Appel de la partie couplage Biot = 1.0
SELECT CASE(get_eleop_value(mdlnb,'kine_'))
       CASE('small')
            CALL COUPLAGE_HPP_ISO(i,ppsnb,ibdyty,iblmty,dt,X,C_couplage)
       CASE('large')
            CALL COUPLAGE_GD_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U,C_couplage)
       CASE default
            CALL logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
            CALL FATERR(IAM,'kinematic type unknown (small | large)')
END SELECT

! Partie concernant la conduction de la thermique
DO i_dof=1,Nb_node_ther * Nb_dof_ther
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       K(poroEF(i)%ther_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) = D_ther(i_dof,j_dof)
    ENDDO
ENDDO

! Partie concernant couplage
DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       K(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) = - C_couplage(j_dof,i_dof)
       K(poroEF(i)%ther_2_poro(j_dof), poroEF(i)%meca_2_poro(i_dof)) =   C_couplage(j_dof,i_dof)
    ENDDO
ENDDO

!Done in assemb_RHS
!do i_dof=1,Nb_node_meca * Nb_dof_meca
!  F(poroEF(i)%meca_2_poro(i_dof)) = dot_product( C_couplage(:,i_dof),P(:) )
!end do
!do i_dof=1,Nb_node_ther * Nb_dof_ther
!  F(poroEF(i)%ther_2_poro(i_dof)) = F_ther(i_dof) - dot_product( C_couplage(i_dof,:), reshape(V,(/size(V)/)) )
!end do

DEALLOCATE(F_ther,C_couplage,D_ther)

END SUBROUTINE DAMPING_ISO_SOLID

!------------------------------------------------------------------------------!
!    Calcul de la matrice de damping elementaire        [Ke]=[0    ; -C   ] [V]         !
!                                                            [C^T  ;  Cpp ] [P]         !
!------------------------------------------------------------------------------!
SUBROUTINE DAMPING_ISO_INCOMPRESSIBLE(i,ppsnb,dt,X,U,P,ibdyty,iblmty,K)


IMPLICIT NONE

INTEGER        , INTENT(IN)     :: i ! le numero de l'element dans la liste locale
INTEGER                         :: ibdyty,iblmty,mdlnb,lawnb
integer,dimension(:),intent(in) :: ppsnb
REAL(KIND=LONG),DIMENSION(:,:)  :: X,U ! coordonnees de depart,deplacement total
REAL(KIND=LONG),DIMENSION(:)    :: P
REAL(KIND=LONG),DIMENSION(:,:)  :: K     ! matrice de rig. elem.
REAL(KIND=LONG)                 :: dt
INTEGER                         :: Nb_node_ther, Nb_node_meca, i_dof, j_dof, Nb_dof_meca, Nb_dof_ther, Nb_dof
REAL(KIND=LONG), ALLOCATABLE    :: D_ther(:,:)     ! matrice de conduction. elem. de la ther
REAL(KIND=LONG), ALLOCATABLE    :: F_ther(:)       ! matrice de conduction. elem. de la ther
REAL(KIND=LONG), ALLOCATABLE    :: C_couplage(:,:) ! matrice de couplage Structure Pression

                        !12345678901234567890123456789012
CHARACTER(len=32):: IAM='a_poro_EF_iso::damping_iso_incom'

! Debut de construction de la matrice elementaire
CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

! Recuperation des infos de l element fini poro mecanique
Nb_node_ther = get_N_NODE_THER_poroEF_iso(i)
Nb_node_meca = get_N_NODE_MECA_poroEF_iso(i)
Nb_dof_meca  = get_N_DOF_by_NODE_MECA_poroEF_iso(i)
Nb_dof_ther  = get_N_DOF_by_NODE_THER_poroEF_iso(i)

! Allocation de la matrice de conduction thermique
ALLOCATE(D_ther(Nb_node_ther * Nb_dof_ther,Nb_node_ther * Nb_dof_ther))
D_ther(:,:) = 0.d0
! Allocation des flux internes
ALLOCATE(F_ther(Nb_node_ther * Nb_dof_ther))
F_ther(:) = 0.d0
! Allocation de la matrice de couplage
ALLOCATE(C_couplage(Nb_node_ther * Nb_dof_ther,Nb_node_meca * Nb_dof_meca))
C_couplage(:,:) = 0.d0

! Appel de la partie incompressibilite
CALL CAPACITY_ISO_THER(poroEF(i)%therID,ppsnb,dt,X(:,1:get_N_NODE_THER_poroEF_iso(i)),P,ibdyty,iblmty,F_ther,D_ther)

! Appel de la partie couplage Biot = 1.0
SELECT CASE(get_eleop_value(mdlnb,'kine_'))
       CASE('small')
            CALL COUPLAGE_HPP_ISO(i,ppsnb,ibdyty,iblmty,dt,X,C_couplage)
       CASE('large')
            CALL COUPLAGE_GD_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U,C_couplage)
       CASE default
            CALL logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
            CALL FATERR(IAM,'kinematic type unknown (small | large)')
END SELECT

! Partie concernant la conduction de la thermique
DO i_dof=1,Nb_node_ther * Nb_dof_ther
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       K(poroEF(i)%ther_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) =  D_ther(i_dof,j_dof)
    ENDDO
ENDDO

! Partie concernant couplage
DO i_dof=1,Nb_node_meca * Nb_dof_meca
    DO j_dof=1,Nb_node_ther * Nb_dof_ther
       K(poroEF(i)%meca_2_poro(i_dof), poroEF(i)%ther_2_poro(j_dof)) = - C_couplage(j_dof,i_dof)
       K(poroEF(i)%ther_2_poro(j_dof), poroEF(i)%meca_2_poro(i_dof)) =   C_couplage(j_dof,i_dof)
    ENDDO
ENDDO

DEALLOCATE(F_ther,C_couplage,D_ther)

END SUBROUTINE DAMPING_ISO_INCOMPRESSIBLE

!------------------------------------------------------------------------
SUBROUTINE gpv2node_3D_iso(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_stored
  
  
  CALL gpv2node_3D_iso_meca(poroEF(i)%mecaID,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)


END SUBROUTINE gpv2node_3D_iso
!------------------------------------------------------------------------

!------------------------------------------------------------------------
SUBROUTINE gpv2node_2D_iso(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_stored
  
  
  CALL gpv2node_2D_iso_meca(poroEF(i)%mecaID,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)


END SUBROUTINE gpv2node_2D_iso

!------------------------------------------------------------------------
SUBROUTINE gpv2nodeP_3D_iso(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==gradient P, 2==darcy flux
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_stored
  
  
  CALL gpv2node_iso_ther(poroEF(i)%therID,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

END SUBROUTINE gpv2nodeP_3D_iso
!------------------------------------------------------------------------

!*************************************************************************
INTEGER FUNCTION get_local_connectivity_edge_poroEF_iso(i,nodes,num)

  IMPLICIT NONE
  INTEGER :: i, nodes,num

  get_local_connectivity_edge_poroEF_iso = poroEF(i)%edge_2_vertex(nodes,num)
  
END FUNCTION get_local_connectivity_edge_poroEF_iso
!------------------------------------------------------------------------
!*************************************************************************
! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE add_elementary_load_ISO(i,ppsnb,dt,X,U,Vec,ibdyty,iblmty,Fint,ideriv)

implicit none
INTEGER        , INTENT(IN)     :: i,ideriv       ! le numero de l'element dans la liste locale
integer,dimension(:),intent(in) :: ppsnb
INTEGER        , INTENT(IN)     :: ibdyty,iblmty
real(kind=8)                    :: dt
REAL(KIND=LONG)                 :: X(:,:),U(:,:) ! coordonnees des sommets
REAL(KIND=LONG)                 :: Vec(:,:) ! Vecteur dont on veut la divergence
REAL(KIND=LONG),DIMENSION(:)    :: Fint 
                        !12345678901234567890123456789012345678
character(len=38):: IAM='a_poro_EF_iso::add_elementary_load_ISO'
integer :: mdlnb,inull

CALL get_ppset_value(ppsnb(1),mdlnb,inull)
    select case(get_eleop_value(mdlnb,'type_'))
      case('solid')
        select case(get_eleop_value(mdlnb,'kine_'))
        case('small')
            call ADD_LOAD_ISO_HPP(i,ppsnb,dt,X,U,Vec,ibdyty,iblmty,Fint,ideriv)
        case('large')
            call ADD_LOAD_ISO_GD(i,ppsnb,dt,X,U,Vec,ibdyty,iblmty,Fint,ideriv)
        case default
            call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
        call FATERR(IAM,'kinematic type unknown (small | large)')
        end select
     case('fluid')
        select case(get_eleop_value(mdlnb,'kine_'))
        case('small')
            !call div_hpp_iso_meca(i,ppsnb,dt,X,U,Vec,ibdyty,iblmty,Fint)
        case('large')
            call logMES('this option - large - is not implemented for fluid model')
        case default
            call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
        call FATERR(IAM,'kinematic type unknown (small | large)')
        end select
     
     case default
        call logMES('type=='//get_eleop_value(mdlnb,'type_'))
        call FATERR(IAM,'type unknown (solid | fluid)')
     end select
END SUBROUTINE
!----------------------------------------------------------------------------!
! DA : Utilise pour appliquer des charges locales externes (effets osmotique )
!---------------------------------------------------------------------------------------------------!
!    Calcul du vecteur de charge locale egal au gradient d'un field scalaire externe                !
!                                         ou a la valeur vectorielle d'un field vectoriel externe   !  
!---------------------------------------------------------------------------------------------------!
SUBROUTINE ADD_LOAD_ISO_HPP(i,ppsnb,dt,X,U,Vec,ibdyty,iblmty,Fint,ideriv)

  IMPLICIT NONE

  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty       ! le numero de l'element dans la liste locale
  INTEGER                     :: IG, ideriv
  real(kind=long)             :: dt            !
  REAL(KIND=LONG)             :: X(:,:),U(:,:) ! coordonnees des sommets
  REAL(KIND=LONG)             :: Vec(:,:) ! Vecteur load
  REAL(KIND=LONG)             :: COEFINT,R
  REAL(KIND=LONG),DIMENSION(:):: Fint
  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:) , & !   
                                     DIV(:,:) , & ! matrice DIV
                                     N(:,:)   , & ! matrice N
                                     B(:,:)       ! matrice B
  !REAL(kind=long)                 :: J
                                     
  !REAL(KIND=LONG), POINTER        :: F_T(:,:)
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: Vecteur
                                     
  !REAL(KIND=LONG), ALLOCATABLE    :: P(:)        ! vecteur de test
                          !1234567890123456789012345678901
  character(len=31):: IAM='a_poro_EF_iso::ADD_LOAD_ISO_HPP'
  
  integer,dimension(:) :: ppsnb

  IF (ideriv==1) THEN
      ALLOCATE(Vecteur(get_N_NODE_MECA_poroEF_iso(i)*nbDIME))
      Vecteur = 0.d0
      Vecteur = reshape(Vec,(/get_N_NODE_MECA_poroEF_iso(i)*nbDIME/))
  ENDIF
  IF (ideriv==0) THEN
      ALLOCATE(Vecteur(get_N_NODE_MECA_poroEF_iso(i)))
      Vecteur = 0.d0
      Vecteur = Vec(1,:)
  ENDIF

  Fint = 0.D0

  ! Pour tous les points de Gauss
  DO IG=1,get_N_PG_RIG_poroEF(i)
     
     ! Initialisation a vide des pointeurs
     NULLIFY(N,DNX)
     ! Appel au fonctions de formes de la meca
     ! Construction de la matrice des fonctions de forme thermique
     CALL N_U_ISO(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,N)
     ! Appel aux gradient des fonctions de formes de la meca
     CALL GRADIENT_ISO('MECA',i,ig,X,DNX,COEFINT,R)
     
     ! Construction de la matrice divergence
     IF (ideriv==0) THEN
        NULLIFY(B)
        CALL GRAD_P_ISO(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,B)
        Fint = Fint +  MATMUL(TRANSPOSE(N),MATMUL(B,Vecteur))*COEFINT
        
        DEALLOCATE(B); NULLIFY(B)
     ENDIF
     
     ! Construction de la matrice fonction de forme
     IF (ideriv==1) THEN

        Fint = Fint +  MATMUL(TRANSPOSE(N),MATMUL(N,Vecteur))*COEFINT
     
     ENDIF
     
     DEALLOCATE(N,DNX); NULLIFY(N,DNX)

  ENDDO

END SUBROUTINE  ADD_LOAD_ISO_HPP

! ------------------------------------------------------------------------------
! DA : Utilise pour appliquer des charges locales externes (effets osmotique )
!---------------------------------------------------------------------------------------------------!
!    Calcul du vecteur de charge locale egal au gradient d'un field scalaire externe                !
!                                         ou a la valeur vectorielle d'un field vectoriel externe   !  
!---------------------------------------------------------------------------------------------------!
SUBROUTINE ADD_LOAD_ISO_GD(i,ppsnb,dt,X,U,Vec,ibdyty,iblmty,Fint,ideriv)

  IMPLICIT NONE

  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty       ! le numero de l'element dans la liste locale
  INTEGER                     :: IG, ideriv
  real(kind=long)             :: dt            !
  REAL(KIND=LONG)             :: X(:,:),U(:,:) ! coordonnees des sommets
  REAL(KIND=LONG)             :: Vec(:,:) ! Vecteur load
  REAL(KIND=LONG)             :: COEFINT,R
  REAL(KIND=LONG),DIMENSION(:):: Fint
  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:) , & !   
                                     DIV(:,:) , & ! matrice DIV
                                     N(:,:)   , & ! matrice N
                                     B(:,:)       ! matrice B
  REAL(kind=long)                 :: J
                                     
  REAL(KIND=LONG), POINTER        :: F_T(:,:)
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: Vecteur

                          !1234567890123456789012345678901
  character(len=31):: IAM='a_poro_EF_iso::ADD_LOAD_ISO_HPP'
  
  integer,dimension(:) :: ppsnb

  IF (ideriv==1) THEN
      ALLOCATE(Vecteur(get_N_NODE_MECA_poroEF_iso(i)*nbDIME))
      Vecteur = 0.d0
      Vecteur = reshape(Vec,(/get_N_NODE_MECA_poroEF_iso(i)*nbDIME/))
  ENDIF
  IF (ideriv==0) THEN
      ALLOCATE(Vecteur(get_N_NODE_MECA_poroEF_iso(i)))
      Vecteur = 0.d0
      Vecteur = Vec(1,:)
  ENDIF

  Fint = 0.D0

  ! Pour tous les points de Gauss
  DO IG=1,get_N_PG_RIG_poroEF(i)
     
     ! Initialisation a vide des pointeurs
     NULLIFY(N,DNX)
     ! Appel au fonctions de formes de la meca
     ! Construction de la matrice des fonctions de forme thermique
     CALL N_U_ISO(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,N)
     ! Appel aux gradient des fonctions de formes de la meca
     CALL GRADIENT_ISO('MECA',i,ig,X,DNX,COEFINT,R)
     ! Calcul du gradient de la transformation F = I + grad U
     CALL  GRAD_TRANSFORMATION(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,U,F_T,J)
     
     ! Construction de la matrice divergence
     IF (ideriv==0) THEN
        NULLIFY(B)
        
        CALL GRAD_P_GD_ISO(get_N_NODE_MECA_poroEF_iso(i),poroEF(i)%PG_MECA(ig)%N,DNX,R,B)
        Fint = Fint +  MATMUL(TRANSPOSE(N),MATMUL(MATMUL(F_T,B),Vecteur))*COEFINT*J
        
        DEALLOCATE(B); NULLIFY(B)
     ENDIF
     
     ! Construction de la matrice fonction de forme
     IF (ideriv==1) THEN

        Fint = Fint +  MATMUL(TRANSPOSE(N),MATMUL(N,Vecteur))*COEFINT*J
     
     ENDIF
     
     DEALLOCATE(N,DNX); NULLIFY(N,DNX)

  ENDDO

END SUBROUTINE  ADD_LOAD_ISO_GD

! ------------------------------------------------------------------------------

SUBROUTINE GRAD_P_ISO(N_NE,N,DNX,R,B)

! computes the GRAD_P matrix (gradient of scalar value) at a gauss point

!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BI  = [ dNI / dX                                                    !
!                  dNI / dY  ]                                                 !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX                                                    !
!                  dNI / dY                                                    !
!                  dNI / dZ ]                                                  !
!                                                                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: DNX(:,:)
REAL(KIND=LONG)                :: R
REAL(KIND=LONG), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF
SELECT CASE(DIME_mod)
 
 CASE(i_2D_strain)
   ALLOCATE(B(2,N_NE))
   DO I=1,N_NE
      B(1,I) = DNX(1,I)
      B(2,I) = DNX(2,I)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(2,N_NE))
   DO I=1,N_NE
      B(1,I) = DNX(1,I)
      B(2,I) = DNX(2,I)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(3,N_NE))
   DO I=1,N_NE
      B(1,I) = DNX(1,I)
      B(2,I) = DNX(2,I)
      B(3,I) = DNX(3,I)
   ENDDO
 CASE DEFAULT
   call faterr('a_poroEF_iso::GRAD_P_ISO','not supported dime: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE GRAD_P_ISO
! ------------------------------------------------------------------------------

SUBROUTINE GRAD_P_GD_ISO(N_NE,N,DNX,R,B)

! computes the GRAD_P matrix (gradient of scalar value) at a gauss point

!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BI  = [ dNI / dX                                                    !
!                  dNI / dY  ]                                                 !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX                                                    !
!                  dNI / dY                                                    !
!                  dNI / dZ ]                                                  !
!                                                                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=LONG)                :: N(:)
REAL(KIND=LONG), POINTER       :: DNX(:,:)
REAL(KIND=LONG)                :: R
REAL(KIND=LONG), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF
SELECT CASE(DIME_mod)
 
 CASE(i_2D_strain)
   ALLOCATE(B(4,N_NE))
   DO I=1,N_NE
      B(1,I) = DNX(1,I)
      B(2,I) = DNX(1,I)
      B(3,I) = DNX(2,I)
      B(4,I) = DNX(2,I)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(4,N_NE))
   DO I=1,N_NE
      B(1,I) = DNX(1,I)
      B(2,I) = DNX(1,I)
      B(3,I) = DNX(2,I)
      B(4,I) = DNX(2,I)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(9,N_NE))
   DO I=1,N_NE
      B(1,I) = DNX(1,I)
      B(2,I) = DNX(1,I)
      B(3,I) = DNX(1,I)
      B(4,I) = DNX(2,I)
      B(5,I) = DNX(2,I)
      B(6,I) = DNX(2,I)
      B(7,I) = DNX(3,I)
      B(8,I) = DNX(3,I)
      B(9,I) = DNX(3,I)
   ENDDO
 CASE DEFAULT
   call faterr('a_poroEF_iso::GRAD_P_GD_ISO','not supported dime: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE GRAD_P_GD_ISO
! ---------------------------------------------------------------------------------------------------

subroutine check_elementary_ppset_iso(i,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty

  !**
  integer                         :: ig  
  
  do IG=1,get_N_PG_RIG_poroEF(i)

    IF (get_eleop_value_bypps(ppsnb(ig),'isext') /= 'no___') THEN
      call check_external_ppset(ppsnb(ig)) 
    endif
   
  enddo   
  
end subroutine check_elementary_ppset_iso

END MODULE a_poroEF_iso
