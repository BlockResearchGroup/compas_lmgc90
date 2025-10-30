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
MODULE a_mecaEF_iso
! class Finite Element for mecanical problems
! Basic computations on Finite Elements
! this class defines data type and methods and type
!
! DOES NOT CONTAIN ANY DATA

use overall
USE utilities
USE algebra
USE a_MATRIX

USE a_EF

USE bulk_behaviour

USE models

USE ExternalModelsHandler

USE MAILx

USE user

IMPLICIT NONE

private

TYPE T_mecaEF_iso
   integer                   :: name
   INTEGER                   :: N_NODE
   INTEGER                   :: N_DOF_by_NODE
   integer(kind=4)           :: T_FONC_FORME
   INTEGER                   :: N_PG_RIG
   integer(kind=4)           :: SCH_GAUSS_RIG
   TYPE(T_PT_GAUSS), POINTER :: PG(:)
   INTEGER                   :: N_PG_MAS
   integer(kind=4)           :: SCH_GAUSS_MAS
   TYPE(T_PT_GAUSS), POINTER :: mPG(:)

   !fd 24/03/11 mapping matrices gp -> node (noeuds sommets)
   real(kind=8),pointer      :: gp2node(:,:) 
   !fd 24/03/11 mapping matrices node -> edge (noeuds cote)
   real(kind=8),pointer      :: node2edge(:,:) 

   !fd 06/02/13 ajouts vecteurs et matrices de travail
   real(kind=8),pointer      :: coor_ele(:),primal_ele(:),dual_ele(:),operator_ele(:,:)

END TYPE T_mecaEF_iso 

TYPE(T_mecaEF_iso),DIMENSION(15),PRIVATE :: mecaEF

integer, parameter, private :: i_t3xxx = 1
integer, parameter, private :: i_t6xxx = 2
integer, parameter, private :: i_q4xxx = 3
integer, parameter, private :: i_q8xxx = 4
integer, parameter, private :: i_q8rxx = 5
integer, parameter, private :: i_q4p0x = 6
integer, parameter, private :: i_h8xxx = 7
integer, parameter, private :: i_h20xx = 8
integer, parameter, private :: i_h20rx = 9
integer, parameter, private :: i_te4xx = 10
integer, parameter, private :: i_te4lx = 11
integer, parameter, private :: i_te10x = 12
integer, parameter, private :: i_pri6x = 13
integer, parameter, private :: i_pri15 = 14
integer, parameter, private :: i_t3lxx = 15

PUBLIC  get_nb_ele_iso, &
        init_mecaEF_iso, &
        get_NAME_mecaEF_iso, &
        get_N_NODE_mecaEF_iso, &
        get_N_DOF_by_NODE_mecaEF_iso, &
        get_N_DOF_mecaEF_iso, &
        get_N_DOF_of_NODE_mecaEF_iso, &
        get_bw_mecaEF_iso, &
        get_N_GP_mecaEF_iso, &
        get_coor_pg_ISO, &
        compute_elementary_bulk_ISO, &
        compute_elementary_fields_ISO, &
        compute_elementary_mass_ISO, &
        compute_elementary_volume_iso, &
        compute_elementary_jacobian_iso, &
        compute_elementary_center_iso, &
        ENERGY_ISO, &
        POWER_ISO, &
        Stress2Fint_ISO, &
        interpolate_node2pg_ISO, &
        gpv2node_2D_iso, &
        gpv2node_3D_iso, &
        gpv2element_3D_iso, &         
        get_gp_ptr_mecaEF_iso, &
        compute_elementary_field_divergence_iso, &
        compute_elementary_field_iso, &
        compute_elementary_ortho_frame_iso, &
        get_elementary_field_iso, &
        gp_external_field_3d_iso, &
        gpv_iso, &
        gpv_all_internal_iso, &
        element_internal_iso, &                
        get_ele_ptr_mecaEF_iso, &
        get_nearest_gp_mecaEF_iso, &
        check_elementary_ppset_iso

! used by poro
PUBLIC  get_N_PG_RIG_mecaEF , get_SCH_GAUSS_RIG_mecaEF, & 
        get_N_PG_MAS_mecaEF , get_SCH_GAUSS_MAS_mecaEF, &
        get_T_FONC_FORME_mecaEF, get_id_mecaef_iso, &
        GRADIENT_ISO, Bl_ISO, B_ISO, id_iso, &
        bulk_gd_iso, bulk_hpp_iso, &
        fields_hpp_iso, fields_gd_iso

CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_iso(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_iso=SIZE(mecaEF)

END FUNCTION get_nb_ele_iso

SUBROUTINE init_mecaEF_iso
  IMPLICIT NONE
  logical :: is_initialize = .false.
  INTEGER :: itempo,i,j,errare
  REAL(kind=long),POINTER :: CG(:,:),POIDS_ELE(:)
!                           12345678901234567890123456789
  CHARACTER(len=29) :: IAM='a_mecaEF_iso::init_mecaEF_iso'

  if( is_initialize ) return
!
! EF 2D
!
! T3
  mecaEF(i_t3xxx)%name          = i_t3xxx
  mecaEF(i_t3xxx)%N_NODE        = 3
  mecaEF(i_t3xxx)%N_DOF_by_NODE = 2
  mecaEF(i_t3xxx)%T_FONC_FORME  = i_T_P1
  mecaEF(i_t3xxx)%N_PG_RIG      = 3 
  mecaEF(i_t3xxx)%SCH_GAUSS_RIG = i_TR03
  mecaEF(i_t3xxx)%N_PG_MAS      = 3
  mecaEF(i_t3xxx)%SCH_GAUSS_MAS = i_TR03
! T3l pour pb lineaire
  mecaEF(i_t3lxx)%name          = i_t3lxx
  mecaEF(i_t3lxx)%N_NODE        = 3
  mecaEF(i_t3lxx)%N_DOF_by_NODE = 2
  mecaEF(i_t3lxx)%T_FONC_FORME  = i_T_P1
  mecaEF(i_t3lxx)%N_PG_RIG      = 1 
  mecaEF(i_t3lxx)%SCH_GAUSS_RIG = i_TR01
  mecaEF(i_t3lxx)%N_PG_MAS      = 3
  mecaEF(i_t3lxx)%SCH_GAUSS_MAS = i_TR03
! T6
  mecaEF(i_t6xxx)%name          = i_t6xxx
  mecaEF(i_t6xxx)%N_NODE        = 6
  mecaEF(i_t6xxx)%N_DOF_by_NODE = 2
  mecaEF(i_t6xxx)%T_FONC_FORME  = i_T_P2
  mecaEF(i_t6xxx)%N_PG_RIG      = 3
  mecaEF(i_t6xxx)%SCH_GAUSS_RIG = i_TR03
  mecaEF(i_t6xxx)%N_PG_MAS      = 6
  mecaEF(i_t6xxx)%SCH_GAUSS_MAS = i_TR06
! Q4
  mecaEF(i_q4xxx)%name          = i_q4xxx
  mecaEF(i_q4xxx)%N_NODE        = 4
  mecaEF(i_q4xxx)%N_DOF_by_NODE = 2
  mecaEF(i_q4xxx)%T_FONC_FORME  = i_Q_P1
  mecaEF(i_q4xxx)%N_PG_RIG      = 4 
  mecaEF(i_q4xxx)%SCH_GAUSS_RIG = i_Q2x2
  !fd wtf
  ! mecaEF(i_q4xxx)%N_PG_MAS      = 4
  ! mecaEF(i_q4xxx)%SCH_GAUSS_MAS = i_Q2x2
  mecaEF(i_q4xxx)%N_PG_MAS      = 9
  mecaEF(i_q4xxx)%SCH_GAUSS_MAS = i_Q3x3
! Q8
  mecaEF(i_q8xxx)%name          = i_q8xxx
  mecaEF(i_q8xxx)%N_NODE        = 8
  mecaEF(i_q8xxx)%N_DOF_by_NODE = 2
  mecaEF(i_q8xxx)%T_FONC_FORME  = i_Q_P2
  mecaEF(i_q8xxx)%N_PG_RIG      = 9 
  mecaEF(i_q8xxx)%SCH_GAUSS_RIG = i_Q3x3
  mecaEF(i_q8xxx)%N_PG_MAS      = 9
  mecaEF(i_q8xxx)%SCH_GAUSS_MAS = i_Q3x3
! Q8R
  mecaEF(i_q8rxx)%name          = i_q8rxx
  mecaEF(i_q8rxx)%N_NODE        = 8
  mecaEF(i_q8rxx)%N_DOF_by_NODE = 2
  mecaEF(i_q8rxx)%T_FONC_FORME  = i_Q_P2
  mecaEF(i_q8rxx)%N_PG_RIG      = 4
  mecaEF(i_q8rxx)%SCH_GAUSS_RIG = i_Q2x2
  mecaEF(i_q8rxx)%N_PG_MAS      = 9
  mecaEF(i_q8rxx)%SCH_GAUSS_MAS = i_Q3x3
 ! Q4P0
  mecaEF(i_q4p0x)%name          = i_q4p0x
  mecaEF(i_q4p0x)%N_NODE        = 4
  mecaEF(i_q4p0x)%N_DOF_by_NODE = 2
  mecaEF(i_q4p0x)%T_FONC_FORME  = i_Q_P1
  mecaEF(i_q4p0x)%N_PG_RIG      = 4 
  mecaEF(i_q4p0x)%SCH_GAUSS_RIG = i_Q2x2
  mecaEF(i_q4p0x)%N_PG_MAS      = 4
  mecaEF(i_q4p0x)%SCH_GAUSS_MAS = i_Q2x2      
! 
! EF 3D VOL
!
! H8
  mecaEF(i_h8xxx)%name          = i_h8xxx
  mecaEF(i_h8xxx)%N_NODE        = 8
  mecaEF(i_h8xxx)%N_DOF_by_NODE = 3
  mecaEF(i_h8xxx)%T_FONC_FORME  = i_H_P1
  mecaEF(i_h8xxx)%N_PG_RIG      = 8
  mecaEF(i_h8xxx)%SCH_GAUSS_RIG = i_H222
  ! fd a cause du field density on doit avoir les mêmes nb de pg en s et m
  ! mecaEF(i_h8xxx)%N_PG_MAS      = 27
  ! mecaEF(i_h8xxx)%SCH_GAUSS_MAS = i_H333
  mecaEF(i_h8xxx)%N_PG_MAS      = 8
  mecaEF(i_h8xxx)%SCH_GAUSS_MAS = i_H222
  
! H20
  mecaEF(i_h20xx)%name          = i_h20xx
  mecaEF(i_h20xx)%N_NODE        = 20
  mecaEF(i_h20xx)%N_DOF_by_NODE = 3
  mecaEF(i_h20xx)%T_FONC_FORME  = i_H_P2
  mecaEF(i_h20xx)%N_PG_RIG      = 27 
  mecaEF(i_h20xx)%SCH_GAUSS_RIG = i_H333
  mecaEF(i_h20xx)%N_PG_MAS      = 27 
  mecaEF(i_h20xx)%SCH_GAUSS_MAS = i_H333
 ! H20R
  mecaEF(i_h20rx)%name          = i_h20rx
  mecaEF(i_h20rx)%N_NODE        = 20
  mecaEF(i_h20rx)%N_DOF_by_NODE = 3
  mecaEF(i_h20rx)%T_FONC_FORME  = i_H_P2
  mecaEF(i_h20rx)%N_PG_RIG      = 8 
  mecaEF(i_h20rx)%SCH_GAUSS_RIG = i_H222
  mecaEF(i_h20rx)%N_PG_MAS      = 27
  mecaEF(i_h20rx)%SCH_GAUSS_MAS = i_H333
! TE4
! fd on utilise 4 points de Gauss comme CodeAster
! fd convient aux problemes non lineaires 
  mecaEF(i_te4xx)%name          = i_te4xx
  mecaEF(i_te4xx)%N_NODE        = 4
  mecaEF(i_te4xx)%N_DOF_by_NODE = 3
  mecaEF(i_te4xx)%T_FONC_FORME  = i_TEP1
  mecaEF(i_te4xx)%N_PG_RIG      = 4
  mecaEF(i_te4xx)%SCH_GAUSS_RIG = i_TE04
  mecaEF(i_te4xx)%N_PG_MAS      = 4
  mecaEF(i_te4xx)%SCH_GAUSS_MAS = i_TE04
! TE4l
  mecaEF(i_te4lx)%name          = i_te4lx
  mecaEF(i_te4lx)%N_NODE        = 4
  mecaEF(i_te4lx)%N_DOF_by_NODE = 3
  mecaEF(i_te4lx)%T_FONC_FORME  = i_TEP1
  mecaEF(i_te4lx)%N_PG_RIG      = 1
  mecaEF(i_te4lx)%SCH_GAUSS_RIG = i_TE01
  mecaEF(i_te4lx)%N_PG_MAS      = 4
  mecaEF(i_te4lx)%SCH_GAUSS_MAS = i_TE04
! TE10
  mecaEF(i_te10x)%name          = i_te10x
  mecaEF(i_te10x)%N_NODE        = 10
  mecaEF(i_te10x)%N_DOF_by_NODE = 3
  mecaEF(i_te10x)%T_FONC_FORME  = i_TEP2
  mecaEF(i_te10x)%N_PG_RIG      = 4 
  mecaEF(i_te10x)%SCH_GAUSS_RIG = i_TE04
  mecaEF(i_te10x)%N_PG_MAS      = 4 
  mecaEF(i_te10x)%SCH_GAUSS_MAS = i_TE04
! PRI6
  mecaEF(i_pri6x)%name          = i_pri6x
  mecaEF(i_pri6x)%N_NODE        = 6
  mecaEF(i_pri6x)%N_DOF_by_NODE = 3
  mecaEF(i_pri6x)%T_FONC_FORME  = i_PRP1
  mecaEF(i_pri6x)%N_PG_RIG      = 6
  mecaEF(i_pri6x)%SCH_GAUSS_RIG = i_PR06
  mecaEF(i_pri6x)%N_PG_MAS      = 6
  mecaEF(i_pri6x)%SCH_GAUSS_MAS = i_PR06
! PRI15
  mecaEF(i_pri15)%name          = i_pri15
  mecaEF(i_pri15)%N_NODE        = 15
  mecaEF(i_pri15)%N_DOF_by_NODE = 3
  mecaEF(i_pri15)%T_FONC_FORME  = i_PRP2
  mecaEF(i_pri15)%N_PG_RIG      = 6
  mecaEF(i_pri15)%SCH_GAUSS_RIG = i_PR06
  mecaEF(i_pri15)%N_PG_MAS      = 6
  mecaEF(i_pri15)%SCH_GAUSS_MAS = i_PR06


  NULLIFY(CG,POIDS_ELE)

  DO i=1,SIZE(mecaEF)

    ! concerning behaviour

    ALLOCATE(mecaEF(i)%PG(get_N_PG_RIG_mecaEF(i)),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating meca_ef%PG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_RIG_mecaEF(i),CG,POIDS_ELE)

    DO j=1,get_N_PG_RIG_mecaEF(i)

      mecaEF(i)%PG(j)%POIDS=POIDS_ELE(j)   

      NULLIFY(mecaEF(i)%PG(j)%N)
      CALL fonct_forme(get_T_FONC_FORME_mecaEF(i),CG(:,j),mecaEF(i)%PG(j)%N)

      NULLIFY(mecaEF(i)%PG(j)%DN)
      CALL derive_forme(get_T_FONC_FORME_mecaEF(i),CG(:,j),mecaEF(i)%PG(j)%DN)
    ENDDO

    ! concerning mass

    ALLOCATE(mecaEF(i)%mPG(get_N_PG_MAS_mecaEF(i)),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating meca_ef%mPG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_MAS_mecaEF(i),CG,POIDS_ELE)

    DO j=1,get_N_PG_MAS_mecaEF(i)

      mecaEF(i)%mPG(j)%POIDS=POIDS_ELE(j)   

      NULLIFY(mecaEF(i)%mPG(j)%N)
      CALL fonct_forme(get_T_FONC_FORME_mecaEF(i),CG(:,j),mecaEF(i)%mPG(j)%N)

      NULLIFY(mecaEF(i)%mPG(j)%DN)
      CALL derive_forme(get_T_FONC_FORME_mecaEF(i),CG(:,j),mecaEF(i)%mPG(j)%DN)

    ENDDO

    ! concerning mapping gp -> node

    SELECT CASE(mecaEF(i)%name)
    CASE(i_t3lxx,i_te4lx)
       allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG))
       mecaEF(i)%gp2node = 1.d0
       nullify(mecaEF(i)%node2edge)
    CASE(i_t3xxx,i_q4xxx,i_q4p0x,i_h8xxx,i_te4xx) !les lineaires
       allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG))
       if (mecaEF(i)%N_NODE == mecaEF(i)%N_PG_RIG) then
         mecaEF(i)%gp2node = 0.d0
         call compute_gp2node(mecaEF(i)%T_FONC_FORME,mecaEF(i)%N_NODE, &
                              mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                              mecaEF(i)%gp2node)
       else
         if (mecaEF(i)%N_PG_RIG == 1) then
           allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG))
           mecaEF(i)%gp2node = 1.d0
         else
            call faterr(IAM,'Impossible')
         endif
       endif
       nullify(mecaEF(i)%node2edge)
    CASE(i_pri6x)
       allocate(mecaEF(i)%gp2node(mecaEF(i)%N_NODE,mecaEF(i)%N_PG_RIG))
       mecaEF(i)%gp2node = 0.d0
       call compute_gp2node(mecaEF(i)%T_FONC_FORME,mecaEF(i)%N_NODE, &
                            mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                            mecaEF(i)%gp2node)
       nullify(mecaEF(i)%node2edge)
    CASE(i_t6xxx)
       allocate(mecaEF(i)%gp2node(3,mecaEF(i)%N_PG_RIG))
       call compute_gp2node(i_T_P1, 3, &
                            mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-3,3))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,1)=0.5d0
    CASE(i_q8xxx,i_q8rxx)
       allocate(mecaEF(i)%gp2node(4,mecaEF(i)%N_PG_RIG))
       call compute_gp2node(i_Q_P1, 4, &
                            mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-4,4))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,4)=0.5d0
       mecaEF(i)%node2edge(4,4)=0.5d0;mecaEF(i)%node2edge(4,1)=0.5d0
    CASE(i_h20xx,i_h20rx)
       allocate(mecaEF(i)%gp2node(8,mecaEF(i)%N_PG_RIG))
       call compute_gp2node(i_H_P1, 8, &
                            mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-8,8))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,4)=0.5d0
       mecaEF(i)%node2edge(4,4)=0.5d0;mecaEF(i)%node2edge(4,1)=0.5d0
       mecaEF(i)%node2edge(5,5)=0.5d0;mecaEF(i)%node2edge(5,6)=0.5d0
       mecaEF(i)%node2edge(6,6)=0.5d0;mecaEF(i)%node2edge(6,7)=0.5d0
       mecaEF(i)%node2edge(7,7)=0.5d0;mecaEF(i)%node2edge(7,8)=0.5d0
       mecaEF(i)%node2edge(8,8)=0.5d0;mecaEF(i)%node2edge(8,5)=0.5d0
       mecaEF(i)%node2edge(9,1)=0.5d0;mecaEF(i)%node2edge(9,5)=0.5d0
       mecaEF(i)%node2edge(10,2)=0.5d0;mecaEF(i)%node2edge(10,6)=0.5d0
       mecaEF(i)%node2edge(11,3)=0.5d0;mecaEF(i)%node2edge(11,7)=0.5d0
       mecaEF(i)%node2edge(12,4)=0.5d0;mecaEF(i)%node2edge(12,8)=0.5d0
    CASE(i_te10x)
       allocate(mecaEF(i)%gp2node(4,mecaEF(i)%N_PG_RIG))
       call compute_gp2node(i_TEP1, 4, &
                            mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-4,4))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,1)=0.5d0
       mecaEF(i)%node2edge(4,1)=0.5d0;mecaEF(i)%node2edge(4,4)=0.5d0
       mecaEF(i)%node2edge(5,2)=0.5d0;mecaEF(i)%node2edge(5,4)=0.5d0
       mecaEF(i)%node2edge(6,3)=0.5d0;mecaEF(i)%node2edge(6,4)=0.5d0
    CASE(i_pri15)
       allocate(mecaEF(i)%gp2node(6,mecaEF(i)%N_PG_RIG))
       call compute_gp2node(i_PRp1, 6, &
                            mecaEF(i)%SCH_GAUSS_RIG,mecaEF(i)%N_PG_RIG, &
                            mecaEF(i)%gp2node)
       allocate(mecaEF(i)%node2edge(mecaEF(i)%N_NODE-6,6))
       mecaEF(i)%node2edge=0.d0
       mecaEF(i)%node2edge(1,1)=0.5d0;mecaEF(i)%node2edge(1,2)=0.5d0
       mecaEF(i)%node2edge(2,2)=0.5d0;mecaEF(i)%node2edge(2,3)=0.5d0
       mecaEF(i)%node2edge(3,3)=0.5d0;mecaEF(i)%node2edge(3,1)=0.5d0
       mecaEF(i)%node2edge(4,4)=0.5d0;mecaEF(i)%node2edge(4,5)=0.5d0
       mecaEF(i)%node2edge(5,5)=0.5d0;mecaEF(i)%node2edge(5,6)=0.5d0
       mecaEF(i)%node2edge(6,6)=0.5d0;mecaEF(i)%node2edge(6,4)=0.5d0
       mecaEF(i)%node2edge(7,1)=0.5d0;mecaEF(i)%node2edge(7,4)=0.5d0
       mecaEF(i)%node2edge(8,2)=0.5d0;mecaEF(i)%node2edge(8,5)=0.5d0
       mecaEF(i)%node2edge(9,3)=0.5d0;mecaEF(i)%node2edge(9,6)=0.5d0
    CASE DEFAULT
      call FATERR(IAM,'gp2node can t be computed for this element')
    END SELECT

    allocate(mecaEF(i)%coor_ele(nbdime*mecaEF(i)%n_node), &
             mecaEF(i)%primal_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node), &
             mecaEF(i)%dual_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node), &
             mecaEF(i)%operator_ele(mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node,mecaEF(i)%n_dof_by_node*mecaEF(i)%n_node))
  ENDDO 


  IF(ASSOCIATED(CG)) DEALLOCATE(CG)
  IF(ASSOCIATED(POIDS_ELE)) DEALLOCATE(POIDS_ELE)

  is_initialize = .true.

END SUBROUTINE init_mecaEF_iso

function get_name_mecaEF_iso(nb)
  implicit none
  integer, intent(in) :: nb
  character(len=5) :: get_NAME_mecaEF_iso

  select case(nb)
  case( i_t3xxx )
    get_NAME_mecaEF_iso = 'T3xxx'
  case( i_t3lxx )
    get_NAME_mecaEF_iso = 'T3Lxx'
  case( i_t6xxx )
    get_NAME_mecaEF_iso = 'T6xxx'
  case( i_q4xxx )
    get_NAME_mecaEF_iso = 'Q4xxx'
  case( i_q8xxx )
    get_NAME_mecaEF_iso = 'Q8xxx'
  case( i_q8rxx )
    get_NAME_mecaEF_iso = 'Q8Rxx'
  case( i_q4p0x )
    get_NAME_mecaEF_iso = 'Q4P0x'
  case( i_h8xxx )
    get_NAME_mecaEF_iso = 'H8xxx'
  case( i_h20xx )
    get_NAME_mecaEF_iso = 'H20xx'
  case( i_h20rx )
    get_NAME_mecaEF_iso = 'H20Rx'
  case( i_te4xx )
    get_NAME_mecaEF_iso = 'TE4xx'
  case( i_te4lx )
    get_NAME_mecaEF_iso = 'TE4Lx'
  case( i_te10x )
    get_NAME_mecaEF_iso = 'TE10x'
  case( i_pri6x )
    get_NAME_mecaEF_iso = 'PRI6x'
  case( i_pri15 )
    get_NAME_mecaEF_iso = 'PRI15'
  case default
    get_NAME_mecaEF_iso = 'xxxxx'
  end select

end function get_NAME_mecaEF_iso

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_mecaEF_iso=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_mecaEF_iso

INTEGER FUNCTION get_N_NODE_mecaEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_mecaEF_iso=mecaEF(nb)%N_NODE

END FUNCTION get_N_NODE_mecaEF_iso


INTEGER FUNCTION get_bw_mecaEF_iso(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,i,j,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_mecaEF_iso=0
  DO i=1,mecaEF(nb)%N_NODE-1
    DO j=i+1,mecaEF(nb)%N_NODE
      tempo = ABS(nodes(i) - nodes(j))
      IF (tempo .GT. get_bw_mecaEF_iso) get_bw_mecaEF_iso=tempo
    ENDDO
  ENDDO

  get_bw_mecaEF_iso = (get_bw_mecaEF_iso+1)*mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_mecaEF_iso

INTEGER FUNCTION get_N_GP_mecaEF_iso(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_mecaEF_iso=mecaEF(nb)%N_PG_RIG

END FUNCTION get_N_GP_mecaEF_iso

! ------------------------------------------------------------------------------
!> computes interpolation function, derivative of interpolation function,
!> reference coordinates, etc
SUBROUTINE GRADIENT_ISO(N_NE,N,DN,POIDS,X,DNX,COEFINT,R)

 IMPLICIT NONE

 ! input ...
 ! number of nodes
 INTEGER     , INTENT(IN)  :: N_NE
 REAL(KIND=8), INTENT(IN)  :: N(:),DN(:,:)
 REAL(KIND=8), INTENT(IN)  :: POIDS

 ! coordinates
 REAL(KIND=8), INTENT(IN)  :: X(:,:)

 ! output ...
 REAL(KIND=8), POINTER     :: DNX(:,:)
 REAL(KIND=8), INTENT(OUT) :: COEFINT
 REAL(KIND=8), INTENT(OUT) :: R

! Variables locales
 INTEGER                   :: I
 ! matrice jacobienne de la transformation geometrique (ref -> actuel)
 REAL(KIND=8), ALLOCATABLE :: J(:,:)
 ! determinant de la jacobienne
 REAL(KIND=8)              :: DETJ
 ! inverse de la jacobienne
 REAL(KIND=8), ALLOCATABLE :: INVJ(:,:)
!                           12345678901234567890123456
  CHARACTER(len=26) :: IAM='a_mecaEF_iso::gradient_iso'

! Initialisation a vide des nouveaux pointeurs
IF (ASSOCIATED(DNX)) THEN ; DEALLOCATE(DNX) ; NULLIFY(DNX) ; ENDIF

SELECT CASE(DIME_mod)               
 CASE(i_2D_strain,i_2D_stress,i_2D_axisym )

   ALLOCATE(J(2,2),INVJ(2,2))
   J=ZERO
   
   ALLOCATE(DNX(2,N_NE))
   DNX=ZERO

   ! Calcul de la matrice Jacobienne, de son determinant, et de son inverse   

   J(1,:)=(/ DOT_PRODUCT(DN(1,:),X(1,:)) , DOT_PRODUCT(DN(1,:),X(2,:)) /)
   J(2,:)=(/ DOT_PRODUCT(DN(2,:),X(1,:)) , DOT_PRODUCT(DN(2,:),X(2,:)) /)

   DETJ=J(1,1)*J(2,2)-J(1,2)*J(2,1)

   IF (ABS(DETJ) < 1.E-16) call faterr(IAM,'Matrice Jacobienne non inv. ')

   INVJ(1,:)=(/  J(2,2)/DETJ ,-J(1,2)/DETJ /)
   INVJ(2,:)=(/ -J(2,1)/DETJ , J(1,1)/DETJ /)

   COEFINT=DETJ*POIDS
   
 CASE(i_3D)

   ALLOCATE(J(3,3),INVJ(3,3))
   J=ZERO
   
   ALLOCATE(DNX(3,N_NE)) 
   DNX=ZERO

   ! Calcul de la matrice Jacobienne, de son determinant, et de son inverse

   J(1,:)=(/DOT_PRODUCT(DN(1,:),X(1,:)),DOT_PRODUCT(DN(1,:),X(2,:)),DOT_PRODUCT(DN(1,:),X(3,:)) /)
   J(2,:)=(/DOT_PRODUCT(DN(2,:),X(1,:)),DOT_PRODUCT(DN(2,:),X(2,:)),DOT_PRODUCT(DN(2,:),X(3,:)) /)
   J(3,:)=(/DOT_PRODUCT(DN(3,:),X(1,:)),DOT_PRODUCT(DN(3,:),X(2,:)),DOT_PRODUCT(DN(3,:),X(3,:)) /)
   
   DETJ=  J(1,1)*(J(2,2)*J(3,3)-J(2,3)*J(3,2)) &
         -J(1,2)*(J(2,1)*J(3,3)-J(3,1)*J(2,3)) &
         +J(1,3)*(J(2,1)*J(3,2)-J(3,1)*J(2,2))     

   IF (ABS(DETJ).LT.1.E-16) call faterr(IAM,' Matrice Jacobienne non inversible')

   INVJ(1,1)= (J(2,2)*J(3,3)-J(3,2)*J(2,3))/DETJ
   INVJ(2,1)=-(J(2,1)*J(3,3)-J(3,1)*J(2,3))/DETJ
   INVJ(3,1)= (J(2,1)*J(3,2)-J(3,1)*J(2,2))/DETJ
   INVJ(1,2)=-(J(1,2)*J(3,3)-J(3,2)*J(1,3))/DETJ
   INVJ(2,2)= (J(1,1)*J(3,3)-J(3,1)*J(1,3))/DETJ
   INVJ(3,2)=-(J(1,1)*J(3,2)-J(3,1)*J(1,2))/DETJ
   INVJ(1,3)= (J(1,2)*J(2,3)-J(2,2)*J(1,3))/DETJ
   INVJ(2,3)=-(J(1,1)*J(2,3)-J(2,1)*J(1,3))/DETJ
   INVJ(3,3)= (J(1,1)*J(2,2)-J(2,1)*J(1,2))/DETJ
   
   COEFINT=DETJ*POIDS

CASE DEFAULT
    CALL FATERR(IAM,'Unknown DIME option')
END SELECT

! Calcul de la matrice des gradients au points (x,y)
DO I=1,N_NE
   DNX(:,I)=MATMUL(INVJ,DN(:,I))
ENDDO

! Calcul du rayon dans le cas axisymetrique

IF (DIME_mod == i_2D_axisym ) THEN

   ! Calcul du rayon
   R=DOT_PRODUCT(N,X(1,:))

   COEFINT=COEFINT*(6.2831853_LONG)*R

ENDIF

DEALLOCATE(J,INVJ)

END SUBROUTINE GRADIENT_ISO

! ------------------------------------------------------------------------------

SUBROUTINE  Bl_ISO(N_NE,N,DNX,R,istrg,Bl)

! computes the Bl matrix (symmetric deformation) at a gauss point

!istrg == 1 => voigt (a verifier pour le 3D !!) -- le stockage lmgc90
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   Bl est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BlI = [ dNI / dX       0                                            !
!                      0      dNI / dY                                         !
!                  dNI / dY   dNI / dX  ]                                      ! 
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                      0      dNI / dY       0                                 ! 
!                      0          0      dNI / dZ                              !
!                  dNI / dY   dNI / dX       0                                 !
!                      0      dNI / dZ   dNI / dY                              !
!                  dNI / dZ       0      dNI / dX  ]                           !  
!                                                                              !
!  EN AXI : BlI= [ dNI / dX       0                                            !
!                      0      dNI / dY                                         !
!                  dNI / dY   dNI / dX                                         !
!                   NI / R        0     ]                                      ! 
!                                                                              !
!------------------------------------------------------------------------------!


!istrg==2 => line by line and lower part (C-storage !!) -- le stockage matlib
!------------------------------------------------------------------------------!
!  Calcul de la matrice Bl pour les elements isoparametriques                  !
!  Calcule la matrice Bl      Bl=[ Bl1 , Bl2 ,  ....   ]                       !
!                                                                              !
!   BI est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BlI = [ dNI / dX       0                                            !
!                  dNI / dY   dNI / dX                                         ! 
!                      0      dNI / dY                                         !
!                      0          0    ]                                       !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                  dNI / dY   dNI / dX       0                                 !
!                      0      dNI / dY       0                                 ! 
!                  dNI / dZ       0      dNI / dX                              !  
!                      0      dNI / dZ   dNI / dY                              !
!                      0          0      dNI / dZ ]                            !
!                                                                              !
!  EN AXI : BlI= [ dNI / dX       0                                            !
!                  dNI / dY   dNI / dX                                         !
!                      0      dNI / dY                                         !
!                   NI / R        0     ]                                      ! 
!                                                                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER     , INTENT(IN)    :: N_NE        ! number of nodes
REAL(KIND=8)                :: N(:)        ! array containing interpolation polynome value at gauss point
REAL(KIND=8), POINTER       :: DNX(:,:)    ! array containing the derivatives of interpolation function at gauss point
REAL(KIND=8)                :: R           ! radius of the gauss point (axi) 
INTEGER                     :: istrg       ! the way you want to store the deformation voigt=1, stainier=2
REAL(KIND=8), POINTER       :: Bl(:,:)     ! 

! Variable locale
INTEGER                        :: I
                                      !12345678901234567890 
CHARACTER(len=20)              :: IAM='a_mecaEF_iso::Bl_iso'

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(Bl)) THEN ; DEALLOCATE(Bl) ; NULLIFY(Bl) ; ENDIF

SELECT CASE(DIME_mod)

 CASE(i_2D_stress,i_2D_strain)

   SELECT CASE (istrg)
   CASE(1)
     ALLOCATE(Bl(3,2*N_NE))
     DO I=1,N_NE
        Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
        Bl(2,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
        Bl(3,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
     ENDDO
   CASE(2)
     ALLOCATE(Bl(4,2*N_NE))
     DO I=1,N_NE
        Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
        Bl(2,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
        Bl(3,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
        Bl(4,2*(I-1)+1:2*I) = (/   ZERO     ,   ZERO      /)
     ENDDO
   CASE default
     CALL FATERR(IAM,'error allocating Bl')
   END SELECT 

 CASE(i_2D_axisym)
   SELECT CASE (istrg)
   CASE(1)
     ALLOCATE(Bl(4,2*N_NE))
     DO I=1,N_NE
        Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
        Bl(2,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
        Bl(3,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
        Bl(4,2*(I-1)+1:2*I) = (/  (N(I)/R)  ,   ZERO      /)
     ENDDO
   CASE(2)
     ALLOCATE(Bl(4,2*N_NE))
     DO I=1,N_NE
        Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
        Bl(2,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
        Bl(3,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
        Bl(4,2*(I-1)+1:2*I) = (/  (N(I)/R)  ,   ZERO      /)
     ENDDO
   CASE default
     CALL FATERR(IAM,'error allocating Bl')
   END SELECT 
   
 CASE(i_3D)
   SELECT CASE (istrg)
   CASE(1)
     ALLOCATE(Bl(6,3*N_NE))
     DO I=1,N_NE
        Bl(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
        Bl(2,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
        Bl(3,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
        Bl(4,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,  DNX(1,I)   ,   ZERO      /)
        Bl(5,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(3,I)   ,  DNX(2,I)   /)
        Bl(6,3*(I-1)+1:3*I) = (/  DNX(3,I)  ,   ZERO      ,  DNX(1,I)   /)      
     ENDDO
   CASE(2)
   ALLOCATE(Bl(6,3*N_NE))
     DO I=1,N_NE
        Bl(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
        Bl(2,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,  DNX(1,I)   ,   ZERO      /)
        Bl(3,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
        Bl(4,3*(I-1)+1:3*I) = (/  DNX(3,I)  ,   ZERO      ,  DNX(1,I)   /)      
        Bl(5,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(3,I)   ,  DNX(2,I)   /)
        Bl(6,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
     ENDDO
   CASE default
     CALL FATERR(IAM,'error allocating Bl')
   END SELECT 

 CASE DEFAULT
   call faterr(IAM,'DIME not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE Bl_ISO

! ------------------------------------------------------------------------------

SUBROUTINE B_ISO(N_NE,N,DNX,R,B)

! computes the B matrix (gradient of deformation)  at a gauss point
! lmgc90 and matlib are the same
!------------------------------------------------------------------------------!
!  Calcul de la matrice B DN/DX pour les elements isoparametriques             !
!  B=[ Bl1 , Bl2 ,  ....   ]                                                   !
!                                                                              !
!   B  est forme grace au tableau DNX et au filtres qui permettent             ! 
!   de replacer les termes a leur place                                        !
!                                                                              !
!  EN 2D : BI  = [ dNI / dX       0                                            !
!                  dNI / dY       0                                            ! 
!                      0      dNI / dX                                         !
!                      0      dNI / dY                                         !
!                      0          0    ]                                       !
!                                                                              !
!  EN 3D : BlI = [ dNI / dX       0          0                                 !
!                  dNI / dY       0          0                                 !
!                  dNI / dZ       0          0                                 ! 
!                      0      dNI / dX       0                                 !  
!                      0      dNI / dY       0                                 !
!                      0      dNI / dZ       0                                 !
!                      0          0      dNI / dX                              !  
!                      0          0      dNI / dY                              !
!                      0          0      dNI / dZ ]                            !
!                                                                              !
!  EN AXI :BI  = [ dNI / dX       0                                            !
!                  dNI / dY       0                                            ! 
!                      0      dNI / dX                                         !
!                      0      dNI / dY                                         !
!                   NI / R        0    ]                                       ! 
!                                                                              !
!------------------------------------------------------------------------------!

IMPLICIT NONE
INTEGER        , INTENT(IN)    :: N_NE
REAL(KIND=8)                :: N(:)
REAL(KIND=8), POINTER       :: DNX(:,:)
REAL(KIND=8)                :: R
REAL(KIND=8), POINTER       :: B(:,:)

! Variable locale
INTEGER                        :: I

! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(B)) THEN ; DEALLOCATE(B) ; NULLIFY(B) ; ENDIF

SELECT CASE(DIME_mod)

 CASE(i_2D_strain)
   ALLOCATE(B(5,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,   ZERO      /)
      B(3,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(1,I)   /)
      B(4,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
      B(5,2*(I-1)+1:2*I) = (/   ZERO     ,   ZERO      /)
   ENDDO
 CASE(i_2D_axisym)
   ALLOCATE(B(5,2*N_NE))
   DO I=1,N_NE
      B(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   ZERO      /)
      B(2,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,   ZERO      /)
      B(3,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(1,I)   /)
      B(4,2*(I-1)+1:2*I) = (/   ZERO     ,  DNX(2,I)   /)
      B(5,2*(I-1)+1:2*I) = (/  (N(I)/R)  ,   ZERO      /)
   ENDDO
   
 CASE(i_3D)
   ALLOCATE(B(9,3*N_NE))
   DO I=1,N_NE
      B(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   ZERO      ,   ZERO      /)
      B(2,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,   ZERO      ,   ZERO      /)
      B(3,3*(I-1)+1:3*I) = (/  DNX(3,I)  ,   ZERO      ,   ZERO      /)
      B(4,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(1,I)   ,   ZERO      /)      
      B(5,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(2,I)   ,   ZERO      /)
      B(6,3*(I-1)+1:3*I) = (/   ZERO     ,  DNX(3,I)   ,   ZERO      /)
      B(7,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(1,I)   /)      
      B(8,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(2,I)   /)
      B(9,3*(I-1)+1:3*I) = (/   ZERO     ,   ZERO      ,  DNX(3,I)   /)
   ENDDO
 CASE DEFAULT
   call faterr('a_mecaEF_iso::B_iso','DIME not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT
 
END SUBROUTINE B_ISO

! ------------------------------------------------------------------------------
!> identity deformation gradient tensor - lmgc90 and matlib are the same
SUBROUTINE Id_ISO(Id)
IMPLICIT NONE
REAL(KIND=8), POINTER  :: Id(:)
! Initialisation des nouveaux pointeurs
IF(ASSOCIATED(Id)) THEN ; DEALLOCATE(Id) ; NULLIFY(Id) ; ENDIF

SELECT CASE(DIME_mod)

 CASE(i_2D_strain,i_2D_axisym)
   ! xx, xy, yx, yy, zz   
   ALLOCATE(Id(5))
   Id(1) =  UN
   Id(2) = ZERO
   Id(3) = ZERO
   Id(4) =  UN
   Id(5) =  UN
 CASE(i_3D)
   ! xx, xy, xz, yx, yy, yz, zx, zy, zz 
   ALLOCATE(Id(9))
   Id(1) =  UN
   Id(2) = ZERO
   Id(3) = ZERO
   Id(4) = ZERO
   Id(5) =  UN
   Id(6) = ZERO
   Id(7) = ZERO
   Id(8) = ZERO
   Id(9) =  UN
 CASE DEFAULT
   call faterr('a_mecaEF_iso::Id_iso','DIME not supported: '//get_dime_mode_name_from_id(dime_mod))
END SELECT

END SUBROUTINE Id_ISO

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_bulk_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U, &
                                       Fint,compute_fint,            &
                                       K,compute_stiffness,          &
                                       push_fields )

   implicit none
   ! le rang de l'element dans la liste locale  
   INTEGER        , INTENT(IN)     :: i       
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   real(kind=8)                    :: dt
   ! coordonnees des sommets   
   REAL(KIND=LONG)                 :: X(:,:)
   ! deplacements des sommets
   REAL(KIND=LONG)                 :: U(:,:)
   ! matrice de rig. elem.
   REAL(KIND=LONG)                 :: K(:,:)
   ! forces interieures elementaires
   REAL(KIND=LONG),DIMENSION(:)    :: Fint
   
   logical :: compute_fint,compute_stiffness,push_fields
                           !1234567890123456789
   character(len=19):: IAM='a_meca_EF_iso::bulk'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)

   select case(get_eleop_value(mdlnb,'kine_'))
   case('small')
     call bulk_hpp_iso(i,ppsnb,ibdyty,iblmty,dt,X,U,Fint,compute_fint,K,compute_stiffness,push_fields)
   case('large')
     call bulk_gd_iso(i,ppsnb,ibdyty,iblmty,dt,X,U,Fint,compute_fint,K,compute_stiffness,push_fields)
   case default
     call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
     call FATERR(IAM,'kinematic type unknown (small | large)')
   end select

END SUBROUTINE

!----------------------------------------------------------------------------!

!
! appel de la partie hpp de la matlib:   elastique | viscoelastique  | elastoplastique
!
! grad  contient la defo
! flux  contient la contrainte de cauchy
! internal (variable)                    rien      |     rien        | defo_plas_tensorielle (sym), defo plastique cumulee  
! internal (stockage)             
!

!------------------------------------------------------------------------------!
!    Calcul de la rigidite elementaire  [Ke]=Sum [Bl]t[D][Bl]i                !
!------------------------------------------------------------------------------!

SUBROUTINE bulk_hpp_iso(i,ppsnb,ibdyty,iblmty,dt,X,U, &
                        Fint,compute_fint,            &
                        K,compute_stiffness,          &
                        push_fields)
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty
  ! coordonnees des sommets
  REAL(KIND=LONG)                 :: X(:,:)
  ! deplacemement des sommets
  REAL(KIND=LONG)                 :: U(:,:) 
  real(kind=8)                    :: dt
  ! matrice de rig. elem.
  REAL(KIND=LONG)                 :: K(:,:)        
  REAL(KIND=LONG),DIMENSION(:)    :: Fint 
  logical :: compute_fint,compute_stiffness,push_fields

  ! variables locales
  REAL(KIND=LONG), POINTER        :: DNX(:,:), & !   
                                     Bl(:,:),  & ! matrice Bl
                                     D(:,:)      ! matrice de comportement

  REAL(KIND=LONG)                 :: COEFINT,R

  INTEGER                         :: IG

  INTEGER                         :: anisotropie

  INTEGER                         :: nb_external, nb_internal
  INTEGER                         :: mdlnb,lawnb

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

  !fd 21/04/08 modif pour gestion des field
  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  ! switch forme de B 
  INTEGER :: istrg

  ! demande calcul matrice tangente
  INTEGER(kind=4) :: calcD

  character(len=5) :: kinematic,formulation

                          !12345678901234567890123
  character(len=19):: IAM='a_meca_EF_iso::bulk_hpp'

  real(kind=8) :: Tref,field,field_begin,alpha(3)

  real(kind=8),dimension(:),allocatable :: grad_th
  
  !fd 09/2020 gestion des fields pour les materiaux
  !           ne fonctionne qu'en isotrope 
  logical :: ec_is_vfield
  real(kind=8) ::my_ec(2)

  !
  integer :: enb,ienb

  ! Initialisation a vide des pointeurs

  NULLIFY(Bl,DNX,D)

  !fd 13/09
  ! mdl is the same for all gp of the element also some time its faster 
  ! to take information from the first one ...

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  ! Allocation internal purpose arrays
  
  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external), &
           FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  allocate(grad_th(nb_external))
  grad_th=0.d0 
  ! on va utiliser les fields attaches au model pour recupere les variables externes
  ! seuls certains fields sont valables TEMPERATURE

  extP_nb = get_external_field_nb(mdlnb)

  enb=0
  do if=1,extP_nb
    name=get_external_field_name(mdlnb,if)
    if (trim(name) == 'TEMPERATURE') enb=enb+1     
    if (trim(name) == 'TEMPERATURE INITIALE') enb=enb+1
    if (trim(name) == 'TEMPERATURE FINALE') enb=enb+1
    if (trim(name) == 'alpha initiale') enb=enb+1        
    if (trim(name) == 'alpha finale') enb=enb+1        
  enddo  
  
  IF (enb /= 0) THEN 

    allocate(extP_lbl(enb), &
             extP_len(enb), &       
             extP_val(enb)  )
  ELSE


    if (get_eleop_value_bypps(ppsnb(1),'isext') == 'Demfi') THEN
     
      allocate(extP_lbl(2), &
               extP_len(2), &       
               extP_val(2))

      extP_lbl(1)=' '
      extP_val(1)=0.
      extP_lbl(2)=' '
      extP_val(2)=0.

    else  
      allocate(extP_lbl(1), &
               extP_len(1), &       
               extP_val(1))

      extP_lbl(1)=' '
      extP_val(1)=0.
    endif
  ENDIF

  !
  K=ZERO
  Fint=ZERO

  ec_is_vfield=.FALSE.
  if (get_elas_coeff_type(lawnb) /= 0) ec_is_vfield=.TRUE.

  ! Pour tous les points de Gauss
  DO IG=1,mecaEF(i)%N_PG_RIG

    ! on rapatrie les infos du debut de pas

    CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_strain_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF (nb_internal /= 0 ) INTERNAL1 = 0.D0

    CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                      mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

   
    IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN


      IF (enb /= 0) THEN 
        ienb=0
        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          if (trim(name) == 'TEMPERATURE') then
            ienb=ienb+1 
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field)

            extP_val(ienb) = field
            Tref = get_Tref_meca(lawnb)
            if (dabs(extP_val(ienb) - Tref) < 1e-10) extP_val(ienb) = Tref
          endif

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF
       
      if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'elas_') then 
        call logmes('mater ='//get_eleop_value_bypps(ppsnb(ig),'mater'))
        call FATERR(IAM,'Using isext == no___ is only possible with mater == elas ')
      endif

      ! formation de Bl avec stockage lmgc90
      istrg = 1
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    
         
      GRAD1(1:SIZE(Bl,dim=1)) = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /)))

      alpha  = get_dilatation(lawnb)
      if (alpha(1) > 0.d0) then
        grad_th(1) = alpha(1) * (extP_val(1) - Tref)
        grad_th(2) = grad_th(1)
        if (nbdime == 3) grad_th(3) = grad_th(1)
      else 
        grad_th=0.d0
      endif 

      if ( .not. ec_is_vfield) then      
         CALL D_SOLID_ISO(ppsnb(ig),D)
         CALL comp_stress(ppsnb(ig),GRAD1-grad_th,FLUX1)         
      else
        call get_meca_vfield_MAILx(ibdyty,iblmty,ig, &
                                   get_meca_vfield_rank_MAILx(ibdyty,iblmty,'elas_coeff'),my_ec,2)
        CALL D_SOLID_ISO(ppsnb(ig),D,my_ec)
        CALL comp_stress(ppsnb(ig),GRAD1-grad_th,FLUX1,my_ec)
      endif

    ELSE IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'MatL_') THEN

      ! on va utiliser la notion de field attache au model

      extP_nb = get_external_field_nb(mdlnb)

      IF (enb /= 0) THEN 
        ienb=0
        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          if (trim(name) == 'TEMPERATURE') then
            ienb=ienb+1 
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field)

            extP_val(ienb) = field
            Tref = get_Tref_meca(lawnb)
            if (dabs(extP_val(ienb) - Tref) < 1e-10) extP_val(ienb) = Tref
          endif

        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      ! formation de Bl - matlib
      istrg = 2
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    

      GRAD1 = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )

      calcD = 0
      if (compute_stiffness) calcD = 1

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,enb, &
                              GRAD0,FLUX0,INTERNAL0, &
                              GRAD1,FLUX1,INTERNAL1, &
                              D,dt,calcD)

    ELSE IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'Demfi') THEN

      extP_nb = get_external_field_nb(mdlnb)

      IF (enb /= 0) THEN 

        !pour le moment on ne gere que 4 fields (temperature initiale et finale et dilatation initial et finale)
        if (enb > 4) call faterr(IAM,'Demmefi model is expecting no more than three fields')

        ienb=0
        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          if (trim(name) == 'TEMPERATURE INITIALE') then
            ienb=1
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
          endif
          if (trim(name) == 'TEMPERATURE FINALE') then
            ienb=2
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
          endif
          if (trim(name) == 'alpha initiale') then
            ienb=3
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
          endif          
          if (trim(name) == 'alpha finale') then
            ienb=4
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
          endif

        enddo

      ELSE

        ! temperature par defaut 
        extP_lbl(1)='TEMPERATURE INITIALE'
        extP_val(1)=20.D0
        extP_lbl(2)='TEMPERATURE FINALE'
        extP_val(2)=20.D0

      ENDIF

      ! formation de Bl - a la lmgc90
      istrg = 1
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    

      GRAD1 = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )

      ! on utilise tjs la matrice elastique
      calcD = 0
      if (compute_stiffness) calcD = 1

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,enb, &
                              GRAD0,FLUX0,INTERNAL0, &
                              GRAD1,FLUX1,INTERNAL1, &
                              D,dt,calcD,X)
    ELSE
       
      call faterr(IAM,get_eleop_value_bypps(ppsnb(ig),'isext')//' not yet implemented') 
       
    ENDIF 

    !  ke= Blt.D.Bl.coef
    if (compute_stiffness) K = K +  MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*COEFINT            
              
    if (compute_fint) then
       Fint=Fint+(MATMUL(TRANSPOSE(Bl),FLUX1(1:SIZE(Bl,dim=1)))*COEFINT)
    endif   
    if (push_fields) then 
      CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
      CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
      IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
    endif

  ENDDO

  deallocate(extP_lbl,extP_len,extP_val)

  DEALLOCATE(Bl,DNX) ;  NULLIFY(Bl,DNX)
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)
  deallocate(grad_th)

END SUBROUTINE bulk_HPP_iso

! ------------------------------------------------------------------------------

SUBROUTINE BULK_GD_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U, &
                       Fint,compute_fint,            &
                       K,compute_stiffness,          &
                       push_fields)

  IMPLICIT NONE
  
  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                     
  integer,dimension(:)           ::ppsnb
  ! coordonnees de depart, deplacement total
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U   

  ! pas de temps
  real(kind=8)                   :: dt

  ! matrice de rig. elem.
  REAL(KIND=LONG),DIMENSION(:,:) :: K
  
  REAL(KIND=LONG),DIMENSION(:)   :: Fint 
  logical :: compute_fint,compute_stiffness,push_fields

  ! variables locales
  REAL(KIND=LONG), POINTER       :: DNX(:,:),B(:,:),Id(:)
  REAL(KIND=LONG)                :: COEFINT,R
  INTEGER                        :: IG

  INTEGER                        :: anisotropie

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb

  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0 
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1 
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D

  integer                                      :: if,rank
  character(len=30)                            :: name
  INTEGER(kind=4)                              :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER(kind=4)  , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER(kind=4)                :: calcD

  INTEGER                        :: istrg
  !
  ! internal and stress values at Gauss points
  ! for computation with a model internal to lmgc90
  real(kind=8), dimension(6,4) :: vpg
  real(kind=8), dimension(4,4) :: Sloc

  !
  integer :: enb,ienb
  

                           !1234567890123456789012345
  CHARACTER(len=25) :: IAM='a_mecaEF_iso::bulk_gd_iso'

  ! on appelle l'element QP0
  !fd a reprendre !!
  IF (get_eleop_value_bypps(ppsnb(1),'isext') == 'no___') THEN

    if (mecaEF(i)%NAME /= i_q4p0x) then
      call logmes('Element iso ='//get_NAME_mecaEF_iso(mecaEF(i)%NAME))
      call FATERR(IAM,'with kine_ == large and isext == no___ only Q4P0x element is available') 
    endif

    CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

    do ig = 1, mecaEF(i)%N_PG_RIG 
      call get_internal_0_MAILx(ibdyty,iblmty,ig,vpg(:,ig))
    end do

    CALL BULK_NL_ISO(i,mdlnb,lawnb,ibdyty,iblmty,X,U,vpg,Sloc,Fint,K)

    do ig = 1, mecaEF(i)%N_PG_RIG 
      call put_internal_MAILx(ibdyty,iblmty,ig,vpg(:,ig))
      call put_stress_MAILx(ibdyty,iblmty,ig,Sloc(:,ig))
    end do

    RETURN
  END IF 

  ! Initialisation a vide des pointeurs

  NULLIFY(DNX,B,Id)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external), &
           FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  enb=0
  do if=1,extP_nb
    name=get_external_field_name(mdlnb,if)
    if (trim(name) == 'TEMPERATURE') enb=enb+1
  enddo  
  
  IF (enb /= 0) THEN 

    allocate(extP_lbl(enb), &
             extP_len(enb), &       
             extP_val(enb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  Fint=ZERO
  K=ZERO

  ! Pour tous les points de Gauss
  DO IG=1,get_N_PG_RIG_mecaEF(i)     

    ! on rapatrie les infos du debut de pas

    CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_strain_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    IF (enb /= 0) THEN 
      ienb=0
      do if=1,extP_nb
        name=get_external_field_name(mdlnb,if)
        if (trim(name) == 'TEMPERATURE') then
          ienb=ienb+1         
          extP_lbl(ienb)=name
          extP_len(ienb)=len_trim(name)
          rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
        endif  
      enddo

    ELSE

      extP_lbl(1)=' '
      extP_val(1)=0.

    ENDIF

    CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                      mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

    CALL B_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,B)

    CALL Id_ISO(Id) ! formation de Id 

    IF (NSTEP == 1) THEN
      !fd a modifier avec la procedure d'initialisation de laurent
      FLUX0     = ZERO
      GRAD0     = Id
      INTERNAL0 = 0.d0
      IF (nb_internal /= 0 .AND. SIZE(INTERNAL0) .GE. SIZE(Id) ) THEN
        INTERNAL0(1:SIZE(Id))=Id
      ENDIF 
    ENDIF

    ! CALCUL DU GRADIENT DES DEFORMATIONS
    !*** Calcul de [1+d(u_n+1)/d(x_0)]

    GRAD1 = MATMUL(B,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
    GRAD1 = Id + GRAD1
    !
    FLUX1 = ZERO
    IF (nb_internal /= 0 ) INTERNAL1 = ZERO
    !
    calcD=0
    if (compute_stiffness) calcD=1

    
    CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,enb, &
                             GRAD0,FLUX0,INTERNAL0, &
                             GRAD1,FLUX1,INTERNAL1, &
                             D,dt,calcD)
    
    !
    !  ke= Bt.D.B.coef
    !

    if (compute_stiffness) K = K + (MATMUL(TRANSPOSE(B),MATMUL(D,B))*COEFINT)

    !
    !  Fint= Bt.PK.coef
    !

    if (compute_fint) Fint=Fint+(MATMUL(TRANSPOSE(B),FLUX1(1:SIZE(B,dim=1)))*COEFINT)

    if (push_fields) then 
      CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
      CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
      IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)
    endif

  ENDDO

  deallocate(extP_lbl,extP_len,extP_val)
  DEALLOCATE(DNX,B,Id) ;  NULLIFY(DNX,B,Id)
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE BULK_GD_ISO

! ------------------------------------------------------------------------------
!
! aiguillage aux differentes cinematiques
! hpp(internal|external)|gd(internal|external)
!
SUBROUTINE compute_elementary_fields_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U)

   implicit none
   ! le numero de l'element dans la liste locale
   INTEGER        , INTENT(IN)     :: i       
   integer,dimension(:),intent(in) :: ppsnb
   INTEGER        , INTENT(IN)     :: ibdyty,iblmty
   ! coordonnees et deplacement des sommets
   REAL(KIND=LONG)                 :: X(:,:),U(:,:) 
   real(kind=8)                    :: dt

                           !12345678901234567890123456789012345678901234
   character(len=44):: IAM='a_meca_EF_iso::compute_elementary_fields_iso'

   integer :: mdlnb,inull

   CALL get_ppset_value(ppsnb(1),mdlnb,inull)

   select case(get_eleop_value(mdlnb,'kine_'))
   case('small')
     call fields_hpp_iso(i,ppsnb,ibdyty,iblmty,dt,X,U)
   case('large')
     call fields_gd_iso(i,ppsnb,ibdyty,iblmty,dt,X,U)
   case default
     call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
     call FATERR(IAM,'kinematic type unknown (small | large)')
   end select

END SUBROUTINE

! ------------------------------------------------------------------------------

SUBROUTINE fields_HPP_ISO (i,ppsnb,ibdyty,iblmty,dt,X,U)

  IMPLICIT NONE

  ! le numero de l'element 
  INTEGER         , INTENT(IN) :: I,ibdyty,iblmty  
  integer, dimension(:)        :: ppsnb
  ! coordonnees des sommets  
  REAL(KIND=LONG)              :: X(:,:)
  ! DDL elementaires
  REAL(KIND=LONG)              :: U(:,:)     
  real(kind=8)                 :: dt

  ! variables locales
  REAL(KIND=LONG) , POINTER    :: DNX(:,:), &    
                                  Bl(:,:), &    
                                  D(:,:)  
  REAL(KIND=LONG)              :: COEFINT,R
  INTEGER                      :: IG

  INTEGER                      :: anisotropie
 
  INTEGER                      :: nb_external, nb_internal
  INTEGER                      :: mdlnb,lawnb

  ! zone de stockage: gradient,flux,internal,operateur tangent

  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD0,FLUX0,INTERNAL0
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: GRAD1,FLUX1,INTERNAL1

  INTEGER(kind=4)                      :: calcD

  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER(kind=4)                              :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER(kind=4)  , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  ! switch forme de B 
  INTEGER :: istrg,inull

  real(kind=8) :: UMTT,field,field_begin,alpha(3),Tref
  real(kind=8),dimension(:),allocatable :: grad_th
  !
  integer :: enb,ienb
  
                           !12345678901234567890123456789
   character(len=29):: IAM='a_meca_EF_iso::fields_hpp_iso'

  
  umtt = (1.d0 - theta)

  ! Initialisation a vide des pointeurs
  NULLIFY(Bl,DNX)

  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)


  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)


  enb=0
  do if=1,extP_nb
    name=get_external_field_name(mdlnb,if)
    if (trim(name) == 'TEMPERATURE') enb=enb+1
  enddo  
  
  IF (enb /= 0) THEN 

    allocate(extP_lbl(enb), &
             extP_len(enb), &       
             extP_val(enb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  allocate(grad_th(nb_external))
  grad_th=0.d0 
  
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  D=>null()

  ! Pour tous les points de Gauss
  DO IG=1,mecaEF(i)%N_PG_RIG                 

    ! on rapatrie les infos du debut de pas

    CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_strain_0_MAILx(ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    FLUX1 = 0.D0
    GRAD1 = 0.D0
    IF ( nb_internal /= 0 ) INTERNAL1 = 0.D0

    CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                      mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

    IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN

      ! formation de Bl  
      istrg = 1
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl) 
                                
      GRAD1(1:SIZE(Bl,dim=1)) = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /)))


      alpha  = get_dilatation(lawnb)
      if (alpha(1) > 0.d0) then
        Tref = get_Tref_meca(lawnb) 
        grad_th(1) = alpha(1) * (extP_val(1) - Tref)
        grad_th(2) = grad_th(1)
        if (nbdime==3) grad_th(3) = grad_th(1)
      else 
        grad_th=0.d0
      endif 

      
      CALL comp_stress(ppsnb(ig),GRAD1-grad_th,FLUX1)
     

    ELSE if (get_eleop_value_bypps(ppsnb(ig),'isext') == 'MatL_') THEN
      !DA : Modification salvatrice a verifier ?
      !extP_nb = get_meca_field_size_MAILx(ibdyty,iblmty,ig)
      IF (enb /= 0) THEN 
        ienb=0
        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          if (trim(name) == 'TEMPERATURE') then
            ienb=ienb+1         
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
          endif  
        enddo

      ELSE

        extP_lbl(1)=' '
        extP_val(1)=0.

      ENDIF

      ! formation de Bl        
      istrg = 2
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    

      GRAD1 = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
 
      calcD=0

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,enb, &
                               GRAD0,FLUX0,INTERNAL0, &
                               GRAD1,FLUX1,INTERNAL1, &
                               D,dt,calcD)

     
    ELSE if (get_eleop_value_bypps(ppsnb(ig),'isext') == 'Demfi') THEN

      IF (enb /= 0) THEN 
        ienb=0
        do if=1,extP_nb
          name=get_external_field_name(mdlnb,if)
          if (trim(name) == 'TEMPERATURE') then
            ienb=ienb+1         
            extP_lbl(ienb)=name
            extP_len(ienb)=len_trim(name)
            rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
            CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
          endif  
        enddo

      ELSE

        extP_lbl(1)='TEMPERATURE'
        extP_val(1)=20.

      ENDIF

      ! formation de Bl        
      istrg = 1
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    

      GRAD1 = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
 
      calcD=0

      CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,enb, &
                               GRAD0,FLUX0,INTERNAL0, &
                               GRAD1,FLUX1,INTERNAL1, &
                               D,dt,calcD)


    else
      call faterr(IAM,get_eleop_value_bypps(ppsnb(ig),'isext')//'not yet implemented')   
    ENDIF

    CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
    CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
    IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)

  ENDDO

  deallocate(extP_lbl, extP_len, extP_val  )

  DEALLOCATE(Bl,DNX) ; NULLIFY(Bl,DNX)
  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1)
  deallocate(grad_th)
  if (associated(D)) deallocate(D)

END SUBROUTINE fields_HPP_ISO

! ------------------------------------------------------------------------------

SUBROUTINE fields_GD_ISO(i,ppsnb,ibdyty,iblmty,dt,X,U)

  IMPLICIT NONE

  ! le numero de l'element dans la liste mecaef locale
  INTEGER        , INTENT(IN)    :: i,ibdyty,iblmty  
                                                       
  integer,dimension(:)           :: ppsnb

  ! coordonnees de dpart,deplacement total
  REAL(KIND=LONG),DIMENSION(:,:) :: X,U
  !
  real(kind=8)                    :: dt

  ! variables locales
  REAL(KIND=LONG), POINTER       :: DNX(:,:),B(:,:),Id(:) 
  REAL(KIND=LONG)                :: COEFINT,R
  INTEGER                        :: IG

  INTEGER                        :: anisotropie

  INTEGER                        :: nb_external, nb_internal  
  INTEGER                        :: mdlnb,inull,lawnb
  ! gradient de la transformation 
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD0,FLUX0,INTERNAL0
  ! contrainte de Cauchy
  REAL(KIND=LONG),DIMENSION(:),ALLOCATABLE   :: GRAD1,FLUX1,INTERNAL1
  REAL(kind=LONG),DIMENSION(:,:),POINTER   :: D

  ! parametres externes
  ! gestion de multiples field|extP pour couplage
  integer           :: if,rank
  character(len=30) :: name
  INTEGER*4                                    :: extP_nb
  CHARACTER(len=30), DIMENSION(:), allocatable :: extP_lbl
  INTEGER*4        , DIMENSION(:), allocatable :: extP_len
  REAL(kind=8)     , DIMENSION(:), allocatable :: extP_val

  INTEGER*4        :: calcD
  
  INTEGER          :: istrg

 !
  integer :: enb,ienb

                           !123456789012345678901234567
  CHARACTER(len=27) :: IAM='a_mecaEF_iso::fields_gd_iso'

  !fd a reprendre !!
  IF (get_eleop_value_bypps(ppsnb(1),'isext') == 'no___') THEN

    if (mecaEF(i)%NAME /= i_q4p0x) then
      call logmes('Element iso ='//get_NAME_mecaEF_iso(mecaEF(i)%NAME))
      call FATERR(IAM,'with kine_ == large and isext == no___ only Q4P0x element is available') 
    endif

    CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

    !fd on n'a pas la fonction on ne recalcule rien

    !?  CALL BULK_NL_ISO(i,mdlnb,lawnb,X,U,ibdyty,iblmty,Fint,K)

    RETURN
  END IF 

  !print *, 'ie : ', iblmty
  !print *, 'X : '
  !print *, X
  !print *, 'U : '
  !print *, U
  ! Initialisation a vide des pointeurs

  NULLIFY(DNX,B,Id)

  ! Allocation internal purpose arrays

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external = get_nb_external_variables(mdlnb)
  nb_internal = get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
  ALLOCATE(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
  ALLOCATE(D(nb_external,nb_external))

  ! on va utiliser la notion de field attache au model

  extP_nb = get_external_field_nb(mdlnb)

  enb=0
  do if=1,extP_nb
    name=get_external_field_name(mdlnb,if)
    if (trim(name) == 'TEMPERATURE') enb=enb+1
  enddo  
  
  IF (enb /= 0) THEN 

    allocate(extP_lbl(enb), &
             extP_len(enb), &       
             extP_val(enb)  )
  ELSE

    allocate(extP_lbl(1), &
             extP_len(1), &       
             extP_val(1))

  ENDIF

  DO IG=1,get_N_PG_RIG_mecaEF(i)     ! Pour tous les points de Gauss

    ! on rapatrie les infos du debut de pas

    CALL get_stress_0_MAILx(ibdyty,iblmty,ig,FLUX0)
    CALL get_strain_0_MAILx  (ibdyty,iblmty,ig,GRAD0)
    IF (nb_internal /= 0 ) CALL get_internal_0_MAILx(ibdyty,iblmty,ig,INTERNAL0)

    IF (enb /= 0) THEN 
      ienb=0
      do if=1,extP_nb
        name=get_external_field_name(mdlnb,if)
        if (trim(name) == 'TEMPERATURE') then
          ienb=ienb+1         
          extP_lbl(ienb)=name
          extP_len(ienb)=len_trim(name)
          rank = get_meca_field_rank_MAILx(ibdyty,iblmty,name)
          CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,extP_val(ienb))
        endif  
      enddo
    ELSE

      extP_lbl(1)=' '
      extP_val(1)=0.

    ENDIF
    
    CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                      mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

    CALL B_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,B)

    CALL Id_ISO(Id) ! formation de Id 
!
    IF (NSTEP == 1) THEN
      !fd a modifier avec la procedure d'initialisation de laurent
      FLUX0     = ZERO
      GRAD0     = Id
      INTERNAL0 = 0.d0
      IF (nb_internal /= 0 .AND. SIZE(INTERNAL0) .GE. SIZE(Id) ) THEN
        INTERNAL0(1:SIZE(Id))=Id
      ENDIF 
    ENDIF

    ! CALCUL DU GRADIENT DES DEFORMATIONS
    !*** Calcul de [1+d(u_n+1)/d(x_0)]

    GRAD1 = MATMUL(B,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
    GRAD1 = Id + GRAD1
    !
    FLUX1 = ZERO
    IF (nb_internal /= 0 ) INTERNAL1 = ZERO
    !

    calcD=0

    CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,enb, &
                             GRAD0,FLUX0,INTERNAL0, &
                             GRAD1,FLUX1,INTERNAL1, &
                             D,dt,calcD)

    CALL put_stress_MAILx(ibdyty,iblmty,ig,FLUX1)
    CALL put_strain_MAILx(ibdyty,iblmty,ig,GRAD1)
    IF (nb_internal /= 0 ) CALL put_internal_MAILx(ibdyty,iblmty,ig,INTERNAL1)

  ENDDO
  deallocate(extP_lbl,extP_len,extP_val)
  DEALLOCATE(DNX,B,Id) ;  NULLIFY(DNX,B,Id)

  DEALLOCATE(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

END SUBROUTINE fields_GD_ISO

!----------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!    Calcul de la masse elementaire  [Me]=Sum rho [N]t [N] i                  !
!------------------------------------------------------------------------------!

SUBROUTINE compute_elementary_mass_iso(i,ppsnb,ibdyty,iblmty,X,M)

  IMPLICIT NONE

  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN) :: i,ibdyty,iblmty
  integer,dimension(:)        :: ppsnb
  ! coordonnees des sommets
  REAL(KIND=8)                :: X(:,:)     
  ! matrice de rig. elem.
  REAL(KIND=8)                :: M(:,:)     

  REAL(KIND=8)                :: COEFINT,R,vol,xm,rho

  INTEGER                     :: IG,in,mdlnb,lawnb

  ! derivee de N par rapport a X   
  REAL(KIND=8), POINTER       :: DNX(:,:) 
  ! les fonctions de formes rangees comme il faut
  REAL(KIND=8),ALLOCATABLE    ::  xn(:,:)  

  INTEGER :: ni,nj,nk,ie,je,ke

  REAL(KIND=8) :: diamas

  ! somme des elements diagonaux de la matrice de masse coherente
  REAL (kind=8) :: sumDiagMass 
  ! masse totale de l'element divisee par la somme des elements diagonaux de la matrice de masse coherente
  REAL (kind=8) :: alpha 

  ! nbDIME est defini dans overall

  CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

  M = 0.d0

  if (get_rho_type(lawnb) == 0) then
     rho= get_RHO(lawnb)
     if (rho == 0.d0) return     
  endif


  NULLIFY(DNX)
  ALLOCATE(xn(nbDIME,nbDIME*mecaEF(i)%N_NODE))

  xm=0.d0; vol = 0.d0

  DO IG=1,mecaEF(i)%N_PG_MAS     ! Pour tous les points de Gauss

    !fd a voir ce qui se passe si pg_mass /= pg_rig !? 
    if (get_rho_type(lawnb) /= 0) then    
       call get_meca_field_MAILx(ibdyty,iblmty,ig,get_meca_field_rank_MAILx(ibdyty,iblmty,'density'),rho)
    endif
     
    xn=0.d0
   
    CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%mPG(ig)%N,mecaEF(i)%mPG(ig)%DN, &
                      mecaEF(i)%mPG(ig)%POIDS,X,DNX,COEFINT,R)

    vol = vol + COEFINT
    xm  = xm  + COEFINT*rho

    SELECT CASE(nbDIME)
    CASE(2)
      ni=0;nj=0
      DO in=1,mecaEF(i)%N_NODE
        ni=nj+1;nj=ni+1
        xn(1,ni)=mecaEF(i)%mPG(ig)%N(in)
        xn(2,nj)=mecaEF(i)%mPG(ig)%N(in)        
      ENDDO
    CASE(3)
      ni=0;nj=0;nk=0
      DO in=1,mecaEF(i)%N_NODE
        ni=nk+1;nj=ni+1;nk=nj+1
        xn(1,ni)=mecaEF(i)%mPG(ig)%N(in)
        xn(2,nj)=mecaEF(i)%mPG(ig)%N(in)        
        xn(3,nk)=mecaEF(i)%mPG(ig)%N(in)        
      ENDDO
    CASE DEFAULT
    END SELECT

    M=M+(MATMUL(TRANSPOSE(xn),xn)*COEFINT*Rho)            !  ke= Blt.D.Bl.coef

  ENDDO

  ! lumping
  !fd attention avec un Q8 ca donne une masse negative !

  IF (get_eleop_value(mdlnb,'mstrg') == 'lump_' ) THEN

    if (mecaEF(i)%NAME /= i_q8xxx .and. &
        mecaEF(i)%NAME /= i_q8rxx .and. &
        mecaEF(i)%NAME /= i_h20xx .and. &
        mecaEF(i)%NAME /= i_h20rx .and. &
        mecaEF(i)%NAME /= i_te10x .and. &
        mecaEF(i)%NAME /= i_pri15 ) then

      ! raw sum
      DO ie=1,nbDIME*mecaEF(i)%N_NODE
        diamas=0.d0
        DO je=1,nbDIME*mecaEF(i)%N_NODE
          diamas=diamas+M(ie,je)
          IF (ie /= je) M(ie,je)=0.d0
        END DO
        M(ie,ie)=diamas 
      END DO

    else
      ! Autre mthode de lumping: "Special Lumping Technique" - voir bouquin de Hugues
      ! elle donne TOUJOURS des masses lumpees strictement positives!   
 
      ! on calcule la somme des elements diagonaux de la matrice de masse coherente
      sumDiagMass = 0.d0

      DO ie=1,nbDIME*mecaEF(i)%N_NODE
        sumDiagMass = sumDiagMass + M(ie, ie)
      END DO

      ! on calcule alpha: masse totale de l'element divisee par la somme
      ! des elements diagonaux de la matrice de masse coherente
      ! i.e. alpha = xm/sumDiagMass

      !fd le 20/11/2015 la masse totale doit etre multipliee par nbdime a cause
      !                 de la construction de sumDiagMass  
      alpha = nbdime*xm/sumDiagMass
      !alpha = xm/sumDiagMass

      ! on en deduit les coefficients de la matrice de masse lumpee
      ! en fonctions de ceux de la mtrice de masse coherente
      ! pour la ligne i: 
      !  * M_lump(i, i) = alpha*M(i, i)
      !  * M_lump(i, j) = 0, pour j different de i
      DO ie=1,nbDIME*mecaEF(i)%N_NODE 
        DO je=1,nbDIME*mecaEF(i)%N_NODE
          IF (ie /= je) M(ie, je)=0.d0
        END DO 
        M(ie, ie)=alpha*M(ie, ie) 
     END DO
      
    endif

  ENDIF

  DEALLOCATE(dnx,xn);NULLIFY(DNX)

END SUBROUTINE

! ------------------------------------------------------------------------------

SUBROUTINE compute_elementary_volume_iso(i,X,volume)

IMPLICIT NONE

! le numero de l'element dans la liste locale
INTEGER        , INTENT(IN) :: i 
! coordonnees des sommets
REAL(KIND=LONG)             :: X(:,:)     
! volume
REAL(KIND=LONG)             :: volume    

! ***
REAL(KIND=LONG)             :: COEFINT,R

INTEGER                     :: IG

REAL(KIND=LONG), POINTER    :: DNX(:,:) ! derive de N par rapport a X   


NULLIFY(DNX)

volume = 0.d0

DO IG=1,mecaEF(i)%N_PG_MAS     ! Pour tous les points de Gauss

   CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%mPG(ig)%N,mecaEF(i)%mPG(ig)%DN, &
                     mecaEF(i)%mPG(ig)%POIDS,X,DNX,COEFINT,R)

   volume = volume + COEFINT

ENDDO

DEALLOCATE(dnx);NULLIFY(DNX)

END SUBROUTINE

! ------------------------------------------------------------------------------

SUBROUTINE compute_elementary_jacobian_iso(i,ppsnb,ibdyty,iblmty,jacobian)

IMPLICIT NONE

INTEGER        , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty
! volume
REAL(KIND=LONG)             :: jacobian    

! ***
INTEGER                     :: IG,nb_external
REAL(KIND=LONG) ,ALLOCATABLE :: GRAD(:)
logical :: is_small, is_ext

integer :: mdlnb,lawnb

CALL get_ppset_value(ppsnb(1),mdlnb,lawnb)

nb_external=get_nb_external_variables(mdlnb)
ALLOCATE(GRAD(nb_external))

is_small = .TRUE.
IF (get_eleop_value(mdlnb,'kine_') == 'large') is_small = .FALSE.
is_ext = .TRUE.
if (get_eleop_value(mdlnb,'isext') == 'no___' ) is_ext = .FALSE.       
jacobian = 0.d0
DO IG=1,mecaEF(i)%N_PG_rig     ! Pour tous les points de Gauss

    CALL get_strain_1_MAILx(ibdyty,iblmty,ig,GRAD)

    if (is_small) then
      if (is_ext) then
        jacobian = jacobian + (1.0D0 + GRAD(1) + GRAD(3) + GRAD(4))
      else
         !fd merdique car GRAD pas tjs la meme taille en 2D jacobian = jacobian + (1.0D0 + GRAD(1) + GRAD(2) + GRAD(4))
         ! ne marchera pas en axi !!
        jacobian = jacobian + (1.0D0 + GRAD(1) + GRAD(2))         
      endif   
    else
      jacobian = jacobian + ((grad(1)*grad(4)) - (grad(2)*grad(3)))
    endif

ENDDO

jacobian = jacobian/mecaEF(i)%N_PG_rig

deallocate(grad)

END SUBROUTINE

! ------------------------------------------------------------------------------

SUBROUTINE compute_elementary_center_iso(i,X,center)

IMPLICIT NONE

! le numero de l'element dans la liste locale
INTEGER        , INTENT(IN) :: i 
! coordonnees des sommets
REAL(KIND=LONG)             :: X(:,:)     
! volume
REAL(KIND=LONG)             :: center(:)

CALL Compute_center(mecaEF(i)%T_FONC_FORME,X,center)

END SUBROUTINE


!============= low level private routines ==================

INTEGER FUNCTION get_N_PG_RIG_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_RIG_mecaEF=mecaEF(nb)%N_PG_RIG

END FUNCTION get_N_PG_RIG_mecaEF

integer(kind=4) FUNCTION get_SCH_GAUSS_RIG_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_RIG_mecaEF=mecaEF(nb)%SCH_GAUSS_RIG

END FUNCTION get_SCH_GAUSS_RIG_mecaEF

INTEGER FUNCTION get_N_PG_MAS_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_PG_MAS_mecaEF=mecaEF(nb)%N_PG_MAS

END FUNCTION get_N_PG_MAS_mecaEF

integer(kind=4) FUNCTION get_SCH_GAUSS_MAS_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_MAS_mecaEF=mecaEF(nb)%SCH_GAUSS_MAS

END FUNCTION get_SCH_GAUSS_MAS_mecaEF

integer(kind=4) FUNCTION get_T_FONC_FORME_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_T_FONC_FORME_mecaEF=mecaEF(nb)%T_FONC_FORME

END FUNCTION get_T_FONC_FORME_mecaEF

!------------------------------------------------------------------------------!
!    Calcul de la rigidite elementaire pour un element Q4/P0                   !
!------------------------------------------------------------------------------!

!fd passer un pps

SUBROUTINE BULK_NL_ISO(I,mdlnb,lawnb,ibdyty,iblmty,X0,U,vpg,Sloc,RESLOC,K)
  implicit none
  integer(kind=4), intent(in)     :: I      !< [in] index of the element in local list mecaef
  integer(kind=4), intent(in)     :: mdlnb  !< [in] model number
  integer(kind=4), intent(in)     :: lawnb  !< [in] material number
  integer(kind=4), intent(in)     :: ibdyty !< [in] body id
  integer(kind=4), intent(in)     :: iblmty !< [in] element id
  real(kind=LONG), dimension(2,4) :: X0     !< [in] initial coordinates of the vertices
  real(kind=LONG), dimension(2,4) :: U      !< [in] total displacement of the vertices
  real(kind=8),    dimension(6,4) :: vpg    !< [in,out] array of Gauss point values
                                            !< Cp**-1 (4 first values) 
                                            !< epsb (fifth value)
                                            !< von mises (sixth value)
  real(kind=8),    dimension(4,4) :: Sloc   !< [out] current Cauchy strain
  real(kind=LONG), dimension(8)   :: RESLOC !< [out] residue
  real(kind=LONG), dimension(8,8) :: K      !< [out] elementary stiffness  matrix
!
! modifier cette taille qui est mise en dur
!

INTEGER                        :: anisotropie
REAL(kind=8),DIMENSION(21)     :: elas_coeff             

!
INTEGER      :: critere                    ! type de critre: 0 pas, 1 VM, 2
REAL(kind=8), DIMENSION(10)    :: crit_coeff  ! VM:   none
                                           ! HILL:
! ecrouissage isotrope
INTEGER                        :: iso_hard    ! ecrouissage isotrope: 0 pas, 1 swift, 2 line, 3 ...                    
REAL(kind=8), DIMENSION(80)    :: isoh_coeff  ! swift   : C0,EPS0,n
                                           ! lineaire: SIG0,H
                                           ! Hollomon: H,n
                                           ! Expo    : SIG0,SIG_inf,n
                                           ! PT by PT: EPS(i),SIG(i)
! ecrouissage cinematique
INTEGER                        :: cine_hard  ! ecrouissage cinematique: 0 pas, 1
REAL(kind=8),DIMENSION(1)      :: cinh_coeff ! le beta

INTEGER                        :: visco_plas
   REAL(kind=8), DIMENSION(3)  :: vplas_coeff


REAL(KIND=8)                   :: ROOT32,ROOT23

REAL(KIND=LONG)                :: YOUNG,PS    ! module d'young, coef de Poisson
REAL(KIND=LONG),DIMENSION(2,4) :: X           ! coordonnees actualisees des sommets 

! variables locales

REAL(KIND=8), POINTER          :: DNX(:,:)    !   

REAL(KIND=8)                   :: R,        & ! rayon dans le cas axisym
                                  VOLU0,    & ! volume initial
                                  VOLU,     & ! volume deforme
                                  VOLU1,    &
                                  DVOLU       ! variation de volume
                                          
REAL(KIND=8)                   :: MUELAS,KELAS, &  ! coefs de Lame elastiques
                                  MMUELAS          ! coef de Lame  modifie


REAL(KIND=8),DIMENSION(5)      :: GRADF       ! gradient de la transformation 

REAL(KIND=8)                   :: JGRADF, &   ! determinant du gradient de la 
                                  MJGRADF     ! transformation

REAL(KIND=8),DIMENSION(4,4)    :: FING, &     ! tenseur de Finger
                                  DTAU, &     ! deviatorique de la contrainte
                                  CPM1_0, &
                                  CPM1_1, &
                                  NORMA       ! normale a la surface de plasticite,
 
REAL(KIND=8),DIMENSION(4)      :: TFING, &    ! trace de FING
                                  NORM_DTAU_Tr,DTAUDTAU, &
                                  VMIS_0,EPSB_0,&
                                  VMIS_1,EPSB_1
REAL(KIND=8)                   :: J2


INTEGER,DIMENSION(4)           :: PLAS_STAT     ! =0 elas, =1 plastique


REAL(KIND=8)                   :: CRITER,     &    !
                                  VONMISI,    &    ! valeur au debut de l'algo de
                                                   ! retour radial de VONMIS
                                  CONS,       &    ! constantes pour le calcul du 
                                  CONS1,      &    ! module elastique   
                                  DCRITER,    &    ! derivee de CRITER
                                  DELGAM           ! parametre de plasticite
                                                   ! de VON MISES 

REAL(KIND=8),DIMENSION(8)      :: BMATV,   &    ! matrice volumique d'interpolation pour
                                                ! la methode b-bar
                                  DIVB          ! operateur divergence modifie 
REAL(KIND=8),DIMENSION(4,8)    :: BMATU, &     ! matrice d'interpolation des defor. 
                                  MDB

REAL(KIND=8)             :: UTRABE1, &       ! Utilitaires pour calculer TRABE
                               UTRABE2, &
                               UTRABE3, &
                               UTRABE4, &  
                               TRABE            ! trace de be corrige pour le calcul 
                                                ! du nouveau Cp**-1  

REAL(KIND=8)             :: CPN11,       &   ! permet le calcul de CPn+1 dans la 
                                                ! conf de ref par transport convectif 
                               CPN12,       &
                               CPN13,       &
                               CPN14,       & 
                               PRES   ! pression hydrostatique


REAL(KIND=8),DIMENSION(4,4):: CAUCHY, &   ! contrainte de Cauchy
                                 MPRES       ! matrice P(1*1-2I) 


REAL(KIND=8),DIMENSION(5,5):: CAUCHYI     ! matrice sigma initiale ??????????????

REAL(KIND=8),DIMENSION(5,8):: GRADV,  &   ! gradient des deplacements
                                 BETEN,  &   ! matrice B etendue
                                 CGRADV, &   ! Dans l'equation d'equilibre, GRADV*CAUCHYI 
                                 CBETEN      ! Dans la meme equation, BETEN*CAUCHYI

REAL(KIND=8),DIMENSION(4,4):: CELAS,&       ! module elastique 
                                 CEP         ! module elasto-plastique

REAL(KIND=8)             :: BETA0, &         ! facteurs Beta   
                               BETA1, &
                               BETA2, &
                               BETA3, &
                               BETA4

INTEGER                     :: IG,INODE, &
                               ICOPLA,   &      ! indice de non convergence pour 
                                                ! la plasticite
                               IITER,    &      ! iteration du retour radial
                               IELE,     &      ! compteur des ddl de l'element
                               JELE,     &
                               A1,A2,    &
                               B1,B2,    &
                               DELTAB,   &
                               KS,NDSIG, &
                               KU2,      &
                               INDIC, &         !fd hyperelastique=1,hypoelastique=0
                               il,jl


REAL(kind=long) :: sig0,dsig0,etol,xlimd, &
                   devn2xx,devn2yy,devn2xy,devn2zz,&
                   tradevn2,&
                   dn2zz,dn2xx,dn2yy,dn2xy

INTEGER :: N_PG_RIG,n_node,n_dof_by_node

INTEGER :: n_stress_gp
INTEGER :: n_internal_gp  

real(kind=8) :: my_ec(2)

character(len=80) :: cout
character(len=16) :: IAM
      !1234567890123456
IAM = 'a_mecaEF::NL_iso'




ROOT32=DSQRT(3.D0/2.D0)
ROOT23=DSQRT(2.D0/3.D0)

n_stress_gp   = get_nb_external_variables(mdlnb)
n_internal_gp = get_nb_internal_variables(mdlnb)

n_node        = mecaEF(i)%N_NODE
n_dof_by_node = mecaEF(i)%N_DOF_by_NODE
N_PG_RIG      = mecaEF(i)%N_PG_RIG 

! Initialisation a vide des pointeurs

NULLIFY(DNX)

IF (nbDIME == 3 ) THEN
  call faterr(IAM,' L''element Q4P0 ne fonctionne pas en 3D')
ENDIF

K=ZERO
RESLOC=ZERO

ICOPLA=0

KS=3
           !1234567890
IF(DIME_mod.EQ.i_2D_axisym) KS=4

NDSIG=4

INDIC=1

KU2=(N_NODE*N_DOF_by_NODE)**2

IF (.NOT. is_ELAS_PLAS(lawnb)) THEN
  call faterr(IAM,'Materiau mal defini')
ENDIF

if (get_elas_coeff_type(lawnb) == 0) then
  CALL get_elas_coeff(lawnb,anisotropie,elas_coeff)
  YOUNG=elas_coeff(1)
  PS=elas_coeff(2)
  ! CALCUL DES VALEURS ELASTIQUES.
  MUELAS=YOUNG/(2.D0*(1.D0+PS))
  KELAS=YOUNG/(3.D0*(1.D0-2.D0*PS))
  IF (anisotropie > 0) THEN        ! On ne traite que de l'isotropie
   call faterr(IAM,'materiau non supporte')
  ENDIF
else
  anisotropie=0 
endif
!

CALL get_plas_coeff(lawnb,anisotropie, &
                         crit_coeff, &
                         iso_hard,isoh_coeff, &
                         cine_hard,cinh_coeff,&
                         visco_plas,vplas_coeff)

!
 ! IF (anisotropie > 0) THEN        ! On ne traite que de l'isotropie
 !   call faterr(IAM,'materiau non supporte')
 ! ENDIF



!!!!!!!! DEBUT PREMIERE BOUCLE POINTS DE GAUSS !!!!!!!!!!!!!!!

VOLU0=ZERO
VOLU =ZERO
TRABE=0.D0

DO IG=1,N_PG_RIG  ! Pour tous les points de Gauss

   if (get_elas_coeff_type(lawnb) /= 0) then    
     call get_meca_vfield_MAILx(ibdyty,iblmty,ig,get_meca_vfield_rank_MAILx(ibdyty,iblmty,'elas_coeff'),my_ec,2)
     YOUNG=my_ec(1)
     PS=my_ec(2)
     ! CALCUL DES VALEURS ELASTIQUES.
     MUELAS=YOUNG/(2.D0*(1.D0+PS))
     KELAS=YOUNG/(3.D0*(1.D0-2.D0*PS))
     anisotropie=0
   endif
   
   ! Au premier increment on initialise le Cp^-1 
   ! on rapatrie les valeurs au point de Gauss
   CPM1_0(1:4,ig) = vpg(1:4,ig)
   EPSB_0(ig)   = vpg(5,ig)
   VMIS_0(ig)   = vpg(6,ig)

   IF(NSTEP.EQ.1) THEN
     CPM1_0(1,ig) = 1.D0
     CPM1_0(2,ig) = 1.D0
     CPM1_0(3,ig) = 0.D0
     CPM1_0(4,ig) = 1.D0
   ENDIF

   ! par defaut on initialise a la valeur d'avant

   CPM1_1(1:4,ig) = CPM1_0(1:4,ig)
   EPSB_1(ig)     = EPSB_0(ig)
   VMIS_1(ig)     = VMIS_0(ig)

   !
   ! calcul de DNX,DVOLU,R
   !

   CALL GRADIENT_ISO(N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                     mecaEF(i)%PG(ig)%POIDS,X0,DNX,DVOLU,R)

   VOLU0=VOLU0+DVOLU

! CALCUL DU GRADIENT DES DEFORMATIONS

   GRADF(1)=1.D0
   GRADF(2)=1.D0
   GRADF(3)=0.D0
   GRADF(4)=0.D0
   GRADF(5)=1.D0

!*** Loi hyperelastique: Calcul de [1+d(u_n+1)/d(x_0)]

   DO INODE=1,N_NODE
     GRADF(1)=GRADF(1)+(U(1,INODE)*DNX(1,INODE))
     GRADF(2)=GRADF(2)+(U(2,INODE)*DNX(2,INODE))
     GRADF(3)=GRADF(3)+(U(1,INODE)*DNX(2,INODE))
     GRADF(4)=GRADF(4)+(U(2,INODE)*DNX(1,INODE))
                !1234567890
     IF(DIME_mod.EQ.i_2D_axisym) THEN
       GRADF(5)=GRADF(5) + (U(1,INODE)*mecaEF(i)%PG(ig)%N(INODE)/R)
     ENDIF
   ENDDO

   JGRADF = ((GRADF(1)*GRADF(2))-(GRADF(3)*GRADF(4)))*GRADF(5)

   IF (JGRADF.LE.0.D0) THEN
     ICOPLA=1
     PRINT*,'Det F < 0'
     EXIT ! on sort de la boucle sur les points de Gauss
   ENDIF

! CALCUL DU TENSEUR DE FINGER (Be)n+1 en utilisant [dF0->n+1]
! operation de push forward

   FING(1,IG)= (CPM1_0(1,ig)*GRADF(1)*GRADF(1))+&
               (CPM1_0(2,ig)*GRADF(3)*GRADF(3))+&
               (2.D0*(CPM1_0(3,ig)*GRADF(3)*GRADF(1)))
   FING(2,IG)= (CPM1_0(1,ig)*GRADF(4)*GRADF(4))+&
               (CPM1_0(2,ig)*GRADF(2)*GRADF(2))+&
               (2.D0*(CPM1_0(3,ig)*GRADF(2)*GRADF(4)))
   FING(3,IG)= (CPM1_0(1,ig)*GRADF(1)*GRADF(4))+&
               (CPM1_0(3,ig)*GRADF(1)*GRADF(2))+&
               (CPM1_0(3,ig)*GRADF(4)*GRADF(3))+&
               (CPM1_0(2,ig)*GRADF(3)*GRADF(2))
   FING(4,IG)= CPM1_0(4,ig)*GRADF(5)*GRADF(5)

! CALCUL DU TENSEUR DE FINGER MODIFIE TILDE(Be)n+1 

   FING(:,ig) = (JGRADF**(-2.D0/3.D0))*FING(:,ig)
!
! CALCUL DE LA TRACE DU TENSEUR DE FINGER MODIFIE

   TFING(ig)=(FING(1,ig)+FING(2,ig)+FING(4,ig))/3.D0  

! ON CONSERVE LA PARTIE DEVIATORIQUE DU TENSEUR DE FINGER MODIFIE

   FING(1,ig)=FING(1,ig)-TFING(ig)
   FING(2,ig)=FING(2,ig)-TFING(ig)
   FING(3,ig)=FING(3,ig) 
   FING(4,ig)=FING(4,ig)-TFING(ig)

!*** CALCUL DU VOLUME DE L'ELEMENT DEFORMEE

   VOLU=VOLU+(JGRADF*DVOLU)

   TRABE=TRABE+TFING(IG)

! CALCUL DE LA CONTRAINTE D'ESSAI DEVIATORIQUE
! dev(tau^tr)  
 
   DTAU(:,ig)= MUELAS*FING(:,ig)

! J2^tr     

   J2= (DTAU(1,ig)*DTAU(1,ig))+ &
       (DTAU(2,ig)*DTAU(2,ig))+ &
       (DTAU(4,ig)*DTAU(4,ig))+ &
       (2.d0*DTAU(3,ig)*DTAU(3,ig))

! petite modif. permettant de palier au plantage du a retour elastique 

   IF(J2.LE.1.0D+38) THEN
     J2=DSQRT(J2)
   ELSE 
     PRINT*,'WARNING: on tronque J2'
     J2=1.0D+20
   ENDIF
      
   NORM_DTAU_Tr(ig) = J2

   VMIS_1(ig)=ROOT32*J2

! ON CONSERVE LA NORMALE A LA SURFACE DE CHARGE POUR LE MODULE TANGENT

   IF(J2 == 0.D0) J2=1.D0

   NORMA(:,ig)= DTAU(:,ig)/J2


!*** VON MISES trial (juste si elastique !)

! ON RECUPERE LA LIMITE ELASTIQUE DE LA FIN L'INCREMENT CV PRECEDENT
!   GRACE A LA VALEUR DE LA DEFO PLASTIQUE CUMULEE EPSB_0

   CALL SEUDUB(EPSB_0(ig),SIG0,DSIG0,iso_hard,isoh_coeff)

! ON CALCULE PAR RETOUR RADIAL:
!
!   DTAU      : (E) PREDICTION ELASTIQUE DE LA CONTRAINTE 
!               (S) CONTRAINTE, CORRIGEE SI NECESSAIRE
! ---------
!   EPSB_1    : LA NOUVELLE DEFORMATION PLASTIQUE CUMULEE
! ---------
!   PLAS_STAT : ETAT PLASTIQUE = 0  si elastique 
!                              = 1  si plastique 
!     --------------------------------------------------
! *** VALEUR AUX POINTS D INTEGRATION CARACTERISANT LE
!     COMPORTEMENT; DEUX CAS:
!       >ON ETAIT ELASTIQUE ON PLASTIFIE
!       >ON ETAIT PLASTIQUE ET LA DERIVEE DE LA FONCTION 
!        DE CHARGE EST POSITIVE
!     --------------------------------------------------

   PLAS_STAT(IG) = 0

   IF ((VMIS_0(IG)-SIG0).LT.0.D0) THEN
     IF ((VMIS_1(ig)-SIG0).GT.0.D0)       PLAS_STAT(IG)= 1 
   ELSE
     IF ((VMIS_1(ig)-VMIS_0(ig)).GT.0.D0) PLAS_STAT(IG)= 1
   END IF

   IF (PLAS_STAT(IG) == 1) THEN

! DANS LE CAS OU ON PLASTIFIE, ON EFFECTUE UNE CORRECTION...
!   ...DE DELTA(GAMMA).

     MMUELAS = MUELAS*TFING(ig)

!*** CORRECTION DE SURFACE DE CHARGE AVEC ALGORITHME IMPLICITE

     CRITER=VMIS_1(ig)
     VONMISI=VMIS_1(ig)

     ETOL=1.D-10
     DELGAM=0.D0

     IF (VONMISI == 0.D0) VONMISI=1.D0

     IITER=1
!
!    retour radial
!
     DO 

       CRITER=CRITER-SIG0

       IF (DABS(CRITER/VONMISI).GT.ETOL) THEN
         IITER=IITER+1
         IF (iiter == 50 ) THEN
           write(cout,'(A)') 'non convergence de la plasticite'
           write(cout,'(A,D14.7,1x,A,D14.7)') 'test d arret = ',DABS(CRITER/VONMISI),'tol = ',etol 
           call faterr(IAM,cout)
         ENDIF 
       ELSE 
         EXIT
       END IF

!*** CALCUL DES TERMES DE CORRECTION

       DCRITER=-ROOT32*2.D0*MMUELAS*(1.d0+(DSIG0/(3.D0*MMUELAS)))

       DELGAM=DELGAM-(CRITER/DCRITER)

!*** ACTUALISATION

       CRITER    = VMIS_1(ig) - (ROOT32*2.D0*MMUELAS*DELGAM)
       EPSB_1(ig) = EPSB_0(ig) + (ROOT23*DELGAM)

       CALL SEUDUB(EPSB_1(ig),SIG0,DSIG0,iso_hard,isoh_coeff)

     END DO

     CRITER    = VMIS_1(ig) - (ROOT32*2.D0*MMUELAS*DELGAM)

     IF (IITER.GT.50) THEN
       WRITE(*,*) 'VONMIS = ', CRITER,' SIG0 = ',SIG0,' ETOL = ',ETOL
       WRITE(*,*) ' Le retour radial n''a pu etre realise en 50'
       WRITE(*,*) ' element :',i
       ICOPLA=1
       EXIT
     END IF

     DTAU(:,ig) = DTAU(:,ig) - (2.D0*MMUELAS*DELGAM*NORMA(:,ig))

! ON REACTUALISE LA PARTIE DEVIATORIQUE DE FINGEUR

     FING(:,ig) = DTAU(:,ig)/MUELAS

! dev(tau)

     J2 =  (DTAU(1,ig)*DTAU(1,ig))+ &
           (DTAU(2,ig)*DTAU(2,ig))+ &
           (DTAU(4,ig)*DTAU(4,ig))+ &
           (2.D0*DTAU(3,ig)*DTAU(3,ig))

!***ON FABRIQUE UN TABLEAU DE CES DERNIERES VALEURS POUR PLUS TARD

     DTAUDTAU(ig) = J2

!   petite modif. permettant de palier au plantage du a retour elastique 
      
     IF(J2.LE.1.0D+38) THEN
       J2=DSQRT(J2)
     ELSE
       PRINT*,'WARNING: on tronque J2'
       J2=1.0D+20
     ENDIF

!*** VON MISES

     VMIS_1(ig)=ROOT32*J2
      
   ENDIF

ENDDO

!!!!!!!! FIN PREMIERE BOUCLE POINTS DE GAUSS !!!!!!!!!!!!!!!

IF (icopla == 1) THEN
     !!!!!!!! des flags a modifier
  PRINT*,'Non convergence de la plasticite !'
  DEALLOCATE(dnx)
  RETURN
ENDIF


!*** CALCUL DU CHAMP J MODIFIE

MJGRADF = VOLU/VOLU0

!*** CALCUL DE LA PRESSION MODIFIE U(XJM)= (K/2)[1/2((XJM)**2 - 1) - LnXJM]

PRES = (KELAS/2.D0) * (MJGRADF-(1.D0/MJGRADF))

TRABE=TRABE/N_PG_RIG

!*** CALCUL DE L'OPERATEUR DIVERGENCE MODIFIE ET DE LA NOUVELLE CONTRAINTE

! Actualisation des coordonnees des noeuds

X = X0 + U

!!!!!!!! DEBUT DEUXIEME BOUCLE POINTS DE GAUSS !!!!!!!!!!!!!!!

DIVB=ZERO
VOLU1=ZERO

DO IG=1,N_PG_RIG

   MMUELAS=MUELAS*TFING(IG)

   CALL GRADIENT_ISO(N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                     mecaEF(i)%PG(ig)%POIDS,X,DNX,DVOLU,R)

   VOLU1=VOLU1+DVOLU

! CALCUL DE L'OPERATEUR DIVERGENCE

   BMATV=ZERO
 
   jl=0
   DO INODE=1,N_NODE
     il=jl+1
     jl=il+1

!***Matrice volumique

     BMATV(il)=DNX(1,INODE) ! verifier si c'est range pareil que dans SIMEM
     BMATV(jl)=DNX(2,INODE)
                !1234567890
     IF(DIME_mod.EQ.i_2D_axisym)THEN
       BMATV(il)=DNX(1,INODE)+(mecaEF(i)%PG(ig)%N(INODE)/R)
     ENDIF
   ENDDO

!*** CALCUL DE L'OPERATEUR BAR_DIV 

   DO IELE=1,N_NODE*N_DOF_by_NODE
     DIVB(IELE)=DIVB(IELE)+(BMATV(IELE)*DVOLU/VOLU)
   ENDDO

!*** CALCUL DE L'INVERSE DU GRADIENT DE LA TRANSFORMATION

! CALCUL DU GRADIENT DES DEFORMATIONS

   GRADF(1)=1.D0
   GRADF(2)=1.D0
   GRADF(3)=0.D0
   GRADF(4)=0.D0
   GRADF(5)=1.D0

!*** Loi hyperelastique: Calcul de [1-d(Delta_u_n+1)/d(x_n+1)]

   DO INODE=1,N_NODE
     GRADF(1)=GRADF(1)-(U(1,INODE)*DNX(1,INODE))   ! Le DNX a change
     GRADF(2)=GRADF(2)-(U(2,INODE)*DNX(2,INODE))
     GRADF(3)=GRADF(3)-(U(1,INODE)*DNX(2,INODE))
     GRADF(4)=GRADF(4)-(U(2,INODE)*DNX(1,INODE))

                !1234567890
     IF(DIME_mod.EQ.i_2D_axisym) THEN
!       print*,'retouche gradf'
       GRADF(5)=GRADF(5)-(U(1,INODE)*mecaEF(i)%PG(ig)%N(INODE)/R)
     ENDIF
   ENDDO

   JGRADF = ((GRADF(1)*GRADF(2))-(GRADF(3)*GRADF(4)))*GRADF(5)

   IF (JGRADF.LE.0.D0) THEN
     ICOPLA=1
     PRINT*,'detF <0'
     EXIT
   ENDIF


   IF (PLAS_STAT(IG) == 1) THEN


!*** CALCUL DE Tr(be) simo-miehe 1992

!     0.5*||DEV(TAU)||**2.D0/MU**2.D0

!     UTRABE1 = 0.5D0*DTAUDTAU(ig) / (MUELAS**2.D0)

!     DET(DEV(TAU))/MU**3.D0

!     UTRABE2 = (((DTAU(1,ig)*DTAU(2,ig))-  &
!                 (DTAU(3,ig)*DTAU(3,ig)))* & 
!                  DTAU(4,ig))/(MUELAS**3.D0)

!     UTRABE3 = (1.D0-UTRABE2)/2.D0
!     UTRABE4 = -((UTRABE1*UTRABE1*UTRABE1)/27.D0)+(UTRABE3*UTRABE3)

!     TRABE = ((UTRABE3+DSQRT(UTRABE4))**(1.D0/3.D0)) + ((UTRABE3-DSQRT(UTRABE4))**(1.D0/3.D0))

!*** CALCUL DU NOUVEAU TENSEUR Cp**-1 

     CPN11=(FING(1,ig) + TRABE)*(JGRADF**(-2.D0/3.D0))
     CPN12=(FING(2,ig) + TRABE)*(JGRADF**(-2.D0/3.D0))
     CPN13=(FING(3,ig)        )*(JGRADF**(-2.D0/3.D0))
     CPN14=(FING(4,ig) + TRABE)*(JGRADF**(-2.D0/3.D0))

!*** TRANSPORT PAR CONVECTION DANS LA CONFIG DE REF PAR GRADI

     CPM1_1(1,ig)=(CPN11*GRADF(1)*GRADF(1))+(CPN12*GRADF(3)*GRADF(3))+(2.D0*(CPN13*GRADF(3)*GRADF(1)))
     CPM1_1(2,ig)=(CPN11*GRADF(4)*GRADF(4))+(CPN12*GRADF(2)*GRADF(2))+(2.D0*(CPN13*GRADF(2)*GRADF(4)))
     CPM1_1(3,ig)=(CPN11*GRADF(1)*GRADF(4))+(CPN13*GRADF(1)*GRADF(2))+(CPN13*GRADF(4)*GRADF(3))+ &
                             (CPN12*GRADF(3)*GRADF(2))
     CPM1_1(4,ig)=CPN14*GRADF(5)*GRADF(5)

   END IF

!*** CALCUL DE LA NOUVELLE CONTRAINTE DE CAUCHY

   CAUCHY(1,ig)= (DTAU(1,ig)*JGRADF) + PRES
   CAUCHY(2,ig)= (DTAU(2,ig)*JGRADF) + PRES
   CAUCHY(3,ig)= (DTAU(3,ig)*JGRADF)
   CAUCHY(4,ig)= (DTAU(4,ig)*JGRADF) + PRES

!***STOCKAGE DES VALEURS DES CONTRAINTES DE CAUCHY
   Sloc(:,ig)  = CAUCHY(:,ig)
   VPG(1:4,ig) = CPM1_1(1:4,ig)
   VPG(5,ig)   = EPSB_1(IG) 
   VPG(6,ig)   = VMIS_1(IG)

!    print*,'VonMisesACTUA=',VPG(6)
!    print*,'defo plast actua=',VPG(5)
!    print*,'cauchy',CAUCHY(:,ig)



!   print*,'Pour:',ibdyty,iblmty,ig,'on envoie:'
!   print*,VPG(1),VPG(2),VPG(3),VPG(4)
!   print*,VPG(5),VPG(6)


ENDDO

!!!!!!!! FIN DEUXIEME BOUCLE POINTS DE GAUSS !!!!!!!!!!!!!!!


!     --------------------------------------------------
! FD   CALCUL DE LA MATRICE DE RIGIDITE...
!     --------------------------------------------------
!
!*** DEUXIEME BOUCLE D'INTEGRATION 

DO IG=1,N_PG_RIG

   MMUELAS=MUELAS*TFING(IG)

   CALL GRADIENT_ISO(N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                     mecaEF(i)%PG(ig)%POIDS,X,DNX,DVOLU,R)

! CALCUL D'UNE MATRICE SIGMA INITIALE

   CAUCHYI = 0.D0

!fd   DO il=1,2
!fd     DO jl=1,2
!fd       A1=2*(il-1)+1
!fd       B1=2*(jl-1)+1
!fd       A2=2*(il-1)+2
!fd       B2=2*(jl-1)+2
!fd       DELTAB=0.D0
!fd       IF(il.EQ.jl)DELTAB=1.D0
!fd       CAUCHYI(A1,B1) =CAUCHY(1,ig)*DELTAB
!fd       CAUCHYI(A2,B1) =CAUCHY(3,ig)*DELTAB
!fd       CAUCHYI(A1,B2) =CAUCHY(3,ig)*DELTAB
!fd       CAUCHYI(A2,B2) =CAUCHY(2,ig)*DELTAB
!fd     END DO
!fd   END DO

     CAUCHYI(1,1) = CAUCHY(1,ig)
     CAUCHYI(1,3) = CAUCHY(3,ig)
     CAUCHYI(2,2) = CAUCHY(1,ig)
     CAUCHYI(2,4) = CAUCHY(3,ig)
     CAUCHYI(3,1) = CAUCHY(3,ig)
     CAUCHYI(3,3) = CAUCHY(2,ig)
     CAUCHYI(4,2) = CAUCHY(3,ig)
     CAUCHYI(4,4) = CAUCHY(2,ig)

              !1234567890
   IF(DIME_mod.EQ.i_2D_axisym) THEN
     CAUCHYI(5,5)=CAUCHY(4,ig)
   ENDIF

!*** CALCUL DE LA MATRICE B ELEMENTS FINIS
!*** EVALUE LA MATRICE D INTERPLOLATION DES DEFO.

   BMATU=ZERO

   jl=0
   DO INODE=1,N_NODE
     il=jl+1
     jl=il+1
     BMATU(1,il)=DNX(1,INODE)
     BMATU(1,jl)=0.D0
     BMATU(2,il)=0.D0
     BMATU(2,jl)=DNX(2,INODE)
     BMATU(3,il)=DNX(2,INODE)
     BMATU(3,jl)=DNX(1,INODE)
     BMATU(4,il)=0.D0
     BMATU(4,jl)=0.D0
                !1234567890
     IF(DIME_mod.EQ.i_2D_axisym) THEN
       BMATU(4,il)=mecaEF(i)%PG(ig)%N(INODE)/R
       BMATU(4,jl)=0.D0
     ENDIF
   ENDDO

!*** EVALUE LES MATRICES GRADV ET BETEND

   GRADV=ZERO
   BETEN=ZERO

   jl=0
   DO INODE=1,N_NODE
     il=jl+1
     jl=il+1

!fd     GRADV(1,il)=DNX(1,INODE)
!fd     GRADV(1,jl)=0.D0
!fd     GRADV(2,il)=DNX(2,INODE)
!fd     GRADV(2,jl)=0.D0
!fd     GRADV(3,il)=0.D0
!fd     GRADV(3,jl)=DNX(1,INODE)
!fd     GRADV(4,il)=0.D0
!fd     GRADV(4,jl)=DNX(2,INODE)

     GRADV(1,il)=DNX(1,INODE)
     GRADV(1,jl)=0.D0
     GRADV(2,il)=0.D0
     GRADV(2,jl)=DNX(1,INODE)
     GRADV(3,il)=DNX(2,INODE)
     GRADV(3,jl)=0.D0
     GRADV(4,il)=0.D0
     GRADV(4,jl)=DNX(2,INODE)

     IF (INDIC.EQ.0) THEN
       BETEN(1,il)=DNX(1,INODE)
       BETEN(1,jl)=0.D0
       BETEN(2,il)=0.5D0*DNX(2,INODE)
       BETEN(2,jl)=0.5D0*DNX(1,INODE)
       BETEN(3,il)=0.5D0*DNX(2,INODE)
       BETEN(3,jl)=0.5D0*DNX(1,INODE)
       BETEN(4,il)=0.D0
       BETEN(4,jl)=DNX(2,INODE)
     ENDIF
                !1234567890
     IF(DIME_mod == i_2D_axisym) THEN
       GRADV(5,il)=mecaEF(i)%PG(ig)%N(INODE)/R
       GRADV(5,jl)=0.D0
       IF (INDIC.EQ.0) THEN
         BETEN(5,il)=mecaEF(i)%PG(ig)%N(INODE)/R
         BETEN(5,jl)=0.D0
       ENDIF
     ENDIF
   ENDDO

!***  CALCUL DE LA MATRICE ELASTIQUE 

   CONS = 2.D0*MMUELAS
   CONS1=(2.D0/3.D0)*NORM_DTAU_Tr(IG)

   CELAS=ZERO

   CELAS(1,1)= ((2.D0/3.D0)*CONS)-(CONS1*(NORMA(1,ig)+NORMA(1,ig))) 
   CELAS(1,2)=-((1.D0/3.D0)*CONS)-(CONS1*(NORMA(1,ig)+NORMA(2,ig)))
   CELAS(1,3)=                   -(CONS1* NORMA(3,ig))  
   CELAS(2,1)= CELAS(1,2)
   CELAS(2,2)= ((2.D0/3.D0)*CONS)-(CONS1*(NORMA(2,ig)+NORMA(2,ig)))  
   CELAS(2,3)=                   -(CONS1* NORMA(3,ig))  
   CELAS(3,1)= CELAS(1,3)  
   CELAS(3,2)= CELAS(2,3)  
   CELAS(3,3)= CONS/2.D0 

!-----------------------------------------------------------------------
!      MATERIAU ELASTIQUE HOMOGENE ISOTROPE EN AXISYMETRIE
!-----------------------------------------------------------------------
               !1234567890
   IF (DIME_mod == i_2D_axisym) THEN
     CELAS(1,4)= (-(1.D0/3.D0)*CONS) - (CONS1*(NORMA(4,ig)+NORMA(1,ig)))
     CELAS(2,4)= (-(1.D0/3.D0)*CONS) - (CONS1*(NORMA(4,ig)+NORMA(2,ig)))
     CELAS(3,4)=                     -  CONS1*NORMA(3,ig)  
     CELAS(4,1)= CELAS(1,4)
     CELAS(4,2)= CELAS(2,4)
     CELAS(4,3)= CELAS(3,4)
     CELAS(4,4)= ( (2.D0/3.D0)*CONS) - (CONS1*(NORMA(4,ig)+NORMA(4,ig)))  
   END IF


   IF(PLAS_STAT(IG) == 1) THEN

     CEP=ZERO

!
!***    Correction elasto-plastique sur la matrice de comportement
!       si  plas_stat(IG) est positif 
!       a voir epsbd -epsb 

     DELGAM=(EPSB_1(IG)- EPSB_0(IG))*ROOT32  

!*** CALCUL DES FACTEURS BETA

     CALL SEUDUB(EPSB_1(IG),SIG0,DSIG0,iso_hard,isoh_coeff)

     BETA0 = 1.D0+(DSIG0/(3.D0*MMUELAS))

!fd est ce bon
     BETA1 = CONS*DELGAM/NORM_DTAU_Tr(IG) 

     BETA2 = (1.D0-(1.D0/BETA0))*CONS1*DELGAM/MMUELAS

! attention 2*mu_bar*beta3
     BETA3 = ((1.D0/BETA0)-BETA1+BETA2)*2.D0*MMUELAS
! attention 2*mu_bar*beta4
     BETA4 = ((1.D0/BETA0)-BETA1)*NORM_DTAU_Tr(ig)*2.D0

 !*** CALCUL DE DEV(N**2)

     DEVN2XX  = (NORMA(1,ig)*NORMA(1,ig)) + &
                (NORMA(3,ig)*NORMA(3,ig))
     DEVN2YY  = (NORMA(2,ig)*NORMA(2,ig)) + &
                (NORMA(3,ig)*NORMA(3,ig))
     DEVN2XY  = (NORMA(3,ig)*NORMA(1,ig)) + &
                (NORMA(2,ig)*NORMA(3,ig))
     DEVN2ZZ  = (NORMA(4,ig)*NORMA(4,ig))
     TRADEVN2 = (DEVN2XX+DEVN2YY+DEVN2ZZ)/3.D0
     DEVN2XX  = DEVN2XX-TRADEVN2
     DEVN2YY  = DEVN2YY-TRADEVN2
     DEVN2ZZ  = DEVN2ZZ-TRADEVN2

     CEP(1,1)=-(CELAS(1,1)*BETA1)-(BETA3*NORMA(1,ig)*NORMA(1,ig)) &
              -(BETA4*NORMA(1,ig)*DEVN2XX)
     CEP(1,2)=-(CELAS(1,2)*BETA1)-(BETA3*NORMA(1,ig)*NORMA(2,ig)) &
              -(BETA4*(NORMA(1,ig)*DEVN2YY+DEVN2XX*NORMA(2,ig))/2.D0)
     CEP(1,3)=-(CELAS(1,3)*BETA1)-(BETA3*NORMA(1,ig)*NORMA(3,ig)) &
              -(BETA4*(NORMA(1,ig)*DEVN2XY+DEVN2XX*NORMA(3,ig))/2.D0)
     CEP(2,1)= CEP(1,2)
     CEP(2,2)=-(CELAS(2,2)*BETA1)-(BETA3*NORMA(2,ig)*NORMA(2,ig)) &
              -(BETA4*NORMA(2,ig)*DEVN2YY)
     CEP(2,3)=-(CELAS(2,3)*BETA1)-(BETA3*NORMA(2,ig)*NORMA(3,ig)) &
              -(BETA4*(NORMA(2,ig)*DEVN2XY+DEVN2YY*NORMA(3,ig))/2.D0)
     CEP(3,1)= CEP(1,3)
     CEP(3,2)= CEP(2,3)
     CEP(3,3)=-(CELAS(3,3)*BETA1)-(BETA3*NORMA(3,ig)*NORMA(3,ig)) &   
              -(BETA4*NORMA(3,ig)*DEVN2XY)

!*** AXISYMMETRIE
                !1234567890
     IF(DIME_mod.EQ.i_2D_axisym) THEN
       CEP(1,4)=-(CELAS(1,4)*BETA1)-(BETA3*NORMA(1,ig)*NORMA(4,ig)) &
                -(BETA4*(NORMA(1,ig)*DEVN2ZZ+DEVN2XX*NORMA(4,ig))/2.D0)
       CEP(2,4)=-(CELAS(2,4)*BETA1)-(BETA3*NORMA(2,ig)*NORMA(4,ig)) &
                -(BETA4*(NORMA(2,ig)*DEVN2ZZ+DEVN2YY*NORMA(4,ig))/2.D0)
       CEP(3,4)=-(CELAS(3,4)*BETA1)-(BETA3*NORMA(3,ig)*NORMA(4,ig)) &
                -(BETA4*(NORMA(3,ig)*DEVN2ZZ+DEVN2XY*NORMA(4,ig))/2.D0)
       CEP(4,1)=CEP(1,4)
       CEP(4,2)=CEP(2,4)
       CEP(4,3)=CEP(3,4)
       CEP(4,4)=-(CELAS(4,4)*BETA1)-(BETA3*NORMA(4,ig)*NORMA(4,ig)) & 
                -(BETA4*NORMA(4,ig)*DEVN2ZZ)
     ENDIF

     CEP = CEP + CELAS


   ELSE
     CEP=CELAS
   ENDIF


!*** ON VIENT CALCULER P(1*1-2I)

!***INTEGRATION REDUITE

   MPRES(1,1)= -PRES  !+(BELAS*(1.D0+(1.D0/(XJM**2)))*XJM)
   MPRES(1,2)=  PRES  !+(BELAS*(1.D0+(1.D0/(XJM**2)))*XJM)
   MPRES(1,3)=  0.D0
   MPRES(1,4)=  PRES  !+(BELAS*(1.D0+(1.D0/(XJM**2)))*XJM)
   MPRES(2,1)=  MPRES(1,2)
   MPRES(2,2)= -PRES  !+(BELAS*(1.D0+(1.D0/(XJM**2)))*XJM)
   MPRES(2,3)= 0.D0
   MPRES(2,4)= PRES  !+(BELAS*(1.D0+(1.D0/(XJM**2)))*XJM)
   MPRES(3,1)= MPRES(1,3)
   MPRES(3,2)= MPRES(2,3)
   MPRES(3,3)= -PRES
   MPRES(3,4)= 0.D0
   MPRES(4,1)= MPRES(1,4)
   MPRES(4,2)= MPRES(2,4)
   MPRES(4,3)= MPRES(3,4)
   MPRES(4,4)= -PRES  !+(BELAS*(1.D0+(1.D0/(XJM**2)))*XJM)

   CEP=CEP+MPRES

!!     print*,'CEP=',CEP(:,:)


   DO Il=1,KS
     DO IELE=1,N_NODE*N_DOF_by_NODE
       MDB(Il,IELE)=0.D0
       DO Jl=1,KS
         MDB(Il,IELE)=MDB(Il,IELE)+(CEP(Il,Jl)*BMATU(Jl,IELE))
       ENDDO
     ENDDO
   ENDDO

!**** CE SOUS PROGRAMME EFFECTUE LES PRODUITS CAUCHY*GRADV ET
!     CAUCHY*BETEN

!fd   DO IELE=1,N_NODE*N_DOF_by_NODE
!fd     DO il=1,5
!fd       CGRADV(il,IELE)=0.D0
!fd       CBETEN(il,IELE)=0.D0
!fd     END DO
!fd     DO il=1,4
!fd       DO jl=1,4
!fd         CGRADV(il,IELE)=CGRADV(il,IELE)+(CAUCHYI(il,jl)*GRADV(jl,IELE))
!fd         IF (INDIC.EQ.0) THEN 
!fd           CBETEN(il,IELE)=CBETEN(il,IELE)+(CAUCHYI(il,jl)*BETEN(jl,IELE))
!fd         ENDIF
!fd       END DO
!fd     END DO
!fd     IF(DIME.EQ.i_2D_axisym)THEN
!fd       CGRADV(5,IELE)=CGRADV(5,IELE)+(CAUCHYI(5,5)*GRADV(5,IELE))
!fd       IF (INDIC.EQ.0) THEN
!fd         CBETEN(5,IELE)=CBETEN(5,IELE)+(CAUCHYI(5,5)*BETEN(5,IELE))
!fd       ENDIF
!fd     ENDIF
!fd   END DO


   DO il=1,KS+1
     DO IELE=1,N_NODE*N_DOF_by_NODE
       CGRADV(il,IELE)=0.D0
       CBETEN(il,IELE)=0.D0
       DO jl=1,KS+1
         CGRADV(il,IELE)=CGRADV(il,IELE)+(CAUCHYI(il,jl)*GRADV(jl,IELE))
         IF (INDIC.EQ.0) CBETEN(il,IELE)=CBETEN(il,IELE)+(CAUCHYI(il,jl)*BETEN(jl,IELE))
       END DO
     END DO
   END DO

!*** CALCUL LA MATRICE DE RIGIDITE DE CHAQUE ELEMENT
!    MATRICE SYMETRIQUE

   DO IELE=1,N_NODE*N_DOF_by_NODE
     DO JELE=1,N_NODE*N_DOF_by_NODE
       DO Il=1,KS
         K(IELE,JELE)=K(IELE,JELE)+(BMATU(Il,IELE)*MDB(Il,JELE)*DVOLU)   
       ENDDO
     ENDDO
   ENDDO

!*** CALCUL DE U''(XJM) = K (1+(1/(XJM**2)))

   DO IELE=1,N_NODE*N_DOF_by_NODE
     DO JELE=1,N_NODE*N_DOF_by_NODE
       K(IELE,JELE)=K(IELE,JELE)+(DVOLU*DIVB(IELE)*DIVB(JELE)*(KELAS/2.D0)*(1.D0+(1.D0/(MJGRADF**2.D0)))*MJGRADF)
!    .     KELAS*(1.D0+(1.D0/(XJM**2.D0)))*XJM*XJI)
     ENDDO
   ENDDO

!*** CORRECTION DE RAIDEUR GEOMETRIQUE

   DO IELE=1,N_NODE*N_DOF_by_NODE
     DO JELE=1,N_NODE*N_DOF_by_NODE
       DO Il=1,KS+1
!fd
!fd semble poser probleme ?!
         K(IELE,JELE)=K(IELE,JELE)+(DVOLU*GRADV(Il,IELE)*CGRADV(Il,JELE))
                            !mado -(2.D0*BETEN(Il,IELE)*CBETEN(Il,JELE))
       ENDDO
     ENDDO
   ENDDO

! FD   CALCUL DU RESIDU...

   il=0
   DO IELE=1,N_NODE*N_DOF_by_NODE
      il=il+1
      DO jl=1,KS
        RESLOC(il)=RESLOC(il)+BMATU(jl,il)*CAUCHY(jl,ig)*DVOLU
      ENDDO
   ENDDO

ENDDO

IF (itchache) THEN
   PRINT*,'K=',K
   PRINT*,'Resloc=',resloc
ENDIF

IF (ICOPLA == 1) THEN
    ! qqch a faire
  PRINT*,'Non convergence de la plasticite'
ENDIF

DEALLOCATE(dnx)

RETURN

END SUBROUTINE BULK_NL_ISO

! **********************************************************************
      SUBROUTINE SEUDUB(EPSB,SIG0,DSIG0,iso_hard,isoh_coeff)                            
! =======================================================================
!     EPSB     deformation equivalente plastique a l'iteration consideree
!     DSIG0    derivee sur courbe d'ecrouissage         //
!     SIG0     contrainte equivalente                   //
! =======================================================================
      IMPLICIT NONE
      
      INTEGER :: iso_hard
      REAL(KIND=8),DIMENSION(:) :: isoh_coeff

      REAL(KIND=8)              :: EPSB
      REAL(KIND=8)              :: SIG0
      REAL(KIND=8)              :: DSIG0
!
      REAL(KIND=8)              :: EPST
      REAL(KIND=8)              :: K
      REAL(KIND=8)              :: SIG00
      REAL(KIND=8)              :: EPS00
      REAL(KIND=8)              :: AIN
      REAL(KIND=8)              :: AIN1
      REAL(KIND=8)              :: TEST
      REAL(KIND=8)              :: PETIT 

      PETIT=1.D-10
      SIG0=0.D0
      DSIG0=0.D0

!       -------------------------------------------             
!       LOI DE SWIFT: SIG0=SIG00*(EPS00+EPSB)**AIN
!       -------------------------------------------  

       IF (iso_hard == 1) THEN

         SIG00=isoh_coeff(1)
         EPS00=isoh_coeff(2)
         AIN  =isoh_coeff(3)

         EPST=EPS00+EPSB                                                   

! ON SEPARE LE CAS DE L'ECROUISSAGE LINEAIRE

         TEST = DABS(AIN - 1.D0)
         IF (TEST.LT.1.D-20) THEN
           SIG0 = SIG00*EPST
           DSIG0 = SIG00
         ELSE
           SIG0=SIG00*(EPST**AIN)                                          
           AIN1=AIN-1.D0                                                     
           DSIG0=SIG00*AIN*(EPST**AIN1) 
         ENDIF
!       -------------------------------------------             
!       LOI DE HOLLOMON: SIG0=SIG00*(EPSB**AIN)
!       -------------------------------------------  

       ELSE IF (iso_hard == 3) THEN

         IF(EPSB.LT.PETIT) THEN
            SIG0=0.
            DSIG0=0.
         ELSE
            SIG00=isoh_coeff(1)
            AIN=isoh_coeff(2)
            SIG0=SIG00*(EPSB**AIN)                                          
            AIN1=AIN-1.                                                     
            DSIG0=SIG00*AIN*(EPSB**AIN1)                                    
         END IF
!       -------------------------------------------             
!       LOI LINEAIRE: SIG0= SIG00 + K*EPSB
!       ------------------------------------------- 

       ELSE IF (iso_hard == 2) THEN

         SIG00=isoh_coeff(1)
         K=isoh_coeff(2)
         SIG0 = SIG00 + K*EPSB
         DSIG0 = K 

       ELSE
         call faterr('a_mecaEF_iso::SEUDUB','loi de plasticite non supportee' )
       ENDIF

      RETURN
      END SUBROUTINE SEUDUB                                                              


!*************************************************************************

!> computes deformation energy 
SUBROUTINE ENERGY_ISO(i,ppsnb,ibdyty,iblmty,X,E_def)

 IMPLICIT NONE

 ! le numero de l'element
 INTEGER      , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty
 INTEGER                   :: mdlnb,lawnb 
 ! coordonnees/dep/vit des sommets
 REAL(KIND=8)              :: X(:,:)

 !*** variables locales

 REAL(KIND=8) , POINTER    :: DNX(:,:)

 ! vecteur de travail local
 REAL(KIND=8) ,ALLOCATABLE :: Sloc(:),Eloc(:)     

 INTEGER                   :: IG
 REAL(KIND=8)              :: COEFINT,R,rho,E_def

 INTEGER                   :: n_stress_gp

 CALL get_ppset_value(ppsnb(1),mdlnb,lawnb) 
 
 n_stress_gp=get_nb_external_variables(mdlnb)

 ! Initialisation a vide des pointeurs
 NULLIFY(DNX)

 ALLOCATE(Sloc(n_stress_gp),Eloc(n_stress_gp))

 E_def = 0.d0

 DO IG=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss

   CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                     mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

   CALL get_stress_1_MAILx(ibdyty,iblmty,ig,Sloc)
   CALL get_strain_1_MAILx(ibdyty,iblmty,ig,Eloc)

   E_def = E_def + (0.5*DOT_PRODUCT(Eloc,Sloc)*COEFINT)
 
 ENDDO

 DEALLOCATE(DNX,sloc,Eloc) ; NULLIFY(DNX)

END SUBROUTINE ENERGY_ISO

!*************************************************************************

SUBROUTINE POWER_ISO(i,mdlnb,lawnb,ibdyty,iblmty,X,V,P_def)

 IMPLICIT NONE

 ! le numero de l'element
 INTEGER      , INTENT(IN) :: i,mdlnb,lawnb 
 INTEGER                   :: ibdyty,iblmty
 ! coordonnees/vit
 REAL(KIND=8)              :: X(:,:),V(:)
 REAL(KIND=8)              :: P_def

 ! variables locales
 REAL(KIND=8) , POINTER    :: DNX(:,:),Bl(:,:)

 ! vecteur de travail local
 REAL(KIND=8) ,ALLOCATABLE :: Sloc(:),Eloc(:)     

 INTEGER                   :: IG
 REAL(KIND=8)              :: COEFINT,R,rho

 INTEGER                   :: n_stress_gp,istrg

                           !123456789012345678901234567
  CHARACTER(len=27) :: IAM='a_mecaEF_iso::compute_power'


 n_stress_gp=get_nb_external_variables(mdlnb)

 ! Initialisation a vide des pointeurs
 NULLIFY(DNX)

 ALLOCATE(Sloc(n_stress_gp),Eloc(n_stress_gp))

 P_def = 0.d0

 ! Pour tous les points de Gauss
 DO IG=1,mecaEF(i)%N_PG_RIG                 

   CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                     mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

   CALL get_stress_1_MAILx(ibdyty,iblmty,ig,Sloc)

   select case(get_eleop_value(mdlnb,'kine_'))
   case('small')
     eloc=0.d0
     if (get_eleop_value(mdlnb,'isext') == 'no___') THEN

       istrg = 1
       CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl) ! formation de Bl 
                                
       Eloc(1:SIZE(Bl,dim=1)) = MATMUL(Bl,V)

     else if (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN

      istrg = 2
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl        

      Eloc = MATMUL(Bl,V)

     else if (get_eleop_value(mdlnb,'isext') == 'Demfi') THEN

      istrg = 2
      CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl        

      Eloc = MATMUL(Bl,V)


     else

       call faterr(IAM,get_eleop_value(mdlnb,'isext')//' not yet implemented')
 
     endif

   case('large')

     call logmes('[mod_a_mecaEF_iso] compute power_iso not available yet for large def')
     eloc=0.d0

   case default
     call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
     call FATERR(IAM,'kinematic type unknown (small | large)')
   end select

   P_def = P_def + (DOT_PRODUCT(Eloc,Sloc)*COEFINT)  ! s : desp/dt

   !print*,iblmty,ig,P_def
 
 ENDDO

 if( associated(Bl) ) deallocate(Bl)
 DEALLOCATE(DNX,sloc,Eloc) ; NULLIFY(DNX,Bl)

END SUBROUTINE POWER_ISO

!*************************************************************************

SUBROUTINE get_coor_pg_ISO(i,X,coor_pg)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i             ! le numero de l'element
REAL(KIND=8)              :: X(:,:)        ! coordonnees des sommets
REAL(KIND=8)              :: coor_pg(:,:)  ! coordonnees des points de Gauss
INTEGER                      :: ig,idime


 DO ig=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss
   do idime=1,nbDIME
     coor_pg(idime,ig)=dot_product(mecaEF(i)%PG(ig)%N(:),X(idime,:))
   enddo
 ENDDO

END SUBROUTINE get_coor_pg_ISO

!*************************************************************************

SUBROUTINE interpolate_node2pg_ISO(i,valnoe,valpg)

! routine calculant les coordonnees des points de Gauss

implicit none

INTEGER         , INTENT(IN) :: i           ! le numero de l'element
REAL(KIND=8)              :: valnoe(:)   ! valeurs aux sommets
REAL(KIND=8)              :: valpg(:)    ! valeurs aux points de Gauss
INTEGER                      :: ig

 DO ig=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss
   valpg(ig)=dot_product(mecaEF(i)%PG(ig)%N(:),valnoe(:))
 ENDDO

END SUBROUTINE interpolate_node2pg_ISO

!*************************************************************************

!------------------------------------------------------------------------

SUBROUTINE gpv2node_2D_iso(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,NbFields,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model of the element  
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  nodalvalues     : the computed values
!  nbfields        : 
!  nbnodes_stored  :

  IMPLICIT NONE
  INTEGER :: i,ibdyty,iblmty,nbfields,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele,ii
  REAL(kind=8),DIMENSION(:),allocatable :: GaussPointValues_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  REAL(kind=8) :: f1,f2,f3
  INTEGER :: NbNodes_stored,i_node

!                            1234567890123456789012345
  CHARACTER(len=25)  :: IAM='a_mecaEF_iso::gpv2node_2D'
  
  ! vecteurs de travail locaux
  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),Field(:,:)     
  integer :: ig,if,mdlnb,inull,nb_external,nb_internal

  real(kind=8) :: tmp,PP(5),FF(5)
  integer :: nbs,nbm

  !print*,'--------------'
  !print*,i,mecaEF(i)%NAME
  !print*,mecaEF(i)%N_NODE
  !print*,mecaEF(i)%N_PG_RIG 

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele

  if (nbfields /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes')
  endif

  allocate(GaussPointValues_ele(NbGp_ele))
  GaussPointValues_ele=0.d0

  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  allocate(Field(Nbfields,NbGp_ele))
  Field = 0.d0

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx(ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

    case(1) ! strain
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
        
        !CALL LOGMES(IAM//' when smoothing strain only external models are supported ')

      ELSE
         
       !fd en dur pour matlib. A transferer dans external
         
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf permutation
         do ig=1,NbGp_ele
           Field(1,ig) = GRAD(1,ig) 
           Field(2,ig) = GRAD(3,ig)
           Field(3,ig) = GRAD(2,ig)
           Field(4,ig) = GRAD(4,ig)
           Field(5,ig) = 1.0D0 + GRAD(1,ig) + GRAD(3,ig) + GRAD(4,ig)
         enddo
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           FF(:)=GRAD(:,ig)

           !fd calcul de F^-1 on stocke dans PP

           tmp=((FF(1)*FF(4)) - (FF(2)*FF(3)))
           if (tmp /= 0.d0) tmp=1.d0/tmp 

           PP(1) = tmp*FF(4)
           PP(2) =-tmp*FF(2) 
           PP(3) =-tmp*FF(3)
           PP(4) = tmp*FF(1)
           PP(5) = 0.d0
           if (FF(5) /= 0.d0) PP(5) = 1.d0/ FF(5)
          
           FIELD(1,ig)=0.5*(1.d0 - (PP(1)*PP(1) + PP(3)*PP(3))) !exx
           FIELD(2,ig)=0.5*(1.d0 - (PP(2)*PP(2) + PP(4)*PP(4))) !eyy 
           FIELD(3,ig)=0.5*(0.d0 - (PP(1)*PP(2) + PP(2)*PP(4))) !exy 
           FIELD(4,ig)=0.5*(1.d0 - (PP(5)*PP(5)))               !ezz
           FIELD(5,ig)=1.d0/tmp                                    !J
         enddo
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF
       

      ENDIF

    case(2) ! stress
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
        ! rien a faire sauf von mises
        do ig=1,NbGp_ele 
           FIELD(1,ig) = FLUX(1,ig)
           FIELD(2,ig) = FLUX(2,ig)
           FIELD(3,ig) = FLUX(3,ig)
           FIELD(4,ig) = FLUX(4,ig)

           tmp= (FIELD(1,ig)+FIELD(2,ig)+FIELD(4,ig))/3.d0
           PP(1) = FIELD(1,ig) - tmp
           PP(2) = FIELD(2,ig) - tmp
           PP(3) = FIELD(3,ig)
           PP(4) = FIELD(4,ig) - tmp

           FIELD(5,ig) = dsqrt(1.5*(PP(1)**2 + &
                                   PP(2)**2 + &
                                   PP(4)**2 + &
                                   (2.*(PP(3)**2)))) 
        end do         
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN 
        ! rien a faire sauf von mises
        do ig=1,NbGp_ele 
           FIELD(1,ig) = FLUX(1,ig)
           FIELD(2,ig) = FLUX(2,ig)
           FIELD(3,ig) = FLUX(3,ig)
           FIELD(4,ig) = FLUX(4,ig)

           FIELD(5,ig)=INTERNAL(6,ig)
        end do         
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF
      ELSE
       !fd en dur pour matlib. A transferer dans external

       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
         do ig=1,NbGp_ele
           FIELD(1,ig) = FLUX(1,ig)
           FIELD(2,ig) = FLUX(3,ig)
           FIELD(3,ig) = FLUX(2,ig)
           FIELD(4,ig) = FLUX(4,ig)

           tmp= (FIELD(1,ig)+FIELD(2,ig)+FIELD(4,ig))/3.d0
           PP(1) = FIELD(1,ig) - tmp
           PP(2) = FIELD(2,ig) - tmp
           PP(3) = FIELD(3,ig)
           PP(4) = FIELD(4,ig) - tmp

           FIELD(5,ig) = dsqrt(1.5*(PP(1)**2 + &
                                   PP(2)**2 + &
                                   PP(4)**2 + &
                                   (2.*(PP(3)**2)))) 

         enddo
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           PP(:)=FLUX(:,ig);FF(:)=GRAD(:,ig)
           tmp=(FF(5)*((FF(1)*FF(4)) - (FF(2)*FF(3))))
           if (tmp /= 0.d0) tmp=1.d0/tmp 
           FIELD(1,ig)=tmp*(PP(1)*FF(1) + PP(2)*FF(2)) !sxx
           FIELD(2,ig)=tmp*(PP(3)*FF(3) + PP(4)*FF(4)) !syy 
           FIELD(3,ig)=tmp*(PP(1)*FF(3) + PP(2)*FF(4)) !sxy 
           FIELD(4,ig)=tmp*(PP(5)*FF(5))               !szz
           FIELD(5,ig)=0.                              !svm
          
           !fd calcul du von mises

           tmp= (FIELD(1,ig)+FIELD(2,ig)+FIELD(4,ig))/3.d0
           PP(1) = FIELD(1,ig) - tmp
           PP(2) = FIELD(2,ig) - tmp
           PP(3) = FIELD(3,ig)
           PP(4) = FIELD(4,ig) - tmp
           FIELD(5,ig) = dsqrt(1.5*(PP(1)**2 + &
                                   PP(2)**2 + &
                                   PP(4)**2 + &
                                   (2.*(PP(3)**2)))) 

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF
       
      ENDIF
      
    case(5) ! Internal variables
      if (nb_internal > 0) then
        !la taille max est au pif    
        if (nb_internal > 10) call faterr(IAM,'number of internal variables too large') 
      
        IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
          !CALL LOGMES(IAM//' when smoothing internal variable only external models are supported ')
        ELSE
           do ig=1,NbGp_ele
              do ii=1,nb_internal
                Field(ii,ig) = INTERNAL(ii,ig) 
               enddo
           enddo
        ENDIF
      endif 
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1|3::strain, 2|4::stress, 5::internal variables')      
  endselect

  nbs=size(mecaEF(i)%gp2node,dim=1)
  nbm=size(mecaEF(i)%node2edge,dim=1)

  !print*,nbs,nbm

  do if=1,nbfields
     !print*,'-',if
     v_nodes(1:nbs) = matmul(mecaEF(i)%gp2node,Field(if,:))

     !print*,v_nodes(1:nbs)

     !print*, associated(mecaEF(i)%node2edge)

     if ( associated(mecaEF(i)%node2edge) ) then
       v_nodes(nbs+1:nbs+nbm) = matmul(mecaEF(i)%node2edge,v_nodes(1:nbs)) 
       !print*,v_nodes(nbs+1:nbs+nbm)
     endif
    
     NodalValues(if,:) = NodalValues(if,:) + v_nodes(:)

  enddo

  !print*,nbfields
  !do if=1,nbfields
  !  print*,'-',if
  !  print*,Field(if,:)
  !  print*,associated(mecaEF(i)%node2edge)
  !  if (associated(mecaEF(i)%node2edge)) print*,mecaEF(i)%node2edge
  !  print*,NodalValues(if,:)
  !enddo 

  deallocate(v_nodes,GRAD,FLUX,INTERNAL,GaussPointValues_ele,field)

END SUBROUTINE gpv2node_2D_iso

!------------------------------------------------------------------------

SUBROUTINE gpv2node_3D_iso(i,mdlnb,ibdyty,iblmty,required_Field,NodalValues,FieldSize,NbNodes_stored)

!fd routine qui ramene aux noeuds les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain,        2==cauchy stress, 
!                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress
!                                        5==internal variables  
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 
!  nbnodes_stored  : in some cases the field is computed 
!                    on less nodes than the element one (ie for quadratic elements) 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: NodalValues

  INTEGER :: NbNodes_ele,NbGp_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: NbNodes_stored,i_node

!                            12345678901234567890123456789
  CHARACTER(len=29)  :: IAM='a_mecaEF_iso::gpv2node_3D_iso'

  REAL(KIND=8) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:),Field(:,:)     ! vecteur de travail local
  integer :: ig,if,inull,nb_external,nb_internal,ii

  real(kind=8) :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

  integer :: nbs,nbm

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE
  Nbnodes_stored = NbNodes_ele 

  if (fieldsize /= size(NodalValues,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbNodes_ele /= size(NodalValues,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
  NodalValues = 0.d0

  if (NSTEP < 1) return

  allocate(v_nodes(NbNodes_ele))
  v_nodes = 0.d0

  allocate(Field(Fieldsize,NbGp_ele))
  Field = 0.d0

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx  (ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

    case(1) ! Euler Almansi Strain
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi') THEN          
        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! rien a faire sauf permutation
          do ig=1,NbGp_ele
            FIELD(1,ig) = GRAD(1,ig)
            FIELD(2,ig) = GRAD(4,ig)
            FIELD(3,ig) = GRAD(2,ig)
            FIELD(4,ig) = GRAD(6,ig)
            FIELD(5,ig) = GRAD(5,ig)
            FIELD(6,ig) = GRAD(3,ig)
            FIELD(7,ig) = GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN

          CALL FATERR(IAM,'Internal models with kine_==large not supported ')

        ENDIF   
      ELSE if (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN
        !fd en dur pour matlib. A transferer dans external ?
        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! rien a faire 
          do ig=1,NbGp_ele
            Field(1:6,ig) = GRAD(1:6,ig) 
            FIELD(7,ig)= GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele
            FF(:)=GRAD(:,ig)

            !fd calcul de F^-1 on stocke dans A33

            A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
            tmp=determinant33(A33)
            A33T=transpose(A33)             ! F

            call inverse33(A33T, inull)     !F^-1

            if (inull == 1) then
              print*,'Body ',ibdyty,' element ',iblmty,' gp ',ig
              print*,A33T(1,:)
              print*,A33T(2,:)
              print*,A33T(3,:)
              call faterr(IAM,'Non inversible F')
            endif


            A33=transpose(A33T)             !F^-T

            !fd almansi e = 0.5 (I - F^-T F^-1) = 0.5 (I - A33 A33T)
            !fd field e11 e12 e22 e13 e23 e33
            FIELD(1,ig)=0.5*(1.d0 - (A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)))
            FIELD(2,ig)=0.5*(     - (A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
            FIELD(3,ig)=0.5*(1.d0 - (A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)))
            FIELD(4,ig)=0.5*(     - (A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
            FIELD(5,ig)=0.5*(     - (A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
            FIELD(6,ig)=0.5*(1.D0 - (A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)))
            FIELD(7,ig)=tmp
          enddo
        ENDIF 

      ELSE
        CALL FATERR(IAM,'Unsupported ext option '//get_eleop_value(mdlnb,'isext'))
      ENDIF

    case(3) ! Green Lagrange Strain
      
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi' ) THEN

        CALL LOGMES(IAM//'Only external models are supported ')
        
      ELSE if (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN


        !fd en dur pour matlib. A transferer dans external ?
        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         ! rien a faire sauf permutation
          do ig=1,NbGp_ele
            Field(1:6,ig) = GRAD(1:6,ig) 
            FIELD(7,ig)= GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele
            FF(:)=GRAD(:,ig)

            !fd calcul de F^-1 on stocke dans A33

            A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
            tmp=determinant33(A33)
            A33T=transpose(A33)             ! F

            !da lagrange e = 0.5 (F^TF - I) = 0.5 (A33 A33T - I)
            !fd field e11 e12 e22 e13 e23 e33
            FIELD(1,ig)=0.5*((A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)) - 1.0)
            FIELD(2,ig)=0.5*((A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
            FIELD(3,ig)=0.5*((A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)) - 1.0)
            FIELD(4,ig)=0.5*((A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
            FIELD(5,ig)=0.5*((A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
            FIELD(6,ig)=0.5*((A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)) - 1.0)
            FIELD(7,ig)=tmp

          enddo
        ELSE
          CALL FATERR(IAM,'Unsupported kine option')
        ENDIF

      ENDIF
    
    case(2) ! Cauchy Stress
       
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi') THEN

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! cauchy     
          ! lmgc90: xx yy zz xy yz xz -> xx yx yy zx zy zz
          do ig=1,NbGp_ele
            FIELD(1,ig) = FLUX(1,ig)
            FIELD(2,ig) = FLUX(4,ig)
            FIELD(3,ig) = FLUX(2,ig)
            FIELD(4,ig) = FLUX(6,ig)
            FIELD(5,ig) = FLUX(5,ig)
            FIELD(6,ig) = FLUX(3,ig)
          enddo
         
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN

          CALL FATERR(IAM,'Internal or Demmefi models with kine_==large not supported ')

        ENDIF
      
      ELSE IF (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN
       !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          do ig=1,NbGp_ele
            FIELD(1:6,ig) = FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           !fd cauchy s = J^-1 P F^T

           PP(:)=FLUX(:,ig);FF(:)=GRAD(:,ig)
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           A33T=reshape(FF,(/3,3/))         ! A33T=F^T car stockage C de FF


           tmp=determinant33(A33T)
           if (tmp /= 0.d0) tmp=1.d0/tmp 
 
           !fd cauchy s = J^-1 P F^T = tmp A33 A33T
           !fd field s11 s12 s22 s13 s23 s33

           FIELD(1,ig)=tmp*(A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1))
           FIELD(2,ig)=tmp*(A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2))
           FIELD(3,ig)=tmp*(A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2))
           FIELD(4,ig)=tmp*(A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3))
           FIELD(5,ig)=tmp*(A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3))
           FIELD(6,ig)=tmp*(A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3))
       
          enddo
        
        ELSE
          CALL FATERR(IAM,'Unsupported kine option')
        ENDIF

      ELSE 
        CALL FATERR(IAM,'Unsupported ext option '//get_eleop_value(mdlnb,'isext'))
      ENDIF
    
      !fd calcul du von mises
      do ig=1,NbGp_ele
        tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
        PP(1) = FIELD(1,ig) - tmp
        PP(2) = FIELD(2,ig)
        PP(3) = FIELD(3,ig) - tmp
        PP(4) = FIELD(4,ig)
        PP(5) = FIELD(5,ig)
        PP(6) = FIELD(6,ig) - tmp

        FIELD(7,ig) = dsqrt(1.5d0*(PP(1)**2 + &
                                 PP(3)**2 + &
                                 PP(6)**2 + &
                                 (2.d0*(PP(2)**2 + &
                                      PP(4)**2 + &
                                      pp(5)**2 )))) 
      enddo
    
    
    case(4) ! Piola Kirchoff Stress
    
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi') THEN
         
        CALL LOGMES(IAM//'Only external models are supported ')


      ELSE IF (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN

        !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          do ig=1,NbGp_ele
            FIELD(1:6,ig) = FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

		   PP(:)=FLUX(:,ig);
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           
           !da field e11 e12 e22 e13 e23 e33  
           FIELD(1,ig)=A33(1,1)
           FIELD(2,ig)=A33(1,2)
           FIELD(3,ig)=A33(2,2)
           FIELD(4,ig)=A33(1,3)
           FIELD(5,ig)=A33(2,3)
           FIELD(6,ig)=A33(3,3)

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       do ig=1,NbGp_ele
         tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
         PP(1) = FIELD(1,ig) - tmp
         PP(2) = FIELD(2,ig)
         PP(3) = FIELD(3,ig) - tmp
         PP(4) = FIELD(4,ig)
         PP(5) = FIELD(5,ig)
         PP(6) = FIELD(6,ig) - tmp

         FIELD(7,ig) = dsqrt(1.5*(PP(1)**2 + &
                                  PP(3)**2 + &
                                  PP(6)**2 + &
                                  (2.*(PP(2)**2 + &
                                       PP(4)**2 + &
                                       pp(5)**2 )))) 
       enddo

      ELSE 
        CALL FATERR(IAM,'Unsupported ext option '//get_eleop_value(mdlnb,'isext'))       
      ENDIF
     
    case(5)
      if (nb_internal > 0) then
        !la taille max est en dur à 57 (endo3d)    
        if (nb_internal > 57) call faterr(IAM,'number of internal variables too large') 
        ! Internal variables 
        IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
          !CALL LOGMES(IAM//' when smoothing internal variable only external models are supported ')
        ELSE
           do ig=1,NbGp_ele
              do ii=1,nb_internal
                Field(ii,ig) = INTERNAL(ii,ig) 
               enddo
           enddo
        ENDIF
      endif
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1|3::strain, 2|4::stress, 5::internal variables')
  endselect

  nbs=size(mecaEF(i)%gp2node,dim=1)
  nbm=size(mecaEF(i)%node2edge,dim=1)
  do if=1,fieldsize
     v_nodes(1:nbs) = matmul(mecaEF(i)%gp2node,Field(if,:))


     if (associated(mecaEF(i)%node2edge)) then
       v_nodes(nbs+1:nbs+nbm) = matmul(mecaEF(i)%node2edge,v_nodes(1:nbs)) 
     endif
    

     NodalValues(if,:) = NodalValues(if,:) + v_nodes(:)

  enddo
  !print *, '  nv  : ', NodalValues(:,1:4)

  deallocate(v_nodes,GRAD,FLUX,INTERNAL,field)

END SUBROUTINE gpv2node_3D_iso

!------------------------------------------------------------------------
SUBROUTINE gpv2element_3D_iso(i,mdlnb,ibdyty,iblmty,required_Field,Field,FieldSize)

!fd routine qui calcule une moyenne sur l'element des valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain,        2==cauchy stress, 
!                                        3==Green Lagrange strain, 4==Piola Kirchoff Stress
!                                        5==internal variables  
!  nodalvalues     : the computed values
!  fieldsize       : number of components of the field 

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_Field
  REAL(kind=8),DIMENSION(:),INTENT(inout) :: Field

  INTEGER :: NbNodes_ele,NbGp_ele
  REAL(kind=8),DIMENSION(:),allocatable :: v_nodes ! allocation automatique

  INTEGER :: i_node

  !                          12345678901234567890123456789012
  CHARACTER(len=32)  :: IAM='a_mecaEF_iso::gpv2element_3D_iso'

  ! vecteurs de travail local
  REAL(KIND=8), ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:) 
  integer :: ig,if,inull,nb_external,nb_internal,ii

  real(kind=8) :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  NbNodes_ele = mecaEF(i)%N_NODE

  if (fieldsize /= size(Field)) then
    CALL FATERR(IAM,'Non conforming sizes')
  endif

  Field = 0.d0

  if (NSTEP < 1) return

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx  (ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

    case(1) ! Euler Almansi Strain
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi') THEN          
        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! rien a faire sauf permutation
          do ig=1,NbGp_ele
            FIELD(1) = FIELD(1) + GRAD(1,ig)
            FIELD(2) = FIELD(2) + GRAD(4,ig)
            FIELD(3) = FIELD(3) + GRAD(2,ig)
            FIELD(4) = FIELD(4) + GRAD(6,ig)
            FIELD(5) = FIELD(5) + GRAD(5,ig)
            FIELD(6) = FIELD(6) + GRAD(3,ig)
            FIELD(7) = FIELD(7) + GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
          enddo
          Field = Field/NbGp_ele         
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN

          CALL FATERR(IAM,'Internal models with kine_==large not supported ')

        ENDIF   
      ELSE if (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN
        !fd en dur pour matlib. A transferer dans external ?
        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! rien a faire 
          do ig=1,NbGp_ele
            Field(1:6) = Field(1:6) + GRAD(1:6,ig) 
            FIELD(7)   = FIELD(7) + GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele
            FF(:)=GRAD(:,ig)

            !fd calcul de F^-1 on stocke dans A33

            A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
            tmp=determinant33(A33)
            A33T=transpose(A33)             ! F

            call inverse33(A33T, inull)     !F^-1

            if (inull == 1) then
              print*,'Body ',ibdyty,' element ',iblmty,' gp ',ig
              print*,A33T(1,:)
              print*,A33T(2,:)
              print*,A33T(3,:)
              call faterr(IAM,'Non inversible F')
            endif


            A33=transpose(A33T)             !F^-T

            !fd almansi e = 0.5 (I - F^-T F^-1) = 0.5 (I - A33 A33T)
            !fd field e11 e12 e22 e13 e23 e33
            FIELD(1)=FIELD(1) + 0.5*(1.d0 - (A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)))
            FIELD(2)=FIELD(2) + 0.5*(     - (A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
            FIELD(3)=FIELD(3) + 0.5*(1.d0 - (A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)))
            FIELD(4)=FIELD(4) + 0.5*(     - (A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
            FIELD(5)=FIELD(5) + 0.5*(     - (A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
            FIELD(6)=FIELD(6) + 0.5*(1.D0 - (A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)))
            FIELD(7)=FIELD(7) + tmp
          enddo
          Field = Field/NbGp_ele
        ENDIF

      ELSE
        CALL FATERR(IAM,'Unsupported ext option '//get_eleop_value(mdlnb,'isext'))
      ENDIF

    case(3) ! Green Lagrange Strain
      
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi' ) THEN

        CALL LOGMES(IAM//'Only external models are supported ')
        
      ELSE if (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN

        !fd en dur pour matlib. A transferer dans external ?
        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         ! rien a faire sauf permutation
          do ig=1,NbGp_ele
            Field(1:6) = Field(1:6) + GRAD(1:6,ig) 
            FIELD(7)   = Field(7)   + GRAD(1,ig) + GRAD(2,ig) +GRAD(3,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele
            FF(:)=GRAD(:,ig)

            !fd calcul de F^-1 on stocke dans A33

            A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
            tmp=determinant33(A33)
            A33T=transpose(A33)             ! F

            !da lagrange e = 0.5 (F^TF - I) = 0.5 (A33 A33T - I)
            !fd field e11 e12 e22 e13 e23 e33
            FIELD(1)=FIELD(1) + 0.5*((A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)) - 1.0)
            FIELD(2)=FIELD(2) + 0.5*((A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
            FIELD(3)=FIELD(3) + 0.5*((A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)) - 1.0)
            FIELD(4)=FIELD(4) + 0.5*((A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
            FIELD(5)=FIELD(5) + 0.5*((A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
            FIELD(6)=FIELD(6) + 0.5*((A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)) - 1.0)
            FIELD(7)=FIELD(7) + tmp
          enddo
          Field = Field/NbGp_ele        
        ELSE
          CALL FATERR(IAM,'Unsupported kine option')
        ENDIF

      ENDIF
    
    case(2) ! Cauchy Stress
       
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi') THEN

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! cauchy     
          ! lmgc90: xx yy zz xy yz xz -> xx yx yy zx zy zz
          do ig=1,NbGp_ele
            FIELD(1) = FIELD(1) + FLUX(1,ig)
            FIELD(2) = FIELD(2) + FLUX(4,ig)
            FIELD(3) = FIELD(3) + FLUX(2,ig)
            FIELD(4) = FIELD(4) + FLUX(6,ig)
            FIELD(5) = FIELD(5) + FLUX(5,ig)
            FIELD(6) = FIELD(6) + FLUX(3,ig)
          enddo
          Field = Field/NbGp_ele
          
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN

          CALL FATERR(IAM,'Internal or Demmefi models with kine_==large not supported ')

        ENDIF
      
      ELSE IF (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN
       !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          do ig=1,NbGp_ele
            FIELD(1:6) = FIELD(1:6)+FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           !fd cauchy s = J^-1 P F^T

           PP(:)=FLUX(:,ig);FF(:)=GRAD(:,ig)
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           A33T=reshape(FF,(/3,3/))         ! A33T=F^T car stockage C de FF


           tmp=determinant33(A33T)
           if (tmp /= 0.d0) tmp=1.d0/tmp 
 
           !fd cauchy s = J^-1 P F^T = tmp A33 A33T
           !fd field s11 s12 s22 s13 s23 s33

           FIELD(1)=FIELD(1) + tmp*(A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1))
           FIELD(2)=FIELD(2) + tmp*(A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2))
           FIELD(3)=FIELD(3) + tmp*(A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2))
           FIELD(4)=FIELD(4) + tmp*(A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3))
           FIELD(5)=FIELD(5) + tmp*(A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3))
           FIELD(6)=FIELD(6) + tmp*(A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3))
       
          enddo

           Field = Field/NbGp_ele
          
        ELSE
          CALL FATERR(IAM,'Unsupported kine option')
        ENDIF

      ELSE 
        CALL FATERR(IAM,'Unsupported ext option '//get_eleop_value(mdlnb,'isext'))
      ENDIF
    
      !fd calcul du von mises
      tmp= (FIELD(1)+FIELD(3)+FIELD(6))/3.d0
      PP(1) = FIELD(1) - tmp
      PP(2) = FIELD(2)
      PP(3) = FIELD(3) - tmp
      PP(4) = FIELD(4)
      PP(5) = FIELD(5)
      PP(6) = FIELD(6) - tmp

      FIELD(7) = dsqrt(1.5d0*(PP(1)**2 + &
                              PP(3)**2 + &
                              PP(6)**2 + &
                              (2.d0*(PP(2)**2 + &
                                     PP(4)**2 + &
                                     pp(5)**2 )))) 
    
    case(4) ! Piola Kirchoff Stress
    
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi') THEN
         
        CALL LOGMES(IAM//'Only external models are supported ')


      ELSE IF (get_eleop_value(mdlnb,'isext') == 'MatL_') THEN

        !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          do ig=1,NbGp_ele
            FIELD(1:6) = FIELD(1:6) + FLUX(1:6,ig)
          enddo
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

            PP(:)=FLUX(:,ig);
            A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
            A33=transpose(A33T)              ! P
           
            !da field e11 e12 e22 e13 e23 e33  
            FIELD(1)=FIELD(1)+A33(1,1)
            FIELD(2)=FIELD(2)+A33(1,2)
            FIELD(3)=FIELD(3)+A33(2,2)
            FIELD(4)=FIELD(4)+A33(1,3)
            FIELD(5)=FIELD(5)+A33(2,3)
            FIELD(6)=FIELD(6)+A33(3,3)

         enddo
         Field = Field/NbGp_ele
         
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       tmp= (FIELD(1)+FIELD(3)+FIELD(6))/3.d0
       PP(1) = FIELD(1) - tmp
       PP(2) = FIELD(2)
       PP(3) = FIELD(3) - tmp
       PP(4) = FIELD(4)
       PP(5) = FIELD(5)
       PP(6) = FIELD(6) - tmp

       FIELD(7) = dsqrt(1.5*(PP(1)**2 + &
                             PP(3)**2 + &
                             PP(6)**2 + &
                             (2.*(PP(2)**2 + &
                                  PP(4)**2 + &
                                  pp(5)**2 )))) 

      ELSE 
        CALL FATERR(IAM,'Unsupported ext option '//get_eleop_value(mdlnb,'isext'))       
      ENDIF
     
    case(5)
      if (nb_internal > 0) then
        !la taille max est en dur à 57 (endo3d)    
        if (nb_internal > 57) call faterr(IAM,'number of internal variables too large') 
        ! Internal variables 
        IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
          !CALL LOGMES(IAM//' when smoothing internal variable only external models are supported ')
        ELSE
           do ig=1,NbGp_ele
              do ii=1,nb_internal
                 Field(ii) = INTERNAL(ii,ig)
              enddo
           enddo
           Field = Field/NbGp_ele           
        ENDIF
      endif
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1|3::strain, 2|4::stress, 5::internal variables')
  endselect

  !print *, '  nv  : ', NodalValues(:,1:4)

  deallocate(GRAD,FLUX,INTERNAL)

END SUBROUTINE gpv2element_3D_iso

!------------------------------------------------------------------------

SUBROUTINE gpv_iso(i,ppsnb,ibdyty,iblmty,required_Field,Field,FieldSize)

!fd routine qui recupere les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  required_field  : the expected field. 1==almansy strain, 2==cauchy stress 
!  field           : the computed values
!  fieldsize       : number of components of the field 

! stockage 11, 12, 22, 13, 23, 33  

!  stress(1,1) = field(1,ig) ; stress (1,2) = field(2,ig) ; stress (1,3) = field(4,ig)     
!  stress(2,1) = field(2,ig) ; stress (2,2) = field(3,ig) ; stress (2,3) = field(5,ig)     
!  stress(3,1) = field(4,ig) ; stress (3,2) = field(5,ig) ; stress (3,3) = field(6,ig)     

  
  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty,fieldsize,required_field
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: field
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: NbGp_ele

!                            123456789012345678901
  CHARACTER(len=21)  :: IAM='a_mecaEF_iso::gpv_iso'

  ! vecteurs de travail locaux
  REAL(KIND=LONG) ,ALLOCATABLE :: GRAD(:,:),FLUX(:,:),INTERNAL(:,:)     
  integer :: ig,inull,nb_external,nb_internal

  real(kind=8) :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  if (fieldsize /= size(Field,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbGp_ele /= size(Field,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif

  Field = 0.d0

  if (NSTEP < 1) return

  nb_external=get_nb_external_variables(mdlnb)
  nb_internal=get_nb_internal_variables(mdlnb)

  ALLOCATE(GRAD(nb_external,NbGp_ele),FLUX(nb_external,NbGp_ele))
  ALLOCATE(INTERNAL(nb_internal,NbGp_ele))

  do ig=1,NbGp_ele
    CALL get_stress_1_MAILx(ibdyty,iblmty,ig,FLUX(:,ig))
    CALL get_strain_1_MAILx(ibdyty,iblmty,ig,GRAD(:,ig))
    IF (nb_internal /= 0 ) CALL get_internal_1_MAILx(ibdyty,iblmty,ig,INTERNAL(:,ig))
  enddo

  Select case(required_field)

    case(1) ! strain
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi' ) THEN

       ! lmgc90 stocke 2D : 11, 22, 12, 33
       !               3D : 11, 22, 33, 12, 23, 31 

       ! field 11 12 22 13 23 33
        
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         if (nbdime==2) then
            do ig=1,NbGp_ele
              Field(1,ig) = GRAD(1,ig)
              Field(2,ig) = GRAD(3,ig)
              Field(3,ig) = GRAD(2,ig)
              Field(6,ig) = GRAD(4,ig)
            enddo
         else
           do ig=1,NbGp_ele
             Field(1,ig) = GRAD(1,ig)
             Field(2,ig) = GRAD(4,ig)
             Field(3,ig) = GRAD(2,ig)
             Field(4,ig) = GRAD(6,ig)
             Field(5,ig) = GRAD(5,ig)
             Field(6,ig) = GRAD(3,ig)             
           enddo
         endif 
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN

         CALL LOGMES(IAM//'Only external models are supported ')
          
       ENDIF
      ELSE
       !fd en dur pour matlib. A transferer dans external ?
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         if (nbdime==2) then
            do ig=1,NbGp_ele
              Field(1:3,ig) = GRAD(1:3,ig)
              Field(6,ig) = GRAD(4,ig)
            enddo
         else
           do ig=1,NbGp_ele
             Field(:,ig) = GRAD(:,ig) 
           enddo
         endif 
       ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
         do ig=1,NbGp_ele
           if (nbdime == 2) then
             FF(1:2)=GRAD(1:2,ig)
             FF(4:5)=GRAD(3:4,ig)
             FF(9) = 1.d0
           else     
             FF(:)=GRAD(:,ig)
           endif
          
           !fd calcul de F^-1 on stocke dans A33

           A33=reshape(FF,(/3,3/))         ! A33=F^T car stockage C de FF
           A33T=transpose(A33)             ! F

           call inverse33(A33T, inull)     !F^-1

           if (inull == 1) then
             print*,'Body ',ibdyty,' element ',iblmty,' gp ',ig
             print*,A33T(1,:)
             print*,A33T(2,:)
             print*,A33T(3,:)
             call faterr(IAM,'Non inversible F')
           endif


           A33=transpose(A33T)             !F^-T

           !fd almansi e = 0.5 (I - F^-T F^-1) = 0.5 (I - A33 A33T)
           !fd field e11 e12 e22 e13 e23 e33
           FIELD(1,ig)=0.5*(1.d0 - (A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)))
           FIELD(2,ig)=0.5*(     - (A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
           FIELD(3,ig)=0.5*(1.d0 - (A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)))
           FIELD(4,ig)=0.5*(     - (A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
           FIELD(5,ig)=0.5*(     - (A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
           FIELD(6,ig)=0.5*(1.D0 - (A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)))
         enddo
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF
       

      ENDIF

    case(2) ! stress
      IF (get_eleop_value(mdlnb,'isext') == 'no___' .or. &
          get_eleop_value(mdlnb,'isext') == 'Demfi' ) THEN

       ! lmgc90 stocke 2D : 11, 22, 12, 33
       !               3D : 11, 22, 33, 12, 23, 31 

       ! field 11 12 22 13 23 33
         
       IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
         if (nbdime==2) then
            do ig=1,NbGp_ele
              Field(1,ig) = FLUX(1,ig)
              Field(2,ig) = FLUX(3,ig)
              Field(3,ig) = FLUX(2,ig)
              Field(6,ig) = FLUX(4,ig)
            enddo
         else
           do ig=1,NbGp_ele
             Field(1,ig) = FLUX(1,ig)
             Field(2,ig) = FLUX(4,ig)
             Field(3,ig) = FLUX(2,ig)
             Field(4,ig) = FLUX(6,ig)
             Field(5,ig) = FLUX(5,ig)
             Field(6,ig) = FLUX(3,ig)             
           enddo
         endif 

        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          CALL LOGMES(IAM//'Only external models are supported ')
       ENDIF

       
      ELSE
        !fd en dur pour matlib. A transferer dans external

        IF (get_eleop_value(mdlnb,'kine_') == 'small') THEN
          ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
          if (nbdime == 2) then
             do ig=1,NbGp_ele
               FIELD(1:3,ig) = FLUX(1:3,ig)
               FIELD(6,ig) = FLUX(4,ig)
            enddo
        
          else  
             do ig=1,NbGp_ele
               FIELD(1:6,ig) = FLUX(1:6,ig)
            enddo
          endif  
        ELSE IF (get_eleop_value(mdlnb,'kine_') == 'large') THEN
          do ig=1,NbGp_ele

           !fd cauchy s = J^-1 P F^T
           if (nbdime == 2) then
              PP(1:2)=FLUX(1:2,ig)
              PP(4:5)=FLUX(3:4,ig)

              FF(1:2)=GRAD(1:2,ig)
              FF(4:5)=GRAD(3:4,ig)
              FF(9)=1.d0
           else      
              PP(:)=FLUX(:,ig);FF(:)=GRAD(:,ig)
           endif   
           A33T=reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
           A33=transpose(A33T)              ! P
           A33T=reshape(FF,(/3,3/))         ! A33T=F^T car stockage C de FF


           tmp=determinant33(A33T)
           if (tmp /= 0.d0) tmp=1.d0/tmp 
 
           !fd cauchy s = J^-1 P F^T = tmp A33 A33T
           !fd field s11 s12 s22 s13 s23 s33

           FIELD(1,ig)=tmp*(A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1))
           FIELD(2,ig)=tmp*(A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2))
           FIELD(3,ig)=tmp*(A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2))
           FIELD(4,ig)=tmp*(A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3))
           FIELD(5,ig)=tmp*(A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3))
           FIELD(6,ig)=tmp*(A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3))

         enddo
        
       ELSE
         CALL FATERR(IAM,'Unsupported kine option')
       ENDIF

       !fd calcul du von mises
       do ig=1,NbGp_ele
         tmp= (FIELD(1,ig)+FIELD(3,ig)+FIELD(6,ig))/3.d0
         PP(1) = FIELD(1,ig) - tmp
         PP(2) = FIELD(2,ig)
         PP(3) = FIELD(3,ig) - tmp
         PP(4) = FIELD(4,ig)
         PP(5) = FIELD(5,ig)
         PP(6) = FIELD(6,ig) - tmp

         FIELD(7,ig) = dsqrt(1.5*(PP(1)**2 + &
                                  PP(3)**2 + &
                                  PP(6)**2 + &
                                  (2.*(PP(2)**2 + &
                                       PP(4)**2 + &
                                       pp(5)**2 )))) 
       enddo

       
      ENDIF
    case default
      CALL FATERR(IAM,'Unsupported required_field : 1::strain, 2::stress')
  endselect


  deallocate(GRAD,FLUX,INTERNAL)

END SUBROUTINE gpv_iso

!------------------------------------------------------------------------

SUBROUTINE gpv_all_internal_iso(i,ppsnb,ibdyty,iblmty,Field,FieldSize)

!fd routine qui recupere les valeurs des internals aux points de gauss
!  i               : iso element id
!  ppsnb           : property set
!  field           : the computed values
!  fieldsize       : number of components of the field 

  IMPLICIT NONE
  INTEGER :: i,ibdyty,iblmty,fieldsize
  REAL(kind=8),DIMENSION(:,:),pointer :: field
  integer,dimension(:),intent(in) :: ppsnb

  !*** variables locales
  !                          123456789012345678901234567890
  CHARACTER(len=30)  :: IAM='a_mecaEF_iso::gpv_internal_iso'
  INTEGER :: NbGp_ele
  INTEGER :: mdlnb,inull,ig

  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  if (associated(field)) then
    deallocate(field)
    field => null()
  endif

  if (NSTEP < 1) return

  fieldsize=get_nb_internal_variables(mdlnb)  

  if (fieldsize == 0) return

  ALLOCATE(field(fieldsize,NbGp_ele))

  do ig=1,NbGp_ele

       CALL get_internal_1_MAILx(ibdyty,iblmty,ig,field(:,ig))

  enddo

END SUBROUTINE gpv_all_internal_iso

!------------------------------------------------------------------------

SUBROUTINE element_internal_iso(i,ppsnb,ibdyty,iblmty,coor_ele,nb,val)

!fd routine qui calcul l'intgral d un internal sur l'element
!  i               : iso element id
!  ppsnb           : property set
!  nb              : rank du internal
!  coor_ele        : coordinates  

  IMPLICIT NONE
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: i,ibdyty,iblmty,nb  
  ! dep des sommets
  REAL(KIND=8)              :: coor_ele(:,:),val

  !*** variables locales
  INTEGER :: mdlnb,inull,fieldsize,ig
  REAL(kind=8),DIMENSION(:,:),pointer :: field
  REAL(KIND=8) , POINTER    :: DNX(:,:)
  REAL(KIND=8)              :: COEFINT,R  
  INTEGER :: NbGp_ele

  !                          123456789012345678901234567890
  CHARACTER(len=30)  :: IAM='a_mecaEF_iso::gpv_internal_iso'

  val = 0.d0

  if (NSTEP < 1) return

  NULLIFY(DNX)
  
  if (associated(field)) then
    deallocate(field)
    field => null()
  endif
  CALL get_ppset_value(ppsnb(1),mdlnb,inull) 
  fieldsize=get_nb_internal_variables(mdlnb)
  NbGp_ele =  mecaEF(i)%N_PG_RIG
  
  if (fieldsize == 0) return
  ALLOCATE(field(fieldsize,NbGp_ele))
  
  do ig=1,NbGp_ele

    CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                      mecaEF(i)%PG(ig)%POIDS,coor_ele,DNX,COEFINT,R)

    CALL get_internal_1_MAILx(ibdyty,iblmty,ig,field(:,ig))
    
    val = val + COEFINT*field(nb,ig)
   
  enddo

  DEALLOCATE(DNX,field) ; NULLIFY(DNX) ; nullify(field)    

END SUBROUTINE element_internal_iso

!------------------------------------------------------------------------

SUBROUTINE gp_external_field_3D_iso(i,ppsnb,ibdyty,iblmty,Field)

! todo gestion de l affichage des internals

!fd routine qui recupere les valeurs aux points de gauss
!  i               : iso element id
!  mdlnb           : model
!  field           : the computed values

  IMPLICIT NONE
  INTEGER :: i,mdlnb,ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:),INTENT(inout) :: field
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER :: NbGp_ele,nbf,if,ig,rank,inull,nbi,iif
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: INTERNAL
  character(len=30) :: name

!                            12345678901234567890123456789012345678
  CHARACTER(len=39)  :: IAM='a_mecaEF_iso::gp_external_field_3D_iso'


  NbGp_ele =  mecaEF(i)%N_PG_RIG  
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nbf = get_external_field_nb(mdlnb)

!!$  nbi = get_nb_internal_variables(mdlnb)
!!$
!!$  if (nbf+nbi /= size(Field,dim=1)) then

  if (nbf /= size(Field,dim=1)) then
    CALL FATERR(IAM,'Non conforming sizes first dim')
  endif

  if (NbGp_ele /= size(Field,dim=2)) then
    CALL FATERR(IAM,'Non conforming sizes second dim')
  endif
         
  do if = 1 , nbf 
    name=get_external_field_name(mdlnb,if)
    rank=get_meca_field_rank_MAILx(ibdyty,iblmty,name)
    do ig=1,NbGp_ele
      CALL get_meca_field_MAILx(ibdyty,iblmty,ig,rank,field(if,ig))
    enddo
  enddo

!!$  if ( nbi == 0) return  
!!$
!!$  ALLOCATE(INTERNAL(nbi))
!!$  
!!$  CALL get_internal_MAILx(ibdyty,iblmty,ig,INTERNAL)
!!$  
!!$  do if = 1 , nbi
!!$    iif = if + nbf
!!$    do ig=1,NbGp_ele
!!$      field(if + nbf,ig) = INTERNAL(if)
!!$    enddo
!!$  enddo
!!$  
!!$  deallocate(internal)

end subroutine

!------------------------------------------------------------------------

SUBROUTINE Stress2Fint_ISO(i,ppsnb,ibdyty,iblmty,X,FINT)

  IMPLICIT NONE

  ! le numero de l'element
  INTEGER         , INTENT(IN) :: i,ppsnb(:),ibdyty,iblmty 
  ! coordonnees des sommets
  REAL(KIND=LONG)              :: X(:,:)                
  ! Vecteur des forces internes ele 
  REAL(KIND=LONG)              :: FINT(:)               

  ! variables locales
  REAL(KIND=LONG) , POINTER    :: DNX(:,:), Bl(:,:)     
  REAL(KIND=LONG)              :: COEFINT,R
  INTEGER                      :: IG

  REAL(KIND=LONG) ,ALLOCATABLE :: FLUX(:) ! vecteur de travail local

  INTEGER                      :: mdlnb,inull,nb_external,istrg

  !                            123456789012345678901234567
  CHARACTER(len=27)  :: IAM='a_mecaEF_iso::stress2fint_iso'

  ! Initialisation a vide des pointeurs
  NULLIFY(Bl,DNX)

  ! Allocation internal purpose arrays
  CALL get_ppset_value(ppsnb(1),mdlnb,inull)

  nb_external=get_nb_external_variables(mdlnb)

  ALLOCATE(FLUX(nb_external))

  FINT=ZERO

  DO IG=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss

   CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                     mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)


   IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN

     istrg = 1
     CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl) ! formation de Bl 

   ELSE if (get_eleop_value_bypps(ppsnb(ig),'isext') == 'MatL_') THEN

     istrg = 2
     select case(get_eleop_value_bypps(ppsnb(ig),'kine_'))
     case('small')

       CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl       

     case('large')

       CALL B_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,Bl)

     end select
 
   ELSE if (get_eleop_value_bypps(ppsnb(ig),'isext') == 'Demfi') THEN

     istrg = 1
     select case(get_eleop_value_bypps(ppsnb(ig),'kine_'))
     case('small')

       CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl       

     case('large')

       CALL B_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,Bl)

     end select


   else

     call faterr(IAM,get_eleop_value_bypps(ppsnb(ig),'isext')//' not yet implemented')
      
   ENDIF
   
   CALL get_stress_1_MAILx(ibdyty,iblmty,ig,Flux)

   FINT=FINT+(MATMUL(TRANSPOSE(Bl),Flux(1:SIZE(Bl,dim=1)))*COEFINT)

 ENDDO

 DEALLOCATE(Bl,DNX) ; NULLIFY(Bl,DNX)
 DEALLOCATE(FLUX)

END SUBROUTINE Stress2Fint_ISO

!*************************************************************************
!> routine calculant le repere d'orthotropie aux 
!> pg en appelant une routine user

SUBROUTINE compute_elementary_ortho_frame_iso(i,ppsnb,ibdyty,iblmty,coor)
  implicit none
  integer(kind=4),intent(in) :: ibdyty    ! body index
  integer(kind=4),intent(in) :: iblmty    ! element index
  INTEGER        ,INTENT(IN) :: i             ! le numero de l'element
  integer        ,intent(in) :: ppsnb(:)      ! le pps du gp
  real(kind=8)   ,intent(in) :: coor(:,:)  ! coordonnees des points sommets de l'element

  !*** variables locales a la routine
  !                                123456789012345678901234567890123456789012345678
  CHARACTER(len=48)        :: IAM='a_mecaEF_iso::compute_elementary_ortho_frame_iso'
  CHARACTER(len=5)         :: anisotropy
  INTEGER                  :: ig,mdlnb,inull,i_f,extV_nb
  real(kind=8),allocatable :: frame(:,:,:), coor_pg(:,:)
  real(kind=8)             :: mat33(3,3), center(3), orient(3)
  character(len=30)        :: name
  logical :: with_orient,with_center

  with_center = .false.
  with_orient = .false.

  ! on exploite la regle comme quoi tous les pg on le meme couple mdl/bulk

  CALL get_ppset_value(ppsnb(1),mdlnb,inull)
  anisotropy=get_eleop_value(mdlnb,'aniso')

  if (anisotropy /= 'ortho') return

  allocate(coor_pg(nbdime,mecaEF(i)%N_PG_RIG))
  do ig = 1, mecaEF(i)%N_PG_RIG
    do i_f = 1, nbDIME
      coor_pg(i_f,ig) = dot_product(mecaEF(i)%PG(ig)%N(:),coor(i_f,:))
    end do
  end do

  select case(DIME_mod)
  CASE(i_3D)
    allocate(frame(3,3,mecaEF(i)%N_PG_RIG))

    extV_nb = get_external_nb_vfield(mdlnb)
    if (extV_nb /= 0) then
      do i_f = 1, extV_nb
        name = get_external_vfield_name(mdlnb,i_f)
        if (trim(name) == 'MARROW_POINT') then
          call get_meca_vfield_MAILx(ibdyty,iblmty,1,i_f,center,3)
          with_center = .true.
        else if (trim(name) == 'MARROW_ORIENT') then
          call get_meca_vfield_MAILx(ibdyty,iblmty,1,i_f,orient,3)
          with_orient = .true.
        end if
      end do
    end if

    if( with_center .and. with_orient ) then
      call gp_ortho_frame(3,mecaEF(i)%N_PG_RIG,coor_pg,frame,center=center,vector=orient)
    else if( with_center ) then
      call gp_ortho_frame(3,mecaEF(i)%N_PG_RIG,coor_pg,frame,center=center)
    else if( with_orient ) then
      call gp_ortho_frame(3,mecaEF(i)%N_PG_RIG,coor_pg,frame,vector=orient)
    else
      call gp_ortho_frame(3,mecaEF(i)%N_PG_RIG,coor_pg,frame)
    end if

    DO ig=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss
      mat33(:,:) = frame(:,:,ig)
      IF (get_eleop_value(mdlnb,'isext') == 'no___') THEN
        call logmes('Error '//IAM//': Unsupported eleop for internal models')
      else
        call set_ortho_frame(ppsnb(ig),mat33)
      endif
    ENDDO
    deallocate(frame)
  CASE DEFAULT
     CALL FATERR(IAM,'Unsupported dimension')
  END SELECT

  deallocate(coor_pg)

END SUBROUTINE 
!*************************************************************************

!*************************************************************************
!> routines calculant les fields aux 
!> pg en appelant une routine user

SUBROUTINE compute_elementary_field_iso(i,ppsnb,time,dt)
  implicit none

  INTEGER        ,INTENT(IN) :: i             ! le numero de l'element
  integer        ,intent(in) :: ppsnb(:)      ! le pps du gp
  real(kind=8)               :: time,dt

  !*** variables locales a la routine
  !                                123456789012345678901234567890123456789012
  CHARACTER(len=42)        :: IAM='a_mecaEF_iso::compute_elementary_field_iso'

  call gp_field('incre',time,dt,nbdime,mecaEF(i)%N_PG_RIG)

END SUBROUTINE 

SUBROUTINE get_elementary_field_iso(i,ppsnb,name,time,dt,coor_node,field_pg)
  implicit none

  INTEGER        ,INTENT(IN) :: i              ! le numero de l'element
  integer        ,intent(in) :: ppsnb(:)       ! le pps du gp
  CHARACTER(len=30)          :: name           ! nom du field
  REAL(KIND=8),intent(in) :: coor_node(:,:) ! coordonnees des points de Gauss
  REAL(KIND=8),intent(in) :: field_pg(:)    ! field aux points de Gauss
  real(kind=8)               :: time,dt

  !*** variables locales a la routine
  !                                12345678901234567890123456789012345678
  CHARACTER(len=38)        :: IAM='a_mecaEF_iso::get_elementary_field_iso'

  integer :: idime,ig

  REAL(KIND=8),allocatable :: coor_pg(:,:) ! coordonnees des points de Gauss

  allocate(coor_pg(nbdime,mecaEF(i)%N_PG_RIG))

  DO ig=1,mecaEF(i)%N_PG_RIG                 ! Pour tous les points de Gauss
    do idime=1,nbDIME
      coor_pg(idime,ig)=dot_product(mecaEF(i)%PG(ig)%N(:),coor_node(idime,:))
    enddo
  ENDDO


  call gp_field('setgp',time,dt,nbdime,mecaEF(i)%N_PG_RIG, &
                field_name=name,gp_coor=coor_pg,field_value=field_pg)

  deallocate(coor_pg)

END SUBROUTINE 

!*************************************************************************

subroutine compute_gp2node(type_interpolation,nbn,type_integration,nbgp,mat)
  implicit none
  integer(kind=4) :: type_interpolation,type_integration
  integer      :: nbn,nbgp
  real(kind=8) :: mat(nbn,nbgp)
  ! ***
  integer      :: i,j,ig
  real(kind=8) :: tmp
  type(T_mat_sym) :: M,M_fact

  ! *** propre a la resolution
  character(len=1) :: isprecon
  real(kind=8) :: scale(nbn)

  ! position points de gauss (dim,nbgp)
  REAL(kind=long),POINTER :: CG(:,:)
  ! poids points de gauss (nbgp) <- pas utilise ici
  REAL(kind=long),POINTER :: poids(:) 
  ! valeurs fonctions de formes aux points de gauss (
  REAL(kind=long),POINTER :: N(:)
  
  nullify(CG,poids)
  CALL pos_gauss(type_integration,CG,POIDS)

  if (size(cg,dim=2) /= nbgp) then
    call FATERR('a_mecaEF_iso::compute_gp2node','nbgp inconsistancy')
  endif

  call new_matrix(M,nbn) 
  call zero_matrix(M) 

  call new_matrix(M_fact,nbn) 
  call zero_matrix(M_fact) 

  do i=1,nbn
    do j=1,nbn
      do ig=1,nbgp 
        NULLIFY(N)
        CALL fonct_forme(type_interpolation,CG(:,ig),N)
        tmp = N(i)*N(j)
        call add_to_matrix(M,tmp,i,j)
        deallocate(N)
      enddo
    end do
  end do

  
  do ig=1,nbgp 
    NULLIFY(N)
    CALL fonct_forme(type_interpolation,CG(:,ig),N)

    if (ig==1) then
      call x_solve_linear_system(M,N,M_fact,scale,isprecon,'E')
    else
      call x_solve_linear_system(M,N,M_fact,scale,isprecon,'F')
    endif

    mat(:,ig)=N(:)

    deallocate(N)
  end do

  call free_matrix(M)
  call free_matrix(M_fact)

  deallocate(cg,poids)

end subroutine

subroutine get_id_mecaef_iso(name,id)
  implicit none
  CHARACTER(len=5) :: name
  integer      :: id
                           !1234567890123456789012345678901
  character(len=31) :: IAM='a_mecaEF_iso::get_id_mecaef_iso'

  do id=1,size(mecaEF)
    if (name == get_NAME_mecaEF_iso(mecaEF(id)%name) ) exit
  enddo
  
  if (id > size(mecaEF)) then
    call FATERR(IAM,'Unknown element:'//name)
  endif

end subroutine

!------------------------------------------------------------------------------!

!> \brief Get the number of dof in an element
function get_N_DOF_mecaEF_iso(id)
  implicit none
  integer(kind=4), intent(in) :: id       !< [in] id of the mechanical iso element
  integer(kind=4) :: get_N_DOF_mecaEF_iso !< [return] number of dof in the element

  get_N_DOF_mecaEF_iso =  mecaEF(id)%N_NODE * mecaEF(id)%N_DOF_by_NODE
end function

!------------------------------------------------------------------------------!

!> \brief Get the number of dof of a node of an element
function get_N_DOF_of_NODE_mecaEF_iso(id, i_node)
  implicit none
  integer(kind=4), intent(in) :: id               !< [in] id of the mechanical iso element
  integer(kind=4), intent(in) :: i_node           !< [in] index of the node
  integer(kind=4) :: get_N_DOF_of_NODE_mecaEF_iso !< [return] number of dof in the element

  get_N_DOF_of_NODE_mecaEF_iso = mecaEF(id)%N_DOF_by_NODE
end function


!------------------------------------------------------------------------------!

!> get a pointer on a gauss_pt object stored at gp ig of ele type id
function get_gp_ptr_mecaEF_iso(id,ig)
  implicit none 
  integer :: id,ig
  type (T_pt_gauss), pointer :: get_gp_ptr_mecaEF_iso

  get_gp_ptr_mecaEF_iso => mecaEF(id)%PG(ig)

end function

!------------------------------------------------------------------------------!
!> fonction qui calcule le vecteur elementaire correspondant a la divergence d un field diago
!> pour un element isoparametrique
subroutine compute_elementary_field_divergence_ISO(i, X, vfield, Fext)

   implicit none

   ! variables d'entree
   !> element id 
   integer, intent(in)      :: i         
   !> nodal coordinates 
   real(kind=8), intent(in) :: X(:, :), vfield(:,:)  
 
   ! variables de sortie
   !> elementary external flux
   real(kind=8), intent(out) :: Fext(:)   
 
   ! variables locales
   integer                  :: iNod,iG,idim     
   ! divergence
   real(kind=8)             :: div
 
   ! variables locales servant a appeler la routine gradient_ISO
   real(kind=8)             :: R         ! sert pour le cas axisymetrique
   real(kind=8)             :: COEFINT   ! pour recuperer le produit du poids associe
                                         ! au point de Gauss courant (pour l'integration)
                                         ! et de la valeur du jacobien au point de Gauss 
                                         ! courant (changement de variable pour integrer
                                         ! sur l'element de reference)
   real(kind=8), pointer    :: DNX(:, :) ! pour recuperer la matrice des gradients au 
                                         !point de Gauss courant
 
   ! on initialise a 0 le vecteur elementaire
   Fext = 0.d0
  
   ! Pour tous les points de Gauss
   do ig=1,mecaEF(i)%N_PG_RIG     


     ! on annulle le pointeur DNX pour recuperer le matrice des gradients
     nullify(DNX)

     ! on recupere :
     !   * la matrice des gradients: DNX
     !   * la valeur du produit du poids associe au point de Gauss courant
     !     et du jacobien au point de gauss courant: w(iG)*det(J)(x_iG)
     call GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
                       mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

     ! on calcule la divergence de field au point de Gauss courant
     ! div(field)(x_iG) = somme, pour j allant de 1 a nbDIME de : dfield_j/dx_j(x_iG)
     ! ou dfield_j/dx_j(x_iG) = somme, pour iNod noeud de l'element de : field^iNod_j*dN_iNod/dx_j(x_iG)

      do idim=1,nbdime

        div = 0.d0
        ! pour chaque noeud de l'element
        do iNod=1, mecaEF(i)%N_NODE
         
         ! on ajoute la contribution du noeud courant au calcul de la divergence
          div = div + (vField(iNod, idim) * DNX(idim, iNod))
  
        end do
      
        ! test: affichage de la divergence
        ! print*, 'iG=', iG, ' div=', div
 
        ! pour chaque noeud de l'element
        do iNod=1, mecaEF(i)%N_NODE
       
          ! on ajoute la contribution du point de Gauss courant
          ! a la composante associee au noeud courant du vecteur
          ! elementaire:
          !   div(field)(x_iG)*N_iNod(x_iG)*w(iG)*det(J)(x_iG)
          Fext(((iNod-1)*nbdime)+idim) = Fext(((iNod-1)*nbdime)+idim) + div*mecaEF(i)%PG(ig)%N(iNod)*COEFINT

          !print*,((iNod-1)*nbdime)+idim,Fext(((iNod-1)*nbdime)+idim)
   
        end do

      enddo
      ! on libere l'espce memoire occupe par DNX
      deallocate(DNX)

   end do

end subroutine compute_elementary_field_divergence_ISO

!> get a pointer on working element array
subroutine get_ele_ptr_mecaEF_iso(id,coor_ele,primal_ele,dual_ele,operator_ele)
  implicit none 
  integer :: id
  real(kind=8), pointer :: coor_ele(:),primal_ele(:),dual_ele(:),operator_ele(:,:)

  coor_ele => mecaEF(id)%coor_ele
  primal_ele => mecaEF(id)%primal_ele
  dual_ele => mecaEF(id)%dual_ele
  operator_ele => mecaEF(id)%operator_ele

end subroutine


subroutine get_nearest_gp_mecaEF_iso(eleid,X,coor,gpid)
  implicit none
  ! le numero de l'element dans la liste locale
  integer, intent(in) :: eleid       
  ! coordonnees des sommets
  real(kind=LONG), dimension(:,:), intent(in)  :: X
  ! coordonnees du point
  real(kind=LONG), dimension(:)  , intent(out) :: coor
  ! gp id
  integer, intent(out) :: gpid

  call get_nearest_gp(mecaEF(eleID)%N_NODE       , &
                      mecaEF(eleID)%T_FONC_FORME , &
                      mecaEF(eleID)%SCH_GAUSS_RIG, &
                      nbdime, X, coor, gpid        ) 
  
end subroutine
  
subroutine check_elementary_ppset_iso(i,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  ! le numero de l'element dans la liste locale
  INTEGER        , INTENT(IN)     :: i       
  integer,dimension(:),intent(in) :: ppsnb
  INTEGER        , INTENT(IN)     :: ibdyty,iblmty

  !**
  integer                         :: ig,mdlnb,lawnb
  integer                         :: drank,ecrank
  real(kind=8)                    :: density,elas_coeff(2)
  ! le materiau est defini par element

  
  IF (get_eleop_value_bypps(ppsnb(1),'isext') /= 'no___') THEN

    call get_ppset_value(ppsnb(1),mdlnb,lawnb)   
     
    drank=0
    if (get_rho_type(lawnb) .ne. 0) then
      drank = get_meca_field_rank_MAILx(ibdyty,iblmty,'density')
    endif
     
    ecrank=0 
    if (get_elas_coeff_type(lawnb) .ne. 0) then
      ecrank = get_meca_vfield_rank_MAILx(ibdyty,iblmty,'elas_coeff')
    endif
     
    do IG=1,mecaEF(i)%N_PG_RIG

       if (drank==0 .and. ecrank==0) then
         call check_external_ppset(ppsnb(ig))  
       else if (drank/=0 .and. ecrank==0) then           
         call get_meca_field_MAILx(ibdyty,iblmty,ig,drank,density)
         call check_external_ppset(ppsnb(ig),sf_density=density)  
       else if (drank/=1 .and. ecrank/=1) then           
         call get_meca_field_MAILx(ibdyty,iblmty,ig,drank,density)
         call get_meca_vfield_MAILx(ibdyty,iblmty,ig,ecrank,elas_coeff,2)          
         call check_external_ppset(ppsnb(ig),sf_density=density,vf_elas_coeff=elas_coeff)  
       else if (drank==0 .and. ecrank/=1) then           
         call get_meca_vfield_MAILx(ibdyty,iblmty,ig,ecrank,elas_coeff,2)          
         call check_external_ppset(ppsnb(ig),vf_elas_coeff=elas_coeff)  
       endif
      
    enddo   
  endif  
end subroutine check_elementary_ppset_iso

!!!! RIP !!!

! ! rm's shit for new arch
! ! a voir si ca ne doit pas degager

! subroutine compute_elementary_bulk_iso2(i,ppsnb,dt,X,U,fields,gauss_map,gauss_names,Fint,K)
!    implicit none
!    integer(kind=4),                   intent(in)    :: i           !< [in] element model number
!    integer(kind=4),   dimension(:)  , intent(in)    :: ppsnb       !< [in] property set
!    real(kind=8)                                     :: dt          !< [in] time step
!    integer(kind=4),   dimension(:,:), intent(in)    :: gauss_map   !< [in] map to acces gauss point fields
!    character(len=30), dimension(:)  , intent(in)    :: gauss_names !< [in] map to get gauss fields name
!    real(kind=LONG),   dimension(:)  , intent(inout) :: fields      !< [in] fields values at gauss points
!    real(kind=LONG),   dimension(:,:), intent(in)    :: X           !< [in] reference coordinates of the nodes
!    real(kind=LONG),   dimension(:)  , intent(in)    :: U           !< [in] displacement of the nodes
!    real(kind=LONG),   dimension(:)  , intent(out)   :: Fint        !< [out] elementary internal forces vector
!    real(kind=LONG),   dimension(:,:), intent(out)   :: K           !< [out] elementary rigidity matrix
!    !
!    integer(kind=4)   :: mdlnb,inull
!    character(len=20) :: IAM
!    !    12345678901234567890
!    IAM='a_meca_EF_iso::bulk2'

!    call get_ppset_value(ppsnb(1),mdlnb,inull)

!    select case(get_eleop_value(mdlnb,'kine_'))
!    case('small')
!      call bulk_hpp_iso2(i,ppsnb,dt,X,U,fields,gauss_map,gauss_names,Fint,K)
!    case('large')
!      call bulk_gd_iso2(i,ppsnb,dt,X,U,fields,gauss_map,gauss_names,Fint,K)
!    case default
!      call logMES('kinematic=='//get_eleop_value(mdlnb,'kine_'))
!      call faterr(IAM,'kinematic type unknown (small | large)')
!    end select

! end subroutine

! subroutine bulk_hpp_iso2(i,ppsnb,dt,X,U,fields,gauss_map,gauss_names,Fint,K)
!   implicit none
!   integer(kind=4),                   intent(in)  :: i           !< [in] element model number
!   integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb       !< [in] property set
!   real(kind=8)                                   :: dt          !< [in] time step
!   integer(kind=4),   dimension(:,:), intent(in)  :: gauss_map   !< [in] map to acces gauss point fields
!   character(len=30), dimension(:)  , intent(in)  :: gauss_names !< [in] map to get gauss fields name
!   real(kind=LONG),   dimension(:)  , intent(in)  :: fields      !< [in] fields values at gauss points
!   real(kind=LONG),   dimension(:,:), intent(in)  :: X           !< [in] reference coordinates of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: U           !< [in] displacement of the nodes
!   real(kind=LONG),   dimension(:)  , intent(out) :: Fint        !< [out] elementary internal forces vector
!   real(kind=LONG),   dimension(:,:), intent(out) :: K           !< [out] elementary rigidity matrix
!   ! 
!   real(kind=LONG), dimension(:,:), pointer :: DNX
!   real(kind=LONG), dimension(:,:), pointer :: Bl
!   real(kind=LONG), dimension(:,:), pointer :: D
!   real(kind=LONG) :: coefint,R
!   integer(kind=4) :: IG, anisotropie, nb_external, nb_internal, mdlnb
!   ! zone de stockage: gradient,flux,internal,operateur tangent
!   real(kind=8), dimension(:), allocatable :: GRAD0,FLUX0,INTERNAL0
!   real(kind=8), dimension(:), allocatable :: GRAD1,FLUX1,INTERNAL1
!   ! parametres externes
!   ! gestion de multiples field|extP pour couplage
!   integer(kind=4)   :: if
!   character(len=30) :: name
!   integer(kind=4)                              :: extP_nb
!   character(len=30), dimension(:), allocatable :: extP_lbl
!   integer(kind=4)  , dimension(:), allocatable :: extP_len
!   real(kind=8)     , dimension(:), allocatable :: extP_val
!   ! switch forme de B 
!   integer(kind=4) :: istrg,inull
!   ! demande calcul matrice tangente
!   integer(kind=4) :: calcD
!   !
!   character(len=100) :: cout
!   character(len=25)  :: IAM
!   !      1234567890123456789012345
!   IAM = 'mecaEF_iso::bulk_hpp_iso2'

!   !fd 13/09
!   !mdl is the same for all gp of the element also some time its faster 
!   !to take information from the first one ...

!   ! Initialisation a vide des pointeurs
!   nullify(Bl,DNX,D)

!   ! Allocation internal purpose arrays
!   call get_ppset_value(ppsnb(1),mdlnb,inull)
 
!   nb_external = get_nb_external_variables(mdlnb)
!   nb_internal = get_nb_internal_variables(mdlnb)

!   allocate(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
!   allocate(INTERNAL0(nb_internal),INTERNAL1(nb_internal))

!   K    = ZERO
!   Fint = ZERO

!   do ig = 1, mecaEF(i)%N_PG_RIG ! Pour tous les points de Gauss
!     ! on rapatrie les infos du debut de pas

!     ! index of stress in gauss_map : 2
!     FLUX0 = fields(gauss_map(2,ig)+1:gauss_map(3,ig))
!     FLUX1 = 0.D0

!     ! index of strain in gauss_map : 1
!     GRAD0 = fields(gauss_map(1,ig)+1:gauss_map(2,ig))
!     GRAD1 = 0.D0

!     ! index of internal in gauss_map : 3
!     if( nb_internal /= 0 ) then
!       INTERNAL0 = fields(gauss_map(3,ig)+1:gauss_map(4,ig))
!       INTERNAL1 = 0.d0
!     end if

!     call GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
!                       mecaEF(i)%PG(ig)%POIDS,X,DNX,coefint,R)

!     if( get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___' ) then

!       if (get_eleop_value_bypps(ppsnb(ig),'mater') /= 'elas_') then 
!         write(cout,'(A)') 'Using isext == no___ is only possible with mater == elas'
!         write(cout,'(A,1x,A)') 'and mater =', get_eleop_value_bypps(ppsnb(ig),'mater')
!         call faterr(IAM,cout)
!       endif

!       istrg = 1

!       call Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl        
!       call D_SOLID_ISO(ppsnb(ig),D)

!       GRAD1(1:size(Bl,dim=1)) = matmul( Bl, U )

!       call comp_stress(ppsnb(ig),GRAD1,FLUX1)

!     else
!       ! on va utiliser la notion de field attache au model
!       extP_nb = get_external_field_nb(mdlnb)
!       if( extP_nb /= 0 ) then 
!         allocate(extP_lbl(extP_nb), &
!                  extP_len(extP_nb), &       
!                  extP_val(extP_nb)  )

!         do if = 1, extP_nb
!           name=get_external_field_name(mdlnb,if)
!           extP_lbl(if)=name
!           extP_len(if)=len_trim(name)
!           extP_val(if) = fields(gauss_map(3+if,ig)+1)
!         end do
!       else ! extP_nb == 0
!         allocate(extP_lbl(1), &
!                  extP_len(1), &       
!                  extP_val(1))
!         extP_lbl(1)=' '
!         extP_val(1)=0.
!       end if

!       istrg = 2
!       call Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl        
!       GRAD1 = matmul( Bl, U )
!       calcD=1
!       CALL compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
!                                GRAD0,FLUX0,INTERNAL0, GRAD1,FLUX1,INTERNAL1, &
!                                D,dt,calcD)

!       deallocate(extP_lbl, &
!                  extP_len, &       
!                  extP_val  )
!     end if 

!     !  ke= Blt.D.Bl.coef
!     K = K +  MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*COEFINT
!     Fint = Fint + matmul( transpose(Bl), FLUX1(1:size(Bl,dim=1)) ) * COEFINT

!   end do

!   deallocate(Bl,DNX) ;  nullify(Bl,DNX)
!   deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

! end subroutine bulk_hpp_iso2

! subroutine bulk_gd_iso2(i,ppsnb,dt,X,U,fields,gauss_map,gauss_names,Fint,K)
!   implicit none
!   integer(kind=4),                   intent(in)    :: i           !< [in] element model number
!   integer(kind=4),   dimension(:)  , intent(in)    :: ppsnb       !< [in] property set
!   real(kind=8)                                     :: dt          !< [in] time step
!   integer(kind=4),   dimension(:,:), intent(in)    :: gauss_map   !< [in] map to acces gauss point fields
!   character(len=30), dimension(:)  , intent(in)    :: gauss_names !< [in] map to get gauss fields name
!   real(kind=LONG),   dimension(:)  , intent(inout) :: fields      !< [in] fields values at gauss points
!   real(kind=LONG),   dimension(:,:), intent(in)    :: X           !< [in] reference coordinates of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)    :: U           !< [in] displacement of the nodes
!   real(kind=LONG),   dimension(:)  , intent(out)   :: Fint        !< [out] elementary internal forces vector
!   real(kind=LONG),   dimension(:,:), intent(out)   :: K           !< [out] elementary rigidity matrix
!   ! 
!   real(kind=LONG), dimension(:,:), pointer :: DNX
!   real(kind=LONG), dimension(:,:), pointer :: B
!   real(kind=LONG), dimension(:,:), pointer :: D
!   real(kind=LONG), dimension(:),   pointer :: Id
!   !
!   real(kind=LONG) :: coefint,R
!   integer(kind=4) :: IG, anisotropie, nb_external, nb_internal, mdlnb, lawnb
!   ! zone de stockage: gradient,flux,internal,operateur tangent
!   real(kind=8), dimension(:), allocatable :: GRAD0,FLUX0,INTERNAL0
!   real(kind=8), dimension(:), allocatable :: GRAD1,FLUX1,INTERNAL1
!   ! parametres externes
!   ! gestion de multiples field|extP pour couplage
!   integer(kind=4)   :: if
!   character(len=30) :: name
!   integer(kind=4)                              :: extP_nb
!   character(len=30), dimension(:), allocatable :: extP_lbl
!   integer(kind=4)  , dimension(:), allocatable :: extP_len
!   real(kind=8)     , dimension(:), allocatable :: extP_val
!   ! switch forme de B 
!   integer(kind=4) :: istrg,inull
!   ! demande calcul matrice tangente
!   integer(kind=4) :: calcD
!   !
!   real(kind=8), dimension(6,4) :: vpg
!   real(kind=8), dimension(4,4) :: Sloc
!   !
!   character(len=100) :: cout
!   character(len=24)  :: IAM
!   !      123456789012345678901234
!   IAM = 'mecaEF_iso::bulk_gd_iso2'

!   ! on appelle l'element QP0
!   !fd a reprendre !!
!   if( get_eleop_value_bypps(ppsnb(1),'isext') == 'no___' ) then

!     if( mecaEF(i)%NAME /= i_q4p0x ) then
!       call logmes('Element iso ='//get_NAME_mecaEF_iso(mecaEF(i)%NAME))
!       call faterr(IAM,'with kine_ == large and isext == no___ only Q4P0x element is available') 
!     end if

!     call get_ppset_value(ppsnb(1),mdlnb,lawnb)

!     do ig = 1, mecaEF(i)%N_PG_RIG 
!       vpg(1:6,ig) = fields(gauss_map(3,ig)+1:gauss_map(4,ig))
!     end do
!     call bulk_nl_iso(i,mdlnb,lawnb,X,U,vpg,Sloc,Fint,K)
!     do ig = 1, mecaEF(i)%N_PG_RIG 
!       fields(gauss_map(2,ig)+1:gauss_map(3,ig)) = Sloc(1:4,ig)
!       fields(gauss_map(3,ig)+1:gauss_map(4,ig)) = vpg(1:6,ig)
!     end do

!     return

!   end if 

!   ! Initialisation a vide des pointeurs
!   nullify(DNX)
!   nullify(B)
!   nullify(Id)

!   ! Allocation internal purpose arrays
!   call get_ppset_value(ppsnb(1),mdlnb,inull)

!   nb_external = get_nb_external_variables(mdlnb)
!   nb_internal = get_nb_internal_variables(mdlnb)

!   allocate(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
!   allocate(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
!   allocate(D(nb_external,nb_external))

!   ! on va utiliser la notion de field attache au model
!   extP_nb = get_external_field_nb(mdlnb)
!   if( extP_nb /= 0 ) then 
!     allocate(extP_lbl(extP_nb), &
!              extP_len(extP_nb), &       
!              extP_val(extP_nb)  )
!   else
!     allocate(extP_lbl(1), &
!              extP_len(1), &       
!              extP_val(1))
!   end if

!   Fint = ZERO
!   K    = ZERO

!   do IG = 1, get_N_PG_RIG_mecaEF(i) ! Pour tous les points de Gauss

!     ! index of stress in gauss_map : 2
!     FLUX0 = fields(gauss_map(2,ig)+1:gauss_map(3,ig))
!     FLUX1 = 0.D0

!     ! index of strain in gauss_map : 1
!     GRAD0 = fields(gauss_map(1,ig)+1:gauss_map(2,ig))
!     GRAD1 = 0.D0

!     ! index of internal in gauss_map : 3
!     if( nb_internal /= 0 ) then
!       INTERNAL0 = fields(gauss_map(3,ig)+1:gauss_map(4,ig))
!       INTERNAL1 = 0.d0
!     end if

!     if( extP_nb /= 0 ) then 

!       do if=1,extP_nb
!         name=get_external_field_name(mdlnb,if)
!         extP_lbl(if)=name
!         extP_len(if)=len_trim(name)
!         extP_val(if) = fields(gauss_map(3+if,ig)+1)
!         !!$ print *,'<<--------------------'
!         !!$ print *,ibdyty,iblmty,ig,if,extP_val(if)
!         !!$ print *,extP_lbl(if),extP_len(if)
!         !!$ print *,'-------------------->>'
!         !if (iblmty==1 .and. ig==1) print*,extP_val
!       enddo

!     else

!       extP_lbl(1)=' '
!       extP_val(1)=0.

!     end if

!     call gradient_iso(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
!                       mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

!     call B_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,B)

!     call Id_ISO(Id) ! formation de Id 

!     if( NSTEP == 1 ) then
!       !fd a modifier avec la procedure d'initialisation de laurent
!       FLUX0     = ZERO
!       GRAD0     = Id
!       INTERNAL0 = 0.d0
!       if( nb_internal /= 0 .and. size(INTERNAL0) .ge. size(Id) ) then
!         INTERNAL0(1:SIZE(Id)) = Id
!       end if 
!     end if

!     ! CALCUL DU GRADIENT DES DEFORMATIONS
!     !*** Calcul de [1+d(u_n+1)/d(x_0)]

!     GRAD1 = matmul(B,reshape(source=U,shape=(/ size(U) /) ) )
!     GRAD1 = Id + GRAD1
!     !
!     FLUX1 = ZERO
!     if( nb_internal /= 0 ) INTERNAL1 = ZERO
!     !
!     calcD = 1

!     call compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
!                              GRAD0,FLUX0,INTERNAL0, &
!                              GRAD1,FLUX1,INTERNAL1, &
!                              D,dt,calcD)
!     !
!     !  ke = Bt.D.B.coef
!     !
!     K = K + (matmul(transpose(B),matmul(D,B))*COEFINT)

!     !
!     !  Fint = Bt.PK.coef
!     !
!     Fint = Fint + (matmul(transpose(B),FLUX1(1:size(B,dim=1)))*COEFINT)

!   end do

!   deallocate(extP_lbl,extP_len,extP_val)
!   deallocate(DNX,B,Id) ;  nullify(DNX,B,Id)
!   deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

! end subroutine bulk_gd_iso2

! subroutine compute_elementary_fields_iso2(i,ppsnb,dt,X,U,fields_old,fields_new,gauss_map,gauss_names)
!   implicit none
!   integer(kind=4),                   intent(in)  :: i           !< [in] element model number
!   integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb       !< [in] property set
!   real(kind=8)                                   :: dt          !< [in] time step
!   integer(kind=4),   dimension(:,:), intent(in)  :: gauss_map   !< [in] map to acces gauss point fields
!   character(len=30), dimension(:)  , intent(in)  :: gauss_names !< [in] map to get gauss fields name
!   real(kind=LONG),   dimension(:,:), intent(in)  :: X           !< [in] reference coordinates of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: U           !< [in] displacement of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: fields_old  !< [in] fields values at gauss points
!   real(kind=LONG),   dimension(:)  , intent(out) :: fields_new  !< [out] new fields values at gauss points
!   !
!   integer(kind=4)  :: mdlnb, inull
!   character(len=45):: IAM
!       !123456789012345678901234567890123456789012345
!   IAM='a_meca_EF_iso::compute_elementary_fields_iso2'

!   CALL get_ppset_value(ppsnb(1),mdlnb,inull)

!   select case(get_eleop_value(mdlnb,'kine_'))
!   case('small')
!     call fields_hpp_iso2(i,ppsnb,dt,X,U,fields_old,fields_new,gauss_map,gauss_names)
!   case('large')
!     call fields_gd_iso2(i,ppsnb,dt,X,U,fields_old,fields_new,gauss_map,gauss_names)
!   case default
!     call logMes('kinematic=='//get_eleop_value(mdlnb,'kine_'))
!     call faterr(IAM,'kinematic type unknown (small | large)')
!   end select

! end subroutine
! !----------------------------------------------------------------------------!

! subroutine fields_hpp_iso2(i,ppsnb,dt,X,U,fields_old,fields_new,gauss_map,gauss_names)
!   implicit none
!   integer(kind=4),                   intent(in)  :: i           !< [in] element model number
!   integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb       !< [in] property set
!   real(kind=8)                                   :: dt          !< [in] time step
!   integer(kind=4),   dimension(:,:), intent(in)  :: gauss_map   !< [in] map to acces gauss point fields
!   character(len=30), dimension(:)  , intent(in)  :: gauss_names !< [in] map to get gauss fields name
!   real(kind=LONG),   dimension(:,:), intent(in)  :: X           !< [in] reference coordinates of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: U           !< [in] displacement of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: fields_old  !< [in] fields values at gauss points
!   real(kind=LONG),   dimension(:)  , intent(out) :: fields_new  !< [out] new fields values at gauss points
!   !
!   ! Bl : epsilon=Bl q
!   ! D  : behaviour matrix
!   real(kind=LONG), dimension(:,:), pointer :: DNX, Bl, D
!   real(kind=LONG) :: COEFINT, R
!   integer(kind=4) :: ig, anisotropie, nb_external, nb_internal, mdlnb

!   ! zone de stockage: gradient,flux,internal,operateur tangent
!   real(kind=8), dimension(:), allocatable :: GRAD0,FLUX0,INTERNAL0
!   real(kind=8), dimension(:), allocatable :: GRAD1,FLUX1,INTERNAL1

!   integer(kind=4)                      :: calcD

!   ! parametres externes
!   ! gestion de multiples field|extP pour couplage
!   integer(kind=4)   :: if
!   character(len=30) :: name
!   integer(kind=4)                              :: extP_nb
!   character(len=30), DIMENSION(:), allocatable :: extP_lbl
!   integer(kind=4)  , DIMENSION(:), allocatable :: extP_len
!   real(kind=8)     , DIMENSION(:), allocatable :: extP_val

!   ! switch forme de B 
!   integer(kind=4) :: istrg,inull

!   ! Initialisation a vide des pointeurs
!   nullify(Bl,DNX)

!   ! Allocation internal purpose arrays
!   call get_ppset_value(ppsnb(1),mdlnb,inull)

!   nb_external=get_nb_external_variables(mdlnb)
!   nb_internal=get_nb_internal_variables(mdlnb)

!   ! on va utiliser la notion de field attache au model

!   extP_nb = get_external_field_nb(mdlnb)
!   if( extP_nb /= 0 ) then 

!     allocate(extP_lbl(extP_nb), &
!              extP_len(extP_nb), &       
!              extP_val(extP_nb)  )
!   else

!     allocate(extP_lbl(1), &
!              extP_len(1), &       
!              extP_val(1))

!   end if

!   allocate(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
!   allocate(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
!   allocate(D(0,0))

!   ! Pour tous les points de Gauss
!   do ig =1, mecaEF(i)%N_PG_RIG

!     ! on rapatrie les infos du debut de pas

!     ! index of stress in gauss_map : 2
!     FLUX0 = fields_old(gauss_map(2,ig)+1:gauss_map(3,ig))
!     FLUX1 = 0.D0

!     ! index of strain in gauss_map : 1
!     GRAD0 = fields_old(gauss_map(1,ig)+1:gauss_map(2,ig))
!     GRAD1 = 0.D0

!     ! index of internal in gauss_map : 3
!     if( nb_internal /= 0 ) then
!       INTERNAL0 = fields_old(gauss_map(3,ig)+1:gauss_map(4,ig))
!       INTERNAL1 = 0.d0
!     end if

!     CALL GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
!                       mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)

!     IF (get_eleop_value_bypps(ppsnb(ig),'isext') == 'no___') THEN

!       istrg = 1
!       CALL Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl) ! formation de Bl 
                                
!       GRAD1(1:SIZE(Bl,dim=1)) = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /)))

!       CALL comp_stress(ppsnb(ig),GRAD1,FLUX1)

!     else

!       extP_nb = get_external_field_nb(mdlnb)
!       if( extP_nb /= 0 ) then 

!         do if=1,extP_nb
!           name=get_external_field_name(mdlnb,if)
!           extP_lbl(if)=name
!           extP_len(if)=len_trim(name)
!           extP_val(if) = fields_old(gauss_map(3+if,ig)+1)

!           !!$ print *,'<<--------------------'
!           !!$ print *,ibdyty,iblmty,ig,if,extP_val(if)
!           !!$ print *,extP_lbl(if),extP_len(if)
!           !!$ print *,'-------------------->>'
!         end do

!       else

!         extP_lbl(1)=' '
!         extP_val(1)=0.

!       end if

!       istrg = 2
!       call Bl_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,istrg,Bl)    ! formation de Bl        

!       GRAD1 = MATMUL(Bl,RESHAPE(source=U,shape=(/ SIZE(U) /) ) )
 
!       calcD=0

!       call compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
!                                GRAD0,FLUX0,INTERNAL0, &
!                                GRAD1,FLUX1,INTERNAL1, &
!                                D,dt,calcD)
!     end if

!     fields_new(gauss_map(1,ig)+1:gauss_map(2,ig)) = GRAD1
!     fields_new(gauss_map(2,ig)+1:gauss_map(3,ig)) = FLUX1
!     if( nb_internal /= 0 ) then
!       fields_new(gauss_map(3,ig)+1:gauss_map(4,ig)) = INTERNAL1
!     end if

!   end do

!   deallocate(extP_lbl, extP_len, extP_val  )

!   deallocate(Bl,DNX) ; nullify(Bl,DNX)
!   deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

! end subroutine fields_hpp_iso2

! subroutine fields_gd_iso2(i,ppsnb,dt,X,U,fields_old,fields_new,gauss_map,gauss_names)
!   implicit none
!   integer(kind=4),                   intent(in)  :: i           !< [in] element model number
!   integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb       !< [in] property set
!   real(kind=8)                                   :: dt          !< [in] time step
!   integer(kind=4),   dimension(:,:), intent(in)  :: gauss_map   !< [in] map to acces gauss point fields
!   character(len=30), dimension(:)  , intent(in)  :: gauss_names !< [in] map to get gauss fields name
!   real(kind=LONG),   dimension(:,:), intent(in)  :: X           !< [in] reference coordinates of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: U           !< [in] displacement of the nodes
!   real(kind=LONG),   dimension(:)  , intent(in)  :: fields_old  !< [in] fields values at gauss points
!   real(kind=LONG),   dimension(:)  , intent(out) :: fields_new  !< [out] new fields values at gauss points
!   !
!   ! D  : behaviour matrix
!   real(kind=LONG), dimension(:,:), pointer :: DNX, B, D
!   real(kind=LONG), dimension(:)  , pointer :: Id
!   real(kind=LONG) :: COEFINT, R
!   integer(kind=4) :: ig, anisotropie, nb_external, nb_internal, mdlnb, lawnb
!   ! zone de stockage: gradient,flux,internal,operateur tangent
!   real(kind=8), dimension(:), allocatable :: GRAD0,FLUX0,INTERNAL0
!   real(kind=8), dimension(:), allocatable :: GRAD1,FLUX1,INTERNAL1

!   integer(kind=4)                      :: calcD
!   ! parametres externes
!   ! gestion de multiples field|extP pour couplage
!   integer(kind=4)   :: if
!   character(len=30) :: name
!   integer(kind=4)                              :: extP_nb
!   character(len=30), DIMENSION(:), allocatable :: extP_lbl
!   integer(kind=4)  , DIMENSION(:), allocatable :: extP_len
!   real(kind=8)     , DIMENSION(:), allocatable :: extP_val
!   ! switch forme de B 
!   integer(kind=4) :: istrg,inull
!   !
!   character(len=28) :: IAM
!   !     1234567890123456789012345678
!   IAM ='a_mecaEF_iso::fields_gd_iso2'

!   !fd a reprendre !!
!   if( get_eleop_value_bypps(ppsnb(1),'isext') == 'no___' ) then

!     if( mecaEF(i)%NAME /= i_q4p0x ) then
!       call logmes('Element iso ='//get_NAME_mecaEF_iso(mecaEF(i)%NAME))
!       call faterr(IAM,'with kine_ == large and isext == no___ only Q4P0x element is available') 
!     endif

!     call get_ppset_value(ppsnb(1),mdlnb,lawnb)

!     !fd on n'a pas la focntion on ne recalcule rien
!     !?  CALL BULK_NL_ISO(i,mdlnb,lawnb,X,U,ibdyty,iblmty,Fint,K)

!     return
!   end if 

!   ! Initialisation a vide des pointeurs
!   nullify(DNX,B,Id)

!   !print *, 'X : '
!   !print *, X
!   !print *, 'U : '
!   !print *, U
!   ! Allocation internal purpose arrays
!   call get_ppset_value(ppsnb(1),mdlnb,inull)

!   nb_external = get_nb_external_variables(mdlnb)
!   nb_internal = get_nb_internal_variables(mdlnb)

!   allocate(GRAD0(nb_external),GRAD1(nb_external),FLUX0(nb_external),FLUX1(nb_external))
!   allocate(INTERNAL0(nb_internal),INTERNAL1(nb_internal))
!   allocate(D(nb_external,nb_external))

!   ! on va utiliser la notion de field attache au model
!   extP_nb = get_external_field_nb(mdlnb)

!   if( extP_nb /= 0 ) then 

!     allocate(extP_lbl(extP_nb), &
!              extP_len(extP_nb), &       
!              extP_val(extP_nb)  )
!   else

!     allocate(extP_lbl(1), &
!              extP_len(1), &       
!              extP_val(1))

!   end if

!   do IG = 1, get_N_PG_RIG_mecaEF(i)     ! Pour tous les points de Gauss

!     ! index of stress in gauss_map : 2
!     FLUX0 = fields_old(gauss_map(2,ig)+1:gauss_map(3,ig))
!     FLUX1 = 0.D0

!     ! index of strain in gauss_map : 1
!     GRAD0 = fields_old(gauss_map(1,ig)+1:gauss_map(2,ig))
!     GRAD1 = 0.D0

!     ! index of internal in gauss_map : 3
!     if( nb_internal /= 0 ) then
!       INTERNAL0 = fields_old(gauss_map(3,ig)+1:gauss_map(4,ig))
!       INTERNAL1 = 0.d0
!     end if

!     if( extP_nb /= 0 ) then 

!       do if=1,extP_nb
!         name=get_external_field_name(mdlnb,if)
!         extP_lbl(if)=name
!         extP_len(if)=len_trim(name)
!         extP_val(if) = fields_old(gauss_map(3+if,ig)+1)

!         !!$ print *,'<<--------------------'
!         !!$ print *,ibdyty,iblmty,ig,if,extP_val(if)
!         !!$ print *,extP_lbl(if),extP_len(if)
!         !!$ print *,'-------------------->>'
!       end do

!     else

!       extP_lbl(1)=' '
!       extP_val(1)=0.

!     end if

!     call GRADIENT_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,mecaEF(i)%PG(ig)%DN, &
!                       mecaEF(i)%PG(ig)%POIDS,X,DNX,COEFINT,R)
!     call B_ISO(mecaEF(i)%N_NODE,mecaEF(i)%PG(ig)%N,DNX,R,B)
!     call Id_ISO(Id) ! formation de Id 

!     if( NSTEP == 1 ) then
!       !fd a modifier avec la procedure d'initialisation de laurent
!       FLUX0     = ZERO
!       GRAD0     = Id
!       INTERNAL0 = 0.d0
!       if( nb_internal /= 0 .and. size(INTERNAL0) .ge. size(Id) ) then
!         INTERNAL0(1:SIZE(Id)) = Id
!       end if 
!     end if

!     ! CALCUL DU GRADIENT DES DEFORMATIONS
!     !*** Calcul de [1+d(u_n+1)/d(x_0)]
!     GRAD1 = matmul(B,reshape(source=U,shape=(/ size(U) /) ) )
!     GRAD1 = Id + GRAD1
!     !
!     FLUX1 = ZERO
!     if( nb_internal /= 0 ) INTERNAL1 = ZERO
!     !
!     calcD=0

!     call compute_external_pg(ppsnb(ig),extP_lbl,extP_len,30_4,extP_val,extP_nb, &
!                              GRAD0,FLUX0,INTERNAL0, &
!                              GRAD1,FLUX1,INTERNAL1, &
!                              D,dt,calcD)

!     fields_new(gauss_map(1,ig)+1:gauss_map(2,ig)) = GRAD1
!     fields_new(gauss_map(2,ig)+1:gauss_map(3,ig)) = FLUX1
!     if( nb_internal /= 0 ) then
!       fields_new(gauss_map(3,ig)+1:gauss_map(4,ig)) = INTERNAL1
!     end if

!     !print *, 'ig : ', IG
!     !print *, 'stress : '
!     !print *, FLUX1
!     !print *, 'strain : '
!     !print *, GRAD1
!     !print *, 'intern : '
!     !print *, INTERNAL1
!   end do

!   deallocate(extP_lbl,extP_len,extP_val)
!   deallocate(DNX,B,Id) ;  nullify(DNX,B,Id)

!   deallocate(GRAD0,GRAD1,FLUX0,FLUX1,INTERNAL0,INTERNAL1,D)

! end subroutine fields_gd_iso2

! !> \brief interpolate a field values from gauss point to nodes
! !> \todo: sort dime thingy
! !> \todo: do field_index other than 1 or 2 (strain and streess)
! !> \todo: what is field_names for ?
! subroutine gpv2node_iso2(blmID,mdlnb,field_index,fields,field_map,field_names,nodal_values,dime)
!   implicit none
!   integer(kind=4),                 intent(in)    :: blmID        !< [in] finite element type id
!   integer(kind=4),                 intent(in)    :: mdlnb        !< [in] model id
!   integer(kind=4),                 intent(in)    :: field_index  !< [in] index of the field to compute
!   real(kind=8)   , dimension(:)  , intent(in)    :: fields       !< [in] fields values at gauss points
!   integer(kind=4), dimension(:,:), intent(in)    :: field_map    !< [in] map to acces gauss point fields
!   character(len=30), dimension(:), intent(in)    :: field_names  !< [in] map to get gauss fields name
!   real(kind=8)   , dimension(:,:), intent(inout) :: nodal_values !< [out] values of the field computed on nodes 
!   integer(kind=4),                 intent(in)    :: dime         !< [in] dimension
!   !
!   integer(kind=4) :: nb_nodes_ele, nb_gp_ele, field_size
!   real(kind=8), dimension(:), allocatable :: v_nodes
!   !                          123456789012345678901234567
!   character(len=27)  :: IAM='a_mecaEF_iso::gpv2node_iso2'

!   real(kind=8), allocatable :: GRAD(:,:),FLUX(:,:),ELE_FIELD(:,:) ! vecteur de travail local
!   integer(kind=4) :: i_node, ig, if, inull, nb_external
!   real(kind=8)    :: tmp,PP(9),FF(9),A33(3,3),A33T(3,3)

!   ! TODO mettre ca dans l'element fini
!   real(kind=8),dimension(:,:),allocatable :: smat,mmat ! calcul valeur aux noeuds sommet, noeuds milieu 
!   real(kind=8) :: f1,f2,f3,f4
!   integer :: nbs,nbm

!   nb_gp_ele    = mecaEF(blmID)%N_PG_RIG  
!   nb_nodes_ele = mecaEF(blmID)%N_NODE

!   field_size = size(nodal_values,dim=1)

!   if( nb_nodes_ele /= size(nodal_values,dim=2) ) then
!     call faterr(IAM,'Non conforming sizes')
!   endif

!   nodal_values(:,:) = 0.d0

!   allocate( v_nodes(nb_nodes_ele) )
!   v_nodes = 0.d0

!   allocate( ELE_FIELD(field_size,nb_gp_ele))
!   ELE_FIELD = 0.d0

!   nb_external = get_nb_external_variables(mdlnb)

!   allocate( GRAD(nb_external,nb_gp_ele), FLUX(nb_external,nb_gp_ele) )
!   ! if to be added again, do not forget the deallocate !!!!
!   !allocate( INTERNAL(nb_internal,nb_gp_ele) )

!   do ig = 1, nb_gp_ele
!     ! index of stress in field_map : 2
!     FLUX(1:nb_external,ig) = fields(field_map(2,ig)+1:field_map(3,ig))

!     ! index of strain in field_map : 1
!     GRAD(1:nb_external,ig) = fields(field_map(1,ig)+1:field_map(2,ig))

!     ! index of internal in field_map : 3
!     !if( nb_internal /= 0 ) then
!     !  INTERNAL(1:nb_internal,ig) = fields(field_map(3,ig)+1:field_map(4,ig))
!     !end if

!   enddo

!   select case(field_index)

!     case(1) ! strain
!       if( get_eleop_value(mdlnb,'isext') == 'no___' ) then

!         call logmes(IAM//'Only external models are supported ')

!       else
!         !fd en dur pour matlib. A transferer dans external ?
!         if( get_eleop_value(mdlnb,'kine_') == 'small' ) then
!           ! rien a faire sauf permutation
!           do ig = 1, nb_gp_ele
!             ELE_FIELD(1:nb_external,ig) = GRAD(1:nb_external,ig) 
!           end do
!         else if( get_eleop_value(mdlnb,'kine_') == 'large') then
!           do ig = 1, nb_gp_ele
!             FF(1:nb_external)=GRAD(1:nb_external,ig)

!             if( dime == 2 ) then
!               tmp=((FF(1)*FF(4)) - (FF(2)*FF(3)))
!               if (tmp /= 0.d0) tmp=1.d0/tmp 

!               PP(1) = tmp*FF(4)
!               PP(2) =-tmp*FF(2) 
!               PP(3) =-tmp*FF(3)
!               PP(4) = tmp*FF(1)
!               PP(5) = 0.d0
!               if (FF(5) /= 0.d0) PP(5) = 1.d0/ FF(5)
          
!               ELE_FIELD(1,ig)=0.5*(1.d0 - (PP(1)*PP(1) + PP(3)*PP(3))) !exx
!               ELE_FIELD(2,ig)=0.5*(1.d0 - (PP(2)*PP(2) + PP(4)*PP(4))) !eyy 
!               ELE_FIELD(3,ig)=0.5*(0.d0 - (PP(1)*PP(2) + PP(2)*PP(4))) !exy 
!               ELE_FIELD(4,ig)=0.5*(1.d0 - (PP(5)*PP(5)))               !ezz
!             else
!               !fd calcul de F^-1 on stocke dans A33
!               A33   = reshape(FF,(/3,3/)) !A33=F^T car stockage C de FF
!               A33T  = transpose(A33)      !F

!               call inverse33(A33T, inull) !F^-1

!               if (inull == 1) then
!                 !print*,'Body ',ibdyty,' element ',iblmty,' gp ',ig
!                 print*,A33T(1,:)
!                 print*,A33T(2,:)
!                 print*,A33T(3,:)
!                 call faterr(IAM,'Non inversible F')
!               endif

!               A33=transpose(A33T)             !F^-T

!               !fd almansi e = 0.5 (I - F^-T F^-1) = 0.5 (I - A33 A33T)
!               !fd field e11 e12 e22 e13 e23 e33
!               ELE_FIELD(1,ig) = 0.5*(1.d0 - (A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1)))
!               ELE_FIELD(2,ig) = 0.5*(     - (A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2)))
!               ELE_FIELD(3,ig) = 0.5*(1.d0 - (A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2)))
!               ELE_FIELD(4,ig) = 0.5*(     - (A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3)))
!               ELE_FIELD(5,ig) = 0.5*(     - (A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3)))
!               ELE_FIELD(6,ig) = 0.5*(1.D0 - (A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3)))
!             end if
!           enddo
!         else
!           call faterr(IAM,'Unsupported kine option')
!         end if
        
!       end if

!     case(2) ! stress
!       if( get_eleop_value(mdlnb,'isext') == 'no___' ) then

!        call logmes(IAM//'Only external models are supported ')

!       else
!         !fd en dur pour matlib. A transferer dans external
!         if( get_eleop_value(mdlnb,'kine_') == 'small' ) then
!           ! matlib: xx,xy,yy,zz -> lmgc90: xx,yy,xy,zz
!           do ig = 1, nb_gp_ele
!             ELE_FIELD(1:nb_external,ig) = FLUX(1:nb_external,ig)
!           enddo
!         else if( get_eleop_value(mdlnb,'kine_') == 'large' ) then
!           do ig = 1, nb_gp_ele
!             !fd cauchy s = J^-1 P F^T
!             PP(1:nb_external) = FLUX(1:nb_external,ig)
!             FF(1:nb_external) = GRAD(1:nb_external,ig)

!             if( dime == 2 ) then
!               tmp = (FF(5)*((FF(1)*FF(4)) - (FF(2)*FF(3))))
!               if (tmp /= 0.d0) tmp=1.d0/tmp 
!               ELE_FIELD(1,ig) = tmp * ( PP(1)*FF(1) + PP(2)*FF(2) ) !sxx
!               ELE_FIELD(2,ig) = tmp * ( PP(3)*FF(3) + PP(4)*FF(4) ) !syy 
!               ELE_FIELD(3,ig) = tmp * ( PP(1)*FF(3) + PP(2)*FF(4) ) !sxy 
!               ELE_FIELD(4,ig) = tmp * ( PP(5)*FF(5))                !szz
!               ELE_FIELD(5,ig) = 0.                                  !svm
!             else
!               A33T =reshape(PP,(/3,3/))         ! A33T=P^T car stockage C de PP
!               A33  =transpose(A33T)             ! P
!               A33T =reshape(FF,(/3,3/))         ! A33T=F^T car stockage C de FF

!               tmp = determinant33(A33T)
!               if( tmp /= 0.d0 ) tmp=1.d0/tmp 
 
!               !fd cauchy s = J^-1 P F^T = tmp A33 A33T
!               !fd field s11 s12 s22 s13 s23 s33

!               ELE_FIELD(1,ig) = tmp*(A33(1,1)*A33T(1,1) + A33(1,2)*A33T(2,1) + A33(1,3)*A33T(3,1))
!               ELE_FIELD(2,ig) = tmp*(A33(1,1)*A33T(1,2) + A33(1,2)*A33T(2,2) + A33(1,3)*A33T(3,2))
!               ELE_FIELD(3,ig) = tmp*(A33(2,1)*A33T(1,2) + A33(2,2)*A33T(2,2) + A33(2,3)*A33T(3,2))
!               ELE_FIELD(4,ig) = tmp*(A33(1,1)*A33T(1,3) + A33(1,2)*A33T(2,3) + A33(1,3)*A33T(3,3))
!               ELE_FIELD(5,ig) = tmp*(A33(2,1)*A33T(1,3) + A33(2,2)*A33T(2,3) + A33(2,3)*A33T(3,3))
!               ELE_FIELD(6,ig) = tmp*(A33(3,1)*A33T(1,3) + A33(3,2)*A33T(2,3) + A33(3,3)*A33T(3,3))
!             end if
!           end do
        
!         else
!           call FATERR(IAM,'Unsupported kine option')
!         end if

!         !fd calcul du von mises
!         do ig = 1,nb_gp_ele
!           if( dime == 2 ) then
!             tmp= (ELE_FIELD(1,ig)+ELE_FIELD(2,ig)+ELE_FIELD(4,ig))/3.d0
!             PP(1) = ELE_FIELD(1,ig) - tmp
!             PP(2) = ELE_FIELD(2,ig) - tmp
!             PP(3) = ELE_FIELD(3,ig)
!             PP(4) = ELE_FIELD(4,ig) - tmp
!             ELE_FIELD(5,ig) = dsqrt(1.5*(PP(1)**2 + PP(2)**2 + PP(4)**2 + (2.*(PP(3)**2)))) 
!           else
!             tmp= ( ELE_FIELD(1,ig)+ELE_FIELD(3,ig)+ELE_FIELD(6,ig) ) / 3.d0
!             PP(1) = ELE_FIELD(1,ig) - tmp
!             PP(2) = ELE_FIELD(2,ig)
!             PP(3) = ELE_FIELD(3,ig) - tmp
!             PP(4) = ELE_FIELD(4,ig)
!             PP(5) = ELE_FIELD(5,ig)
!             PP(6) = ELE_FIELD(6,ig) - tmp

!             ELE_FIELD(7,ig) = dsqrt(1.5*( PP(1)**2 + PP(3)**2 + PP(6)**2 + &
!                                           2.*(PP(2)**2 + PP(4)**2 + pp(5)**2) )) 
!           end if
!         end do

!       end if

!     case default
!       call faterr(IAM,'Unsupported required_field : 1::strain, 2::stress')
!   end select

!   nbs = size(mecaEF(blmID)%gp2node,dim=1)
!   nbm = size(mecaEF(blmID)%node2edge,dim=1)

!   do if = 1, field_size
!      v_nodes(1:nbs) = matmul(mecaEF(blmID)%gp2node,ELE_FIELD(if,1:nbs))

!      if( associated(mecaEF(blmID)%node2edge) ) then
!        v_nodes(nbs+1:nbs+nbm) = matmul(mecaEF(blmID)%node2edge,v_nodes(1:nbs)) 
!      endif

!      nodal_values(if,1:nb_nodes_ele) = nodal_values(if,1:nb_nodes_ele) + v_nodes(1:nb_nodes_ele)

!   enddo

!   deallocate(v_nodes,GRAD,FLUX,ELE_FIELD)
!   !deallocate(smat)
!   !if (allocated(mmat)) deallocate(mmat)

! END SUBROUTINE gpv2node_iso2


END MODULE a_mecaEF_iso
