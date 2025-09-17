!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE wrap_DKDKx

  !!****h* LMGC90.CHIPY/DKDKx
  !! NAME
  !!  module wrap_DKDKx
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DKDKx
  !!****

  USE DKDKx,ONLY: &
       get_nb_DKDKx,&
       get_nb_recup_DKDKx, &
       stock_rloc_DKDKx, &
       recup_rloc_DKDKx, &
       smooth_computation_DKDKx, &
       compute_box_DKDKx, &
       read_ini_Vloc_Rloc_DKDKx, &
       write_xxx_Vloc_Rloc_DKDKx, &
       set_periodic_data_DKDKx, &
       coor_prediction_DKDKx, &
       creation_tab_visu_DKDKx, &
       compute_contact_DKDKx, &
       display_prox_tactors_DKDKx, &
       RUN_DKDKx, &
       CHECK_DKDKx, &
       get_write_Vloc_Rloc_DKDKx, &
       get_adjsz_DKDKx, & ! am: <- debut des fonction supplementaires 
       get_write_Vloc_Rloc_DKDKx, &
       get_local_vloc_DKDKx, &
       get_local_reac_DKDKx, &
       put_local_vloc_DKDKx, &
       put_local_reac_DKDKx, &
       get_nb_DKDKx, get_nb_rough_DKDKx, &
       this2verlt_DKDKx, &
       interface_creation_tab_visu_DKDKx, &
       get_g2g_DKDKx,get_size_g2g_DKDKx 
       

  PUBLIC

CONTAINS
  
!!!-------------------------------------------------------
    SUBROUTINE SelectProxTactors
      IMPLICIT NONE
      integer :: nb_DKDKx
      LOGICAL :: RUN=.FALSE.
       !!****e* DKDKx/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! PURPOSE
       !!  contact detection between DISKx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       !! USES
       !!  LMGC90.CORE/DKDKx/coor_prediction_DKDKx
       !!  LMGC90.CORE/DKDKx/creation_tab_visu_DKDKx
       !!  LMGC90.CORE/DKDKx/compute_contact_DKDKx
       !!****

       if( .not. check_DKDKx() ) then
         return
       end if

       CALL coor_prediction_DKDKx

       RUN = RUN_DKDKx()

       IF (RUN) THEN
          CALL creation_tab_visu_DKDKx

          nb_DKDKx = get_nb_rough_DKDKx()
          WRITE(*,'(1X,I10,A20)') nb_DKDKx,' DKDKx roughly found'       

       END IF

       CALL compute_contact_DKDKx

       nb_DKDKx = get_nb_DKDKx()
       WRITE(*,'(1X,I10,A12)') nb_DKDKx,' DKDKx found'       
    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE StockRloc
      IMPLICIT NONE
       !!****e* DKDKx/StockRloc
       !! NAME 
       !!  StockRloc
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       !! USES
       !!   LMGC90.CORE/DKDKx/stock_rloc_DKDKx
       !!****

       CALL stock_rloc_DKDKx 

    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE RecupRloc
      IMPLICIT NONE
      integer :: nb_DKDKx
       !!****e* DKDKx/RecupRloc
       !! NAME 
       !!  RecupRloc
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!   LMGC90.CORE/DKDKx/recup_rloc_DKDKx
       !!****

       CALL recup_rloc_DKDKx

       nb_DKDKx = get_nb_recup_DKDKx()
       WRITE(*,'(1X,I10,A12)') nb_DKDKx,' recup DKDKx'

    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE SmoothForceComputation
      IMPLICIT NONE
       !!****e* DKDKx/SmoothForceComputation
       !! NAME 
       !!  SmoothForceComputation
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       !! USES
       !!   LMGC90.CORE/DKDKx/md_computation_DKDKx
       !!****

       CALL smooth_computation_DKDKx

    END SUBROUTINE SmoothForceComputation
!!!----------------------------------------------------
    SUBROUTINE WriteLastVlocRloc
      IMPLICIT NONE
       !!****e* DKDKx/WriteLastVlocRloc
       !! NAME
       !!  WriteLastVlocRloc
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDKx contacts
       !! USES
       !!   LMGC90.CORE/DKDKx/write_xxx_Vloc_Rloc_DKDKx
       !!****

       CALL write_xxx_Vloc_Rloc_DKDKx(2)

    END SUBROUTINE
!!!--------------------------------------------------   
    SUBROUTINE WriteOutVlocRloc
      IMPLICIT NONE
      logical :: write_Vloc_Rloc
       !!****e* DKDKx/WriteOutVlocRloc
       !! NAME
       !!  WriteOutVlocRloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDKx contacts
       !! USES
       !!   LMGC90.CORE/DKDKx/write_xxx_Vloc_Rloc_DKDKx
       !!****
       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDKx()
       IF (write_Vloc_Rloc) THEN
          CALL write_xxx_Vloc_Rloc_DKDKx(1)
       END IF

    END SUBROUTINE
!!!--------------------------------------------------    
    SUBROUTINE DisplayOutVlocRloc
      IMPLICIT NONE
       !!****e* DKDKx/DisplayOutVlocRloc
       !! NAME
       !!  DisplayOutVlocRloc
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDKx contacts
       !! USES
       !!   LMGC90.CORE/DKDKx/write_xxx_Vloc_Rloc_DKDKx
       !!****

       CALL write_xxx_Vloc_Rloc_DKDKx(6)

    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE DisplayProxTactors
      IMPLICIT NONE
       !!****e* DKDKx/DisplayProxTactors
       !! NAME
       !!  DisplayProxTactors
       !! PURPOSE
       !!  display contacts
       !! USES
       !!   LMGC90.CORE/DKDKx/display_prox_tactors_DKDKx
       !!****

       CALL display_prox_tactors_DKDKx

    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE ComputeBox
      IMPLICIT NONE
       !!****e* DKDKx/ComputeBox
       !! NAME
       !!  ComputeBox
       !! PURPOSE
       !!  compute parameters for contact detection
       !! USES
       !!   LMGC90.CORE/DKDKx/compute_box_DKDKx
       !!****

       CALL compute_box_DKDKx

    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE ReadIniVlocRloc
      IMPLICIT NONE
       !!****e* DKDKx/ReadIniVlocRloc
       !! NAME
       !!  ReadIniVlocRloc
       !! PURPOSE
       !!  
       !! USES
       !!   LMGC90.CORE/DKDKx/read_ini_Vloc_Rloc_DKDKx
       !!****

       CALL read_ini_Vloc_Rloc_DKDKx

    END SUBROUTINE
!!!--------------------------------------------------
    SUBROUTINE SetPeriodicCondition(rvalue1)
      IMPLICIT NONE
      real(kind=8) :: rvalue1
      logical      :: periodic

       !!****e* DKDKx/SetPeriodicCondition
       !! NAME
       !!  SetPeriodicCondition
       !! SYNOPSIS
       !!  SetPeriodicCondition
       !!  [periode]
       !! INPUTS
       !!  [periode] : periode of periodic condition
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       !!****

       PERIODIC=.TRUE.
       CALL set_periodic_data_DKDKx(rvalue1,PERIODIC)

    END SUBROUTINE

! am: fonction supplémentaires

    ! fonction qui renvoie le nombre de DISKx adjacent à un DISKx donné
    ! précondition:
    !   - icdtac: numéro du DISKx dont on cherche le nombre de voisins
    ! postcondition:
    !   - valeur de retour: le nombre de voisins de icdtac 
    function GetAdjszDKDKx(icdtac)

       implicit none

       ! variable d'entrée
       integer :: icdtac ! numéro du DISKx dont on cherche le nombre de voisins
       
       ! valeur de retour
       integer :: GetAdjszDKDKx ! nombre de voisins de icdtac

       GetAdjszDKDKx = get_adjsz_DKDKx(icdtac)

    end function GetAdjszDKDKx

!------------------------------------------------------------------------ 
    SUBROUTINE PutlocalreacDKDKx(icdtac,iadj,rn,rt)

      IMPLICIT NONE
      INTEGER      :: icdtac,iadj
      REAL(kind=8) :: rn,rt

     call put_local_reac_DKDKx(icdtac,iadj,rn,rt)


    END SUBROUTINE PutlocalreacDKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
   SUBROUTINE PutlocalvlocDKDKx(icdtac,iadj,vn,vt)

     IMPLICIT NONE
     INTEGER      :: icdtac,iadj
     REAL(kind=8) :: vn,vt
   
     call put_local_vloc_DKDKx(icdtac,iadj,vn,vt)

   END SUBROUTINE PutlocalvlocDKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
   FUNCTION GetlocalreacDKDKx(icdtac,iadj)

     IMPLICIT NONE
     REAL(kind=8), dimension(2) :: GetlocalreacDKDKx
     INTEGER      :: icdtac,iadj
     REAL(kind=8) :: rn,rt
     CHARACTER(len=5) :: status   

     call get_local_reac_DKDKx(icdtac,iadj,status,rn,rt)

     GetlocalreacDKDKx(1)=rn
     GetlocalreacDKDKx(2)=rt

   END FUNCTION
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
   FUNCTION GetlocalvlocDKDKx(icdtac,iadj)

     IMPLICIT NONE
     REAL(kind=8), dimension(2) :: GetlocalvlocDKDKx
     INTEGER      :: icdtac,iadj
     REAL(kind=8) :: vn,vt
   
     call get_local_vloc_DKDKx(icdtac,iadj,vn,vt)

     GetlocalvlocDKDKx(1)=vn
     GetlocalvlocDKDKx(2)=vt

   END FUNCTION
!------------------------------------------------------------------------
   FUNCTION This2verltDKDKx(icdan)

     IMPLICIT NONE

     INTEGER, dimension(2) :: This2verltDKDKx
     INTEGER :: icdan,icdtac,iadj
   

     call this2verlt_DKDKx(icdan,icdtac,iadj)

     This2verltDKDKx(1)=icdtac
     This2verltDKDKx(2)=iadj


   END FUNCTION
!------------------------------------------------------------------------
   FUNCTION GetnbDKDKx(ghost)
  
     INTEGER,OPTIONAL :: ghost
     INTEGER :: GetnbDKDKx
    
     GetnbDKDKx = get_nb_DKDKx()

   
  END FUNCTION
!------------------------------------------------------------------------
  SUBROUTINE SelectProxTactorssansmiseajourcoor
    IMPLICIT NONE
    integer :: nb_DKDKx

    !CALL coor_prediction_DKDKx

    CALL creation_tab_visu_DKDKx

    CALL compute_contact_DKDKx

  END SUBROUTINE

!------------------------------------------------------------------------
  SUBROUTINE InterfaceCreationTabVisu(nb_diskx,x_min,x_max,y_min,y_max,diskx2sdm,sdm1,sdm2,diskx2inter)

    implicit none
    integer :: nb_diskx, sdm1, sdm2
    real(kind=8) :: x_min,x_max,y_min,y_max
    integer, dimension(nb_diskx) :: diskx2sdm, diskx2inter    

    call interface_creation_tab_visu_DKDKx(x_min,x_max,y_min,y_max,diskx2sdm,sdm1,sdm2,diskx2inter)

  END SUBROUTINE
!-------------------------------------------------------------------------
 SUBROUTINE GetSizeG2g(sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2)
 
    !di & fd on travaille avec un seul type de primitive des diskx

   IMPLICIT NONE
   
   ! on remonte toutes les tailles
   integer,intent(out) :: sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2
   !f2py integer, intent(out) :: sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2

   call get_size_g2g_DKDKx(sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2)
 
 END SUBROUTINE
! 
 SUBROUTINE GetG2g(g2g,idx_rbdy2,adj_rbdy2,nb_adj_rbdy2, &
                   sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2)

    !di & fd on travaille avec un seul type de primitive des diskx

   IMPLICIT NONE
   ! on donne toutes les tailles
   integer :: sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2
   !stockage de la matrice
   real(kind=8),dimension(sz_g2g),intent(out) :: g2g
   ! index dans le tableau de stockage
   integer,dimension(sz_idx_rbdy2),intent(out) :: idx_rbdy2
   ! liste des corps adjacents
   integer,dimension(sz_adj_rbdy2),intent(out) :: adj_rbdy2 
   ! nb de corps adjacents 
   integer,dimension(sz_nb_adj_rbdy2),intent(out) :: nb_adj_rbdy2

   !f2py real(kind=8), dimension(sz_g2g), intent(out) :: g2g
   !f2py real(kind=8), dimension(sz_idx_rbdy2), intent(out) :: idx_rbdy2
   !f2py real(kind=8), dimension(sz_adj_rbdy2), intent(out) :: adj_rbdy2
   !f2py real(kind=8), dimension(sz_nb_adj_rbdy2), intent(out) :: nb_adj_rbdy2

   call get_g2g_DKDKx(g2g,idx_rbdy2,adj_rbdy2,nb_adj_rbdy2,&
                      sz_g2g,sz_idx_rbdy2,sz_adj_rbdy2,sz_nb_adj_rbdy2)

 END SUBROUTINE

END MODULE wrap_DKDKx
