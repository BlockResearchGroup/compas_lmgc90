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
MODULE wrap_nlgs
  
  !!****h* LMGC90.CHIPY/NLGS
  !! NAME
  !!  module wrap_nlgs
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/NLGS
  !!****


  USE nlgs,ONLY:&
       solve_nlgs, &
       comp_check_nlgs, &
       write_norm_check_nlgs, &
       scramble_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       bimodal_list_nlgs, &
       display_check_nlgs, &
       scale_rloc_nlgs, &
       RnodHRloc_nlgs, &
       display_rlocn_sum_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs, &
       get_error, get_conv

   PUBLIC

CONTAINS

    SUBROUTINE ExPrep(cvalue1)
      IMPLICIT NONE
      CHARACTER(len=30) :: cvalue1
      LOGICAL           :: SDLactif
       !!****e* NLGS/ExPrep
       !! NAME
       !!  ExPrep
       !! SYNOPSIS
       !!  EX PREP(character(len=30) [storage])
       !! INPUTS
       !!  [storage] : matrix storage
       !! PURPOSE
       !!  prepare the matrix and the RHS of the contact problem
       !!  in regards of the selected matrix storage:
       !!  - Exchange_Local_Global (the standard case)
       !!   only the diagonal blocks are computed and stored.
       !!  - Stored_Delassus_Loops (faster but memory expensive)
       !!   the complete Delassus matrix is computed.
       !! USES
       !!  LMGC90.CORE/NLGS/prep_nlgs
       !!****

       SDLactif = .FALSE.
       IF(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
       CALL prep_nlgs(SDLactif)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ExIter
      IMPLICIT NONE

       !!****e* NLGS/ExIter
       !! NAME
       !!   ExIter
       !! PURPOSE
       !!  Execute one NLGS iteration over the contact loop
       !! USES
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!****

       CALL solve_nlgs(1)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ExPost
      IMPLICIT NONE
       !!****e* NLGS/ExPost
       !! NAME
       !!  ExPost
       !! PURPOSE
       !!  run a jacobi iteration with the solution obtain
       !!  with the NLGS algorithm
       !! USES
       !!  LMGC90.CORE/NLGS/RnodHRloc_nlgs
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!  LMGC90.CORE/NLGS/Nullify_EntityList_nlgs
       !!  LMGC90.CORE/NLGS/write_norm_check_nlgs
       !!****

       CALL RnodHRloc_nlgs
       CALL solve_nlgs(3)
       CALL Nullify_EntityList_nlgs
       CALL write_norm_check_nlgs(3)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE AfterIterCheck(ivalue1)
      IMPLICIT NONE
      INTEGER :: ivalue1
       !!****e* NLGS/AfterIterCheck
       !! NAME
       !!  AfterIterCheck
       !! PURPOSE
       !!  Control NLGS convergence
       !! USES
       !!  LMGC90.CORE/NLGS/prep_check_nlgs
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!  LMGC90.CORE/NLGS/comp_check_nlgs
       !!  LMGC90.CORE/NLGS/write_norm_check_nlgs
       !!****

       ivalue1 = 0
       CALL prep_check_nlgs(ivalue1)
       IF (ivalue1 == 0 ) RETURN
       CALL solve_nlgs(2)
       CALL comp_check_nlgs(ivalue1)
       CALL write_norm_check_nlgs(2)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE DisplayAfterIterCheck
      IMPLICIT NONE
       !!****e* NLGS/DisplayAfterIterCheck
       !! NAME 
       !!  DisplayAfterIterCheck
       !! PURPOSE
       !!  display NLGS convergence results
       !! USES
       !!  LMGC90.CORE/NLGS/display_check_nlgs
       !!****

       CALL display_check_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE DisplayNorm
      IMPLICIT NONE
       !!****e* NLGS/DisplayNorm
       !! NAME
       !!  DisplayNorm
       !! PURPOSE
       !!  Active one step norm evolution
       !! USES
       !!  LMGC90.CORE/NLGS/write_norm_check_nlgs
       !!****

       CALL write_norm_check_nlgs(1)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE UpdateTactBehav
      IMPLICIT NONE
       !!****e* NLGS/UpdateTactBehav
       !! NAME
       !!  UpdateTactBehav
       !! PURPOSE
       !!  update internal parameters of contact laws for each contact
       !! USES
       !!  LMGC90.CORE/NLGS/update_tact_behav_nlgs
       !!****

       CALL update_tact_behav_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE SetCheckType(cvalue1,rvalue1,rvalue2)
      IMPLICIT NONE
      character(len=5) :: cvalue1
      real(kind=8) :: rvalue1,rvalue2
       !!****e* NLGS/SetCheckType
       !! NAME
       !!  SetCheckType
       !! SYNOPSIS
       !!  SetCheckType
       !!  [checktype] [tolerance]
       !!  [relax]       
       !! INPUTS
       !!  [checktype] : type of convergence check
       !!  [tolerance] : norm tolerance
       !!  [relax]     : relaxation factor
       !! PURPOSE
       !!  define numerical parameters of the NLGS algorithm
       !!  convergence check keywords:
       !!  Quad  : quadratic norm (faulty contacts are redeemed by accurate
       !!          contacts; laxist norm)
       !!  Maxm  : maximum norm (faulty contacts must comply; severe norm)
       !!  QM/16 : maximum of Quad and Maxm/16 norms (a compromise). For
       !!          large dense collections Quad ranges usually around 1/16 Maxm
       !!  where Quad,Maxm,QM/16 are keywords for the check test, and the 
       !!  following real number is the tolerance value.
       !! USES
       !!  LMGC90.CORE/NLGS/set_nlgs_parameter
       !!****

       CALL set_nlgs_parameter(cvalue1,rvalue1,rvalue2)

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ScrambleContactOrder
      IMPLICIT NONE
       !!****e* NLGS/ScrambleContactOrder
       !! NAME
       !!  ScrambleContactOrder
       !! PURPOSE
       !!  random renumbering of the contact list
       !! USES
       !!  LMGC90.CORE/NLGS/scramble_nlgs
       !!****

       CALL scramble_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE QuickScrambleContactOrder
      IMPLICIT NONE
       !!****e* NLGS/QuickScrambleContactOrder
       !! NAME
       !!  quickSCRAMBLE
       !! PURPOSE
       !!  random renumbering of the contact list
       !! USES
       !!  LMGC90.CORE/NLGS/quick_scramble_nlgs
       !!****

       CALL quick_scramble_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ReverseContactOrder
      IMPLICIT NONE
       !!****e* NLGS/ReverseContactOrder
       !! NAME
       !!  ReverseContactOrder
       !! PURPOSE
       !!  reverse the numbering of the contact list
       !! USES
       !!  LMGC90.CORE/NLGS/reverse_nlgs
       !!****

       CALL reverse_nlgs       

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE BimodalContactOrder
      IMPLICIT NONE
       !!****e* NLGS/BimodalContactOrder
       !! NAME
       !!  BimodalContactOrder
       !! PURPOSE
       !!  renumbering the contact list using the definition of 
       !!  weak and strong network in granular assemblies
       !! USES
       !!  LMGC90.CORE/NLGS/bimodal_list_nlgs
       !!****
       CALL bimodal_list_nlgs       
    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ScaleRloc
      IMPLICIT NONE
       !!****e* NLGS/ScaleRloc
       !! NAME
       !!  ScaleRloc
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1
       !! USES
       !!  LMGC90.CORE/NLGS/scale_rloc_nlgs
       !!****

       CALL scale_rloc_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE ComputeRnod
      IMPLICIT NONE
       !!****e* NLGS/ComputeRnod
       !! NAME
       !!  ComputeRnod
       !! PURPOSE
       !!  mapping from local contact forces to global ones 
       !! USES
       !!  LMGC90.CORE/NLGS/RnodHRloc_nlgs
       !!****

       CALL RnodHRloc_nlgs

    END SUBROUTINE
!!!--------------------------------------------------------
    SUBROUTINE DisplayRlocNSum
      IMPLICIT NONE
       !!****e* NLGS/DisplayRlocNSum
       !! NAME
       !!  DisplayRlocNSum
       !! PURPOSE 
       !!  display the sum of normal contact forces
       !! USES
       !!  LMGC90.CORE/NLGS/display_rlocn_sum_nlgs
       !!****

       CALL display_rlocn_sum_nlgs

    END SUBROUTINE
!!! MACRO COMMAND -----------------------------------------
    SUBROUTINE ExSolver(cvalue1,cvalue2,rvalue1,rvalue2,ivalue1,ivalue2)
      IMPLICIT NONE

      CHARACTER(len=30) :: cvalue1
      CHARACTER(len=5 ) :: cvalue2  
      REAL(kind=8)      :: rvalue1,rvalue2
      INTEGER           :: ivalue1,ivalue2,iconv,iter,ib,ik
      LOGICAL           :: SDLactif
   

       !!****e* NLGS/ExSolver
       !! NAME 
       !!   ExSolver
       !! SYNOPSIS
       !!   ExSolver
       !!  [Wstorage]
       !!  [checktype] [tolerance]
       !!  [relax]
       !!  [nb_iter_chek]
       !!  [nb_block_iter]
       !! INPUTS
       !!  [Wstorage]      : matrix storage (cf EX_PREP_NLGS)
       !!  [checktype]     : convergence test keyword
       !!  [tolerance]     : tolerance value
       !!  [relax]         : relaxation number
       !!  [nb_iter_chek]  : number of iteration between convergence test
       !!  [nb_block_iter] : number of block iterations
       !! PURPOSE
       !!  solve fully the local contact problem
       !! USES
       !!  LMGC90.CORE/NLGS/set_nlgs_parameter
       !!  LMGC90.CORE/NLGS/prep_nlgs
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!  LMGC90.CORE/NLGS/prep_check_nlgs
       !!  LMGC90.CORE/NLGS/comp_check_nlgs
       !!  LMGC90.CORE/NLGS/RnodHRloc_nlgs
       !!  LMGC90.CORE/NLGS/Nullify_EntityList_nlgs
       !!****

       CALL set_nlgs_parameter(cvalue2,rvalue1,rvalue2)

       SDLactif = .FALSE.
       IF(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
       CALL prep_nlgs(SDLactif)

       iter = 0
       DO ib=1,ivalue2
          DO ik=1,ivalue1
             iter = iter + 1
             CALL solve_nlgs(1)
          END DO
          iconv=0
          CALL prep_check_nlgs(iconv)
          IF (iconv == 0 ) RETURN
          CALL solve_nlgs(2)
          CALL comp_check_nlgs(iconv)
          IF (iconv == 0) EXIT
       END DO
       CALL RnodHRloc_nlgs
       CALL solve_nlgs(3)
       CALL Nullify_EntityList_nlgs

    END SUBROUTINE


   !> get all the mean/quad/etc quantities computed by nlgs to check error
   subroutine GetError(DSumDVDVRR,DNactif,DMeanWRR, &
                        Dtol,DSumDVDV,DQuadWR,DSumDVoR,DSumWRR) 
    
     implicit none

     REAL(kind=8) , intent(out) ::  DSumDVDVRR, DNactif, &
                                    DMeanWRR ,Dtol,DSumDVDV, &
                                    DQuadWR,DSumDVoR,DSumWRR

     CALL get_error(DSumDVDVRR,DNactif,DMeanWRR, &
                    Dtol,DSumDVDV,DQuadWR,DSumDVoR,DSumWRR)  

     !pour info:
     !
     !QuadWR   = DSQRT(SumWRWR/REAL(Nactif,8))
     !Dreac    = QuadWR*H  
     !QuadDV   = DSQRT(SumDVDV/REAL(Nactif,8))   / (QuadWR*tol) 
     !MaxmDV   = DSQRT(MaxDVDV)                  / (QuadWR*tol) 
     !       
     !MeanWRR  = SumWRR/REAL(Nactif,8)
     !QuadDVR  = DSQRT(SumDVDVRR/REAL(Nactif,8)) / (MeanWRR*tol)
     !MaxmDVR  = DSQRT(MaxDVDVRR)                / (MeanWRR*tol)            
     !MeanDVoR = SumDVoR                         / (SumWRR*tol)

   end subroutine GetError

   subroutine GetConv(sum_t,sum_n,dsum_t,dsum_n)

     real(kind=8) :: sum_t,sum_n,dsum_t,dsum_n

     call get_conv(sum_t,sum_n,dsum_t,dsum_n)

   end subroutine

END MODULE wrap_nlgs
