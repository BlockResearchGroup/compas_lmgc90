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
  
  !!****h* LMGC90.CHIC/NLGS
  !! NAME
  !!  module WRAP_NLGS
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/NLGS
  !!****

  USE CHIC

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
       Nullify_EntityList_nlgs,&
       init_cohe_nlgs

  PRIVATE

  PUBLIC chic_command_nlgs,more_chic_command_nlgs
  
CONTAINS
  
!!!--------------------------------------------------------
  SUBROUTINE chic_command_nlgs

    IMPLICIT NONE
    INTEGER,PARAMETER :: i_iter=1,i_check=2,i_post =3
    INTEGER           :: iconv,nb_iter_check,ik,ib,nb_block_iter,iter
    CHARACTER(len=5)  :: checktype
    CHARACTER(len=90) :: cout
    CHARACTER(len=30) :: Wstorage
    REAL(kind=8)      :: RELAX,tol
    LOGICAL           :: SDLactif

    IF (INDEX(CHZZZ,'EX ITER NLGS')==1) THEN
       !!****e* NLGS/EX ITER NLGS
       !! NAME
       !!   EX ITER NLGS
       !! PURPOSE
       !!  Execute one NLGS iteration over the contact loop
       !! USES
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL solve_nlgs(i_iter)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'AFTER ITER CHECK')==1) THEN
       !!****e* NLGS/AFTER ITER CHECK
       !! NAME
       !!  AFTER ITER CHECK
       !! PURPOSE
       !!  Control NLGS convergence
       !! USES
       !!  LMGC90.CORE/NLGS/prep_check_nlgs
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!  LMGC90.CORE/NLGS/comp_check_nlgs
       !!  LMGC90.CORE/NLGS/write_norm_check_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       IFLZZZ(NZZZ,1) = 0
       CALL prep_check_nlgs(iconv)
       IF (iconv == 0 ) RETURN
       CALL solve_nlgs(i_check)
       CALL comp_check_nlgs(iconv)
       CALL write_norm_check_nlgs(2)
       IFLZZZ(NZZZ,1)=iconv
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SCRAMBLE')== 1) THEN
       !!****e* NLGS/SCRAMBLE
       !! NAME
       !!  SCRAMBLE
       !! PURPOSE
       !!  random renumbering of the contact list
       !! USES
       !!  LMGC90.CORE/NLGS/scramble_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL scramble_nlgs       
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'quickSCRAMBLE')== 1) THEN
       !!****e* NLGS/quickSCRAMBLE
       !! NAME
       !!  quickSCRAMBLE
       !! PURPOSE
       !!  random renumbering of the contact list
       !! USES
       !!  LMGC90.CORE/NLGS/quick_scramble_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL quick_scramble_nlgs       
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'REVERSE')== 1) THEN
       !!****e* NLGS/REVERSE
       !! NAME
       !!  REVERSE
       !! PURPOSE
       !!  reverse the numbering of the contact list
       !! USES
       !!  LMGC90.CORE/NLGS/reverse_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL reverse_nlgs       
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'BIMODAL LIST')== 1) THEN
       CALL LOGCHIC('NLGS')
       !!****e* NLGS/BIMODAL LIST
       !! NAME
       !!  BIMODAL LIST
       !! PURPOSE
       !!  renumbering the contact list using the definition of 
       !!  weak and strong network in granular assemblies
       !! USES
       !!  LMGC90.CORE/NLGS/bimodal_list_nlgs
       !!****
       CALL bimodal_list_nlgs       
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY AFTER ITER CHECK')==1) THEN
       !!****e* NLGS/DISPLAY AFTER ITER CHECK
       !! NAME 
       !!  DISPLAY AFTER ITER CHECK
       !! PURPOSE
       !!  display NLGS convergence results
       !! USES
       !!  LMGC90.CORE/NLGS/display_check_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL display_check_nlgs
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SCALE NLGS Rloc')==1) THEN
       !!****e* NLGS/SCALE NLGS Rloc
       !! NAME
       !!  SCALE NLGS Rloc
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1
       !! USES
       !!  LMGC90.CORE/NLGS/scale_rloc_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL scale_rloc_nlgs
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'Rnod = [H] Rloc')==1) THEN
       !!****e* NLGS/Rnod = H Rloc
       !! NAME
       !!  Rnod = [H] Rloc
       !! PURPOSE
       !!  mapping from local contact forces to global ones 
       !! USES
       !!  LMGC90.CORE/NLGS/RnodHRloc_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL RnodHRloc_nlgs
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'EX POST NLGS')==1) THEN
       !!****e* NLGS/EX POST NLGS
       !! NAME
       !!  EX POST NLGS
       !! PURPOSE
       !!  run a jacobi iteration with the solution obtain
       !!  with the NLGS algorithm
       !! USES
       !!  LMGC90.CORE/NLGS/RnodHRloc_nlgs
       !!  LMGC90.CORE/NLGS/solve_nlgs
       !!  LMGC90.CORE/NLGS/Nullify_EntityList_nlgs
       !!  LMGC90.CORE/NLGS/write_norm_check_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL RnodHRloc_nlgs
       CALL solve_nlgs(i_post)
       CALL Nullify_EntityList_nlgs
       CALL write_norm_check_nlgs(3)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE TACT BEHAV')==1) THEN
       !!****e* NLGS/UPDATE TACT BEHAV
       !! NAME
       !!  UPDATE TACT BEHAV
       !! PURPOSE
       !!  update internal parameters of contact laws for each contact
       !! USES
       !!  LMGC90.CORE/NLGS/update_tact_behav_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL update_tact_behav_nlgs
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT COHESIVE BEHAV')==1) THEN
       !!****e* NLGS/INIT COHESIVE BEHAV
       !! NAME
       !!  INIT COHESIVE BEHAV
       !! PURPOSE
       !!  update internal parameters of contact laws for each contact
       !! USES
       !!  LMGC90.CORE/NLGS/init_cohe_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL init_cohe_nlgs
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'NLGS CHECK TYPE')==1) THEN
       !!****e* NLGS/NLGS CHECK TYPE
       !! NAME
       !!  NLGS CHECK TYPE
       !! SYNOPSIS
       !!  NLGS CHECK TYPE
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
       CALL LOGCHIC('NLGS')
       READ(CHLZZZ(NZZZ+1),'(A5)') checktype
!        IF (       checktype /= 'QuaN ' .AND. checktype /= 'Quad '  & 
!             .AND. checktype /= 'Maxm ' .AND. checktype /= 'QM/16') THEN
!           WRITE(cout,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ)
!           CALL LOGMESCHIC(cout)
!           CALL LOGMESCHIC(' WARNING, NEW from 05.12.16.')
!           CALL LOGMESCHIC(' You must precise the type of check test')
!           CALL LOGMESCHIC(' Quad  : quadratic norm (faulty contacts are ')
!           CALL LOGMESCHIC('         redeemed by accurate contacts; laxist norm),')
!           CALL LOGMESCHIC(' Maxm  : maximum norm (faulty contacts must')
!           CALL LOGMESCHIC('         comply; severe norm),')
!           CALL LOGMESCHIC(' QM/16 : maximum of Quad and Maxm/16 norms')
!           CALL LOGMESCHIC('         (a compromise). For large dense collections')
!           CALL LOGMESCHIC('         Quad ranges usually around 1/16 Maxm.')
!           CALL LOGMESCHIC(' Syntax:')
!           CALL LOGMESCHIC('       NLGS CHECK TYPE')
!           CALL LOGMESCHIC('       Quad 0.1666D-03')
!           CALL LOGMESCHIC(' or')
!           CALL LOGMESCHIC('       NLGS CHECK TYPE')
!           CALL LOGMESCHIC('       Maxm 0.2750D-02') 
!           CALL LOGMESCHIC(' or')
!           CALL LOGMESCHIC('       NLGS CHECK TYPE')
!           CALL LOGMESCHIC('       QM/16 0.1666D-03')
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' where Quad,Maxm,QM/16 are keywords for the check test,')
!           CALL LOGMESCHIC(' and the following real number is the tolerance value.')
!           STOP
!        END IF
       READ(CHLZZZ(NZZZ+1)(6:30),*) tol
       READ(CHLZZZ(NZZZ+2),*) RELAX

       CALL set_nlgs_parameter(checktype,tol,RELAX)

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'EX PREP NLGS')==1) THEN
       !!****e* NLGS/EX PREP NLGS
       !! NAME
       !!  EX PREP NLGS
       !! SYNOPSIS
       !!  EX PREP NLGS
       !!  [storage]
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
       CALL LOGCHIC('NLGS')
       READ(CHLZZZ(NZZZ+1),'(A30)') Wstorage
       
       IF (KHOZZZ == 1) THEN
          WRITE(cout,'(A14,1X,A30)') ' @ Wstorage = ',Wstorage
          CALL LOGMESCHIC(cout)
!           IF ( Wstorage /= 'Exchange_Local_Global         ' .AND. & 
!                Wstorage /= 'Stored_Delassus_Loops         ') THEN
!              WRITE(cout,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ)
!              CALL LOGMESCHIC(cout)
!              !             123456789012345678901234567890123456789012345678901234567890123456789012
!              CALL LOGMESCHIC(' WARNING, new from 03.05.08.')
!              CALL LOGMESCHIC(' You must mention the type of storage of the Delassus matrix,')
!              CALL LOGMESCHIC(' W = H* p* invM p H ')
!              CALL LOGMESCHIC(' 1)  Exchange_Local_Global (the standard case)')     
!              CALL LOGMESCHIC('     only the diagonal blocks are computed and')
!              CALL LOGMESCHIC('     stored, injj, invM, prjj, subroutines are')
!              CALL LOGMESCHIC('     called to operate H* p* invM p H .')
!              CALL LOGMESCHIC(' 2)  Stored_Delassus_Loops (faster but memory expensive)')
!              CALL LOGMESCHIC('     the complete Delassus matrix is computed') 
!              CALL LOGMESCHIC('     and stored.')
!              CALL LOGMESCHIC(' Syntax:')
!              CALL LOGMESCHIC('     EX PREP NLGS                  :')
!              CALL LOGMESCHIC('     Exchange_Local_Global         :')
!              CALL LOGMESCHIC(' or')
!              CALL LOGMESCHIC('     EX PREP NLGS                  :')
!              CALL LOGMESCHIC('     Stored_Delassus_Loops         :')   
!              STOP
!           END IF
       END IF
       SDLactif = .FALSE.
       IF(Wstorage == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
       CALL prep_nlgs(SDLactif)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY RlocN SUM')==1) THEN
       !!****e* NLGS/DISPLAY RlocN SUM
       !! NAME
       !!  DISPLAY RlocN SUM
       !! PURPOSE 
       !!  display the sum of normal contact forces
       !! USES
       !!  LMGC90.CORE/NLGS/display_rlocn_sum_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL display_rlocn_sum_nlgs
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'NLGS NORM CHECK')==1) THEN
       !!****e* NLGS/NLGS NORM CHECK
       !! NAME
       !!  NLGS NORM CHECK
       !! PURPOSE
       !!  Active one step norm evolution
       !! USES
       !!  LMGC90.CORE/NLGS/write_norm_check_nlgs
       !!****
       CALL LOGCHIC('NLGS')
       CALL write_norm_check_nlgs(1)
       IETZZZ = 1
       RETURN      
    END IF

!!! MACRO COMMAND -----------------------------------------

    IF (INDEX(CHZZZ,'EX NLGS SOLVER')==1) THEN
       !!****e* NLGS/EX NLGS SOLVER
       !! NAME 
       !!   EX NLGS SOLVER
       !! SYNOPSIS
       !!  EX NLGS SOLVER
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
       CALL LOGCHIC('NLGS')
!        IF (KHOZZZ == 1) THEN
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' WARNING: NEW FROM SEPT. 2006')
!           CALL LOGMESCHIC(' You use a new MACRO COMMAND ')
!           CALL LOGMESCHIC(' to solve the contact problem')
!           CALL LOGMESCHIC(' with the NLGS algorithm.    ')
!           CALL LOGMESCHIC(' Parameters are:')
!           CALL LOGMESCHIC(' [Storage]')
!           CALL LOGMESCHIC('     Exchange_Local_Global')
!           CALL LOGMESCHIC('     Stored_Delassus_Loops')
!           CALL LOGMESCHIC(' [NormID] [NormValue]')
!           CALL LOGMESCHIC('      Quad  x.xxxxD-xx')
!           CALL LOGMESCHIC('      Maxm  x.xxxxD-xx')
!           CALL LOGMESCHIC('      QM/16 x.xxxxD-xx')
!           CALL LOGMESCHIC(' [Relaxation]')
!           CALL LOGMESCHIC('  Rxxx')
!           CALL LOGMESCHIC(' [Iteration number for check ]')
!           CALL LOGMESCHIC('  I1xxx')
!           CALL LOGMESCHIC(' [Block iteration number]')
!           CALL LOGMESCHIC('  I2xxx')  
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' ******************************')     
!        END IF
       READ(CHLZZZ(NZZZ+1),'(A30)') Wstorage
       READ(CHLZZZ(NZZZ+2),'(A5)') checktype
       READ(CHLZZZ(NZZZ+2)(6:30),*) tol
       READ(CHLZZZ(NZZZ+3),*) RELAX
       READ(CHLZZZ(NZZZ+4),*) nb_iter_check
       READ(CHLZZZ(NZZZ+5),*) nb_block_iter
       
       CALL set_nlgs_parameter(checktype,tol,RELAX)
       SDLactif = .FALSE.
       IF(Wstorage == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
       CALL prep_nlgs(SDLactif)
       iter = 0
       DO ib=1,nb_block_iter
          DO ik=1,nb_iter_check
             iter = iter + 1
             CALL solve_nlgs(i_iter)
          END DO
          IFLZZZ(NZZZ,1) = 0
          CALL prep_check_nlgs(iconv)
          IF (iconv == 0 ) RETURN
          CALL solve_nlgs(i_check)
          CALL comp_check_nlgs(iconv)
          IF (iconv == 0) EXIT
       END DO
       CALL RnodHRloc_nlgs
       CALL solve_nlgs(i_post)
       CALL Nullify_EntityList_nlgs
       IF(KHOZZZ==1)THEN
          WRITE(cout,'(A20,I8)') '  @ NLGS ITERATION: ',iter
          CALL LOGMESCHIC(cout)
       END IF
       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_nlgs
!!!--------------------------------------------------------

  include '../../more_src/kernel/wrap_nlgs.inc'

END MODULE wrap_nlgs
