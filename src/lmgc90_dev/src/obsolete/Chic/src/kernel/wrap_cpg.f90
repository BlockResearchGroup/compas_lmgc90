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
MODULE wrap_cpg

  !!****h* LMGC90.CHIC/CPG
  !! NAME
  !!  module wrap_cpg
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/CPG
  !!****

  USE CHIC

  USE cpg,ONLY:&
       set_cpg_parameter, &
       ex_iter_cpg, & 
       ex_check_cpg, &
       write_norm_check_cpg, &
       ex_post_cpg, &
       ex_prep_cpg, &
       set_diag_precond_cpg, &
       set_frictionless, &
       set_no_conjugaison, &
       scale_rloc_cpg, &
       Nullify_EntityList_cpg

CONTAINS
!!!-------------------------------------------------------
  SUBROUTINE chic_command_cpg

    IMPLICIT NONE

    INTEGER          :: iconv,nb_iter_check,ik,ib,nb_block_iter
    CHARACTER(len=5) :: checktype
    CHARACTER(len=35) :: cout
    REAL(kind=8)     :: tol

    IF (INDEX(CHZZZ,'EX ITER CPG')==1) THEN
       !!****e* CPG/EX ITER CPG
       !! NAME
       !!   EX ITER CPG
       !! PURPOSE
       !!  Execute one CPG iteration over the contact loop
       !! USES
       !!  LMGC90.CORE/CPG/ex_iter_cpg
       !!****
       CALL LOGCHIC('CPG')
       CALL ex_iter_cpg
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'AFTER ITER CPG CHECK')==1) THEN
       !!****e* CPG/AFTER ITER CPG CHECK
       !! NAME
       !!  AFTER ITER CPG CHECK
       !! PURPOSE
       !!  Control CPG convergence
       !! USES
       !!  LMGC90.CORE/CPG/ex_check_cpg
       !!  LMGC90.CORE/CPG/write_norm_check_cpg
       !!****
       CALL LOGCHIC('CPG')
       CALL ex_check_cpg(iconv)
       CALL write_norm_check_cpg(2)
       IFLZZZ(NZZZ,1) = iconv
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'EX POST CPG')==1) THEN
       !!****e* CPG/EX POST CPG
       !! NAME
       !!  EX POST CPG
       !! PURPOSE
       !!  transfert local solution
       !! USES
       !!  LMGC90.CORE/CPG/ex_post_cpg
       !!  LMGC90.CORE/CPG/Nullify_EntityList_cpg
       !!  LMGC90.CORE/CPG/write_norm_check_cpg
       !!****
       CALL LOGCHIC('CPG')
       CALL ex_post_cpg
       CALL Nullify_EntityList_cpg
       CALL write_norm_check_cpg(3)
       IETZZZ = 1
       RETURN
    END IF

    IF ( INDEX(CHZZZ,'EX PREP CPG') == 1 ) THEN
       !!****e* CPG/EX PREP CPG
       !! NAME
       !!  EX PREP CPG
       !! PURPOSE
       !!  prepare the matrix and the RHS of the contact problem
       !! USES
       !!  LMGC90.CORE/CPG/ex_prep_cpg
       !!****
       CALL LOGCHIC('CPG')
       CALL ex_prep_cpg
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SCALE CPG Rloc')==1) THEN
       !!****e* CPG/SCALE CPG Rloc
       !! NAME
       !!  SCALE CPG Rloc
       !! PURPOSE
       !!  scale all local contact forces of a factor equal to
       !!  0.9 < f < 1.1
       !! USES
       !!  LMGC90.CORE/CPG/scale_rloc_nlgs
       !!****
       CALL LOGCHIC('CPG')
       CALL scale_rloc_cpg
       IETZZZ = 1
       RETURN      
    END IF

    IF ( INDEX(CHZZZ,'USE DIAGONAL PRECOND') == 1 ) THEN
       !!****e* CPG/USE DIAGONAL PRECOND
       !! NAME
       !!  USE DIAGONAL PRECOND
       !! PURPOSE
       !!  active diagonal preconditioner
       !! USES
       !!  LMGC90.CORE/CPG/set_diag_precond_cpg
       !!****
       CALL LOGCHIC('CPG')
       CALL set_diag_precond_cpg
       IETZZZ = 1
       RETURN      
    END IF

    IF ( INDEX(CHZZZ,'FRICTIONLESS') == 1 ) THEN
       !!****e* CPG/FRICTIONLESS
       !! NAME
       !!  FRICTIONLESS
       !! PURPOSE
       !!  active frictionless solver
       !! USES
       !!  LMGC90.CORE/CPG/set_frictionless
       !!****
       CALL LOGCHIC('CPG')
       CALL set_frictionless
       IETZZZ = 1
       RETURN      
    END IF

    IF ( INDEX(CHZZZ,'NO CONJUGAISON') == 1 ) THEN
       !!****e* CPG/NO CONJUGAISON
       !! NAME
       !!  NO CONJUGAISON
       !! PURPOSE
       !!  desactive conjugaison
       !! USES
       !!  LMGC90.CORE/CPG/set_no_conjugaison
       !!****
       CALL LOGCHIC('CPG')
       CALL set_no_conjugaison
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'CPG CHECK TYPE')==1) THEN
       !!****e* CPG/CPG CHECK TYPE
       !! NAME
       !!  CPG CHECK TYPE
       !! SYNOPSIS
       !!  CPG CHECK TYPE
       !!  [checktype] [tolerance]
       !! INPUTS
       !!  [checktype] : type of convergence check
       !!  [tolerance] : norm tolerance
       !! PURPOSE
       !!  define numerical parameters of the CPG algorithm
       !!  convergence check keywords:
       !!  Quad  : quadratic norm (faulty contacts are redeemed by accurate
       !!          contacts; laxist norm)
       !!  Maxm  : maximum norm (faulty contacts must comply; severe norm)
       !!  where Quad,Maxm are keywords for the check test, and the 
       !!  following real number is the tolerance value.
       !! USES
       !!  LMGC90.CORE/CPG/set_cpg_parameter
       !!****
       CALL LOGCHIC('CPG')
       READ(CHLZZZ(NZZZ+1),'(A5)') checktype
!        IF (       checktype /= 'QuaN ' .AND. checktype /= 'Quad '  & 
!             .AND. checktype /= 'Stag ' .AND. checktype /= 'Maxm ') THEN
!           WRITE(cout,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ)
!           CALL LOGMESCHIC(cout)
!           CALL LOGMESCHIC(' WARNING, NEW from 06.01.19')
!           CALL LOGMESCHIC(' You must precise the type of check test')
!           CALL LOGMESCHIC(' Quad  : quadratic norm (faulty contacts are ')
!           CALL LOGMESCHIC('         redeemed by accurate contacts; laxist norm),')
!           CALL LOGMESCHIC(' Maxm  : maximum norm (faulty contacts must')
!           CALL LOGMESCHIC('         comply; severe norm),')
!           CALL LOGMESCHIC(' Syntax:')
!           CALL LOGMESCHIC('       NSCD_GS CHECK TYPE')
!           CALL LOGMESCHIC('       Quad 0.1666D-03')
!           CALL LOGMESCHIC(' or')
!           CALL LOGMESCHIC('       NSCD_GS CHECK TYPE')
!           CALL LOGMESCHIC('       Maxm 0.2750D-02') 
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' where Quad,Maxm,Stag are keywords for the check test,')
!           CALL LOGMESCHIC(' and the following real number is the tolerance value.')
!           STOP
!        END IF

       READ(CHLZZZ(NZZZ+1)(6:30),*) tol

       CALL set_cpg_parameter(checktype,tol)

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CPG NORM CHECK')==1) THEN
       !!****e* CPG/CPG NORM CHECK
       !! NAME
       !!  CPG NORM CHECK
       !! PURPOSE
       !!  Active one step norm evolution
       !! USES
       !!  LMGC90.CORE/CPG/write_norm_check_cpg
       !!****
       CALL LOGCHIC('CPG')
       CALL write_norm_check_cpg(1)
       IETZZZ = 1
       RETURN      
    END IF

!!! MACRO COMMAND -----------------------------------------

    IF (INDEX(CHZZZ,'EX CPG SOLVER')==1) THEN
       !!****e* CPG/EX CPG SOLVER
       !! NAME
       !!  EX CPG SOLVER
       !! SYNOPSIS
       !!  EX CPG SOLVER
       !!  [checktype] [tolerance]
       !!  [nb_iter_chek]
       !!  [nb_block_iter]
       !! INPUTS
       !!  [checktype]     : convergence test keyword
       !!  [tolerance]     : tolerance value
       !!  [nb_iter_chek]  : number of iteration between convergence test
       !!  [nb_block_iter] : number of block iterations
       !! PURPOSE
       !!  solve fully the local contact problem
       !! USES
       !!  LMGC90.CORE/CPG/set_cpg_parameter
       !!  LMGC90.CORE/CPG/ex_prep_cpg
       !!  LMGC90.CORE/CPG/ex_iter_cpg
       !!  LMGC90.CORE/CPG/ex_check_cpg
       !!  LMGC90.CORE/CPG/ex_post_cpg
       !!  LMGC90.CORE/CPG/Nullify_EntityList_cpg
       !!****
       CALL LOGCHIC('CPG')
!        IF (KHOZZZ == 1) THEN
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' WARNING: NEW FROM SEPT. 2006')
!           CALL LOGMESCHIC(' You use a new MACRO COMMAND ')
!           CALL LOGMESCHIC(' to solve the contact problem')
!           CALL LOGMESCHIC(' with the CPG algorithm.     ')
!           CALL LOGMESCHIC(' Parameters are:')
!           CALL LOGMESCHIC(' [NormID] [NormValue]')
!           CALL LOGMESCHIC('      Quad  x.xxxxD-xx')
!           CALL LOGMESCHIC(' [Iteration number for check ]')
!           CALL LOGMESCHIC('  I1xxx')
!           CALL LOGMESCHIC(' [Block iteration number]')
!           CALL LOGMESCHIC('  I2xxx')  
!           CALL LOGMESCHIC(' ')
!           CALL LOGMESCHIC(' ******************************')     
!        END IF
       READ(CHLZZZ(NZZZ+2),'(A5)') checktype
       READ(CHLZZZ(NZZZ+2)(6:30),*) tol
       READ(CHLZZZ(NZZZ+3),*) nb_iter_check
       READ(CHLZZZ(NZZZ+4),*) nb_block_iter
       
       CALL set_cpg_parameter(checktype,tol)
       CALL ex_prep_cpg
       DO ib=1,nb_block_iter
          DO ik=1,nb_iter_check
             CALL ex_iter_cpg
          END DO
          CALL ex_check_cpg(iconv)
          IFLZZZ(NZZZ,1) = iconv
          IF (iconv == 0) EXIT
       END DO
       CALL ex_post_cpg
       CALL Nullify_EntityList_cpg
       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_cpg
!!!-------------------------------------------------------

  include '../../more_src/kernel/wrap_cpg.inc'

END MODULE wrap_cpg
