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
PROGRAM JMNR
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     
! LMGC90 
!
! LMGC stands for 
!
! Lumped Moving Granular Contactors
!
! Logiciel de Mecanique Gerant le Contact 
!
! It stands also for 
!
! Laboratoire de Mecanique et Genie Civil
!
! Laboratoire de Mecanique et Genie Civil is a laboratory located 
! in University of Montpellier II, in South of France, where the software was 
! developped from 1984 to nowadays. It follows the pioneering ideas of NSCD,
! Non Smooth Contact Dynamics, introduced by J.J. Moreau. It became a Fortran77 organized 
! software around 1990. The main author, Michel JEAN, has moved in 1997 at LMA, Laboratoire 
! de Mecanique et d'Acoustique, in Marseille, a CNRS owned unit, within a research team
! of ESM2, Ecole Superieure de Mecanique de Marseille. Michel JEAN and students of both 
! laboratories have contributed to the development of LMGC softwares.
!  
! The Fortran77 software has been remodelled in 2000, using Fortran90, becoming LMGC90.
! A main actor in this remodelling is Frederic Dubois, formerly engineer at LMA lab
! and now at LMGC lab.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! module CHIC
! CHIC is a macrolangage allowing a modular organization of the software. 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  USE CHIC

  USE wrap_overall
  USE utilities

  USE wrap_bulk_behaviour
  USE wrap_tact_behaviour
  USE wrap_models
  
  USE wrap_RBDY3
  USE wrap_SPHER
  USE wrap_PLANx
  USE wrap_DNLYC
  USE wrap_CYLND
  USE wrap_POLYR
  USE wrap_PT3Dx
  
  USE wrap_SPSPx
  USE wrap_SPPLx	
  USE wrap_SPCDx
  USE wrap_SPDCx
  USE wrap_PRPRx
  USE wrap_PRPLx
  USE wrap_PTPT3
  USE wrap_CDCDx
  USE wrap_CDPLx

  USE wrap_MAILx                           
  USE wrap_mecaMAILx
  USE wrap_CSxxx
  USE wrap_ASpxx
  USE wrap_CSPRx
  USE wrap_CSASp

  USE wrap_NLGS_3D
  USE wrap_CPG_3D
  USE wrap_mp_solver_3D

  USE wrap_postpro_3D	
  USE wrap_post3D
  USE wrap_DISPLAY_3D
  
  IMPLICIT NONE

  !
  CHARACTER(len=8)  :: DATE
  CHARACTER(len=10) :: TIME
  
  REAL(kind=8)      :: time0,time_in
  INTEGER           :: Nstep_last=0
  
  CHARACTER(len=18) ::  in_chic_command = 'DATBOX/COMMAND.DAT'        ! command file;
  CHARACTER(len=18) :: out_chic_command = 'OUTBOX/COMMAND.OUT'        ! rewritten displayed file;
  
  CALL cpu_time(time0)
  CALL DATE_AND_TIME(DATE=DATE,TIME=TIME)
  
  PARZZZ = in_chic_command
  DPLZZZ = out_chic_command
  !
  CALL LOGMESCHIC('===================================================')
  CALL LOGMESCHIC('    LMGC90 F.Dubois & M.Jean (c) 2001 CNRS-UMII    ')
  CALL LOGMESCHIC('                                                   ')
  CALL LOGMESCHIC('         version 2.0 F.Dubois & M. Renouf          ')
  CALL LOGMESCHIC('              -------------------                  ')
  CALL LOGMESCHIC(' le : '//DATE(7:8)//'/'//DATE(5:6)//'/'//DATE(1:4))
  CALL LOGMESCHIC(' a  : '//TIME(1:2)//'h'//DATE(3:4)//'mn'//DATE(5:6)//'s')
  CALL LOGMESCHIC('===================================================')
  
  CALL CHIC_comp
  CALL CHIC_ini
  
  ! Scanning file COMMAND.DAT 
  ! IETZZZ     is an indicator of activity of a wanted command
  ! IETZZZ = 0 scanning goes on
  ! IETZZZ = 1 the command has been found and active, scanning stops
  
  DO
     
     NZZZ=NZZZ+1
     IETZZZ=0
     IF (NZZZ.GT.NTZZZ) EXIT
     CHZZZ=CHLZZZ(NZZZ)
     
     CALL CHIC_comd
     IF (IETZZZ == 1) CYCLE
     
     CALL chic_command_overall  
     CALL chic_command_nlgs_3D
     CALL chic_command_cpg_3D
     
     IF (IETZZZ == 1) CYCLE
     
     ! bulk and contact behaviour laws commands
     CALL chic_command_bulk_behav
     CALL chic_command_models
     CALL chic_command_tact_behav

     CALL chic_command_RBDY3
     CALL chic_command_PLANx
     CALL chic_command_SPHER
     CALL chic_command_DNLYC
     CALL chic_command_CYLND
     CALL chic_command_POLYR

     CALL chic_command_PT3Dx

     CALL chic_command_SPSPx
     CALL chic_command_SPPLx
     CALL chic_command_SPCDx
     CALL chic_command_SPDCx
     CALL chic_command_PRPLx
     CALL chic_command_PRPRx
     CALL chic_command_PTPT3
     CALL chic_command_CDCDx
     CALL chic_command_CDPLx
!
     CALL chic_command_MAILx
     CALL more_chic_command_MAILx
     CALL chic_command_mecaMAILx
     CALL more_chic_command_mecaMAILx
!
     CALL chic_command_CSxxx
!     CALL more_chic_command_CSxxx
          !
     CALL chic_command_ASpxx
!     CALL more_chic_command_ASpxx
          !

     CALL chic_command_CSPRx
     CALL chic_command_CSASp

     CALL chic_command_post3D
     CALL chic_command_display_3D
     CALL chic_command_postpro_3D
     
     CALL chic_command_mp_solver_3D

     CALL chic_command_afterall
     IF (IETZZZ == 1) CYCLE
!     IF (MODULO(Nstep,2)==0) THEN
!        IF (Nstep_last/=Nstep) THEN
!           Nstep_last=Nstep
!           CALL interactiv_command
!        END IF
!     END IF
     !$   if (IETZZZ == 1) cycle
     
     CYCLE
     
  END DO
  
CONTAINS
  
  SUBROUTINE interactiv_command
    IMPLICIT NONE
    INTEGER :: err,err1,err2,i,err3
    REAL(kind=8)  :: time
    OPEN(unit=1000,file='stop',status='OLD',iostat=err)
    OPEN(unit=1001,file='save',status='OLD',iostat=err1)
    OPEN(unit=1002,file='display',status='OLD',iostat=err2)
    CLOSE(1000)
    CLOSE(1001)
    CLOSE(1002)
    
    IF (err==0) THEN
       CALL cpu_time(time_in)   
       PRINT*,' !-----------------------------------!'
       PRINT*,' !  CALCUL STOPPED                   !'
!       PRINT*,' !  NSTEP DONE : ',Nstep
       time=(time_in-time0)/3600.D0
       PRINT*,' ! TIME ELAPSED : ',time,'Hours'
       PRINT*,' ! SAVE OF FILE IS GOING TO BE DONE  !'
       
       DO i=1,5
          !123456789012345678901234567890
          IF (i==1) CHZZZ='WRITE LAST Vloc Rloc          '
          IF (i==2) CHZZZ='WRITE LAST DOF                '
          IF (i==3) CHZZZ='UPDATE POST 3D                ' 
          IF (i==4) CHZZZ='DISPLAY AFTER CALCUL          '
          IF (i==5) CHZZZ='POSTPRO AFTER COMPUTATION     '
          CALL chic_command_overall  
          CALL chic_command_bulk_behav
          CALL chic_command_models
          CALL chic_command_tact_behav
          
          CALL chic_command_RBDY3
          CALL chic_command_POLYR
          CALL chic_command_PLANx
          CALL chic_command_SPHER
          CALL chic_command_CYLND
          CALL chic_command_DNLYC
          CALL chic_command_PT3Dx

          CALL chic_command_SPSPx
          CALL chic_command_SPPLx
          CALL chic_command_SPCDx
          CALL chic_command_SPDCx
          CALL chic_command_PRPLx
          CALL chic_command_PRPRx
          CALL chic_command_PTPT3
          CALL chic_command_CDCDx
          CALL chic_command_CDPLx
          
          CALL chic_command_MAILx
          CALL more_chic_command_MAILx
          CALL chic_command_mecaMAILx
          CALL more_chic_command_mecaMAILx
          !
          CALL chic_command_CSxxx
!          CALL more_chic_command_CSxxx
          !
          CALL chic_command_ASpxx
!          CALL more_chic_command_ASpxx
          !

          CALL chic_command_post3D
          CALL chic_command_display_3D
          CALL chic_command_postpro_3D
       ENDDO
       PRINT*,' !-----------------------------------!'
       STOP
    ENDIF
    
    IF (err1==0) THEN
       CALL cpu_time(time_in)   
       PRINT*,' !-----------------------------------!'
       PRINT*,' !  SAVE OF FILE:                    !'
!       PRINT*,' !  NSTEP DONE : ',Nstep
       time=(time_in-time0)/3600.D0
       PRINT*,' ! TIME ELAPSED : ',time,'Hours'
       
       DO i=1,2
          !123456789012345678901234567890
          IF (i==1) CHZZZ='WRITE LAST Vloc Rloc          '
          IF (i==2) CHZZZ='WRITE LAST DOF                '
          CALL chic_command_overall  
          CALL chic_command_bulk_behav
          CALL chic_command_models
          CALL chic_command_tact_behav
          
          CALL chic_command_RBDY3
          CALL chic_command_POLYR
          CALL chic_command_PLANx
          CALL chic_command_SPHER
          CALL chic_command_DNLYC
          CALL chic_command_CYLND
          CALL chic_command_PT3Dx

          CALL chic_command_SPSPx
          CALL chic_command_SPPLx
          CALL chic_command_SPCDx
          CALL chic_command_SPDCx
          CALL chic_command_PRPLx
          CALL chic_command_PRPRx
          CALL chic_command_PTPT3
          CALL chic_command_CDCDx
          CALL chic_command_CDPLx

          
          CALL chic_command_post3D
          CALL chic_command_display_3D
          CALL chic_command_postpro_3D
       ENDDO
       PRINT*,' !-----------------------------------!'
    ENDIF
    IF (err2==0) THEN
       CALL cpu_time(time_in)   
       PRINT*,' !-----------------------------------!'
       PRINT*,' !  FILE FOR DISPLAY:                !'
!       PRINT*,' !  NSTEP DONE : ',Nstep
       time=(time_in-time0)/3600.D0
       PRINT*,' ! TIME ELAPSED : ',time,'Hours'
       
       DO i=1,2
          !123456789012345678901234567890
          IF (i==1) CHZZZ='UPDATE POST 3D                '  
          IF (i==2) CHZZZ='SAVE DISPLAY                  '
          CALL chic_command_overall  
          CALL chic_command_bulk_behav
          CALL chic_command_models
          CALL chic_command_tact_behav
          CALL chic_command_RBDY3
          CALL chic_command_POLYR
          CALL chic_command_PLANx
          CALL chic_command_SPHER
          CALL chic_command_DNLYC
          CALL chic_command_CYLND
          CALL chic_command_PT3Dx

          CALL chic_command_SPSPx
          CALL chic_command_SPPLx
          CALL chic_command_SPCDx
          CALL chic_command_SPDCx
          CALL chic_command_PRPLx
          CALL chic_command_PRPRx
          CALL chic_command_PTPT3
          CALL chic_command_CDCDx
          CALL chic_command_CDPLx

          
          CALL chic_command_post3D
          CALL chic_command_display_3D
          CALL chic_command_postpro_3D
       ENDDO
       PRINT*,' !-----------------------------------!'
    ENDIF
    
  END SUBROUTINE interactiv_command
END PROGRAM JMNR


