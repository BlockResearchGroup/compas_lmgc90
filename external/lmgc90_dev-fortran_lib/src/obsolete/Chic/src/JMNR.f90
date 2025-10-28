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

 PROGRAM LMGC90

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

 USE CHIC                             !see file mod_chic.f90
 USE wrap_overall                          !see file mod_overall.f90

 USE utilities
! use wrap_timer                            !take time data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! users moduli
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
! prepro
!

 USE wrap_PREPRO

!
! rigid bodies
!

 USE wrap_RBDY2
 USE wrap_DISKx
 USE wrap_xKSID
 USE wrap_JONCx
 USE wrap_DISPx
 USE wrap_xPSID
 USE wrap_POLYG
 USE wrap_PT2Dx

!
! rigid to rigid contactors
!

 USE wrap_DKDKx
 USE wrap_DKJCx
 USE wrap_DKKDx
 USE wrap_DKDPx
 USE wrap_DKPDx
 USE wrap_PLPLx
 USE wrap_PLJCx
 USE wrap_PTPT2
 USE wrap_DKPLx

!
! deformable bodies
!
 USE wrap_MAILx                           
 USE wrap_mecaMAILx
 USE wrap_therMAILx
 USE wrap_CLxxx
 USE wrap_ALpxx
 USE wrap_PT2DL
 USE wrap_DISKL
!
! defo to defo and defo 2 rigid contactors
!
 USE wrap_CLJCx
 USE wrap_CLALp
 USE wrap_P2P2L
 USE wrap_PLALp
 USE wrap_DKALp
 USE wrap_DPALp
 USE wrap_DKDKL
!
! shared modules
!

 USE wrap_bulk_behaviour,ONLY : chic_command_bulk_behav, &   !defining bulk behaviour and contacting laws, 
                           more_chic_command_bulk_behav ! see file mod_bulk_behaviour.f90   
 USE wrap_tact_behaviour,ONLY : chic_command_tact_behav, &   !defining tact behaviour and contacting laws,
                           more_chic_command_tact_behav ! see file mod_tact_behaviour.f90  
 USE wrap_models

!
! kernel solvers
!
 USE wrap_nlgs      ! Non Linear Gauss Seidel Algorithm
 USE wrap_cpg       ! Conjugate Projected Gradient Algorithm
 USE wrap_mp_solver ! Multi-Physics Discrete Element Resolution
!
! pospro
!
 USE wrap_postpro
 USE wrap_post2D

 IMPLICIT NONE

!
 CHARACTER(len=8)  :: DATE
 CHARACTER(len=10) :: TIME


!
! for command during calcul
!
 REAL(kind=8)      :: time0,time_in
 INTEGER           :: my_Nstep,Nstep_last=0

                                                  !123456789012345678
 CHARACTER(len=18) ::  in_chic_command         = 'DATBOX/COMMAND.DAT'        ! command file;
 CHARACTER(len=18) :: out_chic_command         = 'OUTBOX/COMMAND.OUT'        ! rewritten displayed file;



 CALL cpu_time(time0)


 CALL DATE_AND_TIME(DATE=DATE,TIME=TIME)

 PARZZZ = in_chic_command
 DPLZZZ = out_chic_command
!
 CALL LOGMES('===================================================')
 CALL LOGMES('LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII')
 CALL LOGMES('              -------------------                  ')
 CALL LOGMES('   le : '//DATE(7:8)//'/'//DATE(5:6)//'/'//DATE(1:4))
 CALL LOGMES('   a  : '//TIME(1:2)//'h'//DATE(3:4)//'mn'//DATE(5:6)//'s')
 CALL LOGMES('===================================================')

!
! Reading PARZZZ (./DATBOX/COMMAND.DAT) & compiling CHIC commands 
!

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
  
   CALL LMGC90_COMMANDS

  !
  ! a way to have some interaction with the code
  !  
  !  touch stop     -> it stops safely a job
  !  touch display  -> it writes some graphical files
  !  touch save     -> it write some output files
  !

   my_NSTEP = get_NSTEP()
   if (modulo(my_Nstep,10)==0) then
     if (Nstep_last /= my_Nstep) then
       Nstep_last = my_Nstep
       call check_interactive_command
     endif  
   endif    

 END DO 

 CONTAINS

 SUBROUTINE check_interactive_command
   IMPLICIT NONE
   LOGICAL :: err,err1,err2,err3
   INTEGER :: i
   REAL(kind=8)  :: time
   !
   INTEGER nb_CHZZZ
   CHARACTER(len=30),DIMENSION(5) :: list_CHZZZ

   INQUIRE(file='stop',exist=err)
   INQUIRE(file='save',exist=err1)
   INQUIRE(file='display',exist=err2)
    
   IF (.NOT. err .AND.  &
       .NOT. err1 .AND. &
       .NOT. err2 ) RETURN
                 !123456789012345678901234567890

   list_CHZZZ(:)='                              '

   IF (err) THEN
     CALL cpu_time(time_in)   
     time=(time_in-time0)/3600.D0
     PRINT*,' !-----------------------------------!'
     PRINT*,' !  CALCUL STOPPED                   !'
     PRINT*,' ! TIME ELAPSED : ',time,'Hours'
     PRINT*,' ! SAVE OF FILE IS GOING TO BE DONE  !'

                   !123456789012345678901234567890
     list_CHZZZ(1)='WRITE OUT Vloc Rloc STEP 1    '
     list_CHZZZ(2)='WRITE OUT DOF STEP 1          '
     list_CHZZZ(3)='WRITE OUT GPV STEP 1          '
     list_CHZZZ(4)='WRITE OUTPUT GMV              '
     list_CHZZZ(5)='CLOSE POSTPRO FILES           '

     nb_CHZZZ = 5

     err1 = .FALSE.
     err2 = .FALSE.

   ENDIF

   IF (err1) THEN
     CALL cpu_time(time_in)   
     time=(time_in-time0)/3600.D0

     PRINT*,' !-----------------------------------!'
     PRINT*,' !  SAVE OF FILE:                    !'
     PRINT*,' ! TIME ELAPSED : ',time,'Hours'

     list_CHZZZ(1)='WRITE OUT Vloc Rloc STEP 1    '
     list_CHZZZ(2)='WRITE OUT DOF STEP 1          '
     list_CHZZZ(3)='WRITE OUT GPV STEP 1          '

     nb_CHZZZ = 3

     err  = .FALSE.
     err2 = .FALSE.

   ENDIF

   IF (err2) THEN
     CALL cpu_time(time_in)   
     time=(time_in-time0)/3600.D0
     PRINT*,' !-----------------------------------!'
     PRINT*,' !  FILE FOR DISPLAY:                !'
     PRINT*,' ! TIME ELAPSED : ',time,'Hours'

                   !123456789012345678901234567890
     list_CHZZZ(1)='WRITE OUTPUT GMV              '

     nb_CHZZZ = 1

     err  = .FALSE.
     err1 = .FALSE.

   ENDIF

   DO i=1,nb_CHZZZ

     CHZZZ=list_CHZZZ(i)

     CALL LMGC90_COMMANDS

   ENDDO


   PRINT*,' !-----------------------------------!'

   IF (err) STOP

 END SUBROUTINE check_interactive_command


 SUBROUTINE LMGC90_COMMANDS

  IMPLICIT NONE

  !
  ! master shared part
  !
   CALL chic_command_overall  
   CALL more_chic_command_overall  
  ! 
  ! kernel part
  !
   CALL chic_command_nlgs
   CALL more_chic_command_nlgs

   CALL chic_command_cpg

   IF (IETZZZ == 1) THEN 
     CALL chic_command_afterall
     CALL more_chic_command_afterall
     RETURN
   ENDIF
  !
  ! contact element part
  !
   ! contact elements DISKx against DISKx commands
   CALL chic_command_DKDKx
   CALL more_chic_command_DKDKx

   ! contact elements DISKx against xKSID commands
   CALL chic_command_DKKDx
   CALL more_chic_command_DKKDx

   ! contact elements DISKx against JONCx commands
   CALL chic_command_DKJCx
   CALL more_chic_command_DKJCx

   ! contact elements DISKx against DISPx commands
   CALL chic_command_DKDPx
   CALL more_chic_command_DKDPx

   ! contact elements DISKx against xPSID commands
   CALL chic_command_DKPDx
   CALL more_chic_command_DKPDx

   ! contact elements PT2Dx against PT2Dx commands
   CALL chic_command_PTPT2
   CALL more_chic_command_PTPT2

   ! contact elements
   CALL chic_command_PLPLx
   CALL more_chic_command_PLPLx

   ! contact elements
   CALL chic_command_PLJCx
   CALL more_chic_command_PLJCx

   ! contact elements
   CALL chic_command_DKPLx
   CALL more_chic_command_DKPLx


   ! contact elements CLxxx against JONCx commands
   CALL chic_command_CLJCx
   CALL more_chic_command_CLJCx

   ! contact elements CLxxx against CLALp commands
   CALL chic_command_CLALp
   CALL more_chic_command_CLALp

   ! contact elements PT2DL against PT2DL commands
   CALL chic_command_P2P2L
   CALL more_chic_command_P2P2L

   ! contact elements POLYG against CLALp commands
   CALL chic_command_PLALp
   CALL more_chic_command_PLALp

   ! contact elements DISKx against CLALp commands
   CALL chic_command_DKALp
   CALL more_chic_command_DKALp

   ! contact elements DISPx against ALpxx commands
   CALL chic_command_DPALp
   CALL more_chic_command_DPALp

   ! contact elements DISKx against DISKL commands
   CALL chic_command_DKDKL
   CALL more_chic_command_DKDKL


   IF (IETZZZ == 1) THEN 
     CALL chic_command_afterall
     CALL more_chic_command_afterall
     RETURN
   ENDIF
  !
  ! shared part
  !
   ! bulk and contact behaviour laws commands
   CALL chic_command_bulk_behav
   CALL more_chic_command_bulk_behav

   CALL chic_command_models
   CALL more_chic_command_models

   CALL chic_command_tact_behav
   CALL more_chic_command_tact_behav
  !
  ! bodies and contactors part
  !
   CALL chic_command_RBDY2
   CALL more_chic_command_RBDY2

   ! bodies DISKx commands 
   CALL chic_command_DISKx
   CALL more_chic_command_DISKx

   ! bodies xKSID commands 
   CALL chic_command_xKSID
   CALL more_chic_command_xKSID

   ! bodies JONCx commands
   CALL chic_command_JONCx
   CALL more_chic_command_JONCx

   ! bodies DISPx commands 
   CALL chic_command_DISPx
   CALL more_chic_command_DISPx

   ! bodies xPSID commands 
   CALL chic_command_xPSID
   CALL more_chic_command_xPSID

   ! bodies POLYG commands
   CALL chic_command_POLYG
   CALL more_chic_command_POLYG

   ! bodies POINT commands
   CALL chic_command_PT2Dx
   CALL more_chic_command_PT2Dx

   ! bodies MAILx commands
   CALL chic_command_MAILx
   CALL more_chic_command_MAILx
   !
   CALL chic_command_mecaMAILx
   CALL more_chic_command_mecaMAILx

   CALL chic_command_therMAILx
   CALL more_chic_command_therMAILx
   !
   CALL chic_command_CLxxx
   CALL more_chic_command_CLxxx
   !
   CALL chic_command_ALpxx
   CALL more_chic_command_ALpxx
   !
   CALL chic_command_PT2DL
   CALL more_chic_command_PT2DL
   !
   CALL chic_command_DISKL
   CALL more_chic_command_DISKL

   IF (IETZZZ == 1) THEN 
     CALL chic_command_afterall
     CALL more_chic_command_afterall
     RETURN
   ENDIF
   ! timer output 
!   call chic_command_timer
!   if (IETZZZ == 1) then 
!     call chic_command_afterall
!     cycle
!   endif

  !
  ! pre - post processing part
  ! 
   ! 2D RIGID sample creation commands 
   CALL chic_command_prepro

   IF (IETZZZ == 1) THEN 
     CALL chic_command_afterall
     CALL more_chic_command_afterall
     RETURN
   ENDIF
       
   CALL chic_command_post2D
   CALL more_chic_command_post2D

   IF (IETZZZ == 1) THEN 
     CALL chic_command_afterall
     CALL more_chic_command_afterall
     RETURN
   ENDIF

   CALL chic_command_postpro

   CALL chic_command_mp_solver

   IF (IETZZZ == 1) THEN 
     CALL chic_command_afterall
     CALL more_chic_command_afterall
     RETURN
   ENDIF


 END SUBROUTINE LMGC90_COMMANDS


END PROGRAM



