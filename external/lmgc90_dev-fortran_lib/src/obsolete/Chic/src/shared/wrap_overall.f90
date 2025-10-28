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
MODULE wrap_OVERALL
  
  !!****h* LMGC90.CHIC/OVERALL
  !! NAME
  !!  module wrap_OVERALL
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/OVERALL
  !!****

  USE CHIC
  USE overall, ONLY: &
      Init_EntityList, &
      BinClose_in_dof, &
      BinClose_out_dof, &
      BinClose_last_dof, &
      Write_out_bodies_Ol, &
      Clean_out_bodies,&
      Clean_in_bodies, &
      Write_out_driven_dof_Ol, &
      Read_in_dof_Ol, &
      BinRead_in_dof_Ol, &
      Write_xxx_dof_Ol, &
      BinOpen_out_dof,&
      BinWrite_out_dof_Ol, &
      BinOpen_last_dof,&
      BinWrite_last_dof_Ol, &
      Read_in_Vloc_Rloc_Ol, &
      Write_xxx_Vloc_Rloc_Ol,&
      Read_in_gpv_Ol, &
      Write_xxx_gpv_Ol,&
      Display_prox_tactors_Ol,&
      Write_xxx_Rnod_Ol,&
      Clean_entitylist, &
      time_increment, &
      Updt_time_begin, & 
      Set_newton_tolerance, &
      Set_newton_maxloop, &
      Set_newton_badloop, &
      Set_newton_goodloop, &
      Set_newton_rate_step, & 
      Set_newton_loop, &
      Incre_newton_loop, &
      Comp_NR_time_step, &
      DISPLAY_TIME, &
      Clean_writing_flags, &
      Get_NSTEP, &
      Init_dimension,&
      Set_time_step, &
      Init_gear_integrator, &
      Init_theta_integrator, &
      init_verlet_integrator, &
      Init_CN_integrator, &
      Set_final_time, &
      Set_min_time_step, &
      Set_max_time_step, &
      Acti_large_computation, &
      set_run_contactor, &
      init_post_data_ol, &
      update_post_data_ol, &
      write_out_gmv_Ol, &
      write_out_mp_values_Ol, &
      get_time , &
      get_time_step , &
      set_with_experimental_dev, &
      set_is_externalFEM


  PRIVATE

  PUBLIC chic_command_overall,more_chic_command_overall, &
         chic_command_afterall,more_chic_command_afterall, &
         get_NSTEP

  CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE chic_command_overall

    IMPLICIT NONE

    INTEGER        :: ii,l_ii,Nstep,entier,entier1,Nstep_rough_seek=1,info
    INTEGER        :: ifirst,ilast
    REAL (kind=8)  :: value
    REAL(kind=8)   :: DTPS,TPS,H
    CHARACTER(len=10) :: chaine10
    CHARACTER(len=30) :: cout

    IF (INDEX(CHZZZ,'SELECT PROX TACTORS')==1 .or. &
        INDEX(CHZZZ,'FD SELECT PROX TACTORS')==1 .or. &
        INDEX(CHZZZ,'WCP SELECT PROX TACTORS')==1) THEN
       !!****e* OVERALL/SELECT PROX TACTORS
       !! NAME 
       !!  SELECT PROX TACTORS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/Clean_EntityList
       !!  LMGC90.CORE/OVERALL/set_run_contactor
       !!****
       CALL LOGCHIC('overall')

       CALL Clean_EntityList

       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
          ! updating visibility array at every step.
          ii  = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) READ(CHZZZ(ii+4:30),*) Nstep_rough_seek
       END IF
       CALL set_run_contactor(Nstep_rough_seek)

       RETURN
    END IF
!!!
    IF ( INDEX(CHZZZ,'MD INCREMENT STEP')==1 .OR. &
         INDEX(CHZZZ,'DEM INCREMENT STEP')==1) THEN 
       CALL LOGCHIC('... USELESS COMMAND !')
       CALL LOGCHIC('HAVE BEEN RELACED BY INCREMENT STEP')
    END IF
!!!
    IF (INDEX(CHZZZ,'INCREMENT STEP')==1)THEN
       !!****e* OVERALL/INCREMENT STEP
       !! NAME
       !!   INCREMENT STEP
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/time_increment
       !!  LMGC90.CORE/OVERALL/Set_newton_loop
       !!****
       CALL LOGCHIC('overall')
       CALL  time_increment
       CALL  Set_newton_loop(0)
       RETURN      
    END IF
!!!
    IF ( INDEX(CHZZZ,'MD UPDATE DOF')==1 .OR. &
         INDEX(CHZZZ,'DEM UPDATE DOF')==1) THEN 
       CALL LOGCHIC('... USELESS COMMAND !')
       CALL LOGCHIC('HAVE BEEN RELACED BY UPDATE DOF')
    END IF
!!!
    IF (INDEX(CHZZZ,'UPDATE DOF')==1 .OR. &
        INDEX(CHZZZ,'UPDATE THERM DOF')==1)THEN
       !!****e* CHIC.OVERALL/UPDATE_DOF
       !! NAME
       !!   UPDATE DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/Updt_time_begin
       !!****
       CALL LOGCHIC('overall')
       CALL Updt_time_begin
       RETURN      
    END IF
!!!
!!! WRITE FILES ************************************************
!!!
    IF (INDEX(CHZZZ,'WRITE LAST DOF')==1) THEN
       !!****e* OVERALL/WRITE LAST DOF
       !! NAME
       !!   WRITE LAST DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_dof_Ol
       !!****
       CALL LOGCHIC('overall')
       CALL Write_xxx_dof_Ol(2)
       RETURN 
    END IF
!!!
    IF (INDEX(CHZZZ,'BINARY WRITE LAST DOF')==1) THEN
       CALL LOGCHIC('overall')
       !!****e* OVERALL/BINARY WRITE LAST DOF
       !! NAME
       !!   BINARY WRITE LAST DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/BinOpen_last_dof
       !!  LMGC90.CORE/OVERALL/BinWrite_last_dof_Ol
       !!****
       CALL BinOpen_last_dof
       CALL BinWrite_last_dof_Ol
       RETURN 
    END IF
    
    IF (INDEX(CHZZZ,'WRITE OUTPUT GMV')==1) THEN
       !!****e* OVERALL/WRITE OUTPUT GMV
       !! NAME
       !!  WRITE OUTPUT GMV
       !! PURPOSE
       !!  active FLAG to write gmv file
       !! USES
       !!  LMGC90.CORE/OVERALL/write_out_gmv_Ol
       !!****
       CALL LOGCHIC('overall')
 
      IF (INDEX(CHZZZ,'STEP') /= 0) THEN
          NSTEP = get_NSTEP() 
          ii  = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) THEN
             READ(CHZZZ(ii+4:30),*) entier 
             IF (MODULO(Nstep,entier) /= 0) RETURN
          END IF

          CALL write_out_gmv_Ol

          RETURN
      END IF

      IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
        ii = INDEX(CHZZZ,'DTPS')
        l_ii= LEN_TRIM(CHZZZ(ii+4:30))
        IF (l_ii /= 0) THEN
          READ(CHZZZ(ii+4:30),*) DTPS
          TPS = get_TIME() 
          H = get_TIME_STEP() 
          IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
            CALL write_out_gmv_Ol
            RETURN
          ENDIF 

        END IF
      END IF

    END IF
!!!
!!! attention pour ce mot clef il faut necessairement un STEP
!!!
    IF (INDEX(CHZZZ,'WRITE OUT DOF')==1) THEN
       !!****e* OVERALL/WRITE OUT DOF
       !! NAME
       !!   WRITE OUT DOF
       !! PURPOSE
       !!   
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_dof_Ol
       !!****
      CALL LOGCHIC('overall')

      IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
        NSTEP = get_NSTEP()
        ii = INDEX(CHZZZ,'STEP')
        l_ii = LEN_TRIM(CHZZZ(ii+4:30))
        IF (l_ii /= 0) THEN
          READ(CHZZZ(ii+4:30),*) entier
          IF (MODULO(Nstep,entier) /= 0) RETURN
        END IF

        CALL write_xxx_dof_Ol(1)

        RETURN 
      END IF

      IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
        ii = INDEX(CHZZZ,'DTPS')
        l_ii= LEN_TRIM(CHZZZ(ii+4:30))
        IF (l_ii /= 0) THEN
          READ(CHZZZ(ii+4:30),*) DTPS
          TPS = get_TIME() 
          H = get_TIME_STEP() 
          IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
            CALL write_xxx_dof_Ol(1)
            RETURN
          ENDIF 

        END IF
      END IF


    END IF

    IF (INDEX(CHZZZ,'BINARY WRITE OUT DOF')==1) THEN
       !!****e* OVERALL/BINARY WRITE OUT DOF
       !! NAME
       !!  BINARY WRITE OUT DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/binwrite_out_dof_ol
       !!****
       CALL LOGCHIC('overall')
       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 

          NSTEP = get_NSTEP()
          
          ii  = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) THEN
             READ(CHZZZ(ii+4:30),*) entier
             IF (MODULO(Nstep,entier) /= 0) RETURN
          END IF
          
          CALL binwrite_out_dof_ol
          
          RETURN 
       END IF

       IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
         ii = INDEX(CHZZZ,'DTPS')
         l_ii= LEN_TRIM(CHZZZ(ii+4:30))
         IF (l_ii /= 0) THEN
           READ(CHZZZ(ii+4:30),*) DTPS
           TPS = get_TIME() 
           H = get_TIME_STEP() 
           IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
             CALL binwrite_out_dof_ol
             RETURN
           ENDIF 
         END IF
       END IF

    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT DOF')==1) THEN
       !!****e* OVERALL/DISPLAY OUT DOF
       !! NAME
       !!   DISPLAY OUT DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_dof_Ol
       !!****
       CALL LOGCHIC('overall')
       CALL write_xxx_dof_Ol(6)
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST Rnod')==1) THEN
       CALL LOGCHIC('overall')
       !!****e* OVERALL/WRITE LAST Rnod
       !! NAME
       !!   WRITE LAST Rnod
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_Rnod_Ol
       !!****
       CALL write_xxx_Rnod_Ol(2)
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT MP VALUES')==1) THEN
       !!****e* OVERALL/WRITE_OUT_MP_VALUES
       !! NAME
       !!   WRITE OUT MP VALUES
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_out_mp_values_Ol
       !!****
       CALL LOGCHIC('overall')

       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 

          NSTEP = get_NSTEP()

          ii = INDEX(CHZZZ,'STEP')
          l_ii = LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) THEN
             READ(CHZZZ(ii+4:30),*) entier
             IF (MODULO(Nstep,entier) /= 0) RETURN
          END IF

          CALL write_out_mp_values_Ol

          RETURN 
       END IF

       IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
         ii = INDEX(CHZZZ,'DTPS')
         l_ii= LEN_TRIM(CHZZZ(ii+4:30))
         IF (l_ii /= 0) THEN
           READ(CHZZZ(ii+4:30),*) DTPS
           TPS = get_TIME() 
           H = get_TIME_STEP() 
           IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
             CALL write_out_mp_values_Ol
             RETURN
           endif 
         END IF
       END IF


    END IF

    IF (INDEX(CHZZZ,'WRITE OUT Rnod')==1) THEN
       !!****e* OVERALL/WRITE OUT Rnod
       !! NAME
       !!   WRITE OUT Rnod
       !! PURPOSE
       !!   
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_Rnod_Ol
       !!****
       CALL LOGCHIC('overall')

       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 
          NSTEP = get_NSTEP()
          ii  = INDEX(CHZZZ,'STEP')
          l_ii= LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) THEN
             READ(CHZZZ(ii+4:30),*) entier
             IF (MODULO(Nstep,entier) /= 0) RETURN
          END IF
          
          CALL write_xxx_Rnod_Ol(1)
          
          RETURN 
       END IF

       IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
         ii = INDEX(CHZZZ,'DTPS')
         l_ii= LEN_TRIM(CHZZZ(ii+4:30))
         IF (l_ii /= 0) THEN
           READ(CHZZZ(ii+4:30),*) DTPS
           TPS = get_TIME() 
           H = get_TIME_STEP() 
           IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
             CALL write_xxx_Rnod_Ol(1)             
             RETURN
           endif 
         END IF
       END IF


    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT Rnod')==1) THEN
       !!****e* OVERALL/DISPLAY OUT Rnod
       !! NAME
       !!   DISPLAY OUT Rnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_Rnod_Ol
       !!****       
       CALL LOGCHIC('overall')
       CALL write_xxx_Rnod_Ol(6)
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE DRIVEN DOF')==1) THEN
       !!****e* OVERALL/WRITE DRIVEN DOF
       !! NAME
       !!   WRITE DRIVEN DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_out_driven_dof_Ol
       !!****       
       CALL LOGCHIC('overall')
       CALL write_out_driven_dof_Ol
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY TIMES')==1) THEN
       !!****e* OVERALL/DISPLAY TIMES
       !! NAME
       !!   DISPLAY TIMES
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/DISPLAY_TIME
       !!****     
       CALL LOGCHIC('overall')
       CALL DISPLAY_TIME
       IETZZZ=1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY PROX TACTORS')==1) THEN
       !!****e* OVERALL/DISPLAY PROX TACTORS'
       !! NAME
       !!   DISPLAY PROX TACTORS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/display_prox_tactors_Ol
       !!****         
       CALL LOGCHIC('overall')
       CALL display_prox_tactors_Ol
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST Vloc Rloc')==1) THEN
       !!****e* OVERALL/WRITE LAST Vloc Rloc
       !! NAME
       !!   WRITE LAST Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_Vloc_Rloc_Ol
       !!****       
       CALL LOGCHIC('overall')
       CALL write_xxx_Vloc_Rloc_Ol(2)
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) THEN
       !!****e* OVERALL/WRITE OUT Vloc Rloc
       !! NAME
       !!   WRITE OUT Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_Vloc_Rloc_Ol
       !!****  
       CALL LOGCHIC('overall')

       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 

          NSTEP = get_NSTEP()

          ii   = INDEX(CHZZZ,'STEP')
          l_ii = LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) THEN
             READ(CHZZZ(ii+4:30),*) entier
             IF (MODULO(Nstep,entier) /= 0) RETURN
          END IF
          CALL write_xxx_Vloc_Rloc_Ol(1)
          RETURN 
       END IF

       IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
         ii = INDEX(CHZZZ,'DTPS')
         l_ii= LEN_TRIM(CHZZZ(ii+4:30))
         IF (l_ii /= 0) THEN
           READ(CHZZZ(ii+4:30),*) DTPS
           TPS = get_TIME() 
           H = get_TIME_STEP() 
           IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
             CALL write_xxx_Vloc_Rloc_Ol(1)
             RETURN
           endif 
         END IF
       END IF

    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT Vloc Rloc')==1) THEN
       !!****e* OVERALL/DISPLAY OUT Vloc Rloc
       !! NAME
       !!   DISPLAY OUT Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_Vloc_Rloc_Ol
       !!****  
       CALL LOGCHIC('overall')
       CALL write_xxx_Vloc_Rloc_Ol(6)

       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST GPV')==1) THEN
       !!****e* OVERALL/WRITE LAST GPV
       !! NAME
       !!   WRITE LAST GPV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_gpv_ol
       !!****    
       CALL LOGCHIC('overall')
       CALL write_xxx_gpv_ol(2)
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT GPV')==1) THEN
       !!****e* OVERALL/WRITE OUT GPV
       !! NAME
       !!   WRITE OUT GPV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_gpv_ol
       !!****  
       CALL LOGCHIC('overall')

       IF (INDEX(CHZZZ,'STEP') /= 0) THEN 

          NSTEP = get_NSTEP()

          ii   = INDEX(CHZZZ,'STEP')
          l_ii = LEN_TRIM(CHZZZ(ii+4:30))
          IF (l_ii /= 0) THEN
             READ(CHZZZ(ii+4:30),*) entier
             IF (MODULO(Nstep,entier) /= 0) RETURN
          END IF
          CALL write_xxx_gpv_ol(1)
          RETURN 
       END IF


       IF (INDEX(CHZZZ,'DTPS') /= 0) THEN 
         ii = INDEX(CHZZZ,'DTPS')
         l_ii= LEN_TRIM(CHZZZ(ii+4:30))
         IF (l_ii /= 0) THEN
           READ(CHZZZ(ii+4:30),*) DTPS
           TPS = get_TIME() 
           H = get_TIME_STEP() 
           IF (floor(TPS/DTPS) /= floor((TPS-H)/DTPS) ) then
             CALL write_xxx_gpv_ol(1)
             RETURN
           endif 
         END IF
       END IF

    END IF

    IF (INDEX(CHZZZ,'CHECK NL CONVERGENCE')==1) THEN
       !!****e* OVERALL/CHECK NL CONVERGENCE
       !! NAME
       !!   CHECK NL CONVERGENCE
       !! SYNOPSIS
       !!   CHECK NL CONVERGENCE
       !!   [value]
       !! INPUTS
       !!   value : tolerance
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/set_newton_tolerance
       !!  LMGC90.CORE/OVERALL/incre_newton_loop
       !!****         
       READ(CHLZZZ(NZZZ+1),*) value
       IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A9)')' @ ',CHZZZ,'overall'
       
       CALL set_newton_tolerance(value) 
       CALL incre_newton_loop

       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'COMPUTE TIME STEP')==1) THEN
       !!****e* OVERALL/COMPUTE TIME STEP
       !! NAME
       !!  COMPUTE TIME STEP
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/Comp_NR_time_step
       !!****
       IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30,1X,A7)')' @ ',CHZZZ,'overall'

       CALL Comp_NR_time_step(entier,entier1)

       IFLZZZ(NZZZ,2)=entier  ! =0 on recommence le pas
       IFLZZZ(NZZZ,3)=entier1 ! =0 on arrete le calcul

       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DIME')==1) THEN
       !!****e* OVERALL/DIME
       !! NAME
       !!  DIME
       !! SYNOPSIS
       !!  DIME
       !!  [ch10]
       !! INPUTS
       !!  ch10 : space dimension
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/init_dimension
       !!****
       READ(CHLZZZ(NZZZ+1),'(A10)') chaine10
       CALL LOGCHIC('overall')
       CALL LOGMESCHIC(chaine10)
       IETZZZ=1
       CALL init_dimension(chaine10)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'TIME STEP')==1) THEN
       !!****e* OVERALL/TIME STEP
       !! NAME
       !!  TIME STEP
       !! SYNOPSIS
       !!  TIME STEP
       !!  [h]
       !! INPUTS
       !!  h : time step value
       !! PURPOSE
       !!  define the lenght of the time step
       !! USES
       !!  LMGC90.CORE/OVERALL/set_time_step
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       WRITE(cout,'(D12.5)') value
       CALL LOGMESCHIC(cout)
       IETZZZ=1
       CALL set_time_step(value)        
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'THETA')==1) THEN
       !!****e* OVERALL/THETA
       !! NAME
       !!  THETA
       !! SYNOPSIS
       !!  THETA
       !!  [T]
       !! INPUTS
       !!  T : parameter of the theta method
       !! PURPOSE
       !!  define the parameter of the theta method T must yield between 0.5 and 1.0
       !! USES
       !!  LMGC90.CORE/OVERALL/init_theta_integrator
       !!  LMGC90.CORE/OVERALL/init_CN_integrator
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       WRITE(cout,'(D12.5)') value
       CALL LOGMESCHIC(cout)
       CALL init_theta_integrator(value)
       CALL init_CN_integrator(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'THERMAL THETA')==1) THEN
       !!****e* OVERALL/THERMAL THETA
       !! NAME
       !!  THERMAL THETA
       !! SYNOPSIS
       !!  THERMAL THETA
       !!  [TT]
       !! INPUTS
       !!  TT : 
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/init_CN_integrator
       !!****
       READ(CHLZZZ(NZZZ+1),*) value 
       CALL LOGCHIC('overall')
       WRITE(cout,'(D12.5)') value
       CALL LOGMESCHIC(cout)
       CALL init_CN_integrator(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'FINAL TIME')==1) THEN
       !!****e* OVERALL/FINAL TIME
       !! NAME
       !!  FINAL TIME
       !! SYNOPSIS
       !!  FINAL TIME
       !!  [FT]
       !! INPUTS
       !!  FT : 
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/set_final_time
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       WRITE(cout,'(D12.5)') value   
       CALL LOGMESCHIC(cout)
       CALL set_final_time(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'MIN TIME STEP')==1) THEN
       !!****e* OVERALL/MIN TIME STEP
       !! NAME
       !!  MIN TIME STEP
       !! SYNOPSIS
       !!  MIN TIME STEP
       !!  [MT]
       !! INPUTS
       !!  MT : minimal value of the time step
       !! PURPOSE
       !!  define the minimal value taken by the time step during the simulation process
       !! USES
       !!  LMGC90.CORE/OVERALL/set_min_time_step
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       WRITE(cout,'(D12.5)') value  
       CALL LOGMESCHIC(cout)
       CALL set_min_time_step(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'MAX TIME STEP')==1) THEN
       !!****e* OVERALL/MAX TIME STEP
       !! NAME
       !!  MAX TIME STEP
       !! SYNOPSIS
       !!  MAX TIME STEP
       !!  [MT]
       !! INPUTS
       !!  MT : maximal value of the time step
       !! PURPOSE
       !!  define the maximal value taken by the time step during the simulation process
       !! USES
       !!  LMGC90.CORE/OVERALL/set_max_time_step
       !!****
       READ(CHLZZZ(NZZZ+1),*) value 
       CALL LOGCHIC('overall')
       WRITE(cout,'(D12.5)') value  
       CALL LOGMESCHIC(cout)
       CALL set_max_time_step(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'NB BULK ITER MAX')==1) THEN
       !!****e* OVERALL/NB BULK ITER MAX
       !! NAME
       !!  NB BULK ITER MAX
       !! SYNOPSIS
       !!  NB BULK ITER MAX
       !!  [iter]
       !! INPUTS
       !!  iter :
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/set_newton_maxloop
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       CALL set_newton_maxloop(value)       
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'NB BULK ITE FOR GOOD CONV')==1) THEN
       !!****e* OVERALL/NB BULK ITE FOR GOOD CONV
       !! NAME
       !!  NB BULK ITE FOR GOOD CONV
       !! SYNOPSIS
       !!  NB BULK ITE FOR GOOD CONV
       !!  [iter]
       !! INPUTS
       !!  iter :
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/set_newton_goodloop
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       CALL set_newton_goodloop(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'NB BULK ITE FOR BAD CONV')==1) THEN
       !!****e* OVERALL/NB BULK ITE FOR BAD CONV
       !! NAME
       !!  NB BULK ITE FOR BAD CONV
       !! SYNOPSIS
       !!  NB BULK ITE FOR BAD CONV
       !!  [iter]
       !! INPUTS
       !!  iter : 
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/set_newton_badloop
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       CALL set_newton_badloop(value)
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'NB INC FOR GOODORBAD CONV')==1) THEN
       !!****e* OVERALL/NB INC FOR GOODORBAD CONV
       !! NAME
       !!  NB INC FOR GOODORBAD CONV
       !! SYNOPSIS
       !!  NB INC FOR GOODORBAD CONV
       !!  [inc]
       !! INPUTS
       !!  inc : 
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/set_newton_rate_step
       !!****
       READ(CHLZZZ(NZZZ+1),*) value
       CALL LOGCHIC('overall')
       CALL set_newton_rate_step(value) 
       IETZZZ=1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'WRITE BODIES')==1 .OR. &
        INDEX(CHZZZ,'CLEARED WRITE BODIES')==1) THEN
       !!****e* OVERALL/WRITE BODIES
       !! NAME
       !!  WRITE BODIES
       !! SYNOPSIS
       !!  WRITE BODIES
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_out_bodies_ol
       !!****
       CALL LOGCHIC('overall')
       CALL LOGMESCHIC('writting overall comments')              
       CALL write_out_bodies_ol
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'READ INI DOF')==1) THEN
       !!****e* OVERALL/READ INI DOF
       !! NAME
       !!   READ INI DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/read_in_dof_ol
       !!****
       CALL LOGCHIC('overall')
       CALL read_in_dof_ol
       RETURN 
    END IF
!!!
    IF (INDEX(CHZZZ,'BINARY READ INI DOF')==1) THEN
       !!****e* OVERALL/BINARY READ INI DOF
       !! NAME
       !!  BINARY READ INI DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/binread_in_dof_ol
       !!****
       CALL LOGCHIC('overall')
       CALL binread_in_dof_ol
       RETURN 
    END IF
!!!
    IF (INDEX(CHZZZ,'READ INI Vloc Rloc')==1) THEN
       !!****e* OVERALL/READ INI Vloc Rloc
       !! NAME
       !!  READ INI Vloc Rloc
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/read_in_Vloc_Rloc_ol
       !!****
       CALL LOGCHIC('overall')
       CALL read_in_Vloc_Rloc_ol
       RETURN 
    END IF
!!!
    IF (INDEX(CHZZZ,'READ INI GPV')==1) THEN
       !!****e* OVERALL/READ INI GPV
       !! NAME
       !!  READ INI GPV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/read_in_gpv_ol
       !!****
       CALL LOGCHIC('overall')
       CALL read_in_gpv_ol
       RETURN 
    END IF
!!!
!!!-------------------------------------
!!!
!!! more CHIC commands for preprocessing
!!!
!!!-------------------------------------
!!!
    IF (INDEX(CHZZZ,'CLEAN BODIES.OUT')==1) THEN
       !!****e* OVERALL/CLEAN BODIES.OUT
       !! NAME
       !!  CLEAN BODIES.OUT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/clean_out_bodies
       !!****
       CALL LOGCHIC('overall')
       CALL clean_out_bodies
       IETZZZ = 1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'REBUILD BODIES.DAT')==1) THEN
       !!****e* OVERALL/REBUILD BODIES.DAT
       !! NAME
       !!  REBUILD BODIES.DAT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/clean_in_bodies
       !!****
       CALL LOGCHIC('overall')
       CALL LOGMESCHIC('rewriting bodies.dat')              

       CALL clean_in_bodies

       RETURN      
    END IF
!!!
!!!fd des tests
!!!
    IF ((INDEX(CHZZZ,'WRITE OUT DOF')==1) .AND. &
        (INDEX(CHZZZ,'STEP') == 0)) THEN  
       CALL LOGCHIC('overall') 
       WRITE(6,*)'WARNING:'   
       WRITE(6,*)'The command "WRITE OUT DOF STEP s" prints dof results every s steps'  
       WRITE(6,*)'in succesive files numbered DOF.OUT.p where p is the number of steps modulo s'   
       WRITE(6,*)'The plain command "WRITE OUT DOF" is obsolete.'
       WRITE(6,*)'It must be replaced by the command "WRITE LAST DOF".'  
       WRITE(6,*)'This command prints full last step results in the file DOF.LAST'
       WRITE(6,*)'The file ./OUTBOX/DOF.LAST may be copied as ./DATBOX/DOF.INI'
       WRITE(6,*)'to operate a restart'
       STOP 
       RETURN
    END IF
!!!
    IF ((INDEX(CHZZZ,'BINARY WRITE OUT DOF')==1) .AND. &
        (INDEX(CHZZZ,'STEP') == 0)) THEN  
       CALL LOGCHIC('overall') 
       WRITE(6,*)'WARNING:'   
       WRITE(6,*)'The command "BINARY WRITE OUT DOF STEP s" prints dof results every s steps'  
       WRITE(6,*)'in succesive files numbered DOF.OUT.p where p is the number of steps modulo s'   
       WRITE(6,*)'The plain command "BINARY WRITE OUT DOF" is obsolete.'
       WRITE(6,*)'It must be replaced by the command "BINARY WRITE LAST DOF".'  
       WRITE(6,*)'This command prints full last step results in the file DOF.LAST'
       WRITE(6,*)'The file ./OUTBOX/DOF.LAST may be copied as ./DATBOX/DOF.INI'
       WRITE(6,*)'to operate a restart'
       STOP 
       RETURN
    END IF
!!!
    IF ((INDEX(CHZZZ,'WRITE OUT Vloc Rloc')==1) .AND. &
        (INDEX(CHZZZ,'STEP') == 0)) THEN 
       CALL LOGCHIC('overall') 
       WRITE(6,*)'WARNING:' 
       WRITE(6,*)'The command "WRITE OUT Vloc Rloc STEP s" prints Vloc Rloc results every s steps'  
       WRITE(6,*)'in succesive files numbered Vloc_Rloc.OUT.p where p is the number of steps modulo s'   
       WRITE(6,*)'The plain command "WRITE OUT Vloc Rloc" is obsolete.' 
       WRITE(6,*)'It must be replaced by the command "WRITE LAST Vloc Rloc".'  
       WRITE(6,*)'This command prints full last step results in the file Vloc_Rloc.LAST'
       WRITE(6,*)'The file ./OUTBOX/Vloc_Rloc.LAST may be copied as ./DATBOX/Vloc_Rloc.INI'
       WRITE(6,*)'to operate a restart'
       STOP 
       RETURN
    END IF
!!!
    IF ((INDEX(CHZZZ,'WRITE OUT Rnod')==1) .AND. & 
        (INDEX(CHZZZ,'STEP') == 0)) THEN 
       CALL LOGCHIC('overall') 
       WRITE(6,*)'WARNING:' 
       WRITE(6,*)'The command "WRITE OUT Rnod STEP s" prints Rnod results every s steps'  
       WRITE(6,*)'in succesive files numbered Rnod.OUT.p where p is the number of steps modulo s'   
       WRITE(6,*)'The plain command "WRITE OUT Rnod" is obsolete.'
       WRITE(6,*)'It must be replaced by the command "WRITE LAST Rnod".'  
       WRITE(6,*)'This command prints full last step results in the file Rnod.LAST'
       STOP 
       RETURN
    END IF
!!!
!!! mr
!!! For large number of particle or people who have eyes larger then stomac
!!!
    IF (INDEX(CHZZZ,'XXL COMPUTATION')==1) THEN
       CALL LOGCHIC('overall')
       CALL acti_large_computation
       IETZZZ = 1
       RETURN      
    END IF
!!!
!!! mr
!!! For computation with Molecular Dynamics (2D/3D)
!!!
    IF (INDEX(CHZZZ,'USE GEAR INTEGRATOR')==1) THEN
       !!****e* OVERALL/USE GEAR INTEGRATOR
       !! NAME
       !!  USE GEAR INTEGRATOR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/init_gear_integrator
       !!****
       CALL LOGCHIC('overall')

       CALL init_gear_integrator

       IETZZZ = 1
       RETURN      
    END IF
!!!
    IF (INDEX(CHZZZ,'USE VERLET INTEGRATOR')==1) THEN
       !!****e* OVERALL/USE VERLET INTEGRATOR
       !! NAME
       !!  USE VERLET INTEGRATOR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/init_verlet_integrator
       !!****
       CALL LOGCHIC('overall')

       CALL init_verlet_integrator

       IETZZZ = 1
       RETURN      
    END IF

!!! fd -> mr faut etre un ane pour programmer comme ca !!
!!! bonjour les effets de bord si value est remplie entre temps aie aie!!
!!! pour tous les utilisateurs qui ne sont au courant de tes modifs 
!!! (i.e. le reste de l'univers) plus rien ne marche

    IF (INDEX(CHZZZ,'USE THETA INTEGRATOR')==1) THEN

!       !!****e* OVERALL/USE THETA INTEGRATOR
!       !! NAME
!       !!  USE THETA INTEGRATOR
!       !! PURPOSE
!       !!  
!       !! USES
!       !!  LMGC90.CORE/OVERALL/init_verlet_integrator
!       !!****
       CALL LOGCHIC('overall')
       CALL LOGCHIC('fd -> mr faut etre un ane pour programmer comme ca !!')

!       CALL init_theta_integrator(value)       
!       CALL init_CN_integrator(value)

       IETZZZ = 1
       RETURN      
!
    END IF
!!!
!!! mr
!!!-------------------------------------
!!!
!!! More command for post processing
!!!
!!!-------------------------------------
!!!
    IF (INDEX(CHZZZ,'UPDATE POST DATA')==1) THEN
       !!****e* OVERALL/UPDATE POST DATA
       !! NAME
       !!  UPDATE_POST_DATA
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/update_post_data_ol
       !!****
       CALL LOGCHIC('overall')
       CALL update_post_data_ol(info)
       IF(info == 0)THEN
          CALL LOGMESCHIC('LAST FILES HAVE BEEN READ')
          CALL LOGMESCHIC('POST PROCESSING IS OVER')
          STOP
       END IF
       IETZZZ = 1
       RETURN 
    END IF
   
    IF (INDEX(CHZZZ,'INIT POST DATA')==1) THEN
       !!****e* OVERALL/INIT POST DATA
       !! NAME
       !!  INIT POST DATA
       !! SYNOPSIS
       !!  INIT POST DATA
       !!  [ifirst]
       !!  [ilast]
       !! INPUTS
       !!  ifirst : identifiant of the first output file
       !!  ilast  : identifiant of the last output file
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/init_post_data_ol
       !!****
       CALL LOGCHIC('overall')
       READ(CHLZZZ(NZZZ+1),*) ifirst
       READ(CHLZZZ(NZZZ+2),*) ilast
       WRITE(6,*) ' @ ',ifirst
       WRITE(6,*) ' @ ',ilast
       CALL init_post_data_ol(ifirst,ilast)
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'USE EXPERIMENTAL DEV') /= 0) THEN
       !!****e* OVERALL/USE EXPERIMENTAL DEV
       !! NAME
       !!  USE EXPERIMENTAL DEV
       !! PURPOSE
       !!  activate some unstable devs
       !! USES
       !!  LMGC90.CORE/OVERALL/set_with_experimental_dev
       !!****
       CALL LOGCHIC('overall') 
       CALL set_with_experimental_dev
    END IF

    IF (INDEX(CHZZZ,'USE EXTERNAL FEM') /= 0) THEN
       !!****e* OVERALL/USE EXTERNAL FEM
       !! NAME
       !!  USE EXTERNAL FEM
       !! PURPOSE
       !!  allow to use the externalFEM library instead of lmgc90 FEM lib
       !! USES
       !!  LMGC90.CORE/OVERALL/set_is_externalFEM
       !!****
       CALL LOGCHIC('overall') 
      CALL set_is_externalFEM
    END IF


 END SUBROUTINE chic_command_overall
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE chic_command_afterall

 IMPLICIT NONE


 IF (INDEX(CHZZZ,'COMPUTE BOX')==1) THEN
       !!****e* AFTERALL/COMPUTE BOX
       !! NAME
       !!  COMPUTE BOX
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/AFTERALL/Init_entitylist
       !!****
   CALL LOGCHIC('afterall')

   CALL Init_entitylist

   IETZZZ = 1
   RETURN      
 END IF

 IF (INDEX(CHZZZ,'BINARY READ INI DOF')==1) THEN
       !!****e* AFTERALL/BINARY READ INI DOF
       !! NAME
       !!  BINARY READ INI DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/AFTERALL/binclose_in_dof
       !!****
   CALL LOGCHIC('afterall') 
   
   CALL binclose_in_dof

 ENDIF

 IF (INDEX(CHZZZ,'BINARY WRITE OUT DOF')==1) THEN
       !!****e* AFTERALL/BINARY WRITE OUT DOF
       !! NAME
       !!  BINARY WRITE OUT DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/AFTERALL/binclose_out_dof
       !!****
   CALL LOGCHIC('afterall') 
   CALL binclose_out_dof

 ENDIF

 IF (INDEX(CHZZZ,'BINARY WRITE LAST DOF')==1) THEN
       !!****e* AFTERALL/BINARY WRITE LAST DOF
       !! NAME
       !!  BINARY WRITE LAST DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/AFTERALL/binclose_out_dof
       !!****
   CALL LOGCHIC('afterall') 
   CALL binclose_out_dof

 ENDIF

 IF (INDEX(CHZZZ,'WRITE OUT') /= 0) THEN
       !!****e* AFTERALL/WRITE OUT
       !! NAME
       !!  WRITE OUT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/AFTERALL/Clean_writing_flags
       !!****
    CALL LOGCHIC('afterall') 
    CALL Clean_writing_flags
 END IF
 


END SUBROUTINE chic_command_afterall

 include '../../more_src/shared/wrap_overall.inc'

END MODULE wrap_overall

