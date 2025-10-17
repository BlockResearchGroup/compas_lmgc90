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
MODULE wrap_therMAILx

  !!****h* LMGC90.CHIC/therMAILx  
  !! NAME
  !!  module wrap_therMAILx  
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/therMAILx  
  !!****

  USE CHIC
  USE therMAILx,ONLY: get_nb_therMAILx, &
       compute_conductivity_therMAILx, &
       compute_capacity_therMAILx, &
       compute_ttFint_therMAILx, &
       compute_Fext_therMAILx, &
       assemb_KT_therMAILx, &
       assemb_RHS_therMAILx, &
       trial_assemb_KT_therMAILx, &
       trial_assemb_RHS_therMAILx, &
       increment_therMAILx, &
       comp_dof_therMAILx, &
       update_dof_therMAILx, &
       update_therm_bulk_therMAILx, &
       check_ther_convergence_therMAILx, &
       read_in_driven_dof_therMAILx, &
       read_models_therMAILx, &
       read_behaviours_therMAILx, &
       write_xxx_dof_therMAILx, &
       read_in_dof_therMAILx, &
       write_out_driven_dof_therMAILx, &
       read_in_gpv_therMAILx, &
       CHECK_therMAILx, &
       get_write_DOF_therMAILx, &
       get_write_Rnod_therMAILx,&
       set_field_bynode, &
       get_field_rank, &
       get_nb_nodes

CONTAINS

!!!----------------------------------------------------
  SUBROUTINE chic_command_therMAILx

    IMPLICIT NONE
    
    INTEGER :: ifrom,ito,IGOZZZ
    INTEGER :: iconv
    INTEGER :: nb_therMAILx
    LOGICAL :: CHECK,write_DOF,write_Rnod
    
    CHECK = CHECK_therMAILx()

    IF (.NOT.CHECK) RETURN
 
!!! HPP PROBLEMS

    IF (INDEX(CHZZZ,'COMPUTE CONDUCTIVITY')==1) THEN
       !!****e* therMAILx/COMPUTE CONDUCTIVITY
       !! NAME
       !!  COMPUTE CONDUCTIVITY
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_conductivity_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL compute_conductivity_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE CAPACITY')==1) THEN
       !!****e* therMAILx/COMPUTE CAPACITY
       !! NAME
       !!  COMPUTE CAPACITY
       !! PURPOSE
       !!  
       !! USES
       !! LMGC90.CORE/therMAILx/compute_capacity_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL compute_capacity_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE INTERNAL FLUX')==1) THEN
       !!****e* therMAILx/COMPUTE INTERNAL FLUX
       !! NAME
       !!  COMPUTE INTERNAL FLUX
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_ttFint_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL compute_ttFint_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE EXTERNAL FLUX')==1) THEN
       !!****e* therMAILx/COMPUTE EXTERNAL FLUX
       !! NAME
       !!  COMPUTE EXTERNAL FLUX
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_Fext_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL compute_Fext_therMAILx
       IETZZZ = 1
       RETURN      
    END IF
 
!!! construction of the iteration matrix and the corresponding right hand side vector

    IF (INDEX(CHZZZ,'ASSEMB THERM KT')==1) THEN
       !!****e* therMAILx/ASSEMB THERM KT
       !! NAME
       !!  ASSEMB THERM KT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/assemb_KT_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL assemb_KT_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'ASSEMB THERM RHS')==1) THEN
       !!****e* therMAILx/ASSEMB THERM RHS
       !! NAME
       !!  ASSEMB THERM RHS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/assemb_RHS_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL assemb_RHS_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

!!! construction of the iteration matrix and the corresponding right hand side vector

    IF (INDEX(CHZZZ,'TRIAL ASSEMB THERM KT')==1) THEN
       !!****e* therMAILx/TRIAL ASSEMB THERM KT
       !! NAME
       !!  TRIAL_ASSEMB_THERM_KT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/trial_assemb_KT_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL trial_assemb_KT_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'TRIAL ASSEMB THERM RHS')==1) THEN
       !!****e* therMAILx/TRIAL ASSEMB THERM RHS
       !! NAME
       !!  TRIAL ASSEMB THERM RHS
       !! PURPOSE
       !!  trial_assemb_RHS_therMAILx
       !! USES
       !!  LMGC90.CORE/therMAILx/
       !!****
       CALL LOGCHIC('therMAILx')
       CALL trial_assemb_RHS_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INCREMENT STEP')==1) THEN
       !!****e* therMAILx/INCREMENT STEP
       !! NAME
       !!  INCREMENT STEP
       !! PURPOSE
       !!  increment_therMAILx
       !! USES
       !!  LMGC90.CORE/therMAILx/
       !!****
       CALL LOGCHIC('therMAILx')
       CALL increment_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE THERM DOF')==1) THEN
       !!****e* therMAILx/COMPUTE THERM DOF
       !! NAME
       !!  COMPUTE THERM DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/comp_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL comp_dof_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE THERM DOF')==1) THEN
       !!****e* therMAILx/UPDATE THERM DOF
       !! NAME
       !!  UPDATE THERM DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/update_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL update_dof_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE THERM BULK')==1) THEN
       !!****e* therMAILx/UPDATE THERM BULK
       !! NAME
       !!  UPDATE THERM BULK
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/update_therm_bulk_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL update_therm_bulk_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CHECK NL CONVERGENCE')==1) THEN
       !!****e* therMAILx/CHECK NL CONVERGENCE
       !! NAME
       !!  CHECK NL CONVERGENCE
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/check_ther_convergence_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       iconv = 1
       CALL check_ther_convergence_therMAILx(iconv)
       nb_therMAILx = get_nb_therMAILx()
       IF (nb_therMAILx .NE. 0) IFLZZZ(NZZZ,1) = iconv
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------------------------

    IF (INDEX(CHZZZ,'READ DRIVEN DOF')==1) THEN
       !!****e* therMAILx/READ DRIVEN DOF
       !! NAME
       !!  READ DRIVEN DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_in_driven_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL read_in_driven_dof_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'LOAD MODELS')==1) THEN
       !!****e* therMAILx/LOAD MODELS
       !! NAME
       !!  READ MODELS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_models_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL read_models_therMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'LOAD BEHAVIOURS')==1) THEN
       !!****e* therMAILx/LOAD BEHAVIOURS
       !! NAME
       !!  READ BEHAVIOURS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_behaviours_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL read_behaviours_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    !am: mot clef foireux
    !IF (INDEX(CHZZZ,'READ_INI_DOF')==1) THEN
    IF (INDEX(CHZZZ,'READ INI DOF')==1) THEN
       !!****e* therMAILx/READ INI DOF
       !! NAME
       !!  READ INI DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_in_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL read_in_dof_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ INI GPV')==1) THEN
       !!****e* therMAILx/READ INI GPV
       !! NAME
       !!  READ INI GPV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_in_gpv_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       CALL read_in_gpv_therMAILx 
       IETZZZ = 1
       RETURN      
    END IF

!!!--------------------------------------------------------------------

    IF (INDEX(CHZZZ,'WRITE LAST DOF')==1) THEN     
       !!****e* therMAILx/WRITE LAST DOF
       !! NAME
       !!  WRITE LAST DOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/write_xxx_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_therMAILx = get_nb_therMAILx()
          ifrom = 1  
          ito   = nb_therMAILx
          CALL write_xxx_dof_therMAILx(2,ifrom,ito)
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT DOF')==1) THEN     
       !!****e* therMAILx/WRITE OUT DOF
       !! NAME
       !!  WRITE OUT DOF
       !! PURPOSE
       !!  write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/therMAILx/write_xxx_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       write_DOF = get_write_DOF_therMAILx()
       IF (write_DOF) THEN
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN 
             nb_therMAILx = get_nb_therMAILx()
             ifrom = 1  
             ito   = nb_therMAILx
             CALL write_xxx_dof_therMAILx(1,ifrom,ito)
          ELSE
             IGOZZZ=0
             DO
                IGOZZZ=IGOZZZ+1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*) ifrom,ito
                      nb_therMAILx = get_nb_therMAILx()
                      ifrom=max0(ifrom,1)
                      ito=min0(ito,nb_therMAILx)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_dof_therMAILx(1,ifrom,ito)
                   END IF
                ELSE
                   EXIT
                END IF
                CYCLE
             END DO
          END IF
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT DOF')==1) THEN
       !!****e* therMAILx/DISPLAY OUT DOF
       !! NAME
       !!  DISPLAY OUT DOF
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/therMAILx/write_xxx_dof_therMAILx
       !!****
       CALL LOGCHIC('therMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_therMAILx = get_nb_therMAILx()
          ifrom = 1  
          ito   = nb_therMAILx
          CALL write_xxx_dof_therMAILx(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   nb_therMAILx = get_nb_therMAILx()
                   ifrom=max0(ifrom,1)
                   ito=min0(ito,nb_therMAILx)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_dof_therMAILx(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST Rnod')==1) THEN     
       !!****e* therMAILx/WRITE LAST Rnod
       !! NAME
       !!  WRITE LAST Rnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  
       !!****
       CALL LOGCHIC('therMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_therMAILx = get_nb_therMAILx()
          ifrom = 1  
          ito   = nb_therMAILx
          !CALL write_xxx_Rnod_therMAILx(2,ifrom,ito)
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT Rnod')==1) THEN     
       !!****e* therMAILx/WRITE OUT Rnod
       !! NAME
       !!  WRITE OUT Rnod
       !! PURPOSE
       !!  write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/therMAILx/
       !!****
       CALL LOGCHIC('therMAILx')
       write_Rnod = get_write_Rnod_therMAILx()
       IF (write_Rnod) THEN
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN 
             nb_therMAILx = get_nb_therMAILx()
             ifrom = 1  
             ito   = nb_therMAILx
             !CALL write_xxx_Rnod_therMAILx(1,ifrom,ito)
          ELSE
             IGOZZZ=0
             DO
                IGOZZZ=IGOZZZ+1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*) ifrom,ito
                      nb_therMAILx = get_nb_therMAILx()
                      ifrom=max0(ifrom,1)
                      ito=min0(ito,nb_therMAILx)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      !CALL write_xxx_Rnod_therMAILx(1,ifrom,ito)
                   END IF
                ELSE
                   EXIT
                END IF
                CYCLE
             END DO
          END IF
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT Rnod')==1) THEN
       !!****e* therMAILx/DISPLAY OUT Rnod
       !! NAME
       !!  DISPLAY OUT Rnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/therMAILx/
       !!****
       CALL LOGCHIC('therMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_therMAILx = get_nb_therMAILx()
          ifrom = 1  
          ito   = nb_therMAILx
          !CALL write_xxx_Rnod_therMAILx(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   nb_therMAILx = get_nb_therMAILx()
                   ifrom=max0(ifrom,1)
                   ito=min0(ito,nb_therMAILx)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   !CALL write_xxx_Rnod_therMAILx(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF

  END SUBROUTINE chic_command_therMAILx
!!!----------------------------------------------------

  include '../../more_src/mailx/wrap_therMAILx.inc'

END MODULE wrap_therMAILx
