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
MODULE wrap_overall
  

  !!****h* LMGC90.CHIPY/OVERALL
  !! NAME
  !!  module wrap_OVERALL
  !! PURPOSE
  !!  Wrap the public API
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/OVERALL
  !!****

  USE utilities
  USE overall, ONLY: &
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
      time_increment, & !!      Updt_time, &
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
      get_time, &
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
      write_out_MP_values_Ol, &
      set_working_directory, &
      set_with_experimental_dev

  IMPLICIT NONE
  PUBLIC 

  CONTAINS

!---------------------------------------------------------------------

    SUBROUTINE SelectProxTactors(Nstep_rough_seek)
       !!****e* OVERALL/SelectProxTactors
       !! NAME 
       !!  SelectProxTactors
       !! SYNOPSIS
       !!  SelectProxTactors(Nstep_rough_seek)
       !! INPUTS
       !!  Integer,optional::Nstep_rough_seek periodicity of rough update
       !! PURPOSE
       !!  Prepare contact detection
       !! USES
       !!  LMGC90.CORE/OVERALL/Clean_EntityList
       !!  LMGC90.CORE/OVERALL/set_run_contactor
       !!****

       IMPLICIT NONE

       INTEGER,optional :: Nstep_rough_seek
                               !1234567890123456789012345678901
       CHARACTER(LEN=31):: IAM='Wrap_Overall::SelectProxTactors'

       CALL LOGMES('Entering: '//IAM)

       CALL Clean_EntityList

       if (.NOT. PRESENT(Nstep_rough_seek)) Nstep_rough_seek=1

       CALL set_run_contactor(Nstep_rough_seek)

       CALL LOGMES('Living: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE IncrementStep
       !!****e* OVERALL/IncrementStep
       !! NAME 
       !!  IncrementStep
       !! SYNOPSIS
       !!  Increment(Nstep_rough_seek)
       !! INPUTS
       !!  None
       !! PURPOSE
       !!  increment time, time step, initialize NR loop counter
       !! USES
       !!  LMGC90.CORE/OVERALL/Updt_time
       !!  LMGC90.CORE/OVERALL/Set_newton_loop(0)
       !!****

       IMPLICIT NONE

                               !123456789012345678901234567
       CHARACTER(LEN=27):: IAM='Wrap_Overall::IncrementStep'

       CALL LOGMES('Entering: '//IAM)

       CALL  time_increment                  !Updt_time
       CALL  Set_newton_loop(0)

       CALL LOGMES('Living: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE UpdateDof
       !!****e* OVERALL/UpdateDof
       !! NAME 
       !!  UpdateDof
       !! SYNOPSIS
       !!  UpdateDof
       !! INPUTS
       !!  None
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/Updt_time_begin
       !!****

       IMPLICIT NONE

                               !12345678901234567890123
       CHARACTER(LEN=23):: IAM='Wrap_Overall::UpdateDof'

       CALL LOGMES('Entering: '//IAM)

       CALL Updt_time_begin

       CALL LOGMES('Living: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastDof
       !!****e* OVERALL/WriteLastDof
       !! NAME 
       !!  WriteLastDof
       !! SYNOPSIS
       !!  WriteLastDof
       !! INPUTS
       !!  None
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/Write_xxx_dof_Ol(2)
       !!****

       IMPLICIT NONE
                               !12345678901234567890123456
       CHARACTER(LEN=26):: IAM='Wrap_Overall::WriteLastDof'

       CALL LOGMES('Entering: '//IAM)

       CALL Write_xxx_dof_Ol(2)

       CALL LOGMES('Living: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE BinaryWriteLastDof
       !!****e* OVERALL/BinaryWriteLastDof
       !! NAME 
       !!  BinaryWriteLastDof
       !! SYNOPSIS
       !!  BinaryWriteLastDof
       !! INPUTS
       !!  None
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/BinOpen_last_dof
       !!  LMGC90.CORE/OVERALL/BinWrite_last_dof_Ol
       !!****

       IMPLICIT NONE
                               !12345678901234567890123456
       CHARACTER(LEN=26):: IAM='Wrap_Overall::BinaryWriteLastDof'

       CALL LOGMES('Entering: '//IAM)

       CALL BinOpen_last_dof
       CALL BinWrite_last_dof_Ol

       CALL LOGMES('Living: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutGMV(Nstep_writeGMV)
       !!****e* OVERALL/WriteOutGMV
       !! NAME 
       !!  WriteOutGMV
       !! SYNOPSIS
       !!  WriteOutGMV(Nstep_writeGMV)
       !! INPUTS
       !!  Integer,optional::Nstep_writeGMV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_out_gmv_Ol
       !!****

       IMPLICIT NONE 

       INTEGER,optional :: Nstep_writeGMV
       INTEGER :: NSTEP

                               !12345678901234567890123456
       CHARACTER(LEN=25):: IAM='Wrap_Overall::WriteOutGMV'

       CALL LOGMES('Entering: '//IAM)

       if (.NOT. PRESENT(Nstep_writeGMV)) Nstep_writeGMV=1

       NSTEP = get_NSTEP() 
       IF (MODULO(Nstep,Nstep_writeGMV) /= 0) RETURN

       CALL write_out_gmv_Ol

       CALL LOGMES('Living: '//IAM)

     END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutDof(Nstep_writeDof)
       !!****e* OVERALL/WriteOutDof
       !! NAME 
       !!  WriteOutDof
       !! SYNOPSIS
       !!  WriteOutDof(Nstep_writeDof)
       !! INPUTS
       !!  Integer,optional::Nstep_writeDof
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/write_xxx_dof_Ol(1)
       !!****

       IMPLICIT NONE 
       INTEGER,optional :: Nstep_writeDof 
       INTEGER :: NSTEP

                               !1234567890123456789012345
       CHARACTER(LEN=25):: IAM='Wrap_Overall::WriteOutDof'

       CALL LOGMES('Entering: '//IAM)

       if (.NOT. PRESENT(Nstep_writeDof)) Nstep_writeDof=1

        NSTEP = get_NSTEP()
        IF (MODULO(Nstep,Nstep_writeDof ) /= 0) RETURN

        CALL write_xxx_dof_Ol(1)

        CALL LOGMES('Entering: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE BinaryWriteOutDof(Nstep_writeDof)
       !!****e* OVERALL/BinaryWriteOutDof
       !! NAME 
       !!  BinaryWriteOutDof
       !! SYNOPSIS
       !!  BinaryWriteOutDof(Nstep_writeDof)
       !! INPUTS
       !!  Integer,optional::Nstep_writeDof
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/OVERALL/binarywrite_out_dof_ol
       !!****

       IMPLICIT NONE 
       INTEGER,optional :: Nstep_writeDof 
       INTEGER :: NSTEP

                               !1234567890123456789012345678901
       CHARACTER(LEN=31):: IAM='Wrap_Overall::BinaryWriteOutDof'

       CALL LOGMES('Entering: '//IAM)

       if (.NOT. PRESENT(Nstep_writeDof)) Nstep_writeDof=1

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,Nstep_writeDof) /= 0) RETURN

       CALL binwrite_out_dof_ol

        CALL LOGMES('Entering: '//IAM)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayOutDof

       IMPLICIT NONE 

       CALL write_xxx_dof_Ol(6)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastRnod

       IMPLICIT NONE 

       CALL write_xxx_Rnod_Ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutMPBehav(entier)

       IMPLICIT NONE 
       INTEGER :: entier, NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN

       CALL write_out_mp_values_Ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutRnod(entier)

       IMPLICIT NONE 
       INTEGER :: entier, NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN

       CALL write_xxx_Rnod_Ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayOutRnod

       IMPLICIT NONE 

       CALL write_xxx_Rnod_Ol(6)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteDrivenDof

       IMPLICIT NONE

       CALL write_out_driven_dof_Ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayTimes

       IMPLICIT NONE

       CALL DISPLAY_TIME

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayProxTactors

       IMPLICIT NONE

       CALL display_prox_tactors_Ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastVlocRloc

       IMPLICIT NONE

       CALL write_xxx_Vloc_Rloc_Ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutVlocRloc(entier)

       IMPLICIT NONE 
       INTEGER :: entier, NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN


       CALL write_xxx_Vloc_Rloc_Ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayOutVlocRloc

       IMPLICIT NONE 

       CALL write_xxx_Vloc_Rloc_Ol(6)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastGpv

       IMPLICIT NONE 

       CALL write_xxx_gpv_ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutGpv(entier)

       IMPLICIT NONE 
       INTEGER :: entier, NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN

       CALL write_xxx_gpv_ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE CheckNlConvergence(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL set_newton_tolerance(value) 
       CALL incre_newton_loop


    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE ComputeTimeStep(irestart,istop)

       IMPLICIT NONE 
       INTEGER :: irestart,istop

       CALL Comp_NR_time_step(irestart,istop)

       !irestart=0 on recommence le pas
       !istop=0 on arrete le calcul

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DIME(idim,imod)

       IMPLICIT NONE 
       INTEGER :: idim
       INTEGER,optional :: imod
       CHARACTER(LEN=10) :: chaine10

       if (idim == 3) then 

                    !1234567890
         chaine10 = '3D        '

       else if (idim == 2) then

         if ( imod == 1) then 
         
                      !1234567890
           chaine10 = '2D PSTRAIN'

         else if ( imod == 2) then 

                      !1234567890
           chaine10 = '2D PSTRESS'

         else if ( imod == 3) then 

                      !1234567890
           chaine10 = '2D AXI    '

         else

           print*,'ERROR: Unsupported dimension' 
           stop

         endif

       else

         print*,'ERROR: Unsupported dimension' 
         stop

       endif

       CALL init_dimension(chaine10)

       print*,chaine10

       RETURN      
    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitThetaIntegrator(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL init_theta_integrator(value)       
       CALL init_CN_integrator(value)


    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitCrankNickolsonIntegrator(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL init_CN_integrator(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetTimeStep(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL set_time_step(value)        

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetMinTimeStep(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL set_min_time_step(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetMaxTimeStep(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetFinalTime(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value
       CALL set_final_time(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE CheckNewtonConvergence(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL set_newton_tolerance(value) 
       CALL incre_newton_loop


    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE SetNewtonMaxIter(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

! un entier ou un reel ?

       CALL set_newton_maxloop(value)       

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE SetNewtonGoodIter(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

! un entier ou un reel ?

       CALL set_newton_goodloop(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetNewtonBadIter(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

! un entier ou un reel ?

       CALL set_newton_badloop(value)

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE SetNewtonIncPatience(value)

       IMPLICIT NONE 
       REAL(KIND=8) :: value

       CALL set_newton_rate_step(value) 

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE WriteBodies

       IMPLICIT NONE 

       CALL write_out_bodies_ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE ReadIniDof

       IMPLICIT NONE 

       CALL read_in_dof_ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE BinaryReadIniDof

       IMPLICIT NONE 

       CALL binread_in_dof_ol

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE ReadIniVlocRloc

       IMPLICIT NONE 

       CALL read_in_Vloc_Rloc_ol

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE ReadIniGpv

       IMPLICIT NONE 

       CALL read_in_gpv_ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE CleanOutBodies

       IMPLICIT NONE 

       CALL clean_out_bodies

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE RebuildInBodies

       IMPLICIT NONE 

       CALL clean_in_bodies

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE RequireXxlComputation

       IMPLICIT NONE 

       CALL acti_large_computation

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitGearIntegrator

       IMPLICIT NONE 

       CALL init_gear_integrator

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitVerletIntegrator

       IMPLICIT NONE 

       CALL init_verlet_integrator

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE UpdatePostData

       IMPLICIT NONE 
       INTEGER :: info

       CALL update_post_data_ol(info)

       IF(info == 0)THEN
          print*,'LAST FILE HAVE BEEN READ'
          print*,'POST PROCESSING IS OVER'
          STOP
       END IF

    END SUBROUTINE
   
    SUBROUTINE InitPostData(ifirst,ilast)

       IMPLICIT NONE 
       INTEGER :: ifirst,ilast

       CALL init_post_data_ol(ifirst,ilast)

    END SUBROUTINE

    SUBROUTINE SetWorkingDirectory(cvalue1)
      implicit none
      character(len=*):: cvalue1

      call Set_Working_Directory(cvalue1)

    END SUBROUTINE

    SUBROUTINE CleanWriteOutFlags()
      implicit none

      call Clean_writing_flags

    END SUBROUTINE

    function GetTime()

      IMPLICIT NONE
	real(kind=8)::GetTime
    
        GetTime=get_time()

    end function GetTime

    SUBROUTINE WithExperimentalDev()
      implicit none

      call set_with_experimental_dev

    END SUBROUTINE

 END MODULE

