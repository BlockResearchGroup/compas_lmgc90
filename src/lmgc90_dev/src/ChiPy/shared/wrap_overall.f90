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
MODULE wrap_overall
 
  USE ISO_C_BINDING

  USE utilities
  USE overall, ONLY: &
      Write_out_bodies_Ol, &
      Clean_out_bodies,&
      Clean_in_bodies, &
      Write_out_driven_dof_Ol, &
      Read_in_dof_Ol, &
      Write_xxx_dof_Ol, &
      Read_in_Vloc_Rloc_Ol, &
      Write_xxx_Vloc_Rloc_Ol,&
      Read_in_gpv_Ol, &
      Write_xxx_gpv_Ol,&
      Display_prox_tactors_Ol,&
      Write_xxx_Rnod_Ol,&
      Init_entitylist, &
      is_EntityList_Initialized, &
      time_increment, & !!      Updt_time, &
      Updt_time_begin, & 
      Set_newton_tolerance, &
      Set_newton_maxloop, &
      Set_newton_badloop, &
      Set_newton_goodloop, &
      Set_newton_rate_step, & 
      Set_newton_loop, &
      Incre_newton_loop, &
      check_newton_convergence, &
      Compute_newton_time_step, &
      DISPLAY_TIME, &
      Clean_writing_flags, &
      Get_NSTEP, &
      get_time, &
      get_time_step, &
      Init_dimension,&
      Set_time_step, &
      Init_gear_integrator, &
      Init_theta_integrator, &
      init_verlet_integrator, &
      Init_CN_integrator, &
      Init_beta2_integrator, &
      Init_QS_integrator, &      
      Set_Contact_Detection_Configuration, &
      Set_initial_step, &
      Set_initial_time, &
      Set_final_time, &
      Set_min_time_step, &
      Set_max_time_step, &
      Acti_large_computation, &
      set_run_contactor, &
      init_post_data_ol, &
      update_post_data_ol, &
      Read_in_mp_values_Ol, &
      write_xxx_MP_values_Ol, &
      set_working_directory, &
      location, &
      set_with_experimental_dev, &
      set_is_externalFEM, &
      io_last_Vloc_Rloc, & ! pta & am
      io_out_Vloc_Rloc, & ! pta & am
      overall_clean_memory, &
      max_internal_tact

  IMPLICIT NONE
  PUBLIC 

  CONTAINS


!---------------------------------------------------------------------
! gestion de l evolution en temps
!---------------------------------------------------------------------


!---------------------------------------------------------------------

    SUBROUTINE SetTimeStep(value) bind(c, name='TimeEvolution_SetTimeStep')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL set_time_step(value)        

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetInitialTime(value) bind(c, name='TimeEvolution_SetInitialTime')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL set_initial_time(value)

    END SUBROUTINE


!---------------------------------------------------------------------

    SUBROUTINE SetInitialStep(ivalue) bind(c, name='TimeEvolution_SetInitialStep')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: ivalue

       CALL set_initial_step(ivalue)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE IncrementStep() bind(c, name='TimeEvolution_IncrementStep')
       !! PURPOSE
       !!  increment time, time step, initialize NR loop counter
       IMPLICIT NONE

       CALL  time_increment                  !Updt_time

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE UpdateStep() bind(c, name='TimeEvolution_UpdateStep')
       IMPLICIT NONE

       CALL Updt_time_begin

    END SUBROUTINE
!---------------------------------------------------------------------

    function GetTimeStep() bind(c, name='TimeEvolution_GetTimeStep')
      implicit none
      real(C_DOUBLE)::GetTimeStep

      GetTimeStep = get_time_step()

    end function

!---------------------------------------------------------------------

    function GetTime() bind(c, name='TimeEvolution_GetTime')
      IMPLICIT NONE
      real(C_DOUBLE)::GetTime

      GetTime=get_time()

    end function GetTime

    function GetStep() bind(c, name='TimeEvolution_GetStep')
      IMPLICIT NONE
      integer(c_int)::GetStep

      GetStep=get_nstep()

    end function GetStep

!---------------------------------------------------------------------

    SUBROUTINE DisplayTimes() bind(c, name='TimeEvolution_DisplayStep')
       IMPLICIT NONE

       CALL DISPLAY_TIME

    END SUBROUTINE


    SUBROUTINE WriteLastDof() bind(c, name='TimeEvolution_WriteLastDof')
       IMPLICIT NONE

       CALL Write_xxx_dof_Ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutDof(Nstep_writeDof) bind(c, name='TimeEvolution_WriteOutDof')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: Nstep_writeDof 
       INTEGER :: NSTEP

        NSTEP = get_NSTEP()
        IF (MODULO(Nstep,Nstep_writeDof ) /= 0) RETURN

        CALL write_xxx_dof_Ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayOutDof() bind(c, name='TimeEvolution_DisplayOutDof')
       IMPLICIT NONE 

       CALL write_xxx_dof_Ol(6)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastRnod() bind(c, name='TimeEvolution_WriteLastRnod')
       IMPLICIT NONE 

       CALL write_xxx_Rnod_Ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutRnod(entier) bind(c, name='TimeEvolution_WriteOutRnod')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: entier
       INTEGER :: NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN

       CALL write_xxx_Rnod_Ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayOutRnod() bind(c, name='TimeEvolution_DisplayOutRnod')
       IMPLICIT NONE 

       CALL write_xxx_Rnod_Ol(6)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastVlocRloc() bind(c, name='TimeEvolution_WriteLastVlocRloc')
       IMPLICIT NONE

       CALL write_xxx_Vloc_Rloc_Ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutVlocRloc(entier) bind(c, name='TimeEvolution_WriteOutVlocRloc')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: entier
       INTEGER :: NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN

       CALL write_xxx_Vloc_Rloc_Ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayOutVlocRloc() bind(c, name='TimeEvolution_DisplayOutVlocRloc')
       IMPLICIT NONE 

       CALL write_xxx_Vloc_Rloc_Ol(6)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteLastGPV() bind(c, name='TimeEvolution_WriteLastGPV')
       IMPLICIT NONE 

       CALL write_xxx_gpv_ol(2)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutGPV(entier) bind(c, name='TimeEvolution_WriteOutGPV')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: entier
       INTEGER :: NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,entier) /= 0) RETURN

       CALL write_xxx_gpv_ol(1)

    END SUBROUTINE

!---------------------------------------------------------------------

    subroutine ReadIniDof(num) bind(c, name='TimeEvolution_ReadIniDof')
       implicit none 
       integer(c_int), intent(in), value :: num

       call read_in_dof_ol(num)

    end subroutine

!---------------------------------------------------------------------

    subroutine ReadIniVlocRloc(num) bind(c, name='TimeEvolution_ReadIniVlocRloc')
       implicit none 
       integer(c_int), intent(in), value :: num

       call read_in_Vloc_Rloc_ol(num)

    end subroutine
!---------------------------------------------------------------------

    subroutine ReadIniGPV(num) bind(c, name='TimeEvolution_ReadIniGPV')
       implicit none 
       integer(c_int), intent(in), value :: num

       call read_in_gpv_ol(num)

    end subroutine



!---------------------------------------------------------------------
! gestion de newton raphson
!---------------------------------------------------------------------

    SUBROUTINE InitializeNewton(value) bind(c, name='NewtonRaphson_Initialize')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL set_newton_tolerance(value) 
       CALL set_newton_loop(0)

    END SUBROUTINE

!---------------------------------------------------------------------

    function CheckNewtonConvergence(value) bind(c, name='NewtonRaphson_CheckConvergence')
       IMPLICIT NONE 
       INTEGER(C_INT) :: CheckNewtonConvergence
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value
       integer :: iconv

       CALL incre_newton_loop
       call check_newton_convergence(value,iconv)

       CheckNewtonConvergence = iconv

    END function

!---------------------------------------------------------------------

    function ComputeTimeStep() bind(c, name='NewtonRaphson_ComputeTimeStep')
       IMPLICIT NONE 
       INTEGER(C_INT) :: ComputeTimeStep
       !
       integer :: ishootagain, igameover

       CALL Compute_newton_time_step(ishootagain,igameover)

       ComputeTimeStep = 0

       !istop=0 on arrete le calcul
       if (igameover == 0) then
         ComputeTimeStep = 2
         return
       end if

       !irestart=0 on recommence le pas
       if (ishootagain == 0) then
         ComputeTimeStep = 1
         return
       end if
 
    END function

!---------------------------------------------------------------------

    SUBROUTINE SetMinTimeStep(value) bind(c, name='NewtonRaphson_SetMinTimeStep')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL set_min_time_step(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetMaxTimeStep(value) bind(c, name='NewtonRaphson_SetMaxTimeStep')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL set_max_time_step(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetFinalTime(value) bind(c, name='NewtonRaphson_SetFinalTime')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL set_final_time(value)

    END SUBROUTINE

!---------------------------------------------------------------------

!---------------------------------------------------------------------

    SUBROUTINE SetNewtonMaxIter(value) bind(c, name='NewtonRaphson_SetMaxIter')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: value

       ! un entier ou un reel ?
       CALL set_newton_maxloop(value)       

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE SetNewtonGoodIter(value) bind(c, name='NewtonRaphson_SetGoodIter')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: value

       ! un entier ou un reel ?
       CALL set_newton_goodloop(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE SetNewtonBadIter(value) bind(c, name='NewtonRaphson_SetBadIter')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: value

       ! un entier ou un reel ?
       CALL set_newton_badloop(value)

    END SUBROUTINE
!---------------------------------------------------------------------

    SUBROUTINE SetNewtonIncPatience(value) bind(c, name='NewtonRaphson_SetIncPatience')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: value

       CALL set_newton_rate_step(value) 

    END SUBROUTINE

!---------------------------------------------------------------------
! initialisation modelisation
!---------------------------------------------------------------------
    SUBROUTINE DIME(idim,imod) bind(c, name='overall_DIME')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: idim
       INTEGER(C_INT), INTENT(IN), VALUE :: imod
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

    END SUBROUTINE

!---------------------------------------------------------------------
!---------------------------------------------------------------------
! initialisation integrateurs
!---------------------------------------------------------------------

    SUBROUTINE InitThetaIntegrator(value) bind(c, name='Integrator_InitTheta')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL init_theta_integrator(value)       

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitQSIntegrator() bind(c, name='Integrator_InitQS')
       IMPLICIT NONE 

       CALL init_qs_integrator()       

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitCrankNickolsonIntegrator(value) bind(c, name='Integrator_InitCrankNickolson')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: value

       CALL init_CN_integrator(value)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitGearIntegrator() bind(c, name='Integrator_InitGear')
       IMPLICIT NONE 

       CALL init_gear_integrator

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE InitVerletIntegrator() bind(c, name='Integrator_InitVerlet')
       IMPLICIT NONE 

       CALL init_verlet_integrator

    END SUBROUTINE
    
 !---------------------------------------------------------------------

    SUBROUTINE InitBeta2Integrator(value) bind(c, name='Integrator_InitBeta2')
       IMPLICIT NONE 
       REAL(C_DOUBLE) , INTENT(IN), VALUE :: value

       CALL init_beta2_integrator(value)

    END SUBROUTINE   

!---------------------------------------------------------------------

    SUBROUTINE SetContactDetectionConfiguration(alpha_b,alpha_e) bind(c, name='Integrator_SetContactDetectionConfiguration')
       IMPLICIT NONE 
       REAL(C_DOUBLE), INTENT(IN), VALUE :: alpha_b,alpha_e

       CALL Set_Contact_Detection_Configuration(alpha_b,alpha_e)       

    END SUBROUTINE


!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

    SUBROUTINE RequireXxlComputation() bind(c, name='overall_RequireXxlComputation')
       IMPLICIT NONE 

       CALL acti_large_computation

    END SUBROUTINE

!---------------------------------------------------------------------

    subroutine UseExperimentalDev() bind(c, name='overall_UseExperimentalDev')
      !! PURPOSE
      !!   activate some unstable devs
      implicit none

      call set_with_experimental_dev

    end subroutine

!---------------------------------------------------------------------

    subroutine UseExternalFem() bind(c, name='overall_UseExternalFem')
      !! PURPOSE
      !!   allow to use the externalFem library instead of lmgc90 Fem lib
      implicit none

      call set_is_externalFEM

    end subroutine

!---------------------------------------------------------------------


!---------------------------------------------------------------------
! detection de contact
!---------------------------------------------------------------------

    SUBROUTINE SelectProxTactors(Nstep_rough_seek) bind(c, name='overall_SelectProxTactors')
       !! PURPOSE
       !!  Prepare contact detection
       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN), VALUE :: Nstep_rough_seek

       ! needed... even if initialized !!!
       call init_entitylist()

       CALL set_run_contactor(Nstep_rough_seek)

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE DisplayProxTactors() bind(c, name='overall_DisplayProxTactors')
       IMPLICIT NONE

       CALL display_prox_tactors_Ol

    END SUBROUTINE


!---------------------------------------------------------------------
! ou on travaille
!---------------------------------------------------------------------

    SUBROUTINE SetWorkingDirectory(cvalue1) bind(c, name='overall_SetWorkingDirectory')
      implicit none
      character(C_CHAR), dimension(*) :: cvalue1
      !
      character(len=256) :: cvalue1_f
      integer :: i

      cvalue1_f = ''
      do i=1,len(cvalue1_f)
          if( cvalue1(i) == c_null_char ) exit
          cvalue1_f = cvalue1_f(1:i-1) // cvalue1(i)
      end do

      call Set_Working_Directory(cvalue1_f(1:i))

    END SUBROUTINE

!---------------------------------------------------------------------

    subroutine GetWorkingDirectory(string_out, string_size, real_size) bind(c, name='overall_GetWorkingDirectory')
      implicit none
      type(c_ptr) :: string_out
      integer(c_int), intent(out) :: string_size, real_size
      !
      character(len=256), pointer :: working_dir
      character(c_char) , pointer :: gniii

      allocate(working_dir)
      working_dir = location('')

      string_size = len(trim(working_dir))
      real_size   = len(working_dir)

      gniii => working_dir(1:1)
      string_out = c_loc(gniii)

    end subroutine

!---------------------------------------------------------------------
! write des entetes
!---------------------------------------------------------------------


!---------------------------------------------------------------------

    SUBROUTINE WriteDrivenDof() bind(c, name='overall_WriteDrivenDof')
       IMPLICIT NONE

       CALL write_out_driven_dof_Ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteBodies() bind(c, name='overall_WriteBodies')
       IMPLICIT NONE 

       CALL write_out_bodies_ol

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE WriteOutDisplayFile(freq_display) bind(c, name='overall_WriteOutDisplayFile')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE:: freq_display

       call faterr('overall_WriteOutDisplayFile','Obsolete function : please python WriteDisplayFile instead with vtk python module')

     END SUBROUTINE

!---------------------------------------------------------------------

    subroutine ReadIniMpValues(num) bind(c, name='TimeEvolution_ReadIniMpValues')
       implicit none 
       integer(c_int), intent(in), value :: num

       call read_in_mp_values_ol(num)

    end subroutine

    SUBROUTINE WriteOutMpValues(Nstep_writeMpv) bind(c, name='TimeEvolution_WriteOutMpValues')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: Nstep_writeMpv
       INTEGER :: NSTEP

       NSTEP = get_NSTEP()
       IF (MODULO(Nstep,Nstep_writeMpv) /= 0) RETURN

       call write_xxx_mp_values_Ol(1)

    END SUBROUTINE

    subroutine WriteLastMpValues() bind(c, name='TimeEvolution_WriteLastMpValues')
       implicit none

       call write_xxx_mp_values_Ol(2)

    end subroutine

!---------------------------------------------------------------------

    SUBROUTINE CleanWriteOutFlags() bind(c, name='overall_CleanWriteOutFlags')
      implicit none

      call Clean_writing_flags

    END SUBROUTINE


!---------------------------------------------------------------------
! lecture des entetes
!---------------------------------------------------------------------

    SUBROUTINE CleanOutBodies() bind(c, name='overall_CleanOutBodies')
       IMPLICIT NONE 

       CALL clean_out_bodies

    END SUBROUTINE

!---------------------------------------------------------------------

    SUBROUTINE RebuildInBodies() bind(c, name='overall_RebuildInBodies')
       IMPLICIT NONE 

       CALL clean_in_bodies

    END SUBROUTINE


!%< ----

! obsolete

!
! pour le postraitement post-mortem
!

    SUBROUTINE UpdatePostData() bind(c, name='overall_UpdatePostData')
       IMPLICIT NONE 
       INTEGER :: info

       CALL update_post_data_ol(info)

       IF(info == 0)THEN
          print*,'LAST FILE HAVE BEEN READ'
          print*,'POST PROCESSING IS OVER'
          STOP
       END IF

    END SUBROUTINE

!---------------------------------------------------------------------
   
    SUBROUTINE InitPostData(ifirst,ilast) bind(c, name='overall_InitPostData')
       IMPLICIT NONE 
       INTEGER(C_INT), INTENT(IN), VALUE :: ifirst,ilast

       CALL init_post_data_ol(ifirst,ilast)

    END SUBROUTINE

!---------------------------------------------------------------------

     subroutine Initialize() bind(c, name='overall_Initialize')
       use LMGC90_MPI
       implicit none

       call init_MPI
       call start_MPI_time

     end subroutine

     subroutine Finalize() bind(c, name='overall_Finalize')
       use LMGC90_MPI
       implicit none

       call stop_MPI_time
       call mpi_finalize_process

       call overall_clean_memory

     end subroutine

     function isMPI() bind(c, name='overall_IsMPI')
       use LMGC90_MPI
       implicit none
       integer(kind=c_int) :: isMPI

       isMPI = 0
       if( with_MPI ) isMPI = 1

     end function

     subroutine InitEntityList() bind(c, name='overall_InitEntityList')
       implicit none

       if ( .not. is_EntityList_Initialized() ) then
           call init_entitylist()
       end if

     end subroutine

!------------------------------------------------------------------------

    function GetMaxInternalTact() bind(c, name='overall_GetMaxInternalTact')
      implicit none
      integer(C_INT) :: GetMaxInternalTact

      GetMaxInternalTact = max_internal_tact

    end function

 END MODULE
