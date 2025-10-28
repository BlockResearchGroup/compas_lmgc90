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
MODULE wrap_mecaMAILx                                       

  !!****h* LMGC90.CHIPY/mecaMAILx  
  !! NAME
  !!  module wrap_mecaMAILx  
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/mecaMAILx  
  !!****

  USE mecaMAILx,ONLY:&
       get_nb_mecaMAILx, &
       compute_bulk_hpp_mecaMAILx, &
       update_bulk_mecaMAILx, &
       compute_free_vlocy_mecaMAILx, &
       assemb_KT_mecaMAILx, &
       assemb_RHS_mecaMAILx, &
!!$       compute_Fint_mecaMAILx, &
       check_nl_convergence_mecaMAILx, &
       compute_nl_free_vlocy_mecaMAILx, &
       assemb_nl_KT_mecaMAILx, &
       assemb_nl_RHS_mecaMAILx, &
       compute_bulk_gd_mecaMAILx, &
       update_dof_mecaMAILx, &
       compute_dof_mecaMAILx, &
       increment_mecaMAILx, &
       compute_Fext_mecaMAILx, &
       compute_mass_mecaMAILx, &
       fatal_damping_mecaMAILx,&
       check_equilibrium_state_mecaMAILx, &
       set_data_equilibrium_mecaMAILx, &
       read_in_driven_dof_mecaMAILx, &
       write_out_driven_dof_mecaMAILx, &
       read_in_gpv_mecaMAILx, &
       read_in_dof_mecaMAILx, &
       write_xxx_dof_mecaMAILx, &
       write_xxx_Rnod_mecaMAILx, &
       load_behaviours_mecaMAILx, &
       write_xxx_nodforces, &
       update_existing_entities_mecaMAILx, &
       load_models_mecaMAILx, &
       push_ppset_mecaMAILx, &
       get_write_DOF_mecaMAILx, &
       get_write_Rnod_mecaMAILx, &
       set_precon_body_mecaMAILx, &
       compute_precon_W_mecaMAILx, &
       put_precon_W_mecaMAILx, &
       init_precon_W_mecaMAILx, &
       put_vector_mecaMAILx, &
       get_vector_mecaMAILx, &
       init_ther_field, &
       update_ther_field,&
       compute_ther_fint, &
       set_field_bynode, &
       DISPLAY_bulk_element_mecaMAILx 

CONTAINS

!!!---------------------------------------------------

!!    CHECK = CHECK_mecaMAILx()
!!    IF (.NOT.CHECK) RETURN
    
    SUBROUTINE SetPreconBody(ivalue)
      IMPLICIT NONE
      INTEGER :: ivalue
       !!****e* mecaMAILx/SetPreconBody
       !! NAME
       !!  SetPreconBody
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****

       CALL set_precon_body_mecaMAILx(ivalue)

    END SUBROUTINE


    SUBROUTINE ComputePreconW
      IMPLICIT NONE

       !!****e* mecaMAILx/ComputePreconW
       !! NAME
       !!  ComputePreconW
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****

       CALL compute_precon_W_mecaMAILx

    END SUBROUTINE

    SUBROUTINE InitPreconW
      IMPLICIT NONE

       !!****e* mecaMAILx/InitPreconW
       !! NAME
       !!  InitPreconW
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/init_precon_W_mecaMAILx
       !!****

       CALL init_precon_W_mecaMAILx

    END SUBROUTINE

    SUBROUTINE PutPreconW(ivalue1,ivalue2,ivalue3,rvect,ivalue4)
      IMPLICIT NONE
      integer,intent(in) :: ivalue1,ivalue2,ivalue3,ivalue4
      real(kind=8),intent(in):: rvect(ivalue4)
!f2py optional, depend(rvect) :: ivalue4=len(rvect)

       !!****e* mecaMAILx/PutPreconW
       !! NAME
       !!  PutPreconW
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****
       ! ibdyty,inodty,jdof,nbdof,vaux

       CALL put_precon_W_mecaMAILx(ivalue1,ivalue2,ivalue3,rvect,ivalue4)

    END SUBROUTINE

    SUBROUTINE PutVector(cvalue1,ivalue1,rvect,ivalue2)
      IMPLICIT NONE
      character(len=5),intent(in) :: cvalue1
      integer,intent(in):: ivalue1,ivalue2

      real(kind=8),intent(in) :: rvect(ivalue2)
!f2py optional, depend(rvect) :: ivalue2=len(rvect)



       !!****e* mecaMAILx/PutVector
       !! NAME
       !!  PutVector
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****
       !ibdyty,nbdof,id_vect,vect

       print*,cvalue1,ivalue1,ivalue2,size(rvect)

       CALL put_vector_mecaMAILx(cvalue1,ivalue1,rvect,ivalue2)


    END SUBROUTINE

    SUBROUTINE GetVector(cvalue1,ivalue1,rvect,ivalue2)
      IMPLICIT NONE
      character(len=5),intent(in)  :: cvalue1
      integer         ,intent(in)  :: ivalue1,ivalue2

      real(kind=8)    ,intent(out) :: rvect(ivalue2)

!f2py optional, depend(rvect) :: ivalue2=len(rvect)

       !!****e* mecaMAILx/GetVector
       !! NAME
       !!  GetVector
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****
       !ibdyty,nbdof,id_vect,vect

       print*,cvalue1,ivalue1,ivalue2

       CALL get_vector_mecaMAILx(cvalue1,ivalue1,rvect,ivalue2)

    END SUBROUTINE


    SUBROUTINE PushProperties
      IMPLICIT NONE
       !!****e* mecaMAILx/PushProperties
       !! NAME
       !!  PushProperties
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****

       CALL push_ppset_mecaMAILx

    END SUBROUTINE

!!! LINEAR PROBLEMS ----------------------------------

    SUBROUTINE ComputeFreeVelocity
      IMPLICIT NONE
       !!****e* mecaMAILx/ComputeFreeVelocity
       !! NAME
       !!  ComputeFreeVelocity
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_free_vlocy_mecaMAILx
       !!****

       CALL compute_free_vlocy_mecaMAILx 

    END SUBROUTINE

    SUBROUTINE AssembKT
      IMPLICIT NONE
       !!****e* mecaMAILx/AssembKT
       !! NAME
       !!  AssembKT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_KT_mecaMAILx
       !!****

       print*,'AssembKT'

       CALL assemb_KT_mecaMAILx

    END SUBROUTINE

    SUBROUTINE AssembRHS
      IMPLICIT NONE
       !!****e* mecaMAILx/AssembRHS
       !! NAME
       !!  AssembRHS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_RHS_mecaMAILx
       !!****

       print*,'AssembRHS'

       CALL assemb_RHS_mecaMAILx

    END SUBROUTINE

!!$    SUBROUTINE ComputeFint
!!$       !!****e* mecaMAILx/ComputeFint
!!$       !! NAME
!!$       !!  ComputeFint
!!$       !! PURPOSE
!!$       !!
!!$       !! USES
!!$       !!  LMGC90.CORE/mecaMAILx/compute_Fint_mecaMAILx
!!$       !!****
!!$
!!$       CALL compute_Fint_mecaMAILx
!!$
!!$    END SUBROUTINE

!!! NON LINEAR PROBLEMS -------------------------------

    SUBROUTINE CheckNLConvergence(iconv)
      IMPLICIT NONE
      INTEGER :: iconv,nb_mecaMAILx

       !!****e* mecaMAILx/CheckNLConvergence
       !! NAME
       !!  CheckNLConvergence
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/check_nl_convergence_mecaMAILx
       !!****

       iconv = 1
       CALL check_nl_convergence_mecaMAILx(iconv)
!fd a voir
       nb_mecaMAILx = get_nb_mecaMAILx()
       IF (nb_mecaMAILx .NE. 0) iconv=0

    END SUBROUTINE

    SUBROUTINE ComputeNLFreeVelocity
      IMPLICIT NONE
       !!****e* mecaMAILx/ComputeNLFreeVelocity
       !! NAME
       !!  ComputeNLFreeVelocity
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_nl_free_vlocy_mecaMAILx
       !!****

       CALL compute_nl_free_vlocy_mecaMAILx

    END SUBROUTINE

    SUBROUTINE AssembNLKT
      IMPLICIT NONE
       !!****e* mecaMAILx/AssembNLKT
       !! NAME
       !!  AssembNLKT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_nl_KT_mecaMAILx
       !!****

       CALL assemb_nl_KT_mecaMAILx

    END SUBROUTINE

    SUBROUTINE AssembNLRHS
      IMPLICIT NONE
       !!****e* mecaMAILx/AssembNLRHS
       !! NAME
       !!  AssembNLRHS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_nl_RHS_mecaMAILx
       !!****

       CALL assemb_nl_RHS_mecaMAILx

    END SUBROUTINE

!!! LINEAR AND NON LINEAR PROBLEMS -------------------------------

    SUBROUTINE ComputeBulk(ikine,ither)

      !ikine : 0=hpp,1=gd   

      IMPLICIT NONE
      integer :: ikine

      !ither : 0=no thermal effect, 1= with thermal effect
      ! ATTENTION rustine qui ne marche qu'avec lmgc90 sans matlib !! 
      integer,optional :: ither 

      integer :: icomp 

      if (.not. present(ither)) ither=0

       !!****e* mecaMAILx/ComputeBulk
       !! NAME
       !!  ComputeBulk
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_hpp_mecaMAILx
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_gd_mecaMAILx
       !!****
       if (ikine == 0) then

         CALL compute_bulk_hpp_mecaMAILx(0)

         if (ither==1) call compute_ther_fint

       else if (ikine == 1) then

         CALL compute_bulk_gd_mecaMAILx(0)

       endif

    END SUBROUTINE

    SUBROUTINE UpdateBulk(ikine)
      IMPLICIT NONE
      integer :: ikine
      !ikine : 0=hpp,1=gd          

       !!****e* mecaMAILx/UpdateBulk
       !! NAME
       !!  UpdateBulk
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/update_bulk_mecaMAILx
       !!****

       if (ikine == 0) then

         CALL compute_bulk_hpp_mecaMAILx(1)

       else if (ikine == 1) then

         CALL compute_bulk_gd_mecaMAILx(1)

       endif

       CALL update_bulk_mecaMAILx 


    END SUBROUTINE


    SUBROUTINE UpdateDof
      IMPLICIT NONE
       !!****e* mecaMAILx/UpdateDof
       !! NAME
       !!  UpdateDof
       !! PURPOSE
       !!  save d.o.f. of the end of the time step to d.o.f. of the begining 
       !!  of the next one
       !! USES
       !!  LMGC90.CORE/mecaMAILx/update_dof_mecaMAILx
       !!****

       CALL update_dof_mecaMAILx 

    END SUBROUTINE
    !
    SUBROUTINE ComputeDof
      IMPLICIT NONE
       !!****e* mecaMAILx/ComputeDof
       !! NAME
       !!  ComputeDof
       !! PURPOSE
       !!  correction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_dof_mecaMAILx
       !!****

       CALL compute_dof_mecaMAILx 

    END SUBROUTINE
    !
    SUBROUTINE IncrementStep
      IMPLICIT NONE
       !!****e* mecaMAILx/IncrementStep
       !! NAME
       !!  IncrementStep
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/mecaMAILx/increment_mecaMAILx
       !!****

       CALL increment_mecaMAILx 

    END SUBROUTINE
    !
    SUBROUTINE ComputeFext
      IMPLICIT NONE
       !!****e* mecaMAILx/ComputeFext
       !! NAME
       !!  ComputeFext
       !! PURPOSE
       !!  compute external forces
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_Fext_mecaMAILx
       !!****

       CALL compute_Fext_mecaMAILx

    END SUBROUTINE
    !
    SUBROUTINE ComputeMass
      IMPLICIT NONE
       !!****e* mecaMAILx/ComputeMass
       !! NAME
       !!  ComputeMass
       !! PURPOSE
       !!  compute mass and inertia of bodies
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_mass_mecaMAILx
       !!****

       CALL compute_mass_mecaMAILx 

    END SUBROUTINE
    !
    SUBROUTINE FatalDamping
      IMPLICIT NONE
       !!****e* mecaMAILx/FatalDamping
       !! NAME
       !!  FatalDamping
       !! PURPOSE
       !!  Nullify body velocities
       !! USES
       !!  LMGC90.CORE/mecaMAILx/fatal_damping_mecaMAILx
       !!****

       CALL fatal_damping_mecaMAILx 

    END SUBROUTINE
    
    SUBROUTINE CheckEquilibriumState(iconv)
      IMPLICIT NONE
      INTEGER :: iconv
      LOGICAL :: check_convergence
       !!****e* mecaMAILx/CheckEquilibriumState
       !! NAME
       !!   CheckEquilibriumState
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0
       !! USES
       !!  LMGC90.CORE/mecaMAILx/check_equilibrium_state_mecaMAILx
       !!****
       iconv=1
       check_convergence = .FALSE.
       CALL check_equilibrium_state_mecaMAILx(check_convergence)
       IF (check_convergence) iconv = 0

    END SUBROUTINE

    SUBROUTINE SetEquilibriumNorm(checktype,tol)
      implicit none
      real(kind=8) :: tol
      character(len=5) :: checktype
       !!****e* mecaMAILx/SetEquilibriumNorm
       !! NAME
       !!   SetEquilibriumNorm
       !! SYNOPSIS
       !!  check equilibrium state
       !!  [checktype] [tol]
       !! INPUTS
       !!  [checktype] : 
       !!  [tol]       : 
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0
       !!  You must precise the type of check test
       !!   - Qvlcy : quadratic norm of velocy
       !!   - Maxm  : maximum   norm of velocy
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_data_equilibrium_mecaMAILx
       !!****

       IF ( checktype .NE. 'Qvlcy' .AND. checktype .NE. 'Mvlcy') THEN
          print*,' @ WARNING, NEW from 03.12.16.'
          print*,' @ You must precise the type of check test:'
          print*,' @  - Qvlcy : quadratic norm of velocy,'
          print*,' @  - Maxm  : maximum   norm of velocy.'
          STOP
       END IF
       CALL set_data_equilibrium_mecaMAILx(checktype,tol)

    END SUBROUTINE

!!! READING DATA ----------------------------------------------------

    SUBROUTINE ReadDrivenDof
      IMPLICIT NONE
       !!****e* mecaMAILx/ReadDrivenDof
       !! NAME
       !!  ReadDrivenDof
       !! PURPOSE
       !!  read DRV_DOF.DAT file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_in_driven_dof_mecaMAILx
       !!****

       CALL read_in_driven_dof_mecaMAILx

    END SUBROUTINE
    
    SUBROUTINE ReadIniGPV
       !!****e* mecaMAILx/ReadIniGPV
       !! NAME
       !!  ReadIniGPV
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_in_gpv_mecaMAILx
       !!****

       CALL read_in_gpv_mecaMAILx

    END SUBROUTINE

    SUBROUTINE ReadIniDof
       !!****e* mecaMAILx/ReadIniDof
       !! NAME
       !!  ReadIniDof
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_in_dof_mecaMAILx
       !!****

       CALL read_in_dof_mecaMAILx

    END SUBROUTINE

    SUBROUTINE LoadBehaviours
      IMPLICIT NONE
       !!****e* mecaMAILx/LoadBehaviours
       !! NAME
       !!  LoadBehaviours
       !! PURPOSE
       !!  read DOF.INI file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/load_behaviours_mecaMAILx
       !!****

       CALL load_behaviours_mecaMAILx 

    END SUBROUTINE

    SUBROUTINE LoadModels
      IMPLICIT NONE
       !!****e* mecaMAILx/LoadModels
       !! NAME
       !!  LoadModels
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_models_mecaMAILx
       !!  LMGC90.CORE/mecaMAILx/update_existing_entities_mecaMAILx
       !!****

       CALL load_models_mecaMAILx
       CALL update_existing_entities_mecaMAILx
       
    END SUBROUTINE

!!! WRITTING DATA --------------------------------------------------

    SUBROUTINE WriteDrivenDof
      IMPLICIT NONE
       !!****e* mecaMAILx/WriteDrivenDof
       !! NAME
       !!  WriteDrivenDof
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_out_driven_dof_mecaMAILx
       !!****

       CALL write_out_driven_dof_mecaMAILx

    END SUBROUTINE

    SUBROUTINE WriteLastDof
       IMPLICIT NONE
       INTEGER :: ifrom,ito

       !!****e* mecaMAILx/WriteLastDof
       !! NAME
       !!  WriteLastDof
       !! PURPOSE
       !!  write ascii DOF.LAST file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_dof_mecaMAILx
       !!****

          ifrom = 1  
          ito   = get_nb_mecaMAILx()
          CALL write_xxx_dof_mecaMAILx(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutDof
       IMPLICIT NONE
       LOGICAL :: write_DOF
       INTEGER :: ifrom,ito

       !!****e* mecaMAILx/WriteOutDof
       !! NAME
       !!  WriteOutDof
       !! PURPOSE
       !!  write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_dof_mecaMAILx
       !!****

       write_DOF = get_write_DOF_mecaMAILx()

       IF (write_DOF) THEN
         ifrom = 1  
         ito   = get_nb_mecaMAILx()
         CALL write_xxx_dof_mecaMAILx(1,ifrom,ito)
       ENDIF

    END SUBROUTINE

    SUBROUTINE DisplayOutDof
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* mecaMAILx/DisplayOutDof
       !! NAME
       !!  DisplayOutDof
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_dof_mecaMAILx
       !!****

          ifrom = 1  
          ito   = get_nb_mecaMAILx()
          CALL write_xxx_dof_mecaMAILx(6,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteLastRnod
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* mecaMAILx/WriteLastRnod
       !! NAME
       !!  WriteLastRnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_Rnod_mecaMAILx
       !!****

          ifrom = 1  
          ito   = get_nb_mecaMAILx()
          CALL write_xxx_Rnod_mecaMAILx(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutRnod
      IMPLICIT NONE
      INTEGER :: ifrom,ito
      LOGICAL :: write_Rnod
       !!****e* mecaMAILx/WriteOutRnod
       !! NAME
       !!  WriteOutRnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_Rnod_mecaMAILx
       !!****
       write_Rnod = get_write_Rnod_mecaMAILx()

       IF (write_Rnod) THEN
         
         ifrom = 1  
         ito   = get_nb_mecaMAILx()
         CALL write_xxx_Rnod_mecaMAILx(1,ifrom,ito)

       END IF

    END SUBROUTINE

    SUBROUTINE DisplayOutRnod
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* mecaMAILx/DisplayOutRnod
       !! NAME
       !!  DisplayOutRnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_Rnod_mecaMAILx
       !!****
       ifrom = 1  
       ito   = get_nb_mecaMAILx()
       CALL write_xxx_Rnod_mecaMAILx(6,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE DisplayOutNodalForces
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* mecaMAILx/DisplayOutNodalForces
       !! NAME
       !!  DisplayOutNodalForces
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_nodforces
       !!****
          
          ifrom = 1  
          ito   = get_nb_mecaMAILx()
          CALL write_xxx_nodforces(6,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE InitTherField(IdBody,Tini)
      IMPLICIT NONE
      integer      :: IdBody
      real(kind=8) :: Tini

       !!****e* mecaMAILx/InitTherField
       !! NAME
       !!  InitTherField
       !! PURPOSE
       !!  Initialise the thermal field to a constant value on a given body 
       !! USES
       !!  LMGC90.CORE/mecaMAILx/init_ther_gpv
       !!****
          
       CALL init_ther_field(IdBody,Tini)

    END SUBROUTINE

    SUBROUTINE UpdateTherField(IdBody,T,Tsize)
      IMPLICIT NONE
      INTEGER :: IdBody,Tsize
      REAL(kind=8),dimension(Tsize) :: T
       !!****e* mecaMAILx/UpdateTherField
       !! NAME
       !!  UpdateTherField
       !! PURPOSE
       !!  Update the thermal field on a given body
       !!  You need to initialize with InitTherField
       !! USES
       !!  LMGC90.CORE/mecaMAILx/update_ther_field
       !!****

       CALL update_ther_field(IdBody,T,Tsize)

    END SUBROUTINE

    SUBROUTINE SetField(IdBody,f_rank,f,f_size)
      IMPLICIT NONE
      INTEGER :: IdBody,f_rank,f_size
      REAL(kind=8),dimension(f_size) :: f

!f2py optional, depend(f) :: f_size=len(f)

       !!****e* mecaMAILx/SetField
       !! NAME
       !!  SetField
       !! PURPOSE
       !!  Update an external field on a given body
       !!  You need to set this field in your models.dat
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_field_bynode
       !!****
     
       print *,IdBody,f_rank,f_size
       print *,f

       CALL set_field_bynode(IdBody,f_rank,f_size,f)

    END SUBROUTINE

    SUBROUTINE DisplayBulkElement(IdBody,Idele)
      IMPLICIT NONE
      INTEGER :: IdBody,Idele

      call DISPLAY_bulk_element_mecaMAILx(IdBody,Idele) 
    end subroutine

END MODULE wrap_mecaMAILx
