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

  !!****h* LMGC90.CHIPY/therMAILx  
  !! NAME
  !!  module wrap_therMAILx  
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/therMAILx  
  !!****

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
       get_write_Rnod_therMAILx, &
       remove_driven_temp, &
       compute_Fext_Poisson_therMAILx, & !am: debut thermique stationnaire
       assemb_KT_Poisson_therMAILx, & 
       assemb_RHS_Poisson_therMAILx, &        
       compute_dof_Poisson_therMAILx, &  
       compute_dof_Poisson_Neumann_therMAILx ! <- fin thermique stationnaire

CONTAINS

!!!----------------------------------------------------
    
!!    CHECK = CHECK_therMAILx()
!!    IF (.NOT.CHECK) RETURN
 
!!! HPP PROBLEMS

    SUBROUTINE ComputeConductivity
      IMPLICIT NONE
       !!****e* therMAILx/ComputeConductivity
       !! NAME
       !!  ComputeConductivity
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_conductivity_therMAILx
       !!****

       CALL compute_conductivity_therMAILx 

    END SUBROUTINE

    SUBROUTINE ComputeCapacity
      IMPLICIT NONE
       !!****e* therMAILx/ComputeCapacity
       !! NAME
       !!  ComputeCapacity
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_capacity_therMAILx
       !!****

       CALL compute_capacity_therMAILx 

    END SUBROUTINE

    SUBROUTINE ComputeInternalFlux
      IMPLICIT NONE
       !!****e* therMAILx/ComputeInternalFlux
       !! NAME
       !!  ComputeInternalFlux
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_ttFint_therMAILx
       !!****

       CALL compute_ttFint_therMAILx

    END SUBROUTINE

    SUBROUTINE ComputeExternalFlux
      IMPLICIT NONE
       !!****e* therMAILx/ComputeExternalFlux
       !! NAME
       !!  ComputeExternalFlux
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/compute_Fext_therMAILx
       !!****

       CALL compute_Fext_therMAILx

    END SUBROUTINE
 
!!! construction of the iteration matrix and the corresponding right hand side vector

    SUBROUTINE AssembThermKT
      IMPLICIT NONE
       !!****e* therMAILx/AssembThermKT
       !! NAME
       !!  AssembThermKT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/assemb_KT_therMAILx
       !!****

       CALL assemb_KT_therMAILx

    END SUBROUTINE

    SUBROUTINE AssembThermRHS
      IMPLICIT NONE
       !!****e* therMAILx/AssembThermRHS
       !! NAME
       !!  AssembThermRHS
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/assemb_RHS_therMAILx
       !!****

       CALL assemb_RHS_therMAILx

    END SUBROUTINE

!!! construction of the iteration matrix and the corresponding right hand side vector

    SUBROUTINE TrialAssembThermKT
      IMPLICIT NONE
       !!****e* therMAILx/TrialAssembThermKT
       !! NAME
       !!  TrialAssembThermKT
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/trial_assemb_KT_therMAILx
       !!****

       CALL trial_assemb_KT_therMAILx

    END SUBROUTINE

    SUBROUTINE TrialAssembThermRHS
      IMPLICIT NONE
       !!****e* therMAILx/TrialAssembThermRHS
       !! NAME
       !!  TrialAssembThermRHS
       !! PURPOSE
       !!  trial_assemb_RHS_therMAILx
       !! USES
       !!  LMGC90.CORE/therMAILx
       !!****

       CALL trial_assemb_RHS_therMAILx

    END SUBROUTINE

    SUBROUTINE IncrementStep
      IMPLICIT NONE
       !!****e* therMAILx/IncrementStep
       !! NAME
       !!  IncrementStep
       !! PURPOSE
       !!  increment_therMAILx
       !! USES
       !!  LMGC90.CORE/therMAILx
       !!****

       CALL increment_therMAILx

    END SUBROUTINE

    SUBROUTINE ComputeThermDOF
      IMPLICIT NONE
       !!****e* therMAILx/ComputeThermDOF
       !! NAME
       !!  ComputeThermDOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/comp_dof_therMAILx
       !!****

       CALL comp_dof_therMAILx

    END SUBROUTINE

    SUBROUTINE UpdateThermDOF
      IMPLICIT NONE
       !!****e* therMAILx/UpdateThermDOF
       !! NAME
       !!  UpdateThermDOF
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/update_dof_therMAILx
       !!****

       CALL update_dof_therMAILx 

    END SUBROUTINE

    SUBROUTINE UpdateThermBulk
      IMPLICIT NONE
       !!****e* therMAILx/UpdateThermBulk
       !! NAME
       !!  UpdateThermBulk
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/update_therm_bulk_therMAILx
       !!****

       CALL update_therm_bulk_therMAILx 

    END SUBROUTINE

    SUBROUTINE CheckNLConvergence(iconv)
      IMPLICIT NONE
      INTEGER :: iconv,nb_therMAILx

       !!****e* therMAILx/CheckNLConvergence
       !! NAME
       !!  CheckNLConvergence
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/check_ther_convergence_therMAILx
       !!****

       iconv = 1
       CALL check_ther_convergence_therMAILx(iconv)
       nb_therMAILx = get_nb_therMAILx()
       IF (nb_therMAILx .NE. 0) iconv = 0

    END SUBROUTINE

!!!--------------------------------------------------------------------

    SUBROUTINE ReadDrivenDof
      IMPLICIT NONE
       !!****e* therMAILx/ReadDrivenDof
       !! NAME
       !!  ReadDrivenDof
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_in_driven_dof_therMAILx
       !!****

       CALL read_in_driven_dof_therMAILx

    END SUBROUTINE

    SUBROUTINE ReadModels
      IMPLICIT NONE
       !!****e* therMAILx/ReadModels
       !! NAME
       !!  ReadModels
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_models_therMAILx
       !!****

       CALL read_models_therMAILx

    END SUBROUTINE

    SUBROUTINE ReadBehaviours
       !!****e* therMAILx/ReadBehaviours
       !! NAME
       !!  ReadBehaviours
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_behaviours_therMAILx
       !!****

       CALL read_behaviours_therMAILx 

    END SUBROUTINE

    SUBROUTINE ReadIniDof
      IMPLICIT NONE
       !!****e* therMAILx/ReadIniDof
       !! NAME
       !!  ReadIniDof
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_in_dof_therMAILx
       !!****

       CALL read_in_dof_therMAILx 

    END SUBROUTINE

    SUBROUTINE ReadIniGPV
      IMPLICIT NONE
       !!****e* therMAILx/ReadIniGPV
       !! NAME
       !!  ReadIniGPV
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/read_in_gpv_therMAILx
       !!****

       CALL read_in_gpv_therMAILx 

    END SUBROUTINE

!!!--------------------------------------------------------------------

    SUBROUTINE WriteLastDof
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* therMAILx/WriteLastDof
       !! NAME
       !!  WriteLastDof
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/therMAILx/write_xxx_dof_therMAILx
       !!****

          ifrom = 1  
          ito   = get_nb_therMAILx()
          CALL write_xxx_dof_therMAILx(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutDOF
      IMPLICIT NONE
      INTEGER :: ifrom,ito
      LOGICAL :: write_DOF
       !!****e* therMAILx/WriteOutDOF
       !! NAME
       !!  WriteOutDOF
       !! PURPOSE
       !!  write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/therMAILx/write_xxx_dof_therMAILx
       !!****

       write_DOF = get_write_DOF_therMAILx()
       IF (write_DOF) THEN
         ifrom = 1  
         ito   = get_nb_therMAILx()
         CALL write_xxx_dof_therMAILx(1,ifrom,ito)
       END IF

    END SUBROUTINE

    SUBROUTINE DisplayOutDOF
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* therMAILx/DisplayOutDOF
       !! NAME
       !!  DisplayOutDOF
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/therMAILx/write_xxx_dof_therMAILx
       !!****
       
       ifrom = 1  
       ito   = get_nb_therMAILx()
       CALL write_xxx_dof_therMAILx(6,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteLastRnod
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* therMAILx/WriteLastRnod
       !! NAME
       !!  WriteLastRnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  
       !!****


          ifrom = 1  
          ito   = get_nb_therMAILx()
          !CALL write_xxx_Rnod_therMAILx(2,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE WriteOutRnod
      IMPLICIT NONE
      INTEGER :: ifrom,ito
      LOGICAL :: write_Rnod
       !!****e* therMAILx/WriteOutRnod
       !! NAME
       !!  WriteOutRnod
       !! PURPOSE
       !!  write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/therMAILx
       !!****

       write_Rnod = get_write_Rnod_therMAILx()
       IF (write_Rnod) THEN
         ifrom = 1  
         ito   = get_nb_therMAILx()
         !CALL write_xxx_Rnod_therMAILx(1,ifrom,ito)
       END IF

    END SUBROUTINE

    SUBROUTINE DisplayOutRnod
      IMPLICIT NONE
      INTEGER :: ifrom,ito
       !!****e* therMAILx/DisplayOutRnod
       !! NAME
       !!  DisplayOutRnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/therMAILx
       !!****

          ifrom = 1  
          ito   = get_nb_therMAILx()
          !CALL write_xxx_Rnod_therMAILx(6,ifrom,ito)

    END SUBROUTINE

    SUBROUTINE RemoveDrivenTemp(id)
      IMPLICIT NONE
      integer :: id

      ! id le numero du corps

      CALL Remove_Driven_Temp(id)

    END SUBROUTINE

!am: debut des fonctions supplementaires :

! fonctions permettant de resoudre une equation de thermique stationnaire (type Poisson)
!     [K]{T} = {f}

! * caclul des flux externes imposes (CL de type Neumann)
subroutine ComputeExternalFluxPoisson

   implicit none

   call compute_Fext_Poisson_therMAILx

end subroutine ComputeExternalFluxPoisson

! * assemblage de la matrice :
subroutine AssembThermKTPoisson

   implicit none

   call assemb_KT_Poisson_therMAILx

end subroutine AssembThermKTPoisson

! * assemblage du second membre :
subroutine AssembThermRHSPoisson

   implicit none

   call assemb_RHS_Poisson_therMAILx

end subroutine AssembThermRHSPoisson

! * calcul du champ de temperature (elimination des CL de type Dirichlet + inversion du systeme) :
subroutine ComputeThermDOFPoisson

   implicit none

   call compute_dof_Poisson_therMAILx

end subroutine ComputeThermDOFPoisson

! * calcul du champ de temperature, dans le cas ou on impose des CL de Neumann
! , flux de pression impose, sur tout le bord : construction d'un probleme
! de moindres carres en ajoutant la contrainte <p> = 0 + resolution 
subroutine ComputeThermDofPoissonNeumann

   implicit none

   call compute_dof_Poisson_Neumann_therMAILx

end subroutine ComputeThermDofPoissonNeumann

END MODULE wrap_therMAILx
