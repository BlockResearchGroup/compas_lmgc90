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
MODULE wrap_post2D

  !!****h* LMGC90.CHIPY/POST2D
  !! NAME
  !!  module wrap_post2D
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/POST2D
  !!****
  USE post2D,ONLY: &
       init_GMV_post2D, &
       update_post2D, &
       write_GMV_post2D, &
       set_ref_reac_max_post2D, &
       set_displ_ampl_post2D, &
       set_ref_radius_post2D, &
       set_periodic_data_post2D, &
       set_extra_material_post2D, &
       active_GMV,&
       put_material_ID_post2D, &
       GMV_circular_selection, &
       get_write_gmv_post2D

!       desacti_GMV_writeascii, &
!       acti_GMV_thermal_gpv, &
!       acti_GMV_mechanical_gpv, &
!       acti_GMV_mechanical_externval, &
!       acti_GMV_stress, &
!       acti_GMV_dspl, &
!       acti_GMV_avlocy, &
!       acti_GMV_ERROR, &
!       acti_GMV_TACTOR, &
!       acti_GMV_tact_point, &
!       acti_GMV_BETA, &
!       acti_GMV_RESTART, &
!       acti_GMV_oxide, &
!       acti_GMV_electro, &
!       acti_GMV_HEAT, &
!       acti_GMV_WEAR, &

CONTAINS

!!$ on ne s'en sert pas !? c'est fait quand on lance le write gmv
!!$    subroutine UpdatePost2D(ivalue)
!!$      implicit none
!!$      integer :: ivalue ! niveau de bavaradage
!!$       !!****e* POST2D/UPDATE POST 2D
!!$       !! NAME
!!$       !!   UPDATE POST 2D
!!$       !! PURPOSE
!!$       !!  Do all the computation without displaying the value. 
!!$       !!  Needed for a use in combination with LMGC90/POSTPRO
!!$       !! USES
!!$       !!  LMGC90.CORE/POST2D/update_post2D
!!$       !!****
!!$       CALL LOGMESS('post2D::UpdatePost2D') 
!!$       CALL update_post2D(ivalue)
!!$
!!$    END subroutine

    subroutine WriteOutGMV(ivalue)
      implicit none
      integer :: ivalue ! niveau de bavaradage
      logical :: write_gmv

       !!****e* POST2D/WriteOutGMV
       !! NAME
       !!  WriteOutGMV
       !! PURPOSE
       !!  write gmv file
       !! USES
       !!  LMGC90.CORE/POST2D/update_post2D
       !!  LMGC90.CORE/POST2D/write_GMV_post2D
       !!****
       write_gmv= get_write_gmv_post2D()

       CALL update_post2D(ivalue)

       IF (write_gmv) CALL write_GMV_post2D

    END SUBROUTINE

    SUBROUTINE InitGMV
      implicit none
      integer :: ivalue ! niveau de bavaradage

       !!****e* POST2D/InitGMV
       !! NAME
       !!   InitGMV
       !! PURPOSE
       !!  Initialize the database
       !! USES
       !!  LMGC90.CORE/POST2D/init_GMV_post2D
       !!****
       CALL init_GMV_post2D
    END SUBROUTINE

    SUBROUTINE InitBinaryGMV
      implicit none
       !!****e* POST2D/InitBinaryGMV
       !! NAME
       !!   Init Gmv
       !! PURPOSE
       !!  Initialize the database and write the GMV file using the ieee binary format
       !! USES
       !!  LMGC90.CORE/POST2D/init_GMV_post2D
       !!  LMGC90.CORE/POST2D/desacti_GMV_writeascii
       !!****
       CALL init_GMV_post2D

!fd a corriger stupide
       call active_GMV('BINAR')

    END SUBROUTINE

    SUBROUTINE DeclareNewGMVMaterial(ivalue)
      Implicit none
      integer :: ivalue

       !!****e* POST2D/DeclareNewGMVMaterial
       !! NAME
       !!  Select Gmv Material 
       !! SYNOPSIS
       !!  Select Gmv Material
       !!  [nb]
       !!  [behav](1)
       !!  ...
       !!  [behav](nb)
       !! INPUTS
       !!  [nb]       : number of selected behavior
       !!  [behav](i) : list of selected behavior
       !! PURPOSE
       !!  Adds specific material for the display
       !! USES
       !!  LMGC90.CORE/POST2D/set_extra_material_post2D
       !!  LMGC90.CORE/POST2D/put_material_ID_post2D
       !!****

       CALL set_extra_material_post2D(ivalue,.TRUE.)
    END SUBROUTINE

    SUBROUTINE SetNewGMVMaterial(ivalue,cvalue)
      Implicit none
      integer :: ivalue
      character(*) :: cvalue
       !!****e* POST2D/SetNewGMVMaterial
       !! NAME
       !!  Select Gmv Material
       !! SYNOPSIS
       !!  Select Gmv Material  
       !!  [nb]
       !!  [behav](1)
       !!  ...
       !!  [behav](nb)
       !! INPUTS
       !!  [nb]       : number of selected behavior
       !!  [behav](i) : list of selected behavior
       !! PURPOSE
       !!  Adds specific material for the display
       !! USES
       !!  LMGC90.CORE/POST2D/set_extra_material_post2D
       !!  LMGC90.CORE/POST2D/put_material_ID_post2D
       !!****
       CALL put_material_ID_post2D(ivalue,cvalue(1:5))
    END SUBROUTINE

    SUBROUTINE ReferenceReacMax(rvalue)
      implicit none
      real(kind=8) :: rvalue
       !!****e* POST2D/ReferenceReacMax
       !! NAME
       !!   ReferenceReacMax
       !! SYNOPSIS
       !!   ReferenceReacMax
       !!   [reac]
       !! INPUTS
       !!   [reac] reference value of the contact force
       !! PURPOSE
       !!   Set the reference value of the contact force. 
       !!   It allows to keep the same reac during the computation. 
       !!   Either this value is recomputed a each time step.
       !! USES
       !!   LMGC90.CORE/POST2D/set_ref_reac_max_post2D
       !!****


       WRITE(*,'(D12.5)') rvalue

       CALL set_ref_reac_max_post2D(rvalue)

    END SUBROUTINE

    SUBROUTINE DisplacementAmplification(rvalue)
      implicit none
      real(kind=8) :: rvalue

       !!****e* POST2D/DisplacementAmplification
       !! NAME
       !!   DisplacementAmplification
       !! SYNOPSIS
       !!   DisplacementAmplification
       !!   [disp_amp]
       !! INPUTS
       !!  [disp_amp] : displacement amplification factor
       !! PURPOSE
       !!   It allows to amplify the magnitude of displacement.  
       !!   Works only with deformable bodies
       !! USES
       !!   LMGC90.CORE/POST2D/set_displ_ampl_post2D
       !!****
       WRITE(*,'(D12.5)') rvalue

       CALL set_displ_ampl_post2D(rvalue)

    END SUBROUTINE

    SUBROUTINE ReferenceRadius(rvalue)
      implicit none
      real(kind=8) :: rvalue

       !!****e* POST2D/ReferenceRadius
       !! NAME
       !!   ReferenceRadius
       !! SYNOPSIS
       !!   ReferenceRadius
       !!   [ref_radius]
       !! INPUTS
       !!  [ref_radius] : value of the reference radius which should be used for contact display
       !! PURPOSE
       !!   Set the reference value for displaying the contact forces. 
       !!   It is related to the size of the smallest body usually a ratio of 0.1 works
       !! USES
       !!   LMGC90.CORE/POST2D/set_ref_radius_post2D
       !!****
       WRITE(*,'(D12.5)') rvalue

       CALL set_ref_radius_post2D(rvalue)

    END SUBROUTINE

    Subroutine WithThermalGPV
      implicit none
       !!****e* POST2D/WithThermalGPV
       !! NAME
       !!   DisplayThermal
       !! PURPOSE
       !!  Specifies that you want to display thermal GPV values if any
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_thermal_gpv
       !!****

       CALL active_GMV('THGPV')

    END SUBROUTINE

    SUBROUTINE withMechanicalGPV
      implicit none
       !!****e* POST2D/withMechanicalGPV
       !! NAME
       !!   DisplayMechanicalGPV
       !! PURPOSE
       !!  Specifies that you want to display mechanical GPV values if any
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_mechanical_gpv
       !!****

       CALL active_GMV('MEGPV')

    END SUBROUTINE

    SUBROUTINE WithStress
      implicit none        
       !!****e* POST2D/WithStress
       !! NAME
       !!   ComputeStress
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_stress
       !!****

       CALL active_GMV('STRSS')

    END SUBROUTINE

    SUBROUTINE WithIncrmentalDisplacement
      implicit none
       !!****e* POST2D/WithIncrmentalDisplacement
       !! NAME
       !!   ComputeIncrmentalDisplacement
       !! PURPOSE
       !!  Specifies that you want to compute and display the cumulated displacement between two displayed steps
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_dspl
       !!****

       CALL active_GMV('DISPL')

    END SUBROUTINE

    SUBROUTINE WithAverageVelocity
      implicit none
       !!****e* POST2D/WithAverageVelocity
       !! NAME
       !!   ComputeAverageVelocity
       !! PURPOSE
       !!  Specifies that you want to compute and display the average velocity between two displayed steps
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_avlocy
       !!****

       CALL active_GMV('VELOC')

    END SUBROUTINE

    SUBROUTINE WithERROR
      implicit none
       !!****e* POST2D/WithERROR
       !! NAME
       !!   ComputeError
       !! PURPOSE
       !!  Specifies that you want to compute and display the contact ERROR 
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_ERROR
       !!****

       CALL active_GMV('ERROR')

    END SUBROUTINE

    SUBROUTINE withTACTOR
      implicit none 
       !!****e* POST2D/withTACTOR
       !! NAME
       !!   ComputeTACTOR
       !! PURPOSE
       !!  Specifies that you want to compute and display the contactor shape 
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_TACTOR
       !!****

       CALL active_GMV('TCTRS')

    END SUBROUTINE

    SUBROUTINE WithContactPoint
      implicit none
       !!****e* POST2D/WithContactPoint
       !! NAME
       !!   ComputeTactPoint
       !! PURPOSE
       !!  Specifies that you want to compute and display the contact point 
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_tact_point
       !!****

       CALL active_GMV('TCTPT')

    END SUBROUTINE
    
    SUBROUTINE WithBeta
      implicit none
       !!****e* POST2D/WithBeta
       !! NAME
       !!   ComputeBeta
       !! PURPOSE
       !!  Specifies that you want to compute and display the beta value (for CZM only) 
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_BETA
       !!****

       CALL active_GMV('BETA_')

    END SUBROUTINE
    
    SUBROUTINE WithOxyde
      implicit none
       !!****e* POST2D/WithOxyde
       !! NAME
       !!   ComputeOxyde
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/acti_GMV_oxide
       !!****

       CALL active_GMV('OXIDE')

    END SUBROUTINE

    SUBROUTINE WithWear
      implicit none
       !!****e* POST2D/WithWear
       !! NAME
       !!   ComputeWear
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/acti_GMV_WEAR
       !!****

       CALL active_GMV('WEAR_')

    END SUBROUTINE

    SUBROUTINE WithHeat
      implicit none
       !!****e* POST2D/WithHeat
       !! NAME
       !!   ComputeHeat
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/acti_GMV_HEAT
       !!****

       CALL active_GMV('HEAT_')

    END SUBROUTINE

    SUBROUTINE WithElectro
      implicit none
       !!****e* POST2D/WithElectro
       !! NAME
       !!   ComputeElectro
       !! PURPOSE
       !!  Specifies that you want to compute and display stresses, principal stresses ...
       !! USES
       !!  LMGC90.CORE/POST2D/acti_GMV_ELECTRO
       !!****

       CALL active_GMV('ELECT')

    END SUBROUTINE

    SUBROUTINE RestartGMV
      implicit none
       !!****e* POST2D/RestartGMV
       !! NAME
       !!   RestartGMV
       !! PURPOSE
       !!  Specifies that you want to restart displaying the gmv file from the value stored in DISPLAY/DISPLAYED_GMV.
       !!  It allows you not to rewrite in some displayed file. 
       !! USES
       !!   LMGC90.CORE/POST2D/acti_GMV_RESTART
       !!****

       CALL active_GMV('RSTRT')

    END SUBROUTINE

    SUBROUTINE SetPeriodicCondition(rvalue)
      implicit none
      real(kind=8) :: rvalue
      logical :: PERIODIC
       !!****e* POST2D/WithPeriodicCondition
       !! NAME
       !!  PeriodicCondition
       !! SYNOPSIS
       !!  PeriodicCondition
       !!  [periode]
       !! PURPOSE
       !!  [periode] periode of simulation system. The X variable reaches
       !!  between 0 and [periode]
       !! USES
       !!  LMGC90.CORE/POST2D/set_periodic_data_post2D
       !!****
       PERIODIC=.TRUE.
       CALL set_periodic_data_post2D(rvalue,PERIODIC)
    END SUBROUTINE

!!!----------------------------------------------------------
!!!
!!! More chic command for postprocessing
!!!
!!!----------------------------------------------------------

    SUBROUTINE CircularSelection(rvalue1,rvalue2,rvalue3)
      implicit none
      real(kind=8) :: rvalue1,rvalue2,rvalue3
       !!****e* POST2D/CircularSelection
       !! NAME 
       !!  CircularSelection
       !! SYNOPSIS
       !!  CircularSelection
       !!  [X]
       !!  [Y]
       !!  [R]
       !! INPUTS
       !!  [X] : X coordinate
       !!  [Y] : Y coordinate
       !!  [R] : radius selection
       !! PURPOSE
       !!  initialize data for postreatment using a circular selection
       !! USES
       !!  LMGC90.CORE/POST2D/GMV_circular_selection
       !!****

       CALL GMV_circular_selection(rvalue1,rvalue2,rvalue3)

    END SUBROUTINE

END MODULE wrap_post2D
