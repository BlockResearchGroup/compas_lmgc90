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

! Quick memo about the name of the different contact_2D modules:
! CLALp: CLxx  - ALpxx
! CLJCx: CLxxx - JONCx
! DKALp: DISKx - ALPxx 
! DKDKL: DISKL - DISKL 
! DKDKx: DISKx - DISKx
! DKDPx: DISKx - DISPx
! DKJCx: DISKx - JONCx
! DKKDx: DISKx - xKSID 
! DKPDx: DISKx - xPSID 
! DKPLx: DISKx - POLYG
! DPALp: DISPx - ALpxx
! P2P2L: PT2DL - PT2DL
! PLALp: POLYG - ALPxx
! PLJCx: POLYG - JONCx
! PLPLx: POLYG - POLYG
! PTPT2: PT2Dx - PT2Dx
! with
! DISKx disk on a rigid
! DISPx pneumatic disk on a rigid
! DISKL disk on a 2D mesh edge 
! xKSID hollow disk on a rigid 
! xPSID hollow pnumatic disk on a rigid
! POLYG polygone on a rigid
! ALpxx  Antagoniste Ligne patch on a 2D mesh

!> define the 2D interaction for mechanics
module inter_meca_2D

  use overall, only: max_internal_tact

  use tact_behaviour, only : T_see, &
                             T_tact_behav, &
                             get_nb_internal, &
                             init_internal

  use utilities, only : logmes, &
                        faterr

  implicit none

  private

  ! space dimension
  integer(kind=4), parameter, public :: space_dim   = 2
  ! number of dofs of the interaction in local contact frame
  integer(kind=4), parameter :: inter_dim   = 2

  ! Defines :
  !  - T_interaction
  !  - T_this_adjac
  !  - T_verlet
  !  - T_con
  include 'interaction_type.f90'

end module inter_meca_2D
