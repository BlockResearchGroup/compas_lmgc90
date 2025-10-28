!===========================================================================
!
! Copyright 2000-2023 CNRS-UM.
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


module MatLibExternalModels

! additional module in order to use the MatLib modelling library

  use utilities, only: faterr, logmes

  implicit none
  private

 ! wrap
  public push_model, push_behaviour, check_ppset

 ! internal API

  public compute_external_gp, set_ortho_frame

contains
!------------------------------------------------------------------------
!------------------------------------------------------------------------
  subroutine push_model(imodel, itchatche)
    implicit none
    !
    ! declarations pour la matlib
    !
    ! Dans le conteneur des variables externes on trouve les gradients et les flux.
    ! L'operateur tangent a donc sa taille
    !
    ! Dans le conteneur des variables internes ont trouve des variables internes ET
    ! des variables cachees necessaires a la matlib
    !
    ! le modele elastique standard prend en compte la dilatation dans l'absolue il
    ! doit permettre un couplage faible
    !
    ! le modele thermoelastique permet les couplages forts
    !
    integer   :: imodel
    logical   :: itchatche

                              !12345678901234567890123456789012345
    character(len=35)  :: IAM='noMatLib_ExternalModels::push_model'

    if (itchatche) call LOGMES('Entering : '//IAM)
    call faterr(iam,'Error: no MatLib external models available')

  end subroutine push_model
!------------------------------------------------------------------------
!------------------------------------------------------------------------
  subroutine push_behaviour(iext_ppset,ibehav,itchatche,density,elas_coeff)
    implicit none

    integer :: iext_ppset, ibehav
    logical :: itchatche
    real(kind=8), optional :: density, elas_coeff(2)

                             !123456789012345678901234567890123456789
    character(len=39) :: IAM='noMatLib_ExternalModels::push_behaviour'

    if (itchatche) call LOGMES('Entering : '//IAM)
    call faterr(iam,'Error: no MatLib external models available')

  end subroutine push_behaviour

!------------------------------------------------------------------------
!------------------------------------------------------------------------
  subroutine check_ppset(iext_ppset,imodel,itchatche)
    implicit none

    integer :: iext_ppset,imodel
    logical :: itchatche

                             !123456789012345678901234567890123456
    character(len=36) :: IAM='noMatLib_ExternalModels::check_ppset'

    if (itchatche) call LOGMES('Entering : '//IAM)
    call faterr(iam,'Error: no MatLib external models available')

  end subroutine check_ppset
!------------------------------------------------------------------------
!------------------------------------------------------------------------
  subroutine compute_external_gp(ppsnb,mdlnb,extP_lbl,extP_len, &
                                 ivalue, extP_val,extP_nb, &
                                 GRAD0,FLUX0,INTERNAL0, &
                                 GRAD1,FLUX1,INTERNAL1, &
                                 D,H,calcD)
    implicit none
    ! parametres externes
    integer   :: ppsnb, mdlnb, ivalue
    character(len=30),dimension(:) :: extP_lbl
    integer(kind=4)  ,dimension(:) :: extP_len
    real(kind=8)     ,dimension(:) :: extP_val
    integer(kind=4)                :: extP_nb, calcD
    ! zone de stockage: gradient,flux,internal,operateur tangent
    real(kind=8),dimension(:)  :: GRAD0,FLUX0,INTERNAL0
    real(kind=8),dimension(:)  :: GRAD1,FLUX1,INTERNAL1
    real(kind=8),dimension(:,:),pointer  :: D
    real(kind=8) :: H

                             !12345678901234567890123456789012345678901234
    character(len=44) :: IAM='noMatLib_ExternalModels::compute_external_pg'

    call faterr(iam,'Error: no MatLib external models available')

  end subroutine compute_external_gp

  subroutine set_ortho_frame(ippset,mdlnb,frame)
    implicit none

    integer(kind=4), intent(in) :: ippset, mdlnb
    real(kind=8)   , intent(in) :: frame(3,3)

                             !1234567890123456789012345678901234567890
    character(len=40) :: IAM='noMatLib_ExternalModels::set_ortho_frame'

    call faterr(iam,'Error: no MatLib external models available')

  end subroutine set_ortho_frame

end module
