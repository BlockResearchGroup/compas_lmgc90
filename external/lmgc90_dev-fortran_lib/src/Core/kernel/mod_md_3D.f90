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

!


module md_3D
!!****h* LMGC90.CORE/MD_3D
!! NAME
!!  module MD_3D
!! CHILDREN
!!  LMGC90.CORE/CHIC
!!  LMGC90.CORE/OVERALL
!!  LMGC90.CORE/TACT_BEHAVIOUR
!!  LMGC90.CORE/UTILITIES
!!  LMGC90.CORE/SPSPx
!!  LMGC90.CORE/SPPLx
!!  LMGC90.CORE/SPCDx
!!  LMGC90.CORE/RBDY3
!! FUNCTION
!! This is the 3D version of the molecular dynamics solver.
!! The computation of the contact law is not rigourously define.
!! Before use this solver check the computation law .
!!****

! shared modules

 use chic
 use overall
 use tact_behaviour
 use utilities

!$ use timer

! contrib modules

 use SPSPx
 use SPPLx
 use SPCDx
 use RBDY3

 implicit none

 contains

!------------------------------------------------------------------------
 subroutine chic_command_md_3D

   implicit none
   integer :: ik

 if (INDEX(CHZZZ,'EX ITER MD')==1) then
   call LOGCHIC(' ')

!$   call start_timer('md                     ')
   call iter_md
!$   call  stop_timer('md                     ')
   IETZZZ = 1
   return
 endif

!                 12345678
 if (INDEX(CHZZZ,'EX PREP MD')==1) then
   call LOGCHIC(' ')

!$   call start_timer('prep_md                ')
   call prep_md
!$   call  stop_timer('prep_md                ')
   IETZZZ = 1
   return
 endif

 end subroutine chic_command_md_3D
!------------------------------------------------------------------------
 subroutine prep_md

 end subroutine prep_md
!------------------------------------------------------------------------
 subroutine iter_md

! edition 28-02-2002

   implicit none
                            !123456
   character(len=6)  :: IAM='md::md'
   character(len=80) :: cout
   character(len=5)  :: status

   integer           :: icdan,nb_SPSPx,nb_SPPLx,lawnb
   real(kind=8)      :: kn,dampn,gamma,finet,mu,norm,normt
   real(kind=8)      :: gap,meff,reff,fadhs
   real(kind=8)      :: vijn,vijs,vijt,fijn,fijs,fijt
   real(kind=8)      :: fext1,fext2,fext3

   meff = 0.0
   reff = 0.0
   lawnb= 0.0
   kn   = 0.0
   dampn= 0.0
   gamma= 0.0
   finet= 0.0
   mu   = 0.0

!-----begin
! get all variable for calculate force

   ! get number of sphere-sphere

   nb_SPSPx = get_nb_SPSPx()

   do icdan=1,nb_SPSPx

     ! get locale relative velocity, gap and status
     call get_locBegin_SPSPx(icdan,vijs,vijt,vijn,gap,status)

     if (gap .ge. 0.d0) then

       fijn=0.d0
       fijs=0.d0
       fijt=0.d0
       status='noctc'
     else       


     ! get mass and radius effective
     call get_eff_SPSPx(icdan,meff,reff)
     
     ! get law number
     lawnb=get_tact_lawnb_SPSPx(icdan)

     ! get constant value
     call get_md_param(lawnb,kn,dampn,gamma,finet,mu)
     
     ! calculate reaction force

     gap=dabs(gap)

     fijn  = (-1.d0*gap*kn) + (dampn*vijn*dsqrt(meff)) + (gamma*dsqrt(gap*reff))

     fadhs = gamma*gamma*reff/(4.*kn)
     norm  = sqrt(vijs*vijs+vijt*vijt)
     normt = dmin1(finet*norm,mu*dabs(-fijn+fadhs))
     
     fijs  = normt*sign(1.d0,vijs)
     fijt  = normt*sign(1.d0,vijt)
     status='  ctc'

     endif

     ! update reaction force
     call injj_SPSPx(icdan,fijs,fijt,-fijn,iIreac)

     call put_loc_SPSPx(icdan,status,0.d0,0.d0,0.d0,fijs,fijt,-fijn,0.d0)


     call put_violation_SPSPx(icdan,gap)

   end do

   ! get number of disk-disk
   nb_SPPLx = get_nb_SPPLx()

   do icdan=1,nb_SPPLx

     ! get locale relative velocity, gap and status
     call get_locBegin_SPPLx(icdan,vijs,vijt,vijn,gap,status)

     if (gap .ge. 0.d0) then

       fijn=0.d0
       fijs=0.d0
       fijt=0.d0
       status='noctc'
     else       


     ! get mass and radius effective
     call get_eff_SPPLx(icdan,meff,reff)

     ! get law number
     lawnb=get_tact_lawnb_SPSPx(icdan)

     ! get constant value
     call get_md_param(lawnb,kn,dampn,gamma,finet,mu)

     !print*,icdan,meff,reff,lawnb,kn,dampn,gamma,finet,mu

     ! calculate reaction force

     gap=dabs(gap)

     fijn  = (-1.d0*gap*kn) + (dampn*vijn*dsqrt(meff)) + (gamma*dsqrt(gap*reff))

     fadhs = gamma*gamma*reff/(4.*kn)
     norm  = sqrt(vijs*vijs+vijt*vijt)
     normt = dmin1(finet*norm,mu*dabs(-fijn+fadhs))
     
     fijs  = normt*sign(1.d0,vijs)
     fijt  = normt*sign(1.d0,vijt)
     status='  ctc'

     endif

     ! update reaction force
     call injj_SPPLx(icdan,fijs,fijt,-fijn,iIreac)

     call put_loc_SPPLx(icdan,status,0.d0,0.d0,0.d0,fijs,fijt,-fijn,0.d0)

     call put_violation_SPPLx(icdan,gap)

   end do

 end subroutine iter_md
!------------------------------------------------------------------------

end module md_3D
!$Log: mod_md_3D.f90,v $
!Revision 1.1  2004/09/07 11:50:27  renouf
!solveur md 3D is here,
!mr
!
