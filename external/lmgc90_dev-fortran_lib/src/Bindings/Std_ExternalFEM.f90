!>binding of an external fem code
module ExternalFEM
implicit none
contains

!------------------------------------------------------------------------
subroutine externalFEM_increment(time,time_step)
  implicit none
  real(kind=8) :: time,time_step

!
! PEL initialisation du temps et pas de temps
!

!  call PELupdateBeforeTimeIteration(time,time_step)

end subroutine 

!------------------------------------------------------------------------
subroutine externalFEM_compute_bulk
  implicit none

!
! PEL calcule l etat, les forces interieures, ...
!

!  call PELupdateBeforeNewtonIteration

end subroutine 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_compute_free_vlocy
  implicit none

  integer :: ibdyty

!
! PEL calcule la vitesse libre
! Vfree = M**-1()
!

!  call PELcomputeallVfree

end subroutine 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_get_free_vlocy(ibdyty,vfree)
  implicit none
  integer :: ibdyty
  real(kind=8),dimension(:) :: vfree 

!
! on recupere les Vfree
!

!    print*,'Corps=',ibdyty

!    call PELgetVfree(ibdyty,bdyty(ibdyty)%Vfree)

!    print*,'Vfree=',Vfree

end subroutine 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_check_convergence(iconv)
  implicit none
  integer :: iconv
!
! PEL calcule la norme de convergence et dit si ca CV (1) ou non (0)
!

!  call PELcheckconvergence(iconv)

end subroutine
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!subroutine externalFEM_check_stop(istop)
!  implicit none
!  integer :: istop,iconv
!
! PEL donne un critère d arret: oui (1) ou non (0)
!
!
!  call PELcheckstop(iconv)
!  istop = 1 - iconv
!
!end subroutine
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_update_bulk(istop)
  implicit none
  integer :: istop

!
! PEL a converge pour les comportements volumiques/surfaciques
! il actualise les degres de liberte et son etat: gradient,flux,variables internes/externes
!

!    call PELupdateAfterNewton()
!    call PELupdateAfterTimeIteration()
!    istop = 1
  
end subroutine 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine externalFEM_comp_vlocy_mecaMAILx(ibdyty,R,V)

   !
   ! called by vitrad
   !
  
   implicit none 
   integer :: ibdyty
   real(kind=8),dimension(:) :: R,V

!   call PELcomputeVaux(ibdyty,R,V)

 end subroutine
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine externalFEM_compute_V

   implicit none 

! PEL calcule Vf=Vfree+M**-1 Reac
!   call PELcomputeV()
!
! PEL met a jour les deplacements Xf = Xm + (h*theta*Vf)
!   call PELupdateAfterNewtonIteration()
!
!fd c'est plus la !!   call PELupdateAfterNewton()

 end subroutine
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine externalFEM_get_V(ibdyty,V)
   implicit none
   integer :: ibdyty
   real(kind=8),dimension(:) :: V
   
   !call PELgetV(ibdyty,V)

 end subroutine 
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 subroutine externalFEM_update_position
   implicit none
!
! PEL met a jour les deplacements Xf = Xm + (h*theta*Vf)
!

!   call PELupdateAfterNewtonIteration()

 end subroutine
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
subroutine externalFEM_get_kinematic(ID,nb_nodes,ux,uy,uz,vx,vy,vz)
   implicit none
   integer :: ID,nb_nodes
   real(kind=8),dimension(nb_nodes) :: ux,uy,uz,vx,vy,vz

!   call PELgetkine(ID,ux,uy,uz,vx,vy,vz)

end subroutine 
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
subroutine externalFEM_get_nbval(nb_val)
   implicit none
   integer :: nb_val

!   call PELNbVarSave(nb_val)

end subroutine 
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
SUBROUTINE externalFEM_get_nameval(ival,name)
   IMPLICIT NONE
   INTEGER :: ival
   CHARACTER(len=5) :: name

!   CALL PELgetNameSave(ival,name)

END SUBROUTINE
!------------------------------------------------------------------------
subroutine externalFEM_get_val(ID,nb_nodes,nb_val,val)
   implicit none
   integer :: ID,nb_nodes,nb_val,ival
   real(kind=8),dimension(nb_nodes,nb_val) :: val

   do ival=1,nb_val

!     call PELgetvaluesave(ID,ival,val(:,ival))

   enddo

end subroutine
!------------------------------------------------------------------------
subroutine externalFEM_get_strain( icdan, vec )
  implicit none
  integer :: icdan
  real(kind=8),dimension(:) :: vec

  ! Xper   
  ! call PELgettactstrain(icdan,vec)

end subroutine externalFEM_get_strain
 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_get_straintriaxiality( icdan, val )
  implicit none
  integer :: icdan
  real(kind=8) :: val

  ! Xper
  ! call PELgettactstraintriaxiality(icdan,val)

end subroutine externalFEM_get_straintriaxiality
 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_get_strainratetriaxiality( icdan, val )
  implicit none
  integer :: icdan
  real(kind=8) :: val
  
  ! Xper
  ! call PELgettactstrainratetriaxiality(icdan,val)

end subroutine externalFEM_get_strainratetriaxiality
 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine externalFEM_get_stress( icdan, vec )
  implicit none
  integer :: icdan
  real(kind=8),dimension(:) :: vec

  ! Xper
  ! call PELgettactstress(icdan,vec)

end subroutine externalFEM_get_stress
 
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_get_stresstriaxiality( icdan, val )
  implicit none
  integer :: icdan
  real(kind=8) :: val

  !call PELgettactstresstriaxiality(icdan,val)

end subroutine externalFEM_get_stresstriaxiality
 
!------------------------------------------------------------------------
subroutine externalFEM_get_pressure( icdan, val )
  implicit none
  integer :: icdan
  real(kind=8) :: val

  ! Xper   
  ! call pelgettactpressure(icdan,val)

end subroutine externalFEM_get_pressure

!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine externalFEM_get_temperature( icdan, val )
  implicit none
  integer :: icdan
  real(kind=8) :: val

  ! Xper  
  ! call PELgettacttemperature(icdan,val)

end subroutine externalFEM_get_temperature

!------------------------------------------------------------------------

!------------------------------------------------------------------------
subroutine ExternalFEM_terminate
  implicit none 
 
!  call PELterminate

end subroutine 

!--- adding dummy function of RAM for DDM ---!

subroutine ExternalFEM_compute_dv_from_other_domains
  implicit none 

  !
  !call pelcomputedvfromotherdomains()
  
end subroutine 

subroutine ExternalFEM_get_body_multiplicity(body_id, body_mult ) 
  implicit none
  integer(kind=4) :: body_id, body_mult

  !call pelgetbodymultiplicity( ibody, mult )
  
end subroutine 

end module
