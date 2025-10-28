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

module DDM_ExternalFEM

use LMGC90_MPI
!use MPI
use overall
use mailx, only : get_nb_MAILx
use mecamailx, only : write_xxx_dof_mecaMAILx, &
                      get_ptr_body_vector_mecaMAILx
use ExternalFEM

use clalp
use csasp

use inter_meca_handler_2D, only : get_ianbdy_2D => get_ianbdy, &
                                  get_icdbdy_2D => get_icdbdy
use inter_meca_handler_3D, only : get_ianbdy_3D => get_ianbdy, &
                                  get_icdbdy_3D => get_icdbdy

implicit none

private 

!integer(kind=4) :: NB_MAILX        ! nombre de corps maillés

public                                          &
   set_working_directory_in_DDM_ExternalFEM    ,&
   sub_get_contacts_interface                  ,&
   compute_dv_from_other_domains               ,&
   reduce_over_domains_convergence_elements_2D ,&
   reduce_over_domains_convergence_elements_3D ,&
   broadcast_of_iconv                          
   

contains

!!!----------------------------------------------------------------------------------------------------------
subroutine set_working_directory_in_DDM_ExternalFEM
    implicit none

    ! variables locales
    character(len=13) :: working_directory ! pour calculer le nom du dossier d'ecriture des DOF, GPV et Vloc
    integer           :: sdm

    ! Determination du sous-domaine qui ecrit
    sdm=rang_COMM_WORLD+1

    working_directory='DDM_WD_xxxxx/'
    write(working_directory(8:12), '(I5.5)') sdm

    call set_working_directory(location(working_directory))

end subroutine set_working_directory_in_DDM_ExternalFEM

!!!----------------------------------------------------------------------------------------------------------

subroutine sub_get_contacts_interface( get_contacts_interface )
   
   implicit none
   !out
   integer(kind=4), dimension(:), allocatable :: get_contacts_interface
   
   integer(kind=4) :: i_cdan, nb_cdan, cd, an, cd_mult, an_mult, nb_contacts_interface
   
   if( .not. allocated( get_contacts_interface ) ) then
      if( nbDIME == 2 ) then
         nb_cdan = get_nb_CLALp( i_real_tactor )
         nb_contacts_interface = 0
         do i_cdan=1, nb_cdan
            cd = get_icdbdy_2D( i_clalp, i_cdan )
            an = get_ianbdy_2D( i_clalp, i_cdan )
            call externalFEM_get_body_multiplicity( cd, cd_mult ) 
            call externalFEM_get_body_multiplicity( an, an_mult )
            if( (cd_mult + an_mult) > 2 ) then
               nb_contacts_interface = nb_contacts_interface +1
            end if
         end do
         allocate( get_contacts_interface( nb_contacts_interface ) )
         nb_contacts_interface = 0
         do i_cdan=1, nb_cdan
            cd = get_icdbdy_2D( i_clalp, i_cdan )
            an = get_ianbdy_2D( i_clalp, i_cdan )
            call externalFEM_get_body_multiplicity( cd, cd_mult ) 
            call externalFEM_get_body_multiplicity( an, an_mult )
            if( (cd_mult + an_mult) > 2 ) then
               nb_contacts_interface = nb_contacts_interface +1
               get_contacts_interface( nb_contacts_interface ) = i_cdan
            end if
         end do
      else
         nb_cdan = get_nb_CSASx( i_real_tactor )
         nb_contacts_interface = 0
         do i_cdan=1, nb_cdan
            cd = get_icdbdy_3D( i_csasp, i_cdan )
            an = get_ianbdy_3D( i_csasp, i_cdan )
            call externalFEM_get_body_multiplicity( cd, cd_mult ) 
            call externalFEM_get_body_multiplicity( an, an_mult )
            if( (cd_mult + an_mult) > 2 ) then
               nb_contacts_interface = nb_contacts_interface +1
            end if
         end do
         allocate( get_contacts_interface( nb_contacts_interface ) )
         nb_contacts_interface = 0
         do i_cdan=1, nb_cdan
            cd = get_icdbdy_3D( i_csasp, i_cdan )
            an = get_ianbdy_3D( i_csasp, i_cdan )
            call externalFEM_get_body_multiplicity( cd, cd_mult ) 
            call externalFEM_get_body_multiplicity( an, an_mult )
            if( (cd_mult + an_mult) > 2 ) then
               nb_contacts_interface = nb_contacts_interface +1
               get_contacts_interface( nb_contacts_interface ) = i_cdan
            end if
         end do
      end if
   end if
   
end subroutine sub_get_contacts_interface  

!!!----------------------------------------------------------------------------------------------------------

subroutine compute_dv_from_other_domains
   implicit none
 
   call externalFEM_compute_dv_from_other_domains()

end subroutine compute_dv_from_other_domains

!!!----------------------------------------------------------------------------------------------------------

subroutine reduce_over_domains_convergence_elements_2D

   use nlgs
   use timer
   
   implicit none

   integer(kind=4)            :: Nactif, Nactif4all
   real(kind=8)               :: tol
   real(kind=8), dimension(5) :: Sums, Sums4all
   real(kind=8), dimension(2) :: Maxima, Maxima4all
   
   real(kind=8)               :: QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR
   !integer(kind=4) :: timer_id_err = 0
   !integer(kind=4) :: code_MPI1 

   ! Getting convergence elements
   !if( timer_id_err == 0 ) timer_id_err = get_new_itimer_ID('-- dont comm        ')
   !if( timer_id_err /= 0 ) call start_itimer(timer_id_err)
   call get_error( Nactif_= Nactif, &
                   SumWRWR_=Sums(1), SumDVDV_=Sums(2), SumWRR_=Sums(3), SumDVDVRR_=Sums(4), SumDVoR_=Sums(5), &
                   MaxDVDV_=Maxima(1), MaxDVDVRR_=Maxima(2), &
                   tol_=tol )
   !if( timer_id_err /= 0 ) call stop_itimer(timer_id_err)
    !if( timer_id_err /= 0 ) call start_itimer(timer_id_err)
    ! World sum of Nactif to rank 0
    call MPI_REDUCE(Nactif, Nactif4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    !if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
                       
    ! on calcule :
    !    * pour chaque somme (SumWRWR, ...) la somme des sommes collectees sur chaque sdm et on l'envoie sur le processus 0
    ! World sum of convergence elements of type "sum"
    call MPI_REDUCE(Sums, Sums4all, 5, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    !if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    !    * pour chaque max (MaxDVDV, MaxDVDVRR) le max des max collectes sur chaque sdm et on l'envoie sur le processus 0
    ! World max of convergence elements of type "max"
    call MPI_REDUCE(Maxima, Maxima4all, 2, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code_MPI)
    !if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    !if( timer_id_err /= 0 ) call stop_itimer(timer_id_err)
    
    if (rang_COMM_WORLD == 0) then
 
       ! on calcule les quantites necessaires pour statuer de la convergence sur le maitre
       ! Computation of convergence norms ( computed in solve_nlgs(2) in sequential )
       call compute_convergence_norms_nlgs(Nactif4all, Sums4all(1), Sums4all(2), Maxima4all(1), &
                                           Sums4all(3), Sums4all(4), Maxima4all(2), Sums4all(5), tol, &
                                           QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR) 
                                           
       ! Pour le post traitement SOLVER INFORMATION
       ! Replace sequential computation                                   
       call put_convergence_norms_nlgs( QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR)

    end if

end subroutine reduce_over_domains_convergence_elements_2D
!!!----------------------------------------------------------------------------------------------------------
subroutine reduce_over_domains_convergence_elements_3D

   use nlgs_3D
   
   implicit none

   integer(kind=4)            :: Nactif, Nactif4all
   real(kind=8)               :: tol
   real(kind=8), dimension(5) :: Sums, Sums4all
   real(kind=8), dimension(2) :: Maxima, Maxima4all
   
   real(kind=8)               :: QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR

   ! Getting convergence elements
   call get_error( Nactif_= Nactif, &
                   SumWRWR_=Sums(1), SumDVDV_=Sums(2), SumWRR_=Sums(3), SumDVDVRR_=Sums(4), SumDVoR_=Sums(5), &
                   MaxDVDV_=Maxima(1), MaxDVDVRR_=Maxima(2), &
                   tol_=tol )     
    
    ! World sum of Nactif to rank 0
    call MPI_REDUCE(Nactif, Nactif4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
                       
    ! on calcule :
    !    * pour chaque somme (SumWRWR, ...) la somme des sommes collectees sur chaque sdm et on l'envoie sur le processus 0
    ! World sum of convergence elements of type "sum"
    call MPI_REDUCE(Sums, Sums4all, 5, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    !    * pour chaque max (MaxDVDV, MaxDVDVRR) le max des max collectes sur chaque sdm et on l'envoie sur le processus 0
    ! World max of convergence elements of type "max"
    call MPI_REDUCE(Maxima, Maxima4all, 2, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    
    
    if (rang_COMM_WORLD == 0) then
 
       ! on calcule les quantites necessaires pour statuer de la convergence sur le maitre
       ! Computation of convergence norms ( computed in solve_nlgs(2) in sequential )
       call compute_convergence_norms_nlgs(Nactif4all, Sums4all(1), Sums4all(2), Maxima4all(1), &
                                           Sums4all(3), Sums4all(4), Maxima4all(2), Sums4all(5), tol, &
                                           QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR) 
                                           
       ! Pour le post traitement SOLVER INFORMATION
       ! Replace sequential computation                                   
       call put_convergence_norms_nlgs( QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR)

    end if

end subroutine reduce_over_domains_convergence_elements_3D
!!!----------------------------------------------------------------------------------------------------------


subroutine broadcast_of_iconv( iconv )

   implicit none
   
   integer(kind=4)            :: iconv
   
   ! rank 0 send iconv to all
   call MPI_BCAST( iconv, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
   if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

end subroutine broadcast_of_iconv
!!!----------------------------------------------------------------------------------------------------------

end module DDM_ExternalFEM
