!==========================================================================
!
! Copyright 2000-2022 CNRS-UM.
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

module nlgs_new_int_2D

! shared modules

  use overall

  use parameters

  use tact_behaviour
  use utilities

  use algebra, only : determinant33

!  .. statements for LAPACK95
  use LA_PRECISION, only: WP => DP
  use F95_LAPACK  , only: LA_GESV

! contrib modules

  use DKDKx, only : put_violation_DKDKx
  use CLALp, only : put_violation_CLALp
 
  use CLxxx, only : get_vlocy_CLxxx, &
                    add_reac_CLxxx

  use ALpxx, only : get_vlocy_ALpxx, &
                    add_reac_ALpxx

  use rbdy2, only : nullify_vlocy_rbdy2 => nullify_vlocy, &
                    get_vlocy_rbdy2     => get_vlocy    , &
                    comp_vlocy_rbdy2    => comp_vlocy   , &
                    nullify_reac_rbdy2  => nullify_reac , &
                    add_reac_rbdy2      => add_reac

  use rbdy3, only : nullify_vlocy_rbdy3 => nullify_vlocy, &
                    get_vlocy_rbdy3     => get_vlocy    , &
                    comp_vlocy_rbdy3    => comp_vlocy   , &
                    nullify_reac_rbdy3  => nullify_reac , &
                    add_reac_rbdy3      => add_reac

  use mecaMAILx, only : nullify_vlocy_mecaMAILx, &
                        comp_vlocy_mecaMAILx   , &
                        nullify_reac_mecaMAILx

  use interaction_2D, only : inter_dim          , &
                             T_interaction      , &
                             get_nb_interactions, &
                             get_interaction

  use interaction_laws_2D, only : inter_laws, init_inter_laws

  implicit none
  
  private

  integer(kind=4), parameter :: inter_dof = 3

  real(kind=8), dimension(:), allocatable :: Wab
  integer(kind=4)                         :: nb_CDAN,nb_ENTITY
 
  !> variable indicating if the W Delassus matrix is built or not
  character(len=30) :: Wstorage 

  logical :: SDLactif=.FALSE.

  integer(kind=4), dimension(:), allocatable :: ialeat,ialeatr,iwksg,randomlist
  real(kind=8)   , dimension(:), allocatable :: Xvlton,WRRarray

  integer(kind=4) :: Nnoact,NOKsta,Nactif,Nstick,Nslide
  integer(kind=4) :: Nvnish,Nhover,Nnoctc,Ntract,Ncompr,Nnknow,Nb_RGR
  integer(kind=4) :: NOKweak,NOKstrg,SNstick,SNslide,WNstick,WNslide

!!  !mj randomlist
!!  integer(kind=4) :: IAL1
!!  real(kind=8)    :: RA

  real(kind=8) :: somme_rn
  real(kind=8) :: HH
  
  real(kind=8) :: DVDV,DVDVRR,DVoR,SumDVDV,MaxDVDV,SumDVDVRR,MaxDVDVRR,SumDVoR,SumWRWR,WRR,SumWRR,dynstat
  real(kind=8) :: QuadDV,MaxmDV,QuadDVR,MaxmDVR,QuadWR,MeanWRR,MeanDVoR
  real(kind=8) :: rcvltm,Dreac,Dcrac,Scale=1.D0,Enrg=0.D0
  
  real(kind=8)      :: tol=0.1666D-03,RELAX=1.D0,RELAX1=0.D0,inv_tol=0.6002401D+04

  ! variable indicating the type of iter check test
  integer(kind=4)  :: i_checktype = 1
  character(len=5) :: checktype
  
!!! parameter table -------------------------------------------------------------

!!! nlgs check keyword
  integer(kind=4), parameter :: i_Quad = 1 , i_Maxm = 2 , i_QMs16 = 3 , i_QuadN = 4

!!! nlgs keyword
  integer(kind=4), parameter :: i_iter = 1 , i_check = 2 , i_post = 3

! RGR CRITIC
  real(kind=8), parameter :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)

  integer(kind=4) :: nlgs_loop,norm_fich
  logical         :: norm_check=.false.,diagonal_resolution=.false.

! pour un calcul de l'evolution de la reaction
  real(KIND=8), dimension(inter_dim) :: sum_,dsum_

  public active_diagonal_resolution    , &
         bimodal_list_nlgs             , &
         comp_check_nlgs               , &
         display_check_nlgs            , &
         quick_scramble_nlgs           , &
         reverse_nlgs                  , &
         scale_rloc_nlgs               , &
         scramble_nlgs                 , &
         solve_nlgs                    , &
         write_norm_check_nlgs         , &
         RnodHRloc_nlgs                , &
         compute_local_free_vlocy      , &
         display_rlocn_sum_nlgs        , &
         update_tact_behav_nlgs        , &
         set_nlgs_parameter            , &
         prep_nlgs                     , &
         prep_check_nlgs               , &
         Nullify_EntityList_nlgs       , &
         !init_cohe_nlgs                , &
         get_error                     , &
         get_conv                      , &
         compute_convergence_norms_nlgs, & !am: functions for check convergence in DDM
         check_convergence_nlgs

  public get_nlgs_loop, get_nlgs_network_change,get_nlgs_contact_status,&
         get_after_iter_check, get_somme_rn, &
         get_all_this

contains

 include 'inc_nlgs_new_int.f90'

 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 !> \brief Compute velocity in local frame
 subroutine prjj(this,vik,storage)
   implicit none
   !> interaction
   type(T_interaction), intent(in) :: this
   !> velocity
   real(kind=8), dimension(inter_dim), intent(out) :: vik
   !> global velocity type to get
   integer(kind=4), intent(in) :: storage
   !
   real(kind=8), dimension(inter_dof) :: Vcd, Van
   character(len=21) :: IAM
   character(len=80) :: cout
   !      123456789012345678901
   IAM = 'nlgs_new_int_2D::prjj'
  
   vik(:) = 0.d0

   select case( this%icdtyp )
   case( i_diskx )!, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     call get_vlocy_rbdy2(this%icdbdy,storage, Vcd(1:3))
     vik(:) = vik(:) + Vcd(3)*this%Gcd(1:2,1)
   case( i_clxxx )!, i_diskl, i_pt2dl )
     call get_vlocy_CLxxx(this%icdtac,storage, Vcd(1:2))
   case default
     write(cout,'(A,1x,A)') get_contactor_name_from_id(this%icdtyp) ,'contactor is not implemented'
     call faterr(IAM,cout)
   end select
 
   select case( this%iantyp )
   case( i_diskx )!, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     call get_vlocy_rbdy2(this%ianbdy,storage, Van(1:3))
     vik(:) = vik(:) - Van(3)*this%Gan(1:2,1)
   case( i_alpxx )!, i_diskl, i_pt2dl )
     call get_vlocy_ALpxx(this%iantac,this%ian_al,storage,Van(1:2),this%cpcd)
   case default
     write(cout,'(A,1x,A)') get_contactor_name_from_id(this%iantyp) ,'contactor is not implemented'
     call faterr(IAM,cout)
   end select

   vik(1) = vik(1) + dot_product(Vcd(1:inter_dim)-Van(1:inter_dim),this%uc(1:inter_dim,1))
   vik(2) = vik(2) + dot_product(Vcd(1:inter_dim)-Van(1:inter_dim),this%uc(1:inter_dim,2))

 end subroutine prjj
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 !> \brief Compute reaction in global frame and add it to involved bodies
 subroutine injj(this,rik,storage)
   implicit none
   !> interaction
   type(T_interaction), intent(in) :: this
   !> reaction in local frame
   real(kind=8), dimension(inter_dim), intent(in) :: rik
   !> global reaction type to set
   integer(kind=4), intent(in) :: storage
   !
   integer(kind=4), dimension(inter_dof) :: cdccdof, anccdof
   real(kind=8)   , dimension(inter_dof) :: cdreac, anreac
   CHARACTER(len=21) :: IAM
   CHARACTER(len=80) :: cout
   !     123456789012345678901
   IAM ='nlgs_new_int_2D::injj'
  
   select case( this%icdtyp )
   case( i_diskx )!, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     cdccdof(1:inter_dim+1) = (/1,2,3/)
     cdreac(1)  = dot_product( rik(1:inter_dim), this%uc(1,1:inter_dim)  )
     cdreac(2)  = dot_product( rik(1:inter_dim), this%uc(2,1:inter_dim)  )
     cdreac(3)  = dot_product( rik(1:inter_dim), this%Gcd(1:inter_dim,1) )
     call add_reac_rbdy2(this%icdbdy, cdccdof(1:inter_dof), cdreac(1:inter_dof),storage)
   case( i_clxxx )!, i_diskl, i_pt2dl )
     cdreac(1) = dot_product( rik(1:inter_dim), this%uc(1,1:inter_dim) )
     cdreac(2) = dot_product( rik(1:inter_dim), this%uc(2,1:inter_dim) )
     call add_reac_CLxxx(this%icdtac,cdreac(1:2),storage)
   case default
     write(cout,'(A,1x,A)') get_contactor_name_from_id(this%icdtyp) ,'contactor is not implemented'
     call faterr(IAM,cout)
   end select
 
   select case( this%iantyp )
   case( i_diskx )!, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     anccdof(1:inter_dim+1) = (/1,2,3/)
     anreac(1:2)= -cdreac(1:2)
     anreac(3)  = -dot_product( rik(1:inter_dim), this%Gan(1:inter_dim,1) )
     call add_reac_rbdy2(this%ianbdy, anccdof(1:inter_dof), anreac(1:inter_dof),storage)
   case( i_alpxx )!, i_diskl, i_pt2dl )
     anreac(1:2) = -cdreac(1:2)
     call add_reac_ALpxx(this%iantac,this%ian_al,anreac(1:2),this%cpcd,storage)
   case default
     write(cout,'(A,1x,A)') get_contactor_name_from_id(this%iantyp) ,'contactor is not implemented'
     call faterr(IAM,cout)
   end select

 end subroutine injj

 subroutine non_uniqueness(this)
   implicit none
   type(T_interaction) :: this
   !
   real(kind=8) :: det, backward, forward
   character(len=31) :: IAM
   character(len=80) :: cout
   !      1234567890123456789012345678901
   IAM = 'nlgs_new_int_2D::non_uniqueness'

   det      = this%WW(1)*this%WW(2)-this%W(1,2)*this%W(2,1)
   this%det = det
   
   if (det < 1.d-24                .and. &
       this%i_law /= i_elastic_rod .and. &
       this%i_law /= i_voigt_rod         ) then

      write(cout,565) this%icdan,det
565   format(1X,'    WWtt*WWnn-Wtn*Wnt (',I5,') =',D12.5,' < 1.D-24')
      call logmes(cout)
   end if

   forward = 1.d0-this%fric*this%W(2,1)/this%WW(2)
   this%forward = forward

   if (forward .le. 1.D-18) then
      write(cout,'(2(1x,D14.7))') this%W(2,2),this%W(2,1)
      call logmes(cout)
      write(cout,'(2(1x,D14.7))') this%W(1,2),this%W(1,1)
      call logmes(cout)
      write(cout,'(1(1x,D14.7))') this%fric
      call logmes(cout)
      call print_info(this%icdan) 
      write(cout,521) this%icdan,this%icdan,this%icdan
521   format(1X,'   1 - fric(',I5,')*Wnt(',I5,')/WWnn(',I5,') < 1.D-18')
      call faterr(IAM,cout)
   end if
   
   backward = 1.d0+this%fric*this%W(2,1)/this%WW(2)
   this%backward=backward
   
   if (backward .le. 1.D-18) then
      write(cout,'(2(1x,D14.7))') this%W(2,2),this%W(2,1)
      call logmes(cout)
      write(cout,'(2(1x,D14.7))') this%W(1,2),this%W(1,1)
      call logmes(cout) 
      write(cout,'(1(1x,D14.7))') this%fric
      call logmes(cout)
      call print_info(this%icdan) 
      write(cout,522) this%icdan,this%icdan,this%icdan
522   format(1X,'   1 + fric(',I5,')*Wnt(',I5,')/WWnn(',I5,') < 1.D-18')
      call faterr(IAM,cout)
   end if

 end subroutine

 subroutine put_violation(cdan, icdan, violation)
   implicit none
   !> contact type
   integer(kind=4), intent(in) :: cdan
   !> contact number
   integer(kind=4), intent(in) :: icdan
   !> violation value
   real(kind=8)   , intent(in) :: violation
   !
   character(len=80) :: cout

   select case (cdan)
   case (i_dkdkx)
     call put_violation_DKDKx(icdan,violation)
   case (i_clalp)
     call put_violation_CLALp(icdan,violation)
   case default
     write(cout,'(A,1x,A)') get_interaction_name_from_id(cdan),'interaction is not implemented'
     call faterr('nlgs_newint_3D::put_violation',cout)
   end select

 end subroutine

end module nlgs_new_int_2D

