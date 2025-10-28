!==========================================================================
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

module interaction_laws_3D

  use parameters

  use utilities, only: logmes, faterr

  use overall, only : H, NSTEP

  use interaction_3D, only: inter_dim, &
                            T_interaction

  implicit none

  private

  !> Type holding a pointer on function working on an interaction
  type interaction_law_behaviours
    procedure(prep_law_proc) , pointer, nopass :: prep_func  => null()
    procedure(iter_law_proc) , pointer, nopass :: iter_func  => null()
    procedure(check_law_proc), pointer, nopass :: check_func => null()
    procedure(post_law_proc) , pointer, nopass :: post_func  => null()
  end type
  
  !> Abstract interface defining the prototype of function working on an interaction
  abstract interface
    subroutine prep_law_proc(inter, is_init)
      use interaction_3D, only : T_interaction
      implicit none
      type(T_interaction) :: inter
      logical, intent(in) :: is_init
    end subroutine
  end interface

  abstract interface
    subroutine iter_law_proc(inter, rlik, vlik, vlfik, Wrlik, WWik, rliki, vliki, stat)
      use interaction_3D, only : T_interaction, inter_dim
      implicit none
      type(T_interaction) :: inter
      real(kind=8), dimension(inter_dim), intent(in)    :: rlik
      real(kind=8), dimension(inter_dim), intent(in)    :: vlik
      real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
      real(kind=8), dimension(inter_dim), intent(in)    :: Wrlik
      real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
      real(kind=8), dimension(inter_dim), intent(out)   :: rliki
      real(kind=8), dimension(inter_dim), intent(inout) :: vliki
      character(len=5), intent(inout) :: stat
    end subroutine
  end interface

  abstract interface
    subroutine check_law_proc(inter, is_init)
      use interaction_3D, only : T_interaction
      implicit none
      type(T_interaction) :: inter
      logical, intent(in) :: is_init
    end subroutine
  end interface

  abstract interface
    subroutine post_law_proc(inter)
      use interaction_3D, only : T_interaction
      implicit none
      type(T_interaction) :: inter
    end subroutine
  end interface

  !> Defining an array of size 4xnb_laws, to store for each interaction law
  !> behaviour in solver (prep, iter, check, post)
  type(interaction_law_behaviours), dimension(total_interaction_law_number), public :: inter_laws

  real(kind=8), parameter :: Oneover2pi = 1.d0/(2.d0*PI_g)
  !real(kind=8), parameter :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)

  public init_inter_laws

  contains

  include 'inc_interaction_law.f90'
  
  subroutine init_inter_laws(inter_dim, diagonal_resolution)
    implicit none
    integer(kind=4), intent(in) :: inter_dim
    logical        , intent(in) :: diagonal_resolution
    !
    integer(kind=4)   :: i_law, i
    character(len=80) :: cout

    do i_law = 1, total_interaction_law_number
      select case( i_law )
      case( i_iqs_clb, i_iqs_ds_clb )
        inter_laws(i_law)%prep_func => iqs_clb_prep
        if( diagonal_resolution ) then
          inter_laws(i_law)%iter_func => mu_clb_diag_solver_3d
        else
          inter_laws(i_law)%iter_func => mu_clb_solver_3d
        end if
      !!case( i_iqs_clb_g0 )
      !!  inter_laws(i_law)%prep_func => iqs_clb_g0_prep_3d
      !!case( i_iqs_clb_rgr )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => iqs_clb_rgr_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => iqs_clb_rgr_prep
      !!  end if
      !!case( i_rst_clb, i_rst_ds_clb )
      !!  inter_laws(i_law)%prep_func => rst_clb_prep
      !!case( i_rst_wet_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => rst_wet_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => rst_wet_clb_prep
      !!  end if
      case( i_gap_sgr_clb, i_gap_sgr_ds_clb )
        inter_laws(i_law)%prep_func => gap_sgr_clb_prep
        if( diagonal_resolution ) then
          inter_laws(i_law)%iter_func => mu_clb_diag_solver_3d
        else
          inter_laws(i_law)%iter_func => mu_clb_solver_3d
        end if
      !!case( i_vel_sgr_clb )
      !!  inter_laws(i_law)%prep_func => vel_sgr_clb_prep
      !!case( i_gap_wet_ds_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => gap_wet_ds_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => gap_wet_ds_clb_prep
      !!  end if
      !!case( i_iqs_wet_ds_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => iqs_wet_ds_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => iqs_wet_ds_clb_prep
      !!  end if
      !!case( i_iqs_mohr_ds_clb )
      !!  inter_laws(i_law)%prep_func => iqs_mohr_ds_clb_prep
      !!case( i_elastic_repell_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_repell_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_repell_clb_prep
      !!  end if
      !!case( i_critical_voigt_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => critical_voigt_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => critical_voigt_clb_prep
      !!  end if
      !!case( i_elastic_repell_wet_clb )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_repell_wet_clb_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_repell_wet_clb_prep
      !!  end if
      !!case( i_visco_elastic_repell_wet )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => visco_elastic_repell_wet_prep_3d_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => visco_elastic_repell_wet_prep_3d
      !!  end if
      !!case( i_skf_grease )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => skf_grease_prep_3d_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => skf_grease_prep_3d
      !!  end if
      !!case( i_elastic_rod )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_prep_diag
      !!    inter_laws(i_law)%iter_func => rod_iter_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_prep
      !!    inter_laws(i_law)%iter_func => rod_iter_3d
      !!  end if
      !!case( i_elastic_wire )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => elastic_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => elastic_prep
      !!  end if
      !!case( i_brittle_elastic_wire )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => brittle_elastic_wire_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => brittle_elastic_wire_prep
      !!  end if
      !!case( i_voigt_rod )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep
      !!  end if
      !!case( i_voigt_wire )
      !!  if( diagonal_resolution ) then
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep_diag
      !!  else
      !!    inter_laws(i_law)%prep_func => voigt_rod_prep
      !!  end if
      !!case( i_rigid_wire, i_tex_sol_unilat )
      !!  inter_laws(i_law)%prep_func => rigid_wire_prep
      !!case( i_mac_czm, i_msmp_czm, i_mal_czm, i_mp_czm, i_mp3_czm, i_th_czm, i_rld_czm )
      !!  inter_laws(i_law)%prep_func => czm_prep_3d
      !!case( i_iqs_mac_czm, i_iqs_mal_czm )
      !!  inter_laws(i_law)%prep_func => iqs_czm_prep_3d
      !!case( i_coupled_dof, i_plastic_coupled_dof, i_tangential_coupled_dof, i_normal_coupled_dof )
      !!  !nothing to do
      case default
        write(cout,'(A,1x,A)') '[interaction_laws] Cannot init law', get_inter_law_name_from_id(i_law)
        call logmes(cout)
      end select
    end do

  end subroutine 

  !===============================================================================!
  ! SOLVER LIST : coulomb_friction                                                !
  !               coupled_dof                                                     !
  !               normal_coupled_dof                                              !
  !               tangential_coupled_dof                                          !
  !               plastic_coupled_dof                                             !
  !               wire                                                            !
  !               czm                                                             !
  !===============================================================================!

  !> \brief Coulomb's friction solver for 3D
  subroutine mu_clb_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik
    !
    real(kind=8)                :: Wrlnik,Wrltik,Wrlsik
    real(kind=8)                :: Wrlniki,Wrltiki,Wrlsiki,rlsiki,rltiki,rlniki
    real(kind=8)                :: rlvn,rlvt,rlvs,rlv,fricik
    real(kind=8)                :: phi3,phis4,phit4,rlv3,rlv_1,Wrefik,Erwik,Ntol=0.1d-05,fric_inew
    real(kind=8)                :: Att,Ass,Ats,Ast,Btn,Bsn,Btt,Bst,Bts,Bss
    real(kind=8),dimension(3)   :: Err,Erv,det
    real(kind=8),dimension(3,3) :: AWB
    integer                     :: inew,ik
    integer                     :: INEWTX=15

    real(kind=8)                :: delta,inv_delta,A,B,C
    
    !calcul préliminaire fait dans le prep: choix de ron et rot rendant contractantes les applications
    ! rn --> hrn - ronun et rt--> hrt - rotut

    Wrlnik = Wrlik(2)
    Wrltik = Wrlik(1)
    Wrlsik = Wrlik(3)

    rliki(:) = rlik(:)

    do inew = 1, INEWTX ! début des itérations de Newton. INEWTX est le nombre 
                        ! d'itérations maximum, fixé par COMMAND.DAT.
      Wrlniki = Wrlnik  ! On cherche à exprimer la matrice gradient:
      Wrltiki = Wrltik  ! Dphi=| I  -W  |
      Wrlsiki = Wrlsik  !      | Ap  Bp | 
      rlsiki  = rliki(3) ! p étant l'indice inew.
      rltiki  = rliki(1) ! On effectue tour à tour les projections sur le cone
      rlniki  = rliki(2) ! positif et sur le cone de Coulomb pour exprimer les différentes
                         ! valeurs des matrices Ap et Bp.
      rlvn = rliki(2) - inter%ron*(Wrlnik+vlfik(2))
      if( rlvn >= 0.d0 ) then          ! Projection sur le cone positif
        phi3 = inter%ron*(Wrlnik+vlfik(2))
        AWB(1,1) = inter%ron*WWik(2,2)
        AWB(1,2) = inter%ron*WWik(2,1)
        AWB(1,3) = inter%ron*WWik(2,3)
      else
        phi3=rliki(2)
        AWB(1,1) = 1.d0
        AWB(1,2) = 0.d0
        AWB(1,3) = 0.d0
        statusik = 'noctc'
      end if

      rlvt   = rliki(1)-inter%rot*(Wrltik+vlfik(1))
      rlvs   = rliki(3)-inter%rot*(Wrlsik+vlfik(3))
      rlv    = sqrt(rlvt*rlvt+rlvs*rlvs)

      fric_inew = inter%fric
      fricik    = fric_inew*rliki(2)

      if( (rlv<fricik) .or. (rlv<1.d-24) ) then  ! Projection sur le cone de Coulomb
        phit4 = inter%rot*(Wrltik+vlfik(1))
        phis4 = inter%rot*(Wrlsik+vlfik(3))
        ! composantes TN,TT et TS.
        AWB(2,1) = inter%rot*WWik(1,2)
        AWB(2,2) = inter%rot*WWik(1,1)
        AWB(2,3) = inter%rot*WWik(1,3)
        ! composantes TN,TT et TS.
        AWB(3,1) = inter%rot*WWik(3,2)
        AWB(3,2) = inter%rot*WWik(3,1)
        AWB(3,3) = inter%rot*WWik(3,3)
        statusik = 'stick'
      else
        rlv_1 = 1.d0/rlv
        phit4 = rliki(1)-fricik*rlvt*rlv_1
        phis4 = rliki(3)-fricik*rlvs*rlv_1
        Btn   = -fric_inew*rlvt*rlv_1
        Bsn   = -fric_inew*rlvs*rlv_1
        rlv3  = fricik*rlv_1*rlv_1*rlv_1
        Att   = rlv3*rlvs*rlvs ; Ats =-rlv3*rlvt*rlvs
        Ast   = Ats            ; Ass = rlv3*rlvt*rlvt
        Btt   = 1.d0-Att       ; Bts = -Ats
        Bst   = -Ast           ; Bss = 1.d0-Ass
        Att   = inter%rot*Att  ; Ass = inter%rot*Ass
        Ast   = inter%rot*Ast  ; Ats = inter%rot*Ats
        ! composantes TN,TT et TS.
        AWB(2,1) = Att*WWik(1,2) + Ats*WWik(3,2) + Btn
        AWB(2,2) = Att*WWik(1,1) + Ats*WWik(3,1) + Btt
        AWB(2,3) = Att*WWik(1,3) + Ats*WWik(3,3) + Bts

        ! composantes TN,TT et TS.
        AWB(3,1) = Ast*WWik(1,2) + Ass*WWik(3,2) + Bsn
        AWB(3,2) = Ast*WWik(1,1) + Ass*WWik(3,1) + Bst
        AWB(3,3) = Ast*WWik(1,3) + Ass*WWik(3,3) + Bss

        statusik = 'slide'
      end if

      ! Calcul de l'inverse de AWB
    
      det(1) = AWB(2,2)*AWB(3,3) - AWB(2,3)*AWB(3,2)
      det(2) = AWB(2,1)*AWB(3,3) - AWB(3,1)*AWB(2,3)
      det(3) = AWB(2,1)*AWB(3,2) - AWB(3,1)*AWB(2,2)

      delta = AWB(1,1)*det(1) - AWB(1,2)*det(2) + AWB(1,3)*det(3)

      if(abs(delta) <= 1.d-24) then
         err=1
         print*,'WARNING! A.W+B is not inversible'
         print*,' IN inverse33 nlgs3D, NG_solver'
         stop
      end if
      
      inv_delta = 1.d0/delta

      A=det(1)*phi3-(AWB(1,2)*AWB(3,3)-AWB(3,2)*AWB(1,3))*phit4+(AWB(1,2)*AWB(2,3)-AWB(2,2)*AWB(1,3))*phis4
      rliki(2) = max(0.d0,rliki(2)-A*inv_delta)

      B=-det(2)*phi3+(AWB(1,1)*AWB(3,3)-AWB(3,1)*AWB(1,3))*phit4-(AWB(1,1)*AWB(2,3)-AWB(2,1)*AWB(1,3))*phis4
      rliki(1) = rliki(1) - B*inv_delta

      C=det(3)*phi3-(AWB(1,1)*AWB(3,2)-AWB(3,1)*AWB(1,2))*phit4+(AWB(1,1)*AWB(2,2)-AWB(2,1)*AWB(1,2))*phis4
      rliki(3) = rliki(3) - C*inv_delta

      Wrltik = WWik(1,1)*rliki(1) + WWik(1,2)*rliki(2) + WWik(1,3)*rliki(3)
      Wrlnik = WWik(2,1)*rliki(1) + WWik(2,2)*rliki(2) + WWik(2,3)*rliki(3)
      Wrlsik = WWik(3,1)*rliki(1) + WWik(3,2)*rliki(2) + WWik(3,3)*rliki(3)

      Err(1) = rliki(2) - rlniki
      Err(2) = rliki(1) - rltiki
      Err(3) = rliki(3) - rlsiki

      Erv(1) = Wrlnik - Wrlniki
      Erv(2) = Wrltik - Wrltiki
      Erv(3) = Wrlsik - Wrlsiki

      Wrefik = Wrlnik*rliki(2) + Wrltik*rliki(1) + Wrlsik*rliki(3)

      if( Wrefik <= 1.d-24 ) Wrefik=1.d0
      Erwik = (Err(1)*Erv(1)+Err(2)*Erv(2)+Err(3)*Erv(3))/(Wrefik*Ntol*Ntol)

      iF(Erwik < 1.d0) exit

    end do

  end subroutine mu_clb_solver_3d

  !> \brief Coulomb's friction diagonal solver for 3D
  subroutine mu_clb_diag_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik
    !
    real(kind=8) :: FFN, DET

    vlfik(1) = vlik(1) + inter%covfree(1) - inter%W(1,1)*rlik(1)
    vlfik(2) = vlik(2) + inter%covfree(2) - inter%W(2,2)*rlik(2)
    vlfik(3) = vlik(3) + inter%covfree(3) - inter%W(3,3)*rlik(3)

    if( vlfik(2) >= 0.d0 ) then
       statusik = 'noctc'
       rliki(:) = 0.d0
    else
       rliki(:) = - (inter%invW*vlfik)

       FFN = inter%fric*rliki(2)
       DET = sqrt(rliki(1)*rliki(1)+rliki(3)*rliki(3))

       if (DET < FFN) then
         statusik = 'stick'
       else
         statusik = 'slide'
         if (DET > 1d-18) then
           rliki(1) = rliki(1)*FFN/DET
           rliki(3) = rliki(3)*FFN/DET
         end if
       end if
    end if

  end subroutine mu_clb_diag_solver_3d

  !> \brief Coupled dof solver for 3D
  subroutine coupled_dof_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    vlfik(1) = vlik(1) + inter%covfree(1) - inter%W(1,1)*rlik(1)
    vlfik(2) = vlik(2) + inter%covfree(2) - inter%W(2,2)*rlik(2)
    vlfik(3) = vlik(3) + inter%covfree(3) - inter%W(3,3)*rlik(3)

    rliki(:) = inter%invW(:) * vlfik(:)

  end subroutine

  !> \brief Normal coupled dof solver for 3D
  subroutine normal_coupled_dof_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    rliki(2) = -vlfik(2) / WWik(2,2)

    vliki(1) = vlfik(1) + WWik(1,2)*rliki(2)
    vliki(2) = 0.d0
    vliki(3) = vlfik(3) + WWik(3,2)*rliki(2)

    statusik = 'stick'

  end subroutine

  !> \brief Tangential coupled dof solver for 3D
  subroutine tangential_coupled_dof_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,statusik)
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: statusik

    rliki(1) =-vlfik(1) / WWik(1,1)
    rliki(2) = 0.d0
    rliki(3) =-vlfik(3) / WWik(3,3)

    vliki(1) = 0.d0
    vliki(2) = vlfik(2) + WWik(2,1)*rliki(1) + WWik(2,3)*rliki(3)
    vliki(3) = 0.d0

    statusik = 'stick'

  end subroutine

  !> \brief CZM solver for 3D
  subroutine czm_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,stat)
    use tact_behaviour, only : iter_czm_3D, get_fric_czm
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: stat
    !
    real(kind=8) :: k_t, k_n, Hradh_t, Hradh_n, Hradh_s, fricik, inv_detA, det
    real(kind=8) :: Att, Ant, Ast, Atn, Ann, Asn, Ats, Ans, Ass
    real(kind=8) :: invAtt, invAnt, invAst, invAtn, invAnn, invAsn, invAts, invAns, invAss, ut, un, us
    logical      :: is_cohesive
    character(len=80) :: cout

    if (stat(1:1) == 'C') then 
       call iter_CZM_3D(inter%lawnb,inter%internal,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s)
    else
       is_cohesive = .false.
       Hradh_t = 0.d0
       Hradh_n = 0.d0
       Hradh_s = 0.d0
    end if

    call get_fric_CZM(inter%lawnb,inter%internal,fricik)

    if (is_cohesive) then

      Att = 1.d0 - (WWik(1,1)*k_t) ; Atn =      -  WWik(1,2)*k_n  ; Ats =      -  WWik(1,3)*k_t   
      Ant =      -  WWik(2,1)*k_t  ; Ann = 1.d0 - (WWik(2,2)*k_n) ; Ans =      -  WWik(2,3)*k_t
      Ast =      -  WWik(3,1)*k_t  ; Asn =      -  WWik(3,2)*k_n  ; Ass = 1.d0 - (WWik(3,3)*k_t) 

      inv_detA = 1.d0/((Att*Ann*Ass + Ant*Asn*Ats + Ast*Atn*Ans) - (Ast*Ann*Ats + Asn*Ans*Att + Ass*Ant*Atn))
      
      invAtt =  inv_detA*(Ann*Ass-Asn*Ans) ; invAtn = -inv_detA*(Atn*Ass-Asn*Ats) ; invAts =  inv_detA*(Atn*Ans-Ann*Ats)
      invAnt = -inv_detA*(Ant*Ass-Ast*Ans) ; invAnn =  inv_detA*(Att*Ass-Ast*Ats) ; invAns = -inv_detA*(Att*Ans-Ant*Ats)
      invAst =  inv_detA*(Ant*Asn-Ast*Ann) ; invAsn = -inv_detA*(Att*Asn-Ast*Atn) ; invAss =  inv_detA*(Att*Ann-Ant*Atn)
      
      ut = vlfik(1) + (WWik(1,1) * Hradh_t) + (WWik(1,3) * Hradh_s)
      un = vlfik(2) + (WWik(2,1) * Hradh_t) + (WWik(2,3) * Hradh_s)
      us = vlfik(3) + (WWik(3,1) * Hradh_t) + (WWik(3,3) * Hradh_s)
      
      vlfik(1) = (invAtt * ut) + (invAtn * un) + (invAts * us)
      vlfik(2) = (invAnt * ut) + (invAnn * un) + (invAns * us)
      vlfik(3) = (invAst * ut) + (invAsn * un) + (invAss * us)
      
      WWik(1,1) = (invAtt * WWik(1,1)) + (invAtn * WWik(2,1)) + (invAts*WWik(3,1))
      WWik(1,2) = (invAtt * WWik(1,2)) + (invAtn * WWik(2,2)) + (invAts*WWik(3,2))
      WWik(1,3) = (invAtt * WWik(1,3)) + (invAtn * WWik(2,3)) + (invAts*WWik(3,3))
      
      WWik(2,1) = (invAnt * WWik(1,1)) + (invAnn * WWik(2,1)) + (invAns*WWik(3,1))
      WWik(2,2) = (invAnt * WWik(1,2)) + (invAnn * WWik(2,2)) + (invAns*WWik(3,2))
      WWik(2,3) = (invAnt * WWik(1,3)) + (invAnn * WWik(2,3)) + (invAns*WWik(3,3))
      
      WWik(3,1) = (invAst * WWik(1,1)) + (invAsn * WWik(2,1)) + (invAss*WWik(3,1))
      WWik(3,2) = (invAst * WWik(1,2)) + (invAsn * WWik(2,2)) + (invAss*WWik(3,2))
      WWik(3,3) = (invAst * WWik(1,3)) + (invAsn * WWik(2,3)) + (invAss*WWik(3,3))
                  
    end if
    
    fricik = inter%fric
    det    = inter%det

    call mu_clb_solver_3d(inter, rlik, vlik, vlfik, Wrlik, WWik, rliki, vliki, stat)

    inter%fric = fricik
    inter%det  = det

    if (is_cohesive) then 

      ut = vlfik(1) + dot_product(WWik(1,:),rliki(:))
      un = vlfik(2) + dot_product(WWik(2,:),rliki(:)) - inter%covfree(2)
      us = vlfik(3) + dot_product(WWik(3,:),rliki(:))

      Hradh_t = Hradh_t + (k_t * ut)
      Hradh_n = Hradh_n + (k_n * un)
      Hradh_s = Hradh_s + (k_t * us)
      
      if ( Hradh_n > 1.d-03 ) then
        write(cout,'(A,D10.3,A,I0)') 'strange cohesive part ',Hradh_n,' for contact ',inter%icdan
        call logmes(cout)
      end if
      
      rliki(1) = rliki(1) + Hradh_t 
      rliki(2) = rliki(2) + Hradh_n 
      rliki(3) = rliki(3) + Hradh_t 
      
      if (stat == 'noctc') then
        stat='Cstck'
      else if (stat == 'stick') then
        stat='Cstck'
      else if (stat == 'slide') then
        stat='Cslid'
      end if
    end if

  end subroutine czm_solver_3d

  !> \brief CZM diagonal solver for 3D
  subroutine czm_diag_solver_3d(inter,rlik,vlik,vlfik,Wrlik,WWik,rliki,vliki,stat)
    use tact_behaviour, only : iter_czm_3D, get_fric_czm
    implicit none
    !> interaction
    type(T_interaction) :: inter
    !> local reaction at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: rlik
    !> local velocity at begining of iteration
    real(kind=8), dimension(inter_dim), intent(in) :: vlik
    !> local free velocity at iteration
    real(kind=8), dimension(inter_dim), intent(inout) :: vlfik
    !> Wxrlik 
    real(kind=8), dimension(inter_dim), intent(in) :: Wrlik
    !> delassus operator at iteration
    real(kind=8), dimension(inter_dim,inter_dim), intent(inout) ::WWik
    !> local reaction at iteration
    real(kind=8), dimension(inter_dim), intent(out) :: rliki
    !> local velocity computed value during iteraction
    real(kind=8), dimension(inter_dim), intent(inout) :: vliki
    !> contact status at iteration
    character(len=5), intent(inout) :: stat
    !
    real(kind=8) :: k_t, k_n, Hradh_t, Hradh_n, Hradh_s, fricik, FFN, DET
    real(kind=8) :: Att, Ann, Ass, invAtt, invAnn, invAss, ut, un, us
    logical      :: is_cohesive
    character(len=80) :: cout

    if (stat(1:1) == 'C') then 
       call iter_CZM_3D(inter%lawnb,inter%internal,k_t,k_n,is_cohesive,Hradh_t,Hradh_n,Hradh_s)
    else
       is_cohesive = .false.
       Hradh_t = 0.d0
       Hradh_n = 0.d0
       Hradh_s = 0.d0
    end if

    fricik = inter%fric

    call get_fric_CZM(inter%lawnb,inter%internal,fricik)

    if (is_cohesive) then

      WWik(1,1) = (inter%W(3,3)+inter%W(1,1))*0.5
      WWik(2,2) = inter%W(2,2)
      WWik(3,3) = (inter%W(3,3)+inter%W(1,1))*0.5 

      ut = vlik(1) + inter%covfree(1) - WWik(1,1)*rlik(1) + WWik(1,1) * Hradh_t
      un = vlik(2) + inter%covfree(2) - WWik(2,2)*rlik(2)
      us = vlik(3) + inter%covfree(3) - WWik(3,3)*rlik(3) + WWik(3,3) * Hradh_s
      
      Att = 1.d0 - (WWik(1,1)*k_t)
      Ann = 1.d0 - (WWik(2,2)*k_n)
      Ass = 1.d0 - (WWik(3,3)*k_t) 

      invAtt = 1.d0/Att
      invAnn = 1.d0/Ann
      invAss = 1.d0/Ass
      
      vlfik(1) = invAtt * ut
      vlfik(2) = invAnn * un
      vlfik(3) = invAss * us
      
      Att = invAtt * WWik(1,1)
      Ann = invAnn * WWik(2,2)
      Ass = invAss * WWik(3,3)

      WWik(1,1) = Att ; WWik(2,1) = 1.d0; WWik(3,1) = 1.d0
      WWik(1,2) = 1.d0; WWik(2,2) = Ann ; WWik(3,2) = 1.d0
      WWik(1,3) = 1.d0; WWik(2,3) = 1.d0; WWik(3,3) = Ass
      
      rliki(1) = -vlfik(1) / WWik(1,1)
      rliki(2) = -vlfik(2) / WWik(2,2)
      rliki(3) = -vlfik(3) / WWik(3,3)

      if( rliki(2) <= 0.d0) then
        stat     = 'noctc'
        rliki(:) = 0.d0
      else
        FFN = fricik*rliki(2)
        DET = sqrt(rliki(1)*rliki(1)+rliki(3)*rliki(3))
         
        if( DET < FFN) then
          stat = 'stick'
        else
          stat = 'slide'
          if( DET > 1d-18) then
            rliki(1) = rliki(1)*FFN/DET
            rliki(3) = rliki(3)*FFN/DET
          end if
        end if
      end if

    else  ! plus cohesif

      vlfik(1) = vlik(1) + inter%covfree(1) - inter%W(1,1) * rlik(1)
      vlfik(2) = vlik(2) + inter%covfree(2) - inter%W(2,2) * rlik(2)
      vlfik(3) = vlik(3) + inter%covfree(3) - inter%W(3,3) * rlik(3)

      if( vlfik(2) >= 0.d0 ) then
        stat     = 'noctc'
        rliki(:) = 0.d0
      else
        rliki(:) = -inter%invW(:)*vlfik(:)
                     
        FFN = fricik*rliki(2)
        DET = sqrt(rliki(1)*rliki(1)+rliki(3)*rliki(3))
           
        if( DET < FFN ) then
          stat = 'stick'
        else
          stat = 'slide'
          if( DET > 1d-18) then
            rliki(1) = rliki(1)*FFN/DET
            rliki(3) = rliki(3)*FFN/DET
          end if
        end if
      end if
    end if
    
    if (is_cohesive) then 

      ut = vlfik(1) + WWik(1,1)*rliki(1)
      un = vlfik(2) + WWik(2,2)*rliki(2) - inter%covfree(2)
      us = vlfik(3) + WWik(3,3)*rliki(3)

      Hradh_t = Hradh_t + (k_t * ut)
      Hradh_n = Hradh_n + (k_n * un)
      Hradh_s = Hradh_s + (k_t * us)
      
      if ( Hradh_n > 1.d-03 ) then
        write(cout,'(A,D10.3,A,I0)') 'strange cohesive part ',Hradh_n,' for contact ',inter%icdan
        call logmes(cout)
      end if
      
      rliki(1) = rliki(1) + Hradh_t 
      rliki(2) = rliki(2) + Hradh_n 
      rliki(3) = rliki(3) + Hradh_t 
      
      if (stat == 'noctc') then
        stat='Cstck'
      else if (stat == 'stick') then
        stat='Cstck'
      else if (stat == 'slide') then
        stat='Cslid'
      end if
    end if

  end subroutine czm_diag_solver_3d

end module interaction_laws_3D

