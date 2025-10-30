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

module ExternalModelsHandler


  !fd 26-07-2021
  ! ajout d'un module chapeau pour appeler des modeles de comportement externes
  ! Ici Matlib, Demmefi et bientôt umat

  use overall, only : nbDIME
  
  use utilities, only : faterr,logmes

  use models, only : modelz               , &
                     get_nb_models        , &
                     get_nb_ppsets        , &
                     get_ppset_value      , &
                     get_eleop_id         , &
                     get_eleop_value      , &
                     get_eleop_value_bypps, &
                     get_nb_external_variables



  use bulk_behaviour, only : get_nb_bulk_behav , &
                             get_bulk_behav_ID , &
                             get_is_a_user_mat , &
                             get_user_mat_filename, &
                             get_rho

  
  use MatlibExternalModels, only : push_model_MatLib => push_model, &
                                   push_behaviour_MatLib => push_behaviour, &
                                   check_ppset_MatLib => check_ppset, & 
                                   compute_external_gp_MatLib => compute_external_gp, &
                                   set_ortho_frame_MatLib => set_ortho_frame                                  
  
  use DemmefiExternalModels, only : push_model_Demmefi => push_model, &
                                    push_behaviour_Demmefi => push_behaviour, &
                                    check_ppset_Demmefi => check_ppset, &
                                    set_nb_ppsets_demmefi => set_nb_ppsets, &
                                    clean_memory_demmefi => clean_memory, &
                                    compute_external_gp_Demmefi => compute_external_gp
  
  
  implicit none
  private

  integer, allocatable :: extm(:)

  ! map between lmgc90 ppset and external ppset 

  integer,dimension(:),allocatable :: m_external_ppset
  integer,dimension(:),allocatable :: d_external_ppset
  integer,dimension(:),allocatable :: u_external_ppset  

  ! bavardage

  logical :: itchatche = .FALSE.

  ! wrap
  public init_external_models,store_external_ppset,check_external_ppset,compute_external_pg,set_ortho_frame,clean_memory

contains


!------------------------------------------------------------------------
  subroutine init_External_Models
    implicit none
    integer          :: ibehav,imodel,ippset,id
    character(len=5) :: isext
                            !123456789012345678901234567890123456 
    character(len=36):: IAM='external_models::init_external_model'

    if (itchatche) call logmes('Entering : '//IAM)

    if (get_nb_models()> 0) then
      allocate(extm(get_nb_models()))
      extm=0
    endif   

    !fd on pousse les modeles dans la librairie externe
     
    do imodel=1,get_nb_models()
      id = get_eleop_id(imodel,'isext') 
      if ( id /= 0) then
        isext = get_eleop_value(imodel,id) 
        select case (isext)
        case('MatL_')   
          call push_model_MatLib(imodel,itchatche)
          extm(imodel)=1 
        case('Demfi')
          call push_model_Demmefi(imodel,itchatche)          
          extm(imodel)=2  
        case('Umat_')
          call faterr(IAM,'not available')
          extm(imodel)=3
        end select
      endif  
    enddo

    if (itchatche) call logmes('Leaving : '//IAM)

  end subroutine
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine store_external_ppset
    implicit none
    
    integer          :: ibehav,imodel,ippset,nb_ppsets,m_iext_ppset,d_iext_ppset,u_iext_ppset
    character(len=5) :: isext

                            !1234567890123456789012345678901234567 
    character(len=37):: IAM='external_models::store_external_ppset'
    character(len=80):: cout 
    
    if (itchatche) call logmes('Entering : '//IAM)
       
    nb_ppsets = get_nb_ppsets()
    if (nb_ppsets == 0) return

    if( allocated(m_external_ppset) ) deallocate(m_external_ppset)
    allocate(M_external_ppset(nb_ppsets))
    M_external_ppset = 0
    
    if( allocated(D_external_ppset) ) deallocate(D_external_ppset)
    allocate(D_external_ppset(nb_ppsets))
    D_external_ppset = 0
    
    if( allocated(U_external_ppset) ) deallocate(U_external_ppset)
    allocate(U_external_ppset(nb_ppsets))
    U_external_ppset = 0

    !fd on compte
    m_iext_ppset = 0; d_iext_ppset = 0; u_iext_ppset = 0
    do ippset=1,get_nb_ppsets()
      isext = get_eleop_value_bypps(ippset,'isext')
      select case(isext)
      case('MatL_')   
        m_iext_ppset = m_iext_ppset + 1
        ! le signe negatif c'est pour savoir ceux qui n'ont pas été initialisés  
        m_external_ppset(ippset)=-m_iext_ppset
      case('Demfi')   
        d_iext_ppset = d_iext_ppset + 1
        ! le signe negatif c'est pour savoir ceux qui n'ont pas été initialisés  
        d_external_ppset(ippset)=-d_iext_ppset        
      case('Umat_')   
        u_iext_ppset = u_iext_ppset + 1
        ! le signe negatif c'est pour savoir ceux qui n'ont pas été initialisés  
        u_external_ppset(ippset)=-u_iext_ppset
      end select
    end do 

    ! on pousse le nb de ppset
    if (d_iext_ppset /= 0) then
     
       if (itchatche) then
        write(cout,'(A,1x,I0)') 'number of demmefi models : ',d_iext_ppset  
        call logmes(cout)
      endif   
       
      call set_nb_ppsets_demmefi(d_iext_ppset,itchatche)
    endif
    
    if (itchatche) call logmes('Leaving : '//IAM)

  end subroutine store_external_ppset
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine check_external_ppset(ippset,sf_density,vf_elas_coeff)
    implicit none
    integer               :: ippset
    real(kind=8),optional :: sf_density
    real(kind=8),optional :: vf_elas_coeff(2)

    integer               :: imodel,ibehav,iext_ppset
    real(kind=8)          :: rho
    character(len=80)     :: filename
    
                                 !1234567890123456789012345678901234567 
    character(len=37)     :: IAM='external_models::check_external_ppset'

    if (itchatche) call logmes('Entering : '//IAM)

    !  pour recuperer le num de behav
    call get_ppset_value(ippset,imodel,ibehav)       

    select case(extm(imodel))
    case(1) !MatLib
      ! on pousse les param behav de lmgc90 dans MatLib
    
      if (m_external_ppset(ippset) < 0) then

        ! ok c'est bon 
        m_external_ppset(ippset) = -m_external_ppset(ippset)

        iext_ppset = m_external_ppset(ippset)
      
        if (present(sf_density) .and. present(vf_elas_coeff)) then
           call push_behaviour_MatLib(iext_ppset,ibehav,itchatche,density=sf_density,elas_coeff=vf_elas_coeff)
        else if (present(sf_density) .and. .not. present(vf_elas_coeff)) then
           call push_behaviour_MatLib(iext_ppset,ibehav,itchatche,density=sf_density)
        else if (.not. present(sf_density) .and. present(vf_elas_coeff)) then
           call push_behaviour_MatLib(iext_ppset,ibehav,itchatche,elas_coeff=vf_elas_coeff)
        else 
           call push_behaviour_MatLib(iext_ppset,ibehav,itchatche)
        endif

        call check_ppset_MatLib(iext_ppset,imodel,itchatche)

      else

        if (itchatche) call logmes(IAM//':: this ppset was already checked') 
        
      endif

    case(2) ! Demmefi
      if (d_external_ppset(ippset) < 0) then
         
        ! ok c'est bon 
        d_external_ppset(ippset) = -d_external_ppset(ippset)

        iext_ppset = d_external_ppset(ippset)
        
        ! pour recuperer le num de behav
        call get_ppset_value(ippset,imodel,ibehav)       

        if (get_is_a_user_mat(ibehav)) then

           rho=get_rho(ibehav)
           filename=get_user_mat_filename(ibehav)           
           call push_behaviour_Demmefi(iext_ppset,itchatche,rho,filename)
        else

          call faterr(IAM,'Material for demmefi must be user mat')

        endif   
        
        call check_ppset_Demmefi(iext_ppset,imodel,itchatche)

      endif
     
    case(3)
      call faterr(IAM,'not yet implemented')
       
    case default
      call faterr(IAM,'not managed')
    end select   
   
    if (itchatche) call logmes('Leaving : '//IAM)

  end subroutine check_external_ppset
!------------------------------------------------------------------------


!------------------------------------------------------------------------
  SUBROUTINE compute_external_pg(ippset,extP_lbl,extP_len,ivalue, &
                                extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,H,calcD,xe3d)

    ! zone de stockage: gradient,flux,internal,operateur tangent
  
    real(kind=8),dimension(:)             :: GRAD0,FLUX0,INTERNAL0
    real(kind=8),dimension(:)             :: GRAD1,FLUX1,INTERNAL1
    real(kind=8),dimension(:,:),pointer   :: D
    real(kind=8)                          :: H
    real(kind=8),dimension(:,:),optional  :: xe3d
    
    ! parametres externes
  
    character(len=30),dimension(:) :: extP_lbl
    integer(kind=4)  ,dimension(:) :: extP_len
    real(kind=8)     ,dimension(:) :: extP_val
    integer(kind=4)                :: extP_nb, calcD
  
    !fd 
    integer                        :: ippset,imodel,ibehav,iext_ppset,ivalue  

                                 !123456789012345678901234567890123456 
    character(len=36)     :: IAM='external_models::compute_external_pg'
    
    !  pour recuperer le num de behav
    call get_ppset_value(ippset,imodel,ibehav)       

    select case(extm(imodel))
    case(1)
       
      iext_ppset = m_external_ppset(ippset)
      call compute_external_gp_matlib(iext_ppset,imodel,extP_lbl,extP_len,ivalue, &
                                extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,H,calcD)
       
   case(2)

      if (.not. present(xe3d)) call faterr(IAM,'xe3d is mandatory')
      
      iext_ppset = d_external_ppset(ippset)      
      call compute_external_gp_demmefi(iext_ppset,imodel,extP_lbl,extP_len,ivalue, &
                                extP_val,extP_nb, &
                                GRAD0,FLUX0,INTERNAL0, &
                                GRAD1,FLUX1,INTERNAL1, &
                                D,H,calcD,xe3d)

    case(3)
      call faterr(IAM,'not yet implemented') 

    case default
      call faterr(IAM,'not implemented')
       
    end select   
    
  end subroutine compute_external_pg
!------------------------------------------------------------------------  

!------------------------------------------------------------------------    
  subroutine set_ortho_frame(ippset,frame)
    implicit none

    integer     ,intent(in) :: ippset
    real(kind=8),intent(in) :: frame(3,3)


    integer                 :: iext_ppset,imodel,ibehav
    
                                   !12345678901234567890123456789012
    character(len=32)       :: IAM='external_models::set_orthe_frame'

    
    !  pour recuperer le num de behav
    call get_ppset_value(ippset,imodel,ibehav)       

    select case(extm(imodel))
    case(1)
      iext_ppset = m_external_ppset(ippset)
      call set_ortho_frame_matlib(iext_ppset,imodel,frame)
      
    case default
      call faterr(IAM,'not managed yet')
    end select
   
  end subroutine 
!------------------------------------------------------------------------  
  
!------------------------------------------------------------------------  
 subroutine clean_memory()
   implicit none

   if ( allocated(extm)             ) deallocate(extm)
   if ( allocated(m_external_ppset) ) deallocate(m_external_ppset)
   if ( allocated(d_external_ppset) ) deallocate(d_external_ppset)
   if ( allocated(u_external_ppset) ) deallocate(u_external_ppset)

   call clean_memory_demmefi()
   
 end subroutine clean_memory
!------------------------------------------------------------------------
  
end module ExternalModelsHandler

