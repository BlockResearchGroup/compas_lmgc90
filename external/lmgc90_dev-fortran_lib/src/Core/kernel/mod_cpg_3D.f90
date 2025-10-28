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
module CPG_3D
  
  !!****h* LMGC90.CORE/CPG_3D
  !! NAME
  !!  module CPG_3D
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/PRPRx
  !!  LMGC90.CORE/SPSPx
  !!  LMGC90.CORE/SPPLx
  !!  LMGC90.CORE/SPCDx
  !!****
  
  use overall

  use parameters

  use tact_behaviour
  use utilities

  use ALGEBRA

  use inter_meca_handler_3D, only : get_nb_inters    , &
                                    set_loc          , &
                                    get_rloc         , &
                                    get_vlocBEGIN    , &
                                    inter2ENT       , &
                                    get_tact_lawnb   , &
                                    injj             , &
                                    prjj             , &
                                    vitrad           , &
                                    nullify_reac     , &
                                    nullify_vlocy

  implicit none
  
  private


  ! -----------------------------------------------------------------------
  ! TYPE T_ctct_element
  !
  ! CDAN                        : type of contact element ik
  ! icdan                       : serial type number in type CDAN of contact element ik
  ! rlt ,rln ,rls               : components of local contact impulse
  ! rlti,rlni,rlsi              : components of local contact impulse during iteration
  ! status                      : contact status
  ! statuscheck                 : contact status check           
  ! Wss,Wst,Wsn,
  ! Wts,Wtt,Wtn,
  ! Wns,Wnt,Wnn                 : components of genuine local dynamic matrix W
  ! vfreet  ,vfreen  ,vfrees    : genuine free relative velocity components
  ! covfreet,covfreen,covfrees  : complementary free reative velocity components to add to
  !                               genuine free relative velocity components to get auxiliaries
  ! vltBEGIN,vlnBEGIN,vlsBEGIN  : components of the relative velocity at the beginning of the time step   
  ! gapTTbegin                  : gap at the beginning of the time step 
  ! statusBEGIN                 : status at the beginning of the time step
  ! forecast                    : set to 'acton' (reactions have to be computed) or 0 (reactions 
  !                               are to be found null) according to some forecast criterion
  ! lawnb                       : law number of contact ik
  ! fric                        : friction coefficient for contact ik
  ! WPs,WPt,WPn,WRs,WRt,WRn     : product of W(i,:) and P or R 
  ! zs,zt,zn                    :
  ! ps,pt,pn                    : residue projection
  ! ws,wt,wn                    :
  ! ress,rest,resn              : residue
  ! nbadj                       :
  ! icdent,ianent               :
  ! istart                      :
  ! adjjl                       :
  ! iws                         :
  ! ic                          :
  ! -----------------------------------------------------------------------
  type T_ctct_element
     
     integer                      :: CDAN
     integer                      :: icdan
     real(kind=8)                 ::   rlt,   rln ,   rls
     real(kind=8)                 ::   rlti,  rlni,   rlsi
     integer(kind=4)              :: status
     integer(kind=4)              :: statuscheck
     
     real(kind=8)                 ::  Wtt, Wtn, Wnt, Wnn
     real(kind=8)                 ::  Wss, Wst, Wts, Wns ,Wsn
     
     real(kind=8)                 ::   vfreet,   vfreen,   vfrees
     real(kind=8)                 :: covfreet, covfreen, covfrees
     real(kind=8)                 :: vltBEGIN, vlnBEGIN, vlsBEGIN

     real(kind=8)                 :: gapTTbegin 
     integer(kind=4)              :: statusBEGIN
     integer(kind=4)              :: forecast
     integer                      :: lawnb
     real(kind=8)                 :: fric
     
     real(kind=8)                 :: WPs,WPt,WPn,WRs,WRt,WRn
     real(kind=8)                 :: zs,zt,zn
     real(kind=8)                 :: ps,pt,pn
     real(kind=8)                 :: ws,wt,wn
     real(kind=8)                 :: ress,rest,resn
     integer                      :: nbadj,icdent,ianent,istart
     integer,dimension(:),pointer :: adjjl
     integer                      :: iws
     logical                      :: ic

  end type T_ctct_element

  type (T_ctct_element),dimension(:),allocatable  ::  this  
  ! -----------------------------------------------------------------------------
  ! Wab extra delassus vector
  !
  ! Wab is the vector which contains all components of each matrix Wik,jl. The 
  ! this(ik)%istart contains the last range before ik where a values have been written.
  !  W(this(ik)%istart+9*iadj-8) = WSS ;  W(this(ik)%istart+9*iadj-3) = WTN 
  !  W(this(ik)%istart+9*iadj-7) = WST ;  W(this(ik)%istart+9*iadj-2) = WNS 
  !  W(this(ik)%istart+9*iadj-6) = WSN ;  W(this(ik)%istart+9*iadj-1) = WNT 
  !  W(this(ik)%istart+9*iadj-5) = WTS ;  W(this(ik)%istart+9*iadj  ) = WNN 
  !  W(this(ik)%istart+9*iadj-4) = WTT
  ! -----------------------------------------------------------------------------

  real(kind=8),dimension(:),allocatable :: Wab

  integer, dimension(:), allocatable,private :: ialeat,ialeatr,iwksg
!
  integer          :: nb_CDAN,cpg_loop,nb_ENTITY,projID=1
  integer          :: Nactif,NOCTC,NOKsta,Nnoctc,Nslide,Nstick

  real(kind=8)     :: tol=0.1666D-03,inv_tol,Enrg=0.0,Scale=1.D0
  real(kind=8)     :: meanDVoR,QuadDVR,QuadDV
  real(kind=8)     :: sumWRR,sumDVoR,sumWRWR,sumDVDV,sumDVDVRR

  real(kind=8)     :: ALPHA,BETA,RWR,PWP,WiiMax,WiiMin

  logical          :: bavard        =.false.
  logical          :: PWPcheck      =.false., &
                      preconditioner=.false., &
                      norm_check    =.false., &
                      frictionless  =.false., &
                      conjugaison   =.true. , &
                      idproj        =.false.

  integer          :: i_checktype = 1,norm_fich
  character(len=5) :: checktype='Quad ' ! default value

  integer,parameter :: i_Quad = 1 , i_Maxm = 2 , i_Stag = 3 , i_QuadN = 4

  public &
     set_cpg_parameter, &
     ex_iter_cpg, & 
     ex_check_cpg, &
     write_norm_check_cpg, &
     ex_post_cpg, &
     ex_prep_cpg, &
     set_diag_precond_cpg, &
     set_frictionless, &
     scale_rloc_cpg, &
     Nullify_EntityList_cpg, &
     set_bimodal_list

  public &
       get_cpg_loop,get_cpg_contact_status,get_cpg_network_change

contains

!!!------------------------------------------------------------------------ 
  subroutine set_bimodal_list

    implicit none
    integer          :: ik
    
    if (nb_CDAN == 0) return
    do ik=1,nb_CDAN
       ialeat(ik)=iwksg(ik)
    end do

  end subroutine set_bimodal_list
!!!------------------------------------------------------------------------ 
  subroutine set_cpg_parameter(normtype,tolerance,id)
    
    implicit none

    integer          :: id
    real(kind=8)     :: tolerance
    character(len=5) :: normtype

    tol = tolerance
    inv_tol = 1./tol
    projID = id
    select case(normtype)
    case('QuaN ')
       i_checktype = i_QuadN
    case('Quad ')
       i_checktype = i_Quad
    case('Maxm ')
       i_checktype = i_Maxm
    case('Stag ')
       i_checktype = i_Stag
    end select

  end subroutine set_cpg_parameter
!!!-----------------------------------------------------------
  subroutine ex_check_cpg(iconv)

    implicit none

    integer :: iconv

    iconv = 1

    call check_cpg

    select case(i_checktype)
    case(i_Quad,i_QuadN)
       meanDVoR = meanDVoR*inv_tol
       QuadDVR  =  QuadDVR*inv_tol
       QuadDV   =   QuadDV*inv_tol
       
       if ( PWPcheck .or. &
            ( ( QuadDV .lt. 1.D0 ) .and. &
             ( QuadDVR .lt. 1.D0 ) .and. &
             ( abs(meanDVoR) .lt. 1.D0 ) ) ) iconv = 0

    case(i_Stag)
       QuadDV   =   QuadDV*inv_tol
       
       if ( PWPcheck .or. ( QuadDV .lt. 1.D0 ) ) iconv = 0
       
    case(i_Maxm)
       
    end select

    Scale    = 1.D0-meanDVoR

  end subroutine ex_check_cpg
!!!-----------------------------------------------------------------
  subroutine ex_prep_cpg

    implicit none

    call prep_cpg

    Scale  = 0.D0

    cpg_solver=.true.
    cpg_loop  = 0

  end subroutine ex_prep_cpg
!!!-----------------------------------------------------------------
  subroutine set_diag_precond_cpg

    implicit none

    preconditioner = .true.
    
  end subroutine set_diag_precond_cpg
!!!--------------------------------------------------------------------
  subroutine set_frictionless
    
    implicit none
    
    frictionless = .true.
    
  end subroutine set_frictionless
!!!---------------------------------------------------------------------
  subroutine write_norm_check_cpg(istat)
    
    implicit none

    integer :: istat

    select case(istat)
    case(1)
       norm_check = .true.
       norm_fich  = get_io_unit() 
       open(UNIT=norm_fich,FILE='NORM_CPG.DAT',STATUS='REPLACE')
    case(2)
       if (norm_check) then
          write(norm_fich,'(3(2X,D14.7),3(1X,I7))') abs(meanDVoR),QuadDVR,QuadDV
       end if
    case(3)
       if (norm_check) close(norm_fich)
    end select

  end subroutine write_norm_check_cpg
!!!---------------------------------------------------------------------
  subroutine scale_rloc_cpg

    implicit none

    integer :: ik

    if (nb_CDAN == 0) return

    Scale = DMIN1(Scale,1.1D0)
    Scale = DMAX1(Scale,0.9D0)      

    do ik=1,nb_CDAN
       this(ik)%rls = this(ik)%rls*Scale
       this(ik)%rlt = this(ik)%rlt*Scale
       this(ik)%rln = this(ik)%rln*Scale
    end do
    
    ! Rnod = [H] Rloc
    do ik=1,nb_CDAN 
       call nullify_reac( this(ik)%CDAN, ik,iIreac)
    end do
    do ik=1,nb_CDAN
       call injj( this(ik)%CDAN, ik,this(ik)%rlt,this(ik)%rln,this(ik)%rls,iIreac)
    end do

  end subroutine scale_rloc_cpg
!!!------------------------------------------------------------------------
  subroutine prep_cpg_aux_(id_inter, nb_inter, reac_mean)
    implicit none
    integer     , intent(in)    :: id_inter, nb_inter
    real(kind=8), intent(inout) :: reac_mean
    !
    integer :: icdan, ik, icdent, ianent

    do icdan = 1, nb_inter

       ik = nb_CDAN + icdan

       this(ik)%CDAN  = id_inter
       this(ik)%icdan = icdan    
       call get_rloc(id_inter, icdan, this(ik)%rlt, this(ik)%rln, this(ik)%rls, this(ik)%status)
       call get_vlocBEGIN(id_inter, icdan, this(ik)%vltBEGIN, this(ik)%vlnBEGIN, this(ik)%vlsBEGIN, this(ik)%gapTTbegin, this(ik)%statusBEGIN)
       call inter2ENT(id_inter, icdan, icdent, ianent)
       this(ik)%lawnb=get_tact_lawnb(id_inter, icdan)
       !mr
       reac_mean = reac_mean + this(ik)%rln
       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    end do

    nb_CDAN = nb_CDAN + nb_inter

  end subroutine prep_cpg_aux_

  subroutine prep_cpg
 
    implicit none

    character(len=14)  :: IAM='nscd::prep_cpg'
    character(len=120) :: cout
    integer            :: errare

    integer            :: ik,ibehav,icdan,ient
    real(kind=8)       :: vsik,vtik,vnik,rsik,rtik,rnik,det,det1,det2,det3,rsjl,rtjl,rnjl
    real(kind=8)       :: WRt,WRs,WRn

    integer            :: nb_SPSPx,nb_SPPLx,nb_PRPLx,nb_PRPRx,nb_SPCDx
    integer            :: nb_SPDCx,nb_CSPRx,nb_CSASx,nb_CDCDx,nb_CDPLx

    real(kind=8)       :: fric,tangalrest,normalrest

    integer            :: jl,iadj,jadj,nbadj,icdik,ianik,bandwith,icdent,ianent,istart,ikjl,iistart
    integer            :: jlstart,jladj,ikadj
    logical            :: is_present=.false.,ok=.false.

    real(kind=8)       :: reac_mean = 0.D0
    integer            :: nb_WEAK,nb_STRONG
 
    PWPcheck       =.false.
    nb_CDAN=0

    nb_SPSPx = get_nb_inters( i_spspx )
    nb_CDAN = nb_CDAN + nb_SPSPx
    
    nb_SPPLx = get_nb_inters( i_spplx )
    nb_CDAN = nb_CDAN + nb_SPPLx

    nb_PRPLx = get_nb_inters( i_prplx )
    nb_CDAN = nb_CDAN + nb_PRPLx
    
    nb_PRPRx = get_nb_inters( i_prprx )
    nb_CDAN = nb_CDAN + nb_PRPRx

    nb_SPCDx = get_nb_inters( i_spcdx )
    nb_CDAN = nb_CDAN + nb_SPCDx

    nb_SPDCx = get_nb_inters( i_spcdx )
    nb_CDAN = nb_CDAN + nb_SPDCx

    nb_CSPRx = get_nb_inters( i_csprx )
    nb_CDAN = nb_CDAN + nb_CSPRx

    nb_CSASx = get_nb_inters( i_csasp )
    nb_CDAN = nb_CDAN + nb_CSASx

    nb_CDCDx = get_nb_inters( i_cdcdx )
    nb_CDAN = nb_CDAN + nb_CDCDx

    nb_CDPLx = get_nb_inters( i_cdplx )
    nb_CDAN = nb_CDAN + nb_CDPLx

    if (nb_CDAN == 0) return 

    if (allocated(this)) deallocate(this)
    allocate(this(nb_CDAN),stat=errare)
    if (errare /= 0) then
       call FATERR(IAM,'error allocating this')
    end if

    if (allocated(ialeat)) deallocate(ialeat)
    allocate(ialeat(nb_CDAN),stat=errare)
    if (errare /= 0) then
       call FATERR(IAM,'error allocating ialeat')
    end if

    if (allocated(ialeatr)) deallocate(ialeatr)
    allocate(ialeatr(nb_CDAN),stat=errare)
    if (errare /= 0) then
       call FATERR(IAM,'error allocating ialeatr')
    end if

    if (allocated(iwksg)) deallocate(iwksg)
    allocate(iwksg(nb_CDAN),stat=errare)
    if (errare /= 0) then
       call FATERR(IAM,'error allocating ialeat')
    end if

    do ik=1,nb_CDAN
       ialeat(ik)=ik
       ialeatr(ik)=ik
    end do

    ! -------------------------------------------------------------------------------

    nb_ENTITY = get_nb_ENTITY()

    do ient=1,nb_ENTITY
       if(associated(entity(ient)%list)) deallocate(entity(ient)%list)
       allocate(entity(ient)%list(entity(ient)%nb),stat=errare)
       if (errare /= 0) then
          call FATERR(IAM,'error allocating entity(ient)%list')
       end if
       entity(ient)%list = 0
    end do

    ! -------------------------------------------------------------------------------
    
    do ik=1,nb_CDAN
       this(ik)%rls     = 0.D0
       this(ik)%rlt     = 0.D0
       this(ik)%rln     = 0.D0
       this(ik)%status  = i_nknow
       this(ik)%vlsBEGIN= 0.D0
       this(ik)%vltBEGIN= 0.D0
       this(ik)%vlnBEGIN= 0.D0
       this(ik)%gapTTBEGIN= 0.D0
       this(ik)%statusBEGIN= i_nknow
       this(ik)%Wss        = 0.D0
       this(ik)%Wtt        = 0.D0
       this(ik)%Wtn        = 0.D0
       this(ik)%Wnt        = 0.D0
       this(ik)%Wnn        = 0.D0
       this(ik)%Wst        = 0.D0
       this(ik)%Wts        = 0.D0
       this(ik)%Wsn        = 0.D0
       this(ik)%Wns        = 0.D0
       this(ik)%vfrees     = 0.D0
       this(ik)%vfreet     = 0.D0
       this(ik)%vfreen     = 0.D0
       this(ik)%covfrees   = 0.D0
       this(ik)%covfreet   = 0.D0
       this(ik)%covfreen   = 0.D0     
       this(ik)%lawnb      = 0
       this(ik)%statuscheck= i_nknow
       this(ik)%forecast   = 1 !'acton'       ! default status
       this(ik)%fric       = 0.D0
       this(ik)%icdent     = 0
       this(ik)%ianent     = 0
       this(ik)%WPs        = 0.D0
       this(ik)%WPt        = 0.D0
       this(ik)%WPn        = 0.D0
       this(ik)%WRs        = 0.D0
       this(ik)%WRt        = 0.D0
       this(ik)%WRn        = 0.D0
       this(ik)%zs         = 0.D0
       this(ik)%zt         = 0.D0
       this(ik)%zn         = 0.D0
       this(ik)%pt         = 0.D0
       this(ik)%pn         = 0.D0
       this(ik)%ws         = 0.D0
       this(ik)%wt         = 0.D0
       this(ik)%wn         = 0.D0
       this(ik)%ress       = 0.D0
       this(ik)%rest       = 0.D0
       this(ik)%resn       = 0.D0
       this(ik)%istart     = 0
       this(ik)%nbadj      = 0
       this(ik)%iws        = 0
       this(ik)%ic         = .false.
       nullify(this(ik)%adjjl)
    end do

    PWP=0.D0
    
    nb_CDAN = 0
    
    call prep_cpg_aux_(i_spspx, nb_SPSPx, reac_mean)
    call prep_cpg_aux_(i_spplx, nb_SPPLx, reac_mean)
    call prep_cpg_aux_(i_prplx, nb_PRPLx, reac_mean)
    call prep_cpg_aux_(i_prprx, nb_PRPRx, reac_mean)
    call prep_cpg_aux_(i_spcdx, nb_SPCDx, reac_mean)
    call prep_cpg_aux_(i_spdcx, nb_SPDCx, reac_mean)
    call prep_cpg_aux_(i_csprx, nb_CSPRx, reac_mean)
    call prep_cpg_aux_(i_csasp, nb_CSASx, reac_mean)
    call prep_cpg_aux_(i_cdcdx, nb_CDCDx, reac_mean)
    call prep_cpg_aux_(i_cdplx, nb_CDPLx, reac_mean)

    if(nb_CDAN /= 0) reac_mean = reac_mean/real(nb_CDAN,8)
    
    !------------------
    ! Rnod = [H] Rloc
    !------------------
    
    do ik=1,nb_CDAN  
       call nullify_reac(this(ik)%CDAN,ik,iIreac)
    end do
    do ik=1,nb_CDAN
       call injj(this(ik)%CDAN,ik,this(ik)%rlt,this(ik)%rln,this(ik)%rls,iIreac)
    end do
    
    nb_STRONG = 0
    nb_WEAK   = 0

    do ik=1,nb_CDAN
       call injj(this(ik)%CDAN,ik,this(ik)%rlt,this(ik)%rln,this(ik)%rls,iIreac)
       if(this(ik)%rln < reac_mean)then
          nb_WEAK = nb_WEAK+1
          iwksg(nb_CDAN+1-nb_WEAK) = ik
       else
          this(ik)%iws = 1
          nb_STRONG = nb_STRONG+1
          iwksg(nb_STRONG) = ik
       end if
    end do

    bandwith = 0
    istart   = 0
    
    do ik=1,nb_CDAN

       nbadj = 0
       icdik = this(ik)%icdent
       ianik = this(ik)%ianent

       ! special case of self contact          
       if (icdik .eq. ianik) then
          nbadj = entity(icdik)%nb-1
       else
          nbadj = entity(icdik)%nb+entity(ianik)%nb-2
       end if

       jl = 0

       if(nbadj /= 0)then
          if(associated(this(ik)%adjjl)) deallocate(this(ik)%adjjl)
          allocate(this(ik)%adjjl(nbadj),stat=errare)
          if (errare /= 0) then
             call FATERR(IAM,'error allocating this(ik)%adjjl')
          end if
          this(ik)%adjjl = 0

          do iadj=1,entity(icdik)%nb
             if(entity(icdik)%list(iadj) .eq. ik) cycle
             jl = jl+1
             this(ik)%adjjl(jl) = entity(icdik)%list(iadj)
          end do

          do iadj=1,entity(ianik)%nb
             if(entity(ianik)%list(iadj) .eq. ik) cycle
             is_present = .false.
             do jadj=1,entity(icdik)%nb-1
                if (this(ik)%adjjl(jadj) .ne. entity(ianik)%list(iadj)) cycle
                is_present = .true.
                exit
             end do
             if (is_present) cycle
             jl = jl+1
             this(ik)%adjjl(jl) = entity(ianik)%list(iadj)
          end do
       end if

       this(ik)%istart = istart
       istart = 9*jl + istart
       
       this(ik)%nbadj = jl
       bandwith = bandwith+jl
       
    end do
    
    do ient=1,nb_ENTITY
       if (entity(ient)%ik .ne. entity(ient)%nb) then
          call LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
          write(cout,'(A7,I5,A4,I5,A4,I5)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
          call FATERR(IAM,cout)
       end if
    end do
    
    if(allocated(Wab)) deallocate(Wab)
    allocate(Wab(9*bandwith),stat=errare)
    Wab = 0.D0
    if (errare .ne. 0) then
       call FATERR(IAM,'error allocating Wab')
    end if
    
    istart = 0

    do ik=1,nb_CDAN

       !-----------------------------------------------
       ! computing local dynamic matrix W
       !-----------------------------------------------
       rtik=1.D0
       rsik=0.D0
       rnik=0.D0
       call nullify_reac(this(ik)%CDAN,ik,iIaux_)
       call injj(this(ik)%CDAN,ik,rtik,rnik,rsik,iIaux_)
       call vitrad(this(ik)%CDAN,ik,iVaux_e_invM_t_Iaux_)
       call prjj(this(ik)%CDAN,ik,vtik,vnik,vsik,iVaux_)
       this(ik)%Wtt=vtik
       this(ik)%Wst=vsik
       this(ik)%Wnt=vnik

       do iadj=1,this(ik)%nbadj

          jl=this(ik)%adjjl(iadj)
          jlstart = this(jl)%istart

          call prjj(this(ik)%CDAN,ik,vtik,vnik,vsik,iVaux_)

          ok = .false.
          do jladj=1,this(jl)%nbadj
             if (this(jl)%adjjl(jladj) .eq. ik) then
                Wab(jlstart + 9*jladj-7) = vsik ! Wst
                Wab(jlstart + 9*jladj-4) = vtik ! Wtt
                Wab(jlstart + 9*jladj-1) = vnik ! Wnt
                ok = .true.
             end if
          end do

          if (.not. ok) then
             call FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
          end if

       end do

       call nullify_vlocy(this(ik)%CDAN,ik,iVaux_)

       !----------------------------

       rtik=0.D0
       rsik=1.D0
       rnik=0.D0
       call nullify_reac(this(ik)%CDAN,ik,iIaux_)
       call injj(this(ik)%CDAN,ik,rtik,rnik,rsik,iIaux_)
       call vitrad(this(ik)%CDAN,ik,iVaux_e_invM_t_Iaux_)
       call prjj(this(ik)%CDAN,ik,vtik,vnik,vsik,iVaux_)
       this(ik)%Wts=vtik
       this(ik)%Wss=vsik
       this(ik)%Wns=vnik

       do ikadj=1,this(ik)%nbadj
             
          jl = this(ik)%adjjl(ikadj)
          jlstart = this(jl)%istart
             
          call prjj(this(ik)%CDAN,jl,vtik,vnik,vsik,iVaux_)
             
          ok = .false.
             
          do jladj=1,this(jl)%nbadj
                
             if (this(jl)%adjjl(jladj) == ik) then
                   
                Wab(jlstart + 9*jladj-8) = vsik ! Wss
                Wab(jlstart + 9*jladj-5) = vtik ! Wts
                Wab(jlstart + 9*jladj-2) = vnik ! Wns
                   
                ok = .true.
             end if
             
          end do
          
          if (.not. ok) then
             call FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
          end if
          
       end do
          
       call nullify_vlocy(this(ik)%CDAN,ik,iVaux_)

       !----------------------------

       rtik=0.D0
       rsik=0.D0
       rnik=1.D0
       call nullify_reac(this(ik)%CDAN,ik,iIaux_)
       call injj(this(ik)%CDAN,ik,rtik,rnik,rsik,iIaux_)
       call vitrad(this(ik)%CDAN,ik,iVaux_e_invM_t_Iaux_)
       call prjj(this(ik)%CDAN,ik,vtik,vnik,vsik,iVaux_)
       this(ik)%Wtn=vtik
       this(ik)%Wsn=vsik
       this(ik)%Wnn=vnik

       do ikadj=1,this(ik)%nbadj
             
          jl = this(ik)%adjjl(ikadj)
          jlstart = this(jl)%istart
          
          call prjj(this(ik)%CDAN,jl,vtik,vnik,vsik,iVaux_)
          
          ok = .false.
          
          do jladj=1,this(jl)%nbadj
             
             if (this(jl)%adjjl(jladj) == ik) then
                
                Wab(jlstart + 9*jladj-6) = vsik ! Wsn
                Wab(jlstart + 9*jladj-3) = vtik ! Wtn
                Wab(jlstart + 9*jladj  ) = vnik ! Wnn
                
                ok = .true.
             end if
             
          end do
          
          if (.not. ok) then
             call FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
          end if
          
       end do
       
       call nullify_vlocy(this(ik)%CDAN,ik,iVaux_)
       
       !-----------------------------------------------
       ! Computing free local vlocy
       !-----------------------------------------------
       
       call prjj(this(ik)%CDAN,ik,this(ik)%vfreet,this(ik)%vfreen,this(ik)%vfrees,iVfree)

       !-----------------------------------------------
       !  Warning and coping with critical cases.
       !- For Wnn -------------------------------------
       
       if (this(ik)%Wnn .le. 1.D-18) then
          write(cout,543) ik,this(ik)%Wnn
543       format(1X,'  Wnn(',I5,') =',D12.5,' < 1.D-18')
          print*,this(ik)%CDAN
          call LOGMES('Error '//IAM//': '//cout)
          !stop
       end if
       
       !- For Wtt ------------------------------------
       
       if (this(ik)%Wtt .le. 1.D-06*this(ik)%Wnn) then
          write(cout,544)ik,this(ik)%Wtt,ik
544       format(1X,'   Wtt(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
          call LOGMES(cout)
          print*,this(ik)%CDAN
          this(ik)%Wtt=1.D-06*this(ik)%Wnn
       end if
       
       !- For Wss ------------------------------------
       
       if (this(ik)%Wss .le. 1.D-06*this(ik)%Wnn) then
          write(cout,545)ik,this(ik)%Wss,ik
545       format(1X,'   Wss(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
          call LOGMES(cout)
          print*,this(ik)%CDAN
          this(ik)%Wss=1.D-06*this(ik)%Wnn
       end if
       
       !-----------------------------------------------
       ! Preparing auxiliaries for contact ik.
       !-----------------------------------------------
       
       ibehav=this(ik)%lawnb

       fric =  get_fric(ibehav,this(ik)%statusBEGIN)

       this(ik)%fric     = fric
       this(ik)%covfrees = 0.D0
       this(ik)%covfreet = 0.D0
       this(ik)%covfreen = 0.D0     

       select case(tact_behav(ibehav)%lawty)
          !123456789012345678901234567890
       case('IQS_CLB                       ')
          this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)
       case('GAP_SGR_CLB                   ')
          this(ik)%covfreen=this(ik)%gapTTbegin/H
       case('VEL_SGR_CLB                   ')
          if (this(ik)%gapTTbegin+0.5D0*H*this(ik)%vlnBEGIN .le. 0.D0) then 
             ! a contact is forecasted if penetration 
             ! at half step time is found negative
             ! this(ik)%forecast='acton' (default is 'acton')
          else
             this(ik)%forecast= 0!0
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status= i_noctc
          end if
       case('IQS_DS_CLB                    ')
          this(ik)%covfreen=max(0.D0,this(ik)%gapTTbegin/H)         
       case('GAP_SGR_DS_CLB                ')
          this(ik)%covfreen=this(ik)%gapTTbegin/H
       case('RST_CLB                       ')
          if (this(ik)%gapTTbegin+0.5D0*H*this(ik)%vlnBEGIN .le. 0.D0) then
             ! a contact is forecasted if penetration
             ! at half step time is found negative 
             ! this(ik)%forecast='acton' !(default is 'acton')
             call get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfrees=tangalrest*this(ik)%vlsBEGIN
             this(ik)%covfreet=tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen=normalrest*this(ik)%vlnBEGIN
          else
             this(ik)%forecast= 0 !0
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status= i_noctc
          end if
       case('RST_DS_CLB                    ')
          if (this(ik)%gapTTbegin+0.5D0*H*this(ik)%vlnBEGIN .le. 0.D0) then
             ! a contact is forecasted if penetration
             ! at half step time is found negative
             !this(ik)%forecast='acton' (default is 'acton')
             call get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfrees=tangalrest*this(ik)%vlsBEGIN
             this(ik)%covfreet=tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen=normalrest*this(ik)%vlnBEGIN
          else
             this(ik)%forecast=0
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status= i_noctc
          end if
       case default
          call LOGMES('WARNING: default case selected')
          call LOGMES(tact_behav(ibehav)%lawty)
       end select
       
       !-----------------------------------------------
       ! Warning non uniqueness cases
       !-----------------------------------------------
       
       det1=this(ik)%Wss*this(ik)%Wnn-this(ik)%Wns*this(ik)%Wsn
       det2=this(ik)%Wts*this(ik)%Wnn-this(ik)%Wns*this(ik)%Wtn
       det3=this(ik)%Wts*this(ik)%Wsn-this(ik)%Wss*this(ik)%Wtn
       det =this(ik)%Wtt*det1-this(ik)%Wst*det2+this(ik)%Wnt*det3
       
       if (det .lt. 1.D-24) then
          write(cout,546)ik,det
546       format(1X,'    det()= (',I5,') =',D12.5,' < 1.D-24')
          call LOGMES(cout)
          print*,this(ik)%CDAN
       end if
    end do
    
    !-----------------------------------
    !- Computing the initial residue
    !-----------------------------------
    
    if(frictionless)then
       do ik=1,nb_CDAN
          
          WRn= 0.D0
          
          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 9*iadj
             ! W(ik,jl)*r(jl)
             WRn = WRn + Wab(iistart  )*this(ikjl)%rln
          end do
          
          !Calcul du residu de depart
          
          this(ik)%WRn = this(ik)%Wnn*this(ik)%rln
          this(ik)%resn = -(this(ik)%vfreen+this(ik)%covfreen)-this(ik)%WRn-WRn
          
          if(this(ik)%forecast == 0) cycle
          
          this(ik)%wn  = this(ik)%resn
          this(ik)%pn  = this(ik)%wn
          
       end do
       
       if(preconditioner)then
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             this(ik)%wn = this(ik)%resn/this(ik)%Wnn
             this(ik)%pn  = this(ik)%wn
          enddo
       endif
    else
       do ik=1,nb_CDAN
          
          WRs= 0.D0
          WRt= 0.D0
          WRn= 0.D0
          
          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 9*iadj
             ! W(ik,jl)*r(jl)
             WRs = WRs + Wab(iistart-8)*this(ikjl)%rls + Wab(iistart-7)*this(ikjl)%rlt &
                  + Wab(iistart-6)*this(ikjl)%rln
             WRt = WRt + Wab(iistart-5)*this(ikjl)%rls + Wab(iistart-4)*this(ikjl)%rlt &
                  + Wab(iistart-3)*this(ikjl)%rln
             WRn = WRn + Wab(iistart-2)*this(ikjl)%rls + Wab(iistart-1)*this(ikjl)%rlt &
                  + Wab(iistart  )*this(ikjl)%rln
          enddo
          
          !Calcul du residu de depart
          
          this(ik)%WRs = this(ik)%Wss*this(ik)%rls+this(ik)%Wst*this(ik)%rlt+this(ik)%Wsn*this(ik)%rln + WRs
          this(ik)%WRt = this(ik)%Wts*this(ik)%rls+this(ik)%Wtt*this(ik)%rlt+this(ik)%Wtn*this(ik)%rln + WRt
          this(ik)%WRn = this(ik)%Wns*this(ik)%rls+this(ik)%Wnt*this(ik)%rlt+this(ik)%Wnn*this(ik)%rln + WRn
          
          this(ik)%ress = -(this(ik)%vfrees+this(ik)%covfrees)-this(ik)%WRs
          this(ik)%rest = -(this(ik)%vfreet+this(ik)%covfreet)-this(ik)%WRt
          this(ik)%resn = -(this(ik)%vfreen+this(ik)%covfreen)-this(ik)%WRn
          if(this(ik)%forecast == 0) cycle
          
          this(ik)%ws  = this(ik)%ress
          this(ik)%wt  = this(ik)%rest
          this(ik)%wn  = this(ik)%resn
          
          this(ik)%ps  = this(ik)%ws
          this(ik)%pt  = this(ik)%wt
          this(ik)%pn  = this(ik)%wn
       enddo
       
       if(preconditioner)then
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             this(ik)%ws = 0.0
             this(ik)%wt = 0.0
             this(ik)%wn = this(ik)%resn/this(ik)%Wnn
             
             this(ik)%ps  = this(ik)%ws
             this(ik)%pt  = this(ik)%wt
             this(ik)%pn  = this(ik)%wn
          enddo
       endif
    endif
    
  end subroutine prep_cpg
!!!------------------------------------------------------------------------ 
  subroutine ex_iter_cpg

    implicit none
    integer           :: ik,istart,iistart,ikjl,iadj,Nstat
    real(kind=8)      :: RWR,bR
    
    real(kind=8)      :: WPs,WPt,WPn,WRs,WRt,WRn
    real(kind=8)      :: rlsik,rlnik,rltik,psik,pnik,ptik,WRsi,WRti,WRni
    real(kind=8)      :: rlniki

    NOCTC  = 0
    PWP    = 0.D0
    NOKsta = 0

    !- FRICTIONLESS CASE -----------------------------
    !  In this case numerous computations are vanish. 
    !  Thus only normal components are active
    !--------------------------------------------------
    if(frictionless)then                                  ! BEGIN
       do ik=1,nb_CDAN
          WPn = 0.D0
          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 9*iadj
             WPn = WPn + Wab(iistart  )*this(ikjl)%pn
          end do
          pnik = this(ik)%pn
          WPn  = WPn + this(ik)%Wnn*pnik
          PWP  = PWP + WPn*pnik
          this(ik)%WPn = WPn
       end do

       if (abs(PWP).lt.1.D-16) then
          PWPcheck=.true.
          return
       end if

       alpha = 0.D0

       do ik=1,nb_CDAN
          if(this(ik)%forecast == 0) cycle
          alpha = alpha+(this(ik)%resn*this(ik)%pn)
       end do

       alpha  = alpha/PWP
       Nactif = 0

       do ik=1,nb_CDAN
          if(this(ik)%forecast == 0) cycle
          rlnik  = this(ik)%rln 
          rlniki = rlnik + alpha*this(ik)%pn

          if(rlniki <= 0.D0)then
             ! No contact
             this(ik)%rln = 0.D0
             this(ik)%statuscheck = i_noctc
          else
             Nactif = Nactif + 1
             this(ik)%rln = rlniki
             this(ik)%statuscheck = i_slide
          end if
          this(ik)%rlni = this(ik)%rln - rlnik 
       end do

       RWR       = 0.D0

       do ik=1,nb_CDAN

          WRn = 0.D0
          WRni= 0.D0

          istart = this(ik)%istart

          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 9*iadj
             WRn = WRn + Wab(iistart  )*this(ikjl)%rln
          end do

          rlnik = this(ik)%rln
          WRni  = this(ik)%Wnn*rlnik
          WRn   = WRn + WRni
          
          this(ik)%resn =-(this(ik)%vfreen+this(ik)%covfreen+WRn)
          
          this(ik)%status = this(ik)%statuscheck

          this(ik)%WRn = WRni

       end do

       if(norm_check)then
          Enrg = 0.D0
          RWR  = 0.D0
          bR   = 0.D0
          do ik=1,nb_CDAN
             RWR = RWR + this(ik)%WRn*this(ik)%rln
             bR  = bR  + (this(ik)%vfreen+this(ik)%covfreen)*this(ik)%rln
          end do
          Enrg = 0.5*RWR + bR
       end if

       if(preconditioner)then
          do ik=1,nb_CDAN
             this(ik)%resn   = this(ik)%resn/this(ik)%Wnn
          end do
       end if
       
       !Projection sur la facette et calcul de beta

       beta = 0.D0
       
       do ik=1,nb_CDAN
          if(this(ik)%forecast == 0) cycle
          if(this(ik)%status == i_noctc)then
             ! Status de non contact
             ! Premiere projection: r -> w
             if(this(ik)%resn <= 0.D0)then
                this(ik)%wn = 0.D0
                this(ik)%zn = 0.D0
             else
                this(ik)%wn = this(ik)%resn
                if(this(ik)%pn <= 0.D0)then
                   this(ik)%zn = 0.D0
                else
                   this(ik)%zn = this(ik)%pn
                endif
             endif
          else
             ! Status de contact glissant 
             this(ik)%wn = this(ik)%resn
             this(ik)%zn = this(ik)%pn
          endif
          beta = beta+this(ik)%WPn*this(ik)%wn
       end do
       
       beta =-beta/PWP
       
       do ik=1,nb_CDAN
          if(this(ik)%forecast == 0) cycle
          this(ik)%pn = this(ik)%wn+beta*this(ik)%zn
       end do
    
    else

       !- FRICTIONLAL CASE -------------------------------
       !
       !  In this case different projection can be performed.
       !  The keyword ID PROJECTION allows to select one of them:
       !  1 - PYRAMIDAL APPROXIMATION - efficient but no more isotropic friction
       !  2 - NORMAL PROJECTION       - the basic one not really efficient
       !  3 - HYBRID CORRECTION       - efficient for sphere with not really sens 
       !                                for other bodies.
       !
       !--------------------------------------------------

       do ik=1,nb_CDAN

          WPs= 0.D0 
          WPt= 0.D0
          WPn= 0.D0

          istart = this(ik)%istart
          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 9*iadj

             WPs = WPs + Wab(iistart-8)*this(ikjl)%ps + Wab(iistart-7)*this(ikjl)%pt &
                       + Wab(iistart-6)*this(ikjl)%pn
             WPt = WPt + Wab(iistart-5)*this(ikjl)%ps + Wab(iistart-4)*this(ikjl)%pt &
                       + Wab(iistart-3)*this(ikjl)%pn
             WPn = WPn + Wab(iistart-2)*this(ikjl)%ps + Wab(iistart-1)*this(ikjl)%pt &
                       + Wab(iistart  )*this(ikjl)%pn
          end do

          psik  = this(ik)%ps
          ptik  = this(ik)%pt
          pnik  = this(ik)%pn

          WPs = WPs + this(ik)%Wss*psik + this(ik)%Wst*ptik + this(ik)%Wsn*pnik
          WPt = WPt + this(ik)%Wts*psik + this(ik)%Wtt*ptik + this(ik)%Wtn*pnik
          WPn = WPn + this(ik)%Wns*psik + this(ik)%Wnt*ptik + this(ik)%Wnn*pnik

          PWP = PWP + WPs*psik + WPt*ptik + WPn*pnik

          this(ik)%WPs=WPs
          this(ik)%WPt=WPt
          this(ik)%WPn=WPn
       end do

       if(abs(PWP) < 1.D-16)then
          PWPcheck=.true.
          return
       end if

       ALPHA = 0.D0

       do ik=1,nb_CDAN
          if(this(ik)%forecast == 0) cycle
          ALPHA = ALPHA + (this(ik)%ress*this(ik)%ps+this(ik)%rest*this(ik)%pt+this(ik)%resn*this(ik)%pn)
       end do

       ALPHA = ALPHA/PWP

       Nactif = 0

       select case(projID)

       case(1)
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle

             rlsik = this(ik)%rls 
             rltik = this(ik)%rlt 
             rlnik = this(ik)%rln 
             
             this(ik)%rlsi = rlsik + alpha*this(ik)%ps
             this(ik)%rlti = rltik + alpha*this(ik)%pt
             this(ik)%rlni = rlnik + alpha*this(ik)%pn

             call corr_iter_pyr(ik,Nstat)
             
             Nactif = Nactif + Nstat
             this(ik)%rlsi = this(ik)%rls - rlsik 
             this(ik)%rlti = this(ik)%rlt - rltik 
             this(ik)%rlni = this(ik)%rln - rlnik 
          end do
       case(2)
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             
             rlsik = this(ik)%rls 
             rltik = this(ik)%rlt 
             rlnik = this(ik)%rln 
             
             this(ik)%rlsi = rlsik + alpha*this(ik)%ps
             this(ik)%rlti = rltik + alpha*this(ik)%pt
             this(ik)%rlni = rlnik + alpha*this(ik)%pn
             
             call corr_iter_norm(ik,Nstat,this(ik)%rlni,this(ik)%rlti,this(ik)%rlsi)
             
             Nactif = Nactif + Nstat
             this(ik)%rlsi = this(ik)%rls - rlsik 
             this(ik)%rlti = this(ik)%rlt - rltik 
             this(ik)%rlni = this(ik)%rln - rlnik 
          end do
       case(3)
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             
             rlsik = this(ik)%rls 
             rltik = this(ik)%rlt 
             rlnik = this(ik)%rln 
             
             this(ik)%rlsi = rlsik + alpha*this(ik)%ps
             this(ik)%rlti = rltik + alpha*this(ik)%pt
             this(ik)%rlni = rlnik + alpha*this(ik)%pn
             
             call corr_iter_hyb(ik,Nstat,this(ik)%rlni,this(ik)%rlti,this(ik)%rlsi)
             
             Nactif = Nactif + Nstat
             this(ik)%rlsi = this(ik)%rls - rlsik 
             this(ik)%rlti = this(ik)%rlt - rltik 
             this(ik)%rlni = this(ik)%rln - rlnik 
          end do
       end select

       RWR       = 0.D0

       do ik=1,nb_CDAN

          WRs = 0.D0
          WRt = 0.D0
          WRn = 0.D0
          WRsi= 0.D0
          WRti= 0.D0
          WRni= 0.D0

          istart = this(ik)%istart

          do iadj=1,this(ik)%nbadj
             ikjl=this(ik)%adjjl(iadj)
             iistart = istart + 9*iadj
             ! W(ik,jl)*r(jl)
             WRs = WRs + Wab(iistart-8)*this(ikjl)%rls + Wab(iistart-7)*this(ikjl)%rlt &
                       + Wab(iistart-6)*this(ikjl)%rln
             WRt = WRt + Wab(iistart-5)*this(ikjl)%rls + Wab(iistart-4)*this(ikjl)%rlt &
                       + Wab(iistart-3)*this(ikjl)%rln
             WRn = WRn + Wab(iistart-2)*this(ikjl)%rls + Wab(iistart-1)*this(ikjl)%rlt &
                       + Wab(iistart  )*this(ikjl)%rln
          end do

          rlsik = this(ik)%rls
          rltik = this(ik)%rlt
          rlnik = this(ik)%rln
          
          WRsi= this(ik)%Wss*rlsik+this(ik)%Wst*rltik+this(ik)%Wsn*rlnik
          WRti= this(ik)%Wts*rlsik+this(ik)%Wtt*rltik+this(ik)%Wtn*rlnik
          WRni= this(ik)%Wns*rlsik+this(ik)%Wnt*rltik+this(ik)%Wnn*rlnik
          
          WRs = WRs+WRsi
          WRt = WRt+WRti
          WRn = WRn+WRni
          
          this(ik)%ress = -(this(ik)%vfrees+this(ik)%covfrees+WRs)
          this(ik)%rest = -(this(ik)%vfreet+this(ik)%covfreet+WRt)
          this(ik)%resn = -(this(ik)%vfreen+this(ik)%covfreen+WRn)
          
          this(ik)%WRs = WRs
          this(ik)%WRt = WRt
          this(ik)%WRn = WRn

       end do

       if(norm_check)then
          Enrg = 0.D0
          RWR  = 0.D0
          bR   = 0.D0
          do ik=1,nb_CDAN
             RWR = RWR + this(ik)%WRt*this(ik)%rlt + this(ik)%WRs*this(ik)%rls + this(ik)%WRn*this(ik)%rln
             bR  = bR  + (this(ik)%vfreet+this(ik)%covfreet)*this(ik)%rlt + (this(ik)%vfreen+this(ik)%covfreen)*this(ik)%rln &
                       + (this(ik)%vfrees+this(ik)%covfrees)*this(ik)%rls
          end do
          Enrg = 0.5*RWR + bR
       end if

       if(preconditioner)then
          do ik=1,nb_CDAN
             this(ik)%ress   = this(ik)%ress/this(ik)%Wss
             this(ik)%rest   = this(ik)%rest/this(ik)%Wtt
             this(ik)%resn   = this(ik)%resn/this(ik)%Wnn
          end do
       end if

       BETA = 0.D0

       select case(projID)
       case(1)
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             call proj_grad_pyr(ik)
             BETA = BETA + this(ik)%WPs*this(ik)%ws+this(ik)%WPt*this(ik)%wt+this(ik)%WPn*this(ik)%wn
          end do
       case(2)
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             call proj_grad_norm(ik)
             BETA = BETA + this(ik)%WPs*this(ik)%ws+this(ik)%WPt*this(ik)%wt+this(ik)%WPn*this(ik)%wn
          end do
       case(3)
          do ik=1,nb_CDAN
             if(this(ik)%forecast == 0) cycle
             call proj_grad_hyb(ik)
             BETA = BETA + this(ik)%WPs*this(ik)%ws+this(ik)%WPt*this(ik)%wt+this(ik)%WPn*this(ik)%wn
          end do
       end select

       BETA =-BETA/PWP

       do ik=1,nb_CDAN
          if(this(ik)%forecast == 0) cycle
          this(ik)%ps = this(ik)%ws+beta*this(ik)%zs
          this(ik)%pt = this(ik)%wt+beta*this(ik)%zt
          this(ik)%pn = this(ik)%wn+beta*this(ik)%zn
       end do
    end if

  end subroutine ex_iter_cpg
!!!------------------------------------------------------------------------  
!!! SUBROUTINE check_cpg
!!! 
!!! CHECK CPG ALGORITHM CONVERGENCE ACCORDING TO SELECTED TEST
!!!
!!!------------------------------------------------------------------------  
  subroutine check_cpg
    
    implicit none

    integer      :: ik
    real(kind=8) :: DVDV,rlniki,rltiki,rlsiki,rlnik,rltik,rlsik
    real(kind=8) :: Wdrs,Wdrt,Wdrn,WRsi,WRti,WRni 

    meanDVoR  = 0.D0
    QuadDVR   = 0.D0
    QuadDV    = 0.D0
    sumWRR    = 0.D0
    sumDVoR   = 0.D0
    sumWRWR   = 0.D0
    sumDVDV   = 0.D0
    sumDVDVRR = 0.D0
    DVDV      = 0.D0

    !$OMP PARALLEL DEFAULT(SHARED)                           &
    !$OMP PRIVATE(ik,rlniki,rltiki,rlsiki,rlnik,rltik,rlsik, &
    !$OMP         WRsi,WRti,WRni,Wdrs,Wdrt,Wdrn,DVDV)
    
    select case(i_checktype)
    case(i_Quad)

       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:sumWRR,sumWRWR,sumDVoR,sumDVDV,sumDVDVRR)
       do ik=1,nb_CDAN

          if( this(ik)%status /= this(ik)%statuscheck) NOKsta = NOKsta + 1        
          this(ik)%status = this(ik)%statuscheck

          rlniki = this(ik)%rlni
          rltiki = this(ik)%rlti
          rlsiki = this(ik)%rlsi
          rlnik = this(ik)%rln
          rltik = this(ik)%rlt
          rlsik = this(ik)%rls

          WRsi = this(ik)%Wss*rlsik + this(ik)%Wst*rltik + this(ik)%Wsn*rlnik
          WRti = this(ik)%Wts*rlsik + this(ik)%Wtt*rltik + this(ik)%Wtn*rlnik
          WRni = this(ik)%Wns*rlsik + this(ik)%Wnt*rltik + this(ik)%Wnn*rlnik
        
          sumWRR  = sumWRR  + WRsi*rlsik + WRti*rltik + WRni*rlnik
          sumWRWR = sumWRWR + WRsi*WRsi  + WRti*WRti  + WRni*WRni
        
          Wdrt = this(ik)%Wss*rlsiki+this(ik)%Wst*rltiki+this(ik)%Wsn*rlniki
          Wdrt = this(ik)%Wts*rlsiki+this(ik)%Wtt*rltiki+this(ik)%Wtn*rlniki
          Wdrn = this(ik)%Wns*rlsiki+this(ik)%Wnt*rltiki+this(ik)%Wnn*rlniki
        
          DVDV = Wdrs*Wdrs + Wdrt*Wdrt + Wdrn*Wdrn
        
          sumDVoR   = sumDVoR + Wdrs*rlsik + Wdrt*rltik + Wdrn*rlnik 
          sumDVDV   = sumDVDV + DVDV
          sumDVDVRR = sumDVDVRR + DVDV*(rlnik*rlnik+rltik*rltik+rlsik*rlsik)
       end do
       !$OMP END DO

    case(i_QuadN)

       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:sumWRR,sumWRWR,sumDVoR,sumDVDV,sumDVDVRR)
       do ik=1,nb_CDAN
   
          if( this(ik)%status /= this(ik)%statuscheck) NOKsta = NOKsta + 1        
          this(ik)%status = this(ik)%statuscheck

          rlniki = this(ik)%rlni
          rlnik = this(ik)%rln
          
          WRni = this(ik)%Wnn*rlnik
                
          sumWRR  = sumWRR  + WRni*rlnik
          sumWRWR = sumWRWR + WRni*WRni
                
          Wdrn= this(ik)%Wnn*rlniki
                
          DVDV = Wdrn*Wdrn
                
          sumDVoR   = sumDVoR + Wdrn*rlnik 
          sumDVDV   = sumDVDV + DVDV
          sumDVDVRR = sumDVDVRR + DVDV*(rlnik*rlnik)
       end do
       !$OMP END DO

    case(i_Maxm)

       ! not yet define

    case(i_Stag)
       
       !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:sumWRWR,sumDVDV)
       do ik=1,nb_CDAN  

          if( this(ik)%status /= this(ik)%statuscheck) NOKsta = NOKsta + 1        
          this(ik)%status = this(ik)%statuscheck

          rlniki = this(ik)%rlni
          rltiki = this(ik)%rlti
          rlsiki = this(ik)%rlsi

          rlnik = this(ik)%rln
          rltik = this(ik)%rlt
          rlsik = this(ik)%rls

          sumDVDV   = sumDVDV + rlniki*rlniki + rltiki*rltiki + rlsiki*rlsiki
          sumWRWR   = sumWRWR + rlnik*rlnik   + rltik*rltik   + rlsik*rlsik
       end do
       !$OMP END DO

    end select
    
    !$OMP SINGLE
    if(sumWRR  < 1.D-16) sumWRR  = 1.D0
    if(sumWRWR < 1.D-16) sumWRWR = 1.D0

    if(bavard)then
       write(*,*) '@ sumDvoR  =',sumDvoR,'@ sumWRR= ',sumWRR
       write(*,*) '@ sumDVDV  =',sumDVDV,'@ sumWRWR=',sumWRWR
       write(*,*) '@ sumDVDVRR=',sumDVDVRR
    end if
   
    MeanDVoR  =  sumDvoR/sumWRR
    QuadDV    =  sqrt(sumDVDV/sumWRWR)
    QuadDVR   =  sqrt(Nactif*sumDVDVRR)/sumWRR

    !$OMP END SINGLE
    !$OMP END PARALLEL

  end subroutine check_cpg
!!!-------------------------------------------------------------------------
!!! ITERATE CORRECTION SUBROUTINE      
!!!-------------------------------------------------------------------------
!!! ID 1 : PYRAMIDAL APPROXIMATION
!!!-------------------------------------------------------------------------
  subroutine corr_iter_pyr(icdan,Nstat)

    implicit none

    integer,intent(in)  :: icdan
    integer,intent(out) :: Nstat
    real(kind=8)        :: murnik,rlni
    
    rlni = this(icdan)%rlni

    if(rlni <= 0.D0)then

       ! No contact
       this(icdan)%rln = 0.D0
       this(icdan)%rlt = 0.D0
       this(icdan)%rls = 0.D0
       this(icdan)%statuscheck = i_noctc
       Nstat = 0
       
    else
       
       Nstat = 1
       this(icdan)%rln = rlni
       murnik = rlni*this(icdan)%fric
       this(icdan)%statuscheck = i_Ssstt
       !this(icdan)%statuscheck = 'S____'
       
       if(this(icdan)%rlti .ge. murnik)then
          this(icdan)%rlt = murnik
          !this(icdan)%statuscheck(4:5) = 't+'
          this(icdan)%statuscheck = this(icdan)%statuscheck + 3
       elseif(this(icdan)%rlti .le. -murnik)then
          this(icdan)%rlt = -murnik
          !this(icdan)%statuscheck(4:5) = 't-'
          this(icdan)%statuscheck = this(icdan)%statuscheck + 6
       else
          this(icdan)%rlt = this(icdan)%rlti
          !this(icdan)%statuscheck(4:5) = 'tt'
       end if
       
       if(this(icdan)%rlsi .ge. murnik)then
          this(icdan)%rls = murnik
          !this(icdan)%statuscheck(2:3) = 's+'
          this(icdan)%statuscheck = this(icdan)%statuscheck + 1
       elseif(this(icdan)%rlsi .le. -murnik)then
          this(icdan)%rls = -murnik
          !this(icdan)%statuscheck(2:3) = 's-'
          this(icdan)%statuscheck = this(icdan)%statuscheck + 2
       else
          this(icdan)%rls = this(icdan)%rlsi
          !this(icdan)%statuscheck(2:3) = 'ss'
       end if
       
       !Correction suivant la direction indiquée par pk
       !if(this(icdan)%pt == 0.D0)then
       !   if(this(icdan)%status(4:5) == 't+')then
       !      this(icdan)%rlt = murnik
       !      this(icdan)%statuscheck(4:5) = 't+'
       !   elseif(this(icdan)%status(4:5) == 't-')then
       !      this(icdan)%rlt = -murnik
       !      this(icdan)%statuscheck(4:5) = 't-'
       !   end if
       !end if
       !
       !if(this(icdan)%ps == 0.D0)then
       !   if(this(icdan)%status(2:3) == 's+')then
       !      this(icdan)%rls = murnik
       !      this(icdan)%statuscheck(2:3) = 's+'
       !   elseif(this(icdan)%status(2:3) == 's-')then
       !      this(icdan)%rls = -murnik
       !      this(icdan)%statuscheck(2:3) = 's-'
       !   end if
       !end if
       if( this(icdan)%pt == 0.d0 ) then
          if( this(icdan)%status >= i_Ssstp .and. this(icdan)%status <= i_Ssmtp ) then
             this(icdan)%rlt = murnik
             if( this(icdan)%statuscheck >= i_Ssstt .and. this(icdan)%statuscheck <= i_Ssmtt ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck + 3
             else if( this(icdan)%statuscheck >= i_Ssstm .and. this(icdan)%statuscheck <= i_Ssmtm ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck - 3
             end if
          else if( this(icdan)%status >= i_Ssstm .and. this(icdan)%status <= i_Ssmtm ) then
             this(icdan)%rlt = -murnik
             if( this(icdan)%statuscheck >= i_Ssstt .and. this(icdan)%statuscheck <= i_Ssmtt ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck + 6
             else if( this(icdan)%statuscheck >= i_Ssstm .and. this(icdan)%statuscheck <= i_Ssmtm ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck + 3
             end if
          end if
       end if

       if( this(icdan)%ps == 0.d0 ) then
          if( mod( this(icdan)%status-i_Ssstt , 3) == 1 ) then
             this(icdan)%rlt = murnik
             if( mod( this(icdan)%statuscheck-i_Ssstt, 3) == 0 ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck + 1
             else if( mod( this(icdan)%statuscheck-i_Ssstt, 3) == 2 ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck - 1
             end if
          else if( mod( this(icdan)%status-i_Ssstt, 3) == 2 ) then
             this(icdan)%rlt = -murnik
             if( mod( this(icdan)%statuscheck-i_Ssstt, 3) == 0 ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck + 2
             else if( mod( this(icdan)%statuscheck-i_Ssstt, 3) == 1 ) then
                 this(icdan)%statuscheck = this(icdan)%statuscheck + 1
             end if
          end if
       end if
    end if

  end subroutine corr_iter_pyr
!!!-------------------------------------------------------------------------
!!! ID 2 : 
!!!-------------------------------------------------------------------------
  subroutine corr_iter_norm(icdan,Nstat,rn,rt,rs)

    implicit none
    
    integer,intent(in)  :: icdan
    integer,intent(out) :: Nstat
    real(kind=8)        :: murnik,rn,rt,rs,norm
    
    if(rn <= 0.D0)then
       
       ! No contact
       this(icdan)%rln = 0.D0
       this(icdan)%rlt = 0.D0
       this(icdan)%rls = 0.D0
       this(icdan)%statuscheck = i_noctc
       Nstat = 0
       
    else
       
       Nstat = 1
       this(icdan)%rln = rn
       murnik = rn*this(icdan)%fric
       norm = rt*rt+ rs*rs
       
       if(this(icdan)%ic)then
          
          this(icdan)%statuscheck = i_slide
          
          if(norm /= 0) norm = 1.D0/sqrt(norm)
          this(icdan)%rlt = rt*norm*murnik
          this(icdan)%rls = rs*norm*murnik
          this(icdan)%ic = .false.
          
       else if(norm .lt. murnik*murnik)then
          
          this(icdan)%statuscheck = i_stick
          this(icdan)%rlt = rt
          this(icdan)%rls = rs
       else
          this(icdan)%statuscheck = i_slide
          if(norm /= 0) norm = 1.D0/sqrt(norm)
          this(icdan)%rlt = rt*norm*murnik
          this(icdan)%rls = rs*norm*murnik
       end if
    end if
    
  end subroutine corr_iter_norm
!!!------------------------------------------------------------------------
!!! ID 3 : HYBRID CORRECTION
!!!-------------------------------------------------------------------------
  subroutine corr_iter_hyb(icdan,Nstat,rn,rt,rs)

    implicit none
    
    integer,intent(in)  :: icdan
    integer,intent(out) :: Nstat
    real(kind=8)        :: murnik,rn,rt,rs,norm
    
    if(rn <= 0.D0)then
       
       ! No contact
       this(icdan)%rln = 0.D0
       this(icdan)%rlt = 0.D0
       this(icdan)%rls = 0.D0
       this(icdan)%statuscheck = i_noctc
       Nstat = 0
       
    else
       
       Nstat = 1
       this(icdan)%rln = rn
       murnik = rn*this(icdan)%fric
       norm = rt*rt+ rs*rs
       
       if(norm .lt. murnik*murnik)then
          
          if(( this(icdan)%pt == 0.0 ).and.( this(icdan)%ps == 0.0 ))then
             
             rt = this(icdan)%rest/this(icdan)%Wtt + this(icdan)%rlt
             rs = this(icdan)%ress/this(icdan)%Wss + this(icdan)%rls 
             norm = rt*rt+ rs*rs
             
             if(norm .lt. murnik*murnik)then
                this(icdan)%statuscheck = i_stick
                this(icdan)%rlt = rt
                this(icdan)%rls = rs
             else
                this(icdan)%statuscheck = i_slide
                if(norm /= 0) norm = 1.D0/sqrt(norm)
                this(icdan)%rlt = rt*norm*murnik
                this(icdan)%rls = rs*norm*murnik
             end if
          else
             this(icdan)%statuscheck = i_stick
             this(icdan)%rlt = rt
             this(icdan)%rls = rs
          end if
       else
          this(icdan)%statuscheck = i_slide
          if(norm /= 0) norm = 1.D0/sqrt(norm)
          this(icdan)%rlt = rt*norm*murnik
          this(icdan)%rls = rs*norm*murnik
       end if
    end if

  end subroutine corr_iter_hyb
!!!-------------------------------------------------------------------------
!!! GRADIENT PROJECTION SUBROUTINE
!!!-------------------------------------------------------------------------
!!! ID 1 : PYRAMIDAL APPROXIMATION
!!!-------------------------------------------------------------------------
  subroutine proj_grad_pyr(icdan)

    implicit none

    integer,intent(in) :: icdan
    
    if(this(icdan)%status == i_noctc)then
       
       if(this(icdan)%resn <= 0.D0)then
          
          this(icdan)%wn = 0.D0
          this(icdan)%wt = 0.D0
          this(icdan)%ws = 0.D0
          
          this(icdan)%zn = 0.D0
          this(icdan)%zt = 0.D0
          this(icdan)%zs = 0.D0
          
       else
          
          this(icdan)%wn = this(icdan)%resn
          this(icdan)%wt = this(icdan)%rest
          this(icdan)%ws = this(icdan)%ress
          
          ! Seconde projection: p -> z
          if(this(icdan)%pn <= 0.D0)then
             this(icdan)%zn = 0.D0
             this(icdan)%zt = 0.D0
             this(icdan)%zs = 0.D0
          else
             this(icdan)%zn = this(icdan)%pn
             this(icdan)%zt = this(icdan)%pt
             this(icdan)%zs = this(icdan)%ps
          end if
       end if
    else

       this(icdan)%wn = this(icdan)%resn
       this(icdan)%zn = this(icdan)%pn
       
       if(this(icdan)%status == i_Ssstp .or. this(icdan)%status == i_Ssptp .or. this(icdan)%status == i_Ssmtp )then
          this(icdan)%wt = min(0.D0,this(icdan)%rest)
          if(this(icdan)%wt == 0.D0)then
             this(icdan)%zt = 0.D0
          else
             this(icdan)%zt = min(0.D0,this(icdan)%pt)
          end if
       else if(this(icdan)%status == i_Ssstm .or. this(icdan)%status == i_Ssptm .or. this(icdan)%status == i_Ssmtm )then
          this(icdan)%wt = max(0.D0,this(icdan)%rest)
          if(this(icdan)%wt == 0.D0)then
             this(icdan)%zt = 0.D0
          else
             this(icdan)%zt = max(0.D0,this(icdan)%pt)
          end if
       else
          this(icdan)%wt = this(icdan)%rest
          this(icdan)%zt = this(icdan)%pt
       end if
       
       if(this(icdan)%status == i_Ssptt .or. this(icdan)%status == i_Ssptp .or. this(icdan)%status == i_Ssptm )then
          this(icdan)%ws = min(0.D0,this(icdan)%ress)
          if(this(icdan)%ws == 0.D0)then
             this(icdan)%zs = 0.D0
          else
             this(icdan)%zs = min(0.D0,this(icdan)%ps)
          end if
       else if(this(icdan)%status == i_Ssmtt .or. this(icdan)%status == i_Ssmtp .or. this(icdan)%status == i_Ssmtm )then
          this(icdan)%ws = max(0.D0,this(icdan)%ress)
          if(this(icdan)%ws == 0.D0)then
             this(icdan)%zs = 0.D0
          else
             this(icdan)%zs = max(0.D0,this(icdan)%ps)
          end if
       else
          this(icdan)%ws = this(icdan)%ress
          this(icdan)%zs = this(icdan)%ps
       end if
    end if
    
  end subroutine proj_grad_pyr
!!!-------------------------------------------------------------------------
  subroutine proj_grad_norm(icdan)

    implicit none
    
    integer,intent(in) :: icdan
    real(kind=8)       :: suc,tuc,norm,rt,rs,murnik
    
    if(this(icdan)%status == i_noctc)then
       
       if(this(icdan)%resn <= 0.D0)then
          
          this(icdan)%wn = 0.D0
          this(icdan)%wt = 0.D0
          this(icdan)%ws = 0.D0
          
          this(icdan)%zn = 0.D0
          this(icdan)%zt = 0.D0
          this(icdan)%zs = 0.D0
          
       else
          
          this(icdan)%wn = this(icdan)%resn
          this(icdan)%wt = this(icdan)%rest
          this(icdan)%ws = this(icdan)%ress
          
          ! Seconde projection: p -> z
          if(this(icdan)%pn <= 0.D0)then
             this(icdan)%zn = 0.D0
             this(icdan)%zt = 0.D0
             this(icdan)%zs = 0.D0
          else
             this(icdan)%zn = this(icdan)%pn
             this(icdan)%zt = this(icdan)%pt
             this(icdan)%zs = this(icdan)%ps
          end if
       end if
    else

       this(icdan)%wn = this(icdan)%resn
       this(icdan)%zn = this(icdan)%pn
       
       murnik = this(icdan)%rln*this(icdan)%fric
       
       if(murnik .lt. 1.D-18)then
          this(icdan)%wt = 0.0
          this(icdan)%zt = 0.0
          this(icdan)%ws = 0.0
          this(icdan)%zs = 0.0
          return
       end if
       
       if(this(icdan)%status == i_stick)then
          this(icdan)%wt = this(icdan)%rest
          this(icdan)%zt = this(icdan)%pt
          this(icdan)%ws = this(icdan)%ress
          this(icdan)%zs = this(icdan)%ps
       else
          
          rt=this(icdan)%rlt/murnik
          rs=this(icdan)%rls/murnik
          
          suc = this(icdan)%ress
          tuc = this(icdan)%rest
          
          norm=rt*tuc+rs*suc
          
          if(norm.le.0)then
             this(icdan)%wt = this(icdan)%rest
             this(icdan)%ws = this(icdan)%ress
             
             suc = this(icdan)%ps
             tuc = this(icdan)%pt
             
             norm=rt*tuc+rs*suc
             
             if(norm.le.0)then
                this(icdan)%zt = this(icdan)%pt
                this(icdan)%zs = this(icdan)%ps
             else
                norm=-rs*tuc+rt*suc
                
                this(icdan)%zt = -rs*norm
                this(icdan)%zs =  rt*norm
             end if
          else
             norm=-rs*tuc+rt*suc
             
             this(icdan)%wt = -rs*norm
             this(icdan)%ws =  rt*norm
             
             suc = this(icdan)%ps
             tuc = this(icdan)%pt
             
             norm=-rs*tuc+rt*suc
             
             this(icdan)%zt = -rs*norm
             this(icdan)%zs =  rt*norm
             this(icdan)%ic = .true.
          end if
       end if
    end if

  end subroutine proj_grad_norm
!!!-------------------------------------------------------------------------
!!! ID 3 : HYBRID CORRECTION
!!!-------------------------------------------------------------------------
  subroutine proj_grad_hyb(icdan)

    implicit none
    
    integer,intent(in) :: icdan
    real(kind=8)       :: suc,tuc,norm,rt,rs
    
    !- No Contact status -------------------
    
    if(this(icdan)%status == i_noctc)then
       
       if(this(icdan)%resn <= 0.D0)then
          
          this(icdan)%wn = 0.D0
          this(icdan)%wt = 0.D0
          this(icdan)%ws = 0.D0
          
          this(icdan)%zn = 0.D0
          this(icdan)%zt = 0.D0
          this(icdan)%zs = 0.D0
          
       else
          
          this(icdan)%wn = this(icdan)%resn
          this(icdan)%wt = this(icdan)%rest
          this(icdan)%ws = this(icdan)%ress
          
          ! Seconde projection: p -> z
          if(this(icdan)%pn <= 0.D0)then
             this(icdan)%zn = 0.D0
             this(icdan)%zt = 0.D0
             this(icdan)%zs = 0.D0
          else
             this(icdan)%zn = this(icdan)%pn
             this(icdan)%zt = this(icdan)%pt
             this(icdan)%zs = this(icdan)%ps
          end if
       end if
    else

       this(icdan)%wn = this(icdan)%resn
       this(icdan)%zn = this(icdan)%pn
       
       ! STICK CONTACT ----------------------
       
       if(this(icdan)%status == i_stick)then
          this(icdan)%wt = this(icdan)%rest
          this(icdan)%zt = this(icdan)%pt
          this(icdan)%ws = this(icdan)%ress
          this(icdan)%zs = this(icdan)%ps
       else
          
          ! SLIP CONTACT -----------------------
          
          rt  =this(icdan)%rlt
          rs  =this(icdan)%rls
          suc = this(icdan)%ress
          tuc = this(icdan)%rest
          
          norm=rt*tuc+rs*suc
          
          if(norm.ge.0)then        
             
             this(icdan)%wt = 0.0
             this(icdan)%ws = 0.0
             
             this(icdan)%zt = 0.0
             this(icdan)%zs = 0.0
             
          else
             
             this(icdan)%wt = this(icdan)%rest
             this(icdan)%ws = this(icdan)%ress
             
             suc = this(icdan)%ps
             tuc = this(icdan)%pt
             
             norm= rs*suc+rt*tuc
             
             if(norm.ge.0)then        
                
                this(icdan)%zt = 0.0
                this(icdan)%zs = 0.0
                
             else
                this(icdan)%zt = this(icdan)%pt
                this(icdan)%zs = this(icdan)%ps
             end if
          end if
       end if
    end if

  end subroutine proj_grad_hyb
!!!------------------------------------------------------------------------  
  subroutine Nullify_EntityList_cpg

    implicit none
    
    call Free_EntityList
    
  end subroutine Nullify_EntityList_cpg
!!!------------------------------------------------------------------------
  subroutine get_cpg_loop(compteur,err1,err2,err3,contact)
    
    implicit none
    integer      :: compteur,contact
    real(kind=8) :: err1,err2,err3
    
    compteur = cpg_loop
    contact  = nb_cdan
    err1  = MeanDVoR
    err2  = QuadDV
    err3  = QuadDVR

  end subroutine get_cpg_loop
!!! ----------------------------------------------------------------------
  subroutine get_cpg_contact_status(noctc,stick,slide)
    
    implicit none
    
    integer :: noctc,stick,slide
    
    noctc = Nnoctc
    stick = Nstick
    slide = Nslide

  end subroutine get_cpg_contact_status
!!!-------------------------------------------------------------------------
  subroutine get_cpg_network_change(nctc,nweak,nstrong)

    implicit none
    integer :: ik,nctc,nweak,nstrong

    nctc    = 0
    nweak   = 0
    nstrong = 0

    do ik = 1,nb_CDAN
       if(this(ik)%status == this(ik)%statusBEGIN) cycle
       nctc = nctc + 1
       if(this(ik)%iws == 0)then
          nweak = nweak + 1
       else
          nstrong = nstrong + 1
       end if
    end do

  end subroutine get_cpg_network_change
!!!------------------------------------------------------------------------  
  subroutine ex_post_cpg
    
    implicit none
    
    character(len=13) :: IAM='cpg::post_cpg'
    character(len=80) :: cout
    integer           :: ik
    real(kind=8)      :: vlsiki,vltiki,vlniki,gapTT

    Nactif = 0
    Nnoctc = 0
    Nstick = 0
    Nslide = 0

    ! Rnod = [H] Rloc
    do ik=1,nb_CDAN  
       call nullify_reac(this(ik)%CDAN,ik,iIreac)
    end do

    do ik=1,nb_CDAN
       if (DABS(this(ik)%rln) <1.D-21) this(ik)%rln = 0.0
       if (DABS(this(ik)%rlt) <1.D-21) this(ik)%rlt = 0.0
       if (DABS(this(ik)%rls) <1.D-21) this(ik)%rls = 0.0
       call injj(this(ik)%CDAN,ik,this(ik)%rlt,this(ik)%rln,this(ik)%rls,iIreac)
    end do

    do ik=1,nb_CDAN
       
       ! Analyzing status
       if (this(ik)%status == i_noctc)then
          Nnoctc = Nnoctc + 1 
       else
          Nactif = Nactif + 1
          if (this(ik)%status == i_stick) then  
             Nstick = Nstick + 1
          elseif (this(ik)%status == i_slide) then  
             Nslide = Nslide + 1
          end if
       end if
       
       vlsiki = -this(ik)%ress
       vltiki = -this(ik)%rest
       vlniki = -this(ik)%resn
       
       if (DABS(vlsiki) .lt. 1.D-21) vlsiki=0.D0
       if (DABS(vltiki) .lt. 1.D-21) vltiki=0.D0
       if (DABS(vlniki) .lt. 1.D-21) vlniki=0.D0
       
       gapTT = this(ik)%gapTTbegin+H*vlniki
       
       if (DABS(gapTT) .lt. 1.D-24) gapTT=0.D0
       
       call set_loc(this(ik)%CDAN, this(ik)%icdan,this(ik)%status, &
                    vltiki,vlniki,vlsiki,                          &
                    this(ik)%rlt,this(ik)%rln,this(ik)%rls,gapTT)
    end do
    
    do ik=1,nb_CDAN
       if (associated(this(ik)%adjjl)) then
          deallocate(this(ik)%adjjl)
          nullify(this(ik)%adjjl)
       end if
    end do
    
    if (allocated(Wab)) then
       deallocate(Wab)
    end if
   
 end subroutine ex_post_cpg
!---------------------------------------------------------------------
end module CPG_3D















