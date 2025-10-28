!===========================================================================
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

!> interaction database : management
!> \author R. Mozul (alas)
!> \date   June 2013
!>
!> General behaviour of a detection :
!> Ask specific modules (DKDK, PRPR,...) to do a rough detection.
!> Each rough interaction will be used to create a F2F structure.
!> The F2F structure will be given to specific modules to generate
!> inside it the desired number of interactions.
!> At last the list of interactions used by the solver will be generated.

module interaction_handler

  use parameters

  use interaction_2D, only : T_interaction, all_t2t   , &
                             get_nb_interactions      , &
                             get_nb_rough_interactions, &
                             get_nb_recup_interactions, &
                             nb_tact2tacts            , &
                             set_nb_tact2tacts        , &
                             t2t_to_this              , &
                             add_t2t                  , &
                             vrlt_map                 , &
                             interaction_stock_rloc => stock_rloc, &
                             interaction_recup_rloc => recup_rloc, &
                             reset_this_for_read       , &
                             read_vloc_rloc_header_1   , &
                             read_vloc_rloc_header_2   , &
                             read_vloc_rloc_header_3   , &
                             read_ini_vloc_rloc        , &
                             get_ptr_all               , &
                             get_nb_verlet_interactions, &
                             get_verlet_interaction    , &
                             clean_memory_interaction

  use DISKx, only : get_nb_DISKx, &
                    diskx2bdyty

  use CLxxx, only : get_nb_CLxxx, &
                    clxxx2bdyty

  use ALpxx, only : alpxx2bdyty

  use DKDKx, only : creation_tab_visu_DKDKx      , &
                    compute_box_DKDKx            , &
                    coor_prediction_DKDKx        , &
                    RUN_DKDKx                    , &
                    CHECK_DKDKx                  , &
                    get_nb_rough_DKDKx           , &
                    get_rough_DKDKx              , &
                    reset_violation_DKDKx        , &
                    reset_nb_adj_DKDKx           , &
                    add_adj_DKDKx                , &
                    get_nb_adj_DKDKx             , &
                    compute_contacts_in_t2t_DKDKx, &
                    write_out_one_vloc_rloc_DKDKx

  use CLALp, only : creation_tab_visu_CLALp      , &
                    compute_box_CLALp            , &
                    coor_prediction_CLALp        , &
                    RUN_CLALp                    , &
                    CHECK_CLALp                  , &
                    get_nb_rough_CLALp           , &
                    get_rough_CLALp              , &
                    reset_violation_CLALp        , &
                    reset_nb_adj_CLALp           , &
                    add_adj_CLALp                , &
                    get_nb_adj_CLALp             , &
                    compute_contacts_in_t2t_CLALp, &
                    write_out_one_vloc_rloc_CLALp

  !use rough_detections, only : boxes_method

  implicit none

  private

  !> array to know 1/ if to detect this type, 2/ is initialized, 3/ nb interactions of this type
  integer(kind=4), dimension(3,nb_interaction_types) :: with_detect = 0

  public get_nb_interactions      , &
         get_nb_rough_interactions, &
         get_nb_recup_interactions, &
         run_rough_detections     , &
         activate_detection , &
         run_detection      , &
         stock_rloc         , &
         recup_rloc         , &
         write_xxx_vloc_rloc, &
         read_xxx_vloc_rloc , &
         get_ptr_all        , &
         clean_memory

  private initialize_t2t_from_rough, &
          run_fine_detections      , &
          write_out_vloc_rloc

  contains

  !----------------------------------------------------------
  ! Functions to perform the detections
  !----------------------------------------------------------

  !> \brief select/activate a detection method for an interaction type
  !> exact same function in 3D, how to write only once ?
  !> \todo : check the contact type is 2D
  subroutine activate_detection(cdan, detec_id)
    !> interaction type
    integer(kind=4), intent(in) :: cdan
    !> detection method to use for the interaction type
    integer(kind=4), intent(in) :: detec_id

    with_detect(1,cdan) = detec_id

  end subroutine

  !> \brief run full detection process
  subroutine run_detection()
    implicit none
    !
    integer(kind=4) :: nb_rough

    nb_rough = run_rough_detections()

    call set_nb_tact2tacts(nb_rough)

    call initialize_t2t_from_rough()
    
    call run_fine_detections()

    call t2t_to_this()

  end subroutine

  !> \brief run rough detections for desired contact types
  function run_rough_detections()
    implicit none
    !> total number of rough interactions found
    integer(kind=4) :: run_rough_detections

    run_rough_detections = 0

    ! for DKDKx
    !rm : maybe this whole bloc should be only one function of mod_DKDKx
    if( with_detect(1,i_dkdkx)>0 .and. check_DKDKx() ) then
      if( with_detect(2,i_dkdkx) < 1 ) then
        call compute_box_DKDKx
        with_detect(2,i_dkdkx) = 1
      end if

      call coor_prediction_DKDKx

      if( RUN_DKDKx() ) then
        call creation_tab_visu_DKDKx
        run_rough_detections = run_rough_detections + get_nb_rough_DKDKx()
      end if
    end if

    ! for CLALp
    if( with_detect(1,i_clalp)>0 .and. check_CLALp() ) then
      if( with_detect(2,i_clalp) < 1 ) then
        call compute_box_CLALp
        with_detect(2,i_clalp) = 1
      end if

      call coor_prediction_CLALp

      if( RUN_CLALp() ) then
        call creation_tab_visu_CLALp
        run_rough_detections = run_rough_detections + get_nb_rough_CLALp()
      end if
    end if

  end function

  !> \brief initialize t2t structure in interaction module from rough interactions
  subroutine initialize_t2t_from_rough()
    implicit none
    integer(kind=4) :: rough_shift, i_rough, icdtac, iantac, isee, i4, periodic

    rough_shift = 0

    if( with_detect(1,i_dkdkx)>0 .and. check_DKDKx() ) then
      do i_rough = 1, get_nb_rough_DKDKx()
        call get_rough_DKDKx(i_rough, icdtac, iantac, isee, periodic)
        call add_t2t(i_dkdkx, rough_shift+i_rough,icdtac, iantac, isee, 0, periodic, 0)
      end do
      rough_shift = get_nb_rough_DKDKx()
    end if

    if( with_detect(1,i_clalp)>0 .and. check_CLALp() ) then
      do i_rough = 1, get_nb_rough_CLALp()
        call get_rough_CLALp(i_rough, icdtac, iantac, isee, periodic, i4)
        call add_t2t(i_clalp, rough_shift+i_rough,icdtac, iantac, isee, i4, periodic, 0)
      end do
      rough_shift = get_nb_rough_CLALp()
    end if

  end subroutine

  !> \brief run fine detections of submodules
  subroutine run_fine_detections()
    use overall, only : logmes, faterr
    implicit none
    integer(kind=4)   :: i_t2t, i_f2f
    character(len=80) :: cout

    with_detect(3,:) = 0

    if( with_detect(1,i_dkdkx) > 0 ) call reset_nb_adj_DKDKx()
    if( with_detect(1,i_clalp) > 0 ) call reset_nb_adj_CLALp()

    if (nb_tact2tacts .eq. 0 ) return

    do i_t2t = 1, nb_tact2tacts
      select case(all_t2t(i_t2t)%cdan)
      case( i_dkdkx )
        call compute_contacts_in_t2t_DKDKx(all_t2t(i_t2t), with_detect(3,i_dkdkx))
      case( i_clalp )
        call compute_contacts_in_t2t_CLALp(all_t2t(i_t2t), with_detect(3,i_clalp))
      case default
        call faterr('interaction_handler::run_fine_detections','no detection for contact type')
      end select 

      do i_f2f = 1, all_t2t(i_t2t)%nb_f2f
        with_detect(3,all_t2t(i_t2t)%cdan) = &
        with_detect(3,all_t2t(i_t2t)%cdan) + all_t2t(i_t2t)%f2f(i_f2f)%nb_ctc
      end do

    end do

    if( with_detect(1,i_dkdkx) > 0 ) call reset_violation_DKDKx(with_detect(3,i_dkdkx))
    if( with_detect(1,i_clalp) > 0 ) call reset_violation_CLALp(with_detect(3,i_clalp))

    if( with_detect(1,i_dkdkx) > 0 ) then
      write(cout,'(1X,I10,A12)') with_detect(3,i_dkdkx), ' DKDKx found'
      call logmes(cout)
    end if
    if( with_detect(1,i_clalp) > 0 ) then
      write(cout,'(1X,I10,A12)') with_detect(3,i_clalp), ' CLALp found'
      call logmes(cout)
    end if

  end subroutine

  !> \brief Stock local reaction from this to verlet
  subroutine stock_rloc()
    implicit none
    integer(kind=4) :: i, i_tact, nb_contactors

    nb_contactors = get_nb_DISKx() + get_nb_CLxxx()
    
    if( .not. allocated(vrlt_map) ) allocate(vrlt_map(nb_contactors+1))

    vrlt_map = 0

    i = 1
    if( with_detect(3, i_dkdkx ) > 0 ) then
      do i_tact = 1, get_nb_DISKx()
        vrlt_map(i+1) = vrlt_map(i) + get_nb_adj_DKDKx(i_tact)
        i = i + 1
      end do
    end if

    if( with_detect(3, i_clalp ) > 0 ) then
      do i_tact = 1, get_nb_CLxxx()
        vrlt_map(i+1) = vrlt_map(i) + get_nb_adj_CLALp(i_tact)
        i = i + 1
      end do
    end if

    !\todo: the other contactor types...

    call interaction_stock_rloc()

  end subroutine

  !> \brief Recup local reaction from verlet to this
  subroutine recup_rloc()
    implicit none

    call interaction_recup_rloc()

  end subroutine

  !> \brief Read Vloc_Rloc.INI file to initialize verlet
  !> \todo : file version management
  subroutine read_xxx_Vloc_Rloc
    use overall, only : G_nfich     , &
                        read_G_clin , &
                        G_clin      , &
                        get_io_unit , &
                        location    , &
                        in_vloc_rloc, &
                        xxl_check   , &
                        logmes, faterr
    implicit none
    integer(kind=4)  :: nb_read, ialxxx, lantac, icdbdy, ianbdy, icdtac, iantac, cdan
    integer(kind=4)  :: nb_contactors, nb_cd_contactors, i, i_tact, htype
    character(len=5) :: cdbdy, anbdy, cdtac, antac, behav, sttus
    
    G_nfich=get_io_unit()
    open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))

    !====== first reading: sizing ===== !
    nb_read = 0

    !>\warning: the first reading is done here because the adj map of each detection
    !           module must be initialized and interaction_2D module is a dependency of
    !           DKDKx and others... Thus it is done here and not in interaction_2D module

    if( with_detect(1,i_dkdkx) > 0 ) call reset_nb_adj_DKDKx()
    if( with_detect(1,i_clalp) > 0 ) call reset_nb_adj_CLALp()

    do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle
      cdan = get_interaction_id_from_name(G_clin(9:13))
      ! fishing for the keyword 'icdan'
      if ( .not. read_G_clin()) exit
      if ( .not. read_G_clin()) exit
      nb_read = nb_read + 1
      select case( cdan )
      case( i_dkdkx )
        if(xxl_check) then
          htype = 2
          call read_vloc_rloc_header_2(cdbdy, icdbdy, cdtac, icdtac, behav,       &
                                       anbdy, ianbdy, antac, iantac, sttus, lantac)
        else
          htype = 1
          call read_vloc_rloc_header_1(cdbdy, icdbdy, cdtac, icdtac, behav,       &
                                       anbdy, ianbdy, antac, iantac, sttus, lantac)
        end if
        call add_adj_DKDKx(icdbdy,icdtac)
      case( i_clalp )
        htype = 3
        call read_vloc_rloc_header_3(cdbdy, icdbdy, cdtac, icdtac, behav ,              &
                                     anbdy, ianbdy, antac, iantac, ialxxx, sttus, lantac)
        call add_adj_CLALp(icdbdy,icdtac)
      case default
        call faterr('interaction_handler::read_xxx_vloc_rloc','no detection for contact type '//get_interaction_name_from_id(cdan))
      end select
    end do

    if( nb_read == 0 ) return
    !========================================!

    !====== preparing for second reading ====!

    call reset_this_for_read(nb_read)

    nb_cd_contactors = 0
    if( with_detect(1,i_dkdkx) > 0 ) then
      nb_contactors = get_nb_DISKx()
      call read_ini_vloc_rloc(nb_contactors, i_dkdkx, diskx2bdyty, diskx2bdyty, htype, nb_read)
      nb_cd_contactors = nb_cd_contactors + nb_contactors
      with_detect(3,i_dkdkx) = nb_read
    end if
    if( with_detect(1,i_clalp) > 0 ) then
      nb_contactors = get_nb_CLxxx()
      call read_ini_vloc_rloc(nb_contactors, i_clalp, clxxx2bdyty, alpxx2bdyty, htype, nb_read)
      nb_cd_contactors = nb_cd_contactors + nb_contactors
      with_detect(3,i_clalp) = nb_read
    end if

    !========================================!

    !=========== create verlet maps =========!
    if( .not. allocated(vrlt_map) ) allocate(vrlt_map(nb_cd_contactors+1))
    vrlt_map = 0
    i = 1
    if( with_detect(1,i_dkdkx) > 0 ) then
      do i_tact = 1, get_nb_DISKx()
        vrlt_map(i+1) = vrlt_map(i) + get_nb_adj_DKDKx(i_tact)
        i = i + 1
      end do
    end if
    if( with_detect(1,i_clalp) > 0 ) then
      do i_tact = 1, get_nb_CLxxx()
        vrlt_map(i+1) = vrlt_map(i) + get_nb_adj_CLALp(i_tact)
        i = i + 1
      end do
    end if
    !========================================!

    !============= finally stock ============!
    call stock_rloc
    !========================================!

    close(G_nfich)
    
  end subroutine read_xxx_Vloc_Rloc

  !> \brief Write Vloc_Rloc.OUT file
  !> exact same functions in 3D... how to write only once ?
  subroutine write_xxx_Vloc_Rloc(which)
    use overall, only : get_io_unit   , &
                        out_Vloc_Rloc , &
                        last_Vloc_Rloc, &
                        location
    implicit none
    !> In what file name to write : 1-OUT, 2-LAST, 6-standard output
    integer(kind=4),intent(in) :: which
    !
    integer(kind=4)            :: nfich,lc
    
    nfich = get_io_unit()
    
    select case(which)
    case(1)
      lc = len_trim(out_Vloc_Rloc)
      open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_Vloc_Rloc(1:lc))))
      call write_out_Vloc_Rloc(nfich)
      close(nfich)
    case(2)
      lc = len_trim(last_Vloc_Rloc)
      open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_Vloc_Rloc(1:lc))))
      call write_out_Vloc_Rloc(nfich)
      close(nfich)
    case(6)
      call write_out_Vloc_Rloc(6)
    end select
    
  end subroutine write_xxx_Vloc_Rloc

  !> \brief Write local velocities and reactions to a file
  subroutine write_out_Vloc_Rloc(nfich)
    use overall, only : faterr
    implicit none
    !> [in] unit file in which to write
    integer(kind=4), intent(in) :: nfich
    !
    integer(kind=4)  :: icdan
    type(T_interaction), pointer :: inter
    
    ! write verlet ?
    do icdan = 1, get_nb_verlet_interactions()

      inter => get_verlet_interaction(icdan)
      select case(inter%cdan)
      case(i_dkdkx)
        call write_out_one_vloc_rloc_dkdkx(nfich, inter)
      case(i_clalp)
        call write_out_one_vloc_rloc_clalp(nfich, inter)
      case default
        call faterr('interaction_handler::write_out_vloc_rloc', &
                    'no impletented for contact type '//get_interaction_name_from_id(inter%cdan))
      end select

    end do

  end subroutine write_out_Vloc_Rloc

  !> \brief Free allocated memory within module and interaction module
  subroutine clean_memory()
    implicit none

    call clean_memory_interaction()

  end subroutine

end module

