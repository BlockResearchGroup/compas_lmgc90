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
MODULE poroMAILx


  !! PURPOSE
  !!  modelling of poro deformable system through finite elements


  USE overall
  USE utilities
  use parameters
  use timer
  
  use algebra
  USE DiscreteGeometry
  USE RigidKinematic
  
  USE bulk_behaviour
  USE models
  
  USE a_DOF
  
  use a_system, only : T_link_connec             , &
                       g_system                  , &
                       initialize_system         , &
                       get_nb_non_zero           , &
                       erase_elementary_matrix   , &
                       add_to_elementary_matrix  , &
                       set_vector                , &
                       assemble_elementary_vector, &
                       erase_drvdofs             , &
                       set_drvdofs               , &
                       set_drvvalues             , &
                       solve_system              , &
                       erase_system              , &
                       sparse_storage_available 
  
  USE a_EF
  USE a_poroEF
  
  USE MAILx
  
  use MAILx_type, only : T_poroMAILx
  
  IMPLICIT NONE
  
  PRIVATE
    
  type( T_poroMAILx ), dimension( : ), allocatable, private, target :: bdyty !
  
  ! mapping between local  to global bdyty number
  
  INTEGER,DIMENSION(:),ALLOCATABLE,PRIVATE           :: bdyty2M_bdyty
  
  ! reverse mapping between global 2 local bdyty,blmty,nodty numbering
  
  TYPE(T_MAILx_2_localMAILx),DIMENSION(:),ALLOCATABLE,PUBLIC :: M2poro
  
  !fd 
  INTEGER :: nb_poroMAILx=0
  INTEGER :: nb_existing_entities     ! when abonning new kind of body to entity list
                                      ! you need to know how many entities already exist  
  ! parametres permettant de stopper un calcul si la vitesse est stabilisee.
  !
  REAL(kind=8)      :: eqs_tol
  INTEGER           :: eqs_ichecktype
  INTEGER,PARAMETER :: iQvlcy = 1 , iMvlcy = 2
  
  !type de stockage des matrices
  ! i_diagonal, i_sparse, i_band, i_skyline, i_full
  INTEGER :: Matrix_storage = -99
  !type de profil des matrices
  ! i_sym , i_std
  INTEGER :: Matrix_shape = i_std
  ! pour les matrices denses
  LOGICAL :: with_renum = .TRUE.
  
  !fd 5/11/09
  ! define the strategy new/re-use of ppset
  ! try to reduce computation time introduced when using a new ppset by gp with matlib  
  LOGICAL :: use_existing_ppset = .TRUE.

  !=============== methodes ===================================!
  
  PUBLIC read_in_driven_dof_poroMAILx, &
         write_out_driven_dof_poroMAILx, &
         load_models_poroMAILx, &
         update_existing_entities_poroMAILx, &
         load_behaviours_poroMAILx, &
         push_ppset_poroMAILx, &
         set_without_renum_poroMAILx, &
         put_vector_poroMAILx, &
         read_in_dof_poroMAILx, &
         read_in_meca_dof_poroMAILx, &
         read_in_gpv_poroMAILx, &
         read_in_meca_gpv_poroMAILx, &
         get_nb_poroMAILx, &
         write_xxx_dof_poroMAILx, &
         compute_mass_poroMAILx, &
         compute_Fext_poroMAILx, &
         compute_bulk_poroMAILx, &
         compute_damping_poroMAILx, &
         assemb_KT_poroMAILx, &
         assemb_RHS_poroMAILx, &
         compute_free_vlocy_poroMAILx, &
         compute_dof_poroMAILx, &
         update_dof_poroMAILx, &
         update_bulk_poroMAILx, &
         compute_residue_norm_poroMAILx, &
         get_vector_poroMAILx, &
         get_N_NODE_poroMAILx, &
         increment_poroMAILx, &
         DISPLAY_bulk_element_poroMAILx, &
         check_equilibrium_state_poroMAILx, &
         apply_drvdof_KT_poroMAILx, &
         set_Matrix_Storage_poroMAILx, &
         set_Matrix_Shape_poroMAILx, &
         get_meca_field_rank, get_ther_field_rank, &
         set_meca_field_bynode, set_meca_field_byelem, &
         set_ther_field_bynode, set_ther_field_byelem, &
         get_meca_vfield_rank, get_ther_vfield_rank, &
         set_meca_vfield_bynode, set_meca_vfield_byelem, &
         set_ther_vfield_bynode, set_ther_vfield_byelem, &
         load_ALE_poroMAILx, &
         set_vlocy_drvdof_poroMAILx, &
         get_2DNodalStrain_poroMAILx, &
         get_3DNodalStrain_poroMAILx, &
         get_2DNodalStress_poroMAILx, &
         get_3DNodalStress_poroMAILx, &
         get_Internal_poroMAILx     , &
         add_reac_nodty_poroMAILx, &
         nullify_poro_driven_dof, &
         nullify_reac_poroMAILx,&
         nullify_vlocy_poroMAILx,&
         comp_vlocy_bynode_poroMAILx,&
         comp_vlocy_poroMAILx,&
         get_V_nodty_poroMAILx,&
         get_Vfree_nodty_poroMAILx,&
         get_Vbegin_nodty_poroMAILx,&
         get_Vaux_nodty_poroMAILx,&
         get_entity_poroMAILx,&
         get_coorTT_nodty_poroMAILx,&
         get_cooref_nodty_poroMAILx,&
         get_visible_poroMAILx,&
         set_visible_poroMAILx,&
         set_precon_node_poroMAILx,&
         compute_configurationTT_poroMAILx,&
         compute_precon_vaux_bynode_poroMAILx,&
         compute_precon_vaux_poroMAILx, &
         set_precon_body_poroMAILx, &
         compute_precon_W_poroMAILx ,&
         get_Pfree_nodty_poroMAILx, &
         get_P_nodty_poroMAILx, &
         get_X_nodty_poroMAILx, &
         get_Pbegin_nodty_poroMAILx, &
         get_Paux_nodty_poroMAILx, &
         add_flux_nodty_poroMAILx, &
         get_coor_poroMAILx, &
         get_All_poroMAILx, &
         get_connectivity_poroMAILx, &
         get_NodalGrad_poroMAILx, &
         get_NodalFlux_poroMAILx, &
         Add_Field_Load_bynode_poroMAILx, &
         get_write_DOF_poroMAILx, &
         get_write_Rnod_poroMAILx, &
         post_models_poroMAILx, &
         clean_memory_poroMAILx, &
         check_properties_poroMAILx, &
         get_nb_gp_by_elem_poroMAILx

  !rm: accessor for hdf5
  public get_nb_elements_poroMAILx, &
         get_nb_gp_poroMAILx, &
         get_field_poroMAILx, &
         set_field_poroMAILx

  public get_bdyty_poroMAILx

CONTAINS 

!------------------------------------------------------------------------

  subroutine get_bdyty_poroMAILx( arg_bdyty )

    implicit none

    type( T_poroMAILx ), dimension( : ), pointer :: arg_bdyty

    arg_bdyty => bdyty

  end subroutine get_bdyty_poroMAILx

!------------------------------------------------------------------------  
! read and write routine
!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------
  SUBROUTINE check_equilibrium_state_poroMAILx(info)

    IMPLICIT NONE 

    INTEGER           :: ibdyty,inodty,iccdof,nbdof,i,nn
    REAL(kind=8)      :: norm,Qnorm,Mnorm
    LOGICAL           :: info
    CHARACTER(len=40) :: cout

    Qnorm = 0.D0
    Mnorm =-1.D20
    nn    = 0

    !am & pta : prevoir le cas rigide...

    DO ibdyty=1,SIZE(bdyty)
       DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)
          nn = nn +1
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          norm = 0.d0
          DO i=1,nbdof
             norm= norm + (bdyty(ibdyty)%Vbegin(iccdof+i)**2)
          ENDDO
          norm = dsqrt(norm)

          Qnorm = Qnorm + norm
          Mnorm= MAX (Mnorm,norm)                                
       END DO
    END DO

    Qnorm = Qnorm / REAL(nn,8)

    WRITE(cout,'(1X,A3,2(3X,A12,D10.3,1X))') &
         ' @ ','Qnorm / tol=',Qnorm/eqs_tol,'Mnorm / tol=',Mnorm/eqs_tol 
    CALL LOGMES(cout)

    info = .FALSE.

    SELECT CASE(eqs_ichecktype)
    CASE(iQvlcy)
       IF (Qnorm <= eqs_tol) info = .TRUE.
    CASE(iMvlcy)
       IF (Mnorm <= eqs_tol) info = .TRUE.
    END SELECT

    
  END SUBROUTINE check_equilibrium_state_poroMAILx 

  SUBROUTINE DISPLAY_bulk_element_poroMAILx(ibdyty,iblmty)
    IMPLICIT NONE
    INTEGER :: ibdyty,iblmty,idof
  
    PRINT *,'Body ',ibdyty,' bulk ',iblmty
    PRINT *,'stiffness :'
    DO idof=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%stiffness,dim=2)
    PRINT '(24(1x,D8.1))', bdyty(ibdyty)%blmty(iblmty)%stiffness(:,idof)
    ENDDO
    PRINT *,'damping :'
    DO idof=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%damping,dim=2)
    PRINT '(24(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%damping(:,idof)
    ENDDO
    PRINT *,'mass :'
    DO idof=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%mass,dim=2)
    PRINT '(24(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%mass(:,idof)
    ENDDO
    PRINT *,'ttfint :'
    PRINT '(1(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%ttfint(:)
    PRINT *,'fext :'
    PRINT '(1(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%fext(:,1)
    PRINT *,'fint :'
    PRINT '(1(1x,D8.1))',bdyty(ibdyty)%blmty(iblmty)%fint(:,1)

    PRINT*,'===='

  END SUBROUTINE DISPLAY_bulk_element_poroMAILx

!!!---------------------------------------------------------
  SUBROUTINE read_in_driven_dof_poroMAILx

    IMPLICIT NONE

    G_nfich = 1
    OPEN(unit=G_nfich,file=TRIM(location(in_driven_dof(:))))
    CALL read_driven_dof
    CLOSE(G_nfich)

  END SUBROUTINE read_in_driven_dof_poroMAILx

!!!---------------------------------------------------------
  SUBROUTINE write_out_driven_dof_poroMAILx

    IMPLICIT NONE

    INTEGER :: nfich

    nfich = get_io_unit()
    OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_driven_dof(:))))
    CALL write_driven_dof(nfich)
    CLOSE(nfich)

  END SUBROUTINE write_out_driven_dof_poroMAILx

!!!---------------------------------------------------------
  !> \brief Read a GPV file to initialize database
  subroutine read_in_meca_gpv_poroMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_gpv(:))))
    else if(step > 1) then
      open(unit=G_nfich,file=trim(location(out_gpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_gpv(:))))
    end if

    call read_in_meca_gpv
    close(G_nfich)

  end subroutine read_in_meca_gpv_poroMAILx

!!!---------------------------------------------------------
  !> \brief Read a GPV file to initialize database
  subroutine read_in_gpv_poroMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_gpv(:))))
    else if(step < 1) then
      open(unit=G_nfich,file=trim(location(out_gpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_gpv(:))))
    end if

    call read_in_gpv
    close(G_nfich)

  end subroutine read_in_gpv_poroMAILx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_dof_poroMAILx(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_dof)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_dof(1:lc)))) 
       CALL write_out_dof(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_dof)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_dof(1:lc)))) 
       CALL write_out_dof(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_dof(6,ifrom,ito)
    END SELECT

  END SUBROUTINE write_xxx_dof_poroMAILx
!!!------------------------------------------------------------------------   
  SUBROUTINE read_driven_dof

    IMPLICIT NONE
    
    INTEGER :: errare,itest  
    INTEGER :: ivd,ifd
    INTEGER :: ibdyty,inodty,dofnb,num_dof
    INTEGER :: iM_bdyty,iM_nodty
    CHARACTER(len=103) :: cout
    CHARACTER(len=5)   :: chnod
                              !123456789012345678901234567890
    CHARACTER(len=26)  :: IAM='poroMAILx::read_driven_dof'
  
    IF (nb_poroMAILx == 0) RETURN

    !
    ! initialisation
    !

    DO ibdyty=1,SIZE(bdyty)
       NULLIFY(bdyty(ibdyty)%poro_driven_dof, &
               bdyty(ibdyty)%Vdriv,bdyty(ibdyty)%Xdriv, &
               bdyty(ibdyty)%VdrivBeg, &
               bdyty(ibdyty)%drvdofs,bdyty(ibdyty)%drvvalues, &
               bdyty(ibdyty)%Tdriv, bdyty(ibdyty)%is_Tdriv_active)
       bdyty(ibdyty)%nb_poro_driven_dof=0

       NULLIFY(bdyty(ibdyty)%flux_driven_dof,bdyty(ibdyty)%Fdriv, &
               bdyty(ibdyty)%Fdriv,bdyty(ibdyty)%FdrivBeg)
       bdyty(ibdyty)%nb_flux_driven_dof=0

    END DO

    ! first reading: sizing array vlocy_driven_dof  

    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty'

       ivd=0
       ifd=0

       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'Problem reading bdyty')
       END IF
       itest=itest_bdyty_MAILx(G_clin)  
       IF (itest .NE. ifound) CYCLE

       !    we keep the body number   
       READ(G_clin(9:13),'(I5)') iM_bdyty    

       IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
       
          WRITE(cout,'(A12,I5,A30)')'body number ',iM_bdyty,' does not belong to collection'
          CALL FATERR(IAM,cout)
       END IF

       ibdyty = M2poro(iM_bdyty)%bdyty
       !if it's a body without MECAx behaviour

       IF (ibdyty == 0) CYCLE     

       DO
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'POROx') CYCLE                ! fishing the POROx part 

          DO
             IF ( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) == '     ') CYCLE
             IF (G_clin(2:6) == 'model' .OR. G_clin(2:6) == '$$$$$') EXIT 
             
             IF (G_clin(2:6) /= 'nodty') THEN                ! fishing for the keyword 'nodty' 
                CALL FATERR(IAM,'keyword nodty expected')
             END IF

             DO
                IF( .NOT. read_G_clin()) EXIT
                IF(G_clin(2:6) == '     ') CYCLE
                IF (.NOT. is_a_nodty(G_clin(2:6))) THEN
                   CALL FATERR(IAM,'Problem reading nodty')
                END IF

                chnod=G_clin(2:6)
          
                READ(G_clin(9:13),'(I5)') iM_nodty    

                IF (iM_nodty <= 0 .OR. iM_nodty > SIZE(M_bdyty(iM_bdyty)%nodty)) THEN 
                   WRITE(cout,'(A12,I5,A25,I5)') 'node number ',iM_nodty,' does not belong to body ',iM_bdyty
                   CALL FATERR(IAM,cout)
                ENDIF
                
                inodty=M2poro(iM_bdyty)%nodty(iM_nodty)
                
                !FD 27/09/2013 pour cette merde on n'en fait rien du chnod !!

                IF ( get_node_id_from_name(chnod) > &
                     nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                   IF ( get_node_id_from_name(chnod) - 1 > &
                        nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                      WRITE(cout,'(A6,A5,A49)') 'nodty ',chnod,' incompatible with the one belonging to the body '
                      CALL FATERR(IAM,cout)
                   ENDIF
                   SELECT CASE(chnod)
                      CASE('NO3xx')
                          chnod = 'NO2xx'
                      CASE('NO4xx')
                          chnod = 'NO3xx'
                   END SELECT
                END IF

                DO
                   IF( .NOT. read_G_clin()) EXIT
                   IF (G_clin(2:6) == '     ') CYCLE
                   IF (G_clin(2:6) /= 'dofty') THEN          ! fishing for the keyword 'dofty'
                      CALL FATERR(IAM,'keyword dofty expected')
                   END IF
                   !
                   DO
                      IF( .NOT. read_G_clin()) EXIT
                      SELECT CASE(G_clin(2:6))
                      CASE('vlocy') 

                         ivd=ivd+1

                         READ(G_clin( 9: 13),'(I5)') dofnb
                         
                         IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                            IF (dofnb <= 0 .OR. dofnb - 1 > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                                WRITE(cout,'(A11,I5,A26,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                                CALL FATERR(IAM,cout)
                            END IF
                            
                            ivd = ivd - 1
                         ENDIF
                      CASE('force') 
                         ifd=ifd+1
                         READ(G_clin( 9: 13),'(I5)') dofnb
                         
                         IF (dofnb <= 0 .OR. dofnb > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                            IF (dofnb <= 0 .OR. dofnb - 1 > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                                WRITE(cout,'(A11,I5,A26,I5)') 'dof number ',dofnb,' does not belong to bdyty ',ibdyty
                                CALL FATERR(IAM,cout)
                            END IF
                            
                            ifd = ifd - 1
                            
                         ENDIF
                   
                      CASE('     ')
                         
                      CASE default
                         EXIT
                      END SELECT
                   END DO
                   EXIT
                END DO
                EXIT
             END DO
             BACKSPACE(G_nfich)
          END DO
          BACKSPACE(G_nfich)
       END DO
       BACKSPACE(G_nfich)  
       
       bdyty(ibdyty)%nb_poro_driven_dof=ivd

       ALLOCATE(bdyty(ibdyty)%poro_driven_dof(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating vlocy_driven_dof')
       END IF
       
       ALLOCATE(bdyty(ibdyty)%Vdriv(ivd),bdyty(ibdyty)%VdrivBeg(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Vdriv')
       END IF

       ALLOCATE(bdyty(ibdyty)%Xdriv(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Xdriv')
       END IF
       
       ALLOCATE(bdyty(ibdyty)%drvdofs(ivd),bdyty(ibdyty)%drvvalues(ivd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating drvdofs/drvvalues')
       END IF
       
       IF (ivd == 0) THEN
          WRITE (cout,'(A,I0,A)') 'Warning: poroMAILx ',ibdyty,' without poro_driven_dof'
          CALL LOGMES(cout)   
       ELSE
         bdyty(ibdyty)%Vdriv(:)   =0.d0
         bdyty(ibdyty)%VdrivBeg(:)=0.d0
         bdyty(ibdyty)%drvdofs(:)  =0
         bdyty(ibdyty)%drvvalues(:)=0.d0
       ENDIF
       
       bdyty(ibdyty)%nb_flux_driven_dof=ifd
       
       ALLOCATE(bdyty(ibdyty)%flux_driven_dof(ifd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating force_driven_dof')
       END IF

       ALLOCATE(bdyty(ibdyty)%Fdriv(ifd),bdyty(ibdyty)%FdrivBeg(ifd),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Fdriv')
       END IF

       IF (ifd == 0) THEN
          WRITE (cout,'(A,I0,A)') 'Warning: poroMAILx ',ibdyty,' without force_driven_dof'
          CALL LOGMES(cout)
       ELSE
         bdyty(ibdyty)%FdrivBeg(:)=0.d0
         bdyty(ibdyty)%Fdriv(:)   =0.d0
       END IF
       
      
    END DO

   ! second reading: filling in data
    
    REWIND(G_nfich)

    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                  ! fishing for the keyword 'bdyty' 

       ivd=0
       ifd=0

       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'Problem reading bdyty')
       ENDIF
       itest=itest_bdyty_MAILx(G_clin)                      
       IF (itest /= ifound) CYCLE

       !    we keep the body number   
       READ(G_clin(9:13),'(I5)') iM_bdyty    
       ibdyty=M2poro(iM_bdyty)%bdyty
       
       DO    
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'POROx') CYCLE                ! fishing the MECAx part 

          DO
             IF ( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) == '     ') CYCLE
             IF (G_clin(2:6) == 'model' .OR. G_clin(2:6) == '$$$$$') EXIT 

             IF (G_clin(2:6) /= 'nodty') THEN                ! fishing for the keyword 'nodty' 
                CALL FATERR(IAM,'keyword nodty expected')
             END IF

             DO
                IF( .NOT. read_G_clin()) EXIT
                IF(G_clin(2:6) == '     ') CYCLE
                IF (.NOT. is_a_nodty(G_clin(2:6))) THEN
                   CALL FATERR(IAM,'Problem reading nodty')
                END IF

                chnod=G_clin(2:6)
          
                READ(G_clin(9:13),'(I5)') iM_nodty    

                inodty=M2poro(iM_bdyty)%nodty(iM_nodty)

                DO
                   IF( .NOT. read_G_clin()) EXIT
                   IF (G_clin(2:6) == '     ') CYCLE
                   IF (G_clin(2:6) /= 'dofty') THEN          ! fishing for the keyword 'dofty'
                      CALL FATERR(IAM,'keyword dofty expected')
                   ENDIF
                   !
                   DO
                      IF( .NOT. read_G_clin()) EXIT
                      SELECT CASE(G_clin(2:6))
                      CASE('vlocy') 
                         
                         READ(G_clin(9:13),'(I5)') num_dof
                         
                         IF (num_dof <= nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                           ivd=ivd+1
                         
                           NULLIFY(bdyty(ibdyty)%poro_driven_dof(ivd)%time_evolution%x, &
                                   bdyty(ibdyty)%poro_driven_dof(ivd)%time_evolution%fx)
                         
                           CALL read_a_driven_dof(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                                                  G_clin,bdyty(ibdyty)%poro_driven_dof(ivd))
                         ENDIF
                         
                      CASE('force') 
                         
                         READ(G_clin(9:13),'(I5)') num_dof
                         
                         IF (num_dof <= nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                            ifd=ifd+1
                         
                            NULLIFY(bdyty(ibdyty)%flux_driven_dof(ifd)%time_evolution%x, &
                              bdyty(ibdyty)%flux_driven_dof(ifd)%time_evolution%fx)
                         
                            CALL read_a_driven_dof(get_nodNAME(bdyty(ibdyty)%nodty(inodty)), &
                                                   inodty,G_clin,bdyty(ibdyty)%flux_driven_dof(ifd))
                         ENDIF
                      
                      CASE('     ')
                         
                      CASE default
                         EXIT
                      END SELECT
                   END DO
                   
                   EXIT
                END DO
                EXIT
             END DO
             BACKSPACE(G_nfich)
          END DO
          BACKSPACE(G_nfich)
       END DO
       BACKSPACE(G_nfich)
    
    !print *,'poro driven dof : ',bdyty(ibdyty)%nb_poro_driven_dof
    !print *,'flux driven dof : ',bdyty(ibdyty)%nb_flux_driven_dof
    
    END DO
  
  END SUBROUTINE read_driven_dof

!------------------------------------------------------------------------  
  SUBROUTINE write_driven_dof(nfich)

    IMPLICIT NONE

    INTEGER :: ivd,ifd,iivd,iifd,nfich
    INTEGER :: ibdyty,inodty,idof
    INTEGER :: iM_bdyty,iM_nodty

    IF (nb_poroMAILx== 0) RETURN

    DO ibdyty=1,SIZE(bdyty)

       IF (       bdyty(ibdyty)%nb_poro_driven_dof == 0 &
            .AND. bdyty(ibdyty)%nb_flux_driven_dof == 0 ) CYCLE

       ! the ibdyty body has some driven dof
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)    'MAILx',bdyty2M_bdyty(ibdyty)
       WRITE(nfich,'(A6)') '$model'
       WRITE(nfich,'(A6)') ' POROx'

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iivd=0
          iifd=0
          DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof
             IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%poro_driven_dof(ivd))) THEN
                iivd=ivd
                EXIT
             END IF
          END DO
          DO ifd=1,bdyty(ibdyty)%nb_flux_driven_dof
             IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%flux_driven_dof(ifd))) THEN
                iifd=ifd
                EXIT
             END IF
          END DO

          IF (iivd > 0 .OR. iifd > 0) THEN
             ! the ibdyty body with inodty node has driven dof
             WRITE(nfich,'(A6)') '$nodty'
             iM_bdyty=bdyty2M_bdyty(ibdyty)
             iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
             !
             WRITE(nfich,101) get_nodNAME(M_bdyty(iM_bdyty)%nodty(iM_nodty)),iM_nodty
             ! write some 
             CALL write_a_driven_dof(nfich)

             DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof
                IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%poro_driven_dof(ivd))) THEN
                   CALL write_a_driven_dof(nfich,'vlocy',bdyty(ibdyty)%poro_driven_dof(ivd))
                END IF
             END DO
             DO ifd=1,bdyty(ibdyty)%nb_flux_driven_dof
                IF (is_a_driven_dof_of_node(inodty,bdyty(ibdyty)%flux_driven_dof(ifd))) THEN
                   CALL write_a_driven_dof(nfich,'force',bdyty(ibdyty)%flux_driven_dof(ifd))
                END IF
             END DO
          END IF
       END DO
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
    END DO
    
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
    CLOSE(nfich) 
      
101 FORMAT(1X,A5,2X,I5)    
 
END SUBROUTINE write_driven_dof

!!!------------------------------------------------------------------------   
  SUBROUTINE read_in_gpv

    IMPLICIT NONE

    INTEGER          :: ibdyty,iblmty,itest
    INTEGER          :: iM_bdyty,iM_blmty
    INTEGER          :: errare,mdlnb,nb_external,nb_internal,ig,i
                              !123456789012345678901234567890    
    CHARACTER(len=22)  :: IAM='poroMAILx::read_in_gpv'
    CHARACTER(len=103) :: cout
    CHARACTER(len=5)   :: ctest


    IF (nb_poroMAILx == 0) RETURN
    
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest == ifound) THEN
          READ(G_clin(7:13),'(I7)') iM_bdyty
          IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
             WRITE(cout,'(A12,I5,A60)') 'body number ',iM_bdyty,' does not belong to collection'
             CALL FATERR(IAM,cout)
          END IF
       ELSE
          CYCLE
       END IF

       DO
          IF( .NOT. read_G_clin()) EXIT           
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE                 ! fishing for the keyword 'blmty'

          IF( .NOT. read_G_clin()) EXIT

          !fd a voir l'utilisation de test_blmty de MAILx
!!!mr?
          READ(G_clin(7:13),'(I7)') iM_blmty     
          SELECT CASE(G_clin(2:6))
          CASE('T6xxx','Q8xxx','H20xx','TE10x')
             ctest = 'found'
          CASE('     ')
             ctest = 'sskip'
             ! derriere un blmty maille on a forcement un nodty !
          CASE('nodty')
             ctest = 'nomor'
          CASE default
             WRITE(cout,'(A7,A5,A34)')' blmty ',G_clin(2:6),' unknown in read_in_gpv '
             call faterr(IAM,cout)
          END SELECT

          ! fd recherche du model ...

          DO    
             IF ( .NOT. read_G_clin()) EXIT 
             IF (G_clin(2:6) == '$$$$$') THEN
                BACKSPACE(G_nfich)
                EXIT
             ENDIF
             IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
             IF ( .NOT. read_G_clin()) THEN
                CALL FATERR(IAM,'Problem reading model')
             ENDIF
             
             IF (G_clin(2:6) /= 'POROx') CYCLE                ! fishing the MECAx part 
             
             
             ibdyty = M2poro(iM_bdyty)%bdyty
             iblmty = M2poro(iM_bdyty)%blmty(iM_blmty)
             
             mdlnb = bdyty(ibdyty)%blmty(iblmty)%mdlnb
             
             nb_external=modelz(mdlnb)%nb_external_variables
             nb_internal=modelz(mdlnb)%nb_internal_variables
             
             IF (nb_external == 0) THEN
                write(cout,'(A)') 'Material without external variable !'
                write(cout,'(A)') 'Check DATBOX/BODIES.DAT and/or DATBOX/MODELS.DAT'
                call faterr(IAM,cout)
             ENDIF
             
             
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca,dim=1)
                
                IF ( .NOT. read_G_clin()) CALL FATERR(IAM,'Problem reading poro_gpv values')
                
                ! gerer le bordel sur plusieurs lignes
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress(i)       
                ENDDO
                
                M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress = &
                     M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress
                
                IF ( .NOT. read_G_clin()) CALL FATERR(IAM,'Problem reading poro_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain(i)       
                ENDDO

                M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain = &
                     M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain
                     
                IF (nb_internal /= 0 ) THEN
                   IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading meca_poro_gpv values')
                   
                   DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal)
                      READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal(i)
                   ENDDO
                   
                   M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal = &
                        M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal       
                ENDIF

             ENDDO
                          
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther,dim=1)
                
                IF ( .NOT. read_G_clin()) CALL FATERR(IAM,'Problem reading poro_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad(i)       
                ENDDO
                                
                M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%grad = &
                     M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%grad
                
                IF ( .NOT. read_G_clin()) CALL FATERR(IAM,'Problem reading poro_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux(i)       
                ENDDO

!~                 PRINT*,'TATA4'

                M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%flux = &
                     M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%flux
                
                ! DA : grosse grouille entre nb_internal de meca gpv et nb internal ther gpv c'est une merde !!!!
!~                 IF (nb_internal /= 0 ) THEN
                
!~                    PRINT*,'TATA1'
                
!~                    IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading meca_poro_gpv values')
                   
!~                    PRINT*,'TATA2'
                   
!~                    DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal)
!~                       READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal(i)
!~                    ENDDO
!~                    
!~                    PRINT*,'TATA5'
                   
!~                    M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,1)%internal = &
!~                         M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_ther(ig,2)%internal       
!~                 ENDIF
                
             ENDDO
             
!~              PRINT*,'TATA2'
             
             EXIT
          ENDDO ! $model
          CYCLE
       ENDDO !blmty
       CYCLE
    END DO ! $bdyty
   
  END SUBROUTINE read_in_gpv
!!!------------------------------------------------------------------------   
  SUBROUTINE read_in_meca_gpv

    IMPLICIT NONE

    INTEGER          :: ibdyty,iblmty,itest
    INTEGER          :: iM_bdyty,iM_blmty
    INTEGER          :: errare,mdlnb,nb_external,nb_internal,ig,i
                              !123456789012345678901234567   
    CHARACTER(len=27)  :: IAM='poroMAILx::read_in_meca_gpv'
    CHARACTER(len=103) :: cout
    CHARACTER(len=5)   :: ctest

    IF (nb_poroMAILx == 0) RETURN
    
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest = itest_bdyty_MAILx(G_clin)                      
       IF (itest == ifound) THEN
          READ(G_clin(7:13),'(I7)') iM_bdyty
          IF (iM_bdyty <= 0 .OR. iM_bdyty > SIZE(M_bdyty)) THEN
             WRITE(cout,'(A12,I5,A60)') 'body number ',iM_bdyty,' does not belong to collection'
             CALL FATERR(IAM,cout)
          END IF
       ELSE
          CYCLE
       END IF

       DO
          IF( .NOT. read_G_clin()) EXIT           
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'blmty') CYCLE                 ! fishing for the keyword 'blmty'

          IF( .NOT. read_G_clin()) EXIT

          !fd a voir l'utilisation de test_blmty de MAILx
!!!mr?
          READ(G_clin(7:13),'(I7)') iM_blmty     
          SELECT CASE(G_clin(2:6))
          CASE('T3xxx','Q4xxx','Q8xxx','H8xxx','H20xx','TE4xx')
             ctest = 'found'
          CASE('     ')
             ctest = 'sskip'
             ! derriere un blmty maille on a forcement un nodty !
          CASE('nodty')
             ctest = 'nomor'
          CASE default
             write(cout,'(A7,A5,A34)')' blmty ',G_clin(2:6),' unknown in read_in_meca_gpv '
             call faterr(IAM,cout)
          END SELECT

          ! fd recherche du model ...

          DO    
             IF ( .NOT. read_G_clin()) EXIT 
             IF (G_clin(2:6) == '$$$$$') THEN
                BACKSPACE(G_nfich)
                EXIT
             ENDIF
             IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
             IF ( .NOT. read_G_clin()) THEN
                CALL FATERR(IAM,'Problem reading model')
             ENDIF
             
             IF (G_clin(2:6) /= 'MECAx') CYCLE                ! fishing the MECAx part 
             
             
             ibdyty = M2poro(iM_bdyty)%bdyty
             iblmty = M2poro(iM_bdyty)%blmty(iM_blmty)
             
             mdlnb = bdyty(ibdyty)%blmty(iblmty)%mdlnb
             
             nb_external=modelz(mdlnb)%nb_external_variables
             nb_internal=modelz(mdlnb)%nb_internal_variables
             
             IF (nb_external == 0) THEN
                write(cout,'(A)') 'Material without external variable !'
                write(cout,'(A)') 'Check DATBOX/BODIES.DAT and/or DATBOX/MODELS.DAT'
                call faterr(IAM,cout)
             ENDIF
             
             DO ig=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca,dim=1)
                
                IF ( .NOT. read_G_clin()) CALL FATERR(IAM,'Problem reading meca_poro_gpv values')

                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress(i)    
                ENDDO
                
                M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%stress = &
                     M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%stress
                
                IF ( .NOT. read_G_clin()) CALL FATERR(IAM,'Problem reading meca_poro_gpv values')
                
                DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain)
                   READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain(i)
                ENDDO
                
                M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%strain = &
                     M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%strain
                     
      
                IF (nb_internal /= 0 ) THEN
                   IF ( .NOT. read_G_clin()) CALL LOGMES('Error '//IAM//': Problem reading meca_poro_gpv values')
                   
                   DO i=1,SIZE(M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal)
                      READ(G_clin((i-1)*14+1:i*14),'(D14.7)') M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal(i)
                   ENDDO
                   
                   M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,1)%internal = &
                        M_bdyty(ibdyty)%blmty(iblmty)%poro_gpv_meca(ig,2)%internal       
                ENDIF
                    
             ENDDO
             
             EXIT
          ENDDO ! $model
          CYCLE
       ENDDO !blmty
       CYCLE
    END DO ! $bdyty
   
  END SUBROUTINE read_in_meca_gpv
!!!------------------------------------------------------------------------
  SUBROUTINE write_out_dof(nfich,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: ifrom,ito
    INTEGER :: ibdyty,inodty,iccdof,nfich,nbdof
    INTEGER :: lc 
    real(kind=8) :: x(6)
    CHARACTER(len=5) :: chnod
   
    IF (nb_poroMAILx == 0) RETURN
    !!   write(66,'(2(1x,D20.13)') TPS,bdyty(1)%T(1)-293.

    DO ibdyty = ifrom,ito
       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101)'MAILx',ibdyty
       WRITE(nfich,'(A6)') '$model'
       WRITE(nfich,'(A6)') ' POROx'
       WRITE(nfich,'(A6)') '$nodty'

       DO inodty = 1,SIZE(bdyty(ibdyty)%nodty) 
          
          nbdof  = nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          iccdof = bdyty(ibdyty)%ccdof(inodty)
          
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof), &
                             'X  ',nfich)

          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof), &
                             'V  ',nfich)
          
                  
          
       END DO
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
    END DO
    !                     123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
101 FORMAT(1X,A5,2X,I5)            
    
  END SUBROUTINE write_out_dof
!!!---------------------------------------------------------
!!!-------- Routine initialisation du modele
!!!---------------------------------------------------------
  SUBROUTINE load_models_poroMAILx

    IMPLICIT NONE

    INTEGER :: nb_MAILx
    INTEGER :: iM_bdyty,iM_blmty,iM_model,inodty
    INTEGER :: ibdyty,iblmty,imodel,iM_nodty
    INTEGER :: errare,itest,imdlnb,iccdof,idof,iG_i,itempo,idof_meca, idof_ther
!    INTEGER :: itempo,bw

    INTEGER :: nb_external,nb_internal
    
!fd 22/04/08 external fields
    INTEGER :: IF,nbf,nb_ef,nb_bf
    CHARACTER(len=30),DIMENSION(:),ALLOCATABLE :: field_name
    !
    integer(kind=4) :: nb_evf,vsize
    character(len=30), dimension(:), allocatable :: vfield_name


    CHARACTER(len=5) :: ctempo
    CHARACTER(len=103) :: cout
                              !1234567890123456789012
    CHARACTER(len=22)  :: IAM='poroMAILx::load_models'

    INTEGER,DIMENSION(:),ALLOCATABLE :: edof2gdof 

    integer :: i,p_inodty,bw,nbdof_meca

    integer(kind=4), dimension(:), allocatable :: perm,inv_perm,i4_vector
    type(T_link_connec),               pointer ::connectivities, tmp
    
    integer(kind=4) ::  max_nod2el, max_dofs_adj, max_conn

    connectivities => null()
    
    ! 0 initialisations

    ! initialisation de la liste des poroEF_xxx disponibles
    CALL init_poroEF

    ! initialisation de la map MAILx -> poroMAILx
    nb_MAILx=get_nb_MAILx()

    ALLOCATE(M2poro(nb_MAILx),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating M2poro')
    END IF
  
    DO iM_bdyty=1,nb_MAILx
       M2poro(iM_bdyty)%bdyty=0
       NULLIFY(M2poro(iM_bdyty)%nodty)
       NULLIFY(M2poro(iM_bdyty)%blmty)
    END DO

    ! choose a default value for matrix_storage if not set
    if( Matrix_storage < 0 ) then
      if( nbDIME == 3 ) then
        if( sparse_storage_available ) then
          Matrix_storage = i_sparse
        else
          Matrix_storage = i_band
        end if
      else
        Matrix_storage = i_band
      end if
    end if
    
    ! first reading: sizing array of models  

    itest = 0
    
    if( .not. allocated(modelz) .or. size(modelz) < 1 ) then
        call faterr(IAM,'please call ReadModels before trying to LoadModels')
    end if

    DO imodel=1,SIZE(modelz)
       IF (modelz(imodel)%mdlty == 'POROx') itest = itest + 1 
    END DO
    
    WRITE(cout,'(I0,1x,A)') itest,'POROx models declared'
    CALL LOGMES(cout)
    
    ! then constructing the poro EF database 
    
    ! first scaning of the MAILx array looking for POROx models 
    ! database determining the size of bdyty
    
    if( .not. allocated(M_bdyty) ) then
        call faterr(IAM,'please call ReadBodies before trying to LoadModels')
    end if
    ibdyty=0
    
    DO iM_bdyty=1,SIZE(M_bdyty)
       itest=0
       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'POROx') itest=1
                END IF
             END DO
             IF (itest == 1) EXIT
          END DO
          IF (itest == 1) EXIT
       END DO
       IF (itest == 1) ibdyty =ibdyty + 1 
    END DO
    
    nb_poroMAILx = ibdyty

    IF (ibdyty == 0) THEN
       CALL LOGMES('no POROx BODIES found')
       CALL LOGMES('if any check BODIES.DAT or MODELS.DAT')
    ELSE
      WRITE(cout,'(I0,1x,A)') nb_poroMAILx,'POROx BODIES found'
      CALL LOGMES(cout)
    END IF
    
    CALL LOGMES('--')

    IF (ibdyty == 0) RETURN   

    ALLOCATE(bdyty(ibdyty),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating bdyty')
    END IF

    ! la map poroMAILx -> MAILx
    ALLOCATE(bdyty2M_bdyty(ibdyty),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating bdyty2M_bdyty')
    END IF

    ! second scaning of the MAILx looking for POROx models
    ! filling the maps bdyty2M_bdyty and M2poro(iM_bdyty)%bdyty
    ! sizing bdyty(ibdyty)%blmty, bdyty(ibdyty)%blmty2M_blmty  

    ibdyty=0

    DO iM_bdyty=1,SIZE(M_bdyty)
       itest  = 0
       iblmty = 0

       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'POROx') THEN 
                      IF (itest == 0) THEN
                         ibdyty =ibdyty + 1
                         bdyty2M_bdyty(ibdyty)=iM_bdyty              
                         M2poro(iM_bdyty)%bdyty=ibdyty
                         itest=1
                      END IF
                      iblmty =iblmty + 1
                   END IF
                END IF
             END DO
          END DO
       END DO
       IF (itest /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty')
          END IF

          ALLOCATE(bdyty(ibdyty)%blmty2M_blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty2M_blmty')
          END IF

          ALLOCATE(M2poro(iM_bdyty)%blmty(SIZE(M_bdyty(iM_bdyty)%blmty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M2poro%blmty')
          END IF
          M2poro(iM_bdyty)%blmty=0

          ALLOCATE(M2poro(iM_bdyty)%nodty(SIZE(M_bdyty(iM_bdyty)%nodty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M2poro%nodty')
          END IF
          M2poro(iM_bdyty)%nodty=0

       END IF
    END DO
    
    ! third scaning of the MAILx database: 
    ! filling the components of bdyty

    if( get_nb_bulk_behav() < 1 ) then
        call faterr(IAM,'Please call ReadBehaviours before LoadModels')
    end if

    DO ibdyty=1,SIZE(bdyty)

       iM_bdyty=bdyty2M_bdyty(ibdyty)

       iblmty=0

       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'POROx') THEN 
                      iblmty =iblmty + 1

                      bdyty(ibdyty)%blmty2M_blmty(iblmty)=iM_blmty
                      M2poro(iM_bdyty)%blmty(iM_blmty)=iblmty 

                      ! allocation de la table de connectivite
                      inodty=SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%NODES(inodty),stat=errare)
                      
       
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%NODES')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%NODES(:)=0

                      bdyty(ibdyty)%blmty(iblmty)%blmnb=get_nb_in_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%mdlnb=imodel
!xxx fd test 30/01/09 
!xxx on court circuite le load behaviours

                      bdyty(ibdyty)%blmty(iblmty)%lawnb = &
                      get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
!xxx
                      
                      
                      
!!!              construction de la table de correspondance entre ddl locaux et ddl globaux
!!!              et des matrices et vecteurs elementaires
!
!!! est ce valide !!!!
!!! il faudrait verifier que le nombre de noeuds lus et dans le type sont les memes ! 
!!!                   

                      bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node = get_N_DOF_by_NODE_MECA_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node = get_N_DOF_by_NODE_THER_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%meca_node = get_N_NODE_MECA_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%ther_node = get_N_NODE_THER_poroEF(modelz(imodel)%ID)
                      
                      idof_meca = bdyty(ibdyty)%blmty(iblmty)%meca_node*bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
                      idof_ther = bdyty(ibdyty)%blmty(iblmty)%ther_node*bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node
                      idof = idof_meca + idof_ther 
                      
                      bdyty(ibdyty)%blmty(iblmty)%ndof_meca = idof_meca
                      bdyty(ibdyty)%blmty(iblmty)%ndof_ther = idof_ther
                      bdyty(ibdyty)%blmty(iblmty)%ndof = idof
                      
                      !! Creation du nombre de dof par noeud
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node(idof),stat=errare)
                      DO i = 1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                         bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node(i) = get_N_DOF_by_NODE_poroEF(modelz(imodel)%ID,i)
                      ENDDO
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%meca2poro(idof_meca),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%meca2poro')
                      END IF
                      DO i = 1,SIZE(bdyty(ibdyty)%blmty(iblmty)%meca2poro)
                          bdyty(ibdyty)%blmty(iblmty)%meca2poro(i)= get_MECA_to_PORO(modelz(imodel)%ID, i)
                      ENDDO
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ther2poro(idof_ther),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%ther2poro')
                      END IF
                      DO i = 1,SIZE(bdyty(ibdyty)%blmty(iblmty)%ther2poro)
                          bdyty(ibdyty)%blmty(iblmty)%ther2poro(i)= get_THER_to_PORO(modelz(imodel)%ID, i)
                      ENDDO
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%edof2gdof(idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%edof2gdof')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%edof2gdof(:)=0
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%stiffness(idof,idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%stiffness')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%stiffness(:,:)=0.d0
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%mass(idof,idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%mass')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%mass(:,:)=0.d0
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%damping(idof,idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%damping')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%damping(:,:)=0.d0
                      
                      !ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%KTloc(idof,idof),stat=errare)
                      !IF (errare /= 0) THEN
                      !   CALL FATERR(IAM,'error allocating bdyty%blmty%KT_loc')
                      !END IF
                      !bdyty(ibdyty)%blmty(iblmty)%KTloc(:,:)=0.d0
                      !
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%Fext(idof,2),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%Fext')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%Fext(:,:)=0.d0
                      !
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%Fint(idof,2),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%Fint')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%Fint(:,:)=0.d0
                      !
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ttFint(idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%ttFint')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%ttFint(:)=0.d0
                      !
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ttFext(idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%ttFext')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%ttFext(:)=0.d0
                      !
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%RHSloc(idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%RHSloc')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%RHSloc(:)=0.d0

                      !!!  construction du vecteur contenant les valeurs aux points de Gauss

                      nb_external=modelz(imodel)%nb_external_variables
                      nb_internal=modelz(imodel)%nb_internal_variables

                      !fd 22/04/08, 28/01/09 
                      ! gestion des fields: external (model) & bulk
                      ! on rapatrie tous les fields declares et on s'en sert pour initialiser les champs aux pg 
                      ! on donne au field le rang dans la pile de field stocke au pg

                      nb_ef = get_external_field_nb(imodel) 
                      nb_bf = get_bulk_field_nb(bdyty(ibdyty)%blmty(iblmty)%lawnb)
                      nbf = nb_ef + nb_bf

                      nb_evf = get_external_nb_vfield(imodel) 

                      IF (nbf /= 0) THEN
                        ALLOCATE(field_name(nbf))
                        DO IF=1,nb_ef
                           field_name(IF) = get_external_field_name(imodel,IF)
                        ENDDO                      
                        DO IF=1,nb_bf
                           field_name(nb_ef+IF) = get_bulk_field_name(imodel,IF)
                        ENDDO                      
                      END IF

                      if (nb_evf /= 0 ) then
                        allocate(vfield_name(nb_evf))
                        do IF = 1,nb_evf
                          vfield_name(IF) = get_external_vfield_name(imodel,IF)
                        end do
                        vsize = get_external_vfield_max_size(imodel)
                      end if

                      if( nbf/=0 .and. nb_evf/=0 ) then

                        call init_porogpv_MAILx(iM_bdyty,iM_blmty, & 
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'MECA'), &
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'THER'), &
                                                nb_external,nb_internal, &
                                                nbf,field_name,nb_evf,vfield_name,vsize)

                        deallocate(field_name,vfield_name)

                        else if( nbf/=0 ) then
                          call init_porogpv_MAILx(iM_bdyty,iM_blmty, & 
                                                  get_N_GP_poroEF(modelz(imodel)%ID, 'MECA'), &
                                                  get_N_GP_poroEF(modelz(imodel)%ID, 'THER'), &
                                                  nb_external,nb_internal, &
                                                  nbf,field_name)

                          deallocate(field_name)

                        else if( nb_evf/=0 ) then
                          call init_porogpv_MAILx(iM_bdyty,iM_blmty, & 
                                                  get_N_GP_poroEF(modelz(imodel)%ID, 'MECA'), &
                                                  get_N_GP_poroEF(modelz(imodel)%ID, 'THER'), &
                                                  nb_external,nb_internal, &
                                                  nb_vfields=nb_evf,vfield_name=vfield_name,vsize=vsize)

                          deallocate(vfield_name)

                      else

                        call init_porogpv_MAILx(iM_bdyty,iM_blmty, & 
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'MECA'), &
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'THER'), &
                                                nb_external,nb_internal)
                      ENDIF

                   END IF
                END IF
             END DO
          END DO
       END DO

       IF (iblmty == 0) THEN
          CALL FATERR(IAM,'no blmty')
       END IF
    END DO


    ! perform some temporary computations
    ! we look for the nodes owing to poroMAILx
    ! 
    ! first we scan the MAILx database and we count the 
    ! nodes owing to a POROx element 
    !
    ! second we fill the bdyty ... database and we determine
    ! the nodty of a node which is the highest one
    !
    
    DO ibdyty=1,SIZE(bdyty)

       iM_bdyty=bdyty2M_bdyty(ibdyty)
       inodty=0

       DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
          DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
             iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)
             IF (M2poro(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
                inodty = inodty+1
                EXIT
             END IF
          END DO
       END DO
       IF (inodty /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%nodty')
          END IF

          ALLOCATE(bdyty(ibdyty)%nodty2M_nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%nodty2M_nodty')
          END IF

          bdyty(ibdyty)%nb_nodes = inodty

          DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
             call new_nodty(bdyty(ibdyty)%nodty(inodty),'     ')
             bdyty(ibdyty)%nodty2M_nodty(inodty)=0
          END DO
       ELSE
          CALL FATERR(IAM,'error computing size of bdyty%nodty')
       END IF

       inodty=0
       DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
          itest=0
          DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
             iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)      
             IF (M2poro(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
                IF (itest == 0) THEN
                   inodty = inodty+1
                   bdyty(ibdyty)%nodty2M_nodty(inodty)=iM_nodty
                   M2poro(iM_bdyty)%nodty(iM_nodty)=inodty
                   itest=1
                END IF
                ! a la peche au type de l'element ...
                iblmty=M2poro(iM_bdyty)%blmty(iM_blmty)
                imodel=bdyty(ibdyty)%blmty(iblmty)%mdlnb
                ! a la peche au type de noeuds de l'element
                DO i = 1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                   itempo=get_n_dof_by_node_poroEF(modelz(imodel)%ID, i)
                   IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(i) ==  inodty) THEN
                      ! Je suis bien sur le noeuds
                      call new_nodty(bdyty(ibdyty)%nodty(inodty),get_node_name_from_id(itempo))
                   ENDIF
                ENDDO
             END IF
          END DO
       END DO
    END DO

    DO iM_bdyty=1,SIZE(M2poro)
       IF (M2poro(iM_bdyty)%bdyty /=0) THEN
          DO iM_blmty=1,SIZE(M2poro(iM_bdyty)%blmty)
             IF (M2poro(iM_bdyty)%blmty(iM_blmty) /=0) THEN
                ibdyty=M2poro(iM_bdyty)%bdyty
                iblmty=M2poro(iM_bdyty)%blmty(iM_blmty)
                DO inodty=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
                   iM_nodty=M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)
                   bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)=M2poro(iM_bdyty)%nodty(iM_nodty)
                END DO
             END IF
          END DO
       END IF
    END DO

    DO ibdyty=1,SIZE(bdyty)

       iccdof = 0

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iccdof=iccdof + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       END DO

       bdyty(ibdyty)%nbdof=iccdof
       IF (iccdof /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%nodnb(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%dofnb(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vbegin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Xbegin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%V(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vlast(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%X(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vfree(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vaux(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%residu(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Fext(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Fint(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Finert(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Ireac(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Iaux(iccdof),stat=errare)
          ! Gestion des vitesses Methode ALE
          ALLOCATE(bdyty(ibdyty)%V_ALE_begin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%V_ALE(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Mask_ALE(bdyty(ibdyty)%nbdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Mask_No_ALE(bdyty(ibdyty)%nbdof),stat=errare)
          
          ALLOCATE(bdyty(ibdyty)%periodicnode(iccdof),stat=errare)

          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating X,V')
          END IF
          
          bdyty(ibdyty)%nodnb=0
          bdyty(ibdyty)%dofnb=0
          
          bdyty(ibdyty)%Vbegin=0.d0
          bdyty(ibdyty)%V     =0.d0
          bdyty(ibdyty)%Vlast =0.d0
          bdyty(ibdyty)%Xbegin=0.d0
          bdyty(ibdyty)%X     =0.d0
          bdyty(ibdyty)%Vfree =0.D0
          bdyty(ibdyty)%Vaux  =0.d0
          bdyty(ibdyty)%residu=0.d0
          bdyty(ibdyty)%Fext  =0.d0 
          bdyty(ibdyty)%Fint  =0.d0
          bdyty(ibdyty)%Finert=0.d0
          bdyty(ibdyty)%Ireac =0.d0
          bdyty(ibdyty)%Iaux  =0.d0
          ! Gestion des vitesses Methode ALE
          bdyty(ibdyty)%V_ALE_begin=0.d0
          bdyty(ibdyty)%V_ALE      =0.d0
          bdyty(ibdyty)%Mask_ALE   =0
          bdyty(ibdyty)%Mask_No_ALE=1
          

          bdyty(ibdyty)%is_precon=.FALSE.

          bdyty(ibdyty)%visible     = .TRUE.
          bdyty(ibdyty)%is_periodic = .FALSE.

          bdyty(ibdyty)%periodicnode = 0
          
          ALLOCATE(bdyty(ibdyty)%coorTT(nbdime,SIZE(bdyty(ibdyty)%nodty)),stat=errare)
          bdyty(ibdyty)%coorTT=0.d0
          ALLOCATE(bdyty(ibdyty)%RcoorTT(nbdime),stat=errare)
          bdyty(ibdyty)%RcoorTT=0.d0

       ELSE 

          NULLIFY(bdyty(ibdyty)%nodnb)
          NULLIFY(bdyty(ibdyty)%dofnb)
          NULLIFY(bdyty(ibdyty)%Vbegin)
          NULLIFY(bdyty(ibdyty)%Xbegin)
          NULLIFY(bdyty(ibdyty)%V)
          NULLIFY(bdyty(ibdyty)%Vlast)
          NULLIFY(bdyty(ibdyty)%X)
          NULLIFY(bdyty(ibdyty)%Vfree)
          NULLIFY(bdyty(ibdyty)%Vaux)
          NULLIFY(bdyty(ibdyty)%residu)
          NULLIFY(bdyty(ibdyty)%Fext)
          NULLIFY(bdyty(ibdyty)%Fint)
          NULLIFY(bdyty(ibdyty)%Finert)
          NULLIFY(bdyty(ibdyty)%Ireac)
          NULLIFY(bdyty(ibdyty)%Iaux)
          
          NULLIFY(bdyty(ibdyty)%V_ALE_begin)
          NULLIFY(bdyty(ibdyty)%V_ALE)
          NULLIFY(bdyty(ibdyty)%Mask_ALE)
          NULLIFY(bdyty(ibdyty)%Mask_No_ALE)
          
          NULLIFY(bdyty(ibdyty)%periodicnode)

          bdyty(ibdyty)%is_precon=.FALSE.          

          bdyty(ibdyty)%visible     = .TRUE.
          bdyty(ibdyty)%is_periodic = .FALSE.

          NULLIFY(bdyty(ibdyty)%coorTT)
          NULLIFY(bdyty(ibdyty)%RcoorTT)

          WRITE(cout,'(A25,I5)') 'Warning DISKx without DOF',ibdyty 
          CALL LOGMES(cout)
       END IF

!    array node -> first global ddl

       IF (SIZE(bdyty(ibdyty)%nodty) /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%ccdof(SIZE(bdyty(ibdyty)%nodty)+1),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating ccdof in read_models')
          END IF
       ELSE 
          NULLIFY(bdyty(ibdyty)%ccdof)
          PRINT*,'Warning MAILx without node',ibdyty 
       END IF
    END DO

!  second: filling ordering arrays, and element local 2 global dof correspondance
    
    DO ibdyty=1,SIZE(bdyty)

       iccdof=0

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          bdyty(ibdyty)%ccdof(inodty)=iccdof
          DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
             iccdof=iccdof+1
             bdyty(ibdyty)%nodnb(iccdof)=inodty       ! reverse mapping
             bdyty(ibdyty)%dofnb(iccdof)=idof         ! reverse mapping
          END DO
       END DO
       
       bdyty(ibdyty)%ccdof(size(bdyty(ibdyty)%nodty)+1) = iccdof
       
       max_conn = 0
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          iccdof=0
          max_conn = max(max_conn, size(bdyty(ibdyty)%blmty(iblmty)%NODES))
          DO itempo=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
             DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                iccdof=iccdof+1
                bdyty(ibdyty)%blmty(iblmty)%edof2gdof(iccdof)=bdyty(ibdyty)%ccdof(inodty)+idof
             END DO
          END DO
       END DO
      !print *,'nodnb',bdyty(ibdyty)%nodnb
      !print *,'dofnb',bdyty(ibdyty)%dofnb
      !print *,'ccdof',bdyty(ibdyty)%ccdof
      !fd gestion de la renumerotation des noeuds
      ! on cree d'abord un tableau de permutation des noeuds
      ! ensuite on construit une map des ddl

      !! connectivite      
      connectivities => get_ll_connectivity_poroMAILx(ibdyty)

      max_nod2el = get_max_nod2el(bdyty2M_bdyty(ibdyty))
      max_dofs_adj = max_nod2el * max_conn * (nbDIME+1)
      
      call initialize_system(bdyty(ibdyty)%g_sys,Matrix_storage,Matrix_shape,bdyty(ibdyty)%ccdof,connectivities,max_dofs_adj)

      do while( associated(connectivities) )
        tmp => connectivities%n
        deallocate(connectivities%connec)
        deallocate(connectivities)
        connectivities => tmp
      end do

    END DO
   
    IF (itchache) THEN

       PRINT*,'Nombre de corps poroMAILx:',SIZE(bdyty)
       DO ibdyty=1,SIZE(bdyty)
          PRINT*,'==========================================='
          PRINT*,'Corps: ',ibdyty
          PRINT*,'Correspondance dans la base Mailx: ',bdyty2M_bdyty(ibdyty)
          PRINT*,'PARANOIAC TEST local->global->local',M2poro(bdyty2M_bdyty(ibdyty))%bdyty
          PRINT*,'**nodty************'
          DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
             PRINT*,'Noeud: ',inodty
             PRINT*,'Type de noeud: ',get_nodNAME(bdyty(ibdyty)%nodty(inodty))
             PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%nodty2M_nodty(inodty)
             PRINT*,'ddl s commencant a: ',bdyty(ibdyty)%ccdof(inodty),'nombre: ',nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
             PRINT*,'PARANOIAC TEST local->global->local',M2poro(bdyty2M_bdyty(ibdyty))%nodty(bdyty(ibdyty)%nodty2M_nodty(inodty))
          ENDDO
          PRINT*,'**blmty************'
          DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
             PRINT*,'Element: ',iblmty
             PRINT*,'Connectivite:',bdyty(ibdyty)%blmty(iblmty)%NODES(:)
             imdlnb=bdyty(ibdyty)%blmty(iblmty)%mdlnb
             PRINT*,'Num de l element dans la liste poroEF: ',bdyty(ibdyty)%blmty(iblmty)%blmnb
             PRINT*,'ID du modele: ',modelz(imdlnb)%ID,' Num de modele: ',imdlnb 
             PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%blmty2M_blmty(iblmty)
          ENDDO
       ENDDO
       

       PRINT*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

!
    END IF


    DO ibdyty=1,SIZE(bdyty)

      ALLOCATE(bdyty(ibdyty)%RHS(bdyty(ibdyty)%nbdof),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating bdyty%RHS')
      END IF
      bdyty(ibdyty)%RHS = 0.d0
            
      ! Gestion de la partie ALE
      
      bdyty(ibdyty)%nbdof_meca = bdyty(ibdyty)%nb_nodes * nbDIME
      
    END DO
    
    CALL load_Display_P_poroMAILx

  END SUBROUTINE load_models_poroMAILx
  
!!!---------------------------------------------------------
  SUBROUTINE write_xxx_Rnod_poroMAILx(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc)))) 
       CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc)))) 
       CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_Rnod(6,ifrom,ito)
    END SELECT
    
  END SUBROUTINE write_xxx_Rnod_poroMAILx
!!!---------------------------------------------------------
  SUBROUTINE write_xxx_nodforces(which,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: which,ifrom,ito
    INTEGER :: nfich,lc 

    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc)))) 
       CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Rnod)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc)))) 
       CALL write_out_Rnod(nfich,ifrom,ito)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_nodforces(6,ifrom,ito)
    END SELECT
    
  END SUBROUTINE write_xxx_nodforces
!!!---------------------------------------------------------

!------------------------------------------------------------------------
 SUBROUTINE write_out_Rnod(nfich,ifrom,ito)

   IMPLICIT NONE
   INTEGER :: ibdyty,inodty,nfich,ifrom,ito,iccdof,nbdof
   INTEGER :: lc 
   INTEGER :: err 

   err = 0

   IF (nfich == 1) THEN
     lc = LEN_TRIM(out_Rnod)
     OPEN(unit=1,STATUS='OLD',POSITION='APPEND',file=TRIM(location(out_Rnod(1:lc))),IOSTAT=err) 
   ELSE IF (nfich == 2) THEN
     lc = LEN_TRIM(last_Rnod)
     OPEN(unit=2,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Rnod(1:lc))),IOSTAT=err) 
   END IF

   !fd a voir si il faut affiner
   IF ( err /= 0) RETURN 

   DO ibdyty = ifrom,ito

                          !123456789012345678901234567890123456789012345678901234567890123456789012
     WRITE(nfich,'(A72)') '$bdyty                                                                  '
     WRITE(nfich,101) 'MAILx',ibdyty
                          !123456789012345678901234567890123456789012345678901234567890123456789012
     WRITE(nfich,'(A72)') '$nodty                                                                  ' 

     DO inodty=1,SIZE(bdyty(ibdyty)%nodty)

       nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       iccdof=bdyty(ibdyty)%ccdof(inodty)

       CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                          bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdof)/H, &
                          'R/H',nfich)

     ENDDO
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
   END DO 
!                      123456789012345678901234567890123456789012345678901234567890123456789012
   WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
     
101   FORMAT(1X,A5,2X,I5)            
           
   IF (nfich == 1) CLOSE(1)
   IF (nfich == 2) CLOSE(2)
   
 END SUBROUTINE write_out_Rnod

!!!------------------------------------------------------------------------
  SUBROUTINE write_out_nodforces(nfich,ifrom,ito)

    IMPLICIT NONE

    INTEGER :: ibdyty,inodty,nfich,ifrom,ito,iccdof,nbdof
    REAL(kind=8) ::sumx,sumy
   
    DO ibdyty = ifrom,ito

       WRITE(nfich,'(A6)') '$bdyty'
       WRITE(nfich,101) 'MAILx',ibdyty
       WRITE(nfich,'(A6)') '$nodty'

       sumx=0.d0
       sumy=0.d0
       
       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdof)/H, &
                             'R/H',nfich)
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%Fint(iccdof+1:iccdof+nbdof),&
                             'Fin',nfich)
          CALL write_a_nodty(get_nodNAME(bdyty(ibdyty)%nodty(inodty)),inodty, &
                             bdyty(ibdyty)%Finert(iccdof+1:iccdof+nbdof), &
                             'Mac',nfich)

          sumx= sumx + bdyty(ibdyty)%Ireac(iccdof+1)/H &
                     + bdyty(ibdyty)%Fint(iccdof+1) &
                     + bdyty(ibdyty)%Finert(iccdof+1)

          sumy= sumy + bdyty(ibdyty)%Ireac(iccdof+2)/H &
                     + bdyty(ibdyty)%Fint(iccdof+2) &
                     + bdyty(ibdyty)%Finert(iccdof+2)

       END DO

       WRITE(nfich,'(2(1x,D20.13))') sumx,sumy
       
       WRITE(nfich,'(A6)')'$$$$$$'
       WRITE(nfich,'(A6)')'      '
   END DO
   !                     123456789012345678901234567890123456789012345678901234567890123456789012
   WRITE(nfich,'(A72)') '!-----------------------------------------------------------------------' 
   
101 FORMAT(1X,A5,2X,I5)            
   
 END SUBROUTINE write_out_nodforces

!------------------------------------------------------------------------
 INTEGER FUNCTION get_nb_poroMAILx()

   IMPLICIT NONE

   get_nb_poroMAILx = nb_poroMAILx

 END FUNCTION get_nb_poroMAILx

!!!------------------------------------------------------------------------
  SUBROUTINE update_existing_entities_poroMAILx

    IMPLICIT NONE

    nb_existing_entities = get_nb_ENTITY()

    CALL add_nb_ENTITY(nb_poroMAILx)

  END SUBROUTINE update_existing_entities_poroMAILx

!!!------------------------------------------------------------------------
  SUBROUTINE load_behaviours_poroMAILx

    IMPLICIT NONE

    INTEGER :: ibdyty,iblmty,ibehav,imodel
    INTEGER :: iM_bdyty,iM_blmty,iM_behav,iM_model
    
    INTEGER :: itest 
    
    CHARACTER(len=103) :: cout
                              !12345678901234567890123456
    CHARACTER(len=26)  :: IAM='poroMAILx::load_behaviours'

    IF (nb_poroMAILx == 0) RETURN

    DO ibdyty=1,SIZE(bdyty)
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
          
          imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb

!fd 12/12/07 on va a la peche au materiau definit dans le fichier BODIES.DAT
!fd pour ca il faut trouver le rang du couple modele/materiau lu a partir du numero de modele stocke
!fd du lourd !!

          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
            IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) EXIT
          ENDDO

          IF (iM_model >  SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)) THEN
             !                    123456789012345678901234567890123456789
             WRITE(cout,'(A39)') 'unable to recover the global model rank'
             CALL FATERR(IAM,cout)
          ENDIF


!print*,imodel,M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(imodel)
!print*,get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(imodel))

          bdyty(ibdyty)%blmty(iblmty)%lawnb = &
            get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
          
          IF (bdyty(ibdyty)%blmty(iblmty)%lawnb == 0) THEN
             !                                 12345678901          123456789          123456789012345678901 
             WRITE(cout,'(A11,I5,A9,I5,A21)') 'poro MAILX ',ibdyty,' element ',iblmty,' without behaviour !?'
             
             CALL LOGMES('check BODIES.DAT in DATBOX')
             CALL LOGMES('check BEHAVIOURS.DAT in DATBOX')
             CALL FATERR(IAM,cout)
          END IF
       END DO
    END DO
    
  END SUBROUTINE load_behaviours_poroMAILx

!!!---------------------------------------------------------
  !> \brief Read a DOF file to initialize database
  subroutine read_in_meca_dof_poroMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step < 1) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
    else
      open(unit=G_nfich,file=trim(location(out_dof(:))))
    end if

    call read_in_meca_dof
    close(G_nfich)

  end subroutine read_in_meca_dof_poroMAILx
!!!---------------------------------------------------------
  !> \brief Read a DOF file to initialize database
  subroutine read_in_dof_poroMAILx(step)
    implicit none
    integer(kind=4), intent(in) :: step

    G_nfich = get_io_unit()

    if(step < 1) then
      open(unit=G_nfich,file=trim(location(in_dof(:))))
    else
      open(unit=G_nfich,file=trim(location(out_dof(:))))
    end if

    call read_in_dof
    close(G_nfich)
    
  end subroutine read_in_dof_poroMAILx
!!!------------------------------------------------------------------------   
  SUBROUTINE read_in_dof

    IMPLICIT NONE

    INTEGER          :: ibdyty,inodty,nbdof,iccdof
    INTEGER          :: iM_bdyty,iM_nodty,iM_ccdof
    CHARACTER(len=5) :: test,chnod
    INTEGER          :: itest,errare
                             !123456789012345678901234567890    
    CHARACTER(len=22)  :: IAM='poroMAILx::read_in_dof'
    CHARACTER(len=103) :: cout
    real(kind=8) :: x(6)

    IF (nb_poroMAILx == 0) RETURN
    
    DO ibdyty = 1,SIZE(bdyty)
       DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) =0.D0
          bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)      =0.D0
          bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) =0.D0
          bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)      =0.D0
       ENDDO
       !am & pta : initialisation a 0 des vitesses et deplacements du centre
       !d'inertie du rigide equivalent
    END DO
   
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty_MAILx(G_clin)                      
       IF (itest == ifound) THEN
          READ(G_clin(9:13),'(I5)') ibdyty
          IF (ibdyty <= 0 .OR. ibdyty > SIZE(bdyty)) THEN
             WRITE(cout,'(A12,I5,A60)') 'body number ',ibdyty,' does not belong to collection'
             CALL FATERR(IAM,cout)
          END IF
       ELSE
          CYCLE
       END IF

       ! fd recherche du model ...

       DO    
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'POROx') CYCLE                ! fishing the MECAx part 

          DO    
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty' 
             DO
                IF( .NOT. read_G_clin()) EXIT
                itest = itest_nodty_MAILx(G_clin,ibdyty)
                !print *,'itest : ',itest
                IF (itest == isskip) CYCLE
                IF (itest == inomor) EXIT                      
                IF (itest == ifound) THEN
                  READ(G_clin(9:13),'(I5)') inodty
                  IF (inodty < 0 .OR. inodty > SIZE(bdyty(ibdyty)%nodty)) THEN 
                    WRITE(cout,'(A12,I5,A25,I5,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                    CALL FATERR(IAM,cout)
                  END IF
                  !print *,'nbdof_a_nodty(G_clin(2:6) : ',nbdof_a_nodty(G_clin(2:6))
                  !print *,'nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) : ',nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                  

                  !fd 27/09/2013 A QUOI CA SERT ?

                  !chnod=G_clin(2:6)
                  !IF (nbdof_a_nodty(chnod) > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN 
                  !   IF (nbdof_a_nodty(chnod) - 1 > nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))) THEN
                  !      WRITE(cout,'(A5,I5,A22,I5)') chnod,inodty,' is not nodty of body ', ibdyty
                  !      CALL FATERR(IAM,cout)
                  !   ENDIF
                  !   SELECT CASE(chnod)
                  !   CASE('NO3xx')
                  !      chnod = 'NO2xx'
                  !   CASE('NO4xx')
                  !      chnod = 'NO3xx'
                  !   END SELECT
                  !END IF

                  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                  iccdof=bdyty(ibdyty)%ccdof(inodty)

                  CALL G_read_a_nodty(bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof),get_nodNAME(bdyty(ibdyty)%nodty(inodty)))

                  bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
                  
                  IF( .NOT. read_G_clin()) EXIT 

                  CALL G_read_a_nodty(bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof),get_nodNAME(bdyty(ibdyty)%nodty(inodty)))

                  bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
                  !print *,'Node : ', inodty, 'nbdof : ',nbdof
                  !print *,'Xbeg : ',bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
                  !print *,'Vbeg : ',bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
                  
                  CYCLE
                ENDIF
             END DO !les valeurs aux nodty
             EXIT       
          END DO ! $nodty 
          EXIT
       END DO ! $model
       CYCLE
    END DO ! $bdyty
   
    ! actualisation des cordonnees du maillage

    DO ibdyty=1,SIZE(bdyty)
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
          M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+2)= &
               M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+2) + &
               bdyty(ibdyty)%X(iccdof+1:iccdof+2)
       END DO
    END DO

  END SUBROUTINE read_in_dof

!!!------------------------------------------------------------------------   
  SUBROUTINE read_in_meca_dof

    IMPLICIT NONE

    INTEGER          :: ibdyty,inodty,nbdof,iccdof
    INTEGER          :: iM_bdyty,iM_nodty,iM_ccdof
    CHARACTER(len=5) :: test,chnod
    INTEGER          :: itest,errare
                             !123456789012345678901234567890    
    CHARACTER(len=22)  :: IAM='poroMAILx::read_in_dof'
    CHARACTER(len=103) :: cout
    real(kind=8) :: x(6)

    IF (nb_poroMAILx == 0) RETURN
    
    DO ibdyty = 1,SIZE(bdyty)
       DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          nbdof= nbDIME
          bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) =0.D0
          bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)      =0.D0
          bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) =0.D0
          bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)      =0.D0
       ENDDO
       !am & pta : initialisation a 0 des vitesses et deplacements du centre
       !d'inertie du rigide equivalent
    END DO
   
    DO    
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'bdyty') CYCLE                 ! fishing for the keyword 'bdyty'
       IF( .NOT. read_G_clin()) EXIT
       itest=itest_bdyty_MAILx(G_clin)                      
       IF (itest == ifound) THEN
          READ(G_clin(9:13),'(I5)') ibdyty
          IF (ibdyty <= 0 .OR. ibdyty > SIZE(bdyty)) THEN
             WRITE(cout,'(A12,I5,A60)') 'body number ',ibdyty,' does not belong to collection'
             CALL FATERR(IAM,cout)
          END IF
       ELSE
          CYCLE
       END IF

       ! fd recherche du model ...

       DO    
          IF ( .NOT. read_G_clin()) EXIT
          IF (G_clin(2:6) == '$$$$$') EXIT
          IF (G_clin(2:6) /= 'model') CYCLE                ! fishing for the keyword 'model' 
          IF ( .NOT. read_G_clin()) THEN
             CALL FATERR(IAM,'Problem reading model')
          END IF

          IF (G_clin(2:6) /= 'MECAx') CYCLE                ! fishing the MECAx part 

          DO    
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(2:6) /= 'nodty') CYCLE                ! fishing for the keyword 'nodty' 
             DO
                IF( .NOT. read_G_clin()) EXIT
                itest = itest_nodty_MAILx(G_clin,ibdyty)
                IF (itest == isskip) CYCLE
                IF (itest == inomor) EXIT                      
                IF (itest == ifound) THEN
                  READ(G_clin(9:13),'(I5)') inodty
                  IF (inodty < 0 .OR. inodty > SIZE(bdyty(ibdyty)%nodty)) THEN 
                    WRITE(cout,'(A12,I5,A25,I5,A29)') 'node number ',inodty,' does not belong to body ',ibdyty
                    CALL FATERR(IAM,cout)
                  END IF
                  !print *,'nbdof_a_nodty(G_clin(2:6) : ',nbdof_a_nodty(G_clin(2:6))
                  !print *,'nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) : ',nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                  
                  chnod=G_clin(2:6)
                  !print *,'chnod : ',G_clin(2:6)
                  nbdof= nbDIME
                  iccdof=bdyty(ibdyty)%ccdof(inodty)

                  CALL G_read_a_nodty(bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof))

                  bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
                  
                  IF( .NOT. read_G_clin()) EXIT 

                  CALL G_read_a_nodty(bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof))

                  bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
                  !print *,'Node : ', inodty, 'nbdof : ',nbdof
                  !print *,'Xbeg : ',bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
                  !print *,'Vbeg : ',bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
                  
                  CYCLE
                ENDIF
             END DO !les valeurs aux nodty
             EXIT       
          END DO ! $nodty 
          EXIT
       END DO ! $model
       CYCLE
    END DO ! $bdyty
   
    ! actualisation des cordonnees du maillage

    DO ibdyty=1,SIZE(bdyty)
       iM_bdyty=bdyty2M_bdyty(ibdyty)
       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)
          M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+2)= &
               M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+2) + &
               bdyty(ibdyty)%X(iccdof+1:iccdof+2)
       END DO
    END DO

  END SUBROUTINE read_in_meca_dof
!!!------------------------------------------------------------------------

  !> initializing X,V, for the first step iteration;    
  !> initializing is set as follows allowing a single theta method iteration
  !> for constant linear system; 

  SUBROUTINE increment_poroMAILx 

   IMPLICIT NONE 
   INTEGER :: ibdyty,ivd,inod,idof,iccdof
   REAL(kind=8) :: Vdrivenbegin,Vdriven,Xdrivenbegin,Xdriven
   real(kind=8) :: spin(3) 
   real(kind=8) :: UMTTH,TTH

   IF (nb_poroMAILx == 0) RETURN
   
   TTH = theta*H
   UMTTH = (1.d0 - theta)*H

   DO ibdyty=1,SIZE(bdyty)

      IF (.NOT. bdyty(ibdyty)%visible) CYCLE

      !am & pta : on palce le corps rigide equivalent dans la configuration
      !intermediaire : integration des vitesses de translation + calcul de la
      !nouvelle orientation du repere principal d'inertie (integration des
      !rotations)
       bdyty(ibdyty)%V_ALE= bdyty(ibdyty)%V_ALE_begin
       bdyty(ibdyty)%V= bdyty(ibdyty)%Vbegin                
       
       bdyty(ibdyty)%X= bdyty(ibdyty)%Xbegin + & 
                      UMTTH*bdyty(ibdyty)%Vbegin*bdyty(ibdyty)%Mask_No_ALE + &
                      TTH  *bdyty(ibdyty)%V     *bdyty(ibdyty)%Mask_No_ALE + & 
                      H*bdyty(ibdyty)%V_ALE_begin*bdyty(ibdyty)%Mask_ALE
                      

     DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof
 
       CALL comp_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),Vdrivenbegin,Vdriven)
       CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)

       iccdof=bdyty(ibdyty)%ccdof(inod)+idof

       Xdrivenbegin = bdyty(ibdyty)%Xbegin(iccdof)
       Xdriven    = Xdrivenbegin + (UMTTH*Vdrivenbegin)+(TTH*Vdriven)
       bdyty(ibdyty)%Vdriv(ivd) = Vdriven
       bdyty(ibdyty)%VdrivBeg(ivd) = Vdrivenbegin
       bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

     END DO

     bdyty(ibdyty)%Vfree=0.d0

   ENDDO

 END SUBROUTINE increment_poroMAILx 

!!!------------------------------------------------------------------------
SUBROUTINE nullify_poro_driven_dof(ibdyty,storage)

  IMPLICIT NONE 
  INTEGER :: ivd,ibdyty,inod,idof,iccdof
  INTEGER :: storage

  IF (nb_poroMAILx == 0) RETURN

! computing driven dof
  DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof

     CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)

     iccdof=bdyty(ibdyty)%ccdof(inod)+idof   

     SELECT CASE(storage)
       CASE(iV____)
         bdyty(ibdyty)%V(iccdof)=0.d0
         bdyty(ibdyty)%X(iccdof)=0.d0
       CASE(iVfree)
         bdyty(ibdyty)%Vfree(iccdof)=0.d0
       CASE(iVaux_)
         bdyty(ibdyty)%Vaux(iccdof)=0.d0
       CASE default
         call faterr('poroMAILx::mullify_poro_driven_dof','unknown storage')
     END SELECT      

   END DO  

END SUBROUTINE nullify_poro_driven_dof
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------    
  SUBROUTINE update_dof_poroMAILx 

    IMPLICIT NONE 
    INTEGER :: ibdyty,inodty,iccdof
    INTEGER :: iM_bdyty,iM_nodty,iM_ccdof

    !fd tableaux aux pour rigid 
    REAL(kind=8),DIMENSION(nbdime):: coor_aux

    IF (nb_poroMAILx == 0) RETURN  

    DO ibdyty=1,SIZE(bdyty)
  
       IF (.NOT. bdyty(ibdyty)%visible) CYCLE  !PTA

      ! calcul partie defo
       bdyty(ibdyty)%V_ALE_begin=bdyty(ibdyty)%V_ALE      
       bdyty(ibdyty)%Vbegin=bdyty(ibdyty)%V                
       bdyty(ibdyty)%Xbegin=bdyty(ibdyty)%X 
      
    END DO

    ! actualisation des cordonnees du maillage (en repere globale)

    DO ibdyty=1,SIZE(bdyty)
 
       IF (.NOT. bdyty(ibdyty)%visible) CYCLE  !PTA
 
       iM_bdyty=bdyty2M_bdyty(ibdyty)

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          iM_ccdof=M_bdyty(iM_bdyty)%ccdof(iM_nodty)

          ! on calcule dans le repere principal d'inertie
          ! on tourne dans le global 

          !fd a confirmer la partie XR ?

          !am & pta : calcul des nouvelles coordonnees pour un corps maille
          !classique
          M_bdyty(iM_bdyty)%coor(iM_ccdof+1:iM_ccdof+nbDIME) = &
            M_bdyty(iM_bdyty)%cooref(iM_ccdof+1:iM_ccdof+nbDIME) + bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME)                

       END DO

    END DO

  END SUBROUTINE update_dof_poroMAILx


!------------------------------------------------------------------------  
! linear routines
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  SUBROUTINE compute_mass_poroMAILx

    IMPLICIT NONE

    INTEGER :: errare
    INTEGER :: ibdyty,iblmty
    INTEGER :: idof,iccdof,inodty
    INTEGER :: i,nbdof_meca, nbdof_ther,nbdof
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: U_ele
    REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: P_ele

                             !12345678901234567890123
    CHARACTER(len=23) :: IAM='poroMAILX::compute_mass'

!!!
!!! on calcule ici la masse sur la configuration de reference 
!!! elle peut etre lumpee
!!!

    IF (nb_poroMAILx == 0) RETURN

    !paranoiac dealloc

    IF (ALLOCATED(coor_ele)) THEN
       CALL FATERR(IAM,'coor_ele already allocated !?')
    END IF

    DO ibdyty=1,SIZE(bdyty)
       !
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          !
          ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

          IF (errare /= 0) THEN
             CALL LOGMES('Error '//IAM//': allocating coor_ele')
          END IF

          !! beurk il y a des trucs pas secure entre nodty et DIME !!

          !am & pta : on utilise les coordonnees des noeuds dans le repere
          !principal d'inertie pour calculer les matrices de masses elementaires
          coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
          !
          ALLOCATE(U_ele(bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node,bdyty(ibdyty)%blmty(iblmty)%meca_node), stat=errare)
          ALLOCATE(P_ele(bdyty(ibdyty)%blmty(iblmty)%ther_node))

          U_ele = 0.0
          P_ele = 0.0
          
          nbdof_ther = bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node
          nbdof_meca = bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
      
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'Error allocating U_ele')
          ENDIF
  
          ! Recuperation des deplacement au point milieu de la partie structure
          DO i=1,bdyty(ibdyty)%blmty(iblmty)%meca_node

                ! passage au numero global
                inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
                ! position un dans le vecteur global pour le numero inodty
                nbdof   = bdyty(ibdyty)%dofnb(inodty)   
                iccdof  = bdyty(ibdyty)%ccdof(inodty) 
                U_ele(1:nbdof_meca,i) = theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof_meca) + &
                               (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof_meca)
          
          END DO
          
          ! Recuperation des pression au point milieu de la partie diffusive
          do i = 1, bdyty(ibdyty)%blmty(iblmty)%ther_node

            ! passage au numero global
            inodty  = bdyty(ibdyty)%blmty(iblmty)%NODES(i)
            ! position un dans le vecteur global pour le numero inodty
            iccdof  = bdyty(ibdyty)%ccdof(inodty)
            P_ele(i) =         theta *bdyty(ibdyty)%V(iccdof+nbdof_ther+nbdof_meca) + &
                       (1.d0 - theta)*bdyty(ibdyty)%Vbegin(iccdof+nbdof_ther+nbdof_meca)
          end do

          CALL compute_elementary_mass(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                       bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                       H,coor_ele,U_ele,P_ele,ibdyty,iblmty, &
                                       bdyty(ibdyty)%blmty(iblmty)%mass)

 

          DEALLOCATE(coor_ele,U_ele,P_ele)
       END DO
    END DO

  END SUBROUTINE compute_mass_poroMAILx
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  SUBROUTINE compute_damping_poroMAILx

    IMPLICIT NONE

    INTEGER :: errare
    INTEGER :: ibdyty,iblmty
    INTEGER :: i,idof,nbdof_meca, nbdof_ther,nbdof
    INTEGER :: inodty,iccdof, iM_bdyty, iM_blmty
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: U_ele,V_ele
    REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE :: P_ele

                             !12345678901234567890123456
    CHARACTER(len=26) :: IAM='poroMAILX::compute_damping'

    IF (nb_poroMAILx == 0) RETURN

    !paranoiac dealloc

    IF (ALLOCATED(coor_ele)) &
      CALL FATERR(IAM,'coor_ele already allocated !?')


    DO ibdyty=1,SIZE(bdyty)
       !
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

          ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
          IF (errare /= 0) THEN
             CALL LOGMES('Error '//IAM//': allocating coor_ele')
          END IF
          
          coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
          
          ALLOCATE(U_ele(bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node,bdyty(ibdyty)%blmty(iblmty)%meca_node), stat=errare)
          ALLOCATE(V_ele(bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node,bdyty(ibdyty)%blmty(iblmty)%meca_node), stat=errare)
          ALLOCATE(P_ele(bdyty(ibdyty)%blmty(iblmty)%ther_node), stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'Error allocating U_ele or P_ele')
          ENDIF

          U_ele = 0.0
          V_ele = 0.0
          P_ele = 0.0
          
          nbdof_ther = bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node
          nbdof_meca = bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
      
          ! Recuperation des deplacement au point milieu de la partie structure
          DO i=1,bdyty(ibdyty)%blmty(iblmty)%meca_node

                ! passage au numero global
                inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
                ! position un dans le vecteur global pour le numero inodty
                nbdof   = bdyty(ibdyty)%dofnb(inodty)   
                iccdof  = bdyty(ibdyty)%ccdof(inodty) 
                U_ele(1:nbdof_meca,i) = theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof_meca) + &
                               (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof_meca)
                               
                V_ele(1:nbdof_meca,i) = theta*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof_meca) + &
                               (1.d0 - theta)*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof_meca) -&
                                        theta*bdyty(ibdyty)%V_ALE(iccdof+1:iccdof+nbdof_meca) + &
                               (1.d0 - theta)*bdyty(ibdyty)%V_ALE_begin(iccdof+1:iccdof+nbdof_meca)
                               
                !print *,'X : ',bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof_meca)
                !print *,'X_beg : ',bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof_meca)
                !print *,'V : ',bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof_meca)
                !print *,'V_beg : ',bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof_meca)
          
          END DO
     
          ! Recuperation des pression au point milieu de la partie diffusive
          DO i=1,bdyty(ibdyty)%blmty(iblmty)%ther_node
          
                 ! passage au numero global
                 inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
                 ! position un dans le vecteur global pour le numero inodty
                 iccdof  = bdyty(ibdyty)%ccdof(inodty)
                 P_ele(i) = theta*bdyty(ibdyty)%V(iccdof+nbdof_ther+nbdof_meca) + &
                   (1.d0 - theta)*bdyty(ibdyty)%Vbegin(iccdof+nbdof_ther+nbdof_meca)
          END DO
          
          iM_bdyty = bdyty2M_bdyty(ibdyty)
          iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
          
          CALL compute_elementary_damping(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                          bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                          H,coor_ele,U_ele,V_ele,P_ele,iM_bdyty,iM_blmty, &
                                          bdyty(ibdyty)%blmty(iblmty)%damping)
          
          DEALLOCATE(coor_ele,U_ele,P_ele,V_ele)
       END DO
    END DO

  END SUBROUTINE compute_damping_poroMAILx
!------------------------------------------------------------------------ 
 SUBROUTINE compute_Fext_poroMAILx

 IMPLICIT NONE
 INTEGER :: ibdyty,iblmty,inodty
 INTEGER :: idof,ifd,inod,iccdof, nbdof,nbdof_meca, mm
 REAL(kind=8) :: Febegin,Fe

 INTEGER :: i,errare, dof_poro
 REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele

!                           12345678901234567890123
  CHARACTER(len=23) :: IAM='poroMAILX::compute_Fext'

!fd
!fd on calcule ici les forces extérieures nodales:
!fd    la contribution de la gravité H*M*grav (a mettre eventuellement dans Fext elementaire)
!fd    la contribution des forces imposées dans DRV_DOF
!fd

 IF (nb_poroMAILx == 0) RETURN

 IF (ALLOCATED(DV_ele)) THEN
   CALL FATERR(IAM,'DV_ele already allocated !?')
 ENDIF

 DO ibdyty=1,SIZE(bdyty)
   bdyty(ibdyty)%Fext=0.d0
   !print *,'Initialisation Effort impose au noeuds : ', bdyty(ibdyty)%Fext
   !am & pta : initialisation a 0 des forces exterieures pour le rigide
   !equivalent
  ! gravity are computed locally ...

  IF ((nbDIME == 2 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0)) .OR. &
      (nbDIME == 3 .AND. (grav1 /= 0.d0 .OR. grav2 /= 0.d0 .OR. grav3 /= 0.d0))) THEN 

    !am & pta : on appplique la gravite uniquement au rigide equivalent, le cas
    !echeant
    !print *,'Calcul des termes de gravite'
      DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
        
        nbdof=bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
        ALLOCATE(DV_ele(bdyty(ibdyty)%blmty(iblmty)%ndof),stat=errare)

        DV_ele=0.d0

        DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
          inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i)
          IF (nbDIME == 2) THEN
            dof_poro = bdyty(ibdyty)%blmty(iblmty)%meca2poro((i-1)*nbdof+1)
            DV_ele(dof_poro) = grav1
            dof_poro = bdyty(ibdyty)%blmty(iblmty)%meca2poro((i-1)*nbdof+2)
            DV_ele(dof_poro) = grav2
          ELSE IF (nbDIME == 3) THEN
            dof_poro = bdyty(ibdyty)%blmty(iblmty)%meca2poro((i-1)*nbdof+1)
            DV_ele(dof_poro) = grav1
            dof_poro = bdyty(ibdyty)%blmty(iblmty)%meca2poro((i-1)*nbdof+2)
            DV_ele(dof_poro) = grav2
            dof_poro = bdyty(ibdyty)%blmty(iblmty)%meca2poro((i-1)*nbdof+3)
            DV_ele(dof_poro) = grav3
          ELSE
            call faterr(IAM,'unknown value of nbDIME')
          ENDIF
        END DO
        !print *,'Gravite : ',DV_ele
        DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)
        !print *,'Effort elementaire Gravite : ',DV_ele
        mm = 1
        DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)

          inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
          ! position un dans le vecteur global pour le numero inodty     
          iccdof=bdyty(ibdyty)%ccdof(inodty)
          ! nombre de dof pour le noeuds considere
          nbdof = bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node(i)
          !print *,'Affectation des charges sur global : Noeud : ',inodty
          !print *,'Position local dof',mm, mm+nbdof-1 
          bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof) + &
                                                          DV_ele(mm:mm+nbdof-1)
          mm = mm + nbdof
          !print *,'Effort aux noeuds', inodty
          !print *,'Fext',bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdof)
        
        END DO
        DEALLOCATE(DV_ele)

      ENDDO
  ENDIF
  ! driven forces
  !print *,'Fext',bdyty(ibdyty)%Fext
  
  
  ! DA : Ajout des efforts etxterieurs locaux (le test sur la gravite fait chier ici non ?
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    
    CALL assemble_elementary_vector(bdyty(ibdyty)%Fext, &
                                    bdyty(ibdyty)%blmty(iblmty)%ttFext(:), &
                                    bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

    !print *,'bdyty(ibdyty)%blmty(iblmty)%ttFext(:) : ',bdyty(ibdyty)%blmty(iblmty)%ttFext(:)

    bdyty(ibdyty)%blmty(iblmty)%ttFext = 0.D0
  ENDDO
  ! Fin de modif
  
  DO ifd=1,bdyty(ibdyty)%nb_flux_driven_dof

    CALL comp_a_driven_dof(bdyty(ibdyty)%flux_driven_dof(ifd),Febegin,Fe)

!!! il faudrait faire un test : si on est en theta-methode c'est ca sinon ca
!!! pourrait etre autre chose

    bdyty(ibdyty)%FdrivBeg(ifd)= Febegin
    bdyty(ibdyty)%Fdriv(ifd)   = Fe     

    CALL owner_of_a_driven_dof(bdyty(ibdyty)%flux_driven_dof(ifd),inod,idof)
    !print *,'Application du chargement impose : ',ifd,inod,idof

    iccdof=bdyty(ibdyty)%ccdof(inod)+idof   
    bdyty(ibdyty)%Fext(iccdof)=bdyty(ibdyty)%Fext(iccdof)+((1.D0-THETA)*Febegin)+(THETA*Fe)
    

   END DO
   !print *,'Effort impose au noeuds : ', bdyty(ibdyty)%Fext
END DO

END SUBROUTINE compute_Fext_poroMAILx

!------------------------------------------------------------------------  

SUBROUTINE compute_bulk_poroMAILx(istate)
  IMPLICIT NONE

  INTEGER,INTENT(in) :: istate ! =0 comp stiffness & ttFint, =1 comp fields

  ! ***
  INTEGER :: errare
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: U_ele, V_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE :: P_ele
  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint
  INTEGER :: ibdyty,iblmty
  INTEGER :: iM_bdyty,iM_blmty
  INTEGER :: idof,nbdof_meca, nbdof_ther,nbdof
  INTEGER :: i,inodty,iccdof
!                           12345678901234567890123
  CHARACTER(len=23) :: IAM='poroMAILX::compute_bulk'

  IF (nb_poroMAILx == 0) RETURN

  IF (ALLOCATED(coor_ele)) THEN
    CALL FATERR(IAM,'coor_ele already allocated !?')
  ENDIF

  DO ibdyty=1,SIZE(bdyty)
    !
    !am & pta : si on ne cherche pas a calculer la deformation, on zappe
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'Error allocating coor_ele')
      ENDIF
      
      !am & pta : on utilise les coordonnees des noeuds dans le repere
      !principal d'inertie pour calculer les matrices de rigidite elementaires

      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 

      ALLOCATE(U_ele(bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node,bdyty(ibdyty)%blmty(iblmty)%meca_node), stat=errare)

      U_ele = 0.0
      
      nbdof_ther = bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node
      nbdof_meca = bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
      
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'Error allocating U_ele')
      ENDIF
      
      !
      ! on calcule les matrices elementaires ici
      ! a revoir plus tard ... 
      ! 
      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)

      IF (istate==0) THEN
         ! Recuperation des deplacement au point milieu de la partie structure
         DO i=1,bdyty(ibdyty)%blmty(iblmty)%meca_node

          ! passage au numero global
          inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
          ! position un dans le vecteur global pour le numero inodty
          nbdof   = bdyty(ibdyty)%dofnb(inodty)   
          iccdof  = bdyty(ibdyty)%ccdof(inodty) 
          U_ele(1:nbdof_meca,i)=         theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof_meca) + &
                                 (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof_meca)
          
        END DO
        ! calcul de K et ttFint 
        ! ne conserve pas les deformations et les contraintes
        
        ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%ttFint,dim=1)),stat=errare)
        IF (errare /= 0) THEN
          CALL FATERR(IAM, 'Error allocating Fint')
        ENDIF

        Fint = 0.d0
        
        CALL compute_elementary_bulk(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     H,coor_ele,U_ele,iM_bdyty,iM_blmty, &
                                     Fint, &
                                     bdyty(ibdyty)%blmty(iblmty)%stiffness)       

        bdyty(ibdyty)%blmty(iblmty)%ttFint = bdyty(ibdyty)%blmty(iblmty)%ttFint + Fint
        !bdyty(ibdyty)%blmty(iblmty)%ttFint = Fint
        !print *,'Fint en structure ',Fint
        !print *,'ttfint ',size(bdyty(ibdyty)%blmty(iblmty)%ttFint)
        DEALLOCATE(Fint)
      ELSE IF (istate == 1) THEN
        
        ALLOCATE(V_ele(bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node,bdyty(ibdyty)%blmty(iblmty)%meca_node), stat=errare)

        V_ele = 0.0
        
        ALLOCATE(P_ele(bdyty(ibdyty)%blmty(iblmty)%ther_node), stat=errare)

        P_ele = 0.0
        ! nombre de dof pour le noeuds considere
        
        ! Modification solvatrice
        !bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.D0
        
        DO i=1,bdyty(ibdyty)%blmty(iblmty)%meca_node

          ! passage au numero global
          inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
          ! position un dans le vecteur global pour le numero inodty      
          nbdof  = bdyty(ibdyty)%dofnb(inodty)
          iccdof = bdyty(ibdyty)%ccdof(inodty)

          U_ele(1:nbdof_meca,i)= bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof_meca)
          V_ele(1:nbdof_meca,i)= bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof_meca)
        END DO
        
        ! Recuperation des pression au point milieu de la partie diffusive
        DO i=1,bdyty(ibdyty)%blmty(iblmty)%ther_node
          
                 ! passage au numero global
                 inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
                 ! position un dans le vecteur global pour le numero inodty
                 iccdof  = bdyty(ibdyty)%ccdof(inodty)
                 P_ele(i) = theta*bdyty(ibdyty)%V(iccdof+nbdof_ther+nbdof_meca) + &
                   (1.d0 - theta)*bdyty(ibdyty)%Vbegin(iccdof+nbdof_ther+nbdof_meca)
        END DO
        
        CALL compute_elementary_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                       bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                       H,coor_ele,U_ele,V_ele,P_ele,iM_bdyty,iM_blmty)
                                       
        DEALLOCATE(P_ele, V_ele)
      ELSE
        CALL FATERR(IAM,'unsupported istate')
      ENDIF
      
      DEALLOCATE(coor_ele,U_ele)

    ENDDO

  ENDDO

 END SUBROUTINE compute_bulk_poroMAILx

 !------------------------------------------------------------------------ 
SUBROUTINE push_ppset_poroMAILx

   IMPLICIT NONE

   INTEGER :: ibdyty,iblmty,ibehav,imodel
   INTEGER :: iM_bdyty,iM_blmty,iM_behav

   INTEGER :: itest 

   CHARACTER(len=103) :: cout
                             !123456789012345678901234567890 
   CHARACTER(len=21)  :: IAM='poroMAILx::push_ppset'

   INTEGER :: igp,nb_gp

   IF (nb_poroMAILx == 0) RETURN

   DO ibdyty=1,SIZE(bdyty)
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
       ibehav=bdyty(ibdyty)%blmty(iblmty)%lawnb

       !fd new 13/08/09
       ! one needs a ppset by gp

       nb_gp=get_N_GP_poroEF(modelz(imodel)%ID, 'MECA')
       ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ppsnb(nb_gp))

       !fd new 05/11/09
       ! one can re-use a ppset if already defined
       ! the strategy new/re-use is defined through the flag use_existing_ppset

       DO igp=1,nb_gp
         bdyty(ibdyty)%blmty(iblmty)%ppsnb(igp) = get_ppset_nb(use_existing_ppset,imodel,ibehav)
       ENDDO
     END DO
   END DO  
   
 END SUBROUTINE push_ppset_poroMAILx
 
!------------------------------------------------------------------------
FUNCTION get_cooref_ele(ibdyty,iblmty,nbNODES)
  IMPLICIT NONE
  INTEGER                     :: ibdyty,iblmty,inodty,inodes,nbNODES
  INTEGER                     :: iM_bdyty,iM_nodty

  REAL(kind=8),DIMENSION(nbDIME,nbNODES) :: get_cooref_ele

  iM_bdyty=bdyty2M_bdyty(ibdyty)

!  print*,iM_bdyty,ibdyty

  DO inodes=1,nbNODES
    inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
    iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

    get_cooref_ele(:,inodes)=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

!    print*,inodty,iM_nodty,coor_ele(:,inodes)

  ENDDO

END FUNCTION get_cooref_ele
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE set_without_renum_poroMAILx
 IMPLICIT NONE

   with_renum=.FALSE.

 END SUBROUTINE set_without_renum_poroMAILx
 
 !------------------------------------------------------------------------
 SUBROUTINE put_vector_poroMAILx(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof,iccdof,inodty,mm
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect

   character(len=21) :: IAM
         !123456789012345678901
   IAM = 'poroMAILx::put_vector'

   IF (nb_poroMAILx == 0) RETURN

   SELECT CASE(id_vect)
    CASE('X____')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Xbeg_')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('V____')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%V(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Vbeg_')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('V_ALE')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%V_ALE(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('VbALE')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%V_ALE_begin(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Raux_')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Vfree')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Reac_')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Fext_')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbDIME) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbDIME) + vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
    CASE('Fint_')
     IF (nbdof /= bdyty(ibdyty)%nbdof_meca ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       bdyty(ibdyty)%Fint(iccdof+1:iccdof+nbDIME) = vect(mm:mm+nbDIME-1)       
       mm = mm + nbDIME
     ENDDO
     CASE('Qint_')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble
       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            bdyty(ibdyty)%Fint(iccdof+nbDIME+1) = vect(mm)
       ENDIF
       mm = mm + 1
     ENDDO
     CASE('Qext_')
     
     !print *,'nbdof : ',nbdof
     !print *,'nbr dof',bdyty(ibdyty)%nbdof 
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            bdyty(ibdyty)%Fext(iccdof+nbDIME+1) = vect(mm)
       ENDIF
       mm = mm + 1
     ENDDO
     CASE('P____')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            bdyty(ibdyty)%V(iccdof+nbDIME+1) = vect(mm)
       ENDIF
       mm = mm + 1
     ENDDO
     CASE('Pbeg_')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
            iccdof=bdyty(ibdyty)%ccdof(inodty)
            bdyty(ibdyty)%Vbegin(iccdof+nbDIME+1) = vect(mm)
       ENDIF
       mm = mm + 1
     ENDDO
    CASE DEFAULT
     call faterr(IAM,'unknown id '//id_vect)
   END SELECT

 END SUBROUTINE put_vector_poroMAILx


!------------------------------------------------------------------------
 SUBROUTINE assemb_KT_poroMAILx
 IMPLICIT NONE

   REAL(kind=8) :: HT,HT2
   INTEGER :: ibdyty,iblmty,ivd,iccdof,idof,inod

   integer :: timer=0

!   print*,'assemb_KT_poroMAILx'

   IF (nb_poroMAILx == 0) RETURN

                                            !01234567890123456789
   if (timer == 0) timer = get_new_itimer_ID('ASSEMB KT           ')

   HT=THETA*H
   HT2=HT*HT

   if (timer /= 0) call start_itimer(timer)

   DO ibdyty=1,SIZE(bdyty)

     !write(*,'(A,I0)') 'Body: ',ibdyty

     !CALL G_zero(bdyty(ibdyty)%KT)

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

        call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%mass)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%damping,HT)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%stiffness,HT2)
        
        !write(*,'(A,I0)') 'Element: ',iblmty
        !print *,'Mass : '
        !write(*,'(20(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%mass
        
        !print *,'Damping : '
        !write(*,'(20(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%damping
        
        !print *,'Stiffness : '
        !write(*,'(20(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%stiffness

        !bdyty(ibdyty)%blmty(iblmty)%KTloc=  bdyty(ibdyty)%blmty(iblmty)%mass + &
        !                                (HT*bdyty(ibdyty)%blmty(iblmty)%damping) + &
        !                               (HT2*bdyty(ibdyty)%blmty(iblmty)%stiffness)
        
        !write(*,'(20(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%KTloc
        !CALL G_assemb(bdyty(ibdyty)%KT,bdyty(ibdyty)%blmty(iblmty)%KTloc, &
        !              bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
     ENDDO 

   ENDDO

   if (timer /= 0) call stop_itimer(timer)

 END SUBROUTINE assemb_KT_poroMAILx
 
 !------------------------------------------------------------------------ 
 SUBROUTINE assemb_RHS_poroMAILx
   IMPLICIT NONE
!                           1234567890123456789012
   CHARACTER(len=22) :: IAM='poroMAILX::assemb_RHS'
   INTEGER :: ibdyty,iblmty,i,inodty,iccdof,nbdof,errare,mm
   REAL(kind=8) :: TT,UMTT,HTT,HUMTT
   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele, visco

!   print*,'assemb_RHS_poroMAILx'
   IF (nb_poroMAILx == 0) RETURN

   IF (ALLOCATED(DV_ele)) THEN
     CALL FATERR(IAM,'DV_ele already allocated !?')
   ENDIF
   IF (ALLOCATED(visco)) THEN
     CALL FATERR(IAM,'visco already allocated !?')
   ENDIF

   TT = THETA
   UMTT = (1.d0-TT)   
   HTT = H*TT
   HUMTT=H*(1.d0-TT)  

!fd
!fd d'abord les contributions elementaires
!fd

   DO ibdyty=1,SIZE(bdyty)

     bdyty(ibdyty)%RHS=0.D0
     bdyty(ibdyty)%Fint=0.D0
     bdyty(ibdyty)%Finert=0.D0

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

       nbdof=bdyty(ibdyty)%blmty(iblmty)%ndof
       
       ALLOCATE(DV_ele(nbdof),stat=errare)
       ALLOCATE(visco(nbdof),stat=errare)
       
       DV_ele = 0.d0
       visco = 0.d0
       
       !print *,'Vbegin : '
       !write(*,'(32(1x,E12.5))') bdyty(ibdyty)%Vbegin
       !print *,'V : '
       !write(*,'(32(1x,E12.5))') bdyty(ibdyty)%V
       mm = 1
       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
         ! passage au numero global
         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
         ! position un dans le vecteur global pour le numero inodty     
         iccdof=bdyty(ibdyty)%ccdof(inodty)
         ! nombre de dof pour le noeuds considere
         nbdof = bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node(i)
         !fd c'est completement idiot car dans increment on fait V=Vbegin 
         !
         DV_ele(mm:mm+nbdof-1)=bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) - &
                               bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
         
         visco(mm:mm+nbdof-1) =TT*bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof) + &
                             UMTT*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof)
         
         !print *,'Nodes : ',inodty
         !print *, 'dV  : ',DV_ele(mm:mm+nbdof-1)
         !print *, 'visco : ',visco(mm:mm+nbdof-1)
         !print *, 'placement : de ',mm,' a ',mm+nbdof-1
         mm = mm + nbdof
       END DO

       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)
       visco  = MATMUL(bdyty(ibdyty)%blmty(iblmty)%damping,visco)


       bdyty(ibdyty)%blmty(iblmty)%RHSloc=  DV_ele +  &
                                            ( ( HUMTT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) + &
                                                  HTT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) &
                                              ) &
                                            -( HUMTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
                                                 HTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) &
                                              ) &
                                            -( H*bdyty(ibdyty)%blmty(iblmty)%ttFint(:) ) &
                                            -( H*visco ) &
                                           )
       !print *,'Element: ',iblmty
       !print *,'DV_ele : '
       !write(*,'(20(1x,E12.5))') DV_ele
       !print *,'Visco : '
       !write(*,'(20(1x,E12.5))') visco
       !print *,'RHS : '
       !write(*,'(20(1x,E12.5))') bdyty(ibdyty)%blmty(iblmty)%RHSloc
      !
       CALL assemble_elementary_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

       CALL assemble_elementary_vector(bdyty(ibdyty)%Fint,-(      TT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) + &
                                                                UMTT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
                                                                     bdyty(ibdyty)%blmty(iblmty)%ttFint(:) + &
                                                                     visco                               ) , &
                                       bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       CALL assemble_elementary_vector(bdyty(ibdyty)%Finert,(DV_ele/H),bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  

       DEALLOCATE(DV_ele)
       DEALLOCATE(visco)
       
       ! Modification salvatrice
       bdyty(ibdyty)%blmty(iblmty)%ttFint = 0.D0

   ENDDO

!fd
!fd ensuite les contributions nodales 
!fd

     !print*,'RHS'
     !if (nbdime==2)then
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
     !else if (nbdime==3)then
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     !endif

    bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(H*bdyty(ibdyty)%Fext)

     !print*,'RHS+Fext'
     !if (nbdime==2)then
     !  write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
     !else if (nbdime==3)then
     !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
     !endif
     !print*,'========='


   ENDDO
 END SUBROUTINE assemb_RHS_poroMAILx
!------------------------------------------------------------------------
 
 !------------------------------------------------------------------------
 SUBROUTINE compute_free_vlocy_poroMAILx
  
   IMPLICIT NONE
   INTEGER :: ibdyty,ivd,inod,idof,iccdof
   INTEGER :: i,info,nbdof
   INTEGER :: errare
   
   real(kind=8),dimension(:)  ,allocatable :: RForce  
   real(kind=8),dimension(:,:),allocatable :: KK, KK_ddl

!                           123456789012345678901234567890
   CHARACTER(len=30) :: IAM='poroMAILX::compute_free_vlocy'

   integer :: timer=0

   IF (nb_poroMAILx == 0) RETURN

                                            !01234567890123456789
   if (timer == 0) timer = get_new_itimer_ID('COMPUTE Vfree       ')
   if (timer /= 0) call start_itimer(timer)
 
!   calcul de la vitesse libre.
!   la formulation utilisee pour les EF est incrementale qu'on soit en hpp ou en gd
!   %V contient la prediction de la vitesse qui peut etre differente de %Vbegin qui 
!   est la vitesse a la fin du pas precedent
   !print *,'###################computr vfree ###################'
   DO ibdyty=1,SIZE(bdyty)

     IF (itchache) THEN

       print*,'objet ',ibdyty

       PRINT*,'predicition de la vitesse:'
       PRINT*,bdyty(ibdyty)%V

       PRINT*,' '
       print*,'RHS'
       if (nbdime == 2) then
         write(*,'(2(1x,D12.5))') bdyty(ibdyty)%RHS
       else if (nbdime == 3) then
         write(*,'(3(1x,D12.5))') bdyty(ibdyty)%RHS
       endif       

       !PRINT*,' '
       !PRINT*,'la matrice'
       !PRINT*,bdyty(ibdyty)%KT%V

       PRINT*,'==========================='

     ENDIF


!
! contributions sur le second membre 
! contribution de ce qui reste des ddl imposes sur le second membre 

     ! vitesse libre defo

     bdyty(ibdyty)%Vfree = 0.d0
 
     call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%RHS)
 
 
     if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then
     
       ! on collecte les valeurs des ddl imposes
       DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof

           CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)

           iccdof=bdyty(ibdyty)%ccdof(inod)+idof 

           bdyty(ibdyty)%drvvalues(ivd) = bdyty(ibdyty)%vdriv(ivd) - bdyty(ibdyty)%V(iccdof)

       ENDDO
     
       ! on les passe au g_system
       call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)
     
     endif
     
     ! on resoud
     CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vfree,info)
     
     IF (info /= 0) THEN
         PRINT*,ibdyty
         CALL FATERR(IAM,'No solution')
     ENDIF

     bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vfree  + bdyty(ibdyty)%V

   ENDDO
   if (timer /= 0) call stop_itimer(timer)
   
   if (.not. is_contactdetectionconfiguration_defined) then

      call compute_configurationTT_poroMAILx

      !call logmes('poroMAILx::contact configuration automatically computed')

   endif

END SUBROUTINE compute_free_vlocy_poroMAILx
!------------------------------------------------------------------------
 
 !------------------------------------------------------------------------   
SUBROUTINE apply_poro_driven_dof(ibdyty,storage)

IMPLICIT NONE 
INTEGER :: ivd,ibdyty,inod,idof,iccdof
INTEGER :: storage

   IF (nb_poroMAILx == 0) RETURN

! computing driven dof
!  print *,'nb poro driven dof : ',bdyty(ibdyty)%nb_poro_driven_dof
  DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof

     CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)

     iccdof=bdyty(ibdyty)%ccdof(inod)+idof   

     SELECT CASE(storage)
       CASE(iV____)
         bdyty(ibdyty)%V(iccdof)=bdyty(ibdyty)%Vdriv(ivd)    
         bdyty(ibdyty)%X(iccdof)=bdyty(ibdyty)%Xdriv(ivd)
       CASE(iVfree)
         bdyty(ibdyty)%Vfree(iccdof)=bdyty(ibdyty)%Vdriv(ivd)
         !print *,'on met Vdriv dans Vfree : Vdriv : ',ivd, bdyty(ibdyty)%Vdriv(ivd)
         !print *,'on met Vdriv dans Vfree : Vfree : ',iccdof, bdyty(ibdyty)%Vfree(iccdof)
       CASE default
         call faterr('poroMAILx::apply_poro_driven_dof','unknown storage')
     END SELECT      

   END DO  

END SUBROUTINE apply_poro_driven_dof

!------------------------------------------------------------------------
 SUBROUTINE apply_drvdof_KT_poroMAILx
 IMPLICIT NONE

   INTEGER :: ibdyty,ivd,iccdof,idof,inod

   IF (nb_poroMAILx == 0) RETURN

   DO ibdyty=1,SIZE(bdyty)

     !write(*,'(A,I0)') 'Body: ',ibdyty

    ! CALL G_store(bdyty(ibdyty)%KT)

     if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then

         DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof
    
           CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)
    
           iccdof=bdyty(ibdyty)%ccdof(inod)+idof 
      
           !CALL G_apply_drvdof(bdyty(ibdyty)%KT,iccdof)
           
           bdyty(ibdyty)%drvdofs(ivd)=iccdof
    
         ENDDO
         
         call set_drvdofs(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvdofs)
     else
     
        call erase_drvdofs(bdyty(ibdyty)%g_sys)
        
     endif

   ENDDO

 END SUBROUTINE apply_drvdof_KT_poroMAILx

!------------------------------------------------------------------------
 SUBROUTINE compute_dof_poroMAILx 

   IMPLICIT NONE 
   INTEGER :: ibdyty,ivd,ibdy,inod,idof,iccdof,info,i
!                           12345678901234567890123
   CHARACTER(len=23) :: IAM='poroMAILX::compute_dof'
   character(len=20) ::fmt

   real(kind=8) :: UMTTH,TTH
   real(kind=8),dimension(:),allocatable :: RIreac 

   integer :: timer=0

   IF (nb_poroMAILx == 0) RETURN

                                            !01234567890123456789
   if (timer == 0) timer = get_new_itimer_ID('COMPUTE Dof         ')
   if (timer /= 0) call start_itimer(timer)

   UMTTH = (1.d0 - theta)*H
   TTH   = theta*H

  !
   DO ibdyty=1,SIZE(bdyty)    

     IF (.NOT. bdyty(ibdyty)%visible) CYCLE

     !PRINT*,' '
     !PRINT*,'Ireac'
     !PRINT*,bdyty(ibdyty)%Ireac

     bdyty(ibdyty)%Vlast = bdyty(ibdyty)%V  
     
       ! calcul partie defo
       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Ireac)
       !bdyty(ibdyty)%Vaux = bdyty(ibdyty)%Ireac  

       if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then
           bdyty(ibdyty)%drvvalues = 0.d0

           ! on les passe au g_system
           call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

       endif

       CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

       !CALL nullify_poro_driven_dof(ibdyty,iVaux_)
       !print *,'V aux'
       !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux
       !CALL G_solve_linear_system(bdyty(ibdyty)%KT,bdyty(ibdyty)%Vaux,info)
       !print *,'V aux'
       !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux
       IF (info /= 0) THEN
          PRINT*,ibdyty
          CALL FATERR(IAM,'No solution')
       ENDIF

       bdyty(ibdyty)%V=bdyty(ibdyty)%Vfree + bdyty(ibdyty)%Vaux

       CALL apply_poro_driven_dof(ibdyty,iV____)

     bdyty(ibdyty)%X = bdyty(ibdyty)%Xbegin + &
                     
                     (UMTTH*bdyty(ibdyty)%Vbegin)*bdyty(ibdyty)%Mask_No_ALE + &
                     (  TTH*bdyty(ibdyty)%V)*bdyty(ibdyty)%Mask_No_ALE + &
                     
                     (UMTTH*bdyty(ibdyty)%V_ALE_begin)*bdyty(ibdyty)%Mask_ALE + &
                     (  TTH*bdyty(ibdyty)%V_ALE)*bdyty(ibdyty)%Mask_ALE
    
     where(dabs(bdyty(ibdyty)%X)<1.d-24) bdyty(ibdyty)%X=0.d0

     !PRINT*,' '
     !PRINT*,'la vitesse'
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%V



     !PRINT*,' '
     !PRINT*,'le deplacement'
     !write(*,'(3(1x,D12.5))') bdyty(ibdyty)%X


   ENDDO

   if (timer /= 0) call stop_itimer(timer)

 END SUBROUTINE compute_dof_poroMAILx
!------------------------------------------------------------------------  

!------------------------------------------------------------------------ 
SUBROUTINE update_bulk_poroMAILx
  IMPLICIT NONE

  INTEGER :: errare,istop
  INTEGER :: ibdyty,iblmty
!                           1234567890123456789012
  CHARACTER(len=22) :: IAM='poroMAILX::update_bulk'

  IF (nb_poroMAILx == 0) RETURN

! calcul de la contrainte et mise a jour des grandeurs internes
!
  DO ibdyty=1,SIZE(bdyty)
    !
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
      bdyty(ibdyty)%blmty(iblmty)%Fint(:,2)=bdyty(ibdyty)%blmty(iblmty)%Fint(:,1)

!fd mesure salutaire

      bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) = 0.d0
      bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) = 0.d0

    ENDDO
    
    CALL update_porogpv_MAILx(bdyty2M_bdyty(ibdyty))
    
    
  ENDDO

 END SUBROUTINE update_bulk_poroMAILx


!------------------------------------------------------------------------ 
 SUBROUTINE compute_residue_norm_poroMAILx(norm_res)

   IMPLICIT NONE
   INTEGER      :: ibdyty,ibdy_V,ibdy_Res
   REAL(kind=8) :: tmp
   REAL(kind=8) :: max_dV,max_V,norm_V
   REAL(kind=8) :: max_res,max_reac,max_f,max_fint,max_finert,max_fext,norm_Res

   character(len=80) :: cout

   norm_res=1.d+20

   IF (nb_poroMAILx == 0) RETURN

   norm_Res = 0.d0
   ibdy_Res = 1
   norm_V   = 0.d0
   ibdy_V   = 1

   !!! Etude de la convergence !!!

   DO ibdyty=1,SIZE(bdyty)

     !print*,'Corps : ',ibdyty

     ! if (itchache) then
     !   print*,' '
     !   print*,'residu libre'
     !   print*,bdyty(ibdyty)%RHS
     !   print*,' '
     !   print*,'efforts de contact'
     !   print*,bdyty(ibdyty)%Ireac
     ! endif

     bdyty(ibdyty)%Vaux = bdyty(ibdyty)%V  - bdyty(ibdyty)%Vlast  

!fd debile ?
!fd
     max_dV=MAX(MAXVAL(bdyty(ibdyty)%Vaux),ABS(MINVAL(bdyty(ibdyty)%Vaux)))
!fd
     max_V=MAX(MAXVAL(bdyty(ibdyty)%V),ABS(MINVAL(bdyty(ibdyty)%V)))


     IF (max_V <= 1d-10 ) max_V=1.D0
     tmp=max_dV/max_V


!     if (itchache) then
!       print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!       print*,'Norme en vitesse :           '
!       print*,'max delta V =',max_dV
!       print*,'max V       =',max_V
!       print*,'norme en V  =',tmp
!       print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!
!     endif

     IF ( norm_V < tmp) THEN
       norm_V=tmp
       ibdy_V=ibdyty
     ENDIF

!fd 18/03/04 debile car doublon avec les deux lignes suivantes!
!     bdyty(ibdyty)%Vaux = bdyty(ibdyty)%RHS

!     call nullify_vlocy_driven_dof(ibdyty,iVaux_) 
!fd


     !print*,bdyty(ibdyty)%Fint

     bdyty(ibdyty)%Vaux = bdyty(ibdyty)%RHS + bdyty(ibdyty)%Ireac  

     CALL nullify_poro_driven_dof(ibdyty,iVaux_) 

     bdyty(ibdyty)%residu = bdyty(ibdyty)%Vaux

     max_res=MAX(MAXVAL(bdyty(ibdyty)%residu),ABS(MINVAL(bdyty(ibdyty)%residu)))

     max_fint =H*MAX(MAXVAL(bdyty(ibdyty)%Fint),ABS(MINVAL(bdyty(ibdyty)%Fint)))

     max_finert = H*MAX(MAXVAL(bdyty(ibdyty)%Finert),ABS(MINVAL(bdyty(ibdyty)%Finert)))

     max_fext =  H*MAX(MAXVAL(bdyty(ibdyty)%Fext),ABS(MINVAL(bdyty(ibdyty)%Fext)))

     max_f = MAX(max_fint,MAX(max_finert,max_fext))

     max_reac = MAX(MAXVAL(bdyty(ibdyty)%Ireac),ABS(MINVAL(bdyty(ibdyty)%Ireac)))

! bof bof
! bof bof

     IF (max_f <= 1d-01 ) max_f=1.D0
     tmp=max_res/max_f

!       if (itchache) then
!       print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!       print*,'Norme en residu :           '
!       print*,'max residu    =',max_res
!       print*,'max force int =',max_fint
!       print*,'max force inertie =',max_finert
!       print*,'max reac =',max_reac
!       print*,'norme en res  =',tmp
!       print*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!       endif

     IF (norm_Res < tmp) THEN
       norm_Res=tmp
       ibdy_Res=ibdyty
     ENDIF
   ENDDO

   write(cout,'(1X)')
   call logmes(cout)
   write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxRes/MaxFint = ',norm_RES,'body : ',ibdy_Res  
   call logmes(cout)
   write(cout,'(1X,A3,3X,A17,D10.3,3X,A7,I7)')  ' @ ','MaxDV /MaxV    = ',norm_V  ,'body : ',ibdy_V 
   call logmes(cout)
   write(cout,'(1X)')
   call logmes(cout)

 END SUBROUTINE compute_residue_norm_poroMAILx

!------------------------------------------------------------------------
  LOGICAL FUNCTION CHECK_poroMAILx(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome
    INTEGER :: nb_MAILx
    
    nb_MAILx = get_nb_MAILx()
    CHECK_poroMAILx = .FALSE.
    
    IF(nb_MAILx.NE.0) CHECK_poroMAILx = .TRUE.

  END FUNCTION CHECK_poroMAILx

!------------------------------------------------------------------------
 SUBROUTINE get_vector_poroMAILx(id_vect,ibdyty,vect,nbdof)
 IMPLICIT NONE

   INTEGER :: ibdyty,nbdof,iM_bdyty,inodty,iccdof,iM_nodty,iM_ccdof,mm,idof,iccdof_1,iccdof_2
   REAL(kind=8),DIMENSION(nbdof) :: vect
   CHARACTER(len=5) :: id_vect

   character(len=21) :: IAM
         !123456789012345678901
   IAM = 'poroMAILx::get_vector'

   IF (nb_poroMAILx == 0) RETURN
   
   vect = 0.d0
   
   SELECT CASE(id_vect)
    CASE('X____')
     
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
       
     ENDDO
     
    CASE('Xbeg_')
    
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO
     
    CASE('V____')
    
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%V(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO

    CASE('Vbeg_')
    
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO
     
     CASE('V_ALE')
    
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%V_ALE(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO

    CASE('VbALE')
    
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%V_ALE_begin(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO
     
    CASE('Vaux_')
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO

    CASE('Vfree')
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO

    CASE('Reac_')
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO
     
    CASE('Fext_')
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO

    CASE('Fint_')
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       vect(mm:mm+nbDIME-1) = bdyty(ibdyty)%Fint(iccdof+1:iccdof+nbDIME)
       mm = mm + nbDIME
     ENDDO

! Recuperation des infos sur l'inconnue scalaire

    CASE('P____')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
     !IF (nbdof /= bdyty(ibdyty)%nbdof - nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)

       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
         vect(mm) = bdyty(ibdyty)%V(iccdof+1+nbDIME)
         mm = mm + 1
       ELSE
         iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
         iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
         vect(mm) = 0.5d0 * ( bdyty(ibdyty)%V(iccdof_1+1+nbDIME) + bdyty(ibdyty)%V(iccdof_2+1+nbDIME) )
         mm = mm + 1
       ENDIF
     ENDDO


    CASE('Pbeg_')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)

       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
         vect(mm) = bdyty(ibdyty)%Vbegin(iccdof+1+nbDIME)
         mm = mm + 1
       ELSE
         iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
         iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
         vect(mm) = 0.5d0 * ( bdyty(ibdyty)%Vbegin(iccdof_1+1+nbDIME) + bdyty(ibdyty)%Vbegin(iccdof_2+1+nbDIME) )
         mm = mm + 1
       ENDIF
     ENDDO

    CASE('Qint_')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)

       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
         vect(mm) = bdyty(ibdyty)%Fint(iccdof+1+nbDIME)
         mm = mm + 1
       ELSE
         iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
         iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
         vect(mm) = 0.5d0 * ( bdyty(ibdyty)%Fint(iccdof_1+1+nbDIME) + bdyty(ibdyty)%Fint(iccdof_2+1+nbDIME) )
         mm = mm + 1
       ENDIF
     ENDDO

    CASE('Qext_')
     IF (nbdof /= bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)

       IF (nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)) > nbDIME) THEN
         vect(mm) = bdyty(ibdyty)%Fext(iccdof+1+nbDIME)
         mm = mm + 1
       ELSE
         iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
         iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
         vect(mm) = 0.5d0 * ( bdyty(ibdyty)%Fext(iccdof_1+1+nbDIME) + bdyty(ibdyty)%Fext(iccdof_2+1+nbDIME) )
         mm = mm + 1
       ENDIF
     ENDDO

    CASE('NodId')
     IF (nbdof /= nbDIME * bdyty(ibdyty)%nb_nodes ) THEN
       call faterr(IAM,'nbdof non concordant for id '//id_vect)
     ENDIF
     ! pour chaque noeud
     mm = 1
     DO inodty=1, bdyty(ibdyty)%nb_nodes
       ! on recupere l'indice qui debute la tranche concerant le noeud 
       ! courant dans un vecteur asssemble 
       iccdof=bdyty(ibdyty)%ccdof(inodty)
       DO idof=1,nbDIME
          vect(mm+idof-1) = bdyty(ibdyty)%nodnb(iccdof+idof)
       ENDDO
       mm = mm + nbDIME
     ENDDO

    CASE DEFAULT
     call faterr(IAM,'unknown id '//id_vect)
   END SELECT

 END SUBROUTINE get_vector_poroMAILx

! fonction qui recupere le nombre de noeud d'un body sur lequel est defini
! un modele de poro elasticite
function get_N_NODE_poroMAILx(ibdyty)

   implicit none

   ! variables d'entree :
   integer, intent(in) :: ibdyty ! indice d'un modele de thermique
   
   ! valeur de retour :
   integer :: get_N_NODE_poroMAILx ! nombre de noeud de l'element
   
   ! on renvoie le nombre de noeud pour cet element
   get_N_NODE_poroMAILx =bdyty(ibdyty)%nb_nodes

end function get_N_NODE_poroMAILx
!------------------------------------------------------------------------  
 SUBROUTINE set_Matrix_Storage_poroMAILx(type)
 IMPLICIT NONE
 character(len=8) :: type
 character(len=29) :: IAM

     !12345678901234567890123456789
 IAM='poroMAILx::set_matrix_storage'

 Matrix_storage=get_matrix_storage_id_from_name(type)

 if (Matrix_storage == -99) call faterr(IAM,'unknown storage type '//type)

 END SUBROUTINE set_Matrix_Storage_poroMAILx
!------------------------------------------------------------------------  
 SUBROUTINE set_Matrix_Shape_poroMAILx(type)
 IMPLICIT NONE
 character(len=8) :: type
 character(len=27) :: IAM

     !123456789012345678901234567
 IAM='poroMAILx::set_matrix_shape'

 Matrix_shape=get_matrix_shape_id_from_name(type)

 if (Matrix_storage == -99) call faterr(IAM,'unknown shape type '//type)

 END SUBROUTINE set_Matrix_Shape_poroMAILx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE set_meca_field_bynode(ibdyty,field_rank,fsize,field)
   IMPLICIT NONE

!!   character(len=30),intent(in)             :: name  ! en attendant de savoir retrouver le rang de facon efficace
   INTEGER,INTENT(in)                       :: ibdyty,fsize,field_rank
   REAL(kind=8),INTENT(in),DIMENSION(fsize) :: field

   INTEGER :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare

   REAL(kind=8),ALLOCATABLE,DIMENSION(:) :: efield_gp,efield_node

                            !123456789012345
   CHARACTER(len=15) :: IAM='set_field_bynode'

   IF (nb_poroMAILx == 0) RETURN

   IF (fsize /= bdyty(ibdyty)%nb_nodes) THEN
     CALL FATERR(IAM,'non conforming vector fsize')
   ENDIF

   !fd interpolation de field au noeud a field au pg

   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(efield_node(bdyty(ibdyty)%blmty(iblmty)%meca_node),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating efield_node')
      END IF

      DO in=1,bdyty(ibdyty)%blmty(iblmty)%meca_node
        efield_node(in) = field(bdyty(ibdyty)%blmty(iblmty)%NODES(in))
      ENDDO

      imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb
      ALLOCATE(efield_gp(get_N_GP_poroEF(modelz(imodel)%ID, 'MECA')),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating efield_gp')
      END IF

      !
      ! on passe des noeuds aux pg
      ! 

      CALL interpolate_node2pg(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                               efield_node,efield_gp,'MECA')       

      !fd on pose dans la bd mailx

      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

      DO ig=1,get_N_GP_poroEF(modelz(imodel)%ID, 'MECA')
         CALL set_poro_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(ig),'MECA')
      ENDDO 
      DEALLOCATE(efield_node,efield_gp)
   ENDDO

END SUBROUTINE set_meca_field_bynode

! Set value of an external meca field on an element (same value for all Gauss points)
subroutine set_meca_field_byelem(ibdyty,field_rank,fsize,field)
  implicit none
  !> body on which to set field value
  integer(kind=4), intent(in) :: ibdyty
  !> rank of the field ot set
  integer(kind=4), intent(in) :: field_rank
  !> size of input field array (must be nb_elem)
  integer(kind=4), intent(in) :: fsize
  !> field values per element
  real(kind=8), intent(in), dimension(fsize) :: field
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel
  character(len=21) :: IAM
       !123456789012345678901
  IAM ='set_meca_field_byelem'

  if (nb_poroMAILx == 0) return

  if (fsize /= size(bdyty(ibdyty)%blmty)) then
    call FATERR(IAM,'non conforming vector fsize')
  end if

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_bdyty = bdyty2M_bdyty(ibdyty)
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_poroEF(modelz(imodel)%ID,'MECA')
      call set_poro_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,field(iblmty),'MECA')
    end do 
  end do

end subroutine set_meca_field_byelem

!------------------------------------------------------------------------ 
 SUBROUTINE set_ther_field_bynode(ibdyty,field_rank,fsize,field)
   IMPLICIT NONE

!!   character(len=30),intent(in)             :: name  ! en attendant de savoir retrouver le rang de facon efficace
   INTEGER,INTENT(in)                       :: ibdyty,fsize,field_rank
   REAL(kind=8),INTENT(in),DIMENSION(fsize) :: field

   INTEGER :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare

   REAL(kind=8),ALLOCATABLE,DIMENSION(:) :: efield_gp,efield_node

                            !123456789012345
   CHARACTER(len=15) :: IAM='set_field_bynode'

   IF (nb_poroMAILx == 0) RETURN

   IF (fsize /= bdyty(ibdyty)%nb_nodes) THEN
     CALL FATERR(IAM,'non conforming vector fsize')
   ENDIF

   !fd interpolation de field au noeud a field au pg

   DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      !
      ALLOCATE(efield_node(bdyty(ibdyty)%blmty(iblmty)%ther_node),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating efield_node')
      END IF

      DO in=1,bdyty(ibdyty)%blmty(iblmty)%ther_node
        efield_node(in) = field(bdyty(ibdyty)%blmty(iblmty)%NODES(in))
      ENDDO

      imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb
      ALLOCATE(efield_gp(get_N_GP_poroEF(modelz(imodel)%ID, 'THER')),stat=errare)

      IF (errare /= 0) THEN
        CALL FATERR(IAM,'allocating efield_gp')
      END IF

      !
      ! on passe des noeuds aux pg
      ! 

      CALL interpolate_node2pg(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                               efield_node,efield_gp,'THER')   

      !fd on pose dans la bd mailx

      iM_bdyty = bdyty2M_bdyty(ibdyty)
      iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

      DO ig=1,get_N_GP_poroEF(modelz(imodel)%ID, 'THER')
         CALL set_poro_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(ig),'THER')
      ENDDO 
      DEALLOCATE(efield_node,efield_gp)
   ENDDO

END SUBROUTINE set_ther_field_bynode

! Set value of an external ther field on an element (same value for all Gauss points)
subroutine set_ther_field_byelem(ibdyty,field_rank,fsize,field)
  implicit none
  !> body on which to set field value
  integer(kind=4), intent(in) :: ibdyty
  !> rank of the field ot set
  integer(kind=4), intent(in) :: field_rank
  !> size of input field array (must be nb_elem)
  integer(kind=4), intent(in) :: fsize
  !> field values per element
  real(kind=8), intent(in), dimension(fsize) :: field
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel
  character(len=21) :: IAM
       !123456789012345678901
  IAM ='set_ther_field_byelem'

  if (nb_poroMAILx == 0) return

  if (fsize /= size(bdyty(ibdyty)%blmty)) then
    call FATERR(IAM,'non conforming vector fsize')
  end if

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_bdyty = bdyty2M_bdyty(ibdyty)
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_poroEF(modelz(imodel)%ID,'THER')
      call set_poro_field_MAILx(iM_bdyty,iM_blmty,ig,field_rank,field(iblmty),'THER')
    end do 
  end do

end subroutine set_ther_field_byelem

!------------------------------------------------------------------------ 
!> \brief Set a meca vector field by nodes
!> Values at nodes are used to interpolate and store
!> corresponding value at each Gauss point
subroutine set_meca_vfield_bynode(ibdyty,field_rank,vfield,dim1,dim2)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> vector field index
  integer(kind=4), intent(in) :: field_rank
  !> first dimension of input field array (vector field size)
  integer(kind=4), intent(in) :: dim1
  !> second dimension of input field array (number of nodes)
  integer(kind=4), intent(in) :: dim2
  !> input field value
  real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare,i_f
  real(kind=8), dimension(:,:), allocatable :: efield_gp,efield_node

  character(len=22) :: IAM
        !1234567890123456789012
  IAM = 'set_meca_vfield_bynode'

  if (nb_poroMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_poroMAILx ) call faterr(IAM,'wrong poroMAILx index')

  if (dim2/= bdyty(ibdyty)%nb_nodes) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  !fd interpolation de field au noeud a field au pg
  do iblmty = 1, size(bdyty(ibdyty)%blmty)

     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

     ! here ?
     if (dim1 > get_meca_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
       call faterr(IAM,'input vector field too large')
     end if

     allocate(efield_node(size(bdyty(ibdyty)%blmty(iblmty)%NODES),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_node')
     end if

     do in = 1, size(bdyty(ibdyty)%blmty(iblmty)%NODES)
       efield_node(in,:) = vfield(:,bdyty(ibdyty)%blmty(iblmty)%NODES(in))
     end do

     imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
     allocate(efield_gp(get_N_GP_poroEF(modelz(imodel)%ID,'MECA'),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_gp')
     end if

     !
     ! on passe des noeuds aux pg
     ! 
     do i_f = 1, dim1
       call interpolate_node2pg(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                efield_node(:,i_f),efield_gp(:,i_f),'MECA')
     end do

     !fd on pose dans la bd mailx

     do ig = 1,get_N_GP_poroEF(modelz(imodel)%ID,'MECA')
       call set_poro_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(:,ig),dim1,'MECA')
     end do

     deallocate(efield_node,efield_gp)
  end do

end subroutine set_meca_vfield_bynode
!------------------------------------------------------------------------
!> \brief Set a meca vector field by at constant value on the element
subroutine set_meca_vfield_byelem(ibdyty,field_rank,vfield,dim1,dim2)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> vector field index
  integer(kind=4), intent(in) :: field_rank
  !> first dimension of input field array (vector field size)
  integer(kind=4), intent(in) :: dim1
  !> second dimension of input field array (number of elements)
  integer(kind=4), intent(in) :: dim2
  !> input field value
  real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel
  character(len=22) :: IAM
       !1234567890123456789012
  IAM ='set_meca_vfield_byelem'

  if (nb_poroMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_poroMAILx ) call faterr(IAM,'wrong poroMAILx index')

  if (dim2/= size(bdyty(ibdyty)%blmty)) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    if (dim1 > get_meca_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
      call faterr(IAM,'input vector field too large')
    end if

    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_poroEF(modelz(imodel)%ID,'MECA')
      call set_poro_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,vfield(:,iblmty),dim1,'MECA')
    end do 
  end do

end subroutine set_meca_vfield_byelem
!------------------------------------------------------------------------ 
!> \brief Set a ther vector field by nodes
!> Values at nodes are used to interpolate and store
!> corresponding value at each Gauss point
subroutine set_ther_vfield_bynode(ibdyty,field_rank,vfield,dim1,dim2)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> vector field index
  integer(kind=4), intent(in) :: field_rank
  !> first dimension of input field array (vector field size)
  integer(kind=4), intent(in) :: dim1
  !> second dimension of input field array (number of nodes)
  integer(kind=4), intent(in) :: dim2
  !> input field value
  real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel,in,errare,i_f
  real(kind=8), dimension(:,:), allocatable :: efield_gp,efield_node

  character(len=22) :: IAM
        !1234567890123456789012
  IAM = 'set_ther_vfield_bynode'

  if (nb_poroMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_poroMAILx ) call faterr(IAM,'wrong poroMAILx index')

  if (dim2/= bdyty(ibdyty)%nb_nodes) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  !fd interpolation de field au noeud a field au pg
  do iblmty = 1, size(bdyty(ibdyty)%blmty)

     iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

     ! here ?
     if (dim1 > get_ther_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
       call faterr(IAM,'input vector field too large')
     end if

     allocate(efield_node(size(bdyty(ibdyty)%blmty(iblmty)%NODES),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_node')
     end if

     do in = 1, size(bdyty(ibdyty)%blmty(iblmty)%NODES)
       efield_node(in,:) = vfield(:,bdyty(ibdyty)%blmty(iblmty)%NODES(in))
     end do

     imodel = bdyty(ibdyty)%blmty(iblmty)%mdlnb
     allocate(efield_gp(get_N_GP_poroEF(modelz(imodel)%ID,'THER'),dim1),stat=errare)

     if (errare /= 0) then
       call faterr(IAM,'allocating efield_gp')
     end if

     !
     ! on passe des noeuds aux pg
     ! 
     do i_f = 1, dim1
       call interpolate_node2pg(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                efield_node(:,i_f),efield_gp(:,i_f),'THER')
     end do

     !fd on pose dans la bd mailx

     do ig = 1,get_N_GP_poroEF(modelz(imodel)%ID,'THER')
       call set_poro_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,efield_gp(:,ig),dim1,'THER')
     end do

     deallocate(efield_node,efield_gp)
  end do

end subroutine set_ther_vfield_bynode
!------------------------------------------------------------------------
!> \brief Set a ther vector field by at constant value on the element
subroutine set_ther_vfield_byelem(ibdyty,field_rank,vfield,dim1,dim2)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> vector field index
  integer(kind=4), intent(in) :: field_rank
  !> first dimension of input field array (vector field size)
  integer(kind=4), intent(in) :: dim1
  !> second dimension of input field array (number of elements)
  integer(kind=4), intent(in) :: dim2
  !> input field value
  real(kind=8), dimension(dim1,dim2), intent(in) :: vfield
  !
  integer(kind=4) :: iblmty,ig,iM_bdyty,iM_blmty,imodel
  character(len=22) :: IAM
       !1234567890123456789012
  IAM ='set_ther_vfield_byelem'

  if (nb_poroMAILx == 0) return
  if ( ibdyty<1 .or. ibdyty>nb_poroMAILx ) call faterr(IAM,'wrong poroMAILx index')

  if (dim2/= size(bdyty(ibdyty)%blmty)) then
    call faterr(IAM,'non conforming vector fsize')
  end if

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  do iblmty = 1, size(bdyty(ibdyty)%blmty)

    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    if (dim1 > get_ther_vfield_max_size_MAILx(iM_bdyty,iM_blmty) ) then
      call faterr(IAM,'input vector field too large')
    end if

    imodel   = bdyty(ibdyty)%blmty(iblmty)%mdlnb

    do ig = 1, get_N_GP_poroEF(modelz(imodel)%ID,'THER')
      call set_poro_vfield_MAILx(iM_bdyty,iM_blmty,ig,field_rank,vfield(:,iblmty),dim1,'THER')
    end do 
  end do

end subroutine set_ther_vfield_byelem
!------------------------------------------------------------------------
!> Get rank of an external scalar field of an element
integer(kind=4) function get_meca_field_rank(ibdyty,iblmty,name)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> element index
  integer(kind=4), intent(in) :: iblmty
  !> name of field to get rank of
  character(len=*), intent(in) :: name
  !
  integer(kind=4) :: iM_bdyty,iM_blmty

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_meca_field_rank = get_meca_field_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
!------------------------------------------------------------------------
!> Get rank of an external vector field of an element
integer(kind=4) function get_meca_vfield_rank(ibdyty,iblmty,name)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> element index
  integer(kind=4), intent(in) :: iblmty
  !> name of field to get rank of
  character(len=*), intent(in) :: name
  !
  integer(kind=4) :: iM_bdyty,iM_blmty

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_meca_vfield_rank = get_meca_vfield_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
!------------------------------------------------------------------------
!> Get rank of an external scalar field of an element
integer(kind=4) function get_ther_field_rank(ibdyty,iblmty,name)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> element index
  integer(kind=4), intent(in) :: iblmty
  !> name of field to get rank of
  character(len=*), intent(in) :: name
  !
  integer(kind=4) :: iM_bdyty,iM_blmty

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_ther_field_rank = get_ther_field_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
!------------------------------------------------------------------------
!> Get rank of an external vector field of an element
integer(kind=4) function get_ther_vfield_rank(ibdyty,iblmty,name)
  implicit none
  !> body index
  integer(kind=4), intent(in) :: ibdyty
  !> element index
  integer(kind=4), intent(in) :: iblmty
  !> name of field to get rank of
  character(len=*), intent(in) :: name
  !
  integer(kind=4) :: iM_bdyty,iM_blmty

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

  get_ther_vfield_rank = get_ther_vfield_rank_MAILx(iM_bdyty,iM_blmty,name)

end function
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------

 SUBROUTINE load_ALE_poroMAILx(ibdyty)
 
IMPLICIT NONE 
INTEGER :: itempo,inodty,iblmty,idof,iccdof,ibdyty,num
INTEGER :: mdlnb, inull,ialedof,errare, nb_solid, nb_fluid
INTEGER,DIMENSION(:),ALLOCATABLE    :: blmty_solid
INTEGER,DIMENSION(:),ALLOCATABLE    :: blmty_fluid
                          !1234567890123456789
CHARACTER(len=19)  :: IAM='poroMAILx::load_ALE'

IF (nb_poroMAILx == 0) RETURN
  
! Creation du Mask ALE recuperation des ddl du solide

nb_solid = 0
nb_fluid = 0
DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    CALL get_ppset_value(bdyty(ibdyty)%blmty(iblmty)%ppsnb(1),mdlnb,inull)
    SELECT CASE(get_eleop_value(mdlnb,'type_'))
        CASE('fluid')
            nb_fluid = nb_fluid + 1
        CASE('solid')
            nb_solid = nb_solid + 1
    END SELECT
ENDDO

ALLOCATE(blmty_solid(nb_solid),stat=errare)
IF (errare/=0) THEN
  CALL FATERR(IAM,'blmty_solid')
END IF
blmty_solid = 0


ALLOCATE(blmty_fluid(nb_fluid),stat=errare)
IF (errare/=0) THEN
  CALL FATERR(IAM,'blmty_fluid')
END IF
blmty_fluid = 0


nb_solid = 0
nb_fluid = 0
DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
    CALL get_ppset_value(bdyty(ibdyty)%blmty(iblmty)%ppsnb(1),mdlnb,inull)
    SELECT CASE(get_eleop_value(mdlnb,'type_'))
        CASE('fluid')
            nb_fluid = nb_fluid + 1
            blmty_fluid(nb_fluid) = iblmty
        CASE('solid')
            nb_solid = nb_solid + 1
            blmty_solid(nb_solid) = iblmty
    END SELECT
ENDDO

DO num=1,SIZE(blmty_fluid)
    iblmty = blmty_fluid(num)
    DO itempo=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
        DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
            iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
            IF (idof < nbDIME +1) THEN
                bdyty(ibdyty)%Mask_ALE(iccdof) = 1
                bdyty(ibdyty)%Mask_No_ALE(iccdof) = 0
            ENDIF
        ENDDO
    ENDDO
ENDDO

DO num=1,SIZE(blmty_solid)
    iblmty = blmty_solid(num)
    DO itempo=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
        DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
            iccdof = bdyty(ibdyty)%ccdof(inodty)+idof
            bdyty(ibdyty)%Mask_ALE(iccdof) = 0
            bdyty(ibdyty)%Mask_No_ALE(iccdof) = 1
        ENDDO
    ENDDO
ENDDO
    
END SUBROUTINE load_ALE_poroMAILx
 

! Routine pour imposer les conditions aux limites par des champs externes
!------------------------------------------------------------------------
 SUBROUTINE set_vlocy_drvdof_poroMAILx(ibdyty,ndof,node, Vdriven)
 IMPLICIT NONE

   
   REAL(kind=8),INTENT(in) :: Vdriven
   INTEGER     ,INTENT(in) :: ibdyty, node, ndof
   REAL(kind=8)            :: Vdrivenbegin, Xdrivenbegin, Xdriven
   INTEGER                 :: ivd,idof,inod,idrvdof,trouve
   
   character(len=26) :: IAM
   !      12345678901234567890123456
   IAM = 'set_vlocy_drvdof_poroMAILx'
   
   
   IF (nb_poroMAILx == 0) RETURN
    
   trouve = 1
   
   DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof
       
       CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)
      
       IF ((inod==node) .AND. (idof==ndof)) THEN
          
          !print *,'Application DRV DOF noeud : ',inod, ' dof : ',idof, ' value : ',nodal_value
          idrvdof = bdyty(ibdyty)%ccdof(inod)+idof
          
          Vdrivenbegin = bdyty(ibdyty)%Vbegin(idrvdof)
          Xdrivenbegin = bdyty(ibdyty)%Xbegin(idrvdof)
          
          Xdriven      = Xdrivenbegin + (1.D0-THETA)*H*Vdrivenbegin+THETA*H*Vdriven
          bdyty(ibdyty)%Vdriv(ivd) = Vdriven
          bdyty(ibdyty)%Xdriv(ivd) = Xdriven 

          CALL apply_poro_driven_dof(ibdyty,iVfree)
          
          trouve = 0
          
       ENDIF

   ENDDO
   
   IF (trouve== 1) CALL faterr(IAM,'driven dof index not found')
   
 END SUBROUTINE set_vlocy_drvdof_poroMAILx

!------------------------------------------------------------------------
 SUBROUTINE get_2DNodalStress_poroMAILx(ibdyty,S)

  IMPLICIT NONE
  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: S
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty
                           !1234567890123456789012345678
  CHARACTER(len=28) :: IAM='poroMAILx::get_2DNodalStress'

   !fd cette routine effectue un lissage aux noeuds des contraintes calculees aux points de gauss
   !fd il n'est pas sur qu'elle devrait se trouver ici ...
   !fd bref ca ne me plait pas.
  
  ! allocation dans routine appelante (5,nb_nodes)
  ! stockage a la MatLib Sxx,Syy,Sxy,Szz,Svm

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp


  iM_bdyty = bdyty2M_bdyty(ibdyty)

  S = 0.d0

  nbfields = 5

  IF ( nbfields /= SIZE(S,dim=1)) THEN
    PRINT *,nbfields,SIZE(S,dim=1)
    CALL FATERR(IAM,'Non conforming size first dim')
  ENDIF

  DO iblmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                     bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                     iM_bdyty,iM_blmty,2,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO   

    DEALLOCATE(temp)

  ENDDO 

  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    S(:,inodty)=S(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO   

END SUBROUTINE get_2DNodalStress_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!------------------------------------------------------------------------
SUBROUTINE get_2DNodalStrain_poroMAILx(ibdyty,E)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: E
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty

                            !1234567890123456789012345678
   CHARACTER(len=28) :: IAM='poroMAILx::get_2DNodalStrain'

  ! allocation dans routine appelante (5,nb_nodes)
  ! Epsxx,Epsyy,Epsxy,Epszz,J

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  E = 0.d0

  nbfields=5

  IF ( nbfields /= SIZE(E,dim=1)) THEN
    PRINT *,nbfields,SIZE(E,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  DO iblmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
 
    ! allocation de la table de connectivite
    !inodty=SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
    !ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%NODES(inodty),stat=errare)
     
     
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)


    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0
    
    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                   bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                   iM_bdyty,iM_blmty,1,temp,NbFields,NbNodes_stored)
 
    DO inodty=1,NbNodes_stored
       E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

 
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    E(:,inodty)=E(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO    

END SUBROUTINE get_2DNodalStrain_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE get_3DNodalStress_poroMAILx(ibdyty,S,requiredfield)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,requiredfield
  INTEGER :: iM_bdyty,iM_blmty
!                            1234567890123456789012345678
   CHARACTER(len=28) :: IAM='mecaMAILX::get_3DNodalStress'

!fd cette routine effectue un lissage aux noeuds des contraintes calculees aux points
!fd il n'est pas sur qu'elle devrait se trouver ici ...
!fd bref ca ne me plait pas.

  ! allocation dans routine appelante (7,nb_nodes)
  ! stockage a la MatLib Sxx,Sxy,Syy,Sxz,Syz,Szz,Svm

  REAL(kind=8),DIMENSION(:,:) :: S

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  S = 0.d0

  nbfields = 7

  IF ( nbfields /= SIZE(S,dim=1)) THEN
    PRINT *,nbfields,SIZE(S,dim=1)
    CALL FATERR(IAM,'Non conforming size first dim')
  ENDIF

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                    bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                    iM_bdyty,iM_blmty,requiredfield,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=S(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    !print*,inodty,S(6,inodty),SIZE(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
    S(:,inodty)=S(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO    

END SUBROUTINE get_3DNodalStress_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
SUBROUTINE get_3DNodalStrain_poroMAILx(ibdyty,E,requiredfield)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,requiredfield
  INTEGER :: iM_bdyty,iM_blmty
!                            1234567890123456789012345678
   CHARACTER(len=28) :: IAM='mecaMAILX::get_3DNodalStrain'

  ! allocation dans routine appelante (6,nb_nodes)
  ! Exx,Exy,Eyy,Exz,Eyz,Ezz

  REAL(kind=8),DIMENSION(:,:) :: E

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  E = 0.d0

  nbfields=7

  IF ( nbfields /= SIZE(E,dim=1)) THEN
    PRINT *,nbfields,SIZE(E,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  iM_bdyty = bdyty2M_bdyty(ibdyty)
  
  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
 
    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)

    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0


    CALL gpv2node_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                    bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                    iM_bdyty,iM_blmty,requiredfield,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

  DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)
    E(:,inodty)=E(:,inodty)/SIZE(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
  ENDDO    

  
END SUBROUTINE get_3DNodalStrain_poroMAILx
!------------------------------------------------------------------------

!!!------------------------------------------------------------------------    
 SUBROUTINE add_reac_nodty_poroMAILx(ibdyty,inodty,reac,storage)

   !
   ! called by injj
   !
  
   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty,iccdof,idof
   INTEGER :: storage

   REAL(kind=8),DIMENSION(nbDIME)  :: reac
   REAL(kind=8),DIMENSION(nbDIME)  :: raux

   iccdof=bdyty(ibdyty)%ccdof(inodty)


   ! calcul de la reaction totale deformable
   !contribution deformable
    
     SELECT CASE(storage)
     CASE (iIreac)
       DO idof=1,nbDIME
         bdyty(ibdyty)%Ireac(iccdof+idof)=  &
         bdyty(ibdyty)%Ireac(iccdof+idof)+reac(idof)
       END DO
     CASE (iIaux_)
       DO idof=1,nbDIME
         bdyty(ibdyty)%Iaux(iccdof+idof)=   &
         bdyty(ibdyty)%Iaux(iccdof+idof)+reac(idof)
       END DO
    
       !write(*,'(A,3(1x,D12.5))') 'Raux', bdyty(ibdyty)%Raux(iccdof+1:iccdof+nbDIME)
       !print*,'--'
    
     CASE (iFext_)
       DO idof=1,nbDIME
         bdyty(ibdyty)%Fext(iccdof+idof)=   &
         bdyty(ibdyty)%Fext(iccdof+idof)+reac(idof)
       END DO
    
    
     CASE default
         call faterr('poroMAILx::add_reac_nodty','unknown storage')
     END SELECT 

   !print*,'injj >'

END SUBROUTINE add_reac_nodty_poroMAILx

!------------------------------------------------------------------------  
SUBROUTINE nullify_reac_poroMAILx(ibdyty,storage)

  !
  ! called by vitrad
  !
  
  IMPLICIT NONE 
  INTEGER :: ibdyty
  INTEGER :: storage
  
   IF (nb_poroMAILx == 0) RETURN

  SELECT CASE(storage)
    CASE (iIreac)
 
      !print*,'nullify reac'

      bdyty(ibdyty)%Ireac = 0.d0

    CASE (iIaux_)
      bdyty(ibdyty)%Iaux = 0.d0

    CASE default

      call faterr('poroMAILx::nullify_reac','unknown storage')
  END SELECT 
  
END SUBROUTINE nullify_reac_poroMAILx
!------------------------------------------------------------------------

!------------------------------------------------------------------------  
SUBROUTINE nullify_vlocy_poroMAILx(ibdyty,storage)

  !
  ! called by SDL solver
  !
  
  IMPLICIT NONE 
  INTEGER :: ibdyty
  INTEGER :: storage
  
   IF (nb_poroMAILx == 0) RETURN

  SELECT CASE(storage)
    CASE (iVaux_)
      bdyty(ibdyty)%Vaux = 0.D0
    CASE default
      call faterr('poroMAILx::nullify_vlocy','unknown storage')
  END SELECT 
  
END SUBROUTINE nullify_vlocy_poroMAILx
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------
 INTEGER FUNCTION get_entity_poroMAILx(ibdyty)

    IMPLICIT NONE

    INTEGER,INTENT(in)       :: ibdyty

    get_entity_poroMAILx = nb_existing_entities + ibdyty

 END FUNCTION get_entity_poroMAILx

!------------------------------------------------------------------------

!------------------------------------------------------------------------
FUNCTION get_V_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_V_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  get_V_nodty_poroMAILx = bdyty(ibdyty)%V(iccdof+1:iccdof+nbDIME)

END FUNCTION get_V_nodty_poroMAILx
!------------------------------------------------------------------------

!------------------------------------------------------------------------
FUNCTION get_Vbegin_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vbegin_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc


  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)


  get_Vbegin_nodty_poroMAILx = bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbDIME)

END FUNCTION get_Vbegin_nodty_poroMAILx
!------------------------------------------------------------------------

!------------------------------------------------------------------------
FUNCTION get_Vfree_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vfree_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  get_Vfree_nodty_poroMAILx = bdyty(ibdyty)%Vfree(iccdof+1:iccdof+nbDIME)


END FUNCTION get_Vfree_nodty_poroMAILx
!------------------------------------------------------------------------

!------------------------------------------------------------------------
FUNCTION get_Vaux_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Vaux_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  REAL(kind=8),DIMENSION(nbDIME) :: Vaux,dloc

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  nbdof=MIN(nbdof,nbDIME)

  get_Vaux_nodty_poroMAILx = bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbDIME)

END FUNCTION get_Vaux_nodty_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!> 
FUNCTION get_Xbegin_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_Xbegin_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof

  iccdof=bdyty(ibdyty)%ccdof(inodty)
  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

  ! attention au cafouillage entre nbdof et nbDIME
  ! ici on remonte 1:nbdime
  nbdof=MIN(nbdof,nbDIME)

  get_Xbegin_nodty_poroMAILx = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbDIME)


END FUNCTION get_Xbegin_nodty_poroMAILx

!------------------------------------------------------------------------
!> 
FUNCTION get_X_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_X_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof

  iccdof=bdyty(ibdyty)%ccdof(inodty)
  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

  ! attention au cafouillage entre nbdof et nbDIME
  ! ici on remonte 1:nbdime
  nbdof=MIN(nbdof,nbDIME)

  get_X_nodty_poroMAILx = bdyty(ibdyty)%X(iccdof+1:iccdof+nbDIME)


END FUNCTION get_X_nodty_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 !> allows contactors to declare support nodes  
 SUBROUTINE set_precon_node_poroMAILx(ibdyty,inodty)
   IMPLICIT NONE

   INTEGER ::   ibdyty,  inodty

!   PRINT*,ibdyty,bdyty(ibdyty)%is_precon

   IF (.NOT. bdyty(ibdyty)%is_precon) RETURN 


!   PRINT*,"poroMAILx: ",ibdyty,"Noeud: ",inodty

   bdyty(ibdyty)%nodes_precon(inodty) = 1

 END SUBROUTINE set_precon_node_poroMAILx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 !> declares that on this body a reduced (on contact dofs) W matrix will be evaluated  
 SUBROUTINE set_precon_body_poroMAILx (iM_bdyty)
   IMPLICIT NONE
   INTEGER :: iM_bdyty
   INTEGER ::   ibdyty

   PRINT*,'Le corps ',iM_bdyty,' est precon'

   ibdyty=M2poro(iM_bdyty)%bdyty
   bdyty(ibdyty)%is_precon = .TRUE.

   ALLOCATE(bdyty(ibdyty)%nodes_precon(bdyty(ibdyty)%nb_nodes))
   bdyty(ibdyty)%nodes_precon = 0

 END SUBROUTINE set_precon_body_poroMAILx
!!!PTA---------------------------------------------------------------------
  LOGICAL FUNCTION get_visible_poroMAILx(ibdyty)

    IMPLICIT NONE
    INTEGER :: ibdyty

    get_visible_poroMAILx = bdyty(ibdyty)%visible
    
  END FUNCTION get_visible_poroMAILx
!!!PTA---------------------------------------------------------------------
  SUBROUTINE  set_visible_poroMAILx(ibdyty,FLAG)

    IMPLICIT NONE
    INTEGER :: ibdyty
    LOGICAL :: FLAG

    bdyty(ibdyty)%visible = FLAG
    
  END SUBROUTINE set_visible_poroMAILx
!!!------------------------------------------------------------------------
  CHARACTER(len=5) FUNCTION get_color_poroMAILx(ibdyty,itacty)

    IMPLICIT NONE

    INTEGER :: ibdyty,itacty
    INTEGER :: iM_bdyty !,iM_tacty

    iM_bdyty=bdyty2M_bdyty(ibdyty)

    get_color_poroMAILx=get_color_MAILx(iM_bdyty,itacty)

  END FUNCTION get_color_poroMAILx
!!!------------------------------------------------------------------------
!------------------------------------------------------------------------
!> returns reference coordinates
FUNCTION get_cooref_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_cooref_nodty_poroMAILx
  ! ***
  INTEGER                        :: iM_bdyty,iM_nodty


  iM_bdyty=bdyty2M_bdyty(ibdyty)
  iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)

  get_cooref_nodty_poroMAILx = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

END FUNCTION get_cooref_nodty_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!> returns contact detection coordinates
FUNCTION get_coorTT_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: get_coorTT_nodty_poroMAILx
  ! ***

  get_coorTT_nodty_poroMAILx = bdyty(ibdyty)%coorTT(:,inodty)

END FUNCTION get_coorTT_nodty_poroMAILx
!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
   FUNCTION get_RcoorTT_poroMAILx(ibdyty,itacty)
    IMPLICIT NONE
    INTEGER :: ibdyty
    INTEGER,OPTIONAL :: itacty !fd je ne sais pas a quoi ca sert ...
    REAL(kind=8) :: get_RcoorTT_poroMAILx(3)

    get_RcoorTT_poroMAILx = bdyty(ibdyty)%RcoorTT

  END FUNCTION
!!!------------------------------------------------------------------------
!------------------------------------------------------------------------
!> computes contact detection coordinates
subroutine compute_configurationTT_poroMAILx
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8),DIMENSION(nbDIME) :: coortt,spin
  ! ***
  INTEGER                        :: iM_bdyty,iM_nodty,iccdof
  REAL(kind=8),DIMENSION(nbDIME) :: cooref, Xbegin, Vbegin, coorloc, V

  if (nb_poroMAILx == 0) return

  if (.not. is_contactdetectionconfiguration_defined) then
    vw_b = 1.d0 - theta
    vw_e = 0.
    !call logmes('poroMAILx::default contact configuration is used')
  endif

  do ibdyty=1,nb_poroMAILx 

    if (.not. bdyty(ibdyty)%visible) cycle
  
    iM_bdyty=bdyty2M_bdyty(ibdyty)

    do inodty=1,bdyty(ibdyty)%nb_nodes

        iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
        cooref=get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

        Xbegin = get_Xbegin_nodty_poroMAILx(ibdyty,inodty)

        Vbegin = get_Vbegin_nodty_poroMAILx(ibdyty,inodty)
        V      = get_V_nodty_poroMAILx(ibdyty,inodty)

        bdyty(ibdyty)%coorTT(:,inodty) = cooref + Xbegin + (H*(vw_b*Vbegin + vw_e*V))
    enddo
      !write(*,'(3(1x,D12.5))')  bdyty(ibdyty)%coorTT
  enddo
END subroutine compute_configurationTT_poroMAILx

!------------------------------------------------------------------------

!!!------------------------------------------------------------------------    
 function get_reac_nodty_poroMAILx(ibdyty,inodty,storage)

   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty
   REAL(kind=8),DIMENSION(nbDIME)  :: get_reac_nodty_poroMAILx
   INTEGER :: storage
   ! ****
   integer :: iccdof
                            !12345678901234567890123456
   character(len=26) :: IAM='poroMAILx::get_reac_nodty'

   iccdof=bdyty(ibdyty)%ccdof(inodty)

   SELECT CASE(storage)
   CASE (iIreac)
     get_reac_nodty_poroMAILx(1:nbdime) = bdyty(ibdyty)%Ireac(iccdof+1:iccdof+nbdime)
   CASE (iIaux_)
     get_reac_nodty_poroMAILx(1:nbdime) = bdyty(ibdyty)%Iaux(iccdof+1:iccdof+nbdime)
   CASE (iFext_)
     get_reac_nodty_poroMAILx(1:nbdime) = bdyty(ibdyty)%Fext(iccdof+1:iccdof+nbdime)
   CASE default
     call faterr(IAM,'storage id not known')
   END SELECT 
   
end function
!------------------------------------------------------------------------

!------------------------------------------------------------------------
 SUBROUTINE comp_vlocy_bynode_poroMAILx(ibdyty,list,storage)

   !
   ! called by vitrad
   !
  
   IMPLICIT NONE 
   INTEGER :: ibdyty,info
   INTEGER :: storage
   INTEGER,DIMENSION(:) :: list
!                            1234567890123456789012345678
   CHARACTER(len=28) :: IAM='poroMAILx::comp_vlocy_bynode'

   IF (nb_poroMAILx == 0) RETURN

   IF (is_externalFEM) call FATERR(IAM,'impossible to use externalFEM and precon')

!   print*,'< vitrad'
!   print*,ibdyty
!   print*,bdyty(ibdyty)%Ireac

!! fd a eviter   bdyty(ibdyty)%Vaux=0.d0

   SELECT CASE(storage) 

   CASE (iVaux_e_invM_t_Ireac,iVaux_e_invM_t_Iaux_)

     IF (bdyty(ibdyty)%is_precon) THEN

       CALL compute_precon_vaux_bynode_poroMAILx(ibdyty,list,storage)

     ELSE

       call FATERR(IAM,'needs precon')

     ENDIF

   CASE default
     call FATERR(IAM,'kind of "storage" not supported')

   END SELECT 
        
!   print*,'vitrad >'

 END SUBROUTINE comp_vlocy_bynode_poroMAILx
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------ 
 SUBROUTINE compute_precon_vaux_bynode_poroMAILx(ibdyty,list,storage)
   IMPLICIT NONE
   INTEGER :: ibdyty,  i, inodty, idof
   INTEGER :: storage
   INTEGER,DIMENSION(:) :: list
   !fd new
   INTEGER :: iccdof

   !fd reduction
   SELECT CASE(storage)
   CASE(iVaux_e_invM_t_Ireac)
!     print*,"Ireac"
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Ireac(bdyty(ibdyty)%p2g(idof))
     ENDDO
!     print*,bdyty(ibdyty)%Vaux_precon

   CASE(iVaux_e_invM_t_Iaux_)
     !print*,"Iaux"
     !print*, bdyty(ibdyty)%Iaux
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Iaux(bdyty(ibdyty)%p2g(idof))
     ENDDO
!     print*,bdyty(ibdyty)%Vaux_precon

   CASE default
     call faterr('poroMAILx::compute_precon_vaux_bynode','unknown storage')
   END SELECT 

!fd produit matrice vecteur + expansion

   !if (storage == iVaux_e_invM_t_Iaux_)  print*,'Vaux'

   !print*,'new'
   bdyty(ibdyty)%Vaux=0.D0
   DO i=1,SIZE(list) 
     inodty = list(i)
     iccdof = bdyty(ibdyty)%ccdof(inodty)

     !if (storage == iVaux_e_invM_t_Iaux_) print*,i,inodty,iccdof

     DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       IF (bdyty(ibdyty)%g2p(iccdof+idof) == 0) THEN
         call faterr('poroMAILx::compute_precon_vaux_bynode','inconsistency')
       ENDIF

       bdyty(ibdyty)%Vaux(iccdof+idof) = DOT_PRODUCT(bdyty(ibdyty)%W_precon(bdyty(ibdyty)%g2p(iccdof+idof),:), &
                                                     bdyty(ibdyty)%Vaux_precon(:))

       !if (storage == iVaux_e_invM_t_Iaux_) print*,'Vaux(',iccdof+idof,') = ',bdyty(ibdyty)%Vaux(iccdof+idof)

     ENDDO

   ENDDO

!!$   ! %<----
!!$   print*,'old'
!!$   
!!$  !fd produit matrice vecteur
!!$   bdyty(ibdyty)%Vaux_precon = MATMUL(bdyty(ibdyty)%W_precon,bdyty(ibdyty)%Vaux_precon)
!!$
!!$   !fd expansion
!!$   bdyty(ibdyty)%Vaux=0.D0
!!$   DO idof = 1, bdyty(ibdyty)%nbdof_precon
!!$     bdyty(ibdyty)%Vaux(bdyty(ibdyty)%p2g(idof)) = bdyty(ibdyty)%Vaux_precon(idof) 
!!$   ENDDO
!!$
!!$
!!$   DO i=1,SIZE(list) 
!!$     inodty = list(i)
!!$     iccdof = bdyty(ibdyty)%ccdof(inodty)
!!$     write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux(iccdof+1:iccdof+nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty)%nodID))
!!$   enddo
!!$
!!$   ! %<----

 END SUBROUTINE compute_precon_vaux_bynode_poroMAILx
!------------------------------------------------------------------------

!------------------------------------------------------------------------
 SUBROUTINE comp_vlocy_poroMAILx(ibdyty,storage)

   !
   ! called by vitrad
   !
  
   IMPLICIT NONE 
   INTEGER :: ibdyty,info
   INTEGER :: storage

!                           1234567890123456789012345
   CHARACTER(len=25) :: IAM='poroMAILx::compute_vlocy'
   CHARACTER(len=80) :: mes

   INTEGER :: timer1=0,timer2=0

   IF (nb_poroMAILx == 0) RETURN
                                              !01234567890123456789
   IF (timer1 == 0) timer1 = get_new_itimer_ID('COMP Vaux=M^-1 Iaux ')
   IF (timer2 == 0) timer2 = get_new_itimer_ID('COMP Vaux=M^-1 Ireac')

   IF (storage == iVaux_e_invM_t_Ireac .AND. timer1 /= 0) CALL start_itimer(timer1)
   IF (storage == iVaux_e_invM_t_Iaux_ .AND. timer2 /= 0) CALL start_itimer(timer2)

   IF (is_externalFEM) THEN
     print *,'The poroMAILx dont use external fem'
     SELECT CASE(storage)  
     CASE (iVaux_e_invM_t_Ireac)
       !CALL externalFEM_comp_vlocy_poroMAILx(ibdyty,bdyty(ibdyty)%Reac,bdyty(ibdyty)%Vaux)
     CASE (iVaux_e_invM_t_Iaux_)
       !CALL externalFEM_comp_vlocy_poroMAILx(ibdyty,bdyty(ibdyty)%Iaux,bdyty(ibdyty)%Vaux)
     CASE default
       call faterr(IAM,'unknown storage')
     END SELECT 

     RETURN
   ENDIF


   SELECT CASE(storage) 
!     case (iV____e_invM_t_Ireac)
!
!       bdyty(ibdyty)%V=bdyty(ibdyty)%Ireac
!
!       call solve_linear_system(KTaux,bdyty(ibdyty)%V)
!
!       print*,bdyty(ibdyty)%V
!
     CASE (iVaux_e_invM_t_Ireac)

       ! print*,'====================='
       ! print*,'Ireac                '       
       ! print*,bdyty(ibdyty)%Ireac


       IF (bdyty(ibdyty)%is_precon) THEN

         CALL compute_precon_vaux_poroMAILx(ibdyty,storage)

       ELSE

         call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Ireac)
 
         if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then

           bdyty(ibdyty)%drvvalues = 0.d0

           ! on les passe au g_system
           call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

         endif
         ! on resoud
         CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

         IF (info /= 0) THEN
           PRINT*,ibdyty
           CALL FATERR(IAM,'No solution')
         ENDIF

       ENDIF

       ! print*,'Vaux                 '       
       ! print*,bdyty(ibdyty)%Vaux
       ! print*,'====================='

     CASE (iVaux_e_invM_t_Iaux_)

       !print*,'Vaux_e_invM_t_Iaux'

        !print*,'====================='
        !print*,'Iaux                 '       
        !print*,bdyty(ibdyty)%Iaux

       IF (bdyty(ibdyty)%is_precon) THEN

         CALL compute_precon_vaux_poroMAILx(ibdyty,storage)

       ELSE

         call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)
 
         if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then

           bdyty(ibdyty)%drvvalues = 0.d0

           ! on les passe au g_system
           call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

         endif

         ! on resoud
         CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

         IF (info /=0 ) THEN
           WRITE(mes,'(A,1x,I0)') 'poroMAILx:',ibdyty
           CALL logmes(mes)
           CALL FATERR(IAM,'No solution')
         ENDIF

       ENDIF

       !print*,'Vaux                 '       
       !write(*,'(3(1x,D12.5))'),bdyty(ibdyty)%Vaux
       !print*,'====================='

     CASE default
         WRITE(mes,'(A,1x,I0)') 'storage:',ibdyty
         CALL LOGMES(mes)
         CALL FATERR(IAM,'storage type unknown')
   END SELECT 

   IF (storage == iVaux_e_invM_t_Ireac .AND. timer1 /= 0) CALL stop_itimer(timer1)
   IF (storage == iVaux_e_invM_t_Iaux_ .AND. timer2 /= 0) CALL stop_itimer(timer2)

        
!fd inutile    call free_matrix(KTaux)

!   print*,'vitrad >'

 END SUBROUTINE comp_vlocy_poroMAILx
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------ 
 SUBROUTINE compute_precon_vaux_poroMAILx(ibdyty,storage)
   IMPLICIT NONE
   INTEGER :: ibdyty,  inodty, idof
   INTEGER :: storage

!     print*,"poroMAILx:",ibdyty
!     print*,bdyty(ibdyty)%nbdof_precon
!     do idof = 1, bdyty(ibdyty)%nbdof_precon
!         print*,bdyty(ibdyty)%W_precon(idof,idof)
!         print*,"pdof ",idof," =gdof ",bdyty(ibdyty)%p2g(idof)
!     enddo
!!$   


!fd reduction
   SELECT CASE(storage)
   CASE(iVaux_e_invM_t_Ireac)
!     print*,"Ireac"
!     print*,bdyty(ibdyty)%Ireac
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Ireac(bdyty(ibdyty)%p2g(idof))
     ENDDO
   CASE(iVaux_e_invM_t_Iaux_)
!     print*,"Iaux"
!     print*,bdyty(ibdyty)%Iaux
     DO idof = 1, bdyty(ibdyty)%nbdof_precon
       bdyty(ibdyty)%Vaux_precon(idof) = bdyty(ibdyty)%Iaux(bdyty(ibdyty)%p2g(idof))
     ENDDO
   CASE default
     call faterr('poroMAILx::compute_precon_vaux','unknown storage')
   END SELECT 

!fd produit matrice vecteur
   bdyty(ibdyty)%Vaux_precon = MATMUL(bdyty(ibdyty)%W_precon,bdyty(ibdyty)%Vaux_precon)

!fd expansion
   bdyty(ibdyty)%Vaux=0.D0
   DO idof = 1, bdyty(ibdyty)%nbdof_precon
     bdyty(ibdyty)%Vaux(bdyty(ibdyty)%p2g(idof)) = bdyty(ibdyty)%Vaux_precon(idof) 
   ENDDO

   !if (storage == iVaux_e_invM_t_Ireac) then
   !  print*,ibdyty
   !  write(*,'(3(1x,D12.5))') bdyty(ibdyty)%Vaux
   !  print*,'==='
   !endif
 END SUBROUTINE compute_precon_vaux_poroMAILx
!------------------------------------------------------------------------
 function get_coor_poroMAILx(ibdyty)
   implicit none 
   integer :: ibdyty
   real(kind=8),dimension(:,:),pointer :: get_coor_poroMAILx
   ! ***
   integer :: inode,iM_bdyty,iM_nodty

   get_coor_poroMAILx => null()

   iM_bdyty=bdyty2M_bdyty(ibdyty)

   allocate(get_coor_poroMAILx(nbdime,bdyty(ibdyty)%nb_nodes)) 

   do inode=1,bdyty(ibdyty)%nb_nodes

     iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inode) 

     get_coor_poroMAILx(1:nbdime,inode) = get_cooref_nodty_MAILx(iM_bdyty,iM_nodty)

   enddo

 end function
 !----------------------------------------------------------------
 function get_connectivity_poroMAILx(ibdyty)
   implicit none
   integer :: ibdyty
   integer(kind=4),dimension(:),pointer :: get_connectivity_poroMAILx
   ! ***
   integer :: sz,iblmty,inode

   get_connectivity_poroMAILx => null()

   ! on compte
   sz=1
   do iblmty=1,size(bdyty(ibdyty)%blmty)
     sz = sz + size(bdyty(ibdyty)%blmty(iblmty)%nodes) + 1
   enddo

   ! on alloue
   allocate(get_connectivity_poroMAILx(sz)) 

   ! on rempli 
   sz=1
   get_connectivity_poroMAILx(sz) = size(bdyty(ibdyty)%blmty)
   do iblmty=1,size(bdyty(ibdyty)%blmty)
     sz = sz + 1
     get_connectivity_poroMAILx(sz)=size(bdyty(ibdyty)%blmty(iblmty)%nodes)
     do inode=1,size(bdyty(ibdyty)%blmty(iblmty)%nodes)
       sz = sz + 1
       get_connectivity_poroMAILx(sz) = bdyty(ibdyty)%blmty(iblmty)%nodes(inode)
     enddo
   enddo

 end function

 !> \brief Get the connectivity of all elements in a linked list
 function get_ll_connectivity_poroMAILx(ibdyty)
   implicit none
   !> poroMAILx index
   integer(kind=4), intent(in)  :: ibdyty
   !> Root of the linked list, first cell holds the number of elements
   type(T_link_connec), pointer :: get_ll_connectivity_poroMAILx
   !
   integer(kind=4) :: nb_elem, iblmty
   type(T_link_connec), pointer :: last, new

   allocate( get_ll_connectivity_poroMAILx )
   allocate( get_ll_connectivity_poroMAILx%connec(1) )

   nb_elem = size(bdyty(ibdyty)%blmty)

   get_ll_connectivity_poroMAILx%connec(1) = nb_elem

   last => get_ll_connectivity_poroMAILx

   do iblmty = 1, nb_elem
     allocate( new )
     allocate( new%connec(size(bdyty(ibdyty)%blmty(iblmty)%NODES)) )
     new%connec = bdyty(ibdyty)%blmty(iblmty)%NODES
     last%n => new
     last   => new
   end do

 end function

!------------------------------------------------------------------------
 !> computes the W matrix
 SUBROUTINE compute_precon_W_poroMAILx
 IMPLICIT NONE

!fd 
! assemble KT
! applique les cdl sur KT
! factorisation de KT
! boucle sur les noeuds precon et calcule la valeur precon  
!fd


   REAL(kind=8) :: HT,HT2
   INTEGER :: ibdyty,iblmty,inodty,idof,jdof,gdof,ivd,iccdof,inod,info
   INTEGER :: nbd,nbn

!                           1234567890123456789012345678
   CHARACTER(len=28) :: IAM='poroMAILx::compute_precon_W'
   character(len=80) :: cout

   IF (nb_poroMAILx == 0) RETURN

   HT=THETA*H
   HT2=HT*HT

   DO ibdyty=1,SIZE(bdyty)

     IF (.NOT. bdyty(ibdyty)%is_precon) CYCLE

     write(cout,'(A,I0)') 'On construit W_precon pour le corps ',ibdyty

!fd 0/ preparation (do the same than init_precon_W)

     nbd = 0; nbn = 0
     DO inodty=1,bdyty(ibdyty)%nb_nodes

       IF (bdyty(ibdyty)%nodes_precon(inodty) /= 0 ) THEN
         nbn  = nbn + 1
         nbd  = nbd + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       ENDIF

     ENDDO

     bdyty(ibdyty)%nbdof_precon=nbd

     write(cout,'(A,1x,I0)') 'nb dof',bdyty(ibdyty)%nbdof
     call logmes(cout)
     write(cout,'(A,1x,I0)') 'nb node precon',nbn
     call logmes(cout)
     write(cout,'(A,1x,I0)') 'nb dof precon',bdyty(ibdyty)%nbdof_precon
     call logmes(cout)

     ALLOCATE(bdyty(ibdyty)%W_precon(nbd,nbd))
     !ALLOCATE(bdyty(ibdyty)%W_precon_T(nbd,nbd))
     ALLOCATE(bdyty(ibdyty)%Vaux_precon(nbd))
     ALLOCATE(bdyty(ibdyty)%p2g(nbd))
     ALLOCATE(bdyty(ibdyty)%g2p(bdyty(ibdyty)%nbdof))

     nbd = 0; nbn = 0
     bdyty(ibdyty)%g2p = 0

     DO inodty=1,bdyty(ibdyty)%nb_nodes

       IF (bdyty(ibdyty)%nodes_precon(inodty) /= 0 ) THEN
         nbn  = nbn + 1
         
         DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
           bdyty(ibdyty)%p2g(nbd+idof) = bdyty(ibdyty)%ccdof(inodty) + idof
           bdyty(ibdyty)%g2p(bdyty(ibdyty)%ccdof(inodty) + idof) = nbd + idof 
         ENDDO
         nbd  = nbd + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       ENDIF

     ENDDO

     call logmes('maps ok')

     !paranoiac test
     DO idof=1,bdyty(ibdyty)%nbdof_precon
       IF (bdyty(ibdyty)%g2p(bdyty(ibdyty)%p2g(idof)) /= idof) THEN
         call faterr(IAM,'wrong maps')
       ENDIF
     ENDDO

!       print*,'p2g:'
!       print*,bdyty(ibdyty)%p2g        
!       print*,'====='

!fd 1/ assemblage ...

!!$     CALL G_zero(bdyty(ibdyty)%KT)

     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

        call erase_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%mass)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%damping,HT)
        call add_to_elementary_matrix(bdyty(ibdyty)%g_sys,iblmty,bdyty(ibdyty)%blmty(iblmty)%stiffness,HT2)

       ! degage dans g_system

!!$       bdyty(ibdyty)%blmty(iblmty)%KTloc=  bdyty(ibdyty)%blmty(iblmty)%mass + &
!!$                                          (HT*bdyty(ibdyty)%blmty(iblmty)%damping) + &
!!$                                          (HT2*bdyty(ibdyty)%blmty(iblmty)%stiffness)
!!$
!!$       CALL G_assemb(bdyty(ibdyty)%KT,bdyty(ibdyty)%blmty(iblmty)%KTloc, &
!!$                     bdyty(ibdyty)%blmty(iblmty)%edof2gdof)

     ENDDO 

!!$     CALL G_store(bdyty(ibdyty)%KT)

     PRINT*,'assemblage ok'


!fd 2/ cdl ...

     if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then

       DO ivd=1,bdyty(ibdyty)%nb_poro_driven_dof

         CALL owner_of_a_driven_dof(bdyty(ibdyty)%poro_driven_dof(ivd),inod,idof)

         bdyty(ibdyty)%drvdofs(ivd)=bdyty(ibdyty)%ccdof(inod)+idof 

       enddo

       call set_drvdofs(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvdofs)

     else
  
       call erase_drvdofs(bdyty(ibdyty)%g_sys)

     endif 

     ! degage dans g_system
!!$
!!$     DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
!!$
!!$       CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
!!$
!!$       iccdof=bdyty(ibdyty)%ccdof(inod)+idof 
!!$  
!!$       CALL G_apply_drvdof(bdyty(ibdyty)%KT,iccdof)
!!$
!!$     ENDDO

     PRINT*,'cdl ok'

!fd 3/ precon

!fd la factorisation est faite a la premiere resolution => un test a chaque resolution !!
!fd il faut separer les 2 etapes.

     if (bdyty(ibdyty)%nb_poro_driven_dof /= 0) then

       bdyty(ibdyty)%drvvalues = 0.d0

       ! on les passe au g_system
       call set_drvvalues(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%drvvalues)           

     endif

     DO jdof=1,bdyty(ibdyty)%nbdof_precon

       bdyty(ibdyty)%Iaux = 0.d0

       !fd on pose la perturbation

       gdof = bdyty(ibdyty)%p2g(jdof)
       bdyty(ibdyty)%Iaux(gdof) = 1.d0

       call set_vector(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Iaux)

       ! on resoud
       CALL solve_system(bdyty(ibdyty)%g_sys,bdyty(ibdyty)%Vaux,info)

!!$! est ce utile ?
!!$       CALL nullify_vlocy_driven_dof(ibdyty,iVaux_)
!!$
!!$
!!$       !if (gdof==518) then
!!$       !  print*,'Iaux precon' 
!!$       !  print*,bdyty(ibdyty)%Vaux
!!$       !endif
!!$
!!$
!!$       CALL G_solve_linear_system(bdyty(ibdyty)%KT,bdyty(ibdyty)%Vaux,info)

       IF (info > 0) THEN
         PRINT*,ibdyty
         CALL FATERR(IAM,'No solution')
       ENDIF


       !if (gdof==518) then
       !  print*,'Vaux precon' 
       !  print*,bdyty(ibdyty)%Vaux
       !endif
!fd on recupere la colonne de W_precon

       DO idof=1, bdyty(ibdyty)%nbdof_precon
         gdof = bdyty(ibdyty)%p2g(idof)
         bdyty(ibdyty)%W_precon(idof,jdof) = bdyty(ibdyty)%Vaux(gdof)
       ENDDO

     ENDDO

     PRINT*,'construction ok'

     bdyty(ibdyty)%Vaux = 0.d0

     !bdyty(ibdyty)%W_precon_T = transpose(bdyty(ibdyty)%W_precon)

!!$!fd pourquoi !!?
!!$     CALL G_zero(bdyty(ibdyty)%KT)

   ENDDO

!!$   do ibdyty=1,nb_mecaMAILx
!!$     print*,"mecaMAILx:",ibdyty
!!$     if (bdyty(ibdyty)%is_precon) then
!!$       print*,bdyty(ibdyty)%nbdof_precon
!!$       do idof = 1, bdyty(ibdyty)%nbdof_precon
!!$         print*,bdyty(ibdyty)%W_precon(idof,idof)
!!$       enddo
!!$     else
!!$       print*,"pas concerne"
!!$     endif
!!$   end do 
!!$


 END SUBROUTINE compute_precon_W_poroMAILx

!------------------------------------------------------------------------
! Contact multi physique
FUNCTION get_Pfree_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8) :: get_Pfree_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  INTEGER                        :: iccdof_1,nbdof_1
  INTEGER                        :: iccdof_2,nbdof_2

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

  IF (nbdof> nbDIME) THEN
    get_Pfree_nodty_poroMAILx = bdyty(ibdyty)%Vfree(iccdof+nbdof)
  ELSE
    iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
    nbdof_1 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,1)))
    iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
    nbdof_2 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,2)))
    
    get_Pfree_nodty_poroMAILx = 0.5 *  bdyty(ibdyty)%Vfree(iccdof_1+nbdof_1) + 0.5 *  bdyty(ibdyty)%Vfree(iccdof_2+nbdof_2)
  ENDIF


END FUNCTION get_Pfree_nodty_poroMAILx

!------------------------------------------------------------------------
! Contact multi physique
FUNCTION get_Pbegin_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8) :: get_Pbegin_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  INTEGER                        :: iccdof_1,nbdof_1
  INTEGER                        :: iccdof_2,nbdof_2

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

  IF (nbdof> nbDIME) THEN
    get_Pbegin_nodty_poroMAILx = bdyty(ibdyty)%Vbegin(iccdof+nbdof)
  ELSE
    iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
    nbdof_1 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,1)))
    iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
    nbdof_2 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,2)))
    
    get_Pbegin_nodty_poroMAILx = 0.5 *  bdyty(ibdyty)%Vbegin(iccdof_1+nbdof_1) + 0.5 *  bdyty(ibdyty)%Vbegin(iccdof_2+nbdof_2)
  ENDIF


END FUNCTION get_Pbegin_nodty_poroMAILx

!------------------------------------------------------------------------
! Contact multi physique
FUNCTION get_P_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8) :: get_P_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  INTEGER                        :: iccdof_1,nbdof_1
  INTEGER                        :: iccdof_2,nbdof_2
  
  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
  IF (nbdof> nbDIME) THEN
    get_P_nodty_poroMAILx = bdyty(ibdyty)%V(iccdof+nbdof)
  ELSE
    iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
    nbdof_1 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,1)))
    iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
    nbdof_2 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,2)))
    
    get_P_nodty_poroMAILx = 0.5 *  bdyty(ibdyty)%V(iccdof_1+nbdof_1) + 0.5 *  bdyty(ibdyty)%V(iccdof_2+nbdof_2)
  ENDIF


END FUNCTION get_P_nodty_poroMAILx

!------------------------------------------------------------------------
! Contact multi physique
FUNCTION get_Paux_nodty_poroMAILx(ibdyty,inodty)
  IMPLICIT NONE
  INTEGER                        :: ibdyty,inodty
  REAL(kind=8) :: get_Paux_nodty_poroMAILx
  ! ***
  INTEGER                        :: iccdof,nbdof,idof
  INTEGER                        :: iccdof_1,nbdof_1
  INTEGER                        :: iccdof_2,nbdof_2

  iccdof=bdyty(ibdyty)%ccdof(inodty)

  ! attention au cafouillage entre nbdof et nbDIME

  nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

  IF (nbdof> nbDIME) THEN
    get_Paux_nodty_poroMAILx = bdyty(ibdyty)%Vaux(iccdof+nbdof)
  ELSE
    iccdof_1 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,1))
    nbdof_1 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,1)))
    iccdof_2 = bdyty(ibdyty)%ccdof(bdyty(ibdyty)%Mask_P2U(inodty,2))
    nbdof_2 = nbdof_a_nodty(bdyty(ibdyty)%nodty(bdyty(ibdyty)%Mask_P2U(inodty,2)))
    
    get_Paux_nodty_poroMAILx = 0.5 *  bdyty(ibdyty)%Vaux(iccdof_1+nbdof_1) + 0.5 *  bdyty(ibdyty)%Vaux(iccdof_2+nbdof_2)
  ENDIF


END FUNCTION get_Paux_nodty_poroMAILx

!!!------------------------------------------------------------------------    
 SUBROUTINE add_flux_nodty_poroMAILx(ibdyty,inodty,reac,storage)

   !
   ! called by injj
   !
  
   IMPLICIT NONE 
   INTEGER :: ibdyty,inodty,iccdof,nbdof
   INTEGER :: storage

   REAL(kind=8)  :: reac

   iccdof=bdyty(ibdyty)%ccdof(inodty)
   nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))

   
   !print *,'Add flux in poroBodies : ',ibdyty, ' au noeud : ',inodty, ' value : ',reac
   
   ! calcul de la reaction totale deformable
   !contribution deformable
   IF (nbdof>nbDIME) THEN    
     SELECT CASE(storage)
     CASE (iIreac)
         bdyty(ibdyty)%Ireac(iccdof+nbdof)=  &
         bdyty(ibdyty)%Ireac(iccdof+nbdof)+reac
     CASE (iIaux_)
         bdyty(ibdyty)%Iaux(iccdof+nbdof)=  &
         bdyty(ibdyty)%Iaux(iccdof+nbdof)+reac
     CASE (iFext_)
         bdyty(ibdyty)%Fext(iccdof+nbdof)=  &
         bdyty(ibdyty)%Fext(iccdof+nbdof)+reac
     CASE default
         call faterr('poroMAILx::add_flux_nodty','unknown storage')
     END SELECT
   ENDIF 

   !print*,'injj >'

END SUBROUTINE add_flux_nodty_poroMAILx

 function get_All_poroMAILx(ibdyty)
   implicit none 
   integer :: ibdyty,iM_bdyty
   real(kind=8),dimension(:,:),pointer :: get_All_poroMAILx
   ! ***
   integer :: inodty,sz,nbn,ns,ne,idx,ng,nf,n_fext,n_fint,iccdof
   REAL(kind=8),DIMENSION(:,:),allocatable :: S
   REAL(kind=8),DIMENSION(:,:),allocatable :: E
   REAL(kind=8),DIMENSION(:,:),allocatable :: G
   REAL(kind=8),DIMENSION(:,:),allocatable :: F
   get_All_poroMAILx => null()

   !iM_bdyty = bdyty2M_bdyty(ibdyty)

   nbn=bdyty(ibdyty)%nb_nodes
   
   select case (nbdime) 
   case(2)
     sz = 2 + 2 + 1 + 5 + 5 + 3 + 3 + 2 + 2
     ne=5
     ns=5
     ng=3
     nf=3
     ! DA : Attention au 
     ! xx, yy, xy, zz, vm|J
     allocate(S(5,nbn),E(5,nbn),G(3,nbn),F(3,nbn))

     call get_2DNodalStrain_poroMAILx(ibdyty,E)
     call get_2DNodalStress_poroMAILx(ibdyty,S)
     call get_NodalGrad_poroMAILx(ibdyty,G)
     call get_NodalFlux_poroMAILx(ibdyty,F)
     
   case(3)
     sz = 3 + 3 + 1 + 7 + 7 + 3 + 3 + 3 + 3
     ne=7 
     ns=7
     ng=3
     nf=3
     ! xx, xy, xz, yy, yz, zz, vm|J
     allocate(S(7,bdyty(ibdyty)%nb_nodes), &
              E(7,bdyty(ibdyty)%nb_nodes), &
              G(3,nbn),F(3,nbn))
              
     call get_3DNodalStrain_poroMAILx(ibdyty,E,1)
     call get_3DNodalStress_poroMAILx(ibdyty,S,2)
     call get_NodalGrad_poroMAILx(ibdyty,G)
     call get_NodalFlux_poroMAILx(ibdyty,F)
   end select

   allocate(get_All_poroMAILx(sz,bdyty(ibdyty)%nb_nodes)) 

   do inodty=1,bdyty(ibdyty)%nb_nodes
     idx=0
     get_All_poroMAILx(idx+1:idx+nbdime,inodty) = get_X_nodty_poroMAILx(ibdyty,inodty)
     idx = idx + nbdime
     get_All_poroMAILx(idx+1:idx+nbdime,inodty) = get_V_nodty_poroMAILx(ibdyty,inodty)
     idx = idx + nbdime
     get_All_poroMAILx(idx+1,inodty) = get_P_nodty_poroMAILx(ibdyty,inodty)
     idx = idx + 1  
     get_All_poroMAILx(idx+1:idx+ne,inodty) = E(1:ne,inodty)
     idx = idx + ne  
     get_All_poroMAILx(idx+1:idx+ns,inodty) = S(1:ns,inodty)
     idx = idx + ns 
     get_All_poroMAILx(idx+1:idx+ng,inodty) = G(1:ng,inodty)
     idx = idx + ng  
     get_All_poroMAILx(idx+1:idx+nf,inodty) = F(1:nf,inodty)
     idx = idx + nf

     iccdof=bdyty(ibdyty)%ccdof(inodty)

     get_All_poroMAILx(idx+1:idx+nbdime,inodty) = bdyty(ibdyty)%fext(iccdof+1:iccdof+nbDIME)
     idx = idx + nbdime
     get_All_poroMAILx(idx+1:idx+nbdime,inodty) = -bdyty(ibdyty)%fint(iccdof+1:iccdof+nbDIME)
     idx = idx + nbdime


   enddo

   deallocate(E,S,G,F) 

 end function

!------------------------------------------------------------------------
SUBROUTINE get_NodalGrad_poroMAILx(ibdyty,G)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,requiredfield
  INTEGER :: iM_bdyty,iM_blmty,nbdof
!                            123456789012345678901234567
   CHARACTER(len=27) :: IAM='poroMAILX::get_3DNodalGradP'

  ! allocation dans routine appelante (3,nb_nodes)
  ! GradPx,GradPy,GradPz

  REAL(kind=8),DIMENSION(:,:) :: G

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  G = 0.d0

  nbfields=3

  requiredfield = 1 ! for gradient

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  IF ( nbfields /= SIZE(G,dim=1)) THEN
    PRINT *,nbfields,SIZE(G,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF
  
  DO iblmty=1, size(bdyty(ibdyty)%blmty)

    iM_blmty   = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = bdyty(ibdyty)%blmty(iblmty)%ther_node

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2nodeP_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                      bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                      iM_bdyty,iM_blmty,requiredfield,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      G(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=G(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    G(:,inodty)=G(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO    
  
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
    IF (nbdof== nbDIME) THEN
       G(:,inodty) = 0.5 *  G(:,bdyty(ibdyty)%Mask_P2U(inodty,1)) + 0.5 *  G(:,bdyty(ibdyty)%Mask_P2U(inodty,2))
    ENDIF
  ENDDO
  
END SUBROUTINE get_NodalGrad_poroMAILx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
SUBROUTINE get_NodalFlux_poroMAILx(ibdyty,G)

  IMPLICIT NONE

  INTEGER :: ibdyty,iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,requiredfield
  INTEGER :: iM_bdyty,iM_blmty,nbdof
!                            12345678901234567890123456
   CHARACTER(len=26) :: IAM='poroMAILX::get_3DNodalFlux'

  ! allocation dans routine appelante (3,nb_nodes)
  ! GradPx,GradPy,GradPz

  REAL(kind=8),DIMENSION(:,:) :: G

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  G = 0.d0

  nbfields=3

  requiredfield = 2 ! for darcy flux 

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  IF ( nbfields /= SIZE(G,dim=1)) THEN
    PRINT *,nbfields,SIZE(G,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  ENDIF

  DO iblmty=1, size(bdyty(ibdyty)%blmty)

    iM_blmty   = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = bdyty(ibdyty)%blmty(iblmty)%ther_node

    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2nodeP_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                      bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                      iM_bdyty,iM_blmty,requiredfield,temp,NbFields,NbNodes_stored)

    !print *,'Taille Nodal vlaues : ',size(temp,1),size(temp,2)
    !print *,'Valeur nodale X: ',temp(1,:)
    !print *,'Valeur nodale Y: ',temp(2,:)
    !print *,'Valeur nodale Z: ',temp(3,:)

    DO inodty=1,NbNodes_stored
      ! DA : Ajout d'un signe négatif pour accord avec la physique
      G(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=G(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    ENDDO    

    DEALLOCATE(temp)

  ENDDO 

  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    G(:,inodty)=G(:,inodty)/SIZE(M_bdyty(iM_bdyty)%nod2blmty(inodty)%G_i)
  ENDDO    
  DO inodty=1,SIZE(M_bdyty(iM_bdyty)%nodty)
    nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
    IF (nbdof== nbDIME) THEN
       G(:,inodty) = 0.5 *  G(:,bdyty(ibdyty)%Mask_P2U(inodty,1)) + 0.5 *  G(:,bdyty(ibdyty)%Mask_P2U(inodty,2))
    ENDIF
  ENDDO 
  
END SUBROUTINE get_NodalFlux_poroMAILx
!------------------------------------------------------------------------


function get_Internal_poroMAILx(ibdyty)
  implicit none
  integer(kind=4) :: ibdyty
  real(kind=8), dimension(:,:),pointer :: get_Internal_poroMAILx
  ! ***
  integer(kind=4) :: inodty,sz,nbn,ns,ne,idx

  get_Internal_poroMAILx => null()

  nbn=bdyty(ibdyty)%nb_nodes
  ne = get_nb_internal_poroMAILx(ibdyty)
  if( ne < 1 ) return

  allocate(get_Internal_poroMAILx(ne,nbn))
  get_Internal_poroMAILx = 0.d0

  select case (nbdime)
  case(2)
     call get_2DNodalInternal_poroMAILx(ibdyty,get_Internal_poroMAILx)
  case(3)
     call get_3DNodalInternal_poroMAILx(ibdyty,get_Internal_poroMAILx)
  end select

end function

function get_nb_internal_poroMAILx(ibdyty)
  implicit none
  integer(kind=4), intent(in) :: ibdyty
  integer(kind=4) :: get_nb_internal_poroMAILx
  integer(kind=4) :: iblmty,imodel,nb_internal

  get_nb_internal_poroMAILx = 0
  do iblmty=1,size(bdyty(ibdyty)%blmty)

    imodel=bdyty(ibdyty)%blmty(iblmty)%mdlnb
    nb_internal = modelz(imodel)%nb_internal_variables
    if (nb_internal>get_nb_internal_poroMAILx) then
       get_nb_internal_poroMAILx = nb_internal
    end if

  end do

end function get_nb_internal_poroMAILx

!------------------------------------------------------------------------
! DA : Permettre la visualisation des variables internes (Attention au lissage pour le passage aux noeuds)
!------------------------------------------------------------------------
SUBROUTINE get_2DNodalInternal_poroMAILx(ibdyty,E)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: E
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty, nb_internal,imodel

                            !123456789012345678901234567890
   CHARACTER(len=30) :: IAM='poroMAILx::get_2DNodalInternal'

  ! allocation dans routine appelante (nb_internal,nb_nodes)
  ! Ordre donne par matlib

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  E = 0.d0

  nbfields = get_nb_internal_poroMAILx(ibdyty)

  IF ( nbfields /= SIZE(E,dim=1)) THEN
    PRINT *,nbfields,SIZE(E,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  END IF

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)


    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_2D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                   bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                   iM_bdyty,iM_blmty,5,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    END DO

    DEALLOCATE(temp)

  END DO


  DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)
    E(:,inodty)=E(:,inodty)/SIZE(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
  END DO

END SUBROUTINE get_2DNodalInternal_poroMAILx

SUBROUTINE get_3DNodalInternal_poroMAILx(ibdyty,E)

  IMPLICIT NONE

  INTEGER :: ibdyty
  REAL(kind=8),DIMENSION(:,:) :: E
  ! ***
  INTEGER :: iblmty,inodty,l_nb_nodes,nbfields,NbNodes_stored,iM_bdyty,iM_blmty, nb_internal,imodel

                            !123456789012345678901234567890
   CHARACTER(len=30) :: IAM='poroMAILx::get_3DNodalInternal'

  ! allocation dans routine appelante (nb_internal,nb_nodes)
  ! Ordre donne par matlib

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: temp

  iM_bdyty = bdyty2M_bdyty(ibdyty)

  E = 0.d0

  nbfields = get_nb_internal_poroMAILx(ibdyty)

  IF ( nbfields /= SIZE(E,dim=1)) THEN
    PRINT *,nbfields,SIZE(E,dim=1)
    CALL FATERR(IAM,'Non conforming size')
  END IF

  DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)

    iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
    l_nb_nodes = SIZE(M_bdyty(iM_bdyty)%blmty(iblmty)%NODES)


    ALLOCATE(temp(nbfields,l_nb_nodes))
    temp=0.d0

    CALL gpv2node_3D(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                   bdyty(ibdyty)%blmty(iblmty)%mdlnb, &
                   iM_bdyty,iM_blmty,5,temp,NbFields,NbNodes_stored)

    DO inodty=1,NbNodes_stored
      E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty))=E(:,M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)) + temp(:,inodty)
    END DO

    DEALLOCATE(temp)

  END DO


  DO inodty=1,SIZE(M_bdyty(ibdyty)%nodty)
    E(:,inodty)=E(:,inodty)/SIZE(M_bdyty(ibdyty)%nod2blmty(inodty)%G_i)
  END DO

END SUBROUTINE get_3DNodalInternal_poroMAILx


SUBROUTINE load_Display_P_poroMAILx
 
IMPLICIT NONE 
INTEGER :: ibdyty,iM_bdyty,inodty,iM_nodty,iblmty,iM_blmty
INTEGER :: nbdof,num,inodes,inode1,inode2,node1,node2,local_connec
INTEGER :: errare

                          !1234567890123456789012345
CHARACTER(len=25)  :: IAM='poroMAILx::load_Display_P'

IF (nb_poroMAILx == 0) RETURN
  
! Creation du masque d'affichage du champ de pression

DO ibdyty=1,SIZE(bdyty)

       !print *,'---------- load_Display_P_poroMAILx ------------'

       iM_bdyty=bdyty2M_bdyty(ibdyty)

       ALLOCATE(bdyty(ibdyty)%Mask_P2U(SIZE(bdyty(ibdyty)%nodty),2),stat=errare)
       IF (errare/=0) THEN
          CALL FATERR(IAM,'error allocating Mask_P2U')
       END IF
       
       
       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
       
          !print *,'Etude du noeud : ',inodty
       
          iM_nodty=bdyty(ibdyty)%nodty2M_nodty(inodty)
          nbdof=nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       
          IF (nbdof>nbDIME) THEN
              bdyty(ibdyty)%Mask_P2U(inodty,1) = inodty
              bdyty(ibdyty)%Mask_P2U(inodty,2) = 0
          ELSE 
              iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(1)
              iblmty = M2poro(iM_bdyty)%blmty(iM_blmty)
              !print *,'noeuds apparteant a une edge'
              num = 1
              DO inodes = 1,size(bdyty(ibdyty)%blmty(iblmty)%NODES)
                 !print *,'inodes : ',bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)
                 !print *,'inodty : ',inodty
                 !print *,'num : ',num
                 !!print *,'bdyty(ibdyty)%blmty(iblmty)%ther_node : ',bdyty(ibdyty)%blmty(iblmty)%ther_node
              
                 IF (bdyty(ibdyty)%blmty(iblmty)%NODES(inodes)==inodty) THEN
                    local_connec = num - bdyty(ibdyty)%blmty(iblmty)%ther_node
                    !print *,'place dans la connectivite : ',local_connec
                 ENDIF
                 num = num + 1
              ENDDO
              
              inode1 = get_local_connectivity_edge_poroEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,local_connec,1)
              inode2 = get_local_connectivity_edge_poroEF(bdyty(ibdyty)%blmty(iblmty)%blmnb,local_connec,2)

              node1 = bdyty(ibdyty)%blmty(iblmty)%NODES(inode1)
              node2 = bdyty(ibdyty)%blmty(iblmty)%NODES(inode2)
              
              bdyty(ibdyty)%Mask_P2U(inodty,1) = node1
              bdyty(ibdyty)%Mask_P2U(inodty,2) = node2

          ENDIF            
          
          !print *,'Mask_P2U (1) : ',bdyty(ibdyty)%Mask_P2U(inodty,1)
          !print *,'Mask_P2U (2) : ',bdyty(ibdyty)%Mask_P2U(inodty,2)
          
          
       END DO

ENDDO

END SUBROUTINE load_Display_P_poroMAILx
!------------------------------------------------------------------------

  !> add an external force or these derivation (grad F)
  SUBROUTINE Add_Field_Load_bynode_poroMAILx(ibdyty, fsize, field)
  
  IMPLICIT NONE

  INTEGER                                  :: ibdyty,ifield,fsize, i, iccdof, inodty, nbdof
  REAL(kind=8),INTENT(in),DIMENSION(fsize) :: field
  
  INTEGER                                  :: errare, nbdof_ther, nbdof_meca, NbNo_meca, NbNo_ther, iblmty,iM_bdyty, iM_blmty
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: vfield 
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: coor_ele,vfield_ele, U_ele
  REAL(kind=8),DIMENSION(:)  ,ALLOCATABLE  :: fext_ele
 
                                                  !12345678901234567890123456789012
  CHARACTER(len=42)                       :: IAM='poroMAILx::Add_Field_Load_bynode'

  IF (nb_poroMAILx == 0) RETURN

  !
    print *,'Add_Field_Load_poroMAILx on body : ',ibdyty
    !print *,'field : ',field
    IF (fsize==bdyty(ibdyty)%nbdof_meca) THEN
        ! Allocation du field vecteur aux noeuds  
        IF (ALLOCATED(vfield)) DEALLOCATE(vfield)
        ALLOCATE(vfield(fsize/nbDIME,nbDIME),stat=errare)

        vfield = reshape(field,(/nbDIME,fsize/nbDIME/))
        
        !rint *,'On passe un field vectoriel'
        ifield = 1
    
    ELSE
        ! Allocation du field scalaire aux noeuds  
        IF (ALLOCATED(vfield)) DEALLOCATE(vfield)
        ALLOCATE(vfield(fsize,1),stat=errare)
        
        vfield(:,1) = field
        
        !print *,'On passe un field scalaire'
        ifield = 0

    ENDIF
    
    !print *,'vfield : ',vfield
 
    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
      
      IF (ALLOCATED(coor_ele)) DEALLOCATE(coor_ele)
      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
      
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating coor_ele')
      ENDIF
    
      coor_ele = 0.D0
      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
      
      nbdof_ther = bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node
      nbdof_meca = bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
      NbNo_meca  = bdyty(ibdyty)%blmty(iblmty)%meca_node
      NbNo_ther  = bdyty(ibdyty)%blmty(iblmty)%ther_node

      IF (ALLOCATED(fext_ele)) DEALLOCATE(fext_ele)
      ALLOCATE(fext_ele(nbdof_meca*NbNo_meca +  &
                        nbdof_ther*NbNo_ther),stat=errare)
    
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating fext_ele')
      ENDIF
    
      fext_ele = 0.D0
    
      IF (ALLOCATED(U_ele)) DEALLOCATE(U_ele)
      ALLOCATE(U_ele(bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node,bdyty(ibdyty)%blmty(iblmty)%meca_node),stat=errare)
      
      IF (errare /= 0) THEN
         CALL FATERR(IAM,'allocating U_ele')
      ENDIF
    
      U_ele = 0.D0
      
      ! Recuperation des deplacement au point milieu de la partie structure
      DO i=1,bdyty(ibdyty)%blmty(iblmty)%meca_node

          ! passage au numero global
          inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
          ! position un dans le vecteur global pour le numero inodty
          nbdof   = bdyty(ibdyty)%dofnb(inodty)   
          iccdof  = bdyty(ibdyty)%ccdof(inodty) 
          U_ele(1:nbdof_meca,i)=         theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof_meca) + &
                                 (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof_meca)
          
      END DO
      
      ! on passe un field vectoriel
      IF (ifield == 1) THEN
      
          IF (ALLOCATED(vfield_ele)) DEALLOCATE(vfield_ele)
          ALLOCATE(vfield_ele(nbdof_meca, NbNo_meca),stat=errare)
        
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'allocating vfield_ele')
          ENDIF
     
          vfield_ele = 0.D0
    
          vfield_ele = vfield(:,bdyty(ibdyty)%blmty(iblmty)%NODES)
      ENDIF
      
      ! on passe un field scalaire
      IF (ifield == 0) THEN
      
          IF (ALLOCATED(vfield_ele)) DEALLOCATE(vfield_ele)
          ALLOCATE(vfield_ele(1,NbNo_meca),stat=errare)
        
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'allocating vfield_ele')
          ENDIF
     
          vfield_ele = 0.D0
    
          vfield_ele(1,:) = vfield(bdyty(ibdyty)%blmty(iblmty)%NODES,1)
      ENDIF

      !
      ! on calcule les sources interieures
      ! 
      iM_bdyty=bdyty2M_bdyty(ibdyty)
      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
      
      !print*,iblmty
      !print*,'field '
      !do i=1,size(vfield_ele,dim=1)
      !  write(6,'(3(1x,D12.5))') vfield_ele(i,:) 
      !enddo

      !print *,'Vfield ele : ',vfield_ele

      CALL add_elementary_field_load(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
                                     bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
                                     H,coor_ele,U_ele,vfield_ele,iM_bdyty,iM_blmty,fext_ele(1:nbdof_meca*NbNo_meca), ifield)

      bdyty(ibdyty)%blmty(iblmty)%ttFext  = bdyty(ibdyty)%blmty(iblmty)%ttFext  + fext_ele

      !print*,'fext '
      !write(6,'(2(1x,D12.5))') bdyty(ibdyty)%blmty(iblmty)%ttFext
      !                             
      ! On ajoute cette contribution à la fin du pas
      !

      DEALLOCATE(fext_ele, vfield_ele, coor_ele )
                                   
    ENDDO
    
    DEALLOCATE(vfield)
    
 END SUBROUTINE Add_Field_Load_bynode_poroMAILx

!------------------------------------------------------------------------

  LOGICAL FUNCTION get_write_DOF_poroMAILx(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome

    get_write_DOF_poroMAILx = write_DOF

  END FUNCTION get_write_DOF_poroMAILx
!------------------------------------------------------------------------
  LOGICAL FUNCTION get_write_Rnod_poroMAILx(fantome)

    IMPLICIT NONE
    INTEGER,OPTIONAL :: fantome

    get_write_Rnod_poroMAILx = write_Rnod

  END FUNCTION get_write_Rnod_poroMAILx
  !------------------------------------------------------------------------
  
    SUBROUTINE post_models_poroMAILx

    IMPLICIT NONE

    INTEGER :: nb_MAILx
    INTEGER :: iM_bdyty,iM_blmty,iM_model,inodty
    INTEGER :: ibdyty,iblmty,imodel,iM_nodty
    INTEGER :: errare,itest,imdlnb,iccdof,idof,iG_i,itempo,idof_meca, idof_ther
!    INTEGER :: itempo,bw

    INTEGER :: nb_external,nb_internal
    
!fd 22/04/08 external fields
    INTEGER :: IF,nbf,nb_ef,nb_bf
    CHARACTER(len=30),DIMENSION(:),ALLOCATABLE :: field_name


    CHARACTER(len=5) :: ctempo
    CHARACTER(len=103) :: cout
                              !1234567890123456789012
    CHARACTER(len=22)  :: IAM='poroMAILx::post_models'

    INTEGER,DIMENSION(:),ALLOCATABLE :: edof2gdof 

    integer :: i,p_inodty,bw,nbdof_meca

    !integer,dimension(:),allocatable :: perm,inv_perm,i4_vector
    integer,dimension(:),allocatable :: i4_vector
    integer(kind=4),dimension(:),pointer ::connectivities
    
    connectivities => null()
    
    ! 0 initialisations

    ! initialisation de la liste des poroEF_xxx disponibles
    CALL init_poroEF

    ! initialisation de la map MAILx -> poroMAILx
    nb_MAILx=get_nb_MAILx()

    ALLOCATE(M2poro(nb_MAILx),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating M2poro')
    END IF
  
    DO iM_bdyty=1,nb_MAILx
       M2poro(iM_bdyty)%bdyty=0
       NULLIFY(M2poro(iM_bdyty)%nodty)
       NULLIFY(M2poro(iM_bdyty)%blmty)
    END DO

    ! first reading: sizing array of models  

    itest = 0
    
    DO imodel=1,SIZE(modelz)
       IF (modelz(imodel)%mdlty == 'POROx') itest = itest + 1 
    END DO
    
    WRITE(cout,'(I0,1x,A)') itest,'POROx models declared'
    CALL LOGMES(cout)
    
    ! then constructing the poro EF database 
    
    ! first scaning of the MAILx array looking for POROx models 
    ! database determining the size of bdyty
    
    ibdyty=0
    
    DO iM_bdyty=1,SIZE(M_bdyty)
       itest=0
       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'POROx') itest=1
                END IF
             END DO
             IF (itest == 1) EXIT
          END DO
          IF (itest == 1) EXIT
       END DO
       IF (itest == 1) ibdyty =ibdyty + 1 
    END DO
    
    nb_poroMAILx = ibdyty

    IF (ibdyty == 0) THEN
       CALL LOGMES('no POROx BODIES found')
       CALL LOGMES('if any check BODIES.DAT or MODELS.DAT')
    ELSE
      WRITE(cout,'(I0,1x,A)') nb_poroMAILx,'POROx BODIES found'
      CALL LOGMES(cout)
    END IF
    
    CALL LOGMES('--')

    IF (ibdyty == 0) RETURN   

    ALLOCATE(bdyty(ibdyty),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating bdyty')
    END IF

    ! la map poroMAILx -> MAILx
    ALLOCATE(bdyty2M_bdyty(ibdyty),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating bdyty2M_bdyty')
    END IF

    ! second scaning of the MAILx looking for POROx models
    ! filling the maps bdyty2M_bdyty and M2poro(iM_bdyty)%bdyty
    ! sizing bdyty(ibdyty)%blmty, bdyty(ibdyty)%blmty2M_blmty  

    ibdyty=0

    DO iM_bdyty=1,SIZE(M_bdyty)
       itest  = 0
       iblmty = 0

       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'POROx') THEN 
                      IF (itest == 0) THEN
                         ibdyty =ibdyty + 1
                         bdyty2M_bdyty(ibdyty)=iM_bdyty              
                         M2poro(iM_bdyty)%bdyty=ibdyty
                         itest=1
                      END IF
                      iblmty =iblmty + 1
                   END IF
                END IF
             END DO
          END DO
       END DO
       IF (itest /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty')
          END IF

          ALLOCATE(bdyty(ibdyty)%blmty2M_blmty(iblmty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%blmty2M_blmty')
          END IF

          ALLOCATE(M2poro(iM_bdyty)%blmty(SIZE(M_bdyty(iM_bdyty)%blmty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M2poro%blmty')
          END IF
          M2poro(iM_bdyty)%blmty=0

          ALLOCATE(M2poro(iM_bdyty)%nodty(SIZE(M_bdyty(iM_bdyty)%nodty)),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating M2poro%nodty')
          END IF
          M2poro(iM_bdyty)%nodty=0

       END IF
    END DO
    
    ! third scaning of the MAILx database: 
    ! filling the components of bdyty

    DO ibdyty=1,SIZE(bdyty)

       iM_bdyty=bdyty2M_bdyty(ibdyty)

       iblmty=0

       DO iM_blmty=1,SIZE(M_bdyty(iM_bdyty)%blmty)
          DO iM_model=1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%model)
             
             DO imodel=1,SIZE(modelz)
                IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%model(iM_model) == modelz(imodel)%model) THEN
                   IF (modelz(imodel)%mdlty == 'POROx') THEN 
                      iblmty =iblmty + 1

                      bdyty(ibdyty)%blmty2M_blmty(iblmty)=iM_blmty
                      M2poro(iM_bdyty)%blmty(iM_blmty)=iblmty 

                      ! allocation de la table de connectivite
                      inodty=SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%NODES(inodty),stat=errare)
                      
       
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%NODES')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%NODES(:)=0

                      bdyty(ibdyty)%blmty(iblmty)%blmnb=get_nb_in_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%mdlnb=imodel
!xxx fd test 30/01/09 
!xxx on court circuite le load behaviours

                      bdyty(ibdyty)%blmty(iblmty)%lawnb = &
                      get_bulk_behav_nb(M_bdyty(iM_bdyty)%blmty(iM_blmty)%behav(iM_model))
!xxx
                      
                      
                      
!!!              construction de la table de correspondance entre ddl locaux et ddl globaux
!!!              et des matrices et vecteurs elementaires
!
!!! est ce valide !!!!
!!! il faudrait verifier que le nombre de noeuds lus et dans le type sont les memes ! 
!!!                   

                      bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node = get_N_DOF_by_NODE_MECA_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node = get_N_DOF_by_NODE_THER_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%meca_node = get_N_NODE_MECA_poroEF(modelz(imodel)%ID)
                      bdyty(ibdyty)%blmty(iblmty)%ther_node = get_N_NODE_THER_poroEF(modelz(imodel)%ID)
                      
                      idof_meca = bdyty(ibdyty)%blmty(iblmty)%meca_node*bdyty(ibdyty)%blmty(iblmty)%meca_n_dof_by_node
                      idof_ther = bdyty(ibdyty)%blmty(iblmty)%ther_node*bdyty(ibdyty)%blmty(iblmty)%ther_n_dof_by_node
                      idof = idof_meca + idof_ther 
                      
                      bdyty(ibdyty)%blmty(iblmty)%ndof_meca = idof_meca
                      bdyty(ibdyty)%blmty(iblmty)%ndof_ther = idof_ther
                      bdyty(ibdyty)%blmty(iblmty)%ndof = idof
                      
                      !! Creation du nombre de dof par noeud
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node(idof),stat=errare)
                      DO i = 1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                         bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node(i) = get_N_DOF_by_NODE_poroEF(modelz(imodel)%ID,i)
                      ENDDO
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%meca2poro(idof_meca),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%meca2poro')
                      END IF
                      DO i = 1,SIZE(bdyty(ibdyty)%blmty(iblmty)%meca2poro)
                          bdyty(ibdyty)%blmty(iblmty)%meca2poro(i)= get_MECA_to_PORO(modelz(imodel)%ID, i)
                      ENDDO
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%ther2poro(idof_ther),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%ther2poro')
                      END IF
                      DO i = 1,SIZE(bdyty(ibdyty)%blmty(iblmty)%ther2poro)
                          bdyty(ibdyty)%blmty(iblmty)%ther2poro(i)= get_THER_to_PORO(modelz(imodel)%ID, i)
                      ENDDO
                      
                      ALLOCATE(bdyty(ibdyty)%blmty(iblmty)%edof2gdof(idof),stat=errare)
                      IF (errare /= 0) THEN
                         CALL FATERR(IAM,'error allocating bdyty%blmty%edof2gdof')
                      END IF
                      bdyty(ibdyty)%blmty(iblmty)%edof2gdof(:)=0
                      
!!!              construction du vecteur contenant les valeurs aux points de Gauss

                      nb_external=modelz(imodel)%nb_external_variables
                      nb_internal=modelz(imodel)%nb_internal_variables

!fd 22/04/08, 28/01/09 
! gestion des fields: external (model) & bulk
! on rapatrie tous les fields declares et on s'en sert pour initialiser les champs aux pg 
! on donne au field le rang dans la pile de field stocke au pg

                      nb_ef = get_external_field_nb(imodel) 
                      nb_bf = get_bulk_field_nb(bdyty(ibdyty)%blmty(iblmty)%lawnb)
                      nbf = nb_ef + nb_bf
                      IF (nbf /= 0) THEN
                        ALLOCATE(field_name(nbf))
                        DO IF=1,nb_ef
                           field_name(IF) = get_external_field_name(imodel,IF)
                        ENDDO                      
                        DO IF=1,nb_bf
                           field_name(nb_ef+IF) = get_bulk_field_name(imodel,IF)
                        ENDDO                      

                        CALL init_porogpv_MAILx(iM_bdyty,iM_blmty, & 
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'MECA'), &
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'THER'), &
                                                nb_external,nb_internal, &
                                                nbf,field_name)

                        DEALLOCATE(field_name)

                      ELSE

                        CALL init_porogpv_MAILx(iM_bdyty,iM_blmty, & 
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'MECA'), &
                                                get_N_GP_poroEF(modelz(imodel)%ID, 'THER'), &
                                                nb_external,nb_internal)
                      ENDIF
!
                   END IF
                END IF
             END DO
          END DO
       END DO

       IF (iblmty == 0) THEN
          CALL FATERR(IAM,'no blmty')
       END IF
    END DO

    ! perform some temporary computations
    ! we look for the nodes owing to poroMAILx
    ! 
    ! first we scan the MAILx database and we count the 
    ! nodes owing to a POROx element 
    !
    ! second we fill the bdyty ... database and we determine
    ! the nodty of a node which is the highest one
    !
    
    DO ibdyty=1,SIZE(bdyty)

       iM_bdyty=bdyty2M_bdyty(ibdyty)
       inodty=0

       DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
          DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
             iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)
             IF (M2poro(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
                inodty = inodty+1
                EXIT
             END IF
          END DO
       END DO
       IF (inodty /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%nodty')
          END IF

          ALLOCATE(bdyty(ibdyty)%nodty2M_nodty(inodty),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating bdyty%nodty2M_nodty')
          END IF

          bdyty(ibdyty)%nb_nodes = inodty

          DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
             call new_nodty(bdyty(ibdyty)%nodty(inodty),'     ')
             bdyty(ibdyty)%nodty2M_nodty(inodty)=0
          END DO
       ELSE
          CALL FATERR(IAM,'error computing size of bdyty%nodty')
       END IF

       inodty=0
       DO iM_nodty =1,SIZE(M_bdyty(iM_bdyty)%nodty) 
          itest=0
          DO iG_i=1,SIZE(M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i) 
             iM_blmty=M_bdyty(iM_bdyty)%nod2blmty(iM_nodty)%G_i(iG_i)      
             IF (M2poro(iM_bdyty)%blmty(iM_blmty) /= 0) THEN
                IF (itest == 0) THEN
                   inodty = inodty+1
                   bdyty(ibdyty)%nodty2M_nodty(inodty)=iM_nodty
                   M2poro(iM_bdyty)%nodty(iM_nodty)=inodty
                   itest=1
                END IF
                ! a la peche au type de l'element ...
                iblmty=M2poro(iM_bdyty)%blmty(iM_blmty)
                imodel=bdyty(ibdyty)%blmty(iblmty)%mdlnb
                ! a la peche au type de noeuds de l'element
                DO i = 1,SIZE(M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES)
                   itempo=get_n_dof_by_node_poroEF(modelz(imodel)%ID, i)
                   IF (M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(i) ==  inodty) THEN
                      ! Je suis bien sur le noeuds
                      call new_nodty(bdyty(ibdyty)%nodty(inodty), get_node_name_from_id(itempo))
                   ENDIF
                ENDDO
             END IF
          END DO
       END DO
    END DO

    DO iM_bdyty=1,SIZE(M2poro)
       IF (M2poro(iM_bdyty)%bdyty /=0) THEN
          DO iM_blmty=1,SIZE(M2poro(iM_bdyty)%blmty)
             IF (M2poro(iM_bdyty)%blmty(iM_blmty) /=0) THEN
                ibdyty=M2poro(iM_bdyty)%bdyty
                iblmty=M2poro(iM_bdyty)%blmty(iM_blmty)
                DO inodty=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
                   iM_nodty=M_bdyty(iM_bdyty)%blmty(iM_blmty)%NODES(inodty)
                   bdyty(ibdyty)%blmty(iblmty)%NODES(inodty)=M2poro(iM_bdyty)%nodty(iM_nodty)
                END DO
             END IF
          END DO
       END IF
    END DO

    DO ibdyty=1,SIZE(bdyty)

       iccdof = 0

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          iccdof=iccdof + nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
       END DO

       bdyty(ibdyty)%nbdof=iccdof
       IF (iccdof /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%nodnb(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%dofnb(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vbegin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Xbegin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%V(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vlast(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%X(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vfree(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Vaux(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%residu(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Fext(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Fint(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Finert(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Ireac(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Iaux(iccdof),stat=errare)
          ! Gestion des vitesses Methode ALE
          ALLOCATE(bdyty(ibdyty)%V_ALE_begin(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%V_ALE(iccdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Mask_ALE(bdyty(ibdyty)%nbdof),stat=errare)
          ALLOCATE(bdyty(ibdyty)%Mask_No_ALE(bdyty(ibdyty)%nbdof),stat=errare)
          
          ALLOCATE(bdyty(ibdyty)%periodicnode(iccdof),stat=errare)

          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating X,V')
          END IF
          
          bdyty(ibdyty)%nodnb=0
          bdyty(ibdyty)%dofnb=0
          
          bdyty(ibdyty)%Vbegin=0.d0
          bdyty(ibdyty)%V     =0.d0
          bdyty(ibdyty)%Vlast =0.d0
          bdyty(ibdyty)%Xbegin=0.d0
          bdyty(ibdyty)%X     =0.d0
          bdyty(ibdyty)%Vfree =0.D0
          bdyty(ibdyty)%Vaux  =0.d0
          bdyty(ibdyty)%residu=0.d0
          bdyty(ibdyty)%Fext  =0.d0 
          bdyty(ibdyty)%Fint  =0.d0
          bdyty(ibdyty)%Finert=0.d0
          bdyty(ibdyty)%Ireac =0.d0
          bdyty(ibdyty)%Iaux  =0.d0
          ! Gestion des vitesses Methode ALE
          bdyty(ibdyty)%V_ALE_begin=0.d0
          bdyty(ibdyty)%V_ALE      =0.d0
          bdyty(ibdyty)%Mask_ALE   =0
          bdyty(ibdyty)%Mask_No_ALE=1
          

          bdyty(ibdyty)%is_precon=.FALSE.

          bdyty(ibdyty)%visible     = .TRUE.
          bdyty(ibdyty)%is_periodic = .FALSE.

          bdyty(ibdyty)%periodicnode = 0
          
          ALLOCATE(bdyty(ibdyty)%coorTT(nbdime,SIZE(bdyty(ibdyty)%nodty)),stat=errare)
          bdyty(ibdyty)%coorTT=0.d0
          ALLOCATE(bdyty(ibdyty)%RcoorTT(nbdime),stat=errare)
          bdyty(ibdyty)%RcoorTT=0.d0

       ELSE 

          NULLIFY(bdyty(ibdyty)%nodnb)
          NULLIFY(bdyty(ibdyty)%dofnb)
          NULLIFY(bdyty(ibdyty)%Vbegin)
          NULLIFY(bdyty(ibdyty)%Xbegin)
          NULLIFY(bdyty(ibdyty)%V)
          NULLIFY(bdyty(ibdyty)%Vlast)
          NULLIFY(bdyty(ibdyty)%X)
          NULLIFY(bdyty(ibdyty)%Vfree)
          NULLIFY(bdyty(ibdyty)%Vaux)
          NULLIFY(bdyty(ibdyty)%residu)
          NULLIFY(bdyty(ibdyty)%Fext)
          NULLIFY(bdyty(ibdyty)%Fint)
          NULLIFY(bdyty(ibdyty)%Finert)
          NULLIFY(bdyty(ibdyty)%Ireac)
          NULLIFY(bdyty(ibdyty)%Iaux)
          
          NULLIFY(bdyty(ibdyty)%V_ALE_begin)
          NULLIFY(bdyty(ibdyty)%V_ALE)
          NULLIFY(bdyty(ibdyty)%Mask_ALE)
          NULLIFY(bdyty(ibdyty)%Mask_No_ALE)
          
          NULLIFY(bdyty(ibdyty)%periodicnode)

          bdyty(ibdyty)%is_precon=.FALSE.          

          bdyty(ibdyty)%visible     = .TRUE.
          bdyty(ibdyty)%is_periodic = .FALSE.

          NULLIFY(bdyty(ibdyty)%coorTT)
          NULLIFY(bdyty(ibdyty)%RcoorTT)

          WRITE(cout,'(A25,I5)') 'Warning DISKx without DOF',ibdyty 
          CALL LOGMES(cout)
       END IF

!    array node -> first global ddl

       IF (SIZE(bdyty(ibdyty)%nodty) /= 0) THEN
          ALLOCATE(bdyty(ibdyty)%ccdof(SIZE(bdyty(ibdyty)%nodty)+1),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating ccdof in read_models')
          END IF
       ELSE 
          NULLIFY(bdyty(ibdyty)%ccdof)
          PRINT*,'Warning MAILx without node',ibdyty 
       END IF
    END DO

!  second: filling ordering arrays, and element local 2 global dof correspondance
    
    DO ibdyty=1,SIZE(bdyty)

       iccdof=0

       DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
          bdyty(ibdyty)%ccdof(inodty)=iccdof
          DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
             iccdof=iccdof+1
             bdyty(ibdyty)%nodnb(iccdof)=inodty       ! reverse mapping
             bdyty(ibdyty)%dofnb(iccdof)=idof         ! reverse mapping
          END DO
       END DO
       
       bdyty(ibdyty)%ccdof(size(bdyty(ibdyty)%nodty)+1) = iccdof
       
       DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
          iccdof=0
          DO itempo=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
             inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(itempo)
             DO idof=1,nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
                iccdof=iccdof+1
                bdyty(ibdyty)%blmty(iblmty)%edof2gdof(iccdof)=bdyty(ibdyty)%ccdof(inodty)+idof
             END DO
          END DO
       END DO
      !print *,'nodnb',bdyty(ibdyty)%nodnb
      !print *,'dofnb',bdyty(ibdyty)%dofnb
      !print *,'ccdof',bdyty(ibdyty)%ccdof
      !fd gestion de la renumerotation des noeuds
      ! on cree d'abord un tableau de permutation des noeuds
      ! ensuite on construit une map des ddl
      !! connectivite      
      connectivities => get_connectivity_poroMAILx(ibdyty)

    END DO
   
    IF (itchache) THEN

       PRINT*,'Nombre de corps poroMAILx:',SIZE(bdyty)
       DO ibdyty=1,SIZE(bdyty)
          PRINT*,'==========================================='
          PRINT*,'Corps: ',ibdyty
          PRINT*,'Correspondance dans la base Mailx: ',bdyty2M_bdyty(ibdyty)
          PRINT*,'PARANOIAC TEST local->global->local',M2poro(bdyty2M_bdyty(ibdyty))%bdyty
          PRINT*,'**nodty************'
          DO inodty=1,SIZE(bdyty(ibdyty)%nodty)
             PRINT*,'Noeud: ',inodty
             PRINT*,'Type de noeud: ',get_nodNAME(bdyty(ibdyty)%nodty(inodty))
             PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%nodty2M_nodty(inodty)
             PRINT*,'ddl s commencant a: ',bdyty(ibdyty)%ccdof(inodty),'nombre: ',nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
             PRINT*,'PARANOIAC TEST local->global->local',M2poro(bdyty2M_bdyty(ibdyty))%nodty(bdyty(ibdyty)%nodty2M_nodty(inodty))
          ENDDO
          PRINT*,'**blmty************'
          DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
             PRINT*,'Element: ',iblmty
             PRINT*,'Connectivite:',bdyty(ibdyty)%blmty(iblmty)%NODES(:)
             imdlnb=bdyty(ibdyty)%blmty(iblmty)%mdlnb
             PRINT*,'Num de l element dans la liste poroEF: ',bdyty(ibdyty)%blmty(iblmty)%blmnb
             PRINT*,'ID du modele: ',modelz(imdlnb)%ID,' Num de modele: ',imdlnb 
             PRINT*,'Correspondance dans la base Mailx: ',bdyty(ibdyty)%blmty2M_blmty(iblmty)
          ENDDO
       ENDDO
       

       PRINT*,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

!
    END IF


    DO ibdyty=1,SIZE(bdyty)

      bdyty(ibdyty)%nbdof_meca = bdyty(ibdyty)%nb_nodes * nbDIME
      
    END DO
    
    CALL load_Display_P_poroMAILx

  END SUBROUTINE post_models_poroMAILx
  
  subroutine clean_memory_poroMAILx()
    implicit none
    integer(kind=4)   :: i_bdyty, i_blmty, i
    character(len=80) :: cout

    if( allocated(M2poro) ) then
      do i = 1, size(M2poro)
        if( associated(M2poro(i)%nodty) ) then
          deallocate(M2poro(i)%nodty)
          nullify(M2poro(i)%nodty)
        end if
        if( associated(M2poro(i)%blmty) ) then
          deallocate(M2poro(i)%blmty)
          nullify(M2poro(i)%blmty)
        end if
      end do
      deallocate(M2poro)
    end if
  
    if( .not. allocated(bdyty) ) return

    nb_poroMAILx=0

    do i_bdyty = 1, size(bdyty)

      if( associated(bdyty(i_bdyty)%blmty) ) then
        do i_blmty = 1, size(bdyty(i_bdyty)%blmty)
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%NODES) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%NODES)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%ppsnb)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%n_dof_by_node) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%n_dof_by_node)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%n_dof_by_node)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%edof2gdof)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%meca2poro) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%meca2poro)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%meca2poro)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ther2poro) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ther2poro)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%ther2poro)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%stiffness) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%stiffness)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%stiffness)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%mass) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%mass)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%mass)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%damping) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%damping)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%damping)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fext) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fext)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%Fext)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%Fint) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%Fint)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%Fint)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ttFext) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ttFext)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%ttFext)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%ttFint) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%ttFint)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%ttFint)
          end if
          if( associated(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc) ) then
            deallocate(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc)
            nullify(bdyty(i_bdyty)%blmty(i_blmty)%RHSloc)
          end if

        end do
        deallocate(bdyty(i_bdyty)%blmty)
        nullify(bdyty(i_bdyty)%blmty)
      end if

      if( associated(bdyty(i_bdyty)%blmty2M_blmty) ) then
        deallocate(bdyty(i_bdyty)%blmty2M_blmty)
        nullify(bdyty(i_bdyty)%blmty2M_blmty)
      end if

      if( associated(bdyty(i_bdyty)%nodty) ) then
        deallocate(bdyty(i_bdyty)%nodty)
        nullify(bdyty(i_bdyty)%nodty)
      end if

      if( associated(bdyty(i_bdyty)%nodty2M_nodty) ) then
        deallocate(bdyty(i_bdyty)%nodty2M_nodty)
        nullify(bdyty(i_bdyty)%nodty2M_nodty)
      end if

      if( associated(bdyty(i_bdyty)%Vlast) ) then
        deallocate(bdyty(i_bdyty)%Vlast)
        nullify(bdyty(i_bdyty)%Vlast)
      end if

      if( associated(bdyty(i_bdyty)%Vbegin) ) then
        deallocate(bdyty(i_bdyty)%Vbegin)
        nullify(bdyty(i_bdyty)%Vbegin)
      end if

      if( associated(bdyty(i_bdyty)%V) ) then
        deallocate(bdyty(i_bdyty)%V)
        nullify(bdyty(i_bdyty)%V)
      end if

      if( associated(bdyty(i_bdyty)%V_ALE_begin) ) then
        deallocate(bdyty(i_bdyty)%V_ALE_begin)
        nullify(bdyty(i_bdyty)%V_ALE_begin)
      end if

      if( associated(bdyty(i_bdyty)%V_ALE) ) then
        deallocate(bdyty(i_bdyty)%V_ALE)
        nullify(bdyty(i_bdyty)%V_ALE)
      end if

      if( associated(bdyty(i_bdyty)%Mask_ALE) ) then
        deallocate(bdyty(i_bdyty)%Mask_ALE)
        nullify(bdyty(i_bdyty)%Mask_ALE)
      end if

      if( associated(bdyty(i_bdyty)%Mask_No_ALE) ) then
        deallocate(bdyty(i_bdyty)%Mask_No_ALE)
        nullify(bdyty(i_bdyty)%Mask_No_ALE)
      end if

      if( associated(bdyty(i_bdyty)%Mask_P2U) ) then
        deallocate(bdyty(i_bdyty)%Mask_P2U)
        nullify(bdyty(i_bdyty)%Mask_P2U)
      end if

      if( associated(bdyty(i_bdyty)%Xbegin) ) then
        deallocate(bdyty(i_bdyty)%Xbegin)
        nullify(bdyty(i_bdyty)%Xbegin)
      end if

      if( associated(bdyty(i_bdyty)%X) ) then
        deallocate(bdyty(i_bdyty)%X)
        nullify(bdyty(i_bdyty)%X)
      end if

      if( associated(bdyty(i_bdyty)%Fext) ) then
        deallocate(bdyty(i_bdyty)%Fext)
        nullify(bdyty(i_bdyty)%Fext)
      end if

      if( associated(bdyty(i_bdyty)%Fint) ) then
        deallocate(bdyty(i_bdyty)%Fint)
        nullify(bdyty(i_bdyty)%Fint)
      end if

      if( associated(bdyty(i_bdyty)%Ireac) ) then
        deallocate(bdyty(i_bdyty)%Ireac)
        nullify(bdyty(i_bdyty)%Ireac)
      end if

      if( associated(bdyty(i_bdyty)%Iaux) ) then
        deallocate(bdyty(i_bdyty)%Iaux)
        nullify(bdyty(i_bdyty)%Iaux)
      end if

      if( associated(bdyty(i_bdyty)%Vfree) ) then
        deallocate(bdyty(i_bdyty)%Vfree)
        nullify(bdyty(i_bdyty)%Vfree)
      end if

      if( associated(bdyty(i_bdyty)%Vaux) ) then
        deallocate(bdyty(i_bdyty)%Vaux)
        nullify(bdyty(i_bdyty)%Vaux)
      end if

      if( associated(bdyty(i_bdyty)%residu) ) then
        deallocate(bdyty(i_bdyty)%residu)
        nullify(bdyty(i_bdyty)%residu)
      end if

      if( associated(bdyty(i_bdyty)%Finert) ) then
        deallocate(bdyty(i_bdyty)%Finert)
        nullify(bdyty(i_bdyty)%Finert)
      end if

      if( associated(bdyty(i_bdyty)%ccdof) ) then
        deallocate(bdyty(i_bdyty)%ccdof)
        nullify(bdyty(i_bdyty)%ccdof)
      end if

      if( associated(bdyty(i_bdyty)%nodnb) ) then
        deallocate(bdyty(i_bdyty)%nodnb)
        nullify(bdyty(i_bdyty)%nodnb)
      end if

      if( associated(bdyty(i_bdyty)%dofnb) ) then
        deallocate(bdyty(i_bdyty)%dofnb)
        nullify(bdyty(i_bdyty)%dofnb)
      end if

      if( associated(bdyty(i_bdyty)%coorTT) ) then
        deallocate(bdyty(i_bdyty)%coorTT)
        nullify(bdyty(i_bdyty)%coorTT)
      end if

      if( associated(bdyty(i_bdyty)%RcoorTT) ) then
        deallocate(bdyty(i_bdyty)%RcoorTT)
        nullify(bdyty(i_bdyty)%RcoorTT)
      end if

      if( associated(bdyty(i_bdyty)%poro_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%poro_driven_dof)
          if( associated(bdyty(i_bdyty)%poro_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%poro_driven_dof(i)%time_evolution%x)
            nullify(bdyty(i_bdyty)%poro_driven_dof(i)%time_evolution%x)
          end if

          if( associated(bdyty(i_bdyty)%poro_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%poro_driven_dof(i)%time_evolution%fx)
            nullify(bdyty(i_bdyty)%poro_driven_dof(i)%time_evolution%fx)
          end if
        end do

        deallocate(bdyty(i_bdyty)%poro_driven_dof)
        nullify(bdyty(i_bdyty)%poro_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%Tdriv) ) then
        deallocate(bdyty(i_bdyty)%Tdriv)
        nullify(bdyty(i_bdyty)%Tdriv)
      end if
      if( associated(bdyty(i_bdyty)%Vdriv) ) then
        deallocate(bdyty(i_bdyty)%Vdriv)
        nullify(bdyty(i_bdyty)%Vdriv)
      end if
      if( associated(bdyty(i_bdyty)%VdrivBeg) ) then
        deallocate(bdyty(i_bdyty)%VdrivBeg)
        nullify(bdyty(i_bdyty)%VdrivBeg)
      end if
      if( associated(bdyty(i_bdyty)%Xdriv) ) then
        deallocate(bdyty(i_bdyty)%Xdriv)
        nullify(bdyty(i_bdyty)%Xdriv)
      end if
      if( associated(bdyty(i_bdyty)%is_Tdriv_active) ) then
        deallocate(bdyty(i_bdyty)%is_Tdriv_active)
        nullify(bdyty(i_bdyty)%is_Tdriv_active)
      end if

      if( associated(bdyty(i_bdyty)%flux_driven_dof) ) then
        do i = 1, size(bdyty(i_bdyty)%flux_driven_dof)
          if( associated(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x) ) then
            deallocate(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x)
            nullify(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%x)
          end if
          if( associated(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx) ) then
            deallocate(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx)
            nullify(bdyty(i_bdyty)%flux_driven_dof(i)%time_evolution%fx)
          end if
        end do
        deallocate(bdyty(i_bdyty)%flux_driven_dof)
        nullify(bdyty(i_bdyty)%flux_driven_dof)
      end if

      if( associated(bdyty(i_bdyty)%Fdriv) ) then
        deallocate(bdyty(i_bdyty)%Fdriv)
        nullify(bdyty(i_bdyty)%Fdriv)
      end if

      if( associated(bdyty(i_bdyty)%FdrivBeg) ) then
        deallocate(bdyty(i_bdyty)%FdrivBeg)
        nullify(bdyty(i_bdyty)%FdrivBeg)
      end if

      if( associated(bdyty(i_bdyty)%drvdofs) ) then
        deallocate(bdyty(i_bdyty)%drvdofs)
        nullify(bdyty(i_bdyty)%drvdofs)
      end if

      if( associated(bdyty(i_bdyty)%drvvalues) ) then
        deallocate(bdyty(i_bdyty)%drvvalues)
        nullify(bdyty(i_bdyty)%drvvalues)
      end if

      if( associated(bdyty(i_bdyty)%RHS) ) then
        deallocate(bdyty(i_bdyty)%RHS)
        nullify(bdyty(i_bdyty)%RHS)
      end if

      if( associated(bdyty(i_bdyty)%nodes_precon) ) then
        deallocate(bdyty(i_bdyty)%nodes_precon)
        nullify(bdyty(i_bdyty)%nodes_precon)
      end if

      if( associated(bdyty(i_bdyty)%W_precon) ) then
        deallocate(bdyty(i_bdyty)%W_precon)
        nullify(bdyty(i_bdyty)%W_precon)
      end if

      if( associated(bdyty(i_bdyty)%W_precon_T) ) then
        deallocate(bdyty(i_bdyty)%W_precon_T)
        nullify(bdyty(i_bdyty)%W_precon_T)
      end if

      if( associated(bdyty(i_bdyty)%Vaux_precon) ) then
        deallocate(bdyty(i_bdyty)%Vaux_precon)
        nullify(bdyty(i_bdyty)%Vaux_precon)
      end if

      if( associated(bdyty(i_bdyty)%p2g) ) then
        deallocate(bdyty(i_bdyty)%p2g)
        nullify(bdyty(i_bdyty)%p2g)
      end if

      if( associated(bdyty(i_bdyty)%g2p) ) then
        deallocate(bdyty(i_bdyty)%g2p)
        nullify(bdyty(i_bdyty)%g2p)
      end if

      if( associated(bdyty(i_bdyty)%periodicnode) ) then
        deallocate(bdyty(i_bdyty)%periodicnode)
        nullify(bdyty(i_bdyty)%periodicnode)
      end if

      call erase_system(bdyty(i_bdyty)%g_sys)
    end do

    deallocate(bdyty)

    if( allocated(bdyty2M_bdyty) ) deallocate(bdyty2M_bdyty)

  end subroutine
  
 function get_nb_gp_poroMAILx(i_bdyty, i_blmty)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> number of gauss points in the element
   integer :: get_nb_gp_poroMAILx
   !
   integer :: iM_bdyty, iM_blmty

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   if( associated( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca ) ) then
       get_nb_gp_poroMAILx = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1 )
   else
       get_nb_gp_poroMAILx = 0
   end if

 end function get_nb_gp_poroMAILx

 subroutine get_field_poroMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:stress, 2:strain, 3:meca_internal, 4:grad, 5:flux, 6:ther_internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(out) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%stress(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%stress(:)
     end do
   case( 2 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%strain(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%strain(:)
     end do
   case( 3 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%internal(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%internal(:)
     end do
   case( 4 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%grad(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%grad(:)
     end do
   case( 5 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%flux(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%flux(:)
     end do
   case( 6 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%internal(:) )
       field_array(1:field_size,i_gp) = M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%internal(:)
     end do
   end select

 end subroutine get_field_poroMAILx

 subroutine set_field_poroMAILx(i_bdyty, i_blmty, i_field, field_array)
   implicit none
   !> body number
   integer, intent(in) :: i_bdyty
   !> element number
   integer, intent(in) :: i_blmty
   !> type of field (1:stress, 2:strain, 3:internal, 4:grad, 5:flux, 6:ther_internal)
   integer, intent(in) :: i_field
   !> gauss point field value
   real(kind=8), dimension(:,:), intent(in) :: field_array
   !
   integer :: iM_bdyty, iM_blmty, i_gp, field_size

   iM_bdyty = bdyty2M_bdyty(i_bdyty)
   iM_blmty = bdyty(i_bdyty)%blmty2M_blmty(i_blmty)

   select case( i_field )
   case( 1 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%stress(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,1)%stress(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%stress(:) = field_array(1:field_size,i_gp)
     end do
   case( 2 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%strain(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,1)%strain(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%strain(:) = field_array(1:field_size,i_gp)
     end do
   case( 3 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%internal(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,1)%internal(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_meca(i_gp,2)%internal(:) = field_array(1:field_size,i_gp)
     end do
   case( 4 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%grad(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,1)%grad(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%grad(:) = field_array(1:field_size,i_gp)
     end do
   case( 5 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther, 1)
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%flux(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,1)%flux(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%flux(:) = field_array(1:field_size,i_gp)
     end do
   case( 6 )
     do i_gp = 1, size(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther, 1)
       if( .not. associated(M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%internal) ) cycle
       field_size = size( M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%internal(:) )
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,1)%internal(:) = field_array(1:field_size,i_gp)
       M_bdyty(iM_bdyty)%blmty(iM_blmty)%poro_gpv_ther(i_gp,2)%internal(:) = field_array(1:field_size,i_gp)
     end do
   end select

 end subroutine set_field_poroMAILx

 integer function get_nb_elements_poroMAILx(i_bdyty)
   implicit none
   integer, intent(in) :: i_bdyty

   get_nb_elements_poroMAILx = size(bdyty(i_bdyty)%blmty)

 end function

 subroutine check_properties_poroMAILx()
   implicit none

   !                         123456789012345678901234567
   CHARACTER(len=25) :: IAM='poroMAILx::check_properties'
   CHARACTER(len=80) :: cout

   integer :: ibdyty,iblmty,iM_bdyty,iM_blmty
   
   if (nb_poroMAILx == 0) return
   
   DO ibdyty=1,SIZE(bdyty)
   
     iM_bdyty = bdyty2M_bdyty(ibdyty)
   
     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
           
       iM_blmty = bdyty(ibdyty)%blmty2M_blmty(iblmty)
 
       call check_elementary_ppset(bdyty(ibdyty)%blmty(iblmty)%blmnb,bdyty(ibdyty)%blmty(iblmty)%ppsnb,&
                                   iM_bdyty,iM_blmty)
      
     enddo
   enddo
 end subroutine check_properties_poroMAILx

 subroutine get_nb_gp_by_elem_poroMAILx(elems, nb_gp_m, nb_gp_t, nb_elems)

    implicit none
    character(len=5), dimension(:), pointer :: elems
    integer         , dimension(:), pointer :: nb_gp_m, nb_gp_t
    integer :: nb_elems
    !
    integer :: i_elem

    CALL init_poroEF

    nb_elems = nb_poroEF
    allocate(elems(nb_elems))
    allocate(nb_gp_m(nb_elems))
    allocate(nb_gp_t(nb_elems))

    do i_elem = 1, nb_elems
        elems(i_elem)   = poroEF(i_elem)%NAME
        nb_gp_m(i_elem) = get_N_GP_poroEF( poroEF(i_elem)%NAME, 'MECA' )
        nb_gp_t(i_elem) = get_N_GP_poroEF( poroEF(i_elem)%NAME, 'THER' )
    end do

 end subroutine get_nb_gp_by_elem_poroMAILx
 
END MODULE poroMAILx
