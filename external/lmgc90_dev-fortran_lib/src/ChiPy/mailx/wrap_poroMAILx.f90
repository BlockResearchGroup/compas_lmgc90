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
MODULE wrap_poroMAILx                                       

  USE ISO_C_BINDING

  USE poroMAILx,ONLY:&
  
                load_models_poroMAILx, &
                update_existing_entities_poroMAILx, &
                load_behaviours_poroMAILx , &
                push_ppset_poroMAILx, &
                set_without_renum_poroMAILx,& 
                read_in_driven_dof_poroMAILx, &
                write_out_driven_dof_poroMAILx, &
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
                write_xxx_dof_poroMAILx, &
                update_dof_poroMAILx, &
                update_bulk_poroMAILx, &
                compute_residue_norm_poroMAILx, &
                get_vector_poroMAILx, &
                get_N_NODE_poroMAILx, &
                get_nb_elements_poroMAILx, &
                increment_poroMAILx , &
                apply_drvdof_KT_poroMAILx, &
                set_Matrix_Storage_poroMAILx, &
                set_Matrix_Shape_poroMAILx, &
                get_meca_field_rank, get_ther_field_rank, &
                set_ther_field_bynode, set_ther_field_byelem, &
                set_meca_field_bynode, set_meca_field_byelem, &
                get_meca_vfield_rank, get_ther_vfield_rank, &
                set_ther_vfield_bynode, set_ther_vfield_byelem, &
                set_meca_vfield_bynode, set_meca_vfield_byelem, &
                load_ALE_poroMAILx, &
                put_vector_poroMAILx , &
                get_2DNodalStress_poroMAILx, &
                get_3DNodalStress_poroMAILx, &
                get_2DNodalStrain_poroMAILx, &
                get_3DNodalStrain_poroMAILx, &
                get_NodalGrad_poroMAILx    , &
                get_NodalFlux_poroMAILx    , &
                get_Internal_poroMAILx     , &
                compute_configurationTT_poroMAILx, &
                set_precon_body_poroMAILx, &
                compute_precon_W_poroMAILx, &
                get_coor_poroMAILx, &
                get_All_poroMAILx, &
                get_connectivity_poroMAILx, &
                set_vlocy_drvdof_poroMAILx, &
                Add_Field_Load_bynode_poroMAILx, &
                get_write_DOF_poroMAILx, &
                post_models_poroMAILx  , &
                clean_memory_poroMAILx, &
                check_properties_poroMAILx, &
                get_nb_gp_by_elem_poroMAILx
                


 
 use utilities, only: faterr,logmes

CONTAINS

!!!---------------------------------------------------
    SUBROUTINE LoadModels() bind(c, name='poroMAILx_LoadModels')
      IMPLICIT NONE

       CALL load_models_poroMAILx
       CALL update_existing_entities_poroMAILx
       
    END SUBROUTINE
    
    SUBROUTINE LoadBehaviours() bind(c, name='poroMAILx_LoadBehaviours')
      IMPLICIT NONE

       CALL load_behaviours_poroMAILx 

    END SUBROUTINE
    
    SUBROUTINE PushProperties() bind(c, name='poroMAILx_PushProperties')
      IMPLICIT NONE

       CALL push_ppset_poroMAILx

    END SUBROUTINE
    
    SUBROUTINE ReadDrivenDof() bind(c, name='poroMAILx_ReadDrivenDof')
      IMPLICIT NONE
       !! PURPOSE
       !!  read DRV_DOF.DAT file

       CALL read_in_driven_dof_poroMAILx

    END SUBROUTINE
    
    SUBROUTINE WriteDrivenDof() bind(c, name='poroMAILx_WriteDrivenDof')
      IMPLICIT NONE
       !! PURPOSE
       !!  write DRV_DOF.OUT file

       CALL write_out_driven_dof_poroMAILx

    END SUBROUTINE
    
    subroutine ReadIniDof(step) bind(c, name='poroMAILx_ReadIniDof')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_dof_poroMAILx(step)

    end subroutine

    subroutine ReadIniMecaDof(step) bind(c, name='poroMAILx_ReadIniMecaDof')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_meca_dof_poroMAILx(step)

    end subroutine

    subroutine ReadIniGPV(step) bind(c, name='poroMAILx_ReadIniGPV')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_gpv_poroMAILx(step)

    end subroutine

    subroutine ReadIniMecaGPV(step) bind(c, name='poroMAILx_ReadIniMecaGPV')
       implicit none
       integer(c_int), intent(in), value :: step

       call read_in_meca_gpv_poroMAILx(step)

    end subroutine

    SUBROUTINE WriteLastDof() bind(c, name='poroMAILx_WriteLastDof')
       IMPLICIT NONE
       INTEGER :: ifrom,ito
       !! PURPOSE
       !!  write ascii DOF.LAST file

          ifrom = 1  
          ito   = get_nb_poroMAILx()
          CALL write_xxx_dof_poroMAILx(2,ifrom,ito)

    END SUBROUTINE

    subroutine ComputeMass() bind(c, name='poroMAILx_ComputeMass')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp mass   ')
      call start_itimer(timer_id)

      call compute_mass_poroMAILx 

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeFext() bind(c, name='poroMAILx_ComputeFext')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp Fext   ')
      call start_itimer(timer_id)

      call compute_Fext_poroMAILx

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeBulk() bind(c, name='poroMAILx_ComputeBulk')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp bulk   ')
      call start_itimer(timer_id)

      call compute_bulk_poroMAILx(0)

    end subroutine

    subroutine ComputeDamping() bind(c, name='poroMAILx_ComputeDamping')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp damp   ')
      call start_itimer(timer_id)

      call compute_damping_poroMAILx()

      call stop_itimer(timer_id)

    end subroutine
    
    subroutine AssembKT() bind(c, name='poroMAILx_AssembKT')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] assemb KT   ')
      call start_itimer(timer_id)

      call assemb_KT_poroMAILx
      call apply_drvdof_KT_poroMAILx

      call stop_itimer(timer_id)

    end subroutine
    
    subroutine AssembRHS() bind(c, name='poroMAILx_AssembRHS')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] assemb RHS  ')
      call start_itimer(timer_id)

      call assemb_RHS_poroMAILx

      call stop_itimer(timer_id)

    end subroutine
    
    subroutine ComputeFreeVelocity() bind(c, name='poroMAILx_ComputeFreeVelocity')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp Free V ')
      call start_itimer(timer_id)

      call compute_free_vlocy_poroMAILx 

      call stop_itimer(timer_id)

    end subroutine
    
    
    subroutine ComputeDof() bind(c, name='poroMAILx_ComputeDof')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp dof    ')
      call start_itimer(timer_id)

      call compute_dof_poroMAILx 

      call stop_itimer(timer_id)

    end subroutine
    
    subroutine DisplayOutDof() bind(c, name='poroMAILx_DisplayOutDof')
      implicit none
      integer(kind=4) :: ifrom,ito

      ifrom = 1  
      ito   = get_nb_poroMAILx()
      call write_xxx_dof_poroMAILx(6,ifrom,ito)

    end subroutine

    subroutine UpdateDof() bind(c, name='poroMAILx_UpdateDof')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] update dof  ')
      call start_itimer(timer_id)

      call update_dof_poroMAILx 

      call stop_itimer(timer_id)

    end subroutine

    subroutine UpdateBulk() bind(c, name='poroMAILx_UpdateBulk')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] update bulk ')
      call start_itimer(timer_id)

      call compute_bulk_poroMAILx(1)
      call update_bulk_poroMAILx 

      call stop_itimer(timer_id)

    end subroutine

    subroutine ComputeGrad() bind(c, name='poroMAILx_ComputeGrad')
      use timer
      implicit none
      integer(kind=4), save :: timer_id = 0

      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] comp grad   ')
      call start_itimer(timer_id)

      call compute_bulk_poroMAILx(1)

      call stop_itimer(timer_id)

    end subroutine

    SUBROUTINE GetBodyVector(cvalue1_c,ivalue1,rvect,ivalue2,ivalue3) bind(c, name='poroMAILx_GetBodyVector')
      use overall, only: nbDIME
      IMPLICIT NONE
      character(c_char),dimension(5),intent(in) :: cvalue1_c
      integer(c_int), intent(in), value         :: ivalue1
      integer(c_int)                            :: ivalue2,ivalue3
      type(c_ptr)                               :: rvect
      !
      real(kind=c_double), dimension(:,:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      select case( cvalue1 )
      case('P____','Pbeg_','Qint_','Qext_')
        ivalue2 = 1
      case default
        ivalue2 = nbDIME
      end select

      ivalue3 = get_N_NODE_poroMAILx(ivalue1)
      allocate(vector(ivalue2,ivalue3))
      CALL get_vector_poroMAILx(cvalue1,ivalue1,vector,ivalue2*ivalue3)

      rvect = c_loc(vector(1,1))

    END SUBROUTINE

    SUBROUTINE IncrementStep() bind(c, name='poroMAILx_IncrementStep')
      IMPLICIT NONE
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method

       CALL increment_poroMAILx 

    END SUBROUTINE

    SUBROUTINE WithoutRenumbering() bind(c, name='poroMAILx_WithoutRenumbering')
      IMPLICIT NONE

       CALL set_without_renum_poroMAILx()

    END SUBROUTINE

    SUBROUTINE BandStorage() bind(c, name='poroMAILx_BandStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_poroMAILx('band____')

    END SUBROUTINE

    SUBROUTINE SparseStorage() bind(c, name='poroMAILx_SparseStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_poroMAILx('sparse__')

    END SUBROUTINE

    SUBROUTINE ExplodedStorage() bind(c, name='poroMAILx_ExplodedStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_poroMAILx('exploded')

    END SUBROUTINE


    SUBROUTINE DiagonalStorage() bind(c, name='poroMAILx_DiagonalStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_poroMAILx('diagonal')

    END SUBROUTINE

    SUBROUTINE SkylineStorage() bind(c, name='poroMAILx_SkylineStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_poroMAILx('skyline_')

    END SUBROUTINE

    SUBROUTINE FullStorage() bind(c, name='poroMAILx_FullStorage')
      IMPLICIT NONE

       CALL set_Matrix_Storage_poroMAILx('full____')

    END SUBROUTINE

    SUBROUTINE SymmetricShape() bind(c, name='poroMAILx_SymmetricShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_poroMAILx('sym_____')

    END SUBROUTINE

    SUBROUTINE UnspecifiedShape() bind(c, name='poroMAILx_UnspecifiedShape')
      IMPLICIT NONE

       CALL set_Matrix_Shape_poroMAILx('std_____')

    END SUBROUTINE

    !fd: fonction qui renvoie le nombre de noeuds d'un poroMAILx
    function GetNbNodes(ibdyty) bind(c, name='poroMAILx_GetNbNodes')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbNodes ! nombre de noeuds d un poroMAILx 

       GetNbNodes = get_N_NODE_poroMAILx(ibdyty)

    end function GetNbNodes

    !fd: fonction qui renvoie le nombre d'elements d'un poroMAILx
    function GetNbElements(ibdyty) bind(c, name='poroMAILx_GetNbElements')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbElements

       GetNbElements = get_nb_elements_poroMAILx(ibdyty)

    end function GetNbElements

    SUBROUTINE SetMecaScalarFieldByNode(IdBody,f_rank,f,f_size) bind(c, name='poroMAILx_SetMecaScalarFieldByNode')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,f_rank,f_size
      REAL(C_DOUBLE), INTENT(in), dimension(f_size) :: f
       !! PURPOSE
       !!  Update an external field on a given body
       !!  You need to set this field in your models.dat
     
       !print *,IdBody,f_rank,f_size
       !print *,f
       CALL set_meca_field_bynode(IdBody,f_rank,f_size,f)

    END SUBROUTINE
    
    SUBROUTINE SetTherScalarFieldByNode(IdBody,f_rank,f,f_size) bind(c, name='poroMAILx_SetTherScalarFieldByNode')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,f_rank,f_size
      REAL(C_DOUBLE), INTENT(in), dimension(f_size) :: f
       !! PURPOSE
       !!  Update an external field on a given body
       !!  You need to set this field in your models.dat
     
       !print *,IdBody,f_rank,f_size
       !print *,f
       CALL set_ther_field_bynode(IdBody,f_rank,f_size,f)

    END SUBROUTINE
    
    subroutine SetMecaScalarFieldByElem(ibdyty,f_rank,f,f_size) bind(c, name='poroMAILx_SetMecaScalarFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty, f_rank, f_size
      real(c_double), intent(in), dimension(f_size) :: f

      call set_meca_field_byelem(ibdyty,f_rank,f_size,f)

    end subroutine

    subroutine SetTherScalarFieldByElem(ibdyty,f_rank,f,f_size) bind(c, name='poroMAILx_SetTherScalarFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty, f_rank, f_size
      real(c_double), intent(in), dimension(f_size) :: f

      call set_ther_field_byelem(ibdyty,f_rank,f_size,f)

    end subroutine

    function getTherScalarFieldRank(ibdyty, iblmty, name) bind(c, name='poroMAILx_GetTherScalarFieldRank')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: iblmty
      character(c_char), dimension(*)   :: name
      integer(c_int) :: getTherScalarFieldRank
      !
      integer(kind=4)   :: i
      character(len=30) :: f_name

      f_name = ''
      i = 1
      do while( name(i) /= c_null_char .and. i <= 30 )
        f_name = f_name(1:i-1) // name(i)
        i = i+1
      end do

      getTherScalarFieldRank = get_ther_field_rank(ibdyty, iblmty, f_name)

    end function

    function getTherVectorFieldRank(ibdyty, iblmty, name) bind(c, name='poroMAILx_GetTherVectorFieldRank')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: iblmty
      character(c_char), dimension(*)   :: name
      integer(c_int) :: getTherVectorFieldRank
      !
      integer(kind=4)   :: i
      character(len=30) :: f_name

      f_name = ''
      i = 1
      do while( name(i) /= c_null_char .and. i <= 30 )
        f_name = f_name(1:i-1) // name(i)
        i = i+1
      end do

      getTherVectorFieldRank = get_ther_vfield_rank(ibdyty, iblmty, f_name)

    end function

    function getMecaScalarFieldRank(ibdyty, iblmty, name) bind(c, name='poroMAILx_GetMecaScalarFieldRank')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: iblmty
      character(c_char), dimension(*)   :: name
      integer(c_int) :: getMecaScalarFieldRank
      !
      integer(kind=4)   :: i
      character(len=30) :: f_name

      f_name = ''
      i = 1
      do while( name(i) /= c_null_char .and. i <= 30 )
        f_name = f_name(1:i-1) // name(i)
        i = i+1
      end do

      getMecaScalarFieldRank = get_meca_field_rank(ibdyty, iblmty, f_name)

    end function

    function getMecaVectorFieldRank(ibdyty, iblmty, name) bind(c, name='poroMAILx_GetMecaVectorFieldRank')
      implicit none
      integer(c_int), intent(in), value :: ibdyty
      integer(c_int), intent(in), value :: iblmty
      character(c_char), dimension(*)   :: name
      integer(c_int) :: getMecaVectorFieldRank
      !
      integer(kind=4)   :: i
      character(len=30) :: f_name

      f_name = ''
      i = 1
      do while( name(i) /= c_null_char .and. i <= 30 )
        f_name = f_name(1:i-1) // name(i)
        i = i+1
      end do

      getMecaVectorFieldRank = get_meca_vfield_rank(ibdyty, iblmty, f_name)

    end function

    subroutine SetMecaVectorFieldByNode(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='poroMAILx_SetMecaVectorFieldByNode')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_meca_vfield_bynode(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    subroutine SetMecaVectorFieldByElem(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='poroMAILx_SetMecaVectorFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_meca_vfield_byelem(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    subroutine SetTherVectorFieldByNode(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='poroMAILx_SetTherVectorFieldByNode')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_ther_vfield_bynode(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    subroutine SetTherVectorFieldByElem(ibdyty,f_rank,f,f_size1,f_size2) bind(c, name='poroMAILx_SetTherVectorFieldByElement')
      implicit none
      integer(c_int), intent(in), value :: ibdyty,f_rank,f_size1,f_size2
      real(c_double), intent(in), dimension(f_size2,f_size1) :: f

      call set_ther_vfield_byelem(ibdyty,f_rank,f,f_size2,f_size1)

    end subroutine

    SUBROUTINE LoadALE(IdBody) bind(c, name='poroMAILx_LoadALE')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
       !! PURPOSE
       !!  Apply ALE Formulation in fluid zone

       CALL load_ALE_poroMAILx(IdBody)

    END SUBROUTINE

    subroutine PutBodyVector(cvalue1_c, ivalue1, mat_in, dim1, dim2) bind(c, name='poroMAILx_PutBodyVector')
      implicit none
      character(c_char), intent(in), dimension(5) :: cvalue1_c
      integer(c_int)   , intent(in), value        :: ivalue1, dim1, dim2
      type(c_ptr)                  , value        :: mat_in
      !
      real(kind=8), dimension(:), pointer :: vector
      character(len=5) :: cvalue1
      integer :: i

      cvalue1 = ''
      do i=1,5
        cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
      end do

      call c_f_pointer(cptr=mat_in, fptr=vector, shape=(/dim1*dim2/))
      call put_vector_poroMAILx(cvalue1, ivalue1, vector, dim1*dim2)

    end subroutine

    function ComputeResidueNorm() bind(c, name='poroMAILx_ComputeResidueNorm')
      use timer
      implicit none
      real(c_double) :: ComputeResidueNorm
      integer(kind=4), save :: timer_id = 0
      !
      if( get_nb_poroMAILx() < 1 ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[poroM] compResNorm ')
      call start_itimer(timer_id)

      call compute_residue_norm_poroMAILx(ComputeResidueNorm)

      call stop_itimer(timer_id)

    end function

    SUBROUTINE GetStress(ivalue1,stress,ivalue2,ivalue3,ivalue4) bind(c, name='poroMAILx_GetStress')
      use overall, only: nbDIME
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1, ivalue4
      integer(c_int)                    :: ivalue2, ivalue3
      type(c_ptr)                       :: stress
      !
      real(c_double), dimension(:,:), pointer :: S

      ivalue3 = get_N_NODE_poroMAILx(ivalue1)

      if( nbDIME == 2 ) then
        ivalue2 = 5
        allocate(S(ivalue2,ivalue3))
        CALL get_2DNodalStress_poroMAILx(ivalue1,S)
      else
        ivalue2 = 7
        allocate(S(ivalue2,ivalue3))
        CALL get_3DNodalStress_poroMAILx(ivalue1,S,ivalue4)
      end if

      stress = c_loc(S(1,1))

    END SUBROUTINE
    
    SUBROUTINE GetStrain(ivalue1,strain,ivalue2,ivalue3,ivalue4) bind(c, name='poroMAILx_GetStrain')
      use overall, only: nbDIME
      IMPLICIT NONE
      integer(c_int), intent(in), value :: ivalue1, ivalue4
      integer(c_int)                    :: ivalue2, ivalue3
      type(c_ptr)                       :: strain
      !
      real(c_double), dimension(:,:), pointer :: E

      ivalue3 = get_N_NODE_poroMAILx(ivalue1)

      if( nbDIME == 2 ) then
        ivalue2 = 5
        allocate(E(ivalue2,ivalue3))
        CALL get_2DNodalStrain_poroMAILx(ivalue1,E)
      else
        ivalue2 = 7
        allocate(E(ivalue2,ivalue3))
        CALL get_3DNodalStrain_poroMAILx(ivalue1,E,ivalue4)
      end if

      strain = c_loc(E(1,1))

    END SUBROUTINE

    SUBROUTINE ComputeContactDetectionConfiguration() bind(c, name='poroMAILx_ComputeContactDetectionConfiguration')
       !! PURPOSE
       !!  computes the contact configuration
       IMPLICIT NONE

       CALL compute_configurationTT_poroMAILx

    END SUBROUTINE ComputeContactDetectionConfiguration

    !da: fonction qui applique la precondensation a tous les coprs mailles pour
    !    la poro
    SUBROUTINE SetPreconAllBodies() bind(c, name='poroMAILx_SetPreconAllBodies')
       IMPLICIT NONE

       ! variables locales
       integer :: ibdyty ! indice de boucle sur les corps mailles pour la meca
 
       ! on applique la precondensation a chaque corps
       ! fd todo ce numero n'est pas le bon ca doit etre l'indice dans mailx et pas poroMAILx
       do ibdyty=1, get_nb_poroMAILx()
          CALL set_precon_body_poroMAILx(ibdyty)
       end do

    END SUBROUTINE SetPreconAllBodies

    SUBROUTINE ComputePreconW() bind(c, name='poroMAILx_ComputePreconW')
      IMPLICIT NONE

       CALL compute_precon_W_poroMAILx

    END SUBROUTINE

    !da: fonction qui renvoie le nombre de poroMAILx
    function GetNbPoroMAILx() bind(c, name='poroMAILx_GetNbPoroMAILx')

       implicit none

       ! valeur de retour
       integer(c_int) :: GetNbPoroMAILx ! nombre mailles pour la poro mecanique 

       GetNbPoroMAILx = get_nb_poroMAILx()

    end function GetNbPoroMAILx

    subroutine GetCoorPoroMAILx(idBody, ptr, dim1, dim2) BIND(c, name='poroMAILx_GetCoor')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_coor_poroMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetCoorPoroMAILx

    subroutine GetAllPoroMAILx(idBody,ptr, dim1, dim2) BIND(c, name='poroMAILx_GetAll')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_All_poroMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetAllPoroMAILx

    subroutine GetGrad(ivalue1,grad,ivalue2,ivalue3) bind(c, name='poroMAILx_GetGrad')
      use overall, only: nbDIME
      implicit none
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2, ivalue3
      type(c_ptr)                       :: grad
      !
      real(c_double), dimension(:,:), pointer :: E

      ivalue2 = 3
      ivalue3 = get_N_NODE_poroMAILx(ivalue1)
      allocate(E(ivalue2,ivalue3))

      call get_NodalGrad_poroMAILx(ivalue1,E)

      grad = c_loc(E(1,1))

    end subroutine

    subroutine GetFlux(ivalue1,grad,ivalue2,ivalue3) bind(c, name='poroMAILx_GetFlux')
      use overall, only: nbDIME
      implicit none
      integer(c_int), intent(in), value :: ivalue1
      integer(c_int)                    :: ivalue2, ivalue3
      type(c_ptr)                       :: grad
      !
      real(c_double), dimension(:,:), pointer :: S

      ivalue2 = 3
      ivalue3 = get_N_NODE_poroMAILx(ivalue1)
      allocate(S(ivalue2,ivalue3))

      call get_NodalFlux_poroMAILx(ivalue1,S)

      grad = c_loc(S(1,1))

    end subroutine

    subroutine GetInternalPoroMAILx(idBody,ptr, dim1, dim2) BIND(c, name='poroMAILx_GetInternal')
      implicit none

      integer(C_INT), intent(in), value :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1, dim2

      real(kind=8), dimension(:,:), pointer :: all

      all => get_Internal_poroMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1,1))
        dim1 = size(all,1)
        dim2 = size(all,2)
      else
        ptr  = c_null_ptr
        dim1 = 0
        dim2 = 0
      end if

    end subroutine GetInternalPoroMAILx

    subroutine GetConnectivityPoroMAILx(idBody, ptr, dim1) BIND(c, name='poroMAILx_GetConnectivity')
      implicit none

      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      type(c_ptr)    :: ptr
      integer(c_int) :: dim1

      integer(kind=4), dimension(:), pointer :: all

      all => get_connectivity_poroMAILx(idBody)

      if( associated(all) ) then
        ptr  = c_loc(all(1))
        dim1 = size(all)
      else
        ptr  = c_null_ptr
        dim1 = 0
      end if

    end subroutine GetConnectivityPoroMAILx

    SUBROUTINE SetVlocyDrivenDof(IdBody,f_dof,f_node,f_value) bind(c, name='poroMAILx_SetVlocyDrivenDof')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,f_dof
      INTEGER(C_INT), INTENT(IN), VALUE :: f_node
      REAL(C_DOUBLE), INTENT(IN), VALUE :: f_value

       !! PURPOSE
       !!  Apply a drvdof on a given body
       
       CALL set_vlocy_drvdof_poroMAILx(IdBody,f_dof,f_node,f_value)

    END SUBROUTINE

    SUBROUTINE AddFieldLoad(IdBody,f,f_size) bind(c, name='poroMAILx_AddFieldLoad')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,f_size
      REAL(C_DOUBLE), INTENT(in), dimension(f_size) :: f
       !! PURPOSE
       !!  Add a loading with an external field on a given body
       !!  You need to set this field in your models.dat
     
       CALL Add_Field_Load_bynode_poroMAILx(IdBody,f_size,f)

    END SUBROUTINE

    SUBROUTINE WriteOutDof() bind(c, name='poroMAILx_WriteOutDof')
       IMPLICIT NONE
       LOGICAL :: write_DOF
       INTEGER :: ifrom,ito
       !! PURPOSE
       !!  write ascii DOF.OUT file. Can be activate only each N step

       write_DOF = get_write_DOF_poroMAILx()

       IF (write_DOF) THEN
         ifrom = 1  
         ito   = get_nb_poroMAILx()
         CALL write_xxx_dof_poroMAILx(1,ifrom,ito)
       ENDIF

    END SUBROUTINE

    SUBROUTINE PostModels() bind(c, name='poroMAILx_PostModels')
      IMPLICIT NONE

       CALL post_models_poroMAILx
       CALL update_existing_entities_poroMAILx
       
    END SUBROUTINE

    subroutine CleanMemory() bind(c, name='poroMAILx_CleanMemory')
      implicit none

      call clean_memory_poroMAILx

    end subroutine

    subroutine CheckProperties() bind(c, name='poroMAILx_CheckProperties')
      implicit none

      if( get_nb_poroMAILx() < 1 ) return

      call check_properties_poroMAILx()
       
    end subroutine CheckProperties

    subroutine GetNbGpByElem(elems, esize, ssize, nbgps_m, nsize_m, nbgps_t, nsize_t) bind(c, name='poroMAILx_GetNbGpByElem')
      implicit none

      type(c_ptr) :: elems, nbgps_m, nbgps_t
      integer(c_int) :: esize, ssize, nsize_m, nsize_t
      !
      character(len=5), dimension(:), pointer :: names
      integer(c_int)  , dimension(:), pointer :: n_GPs_m, n_GPs_t

      ssize = 5

      call get_nb_gp_by_elem_poroMAILx(names, n_GPs_m, n_GPs_t, esize)

      nsize_m = esize
      nsize_t = esize
      elems   = c_loc( names(1)   )
      nbgps_m = c_loc( n_GPs_m(1) )
      nbgps_t = c_loc( n_GPs_t(1) )

    end subroutine

END MODULE wrap_poroMAILx
