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
MODULE wrap_MAILx                                       

  USE ISO_C_BINDING

  USE MAILx,ONLY:&
       read_in_bodies_MAILx, &
       write_out_bodies_MAILx, &
       write_xxx_gpv_MAILx, &
       add_dof2bodies_MAILx, &
       get_write_GPV_actif_MAILx, &
       get_coor_nodty_MAILx, &
       get_nb_node_MAILx, &
       get_nb_MAILx, &
       get_nb_cell_MAILx, &
       set_cooref_nodes_MAILx, &
       init_nodal_fields_MAILx, &
       init_nodal_field_MAILx, &  
       get_nodal_field_rank_MAILx, &
       set_nodal_field_MAILx, &
       clean_memory_MAILx

  use overall, only : faterr

CONTAINS

    subroutine ReadBodies(version) bind(c, name='MAILx_ReadBodies')
      implicit none
      character(c_char), dimension(*) :: version
      !
      integer(kind=4) :: v_maj, v_min, i, j

      if( version(1) == c_null_char ) then
        v_maj = 2
        v_min = 1
      else
        if( version(1) /= 'v' ) then
          call faterr('MAILx::ReadBodies',"Input string must be of form 'vX.Y' (with X and Y major and minor version number)")
        end if
        i = 2
        do while( version(i) /= '.' )
          i = i+1
        end do
        read(version(2:i-1),*) v_maj
        j = i + 1
        do while( version(j) /= c_null_char )
          j = j+1
        end do
        read(version(i+1:j-1),*) v_min
      end if

      call read_in_bodies_MAILx(v_maj,v_min)

    end subroutine

    subroutine WriteBodies(version) bind(c, name='MAILx_WriteBodies')
      implicit none
      character(c_char), dimension(*) :: version
      !
      integer(kind=4) :: v_maj, v_min, i, j

      if( version(1) == c_null_char ) then
        v_maj = 2
        v_min = 1
      else
        if( version(1) /= 'v' ) then
          call faterr('MAILx::ReadBodies',"Input string must be of form 'vX.Y' (with X and Y major and minor version number)")
        end if
        i = 2
        do while( version(i) /= '.' )
          i = i+1
        end do
        read(version(2:i-1),*) v_maj
        j = i + 1
        do while( version(j) /= c_null_char )
          j = j+1
        end do
        read(version(i+1:j-1),*) v_min
      end if
      call write_out_bodies_MAILx(v_maj,v_min)

    end subroutine

    SUBROUTINE WriteLastGPV() bind(c, name='MAILx_WriteLastGPV')
      IMPLICIT NONE

       if( get_nb_MAILX() < 1 ) return

       CALL write_xxx_gpv_MAILx(2)

    END SUBROUTINE

    SUBROUTINE WriteOutGPV() bind(c, name='MAILx_WriteOutGPV')
      IMPLICIT NONE
      LOGICAL :: write_GPV_actif

       if( get_nb_MAILX() < 1 ) return

       write_GPV_actif = get_write_GPV_actif_MAILx()
       IF (write_GPV_actif) CALL write_xxx_gpv_MAILx(1)

    END SUBROUTINE

    SUBROUTINE DisplayOutGPV() bind(c, name='MAILx_DisplayOutGPV')
      IMPLICIT NONE

       CALL write_xxx_gpv_MAILx(6)

    END SUBROUTINE

    SUBROUTINE AddDof2InBodies() bind(c, name='MAILx_AddDof2InBodies')
      IMPLICIT NONE

       CALL add_dof2bodies_MAILx

    END SUBROUTINE

    FUNCTION GetCoordNodty(ivalue1,ivalue2,ivalue3) bind(c, name='MAILx_GetCoordNodty')
      use overall, only: nbDIME
      IMPLICIT NONE

      integer(c_int), intent(in), value          :: ivalue1,ivalue2,ivalue3
      real(c_double)                             :: GetCoordNodty
      !
      real(c_double), dimension(:), pointer :: vect 

      allocate(vect(nbDIME))
      vect = get_coor_nodty_MAILx(ivalue1,ivalue2)

      GetCoordNodty = vect(ivalue3)

      deallocate(vect)
    END function

    SUBROUTINE GetCoordsNodty(ivalue1,ivalue2,rvect,ivalue3) bind(c, name='MAILx_GetCoordsNodty')
      use overall, only: nbDIME
      IMPLICIT NONE

      integer(c_int), intent(in), value          :: ivalue1,ivalue2,ivalue3
      real(c_double), intent(out)                :: rvect(3)
      !
      real(c_double), dimension(:), pointer :: vect 

      rvect = 0.
      allocate(vect(nbDIME))
      vect = get_coor_nodty_MAILx(ivalue1,ivalue2)
      rvect(1:ivalue3) = vect(1:ivalue3)

    END SUBROUTINE


    !fd: fonction qui renvoie le nombre de noeuds d'un mecaMAILx
    function GetNbNodes(ibdyty) bind(c, name='MAILx_GetNbNodes')

       implicit none
       integer(c_int),value :: ibdyty
       ! valeur de retour
       integer(c_int) :: GetNbNodes ! nombre de noeuds d un mecaMAILx 

       GetNbNodes = get_nb_node_MAILx(ibdyty)

    end function GetNbNodes
    
    ! Ajout par DA

    function GetNbMAILx() bind(c, name='MAILx_GetNbMAILx')
      IMPLICIT NONE
      ! valeur de retour
      integer(c_int) :: GetNbMAILx ! nombre de corps maille
      
      GetNbMAILx = get_nb_MAILx()
    END FUNCTION

    function GetNbCell(IdBody) bind(c, name='MAILx_GetNbCell')
    
      IMPLICIT NONE
      ! valeur de retour
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody
      integer(c_int) :: GetNbCell ! nombre d'element pour le corps maille

      GetNbCell = get_nb_cell_MAILx(IdBody)

    END FUNCTION GetNbCell
    
    SUBROUTINE SetCoorRef(IdBody,f,f_size) bind(c, name='MAILx_SetCoorRef')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: IdBody,f_size
      REAL(C_DOUBLE), INTENT(in), dimension(f_size) :: f
        !CALL compute_Biot_therMAILx(IdBody, NbNo, f, f_size)
        
        CALL set_cooref_nodes_MAILx(IdBody, f)
        
    END SUBROUTINE

    subroutine InitNodalFields(ibdyty,nb_nodal_fields) bind(c, name='MAILx_InitNodalFields')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE :: ibdyty,nb_nodal_fields

      call init_nodal_fields_MAILx(ibdyty,nb_nodal_fields)
   

    end subroutine

    subroutine InitNodalField(ibdyty,c_name,rank,sz) bind(c, name='MAILx_InitNodalField')
      IMPLICIT NONE
      INTEGER(C_INT), INTENT(IN), VALUE           :: ibdyty,rank,sz
      character(c_char), dimension(30),intent(in) :: c_name

      character(len=30) :: name
      integer :: i

      name = ''
      do i=1,30
         name = name(1:i-1) // c_name(i)
      end do

      call init_nodal_field_MAILx(ibdyty,name,rank,sz)

    end subroutine

    subroutine SetNodalField(ibdyty,rank,field,length) bind(c, name='MAILx_SetNodalField')
      implicit none
      integer(c_int),intent(in), value ::ibdyty,rank,length
      real(c_double),intent(in) :: field(length)

      call set_nodal_field_MAILx(ibdyty,rank,field) 

    end subroutine 

    subroutine CleanMemory() bind(c, name='MAILx_CleanMemory')
      implicit none

      call clean_memory_MAILx

    end subroutine

END MODULE wrap_MAILx
