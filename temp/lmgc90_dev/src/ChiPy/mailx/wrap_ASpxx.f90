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
MODULE wrap_ASpxx

  use iso_c_binding

  use ASpxx, only: load_tactors_ASpxx, &
                   set_precon_node_ASpxx, &
                   get_connec_ASpxx  , &
                   get_all_data_ASpxx, &
                   clean_memory_ASpxx

  use a_BDARY_xSpxx, only:  set_explode_patch_ASpxx

CONTAINS
!!!----------------------------------------------------

    SUBROUTINE LoadTactors() bind(c, name='ASpxx_LoadTactors')
      IMPLICIT NONE
    
       CALL load_tactors_ASpxx

    END SUBROUTINE 
!!!----------------------------------------------------
    SUBROUTINE PushPreconNodes() bind(c, name='ASpxx_PushPreconNodes')
      IMPLICIT NONE
       !! PURPOSE
       !!  Initializes existing_entities variable for ASpxx contactors

       CALL set_precon_node_Aspxx

    END SUBROUTINE 

    subroutine getAllConnec(iptr,idim1) bind(c,name='ASpxx_GetAllConnec')
      implicit none
      type(c_ptr)    :: iptr
      integer(c_int) :: idim1
      integer     , dimension(:), pointer :: idata

      idata => get_connec_ASpxx()

      if( associated(idata) ) then
        iptr  = c_loc(idata(1))
        idim1 = size(idata,1)
      else
        iptr  = c_null_ptr
        idim1 = 0
      end if

    end subroutine getAllConnec

    subroutine getAllData(iptr,idim1,idim2, rptr, rdim1, rdim2) bind(c,name='ASpxx_GetAllData')
      implicit none
      type(c_ptr)    :: iptr, rptr
      integer(c_int) :: idim1, idim2, rdim1, rdim2
      real(kind=8), dimension(:,:), pointer :: rdata
      integer     , dimension(:,:), pointer :: idata

      call get_all_data_ASpxx(idata, rdata)

      if( associated(idata) ) then
        iptr  = c_loc(idata(1,1))
        idim1 = size(idata,1)
        idim2 = size(idata,2)
      else
        iptr  = c_null_ptr
        idim1 = 0
        idim2 = 0
      end if

      if( associated(rdata) ) then
        rptr  = c_loc(rdata(1,1))
        rdim1 = size(rdata,1)
        rdim2 = size(rdata,2)
      else
        rptr  = c_null_ptr
        rdim1 = 0
        rdim2 = 0
      end if
      
    end subroutine getAllData
  
    subroutine CleanMemory() bind(c, name='ASpxx_CleanMemory')
      implicit none

      call clean_memory_ASpxx

    end subroutine

    subroutine ExplodePatch() bind(c, name='ASpxx_ExplodePatch')
      implicit none

      call set_Explode_Patch_ASpxx

    end subroutine

END MODULE wrap_ASpxx
