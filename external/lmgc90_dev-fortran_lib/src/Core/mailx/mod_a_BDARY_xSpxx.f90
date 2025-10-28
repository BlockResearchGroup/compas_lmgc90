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

!> anonymous loading of a xSpxx surface from data files
!> a xSp surface is discretized surface by T3 or Q4 element
MODULE a_BDARY_xSpxx                                       

  USE utilities

  IMPLICIT NONE
  LOGICAL :: ASpxx_explode_patch = .false.

  CONTAINS

!!!------------------------------------------------------------------------
  !> \brief Read a patch of faces of a mesh
  subroutine read_BDARY_xSpxx(T,idata,v_maj,v_min)
    implicit none
    !> Is the patch candidate or antagonist
    character(len=1), intent(in) :: T
    !> First the size of idata array, then size of next face, then connectivity of the face, the size of next face...
    integer(kind=4), dimension(:), pointer :: idata
    !> Major version number of file format
    integer(kind=4), intent(in) :: v_maj
    !> Minor version number of file format
    integer(kind=4), intent(in) :: v_min
    !***
                             !12345678901234567890123456789
    CHARACTER(len=29) :: IAM='mod_a_BDARY_xSpxx::read_BDARY'

    !***
    INTEGER                       ::  nbnoe,idata_sz,i,fulldata_sz

    INTEGER           :: nb_xSxxx,ixs
    CHARACTER(len=5)  :: id

    TYPE T_tmp_xSpxx
       TYPE(T_tmp_xSpxx), POINTER   :: p    ! pointeur sur le precedent
       INTEGER,dimension(:),pointer :: num
       TYPE(T_tmp_xSpxx), POINTER   :: n    ! pointeur sur le suivant
    END TYPE T_tmp_xSpxx

    TYPE(T_tmp_xSpxx),POINTER :: Root
    TYPE(T_tmp_xSpxx),POINTER :: Current
    TYPE(T_tmp_xSpxx),POINTER :: Previous

    NULLIFY(Root);NULLIFY(Current);NULLIFY(Previous)

    nb_xSxxx = 1
    idata_sz = 0
    fulldata_sz = 0

    ALLOCATE(Root)
    NULLIFY(Root%p)
    NULLIFY(Root%n)

    Current => Root
    Previous => Root

    read(G_clin(2:6),'(A5)') id

    if( v_maj < 3 .and. v_min < 1 ) then
      if     ( id == T//'Spx3') then
        nbnoe    = 3
        idata_sz = 4  ! taille + liste noeuds
      else if( id == T//'Spx4') then
        nbnoe    = 4
        idata_sz = 5  ! taille + liste noeuds
      else if( id == T//'Spx6') then
        nbnoe    = 6
        idata_sz = 7  ! taille + liste noeuds
      else if( id == T//'Spx8') then
        nbnoe    = 8
        idata_sz = 9  ! taille + liste noeuds
      else
        call LOGMES('Error '//IAM//': Unknown kind of element')
      endif
    else if( v_maj < 3 .and. v_min < 2 ) then
      if     ( G_clin(114:117) == 'nodh') then
        nbnoe    = 8
        idata_sz = 9  ! taille + liste noeuds
      else if( G_clin(90:93) == 'nodf') then
        nbnoe    = 6
        idata_sz = 7  ! taille + liste noeuds
      else if( G_clin(66:69) == 'nodd') then
        nbnoe    = 4
        idata_sz = 5  ! taille + liste noeuds
      else if ( G_clin(54:57) == 'nodc') then
        nbnoe    = 3
        idata_sz = 4  ! taille + liste noeuds
      else
        call LOGMES('Error '//IAM//': Unknown kind of element')
      endif

    else
      call faterr(IAM,'Reading with futur version format')
    end if

    allocate(Current%num(nbnoe))

    READ(G_clin(35:40),'(I6)')   Current%num(1)
    READ(G_clin(47:52),'(I6)')   Current%num(2)
    READ(G_clin(59:64),'(I6)')   Current%num(3)
    if (nbnoe == 4 .or. nbnoe == 8 .or. nbnoe == 6) READ(G_clin(71:76),'(I6)')   Current%num(4)
    if (nbnoe == 8 .or. nbnoe == 6) READ(G_clin(83:88),'(I6)')   Current%num(5)
    if (nbnoe == 8 .or. nbnoe == 6) READ(G_clin(95:100),'(I6)')   Current%num(6)
    if (nbnoe == 8) READ(G_clin(107:112),'(I6)')   Current%num(7)
    if (nbnoe == 8) READ(G_clin(119:124),'(I6)')   Current%num(8)

    fulldata_sz = fulldata_sz + idata_sz
     
    DO    
       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'expected values in data file')
       END IF
       
       IF ( (G_clin(1:4) == '+CSp') .or. &
            (G_clin(1:4) == '+ASp' .and. .NOT. is_explode_patch_ASpxx()) ) THEN

          nb_xSxxx = nb_xSxxx + 1
          ALLOCATE(Current)
          Previous%n => Current
          Current%p => Previous
          NULLIFY(Current%n)

          read(G_clin(2:6),'(A5)') id

          if( v_maj < 3 .and. v_min < 1 ) then
            if     ( id == T//'Spx3') then
              nbnoe    = 3
              idata_sz = 4  ! taille + liste noeuds
            else if( id == T//'Spx4') then
              nbnoe    = 4
              idata_sz = 5  ! taille + liste noeuds
            else if( id == T//'Spx6') then
              nbnoe    = 6
              idata_sz = 7  ! taille + liste noeuds
            else if( id == T//'Spx8') then
              nbnoe    = 8
              idata_sz = 9  ! taille + liste noeuds
            else
              call LOGMES('Error '//IAM//': Unknown kind of element')
            endif
          else if( v_maj < 3 .and. v_min < 2 ) then
            if     ( G_clin(114:117) == 'nodh') then
              nbnoe    = 8
              idata_sz = 9  ! taille + liste noeuds
            else if( G_clin(90:93) == 'nodf') then
              nbnoe    = 6
              idata_sz = 7  ! taille + liste noeuds
            else if( G_clin(66:69) == 'nodd') then
              nbnoe    = 4
              idata_sz = 5  ! taille + liste noeuds
            else if ( G_clin(54:57) == 'nodc') then
              nbnoe    = 3
              idata_sz = 4  ! taille + liste noeuds
            else
              call LOGMES('Error '//IAM//': Unknown kind of element')
            endif

          else
            call faterr(IAM,'Reading with futur version format')
          end if

          allocate(Current%num(nbnoe))

          READ(G_clin(35:40),'(I6)')   Current%num(1)
          READ(G_clin(47:52),'(I6)')   Current%num(2)
          READ(G_clin(59:64),'(I6)')   Current%num(3)
          if (nbnoe == 4 .or. nbnoe == 8 .or. nbnoe == 6) READ(G_clin(71:76),'(I6)')   Current%num(4)
          if (nbnoe == 8 .or. nbnoe == 6) READ(G_clin(83:88),'(I6)')   Current%num(5)
          if (nbnoe == 8 .or. nbnoe == 6) READ(G_clin(95:100),'(I6)')   Current%num(6)
          if (nbnoe == 8) READ(G_clin(107:112),'(I6)')   Current%num(7)
          if (nbnoe == 8) READ(G_clin(119:124),'(I6)')   Current%num(8)

          fulldata_sz = fulldata_sz + idata_sz

          !fd manque un test pour verifier que les facettes sont adjacentes !!
 
          Previous => Current

       ELSE
          
          BACKSPACE(G_nfich)
          
          ALLOCATE(idata(0:fulldata_sz))
          idata(0)=fulldata_sz
          i=1
          DO ixs=nb_xSxxx,1,-1
             Previous => Current%p

             idata(i) = size(Current%num)
             idata(i+1:i+size(Current%num))=Current%num(1:size(Current%num))
             i = i + size(Current%num) + 1

             DEALLOCATE(Current%num)
             DEALLOCATE(Current)
             Current => Previous
          END DO

          NULLIFY(Root)
          EXIT
       END IF       
    END DO

    call logmes('Fin de lecture des '//T//'Sxxx')

  end subroutine read_BDARY_xSpxx
!!!------------------------------------------------------------------------
  subroutine write_BDARY_xSpxx(nfich,itacty,tacID,color,idata,v_maj,v_min)
    implicit none
    integer(kind=4) , intent(in) :: nfich, itacty
    character(len=5), intent(in) :: tacID, color
    integer(kind=4) , dimension(0:), intent(in) ::  idata
    integer(kind=4) , intent(in) :: v_maj, v_min
    !***
                             !123456789012345678901234567890
    CHARACTER(len=30) :: IAM='mod_a_BDARY_xSpxx::write_BDARY'
    character(len=5 ) :: tac_name
    !***
    integer(kind=4)   :: i
    character(len=46) :: l1, l2, l3, l4
   
    !1234567890123456789012345678901234567890123456
    l1 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I6,2(1X,A5,I6))"
    l2 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I6,3(1X,A5,I6))"
    l3 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I6,5(1X,A5,I6))"
    l4 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I6,7(1X,A5,I6))"
    !rm : I so hate this...
    do i = 1, idata(1)
      if( idata(i+1) > 99999 ) then
        l1 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2(2X,A5,I5))"
        l2 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,3(2X,A5,I5))"
        l3 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,5(2X,A5,I5))"
        l4 = "(A1,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,7(2X,A5,I5))"
        exit
      end if
    end do

    i=0

    tac_name = tacID
    if( v_maj < 3 .and. v_min < 1 ) then
      write(tac_name(5:5),'(I1)') idata(1)
    end if

    DO 
      i = i + 1
      select case(idata(i)) 
      case(3)
        if (i==1) then
          WRITE(nfich,l1) ' ',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2),'nodc=',idata(i+3)
        else
          WRITE(nfich,l1) '+',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2),'nodc=',idata(i+3)
        endif
      case(4)
        if (i==1) then
          WRITE(nfich,l2) ' ',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                            'nodc=',idata(i+3),'nodd=',idata(i+4)
        else
          WRITE(nfich,l2) '+',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                            'nodc=',idata(i+3),'nodd=',idata(i+4)
        endif
      case(6)
        if (i==1) then
          WRITE(nfich,l3) ' ',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                            'nodc=',idata(i+3),'nodd=',idata(i+4), &
                                                            'node=',idata(i+5),'nodf=',idata(i+6)
        else
          WRITE(nfich,l3) '+',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                            'nodc=',idata(i+3),'nodd=',idata(i+4), &
                                                            'node=',idata(i+5),'nodf=',idata(i+6)
        endif
      case(8)
        if (i==1) then
          WRITE(nfich,l4) ' ',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                            'nodc=',idata(i+3),'nodd=',idata(i+4), &
                                                            'node=',idata(i+5),'nodf=',idata(i+6), &
                                                            'nodg=',idata(i+7),'nodh=',idata(i+8)

        else
          WRITE(nfich,l4) '+',tac_name,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                            'nodc=',idata(i+3),'nodd=',idata(i+4), & 
                                                            'node=',idata(i+5),'nodf=',idata(i+6), & 
                                                            'nodg=',idata(i+7),'nodh=',idata(i+8)
        endif
      end select
      i=i+idata(i)
      if (i == idata(0)) exit
      if (i >  idata(0)) call faterr(IAM,' Error while writting')
    END DO

  END SUBROUTINE write_BDARY_xSpxx
!!!------------------------------------------------------------------------


  SUBROUTINE set_explode_patch_ASpxx
    IMPLICIT NONE
    ASpxx_explode_patch = .true.

  END SUBROUTINE set_explode_patch_ASpxx
  
  LOGICAL FUNCTION is_explode_patch_ASpxx()
    IMPLICIT NONE
    is_explode_patch_ASpxx = ASpxx_explode_patch
    
  END FUNCTION is_explode_patch_ASpxx
  
END MODULE a_BDARY_xSpxx
