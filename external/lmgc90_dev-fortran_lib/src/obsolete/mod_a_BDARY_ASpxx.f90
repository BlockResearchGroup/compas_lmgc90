!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE a_BDARY_ASpxx                                       

  !!****h* LMGC90.CORE/a_BDARY_ASpxx
  !! NAME
  !!  module a_BDARY_ASpxx
  !! PURPOSE
  !!  anonymous loading of the ASpxx parameters from data files
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****

  USE utilities 

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_ASpxx(idata)


!fd le tableau idata contient:
!fd idata(0): la taille de la zone de stockage des facettes (il faudra se palucher une boucle pour reconstruire une liste utile)
!fd idata(1): taille premiere facette
!fd idata(1+1:1+idata(1)): connectivite de la facette
!fd ...

    IMPLICIT NONE


    INTEGER                       ::  nbnoe,idata_sz,i,fulldata_sz
    INTEGER,DIMENSION(:),POINTER  ::  idata

    INTEGER           :: nb_ASxxx,ias
                             !12345678901234567890123456789012345
    CHARACTER(len=35) :: IAM='mod_a_BDARY_ASpxx::read_BDARY_ASpxx'
    CHARACTER(len=5)  :: id

    TYPE T_tmp_ASpxx
       TYPE(T_tmp_ASpxx), POINTER   :: p    ! pointeur sur le precedent
       INTEGER,dimension(:),pointer :: num
       TYPE(T_tmp_ASpxx), POINTER   :: n    ! pointeur sur le suivant
    END TYPE T_tmp_ASpxx

    TYPE(T_tmp_ASpxx),POINTER :: Root
    TYPE(T_tmp_ASpxx),POINTER :: Current
    TYPE(T_tmp_ASpxx),POINTER :: Previous

    NULLIFY(Root);NULLIFY(Current);NULLIFY(Previous)

    nb_ASxxx = 1
    idata_sz = 0
    fulldata_sz = 0

    ALLOCATE(Root)
    NULLIFY(Root%p)
    NULLIFY(Root%n)

    Current => Root
    Previous => Root

    read(G_clin(2:6),'(A5)') id
    if     ( id == 'ASpx3') then
      nbnoe=3
      idata_sz=4  ! taille + liste noeuds
    else if( id == 'ASpx4') then
      nbnoe=4
      idata_sz=5  ! taille + liste noeuds
    else
      call LOGMES('Error '//IAM//': Type de facette inconnue')
    endif

    allocate(Current%num(nbnoe))

    READ(G_clin(35:39),'(I5)')   Current%num(1)
    READ(G_clin(47:51),'(I5)')   Current%num(2)
    READ(G_clin(59:63),'(I5)')   Current%num(3)
    if (nbnoe == 4) READ(G_clin(71:75),'(I5)')   Current%num(4)

    fulldata_sz = fulldata_sz + idata_sz
     
    DO    
       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'expected values in data file')
       END IF
       
       IF (G_clin(1:4) == '+ASp' ) THEN

          nb_ASxxx = nb_ASxxx + 1
          ALLOCATE(Current)
          Previous%n => Current
          Current%p => Previous
          NULLIFY(Current%n)

          read(G_clin(2:6),'(A5)') id
          if     ( id == 'ASpx3') then
            nbnoe=3
            idata_sz= 4 ! taille + liste noeuds
          else if( id == 'ASpx4') then
            nbnoe=4
            idata_sz= 5 ! taille + liste noeuds
          else
            call LOGMES('Error '//IAM//': Type de facette inconnue')
          endif

          allocate(Current%num(nbnoe))

          READ(G_clin(35:39),'(I5)')   Current%num(1)
          READ(G_clin(47:51),'(I5)')   Current%num(2)
          READ(G_clin(59:63),'(I5)')   Current%num(3)
          if (nbnoe == 4) READ(G_clin(71:75),'(I5)')   Current%num(4)

          fulldata_sz = fulldata_sz + idata_sz

!fd manque un test pour verifier que les facettes sont adjacentes !!
 
          Previous => Current

       ELSE
          
          BACKSPACE(G_nfich)
          
          ALLOCATE(idata(0:fulldata_sz))
          idata(0)=fulldata_sz
          i=1
          DO ias=nb_ASxxx,1,-1
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

  END SUBROUTINE read_BDARY_ASpxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_ASpxx(nfich,itacty,tacID,color,idata)

    IMPLICIT NONE

    INTEGER               ::  nfich,itacty,i
    CHARACTER(len=5)      ::  tacID,color
    INTEGER,DIMENSION(0:) ::  idata
   
    i=0

    DO 
      i = i + 1
      if (idata(i) == 3) then
        if (i==1) then
          WRITE(nfich,104) ' ',tacID,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2),'nodc=',idata(i+3)
        else
          WRITE(nfich,104) '+',tacID,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2),'nodc=',idata(i+3)
        endif
      else
        if (i==1) then
          WRITE(nfich,114) ' ',tacID,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                          'nodc=',idata(i+3),'nodd=',idata(i+4)
        else
          WRITE(nfich,114) '+',tacID,itacty,'color',color,'noda=',idata(i+1),'nodb=',idata(i+2), &
                                                          'nodc=',idata(i+3),'nodd=',idata(i+4)
        endif
      endif
      i=i+idata(i)
      if (i == idata(0)) exit
      if (i >  idata(0)) then
        print*,'Error write_BDARY_ASpxx'
        stop
      endif
    END DO



104 FORMAT(A1 ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5)
114 FORMAT(A1 ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5,2X,A5,I5)


  END SUBROUTINE write_BDARY_ASpxx
!!!------------------------------------------------------------------------
  
END MODULE a_BDARY_ASpxx
