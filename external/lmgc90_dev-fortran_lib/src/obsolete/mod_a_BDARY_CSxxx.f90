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
MODULE a_BDARY_CSxxx                                       

  !!****h* LMGC90.CORE/a_BDARY_CSxxx
  !! NAME
  !!  module a_BDARY_CSxxx
  !! PURPOSE
  !!  anonymous loading of the CSxxx parameters from data files
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****
  USE utilities
  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_CSxxx(idata,rdata)

    IMPLICIT NONE

    INTEGER,DIMENSION(:),pointer      :: idata
    REAL(kind=8),DIMENSION(:),pointer :: rdata
    CHARACTER(len=5)                  :: id
                             !1234567890123456789012345678901234567890
    CHARACTER(len=35) :: IAM='mod_a_BDARY_CSxxx::read_BDARY_CSxxx'

    read(G_clin(2:6),'(A5)') id
    if     ( id == 'CSxx3') then
      allocate(idata(3),rdata(3))
      READ(G_clin(35:39),'(I5)')    idata(1)
      READ(G_clin(47:51),'(I5)')    idata(2)
      READ(G_clin(59:63),'(I5)')    idata(3)

      IF( .NOT. read_G_clin()) THEN
        CALL FATERR(IAM,'expected values in data file')
      END IF
 
      READ(G_clin(35:48),'(D14.7)') rdata(1)
      READ(G_clin(56:69),'(D14.7)') rdata(2)
      READ(G_clin(77:90),'(D14.7)') rdata(3)

    else if( id == 'CSxx4') then
      allocate(idata(4),rdata(4))
      READ(G_clin(35:39),'(I5)')    idata(1)
      READ(G_clin(47:51),'(I5)')    idata(2)
      READ(G_clin(59:63),'(I5)')    idata(3)
      READ(G_clin(71:75),'(I5)')    idata(4)


      IF( .NOT. read_G_clin()) THEN
        CALL FATERR(IAM,'expected values in data file')
      END IF
 
      READ(G_clin(35:48),'(D14.7)') rdata(1)
      READ(G_clin(56:69),'(D14.7)') rdata(2)
      READ(G_clin(77:90),'(D14.7)') rdata(3)

      IF( .NOT. read_G_clin()) THEN 
        CALL FATERR(IAM,'error reading NO5xx')
      END IF

      READ(G_clin(35:48),'(D14.7)') rdata(4)

    else
      call LOGMES('Error '//IAM//': Type de facette inconnue')
    endif   

    
  END SUBROUTINE read_BDARY_CSxxx
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_CSxxx(nfich,itacty,tacID,color,idata,rdata)
    
    IMPLICIT NONE

    INTEGER          ::  nfich,itacty
    CHARACTER(len=5) ::  tacID,color
    INTEGER,DIMENSION(:),POINTER       ::  idata
    REAL(kind=8),DIMENSION(:),POINTER  ::  rdata   

                             !1234567890123456789012345678901234567890
    CHARACTER(len=36) :: IAM='mod_a_BDARY_CSxxx::write_BDARY_CSxxx'


    if (tacID == 'CSxx3') then 
          WRITE(nfich,104) ' ',tacID,itacty,'color',color,'noda=',idata(1),'nodb=',idata(2),'nodc=',idata(3)
          WRITE(nfich,132)         'w1  =',rdata(1),'w2  =',rdata(2),'w3  =',rdata(3)
    else if(tacID == 'CSxx4') then
          WRITE(nfich,114) ' ',tacID,itacty,'color',color,'noda=',idata(1),'nodb=',idata(2),'nodc=',idata(3),'nodd=',idata(4)
          WRITE(nfich,132)         'w1  =',rdata(1),'w2  =',rdata(2),'w3  =',rdata(3)
          WRITE(nfich,128)         'w4  =',rdata(4)
    else
      call LOGMES('Error '//IAM//': Type de facette inconnue')
    endif   

105 FORMAT(29X,A5,D14.7,2x,A5,D14.7)


104 FORMAT(A1 ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5)
114 FORMAT(A1 ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5,2X,A5,I5)
128  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,1(2X,A5,D14.7))
132  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
  END SUBROUTINE write_BDARY_CSxxx
!!!------------------------------------------------------------------------

 END MODULE a_BDARY_CSxxx

