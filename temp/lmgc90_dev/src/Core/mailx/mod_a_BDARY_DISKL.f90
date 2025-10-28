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
MODULE a_BDARY_DISKL                                       
   
  !!****h* LMGC90.CORE/a_BDARY_DISKL
  !! NAME
  !!  module a_BDARY_DISKL
  !! PURPOSE
  !!  anonymous loading of the DISKL parameters from data files
  !!****

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_DISKL(idata,rdata,clin)

    IMPLICIT NONE

    CHARACTER(len=121)        ::  clin
    INTEGER,DIMENSION(2)      ::  idata
    REAL(kind=8),DIMENSION(3) ::  rdata

    READ(clin(35:114),103) idata(1),idata(2),rdata(1),rdata(2),rdata(3)
    
103 FORMAT(I5,7X,I5,7X,D14.7,7X,D14.7,7X,D14.7)
 
  END SUBROUTINE read_BDARY_DISKL
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_DISKL(nfich,itacty,tacID,color,numnoda,numnodb,apab,rayon,brpm)

    IMPLICIT NONE

    INTEGER          ::  itacty,nfich,numnoda,numnodb
    CHARACTER(len=5) ::  tacID,color
    REAL(kind=8)     ::  apab,rayon,brpm
    
    WRITE(nfich,104) tacID,itacty,'color',color,'noda=',numnoda,'nodb=',numnodb,'apab=',apab,'bdyr=',rayon,'brpm=',brpm

104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,D14.7,2X,A5,D14.7,2X,A5,D14.7)

  END SUBROUTINE write_BDARY_DISKL
!!!------------------------------------------------------------------------

END MODULE a_BDARY_DISKL

