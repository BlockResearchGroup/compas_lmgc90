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
MODULE a_BDARY_POLYD                                       

  !!  anonymous loading of the POLYD (polyhedra for mailx) parameters from data files
  !!****

  USE utilities 

  IMPLICIT NONE

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_POLYD(idata)

    IMPLICIT NONE

    INTEGER,DIMENSION(:),POINTER  ::  idata

    INTEGER           :: nb_vertex,nb_faces,if,idata_sz
    CHARACTER(len=35) :: IAM='mod_a_BDARY_POLYD::read_BDARY_POLYD'


    READ(G_clin(40:47),*) nb_vertex
    READ(G_clin(60:67),*) nb_faces

    ! nb vertex, nb faces, connectivity
    idata_sz = 1 + 1 + (3*nb_faces)

    ALLOCATE(idata(idata_sz))

    idata(1) = nb_vertex
    idata(2) = nb_faces

    DO if=1,nb_faces   
      IF ( .NOT. read_G_clin()) then
        write(6,'(A,1x,I0)') 'face:',if
        CALL FATERR(IAM,'error while reading faces')
      END IF

      READ(G_clin(35:47),*) idata(2+3*if-2)
      READ(G_clin(56:68),*) idata(2+3*if-1)
      READ(G_clin(77:89),*) idata(2+3*if)
    END DO

  END SUBROUTINE read_BDARY_POLYD
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_POLYD(nfich,itacty,tacID,color,idata)

    IMPLICIT NONE

    INTEGER               ::  nfich,itacty,ial
    CHARACTER(len=5)      ::  tacID,color
    INTEGER,DIMENSION(0:) ::  idata
    INTEGER               ::  nb_vertex,nb_faces,if
   
    nb_vertex = idata(1)
    nb_faces  = idata(2)

    WRITE(nfich,104) tacID,itacty,'color',color,'nb_vertex=',nb_vertex,'nb_faces=',nb_faces
    DO if=1,nb_faces
       WRITE(nfich,132) 'ver1=',idata(2+3*if-2),'ver2=',idata(2+3*if-1),'ver3=',idata(2+3*if)
    END DO
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7,4X,A9,I7)
132 FORMAT(27X,3(2X,A5,I7,7X))

  END SUBROUTINE write_BDARY_POLYD
!!!------------------------------------------------------------------------
  
END MODULE a_BDARY_POLYD
