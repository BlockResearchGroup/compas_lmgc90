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
MODULE a_BDARY_POLYG    

  !!****h* LMGC90.CORE/a_BDARY_POLYG
  !! NAME
  !!  module a_BDARY_POLYG
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!****

  use utilities, only : G_clin, read_G_clin, &
                        faterr

  use overall, only : PI_g

  implicit none

  private

  public read_BDARY_POLYG , &
         write_BDARY_POLYG, &
         comp_BDARY_POLYG

contains

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_POLYG(ibdyty,vertex,nb_vertex,area,avrd,gyrd,cooref)

    IMPLICIT NONE
    
    REAL(kind=8),DIMENSION(:),POINTER   :: vertex
    INTEGER     ,DIMENSION(:),POINTER   :: nb_vertex
    INTEGER                             :: i,ibdyty
    
    REAL(kind=8)                            :: area    
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: sommet
    
    REAL(kind=8)                            :: avrd,gyrd
    REAL(kind=8),DIMENSION(2)               :: cooref
    CHARACTER(len=80)  :: cout
    CHARACTER(len=16)  :: IAM='read_BDARY_POLYG'

    ALLOCATE(nb_vertex(1))
    READ(G_clin(40:46),'(I7)') nb_vertex(1)
    ALLOCATE(vertex(2*nb_vertex(1)))
    DO i=1,nb_vertex(1)
       IF( .NOT. read_G_clin()) THEN
          CALL FATERR(IAM,'error reading')  
       ENDIF
       READ(G_clin(35:48),'(D14.7)') vertex((i-1)*2+1)
       READ(G_clin(56:69),'(D14.7)') vertex((i-1)*2+2)
    ENDDO
    
    allocate(sommet(2,nb_vertex(1)))
    sommet = reshape(source=vertex, shape=(/2,nb_vertex(1)/) )

    call compute_inertia_(ibdyty, nb_vertex(1), sommet, area, avrd, gyrd, cooref)

    vertex = reshape(source=sommet, shape=(/2*nb_vertex(1)/) )
    deallocate(sommet)

  END SUBROUTINE read_BDARY_POLYG
!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_POLYG(nfich,itacty,tacID,color,vertex,nb_vertex,shift)
    
    IMPLICIT NONE
    INTEGER,INTENT(in)   ::  nfich
    INTEGER              ::  itacty
    CHARACTER(len=5)     ::  tacID,color
    INTEGER                             :: nb_vertex,i
    REAL(kind=8),DIMENSION(2*nb_vertex) :: vertex
    REAL(kind=8),DIMENSION(2)           :: shift
    
    
    WRITE(nfich,104) tacID,itacty,'color',color,'nb_vertex=',nb_vertex
    
    DO i=1,nb_vertex
       WRITE(nfich,131) 'coo1=',shift(1)+vertex((i-1)*2+1),'coo2=',shift(2)+vertex((i-1)*2+2)
    ENDDO
    
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7)
131 FORMAT(27X,2(2X,A5,D14.7))
    

  END SUBROUTINE write_BDARY_POLYG
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  subroutine comp_BDARY_POLYG(ibdyty,r8_vector,vertex,nb_vertex,area,avrd,gyrd,cooref)
    implicit none
    !
    integer(kind=4), intent(in)  :: ibdyty
    real(kind=8)   , dimension(:), intent(in)  :: r8_vector
    real(kind=8)   , dimension(:), pointer     :: vertex
    integer(kind=4), dimension(:), pointer     :: nb_vertex
    real(kind=8),    dimension(2), intent(out) :: cooref
    real(kind=8), intent(out) :: area, avrd, gyrd
    !
    real(kind=8), dimension(:,:), allocatable :: sommet

    allocate(nb_vertex(1))
    nb_vertex(1) = size(r8_vector)/2

    allocate(vertex(2*nb_vertex(1)))
    vertex = r8_vector
    
    allocate(sommet(2,nb_vertex(1)))
    sommet = reshape( source=vertex, shape=(/2,nb_vertex(1)/) )

    call compute_inertia_(ibdyty, nb_vertex(1), sommet, area, avrd, gyrd, cooref)

    vertex = reshape( source=sommet, shape=(/2*nb_vertex(1)/) )
    deallocate(sommet)

  end subroutine comp_BDARY_POLYG
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  subroutine compute_inertia_(ibdyty, nb_vertex, sommet, area, avrd, gyrd, cooref)
    implicit none
    integer(kind=4)             , intent(in)    :: ibdyty, nb_vertex
    real(kind=8)                , intent(out)   :: area, avrd, gyrd
    real(kind=8), dimension(2)  , intent(out)   :: cooref
    real(kind=8), dimension(:,:), intent(inout) :: sommet
    !
    integer(kind=4) :: i, ip
    real(kind=8)    :: inertotal!, Ix, Iy
    real(kind=8), dimension(2) :: OG
    real(kind=8), dimension(:), allocatable :: aire
    !
    character(len=80) :: cout
    character(len=29), parameter :: IAM='BDARY_POLYG::compute_inertia_'

    ! formule de calcul de l'aire, du centre de gravite et de l'inertie donnees par
    ! http://paulbourke.net/geometry/polygonmesh/

    ! calcul de l'aire du POLYG
    allocate(aire(nb_vertex))
    aire = 0.d0
    do i = 1,nb_vertex
       ip = modulo(i,nb_vertex) + 1
       aire(i) = sommet(1,i)*sommet(2,ip) - sommet(1,ip)*sommet(2,i)      
    end do

    area = 0.5d0 * sum(aire)

    if( area <= 0.d0 ) then
       write(cout,'(A,1x,I0)') 'bad orientation for the polygon attached to RBDY2 ',ibdyty
       call faterr(IAM,cout)
    end if

    avrd = dsqrt(area/PI_g)

    ! calcul du centre de gravite
    OG = 0.d0
    do i = 1, nb_vertex
       ip = modulo(i,nb_vertex) + 1
       OG(:) = OG(:) + ( sommet(:,i)+sommet(:,ip) ) * aire(i)
    end do

    cooref(:) = OG(:) / (6.d0*area)

    ! on exprime les coordonnees des sommets dans le referentiel barycentrique
    do i = 1, nb_vertex
       sommet(1:2,i) = sommet(1:2,i) - cooref(1:2)
    end do

    ! calcul de l'inertie du polygone par rapport au centre de gravite
    inertotal = 0.d0
    !Ix = 0.d0
    !Iy = 0.d0
    do i = 1, nb_vertex
       ip = modulo(i,nb_vertex) + 1
       !Ix = Ix + ( sommet(2,i)**2 + sommet(2,i)*sommet(2,ip) + sommet(2,ip)**2 ) * aire(i)
       !Iy = Iy + ( sommet(1,i)**2 + sommet(1,i)*sommet(1,ip) + sommet(1,ip)**2 ) * aire(i)
       inertotal = inertotal + (   sommet(2,i )*sommet(2,i ) + sommet(1,i )*sommet(1,i ) &
                                 + sommet(2,i )*sommet(2,ip) + sommet(1,i )*sommet(1,ip) &
                                 + sommet(2,ip)*sommet(2,ip) + sommet(1,ip)*sommet(1,ip) &
                               ) * aire(i)
    end do

    !inertotal = inertotal/12.D0
    !gyrd = dsqrt(inertotal/area)
    gyrd = dsqrt( inertotal/(12.d0*area) )

    deallocate(aire)

  end subroutine compute_inertia_
    
END MODULE a_BDARY_POLYG
