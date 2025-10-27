# 1 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Core/rigid_3D/mod_a_BDARY_POLYR.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Core/rigid_3D/mod_a_BDARY_POLYR.f90"
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
MODULE a_BDARY_POLYR

  !!****h* LMGC90.CORE/a_BDARY_POLYR
  !! NAME
  !!  module a_BDARY_DNLYC
  !! USES
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/ALGEBRA
  !!****

  USE utilities
  USE Algebra
  use DiscreteGeometry

  IMPLICIT NONE

CONTAINS

  subroutine read_patch_(iam, nb_read, nb_vertex, nb_faces, coor, connec)
    implicit none
    character(len=31), intent(in)    ::IAM
    integer          , intent(inout) :: nb_read
    integer          , intent(out)   :: nb_vertex
    integer          , intent(out)   :: nb_faces
    real(kind=8), dimension(:,:), allocatable :: coor
    integer     , dimension(:,:), allocatable :: connec
    !
    integer :: k
    character(len=80) :: mes

    nb_read = nb_read + 1

    read(G_clin(40:47),*) nb_vertex
    read(G_clin(60:67),*) nb_faces

    allocate(coor(3,nb_vertex),connec(3,nb_faces))

    do k = 1, nb_vertex
       if( .not. read_G_clin()) then
          write(mes,'(A,1x,I0)') 'POLYR',nb_read
          call logmes(trim(mes))
          CALL faterr(IAM,'error while reading vertex')
       end if
       read(G_clin(35:48),'(D14.7)') coor(1,k)
       read(G_clin(56:69),'(D14.7)') coor(2,k)
       read(G_clin(77:90),'(D14.7)') coor(3,k)
    end do

    do k = 1, nb_faces
       if (.not. read_G_clin()) then
         write(mes,'(A,1x,I0)') 'POLYR',nb_read
         call logmes(trim(mes))
         CALL faterr(IAM,'error while reading face')
       end if

       read(G_clin(35:47),*) connec(1,k)
       read(G_clin(56:68),*) connec(2,k)
       read(G_clin(77:89),*) connec(3,k)
    end do

  end subroutine read_patch_

  subroutine read_all_patches_(IAM, nb_read, nb_vertices, nb_elements, coor, connec)
    implicit none
    character(len=31), intent(in)    ::IAM
    integer          , intent(inout) :: nb_read
    integer          , intent(out)   :: nb_vertices
    integer          , intent(out)   :: nb_elements
    real(kind=8), dimension(:,:), allocatable :: coor
    integer     , dimension(:,:), allocatable :: connec
    !
    type T_patch
      real(kind=8),dimension(:,:),allocatable :: coor
      integer,dimension(:,:),allocatable :: connec
    end type T_patch
    !
    type(T_patch), dimension(:) , allocatable :: patch
    integer :: ip, nb_patch, nbv, nbe
    character(len=80) :: mes

    read(G_clin(40:47),*) nb_patch
 
    !print*,nb_patch

    allocate(patch(nb_patch))

    nb_vertices=0
    nb_elements=0

    do ip = 1, nb_patch

      if ( .not. read_G_clin()) then
        write(mes,'(A,1x,I0)') 'POLYF', nb_read
        call logmes(trim(mes))
        write(mes,'(A,1x,I0)') 'PATCH',ip
        call logmes(trim(mes))
        CALL faterr(IAM,'error while reading descriptors')
      end if

      call read_patch_(IAM, nb_read, nbv, nbe, patch(ip)%coor, patch(ip)%connec)

      nb_vertices = nb_vertices + nbv
      nb_elements = nb_elements + nbe

    end do

    allocate(coor(3,nb_vertices),connec(3,nb_elements))

    nb_vertices=0; nb_elements=0
    do ip=1,nb_patch
      nbv = size(patch(ip)%coor,dim=2)
      nbe = size(patch(ip)%connec,dim=2)

      coor(:,nb_vertices+1:nb_vertices+nbv) = patch(ip)%coor(:,1:nbv)
      connec(:,nb_elements+1:nb_elements+nbe) = nb_vertices + patch(ip)%connec(:,1:nbe)

      nb_vertices = nb_vertices + nbv
      nb_elements = nb_elements + nbe

      deallocate(patch(ip)%coor,patch(ip)%connec)

    enddo
    deallocate(patch)

  end subroutine read_all_patches_

  subroutine set_BDARY_POLYR(rdata,idata,vol,I1,I2,I3,localframe,shift)
    implicit none
    real(kind=8), dimension(:), pointer :: rdata ! modifies in place if needed
    integer     , dimension(:), pointer :: idata ! modifies in place if needed
    real(kind=8)                 :: vol, I1, I2, I3
    real(kind=8), dimension(3,3) :: localframe
    real(kind=8), dimension(3)   :: shift
    !
    real(kind=8), dimension(:,:), allocatable :: coor
    integer     , dimension(:,:), allocatable :: connec
    integer :: nb_vertex, nb_faces, err_
                               !123456789012345678
    character(len=18) :: IAM = 'a_BDARY_POLYR::set'

    nb_vertex = idata(1)
    nb_faces  = idata(1)

    ! alas... need to reallocate when changing rank
    ! or use iso_c_binding to use c_loc as a workaround

    allocate( connec(3,nb_faces ) )
    allocate( coor  (3,nb_vertex) )
    connec = reshape( idata(3:), shape=(/3,nb_faces /) )
    coor   = reshape( rdata    , shape=(/3,nb_vertex/) )

    call compute_mechanical_properties_surface_T3(nb_vertex, nb_faces, connec, coor, &
                                                  vol, shift, I1, I2, I3, localframe,err_)

    rdata(:) = reshape( coor, shape=(/3*nb_vertex/) )
    deallocate(coor)
    deallocate(connec)

    if (err_ > 0) then
       call faterr(IAM,'Something wrong when computing surfacic mechanical properties')
    endif

    if (vol < 0.d0) then
      call faterr(IAM,'negative volume')
    endif

    if (I1 < 0.d0 .or. I2 < 0.d0 .or. I3 < 0.d0) then
     call faterr(IAM,'negative inertia')
    endif

  end subroutine

!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_POLYR(data,idata,vol,I1,I2,I3,localframe,shift)

    IMPLICIT NONE

    ! data contains vertex coordinates
    ! idata contains the number of vertex, the number of face, the face connectivity

    INTEGER                           :: k,nb_vertex,nb_faces
    REAL(kind=8)                      :: vol,I1,I2,I3
    REAL(kind=8),DIMENSION(3)         :: shift
    REAL(kind=8),DIMENSION(3,3)       :: localframe
    REAL(kind=8),DIMENSION(:),POINTER :: data
    INTEGER,DIMENSION(:),POINTER      :: idata

    !***                    
    real(kind=8),dimension(:,:),allocatable :: coor
    integer,dimension(:,:),allocatable :: connec

    ! c'est une variable statique qui compte ne nombre de POLYR lu
    integer :: nb_read=0

                            !1234567890123456789012345678901
    character(len=31) ::IAM='a_BDARY_POLYR::read_BDARY_POLYR'
    character(len=80) :: mes

    integer :: err_
    
    call read_patch_(IAM, nb_read, nb_vertex, nb_faces, coor, connec)

    !print*,'-',nb_read

    !fd rend tout exprime dans le repere principal d'inertie
    call compute_mechanical_properties_surface_T3(nb_vertex, nb_faces, connec, coor, vol, shift, I1, I2, I3, localframe,err_)
    if (err_ > 0) then
       write(mes,'(A,1x,I0)') 'POLYR',nb_read
       call logmes(trim(mes))
       call faterr(IAM,'Something wrong when computing surfacic mechanical properties')
    endif

    !print*,'--'

    if (vol < 0.d0) then
       write(mes,'(A,1x,I0)') 'POLYR',nb_read
       call logmes(trim(mes))
       call FATERR(IAM,'negative volume')
    endif

    if (I1 < 0.d0 .or. I2 < 0.d0 .or. I3 < 0.d0) then
      write(mes,'(A,1x,I0)') 'POLYR',nb_read
      call logmes(trim(mes))
      call FATERR(IAM,'negative inertia')
    endif

!    write(6,'(3(1x,D12.5))'),localframe,I1,I2,I3

    ALLOCATE(data(3*nb_vertex))
    ALLOCATE(idata(1 + 1 + (3*nb_faces)))

    idata(1) = nb_vertex
    idata(2) = nb_faces

    idata(2 + 1 : 2 + 3*idata(2)) = pack(connec,mask=.TRUE.)
    data = pack(coor,mask=.TRUE.)

    deallocate(coor,connec)
 
  END SUBROUTINE read_BDARY_POLYR
!!!------------------------------------------------------------------------
  SUBROUTINE read_BDARY_POLYF(data,idata,vol,I1,I2,I3,localframe,shift)

    IMPLICIT NONE

    ! data contains vertex coordinates
    ! idata contains the number of patch and for each patch the number of vertices and elements, and the elements connectivity

    INTEGER                           :: k,nb_patch,nb_vertices,nb_elements
    REAL(kind=8)                      :: vol,I1,I2,I3
    REAL(kind=8),DIMENSION(3)         :: shift
    REAL(kind=8),DIMENSION(3,3)       :: localframe
    REAL(kind=8),DIMENSION(:),POINTER :: data
    INTEGER,DIMENSION(:),POINTER      :: idata
    !
    real(kind=8),dimension(:,:),allocatable :: coor
    integer,dimension(:,:),allocatable :: connec

    ! on compte les objects polyf lus
    integer :: nb_read=0

                            !1234567890123456789012345678901
    character(len=31) ::IAM='a_BDARY_POLYR::read_BDARY_POLYF'
    character(len=80) :: mes

    integer :: err_
    
    call read_all_patches_(IAM, nb_read, nb_vertices, nb_elements, coor, connec)
    !print*,'-',nb_read

    !do ip=1,nb_vertices
    !  write(*,'(3(1x,D12.5))') coor(:,ip)
    !enddo  
    !write(*,'(3(1x,I0))') connec

    call compute_mechanical_properties_surface_T3(nb_vertices, nb_elements, connec, coor, vol, shift, I1, I2, I3, localframe, err_)

    if (err_ > 0) then
       write(mes,'(A,1x,I0)') 'POLYF',nb_read
       call logmes(trim(mes))
       call faterr(IAM,'Something wrong')
    endif
       
    !print*,'--'

    !print*,vol,I1,I2,I3

    if (vol < 0.d0) then
       write(mes,'(A,1x,I0)') 'POLYF',nb_read
       call logmes(trim(mes))
       call FATERR(IAM,'negative volume')
    endif

    if (I1 < 0.d0 .or. I2 < 0.d0 .or. I3 < 0.d0) then
      write(mes,'(A,1x,I0,3(1x,D12.5))') 'POLYF',nb_read, I1,I2,I3
      call logmes(trim(mes))
      call FATERR(IAM,'negative inertia')
    endif

!    write(6,'(3(1x,D12.5))'),localframe,I1,I2,I3

    ALLOCATE(data(3*nb_vertices))
    ALLOCATE(idata(1 + 1 + (3*nb_elements)))

    idata(1) = nb_vertices
    idata(2) = nb_elements

    idata(2 + 1 : 2 + 3*idata(2)) = pack(connec,mask=.TRUE.)
    data = pack(coor,mask=.TRUE.)

    deallocate(coor,connec)
 
  END SUBROUTINE read_BDARY_POLYF

  subroutine read_BDARY_POLYO(data,idata,vol,I1,I2,I3,localframe,shift)

    implicit none

    ! data contains vertex coordinates
    ! idata contains the number of vertex, the number of face, the face connectivity

    integer                              :: nb_vertex,nb_faces
    real(kind=8)                         :: vol,I1,I2,I3
    real(kind=8), dimension(3)           :: shift
    real(kind=8), dimension(3,3)         :: localframe
    real(kind=8), dimension(:),  pointer :: data
    integer     , dimension(:),  pointer :: idata

    !***
    real(kind=8), dimension(:,:), allocatable :: coor
    integer     , dimension(:,:), allocatable :: connec

    ! c'est une variable statique qui compte ne nombre de POLYR lu
    integer :: nb_read=0

                            !1234567890123456789012345678901
    character(len=31) ::IAM='a_BDARY_POLYR::read_BDARY_POLYO'
    character(len=80) :: mes

    integer :: err_

    call read_all_patches_(IAM, nb_read, nb_vertex, nb_faces, coor, connec)

    !print*,'-',nb_read

    allocate(data(3*nb_vertex))
    allocate(idata(1 + 1 + (3*nb_faces)))

    idata(1) = nb_vertex
    idata(2) = nb_faces

    idata(2 + 1 : 2 + 3*idata(2)) = pack(connec,mask=.TRUE.)
    data = pack(coor,mask=.TRUE.)

    deallocate(coor,connec)

  end subroutine read_BDARY_POLYO

!!!------------------------------------------------------------------------
  SUBROUTINE old_read_BDARY_POLYR(data,idata,vol,I1,I2,I3,localframe,shift)

    IMPLICIT NONE

    ! data contains vertex coordinates
    ! idata contains the number of vertex, the number of face, the face connectivity

    INTEGER                           :: j,k,nb_vertex,nb_faces
    INTEGER                           :: data_sz, idata_sz
    INTEGER                           :: ibdyty,itacty
!    INTEGER                           :: lda,n,matz
    INTEGER                           :: nb,ierror
    REAL(kind=8)                      :: vol,voli,norm,I1,I2,I3
    REAL(kind=8)                      :: un_6,detJ             
    REAL(kind=8),DIMENSION(3)         :: P1,SP1,SP2,SP3,center,shift
    REAL(kind=8),DIMENSION(3)         :: Tcenter,Gpoint,wr,wi,vertex,Ip
    REAL(kind=8),DIMENSION(3,3)       :: I,I0,localframe,E
    INTEGER                           :: s1,s2,s3
    REAL(kind=8),DIMENSION(:),POINTER :: data
    INTEGER,DIMENSION(:),POINTER      :: idata

    integer :: nb_read=0
    character(len=23) :: IAM
          !12345678901234567890123
    IAM = 'a_BDARY_POLYR::old_read'

    nb_read = nb_read+1

!    READ(G_clin(40:47),'(I8)') nb_vertex
!    READ(G_clin(60:67),'(I8)') nb_faces

    READ(G_clin(40:47),*) nb_vertex
    READ(G_clin(60:67),*) nb_faces
    
    data_sz = 3*nb_vertex
    
    idata_sz = 1 + 1 + (3*nb_faces)
    
    ALLOCATE(data(data_sz))
    ALLOCATE(idata(idata_sz))

    idata(1) = nb_vertex
    idata(2) = nb_faces

    DO k=1,nb_vertex
       IF( .NOT. read_G_clin()) THEN
          print*,nb_read
          call faterr(IAM,'error while reading vertex in bdary_polyr')
       END IF
       READ(G_clin(35:48),'(D14.7)') data((k-1)*3+1)
       READ(G_clin(56:69),'(D14.7)') data((k-1)*3+2)
       READ(G_clin(77:90),'(D14.7)') data((k-1)*3+3)
    END DO

    DO k=1,nb_faces
       IF( .NOT. read_G_clin()) THEN
          print*,nb_read
          call faterr(IAM,'error while reading face in bdary_polyr')
       END IF

       READ(G_clin(35:47),*) idata(2+3*k-2)
       READ(G_clin(56:68),*) idata(2+3*k-1)
       READ(G_clin(77:89),*) idata(2+3*k)
    END DO

    ! Les polyèdres sont représentés par une surface triangularisée.
    ! Connaissant les indices des sommets composant les faces,le centre
    ! du polyèdre, qui est à l'origine, on peut découper le polyèdre en 
    ! tétraèdre dont on sait calculer le volume.
    ! Soit S le sommet d'un tétraèdre et P1,P2,P3 ses 3 autres sommets,
    ! son volume sera:
    ! V=|det(SP1,SP2,SP3)|/6
    ! Dans notre cas S est le barycentre du polyèdre dans le
    ! le repère absolu. P1,P2,P3 sont les sommets de la face considérée.
    ! on parcours toute les faces et on calcule à chaque fois un volume élémentaire. 

    un_6 = 1.D0/6.D0
    
    center=0.D0

!    print *,'calcul d''un centre approche'

    DO k=1,nb_vertex
!      print *,'---'
!      print *,k
!      print '(3(1x,D14.7))',data(3*k-2:3*k)
!      print *,'---'

       center(1) = center(1) + data(3*k-2)
       center(2) = center(2) + data(3*k-1)
       center(3) = center(3) + data(3*k)
    END DO

    center = center/REAL(nb_vertex,8)
    p1     = center

!    print '(3(1x,D14.7))',p1



    Tcenter = 0.D0
    Gpoint   = 0.D0
    vol  = 0.d0
    DO k=1,nb_faces
       s1 = idata(2+3*k-2)
       s2 = idata(2+3*k-1)
       s3 = idata(2+3*k)
                   
       SP1(1) = data(3*s1-2) - center(1)
       SP1(2) = data(3*s1-1) - center(2)
       SP1(3) = data(3*s1)   - center(3)

       SP2(1) = data(3*s2-2) - center(1)
       SP2(2) = data(3*s2-1) - center(2)
       SP2(3) = data(3*s2)   - center(3)

       SP3(1) = data(3*s3-2) - center(1)
       SP3(2) = data(3*s3-1) - center(2)
       SP3(3) = data(3*s3)   - center(3)
                   
       voli = DABS(determinant(SP1,SP2,SP3))*un_6
       vol  = vol + voli

       Tcenter = (0.25*(SP2 + SP3 + SP1)) + p1 
       Gpoint  = Gpoint + (Tcenter*voli)

    END DO

    IF(vol.LT.1.D-18)THEN
       print*,nb_read
       call faterr(IAM,'polyr volume is less than 1.D-18')
    END IF

    Gpoint = Gpoint/vol

!    print *,'G',Gpoint
!    print *,vol

! au cas ou on oublie le dof.ini

    localframe = Id33

    IF ( (DABS(I1).GT.1.D-18).AND.(DABS(I2).GT.1.D-18).AND.(DABS(I3).GT.1.D-18)) RETURN
       
    DO k=1,nb_vertex
       data(3*k-2) = data(3*k-2) - Gpoint(1)
       data(3*k-1) = data(3*k-1) - Gpoint(2)
       data(3*k)   = data(3*k)   - Gpoint(3)
!
!      print *,'---'
!      print *,k
!      print '(3(1x,D14.7))',data(3*k-2:3*k)
!      print *,'---'
    END DO

    P1 = 0.D0
    I  = 0.d0
    DO k=1,nb_faces
       s1 = idata(2+3*k-2)
       s2 = idata(2+3*k-1)
       s3 = idata(2+3*k)
                   
       SP1(1) = data(3*s1-2)
       SP1(2) = data(3*s1-1)
       SP1(3) = data(3*s1)

       SP2(1) = data(3*s2-2)
       SP2(2) = data(3*s2-1)
       SP2(3) = data(3*s2)

       SP3(1) = data(3*s3-2)
       SP3(2) = data(3*s3-1)
       SP3(3) = data(3*s3)
                   
       !print*,'----------'
       !print*,'k'

       call compute_inertia_tetrahedron(p1,sp1,sp2,sp3,I0)

       !print*,I0


       I  = I + I0

    END DO

    !print *,'I'
    !print '(3(1x,D12.5))',I


    shift = Gpoint
    
    call diagonalise33(I,Ip,localframe)

    I1  = Ip(1)
    I2  = Ip(2)
    I3  = Ip(3)

    DO k=1,nb_vertex
      vertex(1) = data(3*k-2)
      vertex(2) = data(3*k-1)
      vertex(3) = data(3*k)
      data(3*k-2) = DOT_PRODUCT(localframe(1:3,1),vertex(1:3))
      data(3*k-1) = DOT_PRODUCT(localframe(1:3,2),vertex(1:3))
      data(3*k  ) = DOT_PRODUCT(localframe(1:3,3),vertex(1:3))
    END DO

!    write(6,'(3(1x,D12.5))'),localframe,I1,I2,I3
    
 
  END SUBROUTINE old_read_BDARY_POLYR
!!!------------------------------------------------------------------------



!!!------------------------------------------------------------------------
  SUBROUTINE write_BDARY_POLYR(nfich,itacty,tacID,color,data,idata,shift)

    IMPLICIT NONE
    INTEGER                             :: nfich,itacty
    CHARACTER(len=5)                    :: tacID,color
    REAL(kind=8),DIMENSION(:),POINTER   :: data
    INTEGER,DIMENSION(:),POINTER        :: idata
    INTEGER                             :: i,nb_vertex,nb_faces
    real(kind=8),dimension(3)           :: shift
    
    nb_vertex = idata(1)
    nb_faces  = idata(2)

    if( tacID == 'POLYR' ) then
      WRITE(nfich,104) tacID,itacty,'color',color,'nb_vertex=',nb_vertex,'nb_faces=',nb_faces
    else
      WRITE(nfich,105) tacID,itacty,'color',color,'nb_patch=',1
      WRITE(nfich,106) 'nb_vertex=',nb_vertex,'nb_faces=',nb_faces
    end if
    DO i=1,nb_vertex
       WRITE(nfich,131) 'coo1=',shift(1)+data(3*i-2),'coo2=',shift(2)+data(3*i-1),'coo3=',shift(3)+data(3*i)

!       print *,i
!       print '(3(1x,D14.5))',shift(1:3)
!       print '(3(1x,D14.5))',data(3*i-2:3*i)

    END DO
    DO i=1,nb_faces
       WRITE(nfich,132) 'ver1=',idata(2+3*i-2),'ver2=',idata(2+3*i-1),'ver3=',idata(2+3*i)
    END DO
    
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7,4X,A9,I7)
105 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A09,I7)
106 FORMAT(29x,A10,I7,4X,A9,I7)
131 FORMAT(27X,3(2X,A5,D14.7))
132 FORMAT(27X,3(2X,A5,I7,7X))

  END SUBROUTINE write_BDARY_POLYR

END MODULE a_BDARY_POLYR
