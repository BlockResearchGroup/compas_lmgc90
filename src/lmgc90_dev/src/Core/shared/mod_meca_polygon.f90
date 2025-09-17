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
MODULE meca_polygon

  use algebra, only : cross_product

  use polygon, only : compute_mean_plane      , &
                      distance_node2edge      , &
                      polygon_translation     , &
                      polygon_rotation        , &
                      polygon_moments         , &
                      envelope                , &
                      polygon_one_point_in_wc , &
                      polygons_intersection_wc, &
                      polygon_principal_properties, &
                      polygon_3D_principal_properties

  use lib_minpack, only : lmdif

  use overall, only : faterr

  implicit none

  private

  public  compute_central_kernel, &
          compute_stress_field

  private compute_stress_field_RDM, &
          compute_stress_field_Distance, &
          get_face_decomprimee, &
          RESIDU, LMDIF1__

CONTAINS

  !> central kernel computation from the contact support (2D or 3D)
  !> if center of pressure is also provided, check if inside
  subroutine compute_central_kernel(points, sizes, decomp, nb_ctc, points_tc, test, is_in, bavard)
    implicit none

    real(kind=8), dimension(:,:), pointer     :: points, points_tc
    real(kind=8), dimension(:)  , pointer     :: test
    integer,      dimension(:),   pointer     :: sizes
    real(kind=8)                              :: decomp
    integer                                   :: is_in
    logical                                   :: bavard
    !
    integer                                   :: err_, psize
    integer                                   :: i,j,ii,k,ip,nbp,nbtc,nidp,nb_ctc
    real(kind=8)                              :: area,Iu,Iv,a,b,mini,tmp
    real(kind=8)                              :: umin,umax,vmin,vmax,pu,pv,tu,tv,uswell,vswell,swell
    real(kind=8), dimension(3)                :: centre,u,v,vec,n
    real(kind=8), dimension(:)  , pointer     :: tst_p
    real(kind=8), dimension(:,:), pointer     :: tc2d, tc2d_
    REAL(kind=8), dimension(:,:), allocatable :: pt2d
    integer,      dimension(:),   allocatable :: idp,itmp
    integer,      dimension(:),   pointer     :: tc_size

                                !123456789012345678901234567890123456
    CHARACTER(len=36)  :: IAM = 'meca_polygon::compute_central_kernel'

    nbp   = size(points,dim=2)
    psize = size(points,dim=1)

    tst_p => null()

    ! moments quadratiques et repere principal d'inertie
    if( psize == 2 ) then
      call polygon_principal_properties(points,sizes,area,centre(1:2),Iu,Iv,u(1:2),v(1:2))
      if (bavard) then
          print*,"centre = ",centre
          print*,"area   = ",area
          print*,"Iu     = ",Iu,u
          print*,"Iv     = ",Iv,v
      endif
    else
      call compute_mean_plane(points,centre,n)
      call polygon_3D_principal_properties(points,sizes,n,centre,u,v,area,Iu,Iv)
      if (bavard) then
          print*, "n     = ", n
          print*,"centre = ",centre
          print*,"area   = ",area
          print*,"Iu     = ",Iu,u
          print*,"Iv     = ",Iv,v
      endif
    end if

    ! passage dans le repere principal d'inertie
    allocate(pt2d(2,nbp))
    DO i = 1,nbp
       vec(1:psize) = points(1:psize,i) - centre(1:psize)
       pt2d(1,i) = DOT_PRODUCT(vec(1:psize), u(1:psize))
       pt2d(2,i) = DOT_PRODUCT(vec(1:psize), v(1:psize))
    ENDDO

    if( associated(test) ) then
       allocate( tst_p(2) )
       test(1:psize) = test(1:psize) - centre(1:psize)
       tst_p(1) = DOT_PRODUCT(test(1:psize), u(1:psize))
       tst_p(2) = DOT_PRODUCT(test(1:psize), v(1:psize))
    end if

    ! calcul des bords du tiers central
    ip   = 0
    allocate(tc2d(2,nbp*(nbp+1)/2))
    tc2d = 0.D0
    DO i = 1,nbp
       DO j = i+1,nbp
          tmp = pt2d(2,i)*pt2d(1,j)-pt2d(2,j)*pt2d(1,i)
          IF ( abs(tmp) > 1.D-20 ) THEN
             a   = -Iv/area * (pt2d(2,i)-pt2d(2,j)) / tmp
             b   =  Iu/area * (pt2d(1,i)-pt2d(1,j)) / tmp
             ! check si le point (a,b) est dans le domaine
             mini = 1.D20
             DO k = 1,nbp
                tmp = pt2d(1,k)*a/Iv + pt2d(2,k)*b/Iu + 1.D0/area
                IF ( tmp < mini ) THEN
                   mini = tmp
                ENDIF
             END DO
             ! si il est dans le domaine, on le garde
             IF ( mini > -1.D-6 ) THEN
                ip = ip+1
                tc2d(1,ip) = a
                tc2d(2,ip) = b
             END IF
          END IF
       END DO
    END DO
    nbtc = ip

    if (bavard) then
       print*,"  tc = ",nbtc
       print*,tc2d(1,1:nbtc)
       print*,tc2d(2,1:nbtc)
    endif

    ! reduction du nb de points de contact en restreignant au convex hull
    allocate(idp(nbtc))
    call envelope(tc2d(:,1:nbtc), nbtc, idp, nidp)
    if (bavard) print*,"hull = ",idp(1:nidp)
    ! recuperation du nombre de points de contact
    nb_ctc = nidp

    swell = 1.D0 + 2.D0*abs(decomp)

    ! si demande, on calcul un swell des points de contact
    IF ( decomp .ne. 0.D0 ) THEN
       ! calcul le bounding box du polygone de depart
       umin = 0.; umax = 0.
       vmin = 0.; vmax = 0.
       DO i = 1,nbp
          umin = min(umin, pt2d(1,i))
          umax = max(umax, pt2d(1,i))
          vmin = min(vmin, pt2d(2,i))
          vmax = max(vmax, pt2d(2,i))
       END DO
       pu = umax-umin
       pv = vmax-vmin
       ! calcul le bounding box du polygone du tiers central
       umin = 0.; umax = 0.
       vmin = 0.; vmax = 0.
       DO i = 1,nidp
          umin = min(umin, tc2d(1,idp(i)))
          umax = max(umax, tc2d(1,idp(i)))
          vmin = min(vmin, tc2d(2,idp(i)))
          vmax = max(vmax, tc2d(2,idp(i)))
       END DO
       tu = umax-umin
       tv = vmax-vmin
       ! swell anisotrope
       uswell = (1D0 + decomp*(pu/tu-1D0))
       vswell = (1D0 + decomp*(pv/tv-1D0))
    ELSE
       uswell = 1D0
       vswell = 1D0
    ENDIF

    ! check if cop in points_tc
    if( associated(test) ) then
      allocate(tc_size(1))
      tc_size(1) = nbtc
      allocate( tc2d_(2,nb_ctc) )
      do i = 1,nb_ctc
       tc2d_(:,i) = tc2d(:,idp(i))
      end do
      is_in = polygon_one_point_in_wc(tst_p, tc2d_, tc_size)
      deallocate(tc_size)
      deallocate( tc2d_ )
    end if

    ! retour dans le repere initial
    allocate( points_tc(psize,nb_ctc) )
    do i = 1,nb_ctc
       !points_tc(:,i) = centre(:) + swell*tc2d(1,idp(i))*u(:) + swell*tc2d(2,idp(i))*v(:)
       points_tc(1:psize,i) = centre(1:psize) + uswell*tc2d(1,idp(i))*u(1:psize) + vswell*tc2d(2,idp(i))*v(1:psize)
    end do

    deallocate(pt2d,tc2d,idp)

    if( associated(tst_p) ) deallocate(tst_p)
    nullify(tst_p)

  end subroutine compute_central_kernel


!-----------------------------------------------------------------------
! Calcul du champ de contraintes dans une face
!-----------------------------------------------------------------------

  !> calcul le champ de contrainte en tenant compte d'une eventuelle decompression
  subroutine compute_stress_field(face, sizes, cdp, normal, Rn, faceC, sizeC, sigmas, faceD, sizeD, decomp, err_, bavard_)
    implicit none

    REAL(kind=8)                         :: Rn,decomp
    INTEGER,     DIMENSION(:),  POINTER  :: sizes,sizeC,sizeD
    REAL(kind=8),DIMENSION(:,:),POINTER  :: face,faceC,faceD
    REAL(kind=8),DIMENSION(3)            :: cdp, normal
    REAL(kind=8),DIMENSION(:),  POINTER  :: sigmas
    INTEGER                              :: err_
    LOGICAL                              :: bavard_,bavard
    !
    INTEGER                              :: i,ip,nbp,nbh
    REAL(kind=8)                         :: area,It,Is,a,b,u,v,rayon,angle,dist
    REAL(kind=8),DIMENSION(2)            :: pnt
    REAL(kind=8),DIMENSION(3)            :: vec,centre,t,s
    INTEGER,     DIMENSION(:),  POINTER  :: sz,hull
    REAL(kind=8),DIMENSION(:,:),POINTER  :: vert,vertC,vertD
    REAL(kind=8),DIMENSION(:),  POINTER  :: areaC,areaD
    LOGICAL                              :: isok
    INTEGER                              :: INFO
    REAL(kind=8)                         :: TOL = 1.49D-8
    REAL(kind=8),DIMENSION(2)            :: X2,FVEC
                                !1234567890123456789012345678901234
    CHARACTER(len=34)  :: IAM = 'meca_polygon::compute_stress_field'

    COMMON /MINIM/ a,b,vert,sz,bavard
    
    if ( size(face,dim=1) .ne. 3 ) then
        call faterr(IAM,"Input face must be of size (3,:)")
    endif
    
    ! recopie de sizes pour eviter une erreur de compilation
    bavard = bavard_
    allocate(sz(size(sizes)))
    sz = sizes
    
    err_ = 0
    
    nbp = size(face,dim=2)
    ! regarde si le CdP ne serait pas sur un bord du convex-hull de la face de contact
    allocate(hull(nbp))
    call envelope(face, nbp, hull, nbh)
    do i = 1,nbh
        ip   = modulo(i,nbh) + 1
        dist = distance_node2edge(cdp, face(:,hull(i)), face(:,hull(ip)))
!        print*,cdp,hull(i),hull(ip)
        if ( dist < 1.D-3 ) then
            if ( bavard ) print*,"  -> cdp au bord de la face de contact"
            ! toute la face est decomprimee
            decomp = 1.D0
            allocate(faceD(3,nbp),sizeD(size(sizes)))
            faceD  = face
            sizeD  = sizes
            deallocate(sz,hull)
            return
        endif
    enddo

    ! calcul le champ de contraintes par une approche lineaire type RDM
    call compute_stress_field_RDM(face, sizes, cdp, normal, Rn, sigmas)
    
    if ( maxval(sigmas) <= 0.D0 ) then
    ! si toute la face est en compression
        if ( bavard ) print*,"  -> cdp dans le tiers central"
        ! toute la face est comprimee
        decomp = 0.D0
        allocate(faceC(3,nbp),sizeC(size(sizes)))
        faceC  = face
        sizeC  = sizes
    
    else
        if ( bavard ) print*,"  -> cdp hors du tiers central"

        ! calcul du repere principal
        call polygon_3D_principal_properties(face, sz, normal, centre, s, t, area, Is, It)

        ! projection en 2D sur le repere local
        allocate(vert(2,nbp))
        do i = 1,nbp
           vec(:)    = face(:,i) - centre(:)
           vert(1,i) = dot_product(vec, s)
           vert(2,i) = dot_product(vec, t)
        enddo
        ! position du centre de pousse dans repere local
        vec(:) = cdp(:) - centre(:)
        a      = dot_product(vec, s)
        b      = dot_product(vec, t)
        
        ! initial guess
        rayon = 0.d0
        angle = sign(1.D0,b)*acos( (a/It) / sqrt((a/It)**2+(b/Is)**2) )
        if ( bavard ) print*,"  |initial| rayon = ",rayon,"     angle = ",angle
        
        ! optimisation sur le rayon ET l'angle
        if ( bavard ) print*,"------------------------ LMDIF --------------------------"
        X2 = (/ 0.D0, angle /)
        CALL LMDIF1__( 2, X2, 2, FVEC, TOL, INFO )
        rayon = X2(1)
        angle = X2(2)
        if ( bavard ) then
            print*,"  |optimis| rayon = ",rayon,"     angle = ",angle,"     INFO = ",INFO
            print*,"          FVEC(1) = ",FVEC(1),"   FVEC(2) = ",FVEC(2)
        endif

        ! LMDIF n'a pas converge
        if ( INFO == 4 ) then
             err_ = 1
             deallocate(sz,hull,vert,sigmas)
             return
        endif
        ! LMDIF a mal converge
        if ( (FVEC(1) > 1.0D-4) .OR. (FVEC(2) > 1.0D-4) ) then
            err_ = 2
        endif
        
        ! recupere le contour de la partie comprimee de la face de contact
        isok = get_face_comprimee(vert, sz, rayon, angle, vertC, sizeC, areaC)
        if ( bavard ) print*,"sizeC = ",sizeC
        
        if ( isok ) then

            ! libere sigmas (qui avait ete alloue par compute_stress_field_RDM)
            deallocate(sigmas)
            ! calcul le champ de contrainte lineaire sur la partie comprimee de la face de contact
            call compute_stress_field_Distance(vertC, sizeC, rayon, angle, Rn, sigmas)

            decomp = 1.d0 - SUM(areaC)/area
            if ( bavard ) print*,"decompression = ",decomp

            ! retour en 3D
            nbp = size(vertC,dim=2)
            allocate(faceC(3,nbp))
            do i = 1,nbp
               faceC(:,i)  = vertC(1,i)*s(:) + vertC(2,i)*t(:) + centre(:)
            enddo

            deallocate(vertC,areaC)
            
            ! recupere la partie decomprimee de la face de contact (= le complementaire de la face comprimee)
            if ( decomp >= 1.D-3 ) then
                isok = get_face_decomprimee(vert, sz, rayon, angle, vertD, sizeD, areaD)
                if ( isok ) then
                    if ( bavard ) print*,"sizeD = ",sizeD
                    ! retour en 3D
                    nbp = size(vertD,dim=2)
                    allocate(faceD(3,nbp))
                    do i = 1,nbp
                       faceD(:,i)  = vertD(1,i)*s(:) + vertD(2,i)*t(:) + centre(:)
                    enddo
                    deallocate(vertD,areaD)
                else
                    ! sinon, on cree une face fictive nulle qui ne sera pas affichee
                    allocate(faceD(3,1),sizeD(1))
                    faceD  = 0.D0
                    sizeD  = 0
                endif

            else
                ! sinon, on cree une face fictive nulle qui ne sera pas affichee
                allocate(faceD(3,1),sizeD(1))
                faceD  = 0.D0
                sizeD  = 0
                
            endif

        else
            err_ = 2

        endif
        deallocate(vert)
        
    endif
    
    deallocate(sz,hull)

  end subroutine compute_stress_field


  !> calcule le champ de contrainte RDM
  subroutine compute_stress_field_RDM(face, sizes, cdp, normal, Rn, sigmas)
    implicit none

    REAL(kind=8)                         :: Rn
    INTEGER,     DIMENSION(:),  POINTER  :: sizes
    REAL(kind=8),DIMENSION(:,:),POINTER  :: face
    REAL(kind=8),DIMENSION(3)            :: cdp, normal
    REAL(kind=8),DIMENSION(:),  POINTER  :: sigmas
    !
    integer                              :: i,nbp
    real(kind=8)                         :: area,It,Is,a,b,a00,a10,a01,a20,a02,a11
    real(kind=8),dimension(3)            :: vec,centre,t,s
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v

                                !1234567890123456789012345678901
    CHARACTER(len=31)  :: IAM = 'STRESS::compute_stress_field_RDM'

    nbp = size(face,dim=2)

    call polygon_3D_principal_properties(face, sizes, normal, centre, s, t, area, Is, It)

    ! projete le polygone dans le repere local
    allocate(v(2,nbp))
    do i = 1,nbp
       vec(:) = face(:,i) - centre(:)
       v(1,i) = dot_product(vec, s)
       v(2,i) = dot_product(vec, t)
    enddo

    ! passage du centre de pousse dans le repere local 
    a   = dot_product( cdp-centre, s )
    b   = dot_product( cdp-centre, t )
    
    ! iteration sur tous les coins de la surface
    allocate(sigmas(nbp))
    do i = 1,nbp
        ! calcul de la contrainte normale (la compression est negative)
        sigmas(i) = -1.D0 * Rn * (1.D0/area + a/It*v(1,i) + b/Is*v(2,i) )
    enddo

    deallocate(v)

  end subroutine compute_stress_field_RDM
 
 
  !> calcule le champ de contrainte avec une distribution lineaire
  subroutine compute_stress_field_Distance(vert, sizes, rayon, angle, Rn, sigmas)
    implicit none

    REAL(kind=8)                         :: Rn, rayon, angle
    INTEGER,     DIMENSION(:),  POINTER  :: sizes
    REAL(kind=8),DIMENSION(:,:),POINTER  :: vert
    REAL(kind=8),DIMENSION(:),  POINTER  :: sigmas
    !
    INTEGER                              :: i,nbp
    REAL(kind=8)                         :: a00,a10,a01,a20,a02,a11
    REAL(kind=8),DIMENSION(2)            :: centre,point,t,s,vec
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v

                                !123456789012345678901234567890123456
    CHARACTER(len=36)  :: IAM = 'STRESS::compute_stress_field_Distance'

    nbp = size(vert,dim=2)

    ! cree un repere local base sur l'axe (point,direction)
    point = (/ rayon*cos(angle) , rayon*sin(angle) /)
    t     = (/      -sin(angle) ,       cos(angle) /)
    s     = (/      -t(2)       ,       t(1)       /)
    ! projete le polygone dans ce repere local
    allocate(v(2,nbp))
    do i = 1,nbp
       vec(:) = vert(:,i) - point(:)
       v(1,i) = dot_product(vec, t)
       v(2,i) = dot_product(vec, s)
    enddo

    call polygon_moments(v, sizes, a00, a10, a01, a20, a02, a11)

    allocate(sigmas(nbp))
    do i = 1,nbp
        ! calcul de la contrainte normale (la compression est negative)
        ! mais le volume calcule par polygon_moments est aussi negatif (a verifier) !
        sigmas(i) =  Rn / a01 * abs(v(2,i))
    enddo
    
    deallocate(v)

  end subroutine compute_stress_field_Distance


  !> calcule la face comprimee
  logical function get_face_comprimee(vert, sizes, rayon, angle, vertC, sizeC, areaC)
    implicit none

    REAL(kind=8)                         :: rayon,angle
    INTEGER,     DIMENSION(:),  POINTER  :: sizes,sizeC,sizes1
    REAL(kind=8),DIMENSION(:),  POINTER  :: areaC
    REAL(kind=8),DIMENSION(:,:),POINTER  :: vert,vertC
    !
    INTEGER                              :: i,nbp
    REAL(kind=8)                         :: centre(2),trans(2)
    REAL(kind=8),DIMENSION(:,:),POINTER  :: carre

                                !1234567890123456789012345
    CHARACTER(len=25)  :: IAM = 'STRESS::get_face_comprimee'
    
    get_face_comprimee = .false.
    nullify(carre,vertC,sizeC,areaC)
    
    ! definition des transformations geometriques
    trans(:)   = (/ rayon, 0.d0 /)
    centre(:)  = (/ 0.d0, 0.d0 /)
   
    allocate(carre(2,4))
    carre(:,1) = (/ 0.d0    , -500.d0 /)
    carre(:,2) = (/ 0.d0    ,  500.d0 /)
    carre(:,3) = (/ 1000.d0 ,  500.d0 /)
    carre(:,4) = (/ 1000.d0 , -500.d0 /)
    
    call polygon_translation(carre,trans)
    call polygon_rotation(carre,angle,centre)
    ! polygon 'carre' is composed of only one polytope
    allocate(sizes1(1))
    sizes1(1) = 4
    call polygons_intersection_wc(carre, sizes1, vert, sizes, 0.d0, 0.d0, 0.d0, 0, 0.d0, vertC, sizeC, areaC)

    deallocate(carre,sizes1)
    
    if ( associated(vertC) ) then
        get_face_comprimee = .true.
    endif

  end function get_face_comprimee


  !> recupere la face decomprimee
  logical function get_face_decomprimee(vert, sizes, rayon, angle, vertD, sizeD, areaD)
    implicit none

    REAL(kind=8)                         :: rayon,angle
    INTEGER,     DIMENSION(:),  POINTER  :: sizes,sizeD,sizes1
    REAL(kind=8),DIMENSION(:),  POINTER  :: areaD
    REAL(kind=8),DIMENSION(:,:),POINTER  :: vert,vertD
    !
    INTEGER                              :: i,nbp
    REAL(kind=8)                         :: centre(2),trans(2)
    REAL(kind=8),DIMENSION(:,:),POINTER  :: carre

                                !123456789012345678901234567
    CHARACTER(len=27)  :: IAM = 'STRESS::get_face_decomprimee'

    get_face_decomprimee = .false.
    nullify(carre,vertD,sizeD,areaD)
    
    ! definition des transformations geometriques
    trans(:)   = (/ rayon, 0.d0 /)
    centre(:)  = (/ 0.d0, 0.d0 /)

    allocate(carre(2,4))
    carre(:,1) = (/     0.d0 , -500.d0 /)
    carre(:,2) = (/     0.d0 ,  500.d0 /)
    carre(:,3) = (/ -1000.d0 ,  500.d0 /)
    carre(:,4) = (/ -1000.d0 , -500.d0 /)
    
    call polygon_translation(carre,trans)
    call polygon_rotation(carre,angle,centre)
    ! polygon 'carre' is composed of only one polytope
    allocate(sizes1(1))
    sizes1(1) = 4
    call polygons_intersection_wc(carre, sizes1, vert, sizes, 0.d0, 0.d0, 0.d0, 0, 0.d0, vertD, sizeD, areaD)
    
    deallocate(carre,sizes1)
    
    if ( associated(vertD) ) then
        get_face_decomprimee = .true.
    endif
    
  end function get_face_decomprimee
 
 
  !> fonction RESIDU a minimiser par LMDIF, HYBRD
  !> calcul le centre de gravite de la zone comprimee
  subroutine RESIDU(M,N,X,F,IFLAG)
    implicit none
    
    INTEGER                              :: M,N,IFLAG
    REAL(kind=8)                         :: X(N),F(M)
    !
    INTEGER                              :: i,nbp
    REAL(kind=8)                         :: rayon,angle,a0,b0,a00,a10,a01,a20,a02,a11
    REAL(kind=8),DIMENSION(2)            :: vec,point,t,s
    INTEGER,     DIMENSION(:),  POINTER  :: sizes,sizeC
    REAL(kind=8),DIMENSION(:),  POINTER  :: areaC
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v,vert,vertC
    LOGICAL                              :: isok
    LOGICAL                              :: bavard
                                !12345678901234
    CHARACTER(len=14)  :: IAM = 'STRESS::RESIDU'

    COMMON /MINIM/ a0,b0,vert,sizes,bavard
    
    if ( bavard ) print*,IAM
    if ( bavard ) print*,"       X = ",X(1),X(2)

    nullify(vertC,sizeC,areaC)
    
    rayon = X(1)
    angle = X(2)
    isok  = get_face_comprimee(vert, sizes, rayon, angle, vertC, sizeC, areaC)
    
    if ( .not. isok ) then
        F(:) = 1.d3*abs(rayon) + abs(angle)
        if ( bavard ) print*,"  1 - F = ",F(1),F(2)

    else
        nbp = size(vertC,dim=2)

        ! cree un repere local base sur l'axe (point,direction)
        point = (/ rayon*cos(angle) , rayon*sin(angle) /)
        t     = (/      -sin(angle) ,       cos(angle) /)
        s     = (/      -cos(angle) ,      -sin(angle) /)
        ! projete le polygone dans ce repere local
        allocate(v(2,nbp))
        do i = 1,nbp
           vec(:) = vertC(:,i) - point(:)
           v(1,i) = dot_product(vec, t)
           v(2,i) = dot_product(vec, s)
        enddo

        call polygon_moments(v, sizeC, a00, a10, a01, a20, a02, a11)
        
        ! distance entre le centre de poussee et le centre de gravite
        F(:) = (/ a0, b0 /) - ( point(:) + a11/a01*t(:) + a02/a01*s(:) )
        
        deallocate(v,vertC,sizeC,areaC)

        if ( bavard ) then
            print*," centre = ",point(:) + a11/a01*t(:) + a02/a01*s(:)
            print*,"  2 - F = ",F(1),F(2)
        endif
     
    endif

  end subroutine RESIDU


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Interfaces avec MINPACK
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine LMDIF1__( n, x, m, fvec, tol, info )
  !*****************************************************************************80
  !
  !  LMDIF1 minimizes M functions in N variables using Levenberg-Marquardt method.
  !
  !  Discussion:
  !
  !    LMDIF1 minimizes the sum of the squares of M nonlinear functions in
  !    N variables by a modification of the Levenberg-Marquardt algorithm.
  !    This is done by using the more general least-squares solver LMDIF.
  !    The user must provide a subroutine which calculates the functions.
  !    The jacobian is then calculated by a forward-difference approximation.
  !
  !  Licensing:
  !
  !    This code may freely be copied, modified, and used for any purpose.
  !
  !  Modified:
  !
  !    06 April 2010
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Jorge More, Burton Garbow, Kenneth Hillstrom.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Jorge More, Burton Garbow, Kenneth Hillstrom,
  !    User Guide for MINPACK-1,
  !    Technical Report ANL-80-74,
  !    Argonne National Laboratory, 1980.
  !
  !  Parameters:
  !
  !    Input, external FCN, the name of the user-supplied subroutine which
  !    calculates the functions.  The routine should have the form:
  !      subroutine fcn ( m, n, x, fvec, iflag )
  !      integer ( kind = 4 ) n
  !      real ( kind = 8 ) fvec(m)
  !      integer ( kind = 4 ) iflag
  !      real ( kind = 8 ) x(n)
  !
  !    If IFLAG = 0 on input, then FCN is only being called to allow the user
  !    to print out the current iterate.
  !    If IFLAG = 1 on input, FCN should calculate the functions at X and
  !    return this vector in FVEC.
  !    To terminate the algorithm, FCN may set IFLAG negative on return.
  !
  !    Input, integer ( kind = 4 ) M, the number of functions.
  !
  !    Input, integer ( kind = 4 ) N, the number of variables.  
  !    N must not exceed M.
  !
  !    Input/output, real ( kind = 8 ) X(N).  On input, X must contain an initial
  !    estimate of the solution vector.  On output X contains the final
  !    estimate of the solution vector.
  !
  !    Output, real ( kind = 8 ) FVEC(M), the functions evaluated at the output X.
  !
  !    Input, real ( kind = 8 ) TOL.  Termination occurs when the algorithm
  !    estimates either that the relative error in the sum of squares is at
  !    most TOL or that the relative error between X and the solution is at
  !    most TOL.  TOL should be nonnegative.
  !
  !    Output, integer ( kind = 4 ) INFO, error flag.  If the user has terminated
  !    execution, INFO is set to the (negative) value of IFLAG. See description
  !    of FCN.  Otherwise, INFO is set as follows:
  !    0, improper input parameters.
  !    1, algorithm estimates that the relative error in the sum of squares
  !       is at most TOL.
  !    2, algorithm estimates that the relative error between X and the
  !       solution is at most TOL.
  !    3, conditions for INFO = 1 and INFO = 2 both hold.
  !    4, FVEC is orthogonal to the columns of the jacobian to machine precision.
  !    5, number of calls to FCN has reached or exceeded 200*(N+1).
  !    6, TOL is too small.  No further reduction in the sum of squares
  !       is possible.
  !    7, TOL is too small.  No further improvement in the approximate
  !       solution X is possible.
  
    implicit none
  
    INTEGER(kind=4) :: n, m, info, ldfjac, maxfev, mode, nfev, nprint
    INTEGER(kind=4) :: ipvt(n)
    REAL(kind=8)    :: tol, ftol, gtol, xtol, epsfcn, factor
    REAL(kind=8)    :: x(n), fvec(m), diag(n), qtf(n), fjac(m,n)
  
                                !1234567890123
    CHARACTER(len=13)  :: IAM = 'STRESS::LMDIF1'
  
    info   = 0
  
    factor = 100.0D+00
    maxfev = 200 * ( n + 1 )
    ftol   = tol
    xtol   = tol
    gtol   = 0.0D+00
!    epsfcn = 0.0D+00
    epsfcn = 1.0D-8
    mode   = 1
    nprint = 0
    ldfjac = m
  
    call lmdif( RESIDU, m, n, x, fvec, ftol, xtol, gtol, maxfev, epsfcn,        &
                diag, mode, factor, nprint, info, nfev, fjac, ldfjac, ipvt, qtf )
  
    if ( info == 8 ) then
      info = 4
    end if
  
  end subroutine LMDIF1__

 
END MODULE meca_polygon

