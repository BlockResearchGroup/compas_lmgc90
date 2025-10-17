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

!> polygon contains a set of function and subroutine
!> usefull to manage 2D polygon

MODULE polygon

  use algebra, only : length3, &
                      cross_product, &
                      diagonalise33

  use clipper, only : polygones_intersection , &
                      polygone_simplification, &
                      polygone_pointinpolygon, &
                      clipper_free

  implicit none

  private

  public compute_mean_plane, &
         distance_node2edge, &
         polygon_principal_properties, &
         polygon_3D_principal_properties, &
         polygon_translation, &
         polygon_rotation, &
         polygon_moments, &
         envelope       , &
         polygons_intersection_wc , &
         polygon_simplification_wc, &
         polygon_one_point_in_wc  , &
         polygon_all_points_in_wc

  private comp_rep


contains


    SUBROUTINE comp_rep(t, n, s)
        IMPLICIT NONE
        INTEGER                   :: i,ip
        REAL(kind=8),DIMENSION(3) :: n,t,s

        IF (dabs(n(1)) < 1.d-15) THEN
        ! fd on est dans le plan y-z
             t(1) =  n(1)
             t(2) = -n(3)
             t(3) =  n(2)

             t = t/length3(t)
             s = cross_product(t,n)

             RETURN

        ENDIF

        IF (dabs(n(2)) < 1.d-15) THEN
        ! fd on est dans le plan z-x
             t(2) =  n(2)
             t(1) = -n(3)
             t(3) =  n(1)

             t = t/length3(t)
             s = cross_product(t,n)

             RETURN

        ENDIF

        IF (dabs(n(3)) < 1.d-15) THEN
        ! fd on est dans le plan x-y
             t(1) = -n(2)
             t(2) =  n(1)
             t(3) =  n(3)

             t = t/length3(t)
             s = cross_product(t,n)

             RETURN

        ENDIF

        !fd cas general
        !fd on genere un premier vecteur perpendiculaire
    !    t(1) = 1.; t(2) = 0. ; t(3) = 0.
        ! fc: essai
        i     = MAXLOC(abs(n), DIM=1)
        ip    = modulo(i,3) + 1
        t     = 0.D0
        t(ip) = SIGN(1.D0, n(i))

        s = cross_product(t,n)
        t = s/length3(s)
        s = cross_product(t,n)

    END SUBROUTINE comp_rep


    ! Translation d'un polygone
    ! Input     v(2,n)    : coordinates of the vertices
    !           trans     : translation vector
    ! Output    vv(2,n)   : coordinates of the vertices
    subroutine polygon_translation(v, trans)
      implicit none
      REAL(kind=8),DIMENSION(:,:) :: v
      REAL(kind=8),DIMENSION(2)   :: trans

      INTEGER       :: i,n

      n = size(v,dim=2)
      do i = 1, n
         v(:,i) = v(:,i) + trans(:)
      end do
    end subroutine polygon_translation


    ! Rotation d'un polygone
    ! Input     v(2,n)    : coordinates of the vertices
    !           angle     : rotation angle
    !           centre    : rotation centre
    ! Output    vv(2,n)   : coordinates of the vertices
    subroutine polygon_rotation(v, angle, centre)
      implicit none
      REAL(kind=8),DIMENSION(:,:)             :: v
      REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: vv
      REAL(kind=8)                            :: angle
      REAL(kind=8),DIMENSION(2)               :: centre

      INTEGER       :: i,n
      REAL(kind=8)  :: c,s

      n = size(v,dim=2)
      allocate(vv(2,n))
      c = cos(angle)
      s = sin(angle)
      do i = 1, n
         vv(1,i) = c*(v(1,i)-centre(1)) - s*(v(2,i)-centre(2)) + centre(1)
         vv(2,i) = s*(v(1,i)-centre(1)) + c*(v(2,i)-centre(2)) + centre(2)
      end do

      v = vv
      deallocate(vv)

    end subroutine polygon_rotation


    ! Calcule les proprietes geometriques d'un polygone 2D
    ! Input     v(2,n)    : coordinates of the vertices
    ! Output    area      : area
    !           centre    : geometrical centre
    !           I1, I2    : principal inertia
    !           V1, V2    : principal directions
    subroutine polygon_principal_properties(v, sizes, area, centre, I1, I2, V1, V2, bavard_)
        implicit none
        REAL(kind=8),DIMENSION(:,:) :: v
        INTEGER,     DIMENSION(:)   :: sizes
        REAL(kind=8)                :: area,I1,I2
        REAL(kind=8),DIMENSION(2)   :: centre,V1,V2
        LOGICAL,OPTIONAL            :: bavard_

        LOGICAL                     :: bavard
        INTEGER                     :: i,ii,ip,nbp,nbpoly,ipoly,isum
        REAL(kind=8)                :: tmp,trace,delta
        REAL(kind=8)                :: a00,a10,a01,a20,a02,a11
                                  !1234567890123456789012345678901234567
        CHARACTER(len=37)  :: IAM = 'POLYGON::polygon_principal_properties'

        if ( present(bavard_) ) then
            bavard = bavard_
        else
            bavard = .false.
        endif

        a00 = 0.D0
        a10 = 0.D0
        a01 = 0.D0
        a20 = 0.D0
        a02 = 0.D0
        a11 = 0.D0

        nbpoly = size(sizes,dim=1)
        isum   = 0
        ! pour chaque polygone composant
        do ipoly = 1,nbpoly
            ! taille du polygone courant
            nbp = sizes(ipoly)
            ! calcul des moments normalises  aij
            do i = 1,nbp
                ! indice du vertex courant
                ii   = isum + i
                ! indice du vertex suivant
                ip   = isum + modulo(i,nbp) + 1
                ! calcul des moments
                tmp  = v(1,ii)*v(2,ip) - v(1,ip)*v(2,ii)
                a00  = a00 + tmp
                a10  = a10 + tmp * ( v(1,ii) + v(1,ip) )
                a01  = a01 + tmp * ( v(2,ii) + v(2,ip) )
                a20  = a20 + tmp * ( v(1,ii)**2 + v(1,ii)*v(1,ip) + v(1,ip)**2  )
                a02  = a02 + tmp * ( v(2,ii)**2 + v(2,ii)*v(2,ip) + v(2,ip)**2  )
                a11  = a11 + tmp * ( v(1,ii) *( 2.D0*v(2,ii) + v(2,ip) ) + v(1,ip)*( v(2,ii) + 2.D0*v(2,ip) )  )
            end do
            ! cumul les sizes des polygones pour avoir l'indice courant
            isum = isum + nbp
        enddo

        a00 = 0.5D0 * a00
        a10 = a10 /  6.D0 / a00
        a01 = a01 /  6.D0 / a00
        a20 = a20 / 12.D0 / a00
        a02 = a02 / 12.D0 / a00
        a11 = a11 / 24.D0 / a00

        area   = abs(a00)
        centre = (/ a10, a01 /)

        ! moments normalises centres
        a20 = a20 - a10*a10
        a02 = a02 - a01*a01
        a11 = a11 - a10*a01

        if ( bavard ) then
            print*,"a00, a01, a10 = ",a00,a01,a10
            print*,"a02, a20, a11 = ",a02,a20,a11
        endif

        ! la matrice d'inertie est definie par
        ! A = [  a02  -a11 ]
        !     [ -a11   a20 ]

        ! si la matrice est quasi diagonale
        if ( abs(a11)/abs(a02+a20) .lt. 1.D-10 ) then
            if ( a02 >= a20 ) then
                I1 = a02
                I2 = a20
                V1 = (/ 1.D0, 0.D0 /)
                V2 = (/ 0.D0, 1.D0 /)
            else
                I1 = a20
                I2 = a02
                V1 = (/ 0.D0, 1.D0 /)
                V2 = (/ 1.D0, 0.D0 /)
            endif

        else

            ! diagonalise la matrice d'inertie
            trace = a02 + a20
            delta = sqrt( (a02-a20)**2 + 4.D0*a11**2 )

            I1    = 0.5d0 * ( trace + delta )
            I2    = 0.5d0 * ( trace - delta )

            ! calcule les directions principales
            tmp = sqrt( 1.D0 + ((a02-I1)/a11)**2 )
            V1  = (/ 1.D0/tmp , (a02-I1)/a11/tmp /)

            tmp = sqrt( 1.D0 + ((a02-I2)/a11)**2 )
            V2  = (/ 1.D0/tmp , (a02-I2)/a11/tmp /)

            if ( bavard ) then
                print*,"trace, delta = ",trace,delta
                print*,"I1, I2 = ",I1,I2
                print*,"V1     = ",V1
                print*,"V2     = ",V2
            endif

        endif
        ! DIMENSIONne les inerties
        I1 = I1 * area
        I2 = I2 * area

    end subroutine polygon_principal_properties


    ! Calcule les proprietes geometriques d'un polygone 3D
    ! Input     v(3,n)    : coordinates of the vertices
    !           n         : normal
    ! Output    centre    : geometrical centre
    !           n,t,s     : principal coordinates system
    !           area      : area
    !           It, Is    : principal inertia
    subroutine polygon_3D_principal_properties(v, sizes, n, centre, s, t, area, Is, It, bavard_)
        implicit none

        REAL(kind=8),DIMENSION(:,:)             :: v
        INTEGER,     DIMENSION(:)               :: sizes
        REAL(kind=8)                            :: area,It,Is
        REAL(kind=8),DIMENSION(3)               :: centre,n,t,s
        LOGICAL,OPTIONAL                        :: bavard_

        LOGICAL                                 :: bavard
        INTEGER                                 :: i,nbp
        REAL(kind=8)                            :: I1,I2
        REAL(kind=8),DIMENSION(2)               :: C,V1,V2
        REAL(kind=8),DIMENSION(3)               :: bary,vec,tt,ss
        REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: pt

                                    !1234567890123456789012345678901234567890
        CHARACTER(len=40)  :: IAM = 'POLYGON::polygon_3D_principal_properties'

        if ( present(bavard_) ) then
            bavard = bavard_
        else
            bavard = .false.
        endif

        nbp = size(v,dim=2)

        ! construction d'un repere local temporaire
        bary = sum(v,dim=2)/nbp
        call comp_rep(tt,n,ss)
        if ( bavard ) then
            print*,"n = ",n
            print*,"t = ",tt
            print*,"s = ",ss
        endif

        ! projection en 2D sur le repere local
        allocate(pt(2,nbp))
        do i = 1,nbp
           vec(:)  = v(:,i) - bary(:)
           pt(1,i) = dot_product(vec, ss)
           pt(2,i) = dot_product(vec, tt)
        enddo

        ! calcul des proprietes principales dans le repere local
        call polygon_principal_properties(pt,sizes,area,C,I1,I2,V1,V2,bavard)

        ! re-projection des proprietes geometriques en 3D
        centre(:) =   C(1) * ss(:) +   C(2) * tt(:) + bary(:)
        s(:)      =  V1(1) * ss(:) +  V1(2) * tt(:)
        Is        = I1
        t         = cross_product(n,s)
        It        = I2

        deallocate(pt)

    end subroutine polygon_3D_principal_properties


    ! Calcule la distance d'un point P a un segment (V1,V2)
    ! Input     vP  : coordonnees du point P
    !           v1  : coordonnees du debut du segment
    !           v2  : coordonnees de la fin du segment
    function distance_node2edge(vP, v1, v2)
        implicit none
        REAL(kind=8)              :: distance_node2edge
        REAL(kind=8)              :: l,proj
        REAL(kind=8),DIMENSION(3) :: vP,v1,v2,LI,LP

        LI = v2 - v1
        LP = vP - v1
        l  = length3(LI)
        if ( l .lt. 1.D-10 ) THEN
            distance_node2edge = length3(LP)
        ELSE
            proj = DOT_PRODUCT(LI,LP) / l
            if ( proj < 0.D0 ) then
                distance_node2edge = length3(vP - v1)
            else if ( proj > l ) then
                distance_node2edge = length3(vP - v2)
            else
                distance_node2edge = length3(cross_product(LI,LP)) / l
            endif
        ENDIF

    end function distance_node2edge

    !> INPUT
    !>   points     : coords des points
    !> OUTPUT
    !>   bary       : barycentre des points
    !>   normal     : normale au joint
     subroutine compute_mean_plane(points, bary, normal)
       implicit none
       REAL(kind=8),DIMENSION(:,:)             :: points
       REAL(kind=8),DIMENSION(3)               :: bary,normal

       INTEGER                                 :: i,nbp
       REAL(kind=8)                            :: X2,Y2,Z2,XY,XZ,YZ
       REAL(kind=8),DIMENSION(3)               :: valp
       REAL(kind=8),DIMENSION(3,3)             :: mat,vecp
       REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: ptrel
       ! ***
                               !123456789012345678901234567
       character(len=27):: IAM='Polygon::compute_mean_plane'

    !~    print*,"in ",IAM

       nbp = size(points,dim=2)
       
       ! calcul du barycentre
       bary = SUM(points(:,1:nbp), DIM=2) / nbp

       allocate(ptrel(3,nbp))

       ! en coord relatives
       do i = 1,nbp
          ptrel(:,i) = points(:,i) - bary(:)
       enddo

       ! calcul du best plane
       X2 = SUM(ptrel(1,:)*ptrel(1,:))
       Y2 = SUM(ptrel(2,:)*ptrel(2,:))
       Z2 = SUM(ptrel(3,:)*ptrel(3,:))
       XY = SUM(ptrel(1,:)*ptrel(2,:))
       XZ = SUM(ptrel(1,:)*ptrel(3,:))
       YZ = SUM(ptrel(2,:)*ptrel(3,:))

       mat(1,1) = X2 ; mat(1,2) = XY ; mat(1,3) = XZ
       mat(2,1) = XY ; mat(2,2) = Y2 ; mat(2,3) = YZ
       mat(3,1) = XZ ; mat(3,2) = YZ ; mat(3,3) = Z2

       CALL diagonalise33(mat,valp,vecp)

       i = MINLOC(valp,DIM=1)
       normal(:) = vecp(:,i)

       deallocate(ptrel)

    !~    print*,"out ",IAM

     end subroutine compute_mean_plane


    ! Calcule les moments d'un polygone
    ! Input     v(2,n) : coordinates of the vertices
    !           sizes
    ! Output    Moments
    subroutine polygon_moments(v, sizes, a00, a10, a01, a20, a02, a11)
        implicit none

        REAL(kind=8),DIMENSION(:,:) :: v
        INTEGER,     DIMENSION(:)   :: sizes

        INTEGER                     :: i,ii,ip,nbp,isum,nbpoly,ipoly
        REAL(kind=8)                :: tmp,a00,a10,a01,a20,a02,a11

                                    !12345678901234567890123456789012345678
        CHARACTER(len=38)  :: IAM = 'POLYR::polygon_3D_principal_properties'

        nbp    = size(v,dim=2)
        nbpoly = size(sizes,dim=1)

        ! initialise les moments
        a00 = 0.D0
        a01 = 0.D0
        a10 = 0.D0
        a11 = 0.D0
        a02 = 0.D0
        a20 = 0.D0

        isum   = 0
        ! pour chaque polygone composant
        do ipoly = 1,nbpoly
            ! taille du polygone courant
            nbp = sizes(ipoly)
            ! calcul des moments normalises  aij
            do i = 1, nbp
                ! indice du vertex courant
                ii   = isum + i
                ! indice du vertex suivant
                ip   = isum + modulo(i,nbp) + 1
                ! calcul des moments
                tmp  = v(1,ii)*v(2,ip) - v(1,ip)*v(2,ii)
                a00  = a00 + tmp
                a10  = a10 + tmp * ( v(1,ii) + v(1,ip) )
                a01  = a01 + tmp * ( v(2,ii) + v(2,ip) )
                a11  = a11 + tmp * ( v(1,ii) *( 2.D0*v(2,ii) + v(2,ip) ) + v(1,ip)*( v(2,ii) + 2.D0*v(2,ip) )  )
                a20  = a20 + tmp * ( v(1,ii)**2 + v(1,ii)*v(1,ip) + v(1,ip)**2  )
                a02  = a02 + tmp * ( v(2,ii)**2 + v(2,ii)*v(2,ip) + v(2,ip)**2  )
            end do
            ! cumul les sizes des polygones pour avoir l'indice courant
            isum = isum + nbp
        enddo
        ! finalisation
        a00 = a00 /  2.D0
        a10 = a10 /  6.D0
        a01 = a01 /  6.D0
        a11 = a11 / 24.D0
        a20 = a20 / 12.D0
        a02 = a02 / 12.D0

    end subroutine polygon_moments


    SUBROUTINE envelope(vl, n, vertex, nvert)

    !  Find the vertices (in clockwise order) of a polygon enclosing
    !  the points (vl(:,i), i=1, ..., n.

    !  On output, vertex(i), i=1, ..., nvert contains the numbers of the vertices.

    !  There is a limit of 100 vertices imposed by the DIMENSION of array next.

    !  Programmer: Alan Miller
    !  Latest revision - 12 September 1987
    !  Fortran 90 version - 8 August 1996

        IMPLICIT NONE
        REAL(kind=8),DIMENSION(:,:) :: vl
        INTEGER                     :: n,nvert
        INTEGER     ,DIMENSION(:)   :: vertex

        !       Local variables
        INTEGER      :: iwk(100), next(100), i, i1, i2, j, jp1, jp2, i2save, i3, i2next
        REAL(kind=8) :: xmax, xmin, ymax, ymin, dist, dmax, dmin, x1, y1, dx, dy, x2, y2
        REAL(kind=8) :: dx1, dx2, dmax1, dmax2, dy1, dy2

        IF (n < 2) RETURN

        !  Choose the points with smallest & largest x- values as the
        !  first two vertices of the polygon.
        vertex = 0
        xmin   = MINVAL(vl(1,:))
        xmax   = MAXVAL(vl(1,:))

        ! Special case, xmax = xmin, (polygone degenere en un ligne)
        IF (xmax == xmin) THEN
          ymin   = MINVAL(vl(2,:))
          ymax   = MAXVAL(vl(2,:))

          ! tous les points confondus
          IF (ymax == ymin) THEN
             nvert = 1
          ELSE
             nvert     = 2
             vertex(1) = MINLOC(vl(2,:),DIM=1)
             vertex(2) = MAXLOC(vl(2,:),DIM=1)
          ENDIF

          RETURN

        ELSE
          vertex(1) = MINLOC(vl(1,:),DIM=1)
          vertex(2) = MAXLOC(vl(1,:),DIM=1)

        ENDIF

        !  Set up two initial lists of points; those points above & those below the
        !  line joining the first two vertices.    next(i) will hold the POINTER to the
        !  point furthest from the line joining vertex(i) to vertex(i+1) on the left
        !  hand side.

        i1      = vertex(1)
        i2      = vertex(2)
        iwk(i1) = -1
        iwk(i2) = -1
        dx      = xmax - xmin
        y1      = vl(2,i1)
        dy      = vl(2,i2) - y1
        dmax    = 0.D0
        dmin    = 0.D0
        next(1) = -1
        next(2) = -1

        DO i = 1, n
          IF (i == vertex(1) .OR. i == vertex(2)) CYCLE
          dist = (vl(2,i) - y1)*dx - (vl(1,i) - xmin)*dy
          IF (dist > 0.D0) THEN
            iwk(i1) = i
            i1      = i
            IF (dist > dmax) THEN
              next(1) = i
              dmax    = dist
            END IF
          ELSE IF (dist < 0.D0) THEN
            iwk(i2) = i
            i2      = i
            IF (dist < dmin) THEN
              next(2) = i
              dmin    = dist
            END IF
          END IF
        END DO

        !  Ends of lists are indicated by POINTERs to -ve positions.
        iwk(i1) = -1
        iwk(i2) = -1
        nvert   = 2

        j = 1

        !  Start of main process.

        !  Introduce new vertex between vertices j & j+1, if one has been found.
        !  Otherwise increase j.   Exit if no more vertices.

        DO

            DO WHILE ( next(j) < 0 )
                IF (j == nvert) RETURN
                j = j + 1
            END DO

            jp1 = j + 1
            DO i = nvert, jp1, -1
              vertex(i+1) = vertex(i)
              next(i+1)   = next(i)
            END DO
            jp2   = jp1 + 1
            nvert = nvert + 1
            IF (jp2 > nvert) jp2 = 1
            i1          = vertex(j)
            i2          = next(j)
            i3          = vertex(jp2)
            vertex(jp1) = i2

            !  Process the list of points associated with vertex j.   New list at vertex j
            !  consists of those points to the left of the line joining it to the new
            !  vertex (j+1).   Similarly for the list at the new vertex.
            !  Points on or to the right of these lines are dropped.

            x1        = vl(1,i1)
            x2        = vl(1,i2)
            y1        = vl(2,i1)
            y2        = vl(2,i2)
            dx1       = x2 - x1
            dx2       = vl(1,i3) - x2
            dy1       = y2 - y1
            dy2       = vl(2,i3) - y2
            dmax1     = 0.D0
            dmax2     = 0.D0
            next(j)   = -1
            next(jp1) = -1
            i2save    = i2
            i2next    = iwk(i2)
            i         = iwk(i1)
            iwk(i1)   = -1
            iwk(i2)   = -1

            DO WHILE ( i > 0 )
                IF (i /= i2save) THEN
                  dist = (vl(2,i) - y1)*dx1 - (vl(1,i) - x1)*dy1
                  IF (dist > 0.D0) THEN
                    iwk(i1) = i
                    i1      = i
                    IF (dist > dmax1) THEN
                      next(j) = i
                      dmax1   = dist
                    END IF
                  ELSE
                    dist = (vl(2,i) - y2)*dx2 - (vl(1,i) - x2)*dy2
                    IF (dist > 0.D0) THEN
                      iwk(i2) = i
                      i2      = i
                      IF (dist > dmax2) THEN
                        next(jp1) = i
                        dmax2     = dist
                      END IF
                    END IF
                  END IF
                  i = iwk(i)
                ELSE
                  i = i2next
                END IF
            !  Get next point from old list at vertex j.
            ENDDO

            !  End lists with -ve values.

            iwk(i1) = -1
            iwk(i2) = -1

        ENDDO

    END SUBROUTINE envelope

    subroutine polygons_intersection_wc(p1, s1, p2, s2, shrink1, shrink2, delta, min_v, min_a, p3, s3, area)
      implicit none
      !> first array of vertices of shape (2,1:m1) (stored anti-clockwise)
      real(kind=8), dimension(:,:), pointer :: p1
      !> second array of vertices of shape (2,1:m2) (stored anti-clockwise)
      real(kind=8), dimension(:,:), pointer :: p2
      !> number of vertices of each polytope in p1
      integer     , dimension(:)  , pointer :: s1
      !> number of vertices of each polytope in p2
      integer     , dimension(:)  , pointer :: s2
      !> the shrink value to use in clipper on first polygon
      real(kind=8), intent(in) :: shrink1
      !> the shrink value to use in clipper on second polygon
      real(kind=8), intent(in) :: shrink2
      !> the delta value to use in clipper (use to simplify polygons)
      real(kind=8), intent(in) :: delta
      !> the minimum number of vertices that must be found to keep result
      integer     , intent(in) :: min_v
      !> the minimum area of contact that must be found to keep result
      real(kind=8), intent(in) :: min_a
      !> the coordinates of the intersection points
      real(kind=8), dimension(:,:), pointer :: p3
      !> number of intersection points for each intersection polytopes
      integer, dimension(:), pointer :: s3
      !> the area of each polytopes
      real(kind=8), dimension(:), pointer :: area
      !
      integer :: nb_points, nb_polys, i_poly, idx
      integer :: i_p3beg, i_p3end, i_cpbeg, i_cpend
      integer     , dimension(:)  , pointer :: csizes
      real(kind=8), dimension(:,:), pointer :: cpoints
      real(kind=8), dimension(:)  , pointer :: careas

      cpoints => null()
      csizes  => null()
      careas  => null()

      ! paranoid
      if( associated(p3)   ) deallocate(p3)
      if( associated(s3)   ) deallocate(s3)
      if( associated(area) ) deallocate(area)
      nullify(p3)
      nullify(s3)
      nullify(area)

      call polygones_intersection(p1, s1, p2, s2, shrink1, shrink2, delta, cpoints, csizes, careas)

      ! nothing found
      if( .not. associated(csizes) ) then
        return
      end if

      !  counting polytopes excluding according to min_v and min_a
      nb_polys  = 0
      nb_points = 0
      do i_poly = 1, size(csizes)
        if( csizes(i_poly) < min_v .or.  careas(i_poly) < min_a ) cycle
        nb_polys  = nb_polys  + 1
        nb_points = nb_points + csizes(i_poly)
      end do

      if ( nb_points > 0 .and. nb_polys > 0 ) then
          allocate( p3(2,nb_points) )
          allocate( s3(nb_polys)    )
          allocate( area(nb_polys)  )
          i_poly  = 1
          i_p3beg = 1
          i_cpbeg = 1
          
          do idx = 1, size(csizes)
            if( csizes(idx) < min_v .or. careas(idx) < min_a ) then
              i_p3beg = i_p3beg + csizes(idx)
              cycle
            end if

            area(i_poly) = careas(idx)
            s3(i_poly) = csizes(idx)
            i_poly = i_poly+1
            i_p3end  = i_p3beg + csizes(idx)-1
            i_cpend  = i_cpbeg + csizes(idx)-1
            p3(1:2,i_p3beg:i_p3end) = cpoints(1:2,i_cpbeg:i_cpend)
            i_p3beg  = i_p3end+1
            i_cpbeg  = i_cpend+1
          end do
      endif

      call clipper_free(cpoints)
      call clipper_free(csizes)
      call clipper_free(careas)

    end subroutine polygons_intersection_wc

    subroutine polygon_simplification_wc(pin, eps, pout)
      implicit none
      real(kind=8), dimension(:,:), pointer :: pin
      real(kind=8)                          :: eps
      real(kind=8), dimension(:,:), pointer :: pout
      real(kind=8), dimension(:,:), pointer :: cout

      !paranoid
      if( associated(pout) ) deallocate(pout)

      nullify(pout)
      nullify(cout)

      call polygone_simplification(pin, eps, cout)

      if( associated(cout) ) then
        allocate( pout( size(cout,1), size(cout,2) ) )
        pout(:,:) = cout(:,:)
        call clipper_free(cout)
      end if

    end subroutine polygon_simplification_wc

    integer function polygon_one_point_in_wc(point, polyg, sizes)
      implicit none
      real(kind=8), dimension(:)  , pointer :: point
      real(kind=8), dimension(:,:), pointer :: polyg
      integer     , dimension(:)  , pointer :: sizes

      polygon_one_point_in_wc = polygone_pointinpolygon(point, polyg, sizes)

    end function polygon_one_point_in_wc

    logical function polygon_all_points_in_wc(points, polyg, s)
      implicit none
      real(kind=8), dimension(:,:), pointer :: points
      real(kind=8), dimension(:,:), pointer :: polyg
      integer     , dimension(:)  , pointer :: s
      !
      integer :: i_point, is_in

      polygon_all_points_in_wc = .true.

      do i_point = 1, size(points,2)
        is_in = polygone_pointinpolygon(points(:,i_point), polyg, s)
        if( is_in < 0 ) then
          polygon_all_points_in_wc = .false.
          exit
        end if
      end do

    end function polygon_all_points_in_wc

end module polygon
