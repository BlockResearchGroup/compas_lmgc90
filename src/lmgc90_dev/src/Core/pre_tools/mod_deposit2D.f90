!=======================================================================
! bounding_box(2) ne sert a rien
! 
!


! module de construction d'un depot dans une boite  
!> Module dedicated to the deposit of particles in a semi-inifinite box
module deposit2D

  use overall, only : PI_g

  IMPLICIT NONE

  PRIVATE

  ! fenetre de travail
  REAL(KIND=8),DIMENSION(2)                 :: bounding_box 

  ! nb grains qu'on souhaite deposer
  INTEGER                                   :: nb_GRAINS = 0

  ! coordonnees des objets deposes               
  REAL(kind=8),ALLOCATABLE,DIMENSION(:,:)   :: coor         


  ! GRANULO des grains
  REAL(kind=8), allocatable, DIMENSION(:) :: radii  ! rayons d'encombrement
  REAL(kind=8)               :: radius_min,radius_max

  !
  !--------------------------------------------------------------------------

  INTEGER      :: nch,nchf

  INTEGER                                   :: nb_BODIES


  !- particle chains and shadows
  INTEGER,PARAMETER                         :: nchain = 20000                      ! taille de la chaine, tester le depassement ...
  INTEGER,DIMENSION(0:nchain)               :: lch                                 ! description du front
  INTEGER,DIMENSION(nchain)                 :: ncf                                 ! 
  
  ! constantes trigonometriques :

  real(kind=8), parameter :: pi2  = 0.5d0 * PI_g ! pi/2
  real(kind=8), parameter :: pit2 = 2.d0 * PI_g  ! 2*pi

 !- particle size, granulometry
  REAL(kind=8), parameter :: epsa = 0.1D-11 ! delta for comparison of angle 
     ! intervals

 !- Geometrical construction of disk pile
  real(kind=8), parameter :: c1 = 1.d0
  real(kind=8), parameter :: c2 = 1.d-3
  real(kind=8), parameter :: c3 = 1.d0
  real(kind=8), parameter :: chp1 = 0.1d0
  real(kind=8), parameter :: chp2 = 1.0d0
  
  REAL(kind=8),DIMENSION(nchain)            :: anca,ancs                           !
  
  !*
  !*************************
  !* HETEROGENEOUS "BIG" PARTICLES
  !* nhmax: maximum hp
  !* radius and position
  !* ndhp: nb of deposited hp 
  !* inhp: flag indicating if hp is deposited o!r not 
  !* nihp: nb of hp intersecting the shadow
  !* ndih: position of hp in the sadow chain
  !* nhid: nb of hp intersection with disk j (f!d say ?)
  !* ahid : angles

  INTEGER                                   :: nb_BIG_BODIES
  !*
  INTEGER,PARAMETER                         :: nhmax  = 100                        ! nb particules hetero
  !*
  REAL(kind=8),DIMENSION(nhmax)             :: het_radius
  REAL(kind=8),DIMENSION(2,nhmax)           :: het_coor
  !*
  INTEGER                                   :: ndhp,nihp
  INTEGER,DIMENSION(nhmax)                  :: inhp,ndih,nhid                   
  real(kind=8), dimension(nhmax) :: ahid 


  !****************
  ! default == gravity
  INTEGER      :: ID_potential=1  

  PUBLIC :: &
       new_deposit, &
       DEPOT, &
       get_coor

  CONTAINS

!!!-----------------------------------------------------------------------------
  !> Compute the deposit.
  !> \warning A new deposit must have been initialisated.
  SUBROUTINE DEPOT()

    IMPLICIT NONE

    ! variables locales
    integer :: ip ! indice de boucle sur les particules a deposer
    
!!! initial conditions for the chain array are specified

    nch= 1                    ! initial number of elements in the chain
!    lch(0) = 2                ! element 2 = left vertical wall   <---- ca devrait etre 3 !!
    lch(0) = 3
    lch(1) = 1                ! element 1 = horizontal base
    lch(2) = 4                ! element 4 = right vertical wall

    IF (nb_big_bodies.GT.0) THEN
       ndhp =0                        ! number of deposited heterogeneous particles
       inhp(1:nb_big_bodies)= 0       ! flag indicating if hp is deposited or not
    ENDIF

    !!! the constants used for potential function are defined

    DO ip = 5, nb_bodies
         
       !   coordinates of disk 'ip' are calculated
       
       CALL shadow(ip)
       CALL shadhet(ip)
       CALL locate(ip)

    END DO

    IF (nb_big_bodies.GT.0 .and. ndhp.LT.nb_big_bodies) THEN
      PRINT *,'deposit2D::DEPOT: FATAL ERROR:'
      WRITE(*,*) ' Number of deposited heterogeneous particles = ',ndhp
      WRITE(*,*) ' is less than ',nb_big_bodies
      stop
    END IF

  END SUBROUTINE DEPOT
!!!=======================================================================
!!!  SR shadow()  
!!!=======================================================================
  !> Calculate the chain of particles and line segments above which the 
  !> new particle ought to be located (deposited).
  SUBROUTINE shadow(ip)

  IMPLICIT NONE

  ! variables d'entree :
  integer, intent(in) :: ip !< index of a given particle
     ! numero de la particule a deposer courante

  ! local variables
  integer :: i, j ! indices de boucle
  integer :: k ! variable auxiliaire (am: role a definir...)
  integer :: ind1, ind2
  integer :: nps0, nps1, nps2
  integer :: nchc, nchmin, nchmax
  real(kind=8) :: x1,y1,r1,x2,y2,r2
  real(kind=8) :: v1x,v1y,v1,v2x,v2y,v2,v1sv2,v1vv2,v1v2
  real(kind=8) :: va,vax,vay
  real(kind=8) :: xvsh,xint,ximin,ximax
  real(kind=8) :: ymaxw,rsh,dx,dy
  real(kind=8) :: a,a12,a2,ab,ac
  real(kind=8) :: sa,sb,sc
  real(kind=8) :: alfa,alfaa,alfas,alfa1,alfa2,beta,gama,teta

! 3, 4 = vertical left and right walls bounding the box            
  lch(nch+1)=4

  nps1=lch(1)

  IF (nps1.EQ.1) THEN
    ! first element in the chain is a horizontal line segment
    anca(1)=radii(ip)
  ELSE
    ! first element in the chain is a circle
    anca(1)=dacos((radii(ip)-radii(nps1))/(radii(ip)+radii(nps1)))
  END IF

  nps1=lch(nch)

  IF (nps1.EQ.1) THEN
    ! last element in the chain is a horizontal line segment
    ancs(nch)=bounding_box(1)-radii(ip)
  ELSE
    ! last element in the chain is a circle
    ancs(nch)=dacos((radii(nps1)-radii(ip))/(radii(nps1)+radii(ip)))
  END IF

  a=0.D0
  v2x=1.D0
  v2y=0.D0
  v2=1.D0

  DO i=1,nch-1
    ! nps0,nps1,nps2= number of particles in the chain to be analized
    nps0=lch(i-1)
    nps1=lch(i)
    nps2=lch(i+1)
    IF (nps1.EQ.1) THEN
      ! the element 'i' is a horizontal line segment
      x2=coor(1,nps2)
      y2=coor(2,nps2)
      r2=radii(nps2)
      beta=dacos( (radii(ip)-r2) / (radii(ip)+r2) )
      ancs(i)=x2-(r2+radii(ip))*dsin(beta)
      anca(i+1)=pi2+beta
      v2x=0.D0
      v2y=1.D0
      v2=1.D0
      a=pi2
    ENDIF
    IF (nps2.EQ.1) THEN
      ! the element 'i+1' is a horizontal line segment
      x1=coor(1,nps1)
      y1=coor(2,nps1)
      r1=radii(nps1)
      beta=dacos( (radii(ip)-r1) / (radii(ip)+r1) )
      ancs(i)=pi2-beta 
      anca(i+1)=x1+(r1+radii(ip))*dsin(beta) 
    ENDIF
    IF ( (nps1.NE.1).AND.(nps2.NE.1) ) THEN
      ! the elements 'i' and 'i+1' are both circles (general case) 
      x1=coor(1,nps1)
      y1=coor(2,nps1)
      r1=radii(nps1)
      x2=coor(1,nps2)
      y2=coor(2,nps2)
      r2=radii(nps2)
      v1x=v2x
      v1y=v2y
      v1=v2
      v2x=x2-x1
      v2y=y2-y1
      v2=r1+r2
      sa=v2
      sb=r1+radii(ip)
      sc=r2+radii(ip)
      ab=dacos((sa*sa+sc*sc-sb*sb)/(2.D0*sa*sc))
      ac=dacos((sa*sa+sb*sb-sc*sc)/(2.D0*sa*sb))
 
      IF (nps0.EQ.nps2) THEN
        a12=-PI_g
      ELSE
        v1sv2=v1x*v2x+v1y*v2y
        v1vv2=v1x*v2y-v2x*v1y
        v1v2=v1*v2
        CALL arcos(v1sv2,v1vv2,v1v2,a12)
      ENDIF

      a2=a+a12
      ancs(i)=a2+ac
 
      IF (lch(i+2).EQ.1) THEN
        v2x=-v2x
        v2y=-v2y
        CALL arcos(v2x,v2y,v2,a)
        IF (a.LT.-pi2) a=a+pit2
        anca(i+1)=a-ab
      ENDIF
 
      IF (lch(i+2).EQ.4) THEN
        v2x=-v2x
        v2y=-v2y
        CALL arcos(v2x,v2y,v2,a)
        IF (a.LT.0.D0) a=a+pit2
        anca(i+1)=a-ab
      ENDIF
 
      IF (lch(i+2).GE.5) THEN
        CALL arcos(v2x,v2y,v2,a)
        IF (a.LT.-pi2) a=a+pit2
        anca(i+1)=a+PI_g-ab
      ENDIF

    ENDIF
  ENDDO  

  DO i=1,nch
    IF (lch(i).GE.5) THEN
      IF (ancs(i).LT.0.D0) THEN
        ancs(i)=ancs(i)+pit2
        anca(i)=anca(i)+pit2
      ENDIF
      IF (ancs(i).GE.pit2) THEN
        ancs(i)=ancs(i)-pit2
        anca(i)=anca(i)-pit2
      ENDIF  
    ENDIF
  ENDDO

! Find out which element defines the uppermost intersection with left wall

  ymaxw=0.D0

  DO i=1,nch

    nps1=lch(i)
    IF (nps1.EQ.1) THEN             ! horizontal line segment
         
      IF (anca(i).LE.ancs(i)) THEN   ! the interval is positive (exists)
 
        IF (i.EQ.1) THEN              ! analysis of first element in the chain
          ymaxw=radii(ip)                 ! initialisation of intersection value
          nchmin=1
        ELSE
          IF ( (anca(i).LE.radii(ip)) .AND. (radii(ip).LE.ancs(i)) ) THEN
            IF (radii(ip).GE.ymaxw) THEN
              ymaxw=radii(ip)
              nchmin=i
              anca(i)=radii(ip)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ELSE                            ! circle element
      IF (ancs(i).LE.anca(i)) THEN   ! the angular interval is positive
        x1=coor(1,nps1)
        y1=coor(2,nps1)
        r1=radii(nps1)
        rsh=r1+radii(ip)
        IF (x1-rsh.LT.radii(ip)) THEN
          dx=radii(ip)-x1
          dy=dsqrt(rsh*rsh-dx*dx)
          alfa=dacos(dx/rsh)
          IF (alfa.GE.ancs(i)) THEN
            IF ((i.EQ.1).OR.(alfa.LE.anca(i))) THEN
              IF (y1+dy.GE.ymaxw) THEN
                ymaxw=y1+dy
                nchmin=i
                anca(i)=alfa
              ENDIF
            ENDIF
          ELSE
            IF ((i.EQ.1).OR.(alfa+pit2.LE.anca(i))) THEN
              IF (y1+dy.GE.ymaxw) THEN
                ymaxw=y1+dy
                nchmin=i
                anca(i)=alfa+pit2
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF 
  ENDDO 

  ! Find out which element defines the uppermost intersection with right wall

  ymaxw=0.D0
  xvsh=bounding_box(1)-radii(ip)                  ! bounding_box(1) = variable global

  DO i=nch,1,-1

    nps1=lch(i)
    IF (nps1.EQ.1) THEN             ! horizontal line segment
       
      IF (anca(i).LE.ancs(i)) THEN   ! the interval is positive (exists)
        IF (i.EQ.nch) THEN            ! analysis of last element in the chain
          ymaxw=radii(ip)               
          nchmax=nch
        ELSE
          IF ( (anca(i).LE.xvsh) .AND. (xvsh.LE.ancs(i)) ) THEN
            IF (radii(ip).GE.ymaxw) THEN
              ymaxw=radii(ip)
              nchmax=i
              ancs(i)=xvsh
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ELSE                            ! circle element
      IF (ancs(i).LE.anca(i)) THEN   ! the angular interval is positive
        x1=coor(1,nps1)
        y1=coor(2,nps1)
        r1=radii(nps1)
        rsh=r1+radii(ip)
        IF (x1+rsh.GT.xvsh) THEN
          dx=xvsh-x1
          dy=dsqrt(rsh*rsh-dx*dx)
          alfa=dacos(dx/rsh)
          IF ((i.EQ.nch).OR.(alfa.GE.ancs(i))) THEN
            IF (alfa.LE.anca(i)) THEN
              IF (y1+dy.GT.ymaxw) THEN
                ymaxw=y1+dy
                nchmax=i
                ancs(i)=alfa
              ENDIF
            ENDIF
          ELSE
            IF ((i.EQ.nch).OR.(alfa+pit2.LE.anca(i))) THEN
              IF (y1+dy.GE.ymaxw) THEN
                ymaxw=y1+dy
                nchmax=i
                ancs(i)=alfa+pit2
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF 
  ENDDO 

  ! compress the list of elements in the chain, selecting those with 
  ! positive intervals

  nchc=0
  DO i=nchmin,nchmax
    nps1=lch(i)
    IF (nps1.EQ.1) THEN
      IF (anca(i).LT.ancs(i)) THEN
        nchc=nchc+1
        anca(nchc)=anca(i)
        ancs(nchc)=ancs(i)
        ncf(nchc)=i
      ENDIF
    ELSE
      IF (anca(i).GT.ancs(i)) THEN
        nchc=nchc+1
        anca(nchc)=anca(i)
        ancs(nchc)=ancs(i)
        ncf(nchc)=i
      ENDIF
    ENDIF
  ENDDO

  ! calculate the final list of elements which defines the shadow
  ! where the new particle ought to be located.

  i=1
  nchf=1

10 IF (i.GE.nchc) GOTO 20     ! while structure
     nps1=lch(ncf(i))
     IF (nps1.NE.1) THEN
       x1=coor(1,nps1)
       y1=coor(2,nps1)
       r1=radii(nps1)+radii(ip)
       k=i+1
       alfaa=anca(i)
       alfas=ancs(i)
       DO j=i+1,nchc
         nps2=lch(ncf(j))
         IF (nps2.NE.1) THEN
           IF (nps1.NE.nps2) THEN
             x2=coor(1,nps2)
             y2=coor(2,nps2)
             r2=radii(nps2)+radii(ip)
             vax=x2-x1
             vay=y2-y1
             va=dsqrt(vax*vax+vay*vay)
             IF (va.LE.r1+r2) THEN
               teta=dacos(vax/va)
               IF (vay.LT.0.D0) teta=-teta
               gama=dacos((va*va+r1*r1-r2*r2)/(2.D0*va*r1))
               beta=dacos((va*va+r2*r2-r1*r1)/(2.D0*va*r2))
               ind1=0
               ind2=0
               alfa1=teta+gama
               alfa2=teta+PI_g-beta
               IF (alfa1.LT.0.D0) alfa1=alfa1+pit2
               IF (alfa2.LT.0.D0) alfa2=alfa2+pit2
               IF (alfa1.GE.alfas) THEN
                 IF (alfa1.LE.alfaa) ind1=1 
               ELSE 
                 IF (alfa1+pit2.LE.alfaa) THEN
                   alfa1=alfa1+pit2
                   ind1=1
                 ENDIF
               ENDIF
               IF (alfa2.GE.ancs(j)) THEN
                 IF (alfa2.LE.anca(j)+epsa) ind2=1
               ELSE
                 IF (alfa2+pit2.LE.anca(j)+epsa) THEN
                   alfa2=alfa2+pit2
                   ind2=1
                 ENDIF
               ENDIF
               IF ((ind1.EQ.1).AND.(ind2.EQ.1)) THEN
                 k=j
                 IF (alfa1.GT.pit2) THEN
                   alfas=alfa1-pit2
                   alfaa=alfaa-pit2
                   anca(nchf)=alfaa
                 ELSE
                 alfas=alfa1
               ENDIF
               anca(j)=alfa2
             ENDIF
           ENDIF
         ENDIF
       ELSE
         ximin=anca(j)
         ximax=ancs(j)
         IF ((x1.LT.ximax).AND.(x1+r1.GE.ximin)) THEN
           IF (y1-r1.LE.radii(ip)) THEN
             alfa=dasin((radii(ip)-y1)/r1)
             IF (alfa.LT.0.D0) alfa=alfa+pit2
             xint=x1+r1*dcos(alfa)
             IF ((xint.GE.ximin).AND.(xint.LE.ximax)) THEN
               IF (alfa.GE.alfas) THEN
                 IF (alfa.LE.alfaa) THEN
                   alfas=alfa
                   anca(j)=xint
                   k=j
                 ENDIF
               ELSE
                 IF (alfa+pit2.LE.alfaa) THEN
                   alfas=alfa
                   alfaa=alfaa-pit2
                   anca(nchf)=alfaa
                   anca(j)=xint
                   k=j
                 ENDIF
               ENDIF
             ENDIF
           ENDIF
         ENDIF
       ENDIF
     ENDDO
     i=k
     nchf=nchf+1
     ncf(nchf)=ncf(k)
     anca(nchf)=anca(k)
     ancs(nchf-1)=alfas
   ELSE
     ximin=anca(i)
     ximax=ancs(i)
     k=i+1
     DO j=i+1,nchc
       nps2=lch(ncf(j))
       IF (nps2.NE.1) THEN
         x2=coor(1,nps2)
         y2=coor(2,nps2)
         r2=radii(nps2)+radii(ip)
         IF ((x2-r2.LT.ximax).AND.(x2.GT.ximin)) THEN
           IF (y2-r2.LT.radii(ip)) THEN
             alfa=PI_g-dasin((radii(ip)-y2)/r2)
             xint=x2+r2*dcos(alfa)
             IF ((xint.GE.ximin).AND.(xint.LE.ximax)) THEN

               IF (alfa.GE.ancs(j)) THEN
                 IF (alfa.LE.anca(j)) THEN
                   ximax=xint
                   anca(j)=alfa
                   k=j
                 ENDIF
               ELSE 
                 IF (alfa+pit2.LE.anca(j)) THEN
                   alfa=alfa+pit2
                   ximax=xint
                   anca(j)=alfa
                   k=j
                 ENDIF
               ENDIF   
             ENDIF
           ENDIF
         ENDIF
       ENDIF
     ENDDO
     nchf=nchf+1
     i=k
     ncf(nchf)=ncf(k)
     anca(nchf)=anca(k)
     ancs(nchf-1)=ximax
   ENDIF

   GOTO 10                    ! end of while structure
20 CONTINUE

   ancs(nchf)=ancs(nchc)

   RETURN

 END SUBROUTINE shadow
!=================================================
!     SR shadhet()  
   !> Calculate the intersection between the shadow of the chain 
   !> of particles and the heterogeneous "big" particles that are
   !> located at known positions.
!=================================================
 SUBROUTINE shadhet(ip)

  IMPLICIT NONE
  
  ! variables d'entree :
  integer, intent(in) :: ip !< index of a given particle
     ! numero de la particule a deposer courante

  ! variables locales :    
  integer :: i, j, k ! indices de boucle
  integer :: nps1,indi
  REAL(kind=8) :: x1,y1,r1,xh,yh,rh,v1hx,v1hy,v1h,xint,ximin,ximax
  REAL(kind=8) :: alfa,alfa1,alfa2,gama,beta,teta,alfaa,alfas
 
  !* adih : angles
  real(kind=8), dimension(nhmax) :: adih ! am : ATTENTION : tableau
     ! surdimensionne a ne pas oublier, si on bascule sur une solution plus 
     ! propre...

! 'het', 'hp' = heterogeneous "big" particles
! number of intersections between the shadow and an hps
  nihp=0
  IF ((nb_big_bodies.GT.0).AND.(ndhp.LT.nb_big_bodies)) THEN 
    DO i=1,nb_big_bodies
      ! number of heterogeneous "big" particles
      IF (inhp(i).EQ.0) THEN    ! flag for hp i
        xh=het_coor(1,i)
        yh=het_coor(2,i)
        rh=het_radius(i)+radii(ip)
        DO j=1,nchf
         ! number of particles in the final clipped shadow
          nps1=lch(ncf(j))
          IF (nps1.GT.4) THEN   ! intersection between disk shadow and hp
            r1=radii(nps1)+radii(ip)
            y1=coor(2,nps1)
            v1hy=yh-y1
            IF (dabs(v1hy).LT.r1+rh) THEN
              x1=coor(1,nps1)
              v1hx=xh-x1
              IF (dabs(v1hx).LT.r1+rh) THEN
                v1h=dsqrt(v1hx*v1hx+v1hy*v1hy)
                IF (v1h.LE.r1+rh) THEN
                  alfaa=anca(j)
                  alfas=ancs(j)
                  teta=dacos(v1hx/v1h)
                  IF (v1hy.LT.0.d0) teta=-teta
                    gama=dacos((v1h*v1h+r1*r1-rh*rh)/(2.d0*v1h*r1))
                    alfa1=teta+gama
                    beta=dacos((v1h*v1h+rh*rh-r1*r1)/(2.D0*v1h*rh))
                    alfa2=teta+PI_g-beta
                    DO k=1,2 !two posible intersections between disk shadow and hp
                      indi=0
                      IF (k.EQ.2) alfa1=teta-gama
                      IF (alfa1.LT.0.d0) alfa1=alfa1+pit2
                      IF (alfa1.GE.alfas) THEN
                        IF (alfa1.LE.alfaa) indi=1
                      ELSE
                        IF (alfa1+pit2.LE.alfaa) THEN
                          alfa1=alfa1+pit2
                          indi=1
                        ENDIF
                      ENDIF
                      IF (indi.EQ.1) THEN
                        IF (k.EQ.2) alfa2=teta+PI_g+beta
                        IF (alfa2.LT.0.D0) alfa2=alfa2+pit2
                        IF (alfa2.GE.pit2) alfa2=alfa2-pit2

                        nihp=nihp+1        !# of intersections between shadow and hp
                        ndih(nihp)=j       !position of the disk in the chain 
                        nhid(nihp)=i       !# of hp intersecting with disk j
                        adih(nihp)=alfa1   !angle of intersection related to disk
                        ahid(nihp)=alfa2   !angle of intersection related to hp
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
            ELSE                           !intersection between the base line shadow and hp
              IF (nps1.EQ.1) THEN
                ximin=anca(j)
                ximax=ancs(j)
                IF (yh-rh.LE.radii(ip)) THEN
                  IF ((xh+rh.GE.ximin).AND.(xh-rh.LE.ximax)) THEN
                    gama=dacos((radii(ip)-yh)/rh)
                    alfa=pi2+gama
                    DO k=1,2   !two posible intersections between disk shadow and hp
                      indi=0
                      IF (k.EQ.2) alfa=pi2-gama
                      xint=xh+rh*dcos(alfa)
                      IF ((xint.GE.ximin).AND.(xint.LE.ximax)) THEN
                        nihp=nihp+1
                        ndih(nihp)=j
                        nhid(nihp)=i
                        adih(nihp)=xint
                        ahid(nihp)=alfa
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF

 END SUBROUTINE shadhet
!======================================================================
!     SR locate()  
   !> Calculate the coordinates of a particle according to a specified
   !> criteria.
!  Option 1: Criteria for location (sedimentation) of particles
!     gravityFLAG,wallFLAG,heteroFLAG=1  : 
   !> Particle is located in the position where potential energy 
   !> is minimun (minimize y coord. in the shadow). It is centred
   !> along horizontal line segments.
!======================================================================
 SUBROUTINE locate(ip)

  IMPLICIT NONE
      
  ! variables d'entree :
  integer, intent(in) :: ip !< index of a given particle
     ! numero de la particule a deposer courante

  ! variables locales :
  integer :: i ! indice de boucle
  integer :: nps1,nmin,inds,imin,norchi,norchf,ntrans
  REAL(kind=8):: xminc,yminc,r1,x1,y1,pmin,pot,alfa

  IF ((nb_big_bodies.GT.0).AND.(nihp.GT.0)) THEN      
    ! the shadows between the final chain and hp do intersect
    DO i=1,nihp
      alfa=ahid(i)
      nps1=nhid(i)
      r1=het_radius(nps1)+radii(ip)
      x1=het_coor(1,nps1)+r1*dcos(alfa)
      y1=het_coor(2,nps1)+r1*dsin(alfa)

      CALL potential(x1,y1,pot)

      IF (i.EQ.1) THEN
        imin=1
        pmin=pot
      ELSE
        IF (pot.LT.pmin) THEN
          pmin=pot
          imin=i
        ENDIF
      ENDIF
    ENDDO

    alfa=ahid(imin)
    nps1=nhid(imin)
    r1=het_radius(nps1)+radii(ip)
    coor(1,ip)=het_coor(1,nps1)+r1*dcos(alfa)
    coor(2,ip)=het_coor(2,nps1)+r1*dsin(alfa)

    ndhp=ndhp+1
    inhp(nhid(imin))=1
    norchi=ncf(ndih(imin))
    norchf=norchi+4
    ntrans=4

    DO i=nch+1,norchi,-1
      lch(i+ntrans)=lch(i)
    ENDDO

    lch(norchi+1)=ip
    lch(norchi+2)=nb_bodies+ndhp
    lch(norchi+3)=ip
    nch=nch+4
    i=nhid(imin)   ! i = number of the hp corresponding to minimum

    coor(1,nb_bodies+ndhp)=het_coor(1,i)
    coor(2,nb_bodies+ndhp)=het_coor(2,i)
    radii(nb_bodies+ndhp)=het_radius(i)

  ELSE   ! the disk is located in the minima of the final chain shadow
         ! the shadows between the final chain and hp do not intersect

    ncf(nchf+1)=nch+1     ! last element in the final chain = right wall
    nps1=lch(ncf(1))
    nmin=ncf(1)
    inds=0

    IF ( id_potential == 1 ) THEN   
!   potential energy function depends only on 'y' position

      IF (nps1.EQ.1) THEN
!       new particle is centred along horizontal line segment
        xminc=(anca(1)+ancs(1))/2.D0
        yminc=radii(ip)
      ELSE
        r1=radii(nps1)+radii(ip)
        xminc=coor(1,nps1)+r1*dcos(anca(1))
        yminc=coor(2,nps1)+r1*dsin(anca(1))
        i=1

 10     IF (i.GT.nchf) GOTO 20   ! while structure between 10,20

          nps1=lch(ncf(i))
          IF (nps1.EQ.1) THEN
!         new particle is centred along horizontal line segment
            xminc=((anca(i)+ancs(i))/2.D0)   
            yminc=radii(ip)
            nmin=ncf(i)
            i=nchf+1
          ELSE
            r1=radii(nps1)+radii(ip)
            y1=coor(2,nps1)+r1*dsin(ancs(i))
          IF (y1.LT.yminc) THEN
            xminc=coor(1,nps1)+r1*dcos(ancs(i))
            yminc=y1
            imin=i
            nmin=ncf(i)
            inds=1
          ENDIF
          i=i+1
        ENDIF 
        GOTO 10                    ! end of while structure
20 CONTINUE
 
      ENDIF

    ELSE

      ! potential energy function depends on vertical walls (wallFLAG=1),
      ! and on 'hp' and vertical walls (heteroFLAG=1)

      IF (nps1.EQ.1) THEN
        ! first particle is a horizontal line segment
        ! minima is located at left corner of horizontal line shadow
        xminc=anca(1)
        yminc=radii(ip)
      ELSE
        ! first particle is a disk
        ! minima is located at limit of angular interval with prev. particles
        r1=radii(nps1)+radii(ip)
        xminc=coor(1,nps1)+r1*dcos(anca(1))
        yminc=coor(2,nps1)+r1*dsin(anca(1))
      ENDIF

      CALL potential(xminc,yminc,pmin)

      DO i=1,nchf

        nps1=lch(ncf(i))
        IF (nps1.EQ.1) THEN
          ! right corner of horizontal line shadow is tested
          x1=ancs(i)   
          y1=radii(ip)
        ELSE
          ! right boundary of angular interval is tested
          r1=radii(nps1)+radii(ip)
          x1=coor(1,nps1)+r1*dcos(ancs(i))
          y1=coor(2,nps1)+r1*dsin(ancs(i))
        ENDIF

        CALL potential(x1,y1,pot)

        IF (pot.LT.pmin) THEN
          xminc=x1
          yminc=y1
          imin=i
          nmin=ncf(i)
          inds=1
          pmin=pot
        ENDIF

      ENDDO

    ENDIF

    ! the chain is modified according to the new element
    coor(1,ip)=xminc
    coor(2,ip)=yminc

    IF ( ID_potential == 1 .AND. lch(nmin) == 1 ) THEN

      DO i=nch+1,nmin,-1
        ! step is negative
        lch(i+2)=lch(i)
      ENDDO
      lch(nmin+1)=ip
      nch=nch+2
    ELSE
      ! the minimum is not centred along a horizontal line segment     
      IF (inds.EQ.0) THEN
        norchi=nmin
        norchf=2
      ELSE
        norchi=ncf(imin+1)
        norchf=nmin+2
      ENDIF

      ntrans=norchf-norchi
 
      IF (ntrans.EQ.1) THEN
 
        DO i=nch+1,norchi,-1
          ! step is negative
          lch(i+ntrans)=lch(i)
        ENDDO
      ENDIF
 
      IF (ntrans.LT.0) THEN 
        DO i=norchi,nch+1   
          lch(i+ntrans)=lch(i)
        ENDDO
      ENDIF

      lch(norchf-1)=ip
      nch=nch+ntrans
    ENDIF

  ENDIF
 
  RETURN

END SUBROUTINE locate
!==============================================================
!     SR arcos(x,y,v,a)
    !> Calculate the arccos of a vector (x,y), between [-pi,pi]
!==============================================================
 SUBROUTINE arcos(x,y,v,a)

  IMPLICIT NONE
 
  ! variables d'entree : 
  real(kind=8), intent(in) :: x, y, v
  
  ! vraiable de sortie :
  real(kind=8), intent(out) :: a

  IF (dabs(x).LE.v) THEN
    a=dacos(x/v)
  ELSE
    IF (x.GT.v) THEN
      a=0.D0
    ELSE
      a=PI_g
    ENDIF
  ENDIF
  IF (y.LT.0.D0) a=-a

END SUBROUTINE arcos
!==============================================================
!     SR potential(x,y,p)
   !> Calculate the value of the potential scalar function p in terms of 
   !> the position x,y and the walls and heterogeneous particles.  
!==============================================================
 SUBROUTINE potential(x,y,p)

  IMPLICIT NONE

  ! variables d'entree :
  real(kind=8), intent(in) :: x !< absissa of a particle
  real(kind=8), intent(in) :: y !< heigth of a particle
  
  ! variables de sortie :
  real(kind=8), intent(out) :: p !< value of the considered potential for this particle

  ! variables locales :
  real(kind=8) :: dx, dy, d
  integer :: i ! indice de boucle

  p=0.D0

  SELECT CASE(ID_potential)
  case(1)
    ! the value of the potential function depends on the altitude
    p=c1*y    
  case(2)
    ! the potential function depends on the distance to the vertical walls 
    p=p-c2/(x+c3)-c2/(bounding_box(1)+c3-x)
  case(3)
    ! the potential function depends on the distance to the hps
    DO i=1,nb_big_bodies
      dx=x-het_coor(1,i)
      dy=y-het_coor(2,i)
      d=dsqrt(dx*dx+dy*dy)/het_radius(i)
      p=p-chp1/d+chp2*d
    ENDDO
  case default
    print *,'deposit2D::potential: FATAL ERROR: unsupported potential'
    stop  
  end select

 END SUBROUTINE potential

!!!------------------------------------------------------------------
!!! WRAPING SUBROUTINE

! procedure qui initialise un nouveau depot 
!> Initialize a new deposit.
subroutine new_deposit(nb_particles, given_radii, lx, type_potential, given_big_radii, given_big_coor)
   implicit none
   !> number of particles
   integer, intent(in) :: nb_particles
   !> given radii list (i.e. input granulometry)
   real(kind=8), dimension(nb_particles), intent(in) :: given_radii
   !> width of the box
   real(kind=8) :: lx
   !> the type of potential to use
   integer, intent(in) :: type_potential
      ! type de potentiel a utiliser :
      ! 1 : depot sous gravite
      ! 2 : depot depuis les murs vers l'interieur
      ! 3 : depot autour de grosses particules
   !> radii of deposited big particles (may be null pointer if not needed)
   real(kind=8), dimension(:)  , pointer :: given_big_radii
   !> coordinates of deposited big particles (may be null pointer if not needed)
   real(kind=8), dimension(:,:), pointer :: given_big_coor

   ! on recupere le nombre de grains de la granulo
   nb_GRAINS = nb_particles
   ! on en deduit le nombre total de corps : 4 murs englobants + les grains
   ! am : raisonnement herite de preprogranul, mais on ne stocke plus les
   !      bords de la boite! il faudrait areter ce surdimenssionement, mais il
   !      semble utilise par les sous-routine de depot : shadow, shadhet et 
   !      locale...
   nb_BODIES = 4 + nb_GRAINS

   ! si on a donne un nombre de grosses particules
   if( associated(given_big_radii) ) then
      ! on le stocke dans le module
      nb_big_bodies = size(given_big_radii)

      ! on teste la compatibilite des donnees :
      ! * completude des donnees
      if (.not. associated(given_big_coor) ) then
         print *,'[deposit2D::new_deposit]: FATAL ERROR: missing given_big_coor'
         stop
      end if
      if (size(given_big_coor,2) /= nb_big_bodies) then
         print *,'[deposit2D::new_deposit]: FATAL ERROR: non conforming size', &
                 ' for radii of big particles'
         stop
      endif
      ! * coherence avec les coordonnes des grosses particules
      if (size(given_big_coor, 1) /= 2 ) then
         print *,'[deposit2D::new_deposit]: FATAL ERROR: non conforming size', &
                 ' for coordinates of big particles'
         stop
      endif
      ! am : la routine de depot sait gerer la presence de grosses particules
      ! quelque soit le potentiel choisi ; il devient donc inutile de verifier
      ! le type de potentiel

      ! si tout est bon, on stocke la caracterisation des grosses particules

      ! ATTENTION : tableaux surdimensionnes!
      ! on les initialise d'abord a 0
      het_radius = 0.d0
      het_coor = 0.d0
      ! on peur alors faire la copie des donnees d'entree
      het_radius(1:nb_big_bodies) = given_big_radii
      het_coor(:, 1:nb_big_bodies) = given_big_coor

      !print *,'het_radius=', het_radius

      !print *,'het_coor(1,:)=', het_coor(1,:)
      !print *,'het_coor(2,:)=', het_coor(2,:)

   ! sinon
   else
      ! il n'y aura pas de grosses particules
      nb_big_bodies = 0
   endif

   ! allocation de la liste des rayons des corps (parois + granulo + grosses  
   ! particules)
   if (allocated(radii)) deallocate(radii)
   allocate(radii(nb_BODIES + nb_big_bodies))

   !print *,'nb_BODIES=', nb_BODIES 
   !print *,'nb_big_bodies=', nb_big_bodies

   ! allocation de la table des coordonnes des corps
   if (allocated(coor)) deallocate(coor)
   allocate(coor(2, nb_BODIES + nb_big_bodies))

   ! mise a 0 des rayons et des coordonnees
   radii = 0.d0
   coor  = 0.d0

   ! gestion de la granulo

   ! on stocke la granulo (i.e. liste des rayons a deposer)
   radii(5:4 + nb_grains) = given_radii(1:nb_grains)

   ! on recupere le rayons min et max de la granulo
   radius_min = minval(given_radii)
   radius_max = maxval(given_radii)
   
   !print*, "radius_min=", radius_min, " radius_max=", radius_max
   !print *,'radii=', radii

   ! gestion de la boite de depot

   ! stockage de la largeur de la boite
   bounding_box = 0.d0
   bounding_box(1) = lx
   ! am : bounding_box(2) est inutile! 

   !print*, 'coor(1,:)=', coor(1,:)
   !print*, 'coor(1,:)=', coor(2,:)

   ! gestion du type de potentiel
 
   !select case(type_potential)
   !   case('gravity') ! cas de la gravite
   !      ID_potential = 1
   !   case('wall') ! cas du depot depuis les murs
   !      ID_potential = 2
   !   case('big_particles') ! cas du depot autour de grosses particules
   !      ID_potential = 3
   !   case default
   if( type_potential < 1 .or. type_potential > 3 ) then
         ! si on n'a pas reconnu le type de potentiel, on arrete tout
         print*,'deposit2D::new_deposit: FATAL ERROR: unknown type of potential'
         stop
   end if
   !end select

   ID_potential = type_potential
   !print *,'ID_potential=', ID_potential

end subroutine new_deposit

! procedure qui recupere les coordonnees des particules deposees
!> Gets the computed coordinates of the particles.
!> \warning allocate the memory
subroutine get_coor(comp_coor)
   implicit none
   !> coordinates of the deposited particles
   real(kind=8), dimension(:,:), pointer :: comp_coor

   ! really paranoid...
   if( associated( comp_coor ) ) deallocate(comp_coor )

   ! just in case...
   if( nb_grains < 1 ) then
     comp_coor  => null()
     return
   end if

   allocate(comp_coor(2,nb_grains))

   ! on ne recupere que les coordonnees des particules deposees, et pas des 
   ! bords de la boite ou les grosses particules
   comp_coor = coor(:, 5:4 + nb_grains) 

   ! cleaning
   deallocate(coor)
   deallocate(radii)

end subroutine get_coor

end module deposit2D
