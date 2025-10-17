!fd le 18/03/10
!fd tentative de creation d'un module permettant de deleguer 
!fd certaines routines a une fonction utilisateur definie au cas par cas
!fd necessitera une compilation du module et une edition de lien avec lmgc90

module user
   implicit none
   private

   public gp_field,gp_ortho_frame

   ! des variables propres au contenu de ce module
! *
! *        Common data
! *        """"""""""
!        Check first call
        INTEGER IFIRST
!
!        Number of elements, nodes, and dofs
        INTEGER NBELE1 , NBNO1 , NDOF1
        PARAMETER ( NBELE1 = 100 )
        PARAMETER ( NBNO1 = NBELE1 + 1 )
        PARAMETER ( NDOF1 = NBNO1 - 1 ) 
!        Element size and thickness
        REAL*8 H1 , EPAI1
        PARAMETER ( EPAI1 = 13.0D-3 )
        PARAMETER ( H1 = EPAI1 / NBELE1 )
!
!        Previous time step, parameter of integration scheme
        REAL*8 DTi , THETA1
!        Material parameters
        REAL*8 RHO0 , D0
!        SPD tridiagonal capacity-like (or mass-like) matrix
!        (lapack storage)
        REAL*8 MD1 ( NBNO1 ) , ME1 ( NBNO1 - 1 )
!        SPD tridiagonal dynamical-like matrix
!        (lapack storage)
        REAL*8 DD1 ( NDOF1 ) , DE1 ( NDOF1 - 1 )

contains

   !> initialisation/calcul des fields par une routine utilisateur 
   !> action : 'incre','setgp'
   !> dt : pas de temps
   !> dim dimension de l'espace
   !> nb_gp nombre de point de gauss
   !> nb_field nombre de field a generer
   !> liste_field nom des field
   !> value_field valeur au gp
   subroutine gp_field(action,tps,dt,dim,nb_gp, &
                       field_name,gp_coor, field_value)
     implicit none
     character(len=5) :: action
     real(kind=8) :: tps,dt
     integer :: dim,nb_gp
     character(len=30),optional :: field_name 
     real(kind=8),optional :: field_value(nb_gp),gp_coor(dim,nb_gp)

     !*** variables locales a la routine
     logical,save :: is_first_time=.TRUE.
     real(kind=8),save :: Hele,tpsf,charf
     integer :: i,rang,id,ig
     real(kind=8) :: apab
     !
     integer,save :: nbele,nbno
     !> wi,wip  value,rate previous
     !> w,wp  value,rate current
     !> wd i
     real(kind=8),allocatable,save :: Wi(:),Wip(:),W(:),Wp(:),WD(:)

     !
     logical :: bavard=.true.
     !
     !real(kind=8),allocatable,save :: z(:)

     !fd on commence par calculer la diffusion 1D

     select case (action)
     case('incre')
       if (is_first_time) then

! ca c'est de la belle merde ....
         tpsf = 0.45D0
         charf = 6.34195558D0

         if (tpsf == 0d0) then
           print*,'Error: final time is equal to 0' 
           stop
         endif
         id = int(tpsf/dt) + 1 
         allocate(WD(id))         
         WD = (/ ((i-1)*charf/(id-1),i=1,id) /)

         if (bavard) then
           print*,'=== initialisation calcul de la diffusion ==='
           print*,'pas de temps: ',DT,' temps final ',tpsf         
           print*,'nombre d increments prevus: ',id
         endif

         call DiffusionMeshSize(DT,Hele,NBELE )
         nbno = nbele + 1

         if (bavard) then
           print*,'pas du maillage: ',hele
           print*,'nombre d elements: ',nbele,' de noeuds: ',nbno
         endif

         ! les tableaux de calcul diffusion par routine DD
         allocate(Wi(nbno),Wip(nbno),W(nbno),Wp(nbno))
         Wi=0.d0; Wip=0.d0; W=0.d0; Wp=0.d0
         ! le tableau de calcul coor maillage diffusion
         !allocate(z(nbno))
         !z = (/ (i-1*Hele,i=1,nbno)  /)

         is_first_time=.false.

       endif

       id = max(1,int(tps/dt))

       if (bavard) then
         print*,'=== calcul de la diffusion ==='
         print*,'pas de temps: ',DT,' temps courrant ',tps         
         print*,'step: ',id
         print*,'chargement: ',WD(id)
       endif

       call DiffusionIncrement (DT , NBNO , Wi , Wip , WD(id) , W  , Wp )


     case('setgp')
       field_value = 0.d0
       ! on cherche les fields qu'on sait traiter
       rang=0
                            !123456789012345678901234567890
       if (field_name /= 'TEMPERATURE                   ') then
         print*,'Error: unable to find TEMPERATURE field'
         stop
       endif

       ! on cherche le point
       do ig=1,nb_gp
         id = int(gp_coor(3,ig)/Hele) + 1 
         if (id < 1 .or. id > nbno) then
           print*,'Error: gp is outside the domain' 
           stop
         endif
         ! on affecte la valeur par interpolation
         apab = (gp_coor(3,ig) - (Hele*(id-1))) / Hele
         field_value(ig) = (1.d0 - apab)*W(id) + apab*W(id+1) 

         if (bavard) then
           print*,'=== affectation valeur au point de gauss ==='
           print*,'pg: ',ig,' position z: ',gp_coor(3,ig) 
           print*,'se trouve dans l element: ',id ,'z1',Hele*(id-1)       
           print*,'coordonnee reduite: ',apab
           print*,'valeur field: ',field_value(ig) 
         endif
       enddo

     case default
       print*,'Error: unknown action'
       stop
     end select


   end subroutine


   subroutine gp_ortho_frame(dim,nb_gp,gp_coor,gp_frame,center,vector)
     implicit none
     integer :: dim,nb_gp
     real(kind=8) :: gp_frame(dim,dim,nb_gp), &
                     gp_coor(dim,nb_gp)
     real(kind=8), dimension(dim), optional :: center, vector

     ! necessaire a la realisation de cette routine
     real(kind=8) ,dimension(3) :: P0,vecl

     if (dim /= 3) then
       print*,'Error: the space dimension must be 3 but it is ',dim     
       stop
     endif


     if (present(center)) then
       P0 = center
     else
       P0 = (/ 0., 0., 0. /)
     end if

     if (present(vector)) then
       VECL = vector
     else
       VECL = (/ 0., 1., 0. /)
     end if


     CALL WoodLRTbasis ( P0,VECL,gp_coor,nb_gp,gp_frame)


   end subroutine


!!!!! les routines utilisees par les fonctions chapeau !!!!

!routine qui permet de calcul l'orientation du repere d'orthotropie
! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE WoodLRTbasis (                             &
!        Input
                               P0 , VECL  , XCOOR , N ,     &
!        Outputs
                                VECLRT )
! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
!     DUREISSEIX David  LMGC THERMOMECANIQUE DES MAT. le 03 / 02 / 2010
!
!        Local LRT basis in a trunc
!
!     Inputs
!        P0(3)            REAL*8                Point of the trunc axis
!        VECL(3)          REAL*8                L direction
!        XCOOR(3,N)       REAL*8                Points coordinates
!        N                INTEGER               Number of points
!     Outputs
!        VECLRT(3,3,N)    REAL*8                Basis vectors
!
! ======================================================================
! *
        IMPLICIT NONE
! *
! *        Global parameter statements
! *        """"""""""""""""""""""""""""
        INTEGER N
        REAL(kind=8) :: P0 ( 3 ) , VECL ( 3 ) , XCOOR ( 3 , N )
        REAL(kind=8) :: VECLRT ( 3 , 3 , N )
! *
! *        Local parameter statements
! *        """"""""""""""""""""""""""
        INTEGER I , J, LENGTH
        REAL(kind=8) :: PROJ1 ( 3 , 3 ) , L ( 3 ) , R ( 3 ), DEFLT ( 3)
        REAL(kind=8) :: MONE , ONE , ZERO
        PARAMETER ( MONE = -1.0D0 , ONE = 1.0D0 , ZERO = 0.0D0 )
        REAL(kind=8) :: WRK ( 3 )
!
        REAL(kind=8) :: ALPHA
        EXTERNAL dnrm2
        REAL(kind=8) :: dnrm2
! ======================================================================
! -
!        Projector on RT plane PROJ1
        ALPHA = dnrm2 ( 3 , VECL , 1 )
        ALPHA = ONE / ALPHA
        CALL dcopy ( 3 , VECL , 1 , L , 1 )
        CALL dscal ( 3 , ALPHA , L , 1 )
        LENGTH = 3 * 3
        CALL ZDANUL ( PROJ1 , LENGTH )
        DO I = 1 , 3
          PROJ1 ( I , I ) = 1.0D0
        ENDDO
        CALL dger ( 3 , 3 , MONE , L , 1 , L , 1 , PROJ1 , 3 )
!       check if L is x-axis
        IF( ANY( L /= (/ONE, ZERO, ZERO/) ) ) THEN
          DEFLT = (/ONE, ZERO, ZERO/)
        ELSE
          DEFLT = (/ZERO, ONE, ZERO/)
        ENDIF
!
!        R = PROJ1 * (P - P0) / NORM(R)
!        T = L ^ R
        DO I = 1 , N
          CALL dcopy ( 3 , L , 1 , VECLRT(1,1,I) , 1 )
!
          CALL dcopy ( 3 , XCOOR(1,I) , 1 , WRK , 1 )
          CALL daxpy ( 3 , MONE , P0 , 1 , WRK , 1 )
          CALL dgemv ( 'N' , 3 , 3 , ONE , PROJ1 , 3 , WRK , 1 , &
                      ZERO , R , 1 )
          IF( ALL(R==0.d0) ) THEN
              CALL ProdVect( 3, L, DEFLT, R )
          ENDif
          ALPHA = dnrm2 ( 3 , R , 1 )
          ALPHA = ONE / ALPHA
          CALL dscal ( 3 , ALPHA , R , 1 )
          CALL dcopy ( 3 , R , 1 , VECLRT(1,2,I) , 1 )
!
          CALL ProdVect ( 3 , VECLRT(1,1,I) , VECLRT(1,2,I), VECLRT(1,3,I) )
        ENDDO
! -
        RETURN
    
  END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calcul de diffusion 1D par David D.
! on raisonne en 1D pour tout le panneau. La direction est perp au panneau.
! la routine DiffusionMeshSize initialise le calcul:
!   - initialise le pas de temps precedent  
!   - definit le maillage 1D, elle rend taille de maille et nb elements,
!     Rq nbno = nbele + 1
! la routine DiffusionIncrement avance d'un pas
! lmgc90 doit gerer le stockage au nbno noeuds du maillage 
!  -W (humidite) 
!  -Wp(derive par rapport au temps) 
!  WD est l'humidite imposee sur le bord libre

! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE DiffusionMeshSize (         & 
!        Input
                               DT ,          &
!        Outputs
                               H  , NBELE )
! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
!     DUREISSEIX David  LMGC THERMOMECANIQUE DES MAT. le 01 / 02 / 2010
!
!        Return the parameters for a 1D regular mesh
!
!     Inputs
!        DT        REAL*8                First time step that will be used
!     Outputs
!        H        REAL*8                Mesh size
!        NBELE        REAL*8                Number of elements
!
! ======================================================================
! *
        IMPLICIT NONE
!fd     < debut modif
!fd     je remplace par le contenue de 
!fd         include 'Diffusion.h'
!fd     par des variables globales
!fd fin modif >

! *
! *        Global parameter statements
! *        """"""""""""""""""""""""""""
        REAL*8 H , DT
        INTEGER NBELE
! *
! *        Local parameter statements
! *        """"""""""""""""""""""""""
        INTEGER LENGTH
!
! ======================================================================
! -
!        From common parameters
        NBELE  = NBELE1
        H      = H1
!
!        Fix common inital values
        IFIRST = 0
        DTi    = DT
        THETA1 = 1.0D0
!
!        Length in m, mass in kg, time in 43200 s = 12 h
!        kg/m3
        RHO0   = 0.45D3
!        m2/s -> m2/12h
        D0     = 2.5D-10 * 43200.D0
!
        CALL zdanul ( MD1 , NBNO1 )
        LENGTH = NBNO1 - 1
        CALL zdanul ( ME1 , LENGTH )
        CALL zdanul ( DD1 , NDOF1 )
        LENGTH = NDOF1 - 1
        CALL zdanul ( DE1 , LENGTH )
! -
        RETURN
        END SUBROUTINE

! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE DiffusionIncrement (                         &
!       Inputs
                                DT , NBNO , Wi , Wip , WD ,   &
!       Outputs
                                W  , Wp )
! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
!     DUREISSEIX David  LMGC THERMOMECA DES MATERIAUX le 01 / 02 / 2010
!
!        Increment the solution for the next time step
!
!     Inputs
!        DT                        REAL*8                Time step length
!        NBNO                        INTEGER                Number of nodes
!        Wi(NBNO)                REAL*8                Initial solution
!        Wip(NBNO)                REAL*8                Initial rate
!        WD                        REAL*8                Prescribed value
!     Outputs
!        W(NBNO)                        REAL*8                Next solution
!        Wp(NBNO)                REAL*8                Next rate
!
! ======================================================================
! *
        IMPLICIT NONE
!fd     < debut modif
!fd     je remplace par le contenue
!fd        include 'Diffusion.h'
!fd     par des variables globales
!fd fin modif >

! *
! *        Global parameter statements
! *        """"""""""""""""""""""""""""
        INTEGER NBNO
        REAL*8 DT , Wi ( NBNO ) , Wip ( NBNO ) , WD
        REAL*8 W ( NBNO ) , Wp ( NBNO )
! *
! *        Local parameter statements
! *        """"""""""""""""""""""""""
        INTEGER KERR , I
        REAL*8 ALPHA
        REAL*8 ZERO
        PARAMETER ( ZERO = 0.0D0 )
!        Static work spaces
        REAL*8 F1 (NBNO1,1) , F2 ( NBNO1 )
!
! ======================================================================
! -
!        Check
!        """"
        KERR = 0
        IF (NBNO .NE. (NBELE1+1)) THEN
          WRITE (*,*) NBNO , NBELE1
          WRITE (*,*) 'DiffusionIncrement: Bad size'
          STOP 17
        ENDIF
!
        IF ((IFIRST .EQ. 0) .OR. (DT .NE. DTi)) THEN
!
!          Initialization or new time step length
!          """"""""""""""
!DD          WRITE (*,*) 'DiffusionIncrement: Initialize matrices'
!
!          New common values
          IFIRST = 1
          DTi    = DT

          print*,'taille F1'
          print*,size(F1,dim=1)
          print*,size(F1,dim=2)


!
!          Fill-in capacity-like (mass-like) matrix MD1,ME1
!          SPD, tridiagonal (pt for lapack)
          I = 1
          MD1 ( I ) = 2.0D0 * RHO0 * H1 / 6.0D0
          ME1 ( I ) = 1.0D0 * RHO0 * H1 / 6.0D0
          DO I = 2 , NBNO1-1
            MD1 ( I ) = 4.0D0 * RHO0 * H1 / 6.0D0
            ME1 ( I ) = 1.0D0 * RHO0 * H1 / 6.0D0
          ENDDO
          I = NBNO1
          MD1 ( I ) = 2.0D0 * RHO0 * H1 / 6.0D0
!
!          Fill-in dynamical-like matrix DD1,DE1
!          (mass-like/THETA1/DT + diffusivity) matrix
!          SPD, tridiagonal (pt for lapack)
!          does not assemble first node (prescribed dof)
          DO I = 1 , NDOF1-1
            DD1 ( I ) = 2.0D0*RHO0*D0/H1 + (1.0D0/THETA1/DT) * MD1(I+1)
            DE1 ( I ) =-1.0D0*RHO0*D0/H1 + (1.0D0/THETA1/DT) * ME1(I+1)
          ENDDO
          I = NDOF1
          DD1 ( I ) = 1.0D0*RHO0*D0/H1 + (1.0D0/THETA1/DT) * MD1(I+1)
!
!          LDLT Factorization of dynamical matrix (dpttrf for lapack)
          CALL dpttrf ( NDOF1 , DD1 , DE1 , KERR )
          IF (KERR .NE. 0) THEN
            WRITE (*,*) 'DiffusionIncrement: dpttrf: KERR' , KERR
            STOP 19
          ENDIF
!
        ENDIF
!
!        Right hand side
!        """"""""""""""
!        Compute F1 <- Wi + (1.-THETA1)*DT*Wip
        DO I = 1 , NBNO1
          F1 (I,1) = Wi(I) + (1.0D0 - THETA1) * DT * Wip(I)
        ENDDO
!        Multiply F2 <- (1/THETA1/DT)*M*F1
        ALPHA = 1.0D0
        CALL dlagtm ( 'N' , NBNO1 , 1 , ALPHA , ME1 , MD1 , ME1 , &
                      F1 , NBNO1 , ZERO , F2 , NBNO1 )
        ALPHA = 1.0D0 / THETA1 / DT
        CALL dscal ( NBNO1 , ALPHA , F2 , 1 )
!        Get rid of first dof (prescribed)
        DO I = 1 , NBNO1-1
          F1 (I,1) = F2 ( I + 1 )
        ENDDO
!        Add prescribed value
        F1 (1, 1 ) = F1 (1, 1 ) - &
                  ((1.0D0/THETA1/DT)*RHO0*H1/6.0D0 - RHO0*D0/H1) * WD
!
!        Solving diffusion (dpttrs for lapack)
!        """"""""""""""""
        CALL dpttrs ( NDOF1 , 1 , DD1 , DE1 , F1 , NBNO1 , KERR )
        IF (KERR .NE. 0) THEN
          WRITE (*,*) 'DiffusionIncrement: dpttrs: KERR' , KERR
          STOP 20
        ENDIF
        DO I = 1 , NDOF1
          W ( I+1 ) = F1(I,1)
        ENDDO
        W ( 1 ) = WD
!
!        Post-process rate
!        """"""""""""
        DO I = 1 , NBNO1
          Wp ( I ) = (1.0D0/THETA1/DT) * (W(I) - Wi(I)) - &
                     ((1.0D0-THETA1)/THETA1) * Wip(I)
        ENDDO
! -
        RETURN
        END SUBROUTINE
!!!------------------------------------------------------------------------  
!fd routine utilitaires de DD
!!!------------------------------------------------------------------------  
! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
      SUBROUTINE ProdVect (                   & 
!        Input
                               N , VECT1 , VECT2 ,  &
!        Outputs
                               VECT )
! [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
!     DUREISSEIX David  LMGC THERMOMECANIQUE DES MAT. le 03 / 02 / 2010
!
!        Compute the cross product of (N-1) vectors
!
!     Inputs
!        N              INTEGER               Size
!        VECT1(N)     REAL*8                Vector
!        VECT2(N)     REAL*8                Vector
!     Outputs
!        VECT(N)      REAL*8                Cross product
! ======================================================================
! *
        IMPLICIT NONE
! *
! *        Global parameter statements
! *        """"""""""""""""""""""""""""
        INTEGER N
        REAL*8 VECT1( N ), VECT2( N ), VECT ( N )
! *
! *        Local parameter statements
! *        """"""""""""""""""""""""""
! ======================================================================
! -
        VECT(1) = VECT1(2)*VECT2(3)-VECT1(3)*VECT2(2)
        VECT(2) = VECT1(3)*VECT2(1)-VECT1(1)*VECT2(3)
        VECT(3) = VECT1(1)*VECT2(2)-VECT1(2)*VECT2(1)
! -
        RETURN
        END SUBROUTINE
!!!------------------------------------------------------------------------  
! ZDANUL    SOURCE    CHAT      05/01/13    04:22:06     5004           
      SUBROUTINE ZDANUL(A,LA)                                                   
!                                                                               
!         ANNULE UN TABLEAU DE LONGUEUR LA        A EN DOUBLE PRECISION         
!                                                                               
      IMPLICIT INTEGER(I-N)
      REAL*8 A                                                                  
      DIMENSION A(1)                                                            
      DO 1 IA=1,LA                                                              
      A(IA)=0.0D0                                                               
    1 CONTINUE                                                                  
      RETURN                                                                    
      END SUBROUTINE


end module user
