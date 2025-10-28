!=======================================================================
! module qui permet de faire une decoupe dans un echantillon 2D
! ATTENTION : cette methode peut change la granulometrie de l'echantillon 
!             initial (retrait de particules)!
!> Module dedicated to cut a 2D sample
!> \warning This method could change the initial granulometry.
MODULE cut2D

   IMPLICIT NONE
 
   PRIVATE

   REAL(kind=8),ALLOCATABLE,DIMENSION(:,:) :: coor ! coordonnees des centre 
      ! d'inertie               
   REAL(kind=8), allocatable, DIMENSION(:) :: radii ! rayons d'encombrement
 
   ! GRANULO ****
   !
   integer :: nb_grains ! nombre de particules
   REAL(kind=8) :: radius_min, radius_max
 
   ! SLOP  ****
   !
   INTEGER :: nb_SLOPE = 0  
   INTEGER :: nslc = 1 ! slope test parameter
   REAL(kind=8), ALLOCATABLE, DIMENSION(:,:) :: coor_slope ! slope coordinate 
      ! vector 
   logical :: is_closed =.false. ! vaut vrai ssi le contour defini est ferme
 
   PUBLIC :: &
        new_cut, &
        compute, &
        get_inner_particles

CONTAINS

   ! fonction qui retire les particules "hors" du contour
   ! preconditions :
   !   - la variable gloable nb_grains contient le nombre total de particules
   !   - la variable globale radii contient les rayons des particules
   !   - la variable globale coor contient les coordonnees des particules
   ! postconditions :
   !   - la fonction renvoie le nombre de particules "dans" le contour : nb_in
   !   - les particules sont dans le contour sont rangees :
   !        * les nb_in premiers ellements de radii sont les rayons des particules
   !          dans le contour
   !        * les nb_in permieres colonnes de coor sont les coordonnees des
   !          particules dans le contour
   !        * les autres elements de radii et coor sont mis a 0
   !> Compute a cut and return the number of remaining particles.
   !> \warning 
   !>    1) a cut is supposed to be initialized \n
   !>    2) numbering of the particles is changed during the process and radii
   !>       and coordinates of the removed particles are set to 0
   function compute()
      IMPLICIT NONE
      ! valeur de retour :
      integer :: compute !< number of remaining particles
         ! nombre de particules "dans" le contour
  
      ! variables locales :
      INTEGER      :: i,j,nps,indi,nints
      REAL(kind=8) :: x1,x2,y1,y2,sm,sb,xsmax,xsmin,ysmax,ysmin
      real(kind=8) :: xpar,ypar,rpar,yslp
      REAL(kind=8) :: svx,svy,sv,vnx,vny,vpx,vpy,dp,ds,da,psc1,psc2

      !print*, 'nb_points:' , nb_slope
      !print*, 'coordonnees', coor_slope

      ! on intialise à 0 le nombre de particules a l'interieur du contour
      nps = 0 

     ! Removing particles above lines
    
      xsmin=minval(coor_slope(1,:))
      xsmax=maxval(coor_slope(1,:))
      ysmin=minval(coor_slope(2,:))
      ysmax=maxval(coor_slope(2,:))
             
      DO i=1, nb_grains
        xpar=coor(1,i)
        ypar=coor(2,i)
        rpar=radii(i)
    
        indi=0
        
        ! contour ouvert on est au dessus du point le plus haut
        if (.not. is_closed .and. (ypar-rpar) .GT. ysmax) cycle
    
        ! contour ferme on est au dehors de la boite englobante
        if ( is_closed .and. ((xpar+rpar) < xsmin .or. (xpar-rpar) > xsmax) .and. &
                             ((ypar+rpar) < ysmin .or. (ypar-rpar) > ysmax)) cycle
    
        ! the particle is "under" the slope
        if (.not. is_closed .and. (ypar+rpar) .LE. ysmin) indi=1    
    
        if (indi == 0) then
          nints=0 ! check if centre of particle is "inside" the slope
          DO j=2, nb_slope
            x1=coor_slope(1,j-1)
            x2=coor_slope(1,j)
            y1=coor_slope(2,j-1)
            y2=coor_slope(2,j)
    
            IF (x1.NE.x2) THEN
              IF ((x1.LT.xpar).AND.(xpar.LE.x2) .or. &
                  (x2.LE.xpar).AND.(xpar.LT.x1))  THEN
                  sm=(y2-y1)/(x2-x1)
                  sb=y1-sm*x1
                  yslp=sm*xpar+sb
                  IF (ypar.LT.yslp) THEN
                    nints=nints+1                
                  ENDIF
              ENDIF
            ENDIF
          ENDDO
    
    ! -------- number of intersections is odd
    ! -------- the centre of particle is inside the slope
    ! -------- indi=1 : suppose the whole particle is inside th slope
    
          IF (MOD(nints,2).EQ.1) THEN
            indi=1
            IF (nslc.EQ.2) THEN  !check if whole particle is inside the slope
              j=2
              DO WHILE (j.LE.nb_slope)
                svx=x2-x1
                svy=y2-y1
                sv=SQRT(svx**2+svy**2)
                vnx=svx/sv
                vny=svy/sv
                vpx=xpar-x1
                vpy=ypar-y1
                dp=ABS((vnx*vpy)-(vny*vpx)) 
                IF (dp.LT.rpar) THEN
                  ds=((vnx*vpx)+(vny*vpy))
                  da=SQRT(rpar**2+dp**2)
                  psc1=ds-da
                  psc2=ds+da
                  IF (((0.LE.psc1).AND.(psc1.LE.sv)).OR. &
                      ((0.LE.psc2).AND.(psc2.LE.sv))) THEN
                    indi=0
                    j=nb_slope
                  ENDIF
                ENDIF             
                j=j+1
              ENDDO  
            ENDIF
          ENDIF
        ENDIF
    
    ! ------ The particle (disk) is within the slope
    ! ------ the disk must be added to the list
    
        IF (indi.EQ.1) THEN
          nps=nps+1
          IF (nps.NE.i) THEN ! this means that nps < 1
    !   it would be possible to correct for interpenetation
            radii(nps)=radii(i)   
            coor(1,nps) = coor(1,i)
            coor(2,nps) = coor(2,i)
          ENDIF
        ENDIF
      ENDDO   
   
      ! on met a 0 les elements de caracterisation des particules hors contour :
      !   * les rayons
      radii(nps + 1 : nb_grains) = 0.d0
      !   * les coordonnees
      coor(:, nps + 1 : nb_grains) = 0.d0
  
      ! on renvoie le nombre de particules a l'interieur du contour
      compute = nps
   
      WRITE(*,*) ' number of disks below the slope profile =', nps
  
   end function compute

   ! procedure qui intialise une nouvelle decoupe d'un echantillon : stocke le
   ! nombre de particules, leur rayons, leurs coordonnees et decrit le contour
   ! a utiliser par une liste de points, le contour etant la polyligne (pas
   ! necessairement fermee) reliant ces points
   !> Initialises a new cut. A polyline is defined by a given set of points. 
   !> In the case of a closed polyline, only the inner particles remain. 
   !> An open polyline is supposed to link the the two vertical walls of the 
   !> box. In this case, only particles under the polyline remain.
   subroutine new_cut(nb_particles, given_radii, given_coor, nb_points, given_slope_coor)

      implicit none

      ! variables d'entree :
      integer, intent(in) :: nb_particles !< number of particles
         ! nombre de particules
      real(kind=8), dimension(nb_particles), intent(in) :: given_radii !< given radii list (i.e. granulometry)
         ! rayons des particules
      real(kind=8), dimension(2, nb_particles), intent(in) :: given_coor !< coordinates of the particles
         ! coordonnees des particules
      integer, intent(in) :: nb_points !< number of points defining the polyline
         ! nombre de points pour la definition du contour
      real(kind=8), dimension(2, nb_points), intent(in) :: given_slope_coor !< coordinates of the points
         ! coordonnees des points

      ! on recupere le nombre de grains de la granulo
      nb_grains = nb_particles
    
      !print *,'nb_grains=', nb_grains 
   
      ! allocation de la liste des rayons des particules
      if (allocated(radii)) deallocate(radii)
      allocate(radii(nb_grains))
   
      ! stockage des rayons des particules
      radii = given_radii
   
      !print *,'radii=', radii 
   
      ! allocation de la table des coordonnes des corps
      if (allocated(coor)) deallocate(coor)
      allocate(coor(2, nb_grains))
   
      ! stockage des coordonnees des particules
      coor = given_coor
   
      !print *,'coor=', coor 
   
      ! on recupere le nombre de points definissant le contour
      nb_slope = nb_points
    
      !print *,'nb_slope=', nb_slope 
 
      ! allocation de la table des coordonnes des points
      if (allocated(coor_slope)) deallocate(coor_slope)
      allocate(coor_slope(2, nb_points))
   
      ! stockage des coordonnees des points
      coor_slope = given_slope_coor
   
      !print *,'coor_slope=', coor_slpoe 

      ! si les premier point est aussi le dernier point, le contour est ferme
      if (dot_product(coor_slope(:,1) - coor_slope(:,nb_slope), & 
                      coor_slope(:,1) - coor_slope(:,nb_slope)) < 1.0d-10) &
         is_closed=.true. 

   end subroutine new_cut

   ! procedure qui recupere les caracteristiques des particules a l'interieur
   ! du contoure
   !> Gets the radii and coordinates defining the remaining particles.
   subroutine get_inner_particles(inner_radii, inner_coor, nps)
      implicit none
      !> output radii
      real(kind=8), dimension(:)  , pointer :: inner_radii
      !> output coordinates
      real(kind=8), dimension(:,:), pointer :: inner_coor
      !> the number of particles to get
      integer, intent(in) :: nps

      ! should not need this
      if( associated(inner_radii) ) deallocate(inner_radii)
      if( associated(inner_coor ) ) deallocate(inner_coor )

      allocate( inner_radii(nps)  )
      allocate( inner_coor(2,nps) )

      ! on recupere les nouveaux rayons des particules :
      ! les particules a l'interieur du contour sont rangees au debut, les
      ! autres ont des rayons nuls 
      inner_radii(:) = radii(:nps)
  
      ! on recupere les nouvelles coordonnees des particules :
      ! les particules a l'interieur du contour sont rangees au debut, les
      ! autres ont des coordonnees nulles 
      inner_coor(:,:) = coor(:,:nps)

      !clean up...
      if( allocated(coor)       ) deallocate(coor)
      if( allocated(radii)      ) deallocate(radii)
      if( allocated(coor_slope) ) deallocate(coor_slope)

   end subroutine get_inner_particles

end module cut2D
