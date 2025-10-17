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


!> generic API to thermal finite element 
MODULE a_therEF

USE utilities

USE a_EF

USE a_therEF_iso
USE a_therEF_bar

IMPLICIT NONE

TYPE T_therEF
  integer          :: ID      ! iso, bar, shell
  integer          :: nb      ! number of ID 
  CHARACTER(len=5) :: NAME    ! nickname DKTxx,...
END TYPE T_therEF

INTEGER :: nb_therEF=0
TYPE(T_therEF),DIMENSION(:),ALLOCATABLE :: therEF

CONTAINS

SUBROUTINE init_therEF
  IMPLICIT NONE

  INTEGER :: i,j,errare
!                           12345678901234567890123
  CHARACTER(len=23) :: IAM='a_therEF::init_therEF'

  !rm : for reload...
  if( nb_therEF > 0 ) return
!
! initialisation des types
! 
  CALL init_therEF_iso
  CALL init_therEF_bar
!
! construction de la table de hachage 
!
  nb_therEF=nb_therEF+get_nb_ele_iso()
  nb_therEF=nb_therEF+get_nb_ele_bar()

  ALLOCATE(therEF(nb_therEF),stat=errare)
  IF (errare /= 0) THEN
    CALL FATERR(IAM,'error allocating therm_ef%point_gauss')
  END IF
!
  j=0
!
  DO i=1,get_nb_ele_iso()
    therEF(i+j)%ID=i_iso
    therEF(i+j)%nb=i
    therEF(i+j)%NAME=get_NAME_therEF_iso(i)
  ENDDO
  j=j+get_nb_ele_iso()
  
  DO i=1,get_nb_ele_bar()
    therEF(i+j)%ID=i_bar
    therEF(i+j)%nb=i
    therEF(i+j)%NAME=get_NAME_therEF_bar(i)
  ENDDO
  j=j+get_nb_ele_bar()
END SUBROUTINE init_therEF
!=======================================================================
INTEGER FUNCTION get_nb_in_therEF(NAME)
!
! fonction qui retourne l'indice dans le tableau therEF 
! de l'element de nom NAME 
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i
!                            1234567890123456789012345678
  CHARACTER(len=28)  :: IAM='a_therEF::get_nb_in_therEF'
  CHARACTER(len=30)  :: cout

  DO i=1,SIZE(therEF)
 
   IF (therEF(i)%NAME == NAME) THEN
     get_nb_in_therEF=i
     RETURN
   ENDIF

  ENDDO
                           !12345678        12345678
  WRITE(cout,'(A8,A5,A8)') 'therEF ',NAME,' unknown'
  CALL FATERR(IAM,cout)

END FUNCTION get_nb_in_therEF
!=======================================================================
INTEGER FUNCTION get_nb_in_therEFmodel(i)
!
! fonction qui retourne l'indice dans le tableau therEFmodel 
! de l'element d'indice i 
!
  IMPLICIT NONE
  INTEGER          :: i
!                           123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_therEF::get_nb_in_therEFmodel'
  CHARACTER(len=30) :: cout
!
  get_nb_in_therEFmodel=therEF(i)%nb
  RETURN

!                            !1234567        12345678
!  write(cout,'(A7,A5,A8)') 'therEF ',NAME,' unknown'
!  call FATERR(IAM,cout)

END FUNCTION get_nb_in_therEFmodel
!=======================================================================
integer function ele_is(nb)
!
! fonction qui retourne l'ID (iso/bar/shell) de l'element 
! d'indice nb dans therEF
!
  IMPLICIT NONE
  integer :: nb

  ele_is = therEF(nb)%ID
!
end function ele_is
!=======================================================================
INTEGER FUNCTION get_bw_therEF(i,nodes)
!
! fonction qui retourne la largueur de bande de l'element
! d'indice i dans therEF
!
  IMPLICIT NONE
  INTEGER :: nb,i
  INTEGER,DIMENSION(:) :: nodes
!
  nb=get_nb_in_therEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_bw_therEF=get_bw_therEF_iso(nb,nodes)
  END SELECT

!!  print*,'element de type: ',ele_is(i)
!!  print*,'numero dans le type:',nb
!!  print*,'connectivite   : ',nodes
!!  print*,'largeur de bande:',get_bw_therEF

END FUNCTION get_bw_therEF

!============ compute elementary conductivity matrix ===

SUBROUTINE compute_elementary_conductivity(blmnb,ppsnb,dt,coor_ele,T_ele,ibdyty,iblmty, &
                                           F,need_F,K,need_K,push_f)

!
! fonction qui calcule la matrice de rigidite elementaire d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: nb,blmnb
  INTEGER :: ibdyty,iblmty
  real(kind=8) :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:) :: T_ele
  REAL(kind=8),DIMENSION(:,:) :: K
  REAL(kind=8),DIMENSION(:)   :: F
  logical :: need_F, need_K, push_f
  
  integer,dimension(:) :: ppsnb

  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL conductivity_ISO(nb,ppsnb,dt,coor_ele,T_ele,ibdyty,iblmty,F,need_F,K,need_K,push_f)
    CASE( i_bar )
     CALL conductivity_BAR(nb,ppsnb,dt,coor_ele,T_ele,ibdyty,iblmty,F,need_F,K,need_K,push_f)
  end select

END SUBROUTINE compute_elementary_conductivity 
                                            
! SUBROUTINE compute_elementary_conductivity_nl(blmnb,ppsnb,dt,beta,coor_ele,T_ele,ibdyty,iblmty,F,K)       

! !
! ! fonction qui calcule la matrice de rigidite elementaire d'un
! ! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! ! et de position coor_ele
! !  
!   IMPLICIT NONE
!   INTEGER :: nb,blmnb
!   INTEGER :: ibdyty,iblmty
!   real(kind=8) :: dt,beta
!   REAL(kind=8),DIMENSION(:,:) :: coor_ele
!   REAL(kind=8),DIMENSION(:) :: T_ele
!   REAL(kind=8),DIMENSION(:,:) :: K
!   REAL(kind=8),DIMENSION(:)   :: F
  
!   integer,dimension(:) :: ppsnb

!   nb=get_nb_in_therEFmodel(blmnb)

!   SELECT CASE (ele_is(blmnb))
!     CASE( i_iso )
!      CALL conductivity_ISO_nl(nb,ppsnb,dt,beta,coor_ele,T_ele,ibdyty,iblmty,F,K)
!     CASE( i_bar )
!      !CALL conductivity_BAR_nl(nb,ppsnb,dt,beta,coor_ele,T_ele,ibdyty,iblmty,F,K)
!   end select

! END SUBROUTINE compute_elementary_conductivity_nl


!============ compute elementary capacity matrix ===


SUBROUTINE compute_elementary_capacity(blmnb,ppsnb,dt,coor_ele,DT_ele,ibdyty,iblmty,F,M)       

!
! fonction qui calcule la matrice de rigidite elementaire d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: nb,blmnb
  INTEGER :: ibdyty,iblmty
  real(kind=8) :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:,:) :: M
  REAL(kind=8),DIMENSION(:)   :: F
  REAL(kind=8),DIMENSION(:)   :: DT_ele

  integer,dimension(:) :: ppsnb
  
  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )

     CALL capacity_ISO(nb,ppsnb,dt,coor_ele,DT_ele,ibdyty,iblmty,F,M)
     
    CASE( i_bar )
     CALL capacity_BAR(nb,ppsnb,dt,coor_ele,DT_ele,ibdyty,iblmty,F,M)
  end select

END SUBROUTINE compute_elementary_capacity

!============ compute elementary internal flux ===

SUBROUTINE compute_elementary_ttfint(blmnb,ppsnb,dt,coor_ele,ibdyty,iblmty,fint)       
!
! fonction qui calcule la contribution aux flux elementaires internes d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: nb,blmnb
  INTEGER :: ibdyty,iblmty
  real(kind=8) :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:)   :: Fint

  integer,dimension(:) :: ppsnb

  fint=0.d0
  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL   FINT_ISO (nb,ppsnb,dt,coor_ele,ibdyty,iblmty,fint)

    CASE( i_bar )

     !CALL   FINT_ISO (nb,ppsnb,dt,coor_ele,ibdyty,iblmty,fint)
  endselect

END SUBROUTINE compute_elementary_ttfint

!> \brief Compute elementary conductivity matrix for an element
!> API compatible with simulation module
subroutine compute_elementary_conductivity2(blmnb,ppsnb,dt,coor_ele,T_ele,fields,map,names,F,K)
  implicit none
  !> [in] element model number
  integer(kind=4),                   intent(in)  :: blmnb
  !> [in] property set
  integer(kind=4),   dimension(:),   intent(in)  :: ppsnb
  !> [in] time differential (matlib use...)
  real(kind=8),                      intent(in)  :: dt
  !> [in] coordinates of the vertices of the element
  real(kind=8),      dimension(:,:), intent(in)  :: coor_ele
  !> [in] temperature on the vertices of the element
  real(kind=8),      dimension(:)  , intent(in)  :: T_ele
  !> [in] map to acces quadrature point fields
  integer(kind=4),   dimension(:,:), intent(in)  :: map
  !> [in] map to get quadrature fields name
  character(len=30), dimension(:),   intent(in)  :: names
  !> [in] fields values at quadrature points
  real(kind=8),      dimension(:),   intent(in)  :: fields
  !> [out] internal elementary flux vector
  real(kind=8),      dimension(:),   intent(out) :: F
  !> [out] conductivity elementary matrix
  real(kind=8),      dimension(:,:), intent(out) :: K
  !
  integer(kind=4) :: nb

  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )

     call conductivity_iso2(nb,ppsnb,dt,coor_ele,T_ele,fields,map,names,F,K)

  endselect

end subroutine compute_elementary_conductivity2

 
!> \brief Compute elementary capacity matrix for an element
!> API compatible with simulation module
subroutine compute_elementary_capacity2(blmnb,ppsnb,dt,coor_ele,DT_ele,fields,map,names,F,M)
  implicit none
  !> [in] element model number
  integer(kind=4),                   intent(in)  :: blmnb
  !> [in] property set
  integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb
  !> [in] time differential (matlib use...)
  real(kind=8)                     , intent(in)  :: dt 
  !> [in] coordinates of the vertices of the element
  real(kind=8),      dimension(:,:), intent(in)  :: coor_ele
  !> [in]  temperature difference on the nodes
  real(kind=8),      dimension(:)  , intent(in)  :: DT_ele
  !> [in] map to acces quadrature point fields
  integer(kind=4),   dimension(:,:), intent(in)  :: map
  !> [in] map to get quadrature fields name
  character(len=30), dimension(:)  , intent(in)  :: names
  !> [in] fields values at quadrature points
  real(kind=8),      dimension(:)  , intent(in)  :: fields
  !> [out] elementary internal fluxes vector
  real(kind=8),      dimension(:)  , intent(out) :: F
  !> [out] capacity elementary matrix
  real(kind=8),      dimension(:,:), intent(out) :: M
  !
  integer(kind=4) :: nb
   
  nb=get_nb_in_therEFmodel(blmnb)

  select case( ele_is(blmnb) )
  case( i_iso )

    call capacity_iso2(nb,ppsnb,dt,coor_ele,DT_ele,fields,map,names,F,M)

  end select

end subroutine compute_elementary_capacity2

!> \brief Compute elementary internal flux of an element
!> API compatible with simulation module
subroutine compute_elementary_ttfint2(blmnb,ppsnb,dt,coor_ele,old_fields,new_fields,map,names,fint)
  implicit none
  !> [in] element model number
  integer(kind=4),                   intent(in)  :: blmnb
  !> [in] property set
  integer(kind=4),   dimension(:)  , intent(in)  :: ppsnb
  !> [in] time differential (matlib use...)
  real(kind=8)                     , intent(in)  :: dt 
  !> [in] coordinates of the vertices of the element
  real(kind=8),      dimension(:,:), intent(in)  :: coor_ele
  !> [in]  temperature difference on the nodes
  integer(kind=4),   dimension(:,:), intent(in)  :: map
  !> [in] map to get quadrature fields name
  character(len=30), dimension(:)  , intent(in)  :: names
  !> [in] fields values at quadrature points of previous time step
  real(kind=8),      dimension(:)  , intent(in)  :: old_fields
  !> [in] current fields values at quadrature points
  real(kind=8),      dimension(:)  , intent(in)  :: new_fields
  !> [out] elementary internal fluxes vector
  real(kind=8),      dimension(:)  , intent(out) :: fint
  !
  integer(kind=4) :: nb
   
  nb=get_nb_in_therEFmodel(blmnb)

  select case( ele_is(blmnb) )
  case( i_iso )

    call fint_iso2(nb,ppsnb,dt,coor_ele,old_fields,new_fields,map,names,fint)

  endselect

end subroutine compute_elementary_ttfint2


!===================================================
!============= low level routines ==================
!===================================================
INTEGER FUNCTION get_N_NODE_therEF(NAME)
!
! fonction qui retourne le nombre de noeud 
! de l'element de nom NAME
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_therEF(NAME)  
  nb=get_nb_in_therEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_NODE_therEF=get_N_NODE_therEF_iso(nb)
    CASE( i_bar )
      get_N_NODE_therEF=get_N_NODE_therEF_bar(nb)
  END SELECT

END FUNCTION get_N_NODE_therEF

!=======================================================================
INTEGER FUNCTION get_N_DOF_by_NODE_therEF(NAME)
!
! fonction qui retourne le nombre de points de gauss
! de l'element de nom NAME
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_therEF(NAME)  
  nb=get_nb_in_therEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_DOF_by_NODE_therEF=get_N_DOF_by_NODE_therEF_iso(nb)
    CASE( i_bar )
      get_N_DOF_by_NODE_therEF=get_N_DOF_by_NODE_therEF_iso(nb)
  END SELECT

END FUNCTION get_N_DOF_by_NODE_therEF
!=======================================================================
INTEGER FUNCTION get_N_GP_therEF(NAME)
!
! pour retrouver le nb de pg  et le nb de val par pg
!
  IMPLICIT NONE
  CHARACTER(len=5) :: NAME
  INTEGER          :: i,nb

  i=get_nb_in_therEF(NAME)  
  nb=get_nb_in_therEFmodel(i)

  SELECT CASE (ele_is(i))
    CASE( i_iso )
      get_N_GP_therEF=get_N_GP_therEF_iso(nb)
    CASE( i_bar )
      get_N_GP_therEF=get_N_GP_therEF_bar(nb)
   CASE default
      get_N_GP_therEF=0
      PRINT*,'get_N_GP_therEF non implemante pour ce type d element'
  END SELECT

END FUNCTION get_N_GP_therEF
!******************************************************************************
subroutine get_coor_pg(blmnb,coor_ele,coor_pg)
!
! fonction qui calcule les coordonnees des points de gauss 
! d'element ayant l'indice blmnb dans mecaEF, 
! de property set  ppsnb
! et de position coor_ele
!  
  implicit none
  integer :: ppsnb,blmnb,nb
  real(kind=8),dimension(:,:) :: coor_ele
  real(kind=8),dimension(:,:) :: coor_pg


  nb=get_nb_in_therEFmodel(blmnb)

  select case (ele_is(blmnb))
    case( i_iso )
     call   get_coor_pg_ISO(nb,coor_ele,coor_pg)
    case( i_bar )
     call   get_coor_pg_BAR(nb,coor_ele,coor_pg)
!    case('shell')  
  endselect

end subroutine get_coor_pg
!******************************************************************************

!******************************************************************************
subroutine interpolate_node2pg(blmnb,valnoe,valpg)
!
! fonction qui calcule les valeurs au pg en partant des valeurs aux noeuds
!  
  implicit none
  integer :: blmnb,nb
  real(kind=8),dimension(:) :: valnoe
  real(kind=8),dimension(:) :: valpg

  nb=get_nb_in_therEFmodel(blmnb)

  select case (ele_is(blmnb))
    case( i_iso )
     call   interpolate_node2pg_ISO(nb,valnoe,valpg)
    case( i_bar )
     call   interpolate_node2pg_BAR(nb,valnoe,valpg)
!    case('shell')  
!    case( i_bar )  
  endselect

end subroutine interpolate_node2pg
!******************************************************************************

!am : debut des fonctions suplementaires

! fonction qui recupere le type de fonction de forme d'un element
function get_T_FONC_FORME_therEF(blmnb)

   implicit none
  
   ! variables d'entree :
   integer, intent(in) :: blmnb ! indice dans therEF
  
   ! valeur de retour :
   integer(kind=4) :: get_T_FONC_FORME_therEF ! type de la fonction
      ! de forme de l'element
  
   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   integer(kind=4) :: T_Fonc_Forme ! pour recuperer le type de fonction de
      ! forme de l'element
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
          T_Fonc_Forme = get_T_FONC_FORME_therEF_iso(nb)
      case( i_bar ) ! cas d'un element bar parametrique
          T_Fonc_Forme = get_T_FONC_FORME_therEF_bar(nb)
   end select

   ! on renvoie le volume de l'element
   get_T_FONC_FORME_therEF = T_Fonc_Forme

end function get_T_FONC_FORME_therEF

! fonction qui calcule le volume d'un element d'indice blmnb dans therEF et 
! de position coor_ele
function compute_element_volume(blmnb, coor_ele)       

   implicit none
  
   ! variables d'entree :
   integer, intent(in) :: blmnb ! indice dans therEF
   real(kind=8), dimension(:,:), intent(in) :: coor_ele ! coordonnees des sommets de l'element
  
   ! valeur de retour :
   real(kind=8) :: compute_element_volume ! volume de l'element
  
   ! variables locales :
   real(kind=8) :: V ! pour recuperer le volume de l'element
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
           V = element_volume_ISO(nb, coor_ele)
      case( i_bar ) ! cas d'un element iso parametrique
           print *,'CAUTION : element_volume_BAR n existe pas encore'
           !V = element_volume_BAR(nb, coor_ele)
   end select

   ! on renvoie le volume de l'element
   compute_element_volume = V

end function compute_element_volume

! fonction qui calcule, pour un element, une part du volume a affecter
! a chaque noeud
subroutine element_volume_by_node_therEF(blmnb, coor_ele, V_nod)

   implicit none

   ! variables d'entree :
   integer, intent(in) :: blmnb ! indice dans therEF
   real(kind=8), dimension(:,:), intent(in) :: coor_ele ! coordonnees des sommets de l'element
  
   ! variable de sortie :
   real(kind=8), dimension(:), intent(out) :: V_nod ! volume a affecter a chaque noeud
                                                    ! V_nod(j) contient le volume a affecter
                                                    ! au noeud i de l'element
                                                    ! on a : somme sur j noeud de l'elelement
                                                    ! de V_nod(j) = volume de l'element

   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
         call element_volume_by_node_ISO(nb, coor_ele, V_nod)
      case( i_bar ) ! cas d'un element iso parametrique
         !call element_volume_by_node_BAR(nb, coor_ele, V_nod)
         
   end select

end subroutine element_volume_by_node_therEF

! fonction qui calcule le vecteur elementaire corespondant a la contribution
! du terme de Biot pour un element d'indice blmnb dans therEF
subroutine compute_elementary_F_Biot_therEF(blmnb, coor_ele, u_ele, P_ele, F_Biot_loc)       

   implicit none
  
   ! variables d'entree :
   integer, intent(in) :: blmnb ! indice dans therEF
   real(kind=8), dimension(:, :), intent(in) :: coor_ele ! coordonnees des sommets de l'element
   real(kind=8), dimension(:, :), intent(in) :: u_ele ! vitesses barycentriques aux sommets 
                                                      ! de l'element
   real(kind=8), dimension(:), intent(in) :: P_ele ! pression aux noeuds de
      ! l'element pour le calcul du terme de Biot :
      !   * la pression moyenne (P0) dans le cas linearise
      !   * la pression au debut du pas de temps dans le cas non-lineaire 

   ! variable de sortie :
   real(kind=8), dimension(:), intent(out) :: F_Biot_loc ! vecteur elementaire contenant la
                                                         ! contribution du terme de Biot pour
                                                         ! l'element consdiere
  
   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adaptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
     
         call compute_elementary_F_Biot_ISO(nb, coor_ele, u_ele, P_ele, F_Biot_loc)
    
   end select

end subroutine compute_elementary_F_Biot_therEF

! fonction qui calcule le vecteur elementaire corespondant a la contribution
! du terme d'advection pour un element d'indice blmnb dans therEF
subroutine compute_elementary_F_advection_therEF(blmnb, coor_ele, v_ele, T_ele, F_advection_loc)       

   implicit none
  
   ! variables d'entree :
   integer, intent(in) :: blmnb ! indice dans therEF
   real(kind=8), dimension(:, :), intent(in) :: coor_ele ! coordonnees des sommets de l'element
   real(kind=8), dimension(:, :), intent(in) :: v_ele ! vitesses d'advection aux sommets 
                                                      ! de l'element
   real(kind=8), dimension(:), intent(in) :: T_ele ! temperatures, au debut du pas de temps, aux noeuds de l'element

   ! variable de sortie :
   real(kind=8), dimension(:), intent(out) :: F_advection_loc ! vecteur elementaire contenant la
                                                         ! contribution du terme d'advection pour
                                                         ! l'element consdiere
  
   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adaptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
     
         call compute_elementary_F_advection_ISO(nb, coor_ele, v_ele, T_ele, F_advection_loc)
      case( i_bar ) ! cas d'un element iso parametrique
     
         !call compute_elementary_F_advection_BAR(nb, coor_ele, v_ele, T_ele, F_advection_loc)
   end select

end subroutine compute_elementary_F_advection_therEF


subroutine compute_elementary_field_divergence_therEF(blmnb, coor_ele, vField_ele, Flux_ele)       

   implicit none
  
   ! variables d'entree :
   ! indice dans therEF
   integer, intent(in) :: blmnb                          
   ! coordonnees des sommets de l'element
   real(kind=8), dimension(:, :), intent(in) :: coor_ele, vField_ele

   ! variable de sortie :
   real(kind=8), dimension(:), intent(out) :: Flux_ele
  
   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adaptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
     
         call compute_elementary_field_divergence_ISO(nb, coor_ele, vfield_ele, Flux_ele)

      case( i_bar ) ! cas d'un element iso parametrique
     
         !call compute_elementary_field_divergence_BAR(nb, coor_ele, vfield_ele, Flux_ele)
   end select

end subroutine compute_elementary_field_divergence_therEF



! fonction qui calcule le gradient de temperature aux noeuds d'un element
! d'indice blmnb dans therEF
subroutine compute_grad_T_ele_therEF(blmnb, coor_ele, T_ele, grad_T_ele)       

   implicit none
  
   ! variables d'entree :
   integer, intent(in) :: blmnb ! indice dans therEF
   real(kind=8), dimension(:, :), intent(in) :: coor_ele ! coordonnees des sommets de l'element
   real(kind=8), dimension(:), intent(in) :: T_ele ! temperature aux sommets de l'element
  
   ! variable de sortie :
   real(kind=8), dimension(:, :), intent(out) :: grad_T_ele ! gradient de temperature aux sommets
                                                            ! de l'element
  
   ! variables locales :
   integer :: nb ! numero de l'element dans la liste locale
   
   ! on recupere le numero de l'element dans la liste locale
   nb=get_nb_in_therEFmodel(blmnb)

   ! on appelle la routine de calcul adptee en fonction du type de l'element
   select case(ele_is(blmnb))
   
      case( i_iso ) ! cas d'un element iso parametrique
     
         call compute_grad_T_ele_ISO(nb, coor_ele, T_ele, grad_T_ele)
      case( i_bar ) ! cas d'un element iso parametrique
     
         !call compute_grad_T_ele_BAR(nb, coor_ele, T_ele, grad_T_ele)
   end select

end subroutine compute_grad_T_ele_therEF

! rm's shit for new arch

!> \brief get the number of dofs in the element
function get_N_DOF_therEF(name)
  implicit none
  character(len=5), intent(in) :: name !< [in] name of the element
  integer(kind=4) :: get_N_DOF_therEF  !< [return] number of dof in the element
  !
  integer(kind=4)   :: i,nb
  character(len=26) :: IAM
  !       12345678901234567890123456
  IAM ='a_mecaEF::get_N_DOF_therEF'

  i  = get_nb_in_therEF(NAME)  
  nb = get_nb_in_therEFmodel(i)

  select case (ele_is(i))
  case( i_iso )
      get_N_DOF_therEF = get_N_DOF_therEF_iso(nb)
  case( i_bar )
      get_N_DOF_therEF = get_N_DOF_therEF_bar(nb)
  case default
    call faterr(IAM,'unknown elementary model')
  end select
end function

! --------------------------------------------------------------------------------------
! DA : Fonction de calcul d'une source volumique ajouter aux flux exterieur Fext

SUBROUTINE add_elementary_source(blmnb,ifield,coor_ele,ibdyty,iblmty,SINT)       
!
! fonction qui calcule la matrice de rigidite elementaire d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,ifield
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:) :: SINT

  nb=get_nb_in_therEFmodel(blmnb)
  !print *,'add_elementary_source'
  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL ADD_SOURCE_ISO(nb,ifield,coor_ele,ibdyty,iblmty,SINT)
    CASE( i_bar )
     CALL ADD_SOURCE_BAR(nb,ifield,coor_ele,ibdyty,iblmty,SINT)
  endselect

END SUBROUTINE add_elementary_source

! --------------------------------------------------------------------------------------

!> \brief get the number of dofs of a node of the element
function get_N_DOF_of_NODE_therEF(name, i_node)
  implicit none
  character(len=5), intent(in) :: name        !< [in] name of the element
  integer(kind=4),  intent(in) :: i_node      !< [in] index of the node
  integer(kind=4) :: get_N_DOF_of_NODE_therEF !< [return] number of dof of the node
  !
  integer(kind=4)   :: i,nb
  character(len=33) :: IAM
  !       123456789012345678901234567890123
  IAM = 'a_therEF::get_N_DOF_of_NODE_therEF'

  i  = get_nb_in_therEF(NAME)  
  nb = get_nb_in_therEFmodel(i)

  select case (ele_is(i))
  case( i_iso )
      get_N_DOF_of_NODE_therEF = get_N_DOF_of_NODE_therEF_iso(nb, i_node)
  case( i_bar )
      get_N_DOF_of_NODE_therEF = get_N_DOF_of_NODE_therEF_bar(nb, i_node)
  case default
    call faterr(IAM,'unknown elementary model')
  end select
end function

!============ compute elementary convectivity matrix ===

SUBROUTINE compute_elementary_convection(blmnb,ppsnb,dt,coor_ele,T_ele,DT_ele,ibdyty,iblmty,Fint,Kv,Kc)
!
! fonction qui calcule la matrice de rigidite elementaire d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: nb,blmnb
  INTEGER :: ibdyty,iblmty
  real(kind=8) :: dt
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:,:) :: Kv,Kc
  REAL(kind=8),DIMENSION(:)   :: T_ele,DT_ele
  REAL(kind=8),DIMENSION(:)   :: Fint
  
  integer,dimension(:) :: ppsnb

  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL convective_ISO(nb,ppsnb,dt,coor_ele,T_ele,DT_ele,ibdyty,iblmty,Fint,Kv,Kc)
    CASE( i_bar )
     !CALL convective_BAR(nb,ppsnb,coor_ele,T_ele,DT_ele,ibdyty,iblmty,Fint,Kv,Kc)
  endselect

END SUBROUTINE compute_elementary_convection

! DA :ajout de la recuperation du gradient et du flux

SUBROUTINE gpv2node(blmnb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)       
!
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,required_field,nbfields,nbnodes_stored,mdlnb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: nodalvalues

!                           123456789012345678
  CHARACTER(len=18) :: IAM='a_therEF::gpv2node'


  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
    CASE( i_iso )
     CALL gpv2node_iso(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)        
    CASE( i_bar )
     CALL gpv2node_bar(nb,mdlnb,ibdyty,iblmty,required_field,NodalValues,NbFields,NbNodes_stored)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  END SELECT

END SUBROUTINE gpv2node

!******************************************************************************

SUBROUTINE compute_elementary_fields(blmnb,ppsnb,dt,coor_ele,T_ele,ibdyty,iblmty)       
!
! fonction qui calcule contraintes et variables internes d'un
! element ayant l'indice blmnb dans therEF, de comportement mdlnb, de loi lawnb
! et de position coor_ele
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  INTEGER :: ibdyty,iblmty
  REAL(kind=8),DIMENSION(:,:) :: coor_ele
  REAL(kind=8),DIMENSION(:) :: T_ele
  real(kind=8) :: dt

  integer,dimension(:) :: ppsnb

!                           12345678901234567890123456789012345
  CHARACTER(len=35) :: IAM='a_therEF::compute_elementary_fields'

  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     CALL compute_elementary_fields_ISO (nb,ppsnb,dt,coor_ele,T_ele,ibdyty,iblmty)
  case( i_bar )  
     CALL compute_elementary_fields_BAR (nb,ppsnb,dt,coor_ele,T_ele,ibdyty,iblmty)
! case('shell')  
!
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE compute_elementary_fields

!=======================================================================

SUBROUTINE get_ele_pg_coor(blmnb,coor_ele,coor_pg)
!
! fonction qui retourne la position des points de gauss d'un element
!
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb
  REAL(kind=8),DIMENSION(:,:) :: coor_ele,coor_pg

!                           1234567890123456789012345
  CHARACTER(len=25) :: IAM='a_mecaEF::get_ele_pg_coor'


  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     call get_coor_pg_ISO(nb,coor_ele,coor_pg)
  CASE( i_bar )
     call get_coor_pg_BAR(nb,coor_ele,coor_pg)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE

!=======================================================================

SUBROUTINE get_ele_pg_fields(mdlnb,blmnb,ibdyty,iblmty,required_Field,Field,FieldSize)
!
! fonction qui retourne les champs (defo/contrainte) aux points de gauss d'un element
!
!  
  IMPLICIT NONE
  INTEGER :: blmnb,nb,mdlnb
  INTEGER :: ibdyty,iblmty
  integer :: required_Field,FieldSize
  REAL(kind=8),DIMENSION(:,:) :: field

!                           123456789012345678901234567
  CHARACTER(len=27) :: IAM='a_therEF::get_ele_pg_fields'

  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )

     call gpv_iso(nb,mdlnb,ibdyty,iblmty,required_Field,Field,FieldSize)
  CASE( i_bar )
     call gpv_bar(nb,mdlnb,ibdyty,iblmty,required_Field,Field,FieldSize)
  CASE DEFAULT
    call FATERR(IAM,'unknown elementary model')
  end select

END SUBROUTINE

subroutine check_elementary_ppset(blmnb,ppsnb,ibdyty,iblmty)
  IMPLICIT NONE
  INTEGER              :: blmnb
  INTEGER              :: ibdyty,iblmty
  integer,dimension(:) :: ppsnb
  integer              :: nb
  !                         123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_therEF::check_ppset'
  
  nb=get_nb_in_therEFmodel(blmnb)

  SELECT CASE (ele_is(blmnb))
  CASE( i_iso )
     call check_elementary_ppset_iso(nb,ppsnb,ibdyty,iblmty)
  case(i_bar)
  case default
     call FATERR(IAM,'unknown elementary model')
  endselect
  
end subroutine check_elementary_ppset



END MODULE a_therEF
