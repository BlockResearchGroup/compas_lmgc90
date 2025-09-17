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

!> manages dense (diag | band | full | skyline) linear systems: assembling, solving, etc
!> may delegate solving to external routines (lapack)

MODULE a_matrix

  ! class matrix
  ! Basic computations on matrix, vector
  ! this class defines data type and methods
  !
  ! DOES NOT CONTAIN ANY DATA
  

  USE overall
  USE utilities
  use parameters

  use algebra

  ! full sym
  TYPE T_mat_sym
     !  private
     INTEGER  :: n
     REAL(kind=8),DIMENSION(:,:),POINTER :: V
  END TYPE T_mat_sym
  
  ! band sym
  TYPE T_mat_sym_band
     !  private 
     INTEGER :: bw,n
     REAL(kind=8),DIMENSION(:,:),POINTER :: V 
  END TYPE T_mat_sym_band

  ! band std
  TYPE T_mat_band
     !  private
     INTEGER :: bw,n
     REAL(kind=8),DIMENSION(:,:),POINTER :: V
  END TYPE T_mat_band


  ! sky sym
  !
  ! NOUVEAU TYPE DE MATRICE : STOCKEE EN LIGNE DE CIEL LIGNE PAR LIGNE
  ! LA PARTIE TRIANGULAIRE INFERIEURE DE LA MATRICE DE SOUPLESSE EST STOCKEE DANS LE VECTEUR V
  ! LA COMPOSANTE i DU VECTEUR LI DONNE LE NOMBRE D'ELEMENTS -1 APPARTENANT AU PROFIL SUR LA LIGNE i
  ! LE COMPOSANTE i DU VECTEUR INDIAG DONNE LA PLACE DE L ELEMENT DIAGONAL DE LA LIGNE i DANS LE VECTEUR V
  ! n REPRESENTE LA TAILLE DE V
  ! IDEM POUR LES GRANDEURS VT,LJ,INDIAGJ et nt MAIS SEULEMENT UTILISES LORS DE LA FACTORISATION POUR STOCKER LA MATRICE LT. 
  !
  !  thanks to FK (Francois Kuss LMA)
  !
  TYPE :: T_mat_sym_sky

     INTEGER                              :: n,nt   !n taille de v, nt taille la factorisee
     REAL(kind=8),DIMENSION(:),POINTER    :: V,VT
     INTEGER,DIMENSION(:),POINTER         :: LI,LJ
     INTEGER,DIMENSION(:),POINTER         :: INDIAG,INDIAGJ

  END TYPE T_mat_sym_sky
  !
  
  ! diag
  !
  ! Nouveau type de matrice: matrice diagonale
  ! Les composantes diagonales sont stockees dans le vecteur V
  !
  TYPE T_mat_diag
    !  private 
    ! nombre de ddl (i.e. nombre de coefficients diagonaux
    INTEGER :: n 
    ! vecteur contenant les coefficients digonaux
    REAL(kind=8), DIMENSION(:), POINTER :: V 
  END TYPE T_mat_diag


  !fd parametres internes a ce module (pas necessite de passer ca dans parameter)
  ! pour eviter des tests sur chaine de caractere
  integer(kind=4), parameter :: i_sym_full = 1, &
                                i_sym_band = 2, &
                                i_sym_sky_ = 3, &
                                i_std_band = 4, &
                                i_diag     = 5 

  ! fd 03/10/08
  ! type matrice generique qui vise a etre utilisee partout
  ! elle contient la matrice de depart, mais aussi 
  ! celle ou on a applique les cdl et la factorisee, etc

  TYPE :: G_matrix
    INTEGER :: id
    LOGICAL :: is_not_factorized      

    TYPE(T_mat_sym),POINTER      :: sym_full   !id == i_sym_full
    TYPE(T_mat_sym_band),POINTER :: sym_band   !id == i_sym_band
    TYPE(T_mat_sym_sky),POINTER  :: sym_sky_   !id == i_sym_sky_
    TYPE(T_mat_band),POINTER     :: std_band   !id == i_std_band
    TYPE(T_mat_diag),POINTER     :: diag       !id == i_diag

    !fd manque les matrices diagonales 3 et 6 pour les rigides.

    REAL(kind=8),DIMENSION(:),POINTER   :: scale    ! preconditioning vector
    INTEGER     ,DIMENSION(:),POINTER   :: ipiv     !
    CHARACTER(len=1)                    :: isprecon

    !fd pour stocker les matrices avec conditions aux limites et factorisees 
    !fd a voir la connerie du profil 
    REAL(kind=8), DIMENSION(:)  , POINTER :: V,VT           ! pour sky
    REAL(kind=8), DIMENSION(:)  , POINTER :: V_ddl,V_fact   ! pour diag
    REAL(kind=8), DIMENSION(:,:), POINTER :: M_ddl,M_fact   ! pour band & full

    !le 21/07/2010
    !gestion renumerotation des ddl 
     logical :: with_perm
     
    ! map
    !  old = perm(new) 
    !  new = inv_perm(old)
    integer,dimension(:),pointer :: perm,inv_perm
    REAL(kind=8), DIMENSION(:)  , POINTER :: Vaux

  END TYPE G_matrix

  LOGICAL :: itchatche=.FALSE.

  interface G_assemb
    module procedure G_assemb_from_mat, &
                     G_assemb_from_elementary_mat
  end interface G_assemb

  !--
  INTERFACE new_matrix
    MODULE PROCEDURE new_mat_sym, & 
                     new_mat_sym_band, &
                     new_mat_sym_sky, & 
                     new_mat_band, &
                     new_mat_diag
  END INTERFACE

  INTERFACE zero_matrix
    MODULE PROCEDURE zero_mat_sym, &
                     zero_mat_sym_band, &
                     zero_mat_sym_sky, &
                     zero_mat_band, &
                     zero_mat_diag
  END INTERFACE

  INTERFACE free_matrix
    MODULE PROCEDURE free_mat_sym, &
                     free_mat_sym_band, &
                     free_mat_band, & 
                     free_mat_sym_sky, &
                     free_mat_diag 
  END INTERFACE

  INTERFACE add_to_matrix
    MODULE PROCEDURE add_to_mat_sym, &
                     add_to_mat_sym_band, &
                     add_to_mat_sym_sky, &
                     add_to_mat_band, & 
                     add_to_mat_diag
  END INTERFACE

  INTERFACE x_solve_linear_system
    MODULE PROCEDURE x_solve_mat_sym, &
                     x_solve_mat_sym_band, &
                     x_solve_mat_sym_sky, &
                     x_solve_mat_band, &
                     x_solve_mat_diag
  END INTERFACE

CONTAINS

!!!--------------------------------------------------------------------------------
!!! la G_matrix contient tout: xxx, xxx_ddl, xxx_fact
!!!
!!! Construction d'une matrice en 3 etapes: 
!!!  - G_declare  declaration (type)
!!!  - G_settle   calcul profil et renumerotation 
!!!  - G_build    allocation
!!!
!!!--------------------------------------------------------------------------------
!!!

  !> declare a G_matrix
  !> type : matrix shape sym_full,sym_band,sym_skyline,diagonal,...
  !> order: nb of equations 
  !> permutation management, to reduce band width

  SUBROUTINE G_declare(G,TYPE,order,with_perm,perm,inv_perm)
    IMPLICIT NONE
    TYPE(G_matrix)       :: G
    CHARACTER(len=8)     :: TYPE
    INTEGER              :: order
    logical              :: with_perm
    integer,dimension(:) :: perm,inv_perm

                             !12345678901234567890123456
    CHARACTER(len=26) :: IAM='a_MATRIX::declare_G_matrix'

    NULLIFY(G%sym_full)
    NULLIFY(G%sym_band)
    NULLIFY(G%sym_sky_)
    NULLIFY(G%std_band)
    NULLIFY(G%diag)

    NULLIFY(G%scale)
    NULLIFY(G%ipiv)

    NULLIFY(G%V,G%VT)
    NULLIFY(G%V_fact,G%V_ddl)
    NULLIFY(G%M_fact,G%M_ddl)

    IF (itchatche) THEN
      PRINT*,'a_MATRIX declaration G_matrix:'
      PRINT*,' -type:   ',TYPE
      PRINT*,' -taille: ',order
      PRINT*,' -permutation: ',with_perm
    ENDIF

    SELECT CASE (TYPE)
    CASE ('sym_full')
       G%id=i_sym_full
       ALLOCATE(G%sym_full)
       G%sym_full%n=order
       NULLIFY(G%sym_full%V)

    CASE ('sym_band')
       G%id=i_sym_band
       ALLOCATE(G%sym_band)
       G%sym_band%bw=0
       G%sym_band%n=order
       NULLIFY(G%sym_band%V)

    CASE('sym_sky_')
       G%id=i_sym_sky_
       ALLOCATE(G%sym_sky_)
       ALLOCATE(G%sym_sky_%LI(order))
       G%sym_sky_%LI=0

       ALLOCATE(G%sym_sky_%INDIAG((order)))
       G%sym_sky_%INDIAG=0

       NULLIFY(G%sym_sky_%V, &
               G%sym_sky_%VT, &
               G%sym_sky_%LJ, &
               G%sym_sky_%INDIAGJ)

    CASE('std_band')
       G%id=i_std_band
       ALLOCATE(G%std_band)
       G%std_band%bw=0
       G%std_band%n=order
       NULLIFY(G%std_band%V)

    CASE('diagonal')
       G%id=i_diag
       ALLOCATE(G%diag)
       G%diag%n=order
       NULLIFY(G%diag%V)
 
    CASE default
      CALL LOGMES('you give '//TYPE)
      CALL LOGMES('Error '//IAM//' : unknown matrix type')
    END SELECT

    !fd new 21/07/2010

    allocate(G%Vaux(order))

    G%with_perm=with_perm
    if (with_perm) then
      allocate(G%perm(order),G%inv_perm(order)) 
      G%perm = perm
      G%inv_perm = inv_perm
    else
      nullify(G%perm,G%inv_perm) 
    endif

  END SUBROUTINE

  !> computes profile scanning elementary matrix
  SUBROUTINE G_settle(G,i4_list_in)
    IMPLICIT NONE
    TYPE(G_matrix)       :: G
    INTEGER,DIMENSION(:) :: i4_list_in ! edof2gdof

                             !12345678901234567890123456
    CHARACTER(len=25) :: IAM='a_MATRIX::settle_G_matrix'

    INTEGER :: itmp,i,j,glob_i,glob_j
    INTEGER :: ltempo

    INTEGER,DIMENSION(:),allocatable :: i4_list ! edof2gdof + perm


    allocate(i4_list(size(i4_list_in)))

    if (G%with_perm) then
      do i=1,size(i4_list_in)
        i4_list(i) = G%inv_perm(i4_list_in(i))
      enddo
    else
      i4_list = i4_list_in
    endif
   
    SELECT CASE (G%id)
    CASE (i_sym_full)

        IF (itchatche) PRINT*,'a_MATRIX ajout element sym_full'

    CASE (i_sym_band)

        itmp = 1 + MAXVAL(i4_list) - MINVAL(i4_list)
        IF (itmp .GT. G%sym_band%bw) G%sym_band%bw=itmp

        IF (itchatche) PRINT*,'a_MATRIX ajout element sym_band'

    CASE(i_sym_sky_)

       ! construction DU PROFIL LI

         DO i=1,SIZE(i4_list)
           glob_i= i4_list(i)

           DO j=1,SIZE(i4_list)
             glob_j=i4_list(j)

             IF (glob_i.GT.glob_j) THEN
               itmp=glob_i-glob_j
               IF (itmp>G%sym_sky_%LI(glob_i)) G%sym_sky_%LI(glob_i)=itmp
             END IF

           END DO
         END DO

         IF (itchatche) PRINT*,'a_MATRIX ajout element sym_sky'

    CASE(i_std_band)

        itmp = 1 + 2*(MAXVAL(i4_list) - MINVAL(i4_list))
        IF (itmp .GT. G%std_band%bw) G%std_band%bw=itmp

        IF (itchatche) PRINT*,'a_MATRIX ajout element std_band'

    CASE(i_diag)

        IF (itchatche) PRINT*,'a_MATRIX ajout element diag'

    CASE default
      CALL LOGMES('Error '//IAM//': unknown matrix type')
    END SELECT

    deallocate(i4_list)

  END SUBROUTINE

  !> allocates and initialize matrix
  SUBROUTINE G_build(G)
    IMPLICIT NONE
    TYPE(G_matrix)       :: G
                             !12345678901234567890123456
    CHARACTER(len=24) :: IAM='a_MATRIX::build_G_matrix'
    INTEGER :: errare

    INTEGER :: i,hbw

    SELECT CASE (G%id)
    CASE (i_sym_full)

      ALLOCATE(G%sym_full%V(G%sym_full%n,G%sym_full%n), &
               G%M_ddl(G%sym_full%n,G%sym_full%n), &
               G%M_fact(G%sym_full%n,G%sym_full%n), &
               stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating sym_full')
      END IF

      G%sym_full%V=0.d0 
      G%M_ddl=0.d0 
      G%M_fact=0.d0 

      ALLOCATE(G%scale(G%sym_full%n),stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating scale')
      END IF

      G%scale=0.d0 

      IF (itchatche) THEN
        PRINT*,'a_MATRIX matrice sym_full construite'
        PRINT*,' -ordre:            ',G%sym_full%n
      ENDIF

    CASE (i_sym_band)

      ALLOCATE(G%sym_band%V(G%sym_band%bw,G%sym_band%n), &
               G%M_ddl(G%sym_band%bw,G%sym_band%n), &
               G%M_fact(G%sym_band%bw,G%sym_band%n), &
               stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating sym_band')
      END IF

      G%sym_band%V=0.d0 
      G%M_ddl=0.d0 
      G%M_fact=0.d0 

      ALLOCATE(G%scale(G%sym_band%n),stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating scale')
      END IF

      G%scale=0.d0 

      IF (itchatche) THEN
        PRINT*,'a_MATRIX matrice sym_band construite'
        PRINT*,' -largeur de bande: ',G%sym_band%bw
        PRINT*,' -ordre:            ',G%sym_band%n
      ENDIF

    CASE(i_sym_sky_)

      ! construction DE INDIAG

      G%sym_sky_%INDIAG(1)=1
     
      DO i=2,size(G%sym_sky_%INDIAG)
        G%sym_sky_%INDIAG(i)=G%sym_sky_%INDIAG(i-1)+G%sym_sky_%LI(i)+1
      END DO

      G%sym_sky_%n = G%sym_sky_%INDIAG(size(G%sym_sky_%INDIAG))

      ALLOCATE(G%sym_sky_%V(G%sym_sky_%n), &
               G%V(G%sym_sky_%n), &
               stat=errare)

      IF (errare .NE. 0) THEN
        CALL FATERR(IAM,'error allocating mat_sym_sky')
      END IF

      G%sym_sky_%V=0.d0
      G%V=0.d0

    CASE(i_std_band)

      hbw = (G%std_band%bw - 1)/2

      ALLOCATE(G%std_band%V(G%std_band%bw,G%std_band%n), &
               G%M_ddl(G%std_band%bw,G%std_band%n), &
               G%M_fact(hbw+G%std_band%bw,G%std_band%n), &
               stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating std_band')
      END IF

      G%std_band%V=0.d0 
      G%M_ddl=0.d0 
      G%M_fact=0.d0 

      
      ALLOCATE(G%ipiv(G%std_band%n),stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating ipiv')
      END IF

      G%ipiv=0

      IF (itchatche) THEN
        PRINT*,'a_MATRIX matrice std_band construite'
        PRINT*,' -largeur de bande: ',G%std_band%bw
        PRINT*,' -ordre:            ',G%std_band%n
      ENDIF


    CASE(i_diag)

      ALLOCATE(G%diag%V(G%diag%n), &
               G%V_ddl(G%diag%n), &
               G%V_fact(G%diag%n), &
               stat=errare)    
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating diag')
      END IF

      G%diag%V=0.d0 
      G%V_ddl=0.d0 
      G%V_fact=0.d0 

      IF (itchatche) THEN
        PRINT*,'a_MATRIX matrice diag construite'
        PRINT*,' -ordre:            ',G%std_band%n
      ENDIF

    CASE default
      CALL LOGMES('Error '//IAM//': unknown matrix type')
    END SELECT

  END SUBROUTINE

  !> free memory allocated to a G_matrix
  subroutine G_free(G)

    implicit none

    type(G_matrix)    :: G
                             !1234567890123456
    character(len=16) :: IAM='a_MATRIX::G_free'

    if (associated(G%sym_full)) then
       call free_mat_sym(G%sym_full)
       deallocate(G%sym_full)
       nullify(G%sym_full)
    end if

    if (associated(G%sym_band)) then
       call free_mat_sym_band(G%sym_band)
       deallocate(G%sym_band)
       nullify(G%sym_band)
    end if

    if (associated(G%sym_sky_)) then
       call free_mat_sym_sky(G%sym_sky_)
       deallocate(G%sym_sky_)
       nullify(G%sym_sky_)
    end if

    if (associated(G%std_band)) then
       call free_mat_band(G%std_band)
       deallocate(G%std_band)
       nullify(G%std_band)
    end if

    if (associated(G%diag)) then
       call free_mat_diag(G%diag)
       deallocate(G%diag)
       nullify(G%diag)
    end if
    if (associated(G%scale)) then
       deallocate(G%scale)
       nullify(G%scale)
    end if
    if (associated(G%ipiv)) then
       deallocate(G%ipiv)
       nullify(G%ipiv)
    end if
    if (associated(G%V)) then
       deallocate(G%V)
       nullify(G%V)
    end if
    if (associated(G%Vaux)) then
       deallocate(G%Vaux)
       nullify(G%Vaux)
    end if
    if (associated(G%VT)) then
       deallocate(G%VT)
       nullify(G%VT)
    end if
    if (associated(G%V_fact)) then
       deallocate(G%V_fact)
       nullify(G%V_fact)
    end if
    if (associated(G%V_ddl)) then
       deallocate(G%V_ddl)
       nullify(G%V_ddl)
    end if
    if (associated(G%M_fact)) then
       deallocate(G%M_fact)
       nullify(G%M_fact)
    end if
    if (associated(G%M_ddl)) then
       deallocate(G%M_ddl)
       nullify(G%M_ddl)
    end if

   if (associated(G%perm)) then
       deallocate(G%perm)
       nullify(G%perm)
    end if
    if (associated(G%inv_perm)) then
       deallocate(G%inv_perm)
       nullify(G%inv_perm)
    end if

  end subroutine G_free

  !> set matrix term to zero
  SUBROUTINE G_zero(G)
    IMPLICIT NONE
    TYPE(G_matrix) :: G
                             !1234567890123456
    CHARACTER(len=16) :: IAM='a_MATRIX::G_zero'

    SELECT CASE (G%id)
    CASE(i_sym_full)
      CALL zero_mat_sym(G%sym_full)

    CASE(i_sym_band)
      CALL zero_mat_sym_band(G%sym_band)

    CASE(i_sym_sky_)
      CALL zero_mat_sym_sky(G%sym_sky_)

    CASE(i_std_band)
      CALL zero_mat_band(G%std_band)

    CASE(i_diag)
      CALL zero_mat_diag(G%diag)

    CASE default
       print*,G%id
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT
  END SUBROUTINE


!!! ---

  SUBROUTINE G_assemb_from_mat(G,emat,edof2gdof_in)
    IMPLICIT NONE
    TYPE(G_matrix) :: G
    REAL(kind=8),DIMENSION(:,:) :: emat
    INTEGER,DIMENSION(:)        :: edof2gdof_in

                             !123456789012345678901234567
    CHARACTER(len=27) :: IAM='a_MATRIX::G_assemb_from_mat'

    integer :: i
    INTEGER,DIMENSION(:),allocatable :: edof2gdof ! edof2gdof + perm

    allocate(edof2gdof(size(edof2gdof_in)))
    if (G%with_perm) then
      do i=1,size(edof2gdof_in)
        edof2gdof(i) = G%inv_perm(edof2gdof_in(i))
      enddo
    else
      edof2gdof = edof2gdof_in
    endif


    SELECT CASE (G%id)
    CASE(i_sym_full)
      CALL assemb_mat_sym(G%sym_full,emat,edof2gdof)
    CASE(i_sym_band)
      CALL assemb_mat_sym_band(G%sym_band,emat,edof2gdof)
    CASE(i_sym_sky_)
      CALL assemb_mat_sym_sky(G%sym_sky_,emat,edof2gdof)
    CASE(i_std_band)
      CALL assemb_mat_band(G%std_band,emat,edof2gdof)
    CASE(i_diag)
      CALL assemb_mat_diag(G%diag, emat(:,1), edof2gdof)
    CASE default
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT

    deallocate(edof2gdof)

  END SUBROUTINE

  SUBROUTINE G_assemb_from_elementary_mat(G,emat,edof2gdof_in)
    IMPLICIT NONE
    TYPE(G_matrix) :: G
    type(G_elementary_matrix) :: emat
    INTEGER,DIMENSION(:)      :: edof2gdof_in
    !
                             !12345678901234567890123456789012345678
    CHARACTER(len=38) :: IAM='a_MATRIX::G_assemb_from_elementary_mat'

    integer :: i
    INTEGER,DIMENSION(:),allocatable :: edof2gdof ! edof2gdof + perm

    allocate(edof2gdof(size(edof2gdof_in)))
    if (G%with_perm) then
      do i=1,size(edof2gdof_in)
        edof2gdof(i) = G%inv_perm(edof2gdof_in(i))
      enddo
    else
      edof2gdof = edof2gdof_in
    endif


    SELECT CASE (G%id)
    CASE(i_sym_full)
      CALL assemb_elementary_mat_sym(G%sym_full,emat,edof2gdof)
    CASE(i_sym_band)
      CALL assemb_elementary_mat_sym_band(G%sym_band,emat,edof2gdof)
    CASE(i_diag)
      CALL assemb_elementary_mat_diag(G%diag, emat, edof2gdof)
    CASE default
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT

    deallocate(edof2gdof)

  END SUBROUTINE
!!! ---
  SUBROUTINE G_add(G,tmp,i_in,j_in)
    IMPLICIT NONE
    TYPE(G_matrix) :: G
    INTEGER                     :: i_in,j_in
    REAL(kind=8)                :: tmp

                             !1234567890123456
    CHARACTER(len=16) :: IAM='a_MATRIX::G_zero'

    integer :: i,j
    
    if (G%with_perm) then
      i=G%inv_perm(i_in)
      j=G%inv_perm(j_in)
    else
      i=i_in
      j=j_in
    endif


    SELECT CASE (G%id)
    CASE(i_sym_full)
      CALL add_to_mat_sym(G%sym_full,tmp,i,j)
    CASE(i_sym_band)
      CALL add_to_mat_sym_band(G%sym_band,tmp,i,j)
    CASE(i_sym_sky_)
      CALL add_to_mat_sym_sky(G%sym_sky_,tmp,i,j)
    CASE(i_std_band)
      CALL add_to_mat_band(G%std_band,tmp,i,j)
    CASE(i_diag)
      CALL add_to_mat_diag(G%diag,tmp,i,j)
    CASE default
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT
  END SUBROUTINE


  !> once the matrix is assembled computes hiden _ddl and _fact matrices 
  SUBROUTINE G_store(G)
    
    IMPLICIT NONE
    TYPE(G_matrix) :: G
                             !12345678901234567
    CHARACTER(len=17) :: IAM='a_MATRIX::G_store'
    INTEGER           :: errare
    INTEGER           :: i,j,n,bw,hbw

    G%is_not_factorized = .TRUE.

    SELECT CASE (G%id)
    !!!----------------
    CASE(i_sym_full)
       IF (ASSOCIATED(G%M_ddl)) THEN
          IF (G%sym_full%n /= SIZE(G%M_ddl,dim=1)) THEN
             DEALLOCATE(G%M_ddl,G%M_fact)
             ALLOCATE(G%M_ddl(G%sym_full%n,G%sym_full%n), &
                      G%M_fact(G%sym_full%n,G%sym_full%n), &
                      stat=errare)
             IF (errare /= 0) THEN
                CALL FATERR(IAM,'error allocating sym_full')
             END IF
          END IF
       ELSE 
          ALLOCATE(G%M_ddl(G%sym_full%n,G%sym_full%n), &
                   G%M_fact(G%sym_full%n,G%sym_full%n), &
                   stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating sym_full')
          END IF
       END IF
       
       n  = G%sym_full%n

       DO j = 1,n
          DO i = 1,n
             G%M_ddl(i,j)  = G%sym_full%V(i,j)
             G%M_fact(i,j) = 0.d0
          END DO
       END DO
    !!!----------------
    CASE(i_sym_band)

       !!print*,'taille ',G%sym_band%n,G%sym_band%bw

       IF (ASSOCIATED(G%M_ddl)) THEN
          IF (G%sym_band%bw /= SIZE(G%M_ddl,dim=1) .OR. &
              G%sym_band%n /= SIZE(G%M_ddl,dim=2)) THEN
             DEALLOCATE(G%M_ddl,G%M_fact)
             ALLOCATE(G%M_ddl(G%sym_band%bw,G%sym_band%n), &
                      G%M_fact(G%sym_band%bw,G%sym_band%n), &
                      stat=errare)
             IF (errare /= 0) THEN
                CALL FATERR(IAM,'error allocating sym_band')
             END IF
          ENDIF
       ELSE 
          ALLOCATE(G%M_ddl(G%sym_band%bw,G%sym_band%n), &
                   G%M_fact(G%sym_band%bw,G%sym_band%n), &
                   stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating sym_band')
          END IF
       ENDIF

       n  = G%sym_band%n
       bw = G%sym_band%bw

       DO j = 1,n
         DO i = 1,bw
             G%M_ddl(i,j)  = G%sym_band%V(i,j)
             G%M_fact(i,j) = 0.d0
         END DO
       END DO

    !!!----------------
    CASE(i_sym_sky_)
       IF (ASSOCIATED(G%V)) THEN
          IF (G%sym_sky_%n /= SIZE(G%V)) THEN
             DEALLOCATE(G%V)
             ALLOCATE(G%V(G%sym_sky_%n),stat=errare)
             IF (errare /= 0) THEN
                CALL FATERR(IAM,'error allocating sym_sky_')
             END IF
          END IF
       ELSE 
          ALLOCATE(G%V(G%sym_sky_%n),stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating sym_sky_')
          END IF
       END IF

       DO i = 1,G%sym_sky_%n
          G%V(i)  = G%sym_sky_%V(i) 
       END DO

    !!!----------------
    CASE(i_std_band)
      hbw = (G%std_band%bw - 1)/2
      IF (ASSOCIATED(G%M_ddl)) THEN
        IF (G%std_band%bw /= SIZE(G%M_ddl,dim=1) .OR. &
            G%std_band%n /= SIZE(G%M_ddl,dim=2)) THEN
          DEALLOCATE(G%M_ddl,G%M_fact)
          ALLOCATE(G%M_ddl(G%std_band%bw,G%std_band%n), &
                   G%M_fact(hbw+G%std_band%bw,G%std_band%n), &
                   stat=errare)
          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating std_band')
          END IF
        ENDIF
      ELSE 
        ALLOCATE(G%M_ddl(G%std_band%bw,G%std_band%n), &
                 G%M_fact(hbw+G%std_band%bw,G%std_band%n), &                 
                 stat=errare)
        IF (errare /= 0) THEN
          CALL FATERR(IAM,'error allocating std_band')
        END IF
      ENDIF

      n  = G%std_band%n
      bw = G%std_band%bw

      DO j = 1,n
        DO i = 1,bw
          G%M_ddl(i,j)  = G%std_band%V(i,j)
        END DO
        G%M_fact(:,j) = 0.d0
      END DO

    !!!----------------
    CASE(i_diag)
       IF (ASSOCIATED(G%V_ddl)) THEN
          IF (G%diag%n /= SIZE(G%V_ddl)) THEN
             DEALLOCATE(G%V_fact,G%V_ddl)
             ALLOCATE(G%V_ddl(G%diag%n), &
                      G%V_fact(G%diag%n), &
                      stat=errare)
             IF (errare /= 0) THEN
                CALL FATERR(IAM,'error allocating diag')
             END IF
          ENDIF
       ELSE 
          ALLOCATE(G%V_ddl(G%diag%n), &
                   G%V_fact(G%diag%n), &
                   stat=errare)
          IF (errare /= 0) THEN
             CALL FATERR(IAM,'error allocating diag')
          END IF
       ENDIF

       n  = G%diag%n

       DO i = 1,n
          G%V_ddl(i)  = G%diag%V(i)  
          G%V_fact(i) = 0.d0
       END DO

    !!!----------------
    CASE default
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT
  END SUBROUTINE

  !> apply drv_dof to _ddl matrix  
  SUBROUTINE G_apply_drvdof(G,idrvdof_in)
    IMPLICIT NONE
    TYPE(G_matrix) :: G
    INTEGER        :: idrvdof_in
                             !123456789012345678901234
    CHARACTER(len=24) :: IAM='a_MATRIX::G_apply_drvdof'

    INTEGER                :: i,j,n,bw,bwh,k,l,ji

    integer :: idrvdof

    if (G%with_perm) then
      idrvdof=G%inv_perm(idrvdof_in)
    else
      idrvdof=idrvdof_in
    endif

    SELECT CASE (G%id)
    CASE(i_sym_full)
      n=G%sym_full%n
      ! -------------------------
      ! mise a zero de la colonne
      ! -------------------------
      DO i=1,MAX(1,idrvdof-1)
        G%M_ddl(i,idrvdof)=0.D0
      ENDDO
      DO i=MIN(idrvdof+1,n),n
        G%M_ddl(i,idrvdof)=0.D0
      ENDDO
      ! -----------------------
      ! mise a zero de la ligne
      ! -----------------------
      DO j=1,MAX(1,idrvdof-1)
        G%M_ddl(idrvdof,j)=0.D0
      ENDDO  
      DO j=MIN(idrvdof+1,n),n
        G%M_ddl(idrvdof,j)=0.D0
      ENDDO  
      ! ------------------------  
      ! mise a 1 de la diagonale
      ! ------------------------ 
      G%M_ddl(idrvdof,idrvdof)=1.D0

    CASE(i_sym_band)
      n=G%sym_band%n
      bw=G%sym_band%bw

      ! -------------------------
      ! mise a zero de la colonne
      ! -------------------------
      DO i=MAX(1,idrvdof-(bw-1)),idrvdof-1
        G%M_ddl(bw+i-idrvdof,idrvdof)=0.D0
      ENDDO
      ! -----------------------
      ! mise a zero de la ligne
      ! -----------------------
      DO j=idrvdof+1,MIN(n,idrvdof+(bw-1))    
        G%M_ddl(bw+idrvdof-j,j)=0.D0
      ENDDO  
      ! ------------------------  
      ! mise a 1 de la diagonale
      ! ------------------------ 
      G%M_ddl(bw,idrvdof)=1.D0

    CASE(i_sym_sky_)
      n=size(G%sym_sky_%LI)
      ! --------------------------
      !  mise a zero de la colonne
      ! --------------------------
      DO i=1,n
        IF (i>1) THEN
          ji=i-G%sym_sky_%LI(i)
        ELSE
          ji=1
        END IF
        IF ((idrvdof>=ji).AND.(idrvdof<i)) THEN 
          l=0
          DO l=0,1+G%sym_sky_%LI(i)
            IF (ji+l==idrvdof) THEN
              EXIT
            END IF
          END DO
          k=G%sym_sky_%INDIAG(i-1)+1+l
          G%V(k)=0.
        END IF
      END DO
      ! -----------------------
      ! mise a zero de la ligne
      ! -----------------------

      IF (idrvdof /=1) THEN
        DO k=G%sym_sky_%INDIAG(idrvdof-1)+1,G%sym_sky_%INDIAG(idrvdof)-1
          G%V(k)=0.
        END DO
      ENDIF
      ! ------------------------  
      ! mise a 1 de la diagonale
      ! -- 
      k=G%sym_sky_%INDIAG(idrvdof) 
      G%V(k)=1.

    CASE(i_std_band)
      n=G%std_band%n
      bw=G%std_band%bw
      bwh=(bw-1)/2

      ! --------------------------
      !  mise a zero de la colonne
      ! --------------------------
      DO i=MAX(1,idrvdof-bwh),MIN(n,idrvdof+bwh)
        G%M_ddl(bwh+1+i-idrvdof,idrvdof)=0.D0
      ENDDO
      ! -----------------------
      ! mise a zero de la ligne
      ! -----------------------
      DO j=MAX(1,idrvdof-bwh),MIN(n,idrvdof+bwh)
        G%M_ddl(bwh+1+idrvdof-j,j)=0.D0
      ENDDO
      ! ------------------------  
      ! mise a 1 de la diagonale
      ! ------------------------  
      G%M_ddl(bwh+1,idrvdof)=1.D0

    CASE(i_diag)
      G%V_ddl(idrvdof)=1.D0

    CASE default
        CALL FATERR(IAM,'unknown type of matrix')
    END SELECT


  END SUBROUTINE

  !> solve the linear system
  !> if necessary the system is factorized in _fact matrix
  SUBROUTINE G_solve_linear_system(G,V,info)
    IMPLICIT NONE
    TYPE(G_matrix)            :: G
    REAL(kind=8),DIMENSION(:) :: V
    INTEGER                   :: info          
                             !12345678901234567890123456
    CHARACTER(len=24) :: IAM='a_MATRIX::build_G_matrix'

    integer :: i

    if (G%with_perm) then
      ! parcourt des index reels
      do i=1,size(V) 
        G%Vaux(G%inv_perm(i)) = V(i)
      enddo
    else
      G%Vaux = V
    endif

    SELECT CASE (G%id)
     CASE (i_sym_full)
       IF (G%is_not_factorized) THEN
         CALL G_solve_sym_full(G,G%Vaux,'E',info)
         G%is_not_factorized = .FALSE.
       ELSE
         CALL G_solve_sym_full(G,G%Vaux,'F',info)
       ENDIF

     CASE (i_sym_band)
       IF (G%is_not_factorized) THEN
         CALL G_solve_sym_band(G,G%Vaux,'E',info)
         G%is_not_factorized = .FALSE.
       ELSE
         CALL G_solve_sym_band(G,G%Vaux,'F',info)
       ENDIF

     CASE(i_sym_sky_)
     ! thanks to FK
       IF (G%is_not_factorized) THEN
         CALL G_solve_sym_sky_(G,G%Vaux,'E',info)
         G%is_not_factorized = .FALSE.
       ELSE
         CALL G_solve_sym_sky_(G,G%Vaux,'F',info)
       ENDIF

     CASE (i_std_band)
       IF (G%is_not_factorized) THEN
         CALL G_solve_std_band(G,G%Vaux,'E',info)
         G%is_not_factorized = .FALSE.
       ELSE
         CALL G_solve_std_band(G,G%Vaux,'F',info)
       ENDIF

     CASE (i_diag)
       IF (G%is_not_factorized) THEN
         CALL G_solve_diag(G,G%Vaux,'E',info)
         G%is_not_factorized = .FALSE.
       ELSE
         CALL G_solve_diag(G,G%Vaux,'F',info)
       ENDIF

     CASE default
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT

    if (G%with_perm) then
      ! parcourt des index permutes
      do i=1,size(V) 
        V(G%perm(i)) = G%Vaux(i)
      enddo
    else
      v = G%vaux
    endif

  END SUBROUTINE

  
  !> solve a symetric full system using LAPACK
  SUBROUTINE G_solve_sym_full(G,vec,opt,info)
    IMPLICIT NONE
    TYPE(G_matrix)            :: G
    REAL(kind=8),DIMENSION(:) :: vec
    CHARACTER(len=1)          :: opt
    INTEGER                   :: info
    !                                    12345678901234567890123456
    CHARACTER(len=26)         :: IAM='a_matrix::G_solve_sym_full'
    !
    REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               

    ! for lapack
    real(kind=8) :: rcond
    real(kind=8)   , dimension(1) :: ferr, berr
    real(kind=8)   , dimension(:), allocatable :: work
    integer(kind=4), dimension(:), allocatable :: iwork

    rcond = 1.d0
    allocate(work(3*G%sym_full%n))
    allocate(iwork(G%sym_full%n))

    !fd c'est un peu paranoiac ... on pourrait utiliser 
    !fd  *vec en entree et en sortie.
    !fd  *mat%V en entree et en sortie
    !fd  *ne pas passer scale puisque isprecon='N'

    ALLOCATE(vec2(SIZE(vec))) 
    vec2=0.d0

    IF (opt == 'E') G%isprecon='N'

    CALL DPOSVx(opt, 'U', G%sym_full%n, 1, G%M_ddl, G%sym_full%n, G%M_fact, G%sym_full%n, G%isprecon, &
                G%scale, vec, G%sym_full%n, vec2, G%sym_full%n, rcond, ferr, berr, work, iwork, info)

    vec=vec2
    DEALLOCATE(vec2,work,iwork)

  END SUBROUTINE G_solve_sym_full

!!! ---
  SUBROUTINE G_solve_sym_band(G,vec,opt,info)

    IMPLICIT NONE
    TYPE(G_matrix)             :: G
    REAL(kind=8),DIMENSION(:)  :: vec
    CHARACTER(len=1)           :: opt
    INTEGER                    :: info
    integer :: i,j

!                                     123456789012345678901234567890 
    CHARACTER(len=30)          :: IAM='a_matrix::G_solve_mat_sym_band'

    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: m2               
    REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               

!fd c'est un peu paranoiac ... on pourrait utiliser 
!fd  *vec en entree et en sortie.
!fd  *mat%V en entree et en sortie
!fd  *ne pas passer scale puisque isprecon='N'


    ALLOCATE(m2(SIZE(vec),1)) 
    m2(:,1)=vec(:)

    if (opt == 'E') then
      do j=1,G%sym_band%n
        G%M_fact(:,j)=G%M_ddl(:,j)
      enddo

                 ! UPLO, N, KD, AB, LDAB, INFO 
      call DPBTRF('U',G%sym_band%n, G%sym_band%bw-1, G%M_fact,G%sym_band%bw,info)
    endif
 
               ! UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO 
    call DPBTRS( 'U',G%sym_band%n, G%sym_band%bw-1, 1,G%M_fact,G%sym_band%bw,m2,G%sym_band%n,info)

    vec(:)=m2(:,1)

    DEALLOCATE(m2)

  END SUBROUTINE G_solve_sym_band
!!! ---
  SUBROUTINE G_solve_sym_sky_(G,vec,opt,info)
    IMPLICIT NONE
    !
    TYPE(G_matrix)             :: G
    REAL(kind=8),DIMENSION(:)  :: vec
    CHARACTER(len=1)           :: opt
    INTEGER                    :: info

    !                         12345678901234567890123456
    CHARACTER(len=26) :: IAM='a_matrix::G_solve_sym_sky_'
    !
    INTEGER,DIMENSION(:),ALLOCATABLE         :: LJTEMP ! FK
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: Mattemp
    INTEGER :: i,j,k,ji,jj,n,LT
    REAL(kind=8) :: s
    !
    info = 0
    !
    LT=1
    n=size(G%sym_sky_%indiag)
    IF (opt == 'E') THEN

      ! Premier passage 

      !fd print*,'//////////////// FActorisation /////////////////////////////////////'
      !fd print*,'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
      !! FACTORISATION A= L.D.LT=============================================================
      ! LA MATRICE L A LE MEME PROFIL QUE A, ON STOCKE DONC LA MATRICE L DANS A
      ! LES ELEMENTS DIAGONAUX DE L SONT TOUJOURS EGAUX A 1, ON STOCKE DONC D DANS LA DIAGO DE L
      DO i=1,n
        IF (i>1) THEN
           ji=i-G%sym_sky_%INDIAG(i)+G%sym_sky_%INDIAG(i-1)+1
        ELSE
           ji=1
        END IF

        DO j=ji,i-1

           IF (j>1) THEN
              jj=j-G%sym_sky_%INDIAG(j)+G%sym_sky_%INDIAG(j-1)+1
           ELSE
              jj=1
           END IF

           DO k=MAX(ji,jj),j-1
              G%V(G%sym_sky_%INDIAG(i)-i+j)= G%V(G%sym_sky_%INDIAG(i)-i+j) - & 
                  G%V(G%sym_sky_%INDIAG(i)-i+k)*G%V(G%sym_sky_%INDIAG(j)-j+k)
           END DO
        END DO

        DO j=ji,(i-1)
           s=G%V(G%sym_sky_%INDIAG(i)-i+j)/G%V(G%sym_sky_%INDIAG(j))
           G%V(G%sym_sky_%INDIAG(i))=G%V(G%sym_sky_%INDIAG(i))-s*G%V(G%sym_sky_%INDIAG(i)-i+j)
           G%V(G%sym_sky_%INDIAG(i)-i+j)=s
        END DO

     END DO

     IF (LT==0) GOTO 2000

     ! ON STOCKE LA MATRICE LT, AVEC UN STOCKAGE SKYLINE LIGNES, DE MANIERE A Y ACCEDER RAPIDEMENT :
     ! Definition du profil de la matrice LT à partir de celui de la matrice L
     IF (.NOT. ASSOCIATED(G%sym_sky_%LJ)) ALLOCATE(G%sym_sky_%LJ(n))
     G%sym_sky_%LJ=0

     DO j=1,n
        DO i=j,n  !! Peut etre i=j+1,n  ??
           IF (i>1) THEN
              ji=i-G%sym_sky_%INDIAG(i)+G%sym_sky_%INDIAG(i-1)+1
           ELSE
              ji=1
           END IF
           IF (ji<=j) G%sym_sky_%LJ(j)=i-j
        END DO
     END DO

     IF (.NOT. ASSOCIATED(G%sym_sky_%INDIAGJ)) ALLOCATE(G%sym_sky_%INDIAGJ(n))
     G%sym_sky_%INDIAGJ=0

     G%sym_sky_%INDIAGJ(1)=1
     DO i=2,n
        G%sym_sky_%INDIAGJ(i)=G%sym_sky_%INDIAGJ(i-1)+G%sym_sky_%LJ(i-1)+1
     END DO

     G%sym_sky_%nT=G%sym_sky_%INDIAGJ(n)

     IF (ASSOCIATED(G%VT)) DEALLOCATE(G%VT) !LA MATRICE LT EST STOCKEE DANS MAT%VT
     ALLOCATE(G%VT(G%sym_sky_%nT))

     G%VT=0.D0

     ! REMPLISSAGE DE LA MATRICE LT

     k=0
     DO i=1,n
       IF (i>1) THEN
         ji=i-G%sym_sky_%INDIAG(i)+G%sym_sky_%INDIAG(i-1)+1
       ELSE
         ji=1
       END IF

       DO j=ji,i
         k=k+1
         IF (i<=G%sym_sky_%LJ(j)+j) THEN  !i=j2,j=i2
           G%VT(G%sym_sky_%INDIAGJ(j)+i-j)=G%V(k)
           !  Mat_t(j,i)=Mat%V(k)
         END IF
       END DO
     END DO

2000 k=0
   END IF

   !  MONTEE DESCENTE

   ! DESCENTE =====================================

   DO i=1,n
      IF (i>1) THEN
         ji=i-G%sym_sky_%INDIAG(i)+G%sym_sky_%INDIAG(i-1)+1
      ELSE
         ji=1
      END IF
      DO j=ji,i-1 ! ESSAYER DO PRODUCT
        vec(i)=vec(i)-G%V(G%sym_sky_%INDIAG(i)-i+j)*vec(j)
      END DO
   END DO


   ! MONTEE AVEC LT =========================
   IF (LT==1) THEN

     DO k=1,n
       i=n-k+1
       DO j=i+1,MIN(G%sym_sky_%LJ(i)+i,n)
        !print*,i,j
         vec(i)=vec(i)-G%VT(G%sym_sky_%INDIAGJ(i)+j-i)*vec(j)*G%VT(G%sym_sky_%INDIAGJ(i))  ! OK
       END DO

       vec(i)=vec(i)/G%VT(G%sym_sky_%INDIAGJ(i))
 
     END DO

   ELSE

   ! MONTEE AVEC L =============================

     DO k=1,n
       i=n-k+1
       DO j=i+1,n
         ji=j-G%sym_sky_%INDIAG(j)+G%sym_sky_%INDIAG(j-1)+1
         IF (i>=ji) THEN
           vec(i)=vec(i)-G%V(G%sym_sky_%INDIAG(j)-j+i)*vec(j)*G%V(G%sym_sky_%INDIAG(i))
         END IF
       END DO

       vec(i)=vec(i)/(G%V(G%sym_sky_%INDIAG(i)))

     END DO
   END IF

  END SUBROUTINE G_solve_sym_sky_
!!! ---
  SUBROUTINE G_solve_std_band(G,vec,opt,info)
    IMPLICIT NONE
    TYPE(G_matrix)            :: G
    REAL(kind=8),DIMENSION(:) :: vec
    INTEGER                   :: info 
    CHARACTER(len=1)          :: opt
    !                                    12345678901234567890123456 
    CHARACTER(len=26)         :: IAM='a_matrix::G_solve_mat_band'

    REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: m2               

    integer :: j,hbw,lbw

    !lapack
    real(kind=8) :: rcond
    real(kind=8)   , dimension(1) :: ferr, berr
    real(kind=8)   , dimension(:), allocatable :: work, lr, lc
    integer(kind=4), dimension(:), allocatable :: iwork

    rcond = 1.d0
    
!fd c'est un peu paranoiac ... on pourrait utiliser 
!fd  *vec en entree et en sortie.
!fd  *mat%V en entree et en sortie
!fd  *ne pas passer scale puisque isprecon='N'

    ! half band width
    hbw = (G%std_band%bw - 1)/2

    ALLOCATE(m2(SIZE(vec),1)) 
    m2(:,1)=vec(:)

    if (opt == 'E') then
      do j=1,G%std_band%n
        G%M_fact(hbw+1:hbw+G%std_band%bw,j)=G%M_ddl(1:G%std_band%bw,j)
      enddo

                 ! M, N, KL, KU, AB, LDAB, IPIV, INFO 
      call DGBTRF(G%std_band%n, G%std_band%n, hbw, hbw, G%M_fact, hbw+G%std_band%bw, G%ipiv, info)
    endif
 
               ! TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO 
    call DGBTRS('N', G%std_band%n, hbw, hbw, 1, G%M_fact, hbw+G%std_band%bw, G%ipiv, m2, G%std_band%n, info)

    vec(:)=m2(:,1)

    DEALLOCATE(m2)

  END SUBROUTINE G_solve_std_band

!!! ---

  SUBROUTINE G_solve_diag(G,vec,opt,info)
  IMPLICIT NONE
  !
  TYPE(G_matrix)             :: G             ! G: matrice du systeme a resoudre
                                              ! G%V_ddl: matrice du systeme 
                                              ! G%V_fact: inverse matrice du systeme
  !
  REAL(kind=8),DIMENSION(:)  :: vec           ! vec: 
                                              !  * precondition: contient le second
                                              !   membre du systeme  
                                              !   * postcondition: contient la solution
                                              !   du systeme lineaire      
  !
  CHARACTER(len=1)           :: opt           ! permet de choisir le comportement de la fonction:
                                              ! * 'E': on factorise le systeme et on resoud
                                              ! * 'F': on resoud le systeme a l'aide de la matrice deja factorisee
      
  INTEGER                    :: info          ! =0 solution , =1 pas de solution

  !                                  1234567890123456789012
  CHARACTER(len=22)          :: IAM='a_matrix::G_solve_diag'
  !
  INTEGER      :: n, i  ! n: taille du systeme
                        ! i: indice de boucle
  !

!fd c'est un peu paranoiac ... on pourrait utiliser 
!fd  *vec en entree et en sortie.
!fd  *mat%V en entree et en sortie
!fd  *ne pas passer scale puisque isprecon='N'

  info =0
  
  ! on récupère la taille des matrices
  n = G%diag%n

  ! si l'utilisateur veut factoriser la matrice du systeme
  IF (opt == 'E') THEN

    ! dans le cas d'une matrice diagonale, la matrice factorisee calculee est son inverse
    ! pour chaque coefficient diagonal de la matrice du syteme
    DO i=1, n
      ! si le coefficient courant est nul, la matrice n'est pas inversible et le systeme
      ! n'a donc pas de solution
      IF (G%V_ddl(i) <= 0.D0) THEN
        info=1            
        RETURN
      ENDIF
   
      ! sinon, le coefficient diagonal correspondant dans la matrice factorisee est 
      ! l'inverse du coefficient courant
      G%V_fact(i) = 1.D0/G%V_ddl(i)
 
    END DO
  ENDIF

  ! on resoud le systeme, a l'aide de la matrice factorisee
  
  ! la solution du systeme s'obtient en multipliant la matrice du systeme, sous forme
  ! factorisee, par le second membre
  
  vec(:) = G%V_fact(:)*vec(:)
 
  END SUBROUTINE G_solve_diag
!!! ---
  SUBROUTINE G_product_vector(G,vec)
    IMPLICIT NONE
    TYPE(G_matrix) :: G
    REAL(kind=8),DIMENSION(:)  :: vec               
                             !12345678901234567890123456
    CHARACTER(len=26) :: IAM='a_MATRIX::G_product_vector'

    integer :: i

    if (G%with_perm) then
      ! parcourt des indices reels
      do i=1,size(vec) 
        G%Vaux(G%inv_perm(i)) = vec(i)
      enddo
    else
      G%Vaux = Vec
    endif

    SELECT CASE (G%id)
    CASE(i_sym_full)
      CALL product_mat_sym_vector(G%sym_full,G%vaux)
    CASE(i_sym_band)
      CALL product_mat_sym_band_vector(G%sym_band,G%vaux)
    CASE(i_sym_sky_)
      CALL product_mat_sym_sky_vector(G%sym_sky_,G%vaux)
    CASE(i_std_band)
      CALL product_mat_band_vector(G%std_band,G%vaux)
    CASE(i_diag)
      CALL product_mat_diag_vector(G%diag,G%vaux)
    CASE default
       CALL FATERR(IAM,'unknown type of matrix')
    END SELECT

    if (G%with_perm) then
      !parcourt des indices permutes
      do i=1,size(vec) 
        vec(G%perm(i)) = G%Vaux(i)
      enddo
    else
      vec = G%vaux
    endif


  END SUBROUTINE

  ! a function to check the storage of a given matrix
  function G_is_a_sym_full_matrix(G)
    implicit none 
    TYPE(G_matrix) :: G
    logical G_is_a_sym_full_matrix

    if (G%id == i_sym_full) then 
       G_is_a_sym_full_matrix = .TRUE.
    else
       G_is_a_sym_full_matrix = .FALSE.
    endif
  end function 

!----- fin des appels par G_matrix ----

!!!--------------------------------------------------------------------------------
!!!
!!! NEW MATRIX: reservation de l'espace memoire
!!!
!!!--------------------------------------------------------------------------------
  SUBROUTINE new_mat_sym(mat,n)

    IMPLICIT NONE
    INTEGER :: i,j,n,errare
    TYPE(T_mat_sym) :: mat
    !                         123456789012345678901
    CHARACTER(len=21) :: IAM='a_MATRIX::new_mat_sym'
    CHARACTER(len=103) :: cout

    ALLOCATE(mat%V(n,n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym')
    END IF
    
    mat%n=n
    ! mat%V=0.d0
    DO i=1,n
       DO j=1,n
          mat%V(i,j) = 0.D0
       END DO
    END DO

  END SUBROUTINE new_mat_sym
!!!--------------------------------
  SUBROUTINE new_mat_sym_band(mat,bw,n)

    IMPLICIT NONE
    INTEGER              :: i,j,bw,n,errare
    TYPE(T_mat_sym_band) :: mat
    !                          12345678901234567890123456
    CHARACTER(len=26)  :: IAM='a_MATRIX::new_mat_sym_band'
    CHARACTER(len=103) :: cout
    
    ALLOCATE(mat%V(bw,n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym_band')
    END IF
    
    mat%bw = bw
    mat%n  = n
    
    ! mat%V=0.d0
    DO i=1,bw
       DO j=1,n
          mat%V(i,j) = 0.D0
       END DO
    END DO
    
  END SUBROUTINE new_mat_sym_band
!!!--------------------------------
  SUBROUTINE new_mat_band(mat,bw,n)

    IMPLICIT NONE
    INTEGER :: i,j,bw,n,errare
    TYPE(T_mat_band)   :: mat
    !                         1234567890123456789012
    CHARACTER(len=22) :: IAM='a_MATRIX::new_mat_band'

    ALLOCATE(mat%V(bw,n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_band')
    END IF

    mat%bw=bw
    mat%n=n

    !mat%V=0.d0
    DO i=1,bw
       DO j=1,n
          mat%V(i,j) = 0.D0
       END DO
    END DO
  
  END SUBROUTINE new_mat_band
!!!--------------------------------
!!! FK
  SUBROUTINE new_mat_sym_sky(mat,n) 

    IMPLICIT NONE
    INTEGER             :: i,n,errare,nt
    TYPE(T_mat_sym_sky) :: mat
    !                           1234567890123456789012345
    CHARACTER(len=25)   :: IAM='a_MATRIX::new_mat_sym_sky'

    ALLOCATE(mat%V(n),stat=errare)

    IF (errare .NE. 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym_sky')
    END IF

    mat%n=n

    ! mat%V=0.d0
    DO i=1,n
       mat%V(i) = 0.D0
    END DO

  END SUBROUTINE new_mat_sym_sky
!!!--------------------------------
  SUBROUTINE new_mat_diag(mat, n)
    IMPLICIT NONE
    INTEGER :: n,errare
    TYPE(T_mat_diag) :: mat
    !                           12345678901234567890123456
    CHARACTER(len=26) :: IAM='a_MATRIX::new_mat_diag'
    CHARACTER(len=103) :: cout

    ! on alloue le vecteur contenant les coefficients diagonaux
    ALLOCATE(mat%V(n), stat=errare)
  
    ! si l'allocation s'est mal passee
    IF (errare /= 0) THEN
      CALL FATERR(IAM, 'error allocating mat_sym')
    END IF

    mat%n=n
  
    mat%V=0.d0
  
  END SUBROUTINE new_mat_diag   


!!!
!!!--------------------------------------------------------------------------------
!!!
!!! ZERO MATRIX: mise a zero des valeurs contenues dans l'espace memoire
!!!
!!!--------------------------------------------------------------------------------
!!!
  SUBROUTINE zero_mat_sym(mat)
  
    IMPLICIT NONE
    INTEGER         :: n,i,j
    TYPE(T_mat_sym) :: mat 
  
    n  = mat%n

    ! mat%V=0.d0
    DO j=1,n
       DO i=1,n
          mat%V(i,j) = 0.D0
       END DO
    END DO

  END SUBROUTINE zero_mat_sym
!!!--------------------------------
  SUBROUTINE zero_mat_sym_band(mat)
  
    IMPLICIT NONE
    INTEGER              :: bw,n,i,j
    TYPE(T_mat_sym_band) :: mat 

    bw = mat%bw
    n  = mat%n

    ! mat%V=0.d0
    DO j=1,n
       DO i=1,bw
          mat%V(i,j) = 0.D0
       END DO
    END DO

  END SUBROUTINE zero_mat_sym_band
!!!--------------------------------
  SUBROUTINE zero_mat_band(mat)
  
    IMPLICIT NONE
    INTEGER          :: bw,n,i,j
    TYPE(T_mat_band) :: mat 

    bw = mat%bw
    n  = mat%n

    ! mat%V=0.d0
    DO j=1,n
       DO i=1,bw
          mat%V(i,j) = 0.D0
       END DO
    END DO

  END SUBROUTINE zero_mat_band
!!!--------------------------------
  SUBROUTINE zero_mat_sym_sky(mat) 
    ! thanks to FK
    IMPLICIT NONE
    INTEGER             :: n,i
    TYPE(T_mat_sym_sky) :: mat 

    n  = size(mat%V)

    ! mat%V=0.d0
    DO i=1,n
       mat%V(i) = 0.D0
    END DO

  END SUBROUTINE zero_mat_sym_sky
!!!--------------------------------
  SUBROUTINE zero_mat_diag(mat)
    IMPLICIT NONE

    TYPE(T_mat_diag)  :: mat 
  
    ! on remplit la matrice de 0
    mat%V=0.d0

  END SUBROUTINE zero_mat_diag

!!!
!!!--------------------------------------------------------------------------------
!!!
!!! COPY MATRIX: construction par copie
!!!
!!!--------------------------------------------------------------------------------
!!!
  SUBROUTINE copy_mat_sym(mato,matn)
  
    IMPLICIT NONE
    INTEGER            :: i,j,n,errare
    TYPE(T_mat_sym)    :: mato, matn
    !                          123456789012345678901
    CHARACTER(len=21)  :: IAM='a_MATRIX::new_mat_sym'

    n  = mato%n
    IF ( ASSOCIATED(matn%V) ) DEALLOCATE(matn%V)

    ALLOCATE(matn%V(n,n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym')
    END IF

    matn%n = n

    ! matn%V=mato%V  
    DO i=1,n
       DO j=1,n
          matn%V(i,j) = mato%V(i,j)
       END DO
    END DO

  END SUBROUTINE copy_mat_sym
!!!--------------------------------
  SUBROUTINE copy_mat_sym_band(mato,matn)
  
    IMPLICIT NONE
    INTEGER              :: i,j,bw,n,errare
    TYPE(T_mat_sym_band) :: mato, matn
    !                         12345678901234567890123456
    CHARACTER(len=26) :: IAM='a_MATRIX::new_mat_sym_band'

    bw = mato%bw
    n  = mato%n

    IF ( ASSOCIATED(matn%V) ) DEALLOCATE(matn%V)

    ALLOCATE(matn%V(bw,n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym_band')
    END IF
    
    matn%bw = bw
    matn%n  = n

    ! matn%V=mato%V  
    DO i=1,bw
       DO j=1,n
          matn%V(i,j) = mato%V(i,j)
       END DO
    END DO

  END SUBROUTINE copy_mat_sym_band
!!!--------------------------------
  SUBROUTINE copy_mat_band(mato,matn)

    IMPLICIT NONE
    INTEGER           :: i,j,bw,n,errare
    TYPE(T_mat_band)  :: mato,matn
    !                         1234567890123456789012
    CHARACTER(len=22) :: IAM='a_MATRIX::new_mat_band'

    bw = mato%bw
    n  = mato%n

    IF ( ASSOCIATED(matn%V) ) DEALLOCATE(matn%V)

    ALLOCATE(matn%V(bw,n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_band')
    END IF

    matn%bw = bw
    matn%n  = n

    ! matn%V=mato%V  
    DO i=1,bw
       DO j=1,n
          matn%V(i,j) = mato%V(i,j)
       END DO
    END DO
    
  END SUBROUTINE copy_mat_band
!!!--------------------------------
  SUBROUTINE copy_mat_sym_sky(mato,matn)
  
    IMPLICIT NONE
    INTEGER             :: errare
    TYPE(T_mat_sym_sky) :: mato,matn
    !                           12345678901234567890123456
    CHARACTER(len=26)   :: IAM='a_MATRIX::copy_mat_sym_sky'

    IF (ASSOCIATED(matn%V) ) DEALLOCATE(matn%V)
    ALLOCATE(matn%V(mato%n),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym_sky')
    END IF

    IF (ASSOCIATED(matn%INDIAG) ) DEALLOCATE(matn%INDIAG)
    ALLOCATE(matn%INDIAG(SIZE(mato%INDIAG)),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym_sky')
    END IF

    IF (ASSOCIATED(matn%LI) ) DEALLOCATE(matn%LI)
    ALLOCATE(matn%LI(SIZE(mato%LI)),stat=errare)
    IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating mat_sym_sky')
    END IF
    
    matn%n=mato%n
    matn%V=mato%V  
    matn%INDIAG=mato%INDIAG  
    matn%LI=mato%LI 

    matn%nt = 0

    !fd faudrait reprendre toute cette merde d'initialisation/allocation/desallocation

    IF (ASSOCIATED(matn%VT)) DEALLOCATE(matn%VT)
    NULLIFY(matn%VT)
    IF (ASSOCIATED(matn%INDIAGJ)) DEALLOCATE(matn%INDIAGJ) 
    NULLIFY(matn%INDIAGJ)
    IF (ASSOCIATED(matn%LJ)) DEALLOCATE(matn%LJ)
    NULLIFY(matn%LJ)
    
  END SUBROUTINE copy_mat_sym_sky
!!!--------------------------------
  SUBROUTINE copy_mat_diag(mato, matn)
    IMPLICIT NONE
    INTEGER :: n,errare
    ! on construit matn (i.e. matrix new), copie de mato (i.e. matrix old)
    TYPE(T_mat_diag) :: mato, matn 
    !                           123456789012345678901
    CHARACTER(len=21) :: IAM='a_MATRIX::copy_mat_diag'
    CHARACTER(len=103) :: cout

    ! on recupere la taille de mato
    n  = mato%n
  
    ! si on a deja alloue de l'espace mémoire pour stocker matn, on le desalloue
    IF ( ASSOCIATED(matn%V) ) DEALLOCATE(matn%V)

    !  write(cout, '(A26, I5)') 'allocation  d une matrice diagonale de taille', n 
    !  call LOGMES(cout)

    ! on alloue l'espace memoire pour stocker matn
    ALLOCATE(matn%V(n), stat=errare)
  
    ! si l'allocation s'est mal passee
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating mat_diag')
    END IF
  
    ! on stocke la taille de la matrice dans l'attribut prevu a cet effet
    matn%n=n
  
    ! on copie les éléments diagonaux de mato dans ceux de matn
    matn%V=mato%V  
  END SUBROUTINE copy_mat_diag
!!!
!!!--------------------------------------------------------------------------------
!!!
!!! FREE MATRIX
!!!
!!!--------------------------------------------------------------------------------
!!!
  SUBROUTINE free_mat_sym(mat)
    
    IMPLICIT NONE
    TYPE(T_mat_sym) :: mat 
  
    mat%n = 0
    if (associated(mat%V)) then
       DEALLOCATE(mat%V)
       NULLIFY(mat%V)
    end if

  END SUBROUTINE free_mat_sym
!!!--------------------------------
  SUBROUTINE free_mat_sym_band(mat)
  
    IMPLICIT NONE
    TYPE(T_mat_sym_band) :: mat 

    mat%bw = 0
    mat%n  = 0
    if (associated(mat%V)) then
       DEALLOCATE(mat%V)
       NULLIFY(mat%V)
    end if
  
  END SUBROUTINE free_mat_sym_band
!!!--------------------------------
  SUBROUTINE free_mat_band(mat)

    IMPLICIT NONE
    TYPE(T_mat_band) :: mat 

    mat%bw = 0
    mat%n  = 0
    if (associated(mat%V)) then
       DEALLOCATE(mat%V)
       NULLIFY(mat%V)
    end if

  END SUBROUTINE free_mat_band
!!!--------------------------------
  SUBROUTINE free_mat_sym_sky(mat)

    IMPLICIT NONE
    TYPE(T_mat_sym_sky) :: mat 

    mat%n  = 0
    mat%nt  = 0
    if (associated(mat%V)) then
       DEALLOCATE(mat%V)
       NULLIFY(mat%V)
    end if
    if (associated(mat%VT)) then
       DEALLOCATE(mat%VT)
       NULLIFY(mat%VT)
    end if
    if (associated(mat%LI)) then
       DEALLOCATE(mat%LI)
       NULLIFY(mat%LI)
    end if
    if (associated(mat%LJ)) then
       DEALLOCATE(mat%LJ)
       NULLIFY(mat%LJ)
    end if
    if (associated(mat%INDIAG)) then
       DEALLOCATE(mat%INDIAG)
       NULLIFY(mat%INDIAG)
    end if
    if (associated(mat%INDIAGJ)) then
       DEALLOCATE(mat%INDIAGJ)
       NULLIFY(mat%INDIAGJ)
    end if

  END SUBROUTINE free_mat_sym_sky
!!!--------------------------------
  SUBROUTINE free_mat_diag(mat)

    IMPLICIT NONE
    TYPE(T_mat_diag) :: mat 

    mat%n  = 0
    if (associated(mat%V)) then
       DEALLOCATE(mat%V)
       NULLIFY(mat%V)
    end if

  END SUBROUTINE free_mat_diag

!!!
!!!--------------------------------------------------------------------------------
!!!
!!! ASSEMB MATRIX
!!!
!!!--------------------------------------------------------------------------------
!!!

  SUBROUTINE assemb_mat_sym(mat,emat,edof2gdof)
  
    IMPLICIT NONE
    INTEGER                     :: enbdof,ei,ej,i,j
    TYPE(T_mat_sym)             :: mat 
    REAL(kind=8),DIMENSION(:,:) :: emat
    INTEGER,DIMENSION(:)        :: edof2gdof
    
    ! on stoque toutes les colonnes
    
    enbdof=SIZE(emat,dim=1)
    DO ej=1,enbdof
       j=edof2gdof(ej)
       DO ei=1,enbdof
          i=edof2gdof(ei)
          mat%V(i,j)=mat%V(i,j)+emat(ei,ej)
       END DO
    END DO

  END SUBROUTINE assemb_mat_sym

  subroutine assemb_elementary_mat_sym(mat,emat,edof2gdof)
    implicit none
    integer(kind=4)               :: enbdof,ei,ej,i,j
    type(T_mat_sym)               :: mat 
    type(G_elementary_matrix)     :: emat
    integer(kind=4), dimension(:) :: edof2gdof
    
    ! on stoque toutes les colonnes
    
    enbdof = get_G_elementary_matrix_order(emat)
    do ej = 1, enbdof
      j = edof2gdof(ej)
      do ei = 1, enbdof
        i = edof2gdof(ei)
        mat%V(i,j) = mat%V(i,j) + get_term_of_G_elementary_matrix(emat,ei,ej)
      end do
    end do

  end subroutine assemb_elementary_mat_sym
!!!--------------------------------
  SUBROUTINE assemb_mat_sym_band(mat,emat,edof2gdof)
  
    IMPLICIT NONE
    INTEGER                     :: enbdof,ei,ej,i,j
    INTEGER                     :: n,bw
    TYPE(T_mat_sym_band)        :: mat 
    REAL(kind=8),DIMENSION(:,:) :: emat
    INTEGER,DIMENSION(:)        :: edof2gdof
 
    n  = mat%n
    bw = mat%bw

    IF (itchatche) THEN
      PRINT*,n,bw
      PRINT*,SIZE(mat%V)
    ENDIF
    !
    ! on stoque les colonnes de la partie superieure 
    ! de la matrice 
    !
    
    enbdof = SIZE(emat,dim=1)

    IF (itchatche) THEN
      PRINT*,enbdof
      PRINT*,edof2gdof
    ENDIF

    DO ej=1,enbdof
       j=edof2gdof(ej)
       DO ei=1,enbdof
          i=edof2gdof(ei)
          !c'est pas la peine de tester
          !      if (i.ge.max(1,j-(bw-1)) .and. i.le.j) &
          IF (i.LE.j) &
               mat%V(bw+i-j,j)=mat%V(bw+i-j,j)+emat(ei,ej)
       END DO
    END DO

  END SUBROUTINE assemb_mat_sym_band

  subroutine assemb_elementary_mat_sym_band(mat,emat,edof2gdof)
    implicit none
    integer(kind=4)               :: enbdof,ei,ej,i,j
    integer(kind=4)               :: n,bw
    type(T_mat_sym_band)          :: mat 
    type(G_elementary_matrix)     :: emat
    integer(kind=4), dimension(:) :: edof2gdof
 
    n  = mat%n
    bw = mat%bw

    if( itchatche ) then
      print *, n, bw
      print *, size(mat%V)
    end if
    !
    ! on stoque les colonnes de la partie superieure 
    ! de la matrice 
    !
    
    enbdof = get_G_elementary_matrix_order(emat)

    if( itchatche ) then
      print *, enbdof
      print *, edof2gdof
    end if

    do ej = 1, enbdof
      j = edof2gdof(ej)
      do ei = 1, enbdof
        i =edof2gdof(ei)
        !c'est pas la peine de tester
        !      if (i.ge.max(1,j-(bw-1)) .and. i.le.j) &
        if( i.le.j ) &
             mat%V(bw+i-j,j) = mat%V(bw+i-j,j) + get_term_of_G_elementary_matrix(emat,ei,ej)
      end do
    end do

  end subroutine assemb_elementary_mat_sym_band
!!!--------------------------------
  SUBROUTINE assemb_mat_band(mat,emat,edof2gdof)
  
    IMPLICIT NONE
    INTEGER                     :: enbdof,ei,ej,i,j
    INTEGER                     :: n,bw,bwh
    TYPE(T_mat_band)            :: mat 
    REAL(kind=8),DIMENSION(:,:) :: emat
    INTEGER,DIMENSION(:)        :: edof2gdof
    
    n   = mat%n
    bw  = mat%bw
    bwh = (bw-1)/2 
    !
    ! on stoque les colonnes de la matrice 
    !
    
    enbdof=SIZE(emat,dim=1)

    DO ej=1,enbdof
       j=edof2gdof(ej)
       DO ei=1,enbdof
          i=edof2gdof(ei)
          ! c'est pas la peine de tester 
          !      if (i.ge.max(1,j-bwh) .and. i.le.min(n,j+bwh) &
          ! rm: c'est mieux ca ?
          !mat%V(bw+i-j,j)=mat%V(bw+i-j,j)+emat(ei,ej)
          mat%V(bwh+1+i-j,j)=mat%V(bwh+1+i-j,j)+emat(ei,ej)
       END DO
    END DO

  END SUBROUTINE assemb_mat_band
!!!--------------------------------
!!! FK
  SUBROUTINE assemb_mat_sym_sky(mat,emat,edof2gdof) 

    IMPLICIT NONE
    INTEGER                     :: enbdof,ei,ej,i,j,k
    TYPE(T_mat_sym_sky)         :: mat 
    REAL(kind=8),DIMENSION(:,:) :: emat
    INTEGER,DIMENSION(:)        :: edof2gdof
    
    enbdof=SIZE(emat,dim=1)

    DO ej=1,enbdof
       j=edof2gdof(ej)
       DO ei=1,enbdof
          i=edof2gdof(ei)
          IF(i.GE.j) THEN
             IF (i.GT.1) THEN
                k=mat%INDIAG(i)+j-i
             ELSE
                k=1
             END IF
             
             mat%V(k)=emat(ei,ej)+mat%V(k)
             
          END IF
       END DO
    END DO
    
  END SUBROUTINE assemb_mat_sym_sky
!!!--------------------------------
! am
  SUBROUTINE assemb_mat_diag(mat, emat, edof2gdof)
    IMPLICIT NONE
    TYPE(T_mat_diag)              :: mat       ! matrice assemblee
    REAL(kind=8), DIMENSION(:)    :: emat      ! matrices locales
    INTEGER, DIMENSION(:)         :: edof2gdof ! map from local dof index to global dof index
    !
    INTEGER :: enbdof, ei, i

    enbdof=SIZE(emat,dim=1)
  
    DO ei=1,enbdof
      i=edof2gdof(ei)
      mat%V(i)=mat%V(i) + emat(ei)
    ENDDO

  END SUBROUTINE assemb_mat_diag

  SUBROUTINE assemb_elementary_mat_diag(mat, emat, edof2gdof)
    implicit none
    type(T_mat_diag)              :: mat       ! matrice assemblee
    type(G_elementary_matrix)     :: emat      ! matrices locales
    integer(kind=4), dimension(:) :: edof2gdof ! map from local dof index to global dof index
    !
    integer(kind=4) :: enbdof, ei, i

    enbdof = get_G_elementary_matrix_order(emat)
  
    do ei=1,enbdof
      i=edof2gdof(ei)
      mat%V(i) = mat%V(i) + get_term_of_G_elementary_matrix(emat,ei,ei)
    end do

  end subroutine assemb_elementary_mat_diag
!!!
!!!--------------------------------------------------------------------------------
!!!
!!! ADD TO MATRIX
!!!
!!!--------------------------------------------------------------------------------
!!!
  SUBROUTINE add_to_mat_sym(mat,tmp,i,j)
  
    IMPLICIT NONE
    INTEGER                     :: i,j
    TYPE(T_mat_sym)             :: mat 
    REAL(kind=8)                :: tmp

    mat%V(i,j)=mat%V(i,j) + tmp

  END SUBROUTINE add_to_mat_sym
!!!--------------------------------
  SUBROUTINE add_to_mat_sym_band(mat,tmp,i,j)
  
    IMPLICIT NONE
    INTEGER                     :: i,j
    INTEGER                     :: bw
    TYPE(T_mat_sym_band)        :: mat 
    REAL(kind=8)                :: tmp
 
    bw = mat%bw

    IF (i.LE.j) mat%V(bw+i-j,j)=mat%V(bw+i-j,j) + tmp

  END SUBROUTINE add_to_mat_sym_band
!!!--------------------------------
  SUBROUTINE add_to_mat_band(mat,tmp,i,j)
  
    IMPLICIT NONE
    INTEGER                     :: i,j
    INTEGER                     :: bw
    TYPE(T_mat_band)            :: mat 
    REAL(kind=8)                :: tmp
    
    bw  = (mat%bw-1)/2

    mat%V(bw+1+i-j,j)=mat%V(bw+1+i-j,j) + tmp

  END SUBROUTINE add_to_mat_band
!!!--------------------------------
!!! FK
  SUBROUTINE add_to_mat_sym_sky(mat,tmp,i,j)

    IMPLICIT NONE
    INTEGER                     :: i,j,k
    TYPE(T_mat_sym_sky)         :: mat 
    REAL(kind=8)                :: tmp
    
    IF (i.GE.j) THEN
      IF (i.GT.1) THEN
        k=mat%INDIAG(i)+j-i
      ELSE
        k=1
      END IF
             
      mat%V(k) = mat%V(k) + tmp
             
    END IF
    
  END SUBROUTINE add_to_mat_sym_sky
!!!--------------------------------
  SUBROUTINE add_to_mat_diag(mat,tmp,i,j)
    IMPLICIT NONE

    TYPE(T_mat_diag)      :: mat  ! matrice assemblee
    REAL(kind=8)          :: tmp
    INTEGER               :: i,j  

    IF (i == j) mat%V(i)=mat%V(i) + tmp

  END SUBROUTINE add_to_mat_diag
!!!
!!!--------------------------------------------------------------------------------
!!!
!!! ASSEMB VECTOR
!!!
!!!--------------------------------------------------------------------------------
!!!
  SUBROUTINE assemb_vector(V,eV,edof2gdof)
  
    IMPLICIT NONE 
    INTEGER                   :: enbdof,ei,i
    REAL(kind=8),DIMENSION(:) :: V,eV
    INTEGER,DIMENSION(:)      :: edof2gdof

    enbdof=SIZE(eV)

    DO ei=1,enbdof
       i=edof2gdof(ei)
       V(i)=V(i)+eV(ei)
    END DO

  END SUBROUTINE assemb_vector
!!!--------------------------------
!================== conditions on matrix =============================

SUBROUTINE apply_drvdof_mat_sym(mat,idrvdof)

  IMPLICIT NONE
  TYPE(T_mat_sym)        :: mat 
  INTEGER                :: i,j,idrvdof,n

  n=mat%n

! -------------------------
! mise a zero de la colonne
! -------------------------
  DO i=1,MAX(1,idrvdof-1)
    mat%V(i,idrvdof)=0.D0
  ENDDO
  DO i=MIN(idrvdof+1,n),n
    mat%V(i,idrvdof)=0.D0
  ENDDO
! -----------------------
! mise a zero de la ligne
! -----------------------
  DO j=1,MAX(1,idrvdof-1)
    mat%V(idrvdof,j)=0.D0
  ENDDO  
  DO j=MIN(idrvdof+1,n),n
    mat%V(idrvdof,j)=0.D0
  ENDDO  
! ------------------------  
! mise a 1 de la diagonale
! ------------------------ 
  mat%V(idrvdof,idrvdof)=1.D0

END SUBROUTINE apply_drvdof_mat_sym

SUBROUTINE apply_drvdof_mat_sym_band(mat,idrvdof)

  IMPLICIT NONE
  TYPE(T_mat_sym_band)       :: mat 
  INTEGER                :: i,j,idrvdof,n,bw

  n=mat%n
  bw=mat%bw

! -------------------------
! mise a zero de la colonne
! -------------------------
  DO i=MAX(1,idrvdof-(bw-1)),idrvdof-1
    mat%V(bw+i-idrvdof,idrvdof)=0.D0
  ENDDO
! -----------------------
! mise a zero de la ligne
! -----------------------
  DO j=idrvdof+1,MIN(n,idrvdof+(bw-1))    
    mat%V(bw+idrvdof-j,j)=0.D0
  ENDDO  
! ------------------------  
! mise a 1 de la diagonale
! ------------------------ 
  mat%V(bw,idrvdof)=1.D0

END SUBROUTINE apply_drvdof_mat_sym_band

SUBROUTINE apply_drvdof_mat_band(mat,idrvdof)
  IMPLICIT NONE
  TYPE(T_mat_band)       :: mat 
  INTEGER                :: i,j,idrvdof,n,bw,bwh

  n=mat%n
  bw=mat%bw
  bwh=(bw-1)/2

! --------------------------
!  mise a zero de la colonne
! --------------------------
  DO i=MAX(1,idrvdof-bwh),MIN(bw,idrvdof+bwh)
    mat%V(bwh+1+i-idrvdof,idrvdof)=0.D0
  ENDDO
! -----------------------
! mise a zero de la ligne
! -----------------------
  DO j=MAX(1,idrvdof-bwh),MIN(bw,idrvdof+bwh)
    mat%V(bwh+1+idrvdof-j,j)=0.D0
  ENDDO
! ------------------------  
! mise a 1 de la diagonale
! ------------------------  
  mat%V(bwh+1,idrvdof)=1.D0

END SUBROUTINE apply_drvdof_mat_band

SUBROUTINE apply_drvdof_mat_sym_sky(mat,idrvdof) 
! thanks to FK
  IMPLICIT NONE
  TYPE(T_mat_sym_sky)       :: mat 
  INTEGER                :: i,j,idrvdof,n,k,l,ji

  n=SIZE(mat%LI)

  ! --------------------------
  !  mise a zero de la colonne
  ! --------------------------
  DO i=1,n
     IF (i>1) THEN
       
        ji=i-mat%LI(i)
     ELSE
        ji=1
     END IF

     IF ((idrvdof>=ji).AND.(idrvdof<i)) THEN 
        l=0
        DO l=0,1+mat%LI(i)
          
           IF (ji+l==idrvdof) THEN
           
              EXIT
           END IF
        END DO

        k=mat%INDIAG(i-1)+1+l
        mat%V(k)=0.
     END IF
  END DO
  ! -----------------------
  ! mise a zero de la ligne
  ! -----------------------

  IF (idrvdof /=1) THEN
    DO k=mat%INDIAG(idrvdof-1)+1,mat%INDIAG(idrvdof)-1
       mat%V(k)=0.
    END DO
  ENDIF
  ! ------------------------  
  ! mise a 1 de la diagonale
  ! -- 
  k=mat%INDIAG(idrvdof) 
  mat%V(k)=1.

END SUBROUTINE apply_drvdof_mat_sym_sky

! am
SUBROUTINE apply_drvdof_mat_diag(mat, idrvdof)

  IMPLICIT NONE
  TYPE(T_mat_diag)        :: mat  ! matrice a laquelle on applique la condition limite en vitesse
  INTEGER                 :: idrvdof ! numero du noeud dont la vitesse est imposee

  ! dans le cas d'une matrice diagonale, les termes extra diagonaux sont
  ! toujours nuls (ils ne sont pas stockes), il est donc inutile de mettre
  ! à 0 la ligne et la colonne idrvdof.
  ! on se contente donc de mettre à 1 le terme diagonal. 

  ! ------------------------  
  ! mise a 1 de la diagonale
  ! ------------------------ 
  mat%V(idrvdof)=1.D0

END SUBROUTINE apply_drvdof_mat_diag


!================== solve linear system ============================= 

SUBROUTINE solve_mat_sym_band(mat,vec)
IMPLICIT NONE
TYPE(T_mat_sym_band)       :: mat 
REAL(kind=8),DIMENSION(:)  :: vec               

INTEGER                    :: info
!                                    1234567890123456789012345678 
CHARACTER(len=28)            :: IAM='a_matrix::solve_mat_sym_band'
!
INTEGER :: i
!fd new pour dpbsvx
REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               

!for lapack
real(kind=8) :: rcond
real(kind=8)   , dimension(1) :: ferr, berr
real(kind=8)   , dimension(:,:), allocatable :: af
real(kind=8)   , dimension(:)  , allocatable :: work, s
integer(kind=4), dimension(:)  , allocatable :: iwork

rcond = 1.d0
allocate(iwork(mat%n),work(3*mat%n),s(mat%n),af(mat%bw,mat%n))


ALLOCATE(vec2(SIZE(vec))) 
vec2=0.d0

CALL DPBSVx('E', 'U', mat%n, mat%bw-1, 1, mat%V, mat%bw, af, mat%bw, 'N', &
             s, vec, mat%n, vec2, mat%n, rcond, ferr, berr, work, iwork, INFO)

IF (INFO /= 0) THEN
  PRINT*,'INFO=',INFO
  CALL FATERR(IAM,'no solution')
ENDIF

vec=vec2

DEALLOCATE(vec2)
deallocate(work,iwork,s,af)

END SUBROUTINE solve_mat_sym_band

SUBROUTINE solve_mat_band(mat,vec)
IMPLICIT NONE
TYPE(T_mat_band)          :: mat
REAL(kind=8),DIMENSION(:) :: vec
!                                    123456789012345678901234 
CHARACTER(len=24)            :: IAM='a_matrix::solve_mat_band'

INTEGER              :: info , lkl, ku
INTEGER, ALLOCATABLE :: IPIV(:)

ALLOCATE(ipiv(mat%n))

lkl = (mat%bw-1)/3
ku  = mat%bw - 2*lkl - 1
CALL DGBSV(mat%n, lkl, ku, 1, mat%V, mat%bw, ipiv, vec, mat%n, INFO)

IF (INFO /= 0) THEN
  CALL FATERR(IAM,'no solution')
ENDIF

DEALLOCATE(ipiv)

END SUBROUTINE solve_mat_band

SUBROUTINE solve_mat_sym(mat,vec)
IMPLICIT NONE
TYPE(T_mat_sym)       :: mat 
REAL(kind=8),DIMENSION(:)  :: vec               

INTEGER                    :: info
!                                    12345678901234567890123 
CHARACTER(len=23)            :: IAM='a_matrix::solve_mat_sym'
!
INTEGER :: i
!fd new pour dpbsvx
REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               

! for lapack
real(kind=8) :: rcond
real(kind=8)   , dimension(1) :: ferr, berr
real(kind=8)   , dimension(:)  , allocatable :: work, s
real(kind=8)   , dimension(:,:), allocatable :: af
integer(kind=4), dimension(:)  , allocatable :: iwork

rcond = 1.d0
allocate(work(3*mat%n))
allocate(iwork(mat%n))
allocate(s(mat%n))
allocate(af(mat%n,mat%n))

ALLOCATE(vec2(SIZE(vec))) 
vec2=0.d0

CALL DPOSVx('E', 'U', mat%n, 1, mat%V, mat%n, af, mat%n, 'N', s, vec, mat%n, vec2, mat%n, &
            rcond, ferr, berr, work, iwork, info)

IF (INFO /= 0) THEN
  PRINT*,'INFO=',INFO
  CALL FATERR(IAM,'no solution')
ENDIF

vec=vec2

DEALLOCATE(vec2)
deallocate(work, iwork, s, af)

END SUBROUTINE solve_mat_sym
!================== solve linear system ============================= 
!
!================== x solve linear system ============================= 
SUBROUTINE x_solve_mat_sym_band(mat,vec,fact_mat,scale,isprecon,opt)
IMPLICIT NONE
!
TYPE(T_mat_sym_band)       :: mat,fact_mat
!
REAL(kind=8),DIMENSION(:)  :: vec,scale               
!
INTEGER                    :: info
!                                    1234567890123456789012345678 
CHARACTER(len=30)            :: IAM='a_matrix::x_solve_mat_sym_band'
!
INTEGER :: i
!
CHARACTER(len=1)             :: opt
CHARACTER(len=1)             :: isprecon
!
REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               

!for lapack
real(kind=8) :: rcond
real(kind=8)   , dimension(1) :: ferr, berr
real(kind=8)   , dimension(:), allocatable :: work
integer(kind=4), dimension(:), allocatable :: iwork

rcond = 1.d0
allocate(iwork(mat%n),work(3*mat%n))

!fd c'est un peu paranoiac ... on pourrait utiliser 
!fd  *vec en entree et en sortie.
!fd  *mat%V en entree et en sortie
!fd  *ne pas passer scale puisque isprecon='N'

ALLOCATE(vec2(SIZE(vec))) 
vec2=0.d0

IF (opt == 'E') THEN

  isprecon='N'

  CALL DPBSVx(opt, 'U', mat%n, mat%bw-1, 1, mat%V, mat%bw, fact_mat%V, fact_mat%bw, &
              isprecon, scale, vec, mat%n, vec2, mat%n, rcond, ferr, berr, work, iwork, INFO)

!fd  print*,'EQUED:',isprecon 

ELSE IF (opt == 'F') THEN

!fd  print*,'EQUED:',isprecon 

  CALL DPBSVx(opt, 'U', mat%n, mat%bw-1, 1, mat%V, mat%bw, fact_mat%V, fact_mat%bw, &
              isprecon, scale, vec, mat%n, vec2, mat%n, rcond, ferr, berr, work, iwork, INFO)

ENDIF

IF (INFO /= 0) THEN
  PRINT*,'INFO=',INFO
  CALL FATERR(IAM,'no solution')
ENDIF

vec=vec2
DEALLOCATE(vec2)
deallocate(work,iwork)

END SUBROUTINE x_solve_mat_sym_band

SUBROUTINE x_solve_mat_band(mat,vec,matfact,scale,isprecon,opt)
IMPLICIT NONE
TYPE(T_mat_band)          :: mat,matfact
REAL(kind=8),DIMENSION(:) :: vec,scale
!                                    123456789012345678901234 
CHARACTER(len=26)            :: IAM='a_matrix::x_solve_mat_band'

INTEGER              :: info 
INTEGER, ALLOCATABLE :: IPIV(:)

CHARACTER(len=1)             :: opt,isprecon

  CALL FATERR(IAM,'PAS ACTIF')

END SUBROUTINE x_solve_mat_band


SUBROUTINE x_solve_mat_sym(mat,vec,fact_mat,scale,isprecon,opt)
IMPLICIT NONE
!
TYPE(T_mat_sym)            :: mat,fact_mat
!
REAL(kind=8),DIMENSION(:)  :: vec,scale               
!
INTEGER                    :: info
!                                    1234567890123456789012345
CHARACTER(len=25)            :: IAM='a_matrix::x_solve_mat_sym'
!
INTEGER :: i
!
CHARACTER(len=1)             :: opt
CHARACTER(len=1)             :: isprecon
!
REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: vec2               

! for lapack
real(kind=8) :: rcond
real(kind=8)   , dimension(1) :: ferr, berr
real(kind=8)   , dimension(:), allocatable :: work
integer(kind=4), dimension(:), allocatable :: iwork

rcond = 1.d0
allocate(work(3*mat%n))
allocate(iwork(mat%n))

!fd c'est un peu paranoiac ... on pourrait utiliser 
!fd  *vec en entree et en sortie.
!fd  *mat%V en entree et en sortie
!fd  *ne pas passer scale puisque isprecon='N'

ALLOCATE(vec2(SIZE(vec))) 
vec2=0.d0

IF (opt == 'E') THEN

  isprecon='N'

  CALL DPOSVx(opt, 'U', mat%n, 1, mat%V, mat%n, fact_mat%V, fact_mat%n, isprecon, &
              scale, vec, mat%n, vec2, mat%n, rcond, ferr, berr, work, iwork, info)

!fd  print*,'EQUED:',isprecon 

ELSE IF (opt == 'F') THEN

!fd  print*,'EQUED:',isprecon 

  CALL DPOSVx(opt, 'U', mat%n, 1, mat%V, mat%n, fact_mat%V, fact_mat%n, isprecon, &
              scale, vec, mat%n, vec2, mat%n, rcond, ferr, berr, work, iwork, info)

ENDIF

IF (INFO /= 0) THEN
  PRINT*,'INFO=',INFO
  CALL FATERR(IAM,'no solution')
ENDIF

vec=vec2
DEALLOCATE(vec2)
deallocate(work,iwork)

END SUBROUTINE x_solve_mat_sym

SUBROUTINE x_solve_mat_sym_sky(mat,vec,fact_mat,scale,isprecon,opt) ! FK ==============================
  IMPLICIT NONE
  !
  TYPE(T_mat_sym_sky)       :: mat,fact_mat
  !
  REAL(kind=8),DIMENSION(:)  :: vec,scale  
  INTEGER,DIMENSION(:),ALLOCATABLE  :: LJTEMP ! FK
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: Mattemp
  INTEGER                    :: info=0
  !                         12345678901234567890123456789 
  CHARACTER(len=29) :: IAM='a_matrix::x_solve_mat_sym_sky'
  !
  INTEGER :: i,j,k,ji,jj,n,LT
  REAL(kind=8) :: s
  !
  CHARACTER(len=1)             :: opt
  CHARACTER(len=1)             :: isprecon
  !

  LT=1

  n=SIZE(mat%INDIAG)
  IF (opt == 'E') THEN

     ! Premier passage 

!fd     print*,'//////////////// FActorisation /////////////////////////////////////'
!fd     print*,'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
     !! FACTORISATION A= L.D.LT=============================================================
     ! LA MATRICE L A LE MEME PROFIL QUE A, ON STOCKE DONC LA MATRICE L DANS A
     ! LES ELEMENTS DIAGONAUX DE L SONT TOUJOURS EGAUX A 1, ON STOCKE DONC D DANS LA DIAGO DE L
     DO i=1,n
        IF (i>1) THEN
           ji=i-Mat%INDIAG(i)+Mat%INDIAG(i-1)+1
        ELSE
           ji=1
        END IF

        DO j=ji,i-1

           IF (j>1) THEN
              jj=j-Mat%INDIAG(j)+Mat%INDIAG(j-1)+1
           ELSE
              jj=1
           END IF

           DO k=MAX(ji,jj),j-1

              Mat%V(MAT%INDIAG(i)-i+j)= Mat%V(MAT%INDIAG(i)-i+j) - Mat%V(MAT%INDIAG(i)-i+k)*Mat%V(MAT%INDIAG(j)-j+k)
           END DO
        END DO


        DO j=ji,(i-1)
           s=Mat%V(MAT%INDIAG(i)-i+j)/Mat%V(MAT%INDIAG(j))
           Mat%V(MAT%INDIAG(i))=Mat%V(MAT%INDIAG(i))-s*Mat%V(MAT%INDIAG(i)-i+j)
           Mat%V(MAT%INDIAG(i)-i+j)=s
        END DO


     END DO

     IF (LT==0) GOTO 2000
     ! ON STOCKE LA MATRICE LT, AVEC UN STOCKAGE SKYLINE LIGNES, DE MANIERE A Y ACCEDER RAPIDEMENT :
     ! Definition du profil de la matrice LT à partir de celui de la matrice L
     IF (.NOT. ASSOCIATED(Mat%LJ)) ALLOCATE(Mat%LJ(n))
     Mat%LJ=0.
     DO j=1,n
        DO i=j,n  !! Peut etre i=j+1,n  ??
           IF (i>1) THEN
              ji=i-Mat%INDIAG(i)+Mat%INDIAG(i-1)+1
           ELSE
              ji=1
           END IF


           IF (ji<=j) MAt%LJ(j)=i-j
        END DO
     END DO



     IF (.NOT. ASSOCIATED(Mat%INDIAGJ)) ALLOCATE(Mat%INDIAGJ(n))
     Mat%INDIAGJ=0
     Mat%INDIAGJ(1)=1
     DO i=2,n


        Mat%INDIAGJ(i)=Mat%INDIAGJ(i-1)+Mat%LJ(i-1)+1
     END DO
     IF (ASSOCIATED(MAt%VT)) DEALLOCATE(MAt%VT) !LA MATRICE LT EST STOCKEE DANS MAT%VT
     ALLOCATE(MAt%VT(MAt%INDIAGJ(n)))

  
     Mat%nT=MAt%INDIAGJ(n)


     ! REMPLISSAGE DE LA MATRICE LT
     Mat%VT=0.
     k=0
     DO i=1,n
        IF (i>1) THEN
           ji=i-Mat%INDIAG(i)+Mat%INDIAG(i-1)+1
        ELSE
           ji=1
        END IF

        DO j=ji,i
           k=k+1
           IF (i<=Mat%LJ(j)+j) THEN  !i=j2,j=i2
              Mat%VT(Mat%INDIAGJ(j)+i-j)=Mat%V(k)
              !  Mat_t(j,i)=Mat%V(k)
           END IF
        END DO
     END DO
2000 k=0
  END IF
  !  MONTEE DESCENTE

  ! DESCENTE =====================================


  DO i=1,n
     IF (i>1) THEN
        ji=i-Mat%INDIAG(i)+Mat%INDIAG(i-1)+1
     ELSE
        ji=1
     END IF
     DO j=ji,i-1 ! ESSAYER DO PRODUCT
        vec(i)=vec(i)-Mat%V(MAT%INDIAG(i)-i+j)*vec(j)
     END DO
  END DO


  ! MONTEE AVEC LT =========================
IF (LT==1) THEN

  DO k=1,n
     i=n-k+1
     DO j=i+1,MIN(MAt%LJ(i)+i,n)
       !print*,i,j
        vec(i)=vec(i)-Mat%VT(MAT%INDIAGJ(i)+j-i)*vec(j)*Mat%VT(MAT%INDIAGJ(i))  ! OK

     END DO

     vec(i)=vec(i)/Mat%VT(MAT%INDIAGJ(i))

  END DO


ELSE

  ! MONTEE AVEC L =============================


  DO k=1,n
     i=n-k+1
     DO j=i+1,n

        ji=j-Mat%INDIAG(j)+Mat%INDIAG(j-1)+1


        IF (i>=ji) THEN
           vec(i)=vec(i)-Mat%V(MAT%INDIAG(j)-j+i)*vec(j)*Mat%V(MAT%INDIAG(i))
        END IF

     END DO

     vec(i)=vec(i)/(Mat%V(MAT%INDIAG(i)))

  END DO


END IF

  IF (INFO /= 0) THEN
     PRINT*,'INFO=',INFO
     CALL FATERR(IAM,'no solution')
  ENDIF

END SUBROUTINE x_solve_mat_sym_sky

! am
SUBROUTINE x_solve_mat_diag(mat,vec,fact_mat,scale,isprecon,opt)
IMPLICIT NONE
!
TYPE(T_mat_diag)           :: mat,fact_mat  ! mat: matrice du syateme a resoudre
      ! fact_mat: matrice du systeme, sous 
      ! forme factorisee
!
REAL(kind=8),DIMENSION(:)  :: vec,scale     ! vec: 
      !  * precondition: contient le second
      !   membre du systeme  
      !   * postcondition: contient la solution
      !   du syteme lineaire      
      ! scale: utilise pour le preconditionnement
!
CHARACTER(len=1)           :: opt  ! permet de choisir le comporetment de la fonction:
      ! * 'E': on factorise le systeme
      ! * 'F': on resoud le systeme a l'aide de la matrice factorisee
      
CHARACTER(len=1)           :: isprecon  ! utilise pour le preconditionnement

!                                    1234567890123456789012345
CHARACTER(len=25)          :: IAM='a_matrix::x_solve_mat_diag'
!
INTEGER      :: n, i  ! n: taille du systeme
                      ! i: indice de boucle
!

!fd c'est un peu paranoiac ... on pourrait utiliser 
!fd  *vec en entree et en sortie.
!fd  *mat%V en entree et en sortie
!fd  *ne pas passer scale puisque isprecon='N'

! on récupère la taille des matrices
n = mat%n

! si l'utilisateur veut factoriser la matrice du systeme
IF (opt == 'E') THEN

 ! dans le cas d'une matrice diagonale, la matrice factorisee calculee est son inverse
  
 ! pour chaque coefficient diagonal de la matrice du syteme
 DO i=1, n
  
  ! si le coefficient courant est nul, la matrice n'est pas inversible et le systeme
  ! n'a donc pas de solution
  IF (mat%V(i) == 0.D0) THEN
    
   ! on affiche un message d'erreur
   CALL FATERR(IAM,'no solution')
  ENDIF
   
  ! sinon, le coefficient diagonal correspondant dans la matrice factorisee est 
  ! l'inverse du coefficient courant
  fact_mat%V(i) = 1.D0/mat%V(i)
 
 END DO

ENDIF

! on resoud le systeme, a l'aide de la matrice factorisee
  
! la solution du systeme s'obtient en multipliant la matrice du systeme, sous forme
! factorisee, par le second membre
  
CALL product_mat_diag_vector(fact_mat, vec)
 
END SUBROUTINE x_solve_mat_diag

!================== x_solve linear system ============================= 

!================== G solve linear system ============================= 

SUBROUTINE product_mat_sym_band_vector(mat,vec)
IMPLICIT NONE
TYPE(T_mat_sym_band)       :: mat 
REAL(kind=8),DIMENSION(:)  :: vec               

REAL(kind=8),DIMENSION(SIZE(vec)) :: X

INTEGER                    :: n,bw

n=mat%n
bw=mat%bw

X=vec

! use blas 2 routine
! SUBROUTINE DSBMV ( UPLO, N, K, ALPHA,  A,  LDA,  X,  INCX,
!                    BETA, Y, INCY )
!
! y := alpha*A*x + beta*y

CALL DSBMV ('U',n,bw - 1,1.d0,mat%V,bw,X,1,0.d0,vec,1)

END SUBROUTINE product_mat_sym_band_vector

SUBROUTINE product_mat_band_vector(mat,vec)
IMPLICIT NONE
TYPE(T_mat_band)       :: mat 
REAL(kind=8),DIMENSION(:)  :: vec
             
REAL(kind=8),DIMENSION(:),ALLOCATABLE :: X

INTEGER                    :: n,bw,bwh

n=mat%n
bw=mat%bw
bwh=(bw-1)/2

ALLOCATE(X(n))
X=vec

! use blas 2 routine
! SUBROUTINE DGBMV ( TRANS, M, N, KL, KU, ALPHA, A, LDA,  X,
!                     INCX, BETA, Y, INCY )
! y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y,


CALL DGBMV('N',n,n,bwh,bwh,1.D0,mat%V,bw,X,1,0.d0,vec,1)

DEALLOCATE(X)

END SUBROUTINE product_mat_band_vector

SUBROUTINE product_mat_sym_vector(mat,vec)
IMPLICIT NONE
TYPE(T_mat_sym)       :: mat 
REAL(kind=8),DIMENSION(:)  :: vec               

REAL(kind=8),DIMENSION(SIZE(vec)) :: X

INTEGER                    :: n

n=mat%n

X=vec

! use blas 2 routine
! SUBROUTINE DSBMV ( UPLO, N, K, ALPHA,  A,  LDA,  X,  INCX,
!                    BETA, Y, INCY )
!
! y := alpha*A*x + beta*y

CALL DSYMV ('U',n,1.d0,mat%V,n,X,1,0.d0,vec,1)

END SUBROUTINE product_mat_sym_vector

SUBROUTINE product_mat_sym_sky_vector(mat,vec)
!thanks to FK
IMPLICIT NONE
TYPE(T_mat_sym_sky)       :: mat 
REAL(kind=8),DIMENSION(:)  :: vec
             
REAL(kind=8),DIMENSION(:),ALLOCATABLE :: sol

INTEGER                    :: n,i,j,ji,k



n=SIZE(Mat%INDIAG)
ALLOCATE(sol(n))
sol=0.
! --------------------------
! multiplication colonne
! --------------------------


  DO k=1,n
     i=n-k+1
     DO j=i+1,n

        ji=j-Mat%INDIAG(j)+Mat%INDIAG(j-1)+1


        IF (i>=ji) THEN
           sol(i)=sol(i)+Mat%V(MAT%INDIAG(j)-j+i)*vec(j)
        END IF
       
     END DO

  END DO


 
  ! -----------------------
  ! multiplication ligne
  ! -----------------------

  DO i=1,n
     IF (i>1) THEN
        ji=i-Mat%INDIAG(i)+Mat%INDIAG(i-1)+1
     ELSE
        ji=1
     END IF
     DO j=ji,i
        sol(i)=sol(i)+Mat%V(MAT%INDIAG(i)-i+j)*vec(j)
     END DO
  END DO


vec=sol
DEALLOCATE(sol)
END SUBROUTINE product_mat_sym_sky_vector

! am
SUBROUTINE product_mat_diag_vector(mat,vec)
IMPLICIT NONE
TYPE(T_mat_diag)           :: mat   
REAL(kind=8),DIMENSION(:)  :: vec  ! precondition: contient le vecteur a 
     !  multiplier par la matrice mat
     ! postcondition: contient le resultat
     !  du produit matrice-vecteur           

INTEGER                    :: n, i ! n: taille de la matrice
     ! i: indice de boucle

! on récupère la taille de la matrice
n=mat%n

! pour chaque composante de vec
DO i=1, n
 ! on calcule la composante correspondante du vecteur resultat
 vec(i) = mat%V(i) * vec(i)
END DO 

END SUBROUTINE product_mat_diag_vector


SUBROUTINE diff_mat_sym_band(mato,matn)
  IMPLICIT NONE

  TYPE(T_mat_sym_band) :: mato, matn
!                           12345678901234567890123456
  CHARACTER(len=26)  :: IAM='a_MATRIX::diff_mat_sym_band'
  CHARACTER(len=103) :: cout
  INTEGER            :: i,j

  DO i=1,mato%bw
    DO j=1,mato%n
      IF (mato%V(i,j) /= matn%V(i,j)) PRINT*,i,j,mato%V(i,j),matn%V(i,j)
    ENDDO
  ENDDO
END SUBROUTINE diff_mat_sym_band   

SUBROUTINE diff_mat_sym_sky(mato,matn)
  IMPLICIT NONE

  TYPE(T_mat_sym_sky) :: mato, matn
!                           12345678901234567890123456
  CHARACTER(len=26)  :: IAM='a_MATRIX::diff_mat_sym_sky'
  CHARACTER(len=103) :: cout
  INTEGER            :: i 
  DO i=1,mato%n
    IF (mato%V(i) /= matn%V(i)) THEN 
      PRINT*,i,mato%V(i),matn%V(i)
    ENDIF
  ENDDO
END SUBROUTINE diff_mat_sym_sky

SUBROUTINE diff_mat_diag(mato,matn)
  IMPLICIT NONE

  TYPE(T_mat_diag) :: mato, matn
!                           12345678901234567890123456
  CHARACTER(len=26)  :: IAM='a_MATRIX::diff_mat_diag'
  CHARACTER(len=103) :: cout
  INTEGER            :: i,j

  DO i=1,mato%n
    IF (mato%V(i) /= matn%V(i)) PRINT*,i,mato%V(i),matn%V(i)
  END DO
END SUBROUTINE diff_mat_diag  

!am: fonctions supplementaires pour resoudre une equation de Poisson
!    avec conditions de Neumann sur tout le bord

! copie d'une matrice carree "creuse" dans une matrice quelconque pleine, i.e. :
!    B(1:n, 1:n) <- A
! ou :
!   A : est une matrice carre "creuse"
!   n : est la taille de la matrice A
!   B : est une matrice m x p, n <= m, n <= p, pleine  

!   * cas de la matrice bande symetrique :
SUBROUTINE copy_mat_sym_band_in_mat_full(mat_in, mat_out, other_mat)
   IMPLICIT NONE

   TYPE(T_mat_sym_band), INTENT(in) :: mat_in ! matrice symetrique + stockage bande 

   REAL(kind=8), DIMENSION(:, :), INTENT(out) :: mat_out ! matrice pleine
   real(kind=8), dimension(:,:), optional, intent(in) :: other_mat !

   ! variables locales:        1234567890123456789012345678901234567
   CHARACTER(len=37) :: IAM = 'MATRIX::copy_mat_sym_band_to_mat_full'
   INTEGER :: n ! taille de la matrice mat_in
   INTEGER :: bw ! largeur de bande de la matrice mat_in
   INTEGER :: m ! nombre de lignes de la matrice mat_out
   INTEGER :: p ! nombre de colonnes de la matrice mat_out
   INTEGER :: i, j ! indices de boucle

   ! on recupere les carteristiques de la matrice mat_in
   n = mat_in%n
   bw = mat_in%bw

   ! on recupere les carteristiques de la matrice mat_out
   m = SIZE(mat_out, 1)
   p = SIZE(mat_out, 2)

   ! si les deux matrices sont incompatibles
   IF (n > m .OR. n > p) THEN
      ! on affiche un message d'erreur
      CALL faterr(IAM,'incompatibles matrices!')
   END IF

   if( present(other_mat) ) then
     if( size(other_mat,1) /= bw  .or. &
         size(other_mat,2) /= n        ) then
        call faterr(IAM,'incompatibles matrices other_mat and mat_in!')
     end if
   end if

   ! on annulle mat_out(1:n, 1:n)
   mat_out(1:n, 1:n) = 0.d0

   ! on remplit les termes corespondants aux bw premiers termes
   DO j=1, bw
      ! on remplit le terme sur la diagonale
      mat_out(j, j) = mat_in%V(bw, j)
      ! on remplit les termes extra-diagonaux
      DO i=1, j - 1
         ! on remplit le terme au dessus de la diagonale
         mat_out(i, j) = mat_in%V(bw + i - j, j)
         ! on obtient le terme en dessous de la diagonale par symetrie
         mat_out(j, i) = mat_out(i, j)
      END DO
   END DO

   ! on remplit les colonnes suivantes
   DO j=bw + 1, n
      ! on remplit le terme sur la diagonale
      mat_out(j, j) = mat_in%V(bw, j)
      ! on remplit les termes extra-diagonaux
      DO i=j - bw + 1, j - 1
         ! on remplit le terme au dessus de la digonale
         mat_out(i, j) = mat_in%V(bw + i - j, j)
         ! on obtient le terme en dessous de la diagonale par symetrie
         mat_out(j, i) = mat_out(i, j)
      END DO
   END DO

   if( present(other_mat) ) then
     ! override values in mat_out with those of other_mat instead of mat_in%V
     do j = 1, bw
        mat_out(j, j) = other_mat(bw, j)
        do i = 1, j-1
           mat_out(i, j) = other_mat(bw+i-j, j)
           mat_out(j, i) = mat_out(i, j)
        end do
     end do

     do j = bw+1, n
        mat_out(j, j) = other_mat(bw, j)
        do i = j-bw+1, j-1
           mat_out(i, j) = other_mat(bw+i-j, j)
           mat_out(j, i) = mat_out(i, j)
        end do
     end do
   end if
END SUBROUTINE copy_mat_sym_band_in_mat_full

! fonction qui resoud un probleme des moindres carres
SUBROUTINE solve_least_square_problem(mat, vec)

   IMPLICIT NONE

   ! variables d'entree-sortie :
   REAL(kind=8), DIMENSION(:, :), INTENT(inout) :: mat 
      ! * entree : matrice du systeme lineaire surdetermine a resoudre
      ! * sortie : matrice modifiee par la factorisation QR 
   REAL(kind=8), DIMENSION(:), INTENT(inout) :: vec 
      ! * entree : second membre du systeme
      ! * sortie : - les "nombre de lignes de mat" preimeres lignes
      !              contiennent la solution
      !            - les lignes suivantes contiennent le necessaire pour le
      !              calcul du residu
 
   ! variables locales :     1234567890123456789012345678901234
   CHARACTER(len=34) :: IAM='MATRIX::solve_least_square_problem'
   INTEGER :: m, n ! m : nombre de lignes de la matrice
                   ! n : nombre de colonnes de la matrice
   INTEGER :: info ! valeur de retour de la fonction LAPACK95 : 
                   !   - 0 : tout va bien
                   !   - < 0 : l'argument -info a une valeur illegale!

   integer(kind=4) :: nb, lwork

   real(kind=8), dimension(:), allocatable :: work

   ! on recupere les caracteristiques de la matrice
   m = SIZE(mat, 1) ! le nombre de lignes
   n = SIZE(mat, 2) ! le nombre de colonnes

   ! on verifie la compatibilite des donnees :
   ! * la forme de la matrice :
   IF (m <= n) THEN
      CALL faterr(IAM, 'matrix do not corespond to an overdetermined system!')
   END IF
   ! * matrice et second membre :
   IF (m .NE. SIZE(vec)) THEN
      CALL faterr(IAM, 'matrix and right hand side are incompatible!')
   END IF  

   ! on appelle la routine LAPACK
   ! it has been checked that m > n... nb should be the optimum block size
   nb = 1
   lwork = n + m*nb
   allocate(work(lwork))
   CALL dgels('N', m, n, 1, mat, m, vec, m, work, lwork, info)

   ! on verifie que l'appel a la fonction LAPACK se soit bien passe
   IF (info < 0) THEN
      CALL faterr(IAM, 'LAPACK call failed!')
   END IF

   deallocate(work)

END SUBROUTINE solve_least_square_problem

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE a_matrix

