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
MODULE a_mecaEF_shell
  !!****h* LMGC90.CORE/a_mecaEF_shell
  !! NAME
  !!  module a_mecaEF_bar
  !! PURPOSE
  !! Basic computations on shell Finite Elements
  !! ---> Unstable yet !!
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/UTILITIES
  !!  LMGC90.CORE/a_EF
  !!****

USE overall
USE utilities
USE a_EF

TYPE T_mecaEF_shell
   integer :: name
   integer :: N_NODE
   integer :: N_DOF_by_NODE
   integer :: N_PG_RIG
   integer :: N_CONT_PG_RIG
   integer :: SCH_GAUSS_RIG
   integer :: N_TETA_PG
   integer :: N_PG_MAS
   integer :: SCH_GAUSS_MAS
   type(T_PT_GAUSS), pointer :: PG(:) => null()
END TYPE T_mecaEF_shell 

TYPE(T_mecaEF_shell),DIMENSION(1),PRIVATE :: mecaEF

integer, parameter, private :: i_dktxx = 1

PUBLIC get_N_GP_mecaEF_shell
PRIVATE get_SCH_GAUSS_RIG_mecaEF
!private get_T_FONC_FORME_mecaEF

CONTAINS

!============ some low level function ==================
!== you to create to construct the a_mecaEF data base ==
!

INTEGER FUNCTION get_nb_ele_shell(ghost)

  IMPLICIT NONE
  INTEGER,OPTIONAL :: ghost

  get_nb_ele_shell=SIZE(mecaEF)

END FUNCTION get_nb_ele_shell

SUBROUTINE init_mecaEF_shell
  IMPLICIT NONE
  logical :: is_initialize = .false.
  INTEGER :: itempo,i,j,errare
  REAL(kind=long),POINTER :: CG(:,:),POIDS_ELE(:)
!                           123456789012345678901234567890123
  CHARACTER(len=33) :: IAM='a_mecaEF_shell::init_mecaEF_shell'
!
  if( is_initialize ) return

  NULLIFY(CG,POIDS_ELE)
!
! EF 3D SURF  
!
! DKT
!
  mecaEF(1)%name          = i_dktxx
  mecaEF(1)%N_NODE        = 3
  mecaEF(1)%N_DOF_by_NODE = 4
!  mecaEF(1)%T_FONC_FORME  ='   '
  mecaEF(1)%N_PG_RIG      = 3 
  mecaEF(1)%N_CONT_PG_RIG = 6 
  mecaEF(1)%SCH_GAUSS_RIG = i_TR03
  mecaEF(1)%N_TETA_PG     = 2      
  mecaEF(1)%N_PG_MAS      = 6
  mecaEF(1)%SCH_GAUSS_MAS = i_TR06

  DO i=1,SIZE(mecaEF)
    ALLOCATE(mecaef(i)%PG(get_N_GP_mecaEF_shell(i)),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating mecaef%PG')
    END IF

    CALL pos_gauss(get_SCH_GAUSS_RIG_mecaEF(i),CG,POIDS_ELE)

    DO j=1,get_N_GP_mecaEF_shell(i)

      mecaEF(i)%PG(j)%POIDS=POIDS_ELE(j)  

    ENDDO

  ENDDO 

  is_initialize = .true.

  IF(ASSOCIATED(CG)) DEALLOCATE(CG)
  IF(ASSOCIATED(POIDS_ELE)) DEALLOCATE(POIDS_ELE)

END SUBROUTINE init_mecaEF_shell

character(len=5) function get_NAME_mecaEF_shell(i)
  implicit none
  integer, intent(in) :: i
  select case(mecaEF(i)%name)
  case( i_dktxx )
    get_NAME_mecaEF_shell = 'DKTxx'
  case default
    get_NAME_mecaEF_shell = 'xxxxx'
  end select
end function get_NAME_mecaEF_shell

INTEGER FUNCTION get_N_DOF_by_NODE_mecaEF_shell(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_DOF_by_NODE_mecaEF_shell=mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_N_DOF_by_NODE_mecaEF_shell

INTEGER FUNCTION get_N_NODE_mecaEF_shell(nb)

  IMPLICIT NONE
  INTEGER          :: nb

  get_N_NODE_mecaEF_shell=mecaEF(nb)%N_NODE

END FUNCTION get_N_NODE_mecaEF_shell


INTEGER FUNCTION get_bw_mecaEF_shell(nb,nodes)
  IMPLICIT NONE
  INTEGER :: nb,i,j,tempo
  INTEGER,DIMENSION(:) :: nodes

  tempo=0
  get_bw_mecaEF_shell=0
  DO i=1,mecaEF(nb)%N_NODE-1
    DO j=i+1,mecaEF(nb)%N_NODE
      tempo = ABS(nodes(i) - nodes(j))
      IF (tempo .GT. get_bw_mecaEF_shell) get_bw_mecaEF_shell=tempo
    ENDDO
  ENDDO

  get_bw_mecaEF_shell = (get_bw_mecaEF_shell+1)*mecaEF(nb)%N_DOF_by_NODE

END FUNCTION get_bw_mecaEF_shell

!==========================================================

!---------------------------------------------------------------------------
! Pour les elements DKT
! CALCUL DES COORD LOCALES XL ET DE LA MATRICES DE ROTATION LOCAL,GLOBAL    
!---------------------------------------------------------------------------
      
SUBROUTINE COOR_LOC(XG,XL,TRA)

 REAL(KIND=LONG)            :: XG(:,:)
 REAL(KIND=LONG)            :: XL(:,:),TRA(:,:)   

 ! variables locales 
 INTEGER                    :: I
 REAL(KIND=LONG)            :: XD(3,3),X(3),Y(3),Z(3) 

 ! Initialisation des nouveaux pointeurs 
 
 DO  I=1,3
   XD(1,I)=XG(1,I)-XG(1,1)
   XD(2,I)=XG(2,I)-XG(2,1)
   XD(3,I)=XG(3,I)-XG(3,1)
 ENDDO

 X = XD(1,:) ; Y = XD(2,:) ; Z = XD(3,:)

 CALL TRANSF(X,Y,Z,TRA)

  XL=ZERO
  XL(1,2)=DOT_PRODUCT(TRA(1,:),XD(:,2)) 
  XL(1,3)=DOT_PRODUCT(TRA(1,:),XD(:,3))
  XL(2,3)=DOT_PRODUCT(TRA(2,:),XD(:,3)) 

  
END SUBROUTINE COOR_LOC

!----------------------------------------------  
               
SUBROUTINE TRANSF(X,Y,Z,TRA)

 REAL(KIND=LONG)    :: X(:),Y(:),Z(:),TRA(:,:)   

 ! variables locales 
 REAL(KIND=LONG)            :: RN,RN2,CT,ST,CF,SF,CP,SP,A,B,C 
     
 RN = X(2)*X(2)+Y(2)*Y(2)
 RN2= SQRT(RN)
 IF(RN2.LT.0.00001_LONG) THEN
     CT=UN  ; ST=ZERO
 ELSE
     CT = X(2)/RN2   ;  ST = Y(2)/RN2
 ENDIF
 RN = RN + Z(2)*Z(2)
 RN2= SQRT(RN)
 SF = Z(2)/RN2
 CF = SQRT(UN-SF*SF)
 A  = Y(2)*Z(3)-Z(2)*Y(3)
 B  = Z(2)*X(3)-X(2)*Z(3)
 C  = X(2)*Y(3)-Y(2)*X(3)
 RN2= SQRT(A*A+B*B+C*C)
 SP =-((Z(2)*Y(3)-Y(2)*Z(3))*ST+(Z(2)*X(3)-X(2)*Z(3))*CT)/RN2
 RN =(X(2)*X(3)+Y(2)*Y(3)+Z(2)*Z(3))/(X(2)*X(2)+Y(2)*Y(2)+Z(2)*Z(2))
 A  = X(3)-X(2)*RN
 B  = Y(3)-Y(2)*RN
 C  = Z(3)-Z(2)*RN
 C  = SQRT(A*A+B*B+C*C)
 CP =(-A*ST+B*CT)/C
 TRA(1,1)= CF*CT  ; TRA(2,1)=-ST*CP-SP*CT*SF ; TRA(3,1)= ST*SP-CP*CT*SF
 TRA(1,2)= CF*ST  ; TRA(2,2)= CT*CP-ST*SF*SP ; TRA(3,2)=-SP*CT-ST*SF*CP
 TRA(1,3)= SF     ; TRA(2,3)= SP*CF          ; TRA(3,3)= CP*CF
 
END SUBROUTINE TRANSF

!------------------------------------------------------------------------------!
!  Calcul de la matrice BF de flexion pour l'element DKT                        ! 
!  et le coefficient        COEFINT=DET(JACOBIENNE)* POIDS_DE_GAUSS            !
!------------------------------------------------------------------------------!
SUBROUTINE GRADIENT_DKT(XL,KEZ,POIDS,BF,COEFINT)

 REAL(KIND=LONG), INTENT(IN)  :: XL(:,:),KEZ(:),POIDS
 REAL(KIND=LONG)              :: BF(:,:)
 REAL(KIND=LONG), INTENT(OUT) :: COEFINT

! Variables locales
 INTEGER             :: I
 REAL(KIND=LONG)     :: DETJ,KSI,ETA,X1,X2,X3,Y1,Y2,Y3,X23,Y23,X31,Y31,X12,Y12
 REAL(KIND=LONG)     :: XL23C,XL31C,XL12C,P4,P5,P6,T4,T5,T6,Q4,Q5,Q6,R4,R5,R6
 REAL(KIND=LONG)     :: H_X_KSI(9),H_Y_KSI(9),H_X_ETA(9),H_Y_ETA(9),DEUX_A
 
  DETJ=XL(1,2)*XL(2,3)
  COEFINT=DETJ*POIDS
  
  KSI=KEZ(1)   ;  ETA=KEZ(2)

  X1=ZERO  ;  X2=XL(1,2)  ; X3=XL(1,3)
  Y1=ZERO  ;  Y2=ZERO     ; Y3=XL(2,3)  
  
  X23=X2-X3                ; X31=X3-X1               ; X12=X1-X2
  Y23=Y2-Y3                ; Y31=Y3-Y1               ; Y12=Y1-Y2          
  XL23C=(X23*X23)+(Y23*Y23);XL31C=(X31*X31)+(Y31*Y31);XL12C=(X12*X12)+(Y12*Y12)
  
  P4=(-SIX )*X23/XL23C     ; P5=(-SIX )*X31/XL31C     ; P6=(-SIX )*X12/XL12C
  T4=(-SIX )*Y23/XL23C     ; T5=(-SIX )*Y31/XL31C     ; T6=(-SIX )*Y12/XL12C
  Q4=(TROIS)*X23*Y23/XL23C ; Q5=(TROIS)*X31*Y31/XL31C ; Q6=(TROIS)*X12*Y12/XL12C
  R4=(TROIS)*Y23*Y23/XL23C ; R5=(TROIS)*Y31*Y31/XL31C ; R6=(TROIS)*Y12*Y12/XL12C
  DEUX_A=(X31*Y12) - (X12*Y31)

  H_X_KSI(1)=P6*(UN-(DEUX*KSI)) + (P5-P6)*ETA 
  H_X_KSI(2)=Q6*(UN-(DEUX*KSI)) - (Q5+Q6)*ETA
  H_X_KSI(3)=(-QUATRE) + (SIX*(KSI+ETA)) + R6*(UN-(DEUX*KSI))- (R5+R6)*ETA
  H_X_KSI(4)=P6*((DEUX*KSI)-UN) + (P4+P6)*ETA
  H_X_KSI(5)=Q6*(UN-(DEUX*KSI)) - (Q6-Q4)*ETA
  H_X_KSI(6)=(-DEUX)+(SIX*KSI)+R6*(UN-(DEUX*KSI))+(R4-R6)*ETA
  H_X_KSI(7)=(P5+P4)*ETA*(-UN)
  H_X_KSI(8)=(Q4-Q5)*ETA
  H_X_KSI(9)=(R4-R5)*ETA 

  H_X_ETA(1)=P5*((DEUX*ETA)-UN) - (P6-P5)*KSI 
  H_X_ETA(2)=Q5*(UN-(DEUX*ETA)) - (Q5+Q6)*KSI
  H_X_ETA(3)=(-QUATRE) + (SIX*(ETA+KSI)) + R5*(UN-(DEUX*ETA))- (R5+R6)*KSI
  H_X_ETA(4)=(P4+P6)*KSI
  H_X_ETA(5)=(Q4-Q6)*KSI
  H_X_ETA(6)=(R4-R6)*KSI 
  H_X_ETA(7)=P5*(UN-(DEUX*ETA)) - (P4+P5)*KSI
  H_X_ETA(8)=Q5*(UN-(DEUX*ETA)) + (Q4-Q5)*KSI
  H_X_ETA(9)=(-DEUX)+(SIX*ETA)+R5*(UN-(DEUX*ETA))+(R4-R5)*KSI  

  H_Y_KSI(1)=T6*(UN-(DEUX*KSI)) + (T5-T6)*ETA
  H_Y_KSI(2)=(UN) + R6*(UN-(DEUX*KSI)) - (R5+R6)*ETA
  H_Y_KSI(3)=Q6*((DEUX*KSI)-UN) + (Q5+Q6)*ETA
  H_Y_KSI(4)=T6*((DEUX*KSI)-UN) + (T4+T6)*ETA
  H_Y_KSI(5)=(-UN) + R6*(UN-(DEUX*KSI)) + (R4-R6)*ETA
  H_Y_KSI(6)=Q6*((DEUX*KSI)-UN) - (Q4-Q6)*ETA
  H_Y_KSI(7)=(T5+T4)*ETA*(-UN)
  H_Y_KSI(8)=(R4-R5)*ETA
  H_Y_KSI(9)=(Q5-Q4)*ETA  
                                                                   
  H_Y_ETA(1)=T5*((DEUX*ETA)-UN) + (T5-T6)*KSI
  H_Y_ETA(2)=(UN) + R5*(UN-(DEUX*ETA)) - (R5+R6)*KSI
  H_Y_ETA(3)=Q5*((DEUX*ETA)-UN) + (Q5+Q6)*KSI
  H_Y_ETA(4)=(T6+T4)*KSI
  H_Y_ETA(5)=(R4-R6)*KSI
  H_Y_ETA(6)=(Q6-Q4)*KSI 
  H_Y_ETA(7)=T5*(UN-(DEUX*ETA)) - (T4+T5)*KSI
  H_Y_ETA(8)=(-UN) + R5*(UN-(DEUX*ETA)) + (R4-R5)*KSI
  H_Y_ETA(9)=Q5*((DEUX*ETA)-UN) - (Q4-Q5)*KSI
  
  DO I=1,9
    BF(1,I)=( Y31*H_X_KSI(I) + Y12*H_X_ETA(I) )/DEUX_A
    BF(2,I)=( X31*H_Y_KSI(I) + X12*H_Y_ETA(I) )*(-UN)/DEUX_A
    BF(3,I)=( Y31*H_Y_KSI(I) + Y12*H_Y_ETA(I)  &
            - X31*H_X_KSI(I) - X12*H_X_ETA(I) )/DEUX_A
  ENDDO 
  
END SUBROUTINE GRADIENT_DKT


!------------------------------------------------------------------------------!
!  Matrice Bl pour les elements DKT       
!------------------------------------------------------------------------------!

SUBROUTINE Bl_DKT(XL,BF,Bl)

REAL(KIND=LONG)                :: XL(:,:),BF(:,:)
REAL(KIND=LONG)                :: Bl(6,18)

 Bl=ZERO
 ! Termes de tension
 Bl(1,1)      = -UN/XL(1,2)
 Bl(1,7)      = -Bl(1,1)
 Bl(2,2)      = (XL(1,3)-XL(1,2))/(XL(1,2)*XL(2,3))
 Bl(2,8)      = -XL(1,3)/(XL(1,2)*XL(2,3))
 Bl(2,14)     = UN/XL(2,3)
 Bl(3,1)      = Bl(2,2)
 Bl(3,2)      = Bl(1,1)
 Bl(3,7)      = Bl(2,8)
 Bl(3,8)      = Bl(1,7)
 Bl(3,13)     = Bl(2,14)
 ! Termes de Flexion
 Bl(4:6,3:5)  = BF(:,1:3)
 Bl(4:6,9:11) = BF(:,4:6)
 Bl(4:6,15:17)= BF(:,7:9) 
 
END SUBROUTINE Bl_DKT

!------------------------------------------------------------------------------
!  " Routine de pilotage du calcul de la matrice de rigidite elementaire
!   pour les elements DKT "
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------!
!    Calcul de la rigidite elementaire elastique [Ke]=Sum [Bl]t[D][Bl]µi 
!------------------------------------------------------------------------------!

SUBROUTINE RIG_ELA_DKT(I,X,K)

INTEGER        , INTENT(IN) :: I          ! le numero de l'element
REAL(KIND=LONG), INTENT(IN) :: X(:,:)     ! coordonnees des noeuds   
REAL(KIND=LONG)             :: K(:,:)     ! matrice de rig. elem.

! variables locales
REAL(KIND=LONG)             :: D(6,6)     ! matrice de comportement
REAL(KIND=LONG)             :: Bl(6,18),CT1,CT2       
INTEGER                     :: IG,J,L
REAL(KIND=LONG)             :: XL(3,3),BF(3,9),TRA(3,3)
REAL(KIND=LONG)             :: COEFINT
REAL(kind=long),POINTER     :: CG(:,:),POIDS_ELE(:)

NULLIFY(CG,POIDS_ELE)

! Initialisation a vide des pointeurs

CALL pos_gauss(get_SCH_GAUSS_RIG_mecaEF(i),CG,POIDS_ELE)

K=ZERO

CALL D_COQ_ISOTROP(D)                   

CALL COOR_LOC(X,XL,TRA)

DO IG=1,mecaEF(i)%N_PG_RIG    ! Pour tous les points de Gauss

  CALL GRADIENT_DKT(XL,CG(:,ig),mecaEF(i)%PG(ig)%POIDS, &
                    BF,COEFINT)

  CALL Bl_DKT(XL,BF,Bl)

  K=K+MATMUL(TRANSPOSE(Bl),MATMUL(D,Bl))*COEFINT

ENDDO
 
 !Rigidite additionelle pour le ddl teta z
  CT1=D(4,4)*0.0001_LONG
  CT2=-CT1*(UN/DEUX)
  
  K( 6, 6)=CT1 ; K( 6,12)=CT2 ; K( 6,18)=CT2
  K(12, 6)=CT2 ; K(12,12)=CT1 ; K(12,18)=CT2  
  K(18, 6)=CT2 ; K(18,12)=CT2 ; K(18,18)=CT1     

 ! Passage local global pour la rigidité
  DO J=1,16,3
    DO L=1,16,3
      K(J:J+2,L:L+2)=MATMUL(TRANSPOSE(TRA),  &
                            MATMUL(K(J:J+2,L:L+2),TRA)) 
    ENDDO
  ENDDO
  
END SUBROUTINE RIG_ELA_DKT


!------------------------------------------------------------------------------!
!  Calcul de la matrice de comportement elastique [D] pour les elements DKT
!------------------------------------------------------------------------------!
SUBROUTINE D_COQ_ISOTROP(D)

 REAL(KIND=LONG)              :: YOUNG,PS,H
 REAL(KIND=LONG)              :: D(:,:) 
   
 REAL(KIND=LONG)              :: C1,C2,C3,C4
 
 YOUNG=200000.d0
 PS=0.3
 H=0.1d0

   C1=YOUNG*H/(UN-PS*PS) ; C2=US2*C1*(UN-PS)
   C3=YOUNG*H*H*H/(DOUZE*(UN-PS*PS)) ; C4=US2*C3*(UN-PS)
   
   D=RESHAPE( (/  C1   , C1*PS , ZERO , ZERO  , ZERO  , ZERO , &
                 C1*PS ,  C1   , ZERO , ZERO  , ZERO  , ZERO , &
                 ZERO  , ZERO  ,  C2  , ZERO  , ZERO  , ZERO , &
                 ZERO  , ZERO  , ZERO ,  C3   , C3*PS , ZERO , &
                 ZERO  , ZERO  , ZERO , C3*PS ,  C3   , ZERO , &     
                 ZERO  , ZERO  , ZERO , ZERO  , ZERO  ,  C4  /), (/6,6/) )      

END SUBROUTINE D_COQ_ISOTROP



INTEGER FUNCTION get_N_GP_mecaEF_shell(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_N_GP_mecaEF_shell=mecaEF(nb)%N_PG_RIG

END FUNCTION get_N_GP_mecaEF_shell


!
!============= low level private routines ==================
!

integer(kind=4) FUNCTION get_SCH_GAUSS_RIG_mecaEF(nb)
  IMPLICIT NONE
  INTEGER          :: nb

  get_SCH_GAUSS_RIG_mecaEF=mecaEF(nb)%SCH_GAUSS_RIG

END FUNCTION get_SCH_GAUSS_RIG_mecaEF

!character(len=4) function get_T_FONC_FORME_mecaEF(nb)
!  implicit none
!  integer          :: nb
!
!  get_T_FONC_FORME_mecaEF=mecaEF(nb)%T_FONC_FORME
!
!end function get_T_FONC_FORME_mecaEF


END MODULE a_mecaEF_shell
