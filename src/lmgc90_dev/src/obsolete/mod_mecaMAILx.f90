!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================

!> manages FE mechanical models: assembling, time integration, etc
MODULE mecaMAILx

  USE overall
  USE utilities
  USE parameters
  USE timer
  USE algebra
  USE DiscreteGeometry
  USE RigidKinematic

  USE bulk_behaviour
  USE models
  
  USE a_DOF
  
  USE a_EF
  USE a_mecaEF
  
  USE MAILx
  USE externalFEM

  USE a_system

private

contains

!------------------------------------------------------------------------ 
! OBSOLETE !!!!
!------------------------------------------------------------------------ 
!!$SUBROUTINE compute_bulk_hpp_mecaMAILx_old(istate)
!!$  IMPLICIT NONE
!!$
!!$
!!$  INTEGER,INTENT(in) :: istate ! =0 comp stiffness & ttFint, =1 comp fields
!!$
!!$  INTEGER :: errare
!!$  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele,dep_ele
!!$  REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: dep_aux_ele
!!$  INTEGER :: ibdyty,iblmty
!!$  INTEGER :: iM_bdyty,iM_blmty
!!$  INTEGER :: idof,nbdof
!!$  INTEGER :: i,inodty,iccdof
!!$!                           1234567890123456789012345678
!!$  CHARACTER(len=27) :: IAM='mecaMAILx::compute_bulk_hpp'
!!$
!!$  IF (nb_mecaMAILx == 0) RETURN
!!$
!!$  IF (ALLOCATED(coor_ele)) THEN
!!$    CALL FATERR(IAM,'coor_ele already allocated !?')
!!$  ENDIF
!!$
!!$  DO ibdyty=1,SIZE(bdyty)
!!$    !
!!$    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$      !
!!$      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$      IF (errare /= 0) THEN
!!$        CALL LOGMES('Error '//IAM//': allocating coor_ele')
!!$      ENDIF
!!$
!!$      coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
!!$
!!$      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
!!$
!!$      ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)), &
!!$               stat=errare)
!!$
!!$      IF (errare /= 0) THEN
!!$        CALL LOGMES('Error '//IAM//': allocating dep_ele')
!!$      ENDIF
!!$
!!$      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$
!!$        ! passage au numero global
!!$        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$        ! position un dans le vecteur global pour le numero inodty     
!!$        iccdof=bdyty(ibdyty)%ccdof(inodty) 
!!$        dep_ele(1:nbdof,i)=bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
!!$
!!$      END DO
!!$
!!$      !
!!$      ! on calcule les matrices elementaires ici
!!$      ! a revoir plus tard ... 
!!$      ! 
!!$
!!$      iM_bdyty=bdyty2M_bdyty(ibdyty)
!!$      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
!!$
!!$      IF (istate==0) THEN
!!$
!!$        ! calcul de K et ttFint
!!$
!!$        CALL compute_elementary_stiffness_old(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                          bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
!!$                                          coor_ele,dep_ele,iM_bdyty,iM_blmty, &
!!$                                          bdyty(ibdyty)%blmty(iblmty)%stiffness)       
!!$        !
!!$        DEALLOCATE(coor_ele,dep_ele)
!!$
!!$        !fd
!!$        !fd on calcule ici le terme H*K(q(i)+H*theta*dqdt(i)) 
!!$        !fd on le stocke dans ttFint(:)
!!$        !fd
!!$
!!$        ALLOCATE(dep_aux_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$        IF (errare /= 0) THEN
!!$          CALL LOGMES('Error '//IAM//': allocating dep_ele')
!!$        ENDIF
!!$
!!$        idof=0
!!$        DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$
!!$          ! passage au numero global
!!$          inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$          ! position un dans le vecteur global pour le numero inodty     
!!$          iccdof=bdyty(ibdyty)%ccdof(inodty) 
!!$
!!$
!!$          dep_aux_ele(idof+1:idof+nbdof) = bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof) + &
!!$                                           (H*THETA*bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof))
!!$
!!$          idof=idof+nbdof
!!$
!!$        END DO
!!$
!!$        bdyty(ibdyty)%blmty(iblmty)%ttFint(:) = MATMUL(bdyty(ibdyty)%blmty(iblmty)%stiffness,dep_aux_ele)
!!$
!!$        DEALLOCATE(dep_aux_ele)
!!$
!!$      ELSE IF (istate == 1) THEN
!!$
!!$        CALL compute_elementary_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                       bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
!!$                                       coor_ele,dep_ele,iM_bdyty,iM_blmty)
!!$
!!$        DEALLOCATE(coor_ele,dep_ele)
!!$
!!$      ELSE
!!$        CALL FATERR(IAM,'unsupported istate')
!!$      ENDIF
!!$
!!$    ENDDO
!!$  ENDDO
!!$
!!$ END SUBROUTINE compute_bulk_hpp_mecaMAILx_old
!!$!------------------------------------------------------------------------ 
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE compute_bulk_gd_mecaMAILx_old
!!$   IMPLICIT NONE
!!$
!!$   INTEGER :: errare
!!$   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
!!$   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: dep_ele
!!$   REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint
!!$
!!$   INTEGER :: ibdyty,iblmty
!!$   INTEGER :: iM_bdyty,iM_blmty
!!$   INTEGER :: idof
!!$   INTEGER :: nbdof,i,inodty,iccdof
!!$!                            12345678901234567890123456
!!$   CHARACTER(len=26) :: IAM='mecaMAILx::compute_nl_bulk'
!!$
!!$   IF (nb_mecaMAILx == 0) RETURN
!!$
!!$   IF (ALLOCATED(coor_ele)) THEN
!!$     CALL FATERR(IAM,'coor_ele already allocated !?')
!!$   ENDIF
!!$
!!$   IF (ALLOCATED(dep_ele)) THEN
!!$     CALL FATERR(IAM,'dep_ele already allocated !?')
!!$   ENDIF
!!$
!!$
!!$   DO ibdyty=1,SIZE(bdyty)
!!$     !
!!$     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$       !
!!$       ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%Fint,dim=1)),stat=errare)
!!$       Fint = 0.d0
!!$
!!$       ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$       IF (errare /= 0) THEN
!!$         CALL LOGMES('Error '//IAM//': allocating coor_ele')
!!$       ENDIF
!!$
!!$       coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
!!$
!!$       IF (((coor_ele(1,2) - coor_ele(1,1))*(coor_ele(2,3) - coor_ele(2,2))) - &    
!!$           ((coor_ele(2,2) - coor_ele(2,1))*(coor_ele(1,3) - coor_ele(1,2))) .LE. 0.d0) THEN
!!$
!!$
!!$         PRINT *,coor_ele(:,1),coor_ele(:,2),coor_ele(:,3)
!!$
!!$         PRINT *, 'element :',iblmty, 'pas dans le bon sens'
!!$         STOP
!!$       ENDIF 
!!$
!!$       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
!!$
!!$       ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
!!$
!!$       IF (errare /= 0) THEN
!!$         CALL LOGMES('Error '//IAM//': allocating dep_ele')
!!$       ENDIF
!!$
!!$       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$         ! passage au numero global
!!$         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$         ! position un dans le vecteur global pour le numero inodty     
!!$         iccdof=bdyty(ibdyty)%ccdof(inodty) 
!!$         dep_ele(1:nbdof,i)=bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
!!$       END DO
!!$       !
!!$       ! on calcule les matrices elementaires ici
!!$       ! a revoir plus tard ... 
!!$       ! 
!!$
!!$       iM_bdyty=bdyty2M_bdyty(ibdyty)
!!$       iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
!!$
!!$       CALL compute_elementary_nl_bulk(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                       bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
!!$                                       coor_ele,dep_ele,iM_bdyty, iM_blmty, &
!!$                                       Fint, &
!!$                                       bdyty(ibdyty)%blmty(iblmty)%stiffness)       
!!$
!!$       bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) = Fint
!!$       DEALLOCATE(coor_ele)
!!$       DEALLOCATE(dep_ele)
!!$       DEALLOCATE(Fint)
!!$     ENDDO
!!$   ENDDO
!!$ END SUBROUTINE compute_bulk_gd_mecaMAILx_old
!!$!------------------------------------------------------------------------ 
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE assemb_NL_RHS_mecaMAILx_old
!!$
!!$   IMPLICIT NONE
!!$
!!$!                            1234567890123456789012345
!!$   CHARACTER(len=24) :: IAM='mecaMAILx::assemb_NL_RHS'
!!$
!!$   INTEGER :: ibdyty,iblmty,i,inodty,iccdof,nbdof,errare
!!$
!!$   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele
!!$
!!$   REAL(kind=8) :: HT,HUMT
!!$
!!$   REAL(kind=8),DIMENSION(8) :: pipo
!!$    
!!$   IF (nb_mecaMAILx == 0) RETURN
!!$
!!$   IF (ALLOCATED(DV_ele)) THEN
!!$     CALL FATERR(IAM,'DV_ele already allocated !?')
!!$   ENDIF
!!$
!!$   HT = H*THETA
!!$   HUMT=H*(1.d0-THETA)  
!!$
!!$   DO ibdyty=1,SIZE(bdyty)
!!$
!!$     bdyty(ibdyty)%RHS=0.D0
!!$     bdyty(ibdyty)%Fint=0.D0
!!$     bdyty(ibdyty)%Finert=0.D0
!!$
!!$     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$
!!$       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node  
!!$
!!$       ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$         ! passage au numero global
!!$         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$         ! position un dans le vecteur global pour le numero inodty     
!!$         iccdof=bdyty(ibdyty)%ccdof(inodty)
!!$         ! 
!!$         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) - &
!!$                                       bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
!!$       END DO
!!$
!!$!xx       print*,'DV_ele:',DV_ele
!!$
!!$       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)
!!$
!!$!xx       print*,'M*(V- - V+):',DV_ele
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)
!!$!xx       print*,'Fext-:',pipo
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
!!$!xx       print*,'Fext+:',pipo
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fint(:,2)
!!$!xx       print*,'Fint-:',pipo
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fint(:,1)
!!$!xx       print*,'Fint+:',pipo
!!$
!!$
!!$       bdyty(ibdyty)%blmty(iblmty)%RHSloc= DV_ele +  &
!!$                                           ( ( HUMT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) + &
!!$                                                 HT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) &
!!$                                             ) &
!!$                                            -( HUMT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
!!$                                                 HT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) &
!!$                                             ) &
!!$                                            )
!!$       !
!!$
!!$       CALL assemb_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
!!$
!!$       CALL assemb_vector(bdyty(ibdyty)%Fint,-(       THETA*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) + &
!!$                                               (1.d0-THETA)*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2)), &
!!$                          bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  
!!$
!!$       CALL assemb_vector(bdyty(ibdyty)%Finert,(DV_ele/H),bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  
!!$
!!$       DEALLOCATE(DV_ele)
!!$
!!$     ENDDO
!!$
!!$!xx     print*,'RHS dans assemb'
!!$!xx     print*,bdyty(ibdyty)%RHS
!!$
!!$     bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(H*bdyty(ibdyty)%Fext)
!!$
!!$!xx     print*,'H*Fext dans assemb'
!!$!xx     print*,H*bdyty(ibdyty)%Fext
!!$!xx     print*,bdyty(ibdyty)%RHS
!!$
!!$
!!$   ENDDO
!!$ END SUBROUTINE assemb_NL_RHS_mecaMAILx_old
!!$!------------------------------------------------------------------------
!!$!------------------------------------------------------------------------ 
!!$SUBROUTINE compute_fint_mecaMAILx
!!$  IMPLICIT NONE
!!$
!!$  INTEGER :: errare
!!$  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
!!$  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: dep_ele
!!$  INTEGER :: ibdyty,iblmty
!!$  INTEGER :: iM_bdyty,iM_blmty
!!$  INTEGER :: nbdof,i,inodty,iccdof
!!$!                           12345678901234567890123
!!$  CHARACTER(len=23) :: IAM='mecaMAILx::compute_fint'
!!$
!!$  IF (nb_mecaMAILx == 0) RETURN
!!$
!!$  IF (ALLOCATED(coor_ele)) THEN
!!$    CALL FATERR(IAM,'coor_ele already allocated !?')
!!$  ENDIF
!!$
!!$!
!!$! calcul de la contrainte et mise a jour des grandeurs internes
!!$!
!!$  DO ibdyty=1,SIZE(bdyty)
!!$    !
!!$    DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$      !
!!$      ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$      IF (errare /= 0) THEN
!!$        CALL LOGMES('Error '//IAM//': allocating coor_ele')
!!$      ENDIF
!!$      nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
!!$
!!$      ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$      IF (errare /= 0) THEN
!!$        CALL LOGMES('Error '//IAM//': allocating dep_ele')
!!$      ENDIF
!!$
!!$      DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$
!!$        ! passage au numero global
!!$        inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$        ! position un dans le vecteur global pour le numero inodty     
!!$        iccdof=bdyty(ibdyty)%ccdof(inodty) 
!!$
!!$
!!$        dep_ele(1:nbdof,i)= bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
!!$
!!$      END DO
!!$
!!$      !
!!$      ! on calcule les forces interieures
!!$      ! 
!!$
!!$      iM_bdyty=bdyty2M_bdyty(ibdyty)
!!$      iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
!!$
!!$      CALL compute_elementary_fint(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                   bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
!!$                                   coor_ele,dep_ele,iM_bdyty,iM_blmty, &
!!$                                   bdyty(ibdyty)%blmty(iblmty)%Fint(:,1))
!!$
!!$      !
!!$
!!$      DEALLOCATE(coor_ele,dep_ele)
!!$
!!$    ENDDO
!!$  ENDDO
!!$
!!$ END SUBROUTINE compute_fint_mecaMAILx
!!!--------------------------------------------------------

!fd la resolution hpp et gd utilise dorenavant les meme routine.

!!$!------------------------------------------------------------------------ 
!!$! Non-linear routines
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE compute_bulk_gd_mecaMAILx(istate)
!!$   IMPLICIT NONE
!!$
!!$   INTEGER, INTENT(in) :: istate ! =0 comp stiffness & ttFint, =1 comp fields
!!$   INTEGER :: errare
!!$   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: coor_ele
!!$   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: dep_ele
!!$   REAL(kind=8),DIMENSION(:),ALLOCATABLE   :: Fint
!!$
!!$   INTEGER :: ibdyty,iblmty
!!$   INTEGER :: iM_bdyty,iM_blmty
!!$   INTEGER :: idof
!!$   INTEGER :: nbdof,i,inodty,iccdof
!!$!                            12345678901234567890123456
!!$   CHARACTER(len=26) :: IAM='mecaMAILx::compute_bulk_gd'
!!$
!!$   IF (nb_mecaMAILx == 0) RETURN
!!$
!!$   IF (is_externalFEM) THEN
!!$     IF (istate == 0) CALL externalFEM_compute_bulk
!!$     RETURN   
!!$   ENDIF
!!$
!!$   IF (ALLOCATED(coor_ele)) THEN
!!$     CALL FATERR(IAM,'coor_ele already allocated !?')
!!$   ENDIF
!!$
!!$   IF (ALLOCATED(dep_ele)) THEN
!!$     CALL FATERR(IAM,'dep_ele already allocated !?')
!!$   ENDIF
!!$
!!$
!!$   DO ibdyty=1,SIZE(bdyty)
!!$     !
!!$     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$       !
!!$       ALLOCATE(coor_ele(nbDIME,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$       IF (errare /= 0) THEN
!!$         CALL LOGMES('Error '//IAM//': allocating coor_ele')
!!$       ENDIF
!!$
!!$       coor_ele=get_cooref_ele(ibdyty,iblmty,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)) 
!!$
!!$       IF (((coor_ele(1,2) - coor_ele(1,1))*(coor_ele(2,3) - coor_ele(2,2))) - &    
!!$           ((coor_ele(2,2) - coor_ele(2,1))*(coor_ele(1,3) - coor_ele(1,2))) .LE. 0.d0) THEN
!!$
!!$
!!$         PRINT*,coor_ele(:,1),coor_ele(:,2),coor_ele(:,3)
!!$
!!$         PRINT*, 'element :',iblmty, 'pas dans le bon sens'
!!$         STOP
!!$       ENDIF 
!!$
!!$       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node   
!!$
!!$       ALLOCATE(dep_ele(nbdof,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare) 
!!$
!!$       IF (errare /= 0) THEN
!!$         CALL LOGMES('Error '//IAM//': allocating dep_ele')
!!$       ENDIF
!!$
!!$       iM_bdyty=bdyty2M_bdyty(ibdyty)
!!$       iM_blmty=bdyty(ibdyty)%blmty2M_blmty(iblmty)
!!$
!!$       IF (istate==0) THEN
!!$
!!$         DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$           ! passage au numero global
!!$           inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$           ! position un dans le vecteur global pour le numero inodty     
!!$           iccdof=bdyty(ibdyty)%ccdof(inodty) 
!!$           dep_ele(1:nbdof,i)=         theta*bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof) + &
!!$                              (1.d0 - theta)*bdyty(ibdyty)%Xbegin(iccdof+1:iccdof+nbdof)
!!$         END DO
!!$         !
!!$         ! on calcule les matrices elementaires ici
!!$         ! a revoir plus tard ... 
!!$         ! 
!!$
!!$         ALLOCATE(Fint(SIZE(bdyty(ibdyty)%blmty(iblmty)%Fint,dim=1)),stat=errare)
!!$         IF (errare /= 0) THEN
!!$           CALL LOGMES('Error '//IAM//': allocating Fint')
!!$         ENDIF
!!$
!!$         Fint = 0.d0
!!$
!!$         CALL compute_elementary_bulk(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                      bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
!!$                                      coor_ele,dep_ele,iM_bdyty, iM_blmty, &
!!$                                      Fint, &
!!$                                      bdyty(ibdyty)%blmty(iblmty)%stiffness)       
!!$!fd a mettre dans ttFint ...
!!$
!!$         bdyty(ibdyty)%blmty(iblmty)%ttFint(:) = Fint
!!$
!!$         !print*,ibdyty,iblmty
!!$         !print*,Fint
!!$         !print*,"=================="
!!$
!!$         DEALLOCATE(Fint)
!!$       ELSE IF (istate == 1) THEN
!!$         DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$           ! passage au numero global
!!$           inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$           ! position un dans le vecteur global pour le numero inodty     
!!$           iccdof=bdyty(ibdyty)%ccdof(inodty) 
!!$           dep_ele(1:nbdof,i) = bdyty(ibdyty)%X(iccdof+1:iccdof+nbdof)
!!$         END DO
!!$
!!$         CALL compute_elementary_fields(bdyty(ibdyty)%blmty(iblmty)%blmnb, &
!!$                                        bdyty(ibdyty)%blmty(iblmty)%ppsnb, &
!!$                                        coor_ele,dep_ele,iM_bdyty,iM_blmty)
!!$
!!$       ELSE
!!$         CALL FATERR(IAM,'unsupported istate')
!!$       ENDIF
!!$       DEALLOCATE(coor_ele)
!!$       DEALLOCATE(dep_ele)
!!$
!!$     ENDDO
!!$   ENDDO
!!$ END SUBROUTINE compute_bulk_gd_mecaMAILx
!!$!------------------------------------------------------------------------ 
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE assemb_NL_KT_mecaMAILx
!!$   IMPLICIT NONE
!!$
!!$   REAL(kind=8) :: HT2
!!$   INTEGER :: ibdyty,iblmty
!!$   INTEGER :: idof,ivd,iccdof,inod
!!$
!!$
!!$!fd
!!$   IF (nb_mecaMAILx == 0) RETURN
!!$
!!$
!!$   HT2=THETA*H*THETA*H
!!$
!!$   DO ibdyty=1,SIZE(bdyty)
!!$
!!$     CALL G_zero(bdyty(ibdyty)%KT)
!!$
!!$     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$
!!$       bdyty(ibdyty)%blmty(iblmty)%KTloc=  bdyty(ibdyty)%blmty(iblmty)%mass  + &
!!$                                          (HT2*bdyty(ibdyty)%blmty(iblmty)%stiffness)
!!$
!!$!fd       print*,'masse:'
!!$!fd       write(*,'(8(1x,D14.7)') bdyty(ibdyty)%blmty(iblmty)%mass
!!$!fd       print*,'stiffness:'
!!$!fd       write(*,'(8(1x,D14.7)') bdyty(ibdyty)%blmty(iblmty)%stiffness
!!$!fd       print*,'KTloc:' 
!!$!fd       write(*,'(8(1x,D14.7)') bdyty(ibdyty)%blmty(iblmty)%KTloc
!!$
!!$       CALL G_assemb(bdyty(ibdyty)%KT,bdyty(ibdyty)%blmty(iblmty)%KTloc, &
!!$                     bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
!!$
!!$!fd       print*,'KT' 
!!$!fd       write(*,'(8(1x,D14.7)') bdyty(ibdyty)%KT%V
!!$
!!$
!!$     ENDDO 
!!$
!!$!fd     print*,'KT'
!!$!fd     print*,bdyty(ibdyty)%KT%V
!!$!     print*,'========='
!!$
!!$     CALL G_store(bdyty(ibdyty)%KT)
!!$
!!$     ! print*,bdyty(ibdyty)%KT%M_ddl(bdyty(ibdyty)%KT%sym_band%bw,:)
!!$
!!$     DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
!!$
!!$       CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
!!$
!!$       iccdof=bdyty(ibdyty)%ccdof(inod)+idof 
!!$  
!!$       CALL G_apply_drvdof(bdyty(ibdyty)%KT,iccdof)
!!$
!!$     ENDDO
!!$
!!$   ENDDO
!!$
!!$ END SUBROUTINE assemb_NL_KT_mecaMAILx
!!$!------------------------------------------------------------------------  
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE assemb_NL_RHS_mecaMAILx
!!$
!!$   IMPLICIT NONE
!!$
!!$!                            1234567890123456789012345
!!$   CHARACTER(len=24) :: IAM='mecaMAILx::assemb_NL_RHS'
!!$
!!$   INTEGER :: ibdyty,iblmty,i,inodty,iccdof,nbdof,errare
!!$
!!$   REAL(kind=8),DIMENSION(:),ALLOCATABLE :: DV_ele
!!$
!!$   REAL(kind=8) :: HT,HUMT
!!$
!!$   REAL(kind=8),DIMENSION(8) :: pipo
!!$    
!!$   IF (nb_mecaMAILx == 0) RETURN
!!$
!!$   IF (ALLOCATED(DV_ele)) THEN
!!$     CALL FATERR(IAM,'DV_ele already allocated !?')
!!$   ENDIF
!!$
!!$   HT = H*THETA
!!$   HUMT=H*(1.d0-THETA)  
!!$
!!$   DO ibdyty=1,SIZE(bdyty)
!!$
!!$     bdyty(ibdyty)%RHS=0.D0
!!$     bdyty(ibdyty)%Fint=0.D0
!!$     bdyty(ibdyty)%Finert=0.D0
!!$
!!$     DO iblmty=1,SIZE(bdyty(ibdyty)%blmty)
!!$
!!$       nbdof=bdyty(ibdyty)%blmty(iblmty)%n_dof_by_node  
!!$
!!$       ALLOCATE(DV_ele(nbdof*SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)),stat=errare)
!!$
!!$       DO i=1,SIZE(bdyty(ibdyty)%blmty(iblmty)%NODES)
!!$         ! passage au numero global
!!$         inodty=bdyty(ibdyty)%blmty(iblmty)%NODES(i) 
!!$         ! position un dans le vecteur global pour le numero inodty     
!!$         iccdof=bdyty(ibdyty)%ccdof(inodty)
!!$         ! 
!!$         DV_ele((i-1)*nbdof+1:i*nbdof)=bdyty(ibdyty)%Vbegin(iccdof+1:iccdof+nbdof) - &
!!$                                       bdyty(ibdyty)%V(iccdof+1:iccdof+nbdof)
!!$       END DO
!!$
!!$!xx       print*,'DV_ele:',DV_ele
!!$
!!$       DV_ele = MATMUL(bdyty(ibdyty)%blmty(iblmty)%mass,DV_ele)
!!$
!!$!xx       print*,'M*(V- - V+):',DV_ele
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fext(:,2)
!!$!xx       print*,'Fext-:',pipo
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fext(:,1)
!!$!xx       print*,'Fext+:',pipo
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fint(:,2)
!!$!xx       print*,'Fint-:',pipo
!!$!xx       pipo=bdyty(ibdyty)%blmty(iblmty)%Fint(:,1)
!!$!xx       print*,'Fint+:',pipo
!!$
!!$
!!$       bdyty(ibdyty)%blmty(iblmty)%RHSloc= DV_ele +  &
!!$                                           ( ( HUMT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,2) + &
!!$                                                 HT*bdyty(ibdyty)%blmty(iblmty)%Fext(:,1) &
!!$                                             ) &
!!$                                            -( HUMT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
!!$                                                 HT*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) &
!!$                                              ) &
!!$                                            -( H*bdyty(ibdyty)%blmty(iblmty)%ttFint(:) ) &
!!$                                            )
!!$       !
!!$
!!$       CALL assemb_vector(bdyty(ibdyty)%RHS,bdyty(ibdyty)%blmty(iblmty)%RHSloc,bdyty(ibdyty)%blmty(iblmty)%edof2gdof)
!!$
!!$       CALL assemb_vector(bdyty(ibdyty)%Fint,-(      THETA*bdyty(ibdyty)%blmty(iblmty)%Fint(:,1) + &
!!$                                              (1.d0-THETA)*bdyty(ibdyty)%blmty(iblmty)%Fint(:,2) + &
!!$                                               bdyty(ibdyty)%blmty(iblmty)%ttFint(:)), &
!!$                          bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  
!!$
!!$       CALL assemb_vector(bdyty(ibdyty)%Finert,(DV_ele/H),bdyty(ibdyty)%blmty(iblmty)%edof2gdof)  
!!$
!!$       DEALLOCATE(DV_ele)
!!$
!!$     ENDDO
!!$
!!$!xx     print*,'RHS dans assemb'
!!$!xx     print*,bdyty(ibdyty)%RHS
!!$
!!$     bdyty(ibdyty)%RHS=bdyty(ibdyty)%RHS+(H*bdyty(ibdyty)%Fext)
!!$
!!$!xx     print*,'H*Fext dans assemb'
!!$!xx     print*,H*bdyty(ibdyty)%Fext
!!$!xx     print*,bdyty(ibdyty)%RHS
!!$
!!$
!!$   ENDDO
!!$ END SUBROUTINE assemb_NL_RHS_mecaMAILx
!!$!------------------------------------------------------------------------
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE compute_nl_free_vlocy_mecaMAILx
!!$  
!!$   IMPLICIT NONE
!!$   INTEGER :: ibdyty,ivd,inod,idof,iccdof
!!$
!!$   INTEGER :: i,info
!!$
!!$!                           123456789012345678901234567890123
!!$   CHARACTER(len=33) :: IAM='mecaMAILx::compute_nl_free_vlocy'
!!$
!!$! calcul de la vitesse libre
!!$
!!$   IF (nb_mecaMAILx == 0) RETURN
!!$
!!$   IF (is_externalFEM) THEN
!!$
!!$      CALL externalFEM_compute_free_vlocy
!!$      DO ibdyty=1,SIZE(bdyty)
!!$        CALL externalFEM_get_free_vlocy(ibdyty,bdyty(ibdyty)%Vfree)
!!$      ENDDO
!!$
!!$      RETURN
!!$
!!$   ENDIF
!!$
!!$   DO ibdyty=1,SIZE(bdyty)
!!$
!!$
!!$!     if (itchache) then
!!$!       print*,'le corps :',ibdyty
!!$!       print*,'la vitesse avant correction:'
!!$!       print*,bdyty(ibdyty)%V
!!$
!!$!       print*,' '
!!$!       print*,'les efforts internes'
!!$!       print*,bdyty(ibdyty)%Fint
!!$
!!$!     endif
!!$!
!!$! contributions sur le second membre 
!!$! contribution de ce qui reste des ddl imposes sur le second membre 
!!$! 
!!$
!!$     bdyty(ibdyty)%Vfree = 0.d0
!!$
!!$     CALL apply_vlocy_driven_dof(ibdyty,iVfree)
!!$
!!$     DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
!!$
!!$       CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
!!$
!!$       iccdof=bdyty(ibdyty)%ccdof(inod)+idof 
!!$
!!$       bdyty(ibdyty)%Vfree(iccdof) = bdyty(ibdyty)%Vfree(iccdof) - &
!!$                                     bdyty(ibdyty)%V(iccdof)
!!$
!!$     ENDDO
!!$
!!$!
!!$!    print*,'le second membre'
!!$!    print*,bdyty(ibdyty)%RHS
!!$
!!$     bdyty(ibdyty)%Vfree=-bdyty(ibdyty)%Vfree  
!!$
!!$     CALL G_product_vector(bdyty(ibdyty)%KT,bdyty(ibdyty)%Vfree)
!!$
!!$     bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vfree   + &
!!$                          bdyty(ibdyty)%RHS
!!$
!!$     CALL apply_vlocy_driven_dof(ibdyty,iVfree)
!!$
!!$!
!!$! contribution sur la matrice
!!$!
!!$     DO ivd=1,bdyty(ibdyty)%nb_vlocy_driven_dof
!!$
!!$       CALL owner_of_a_driven_dof(bdyty(ibdyty)%vlocy_driven_dof(ivd),inod,idof)
!!$
!!$       iccdof=bdyty(ibdyty)%ccdof(inod)+idof 
!!$  
!!$
!!$       bdyty(ibdyty)%Vfree(iccdof) = bdyty(ibdyty)%Vfree(iccdof) - &
!!$                                     bdyty(ibdyty)%V(iccdof)
!!$
!!$     ENDDO
!!$
!!$!
!!$!     
!!$!    print*,'KT'
!!$!
!!$!     write(*,'(8(1x,D14.7)') bdyty(ibdyty)%KT%V
!!$!     
!!$!     print*,bdyty(ibdyty)%KT%M_ddl(bdyty(ibdyty)%KT%sym_band%bw,:)
!!$!     print*,bdyty(ibdyty)%KT%sym_band%V(bdyty(ibdyty)%KT%sym_band%bw,:)
!!$
!!$!    print*,'le second membre corrige'
!!$!    print*,bdyty(ibdyty)%Vfree
!!$
!!$     CALL G_solve_linear_system(bdyty(ibdyty)%KT,bdyty(ibdyty)%Vfree,info)
!!$
!!$     IF (info /= 0) THEN
!!$       PRINT*,ibdyty
!!$       CALL FATERR(IAM,'No solution')
!!$     ENDIF
!!$
!!$!
!!$!     print*,'KT'
!!$!
!!$!     write(*,'(8(1x,D14.7)') bdyty(ibdyty)%KT%V
!!$!     print*,bdyty(ibdyty)%KT%V(bdyty(ibdyty)%KT%bw,:)
!!$
!!$!    print*,'le second membre corrige'
!!$!    print*,bdyty(ibdyty)%Vfree
!!$
!!$     bdyty(ibdyty)%Vfree= bdyty(ibdyty)%Vfree  + bdyty(ibdyty)%V
!!$  
!!$!     print*,'=================================================='
!!$!     print*,'Step:',Nstep,' max de la correction libre=', &
!!$!       max(maxval(bdyty(ibdyty)%Vfree),abs(minval(bdyty(ibdyty)%Vfree)))
!!$!     print*,'=================================================='
!!$
!!$   END DO
!!$
!!$ END SUBROUTINE compute_nl_free_vlocy_mecaMAILx
!!$!------------------------------------------------------------------------
!!$

!!$  !> add a spring to a dof of a node
!!$  SUBROUTINE add_spring_to_node_mecaMAILx(ibdyty,inodty,idof,k)
!!$    IMPLICIT NONE 
!!$    INTEGER(kind=4), INTENT(in) :: ibdyty !< body number
!!$    INTEGER(kind=4), INTENT(in) :: inodty !< node number
!!$    INTEGER(kind=4), INTENT(in) :: idof   !< dof  number
!!$    REAL(kind=8)   , INTENT(in) :: k      !< nodal stiffness
!!$
!!$    ! ***
!!$
!!$    INTEGER :: i
!!$
!!$    i  = bdyty(ibdyty)%ccdof(inodty)+idof
!!$    
!!$    CALL G_add(bdyty(ibdyty)%KT,h*h*theta*theta*k,i,i)
!!$
!!$  END SUBROUTINE


!   SUBROUTINE check_periodic

!     IMPLICIT NONE
!     INTEGER      :: ibdyty,nbdof,iperiodic
!     REAL(kind=8) :: X

!     DO ibdyty=1,SIZE(bdyty)
       
!        bdyty(ibdyty)%periode = .FALSE.
!        iperiodic = 1

!        DO inodty = 1, SIZE(bdyty(ibdyty)%nodty)

!           iccdof= bdyty(ibdyty)%ccdof(inodty)
!           nbdof = nbdof_a_nodty(bdyty(ibdyty)%nodty(inodty))
          
!           X = bdyty(ibdyty)%X(iccdof+1) + bdyty(ibdyty)%cooref(iccdof+1)

!           IF ( X .GT. periode ) THEN

!              bdyty(ibdyty)%X(iccdof+1) = bdyty(ibdyty)%X(iccdof+1) - periode
!              bdyty(ibdyty)%periodicnode(iccdof+1) = 1
        
!           ELSE IF( X .LT. 0.D0 ) THEN

!              bdyty(ibdyty)%X(iccdof+1) = bdyty(ibdyty)%X(iccdof+1) - periode
!              bdyty(ibdyty)%periodicnode(iccdof+1) = -1
 
!           ELSE

!              bdyty(ibdyty)%periodicnode(iccdof+1) = 0

!           END IF
          
!           iperiodic = iperiodic * bdyty(ibdyty)%periodicnode(iccdof+1)

!        END DO

!        IF (iperiodic.NE.0) bdyty(ibdyty)%periode = .TRUE.

!     END DO
    
!  END SUBROUTINE check_periodic
!!!------------------------------------------------------------------------  

end module
