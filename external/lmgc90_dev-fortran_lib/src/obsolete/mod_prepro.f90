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
MODULE PREPRO                        

  !!****h* LMGC90.CORE/PREPRO
  !! NAME
  !!  module PREPRO
  !! PURPOSE
  !!  creation of a simulation sample.
  !!  generate the files PREBOX/BODIES.DAT, PREBOX/DOF.INI and PREBOX/DRV_DOF.DAT.
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/UTILITIES
  !!****

  USE overall
  USE utilities
  USE a_DOF

  PRIVATE
!------------------------------------------------------------------  
! Definition des différents types utilisables...
!------------------------------------------------------------------  
  TYPE M_DISK

     CHARACTER(len=5)           :: behav,bndry
     REAL(kind=8)               :: av_radius,percent,var !fd,gr_radius       
     INTEGER                    :: number
     REAL(kind=8)               :: rmin,rmax
     REAL(kind=8)               :: Vbegin
     REAL(kind=8),DIMENSION(3)  :: cooref

  END TYPE M_DISK
!------------------------------------------------------------------  
 TYPE M_JONC

    CHARACTER(len=5)           :: behav,bndry
    REAL(kind=8)               :: ax1,ax2
    REAL(kind=8)               :: av_radius !,gr_radius             
    REAL(kind=8)               :: Vbegin
    REAL(kind=8),DIMENSION(3)  :: cooref
    INTEGER                    :: bip

 END TYPE M_JONC
!------------------------------------------------------------------  
 TYPE M_POLYG

    CHARACTER(len=5)                     :: behav,bndry,ptype
    REAL(kind=8)                         :: av_radius !,gr_radius
    INTEGER                              :: vrtxnb
    REAL(kind=8)                         :: var,Hfactor,percent
    REAL(kind=8),DIMENSION(3)            :: cooref,Vbegin
    REAL(kind=8),DIMENSION(:,:),POINTER  :: vertex
    
 END TYPE M_POLYG
!------------------------------------------------------------------  
 TYPE M_TEX

    REAL(kind=8)     :: var
    REAL(kind=8)     :: lenght
    INTEGER          :: periode
    CHARACTER(len=5) :: behav,bndry

 END TYPE M_TEX
!------------------------------------------------------------------  

 TYPE(M_DISK) ,DIMENSION(:),ALLOCATABLE,PRIVATE :: DISKx,MAILLON
 TYPE(M_JONC) ,DIMENSION(4)            ,PRIVATE :: JONCx
 TYPE(M_POLYG),DIMENSION(:),ALLOCATABLE,PRIVATE :: POLYG,POLYG_BOX,Facette,BLOCHET
 TYPE(M_TEX)                           ,PRIVATE :: THREAD
 TYPE(M_DISK)                          ,PRIVATE :: DRUM

!------------------------------------------------------------------  
! variable globale ...

 INTEGER                   :: nbDISKx=0,nbPOLYG=0
 INTEGER                   :: nbROW=0,nbCOL=0,nbROW2=0,nbCOL2=0
 INTEGER                   :: spDISKx=0,spPOLYG=0,i_spDISKx=99,i_spPOLYG=99
 REAL(kind=8)              :: DISKx_radius_max=0.d0,DISKx_radius_min,POLYG_radius_max=0.D0,Hfactor, &
                              omega0,ANGLE,blw,blh,sdep,int_len,max_rad,ssch,brkh,brkw,EXTRAD,INTRAD
 INTEGER                   :: is,nbd,nbp,nbdt,NBRE=0,nb_inter
 INTEGER                   :: m,d_vlocy=0,p_vlocy=0,m_rugos = 0 
 INTEGER,DIMENSION(5)      :: ib=1       
 REAL(kind=8),DIMENSION(2) :: box_size        
 CHARACTER(len=5)          :: RUGOS='QUADR',rlbehav,blbehav,brk_behav,floor_behav
 CHARACTER(len=8)          :: MZZZ
 REAL(kind=8)              :: sq3 = 0.17320508D+01,sq2 = 0.14142135D+01, &
                              sq3_1 = 0.57735026D+01,sq2_1 = 0.70710678D+01
 REAL(kind=8)              :: psdim = 10.0             ! dimension de l'EPS (W ou bien H) 
 LOGICAL                   :: rail=.FALSE.,strate=.FALSE.
 LOGICAL                   :: imposedMailParam=.FALSE. ! .true.  pour Width  impose
                                                       ! .false. pour Height impose

 LOGICAL                   :: epsfile=.FALSE., Wflag=.TRUE.

 REAL(kind=8)              :: coor_src_x,coor_src_y
 REAL(kind=8)              :: grd_rd

 REAL(kind=8)              :: deep,factor 

 PUBLIC prepro_command_model

!------------------------------------------------------------------  

CONTAINS

 SUBROUTINE prepro_command_model

   IMPLICIT NONE       

   INTEGER           :: i,nfich
   CHARACTER(len=5)  :: behav,bndry,ptype
   REAL(kind=8)      :: av_radius,x,var,percent !,gr_radius
   CHARACTER(len=30) :: c_name
                            !123456789012345678901
   CHARACTER(len=21) :: IAM='prepro::command_model'

   logical           :: is_symmetriclayer =.TRUE.
   logical           :: is_rigid =.TRUE.

!- Valeur par default pour les joncs
!
   JONCx(1:4)%behav    ='JCxxx'
   JONCx(1:4)%bndry    ='JCxxx'
   JONCx(1:4)%ax1      = 0.0D+00
   JONCx(1:4)%ax2      = 0.0D+00
   JONCx(1:4)%av_radius= 0.0D+00
!fd   JONCx(1:4)%gr_radius= 0.0D+00

!- Valeur par default pour les echantillons de Texsol -
!
    THREAD%behav   ='TSxxx'
    THREAD%bndry   ='TSxxx'
    THREAD%periode = 5
    THREAD%var     = 0.0D+00
    THREAD%lenght  = 0.0D+00

!------------------------

    OPEN(UNIT=10,FILE=sample_bodies,STATUS='replace',IOSTAT=err)
    IF (err>0) THEN
      WRITE(*,*) ' !-------------------------------------------------------------!'
      WRITE(*,*) ' ! Error in mod_prepro while opening PREBOX/BODIES.DAT:        !'
      WRITE(*,*) ' !  You put the command PREPRO in COMMAND.DAT                  !'
      WRITE(*,*) ' !  but you might forget to create the folder PREBOX.          !'
      WRITE(*,*) ' !  Just type : mkdir PREBOX or comment the command            !'
      WRITE(*,*) ' !  in COMMAND.DAT ie #PREPRO                                  !'
      WRITE(*,*) ' !-------------------------------------------------------------!'
      STOP
    ENDIF

    OPEN(UNIT=20,FILE=sample_dof,STATUS='replace',IOSTAT=err)

    IF (err>0) THEN
      WRITE(*,*) ' !-------------------------------------------------------------!'
      WRITE(*,*) ' ! Error in mod_prepro while opening PREBOX/DOF.INI:           !'
      WRITE(*,*) ' !  You put the command PREPRO in COMMAND.DAT                  !'
      WRITE(*,*) ' !  but you might forget to create the folder PREBOX.          !'
      WRITE(*,*) ' !  Just type : mkdir PREBOX or comment the command            !'
      WRITE(*,*) ' !  in COMMAND.DAT ie #PREPRO                                  !'
      WRITE(*,*) ' !-------------------------------------------------------------!'
      STOP
    ENDIF
  
    CALL entete
    CALL top

    nfich=5
    OPEN(UNIT=nfich,FILE=in_sample,STATUS='old',IOSTAT=err)
    IF (err>0) THEN
      WRITE(*,*) ' !-------------------------------------------------------------!'
      WRITE(*,*) ' ! Error in mod_prepro while opening DATBOX/SAMPLE.DAT:        !'
      WRITE(*,*) ' !-------------------------------------------------------------!'
      STOP
    ENDIF

    is     = 0
    percent= 0.0D+00
    nbd    = 0
    nbdt   = 1

    DO
      READ(nfich,'(A30)') c_name

      IF (c_name(1:1) == '#') CYCLE

!- TEXSOL DIRECTIVES-------------------------------------

!                  123456789012345678901234567890
      IF(INDEX(c_name,'TEXSOL BEHAVIOUR')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(A5)') THREAD%behav
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'TEXSOL BOUNDARY')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(A5)') THREAD%bndry
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'TEXSOL PERIOD')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I5)') THREAD%periode
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'TEXSOL AVERAGE LENGTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') THREAD%lenght
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'TEXSOL LENGTH VARIATION')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') THREAD%var
        CYCLE
      ENDIF

!- BLOCHET DIRECTIVES -----------------------------------
!                      123456789012345
      IF(INDEX(c_name,'INTERVAL NUMBER')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I5)') nb_inter
        WRITE(*,'(I5)') nb_inter
        CYCLE
      ENDIF
!                      123456789012345
      IF(INDEX(c_name,'INTERVAL LENGTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') int_len
        WRITE(*,'(D14.7)') int_len
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'SLEEPER WIDTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') blw
        WRITE(*,'(D14.7)') blw
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'SLEEPER HEIGTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') blh
        WRITE(*,'(D14.7)') blh
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'SAMPLE DEPTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') sdep
        WRITE(*,'(D14.7)') sdep
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'SLEEPER BEHAVIOUR')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(A5)') blbehav
        WRITE(*,'(A5)') blbehav
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'RAIL BEHAVIOUR')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(A5)') rlbehav
        WRITE(*,'(A5)') rlbehav
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'MAX RADIUS PARTICLES')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') max_rad
        WRITE(*,'(D14.7)') max_rad
        !longueur de la sous couche
        ssch    = 2*blw+nb_inter*(int_len+blw)
        nbROW   = INT(sdep*0.75/max_rad)+1
        nbCOL   = INT(ssch*0.5/max_rad)+1
        nbPOLYG = nbCOL*NBROW
        nbROW2  = INT(blh*0.875/max_rad)+1
        nbCOL2  = INT((int_len-blw)*0.5/max_rad)
        nbPOLYG = nbPOLYG+nb_inter*(nbCOL2*nbROW2)
        IF(ALLOCATED(POLYG)) DEALLOCATE(POLYG)
        ALLOCATE(POLYG(nbPOLYG))

        IF(ALLOCATED(POLYG_BOX)) DEALLOCATE(POLYG_BOX)
        ALLOCATE(POLYG_BOX(4))

        IF(ALLOCATED(BLOCHET)) DEALLOCATE(BLOCHET)
        ALLOCATE(BLOCHET(1))
        rail=.TRUE.
        CYCLE
      ENDIF

!--------------------------------------------------------
! THE WALL DIRECTIVES
!
!                  123456789012345678901234567890
      IF(INDEX(c_name,'BRICK ON THE FIRST LAYER')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name     
        READ(nfich,'(I5)') nbCOL
        WRITE(*,'(I5)') nbCOL
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'NON SYMMETRIC LAYER')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        is_symmetriclayer=.FALSE.
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'DEFORMABLE BRICKS')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        is_rigid=.FALSE.
        CYCLE
      ENDIF

!                  123456789012345678901234567890
      IF(INDEX(c_name,'LAYERS OF BRICKS')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I5)') nbROW
        WRITE(*,'(I5)') nbROW
        if (is_symmetriclayer) then
!fd bof          nbPOLYG=INT(nbROW/2)*(nbCOL+1)+INT((nbROW+1)/2)*nbCOL
          nbPOLYG= nbROW*nbCOL+INT((nbROW+1)/2)
        else
          nbPOLYG= nbROW*nbCOL
        endif
        CYCLE
      ENDIF

!                  123456789012345678901234567890
      IF(INDEX(c_name,'BRICK HEIGTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') brkh
        WRITE(*,'(D14.7)') brkh
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'BRICK WIDTH')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(D14.7)') brkw
        WRITE(*,'(D14.7)') brkw
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'BRICK BEHAVIOUR')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(A5)') brk_behav
        WRITE(*,'(A5)') brk_behav
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'FLOOR BEHAVIOUR')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(A5)') floor_behav
        WRITE(*,'(A5)') floor_behav
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'WALL BUILDING')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        CALL mod_wall(is_symmetriclayer,is_rigid)
        CYCLE
      ENDIF

!--------------------------------------------------------
!- caracteristiques generales de l'echantillon

!                  123456789012345678901234567890
      IF(INDEX(c_name,'BODIES ON THE FIRST LAYER')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name     
        READ(nfich,'(I5)') nbCOL
        WRITE(*,'(I5)') nbCOL
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'NUMBER OF LAYERS')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I5)') nbROW
        WRITE(*,'(I5)') nbROW
        CYCLE
      ENDIF
!--------------------------------------------------------
!- DISK DATA DEFINITION
!                  123456789012345678901234567890
      IF(INDEX(c_name,'MAX NUMBER OF DISKx')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I7)') nbDISKx
        IF(ALLOCATED(DISKx)) DEALLOCATE(DISKx)
        ALLOCATE(DISKx(nbDISKx))
        WRITE(*,'(I7)') nbDISKx
        CYCLE
      ENDIF

      IF(INDEX(c_name,'NO DISKx VELOCITY')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name       
        d_vlocy=0
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'SCRAMBLE DISKx VELOCITY')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        d_vlocy=1
        CYCLE
      ENDIF

!                  123456789012345678901234567890
      IF(INDEX(c_name,'SORT OF DISKx')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I5)') spDISKx
        WRITE(*,'(I5)') spDISKx
!        i_spDISKx=0
!        cycle
!      endif

        IF (nbDISKx == 0) THEN
          PRINT*,'ERROR: Uncompatible command MAX NUMBER OF DISKs not defined'
          STOP
        ENDIF  

!      if ( nbDISKx /= 0 .and. i_spDISKx == 0) then

        DO i_spDISKx=1,spDISKx

         DO
         
          READ(nfich,'(A30)') c_name

          IF (c_name(1:1) == '#') CYCLE
          
!                     123456789012345678901234567890
          IF(INDEX(c_name,'PERCENT')==1)THEN
            WRITE(*,'(A1,A30)') '@',c_name
            READ(nfich,'(D14.7)') percent   
            WRITE(*,'(D14.7)') percent   
            is=is+1
            nbd = nbd + percent*(nbDISKx/100)
            CYCLE
          ENDIF
!                     123456789012345678901234567890

          IF(INDEX(c_name,'DISKx BOUNDARY')==1)THEN
            WRITE(*,'(A1,A30)') '@',c_name
            READ(nfich,'(A5)') bndry        
            WRITE(*,'(A5)') bndry
            CYCLE
          ENDIF
!                     123456789012345678901234567890
          IF(INDEX(c_name,'DISKx BEHAVIOUR')==1)THEN
            WRITE(*,'(A1,A30)') '@',c_name
            READ(nfich,'(A5)') behav        
            WRITE(*,'(A5)') behav
            CYCLE
          ENDIF
!                     123456789012345678901234567890
          IF(INDEX(c_name,'DISKx AVERAGE RADIUS')==1)THEN
            WRITE(*,'(A1,A30)') '@',c_name
            READ(nfich,'(D14.7)') av_radius
            WRITE(*,'(D14.7)') av_radius
!fd            gr_radius=av_radius*sqrt(2.)   
            var=0
            CYCLE
          ENDIF
!                     123456789012345678901234567890
!          if(INDEX(c_name,'DISKx GYRATION RADIUS')==1)then
!            write(*,'(A1,A30)') '@',c_name   
!            read(nfich,'(D14.7)') gr_radius
!            write(*,'(D14.7)') gr_radius
!            cycle
!          endif

!                     123456789012345678901234567890
          IF(INDEX(c_name,'DISKx RADIUS VARIATION')==1)THEN
            WRITE(*,'(A1,A30)') '@',c_name
            READ(nfich,'(D14.7)') var         
            WRITE(*,*) var
            DO i=nbdt,nbd
              CALL RANDOM_NUMBER(x)
              DISKx(i)%behav    = behav
              DISKx(i)%bndry    = bndry
              DISKx(i)%av_radius= av_radius+av_radius*(var/100.)*x
!fd              DISKx(i)%gr_radius= gr_radius+gr_radius*(var/100.)*x
              DISKx(i)%var      = var
            ENDDO
            nbdt=1+nbd

            IF(is == spDISKx) THEN
              IF(nbd == nbDISKx) EXIT
              CALL RANDOM_NUMBER(x)
              DISKx(nbdt:nbDISKx)%behav    = behav
              DISKx(nbdt:nbDISKx)%bndry    = bndry
              DISKx(nbdt:nbDISKx)%av_radius= av_radius+av_radius*(var/100.)*x
!fd              DISKx(nbdt:nbDISKx)%gr_radius= gr_radius+gr_radius*(var/100.)*x
              DISKx(nbdt:nbDISKx)%var      = var
            ENDIF
            EXIT
          ENDIF

          PRINT*,'ERROR: unknown command or mismatch in the order of the commands dd'
          PRINT*,c_name
          STOP 

         ENDDO 

        ENDDO 
        CYCLE
      ENDIF

!------------------------------------------------------------------------------
!- POLYGON DATA DEFINITIONS
!                  123456789012345678901234567890
      IF(INDEX(c_name,'MAX NUMBER OF POLYG')==1) THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I7)') nbPOLYG
        IF(ALLOCATED(POLYG)) DEALLOCATE(POLYG)
        ALLOCATE(POLYG(nbPOLYG))

        !manipulation obligatoire temps que les contacts PLJC ne sont pas actifs.
        IF(ALLOCATED(POLYG_BOX)) DEALLOCATE(POLYG_BOX)
        ALLOCATE(POLYG_BOX(4))
        IF(ALLOCATED(Facette)) DEALLOCATE(Facette)
        ALLOCATE(Facette(1))

        WRITE(*,'(I5)') nbPOLYG
        CYCLE
      ENDIF 
!                  123456789012345678901234567890
      IF(c_name == 'REGULAR POLYG                ') THEN
        WRITE(*,'(A1,A30)') '@',c_name
        MZZZ = 'REGULIER'
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'NO POLYG VELOCITY')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        p_vlocy=0
        CYCLE
      ENDIF
!                  123456789012345678901234567890
      IF(INDEX(c_name,'SCRAMBLE POLYG VELOCITY')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        p_vlocy=1
        CYCLE
      ENDIF

!                  123456789012345678901234567890
      IF(INDEX(c_name,'SORT OF POLYG')==1)THEN
        WRITE(*,'(A1,A30)') '@',c_name
        READ(nfich,'(I5)') spPOLYG
        WRITE(*,'(I5)') spPOLYG
!        i_spPOLYG=0
!        cycle
!      endif

        IF (nbPOLYG == 0) THEN 
          PRINT*,'ERROR: Uncompatible command MAX NUMBER OF POLYGONS not defined'
          STOP
        ENDIF  

!      if (nbPOLYG /= 0 .and. i_spPOLYG == 0 )then 

        DO i_spPOLYG=1,spPOLYG

          DO
          
            READ(nfich,'(A30)') c_name

            IF (c_name(1:1) == '#') CYCLE
          
            IF(INDEX(c_name,'PERCENT')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name
              READ(nfich,'(D14.7)') percent             
              WRITE(*,'(D14.7)') percent
              is=is+1
              nbd=percent*nbPOLYG/100+nbd
              CYCLE
            ENDIF
!                     123456789012345678901234567890
            IF(INDEX(c_name,'TYPE OF POLYG')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name
              READ(nfich,'(A5)') ptype        
              WRITE(*,'(A5)') ptype
              CYCLE
            ENDIF
!                     123456789012345678901234567890
            IF(INDEX(c_name,'POLYG BOUNDARY')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name  
              READ(nfich,'(A5)') bndry  
              WRITE(*,'(A5)') bndry
              CYCLE
            ENDIF
!                     123456789012345678901234567890
            IF(INDEX(c_name,'POLYG BEHAVIOUR')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name
              READ(nfich,'(A5)') behav        
              WRITE(*,'(A5)') behav
              CYCLE
            ENDIF
!                     123456789012345678901234567890
            IF(INDEX(c_name,'POLYG AVERAGE RADIUS')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name    
              READ(nfich,'(D14.7)') av_radius        
              WRITE(*,'(D14.7)') av_radius
              var=0.
              Hfactor=1.
              CYCLE
            ENDIF
!                     123456789012345678901234567890
!            if(INDEX(c_name,'POLYGON GYRATION RADIUS')==1)then
!              write(*,'(A1,A30)') '@',c_name
!              read(nfich,'(D14.7)') gr_radius        
!              write(*,'(D14.7)') gr_radius        
!              cycle
!            endif

!                     123456789012345678901234567890
            IF(INDEX(c_name,'POLYG RADIUS VARIATION')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name           
              READ(nfich,'(D14.7)') var 
              WRITE(*,'(D14.7)') var        
              CYCLE
            ENDIF
!                     123456789012345678901234567890
            IF(INDEX(c_name,'ELLIPTIC FACTOR               ')==1)THEN
              WRITE(*,'(A1,A30)') '@',c_name           
              READ(nfich,'(D14.7)') Hfactor 
              WRITE(*,'(D14.7)') Hfactor        

              SELECT CASE(ptype)
              CASE('TRIAN')

                DO i=nbdt,nbd
                  IF(ASSOCIATED(POLYG(i)%vertex)) DEALLOCATE(POLYG(i)%vertex)
                  ALLOCATE(POLYG(i)%vertex(2,3))
                  POLYG(i)%vrtxnb=3
                ENDDO
              CASE('QUADR')
                DO i=nbdt,nbd
                  IF(ASSOCIATED(POLYG(i)%vertex)) DEALLOCATE(POLYG(i)%vertex)
                  ALLOCATE(POLYG(i)%vertex(2,4))
                  POLYG(i)%vrtxnb=4
                ENDDO
              CASE('PENTA')
                DO i=nbdt,nbd
                  IF(ASSOCIATED(POLYG(i)%vertex)) DEALLOCATE(POLYG(i)%vertex)
                  ALLOCATE(POLYG(i)%vertex(2,5))
                  POLYG(i)%vrtxnb=5
                ENDDO
              CASE('HEXAG')
                DO i=nbdt,nbd
                  IF(ASSOCIATED(POLYG(i)%vertex)) DEALLOCATE(POLYG(i)%vertex)
                  ALLOCATE(POLYG(i)%vertex(2,6))
                  POLYG(i)%vrtxnb=6
                ENDDO
              CASE('HEPTA')
                DO i=nbdt,nbd
                  IF(ASSOCIATED(POLYG(i)%vertex)) DEALLOCATE(POLYG(i)%vertex)
                  ALLOCATE(POLYG(i)%vertex(2,7))
                  POLYG(i)%vrtxnb=7
                ENDDO
              CASE default
                PRINT*,'WARNING! UNKNOWN POLYGON TYPE'
                STOP
              END SELECT
           
              POLYG(nbdt:nbd)%behav = behav
              POLYG(nbdt:nbd)%bndry = bndry

              DO i=nbdt,nbd
                CALL RANDOM_NUMBER(x)
                POLYG(i)%av_radius=av_radius+av_radius*(var/100.)*x
!fd                POLYG(i)%gr_radius=gr_radius+gr_radius*(var/100.)*x
              ENDDO
              POLYG(nbdt:nbd)%var      =var
              POLYG(nbdt:nbd)%Hfactor  =Hfactor
              nbdt=1+nbd

              IF(is == spPOLYG) THEN
                IF(nbd == nbPOLYG) EXIT
                CALL RANDOM_NUMBER(x)
                DO i=nbdt,nbPOLYG
                  IF(ASSOCIATED(POLYG(i)%vertex)) DEALLOCATE(POLYG(i)%vertex)
                  ALLOCATE(POLYG(i)%vertex(2,5))
                  POLYG(i)%vrtxnb   = 5
                  POLYG(i)%behav    = behav
                  POLYG(i)%bndry    = bndry
                  POLYG(i)%av_radius= av_radius+av_radius*(var/100.)*x
!fd                  POLYG(i)%gr_radius= gr_radius+gr_radius*(var/100.)*x
                  POLYG(i)%var      = var
                  POLYG(i)%Hfactor  = Hfactor
                ENDDO
              ENDIF
              EXIT
            ENDIF

            PRINT*,'ERROR: unknown command or mismatch in the order of the commands'
            PRINT*,c_name
            STOP 

          ENDDO
        ENDDO
        CYCLE
      ENDIF
!-----------------------------------------------------------------------------
! AUXILLIARY DATA
!
!                  123456789012345678901234567890

!                                 2
!                                ----
!  pour les boites avec jonc  3 |    | 4
!                                ----
!                                  1

!                  123456789012345678901234567890
     IF(INDEX(c_name,'JONCx BEHAVIOUR')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(A5)') behav
       WRITE(*,'(A5)') behav
       JONCx(1:4)%behav=behav
       CYCLE
     ENDIF
!                  123456789012345678901234567890
     IF(INDEX(c_name,'JONCx UP BEHAVIOUR')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(A5)') JONCx(2)%behav
       WRITE(*,'(A5)') JONCx(2)%behav
       CYCLE
     ENDIF
!                  123456789012345678901234567890
     IF(INDEX(c_name,'JONCx DOWN BEHAVIOUR')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(A5)') JONCx(1)%behav
       WRITE(*,'(A5)') JONCx(1)%behav
       CYCLE
    ENDIF
!                  123456789012345678901234567890     
     IF(INDEX(c_name,'JONCx RIGHT BEHAVIOUR')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(A5)') JONCx(4)%behav
       WRITE(*,'(A5)') JONCx(4)%behav
       CYCLE
     ENDIF     
!                  123456789012345678901234567890     
     IF(INDEX(c_name,'JONCx LEFT BEHAVIOUR')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(A5)') JONCx(3)%behav
       WRITE(*,'(A5)') JONCx(3)%behav
       CYCLE
     ENDIF     
!                  123456789012345678901234567890     
     IF(INDEX(c_name,'JONCx AVERAGE RADIUS')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') av_radius
       WRITE(*,'(D14.7)') av_radius
       JONCx(1:4)%av_radius=av_radius
       CYCLE
     ENDIF     
!                  123456789012345678901234567890     
!     if(INDEX(c_name,'JONC GYRATION RADIUS')==1)then
!      write(*,'(A1,A30)') '@',c_name
!       read(nfich,'(D14.7)') gr_radius
!       write(*,'(D14.7)') gr_radius
!       JONCx(1:4)%gr_radius=gr_radius
!       cycle
!     endif

!                  123456789012345678901234567890  
     IF(INDEX(c_name,'JONCx UP VELOCITY')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') JONCx(2)%Vbegin
       JONCx(2)%bip=1
       WRITE(*,'(D14.7)') JONCx(2)%Vbegin
       CYCLE
     ENDIF
!                  123456789012345678901234567890  
     IF(INDEX(c_name,'JONCx DOWN VELOCITY')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') JONCx(1)%Vbegin
       JONCx(1)%bip=1
       WRITE(*,'(D14.7)') JONCx(1)%Vbegin
       CYCLE
     ENDIF     
!                  123456789012345678901234567890       
     IF(INDEX(c_name,'JONCx RIGHT VELOCITY')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') JONCx(4)%Vbegin
       JONCx(4)%bip=1
       WRITE(*,'(D14.7)') JONCx(4)%Vbegin
     ENDIF     

!                  123456789012345678901234567890       
     IF(INDEX(c_name,'JONCx LEFT VELOCITY')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name        
       READ(nfich,'(D14.7)') JONCx(3)%Vbegin
       JONCx(3)%bip=1
       WRITE(*,'(D14.7)') JONCx(3)%Vbegin
       CYCLE
     ENDIF

!                  123456789012345678901234567890       
     IF(INDEX(c_name,'JONCx HORIZONTAL LENGTH')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(D14.7)') JONCx(2)%ax1
       WRITE(*,'(D14.7)') JONCx(2)%ax1
       JONCx(1)%ax1=JONCx(2)%ax1
       CYCLE
     ENDIF
!                  123456789012345678901234567890       
     IF(INDEX(c_name,'JONCx HORIZONTAL DEPTH')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(D14.7)') JONCx(2)%ax2
       WRITE(*,'(D14.7)') JONCx(2)%ax2
       JONCx(1)%ax2=JONCx(2)%ax2  
       CYCLE
     ENDIF     

!                  123456789012345678901234567890       
     IF(INDEX(c_name,'JONCx VERTICAL LENGTH')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name  
       READ(nfich,'(D14.7)') JONCx(3)%ax1
       WRITE(*,'(D14.7)') JONCx(3)%ax1        
       JONCx(4)%ax1=JONCx(3)%ax1
       CYCLE
     ENDIF
!                  123456789012345678901234567890       
     IF(INDEX(c_name,'JONCx VERTICAL DEPTH')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(D14.7)') JONCx(3)%ax2
       WRITE(*,'(D14.7)') JONCx(3)%ax2
       JONCx(4)%ax2=JONCx(3)%ax2
       CYCLE
     ENDIF


!                  123456789012345678901234567890       
     IF(INDEX(c_name,'DRUM BEHAVIOUR')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       IF(nbDISKx /=0)THEN
         READ(nfich,'(A5)') DRUM%behav
         WRITE(*,'(A5)') DRUM%behav
       ENDIF
       IF(nbPOLYG /=0)THEN
         READ(nfich,'(A5)') Facette%behav
         WRITE(*,'(A5)') Facette%behav
       ENDIF
       CYCLE
     ENDIF
!                  123456789012345678901234567890       
     IF(INDEX(c_name,'DRUM BOUNDARY')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       IF(nbDISKx /=0)THEN
         READ(nfich,'(A5)') DRUM%bndry
         WRITE(*,'(A5)') DRUM%bndry
       ENDIF
       IF(nbPOLYG /=0)THEN
         READ(nfich,'(A5)') Facette%bndry
         WRITE(*,'(A5)') Facette%bndry
       ENDIF
       CYCLE
     ENDIF
!                  123456789012345678901234567890       
     IF(INDEX(c_name,'DRUM ROTATION')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') omega0
       WRITE(*,'(D14.7)') omega0
       CYCLE
     ENDIF 

!                     123456789012345678901234567890
     IF(INDEX(c_name,'BOTTOM ANGLE')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(D14.7)') ANGLE
       WRITE(*,'(D14.7)') ANGLE
       CYCLE
     ENDIF
!                     123456789012345678901234567890
     IF(INDEX(c_name,'INTERNAL RADIUS')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       READ(nfich,'(D14.7)') INTRAD
       WRITE(*,'(D14.7)') INTRAD
       CYCLE
     ENDIF

!---------------------------------------------------------------------------
! End of the definition of all bodies. Definition of the walls of the sample
!
!                     123456789012345678901234567890

     IF(INDEX(c_name,'STRATES')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name       
       strate=.TRUE.
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'TEXSOL SAMPLE')==1) THEN
       WRITE(*,'(A1,A30)') '@',c_name
       DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
       DISKx_radius_min=MINVAL(DISKx(:)%av_radius)
       CALL TEX_SOL_NETWORK
       CALL mod_jonc(1)
       CYCLE
     ENDIF
!                  123456789012345678901234567890
     IF(INDEX(c_name,'COUETTE SAMPLE')==1) THEN
       WRITE(*,'(A1,A30)') '@',c_name
       DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
       DISKx_radius_min=MINVAL(DISKx(:)%av_radius)
       CALL COUETTE_SAMPLE
       CYCLE
     ENDIF

!vr                   123456789012345678901234567890
     IF(INDEX(c_name,'NETWORK PARAMETER')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') DISKx_radius_max
       WRITE(*,'(D14.7)') DISKx_radius_max
       DISKx_radius_max = DISKx_radius_max/2
       POLYG_radius_max = DISKx_radius_max
       imposedMailParam=.TRUE.
       CYCLE
     ENDIF

!                  123456789012345678901234567890   
     IF(INDEX(c_name,'SQUARE NETWORK')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       IF (.NOT. imposedMailParam) THEN !vr
         IF( nbDISKx /= 0 ) DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
         IF( nbPOLYG /= 0 ) POLYG_radius_max=MAXVAL(POLYG(:)%av_radius*POLYG(:)%Hfactor)
       END IF
       CALL SQUARE_NETWORK
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'UP TRIANGLE NETWORK')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       IF (.NOT. imposedMailParam) THEN !vr
         IF( nbDISKx /= 0 ) DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
         IF( nbPOLYG /= 0 ) POLYG_radius_max=MAXVAL(POLYG(:)%av_radius*POLYG(:)%Hfactor)
       END IF
       CALL UP_TRIANGLE_NETWORK
       CYCLE
     ENDIF     

!                  123456789012345678901234567890
     IF(INDEX(c_name,'DOWN TRIANGLE NETWORK')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       IF (.NOT. imposedMailParam) THEN !vr       
         IF( nbDISKx /= 0 ) DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
         IF( nbPOLYG /= 0 ) POLYG_radius_max=MAXVAL(POLYG(:)%av_radius*POLYG(:)%Hfactor)
       END IF
       CALL DOWN_TRIANGLE_NETWORK
       CYCLE
     ENDIF

!                        123456789012345678901234567890
     IF(INDEX(c_name,'PYRAMIDAL STACKING')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name      
       IF( nbDISKx /= 0 ) DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
       IF( nbPOLYG /= 0 ) POLYG_radius_max=MAXVAL(POLYG(:)%av_radius*POLYG(:)%Hfactor)
       CALL PYRAMIDAL_STACKING
       CYCLE
     ENDIF

     IF(INDEX(c_name,'SOURCE POINT')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name      
       READ(nfich,*) coor_src_x,coor_src_y
       IF( nbDISKx /= 0 ) DISKx_radius_max=MAXVAL(DISKx(:)%av_radius)
       IF( nbPOLYG /= 0 ) POLYG_radius_max=MAXVAL(POLYG(:)%av_radius*POLYG(:)%Hfactor)
       CALL SOURCE_POINT
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'DRUM SAMPLE')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CALL re_drum
       CYCLE
     ENDIF
!                  123456789012345678901234567890
     IF(INDEX(c_name,'RUGOSITE')==1)THEN
       READ(nfich,'(A5)') RUGOS
       WRITE(*,'(A1,A30)') '@',c_name
       WRITE(*,'(A5)') RUGOS
       m_rugos = 1
       CYCLE
     ENDIF

     IF(INDEX(c_name,'GIVEN BOX SIZE')==1)THEN
       READ(nfich,*) box_size(1),box_size(2)
       WRITE(*,'(A1,A30)') '@',c_name
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'HORIZONTAL DISKS THREAD')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CALL CHAINETTE_DISKx(1)
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'OPEN BOX DISKS THREAD')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CALL CHAINETTE_DISKx(2)
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'GROUND DISKS THREAD')==1)THEN
       READ(nfich,*) grd_rd
       WRITE(*,'(A1,A30)') '@',c_name
       CALL CHAINETTE_DISKx(3)
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'SPECIAL BOX')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CALL special_box
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'CLOSED BOX')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       m=1
       IF( m_rugos == 0 )THEN
         CALL mod_jonc(m)
       ELSE
         CALL mod_rugosite(m)
       ENDIF
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'OPEN BOX')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       m=2
       IF( m_rugos == 0 )THEN
         CALL mod_jonc(m)
       ELSE
         CALL mod_rugosite(m)
       ENDIF
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'ETAU')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       m=3
       IF( m_rugos == 0 )THEN
         CALL mod_jonc(m)
       ELSE
         CALL mod_rugosite(m)
       ENDIF
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'PLAN')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       m=4
       IF( m_rugos == 0 )THEN
         CALL mod_jonc(m)
       ELSE
         CALL mod_rugosite(m)
       ENDIF
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'SILO')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CALL mod_silo
       CYCLE
     ENDIF

!                  123456789012345678901234567890
     IF(INDEX(c_name,'RAILWAY SAMPLE')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CALL mod_railway
       CYCLE
     ENDIF

!fd a finir
!                  123456789012345678901234567890
     IF(INDEX(c_name,'3D EXTRUSION')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') deep 
       READ(nfich,'(D14.7)') factor
!       CALL extrusion(deep,factor)
       CYCLE
     ENDIF




!vr                   123456789012345678901234567890
     IF(INDEX(c_name,'EPS WIDTH')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') psdim
       WRITE(*,'(D14.7)') psdim  
       Wflag=.TRUE.
       CYCLE
     ENDIF
!vr                   123456789012345678901234567890
     IF(INDEX(c_name,'EPS HEIGTH')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       READ(nfich,'(D14.7)') psdim
       WRITE(*,'(D14.7)') psdim
       Wflag=.FALSE.
       CYCLE
     ENDIF
!vr                   123456789012345678901234567890
     IF(INDEX(c_name,'EPS SAMPLE PREVIEW')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       epsfile=.TRUE.
       CYCLE
     ENDIF


!                     123456789012345678901234567890
     IF(INDEX(c_name,'END')==1)THEN
       WRITE(*,'(A1,A30)') '@',c_name
       CLOSE(10)
       CLOSE(20)
       IF (epsfile) CALL bodies2eps !vr il faut le faire apres avoir ferme BODIES.DAT
!                        123456789012345678901234567890
       WRITE(*,'(A30)') 'END OF THE CREATION OF YOUR   '
       WRITE(*,'(A30)') 'SAMPLE.                       '
       WRITE(*,'(A30)') '                              '
       WRITE(*,'(A30)') 'PLEASE, RESTART YOUR PROGRAM  '
       WRITE(*,'(A30)') 'WHITOUT THE "PREPRO" DIRECTIVE'
       STOP
     ENDIF

     PRINT*,'ERROR: Unknown Command'
     PRINT*,c_name
     PRINT*,'CHECK DOCUMENTATION'
     STOP

   ENDDO

 END SUBROUTINE prepro_command_model
!------------------------------------------------------------------------------------------------------------------
 SUBROUTINE TEX_SOL_NETWORK

   IMPLICIT NONE  

   INTEGER                    :: nb_LINE,nb_disk,nb_maillon,nfwd,nbwd,NDISKx,NBRE
   REAL(kind=8)               :: radius_min,radius_max,radius,lvar
   INTEGER                    :: i,j,rang,IAL1,IAL2,ik,ikl,ikr
   REAL(kind=8),DIMENSION(3)  :: Vbegin    
   REAL(kind=8)               :: DA,x
   TYPE(M_DISK)               :: dik

!   if( nbCOL /= nbROW ) then
!      print*,'WARNING ! For TEXSOL sample, nbCOL must be equal to nbROW. Sorry, you must modified your file SAMPLE.DAT'
!      stop
!   endif


   nb_LINE = nbROW/THREAD%periode
   nbROW   = nbROW-nb_LINE
   radius  = DISKx_radius_min*0.5
   NBRE    = 0
   NDISKx  = 0

   box_size(1) = 2.*DISKx_radius_max*nbCOL
   box_size(2) = 2.*DISKx_radius_max*nbROW !change

   IF(.NOT.strate)THEN
     DO i=1,nbDISKx/2
       CALL RANDOM_NUMBER(DA)
       IAL1=INT(DA*nbDISKx)+1
       IAL1=MIN(IAL1,nbDISKx)
       IAL1=MAX(1,IAL1)
       
       CALL RANDOM_NUMBER(DA)
       IAL2=IDINT(DA*nbDISKx)+1
       IAL2=MIN(IAL2,nbDISKx)
       IAL2=MAX(1,IAL2)
 
       dik= DISKx(IAL1)
       DISKx(IAL1)=DISKx(IAL2)    
       DISKx(IAL2)=Dik
     END DO
   ENDIF

   ALLOCATE(MAILLON(1))

   MAILLON(1)%behav    = THREAD%behav
   MAILLON(1)%bndry    = THREAD%bndry
   MAILLON(1)%av_radius= radius
!fd   MAILLON(1)%gr_radius= radius*sq2_1
   MAILLON(1)%var      = 0.0D+00
   MAILLON(1)%percent  = 100.D+00
   MAILLON(1)%number   = 1
   MAILLON(1)%rmin     = radius
   MAILLON(1)%rmax     = radius
   MAILLON(1)%Vbegin   = 0.0D+00
   MAILLON(1)%cooref(3)= 0.0D+00


   DO ik=1,nb_LINE

     CALL RANDOM_NUMBER(lvar)
     lvar=INT(THREAD%var*lvar)+1
      
     IF( THREAD%lenght == 0.D0)THEN
       nb_maillon = box_size(1)/(6.*radius)
       nb_disk    = 2.*box_size(1)/(6.*DISKx_radius_max)
     ELSE
       nb_maillon = (THREAD%lenght*(1.+lvar/100.))/(2.*radius)
       nb_disk    = (box_size(1)-THREAD%lenght*(1.+lvar/100.))/(2.*DISKx_radius_max)
     ENDIF

     IF(THREAD%lenght*(1.+lvar/100.).GT.box_size(1))THEN
       nb_disk    = 0
       nb_maillon = box_size(1)/(2.*radius)
     ENDIF

     ikl = 1+(THREAD%periode+1)*(ik-1)
     ikr = MIN(THREAD%periode+(THREAD%periode+1)*(ik-1),nbCOL)

     DO i=ikl,ikr
       DO j=1,nbCOL
         NBRE=NBRE+1
         NDISKx=NDISKx+1
         DISKx(NDISKx)%cooref(1)= (2.*j-1.)*DISKx_radius_max
         DISKx(NDISKx)%cooref(2)= (2.*i-1.)*DISKx_radius_max
         DISKx(NDISKx)%cooref(3)=  0.
         CALL write_mod_BODIES(NBRE,'RBDY2')
         CALL write_mod_PLAIN(NDISKx,'DISKx')
         CALL write_mod_NO3xx(NDISKx,'DISKx')
         CALL write_mod_BDARY(NDISKx,'DISKx')
         IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
         IF(d_vlocy == 0) Vbegin = 0.
         CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
       ENDDO
     ENDDO

     IF(ikr == nbCOL) EXIT
       
     CALL RANDOM_NUMBER(x)
     nbwd=INT(x*nb_disk)+1
     nfwd=nb_disk-nbwd


     i=ikr+1

     DO j=1,nbwd
       NBRE=NBRE+1
       NDISKx=NDISKx+1
       DISKx(NDISKx)%cooref(1)= (2.*j-1.)*DISKx_radius_max
       DISKx(NDISKx)%cooref(2)= (2.*i-1.)*DISKx_radius_max
       DISKx(NDISKx)%cooref(3)=  0.
       CALL write_mod_BODIES(NBRE,'RBDY2')
       CALL write_mod_PLAIN(NDISKx,'DISKx')
       CALL write_mod_NO3xx(NDISKx,'DISKx')
       CALL write_mod_BDARY(NDISKx,'DISKx')
       IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
       IF(d_vlocy == 0) Vbegin = 0.
       CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
     ENDDO

     DO j=1,nb_maillon
       NBRE=NBRE+1
       Vbegin = 0.
       MAILLON(1)%cooref(1)= 2.*nbwd*DISKx_radius_max+(2.*j-1.)*radius
       MAILLON(1)%cooref(2)=(2.*i-1.)*DISKx_radius_max
       CALL write_mod_BODIES(NBRE,'RBDY2')
       CALL write_mod_PLAIN(1,'MAILL')
       CALL write_mod_NO3xx(1,'MAILL')
       CALL write_mod_BDARY(1,'MAILL')
       CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
     ENDDO

     DO j=1,nfwd
       NBRE=NBRE+1
       NDISKx=NDISKx+1
       DISKx(NDISKx)%cooref(1)=  2.*nbwd*DISKx_radius_max+2.*nb_maillon*radius+(2.*j-1.)*DISKx_radius_max
       DISKx(NDISKx)%cooref(2)= (2.*i-1.)*DISKx_radius_max
       DISKx(NDISKx)%cooref(3)=  0.
       CALL write_mod_BODIES(NBRE,'RBDY2')
       CALL write_mod_PLAIN(NDISKx,'DISKx')
       CALL write_mod_NO3xx(NDISKx,'DISKx')
       CALL write_mod_BDARY(NDISKx,'DISKx')
       IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
       IF(d_vlocy == 0) Vbegin = 0.
       CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
     ENDDO
   ENDDO

 END SUBROUTINE TEX_SOL_NETWORK
!-------------------------------------------------------------------------------------------------
 SUBROUTINE COUETTE_SAMPLE

    IMPLICIT NONE

    INTEGER                    :: i,j,nbpi,NP,IAL1,IAL2
    REAL(kind=8),DIMENSION(3)  :: Vbegin    
    REAL(kind=8)               :: DA,Ri,RR,alpha,beta,alpha0
    TYPE(M_DISK)               :: dik
    LOGICAL                    :: last_layer=.FALSE.

    NBRE = 0  

    DO i=1,nbDISKx/2
      CALL RANDOM_NUMBER(DA)
      IAL1=INT(DA*nbDISKx)+1
      IAL1=MIN(IAL1,nbDISKx)
      IAL1=MAX(1,IAL1)       
      CALL RANDOM_NUMBER(DA)
      IAL2=IDINT(DA*nbDISKx)+1
      IAL2=MIN(IAL2,nbDISKx)
      IAL2=MAX(1,IAL2)        
      dik= DISKx(IAL1)
      DISKx(IAL1)=DISKx(IAL2)
      DISKx(IAL2)=Dik        
    END DO

    Ri = 3.*DISKx_radius_max
    NP = 0
    RR = INTRAD
    alpha = PI_g/Ri

    DO
      RR = RR+Ri
      nbpi = INT(alpha*RR)+1
      IF(NP+nbpi > nbDISKx)THEN
        last_layer=.TRUE.
        nbpi = nbDISKx-NP
      ENDIF

      IF(nbpi == 0) EXIT

      CALL RANDOM_NUMBER(DA)
      alpha0 = DA*PI_g

      DO i=1,nbpi
        NBRE=NBRE+1
        beta=6.283185307_8/nbpi
        DISKx(NBRE)%cooref(1)= RR*COS(beta*i+alpha0)
        DISKx(NBRE)%cooref(2)= RR*SIN(beta*i+alpha0)
        DISKx(NBRE)%cooref(3)=  0.D0

        CALL write_mod_BODIES(NBRE,'RBDY2')
        CALL write_mod_PLAIN(NBRE,'DISKx')
        CALL write_mod_NO3xx(NBRE,'DISKx')
        CALL write_mod_BDARY(NBRE,'DISKx')
        IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
        IF(d_vlocy == 0) Vbegin = 0.
        CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
      ENDDO

      NP=NP+nbpi
      IF(last_layer) EXIT
    ENDDO

    EXTRAD = RR+Ri

    CALL write_couette_data

  END SUBROUTINE COUETTE_SAMPLE
!------------------------------------------------------------------------------------------------------------------
 SUBROUTINE SQUARE_NETWORK

    IMPLICIT NONE

    INTEGER                    :: i,j,rang,IAL1,IAL2
    REAL(kind=8),DIMENSION(3)  :: Vbegin    
    REAL(kind=8)               :: DA
    TYPE(M_DISK)               :: dik
    TYPE(M_POLYG)              :: pik

    NBRE = 0  
    rang = 0

    IF (( nbDISKx /= 0).AND.( nbPOLYG /= 0)) THEN
      PRINT*,'WARNING ! DISKx and POLYG can not appear in the same sample. Sorry!'
      STOP
    ENDIF

    IF ( nbDISKx /= 0) THEN

      IF(.NOT.strate)THEN
        DO i=1,nbDISKx/2
          CALL RANDOM_NUMBER(DA)
          IAL1=INT(DA*nbDISKx)+1
          IAL1=MIN(IAL1,nbDISKx)
          IAL1=MAX(1,IAL1)       
          CALL RANDOM_NUMBER(DA)
          IAL2=IDINT(DA*nbDISKx)+1
          IAL2=MIN(IAL2,nbDISKx)
          IAL2=MAX(1,IAL2)        
          dik= DISKx(IAL1)
          DISKx(IAL1)=DISKx(IAL2)
          DISKx(IAL2)=Dik        
        END DO
      ENDIF

      DO i=1,nbROW
        DO j=1,nbCOL

          NBRE=j+(i-1)*nbCOL
             
          DISKx(NBRE)%cooref(1)= (2.*j-1.)*DISKx_radius_max
          DISKx(NBRE)%cooref(2)= (2.*i-1.)*DISKx_radius_max
          DISKx(NBRE)%cooref(3)=  0.

          CALL write_mod_BODIES(NBRE,'RBDY2')
          CALL write_mod_PLAIN(NBRE,'DISKx')
          CALL write_mod_NO3xx(NBRE,'DISKx')
          CALL write_mod_BDARY(NBRE,'DISKx')
          IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
          IF(d_vlocy == 0) Vbegin = 0.
          CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
        ENDDO
      ENDDO
      box_size(1)=2.*DISKx_radius_max*nbCOL
      box_size(2)=2.*DISKx_radius_max*nbROW
    ENDIF

    IF ( nbPOLYG /= 0) THEN

      IF(.NOT.strate)THEN    
        DO i=1,nbPOLYG/2
          CALL RANDOM_NUMBER(DA)
          IAL1=INT(DA*nbPOLYG)+1
          IAL1=MIN(IAL1,nbPOLYG)
          IAL1=MAX(1,IAL1)

          CALL RANDOM_NUMBER(DA)
          IAL2=IDINT(DA*nbPOLYG)+1
          IAL2=MIN(IAL2,nbPOLYG)
          IAL2=MAX(1,IAL2)

          pik= POLYG(IAL1)
          POLYG(IAL1)=POLYG(IAL2)
          POLYG(IAL2)=pik

        END DO
      ENDIF


      DO i=1,nbROW

        DO j=1,nbCOL
          NBRE=j+((i-1)*nbCOL)
             
          POLYG(NBRE)%cooref(1)= (2.*j-1.)*POLYG_radius_max
          POLYG(NBRE)%cooref(2)= (2.*i-1.)*POLYG_radius_max
          POLYG(NBRE)%cooref(3)=  0.
             
          CALL write_mod_BODIES(NBRE,'RBDY2')
          CALL write_mod_PLAIN(NBRE,'POLYG')
          CALL write_mod_NO3xx(NBRE,'POLYG')
          CALL write_mod_BDARY(NBRE,'POLYG')
          IF(p_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
          IF(p_vlocy == 0) Vbegin = 0.
          CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
        ENDDO
      ENDDO
      box_size(1)=2.*POLYG_radius_max*nbCOL
      box_size(2)=2.*POLYG_radius_max*nbROW
    ENDIF

  END SUBROUTINE SQUARE_NETWORK
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE UP_TRIANGLE_NETWORK

    IMPLICIT NONE

    INTEGER                    :: i,j,rang,IAL1,IAL2
    REAL(kind=8),DIMENSION(3)  :: Vbegin
    REAL(kind=8)               :: DA
    TYPE(M_DISK)               :: dik
    TYPE(M_POLYG)              :: pik

    IF (( nbDISKx /= 0).AND.( nbPOLYG /= 0)) THEN
       PRINT*,'WARNING ! DISKx and POLYG can not appear in the same sample. Sorry!'
       STOP
    ENDIF

    IF ( nbDISKx /= 0) THEN
      NBRE=0  
      rang=0

      IF(.NOT.strate)THEN
      DO i=1,nbDISKx/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbDISKx)+1
        IAL1=MIN(IAL1,nbDISKx)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=INT(DA*nbDISKx)+1
        IAL2=MIN(IAL2,nbDISKx)
        IAL2=MAX(1,IAL2)
        dik= DISKx(IAL1)
        DISKx(IAL1)=DISKx(IAL2)
        DISKx(IAL2)=Dik
      END DO
      ENDIF

      DO i=1,nbROW
        IF(MOD(i,2).EQ.0)THEN
          DO j=1,nbCOL
            NBRE=j+rang

            DISKx(NBRE)%cooref(1)= (2.*j-1.)*DISKx_radius_max
            DISKx(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*DISKx_radius_max
            DISKx(NBRE)%cooref(3)=  0.

            CALL write_mod_BODIES(NBRE,'RBDY2')
            CALL write_mod_PLAIN(NBRE,'DISKx')
            CALL write_mod_NO3xx(NBRE,'DISKx')
            CALL write_mod_BDARY(NBRE,'DISKx')
            IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
            IF(d_vlocy == 0) Vbegin = 0.
            CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
          ENDDO
          rang=nbCOL+rang
        ELSE
          DO j=1,nbCOL-1
            NBRE=j+rang
                
            DISKx(NBRE)%cooref(1)= 2.*j*DISKx_radius_max
            DISKx(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*DISKx_radius_max
            DISKx(NBRE)%cooref(3)= 0.
                
            CALL write_mod_BODIES(NBRE,'RBDY2')
            CALL write_mod_PLAIN(NBRE,'DISKx')
            CALL write_mod_NO3xx(NBRE,'DISKx')
            CALL write_mod_BDARY(NBRE,'DISKx')
            IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
            IF(d_vlocy == 0) Vbegin = 0.
            CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
          ENDDO
          rang=nbCOL-1+rang
        ENDIF
      ENDDO
      box_size(1) = 2.*DISKx_radius_max*nbCOL
      box_size(2) = DISKx_radius_max*(1.+nbROW*sq3)
    ENDIF

    IF ( nbPOLYG /= 0) THEN
      NBRE=0  
      rang=0

      IF(.NOT.strate)THEN
      DO i=1,nbPOLYG/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbPOLYG)+1
        IAL1=MIN(IAL1,nbPOLYG)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=INT(DA*nbPOLYG)+1
        IAL2=MIN(IAL2,nbPOLYG)
        IAL2=MAX(1,IAL2)
        pik= POLYG(IAL1)
        POLYG(IAL1)=POLYG(IAL2)
        POLYG(IAL2)=pik
      END DO
      ENDIF

      DO i=1,nbROW
        IF(MOD(i,2).EQ.0)THEN
          DO j=1,nbCOL
            NBRE=j+rang

            POLYG(NBRE)%cooref(1)= (2.*j-1.)*POLYG_radius_max
            POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
            POLYG(NBRE)%cooref(3)= 0.

            CALL write_mod_BODIES(NBRE,'RBDY2')
            CALL write_mod_PLAIN(NBRE,'POLYG')
            CALL write_mod_NO3xx(NBRE,'POLYG')
            CALL write_mod_BDARY(NBRE,'POLYG')
            IF(p_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
            IF(p_vlocy == 0) Vbegin = 0.
            CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
          ENDDO
          rang=nbCOL+rang
        ELSE
          DO j=1,nbCOL-1
            NBRE=j+rang
             
            POLYG(NBRE)%cooref(1)= 2.*j*POLYG_radius_max
            POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
            POLYG(NBRE)%cooref(3)= 0.
    
            CALL write_mod_BODIES(NBRE,'RBDY2')
            CALL write_mod_PLAIN(NBRE,'POLYG')
            CALL write_mod_NO3xx(NBRE,'POLYG')
            CALL write_mod_BDARY(NBRE,'POLYG')
            IF(p_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
            IF(p_vlocy == 0) Vbegin = 0.
            CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
          ENDDO
          rang=nbCOL-1+rang
        ENDIF
      ENDDO
      box_size(1) = 2.*POLYG_radius_max*nbCOL
      box_size(2) = POLYG_radius_max*(1.+nbROW*sq3)
    ENDIF

  END SUBROUTINE  UP_TRIANGLE_NETWORK
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE DOWN_TRIANGLE_NETWORK

    IMPLICIT NONE

    INTEGER                    :: i,j,rang,IAL1,IAL2
    REAL(kind=8),DIMENSION(3)  :: Vbegin
    REAL(kind=8)               :: DA
    TYPE(M_DISK)               :: dik
    TYPE(M_POLYG)              :: pik

    IF (( nbDISKx /= 0).AND.( nbPOLYG /= 0)) THEN
      PRINT*,'WARNING ! DISKx and POLYG can not appear in the same sample. Sorry!'
      STOP
    ENDIF

    NBRE=0  
    rang=0

    IF ( nbDISKx /= 0) THEN

      IF(.NOT.strate)THEN
      DO i=1,nbDISKx/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbDISKx)+1
        IAL1=MIN(IAL1,nbDISKx)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=DINT(DA*nbDISKx)+1
        IAL2=MIN(IAL2,nbDISKx)
        IAL2=MAX(1,IAL2)
        dik= DISKx(IAL1)
        DISKx(IAL1)=DISKx(IAL2)
        DISKx(IAL2)=Dik
      END DO
      ENDIF

      DO i=1,nbROW
        IF(MOD(i,2).EQ.0)THEN
          DO j=1,nbCOL-1
            NBRE=j+rang

            DISKx(NBRE)%cooref(1)= 2.*j*DISKx_radius_max
            DISKx(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*DISKx_radius_max
            DISKx(NBRE)%cooref(3)= 0.

            CALL write_mod_BODIES(NBRE,'RBDY2')
            CALL write_mod_PLAIN(NBRE,'DISKx')
            CALL write_mod_NO3xx(NBRE,'DISKx')
            CALL write_mod_BDARY(NBRE,'DISKx')
            IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
            IF(d_vlocy == 0) Vbegin = 0.
            CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
          ENDDO
          rang=nbCOL-1+rang
        ELSE
          DO j=1,nbCOL
            NBRE=j+rang

            DISKx(NBRE)%cooref(1)= (2.*j-1.)*DISKx_radius_max
            DISKx(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*DISKx_radius_max
            DISKx(NBRE)%cooref(3)= 0.

            CALL write_mod_BODIES(NBRE,'RBDY2')
            CALL write_mod_PLAIN(NBRE,'DISKx')
            CALL write_mod_NO3xx(NBRE,'DISKx')
            CALL write_mod_BDARY(NBRE,'DISKx')
            IF(d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
            IF(d_vlocy == 0) Vbegin = 0.
            CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
          ENDDO
          rang=nbCOL+rang
        ENDIF
      ENDDO
      box_size(1) = 2.*DISKx_radius_max*nbCOL
      box_size(2) = DISKx_radius_max*(1.+nbROW*sq3)
    ENDIF

    IF ( nbPOLYG /= 0) THEN

      IF(.NOT.strate)THEN
       DO i=1,nbPOLYG/2
          CALL RANDOM_NUMBER(DA)
          IAL1=INT(DA*nbPOLYG)+1
          IAL1=MIN(IAL1,nbPOLYG)
          IAL1=MAX(1,IAL1)
          CALL RANDOM_NUMBER(DA)
          IAL2=INT(DA*nbPOLYG)+1
          IAL2=MIN(IAL2,nbPOLYG)
          IAL2=MAX(1,IAL2)
          pik= POLYG(IAL1)
          POLYG(IAL1)=POLYG(IAL2)
          POLYG(IAL2)=pik
        END DO
        ENDIF

        DO i=1,nbROW
          IF(MOD(i,2).EQ.0)THEN
           DO j=1,nbCOL-1
             NBRE=j+rang

             POLYG(NBRE)%cooref(1)= 2.*j*POLYG_radius_max  !a voir
             POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
             POLYG(NBRE)%cooref(3)= 0.

             CALL write_mod_BODIES(NBRE,'RBDY2')
             CALL write_mod_PLAIN(NBRE,'POLYG')
             CALL write_mod_NO3xx(NBRE,'POLYG')
             CALL write_mod_BDARY(NBRE,'POLYG')
             IF(p_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
             IF(p_vlocy == 0) Vbegin = 0.
             CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
           ENDDO
           rang=nbCOL-1+rang
         ELSE
           DO j=1,nbCOL
             NBRE=j+rang

             POLYG(NBRE)%cooref(1)= (2.*j-1.)*POLYG_radius_max
             POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
             POLYG(NBRE)%cooref(3)= 0.

             CALL write_mod_BODIES(NBRE,'RBDY2')
             CALL write_mod_PLAIN(NBRE,'POLYG')
             CALL write_mod_NO3xx(NBRE,'POLYG')
             CALL write_mod_BDARY(NBRE,'POLYG')
             IF(p_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
             IF(p_vlocy == 0) Vbegin = 0.
             CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
           ENDDO
           rang=nbCOL+rang
         ENDIF
       ENDDO
       box_size(1) = 2.*POLYG_radius_max*nbCOL
       box_size(2) =    POLYG_radius_max*(1.+nbROW*sq3)
     ENDIF  

  END SUBROUTINE  DOWN_TRIANGLE_NETWORK
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE PYRAMIDAL_STACKING

    IMPLICIT NONE

    INTEGER                    :: i,j,rang,Num,IAL1,IAL2
    REAL(kind=8),DIMENSION(3)  :: Vbegin
    REAL(kind=8)               :: DA
    TYPE(M_DISK)               :: dik
    TYPE(M_POLYG)              :: pik

    IF (( nbDISKx /= 0).AND.( nbPOLYG /= 0)) THEN
       PRINT*,'WARNING ! DISKx and POLYG can not appear in the same sample. Sorry!'
       STOP
    ENDIF

    Num=nbCOL*(nbCOL+1)/2

    NBRE=0  
    rang=0

    IF ( nbDISKx /= 0) THEN

      IF(.NOT.strate)THEN
      DO i=1,nbDISKx/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbDISKx)+1
        IAL1=MIN(IAL1,nbDISKx)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=DINT(DA*nbDISKx)+1
        IAL2=MIN(IAL2,nbDISKx)
        IAL2=MAX(1,IAL2)
        dik= DISKx(IAL1)
        DISKx(IAL1)=DISKx(IAL2)
        DISKx(IAL2)=Dik
      END DO
      ENDIF

      DO i=1,nbCOL
        DO j=1,nbCOL-i+1
          NBRE=j+rang

          DISKx(NBRE)%cooref(1)= (2.*j-2.+i)*DISKx_radius_max
          DISKx(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*DISKx_radius_max
          DISKx(NBRE)%cooref(3)= 0.
          CALL write_mod_BODIES(NBRE,'RBDY2')
          CALL write_mod_PLAIN(NBRE,'DISKx')
          CALL write_mod_NO3xx(NBRE,'DISKx')
          CALL write_mod_BDARY(NBRE,'DISKx')
          IF (d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
          IF (d_vlocy == 0) Vbegin = 0.
          CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
        ENDDO
        rang=nbCOL-i+1+rang
      ENDDO

      box_size(1) = 2.*DISKx_radius_max*nbCOL
      box_size(2) =    DISKx_radius_max*(1.+nbCOL*sq3)
    ENDIF

    IF ( nbPOLYG /= 0) THEN

      IF(.NOT.strate)THEN
      DO i=1,nbPOLYG/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbPOLYG)+1
        IAL1=MIN(IAL1,nbPOLYG)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=INT(DA*nbPOLYG)+1
        IAL2=MIN(IAL2,nbPOLYG)
        IAL2=MAX(1,IAL2)
        pik= POLYG(IAL1)
        POLYG(IAL1)=POLYG(IAL2)
        POLYG(IAL2)=pik
      END DO
      ENDIF

      DO i=1,nbCOL
        DO j=1,nbCOL-i+1
          NBRE=j+rang

          POLYG(NBRE)%cooref(1)= (2.*j-2.+i)*POLYG_radius_max
          POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
          POLYG(NBRE)%cooref(3)= 0.
          CALL write_mod_BODIES(NBRE,'RBDY2')
          CALL write_mod_PLAIN(NBRE,'POLYG')
          CALL write_mod_NO3xx(NBRE,'POLYG')
          CALL write_mod_BDARY(NBRE,'POLYG')
          IF (d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
          IF (d_vlocy == 0) Vbegin = 0.
          CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
        ENDDO
        rang=nbCOL-i+1.+rang
      ENDDO

      box_size(1) = 2.*POLYG_radius_max*nbCOL
      box_size(2) =    POLYG_radius_max*(1.+nbCOL*sq3)
    ENDIF

 END SUBROUTINE PYRAMIDAL_STACKING
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE SOURCE_POINT

    IMPLICIT NONE

    INTEGER                    :: i,j,IAL1,IAL2
    REAL(kind=8),DIMENSION(3)  :: Vbegin
    REAL(kind=8)               :: DA
    TYPE(M_DISK)               :: dik
    TYPE(M_POLYG)              :: pik

    IF (( nbDISKx /= 0).AND.( nbPOLYG /= 0)) THEN
       PRINT*,'WARNING ! DISKx and POLYG can not appear in the same sample. Sorry!'
       STOP
    ENDIF

    IF ( nbDISKx /= 0) THEN


      !fd magouille pour remelanger

      IF(.NOT.strate)THEN
      DO i=1,nbDISKx/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbDISKx)+1
        IAL1=MIN(IAL1,nbDISKx)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=DINT(DA*nbDISKx)+1
        IAL2=MIN(IAL2,nbDISKx)
        IAL2=MAX(1,IAL2)
        dik= DISKx(IAL1)
        DISKx(IAL1)=DISKx(IAL2)
        DISKx(IAL2)=Dik
      END DO
      ENDIF

      DO i=1,nbDISKx
          DISKx(i)%cooref(1)= coor_src_x
          DISKx(i)%cooref(2)= coor_src_y
          DISKx(i)%cooref(3)= 0.
          CALL write_mod_BODIES(i,'RBDY2')
          CALL write_mod_PLAIN(i,'DISKx')
          CALL write_mod_NO3xx(i,'DISKx')
          CALL write_mod_BDARY(i,'DISKx')
          IF (d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,DISKx_radius_max)
          IF (d_vlocy == 0) Vbegin = 0.
          CALL write_mod_dof_ini(i,Vbegin,'RBDY2')
      ENDDO

      NBRE=NBRE+nbDISKx

    ENDIF

    IF ( nbPOLYG /= 0) THEN

      IF(.NOT.strate)THEN
      DO i=1,nbPOLYG/2
        CALL RANDOM_NUMBER(DA)
        IAL1=INT(DA*nbPOLYG)+1
        IAL1=MIN(IAL1,nbPOLYG)
        IAL1=MAX(1,IAL1)
        CALL RANDOM_NUMBER(DA)
        IAL2=INT(DA*nbPOLYG)+1
        IAL2=MIN(IAL2,nbPOLYG)
        IAL2=MAX(1,IAL2)
        pik= POLYG(IAL1)
        POLYG(IAL1)=POLYG(IAL2)
        POLYG(IAL2)=pik
      END DO
      ENDIF

      DO i=1,nbPOLYG

          POLYG(i)%cooref(1)=  coor_src_x
          POLYG(i)%cooref(2)=  coor_src_y
          POLYG(i)%cooref(3)= 0.
          CALL write_mod_BODIES(i,'RBDY2')
          CALL write_mod_PLAIN(i,'POLYG')
          CALL write_mod_NO3xx(i,'POLYG')
          CALL write_mod_BDARY(i,'POLYG')
          IF (d_vlocy == 1) CALL vitesse_aleatoire(Vbegin,POLYG_radius_max)
          IF (d_vlocy == 0) Vbegin = 0.
          CALL write_mod_dof_ini(i,Vbegin,'RBDY2')
      ENDDO

      NBRE=NBRE+nbPOLYG

    ENDIF


 END SUBROUTINE SOURCE_POINT
!-----------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 SUBROUTINE mod_railway

   IMPLICIT NONE

   INTEGER                    :: i,j,rang,IAL1,IAL2,k
   REAL(kind=8),DIMENSION(3)  :: Vbegin=0.D0
   REAL(kind=8)               :: DA,Xbox,Ybox
   TYPE(M_POLYG)              :: pik

   NBRE=0  
   rang=0
   POLYG_radius_max=max_rad

   IF(.NOT.strate)THEN
   DO i=1,nbPOLYG/2
     CALL RANDOM_NUMBER(DA)
     IAL1=INT(DA*nbPOLYG)+1
     IAL1=MIN(IAL1,nbPOLYG)
     IAL1=MAX(1,IAL1)
     CALL RANDOM_NUMBER(DA)
     IAL2=INT(DA*nbPOLYG)+1
     IAL2=MIN(IAL2,nbPOLYG)
     IAL2=MAX(1,IAL2)
     pik= POLYG(IAL1)
     POLYG(IAL1)=POLYG(IAL2)
     POLYG(IAL2)=pik
   END DO
   ENDIF

   DO i=1,nbROW
     IF(MOD(i,2).EQ.0)THEN
       DO j=1,nbCOL
         NBRE=j+rang

         POLYG(NBRE)%cooref(1)= (2.*j-1.)*POLYG_radius_max
         POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
         POLYG(NBRE)%cooref(3)= 0.

         CALL write_mod_BODIES(NBRE,'RBDY2')
         CALL write_mod_PLAIN(NBRE,'POLYG')
         CALL write_mod_NO3xx(NBRE,'POLYG')
         CALL write_mod_BDARY(NBRE,'POLYG')
         CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
       ENDDO
       rang=nbCOL+rang
     ELSE
       DO j=1,nbCOL-1
         NBRE=j+rang
             
         POLYG(NBRE)%cooref(1)= 2.*j*POLYG_radius_max
         POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max
         POLYG(NBRE)%cooref(3)= 0.
    
         CALL write_mod_BODIES(NBRE,'RBDY2')
         CALL write_mod_PLAIN(NBRE,'POLYG')
         CALL write_mod_NO3xx(NBRE,'POLYG')
         CALL write_mod_BDARY(NBRE,'POLYG')
         CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
       ENDDO
       rang=nbCOL-1+rang
     ENDIF
   ENDDO

   DO k=1,nb_inter

     Xbox = 2*blw+(k-1)*(int_len+blw)
     Ybox = 2*POLYG_radius_max*(nbROW-1)
     
     DO i=1,nbROW2
       IF(MOD(i,2).EQ.0)THEN
         DO j=1,nbCOL2
           NBRE=j+rang

           POLYG(NBRE)%cooref(1)= (2.*j-1.)*POLYG_radius_max+Xbox
           POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max+Ybox
           POLYG(NBRE)%cooref(3)= 0.

           CALL write_mod_BODIES(NBRE,'RBDY2')
           CALL write_mod_PLAIN(NBRE,'POLYG')
           CALL write_mod_NO3xx(NBRE,'POLYG')
           CALL write_mod_BDARY(NBRE,'POLYG')
           CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
         ENDDO
         rang=nbCOL2+rang
       ELSE
         DO j=1,nbCOL2-1
           NBRE=j+rang
             
           POLYG(NBRE)%cooref(1)= 2.*j*POLYG_radius_max+Xbox
           POLYG(NBRE)%cooref(2)= ((i-1.)*sq3+1.)*POLYG_radius_max+Ybox
           POLYG(NBRE)%cooref(3)= 0.
           CALL write_mod_BODIES(NBRE,'RBDY2')
           CALL write_mod_PLAIN(NBRE,'POLYG')
           CALL write_mod_NO3xx(NBRE,'POLYG')
           CALL write_mod_BDARY(NBRE,'POLYG')
           CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
         ENDDO
         rang=nbCOL2-1+rang
       ENDIF
     ENDDO  
   ENDDO

   CALL write_begin_dof

   BLOCHET(1)%vrtxnb=4       
   ALLOCATE(BLOCHET(1)%vertex(2,4))
   BLOCHET(1)%vertex(1,1)= blw
   BLOCHET(1)%vertex(2,1)=-blh*0.5
   BLOCHET(1)%vertex(1,2)= blw*0.5
   BLOCHET(1)%vertex(2,2)= blh*0.5
   BLOCHET(1)%vertex(1,3)=-blw*0.5
   BLOCHET(1)%vertex(2,3)= blh*0.5
   BLOCHET(1)%vertex(1,4)=-blw
   BLOCHET(1)%vertex(2,4)=-blh*0.5
   BLOCHET(1)%bndry='VERTx'
   BLOCHET(1)%behav=blbehav
   BLOCHET(1)%av_radius=1.
!fd   BLOCHET(1)%gr_radius=1.  
   
   DO i=1,nb_inter+1

     NBRE=NBRE+1
     BLOCHET(1)%cooref(1) = blw+(i-1)*(int_len+blw)
     BLOCHET(1)%cooref(2) = 2*POLYG_radius_max*(nbROW-1)+blh*0.5
     BLOCHET(1)%cooref(3) = 0.D0
     CALL write_mod_BODIES(NBRE,'RBDY2')
     CALL write_mod_PLAIN(4,'BLOCH')
     CALL write_mod_NO3xx(4,'BLOCH')
     CALL write_mod_BDARY(4,'BLOCH')
     CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
     CALL write_mod_dof(NBRE,'RBDY2',1,0,1)

   ENDDO

   RUGOS='PENTA'
   box_size(1)= nbCOL*2.*max_rad
   box_size(2)= sdep

   JONCx(1)%av_radius = 1.0D0
!fd   JONCx(1)%gr_radius = 0.5D0
   JONCx(1)%behav     = rlbehav
   JONCx(1)%bndry     = 'UPxxx'
   JONCx(1)%ax1       = box_size(1)*0.5
   JONCx(1)%cooref(1) = box_size(1)*0.5
   JONCx(1)%ax2       = max_rad
   JONCx(1)%cooref(2) = 2*POLYG_radius_max*(nbROW-1)+blh+max_rad
   JONCx(1)%cooref(3) = 0.0D+00

   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(1,'JONCx')
   CALL write_mod_NO3xx(1,'JONCx')
   CALL write_mod_BDARY(1,'JONCx','UPxxx')
   CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
   CALL write_mod_dof(NBRE,'RBDY2',1,0,1)

   CALL mod_rugosite(2)

  END SUBROUTINE  mod_railway
!----------------------------------------------------------------------------------------
  SUBROUTINE mod_wall(is_symmetriclayer,is_rigid)

    IMPLICIT NONE

    logical                    :: is_symmetriclayer,is_rigid,is_lastrow
    INTEGER                    :: i,j,k,rang
    REAL(kind=8),DIMENSION(3)  :: Vbegin=0.D0

    IF(ALLOCATED(POLYG_BOX)) DEALLOCATE(POLYG_BOX)
    ALLOCATE(POLYG_BOX(2))

    NBRE=0  
    rang=0
    MZZZ='REGULIER'

    POLYG_BOX(1)%vrtxnb=4       
    ALLOCATE(POLYG_BOX(1)%vertex(2,4))
    POLYG_BOX(1)%vertex(1,1)= brkw*0.5
    POLYG_BOX(1)%vertex(2,1)=-brkh*0.5
    POLYG_BOX(1)%vertex(1,2)= brkw*0.5
    POLYG_BOX(1)%vertex(2,2)= brkh*0.5
    POLYG_BOX(1)%vertex(1,3)=-brkw*0.5
    POLYG_BOX(1)%vertex(2,3)= brkh*0.5
    POLYG_BOX(1)%vertex(1,4)=-brkw*0.5
    POLYG_BOX(1)%vertex(2,4)=-brkh*0.5
    POLYG_BOX(1)%bndry='REDxx'
    POLYG_BOX(1)%behav=brk_behav
    POLYG_BOX(1)%av_radius=1.0D+00
!fd    POLYG_BOX(1)%gr_radius=1.0D+00  

    POLYG_BOX(2)%vrtxnb=4       
    ALLOCATE(POLYG_BOX(2)%vertex(2,4))
    POLYG_BOX(2)%vertex(1,1)= brkw*0.25
    POLYG_BOX(2)%vertex(2,1)=-brkh*0.5
    POLYG_BOX(2)%vertex(1,2)= brkw*0.25
    POLYG_BOX(2)%vertex(2,2)= brkh*0.5
    POLYG_BOX(2)%vertex(1,3)=-brkw*0.25
    POLYG_BOX(2)%vertex(2,3)= brkh*0.5
    POLYG_BOX(2)%vertex(1,4)=-brkw*0.25
    POLYG_BOX(2)%vertex(2,4)=-brkh*0.5
    POLYG_BOX(2)%bndry='REDxx'
    POLYG_BOX(2)%behav=brk_behav
    POLYG_BOX(2)%av_radius=1.0D+00
!fd    POLYG_BOX(2)%gr_radius=1.0D+00


    is_lastrow=.false.
    if (is_symmetriclayer) then
       DO i=1,nbROW
          if (i == nbROW) is_lastrow=.TRUE.
          IF(MOD(i,2).NE.0)THEN
             DO j=1,nbCOL
                NBRE=j+rang

                POLYG_BOX(1)%cooref(1)= (j-1)*brkw+0.5*brkw
                POLYG_BOX(1)%cooref(2)= (i-1)*brkh+0.5*brkh
                POLYG_BOX(1)%cooref(3)= 0.0D+00
                if (is_rigid) then
                   CALL write_mod_BODIES(NBRE,'RBDY2')
                   CALL write_mod_PLAIN(1,'POLYB')
                   CALL write_mod_NO3xx(1,'POLYB')
                   CALL write_mod_BDARY(1,'POLYB')
                   CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
                else
                   CALL write_brick_mailx(NBRE,1,is_lastrow)
                endif
             ENDDO
             rang=nbCOL+rang
          ELSE
             rang=rang+1
             POLYG_BOX(2)%cooref(1)= 0.25*brkw
             POLYG_BOX(2)%cooref(2)= (i-1)*brkh+0.5*brkh
             POLYG_BOX(2)%cooref(3)= 0.0D+00
             if (is_rigid) then         
                CALL write_mod_BODIES(NBRE+1,'RBDY2')
                CALL write_mod_PLAIN(2,'POLYB')
                CALL write_mod_NO3xx(2,'POLYB')
                CALL write_mod_BDARY(2,'POLYB')
                CALL write_mod_dof_ini(NBRE+1,Vbegin,'RBDY2')
             else
                CALL write_brick_mailx(NBRE,2,is_lastrow)
             endif

             DO j=1,nbCOL-1
                NBRE=j+rang

                POLYG_BOX(1)%cooref(1)= j*brkw
                POLYG_BOX(1)%cooref(2)= (i-1)*brkh+0.5*brkh
                POLYG_BOX(1)%cooref(3)= 0.0D+00
                if (is_rigid) then         
                   CALL write_mod_BODIES(NBRE,'RBDY2')
                   CALL write_mod_PLAIN(1,'POLYB')
                   CALL write_mod_NO3xx(1,'POLYB')
                   CALL write_mod_BDARY(1,'POLYB')
                   CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2') 
                else
                   CALL write_brick_mailx(NBRE,1,is_lastrow)
                endif
             ENDDO
             rang=nbCOL-1+rang
             rang=rang+1
             POLYG_BOX(2)%cooref(1)= 0.75*brkw+(nbCOL-1)*brkw
             POLYG_BOX(2)%cooref(2)= (i-1)*brkh+0.5*brkh
             POLYG_BOX(2)%cooref(3)= 0.0D+00

             if (is_rigid) then         
                CALL write_mod_BODIES(NBRE+1,'RBDY2')
                CALL write_mod_PLAIN(2,'POLYB')
                CALL write_mod_NO3xx(2,'POLYB')
                CALL write_mod_BDARY(2,'POLYB')
                CALL write_mod_dof_ini(NBRE+1,Vbegin,'RBDY2')
             else
                CALL write_brick_mailx(NBRE,2,is_lastrow)
             endif



          ENDIF
       ENDDO

    else

       DO i=1,nbROW
          if (i == nbROW) is_lastrow=.TRUE.
          IF(MOD(i,2).NE.0)THEN
             DO j=1,nbCOL-1
                NBRE=j+rang

                POLYG_BOX(1)%cooref(1)= (j-1)*brkw+0.5*brkw
                POLYG_BOX(1)%cooref(2)= (i-1)*brkh+0.5*brkh
                POLYG_BOX(1)%cooref(3)= 0.0D+00
                if (is_rigid) then         
                   CALL write_mod_BODIES(NBRE,'RBDY2')
                   CALL write_mod_PLAIN(1,'POLYB')
                   CALL write_mod_NO3xx(1,'POLYB')
                   CALL write_mod_BDARY(1,'POLYB')
                   CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
                else
                   CALL write_brick_mailx(NBRE,1,is_lastrow)
                endif
             ENDDO
             NBRE=NBRE+1
             POLYG_BOX(2)%cooref(1)= (nbCOL-1)*brkw + 0.25*brkw
             POLYG_BOX(2)%cooref(2)= (i-1)*brkh+0.5*brkh
             POLYG_BOX(2)%cooref(3)= 0.0D+00
             if (is_rigid) then         
                CALL write_mod_BODIES(NBRE,'RBDY2')
                CALL write_mod_PLAIN(2,'POLYB')
                CALL write_mod_NO3xx(2,'POLYB')  
                CALL write_mod_BDARY(2,'POLYB')
                CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
             else
                CALL write_brick_mailx(NBRE,2,is_lastrow)
             endif


          ELSE
             NBRE=rang+1         
             POLYG_BOX(2)%cooref(1)= 0.25*brkw
             POLYG_BOX(2)%cooref(2)= (i-1)*brkh+0.5*brkh
             POLYG_BOX(2)%cooref(3)= 0.0D+00
             if (is_rigid) then         
                CALL write_mod_BODIES(NBRE,'RBDY2')
                CALL write_mod_PLAIN(2,'POLYB')
                CALL write_mod_NO3xx(2,'POLYB')
                CALL write_mod_BDARY(2,'POLYB')
                CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
             else
                CALL write_brick_mailx(NBRE,2,is_lastrow)
             endif


             DO j=1,nbCOL-1
                NBRE=1+j+rang

                POLYG_BOX(1)%cooref(1)= brkw + (j-1)*brkw
                POLYG_BOX(1)%cooref(2)= (i-1)*brkh+0.5*brkh
                POLYG_BOX(1)%cooref(3)= 0.0D+00
                if (is_rigid) then             
                   CALL write_mod_BODIES(NBRE,'RBDY2')
                   CALL write_mod_PLAIN(1,'POLYB')
                   CALL write_mod_NO3xx(1,'POLYB')
                   CALL write_mod_BDARY(1,'POLYB')
                   CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')  
                else
                   CALL write_brick_mailx(NBRE,1,is_lastrow)
                endif

             ENDDO

          ENDIF
      rang=nbCOL+rang


    ENDDO
    endif


    box_size(1) = nbCOL*brkw
    box_size(2) = nbROW*brkh

    JONCx(1)%ax1       = box_size(1)
    JONCx(1)%cooref(1) = box_size(1)*0.5
    JONCx(1)%ax2       = brkh*0.5
    JONCx(1)%cooref(2) =-brkh*0.5
    JONCx(1)%cooref(3) = 0.0D+00
    JONCx(1)%av_radius = 1.0D0
!fd    JONCx(1)%gr_radius = 0.5D0
    JONCx(1)%behav     = floor_behav
    JONCx(1)%bndry     = 'DOWNx'

    NBRE=NBRE+1
    CALL write_begin_dof
    CALL write_mod_BODIES(NBRE,'RBDY2')
    CALL write_mod_PLAIN(1,'JONCx')
    CALL write_mod_NO3xx(1,'JONCx')
    CALL write_mod_BDARY(1,'JONCx','DOWNx')
    CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
    CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
    CALL write_end_dof

  END SUBROUTINE  mod_wall

!-----------------------------------------------------------------------------------------------------------------------
 SUBROUTINE mod_jonc(m)

   IMPLICIT NONE

   INTEGER,INTENT(in)        :: m
   INTEGER                   :: i,j,ip,nb_base,nb_wall
   REAL(kind=8)              :: e,raybox,pi=3.141592654_8,alpha0,radius_max
   REAL(kind=8),DIMENSION(3) :: Vfree
    
! fd le 26/05/04
! on gere le cas ou l'echantillon n'est pas cree avec nbCOL et nbROW 
! mais avec point source par exemple donc ax2 est calcule par rapport 
! a max radius
!
   radius_max=MAX(POLYG_radius_max,DISKx_radius_max)

   Vfree= 0
   IF (JONCx(1)%ax1 == 0 )THEN
     JONCx(1)%ax1       = box_size(1)
     JONCx(1)%cooref(1) = box_size(1)*0.5
   ELSE
     JONCx(1)%cooref(1) = JONCx(1)%ax1*0.5
   ENDIF

   IF ( JONCx(1)%ax2 == 0 )THEN
     JONCx(1)%ax2       = radius_max*0.5
     JONCx(1)%cooref(2) =-radius_max*0.5
   ELSE
     JONCx(1)%cooref(2) =-JONCx(1)%ax2*0.5
   ENDIF

   JONCx(1)%cooref(3) = 0.0D+00

   NBRE=NBRE+1
   CALL write_begin_dof
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(1,'JONCx')
   CALL write_mod_NO3xx(1,'JONCx')
   CALL write_mod_BDARY(1,'JONCx','DOWNx')
   Vfree(2)=JONCx(1)%Vbegin
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   IF( JONCx(1)%bip == 1 )THEN
     CALL write_mod_dof(NBRE,'RBDY2',1,0,1)
   ELSE
     CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
   ENDIF
       
   IF( m == 2 )GOTO 24
   IF( m == 4 )GOTO 21
       
   IF ( JONCx(2)%ax2 == 0 ) JONCx(2)%ax2 = radius_max*0.5
       
   IF ( JONCx(2)%ax1 == 0 )THEN
     JONCx(2)%ax1       = box_size(1)
     JONCx(2)%cooref(1) = box_size(1)*0.5
   ELSE
     JONCx(2)%cooref(1) = JONCx(2)%ax1*0.5
   ENDIF
       
   IF ( JONCx(3)%ax1 == 0 )THEN
     JONCx(2)%cooref(2) = box_size(2)+JONCx(2)%ax2
   ELSE
     JONCx(2)%cooref(2) = JONCx(3)%ax1+JONCx(2)%ax2
   ENDIF

   JONCx(2)%cooref(3) = PI_g

   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(2,'JONCx')
   CALL write_mod_NO3xx(2,'JONCx')
   CALL write_mod_BDARY(2,'JONCx','UPxxx')
   Vfree(2)=JONCx(2)%Vbegin
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   IF( JONCx(2)%bip == 1 )THEN
     CALL write_mod_dof(NBRE,'RBDY2',1,0,1)
   ELSE
     CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
   ENDIF
       
24 CONTINUE
  
   IF( m == 3 ) GOTO 21
       
   IF ( JONCx(3)%ax2 == 0 )THEN
     JONCx(3)%ax2      = radius_max*0.5
     JONCx(3)%cooref(1)=-radius_max*0.5
   ELSE
     JONCx(3)%cooref(1)=-JONCx(3)%ax2*0.5
   ENDIF

   IF ( JONCx(3)%ax1 == 0 )THEN
     JONCx(3)%ax1      = box_size(2)
     JONCx(3)%cooref(2)= box_size(2)*0.5
   ELSE
     JONCx(3)%cooref(2)= JONCx(3)%ax1
   ENDIF

   JONCx(3)%cooref(3)=-0.1570800D+01

   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(3,'JONCx')
   CALL write_mod_NO3xx(3,'JONCx')
   CALL write_mod_BDARY(3,'JONCx','LEFTx')
   Vfree(2) = 0.
   Vfree(1) = JONCx(3)%Vbegin
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   IF( JONCx(3)%bip == 1 )THEN
     CALL write_mod_dof(NBRE,'RBDY2',0,1,1)
   ELSE
     CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
   ENDIF
       
   IF ( JONCx(4)%ax2 == 0 ) JONCx(4)%ax2 =radius_max*0.5
   IF ( JONCx(4)%ax1 == 0 ) JONCx(4)%ax1 =box_size(2)
       
   IF ( JONCx(1)%ax1 == 0 )THEN
     JONCx(4)%cooref(1) = box_size(1)+JONCx(4)%ax2
   ELSE
     JONCx(4)%cooref(1) = JONCx(1)%ax1+JONCx(4)%ax2
   ENDIF
       
   IF ( JONCx(3)%ax1 == 0 )THEN
     JONCx(4)%cooref(2) = box_size(2)*0.5
   ELSE
     JONCx(4)%cooref(2) = JONCx(3)%ax1*0.5
   ENDIF

   JONCx(4)%cooref(3) = 0.1570800D+01
   
   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(4,'JONCx')
   CALL write_mod_NO3xx(4,'JONCx')
   CALL write_mod_BDARY(4,'JONCx','RIGHT')
   Vfree(1) = JONCx(4)%Vbegin
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   IF( JONCx(4)%bip == 1 )THEN
   CALL write_mod_dof(NBRE,'RBDY2',0,1,1)
   ELSE
     CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
   ENDIF
       
21 CONTINUE

   CALL write_end_dof
 
  END SUBROUTINE mod_jonc
!-----------------------------------------------------------------------------------------------------------------------
 SUBROUTINE mod_silo

   IMPLICIT NONE

   REAL(kind=8)              :: pi=3.141592654_8,radius_max
   REAL(kind=8),DIMENSION(3) :: Vfree
    
! fd le 26/05/04
! on gere le cas ou l'echantillon n'est pas cree avec nbCOL et nbROW 
! mais avec point source par exemple donc ax2 est calcule par rapport 
! a max radius
!

   radius_max=MAX(POLYG_radius_max,DISKx_radius_max)

   Vfree= 0
   JONCx(1)%ax1       = 0.5*box_size(1)/COS(ANGLE)
   JONCx(1)%ax2       = radius_max*0.5
   JONCx(1)%cooref(1) =-JONCx(1)%ax2*0.5
   JONCx(1)%cooref(2) =-JONCx(1)%ax2*0.5
   JONCx(1)%cooref(3) =-ANGLE

   NBRE=NBRE+1
   CALL write_begin_dof
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(1,'JONCx')
   CALL write_mod_NO3xx(1,'JONCx')
   CALL write_mod_BDARY(1,'JONCx','DOWNx')
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
       
   JONCx(1)%ax1       = 0.5*box_size(1)/COS(ANGLE)
   JONCx(1)%ax2       = radius_max*0.5
   JONCx(1)%cooref(1) = box_size(1)+JONCx(1)%ax2*0.5
   JONCx(1)%cooref(2) =-JONCx(1)%ax2*0.5
   JONCx(1)%cooref(3) = ANGLE

   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(1,'JONCx')
   CALL write_mod_NO3xx(1,'JONCx')
   CALL write_mod_BDARY(1,'JONCx','DOWNx')
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   CALL write_mod_dof(NBRE,'RBDY2',1,1,1)

   JONCx(3)%ax1      = box_size(2)*0.5
   JONCx(3)%ax2      = JONCx(1)%ax2
   JONCx(3)%cooref(1)=-JONCx(3)%ax2
   JONCx(3)%cooref(2)= box_size(2)*0.5
   JONCx(3)%cooref(3)=-pi*0.5

   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(3,'JONCx')
   CALL write_mod_NO3xx(3,'JONCx')
   CALL write_mod_BDARY(3,'JONCx','LEFTx')
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   CALL write_mod_dof(NBRE,'RBDY2',1,1,1)

   JONCx(4)%ax2 = JONCx(3)%ax2
   JONCx(4)%ax1 = box_size(2)*0.5
   JONCx(4)%cooref(1) = box_size(1)+JONCx(4)%ax2
   JONCx(4)%cooref(2) = box_size(2)*0.5
   JONCx(4)%cooref(3) = pi*0.5
       
   NBRE=NBRE+1
   CALL write_mod_BODIES(NBRE,'RBDY2')
   CALL write_mod_PLAIN(4,'JONCx')
   CALL write_mod_NO3xx(4,'JONCx')
   CALL write_mod_BDARY(4,'JONCx','RIGHT')
   CALL write_mod_dof_ini(NBRE,Vfree,'RBDY2')
   CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
       
   CALL write_end_dof
 
  END SUBROUTINE mod_silo
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE mod_rugosite(m)

    IMPLICIT NONE

    INTEGER,INTENT(in)        :: m
    INTEGER                   :: i,j,Num,ip,nb_base,nb_wall
    REAL(kind=8)              :: e,raybox,pi=3.141592654_8,alpha0
    REAL(kind=8),DIMENSION(3) :: Vfree
    REAL(kind=8)              :: radius_max

    IF(ALLOCATED(POLYG_BOX)) DEALLOCATE(POLYG_BOX)
    ALLOCATE(POLYG_BOX(4))

    radius_max=MAX(POLYG_radius_max,DISKx_radius_max)

    IF(.NOT.rail) CALL write_begin_dof

    nb_wall = 1+nbROW/2
    nb_base = 1+nbCOL/2

! fd le 26/05/04
! on gere le cas ou l'echantillon n'est pas cree avec nbCOL et nbROW 
! mais avec point source par exemple
!
    IF (nbROW==0) nb_wall=INT(box_size(2)/radius_max)
    IF (nbCOL==0) nb_base=INT(box_size(1)/radius_max)

    !                                       2 
    !                                      ---
    ! on construit les differents cotes 3 |   | 4
    !                                      ---
    !                                       1
    !

    POLYG_BOX(1)%Vbegin = 0.0D+00

    SELECT CASE(RUGOS)

    CASE('QUADR')
      POLYG_BOX(1)%vrtxnb=4       
      ALLOCATE(POLYG_BOX(1)%vertex(2,4))
      POLYG_BOX(1)%vertex(1,1)= 2.*radius_max
      POLYG_BOX(1)%vertex(2,1)=-radius_max*0.5
      POLYG_BOX(1)%vertex(1,2)= 2.*radius_max
      POLYG_BOX(1)%vertex(2,2)= radius_max*0.5
      POLYG_BOX(1)%vertex(1,3)=-2.*radius_max
      POLYG_BOX(1)%vertex(2,3)= radius_max*0.5
      POLYG_BOX(1)%vertex(1,4)=-2.*radius_max
      POLYG_BOX(1)%vertex(2,4)=-radius_max*0.5
      POLYG_BOX(1)%bndry='DOWNx'
      POLYG_BOX(1)%behav='HULKx'
      POLYG_BOX(1)%av_radius=0.
!fd      POLYG_BOX(1)%gr_radius=0.

    CASE('PENTA')
      POLYG_BOX(1)%vrtxnb=5       
      ALLOCATE(POLYG_BOX(1)%vertex(2,5))
      raybox=radius_max
      DO ip=1,5
        POLYG_BOX(1)%vertex(1,ip)= raybox*COS(0.4*pi*(ip-1.))
        POLYG_BOX(1)%vertex(2,ip)= raybox*SIN(0.4*pi*(ip-1.))
      ENDDO
      POLYG_BOX(1)%bndry='DOWNx'
      POLYG_BOX(1)%behav='HULKx'
      POLYG_BOX(1)%av_radius=0.
!fd      POLYG_BOX(1)%gr_radius=0.

    CASE('HEXAG')
      POLYG_BOX(1)%vrtxnb=6       
      ALLOCATE(POLYG_BOX(1)%vertex(2,6))
      raybox=radius_max
      DO ip=1,6
        POLYG_BOX(1)%vertex(1,ip)= raybox*COS(0.33333*pi*(ip-1.))
        POLYG_BOX(1)%vertex(2,ip)= raybox*SIN(0.33333*pi*(ip-1.))
      ENDDO
      POLYG_BOX(1)%bndry='DOWNx'
      POLYG_BOX(1)%behav='HULKx'
      POLYG_BOX(1)%av_radius=0.
!fd      POLYG_BOX(1)%gr_radius=0.
    CASE default
    END SELECT

    DO ip=1,2*nb_base
      NBRE=NBRE+1
      CALL RANDOM_NUMBER(alpha0)
      POLYG_BOX(1)%cooref(1) = 2.*radius_max*(ip-1.)
      POLYG_BOX(1)%cooref(2) =-radius_max
      POLYG_BOX(1)%cooref(3) = alpha0*pi
      IF(RUGOS == 'QUADR') POLYG_BOX(1)%cooref(3)= 0.0D+00
      
      CALL write_mod_BODIES(NBRE,'RBDY2')
      CALL write_mod_PLAIN(1,'POLYB')
      CALL write_mod_NO3xx(1,'POLYB')
      CALL write_mod_BDARY(1,'POLYB')
      POLYG_BOX(1)%Vbegin(2) = JONCx(1)%Vbegin
      CALL write_mod_dof_ini(NBRE,POLYG_BOX(1)%Vbegin,'RBDY2')
      IF( JONCx(1)%bip == 1 )THEN
        CALL write_mod_dof(NBRE,'RBDY2',1,0,1)
      ELSE
        CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
      ENDIF
    ENDDO

    IF( m == 2 )GOTO 34
    IF( m == 4 )GOTO 31

    POLYG_BOX(2)%Vbegin   = 0.
    SELECT CASE(RUGOS)
    CASE('QUADR')
      POLYG_BOX(2)%vrtxnb   = 4
      ALLOCATE(POLYG_BOX(2)%vertex(2,4))
      POLYG_BOX(2)%vertex(1,1) = 2.*radius_max
      POLYG_BOX(2)%vertex(2,1) =-radius_max*0.5
      POLYG_BOX(2)%vertex(1,2) = 2.*radius_max
      POLYG_BOX(2)%vertex(2,2) = radius_max*0.5
      POLYG_BOX(2)%vertex(1,3) =-2.*radius_max
      POLYG_BOX(2)%vertex(2,3) = radius_max*0.5
      POLYG_BOX(2)%vertex(1,4) =-2.*radius_max
      POLYG_BOX(2)%vertex(2,4) =-radius_max*0.5
      POLYG_BOX(2)%bndry='UPxxx'
      POLYG_BOX(2)%behav='HULKx'
      POLYG_BOX(2)%av_radius = 0.
!fd      POLYG_BOX(2)%gr_radius = 0.
    CASE('PENTA')
      POLYG_BOX(2)%vrtxnb=5       
      ALLOCATE(POLYG_BOX(2)%vertex(2,5))
      raybox=radius_max
      DO ip=1,5
        POLYG_BOX(2)%vertex(1,ip)= raybox*COS(0.4*pi*(ip-1.))
        POLYG_BOX(2)%vertex(2,ip)= raybox*SIN(0.4*pi*(ip-1.))
      ENDDO
      POLYG_BOX(2)%bndry='UPxxx'
      POLYG_BOX(2)%behav='HULKx'
      POLYG_BOX(2)%av_radius=0.
!fd      POLYG_BOX(2)%gr_radius=0.
    CASE('HEXAG')
      POLYG_BOX(2)%vrtxnb=6       
      ALLOCATE(POLYG_BOX(2)%vertex(2,6))
      raybox=radius_max
      DO ip=1,6
        POLYG_BOX(2)%vertex(1,ip)= raybox*COS(0.33333*pi*(ip-1.))
        POLYG_BOX(2)%vertex(2,ip)= raybox*SIN(0.33333*pi*(ip-1.))
      ENDDO
      POLYG_BOX(2)%bndry='UPxxx'
      POLYG_BOX(2)%behav='HULKx'
      POLYG_BOX(2)%av_radius=0.
!fd      POLYG_BOX(2)%gr_radius=0.
    CASE default
    END SELECT

    DO ip=1,2*nb_base

      NBRE=NBRE+1
      CALL RANDOM_NUMBER(alpha0)
      POLYG_BOX(2)%cooref(1) = 2.*radius_max*(ip-1.)
      POLYG_BOX(2)%cooref(2) =    radius_max+box_size(2)
      POLYG_BOX(2)%cooref(3) =    alpha0*pi
      IF(RUGOS == 'QUADR') POLYG_BOX(2)%cooref(3)= 0.0D+00

      CALL write_mod_BODIES(NBRE,'RBDY2')
      CALL write_mod_PLAIN(2,'POLYB')
      CALL write_mod_NO3xx(2,'POLYB')
      CALL write_mod_BDARY(2,'POLYB')
          
      POLYG_BOX(2)%Vbegin(2) = JONCx(2)%Vbegin
          
      CALL write_mod_dof_ini(NBRE,POLYG_BOX(2)%Vbegin,'RBDY2')
      IF( JONCx(2)%bip == 1 )THEN
        CALL write_mod_dof(NBRE,'RBDY2',1,0,1)
      ELSE
        CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
      ENDIF
    ENDDO

34  CONTINUE
  
    IF(m.EQ.3)GOTO 31

    POLYG_BOX(3)%Vbegin = 0.

    SELECT CASE(RUGOS)
    CASE('QUADR')
      POLYG_BOX(3)%vrtxnb = 4
      ALLOCATE(POLYG_BOX(3)%vertex(2,4))
      POLYG_BOX(3)%vertex(1,1) = radius_max*0.5
      POLYG_BOX(3)%vertex(2,1) =-2.*radius_max
      POLYG_BOX(3)%vertex(1,2) = radius_max*0.5
      POLYG_BOX(3)%vertex(2,2) = 2.*radius_max
      POLYG_BOX(3)%vertex(1,3) =-radius_max*0.5
      POLYG_BOX(3)%vertex(2,3) = 2.*radius_max
      POLYG_BOX(3)%vertex(1,4) =-radius_max*0.5
      POLYG_BOX(3)%vertex(2,4) =-2.*radius_max
      POLYG_BOX(3)%bndry='LEFTx'
      POLYG_BOX(3)%behav='HULKx'
      POLYG_BOX(3)%av_radius=0.
!fd      POLYG_BOX(3)%gr_radius=0.
    CASE('PENTA')
      POLYG_BOX(3)%vrtxnb=5       
      ALLOCATE(POLYG_BOX(3)%vertex(2,5))
      raybox=radius_max
      DO ip=1,5
        POLYG_BOX(3)%vertex(1,ip)= raybox*COS(0.4*pi*(ip-1.))
        POLYG_BOX(3)%vertex(2,ip)= raybox*SIN(0.4*pi*(ip-1.))
      ENDDO
      POLYG_BOX(3)%bndry='LEFTx'
      POLYG_BOX(3)%behav='HULKx'
      POLYG_BOX(3)%av_radius=0.
!fd      POLYG_BOX(3)%gr_radius=0.
    CASE('HEXAG')
      POLYG_BOX(3)%vrtxnb = 6       
      ALLOCATE(POLYG_BOX(3)%vertex(2,6))
      raybox=radius_max
      DO ip=1,6
        POLYG_BOX(3)%vertex(1,ip)= raybox*COS(0.33333*pi*(ip-1.))
        POLYG_BOX(3)%vertex(2,ip)= raybox*SIN(0.33333*pi*(ip-1.))
      ENDDO
      POLYG_BOX(3)%bndry='LEFTx'
      POLYG_BOX(3)%behav='HULKx'
      POLYG_BOX(3)%av_radius = 0.
!fd      POLYG_BOX(3)%gr_radius = 0.
    CASE default
    END SELECT

    DO ip=1,2*nb_wall
          
      NBRE=NBRE+1
      CALL RANDOM_NUMBER(alpha0)
      POLYG_BOX(3)%cooref(1) =-radius_max
      POLYG_BOX(3)%cooref(2) = 2.*radius_max*(ip-1.)
      POLYG_BOX(3)%cooref(3) = alpha0*pi
      IF(RUGOS == 'QUADR') POLYG_BOX(3)%cooref(3)= 0.0D+00

      CALL write_mod_BODIES(NBRE,'RBDY2')
      CALL write_mod_PLAIN(3,'POLYB')
      CALL write_mod_NO3xx(3,'POLYB')
      CALL write_mod_BDARY(3,'POLYB')

      POLYG_BOX(3)%Vbegin(1)=JONCx(3)%Vbegin

      CALL write_mod_dof_ini(NBRE,POLYG_BOX(3)%Vbegin,'RBDY2')

      IF( JONCx(3)%bip == 1 )THEN
        CALL write_mod_dof(NBRE,'RBDY2',0,1,1)
      ELSE
        CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
      ENDIF
    ENDDO

    POLYG_BOX(4)%Vbegin=0

    SELECT CASE(RUGOS)
    CASE('QUADR')
      POLYG_BOX(4)%vrtxnb=4
      ALLOCATE(POLYG_BOX(4)%vertex(2,4))
      POLYG_BOX(4)%vertex(1,1)= radius_max*0.5
      POLYG_BOX(4)%vertex(2,1)=-2.*radius_max
      POLYG_BOX(4)%vertex(1,2)= radius_max*0.5
      POLYG_BOX(4)%vertex(2,2)= 2.*radius_max
      POLYG_BOX(4)%vertex(1,3)=-radius_max*0.5
      POLYG_BOX(4)%vertex(2,3)= 2.*radius_max
      POLYG_BOX(4)%vertex(1,4)=-radius_max*0.5
      POLYG_BOX(4)%vertex(2,4)=-2.*radius_max
      POLYG_BOX(4)%bndry='RIGHT'
      POLYG_BOX(4)%behav='HULKx' 
      POLYG_BOX(4)%av_radius=0.
!fd      POLYG_BOX(4)%gr_radius=0.
    CASE('PENTA')
      POLYG_BOX(4)%vrtxnb = 5       
      ALLOCATE(POLYG_BOX(4)%vertex(2,5))
      raybox = radius_max
      DO ip=1,5
        POLYG_BOX(4)%vertex(1,ip)= raybox*COS(0.4*pi*(ip-1.))
        POLYG_BOX(4)%vertex(2,ip)= raybox*SIN(0.4*pi*(ip-1.))
      ENDDO
      POLYG_BOX(4)%bndry='RIGHT'
      POLYG_BOX(4)%behav='HULKx'
      POLYG_BOX(4)%av_radius=0.
!fd      POLYG_BOX(4)%gr_radius=0.
    CASE('HEXAG')
      POLYG_BOX(4)%vrtxnb=6       
      ALLOCATE(POLYG_BOX(4)%vertex(2,6))
      raybox=radius_max
      DO ip=1,6
        POLYG_BOX(4)%vertex(1,ip)= raybox*COS(0.33333*pi*(ip-1.))
        POLYG_BOX(4)%vertex(2,ip)= raybox*SIN(0.33333*pi*(ip-1.))
      ENDDO
      POLYG_BOX(4)%bndry='RIGHT'
      POLYG_BOX(4)%behav='HULKx'
      POLYG_BOX(4)%av_radius=0.
!fd      POLYG_BOX(4)%gr_radius=0.
    CASE default
    END SELECT

    DO ip=1,2*nb_wall

       NBRE=NBRE+1
       CALL RANDOM_NUMBER(alpha0)
       POLYG_BOX(4)%cooref(1) =   radius_max+box_size(1)
       POLYG_BOX(4)%cooref(2) = 2*radius_max*(ip-1.)
       POLYG_BOX(4)%cooref(3) = alpha0*pi
       IF(RUGOS == 'QUADR') POLYG_BOX(4)%cooref(3)= 0.0D+00

       CALL write_mod_BODIES(NBRE,'RBDY2')
       CALL write_mod_PLAIN(4,'POLYB')
       CALL write_mod_NO3xx(4,'POLYB')
       CALL write_mod_BDARY(4,'POLYB')

       POLYG_BOX(4)%Vbegin(1)=JONCx(4)%Vbegin
       
       CALL write_mod_dof_ini(NBRE,POLYG_BOX(4)%Vbegin,'RBDY2')
          
       IF( JONCx(4)%bip == 1 )THEN
         CALL write_mod_dof(NBRE,'RBDY2',0,1,1)
       ELSE
         CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
       ENDIF
     ENDDO

31   CONTINUE

     CALL write_end_dof
    
  END SUBROUTINE mod_rugosite
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE special_box

    IMPLICIT NONE

    INTEGER                   :: ip,nb_wall,nb_base
    REAL(kind=8),DIMENSION(3) :: Vbegin=0.D0
    REAL(kind=8)              :: radius_max

    IF(ALLOCATED(MAILLON)) DEALLOCATE(MAILLON)
    ALLOCATE(MAILLON(1))

    CALL write_begin_dof

    nb_wall=1+nbROW/2
    nb_base=1+nbCOL/2

! fd le 26/05/04
! on gere le cas ou l'echantillon n'est pas cree avec nbCOL et nbROW 
! mais avec point source par exemple
!
    radius_max=MAX(POLYG_radius_max,DISKx_radius_max)
    IF (nbROW==0) nb_wall=INT(box_size(2)/radius_max)
    IF (nbCOL==0) nb_base=INT(box_size(1)/radius_max)



    MAILLON(1)%Vbegin = 0.0D+00
    MAILLON(1)%bndry  ='VERTx'
    MAILLON(1)%behav  ='PLEXx'
    MAILLON(1)%av_radius=DISKx_radius_max
!fd    MAILLON(1)%gr_radius=DISKx_radius_max*sq2_1

    DO ip=1,4*nb_base
       
      NBRE=NBRE+1
      MAILLON(1)%cooref(1)= DISKx_radius_max*(ip-1.)
      MAILLON(1)%cooref(2)=-DISKx_radius_max
      MAILLON(1)%cooref(3)= 0.0D+00

      CALL write_mod_BODIES(NBRE,'RBDY2')
      CALL write_mod_PLAIN(1,'MAILL')
      CALL write_mod_NO3xx(1,'MAILL')
      CALL write_mod_BDARY(1,'MAILL')
      CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
      CALL write_mod_dof(NBRE,'RBDY2',1,1,1)          
    ENDDO

    DO ip=1,4*nb_wall
          
      NBRE=NBRE+1
      MAILLON(1)%cooref(1)=-DISKx_radius_max
      MAILLON(1)%cooref(2)= DISKx_radius_max*(ip-1.)
      MAILLON(1)%cooref(3)= 0.0D+00

      CALL write_mod_BODIES(NBRE,'RBDY2')
      CALL write_mod_PLAIN(1,'MAILL')
      CALL write_mod_NO3xx(1,'MAILL')
      CALL write_mod_BDARY(1,'MAILL')
      CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
      CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
    ENDDO

    DO ip=1,4*nb_wall

      NBRE=NBRE+1
      MAILLON(1)%cooref(1)= DISKx_radius_max+box_size(1)
      MAILLON(1)%cooref(2)= DISKx_radius_max*(ip-1.)
      MAILLON(1)%cooref(3)= 0.0D+00

      CALL write_mod_BODIES(NBRE,'RBDY2')
      CALL write_mod_PLAIN(1,'MAILL')
      CALL write_mod_NO3xx(1,'MAILL')
      CALL write_mod_BDARY(1,'MAILL')
      CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
      CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
    ENDDO

    CALL write_end_dof   
  
 END SUBROUTINE special_box
!-----------------------------------------------------------------------------------------------------------------------
 SUBROUTINE CHAINETTE_DISKx(coef)

   IMPLICIT NONE

   INTEGER,INTENT(in)        :: coef
   INTEGER                   :: NUMbegin,nb_maillon,i,nb_base,nb_wall
   REAL(kind=8)              :: radius,bup,bright,longueur,base,wall
   REAL(kind=8),DIMENSION(3) :: Vbegin = 0.0D+00

   CALL write_begin_dof

   bup    = box_size(2)
   bright = box_size(1)
   radius = DISKx_radius_max*0.5
   IF (coef == 3) radius=grd_rd
   NUMbegin = NBRE
   longueur=(2.*bup+bright)
   IF (coef == 3) longueur=bright+radius
   nb_maillon = INT(longueur/(2.*radius))+1

   ALLOCATE(MAILLON(nb_maillon))

   IF( coef == 1 .OR. coef == 3)THEN
     DO i=1,nb_maillon
       MAILLON(i)%behav    ='plexx'
       MAILLON(i)%bndry    ='YELLO'
       MAILLON(i)%av_radius= radius
!fd       MAILLON(i)%gr_radius= radius*sq2_1
       MAILLON(i)%var      = 0.0D+00
       MAILLON(i)%percent  = 100.D0
       MAILLON(i)%number   = nb_maillon
       MAILLON(i)%rmin     = radius
       MAILLON(i)%rmax     = radius
       MAILLON(i)%Vbegin   = 0.0D+00
       MAILLON(i)%cooref(1)= (2.*i-1.)*radius-bright
       IF (coef == 3) MAILLON(i)%cooref(1)= ((2.*i-1.)*radius)-radius
       MAILLON(i)%cooref(2)=-radius
       MAILLON(i)%cooref(3)= 0.0D+00
     ENDDO

     DO i=1,nb_maillon
       CALL write_mod_BODIES(NBRE+i,'RBDY2')
       CALL write_mod_PLAIN(i,'MAILL')
       CALL write_mod_NO3xx(i,'MAILL')
       CALL write_mod_BDARY(i,'MAILL')
       CALL write_mod_dof_ini(NBRE+i,Vbegin,'RBDY2')

       IF (coef == 3) CALL write_mod_dof(NBRE+i,'RBDY2',1,1,1)
 
     ENDDO

     IF (coef == 1) THEN
       CALL write_mod_dof(NBRE+1,'RBDY2',1,1,0)
       CALL write_mod_dof(NBRE+nb_maillon,'RBDY2',1,1,0)
     ENDIF
   ENDIF

   IF( coef == 2 )THEN
     base=3*bright/2.
     wall=0.5*(longueur-base)
     nb_base=INT(base/(2.*radius))
     nb_wall=INT(wall/(2.*radius))
     DO i=1,nb_wall
       MAILLON(i)%behav    = 'plexx'
       MAILLON(i)%bndry    = 'YELLO'
       MAILLON(i)%av_radius= radius
!fd       MAILLON(i)%gr_radius= radius*sq2_1
       MAILLON(i)%var      = 0.0D+00
       MAILLON(i)%percent  = 100.D+00
       MAILLON(i)%number   = nb_maillon
       MAILLON(i)%rmin     = radius
       MAILLON(i)%rmax     = radius
       MAILLON(i)%Vbegin   = 0.0D+00
       MAILLON(i)%cooref(1)=-bright*0.25
       MAILLON(i)%cooref(2)= (2.*nb_wall-2.*i+1.)*radius
       MAILLON(i)%cooref(3)= 0.0D+00
     ENDDO
     DO i=1,nb_base
       MAILLON(nb_wall+i)%behav    = 'plexx'
       MAILLON(nb_wall+i)%bndry    = 'YELLO'
       MAILLON(nb_wall+i)%av_radius= radius
!fd       MAILLON(nb_wall+i)%gr_radius= radius*sq2_1
       MAILLON(nb_wall+i)%var      = 0.0D+00
       MAILLON(nb_wall+i)%percent  = 100.D+00
       MAILLON(nb_wall+i)%number   = nb_maillon
       MAILLON(nb_wall+i)%rmin     = radius
       MAILLON(nb_wall+i)%rmax     = radius
       MAILLON(nb_wall+i)%Vbegin   = 0.0D+00
       MAILLON(nb_wall+i)%cooref(1)=-bright*0.25+2.*(i-1.)*radius
       MAILLON(nb_wall+i)%cooref(2)=-radius
       MAILLON(i)%cooref(3)        = 0.0D+00
     ENDDO
     DO i=1,nb_wall
       MAILLON(nb_base+nb_wall+i)%behav    = 'plexx'
       MAILLON(nb_base+nb_wall+i)%bndry    = 'YELLO'
       MAILLON(nb_base+nb_wall+i)%av_radius= radius
!fd       MAILLON(nb_base+nb_wall+i)%gr_radius= radius*sq2_1
       MAILLON(nb_base+nb_wall+i)%var      = 0.0D+00
       MAILLON(nb_base+nb_wall+i)%percent  = 100.D+00
       MAILLON(nb_base+nb_wall+i)%number   = nb_maillon
       MAILLON(nb_base+nb_wall+i)%rmin     = radius
       MAILLON(nb_base+nb_wall+i)%rmax     = radius
       MAILLON(nb_base+nb_wall+i)%Vbegin   = 0.0D+00
       MAILLON(nb_base+nb_wall+i)%cooref(1)=-bright*0.25+2.*(nb_base-1.)*radius
       MAILLON(nb_base+nb_wall+i)%cooref(2)= (2.*i-1.)*radius
       MAILLON(nb_base+nb_wall+i)%cooref(3)= 0.0D+00
     ENDDO
     nb_maillon=2*nb_wall+nb_base

     DO i=1,nb_maillon
       CALL write_mod_BODIES(NBRE+i,'RBDY2')
       CALL write_mod_PLAIN(i,'MAILL')
       CALL write_mod_NO3xx(i,'MAILL')
       CALL write_mod_BDARY(i,'MAILL')
       CALL write_mod_dof_ini(NBRE+i,Vbegin,'RBDY2')
     ENDDO

     CALL write_mod_dof(NBRE+1,'RBDY2',1,1,0)
     CALL write_mod_dof(NBRE+nb_maillon,'RBDY2',1,1,0)
   ENDIF

   CALL write_end_dof

 END SUBROUTINE CHAINETTE_DISKx
!-----------------------------------------------------------------------------------------------------------------------
 SUBROUTINE re_drum

   IMPLICIT NONE

   REAL(kind=8),DIMENSION(3) :: Vbegin=0.0D+00
   REAL(kind=8)              :: radius,h,alpha,alpha2
   INTEGER                   :: nb_facette=40,ip

   IF (nbDISKx /= 0) THEN

     DRUM%cooref(1) = box_size(1)*0.5
     DRUM%cooref(2) = box_size(2)*0.5
     DRUM%cooref(3) = 0.0D+00

     DRUM%av_radius = dsqrt(DRUM%cooref(1)*DRUM%cooref(1)+DRUM%cooref(2)*DRUM%cooref(2))
!fd     DRUM%gr_radius = DRUM%av_radius*sq2_1
     DRUM%number    = 1
     DRUM%rmin      = DRUM%av_radius
     DRUM%rmax      = DRUM%av_radius
     DRUM%Vbegin    = 0.0D+00

     NBRE=NBRE+1
     CALL write_begin_dof
     CALL write_mod_BODIES(NBRE,'RBDY2')
     CALL write_mod_PLAIN(1,'DRUMx')
     CALL write_mod_NO3xx(1,'DRUMx')
     CALL write_mod_BDARY(1,'DRUMx')
     CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
     CALL write_mod_dof(NBRE,'RBDY2',1,1,1)
   ENDIF

   IF (nbPOLYG /= 0) THEN

     radius = dsqrt((box_size(1)*0.5)*(box_size(1)*0.5)+(box_size(2)*0.5)*(box_size(2)*0.5))
     alpha  = 0.1570796D+00  !pi/20
     alpha2 = alpha*0.5
     h      = POLYG_radius_max

     IF(.NOT.ASSOCIATED(Facette(1)%vertex)) ALLOCATE(Facette(1)%vertex(2,4))

     Facette(1)%ptype     = 'QUADR'
     Facette(1)%vrtxnb    = 4
     Facette(1)%var       = 0.0D+00
     Facette(1)%Hfactor   = 1.D+00
     Facette(1)%percent   = 100.D+00
     Facette(1)%Vbegin    = 0.0D+00
     Facette(1)%av_radius = POLYG_radius_max
!fd     Facette(1)%gr_radius = POLYG_radius_max
       
     Facette(1)%vertex(1,1) = radius*SIN(alpha2)
     Facette(1)%vertex(2,1) = h*0.5
     Facette(1)%vertex(1,2) =-radius*SIN(alpha2)
     Facette(1)%vertex(2,2) = h*0.5
     Facette(1)%vertex(1,3) =-radius*SIN(alpha2)-h*TAN(alpha2)
     Facette(1)%vertex(2,3) =-h*0.5
     Facette(1)%vertex(1,4) = radius*SIN(alpha2)+h*TAN(alpha2)
     Facette(1)%vertex(2,4) =-h*0.5
     CALL write_begin_dof
     CALL write_begin_dof_rot
     DO ip=1,nb_facette
       NBRE=NBRE+1
       Facette(1)%cooref(1) = (box_size(1)*0.5)+radius*COS(-1.5707963+alpha2+(ip-1.)*alpha)
       Facette(1)%cooref(2) = (box_size(2)*0.5)+radius*SIN(-1.5707963+alpha2+(ip-1.)*alpha)
       Facette(1)%cooref(3) = alpha2 +(ip-1.)*alpha
          
       CALL write_mod_BODIES(NBRE,'RBDY2')
       CALL write_mod_PLAIN(ip,'FACET')
       CALL write_mod_NO3xx(ip,'FACET')
       CALL write_mod_BDARY(ip,'FACET')
       CALL write_mod_dof_ini(NBRE,Vbegin,'RBDY2')
       CALL write_mod_dof(NBRE,'RBDY2',1,1,1)      
       CALL write_mod_dof_rot(NBRE,'RBDY2',radius,alpha,ip)
     ENDDO
   ENDIF

   CALL write_end_dof
   CALL write_end_dof_rot

 END SUBROUTINE re_drum
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE vitesse_aleatoire(V___,radius_max)
  
    IMPLICIT NONE
    
    REAL(kind=8),DIMENSION(3),INTENT(inout)  :: V___
    REAL(kind=8),INTENT(inout)               :: radius_max
    REAL(kind=8)                             :: dx,dy,vx,vy
 
    CALL RANDOM_NUMBER(dx)
    CALL RANDOM_NUMBER(dy)
    CALL RANDOM_NUMBER(vx)
    CALL RANDOM_NUMBER(vy)

    V___(1)= 3.*vx*((-1.)**INT(2.*dx))
    V___(2)= 3.*vy*((-1.)**INT(2.*dy))
    V___(3)= 0.0D+00

  END SUBROUTINE vitesse_aleatoire
!-----------------------------------------------------------------------------------------------------------------------
! Subroutine pour l'écriture des fichiers BOBIES.DAT,DOF.INI et DRV_DOF.DAT 
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_begin_dof

    IMPLICIT NONE

    OPEN(UNIT=30,FILE=sample_driven_dof,STATUS='replace')
                    !12345
    WRITE(30,'(A5)')'! DOF'
    WRITE(30,'(A5)')''
                     !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(30,'(A72)')'!-----------------------------------------------------------------------'

  END SUBROUTINE write_begin_dof
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_begin_dof_rot

    IMPLICIT NONE

    OPEN(UNIT=35,FILE='DATBOX/DRV_DOF.DAT.ROT', STATUS='replace')
                    !12345
    WRITE(35,'(A5)')'! DOF'
    WRITE(35,'(A5)')''
                     !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(35,'(A72)')'!-----------------------------------------------------------------------'

  END SUBROUTINE write_begin_dof_rot
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_dof(Num,nom,d1,d2,d3)
   
    IMPLICIT NONE

    INTEGER,INTENT(in)          :: Num,d1,d2,d3
    CHARACTER(len=5),INTENT(in) :: nom


    WRITE(30,'(A6)') '      '
    WRITE(30,'(A72)')'$bdyty                                                                  '
    WRITE(30,101)         nom,Num
    WRITE(30,'(A72)')'$nodty                                                                  '
    WRITE(30,101)     'NO3xx',1
    WRITE(30,'(A103)')  &
 '$dofty        [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)]...*...[RAMPI.....+.....RAMP.*.time]'
    IF(d1.EQ.1)THEN
       WRITE(30,120)'vlocy',1,0.,0.,0.,0.,1.,0.
    ENDIF
    IF(d2.EQ.1)THEN
       WRITE(30,120)'vlocy',2,0.,0.,0.,0.,1.,0.
    ENDIF
    IF(d3.EQ.1)THEN
       WRITE(30,120)'vlocy',3,0.,0.,0.,0.,1.,0.
    ENDIF
    
    WRITE(30,'(A6)')'$$$$$$'

101 FORMAT(1X,A5,2X,I5)    
120 FORMAT(1X,A5,2X,I5,6(1X,D14.7))
 
  END SUBROUTINE write_mod_dof
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_dof_rot(Num,nom,radius,alpha,ip)
   
    IMPLICIT NONE

    INTEGER          :: Num,ip
    CHARACTER(len=5) :: nom
    REAL(kind=8)     :: radius,alpha

    WRITE(35,'(A6)') '      '
    WRITE(35,'(A72)')'$bdyty                                                                  '
    WRITE(35,101)         nom,Num
    WRITE(35,'(A72)')'$nodty                                                                  '
    WRITE(35,101)     'NO3xx',1
    WRITE(35,'(A103)')  &
 '$dofty        [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)]...*...[RAMPI.....+.....RAMP.*.time]'
    WRITE(35,120)'vlocy',1,0.,radius*omega0,omega0,alpha*0.5+(ip-1)*alpha,1.,0.

    WRITE(35,120)'vlocy',2,0.,radius*omega0,omega0,-1.5707963+alpha*0.5+(ip-1)*alpha,1.,0.
    
    WRITE(35,120)'vlocy',3,omega0,0.,0.,0.,1.,0.
    
    WRITE(35,'(A6)')'$$$$$$'

101 FORMAT(1X,A5,2X,I5)    
120 FORMAT(1X,A5,2X,I5,6(1X,D14.7))
 
  END SUBROUTINE write_mod_dof_rot
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_end_dof

    IMPLICIT NONE

    CLOSE(30)

  END SUBROUTINE write_end_dof
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_end_dof_rot

    IMPLICIT NONE

    CLOSE(35)

  END SUBROUTINE write_end_dof_rot
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE top

    IMPLICIT NONE

                     !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(20,'(A72)')'! DOF                                                                   '
    WRITE(20,'(A72)')'                                                                        '
    WRITE(20,'(A72)')'$steps      0                time= 0.0000000D+00                        '
    WRITE(20,'(A72)')'                                                                        '
    WRITE(20,'(A72)')'!-----------------------------------------------------------------------'
    
  END SUBROUTINE top
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_dof_ini(Num,V___,nom)

    IMPLICIT NONE

    INTEGER,INTENT(in)                    :: Num
    CHARACTER(len=5),INTENT(in)           :: nom
    REAL(kind=8),DIMENSION(3),INTENT(in)  :: V___
    REAL(kind=8),DIMENSION(3)             :: cooref
    
    cooref(1) = 0.
    cooref(2) = 0.
    cooref(3) = 0.

                      !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(20,'(A72)') '$bdyty                                                                  '
    WRITE(20,105) nom,Num
105 FORMAT(1X,A5,2X,I7)            
     
    WRITE(20,'(A72)') '$nodty                                                                  '
    WRITE(20,106)'NO3xx',1,'X(1)=',cooref(1),'X(2)=',cooref(2),'X(3)=',cooref(3)
    WRITE(20,107)'V(1)=',V___(1),'V(2)=',V___(2),'V(3)=',V___(3)
106 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,D14.7))
107 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
     
    WRITE(20,'(A6)')'$$$$$$'
    WRITE(20,'(A6)')'      '
     
 END SUBROUTINE write_mod_dof_ini
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE entete

    IMPLICIT NONE

                     !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(10,'(A72)')'! File BODIES                                                           '
    WRITE(10,'(A72)')'!                                                                       '
    WRITE(10,'(A72)')'! The symbol    $       preceeds a keyword used in scanning files.      '
    WRITE(10,'(A72)')'!                                                                       ' 
    WRITE(10,'(A72)')'! The symbol    bdyty   stands for  body type data.                     '
    WRITE(10,'(A72)')'! These data are distributed according to some species.                 '
    WRITE(10,'(A72)')'!                                                                       ' 
    WRITE(10,'(A72)')'! the specy     blmty   stands for  bulk element type data ,            '
    WRITE(10,'(A72)')'! i.e. part or total bulk geometric description,                        '
    WRITE(10,'(A72)')'! and bulk behaviour laws;                                              '
    WRITE(10,'(A72)')'!                                                                       '
    WRITE(10,'(A72)')'! the specy     nodty   stands for  node type data ,                    '
    WRITE(10,'(A72)')'! i.e. degrees of freedom data;                                         '
    WRITE(10,'(A72)')'!                                                                       '
    WRITE(10,'(A72)')'! the specy     tacty   stands for  contactor type data ;               '
    WRITE(10,'(A72)')'!                                                                       '
    WRITE(10,'(A72)')'! the keyword   $$$$$$  ends a body record.                             '
    WRITE(10,'(A72)')'!                                                                       '
    WRITE(10,'(A6)')'      '

  END SUBROUTINE entete
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_BODIES(n,nom)
    
    IMPLICIT NONE

    INTEGER,INTENT(in)          :: n
    CHARACTER(len=5),INTENT(in) :: nom
                      !123456789012345678901234567890123456789012345678901234567890123456789012
    WRITE(10,'(A72)') '$bdyty                                                                  '
    WRITE(10,101) nom,n
101 FORMAT(1X,A5,2X,I7)
    
  END SUBROUTINE write_mod_BODIES
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_PLAIN(n,whatiam)

    IMPLICIT NONE

    INTEGER,INTENT(in)          :: n
    CHARACTER(len=5),INTENT(in) :: whatiam
           !12345678901234        


    SELECT CASE(whatiam)
    CASE('DISKx')
                        !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',DISKx(n)%behav,'avrd=',DISKx(n)%av_radius,'gyrd=',DISKx(n)%gr_radius
       WRITE(10,102)'PLAIN',1,'behav',DISKx(n)%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE('MAILL')
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',MAILLON(n)%behav,'avrd=',MAILLON(n)%av_radius,'gyrd=',MAILLON(n)%gr_radius
       WRITE(10,102)'PLAIN',1,'behav',MAILLON(n)%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE('DRUMx')
       WRITE(10,'(A72)')'$blmty                                                                  '
       WRITE(10,102)'PLAIN',1,'behav',DRUM%behav,'avrd=',DRUM%av_radius,'gyrd=',0.d0
    CASE('JONCx')
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',JONCx(n)%behav,'avrd=',JONCx(n)%av_radius,'gyrd=',JONCx(n)%gr_radius
       WRITE(10,102)'PLAIN',1,'behav',JONCx(n)%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE('POLYG')
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',POLYG(n)%behav,'avrd=',POLYG(n)%av_radius,'gyrd=',POLYG(n)%gr_radius
       WRITE(10,102)'PLAIN',1,'behav',POLYG(n)%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE('POLYB')
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',POLYG_BOX(n)%behav,'avrd=',POLYG_BOX(n)%av_radius, &
!fd                              'gyrd=',POLYG_BOX(n)%gr_radius
       WRITE(10,102)'PLAIN',1,'behav',POLYG_BOX(n)%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE('FACET')
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',Facette%behav,'avrd=',Facette%av_radius, &
!fd                              'gyrd=',Facette%gr_radius
       WRITE(10,102)'PLAIN',1,'behav',Facette%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE('BLOCH')
       WRITE(10,'(A72)')'$blmty                                                                  '
!fd       write(10,102)'PLAIN',1,'behav',BLOCHET(1)%behav,'avrd=',BLOCHET(1)%av_radius, &
!fd                              'gyrd=',BLOCHET(1)%gr_radius
      WRITE(10,102)'PLAIN',1,'behav',BLOCHET(1)%behav,'avrd=',0.d0,'gyrd=',0.d0
    CASE default
    END SELECT

102 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
    
  END SUBROUTINE write_mod_PLAIN
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_NO3xx(n,whatiam)
    
    IMPLICIT NONE

    INTEGER,INTENT(in)     :: n
    CHARACTER(len=5),INTENT(in)   :: whatiam

    SELECT CASE(whatiam)
    CASE('DISKx')
                        !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',DISKx(n)%cooref(1),'coo2=',DISKx(n)%cooref(2),'coo3=',DISKx(n)%cooref(3)
    CASE('MAILL')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',MAILLON(n)%cooref(1),'coo2=',MAILLON(n)%cooref(2),'coo3=',MAILLON(n)%cooref(3)
    CASE('DRUMx')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',DRUM%cooref(1),'coo2=',DRUM%cooref(2),'coo3=',DRUM%cooref(3)
    CASE('JONCx')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',JONCx(n)%cooref(1),'coo2=',JONCx(n)%cooref(2),'coo3=',JONCx(n)%cooref(3)
    CASE('POLYG')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',POLYG(n)%cooref(1),'coo2=',POLYG(n)%cooref(2),'coo3=',POLYG(n)%cooref(3)
    CASE('POLYB')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',POLYG_BOX(n)%cooref(1),'coo2=',POLYG_BOX(n)%cooref(2), &
                                  'coo3=',POLYG_BOX(n)%cooref(3)
    CASE('FACET')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',Facette%cooref(1),'coo2=',Facette%cooref(2), &
                              'coo3=',Facette%cooref(3)
    CASE('BLOCH')
       WRITE(10,'(A72)')'$nodty                                                                  '
       WRITE(10,103)'NO3xx',1,'coo1=',BLOCHET(1)%cooref(1),'coo2=',BLOCHET(1)%cooref(2), &
                              'coo3=',BLOCHET(1)%cooref(3)
    CASE default

    END SELECT

103 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,D14.7))
    
  END SUBROUTINE write_mod_NO3xx
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_mod_BDARY(n,whatiam,color)   

    IMPLICIT NONE
    
    INTEGER,INTENT(in)          :: n
    CHARACTER(len=5),INTENT(in) :: whatiam
    CHARACTER(len=5),OPTIONAL   :: color
    INTEGER                     :: i
    REAL(kind=8)                 ::alpha,alpha0,pertub,signe,OGx,OGy

    SELECT CASE(whatiam)
    CASE('DISKx')
                        !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,106)'DISKx',1,'color',DISKx(n)%bndry,'byrd=',DISKx(n)%av_radius
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'      '
    CASE('MAILL')
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,106)'DISKx',1,'color',MAILLON(n)%bndry,'byrd=',MAILLON(n)%av_radius
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'       '
    CASE('DRUMx')
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,106)'xKSID',1,'color',DRUM%bndry,'byrd=',DRUM%av_radius
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'       '
    CASE('JONCx')
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,105)'JONCx',1,'color',color,'ax1=',JONCx(n)%ax1,'ax2=',JONCx(n)%ax2
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'      ' 
    CASE('POLYG')
       CALL RANDOM_NUMBER(alpha)
       alpha0=6.283038*alpha
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,104)'POLYG',1,'color',POLYG(n)%bndry,'nb_vertex=',POLYG(n)%vrtxnb,'byrd=',POLYG(n)%av_radius
       DO i=1,POLYG(n)%vrtxnb
          CALL RANDOM_NUMBER(pertub)
          CALL RANDOM_NUMBER(signe)
          pertub=((-1)**INT(signe*2))*pertub*3.141529/(2.*POLYG(n)%vrtxnb)
          IF( MZZZ == 'REGULIER') pertub = 0.D0

          POLYG(n)%vertex(1,i)=POLYG(n)%av_radius*COS(alpha+pertub+6.283038*(i-1)/REAL(POLYG(n)%vrtxnb,8))
          POLYG(n)%vertex(2,i)=POLYG(n)%av_radius*POLYG(n)%Hfactor*SIN(alpha+pertub+ &
                                                                   6.283038*(i-1)/REAL(POLYG(n)%vrtxnb,8))
       ENDDO
       OGx= SUM(POLYG(n)%vertex(1,1:POLYG(n)%vrtxnb))/REAL(POLYG(n)%vrtxnb,kind=8)
       OGy= SUM(POLYG(n)%vertex(2,1:POLYG(n)%vrtxnb))/REAL(POLYG(n)%vrtxnb,kind=8)
       DO i=1,POLYG(n)%vrtxnb
          WRITE(10,'(29X,A5,D14.7,2X,A5,D14.7)') 'coo1=',POLYG(n)%vertex(1,i)-OGx ,'coo2=',POLYG(n)%vertex(2,i)-OGy
       ENDDO
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'      '   
    CASE('POLYB')
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,104)'POLYG',1,'color',POLYG_BOX(n)%bndry,'nb_vertex=',POLYG_BOX(n)%vrtxnb, &
            'byrd=',POLYG_BOX(n)%av_radius
       DO i=1,POLYG_BOX(n)%vrtxnb
          WRITE(10,'(29X,A5,D14.7,2X,A5,D14.7)') 'coo1=',POLYG_BOX(n)%vertex(1,i) ,'coo2=',POLYG_BOX(n)%vertex(2,i)
       ENDDO
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'      ' 
    CASE('FACET')
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,104)'POLYG',1,'color',Facette(1)%bndry,'nb_vertex=',Facette(1)%vrtxnb, &
            'byrd=',Facette(1)%av_radius
       DO i=1,4
          WRITE(10,'(29X,A5,D14.7,2X,A5,D14.7)') 'coo1=',Facette(1)%vertex(1,i) ,'coo2=',Facette(1)%vertex(2,i)
       ENDDO
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'      ' 
    CASE('BLOCH')
       WRITE(10,'(A72)')'$tacty                                                                  '
       WRITE(10,104)'POLYG',1,'color',BLOCHET(1)%bndry,'nb_vertex=',BLOCHET(1)%vrtxnb, &
            'byrd=',BLOCHET(1)%av_radius
       DO i=1,4
          WRITE(10,'(29X,A5,D14.7,2X,A5,D14.7)') 'coo1=',BLOCHET(1)%vertex(1,i) ,'coo2=',BLOCHET(1)%vertex(2,i)
       ENDDO
       WRITE(10,'(A6)')'$$$$$$'
       WRITE(10,'(A6)')'      '
    END SELECT

104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I5,6X,A5,D14.7)
105 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
106 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,1(2X,A5,D14.7))

  END SUBROUTINE write_mod_BDARY
!-----------------------------------------------------------------------------------------------------------------------
  SUBROUTINE write_couette_data

    IMPLICIT NONE

    NBRE=NBRE+1

    WRITE(10,'(A72)') '$bdyty                                                                  '
    WRITE(10,101)      'RBDY2',NBRE
    WRITE(10,'(A72)') '$blmty                                                                  '
    WRITE(10,102)      'PLAIN',1,'behav','MEMBx','avrd=',INTRAD,'gyrd=',INTRAD*0.707
    WRITE(10,'(A72)') '$nodty                                                                  '
    WRITE(10,103)      'NO4xx',1,'coo1=',0.D0,'coo2=',0.D0,'coo3=',0.D0
    WRITE(10,'(29X,A5,D14.7)')   'coo4=',INTRAD
    WRITE(10,'(A72)')'$tacty                                                                  '
    WRITE(10,104)      'DISPx',1,'color','INTER','byrd=','     '
    WRITE(10,'(A6)')'$$$$$$'
    WRITE(10,'(A6)')'      '

    WRITE(20,'(A72)') '$bdyty                                                                  '
    WRITE(20,101)      'RBDY2',NBRE
    WRITE(20,'(A72)') '$nodty                                                                  '
    WRITE(20,103)      'NO3xx',1,'X(1)=',0.D0,'X(2)=',0.D0,'X(3)=',0.D0
    WRITE(20,'(29X,A5,D14.7)')   'X(4)=',0.D0
    WRITE(20,105)                'V(1)=',0.D0,'V(2)=',0.D0,'V(3)=',0.D0
    WRITE(20,'(29X,A5,D14.7)')   'V(4)=',0.D0
    WRITE(20,'(A6)')'$$$$$$'
    WRITE(20,'(A6)')'      '

    CALL write_begin_dof

    WRITE(30,'(A6)')  '      '
    WRITE(30,'(A72)') '$bdyty                                                                  '
    WRITE(30,101)      'RBDY2',NBRE
    WRITE(30,'(A72)') '$nodty                                                                  '
    WRITE(30,101)      'NO3xx',1
    WRITE(30,'(A103)')  &
 '$dofty        [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)]...*...[RAMPI.....+.....RAMP.*.time]'
    WRITE(30,106)'vlocy',1,0.,0.,0.,0.,1.,0.
    WRITE(30,106)'vlocy',2,0.,0.,0.,0.,1.,0.
    WRITE(30,106)'vlocy',3,0.,0.,0.,0.,1.,0.
    WRITE(30,106)'vlocy',4,0.,0.,0.,0.,1.,0.
    
    WRITE(30,'(A6)')'$$$$$$'


    NBRE=NBRE+1

    WRITE(10,'(A72)') '$bdyty                                                                  '
    WRITE(10,101)      'RBDY2',NBRE
    WRITE(10,'(A72)') '$blmty                                                                  '
    WRITE(10,102)      'PLAIN',1,'behav','MEMBx','avrd=',EXTRAD,'gyrd=',EXTRAD*0.707
    WRITE(10,'(A72)') '$nodty                                                                  '
    WRITE(10,103)      'NO4xx',1,'coo1=',0.D0,'coo2=',0.D0,'coo3=',0.D0
    WRITE(10,'(29X,A5,D14.7)')   'coo4=',EXTRAD
    WRITE(10,'(A72)')'$tacty                                                                  '
    WRITE(10,104)      'xPSID',1,'color','EXTER','byrd=','     '
    WRITE(10,'(A6)')'$$$$$$'
    WRITE(10,'(A6)')'      '

    WRITE(20,'(A72)') '$bdyty                                                                  '
    WRITE(20,101)      'RBDY2',NBRE
    WRITE(20,'(A72)') '$nodty                                                                  '
    WRITE(20,103)      'NO3xx',1,'X(1)=',0.D0,'X(2)=',0.D0,'X(3)=',0.D0
    WRITE(20,'(29X,A5,D14.7)')   'X(4)=',0.D0
    WRITE(20,105)                'V(1)=',0.D0,'V(2)=',0.D0,'V(3)=',0.D0
    WRITE(20,'(29X,A5,D14.7)')   'V(4)=',0.D0
    WRITE(20,'(A6)')'$$$$$$'
    WRITE(20,'(A6)')'      '

    WRITE(30,'(A6)')  '      '
    WRITE(30,'(A72)') '$bdyty                                                                  '
    WRITE(30,101)      'RBDY2',NBRE
    WRITE(30,'(A72)') '$nodty                                                                  '
    WRITE(30,101)      'NO3xx',1
    WRITE(30,'(A103)')  &
 '$dofty        [CT......+......AMP..*..cos.(..OMEGA.*.time.+.PHI..)]...*...[RAMPI.....+.....RAMP.*.time]'
    WRITE(30,106)'vlocy',1,0.,0.,0.,0.,1.,0.
    WRITE(30,106)'vlocy',2,0.,0.,0.,0.,1.,0.
    WRITE(30,106)'vlocy',3,0.,0.,0.,0.,1.,0.
    WRITE(30,106)'vlocy',4,0.,0.,0.,0.,1.,0.
    
    WRITE(30,'(A6)')'$$$$$$'

    CALL write_end_dof


101 FORMAT(1X,A5,2X,I5)
102 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
103 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,D14.7))
104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,1(2X,A5,D14.7))
105 FORMAT(27X,3(2X,A5,D14.7))
106 FORMAT(1X,A5,2X,I5,6(1X,D14.7))

  END SUBROUTINE write_couette_data

!vr -- CONVERSION du fichier PREBOX/BODIES.DAT en bodies.eps --
SUBROUTINE bodies2eps
  IMPLICIT NONE 
  INTEGER,PARAMETER  :: infile=874
  INTEGER,PARAMETER  :: psfile=876
  CHARACTER(len=95)  :: line
  CHARACTER(len=5)   :: objtype
  REAL(kind=8)       :: angldeb, angl
  REAL(kind=8)       :: rayon, x, y, teta, xjonc, yjonc
  REAL(kind=8)       :: xmin=999.0, xmax=-999.0, ymin=999.0, ymax=-999.0
  REAL(kind=8)       :: zoomfactor=1.0 
  !logical::Wflag=.true. ! .true.  pour Width  impose a la valeur psdim
                        ! .false. pour Height impose a la valeur psdim

  OPEN(infile,file="PREBOX/BODIES.DAT",status="old", &
       action="read",position="rewind")

! determination du cadre (i.e. la valeurs max et min)
! premiere lecture de PREBOX/BODIES.DAT
  DO 
     READ (infile,"(A)",END=888) line
     IF (line(2:6) == "RBDY2") THEN
           DO WHILE (line(1:6) /= "$$$$$$")
              READ (infile,"(A)",END=888) line

              IF (line(2:6)=="PLAIN") THEN
                 READ(line(35:48),"(D14.7)") rayon
              END IF

              IF (line(2:6)=="NO3xx") THEN
                 READ(line(35:48),"(D14.7)") x
                 READ(line(56:69),"(D14.7)") y
                 READ(line(77:90),"(D14.7)") teta
                 IF (teta<0.0)        teta = teta+6.283185D0
                 IF (teta>6.283185D0) teta = teta-6.283185D0
                 IF (x+rayon > xmax) xmax = x+rayon
                 IF (x-rayon < xmin) xmin = x-rayon
                 IF (y+rayon > ymax) ymax = y+rayon
                 IF (y-rayon < ymin) ymin = y-rayon
              END IF
              
              IF (line(2:6)=="JONCx") THEN
                 objtype="JONCx"
                 READ(line(35:48),"(D14.7)") xjonc
                 READ(line(56:69),"(D14.7)") yjonc
                 IF (x+ABS(xjonc*COS(teta))+yjonc > xmax) xmax = x+ABS(xjonc*COS(teta))+yjonc
                 IF (x-ABS(xjonc*COS(teta))-yjonc < xmin) xmin = x-ABS(xjonc*COS(teta))-yjonc
                 IF (y+ABS(xjonc*SIN(teta))+yjonc > ymax) ymax = y+ABS(xjonc*SIN(teta))+yjonc
                 IF (y-ABS(xjonc*SIN(teta))-yjonc < ymin) ymin = y-ABS(xjonc*SIN(teta))-yjonc
              ENDIF

           END DO
     END IF
  END DO
888 CLOSE(infile)
!------------------------------------------------------------

  OPEN(infile,file="PREBOX/BODIES.DAT",status="old", &
       action="read",position="rewind")
  OPEN(psfile,file="bodies.eps",status="replace", &
       action="write",position="rewind")

  IF (Wflag) THEN            ! avec pswidth
    zoomfactor = (psdim*29.5276)/(xmax-xmin)
  ELSE                       ! avec psheight
    zoomfactor = (psdim*29.5276)/(ymax-ymin)
  END IF

  WRITE (psfile,"(A)") "%!PS-Adobe-3.0 EPSF-3.0"
  WRITE (psfile,"(A18,I4,I4)") "%%BoundingBox: 0 0 ",&
                      INT((xmax-xmin)*zoomfactor)+4, &
                      INT((ymax-ymin)*zoomfactor)+4

  !TODO : ces definitions permettent de reduire la taille du fichier eps
  !write (psfile,"(A)") "/lt {lineto} def"
  !write (psfile,"(A)") "/s {stroke} def"
  !write (psfile,"(A)") "/np {newpath} def"
  !write (psfile,"(A)") "/cp {closepath} def"

  DO
     ! seconde lecture de DATBOX/BODIES.DAT
     READ (infile,"(A)",END=999) line
     IF (line(2:6) == "RBDY2") THEN
        objtype="xxxxx"
           DO WHILE (line(1:6) /= "$$$$$$")
              READ (infile,"(A)",END=999) line

              IF (line(2:6)=="PLAIN") THEN
                 READ(line(35:48),"(D14.7)") rayon
              END IF

              IF (line(2:6)=="NO3xx") THEN
                 READ(line(35:48),"(D14.7)") x
                 READ(line(56:69),"(D14.7)") y
                 READ(line(77:90),"(D14.7)") teta
              END IF

              IF (line(2:6)=="DISKx") THEN
                 objtype="DISKx"
              END IF
              
              IF (line(2:6)=="JONCx") THEN
                 objtype="JONCx"
                 READ(line(35:48),"(D14.7)") xjonc
                 READ(line(56:69),"(D14.7)") yjonc
              ENDIF

           END DO
  
     ! convertion postscript :
     IF (objtype=="DISKx") THEN
        WRITE (psfile,"(A)",advance="no") "newpath "
        WRITE (psfile,841,advance="no") (x-xmin)*zoomfactor+2,&
                                        (y-ymin)*zoomfactor+2,&
                                         rayon*zoomfactor
        WRITE (psfile,"(A)") " closepath stroke"
     END IF

     IF (objtype=="JONCx") THEN
        
        angldeb=-(1.5707963D0-teta)*57.295779D0
        angl = angldeb+180.0
        IF (angldeb<0.0) angldeb = angldeb+360.0
        IF (angl<0.0)       angl = angl+360.0
        IF (angldeb>360.0) angldeb = angldeb-360.0
        IF (angl>360.0)       angl = angl-360.0
        WRITE (psfile,"(A)") "newpath"

        WRITE (psfile,842) (x-xmin+xjonc*COS(teta))*zoomfactor+2,&
                           (y-ymin+xjonc*SIN(teta))*zoomfactor+2,&
                           yjonc*zoomfactor, &
                           angldeb,          &
                           angl
        angldeb=angl
        angl = angldeb+180.0
        IF (angl>360.0)       angl = angl-360.0        
        WRITE (psfile,843) (x-xmin-(xjonc*COS(teta)+yjonc*SIN(teta)))*zoomfactor+2,&
                           (y-ymin-(xjonc*SIN(teta)-yjonc*COS(teta)))*zoomfactor+2
        WRITE (psfile,842) (x-xmin-xjonc*COS(teta))*zoomfactor+2,&
                           (y-ymin-xjonc*SIN(teta))*zoomfactor+2,&
                           yjonc*zoomfactor,  &
                           angldeb,           &
                           angl
        WRITE (psfile,"(A)") "closepath stroke"

     END IF
    END IF
  END DO

841     FORMAT(G11.5,1X,G11.5,1X,G11.5," 0.0 360.0 arc",1X)
842     FORMAT(G11.5,1X,G11.5,1X,G11.5,1X,2G10.4," arc",1X)
843     FORMAT(G11.5,1X,G11.5," lineto",1X)

999     PRINT *,"PREBOX/BODIES.DAT convert in bodies.eps"
  CLOSE(infile)
  CLOSE(psfile)
END SUBROUTINE bodies2eps
!fd
  SUBROUTINE write_brick_mailx(imailx,id,is_last)
     implicit none
     integer,intent(in) :: imailx,id
     logical :: is_last
     integer :: nfich=10

     REAL(kind=8),DIMENSION(3)   :: X,X0
     INTEGER :: i,j,it3xxx,inode,itac
     REAL(kind=8) :: dir,LX,LY

     WRITE(nfich,'(A72)') '$bdyty                                                                  '
     WRITE(nfich,10)'MAILx',imailx
                      !123456789012345678901234567890123456789012345678901234567890123456789012


     if (id == 2) then
     WRITE(nfich,'(A72)') '$blmty                                                                  '
     inode=1
     it3xxx=0
     DO i=1,POLYG_BOX(id)%vrtxnb-1
       it3xxx=it3xxx+1
       inode=inode+1
       WRITE(nfich,20) it3xxx,1,inode,inode+1
       WRITE(nfich,30) 'M2D_L','stone'
       it3xxx=it3xxx+1
       inode=inode+1
       WRITE(nfich,20) it3xxx,1,inode,inode+1
       WRITE(nfich,30) 'M2D_L','stone'
     ENDDO
     it3xxx=it3xxx+1
     inode=inode+1
     WRITE(nfich,20) it3xxx,1,inode,inode+1
     WRITE(nfich,30) 'M2D_L','stone'
     it3xxx=it3xxx+1
     inode=inode+1
     WRITE(nfich,20) it3xxx,1,inode,2
     WRITE(nfich,30) 'M2D_L','stone'


     WRITE(nfich,'(A72)') '$nodty                                                                  '

     inode=1
     X(1:2)=POLYG_BOX(id)%cooref(1:2)
     CALL write_a_nodty('NO2xx',inode,X,'coo',nfich)
     DO i=1,POLYG_BOX(id)%vrtxnb-1
       inode=inode+1
       X(1:2)=POLYG_BOX(id)%cooref(1:2)+POLYG_BOX(id)%vertex(1:2,i)
       CALL write_a_nodty('NO2xx',inode,X,'coo',nfich)
       inode=inode+1
       X(1:2)=POLYG_BOX(id)%cooref(1:2)+(POLYG_BOX(id)%vertex(1:2,i)+POLYG_BOX(id)%vertex(1:2,i+1))*0.5
       CALL write_a_nodty('NO2xx',inode,X,'coo',nfich)
     END DO
     inode=inode+1
     X(1:2)=POLYG_BOX(id)%cooref(1:2)+POLYG_BOX(id)%vertex(1:2,POLYG_BOX(id)%vrtxnb)
     CALL write_a_nodty('NO2xx',inode,X,'coo',nfich)
     inode=inode+1
     X(1:2)=POLYG_BOX(id)%cooref(1:2)+(POLYG_BOX(id)%vertex(1:2,POLYG_BOX(id)%vrtxnb)+POLYG_BOX(id)%vertex(1:2,1))*0.5
     CALL write_a_nodty('NO2xx',inode,X,'coo',nfich)


     WRITE(nfich,'(A72)') '$tacty                                                                  '
     itac=0
     inode=1
     DO i=1,POLYG_BOX(id)%vrtxnb-1
       inode=inode+1
       X(1:2)=POLYG_BOX(id)%vertex(1:2,i+1) - POLYG_BOX(id)%vertex(1:2,i)
       dir=X(2)*dsqrt(2.d0) - X(1)*dsqrt(2.d0)
       IF (dir < 0.d0) THEN
         itac=itac+1
         WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
         itac=itac+1
         WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75

         inode=inode+1
         itac=itac+1
         WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
         itac=itac+1
         WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
       ELSE 
         itac=itac+1
         WRITE(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode 
         if (is_last) then
           itac=itac+1
           WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
           itac=itac+1
           WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
         endif

         inode=inode+1
         itac=itac+1
         WRITE(nfich,40) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode
         if (is_last) then
           itac=itac+1
           WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
           itac=itac+1
           WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
         endif
       ENDIF
    ENDDO
    X(1:2)=POLYG_BOX(id)%vertex(1:2,1) - POLYG_BOX(id)%vertex(1:2,POLYG_BOX(id)%vrtxnb)
    dir=X(2)*dsqrt(2.d0) - X(1)*dsqrt(2.d0)
    inode=inode+1
    IF (dir < 0.d0) THEN
      itac=itac+1
      WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
      itac=itac+1
      WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75

      inode=inode+1
      itac=itac+1
      WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.25
      itac=itac+1
      WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.75
    ELSE
      itac=itac+1
      WRITE(nfich,50) 'ALpxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode 
      if (is_last) then
        itac=itac+1
        WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.25
        itac=itac+1
        WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',inode+1,'nodb=',inode,'apab=',0.75
      endif
      inode=inode+1
      itac=itac+1
      WRITE(nfich,40) 'ALpxx',itac,'color','REDxx','noda=',2,'nodb=',inode
      if (is_last) then
        itac=itac+1
        WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.25
        itac=itac+1
        WRITE(nfich,40) 'CLxxx',itac,'color','REDxx','noda=',2,'nodb=',inode,'apab=',0.75
      endif
    ENDIF

    else

     WRITE(nfich,'(A72)') '$blmty                                                                  '

     WRITE(nfich,20) 1,1,4,5
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 2,1,5,2
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 3,2,5,3
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 4,3,5,6
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 5,4,7,5
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 6,5,7,8
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 7,5,8,9
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 8,5,9,6
     WRITE(nfich,30) 'M2D_L','stone'

     WRITE(nfich,20) 9,7,10,11
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 10,7,11,8
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 11,8,11,9
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 12,9,11,12
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 13,10,13,11
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 14,11,13,14
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 15,11,14,15
     WRITE(nfich,30) 'M2D_L','stone'
     WRITE(nfich,20) 16,11,15,12
     WRITE(nfich,30) 'M2D_L','stone'


     WRITE(nfich,'(A72)') '$nodty                                                                  '

     inode=1
     X0(1:2)=POLYG_BOX(id)%cooref(1:2)+POLYG_BOX(id)%vertex(1:2,4)
     LX=(POLYG_BOX(id)%vertex(1,1)-POLYG_BOX(id)%vertex(1,4))*0.25   
     LY=(POLYG_BOX(id)%vertex(2,3)-POLYG_BOX(id)%vertex(2,4))*0.5
     DO i=1,5
       X(1) = X0(1) + ((i-1)*LX)
       DO j=1,3
         X(2) = X0(2) +  ((j-1)*LY)
         CALL write_a_nodty('NO2xx',inode,X,'coo',nfich)
         inode = inode + 1
       ENDDO
     ENDDO 

     WRITE(nfich,'(A72)') '$tacty                                                                  '

     WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',13,'nodb=',10,'apab=',0.05
     WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',13,'nodb=',10,'apab=',0.95
     WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',10,'nodb=',7,'apab=',0.05
     WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',10,'nodb=',7,'apab=',0.95
     WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',7,'nodb=',4,'apab=',0.05
     WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',7,'nodb=',4,'apab=',0.95
     WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',4,'nodb=',1,'apab=',0.05
     WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',4,'nodb=',1,'apab=',0.95
     WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',1,'nodb=',2,'apab=',0.05
     WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',1,'nodb=',2,'apab=',0.95
     WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',2,'nodb=',3,'apab=',0.05
     WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',2,'nodb=',3,'apab=',0.95
     if (is_last) then
       WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',3,'nodb=',6,'apab=',0.05
       WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',3,'nodb=',6,'apab=',0.95
       WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',6,'nodb=',9,'apab=',0.05
       WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',6,'nodb=',9,'apab=',0.95
       WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',9,'nodb=',12,'apab=',0.05
       WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',9,'nodb=',12,'apab=',0.95
       WRITE(nfich,40) 'CLxxx',1,'color','REDxx','noda=',12,'nodb=',15,'apab=',0.05
       WRITE(nfich,40) 'CLxxx',2,'color','REDxx','noda=',12,'nodb=',15,'apab=',0.95
     else
       WRITE(nfich,40) 'ALpxx',1,'color','REDxx','noda=',3,'nodb=',6
       WRITE(nfich,40) 'ALpxx',1,'color','REDxx','noda=',6,'nodb=',9
       WRITE(nfich,40) 'ALpxx',1,'color','REDxx','noda=',9,'nodb=',12
       WRITE(nfich,40) 'ALpxx',1,'color','REDxx','noda=',12,'nodb=',15
     endif
     WRITE(nfich,40) 'ALpxx',1,'color','REDxx','noda=',15,'nodb=',14
     WRITE(nfich,40) 'ALpxx',1,'color','REDxx','noda=',14,'nodb=',13

    endif

    WRITE(nfich,'(A72)') '$$$$$$                                                                  '

10 FORMAT(1X,A5,2X,I5)            
20 FORMAT(' T3xxx',2x,I5,2x,'nodes',8(2x,I5))
30 FORMAT(15x,'model',2x,A5,2x,'behav',2x,A5)
40 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,D14.7)
50 FORMAT(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)

  END SUBROUTINE

!!$  SUBROUTINE extrusion(nfich,deep,factor)
!!$    
!!$    IMPLICIT NONE
!!$    INTEGER                   :: nfich,nb_bodies
!!$    INTEGER                   :: ibdyty,itacty,nb_tacty,ibdy,i,itact
!!$    INTEGER                   :: nb_faces,nb_vertex_3D
!!$    REAL(kind=8),DIMENSION(3) :: vect 
!!$    CHARACTER(len=5)          :: tactID
!!$    TYPE(T_POLYG)             :: Bodie_polyg
!!$    CHARACTER(len=5)          :: tactID_3D,color,clin
!!$    CHARACTER(len=70)         :: clin3
!!$    REAL(kind=8),DIMENSION(2) :: axes
!!$    REAL(kind=8)              :: radius
!!$
!!$    REAL(kind=8)              :: deep,factor
!!$
!!$    nb_bodies=0
!!$
!!$    DO ibdyty=1,nb_POLYG
!!$       nb_bodies=nb_bodies+1
!!$       Bodie_polyg  = get_l_polyg(ibdyty)
!!$       nb_vertex_3D = 2*Bodie_polyg%nb_vertex
!!$       nb_faces      = 4*Bodie_polyg%nb_vertex-4
!!$       tactID_3D='POLYH';
!!$       itacty=1
!!$       color = get_color(polyg2bdyty(1,ibdyty),polyg2bdyty(2,itacty))
!!$       itact=1
!!$       vect=0.D0
!!$       clin='NO6xx'
!!$       WRITE(nfich,'(A6)')       '$bdyty'
!!$       WRITE(nfich,'(1X,A5,I7)') 'RBDY3',nb_bodies
!!$       WRITE(nfich,'(A6)')       '$blmty'
!!$       WRITE(nfich,'(A72)')       ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
!!$       
!!$       WRITE(nfich,'(A6)') '$nodty'
!!$       WRITE(nfich,105) clin,itact,'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
!!$       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
!!$       vect=0.D0
!!$       WRITE(nfich,'(A6)') '$tacty'
!!$       WRITE(nfich,104) tactID_3D,itact,'color',color,'nb_vertex=',nb_vertex_3D,'nb_faces=',nb_faces
!!$       
!!$       DO i=1,Bodie_polyg%nb_vertex
!!$          vect(1)=Bodie_polyg%vertex(1,i)
!!$          vect(2)=0.D0
!!$          vect(3)=Bodie_polyg%vertex(2,i)
!!$          WRITE(nfich,131) 'coo1=',vect(1)*factor,'coo2=',vect(2)*factor,'coo3=',vect(3)*factor
!!$       ENDDO
!!$       DO i=1,Bodie_polyg%nb_vertex
!!$          vect(1)=Bodie_polyg%vertex(1,i)
!!$          vect(2)=deep
!!$          vect(3)=Bodie_polyg%vertex(2,i)
!!$          WRITE(nfich,131) 'coo1=',vect(1)*factor,'coo2=',vect(2),'coo3=',vect(3)*factor
!!$       ENDDO
!!$       DO i=1,Bodie_polyg%nb_vertex-1
!!$          WRITE(nfich,132) 'ver1=',i,'ver2=',Bodie_polyg%nb_vertex+i,'ver3=',Bodie_polyg%nb_vertex+i+1
!!$          WRITE(nfich,132) 'ver1=',i,'ver2=',Bodie_polyg%nb_vertex+i+1,'ver3=',i+1
!!$       ENDDO
!!$       i=1
!!$       WRITE(nfich,132) 'ver1=',Bodie_polyg%nb_vertex,'ver2=',2*Bodie_polyg%nb_vertex,'ver3=',i
!!$       WRITE(nfich,132) 'ver1=',2*Bodie_polyg%nb_vertex,'ver2=',i,'ver3=',Bodie_polyg%nb_vertex+1
!!$       DO i=2,Bodie_polyg%nb_vertex-1
!!$          itact=1 
!!$          WRITE(nfich,132) 'ver1=',itact,'ver2=',i,'ver3=',i+1
!!$       ENDDO
!!$       DO i=Bodie_polyg%nb_vertex+2,2*Bodie_polyg%nb_vertex-1
!!$          itact=Bodie_polyg%nb_vertex+1
!!$          WRITE(nfich,132) 'ver1=',itact,'ver2=',i,'ver3=',i+1
!!$       ENDDO
!!$       WRITE(nfich,'(A6)') '$$$$$$'
!!$    ENDDO
!!$    
!!$    DO ibdyty=1,nb_DISKx
!!$       nb_bodies=nb_bodies+1
!!$       itacty=1
!!$       vect(1:3) = get_coor(diskx2bdyty(1,ibdyty),diskx2bdyty(2,itacty))
!!$       itacty=1
!!$       color     = get_color(diskx2bdyty(1,ibdyty),diskx2bdyty(2,itacty))
!!$       radius=get_radius_DISKx(ibdyty)
!!$       clin='NO6xx'
!!$       WRITE(nfich,'(A72)') '$bdyty                                                                  '
!!$       WRITE(nfich,101)'RBDY3',nb_bodies
!!$       WRITE(nfich,'(A6)') '$blmty'
!!$       WRITE(nfich,'(A72)') ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
!!$       WRITE(nfich,'(A6)') '$nodty'
!!$       vect(3)=0.D0
!!$       itact=1
!!$       WRITE(nfich,105)clin,itact,'coo1=',vect(1)*factor,'coo2=',vect(3)*factor,'coo3=',vect(2)*factor
!!$       vect=0.D0
!!$       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
!!$       WRITE(nfich,'(A6)') '$tacty'
!!$       !1234567890123456789012345678901234567890123456789012345678901234567890
!!$       clin3='                                                                      '
!!$       clin3(1:5)='SPHER'
!!$       WRITE(clin3(10:12),'(I3)') 1
!!$       clin3(15:19)='color'
!!$       clin3(22:26)=color
!!$       clin3(29:34)='byrd='
!!$       WRITE(clin3(34:47),'(D14.7)') radius*factor
!!$       WRITE(nfich,*) clin3
!!$       WRITE(nfich,'(A6)') '$$$$$$'
!!$    ENDDO
!!$    
!!$    DO ibdyty=1,nb_xKSID
!!$       nb_bodies=nb_bodies+1
!!$       itacty=1
!!$       vect(1:3) = get_coor(xksid2bdyty(1,ibdyty),xksid2bdyty(2,itacty))
!!$       itacty=1
!!$       color     = get_color(xksid2bdyty(1,ibdyty),xksid2bdyty(2,itacty))
!!$       radius=get_radius_xKSID(ibdyty)
!!$       clin='NO6xx'
!!$       WRITE(nfich,'(A72)') '$bdyty                                                                  '
!!$       WRITE(nfich,101)'RBDY3',nb_bodies
!!$       WRITE(nfich,'(A6)') '$blmty'
!!$       WRITE(nfich,'(A72)') ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
!!$       WRITE(nfich,'(A6)') '$nodty'
!!$       vect(3)=0.D0
!!$       itact=1
!!$       WRITE(nfich,105)clin,itact,'coo1=',vect(1)*factor,'coo2=',vect(3)*factor,'coo3=',vect(2)*factor
!!$       vect=0.D0
!!$       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
!!$       WRITE(nfich,'(A6)') '$tacty'
!!$       !1234567890123456789012345678901234567890123456789012345678901234567890
!!$       clin3='                                                                      '
!!$       clin3(1:5)='CYLDx'
!!$       WRITE(clin3(10:12),'(I3)') 1
!!$       clin3(15:19)='color'
!!$       clin3(22:26)='BLEUx'
!!$       clin3(29:34)='High='
!!$       WRITE(clin3(34:47),'(D14.7)') deep
!!$       clin3(50:55)='Byrd='
!!$       WRITE(clin3(55:68),'(D14.7)') radius*factor
!!$       WRITE(1,*) clin3
!!$       WRITE(1,'(A6)')'$$$$$$'
!!$    ENDDO
!!$    
!!$    DO ibdyty=1,nb_JONCx
!!$       nb_bodies=nb_bodies+1
!!$       itacty=1
!!$       vect(1:3) = get_coor(joncx2bdyty(1,ibdyty),joncx2bdyty(2,itacty))
!!$       color = get_color(joncx2bdyty(1,ibdyty),joncx2bdyty(2,itacty))
!!$       CALL get_data(joncx2bdyty(1,ibdyty),joncx2bdyty(2,itacty),axes)
!!$       clin='NO6xx'
!!$       !1234567890123456789012345678901234567890123456789012345678901234567890
!!$       clin3='                                                                      '
!!$       clin3(1:5)='PLANx'
!!$       WRITE(clin3(10:12),'(I3)') 1
!!$       clin3(15:19)='color'
!!$       clin3(22:26)=color
!!$       WRITE(nfich,'(A6)') '$bdyty'
!!$       WRITE(nfich,101)'RBDY3',nb_bodies
!!$       WRITE(nfich,'(A6)') '$blmty'
!!$       WRITE(nfich,'(A72)') ' PLAIN      1  behav  PLEXx  avrd= 0.0000000D+01  gyrd= 0.0000000D+00    '
!!$       WRITE(nfich,'(A6)') '$nodty'
!!$       vect(3)=0.D0
!!$       WRITE(nfich,105)clin,itact,'coo1=',vect(1)*factor,'coo2=',vect(3)*factor,'coo3=',vect(2)*factor
!!$       vect=0.D0
!!$       WRITE(nfich,103) 'coo4=',vect(1),'coo5=',vect(2),'coo6=',vect(3)
!!$       WRITE(nfich,'(A6)') '$tacty'
!!$       WRITE(nfich,*) clin3
!!$       vect=0.D0;
!!$       vect(1)=axes(1)*factor
!!$       WRITE(nfich,103) 'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
!!$       vect=0.D0;
!!$       vect(2)=4.D0*deep
!!$       WRITE(nfich,103) 'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
!!$       vect=0.D0;
!!$       vect(3)=axes(2)*factor
!!$       WRITE(nfich,103) 'coo1=',vect(1),'coo2=',vect(2),'coo3=',vect(3)
!!$       WRITE(nfich,'(A6)')'$$$$$$'
!!$    END DO
!!$    
!!$104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7,4X,A9,I7)
!!$131 FORMAT(27X,3(2X,A5,D14.7))
!!$132 FORMAT(27X,3(2X,A5,I7,7X))
!!$105 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,D14.7))
!!$101 FORMAT(1X,A5,2X,I5)
!!$103 FORMAT(27X,3(2X,A5,D14.7))
!!$
!!$  END SUBROUTINE extrusion
!!$!!!--------------------------------------------------------------------------------


END MODULE PREPRO

