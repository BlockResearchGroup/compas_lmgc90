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
module CHIC
!!****h* LMGC90.CHIC/CHIC
!! NAME
!!  module CHIC
!!****



  !      Declaration des variables CHIC
  !           ..., 25.06.93, 25.08.93, 13.04.94
  ! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa      
  ! -----Variables-CHIC---------------------------------------------------
  !
  !
  !      PARZZZ               nom du fichier de commandes
  !      DPLZZZ               nom du fichier de commandes deployees
  !
  !      NTZZZ                nombre total de commandes
  !      NTXZZZ               nombre maximum de commandes
  !      CHZZZ                nom de commande
  !
  !                           Commande de rang NZZZ
  !      CHPZZZ(NZZZ)         nom de la commande de rang NZZZ dans la
  !                           liste PARZZZ
  !      CHLZZZ(NZZZ)         nom de la commande de rang NZZZ dans la 
  !                           liste DPLZZZ, liste deployee
  !      CHSZZZ(NZZZ)         nom de la commande de rang NZZZ dans la 
  !                           liste des sous programmes de PARZZZ

  !      IRCZZZ(NZZZ)         numero de la boucle en cours contenant la 
  !                           commande de rang NZZZ
  !      IOCZZZ(NZZZ)         numero d'occurence de la 
  !                           commande de rang NZZZ
  !       
  !      NFXZZZ               nombre maximum de flags ou drapeaux par
  !                           commande 
  !      IFLZZZ(NZZZ,KFZZZ)   flag entier disponible no KFZZZ affecte a 
  !                           la commande de rang NZZZ
  !      DRPZZZ(NZZZ,KFZZZ)   drapeau reel disponible no KFZZZ affecte a 
  !                           la commande de rang NZZZ
  !
  !      NBPZZZ               nombre de boucles REPETE
  !      NBXZZZ               nombre maximum de boucles REPETE
  !      IRZZZ                numero de la boucle REPETE en cours
  !
  !                           Boucle REPETE numero IRZZZ:
  !      CKRZZZ(IRZZZ)        nom de l'etiquette de la boucle 
  !      NBKZZZ(IRZZZ)        rang de l'etiquette de reprise
  !      NRPZZZ(IRZZZ)        rang de la commande REPETE
  !      NRFZZZ(IRZZZ)        nombre de reprises de la boucle
  !      ICTZZZ(IRZZZ)        compteur degressif de la boucle
  !      ITOZZZ(IRZZZ)        totalisateur de la boucle (non utilise dans
  !                           cette routine)
  !
  !      KHOZZZ               si KHOZZZ=1 ECHO ON , sinon ECHO OFF
  !      IOKZZZ               valeur de FLAG
  !      JOKZZZ               occurence d'une commande
  !      ROKZZZ               valeur de DRAP
  !
  !      IFIZZZ               nombre de fichiers ouverts par CHIC
  !      IFXZZZ               nombre maximum de fichiers ouverts par CHIC
  !      FICZZZ(IFXZZZ)       tableau des noms de fichiers ouverts 
  !                           par CHIC
  !      AFDZZZ               adresse par defaut des fichiers ouverts
  !                           par CHIC
  !      NFDZZZ               nombre de caracteres de AFDZZZ
  !      TOCZZZ               adresse de fichier CHIC
  !      BLCZZZ               chaine de caractere composee de 72 blancs
  !      I9ZZZ                (=9999) symbole de fin de fichier CHIC
  !      UNZZZ                (=1.D0) symbole de fin de fichier CHIC
  !
  !      Les symboles de CHIC sont necessairement termines par ZZZ
  !      Les etiquettes de CHIC vont de 99001 a 99999

  ! 3456789012345678901234567890123456789012345678901234567890123456789012


  PARAMETER (NTXZZZ=1000,NBXZZZ=50,NFXZZZ=15,IFXZZZ=50)              
  CHARACTER*72 :: PARZZZ,DPLZZZ,AFDZZZ,TOCZZZ,BLCZZZ
  CHARACTER*30 :: CHZZZ
  CHARACTER*72,dimension(IFXZZZ) :: FICZZZ
  CHARACTER*30,dimension(NTXZZZ) :: CHPZZZ,CHLZZZ,CHSZZZ
  CHARACTER*30,dimension(NBXZZZ) :: CKRZZZ
  integer :: NZZZ,NTZZZ,NFDZZZ,IFIZZZ,I9ZZZ,NBPZZZ,JOKZZZ,IOKZZZ,KHOZZZ,IETZZZ
  integer,dimension(NTXZZZ) :: IRCZZZ,IOCZZZ
  integer,dimension(NTXZZZ,NFXZZZ) :: IFLZZZ
  integer,dimension(NBXZZZ) ::NBKZZZ,NRPZZZ,NRFZZZ,ICTZZZ,ITOZZZ   
  real(kind=8) :: ROKZZZ,UNZZZ
  real(kind=8),dimension(NTXZZZ,NFXZZZ) :: DRPZZZ


  ! -----Fin-de-variables-CHIC -------------------------------------------
  ! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz


contains

! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
!
!     Je le Machin
!
!     LMGC Montpellier
!
!     15.03.90, 01.04.90, 25.05.90, 21.08.90, 04.09.90,
!     17.11.90, 08.02.91, 13.05.91, 14.05.91  Reg Ren ,
!     17.05.91, 26.08.91, 27.09.91, 29.02.92, 26.03.92,
!     15.02.93, 03.05.93, 25.06.93, 15.09.93, 24.03.94,
!     14.04.94, 29.05.94
!
!     LMA ESM2 IMT Marseille
!
!     15.01.98, 19.01.98
!
! 3456789012345678901234567890123456789012345678901234567890123456789012


! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 
      SUBROUTINE CHIC_comp

      IMPLICIT none
      CHARACTER*5  :: CZZZ
      CHARACTER*30 :: CRZZZ,CKZZZ
      integer :: i,izzz,jzzz,mzzz
      integer :: ietzzz,mtzzz,mftzzz,iapzzz,mszzz,icher,nfzzz,joczzz,nrizzz,nbizzz   
! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa      




      IFIZZZ=10
      KHOZZZ=1
      DO 99001 I=1,72
99001 BLCZZZ(I:I)=' '
      I9ZZZ=9999
      UNZZZ=1.D0
                 
!     Lecture de PARZZZ
!     -----------------

! 34567..........123456789012345678901234567890.
    
      OPEN(1,FILE=PARZZZ)

      MZZZ=0
99002 MZZZ=MZZZ+1
      IF (MZZZ.GT.NTXZZZ) THEN
        WRITE(*,99505)NTXZZZ
99505   FORMAT(1X,' @ Nombre de commandes superieur a ',I4)
        STOP
      ENDIF
      READ(1,'(A30)')CHZZZ

!     manipulation pour enlever les blancs de tete
      IZZZ=0
99003 IF(IZZZ.LE.29) THEN
        IF(CHZZZ(IZZZ+1:IZZZ+1).EQ.' ') THEN
          IZZZ=IZZZ+1
          GOTO 99003
        ENDIF
      ENDIF
      DO 99004 JZZZ=1,30-IZZZ
99004 CHZZZ(JZZZ:JZZZ)=CHZZZ(JZZZ+IZZZ:JZZZ+IZZZ)
      DO 99005 JZZZ=30-IZZZ+1,30
99005 CHZZZ(JZZZ:JZZZ)=' '      
      CHPZZZ(MZZZ)=CHZZZ
! 34567.............123456789012345678901234567890.
      IF (CHZZZ.EQ.'FIN DU PROGRAMME              '.OR. &
         CHZZZ.EQ.'FIN_DU_PROGRAMME              ') THEN
        MTZZZ=MZZZ
      ELSE
        GOTO 99002
      ENDIF

      MZZZ=0      
99006 MZZZ=MZZZ+1
      IF (MZZZ.GT.NTXZZZ) THEN
        WRITE(*,99505)NTXZZZ
        STOP
      ENDIF
      READ(1,'(A30)')CHZZZ
!     manipulation pour enlever les blancs de tete
      IZZZ=0
99007 IF(IZZZ.LE.29) THEN
        IF (CHZZZ(IZZZ+1:IZZZ+1).EQ.' ') THEN
          IZZZ=IZZZ+1
          GOTO 99007
        ENDIF
      ENDIF
      DO 99008 JZZZ=1,30-IZZZ
99008 CHZZZ(JZZZ:JZZZ)=CHZZZ(JZZZ+IZZZ:JZZZ+IZZZ)
      DO 99009 JZZZ=30-IZZZ+1,30
99009 CHZZZ(JZZZ:JZZZ)=' '      
      CHSZZZ(MZZZ)=CHZZZ
! 34567.............123456789012345678901234567890.
      IF (CHZZZ.EQ.'FIN DU FICHIER                '.OR. &
     &    CHZZZ.EQ.'FIN_DU_FICHIER                ') THEN
        MFTZZZ=MZZZ
      ELSE
        GOTO 99006
      ENDIF
      
      CLOSE(1)

call  CHECK_CHIC_COMMAND

! 34567..........123456789012345678901234567890.
      WRITE(*,99010)PARZZZ
99010 FORMAT(1X,' @ Lecture OK de     ',A72) 


!     Identification des sous programmes et deploiement des commandes
!     ---------------------------------------------------------------

99012 MZZZ=0
      NZZZ=0
      IAPZZZ=0

99013 MZZZ=MZZZ+1
      NZZZ=NZZZ+1
      IF (NZZZ.GT.NTXZZZ) THEN
        WRITE(*,99505)NTXZZZ
        STOP
      ENDIF
      CHZZZ=CHPZZZ(MZZZ)
      CHLZZZ(NZZZ)=CHZZZ
      CZZZ=CHZZZ(1:5)
      IF (CZZZ.EQ.'APPEL') THEN
        IAPZZZ=1
        CRZZZ(1:4)='SUB '
        CRZZZ(5:26)=CHZZZ(9:30)
  CRZZZ(27:30)='   '
        CHLZZZ(NZZZ)=CRZZZ
        DO 99014 MSZZZ=1,MFTZZZ
99014   IF (CHSZZZ(MSZZZ).EQ.CRZZZ) GOTO 99015
        WRITE(*,99016)CRZZZ
99016   FORMAT(1X,' @ ',A30,' introuvable')
        STOP
99015   NZZZ=NZZZ+1
        MSZZZ=MSZZZ+1
        IF (MSZZZ.GE.MFTZZZ) THEN
! 34567...               123456789012345678901234567890.
          WRITE(*,99016)'RETOUR                        '
          STOP
        ENDIF
! 34567...                    123456789012345678901234567890.
        IF (CHSZZZ(MSZZZ).EQ.'RETOUR                        ') THEN
! 34567...              123456789012345678901234567890.
          CHLZZZ(NZZZ)='RETOUR                        '
          GOTO 99013
        ELSE
          CHLZZZ(NZZZ)=CHSZZZ(MSZZZ)
           GOTO 99015
        ENDIF
      ENDIF
      IF (CHZZZ.NE.'FIN DU PROGRAMME              ') GOTO 99013      
      NTZZZ=NZZZ
      
      IF (IAPZZZ.EQ.1) THEN
        DO 99018 IZZZ=1,NZZZ
99018   CHPZZZ(IZZZ)=CHLZZZ(IZZZ)
        MTZZZ=NZZZ
        GOTO 99012
      ENDIF      

      OPEN(1,FILE=DPLZZZ,STATUS='UNKNOWN')
      DO 99077 IZZZ=1,NTZZZ
99077 WRITE(1,'(A30)')CHLZZZ(IZZZ)      
      CLOSE(1) 
      
!     Identification du numero d'occurence d'une commande
!     ---------------------------------------------------

      DO 99042 NZZZ=2,NTZZZ
      CHZZZ=CHLZZZ(NZZZ)
      IOCZZZ(NZZZ-1)=0
      IF (CHZZZ(1:9).EQ.'OCCURENCE') THEN
        CHZZZ(1:9)=     '         '
        !  READ(CHZZZ,'(I30)')IOCZZZ(NZZZ-1)
        READ(CHZZZ,*)IOCZZZ(NZZZ-1)
      ENDIF       
99042 CONTINUE

!     Identification des commandes
!     REPETE n FOIS
!     DEPUIS abcd efgh
!     DEPUIS OCCURENCE m
!     ----------------------------
            
      NZZZ=0
      NBPZZZ=0

99020 NZZZ=NZZZ+1
      CHZZZ=CHLZZZ(NZZZ)
      
      IF (CHZZZ(1:6).EQ.'REPETE') THEN
        NBPZZZ=NBPZZZ+1
        IF (NBPZZZ.GT.NBXZZZ) THEN
          WRITE(*,99507)NBXZZZ
99507     FORMAT(1X,' @ Nombre de commandes REPETE superieur a ',I4)
          STOP
        ENDIF
        CHZZZ(1:24)=CHZZZ(7:30)
        CHZZZ(25:30)='      '
        ICHER=0
        I=0
        DO 99908 WHILE (ICHER.EQ.0)
        I=I+1
        IF (CHZZZ(I:I+3).EQ.'FOIS') THEN
           ICHER=1
           CHZZZ(I:I+3)='    '
        ENDIF
99908   CONTINUE
!        READ(CHZZZ,'(I30)')NFZZZ
        READ(CHZZZ,*)NFZZZ
        CKZZZ=CHLZZZ(NZZZ+1)
        CKZZZ(1:23)=CKZZZ(8:30)
        CKZZZ(24:30)='       '
        CRZZZ=CHLZZZ(NZZZ+2)
        IF (CRZZZ(1:16).EQ.'DEPUIS OCCURENCE') THEN
          CRZZZ(1:16)=     '                '
!         READ(CRZZZ,'(I30)')JOCZZZ
          READ(CRZZZ,*)JOCZZZ
        ELSE
          JOCZZZ=0
        ENDIF
        CKRZZZ(NBPZZZ)=CKZZZ
        NRFZZZ(NBPZZZ)=NFZZZ
        ICTZZZ(NBPZZZ)=NFZZZ
        NRPZZZ(NBPZZZ)=NZZZ
        IZZZ=NZZZ
99021   IZZZ=IZZZ-1
        IF (IZZZ.LE.0) THEN
          WRITE(*,99022)CKZZZ
99022     FORMAT(1X,' @ ',A30,' introuvable')
          STOP
        ENDIF
        IF (CHLZZZ(IZZZ).NE.CKZZZ.OR.IOCZZZ(IZZZ).NE.JOCZZZ) GOTO 99021
        NBKZZZ(NBPZZZ)=IZZZ
        GOTO 99020
      ENDIF
      
   
      IF (NZZZ.LE.NTZZZ) GOTO 99020
! 34567..........123456789012345678901234567890.


!     Identification du numero de la boucle en cours
!     contenant la commande de rang NZZZ
!     ----------------------------------------------


      DO 99052 NZZZ=1,NTZZZ
      NRIZZZ=NTZZZ
      NBIZZZ=1
      IRCZZZ(NZZZ)=1
        DO 99051 IZZZ=1,NBPZZZ
          IF (NRPZZZ(IZZZ).GE.NZZZ.AND.NBKZZZ(IZZZ).LE.NZZZ) THEN
          IF (NRPZZZ(IZZZ).LT.NRIZZZ.AND.NBKZZZ(IZZZ).GT.NBIZZZ) THEN
            NRIZZZ=NRPZZZ(IZZZ)
            NBIZZZ=NBKZZZ(IZZZ)
            IRCZZZ(NZZZ)=IZZZ
          ENDIF
          ENDIF
99051   CONTINUE
99052 CONTINUE

!     Fin
!     ---
       
      WRITE(*,99023)PARZZZ
99023 FORMAT(1X,' @ Compilation OK de ',A72)      
      GOTO 99997
            
      
      
99997 RETURN
      END SUBROUTINE CHIC_comp

! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
      SUBROUTINE REPETE
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ


      
! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


! 23456789012345678901234567890:
!                              :
!     REPETE n FOIS            :
!     DEPUIS abcd efgh         :
!     DEPUIS OCCURENCE m       :

! 34567

        IMPLICIT none
        integer :: irzzz

        do IRZZZ=1,NBPZZZ
           IF (NRPZZZ(IRZZZ).EQ.NZZZ) THEN
              IF (ICTZZZ(IRZZZ).GT.0) THEN
                 NZZZ=NBKZZZ(IRZZZ)-1
                 ICTZZZ(IRZZZ)=ICTZZZ(IRZZZ)-1
                 ITOZZZ(IRZZZ)=ITOZZZ(IRZZZ)+1
                 exit
              ELSE
                 ICTZZZ(IRZZZ)=NRFZZZ(IRZZZ)
                 exit
              ENDIF
           ENDIF
        end do

      END SUBROUTINE REPETE


! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
      SUBROUTINE STOPREP
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ


! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


! 23456789012345678901234567890:
!                              :
!      STOP REPETE SI IOK = k  :
!                              :

! 34567
      
      IMPLICIT none
      integer :: izzz,irzzz
      
      CHZZZ=CHLZZZ(NZZZ)
      CHZZZ(1:20)='                    '
!     READ(CHZZZ,'(I30)')JOKZZZ
      READ(CHZZZ,*)JOKZZZ
      IF (IOKZZZ.EQ.JOKZZZ) THEN
        IZZZ=NZZZ
99922    IZZZ=IZZZ+1
        IF (IZZZ.GT.NTZZZ) GOTO 99901
        IF (CHLZZZ(IZZZ)(1:6).NE.'REPETE') &
     &  GOTO 99922
        DO 99924 IRZZZ=1,NBPZZZ
        IF (NRPZZZ(IRZZZ).EQ.IZZZ) THEN
          ICTZZZ(IRZZZ)=0
          NZZZ=IZZZ-1
          GOTO 99901
        ENDIF
99924   CONTINUE    
      ENDIF

99901 RETURN
      END SUBROUTINE STOPREP


! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ      
      SUBROUTINE IOKEGAL
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ


! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


! 23456789012345678901234567890:

!                              :
!     IOK = ?                  :
!     ...
!                              :
!     IOK = k                  :
!     ...
!                              :
!     IOK = FLAG f             :
!     INTO abcd efgh           :
!     INTO OCCURENCE m         :


! 34567
       
      IMPLICIT none
      integer :: kfzzz,joczzz,mzzz
      CHARACTER*30 :: chhzzz
      
      CHZZZ=CHLZZZ(NZZZ)
      CHZZZ(1:25)=CHZZZ(6:30)
      CHZZZ(26:30)='     '
      IF (CHZZZ(1:5).EQ.' FLAG') THEN
        CHZZZ(1:25)=CHZZZ(6:30)
        CHZZZ(26:30)='     '
        !  READ(CHZZZ,'(I30)')KFZZZ
        READ(CHZZZ,*)KFZZZ
        IF (KFZZZ.GT.NFXZZZ) THEN
          WRITE(*,99912)KFZZZ,NFXZZZ
99912     FORMAT(1X,' @ numero de FLAG ',I12,' > ',I2,' maximum autorise')
          STOP
        ENDIF
        CHZZZ=CHLZZZ(NZZZ+1)
        CHZZZ(1:25)=CHZZZ(6:30)
        CHZZZ(26:30)='     '
        CHHZZZ=CHLZZZ(NZZZ+2)
        IF (CHHZZZ(1:14).EQ.'INTO OCCURENCE') THEN
          CHHZZZ(1:14)=     '              '
!         READ(CHHZZZ,'(I30)')JOCZZZ
          READ(CHHZZZ,*)JOCZZZ
        ELSE
          JOCZZZ=0
        ENDIF
!       identification du rang de la commande CHZZZ d'occurence JOCZZZ
        DO 99922 MZZZ=1,NTZZZ
        IF (CHZZZ.EQ.CHLZZZ(MZZZ).AND.IOCZZZ(MZZZ).EQ.JOCZZZ) THEN
          GOTO 99924
        ENDIF
99922   CONTINUE
99924   IOKZZZ=IFLZZZ(MZZZ,KFZZZ)
        IF (KHOZZZ.EQ.1) WRITE(*,99926)IOKZZZ,KFZZZ,CHZZZ
99926   FORMAT(1X,' @ IOK = ',I12,' = FLAG',I2,/,1X,' @ INTO ',A30)
        IF (JOCZZZ.NE.0) THEN
          IF (KHOZZZ.EQ.1) WRITE(*,99928)JOCZZZ
        ENDIF
99928   FORMAT(1X,' @ INTO OCCURENCE ',I2)
      ELSE IF (CHZZZ(1:2).EQ.' ?') THEN
        WRITE(*,'(A30)')'  @ IOK = ?'
        !  READ(*,'(I30)')IOKZZZ
        READ(*,*)IOKZZZ
        WRITE(*,99929)IOKZZZ
99929   FORMAT(1X,' @ IOK = ',I12)      
      ELSE
!        READ(CHZZZ,'(I30)')IOKZZZ
        READ(CHZZZ,*)IOKZZZ
        IF (KHOZZZ.EQ.1)WRITE(*,99929)IOKZZZ
      ENDIF
      
      RETURN       
      END SUBROUTINE IOKEGAL
      
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ      
      SUBROUTINE FLAGEGAL
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ


! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


! 23456789012345678901234567890:

!     FLAG f = IOK             :
!     INTO abcd efgh           :
!     INTO OCCURENCE m         :


! 34567
       
      IMPLICIT none
      integer :: kfzzz,icher,i,joczzz,mzzz
      character*30 :: chhzzz
      
      CHZZZ=CHLZZZ(NZZZ)
      CHZZZ(1:26)=CHZZZ(5:30)
      ICHER=0
      I=0
      DO 99908 WHILE (ICHER.EQ.0)
      I=I+1
      IF (CHZZZ(I:I+4).EQ.'= IOK') THEN
         ICHER=1
         CHZZZ(I:I+4)='     '
      ENDIF
99908 CONTINUE
!      READ(CHZZZ,'(I30)')KFZZZ
      READ(CHZZZ,*)KFZZZ
         IF (KFZZZ.GT.NFXZZZ) THEN
            WRITE(*,99912)KFZZZ,NFXZZZ
99912 FORMAT(1X,' @ numero de FLAG ',I12,' > ',I2,' maximum autorise')
            STOP
         ENDIF
      CHZZZ=CHLZZZ(NZZZ+1)
      CHZZZ(1:25)=CHZZZ(6:30)
      CHZZZ(26:30)='     '
      CHHZZZ=CHLZZZ(NZZZ+2)
      IF (CHHZZZ(1:14).EQ.'INTO OCCURENCE') THEN
        CHHZZZ(1:14)=     '              '
        !  READ(CHHZZZ,'(I30)')JOCZZZ
        READ(CHHZZZ,*)JOCZZZ
      ELSE
        JOCZZZ=0
      ENDIF
!     identification du rang de la commande CHZZZ d'occurence JOCZZZ
      DO 99922 MZZZ=1,NTZZZ
        IF (CHZZZ.EQ.CHLZZZ(MZZZ).AND.IOCZZZ(MZZZ).EQ.JOCZZZ) THEN
          GOTO 99924
        ENDIF
99922 CONTINUE
99924 IFLZZZ(MZZZ,KFZZZ)=IOKZZZ
      IF (KHOZZZ.EQ.1) WRITE(*,99926)KFZZZ,IOKZZZ,CHZZZ
99926 FORMAT(1X,' @ FLAG',I2,' = IOK = ',I12,/,1X,' @ INTO ',A30)
      IF (JOCZZZ.NE.0) THEN
          IF (KHOZZZ.EQ.1) WRITE(*,99928)JOCZZZ
      ENDIF
99928 FORMAT(1X,' @ INTO OCCURENCE ',I2)

      
      RETURN
      END  SUBROUTINE FLAGEGAL
      
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ      
      SUBROUTINE ROKEGAL
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ


! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


! 23456789012345678901234567890:

!                              :
!     ROK = ?                  :
!     ...
!                              :
!     ROK = r                  :
!     ...
!                              :
!     ROK = DRAP f             :
!     INTO abcd efgh           :
!     INTO OCCURENCE m         :


! 34567
       
      IMPLICIT none
      character*30 :: CHHZZZ 
      integer ::  icher,i,kfzzz,joczzz,mzzz
      
      CHZZZ=CHLZZZ(NZZZ)
      CHZZZ(1:25)=CHZZZ(6:30)
      CHZZZ(26:30)='     '
      IF (CHZZZ(1:5).EQ.' DRAP') THEN
          CHZZZ(1:25)=CHZZZ(6:30)
          CHZZZ(26:30)='     '
          !  READ(CHZZZ,'(I30)')KFZZZ
          READ(CHZZZ,*)KFZZZ
          IF (KFZZZ.GT.NFXZZZ) THEN
            WRITE(*,99912)KFZZZ,NFXZZZ
99912       FORMAT(1X,' @ numero de DRAP ',I12,' > ',I2,' maximum autorise')
            STOP
          ENDIF
          CHZZZ=CHLZZZ(NZZZ+1)
          CHZZZ(1:25)=CHZZZ(6:30)
          CHZZZ(26:30)='     '
          CHHZZZ=CHLZZZ(NZZZ+2)
          IF (CHHZZZ(1:14).EQ.'INTO OCCURENCE') THEN
            CHHZZZ(1:14)=      '              '
!           READ(CHHZZZ,'(I30)')JOCZZZ
            READ(CHHZZZ,*)JOCZZZ
          ELSE
            JOCZZZ=0
          ENDIF
!       identification du rang de la commande CHZZZ d'occurence JOCZZZ
          DO 99922 MZZZ=1,NTZZZ
            IF (CHZZZ.EQ.CHLZZZ(MZZZ).AND.IOCZZZ(MZZZ).EQ.JOCZZZ) THEN
              GOTO 99924
            ENDIF
99922     CONTINUE
99924     ROKZZZ=DRPZZZ(MZZZ,KFZZZ)
          IF (KHOZZZ.EQ.1) WRITE(*,99926)ROKZZZ,KFZZZ,CHZZZ
99926     FORMAT(1X,' @ ROK = ',D12.5,' = DRAP',I2,/,1X,' @ INTO ',A30)
          IF (JOCZZZ.NE.0) THEN
            IF (KHOZZZ.EQ.1)WRITE(*,99928)JOCZZZ
          ENDIF
99928     FORMAT(1X,' @ INTO OCCURENCE ',I2)
        ELSE IF (CHZZZ(1:2).EQ.' ?') THEN
          WRITE(*,'(A30)')' @ ROK = ?'
!         READ(*,'(F30.30)')ROKZZZ
          READ(*,*)ROKZZZ
          WRITE(*,99929)ROKZZZ
99929     FORMAT(1X,' @ ROK = ',D12.5)      
        ELSE
!          READ(CHZZZ,'(F30.30)')ROKZZZ
           READ(CHZZZ,*)ROKZZZ
           IF (KHOZZZ.EQ.1) WRITE(*,99929)ROKZZZ
        ENDIF
      
      RETURN
      END       SUBROUTINE ROKEGAL
      
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ      
      SUBROUTINE DRAPEGAL
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ


! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


! 23456789012345678901234567890:

!     DRAP f = ROK             :
!     INTO abcd efgh           :
!     INTO OCCURENCE m         :


! 34567
       
      IMPLICIT none
      integer ::icher,i,kfzzz,joczzz,mzzz
      character*30 :: CHHZZZ     

      CHZZZ=CHLZZZ(NZZZ)
      CHZZZ(1:26)=CHZZZ(5:30)
      ICHER=0
      I=0
      DO 99908 WHILE (ICHER.EQ.0)
      I=I+1
      IF (CHZZZ(I:I+4).EQ.'= ROK') THEN
         ICHER=1
         CHZZZ(I:I+4)='     '
      ENDIF
99908 CONTINUE
!      READ(CHZZZ,'(I30)')KFZZZ
      READ(CHZZZ,*)KFZZZ
         IF (KFZZZ.GT.NFXZZZ) THEN
            WRITE(*,99912)KFZZZ,NFXZZZ
99912 FORMAT(1X,' @ numero de DRAP ',I12,' > ',I2,' maximum autorise')
            STOP
         ENDIF
      CHZZZ=CHLZZZ(NZZZ+1)
      CHZZZ(1:25)=CHZZZ(6:30)
      CHZZZ(26:30)='     '
      CHHZZZ=CHLZZZ(NZZZ+2)
      IF (CHHZZZ(1:14).EQ.'INTO OCCURENCE') THEN
        CHHZZZ(1:14)=      '              '
!  READ(CHHZZZ,'(I30)')JOCZZZ
        READ(CHHZZZ,*)JOCZZZ
      ELSE
        JOCZZZ=0
      ENDIF
!     identification du rang de la commande CHZZZ d'occurence JOCZZZ
      DO 99922 MZZZ=1,NTZZZ
        IF (CHZZZ.EQ.CHLZZZ(MZZZ).AND.IOCZZZ(MZZZ).EQ.JOCZZZ) THEN
      GOTO 99924
      ENDIF
99922 CONTINUE
99924 DRPZZZ(MZZZ,KFZZZ)=ROKZZZ
      IF (KHOZZZ.EQ.1) WRITE(*,99926)KFZZZ,ROKZZZ,CHZZZ
99926 FORMAT(1X,' @ DRAP',I2,' = ROK = ',D12.5,/,1X,' @ INTO ',A30)
      IF (JOCZZZ.NE.0) THEN
          IF (KHOZZZ.EQ.1) WRITE(*,99928)JOCZZZ
      ENDIF
99928 FORMAT(1X,' @ INTO OCCURENCE ',I2)

      
      RETURN
      END SUBROUTINE DRAPEGAL
      
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
      SUBROUTINE CAPSUR
! 23456789012345678901234567890:
!                              :
!     CAP SI IOK = k           :
!     SUR abcd efgh            :
!     SUR OCCURENCE m          :
!                              :

! 34567
! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA      
      IMPLICIT none
      integer :: joczzz,mzzz,izzz  
      character*30 :: CHHZZZ  
      
      CHZZZ=CHLZZZ(NZZZ)
      CHZZZ(1:18)=CHZZZ(13:30)
      CHZZZ(19:30)='            '
!      READ(CHZZZ,'(I30)')JOKZZZ
      READ(CHZZZ,*)JOKZZZ
      IF (IOKZZZ.EQ.JOKZZZ) THEN
        CHZZZ=CHLZZZ(NZZZ+1)
        CHZZZ(1:26)=CHZZZ(5:30)
        CHZZZ(27:30)='    '
        CHHZZZ=CHLZZZ(NZZZ+2)
       IF (CHHZZZ(1:13).EQ.'SUR OCCURENCE') THEN
       CHHZZZ(1:13)='             '
!  READ(CHZZZ,'(I30)')JOCZZZ
         READ(CHZZZ,*)JOCZZZ
        ELSE
          JOCZZZ=0
       ENDIF
!       identification du rang de la commande CHZZZ d'occurence JOCZZZ
        DO 99922 MZZZ=1,NTZZZ
          IF (CHZZZ.EQ.CHLZZZ(MZZZ).AND.IOCZZZ(MZZZ).EQ.JOCZZZ) THEN
            GOTO 99924
          ENDIF
99922   CONTINUE
!       mise a zero des compteurs de boucles
99924   DO 99045 IZZZ=1,NBPZZZ
        IF (NRPZZZ(IZZZ).GE.NZZZ.AND.NBKZZZ(IZZZ).LE.NZZZ) THEN
        IF (NRPZZZ(IZZZ).LT.MZZZ.OR.NBKZZZ(IZZZ).GT.MZZZ) THEN
        ICTZZZ(IZZZ)=NRFZZZ(IZZZ)
        ENDIF
       ENDIF
99045  CONTINUE
        NZZZ=MZZZ-1
      ENDIF
      
      RETURN
      END       SUBROUTINE CAPSUR


! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
      SUBROUTINE CHIC_comd
! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
!     Commandes CHIC
!
!     Je le Machin
!
!     LMGC Montpellier
!
!     15.03.90, 01.04.90,..., 
!     18.03.92, 23.03.92, 28.12.92, 15.02.93, 25.08.93,
!     15.09.93, 24.03.94, 14.04.94
!
!     LMA ESM2 IMT Marseille
!
!     15.01.98, 19.01.98
!
! 3456789012345678901234567890123456789012345678901234567890123456789012
! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      IMPLICIT none
      integer :: ifzzz,leczzz,icher,i,ipas,irzzz, &
                npas,ncpt,nfich,lutil,mzzz,i3,iszzz     
      real(kind=8) ::  tps,uti1,uti2,uti3
! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa       


! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:5).EQ.'PAUSE') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A8)')' @ PAUSE'
      PAUSE     
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'ECHO ON                       ') THEN
      KHOZZZ=1
      IETZZZ=1
      RETURN
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'ECHO OFF                      ') THEN
      KHOZZZ=0
      IETZZZ=1
      RETURN
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:1).EQ.'.') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(4X,A30)')CHZZZ
      IETZZZ=1
      RETURN     
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:5).EQ.'IOK =') THEN
      CALL IOKEGAL
      IETZZZ=1
      RETURN     
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:4).EQ.'FLAG') THEN
      CALL FLAGEGAL
      IETZZZ=1
      RETURN     
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:5).EQ.'ROK =') THEN
      CALL ROKEGAL
      IETZZZ=1
      RETURN     
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:4).EQ.'DRAP') THEN
      CALL DRAPEGAL
      IETZZZ=1
      RETURN     
      ENDIF      
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:6).EQ.'REPETE') THEN
      CALL REPETE
      IETZZZ=1
      RETURN     
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:20).EQ.'STOP REPETE SI IOK =') THEN
      CALL STOPREP
      IETZZZ=1
      RETURN
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:12).EQ.'CAP SI IOK =') THEN
      CALL CAPSUR
      IETZZZ=1
      RETURN
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:6).EQ.'OUVRE ') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHZZZ
      IFIZZZ=IFIZZZ+1
      FICZZZ(IFIZZZ)=BLCZZZ
      FICZZZ(IFIZZZ)(1:NFDZZZ)=AFDZZZ(1:NFDZZZ)
      FICZZZ(IFIZZZ)(NFDZZZ+1:NFDZZZ+24)=CHZZZ(7:30)
      ENDIF
      IF (CHZZZ(1:6).EQ.'OUVRE?') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHZZZ
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+1)
      IFIZZZ=IFIZZZ+1
      FICZZZ(IFIZZZ)=BLCZZZ
      FICZZZ(IFIZZZ)(1:30)=CHLZZZ(NZZZ+1)
      ENDIF
      IF (CHZZZ(1:5).EQ.'OUVRE') THEN
      OPEN(IFIZZZ,FILE=FICZZZ(IFIZZZ),STATUS='UNKNOWN')
      WRITE(IFIZZZ,'(A72)')FICZZZ(IFIZZZ)
      IETZZZ=1
      RETURN
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:6).EQ.'FERME ') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHZZZ
      TOCZZZ=BLCZZZ
      TOCZZZ(1:NFDZZZ)=AFDZZZ(1:NFDZZZ)
      TOCZZZ(NFDZZZ+1:NFDZZZ+24)=CHZZZ(7:30)
      ENDIF
      IF (CHZZZ(1:6).EQ.'FERME?') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHZZZ
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+1)
      TOCZZZ=BLCZZZ
      TOCZZZ(1:30)=CHLZZZ(NZZZ+1) 
      ENDIF
      IF (CHZZZ(1:5).EQ.'FERME') THEN
      IFZZZ=0
      LECZZZ=0
      DO WHILE (LECZZZ.EQ.0)
      IFZZZ=IFZZZ+1
      IF (IFZZZ.GT.IFIZZZ) THEN
      WRITE(*,'(1X,A71)')TOCZZZ(1:71)
      WRITE(*,'(A14)')' introuvable !'
      STOP
      ENDIF
      IF (TOCZZZ.EQ.FICZZZ(IFZZZ)) LECZZZ=1
      END DO
      WRITE(IFZZZ,'(A14)')'FIN_DU_FICHIER'
      WRITE(IFZZZ,'(I5,13X,D14.7)')I9ZZZ,UNZZZ
      CLOSE(IFZZZ)
      IETZZZ=1
      RETURN
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012
      IF (CHZZZ.EQ.'ECRIT DRAP TS LES             '.OR. &
          CHZZZ.EQ.'ECRIT JPBDRAP.RES TS LES      ') THEN     
      CHZZZ=CHLZZZ(NZZZ+1)
        ICHER=0
        I=0
        DO WHILE (ICHER.EQ.0)
        I=I+1
        IF (CHZZZ(I:I+2).EQ.'PAS') THEN
           ICHER=1
           CHZZZ(I:I+2)='   '
        ENDIF
        END DO
!      READ(CHZZZ,'(I30)')IPAS
      READ(CHZZZ,*)IPAS
      IRZZZ=IRCZZZ(NZZZ)      
      NPAS=NRFZZZ(IRZZZ)-ICTZZZ(IRZZZ)
      NCPT=IDINT(DBLE(NPAS+1)/DBLE(IPAS))
      IF ((NCPT*IPAS).NE.(NPAS+1)) THEN
        IETZZZ=1
        RETURN
      ENDIF
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A20)')' @ ECRIT JPBDRAP.RES'
      TOCZZZ=BLCZZZ
      TOCZZZ(1:NFDZZZ)=AFDZZZ(1:NFDZZZ)
      TOCZZZ(NFDZZZ+1:NFDZZZ+11)='JPBDRAP.RES'
      IFZZZ=0
      LECZZZ=0
      DO WHILE (LECZZZ.EQ.0)
      IFZZZ=IFZZZ+1
      IF (IFZZZ.GT.IFIZZZ) THEN
      WRITE(*,'(1X,A71)')TOCZZZ(1:71)
      WRITE(*,'(A14)')' introuvable !'
      STOP
      ENDIF
      IF (TOCZZZ.EQ.FICZZZ(IFZZZ)) LECZZZ=1
      END DO
      NFICH=IFZZZ
      LUTIL=IFLZZZ(NZZZ,1)
      LECZZZ=0
      MZZZ=0
      DO WHILE (LECZZZ.EQ.0)
      MZZZ=MZZZ+1
      IF (CHLZZZ(MZZZ)(1:5).EQ.'TEMPS') LECZZZ=1
      IF (MZZZ.GT.NTZZZ) THEN
      WRITE(*,'(1X,A5)')'TEMPS'
      WRITE(*,'(A14)')' introuvable !'
      STOP
      ENDIF
      END DO
      TPS=DRPZZZ(MZZZ,1)                                        
      WRITE(NFICH,'(A72)')'NPASx__________TEMPS_________LUTIL___________ &
     &____________________________'
      WRITE(NFICH,'(I5,9X,D12.5,3X,I5)')NPAS,TPS,LUTIL
      WRITE(NFICH,'(A72)')'N_util_________UTI1__________UTI2__________UT &
     &I3__________________________'
      DO I=1,LUTIL
        I3=3*(I-1)
        UTI1=DRPZZZ(NZZZ,I3+1)
        UTI2=DRPZZZ(NZZZ,I3+2)
        UTI3=DRPZZZ(NZZZ,I3+3)        
      WRITE(NFICH,'(I5,9X,D12.5,2X,D12.5,2X,D12.5)') &
       I,UTI1,UTI2,UTI3
      END DO
      IETZZZ=1
      RETURN
      ENDIF
    
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'AFFICHE COMPTEURS            ') THEN
      WRITE(*,'(A1)')' '
      DO IRZZZ=1,NBPZZZ
      ISZZZ=NRFZZZ(IRZZZ)-ICTZZZ(IRZZZ)
      WRITE(*,'(1X,A12,A30,A2,I9)')'   Cpt bcle ',CKRZZZ(IRZZZ),' =',ISZZZ
      MZZZ=NBKZZZ(IRZZZ)
      IF (IOCZZZ(MZZZ).NE.0) THEN
      WRITE(*,'(1X,A22,I2)')'            OCCURENCE ',IOCZZZ(MZZZ)
      ENDIF
      END DO
      WRITE(*,'(A1)')' '
      IETZZZ=1
      RETURN
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'ECRIT COMPTEURS              ') THEN
      IFZZZ=0
      LECZZZ=0
      TOCZZZ=BLCZZZ
      TOCZZZ(1:NFDZZZ)=AFDZZZ(1:NFDZZZ)
      TOCZZZ(NFDZZZ+1:NFDZZZ+12)='JPBITER.RES'
      DO WHILE (LECZZZ.EQ.0)
      IFZZZ=IFZZZ+1
      IF (IFZZZ.GT.IFIZZZ) THEN
      WRITE(*,'(1X,A71)')TOCZZZ(1:71)
      WRITE(*,'(A14)')' introuvable !'
      STOP
      ENDIF
      IF (TOCZZZ.EQ.FICZZZ(IFZZZ)) LECZZZ=1
      END DO
      NFICH=IFZZZ
      WRITE(NFICH,'(A1)')' '
      DO IRZZZ=1,NBPZZZ
      ISZZZ=NRFZZZ(IRZZZ)-ICTZZZ(IRZZZ)
      WRITE(NFICH,'(A12,A30,A2,I9)')'   Cpt bcle ',CKRZZZ(IRZZZ),' =',ISZZZ
      MZZZ=NBKZZZ(IRZZZ)
      IF (IOCZZZ(MZZZ).NE.0) THEN
      WRITE(NFICH,'(A22,I2)')'            OCCURENCE ',IOCZZZ(MZZZ)
      ENDIF
      END DO
      WRITE(NFICH,'(A1)')' '
      IETZZZ=1
      RETURN
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'TOTALISATEURS A ZERO          ') THEN
      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHZZZ
      DO IRZZZ=1,NBPZZZ
      ITOZZZ(IRZZZ)=0
      END DO
      IETZZZ=1
      RETURN
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'AFFICHE TOTALISATEURS         ') THEN
      WRITE(*,'(A1)')' '
      DO IRZZZ=1,NBPZZZ
      ISZZZ=ITOZZZ(IRZZZ)
      WRITE(*,'(1X,A12,A30,A2,I9)')'   Tot bcle ',CKRZZZ(IRZZZ),' =',ISZZZ
      MZZZ=NBKZZZ(IRZZZ)
      IF (IOCZZZ(MZZZ).NE.0) THEN
      WRITE(*,'(1X,A22,I2)')'            OCCURENCE ',IOCZZZ(MZZZ)
      ENDIF
      END DO
      WRITE(*,'(A1)')' '
      IETZZZ=1
      RETURN
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ.EQ.'ECRIT TOTALISATEURS           ') THEN
      IFZZZ=0
      LECZZZ=0
      TOCZZZ=BLCZZZ
      TOCZZZ(1:NFDZZZ)=AFDZZZ(1:NFDZZZ)
      TOCZZZ(NFDZZZ+1:NFDZZZ+12)='JPBITER.RES'
      DO WHILE (LECZZZ.EQ.0)
      IFZZZ=IFZZZ+1
      IF (IFZZZ.GT.IFIZZZ) THEN
      WRITE(*,'(1X,A71)')TOCZZZ(1:71)
      WRITE(*,'(A14)')' introuvable !'
      STOP
      ENDIF
      IF (TOCZZZ.EQ.FICZZZ(IFZZZ)) LECZZZ=1
      END DO
      NFICH=IFZZZ
      WRITE(NFICH,'(A1)')' '
      DO IRZZZ=1,NBPZZZ
      ISZZZ=ITOZZZ(IRZZZ)
      WRITE(NFICH,'(A12,A30,A2,I9)')'   Tot bcle ',CKRZZZ(IRZZZ),' =',ISZZZ
      MZZZ=NBKZZZ(IRZZZ)
      IF (IOCZZZ(MZZZ).NE.0) THEN
      WRITE(NFICH,'(A22,I2)')'            OCCURENCE ',IOCZZZ(MZZZ)
      ENDIF
      END DO
      WRITE(NFICH,'(A1)')' '
      IETZZZ=1
      RETURN
      ENDIF      
      


! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

      END  SUBROUTINE CHIC_comd   

! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

!
!     Je le Machin
!
!     LMGC Montpellier
!
!     15.03.90, 01.04.90, 25.05.90, 17.10.90, 01.11.90,
!     17.11.90, 17.12.90, 06.03.91, 17.04.91, 30.04.91,
!     12.05.91, 18.95.91, 28.05.91, 30.05.91, 06.05.91,
!     12.06.91, 13.06.91, 08.07.91, 26.08.91, 10.09.91,
!     13.09.91, 17.09.91, 27.09.91, 08.01.92, 27.01.92,
!     07.02.92, 21.02.92, 18.03.92, 23.03.92, 09.11.92,
!     10.12.92, 25.08.93, 24.03.94, 29.05.94, 17.01.95
!
!     LMA ESM2 IMT Marseille
!
!     19.01.98
!
! 3456789012345678901234567890123456789012345678901234567890123456789012


! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa


!     Initialisations

      SUBROUTINE CHIC_ini

      IMPLICIT none

! aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa       



!     Chargement des Drapeaux et Flags
!     --------------------------------

      WRITE(*,'(A36)') '  @                                 '
      WRITE(*,'(A36)') '  @ Chargement des Drapeaux et Flags'
      WRITE(*,'(A36)') '  @                                 '

      NZZZ=0
      do 
      NZZZ=NZZZ+1
      IF (NZZZ.GT.NTZZZ) exit
      CHZZZ=CHLZZZ(NZZZ)


! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:5).EQ.'IOK =') THEN
      CALL IOKEGAL
      cycle     
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:4).EQ.'FLAG') THEN
      CALL FLAGEGAL
      cycle     
      ENDIF

! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:5).EQ.'ROK =') THEN
      CALL ROKEGAL
      cycle     
      ENDIF
      
! 3456789012345678901234567890123456789012345678901234567890123456789012      
      IF (CHZZZ(1:4).EQ.'DRAP') THEN
      CALL DRAPEGAL
      cycle     
      ENDIF    

      exit
      
     end do


      WRITE(*,'(A36)') '  @                                 '
      WRITE(*,'(A36)') '  @ Chargement des Drapeaux et Flags'
      WRITE(*,'(A36)') '  @                                 '


! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

      RETURN
      END SUBROUTINE CHIC_ini

! zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz

!!
! Ajout par Fred pour l'affichage des messages 
!!

  subroutine LOGCHIC(MESSAGE)

    implicit none
      
    character(len=*),optional  :: MESSAGE
    character(len=80)          :: cout

    if (KHOZZZ.EQ.0) RETURN

    write(cout(1:35),'(1x,A3,A30,1x)') ' @ ',CHZZZ

    if (present(MESSAGE)) write(cout(36:80),*) MESSAGE(1:len_trim(MESSAGE))

    write(*,'(A80)') cout

  end subroutine LOGCHIC

  subroutine LOGMESCHIC(MESSAGE)

    implicit none
      
    character(len=*)  :: MESSAGE
    character(len=80) :: cout

    if (KHOZZZ.EQ.0) RETURN

    write(cout(1:4),'(1x,A3)') ' @ '

    write(cout(5:80),*) MESSAGE(1:len_trim(MESSAGE))

    write(*,'(A80)') cout

  end subroutine LOGMESCHIC

  subroutine CHECK_CHIC_COMMAND
   implicit none
   integer      :: i,compute_box,compute_mass
   do i=1,size(CHPZZZ)
      CHZZZ=CHPZZZ(i)
                       !123456789012345678901234567890.
      if (CHZZZ(1:30)=='NO CHECK CHIC COMMAND         ') return
   enddo
   write(*,*) ' @MOD CHIC: CHECK COMMAND CHIC'
   write(*,*) '   TO REMOVE THIS CHECH PUT:   '
   write(*,*) '   "NO CHECK CHIC COMMAND" IN TOP OF COMMAND.DAT'
   compute_box=0
   compute_mass=0

   do i=1,size(CHPZZZ)
      CHZZZ=CHPZZZ(i)
                         !123456789012345678901234567890.
      if (CHZZZ(1:30)=='COMPUTE BOX                     ') compute_box=1
                         !123456789012345678901234567890.
      if (CHZZZ(1:30)=='COMPUTE MASS                    ') compute_mass=1
     
      if (CHZZZ(1:1)=='#') cycle
                        !123456789012345678901234567890.
      if (CHZZZ(1:30)=='POSTPRO DURING CALCUL         ') then
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) CHZZZ(1:30)
         write(*,*) ' is an old command                      '
         write(*,*) 'REPLACE IT BY POSTPRO DURING COMPUTATION'
         write(*,*) '----------------------------------------'
         pause
      endif   
      if (CHZZZ(1:30)=='POSTPRO AFTER CALCUL         ') then
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) CHZZZ(1:30)
         write(*,*) ' is an old command                      '
         write(*,*) 'REPLACE IT BY POSTPRO AFTER COMPUTATION '
         write(*,*) '----------------------------------------'
         pause
      endif   
      if (CHZZZ(1:30)=='POSTPRO_3D DURING CALCUL         ') then
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) CHZZZ(1:30)
         write(*,*) ' is an old command                      '
         write(*,*) 'REPLACE IT BY POSTPRO DURING COMPUTATION'
         write(*,*) '----------------------------------------'
         pause
      endif   
      if (CHZZZ(1:30)=='POSTPRO_3D AFTER CALCUL         ') then
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) CHZZZ(1:30)
         write(*,*) ' is an old command                      '
         write(*,*) 'REPLACE IT BY POSTPRO AFTER COMPUTATION '
         write(*,*) '----------------------------------------'
         pause
      endif 

      if (CHZZZ(1:30)=='COLLECT OUTPUT GMV           ') then
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:'
         write(*,*) CHZZZ(1:30)
         write(*,*) ' is an old command'
         write(*,*) 'REPLACE IT BY:                          '
         write(*,*) 'Before loading step: INIT GMV           '
         write(*,*) 'Before loading step: WRITE OUTPUT GMV   '
         write(*,*) 'During computation : WRITE OUTPUT GMV STEP  '
         write(*,*) '----------------------------------------'
         pause
      endif   
                       !123456789012345678901234567890.
      if (CHZZZ(1:30)=='DISPLAY SELECT TACT           ') then
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) CHZZZ(1:30)
         write(*,*) ' is an old command                      '
         write(*,*) 'REPLACE IT BY DISPLAY PROX TACTORS      '
         write(*,*) '----------------------------------------'
         pause
      endif   
      
   enddo
   if (compute_box==0) then 
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) 'YOU FORGOT COMMAND COMPUTE BOX          '
         write(*,*) 'IF YOU DONT WANT TO USE IT PUT THE COMMAND'
         write(*,*) ' NO CHECK CHIC COMMAND IN COMMAND.DAT   '
         write(*,*) '----------------------------------------'
         stop
   endif
   if (compute_mass==0) then 
         write(*,*) '----------------------------------------'
         write(*,*) 'CHIC WARNING:                           '
         write(*,*) 'YOU FORGOT COMMAND "COMPUTE MASS"       '
         write(*,*) '----------------------------------------'
         stop
   endif   
  end subroutine CHECK_CHIC_COMMAND
end module CHIC

