*$$$$ CCCJYY 
*      -INC CCCJYY 
*
C *     COMMONS associe a la routine CJYY 
C *
C *   format de tableau reels 
C *   -----------------------
C *                format corrant
       CHARACTER*32 FORTDP 
C *                precision normale
       CHARACTER*32 FOTDPN
C *                super precision
       CHARACTER*32 FOTDPS
C *
C *   format d'une valeur reelle
C *   --------------------------
C *                format corrant
       CHARACTER*32 FORVDP 
C *                precision normale
       CHARACTER*32 FOVDPN
C *                super precision
       CHARACTER*32 FOVDPS
C *
C *
       COMMON / CCJYYC / FORTDP , FORVDP ,
     &                  FOTDPN , FOVDPN ,
     &                  FOTDPS , FOVDPS 
       SAVE   / CCJYYC /
C *
C *
C *           pour le trace en blanc
       LOGICAL LBLANC
C *
       COMMON / CCJYYI /  LBLANC
       SAVE   / CCJYYI /

C *    Pour le niveau d'impression
       INTEGER IIMPI
C *    Pour la sortie
       INTEGER IOIMP
       COMMON / COPTIO / IIMPI , IOIMP
       SAVE / COPTIO /
