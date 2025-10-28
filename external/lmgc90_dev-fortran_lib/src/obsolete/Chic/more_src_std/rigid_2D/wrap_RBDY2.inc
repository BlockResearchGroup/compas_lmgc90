  SUBROUTINE more_chic_command_RBDY2

    IMPLICIT NONE
    integer :: ibdy
    real(kind=8) :: alpha,dT(4),H,TPSi
    real(kind=8) :: T1=0.D0,T2=0.D0,T3=0.D0,T4=0.D0  
    real(kind=8) :: TPS_start,TPS_stop,TPS,xx
    real(kind=8) :: T1_start,T1_stop
    real(kind=8) :: T2_start,T2_stop
    real(kind=8) :: T3_start,T3_stop
    real(kind=8) :: T4_start,T4_stop

    character(len=30) :: zobzz

                    !123456789012345678901234567890
    IF (INDEX(CHZZZ,'SET BRICK THERMAL STRAIN')==1) THEN
       !!****e* CHIC_cmd/SET BRICK THERMAL FIELD
       !! NAME
       !!   SET BRICK THERMAL FIELD
       !! PURPOSE
       !!  Set a linear thermal field on a rectangular domain
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_field_bynode
       !!****
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),*) ibdy
       READ(CHLZZZ(NZZZ+2),*) alpha
       zobzz = CHLZZZ(NZZZ+3)
       IF (INDEX(zobzz,'EVOLUTIF')==1) THEN       

         !print*,ibdy,alpha

         READ(CHLZZZ(NZZZ+4),*) TPS_start,TPS_stop
         READ(CHLZZZ(NZZZ+5),*) T1_start,T2_start,T3_start,T4_start
         READ(CHLZZZ(NZZZ+6),*) T1_stop ,T2_stop ,T3_stop ,T4_stop       
         TPS=get_time()
         H = get_time_step()

         TPSi = TPS-H
         if ( TPSi <= TPS_start) then 
           xx = 0.d0 
	 else if ( TPSi > TPS_start  .and. TPSi < TPS_stop) then
	   xx = (TPSi - TPS_start) / (TPS_stop - TPS_start)
         else
           xx = 1.d0
         endif

	 !print *,TPS,xx

	 T1 = ((1.d0 - xx) * T1_start) + (xx * T1_stop) 
	 T2 = ((1.d0 - xx) * T2_start) + (xx * T2_stop) 
	 T3 = ((1.d0 - xx) * T3_start) + (xx * T3_stop) 
	 T4 = ((1.d0 - xx) * T4_start) + (xx * T4_stop) 

         if ( TPS <= TPS_start) then 
           xx = 0.d0 
	 else if ( TPS > TPS_start  .and. TPS < TPS_stop) then
	   xx = (TPS - TPS_start) / (TPS_stop - TPS_start)
         else
           xx = 1.d0
         endif

	 dT(1) = (((1.d0 - xx) * T1_start) + (xx * T1_stop)) - T1
	 dT(2) = (((1.d0 - xx) * T2_start) + (xx * T2_stop)) - T2
	 dT(3) =( ((1.d0 - xx) * T3_start) + (xx * T3_stop)) - T3
	 dT(4) = (((1.d0 - xx) * T4_start) + (xx * T4_stop)) - T4

	 !print *,dT
         !print *,'----------'

       ELSE
         READ(CHLZZZ(NZZZ+3),*) dT(1),dT(2),dT(3),dT(4)
       ENDIF


       CALL SetThermalStrain(ibdy,alpha,dT)

       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE more_chic_command_RBDY2

  subroutine SetThermalStrain(ibdy,alpha,dT)
    ! petite routine pour affecter un champ de Temperature
    ! lineaire sur un domaine rectangulaire 
    ! l espacement des noeuds est suppose regulier

    implicit none
       integer      :: ibdy
       real(kind=8) :: alpha,dT(4),dTm
       real(kind=8) :: inc_dilat(4)

       dTm = 0.25*(dT(1)+dT(2)+dT(3)+dT(4))
       
       inc_dilat = alpha * dT

       call set_dilatation_increment(ibdy,dTm,inc_dilat)

  end subroutine