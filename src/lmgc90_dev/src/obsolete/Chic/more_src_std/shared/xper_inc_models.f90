!!!------------------------------------------------------------------------
!!! The following more_chic_command are some chic commands for exotic purpose  
!!!------------------------------------------------------------------------
 SUBROUTINE more_chic_command_models

   IMPLICIT NONE

   if (INDEX(CHZZZ,'IRSN INIT MODELS')==1) then
     if (KHOZZZ.EQ.1) write(*,'(1X,A3,A30,1X,A5)')' @ ',CHZZZ,'MODEL'
     call irsn_init_models
     IETZZZ = 1
     return      
   end if


 END SUBROUTINE more_chic_command_models
!!!------------------------------------------------------------------------

 subroutine irsn_init_models
   implicit none
                             !123456789012345678901234567
   character(len=27)  :: IAM='mod_MAILx::irsn_init_models'

   call set_nb_models(2)

   call set_model(1,'xper3','MECAx','T3xxx')

   call set_model(2,'xper4','MECAx','Q4xxx')

 end subroutine
