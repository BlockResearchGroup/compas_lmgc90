!------------------------------------------------------------------------
! The following more_chic_command are some chic commands for exotic purpose  
!------------------------------------------------------------------------
 SUBROUTINE more_chic_command_bulk_behav

   IMPLICIT NONE

   if (INDEX(CHZZZ,'IRSN INIT BULK BEHAV')==1) then
     if (KHOZZZ.EQ.1) write(*,'(1X,A3,A30,1X,A5)')' @ ',CHZZZ,'BULK '
     call irsn_init_bulks
     IETZZZ = 1
     return      
   end if

  
 END SUBROUTINE more_chic_command_bulk_behav
!------------------------------------------------------------------------
 subroutine irsn_init_bulks
   implicit none
                             !123456789012345678901234567
   character(len=26)  :: IAM='mod_MAILx::irsn_init_bulks'

   call set_nb_bulks(1)

   call set_bulk(1,'xper_')

 end subroutine
