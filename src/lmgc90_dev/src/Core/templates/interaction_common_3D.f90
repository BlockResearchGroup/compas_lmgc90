
!> \brief Get the tact law number of an 'verlet' interaction
function get_verlet_tact_lawnb(icdtac, iadj)
  implicit none
  !> candidate contactor id
  integer, intent(in) :: icdtac
  !> adjacent number
  integer, intent(in) :: iadj
  !> tact law nb associated to contact
  integer :: get_verlet_tact_lawnb
  !
  integer           :: isee , cdmodel, anmodel
  character(len=5)  :: cdcol, ancol
  character(len=103):: cout

   get_verlet_tact_lawnb = 0

   cdmodel = verlt(icdtac)%cdmodel
   anmodel = verlt(icdtac)%anmodel(iadj)

   select case( cdmodel )
   case( i_rbdy3 )
     cdcol = get_color_RBDY3(verlt(icdtac)%cdbdy, verlt(icdtac)%cdtac)
   case( i_mbs3  )
     cdcol = get_color_MBS3D(verlt(icdtac)%cdbdy, verlt(icdtac)%cdtac)
   case( i_mailx )
     cdcol = get_color_MAILx(verlt(icdtac)%cdbdy, verlt(icdtac)%cdtac)
   end select

   select case( anmodel )
   case( i_rbdy3 )
     ancol = get_color_RBDY3(verlt(icdtac)%anbdy(iadj), verlt(icdtac)%antac(iadj))
   case( i_mbs3  )
     ancol = get_color_MBS3D(verlt(icdtac)%anbdy(iadj), verlt(icdtac)%antac(iadj))
   case( i_mailx )
     ancol = get_color_MAILx(verlt(icdtac)%anbdy(iadj), verlt(icdtac)%antac(iadj))
   end select

   isee = get_isee_by_ids(con_pedigree%id_cdan, cdmodel, cdcol, anmodel, ancol)

   if( isee == 0 ) then
     write( cout, * ) 'see type not found for ', con_pedigree%id_cdan
     call faterr( con_pedigree%module_name // "::get_verlet_tact_lawnb", cout )
   end if

   get_verlet_tact_lawnb = see(isee)%lawnb

end function

