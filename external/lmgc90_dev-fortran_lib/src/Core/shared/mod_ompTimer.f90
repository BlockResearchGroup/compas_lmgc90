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

!> manages timer to evualate cpu time

MODULE TIMER

  use overall, only: get_io_unit, &
                     location   , &
                     out_timer  , &
                     logmes     , &
                     faterr

  use omp_lib

  IMPLICIT NONE

  PRIVATE

  TYPE T_timer

     CHARACTER(len=20) :: name                 ! name of commande associated to meter
     REAL(kind=8)      :: begin , END , total  ! time before, after and total.
     REAL(kind=8)      :: wbegin, wEND, wtotal ! wall time before, after and total.

     integer           :: thread_id
     
  END TYPE T_timer

 ! 200 is the max number of timers
  integer, parameter                    :: max_nb_timer=200

  ! internal timer
  TYPE(T_timer),DIMENSION(max_nb_timer) :: itimer    
  INTEGER                               :: nb_it=0
  LOGICAL :: is_it_started = .FALSE.

  ! user timer
  TYPE(T_timer),DIMENSION(max_nb_timer) :: utimer     
  INTEGER                               :: nb_ut=0
  LOGICAL :: is_ut_started = .FALSE.

  ! external timer
  type(T_timer), dimension(max_nb_timer) :: etimer     
  integer(kind=4)                        :: nb_et = 0
  logical                                :: is_et_started = .false.

  ! wrap API

  public initialize_itimer, write_itimer, &
         initialize_utimer, write_utimer, &
         initialize_etimer, write_etimer

  ! internal API

  public start_itimer, stop_itimer, get_new_itimer_ID, is_started_itimer, &
         start_utimer, stop_utimer, get_new_utimer_ID, is_started_utimer, &
         start_etimer, stop_etimer, get_new_etimer_ID, is_started_etimer, &
         clear_all

  ! private function to factorize code
  ! all take an array of T_timer in inpute

  private initialize_   , write_       , &
          start_byID_   , stop_byID_   , &
          start_byname_ , stop_byname_ , &
          get_new_ID_   , write_in_omp_, &
          clear_

  INTERFACE start_itimer
     MODULE PROCEDURE start_itimer_byname,start_itimer_byID
  END INTERFACE

  INTERFACE stop_itimer
     MODULE PROCEDURE stop_itimer_byname,stop_itimer_byID
  END INTERFACE

  INTERFACE start_utimer
     MODULE PROCEDURE start_utimer_byname,start_utimer_byID
  END INTERFACE

  INTERFACE stop_utimer
     MODULE PROCEDURE stop_utimer_byname,stop_utimer_byID
  END INTERFACE

  interface start_etimer
     module procedure start_etimer_byname,start_etimer_byID
  end interface

  interface stop_etimer
     module procedure stop_etimer_byname,stop_etimer_byID
  end interface

contains

!!!-----------!!!
! generic timer !
!!!-----------!!!
  subroutine initialize_(timers, nb_t)
    implicit none
    !> the timer list to initialize
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timer used in the list
    integer(kind=4), intent(inout) :: nb_t

    timers(:)%begin  = 0.d0
    timers(:)%end    = 0.d0
    timers(:)%total  = 0.d0
    timers(:)%wbegin = 0.d0
    timers(:)%wend   = 0.d0
    timers(:)%wtotal = 0.d0

    timers(:)%thread_id = -1

    nb_t = 1

    !                 12345678901234567890
    timers(nb_t)%name='global              '
    call cpu_time(timers(nb_t)%begin)
    timers(nb_t)%wbegin = omp_get_wtime()
  end subroutine initialize_
    
  subroutine write_(i_unit, timers, nb_t)
    implicit none
    !> unit number on which to write
    integer(kind=4), optional, intent(in) :: i_unit 
    !> the timer list to write
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timer used in the list
    integer(kind=4), intent(in) :: nb_t
    !
    integer(kind=4) :: i, io
    real(kind=8)    :: percent_global, wpercent_global, scaling, ti_total

    ! stopping global timer
    call cpu_time(timers(1)%end)
    timers(1)%wend = omp_get_wtime()

    timers(1)%total  = timers(1)%end  - timers(1)%begin 
    timers(1)%wtotal = timers(1)%wend - timers(1)%wbegin 

    if( present(i_unit) ) then
      io = i_unit
    else
      io = get_io_unit()
      open( unit=io, file=location(out_timer), status='REPLACE')
    end if

                                                    !1234567890123456   !1234567   !1234567890123456789   !123457    !12345678901
    write(io,'(25x,A16,2x,A7,2x,A19,2x,A6,2x,A11)') 'CPU time       :', 'ratio :', '|  Elapsed time   :', 'ratio:', '|  scaling :'
    write(io,'(A1)') ' '

    if( timers(1)%total /= 0.d0 ) then
      percent_global  = 100.d0/timers(1)%total 
    else
      percent_global  = 0.d0
    end if
    if( timers(1)%wtotal /= 0.d0 ) then
      wpercent_global = 100.d0/timers(1)%wtotal 
    else
      wpercent_global = 0.d0
    end if

    do i=2,nb_t
       if( timers(i)%wtotal /= 0.d0 ) then
         ti_total = timers(i)%total / timers(i)%wtotal
       else
         ti_total = 0.d0
       end if
       write(io,212) '-',timers(i)%name,timers(i)%total ,'s',timers(i)%total*percent_global  ,'%  | ', &
                                        timers(i)%wtotal,'s',timers(i)%wtotal*wpercent_global,'%  | ', &
                                        ti_total
    end do

    if( sum(timers(2:nb_t)%wtotal) /= 0.d0 ) then
      scaling = sum(timers(2:nb_t)%total) / sum(timers(2:nb_t)%wtotal)
    else
      scaling = 1.d0
    end if
                           !123456789012345678901234567890123456789012345678901234567890123456
    write(io,'(24x,A66)')  '----------------------------+----------------------------+--------'
    write(io,'(A24,D14.7,2X,A1,1X,F6.2,1X,A5,D14.7,1X,A1,1x,F6.2,1X,A5,F6.2)') &
             '  Accounted time      :', sum(timers(2:nb_t)%total),'s', &
                                        sum(timers(2:nb_t)%total)*percent_global,'%  | ', &
                                        sum(timers(2:nb_t)%wtotal),'s', &
                                        sum(timers(2:nb_t)%wtotal)*wpercent_global,'%  | ', &
                                        scaling
    if( timers(1)%wtotal /= 0.d0 ) then
      ti_total = timers(1)%total/timers(1)%wtotal 
    else
      ti_total = 0.d0
    end if
    write(io,'(A24,D14.7,2X,A1,11X,A1,1X,D14.7,1X,A1,11X,A1,1x,F6.2)') '  Total elapsed time  :', &
                                                                       timers(1)%total ,'s','|', &
                                                                       timers(1)%wtotal,'s','|', &
                                                                       ti_total

    if( .not. present(i_unit) ) close(io)

212 format(1X,A1,1X,A20,1X,D14.7,2X,A1,1x,F6.2,1X,A5,D14.7,1X,A1,1x,F6.2,1X,A5,F6.2)

  end subroutine write_

  subroutine write_in_omp_(i_unit, timers, nb_t)
    implicit none
    !> unit number on which to write
    integer(kind=4), optional, intent(in) :: i_unit
    !> the timer list to write
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timer used in the list
    integer(kind=4), intent(in) :: nb_t
    !
    integer           :: i, io, nb_diff, i_diff
    real(kind=8)      :: percent_global, total
    character(len=20) :: name
    integer, allocatable, dimension(:) :: cc_timer

    ! stopping global timer
    call cpu_time(timers(1)%end)

    timers(1)%total  = timers(1)%end  - timers(1)%begin

    if( present(i_unit) ) then
      io = i_unit
    else
      io = get_io_unit()
      open( unit=io, file=location(out_timer), status='REPLACE')
    end if

                                                    !1234567890123456   !1234567890
    write(io,'(25x,A16,2x,A7,2x,A19,2x,A6,2x,A10)') 'CPU time       :', 'ratio :  |'
    write(io,'(A1)') ' '

    percent_global  = 100.D0/timers(1)%total

    ! counting the different timers:
    ! relies on the fact that all the timers of
    ! of a name are created in a consecutive manner
    nb_diff = 0
    do i = 2, nb_t
      if( timers(i)%name /= timers(i-1)%name ) nb_diff = nb_diff + 1
    end do

    allocate( cc_timer( nb_diff+1 ) )
    cc_timer = 1

    i_diff = 1
    do i = 2, nb_t
      if( timers(i)%name /= timers(i-1)%name ) then
        i_diff = i_diff + 1
        cc_timer(i_diff) = cc_timer(i_diff-1)
      end if
      cc_timer(i_diff) = cc_timer(i_diff)+1
    end do

    do i_diff = 1, nb_diff
       total = 0.d0
       do i = cc_timer(i_diff)+1, cc_timer(i_diff+1)
         name  = timers(i)%name
         total = total + timers(i)%total
       end do
       write(io,213) '-', name, total ,'s', total*percent_global, '% |'
    end do

                           !1234567890123456789012345678
    write(io,'(24x,A28)')  '---------------------------+'
    write(io,'(A24,D14.7,2X,A1,1X,F6.2,1X,A3)') &
             '  Accounted time      :', sum(timers(2:nb_t)%total),'s', &
                                        sum(timers(2:nb_t)%total)*percent_global, '% |'
    write(io,'(A24,D14.7,2X,A1,10X,A1)') '  Total elapsed time  :', timers(1)%total ,'s','|'

    if( .not. present(i_unit) ) close(io)

213 format(1X,A1,1X,A20,1X,D14.7,2X,A1,1x,F6.2,1X,A3)

  end subroutine write_in_omp_

  subroutine start_byname_(XYZ, timers, nb_t)
    implicit none
    !> name of timer to start
    character(len=20), intent(in) :: XYZ
    !> list of timers to work on
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timer used in the list
    integer(kind=4), intent(inout) :: nb_t
    !
    integer(kind=4) :: i

    do i = 2, nb_t
       if ( timers(i)%name == XYZ ) then
          call cpu_time(timers(i)%begin)
          timers(i)%wbegin = omp_get_wtime()
          return
       end if
    end do

    nb_t = nb_t + 1

    call cpu_time(timers(nb_t)%begin)
    timers(nb_t)%wbegin = omp_get_wtime()
    timers(nb_t)%name   = XYZ

    return

  end subroutine start_byname_
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_byname_(XYZ, timers, nb_t)
    implicit none
    !> name of timer to stop
    character(len=20), intent(in) :: XYZ
    !> list of timers to work on
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timer used in the list
    integer(kind=4), intent(in) :: nb_t
    !
    integer(kind=4) :: i
    
    do i = 2, nb_t
       if ( timers(i)%name .eq. XYZ ) then
          call cpu_time(timers(i)%end)
          timers(i)%wend   = omp_get_wtime()
          timers(i)%total  = timers(i)%total  + timers(i)%end  - timers(i)%begin
          timers(i)%wtotal = timers(i)%wtotal + timers(i)%wend - timers(i)%wbegin
          return
       end if
    end do
    
  end subroutine stop_byname_
!!!------------------------------------------------------------------------------------------------
  subroutine start_byID_(ID, timers)
    implicit none
    !> id of timer to start
    integer(kind=4), intent(in) :: id
    !> list of timers to work on
    type(T_timer), dimension(max_nb_timer) :: timers

    call cpu_time(timers(ID)%begin)
    timers(ID)%wbegin = omp_get_wtime()

  end subroutine start_byID_
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_byID_(ID, timers)
    implicit none
    !> id of timer to stop
    integer(kind=4), intent(in) :: id
    !> list of timers to work on
    type(T_timer), dimension(max_nb_timer) :: timers

    call cpu_time(timers(iD)%end)
    timers(iD)%wend = omp_get_wtime()

    timers(ID)%total  = timers(ID)%total  + timers(iD)%end  - timers(ID)%begin
    timers(ID)%wtotal = timers(ID)%wtotal + timers(iD)%wend - timers(ID)%wbegin
    
  end subroutine stop_byID_
!!!---------------------------------------------------------------------------------------------------
  integer(kind=4) function get_new_ID_(name, timers, nb_t, in_omp)
    implicit none
    !> name of the timer
    character(len=20), intent(in) :: name
    !> the timer list to initialize
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timer used in the list
    integer(kind=4), intent(inout) :: nb_t
    !> is timer started inside an openMP loop
    logical, optional :: in_omp
    !
    integer(kind=4) :: i, thread_id

    get_new_ID_ = 0
 
    if (nb_t == max_nb_timer) then
      call logmes(name)
      call faterr('timer::get_new_ID_','maximum number of timers reached')
    end if

    thread_id = omp_get_thread_num()

    do i = 1,nb_t
      if (name == timers(i)%name) then
        if( present(in_omp) ) then
          if( thread_id == timers(i)%thread_id ) then
            call logmes(name)
            call faterr('timer::get_new_ID_','timer already allocated')
          end if
        else
          call logmes(name)
          call faterr('timer::get_new_ID_','timer already allocated')
        end if
      end if
    end do

    nb_t = nb_t + 1

    timers(nb_t)%name = name

    if( present(in_omp) ) then
      timers(nb_t)%thread_id = thread_id
    end if

    get_new_ID_ = nb_t

  end function get_new_ID_

  subroutine clear_(timers, nb_t)
    implicit none
    !> the timer list to clear
    type(T_timer), dimension(max_nb_timer) :: timers
    !> the actual number of timers used in the list
    integer, intent(in) :: nb_t
    !
    integer :: i

    do i = 1, nb_t

      timers(i)%begin  = 0.d0
      timers(i)%end    = 0.d0
      timers(i)%total  = 0.d0

      timers(i)%wbegin = 0.d0
      timers(i)%wend   = 0.d0
      timers(i)%wtotal = 0.d0

    end do

  end subroutine clear_

!!!---------------------------------------------------------------------------------------------------
! internal timer
!!!------------------------------------------------------------------------------------------------
  subroutine initialize_itimer
    implicit none

    ! si on a deja demarre on sort
    if ( is_it_started ) return

    call initialize_(itimer, nb_it)

    is_it_started = .true.

  end subroutine initialize_itimer
!!!------------------------------------------------------------------------------------------------
  subroutine write_itimer(i_unit)
    implicit none
    !> unit number on which to write
    integer(kind=4), optional, intent(in) :: i_unit 

    if (.not. is_it_started) return

    call write_(i_unit, itimer, nb_it)

  end subroutine write_itimer
!!!---------------------------------------------------------------------------------------------------
  subroutine start_itimer_byname(XYZ)
    implicit none
    !> name of internal timer to start
    character(len=20), intent(in) :: XYZ

    if (.not. is_it_started) return

    call start_byname_(XYZ, itimer, nb_it)

  end subroutine start_itimer_byname
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_itimer_byname(XYZ)
    implicit none
    !> name of internal timer to stop
    character(len=20), intent(in) :: XYZ

    if (.not. is_it_started) return
    
    call stop_byname_(XYZ, itimer, nb_it)

  end subroutine stop_itimer_byname
!!!------------------------------------------------------------------------------------------------
  subroutine start_itimer_byID(ID)
    implicit none
    !> id of internal timer to start
    integer(kind=4), intent(in) :: ID

    if (.not. is_it_started .or. ID == 0) return

    call start_byID_(ID, itimer)

  end subroutine start_itimer_byID
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_itimer_byID(ID)
    implicit none
    !> id of internal timer to stop
    integer(kind=4), intent(in) :: ID
    
    if (.not. is_it_started .or. ID == 0) return
    
    call stop_byID_(ID, itimer)

  end subroutine stop_itimer_byID
!!!---------------------------------------------------------------------------------------------------
  integer(kind=4) function get_new_itimer_ID(name)
    implicit none
    !> name of the new internal timer
    character(len=20), intent(in) :: name

    
    get_new_itimer_ID = 0
 
    if (.not. is_it_started) return

    get_new_itimer_ID = get_new_ID_(name, itimer, nb_it)

  end function get_new_itimer_ID
!!!---------------------------------------------------------------------------------------------------
  !> fonction pour savoir si les timers tournent
  function is_started_itimer()
      implicit none
      !> True if internal timers started
      logical :: is_started_itimer 
                                   
      is_started_itimer = is_it_started

  end function is_started_itimer

!!!---------------------------------------------------------------------------------------------------
! external timer
!!!------------------------------------------------------------------------------------------------
  subroutine initialize_etimer
    implicit none

    ! si on a deja demarre on sort
    if ( is_et_started ) return

    call initialize_(etimer, nb_et)

    is_et_started = .true.

  end subroutine initialize_etimer
!!!------------------------------------------------------------------------------------------------
  subroutine write_etimer(i_unit)
    implicit none
    !> unit number on which to write
    integer(kind=4), optional, intent(in) :: i_unit 

    if (.not. is_et_started) return

    call write_in_omp_(i_unit, etimer, nb_et)

  end subroutine write_etimer
!!!---------------------------------------------------------------------------------------------------
  subroutine start_etimer_byname(XYZ)
    implicit none
    !> name of internal timer to start
    character(len=20), intent(in) :: XYZ

    if (.not. is_et_started) return

    call start_byname_(XYZ, etimer, nb_et)

  end subroutine start_etimer_byname
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_etimer_byname(XYZ)
    implicit none
    !> name of internal timer to stop
    character(len=20), intent(in) :: XYZ

    if (.not. is_et_started) return
    
    call stop_byname_(XYZ, etimer, nb_et)

  end subroutine stop_etimer_byname
!!!------------------------------------------------------------------------------------------------
  subroutine start_etimer_byID(ID)
    implicit none
    !> id of internal timer to start
    integer(kind=4), intent(in) :: ID

    if (.not. is_et_started .or. ID == 0) return

    call start_byID_(ID, etimer)

  end subroutine start_etimer_byID
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_etimer_byID(ID)
    implicit none
    !> id of internal timer to stop
    integer(kind=4), intent(in) :: ID
    
    if (.not. is_et_started .or. ID == 0) return
    
    call stop_byID_(ID, etimer)

  end subroutine stop_etimer_byID
!!!---------------------------------------------------------------------------------------------------
  integer(kind=4) function get_new_etimer_ID(name)
    implicit none
    !> name of the new internal timer
    character(len=20), intent(in) :: name

    get_new_etimer_ID = 0
 
    if (.not. is_et_started) return

    get_new_etimer_ID = get_new_ID_(name, etimer, nb_et, .true.)

  end function get_new_etimer_ID
!!!---------------------------------------------------------------------------------------------------
  !> fonction pour savoir si les timers tournent
  function is_started_etimer()
      implicit none
      !> True if internal timers started
      logical :: is_started_etimer 
                                   
      is_started_etimer = is_et_started

  end function is_started_etimer

!!!---------------------------------------------------------------------------------------------------
! user timer
!!!------------------------------------------------------------------------------------------------
  subroutine initialize_utimer
    implicit none

    ! si on a deja demarre on sort
    if ( is_ut_started ) return

    call initialize_(utimer, nb_ut)

    ! pour avoir le temps global
    call initialize_itimer()
    call initialize_etimer()

    is_ut_started = .true.

  end subroutine initialize_utimer
!!!------------------------------------------------------------------------------------------------
  subroutine write_utimer(i_unit)
    implicit none
    !> unit number on which to write
    integer(kind=4), optional, intent(in) :: i_unit 
    !
    integer(kind=4) :: i,io

    if (.not. is_ut_started) return

    if( present(i_unit) ) then
      io = i_unit
    else
      io = get_io_unit()
      open(unit=io, file=location(out_timer), status='REPLACE')
    end if

    ! on ecrit les timers internes ...
    if ( is_it_started .and. nb_it > 1 ) then
      write(io,'(A)')           'Internal timers: '
      write(io,'(A)')           '================ '
      call write_(io, itimer, nb_it)

      write(io,'(A1)')          ' '
      write(io,'(A1)')          ' '
    end if

    if ( is_et_started .and. nb_et > 1 ) then
      write(io,'(A)')           'External timers: '
      write(io,'(A)')           '================ '
      call write_in_omp_(io, etimer, nb_et)

      write(io,'(A1)')          ' '
      write(io,'(A1)')          ' '
    end if

    ! ... puis user
    if ( is_ut_started .and. nb_ut >  1) then
      write(io,'(A)')           'User timers: '
      write(io,'(A)')           '============ '
      call write_(io, utimer, nb_ut)
    end if

    if( .not. present(i_unit) ) close(io)

  end subroutine write_utimer
!!!---------------------------------------------------------------------------------------------------
  subroutine start_utimer_byname(XYZ)
    implicit none
    !> name of internal timer to start
    character(len=20), intent(in) :: XYZ

    if (.not. is_ut_started) return

    call start_byname_(XYZ, utimer, nb_ut)

  end subroutine start_utimer_byname
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_utimer_byname(XYZ)
    implicit none
    !> name of internal timer to stop
    character(len=20), intent(in) :: XYZ

    if (.not. is_ut_started) return
    
    call stop_byname_(XYZ, utimer, nb_ut)

  end subroutine stop_utimer_byname
!!!------------------------------------------------------------------------------------------------
  subroutine start_utimer_byID(ID)
    implicit none
    !> id of internal timer to start
    integer(kind=4), intent(in) :: ID

    if (.not. is_ut_started .or. ID == 0) return

    call start_byID_(ID, utimer)

  end subroutine start_utimer_byID
!!!---------------------------------------------------------------------------------------------------
  subroutine stop_utimer_byID(ID)
    implicit none
    !> id of internal timer to stop
    integer(kind=4), intent(in) :: ID
    
    if (.not. is_ut_started .or. ID == 0) return
    
    call stop_byID_(ID, utimer)

  end subroutine stop_utimer_byID
!!!---------------------------------------------------------------------------------------------------
  integer(kind=4) function get_new_utimer_ID(name)
    implicit none
    !> name of the new internal timer
    character(len=20), intent(in) :: name

    
    get_new_utimer_ID = 0
 
    if (.not. is_ut_started) return

    get_new_utimer_ID = get_new_ID_(name, utimer, nb_ut)

  end function get_new_utimer_ID
!!!---------------------------------------------------------------------------------------------------
  !> fonction pour savoir si les timers tournent
  function is_started_utimer()
      implicit none
      !> True if internal timers started
      logical :: is_started_utimer 
                                   
      is_started_utimer = is_ut_started

  end function is_started_utimer
!!!---------------------------------------------------------------------------------------------------

  subroutine clear_all()
    implicit none


    if ( is_it_started ) then
      call clear_(itimer, nb_it)
      call cpu_time(itimer(1)%begin)
      itimer(1)%wbegin = omp_get_wtime()
    end if

    if ( is_et_started ) then
      call clear_(etimer, nb_et)
      call cpu_time(etimer(1)%begin)
      etimer(1)%wbegin = omp_get_wtime()
    end if

    if ( is_ut_started ) then
      call clear_(utimer, nb_ut)
      call cpu_time(utimer(1)%begin)
      utimer(1)%wbegin = omp_get_wtime()
    end if

  end subroutine clear_all

END MODULE TIMER
