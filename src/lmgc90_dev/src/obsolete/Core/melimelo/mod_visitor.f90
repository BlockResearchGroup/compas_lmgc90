! TODO: reprendre pour faire appel au mod_IO

!> LMGC90 visiting procedures
!> \author F. Dubois
!> \date   January 2009

module visitor

  implicit none

  private

  !> what to do (write...)
  character(len=5) :: todo
  !> where to do (unit number)
  integer(kind=4)  :: ifich

  ! writting visitor
  
  public set_visitor, &
         start_entity_visitor, stop_entity_visitor, &
         start_avatar_visitor, stop_avatar_visitor, &
         start_bulk_visitor, stop_bulk_visitor, &
         start_node_visitor, stop_node_visitor, &
         start_tact_visitor, stop_tact_visitor

  contains

  !> \brief set what the visitor must do and where
  subroutine set_visitor(what,where)
    implicit none
    !> [in] what to do (write...)
    character(len=5)          :: what
    !> [in] unit number (default 0)
    integer(kind=4) ,optional :: where

    todo = what
    if (present(where)) then
      ifich=where
    else
      ifich = 0
    endif

  end subroutine

  !> \brief start entity visit
  subroutine start_entity_visitor(status,rank)
    implicit none
    !> [in] 0: open, 1: closed
    integer(kind=4) :: status
    !> [in] rank of the visited entity
    integer(kind=4) :: rank
    !

    select case (todo)
    case('write')
    case default
      print*,'nothing to do in the entity visitor with' 
      print*,todo
    end select

  end subroutine

  !> \brief stop entity visit
  subroutine stop_entity_visitor(status)
    implicit none
    !> [in] 0: open, 1: closed
    integer          :: status

    select case (todo)
    case('write')

    case default
      print*,'nothing to do in the entity visitor with' 
      print*,todo
    end select

  end subroutine

  !> \brief start avatar visit
  subroutine start_avatar_visitor(status,rank,type)
    implicit none
    !> [in] 0: open, 1: closed
    integer(kind=4)  :: status
    !> [in] rank of the visited avatar
    integer(kind=4)  :: rank
    !> [in] type of the visited avatar
    character(len=5) :: type 


    select case (todo)
    case('write')
      
                           !123456789012345678901234567890123456789012345678901234567890123456789012
      write(ifich,'(A72)') '$bdyty                                                                  '
      write(ifich,'(1x,A5)') type

    case default
      print*,'nothing to do in the avatar visitor with' 
      print*,todo
    end select
    
  end subroutine

  !> \brief stop avatar visit
  subroutine stop_avatar_visitor(status)
    implicit none
    !> [in] 0: open, 1: closed
    integer(kind=4) :: status

    select case (todo)
    case('write')
                           !123456789012345678901234567890123456789012345678901234567890123456789012
      write(ifich,'(A72)') '$$$$$$                                                                  '

    case default
      print*,'nothing to do in the avatar visitor with' 
      print*,todo
    end select
  end subroutine

  !> \brief start node visit
  subroutine start_node_visitor(status,rank,coor)
    implicit none
    !> [in] 0: open, 1: closed
    integer(kind=4)       :: status
    !> [in] rank of the visited node
    integer(kind=4)       :: rank
    !> [in] node coordinates
    real(kind=8), pointer :: coor(:)

    select case (todo)
    case('write')

      if (status == 0) then
                             !123456789012345678901234567890123456789012345678901234567890123456789012
        WRITE(ifich,'(A72)') '$nodty                                                                  '
      else if (status == 1 .and. rank == 1) then
                             !123456789012345678901234567890123456789012345678901234567890123456789012
        WRITE(ifich,'(A72)') '$nodty                                                                  '
      endif

      SELECT CASE(size(coor)) 
      CASE(2)
        WRITE(ifich,2) 'NO2xx',rank, &
                       'coo1=',coor(1), &
                       'coo2=',coor(2)
      CASE(3)
        WRITE(ifich,3) 'NO3xx',rank, &
                       'coo1=',coor(1), &
                       'coo2=',coor(2), &
                       'coo3=',coor(3)
      CASE(4)
        WRITE(ifich,3) 'NO4xx',rank, &
                       'coo1=',coor(1), &
                       'coo2=',coor(2), &
                       'coo3=',coor(3)
        WRITE(ifich,4) &
                       'coo4=',coor(4)

      CASE(6)
        WRITE(ifich,3) 'NO6xx',rank, &
                       'coo1=',coor(1), &
                       'coo2=',coor(2), &
                       'coo3=',coor(3)
        WRITE(ifich,6) &
                       'coo4=',coor(4), &
                       'coo5=',coor(5), &
                       'coo6=',coor(6)
      CASE DEFAULT
        print *,'FATAL ERROR: node type not yet implemented'
        print *, size(coor)
        stop
      END SELECT

    case default
      print*,'nothing to do in the node visitor with'
      print*,todo 
    end select

  2 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,2(2X,A5,D14.7))
  3 FORMAT(1X,A5,2X,I5,2X,5X,2X,5X,3(2X,A5,D14.7))
  4 FORMAT(27X                    ,1(2X,A5,D14.7))
  6 FORMAT(27X                    ,3(2X,A5,D14.7))

  end subroutine
 
  !> \brief stop node visit
  subroutine stop_node_visitor(status)
    implicit none
    !> 0: open, 1: closed
    integer          :: status

    select case (todo)
    case('write')
      
    case default
      print*,'nothing to do in the node visitor with' 
      print*,todo
    end select
  end subroutine


  !> \brief start bulk visit
  subroutine start_bulk_visitor(status,rank,type,model,behav,r8_vector,i4_vector)
    implicit none
    !> [in] 0: open, 1: closed
    integer(kind=4)  :: status
    !> [in] rank of the visited bulk
    integer(kind=4)  :: rank
    !> [in] bulk type of the visited bulk
    character(len=5) :: type
    !> [in] model name of the visited bulk
    character(len=5) :: model
    !> [in] material name of the visited bulk
    character(len=5) :: behav
    !> [in] anonymous real vector of the visited bulk
    real(kind=8),    pointer :: r8_vector(:)
    !> [in] anonymous integer vector of the visited bulk
    integer(kind=4), pointer :: i4_vector(:)
    !
    integer(kind=4) :: k

    select case (todo)
    case('write')
      if (status == 0) then
                             !123456789012345678901234567890123456789012345678901234567890123456789012
        WRITE(ifich,'(A72)') '$blmty                                                                  '
      else if (status == 1 .and. rank == 1) then
                             !123456789012345678901234567890123456789012345678901234567890123456789012
        WRITE(ifich,'(A72)') '$blmty                                                                  '
      endif

      SELECT CASE(type)
      CASE ('RBDY2')
        WRITE(ifich,1) model,rank,&
                      'behav',behav,&
                      'avrd=',r8_vector(1), &
                      'gyrd=',r8_vector(2)
      CASE ('RBDY3')
        WRITE(ifich,1) model,rank,&
                      'behav',behav,&
                      'avrd=',0.d0, &
                      'gyrd=',0.d0
        WRITE(ifich,2) &
                      'I1  =',0.d0,&
                      'I2  =',0.d0, &
                      'I3  =',0.d0
      CASE ('S2xxx')   
          WRITE(ifich,12) type, &
                          rank, &
                          'nodes', i4_vector
          WRITE(ifich,102) model,behav
      CASE ('T3xxx')
          WRITE(ifich,13) type, &
                          rank, &
                          'nodes', i4_vector
          WRITE(ifich,102) model,behav
      CASE ('Q4xxx','TE4xx')
          WRITE(ifich,14) type, &
                          rank, &
                          'nodes', i4_vector
          WRITE(ifich,102) model,behav
      CASE ('Q8xxx','H8xxx')
          WRITE(ifich,15) type, &
                          rank, &
                          'nodes', i4_vector
          WRITE(ifich,102) model,behav
      CASE ('H20xx')
          WRITE(ifich,15) type, &
                          rank, &
                          'nodes', (i4_vector(k),k=1,8)
          WRITE(ifich,16) (i4_vector(k),k=9,16) 
          WRITE(ifich,16) (i4_vector(k),k=17,20)     
          WRITE(ifich,102) model,behav
      CASE ('TE10x')
          WRITE(ifich,15) type, &
                          rank, &
                          'nodes', (i4_vector(k),k=1,8)
          WRITE(ifich,17) (i4_vector(k),k=9,10) 
          WRITE(ifich,102) model,behav
      CASE DEFAULT
        print *,'FATAL ERROR: bulk type not yet implemented'
        print *, type
        stop
      END SELECT
    case default
      print*,'nothing to do in the bulk visitor with' 
      print*,todo
    end select


  1 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
  2 FORMAT(1X,5x,2X,5x,2X,5x,2X,5x,3(2X,A5,D14.7))

 12 FORMAT(1X,A5,2x,I5,2x,A5,2(2x,I5))
 13 FORMAT(1X,A5,2x,I5,2x,A5,3(2x,I5))
 14 FORMAT(1X,A5,2x,I5,2x,A5,4(2x,I5))
 15 FORMAT(1X,A5,2x,I5,2x,A5,8(2x,I5))
 16 FORMAT(1X,5X,2x,5x,2x,5X,8(2x,I5))
 17 FORMAT(1X,5X,2x,5x,2x,5X,2(2x,I5))
102 FORMAT(15x,'model',2x,A5,2x,'behav',2x,A5)

  end subroutine

  !> \brief stop bulk visit
  subroutine stop_bulk_visitor(status)
    implicit none
    !> 0: open, 1: closed
    integer(kind=4) :: status

    select case (todo)
    case('write')
      
    case default
      print*,'nothing to do in the avatar visitor with' 
      print*,todo
    end select
  end subroutine

  !> \brief start tact visit
  subroutine start_tact_visitor(status,rank,type,color,r8_vector,i4_vector)
    implicit none
    !> [in] 0: open, 1: closed
    integer(kind=4)  :: status
    !> [in] rank of the visited tact
    integer(kind=4)  :: rank
    !> [in] type of the visited tact
    character(len=5) :: type
    !> [in] color of the visited tact
    character(len=5) :: color
    !> [in] anonymous real vector of the visited tact
    real(kind=8),    pointer :: r8_vector(:)
    !> [in] anonymous integer vector of the visited tact
    integer(kind=4), pointer :: i4_vector(:)
    !
    integer(kind=4) :: i,j

    select case (todo)
    case('write')
      if (status == 0) then
                             !123456789012345678901234567890123456789012345678901234567890123456789012
        WRITE(ifich,'(A72)') '$tacty                                                                  '
      else if (status == 1 .and. rank == 1) then
                             !123456789012345678901234567890123456789012345678901234567890123456789012
        WRITE(ifich,'(A72)') '$tacty                                                                  '
      endif


      SELECT CASE(type)
      CASE('DISKx','xKSID','DISPx','xPSID')

        if (.not. associated(r8_vector)) then


        endif
        WRITE(ifich,104) type, &
                         rank, &
                         'color',color, &
                         'byrd=',r8_vector(1)
      CASE('JONCx')
        WRITE(ifich,105) type, &
                         rank, &
                         'color',color, &
                         'axe1=',r8_vector(1), &
                         'axe2=',r8_vector(2)
      CASE('DISKb')
        WRITE(ifich,104) type, &
                         rank, &
                         'color',color, &
                         'byrd=',r8_vector(1)
        WRITE(ifich,131) 'coo1=',r8_vector(2), &
                         'coo2=',r8_vector(3)
      CASE('POLYG')
        WRITE(ifich,1) type, &
                       rank,  &
                       'color',color, &
                       'nb_vertex=',i4_vector(1)
        j=0
        DO i=1,i4_vector(1)
          WRITE(ifich,2) 'coo1=',r8_vector(j+1), &
                         'coo2=',r8_vector(j+2)
          j = j + 2
        ENDDO
      CASE('PT2Dx')
          WRITE(ifich,105) type, &
                           rank, &
                           'color',color, &
                           'coo1=',r8_vector(1), &
                           'coo2=',r8_vector(2)
      CASE('CLxxx')
          WRITE(ifich,10) type, &
                          rank, &
                         'color',color, &
                         'noda=',i4_vector(1), &
                         'nodb=',i4_vector(2), &
                         'apab=',r8_vector(1) 
      CASE('ALpxx')
          WRITE(ifich,11) type, &
                          rank, &
                         'color',color, &
                         'noda=',i4_vector(2), &
                         'nodb=',i4_vector(3)

          if (i4_vector(1) > 1) then 
            do i=2,i4_vector(1)
              WRITE(ifich,12) type, &
                              rank, &
                             'color',color, &
                             'noda=',i4_vector(2*i    ), &
                             'nodb=',i4_vector(2*i + 1)
            enddo
          endif

      CASE('ASpx3')
          WRITE(ifich,30) type, &
                          rank, &
                         'color',color, &
                         'noda=',i4_vector(2), &
                         'nodb=',i4_vector(3), &
                         'nodc=',i4_vector(4)

          if (i4_vector(1) > 1) then 
            do i=2,i4_vector(1)
              WRITE(ifich,31) type, &
                              rank, &
                             'color',color, &
                             'noda=',i4_vector(3*i - 1), &
                             'nodb=',i4_vector(3*i    ), &
                             'nodc=',i4_vector(3*i + 1)
            enddo
          endif

      CASE('ASpx4')
          WRITE(ifich,32) type, &
                          rank, &
                         'color',color, &
                         'noda=',i4_vector(2), &
                         'nodb=',i4_vector(3), &
                         'nodc=',i4_vector(4), &
                         'nodd=',i4_vector(5)

          if (i4_vector(1) > 1) then 
            do i=2,i4_vector(1)
              WRITE(ifich,33) type, &
                              rank, &
                             'color',color, &
                             'noda=',i4_vector(4*i - 2), &
                             'nodb=',i4_vector(4*i - 1), &
                             'nodc=',i4_vector(4*i    ), &
                             'nodd=',i4_vector(4*i + 1)
            enddo
          endif

      CASE('CSxx3')
          WRITE(ifich,30)  type, &
                           rank, &
                           'color',color, &
                           'noda=',i4_vector(1), &
                           'nodb=',i4_vector(2), &
                           'nodc=',i4_vector(3)
          WRITE(ifich,132) 'w1  =',r8_vector(1), &
                           'w2  =',r8_vector(2), &
                           'w3  =',r8_vector(3)
      CASE('CSxx4')
          WRITE(ifich,32)  type, &
                           rank, &
                           'color',color, &
                           'noda=',i4_vector(1), &
                           'nodb=',i4_vector(2), &
                           'nodc=',i4_vector(3), &
                           'nodd=',i4_vector(4)
          WRITE(ifich,132) 'w1  =',r8_vector(1), &
                           'w2  =',r8_vector(2), &
                           'w3  =',r8_vector(3)
          WRITE(ifich,130) 'w4  =',r8_vector(4)

      CASE('DISKL')
          WRITE(ifich,41) type, &
                          rank, &
                          'color',color, &
                          'noda=',i4_vector(1), &
                          'nodb=',i4_vector(2), &
                          'apab=',r8_vector(1), &
                          'bdyr=',r8_vector(2), &
                          'brpm=',r8_vector(3)

      CASE('PT2DL')
          WRITE(ifich,40) type, &
                          rank, &
                          'color',color, &
                          'noda=',i4_vector(1), &
                          'nodb=',i4_vector(2), &
                          'apab=',r8_vector(1)

      CASE('CYLND','DNLYC')
          WRITE(ifich,105) type, &
                           rank, &
                           'color',color, &
                           'High=',r8_vector(1), &
                           'byrd=',r8_vector(2)

      CASE('PLANx')
        WRITE(ifich,106) type, &
                         rank, &
                        'color',color, &
                        'axe1=',r8_vector(1), &
                        'axe2=',r8_vector(2), &
                        'axe3=',r8_vector(3)
        WRITE(ifich,'(29X,10A)') 'localframe'
        WRITE(ifich,231) 'alp1=',r8_vector(4), &
                         'alp2=',r8_vector(5), &
                         'alp3=',r8_vector(6)
        WRITE(ifich,231) 'bet1=',r8_vector(7), &
                         'bet2=',r8_vector(8), &
                         'bet3=',r8_vector(9)
        WRITE(ifich,231) 'gam1=',r8_vector(10), &
                         'gam2=',r8_vector(11), &
                         'gam3=',r8_vector(12)
        WRITE(ifich,231) 'coo1=',r8_vector(13), &
                         'coo2=',r8_vector(14), &
                         'coo3=',r8_vector(15)

      CASE('POLYR')
        WRITE(ifich,204) type, &
                         rank, &
                        'color',color, &
                        'nb_vertex=',i4_vector(1), &
                        'nb_faces=',i4_vector(2)
        j=0
        DO i=1,i4_vector(1)
          WRITE(ifich,231) 'coo1=',r8_vector(j+1), &
                           'coo2=',r8_vector(j+2), &
                           'coo3=',r8_vector(j+3)
          j = j + 3
        ENDDO

        j=2
        DO i=1,i4_vector(2)
          WRITE(ifich,232) 'ver1=',i4_vector(j+1), &
                           'ver2=',i4_vector(j+2), &
                           'ver3=',i4_vector(j+3)
          j=j+3
        END DO

      CASE('PT3Dx')
          WRITE(ifich,106) type, &
                           rank, &
                           'color',color, &
                           'coo1=',r8_vector(1), &
                           'coo2=',r8_vector(2), &
                           'coo3=',r8_vector(3)

      CASE('SPHER')
        WRITE(ifich,104) type, &
                         rank, &
                         'color',color, &
                         'byrd=',r8_vector(1)
      CASE('SPHEb')
        WRITE(ifich,104) type, &
                         rank, &
                         'color',color, &
                         'byrd=',r8_vector(1)
        WRITE(ifich,132) 'coo1=',r8_vector(2), &
                         'coo2=',r8_vector(3), &
                         'coo3=',r8_vector(4)


      CASE DEFAULT
        print *,'FATAL ERROR: tact type not yet implemented'
        print *, type
        stop
      END SELECT
    case default
      print*,'nothing to do in the tact visitor with' 
      print*,todo
    end select


  1 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7)
  2 FORMAT(27X,2(2X,A5,D14.7))

 10 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,D14.7)
 11 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)
 12 FORMAT('+',A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)

 20 FORMAT(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5)
 21 FORMAT('+',A5,2X,I5,2X,5x,2X,5x,2X,A5,I5,2X,A5,I5)

 30 FORMAT(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5)
 31 FORMAT('+',A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5)
 32 FORMAT(1X ,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5,2X,A5,I5)
 33 FORMAT('+',A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,2X,A5,I5,2X,A5,I5)

 40 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,1(2X,A5,D14.7))
 41 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A5,I5,2X,A5,I5,3(2X,A5,D14.7))

104 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,1(2X,A5,D14.7))
105 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2(2X,A5,D14.7))
106 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,3(2X,A5,D14.7))

130 FORMAT(27X,1(2X,A5,D14.7))
131 FORMAT(27X,2(2X,A5,D14.7))
132 FORMAT(27X,3(2X,A5,D14.7))

204 FORMAT(1X,A5,2X,I5,2X,A5,2X,A5,2X,A10,I7,4X,A9,I7)
231 FORMAT(27X,3(2X,A5,D14.7))
232 FORMAT(27X,3(2X,A5,I7,7X))

  end subroutine

  !> \brief stop tact visit
  subroutine stop_tact_visitor(status)
    implicit none
    !> 0: open, 1: closed
    integer(kind=4) :: status

    select case (todo)
    case('write')
      
    case default
      print*,'nothing to do in the avatar visitor with' 
      print*,todo
    end select
  end subroutine

end module
