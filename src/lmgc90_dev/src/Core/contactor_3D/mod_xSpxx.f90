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
 MODULE xSpxx                                       

 !fd on masque l'usage de tface, vertex, etc


 USE overall
 USE utilities
 USE bulk_behaviour
 USE tact_behaviour
 USE a_DOF

 USE MAILx
 USE mecaMAILx
  
 USE a_EF
 USE DiscreteGeometry

 IMPLICIT NONE

 PRIVATE

 !> xSp object contains a set of element (T3|Q4|T6|Q8)
 type,public :: t_xSpxx                                 

   ! public for contactor and bodies

   ! id du corps correspondant
   INTEGER                              :: ibdyty       

   ! nombre d'elements sous jacents (3, 4, 6 ou 8 noeuds)
   INTEGER                              :: nb_xSxxx     

   ! nombre de noeuds locaux sous jacents
   INTEGER                              :: nb_vertex    

   ! map between vertex in xSpxx and node in mesh
   integer,dimension(:)  ,pointer       :: vertex2node => null()

   ! number of vertices by element (anciennement face)
   integer,dimension(:)  ,pointer       :: nb_vertex_bf => null()

   ! element connectivity (surdimensionne a 8 sur dim=1) using mesh node ids
   integer,dimension(:,:),pointer       :: face => null()

   logical                              :: is_precon
   logical                              :: well_oriented

   ! public for contact detection

   !fd tableaux de travail
   !   la structure HE ne travaille que sur des triangles
   !   donc ici on a la liste des faces reelles

   ! nombre de faces triangulaires sous jacentes
   INTEGER                              :: nb_tface        

   ! connectivite des faces triangulaires avec la numerotation locale
   integer,dimension(:,:),pointer       :: tface => null() 

   ! map entre num de tface et num de face
   integer,dimension(:),pointer         :: tface2face => null()

   ! Structure Half Edge necessaire a la recherche de contact
   ! Rq: cette structure gere les triangles donc on travaille sur les "tface"
   type(T_HE_Hdl),pointer :: HE_Hdl

   !fd a voir si on garde
   real(kind=8),dimension(:),pointer    :: weight => null()

   !fd local arrays for contact detection
   real(kind=8),dimension(:,:),pointer :: tcoor   => null()
   real(kind=8),dimension(:,:),pointer :: tnormal => null()

   !rm quadrature to use on element
   integer(kind=4) :: quadrature

 END TYPE T_xSpxx

 PUBLIC :: load_tactors_xSpxx,&
           get_num_xSPxx, &
           get_interp_xSPxx, &
           get_coorTT_xSpxx, &
           get_nb_xSxxx, &
           get_nb_vertex_xSpxx, &
           is_singleton_xSpxx, &
           get_HE_Hdl_xSpxx, &
           compute_nodal_resultant_of_constant_field, &
           increment_xSpxx, &
           get_connec_xSpxx  , &
           get_all_data_xSpxx, &
           erase_xSpxx

 CONTAINS

!!!------------------------------------------------------------------------
 SUBROUTINE load_tactors_xSpxx(T,l_xSpxx,xSpxx2bdyty)

   !fd attention les xSpxx sont des conteneurs de xSxxx

   IMPLICIT NONE

   ! C ou A
   character(len=1) :: T
   character(len=120) :: mes
   type(T_xSpxx),dimension(:)   :: l_xSpxx
   integer      ,dimension(:,:) :: xSpxx2bdyty

   ! ***
   INTEGER :: nb_MAILx,idata_sz
   INTEGER :: ibdyty,itacty,errare
   INTEGER,dimension(:),allocatable :: l_idata

   integer :: i,f,nb_xSxxx,nb_vertex_bf,nb_xSxxt,tface(3),idx,nb_xSpxx

   ! le nombre max de triangle adjacent a un noeud
   integer,parameter :: max_adj_face=20

   ! true => normals are located on face, false => normals are located on nodes   
   logical :: with_facenormal 
   character(len=5) :: ttype

                            !1234567890123456789
   CHARACTER(len=19) :: IAM='xSpxx::load_tactors'

   CHARACTER(len=103) :: cout

   integer :: err_

   nb_xSpxx=0

   nb_MAILx=get_nb_MAILx()

   DO ibdyty=1,nb_MAILx
     !!
     DO itacty=1,get_nb_tacty_MAILx(ibdyty)

       !print*, get_tacID_MAILx(ibdyty,itacty),ibdyty,itacty

       ttype = get_tacID_MAILx(ibdyty,itacty)
       IF ( ttype == T//'Spxx'.or. &
            ttype == T//'Spx0'.or. &
            ttype == T//'Spx1'.or. &
            ttype == T//'Spx2') THEN

          nb_xSpxx=nb_xSpxx+1
          ! serial number of body MAILx to which is attached the 
          ! contactor xSpxx numbered itac in the list of all 
          ! contactors xSpxx
          xspxx2bdyty(1,nb_xSpxx)=ibdyty   
          ! serial number of contactor xSpxx itac in the list of contactors of its kind
          xspxx2bdyty(2,nb_xSpxx)=itacty            
          l_xSpxx(nb_xSpxx)%ibdyty = M2meca(ibdyty)%bdyty

          if( ttype(5:) == 'x' ) then
            with_facenormal = .false.
            l_xSpxx(nb_xSpxx)%quadrature = -1
          else
            with_facenormal = .true.
            read(ttype(5:),'(I1)') l_xSpxx(nb_xSpxx)%quadrature
          end if

          !fd permet de recuperer la taille vraie sans le terme masque en 0
          CALL get_idata_sz_MAILx(ibdyty,itacty,idata_sz)

          allocate(l_idata(idata_sz),stat=errare)
          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating l_idata')
          END IF

          !fd recupere le tableau sans le terme masque en 0
          CALL get_idata_MAILx(ibdyty,itacty,l_idata(1:idata_sz))

          l_xSpxx(nb_xSpxx)%is_precon = .FALSE.
          l_xSpxx(nb_xSpxx)%well_oriented = .TRUE.

          !fd on compte les facettes contenues dans idata
          nb_xSxxx=1
          nb_xSxxt=1
          i=0
          do
            nb_vertex_bf=l_idata(i+1)
            if (nb_vertex_bf == 4) nb_xSxxt= nb_xSxxt + 1                       
            if (nb_vertex_bf == 8) nb_xSxxt= nb_xSxxt + 5                       
            if (nb_vertex_bf == 6) nb_xSxxt= nb_xSxxt + 3                       
           
            i = i + 1 + nb_vertex_bf
            if ( i == idata_sz) exit
            if ( i > idata_sz) then
              CALL FATERR(IAM,'mismatch counting the xSpxx facets')
            endif
            nb_xSxxx= nb_xSxxx + 1
            nb_xSxxt= nb_xSxxt + 1 
           enddo
          l_xSpxx(nb_xSpxx)%nb_xSxxx = nb_xSxxx
          l_xSpxx(nb_xSpxx)%nb_tface = nb_xSxxt

          !print*,nb_xSxxx,nb_xSxxt

          ALLOCATE(l_xSpxx(nb_xSpxx)%face(8,nb_xSxxx),stat=errare)
          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating l_xSpxx%face')
          END IF

          ALLOCATE(l_xSpxx(nb_xSpxx)%nb_vertex_bf(nb_xSxxx),stat=errare)
          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating l_xSpxx%nb_vertex_bf')
          END IF

          ALLOCATE(l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt),stat=errare)
          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating l_xSpxx%tface')
          END IF

          ALLOCATE(l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt),stat=errare)
          IF (errare /= 0) THEN
            CALL FATERR(IAM,'error allocating l_xSpxx%tface2face')
          END IF

          !fd on charge les facettes contenues dans idata
          nb_xSxxx=1
          nb_xSxxt=1
          i=0
          do
            nb_vertex_bf=l_idata(i+1)
            l_xSpxx(nb_xSpxx)%nb_vertex_bf(nb_xSxxx) = nb_vertex_bf
            l_xSpxx(nb_xSpxx)%face(:,nb_xSxxx) = 0
            l_xSpxx(nb_xSpxx)%face(1:nb_vertex_bf,nb_xSxxx)=l_idata(i+1+1:i+1+nb_vertex_bf)
            if (nb_vertex_bf == 4) then
              !fd on cree 2 faces (/1,2,3,4/) -> (/1,2,3/) + (/1,3,4/)
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+1)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+2)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+3)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+1)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+3)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+4)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
            elseif (nb_vertex_bf == 8) then
              !DA on cree 6 faces (/1,2,3,4,5,6,7,8/) -> (/1,5,8/) + (/5,2,6/) + (/6,3,7/) + 
              !DA                                        (/4,8,7/) + (/5,6,7/) + (/5,7,8/)
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+1)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+8)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+2)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+6)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+6)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+3)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+7)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+4)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+8)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+7)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+6)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+7)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+7)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+8)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
            elseif (nb_vertex_bf == 6) then
              !DA on cree 4 faces (/1,2,3,4,5,6/) -> (/1,4,6/) + (/4,2,5/) + (/6,5,3/) + (/6,4,5/)
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+1)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+4)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+6)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+4)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+2)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+6)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+3)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
              nb_xSxxt= nb_xSxxt + 1                       
              l_xSpxx(nb_xSpxx)%tface(1,nb_xSxxt)=l_idata(i+1+6)
              l_xSpxx(nb_xSpxx)%tface(2,nb_xSxxt)=l_idata(i+1+4)
              l_xSpxx(nb_xSpxx)%tface(3,nb_xSxxt)=l_idata(i+1+5)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
            else
              l_xSpxx(nb_xSpxx)%tface(1:nb_vertex_bf,nb_xSxxt)=l_idata(i+1+1:i+1+nb_vertex_bf)
              l_xSpxx(nb_xSpxx)%tface2face(nb_xSxxt) = nb_xSxxx
            endif
            i = i + 1 + nb_vertex_bf
            if ( i == idata_sz) exit
            if ( i > idata_sz) then
              CALL FATERR(IAM,'mismatch counting the xSpxx facets')
            endif
            nb_xSxxx= nb_xSxxx + 1           
            nb_xSxxt= nb_xSxxt + 1 
          enddo

          if (l_xSpxx(nb_xSpxx)%nb_xSxxx /= nb_xSxxx .or. &
              l_xSpxx(nb_xSpxx)%nb_tface /= nb_xSxxt) then
            CALL FATERR(IAM,'something strange in recovering faces')
          endif

          deallocate(l_idata)

          !fd on compte le nombre de noeuds locaux contenu dans le groupe de faces

          allocate(l_idata(8*l_xSpxx(nb_xSpxx)%nb_xSxxx))

          !print*,'rank ',nb_xSpxx,' type ',ttype,' nbS ',nb_xSxxx,' nbt ',nb_xSxxt
          
          l_idata = 0
          do f=1,l_xSpxx(nb_xSpxx)%nb_xSxxx
            do i=1,l_xSpxx(nb_xSpxx)%nb_vertex_bf(f)
              if (count(l_idata == l_xSpxx(nb_xSpxx)%face(i,f)) == 0 ) then
                l_idata(minloc(l_idata,dim=1)) = l_xSpxx(nb_xSpxx)%face(i,f)
              endif
            enddo
          enddo
          !print*,l_idata
          l_xSpxx(nb_xSpxx)%nb_vertex = count(l_idata > 0)
          !print*,'nbv ',l_xSpxx(nb_xSpxx)%nb_vertex

          !fd on conserve la liste de ces noeuds locaux

          allocate(l_xSpxx(nb_xSpxx)%vertex2node(l_xSpxx(nb_xSpxx)%nb_vertex))
          f=0
          do i=1,size(l_idata)
            if (l_idata(i) > 0) then
              f=f+1
              l_xSpxx(nb_xSpxx)%vertex2node(f)=l_idata(i)
            endif
          enddo

          deallocate(l_idata)

          !fd on renumerote les somments de tfaces avec l'index dans la liste des noeuds locaux

          do f=1,l_xSpxx(nb_xSpxx)%nb_tface
            do i=1,3
              idx=minloc(l_xSpxx(nb_xSpxx)%vertex2node, &
                         mask=l_xSpxx(nb_xSpxx)%vertex2node==l_xSpxx(nb_xSpxx)%tface(i,f), &
                         dim=1)
              l_xSpxx(nb_xSpxx)%tface(i,f)=idx
            enddo
          enddo

          write(mes,'(A)') 'The container '//T//'Spxx has:'
          call logmes(mes)
          write(mes, '(A2,I13,A10)') '  ',l_xSpxx(nb_xSpxx)%nb_vertex,'  vertices'
          call logmes(mes)
          write(mes, '(A2,I13,A18)') '  ',l_xSpxx(nb_xSpxx)%nb_xSxxx,'  faces (tri+quad)'
          call logmes(mes)
          write(mes, '(A2,I13,A18)') '  ',l_xSpxx(nb_xSpxx)%nb_tface,'  triangular faces'
          call logmes(mes)
          
          allocate(l_xSpxx(nb_xSpxx)%HE_Hdl)
 
          !print*,'creation new HE_Hdl',nb_xSpxx
          !print*,'nb vertex',l_xSpxx(nb_xSpxx)%nb_vertex
          !do i=1,l_xSpxx(nb_xSpxx)%nb_vertex
          !  print*,'sommet ',i,': ',' vertex reel ',l_xSpxx(nb_xSpxx)%vertex2node(i)
          !enddo
          !print*,'nb triangular faces ',l_xSpxx(nb_xSpxx)%nb_tface
          !do f=1,l_xSpxx(nb_xSpxx)%nb_tface
          !  print*,f,': ',l_xSpxx(nb_xSpxx)%tface(:,f)
          !enddo

          l_xSpxx(nb_xSpxx)%HE_Hdl = new_HE_Hdl(l_xSpxx(nb_xSpxx)%nb_vertex, &
                                                l_xSpxx(nb_xSpxx)%nb_tface, &
                                                max_adj_face)

          do f=1,l_xSpxx(nb_xSpxx)%nb_tface
             tface = l_xSpxx(nb_xSpxx)%tface(1:3,f)
             call settle_HE_Hdl(l_xSpxx(nb_xSpxx)%HE_Hdl, &
                  tface,err_)
             if (err_ > 0) then
               cout=''
               write(cout,'("xSpxx ",I0)') nb_xSpxx 
               call logmes(cout)
               call faterr(IAM,'unexpected error when calling settle_HE_Hdl')
             endif   
             
          enddo
          
          call build_HE_Hdl(l_xSpxx(nb_xSpxx)%HE_Hdl,err_)
         
          if (err_ > 0) then
            write(cout,*) 'contactor: ',nb_xSpxx
            call logmes(cout, .true.)
            call FATERR(IAM,'Can t create HE structure') 
          endif

          ! pour la gestion du contact
          allocate(l_xSpxx(nb_xSpxx)%tcoor(3,l_xSpxx(nb_xSpxx)%nb_vertex)) 
          if (with_facenormal) then
            allocate(l_xSpxx(nb_xSpxx)%tnormal(3,l_xSpxx(nb_xSpxx)%nb_tface)) 
          else 
            allocate(l_xSpxx(nb_xSpxx)%tnormal(3,l_xSpxx(nb_xSpxx)%nb_vertex)) 
          endif          
          l_xSpxx(nb_xSpxx)%tcoor = 0.d0
          l_xSpxx(nb_xSpxx)%tnormal = 0.d0

       endif
     END DO
   END DO

 END SUBROUTINE load_tactors_xSPxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine get_num_xSPxx(xSpxx,ixSxt,num,isize)
   IMPLICIT NONE 

   type(T_xSpxx) ,INTENT(in)   :: xSpxx 
   INTEGER       ,INTENT(in)   :: ixSxt
   integer       ,dimension(8) :: num
   INTEGER                     :: isize
   ! ***
                            !12345678901234
   CHARACTER(len=14) :: IAM='xSpxx::get_num'

   ! ***
   INTEGER :: i, ixSxx
   
   !fd on recupere la vraie face ...
   ixSxx = xSpxx%tface2face(ixSxt)

   !fd ... son nombre de noeuds ...
   isize = xSpxx%nb_vertex_bf(ixSxx) 

   num=0
   do i=1,isize
     num(i) =xSpxx%face(i,ixSxx)
   enddo

 end subroutine
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 

!\TODO: a mettre au propre

 subroutine get_interp_xSPxx(xSpxx,ixSxt,tweight,weight,isize)
   use algebra
   IMPLICIT NONE 
   type(T_xSpxx) ,INTENT(in)   :: xSpxx 
   INTEGER       ,INTENT(in)   :: ixSxt
   REAL(kind=8),dimension(3)   :: tweight
   REAL(kind=8),dimension(8)   :: weight
   INTEGER                     :: isize

   ! ***
                            !12345678901234567
   CHARACTER(len=17) :: IAM='xSpxx::get_interp'

   ! ***
   INTEGER :: i, ixSxx, ibdyty

   real(kind=8) :: coorq4(3,4),coort6(3,6),coorq8(3,8),coorn(3),coornn(3)
   character(len=108) :: cout

   integer :: err_
   
   ibdyty=xSpxx%ibdyty

   !fd on recupere la vraie face ...
   ixSxx = xSpxx%tface2face(ixSxt)

   !fd ... son nombre de noeuds ...
   isize = xSpxx%nb_vertex_bf(ixSxx) 

   !fd ... l'interpolation ...

   weight = 0.d0
   select case(isize) 
   case(3)
     weight(1:3) = tweight(1:3)
   case(4) 

     !fd ... pour le q4 on recalcule l interpolation ... 

     coorn=0.d0
     do i=1,3
       coorn = coorn + &
               tweight(i)*get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%vertex2node(xSpxx%tface(i,ixSxt)))
     enddo

     do i=1,4
       coorq4(:,i) = get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%face(i,ixSxx))
     enddo

     !print*,'zoooooooooooooooooooooooooooooooooooooooooooooooooooooooooob reac'
     !print*,ibdyty,iaspx,iasxx,iasxt
     !print*,'noeuds supports '
     !print*,l_Aspxx(iaspx)%vertex2node(l_ASpxx(iaspx)%tface(:,iasxt))
     !print*,'poids '
     !print*,tweight

     call compute_node_reduced_coor_in_linear_quadrangle(coorn,coorq4,weight, err_)
     if (err_ > 0) then
        call faterr('xSpxx::get_interp',' something wrong with linear_quadrangle')
     endif   

     coornn = weight(1)*coorq4(:,1)+weight(2)*coorq4(:,2)+weight(3)*coorq4(:,3)+weight(4)*coorq4(:,4)
     !> \todo: fd doit corriger (cette merde selon lui)
!!$     if (length3(coornn-coorn) > 1e-6) then
!!$       call logMes('bad projection')
!!$       write(cout,'(3(1x,D12.4))') coorn
!!$       call logMes(cout)
!!$       write(cout,'(3(1x,D12.4))') tweight
!!$       call logMes(cout)
!!$       write(cout,'(3(1x,D12.4))') coornn
!!$       call logMes(cout)
!!$       write(cout,'(8(1x,D12.4))') weight
!!$       call logMes(cout)
!!$       write(cout,*) '---'
!!$       call logMes(cout)
!!$     endif
   case(6) 

     !fd ... pour le t6 on recalcule l interpolation ... 

     coorn=0.d0
     do i=1,3
       coorn = coorn + &
               tweight(i)*get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%vertex2node(xSpxx%tface(i,ixSxt)))
     enddo

     do i=1,6
       coort6(:,i) = get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%face(i,ixSxx))
     enddo

     !print*,'zoooooooooooooooooooooooooooooooooooooooooooooooooooooooooob reac'
     !print*,ibdyty,iaspx,iasxx,iasxt
     !print*,'noeuds supports '
     !print*,l_Aspxx(iaspx)%vertex2node(l_ASpxx(iaspx)%tface(:,iasxt))
     !print*,'poids '
     !print*,tweight

     call FATERR(IAM,' not yet implemented')
     !! a faire call compute_node_reduced_coor_in_quadratic_triangle(coorn,coort6,weight)

   case(8) 

     !fd ... pour le q8 on recalcule l interpolation ... 

     coorn=0.d0
     do i=1,3
       coorn = coorn + &
               tweight(i)*get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%vertex2node(xSpxx%tface(i,ixSxt)))
     enddo

     do i=1,8
       coorq8(:,i) = get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%face(i,ixSxx))
     enddo

     !print*,'zoooooooooooooooooooooooooooooooooooooooooooooooooooooooooob reac'
     !print*,ibdyty,iaspx,iasxx,iasxt
     !print*,'noeuds supports '
     !print*,l_Aspxx(iaspx)%vertex2node(l_ASpxx(iaspx)%tface(:,iasxt))
     !print*,'poids '
     !print*,tweight

     !call FATERR(IAM,' not yet implemented')
     call compute_node_reduced_coor_in_quadratic_quadrangle(coorn,coorq8,weight,err_)
     if (err_ > 0) then
        call faterr('xSpxx::get_interp',' something wrong with quadratic_quadrangle')
     endif   

     
!     coornn = 0.D0
!     DO i = 1,8
!        coornn = coornn +  weight(i)*coorq8(:,i)
!     ENDDO

!      if (length3(coornn-coorn) > 1e-6) then
!        call logMes('bad projection')
!        write(cout,'(3(1x,D12.4))') coorn
!        call logMes(cout)
!        write(cout,'(3(1x,D12.4))') tweight
!        call logMes(cout)
!        write(cout,'(3(1x,D12.4))') coornn
!        call logMes(cout)
!        write(cout,'(8(1x,D12.4))') weight
!        call logMes(cout)
!        write(cout,*) '---'
!        call logMes(cout)
!~      endif

!~      call FATERR(IAM,' not yet implemented')
     !! a faire call compute_node_reduced_coor_in_quadratic_quadrangle(coorn,coorq8,weight)

   case default
     call FATERR(IAM,' should be 3 or 4 or 6 or 8')
   end select    

 end subroutine
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
subroutine get_coorTT_xSpxx(xSpxx,coorTT)
  IMPLICIT NONE

  type(T_xSpxx) ,INTENT(in)   :: xSpxx 
  REAL(kind=8),DIMENSION(:,:) :: coorTT

  INTEGER :: ibdyty
  INTEGER :: i

  !print*,'en entree',size(coorTT,dim=1),size(coorTT,dim=2)

  ibdyty= xSpxx%ibdyty

  !print*,'on recupere les coordonnees:'
  !print*,'appuye sur le corps:',ibdyty
  do i=1,xSpxx%nb_vertex
    !print*,'vertex :',i,l_Aspxx(iaspxx)%vertex2node(i)
    coorTT(:,i) = get_coorTT_nodty_mecaMAILx(ibdyty,xSpxx%vertex2node(i))
  ENDDO

END subroutine
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_xSxxx(xSpxx)

   IMPLICIT NONE
   type(T_xSpxx) ,INTENT(in)   :: xSpxx 
   INTEGER :: get_nb_xSxxx
  
   get_nb_xSxxx = xSpxx%nb_tface

 END FUNCTION get_nb_xSxxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_nb_vertex_xSpxx(xSpxx)

   IMPLICIT NONE
   type(T_xSpxx) ,INTENT(in)   :: xSpxx 
   INTEGER :: get_nb_vertex_xSpxx
  
   get_nb_vertex_xSpxx = xSpxx%nb_vertex

 END FUNCTION get_nb_vertex_xSpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION is_singleton_xSpxx(xSpxx)

   IMPLICIT NONE
   type(T_xSpxx) ,INTENT(in)   :: xSpxx 
   logical :: is_singleton_xSpxx
 
   is_singleton_xSpxx = .FALSE. 

   !fd a faire sur xSxxt ?
   if (xSpxx%nb_xSxxx == 1) is_singleton_xSpxx = .TRUE.

 END FUNCTION is_singleton_xSpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_HE_Hdl_xSpxx(xSpxx)

   IMPLICIT NONE
   type(T_xSpxx) ,INTENT(in)   :: xSpxx 
   type(T_HE_Hdl),pointer :: get_HE_Hdl_xSpxx
  
   get_HE_Hdl_xSpxx => xSpxx%HE_Hdl

 END FUNCTION get_HE_Hdl_xSpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE compute_nodal_resultant_of_constant_field(xSpxx,field,nr)

   !TODO faire le lien avec ce qui est en postraitement

   IMPLICIT NONE
   type(T_xSpxx) ,INTENT(in)         :: xSpxx 
   real(kind=8)                      :: field
   real(kind=8),dimension(:)         :: nr                
   real(kind=8),parameter            :: un_tiers=1.d0/3.d0, un_quart=1.d0/4.d0, un_sixieme=1.d0/6.d0, un_huitieme=1.d0/8.d0

   !***

   integer :: ixSxxx,i,j,ibdyty,in
   real(kind=8) :: val
   real(kind=8) :: field_3(3),coor_3(3,3),field_6(6),coor_6(3,6)
   real(kind=8) :: field_4(4),coor_4(3,4),field_8(8),coor_8(3,8)

   character(len=47) :: IAM
         !12345678901234567890123456789012345678901234567
   IAM = 'xSpxx:compute_nodal_resultant_of_constant_field'

   ibdyty = xSpxx%ibdyty

   nr = 0.d0
   do ixSxxx=1,xSpxx%nb_xSxxx

      ! calcul de la resultante aux noeuds d'un champs constant sur un element   
      ! avec INTEGRATE_field(field,NNOE,TYPE_FORME,TYPE_GAUSS,ndime,is_axi,X,res)

      select case(xSpxx%nb_vertex_bf(ixSxxx))
      case(3)

        field_3=field
        do i=1,3
           !in global
           in = xSpxx%face(i,ixSxxx)
           coor_3(:,i)=get_cooref_nodty_mecaMAILx(ibdyty,in)
        enddo

        call INTEGRATE_field(field_3,3,i_T_P1,i_TR03,3,.FALSE.,coor_3,val) 

        do i=1,3
          in = xSpxx%face(i,ixSxxx)
          j=maxloc(xSpxx%vertex2node,dim=1,mask=xSpxx%vertex2node == in)
          nr(j) = nr(j)+ (un_tiers*val)              
        enddo

      case(4)

        field_4=field
        do i=1,4
           !in global
           in = xSpxx%face(i,ixSxxx)
           coor_4(:,i)=get_cooref_nodty_mecaMAILx(ibdyty,in)
        enddo

        call INTEGRATE_field(field_4,4,i_Q_P1,i_Q2x2,3,.FALSE.,coor_4,val) 

        do i=1,4
          in = xSpxx%face(i,ixSxxx)
          j=maxloc(xSpxx%vertex2node,dim=1,mask=xSpxx%vertex2node == in)
          nr(j) = nr(j)+ (un_quart*val)              
        enddo

      case(6)

        field_6=field
        do i=1,6
           !in global
           in = xSpxx%face(i,ixSxxx)
           coor_6(:,i)=get_cooref_nodty_mecaMAILx(ibdyty,in)
        enddo

        call INTEGRATE_field(field_6,6,i_T_P2,i_TR03,3,.FALSE.,coor_6,val) 

        do i=1,6
          in = xSpxx%face(i,ixSxxx)
          j=maxloc(xSpxx%vertex2node,dim=1,mask=xSpxx%vertex2node == in)
          nr(j) = nr(j)+ (un_sixieme*val)              
        enddo

      case(8)
        
        field_8=field
        do i=1,8
           !in global
           in = xSpxx%face(i,ixSxxx)
           coor_8(:,i)=get_cooref_nodty_mecaMAILx(ibdyty,in)
        enddo

        call INTEGRATE_field(field_8,8,i_Q_P2,i_Q3x3,3,.FALSE.,coor_8,val) 

        do i=1,8
          in = xSpxx%face(i,ixSxxx)
          j=maxloc(xSpxx%vertex2node,dim=1,mask=xSpxx%vertex2node == in)
          nr(j) = nr(j)+ (un_huitieme*val)              
        enddo

      case default

         call faterr(IAM,'wrong number of point on the surfacic element')

      end select
   enddo

  end subroutine
!!!------------------------------------------------------------------------ 
  subroutine increment_xSpxx(xSpxx,with_facenormal)
    implicit none  
    type(T_xSpxx) ,INTENT(in) :: xSpxx 
    logical                   :: with_facenormal
    integer                   :: err_

    call update_HE_Hdl(xSpxx%HE_Hdl,xSpxx%tcoor,err_)
    if (err_ > 0) then
      call faterr('xSpxx::increment',' something wrong with HE')
    endif   
     
    if (with_facenormal) then
       call get_face_normals_HE_Hdl(xSpxx%HE_Hdl,xSpxx%tnormal,err_)
       if (err_ > 0) then
        call faterr('xSpxx::increment',' something wrong with HE face normals')
       endif   
       
    else
       call get_nodal_normals_HE_Hdl(xSpxx%HE_Hdl,xSpxx%tnormal,err_)
       if (err_ > 0) then
         call faterr('xSpxx::increment',' something wrong with HE nodal normals')
       endif   

    end if
  end subroutine

!!!------------------------------------------------------------------------ 
  function get_connec_xSpxx(l_xSpxx)
    implicit none
    type(T_xSpxx), dimension(:) :: l_xSpxx 
    integer, dimension(:)  , pointer :: get_connec_xSpxx
    !
    integer :: connec_size, i_xsp, i_face, nb_f
    integer :: offset, idx, csize, nb_xSpxx

    get_connec_xSpxx => null()

    nb_xSpxx = size(l_xSpxx)

    ! count to size connec
    connec_size = 1
    do i_xsp = 1, nb_xSpxx
      do i_face = 1, l_xSpxx(i_xsp)%nb_xSxxx
        connec_size = connec_size + l_xSpxx(i_xsp)%nb_vertex_bf(i_face) + 1
      end do
    end do

    allocate(get_connec_xSpxx(connec_size))

    ! need to remap MAILx numbering to 
    nb_f = 0
    idx  = 2
    do i_xsp = 1, nb_xSpxx
      do i_face = 1, l_xSpxx(i_xsp)%nb_xSxxx
        csize = l_xSpxx(i_xsp)%nb_vertex_bf(i_face)
        get_connec_xSpxx(idx) = csize
        get_connec_xSpxx(idx+1:idx+csize) = l_xSpxx(i_xsp)%face(1:csize,i_face)
        idx = idx + csize + 1
        nb_f = nb_f + 1
      end do
    end do
    get_connec_xSpxx(1) = nb_f

  end function

  subroutine get_all_data_xSpxx(l_xSpxx, idata, rdata, with_quad)
    implicit none
    type(T_xSpxx), dimension(:)            :: l_xSpxx 
    integer      , dimension(:,:), pointer :: idata
    real(kind=8) , dimension(:,:), pointer :: rdata
    logical :: with_quad
    !
    integer :: nb_xsp, nb_xs, i_xsp, idx, i_face
    integer :: i_node, i_bdyty, i_tri, isize

    nb_xsp = size(l_xSpxx)

    ! count to size
    nb_xs = 0
    do i_xsp = 1, nb_xsp
      nb_xs = nb_xs + l_xSpxx(i_xsp)%nb_xSxxx
    end do

    ! get normal of each ASx
    allocate(rdata(3,nb_xs))

    ! get i_bdyty, i_xsp and local as number
    if( with_quad ) then
      isize = 4
    else
      isize = 3
    end if
    allocate(idata(isize,nb_xs))

    idx  = 1
    do i_xsp = 1, nb_xsp
      ! must set coordinates of vertices
      i_bdyty = l_xSpxx(i_xsp)%ibdyty
      do i_node = 1, l_xSpxx(i_xsp)%nb_vertex
         l_xSpxx(i_xsp)%tcoor(:,i_node) = get_cooref_nodty_mecaMAILx(i_bdyty,l_xSpxx(i_xsp)%vertex2node(i_node))
      end do
      ! must re-compute normals
      call increment_xSpxx(l_xSpxx(i_xsp), .true.)
      ! set data value
      do i_face = 1, l_xSpxx(i_xsp)%nb_xSxxx
        idata( 1 ,idx) = i_bdyty
        idata( 2 ,idx) = i_xsp
        idata( 3 ,idx) = i_face
        if( with_quad ) then
          idata( 4 ,idx) = l_xSpxx(i_xsp)%quadrature
        end if
        ! look for triangle id to get normal
        do i_tri = 1, l_xSpxx(i_xsp)%nb_tface
          if( l_xSpxx(i_xsp)%tface2face(i_tri) == i_face ) then
            !print *, i_xsp, i_face, ' has triangle id ', i_tri
            exit
          end if
        end do
        ! normal to surface is normal to first node/tri ?
        if (l_xSpxx(i_xsp)%well_oriented) then
           rdata(1:3,idx) = l_xSpxx(i_xsp)%tnormal(:,i_tri)
        else
           rdata(1:3,idx) =-l_xSpxx(i_xsp)%tnormal(:,i_tri)
        endif
        idx = idx + 1
      end do
    end do

  end subroutine

  subroutine erase_xSpxx(patch)
    implicit none
    type(T_xSpxx) :: patch

    patch%ibdyty    = 0
    patch%nb_xSxxx  = 0
    patch%nb_vertex = 0  

    if( associated(patch%vertex2node) ) then
      deallocate(patch%vertex2node)
      nullify(patch%vertex2node)
    end if

    if( associated(patch%nb_vertex_bf) ) then
      deallocate(patch%nb_vertex_bf)
      nullify(patch%nb_vertex_bf)
    end if

    if( associated(patch%face) ) then
      deallocate(patch%face)
      nullify(patch%face)
    end if

    patch%is_precon     = .false.
    patch%well_oriented = .false.

    patch%nb_tface = 0
    if( associated(patch%tface) ) then
      deallocate(patch%tface)
      nullify(patch%tface)
    end if
    if( associated(patch%tface2face) ) then
      deallocate(patch%tface2face)
      nullify(patch%tface2face)
    end if

    if( associated(patch%HE_Hdl) ) then
      call erase_HE_Hdl(patch%HE_Hdl)
      deallocate(patch%HE_Hdl)
      nullify(patch%HE_Hdl)
    end if

    if( associated(patch%weight) ) then
      deallocate(patch%weight)
      nullify(patch%weight)
    end if
    if( associated(patch%tcoor) ) then
      deallocate(patch%tcoor)
      nullify(patch%tcoor)
    end if
    if( associated(patch%tnormal) ) then
      deallocate(patch%tnormal)
      nullify(patch%tnormal)
    end if

 end subroutine

 END MODULE xSpxx



