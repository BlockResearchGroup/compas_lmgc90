
!! logical :: draw_gmv = .false.

!!contains

!! RIP GMV: bit of code inside the function
!!          dedicated to gmv file writing
!! function compute_cundall_common_plane()
!!
!!! !--------------------------
!!! ! pour afficher ce qu'on a calcule

!!! if (m > cundall_iter .and. draw_gmv) then

!!!   ! on essaie de comprendre ce qu'il se passe

!!!   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!   do i=1,PRcd%nb_vertex
!!!     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!   enddo

!!!   do i=1,PRan%nb_vertex
!!!     v_coor_an(i,:) = PRan%vertex(:,i)
!!!   enddo

!!!   rd= 0.5*(PRcd%radius+PRan%radius)

!!!   v_coor_ptc(1,:) = center(:) + rd*(- t(:) - s(:))     
!!!   v_coor_ptc(2,:) = center(:) + rd*(+ t(:) - s(:))   
!!!   v_coor_ptc(3,:) = center(:) + rd*(+ t(:) + s(:))  
!!!   v_coor_ptc(4,:) = center(:) + rd*(- t(:) + s(:))  

!!!   call gmv_draw(PRcd%id,PRan%id, &
!!!                 PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!                 PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!                 4,v_coor_ptc(1:4,1:3))

!!!   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)

!!! endif
!! end function compute_cundall_common_plane

!! function compute_cundall_common_plane_old()
!!!--------------------------
!!! pour afficher ce qu'on a calcule

!!!! if (m > cundall_iter .and. draw_gmv) then      

!!!!   ! on essaie de comprendre ce qu'il se passe

!!!!   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!!   do i=1,PRcd%nb_vertex
!!!!     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!   enddo

!!!!   do i=1,PRan%nb_vertex
!!!!     v_coor_an(i,:) = PRan%vertex(:,i)
!!!!   enddo

!!!!   rd= 0.5*(PRcd%radius+PRan%radius)

!!!!   v_coor_ptc(1,:) = center(:) + rd*(- t(:) - s(:))     
!!!!   v_coor_ptc(2,:) = center(:) + rd*(+ t(:) - s(:))   
!!!!   v_coor_ptc(3,:) = center(:) + rd*(+ t(:) + s(:))  
!!!!   v_coor_ptc(4,:) = center(:) + rd*(- t(:) + s(:))  

!!!!   call gmv_draw(PRcd%id,PRan%id, &
!!!!                 PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!                 PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!                 4,v_coor_ptc(1:4,1:3))

!!!!   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)

!!!! endif

!! end function compute_cundall_common_plane_old





!! RIP GMV: bit of code inside the function
!!          dedicated to gmv file writing
!! function compute_f2f_common_plane()
!!!! if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then 

!!!!   ! on essaie de comprendre ce qu'il se passe pour le couple (idcd,idan) 
!!!!   ! histoire de ne pas gerer des structures de donnees a la noix 
!!!!   ! on passe tout au format stl

!!!!   allocate (v_coor_cd(3*size(PRcd%f2f_set(fi)%G_i),3),&
!!!!   v_coor_an(3*size(PRan%f2f_set(fj)%G_i),3))

!!!!   do i=1,size(PRcd%f2f_set(fi)%G_i)
!!!!     v_coor_cd(3*(i-1)+1,:) = PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+2,:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+3,:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))
!!!!   enddo

!!!!   do i=1,size(PRan%f2f_set(fj)%G_i)
!!!!     v_coor_an(3*(i-1)+1,:) = PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+2,:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+3,:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(i)))
!!!!   enddo

!!!!   call f2f_gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!                     polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!                     size(PRcd%f2f_set(fi)%G_i),v_coor_cd, &
!!!!                     size(PRan%f2f_set(fj)%G_i),v_coor_an )
!!!          
!!!!   deallocate(v_coor_cd,v_coor_an)

!!!! endif

!!!! if (nstep == 1) then
!!!!   allocate (v_coor_cd(3*size(PRcd%f2f_set(fi)%G_i),3),&
!!!!             v_coor_an(3*size(PRan%f2f_set(fj)%G_i),3))

!!!!   do i=1,size(PRcd%f2f_set(fi)%G_i)
!!!!     v_coor_cd(3*(i-1)+1,:) = PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+2,:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+3,:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))
!!!!   enddo

!!!!   do i=1,size(PRan%f2f_set(fj)%G_i)
!!!!     v_coor_an(3*(i-1)+1,:) = PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+2,:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+3,:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(i)))
!!!!   enddo

!!!!   call f2f_gmv_draw_all(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!                         polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!                         size(PRcd%f2f_set(fi)%G_i),v_coor_cd, &
!!!!                         size(PRan%f2f_set(fj)%G_i),v_coor_an )
!!!          
!!!!   deallocate(v_coor_cd,v_coor_an)
!!!! endif

!!!! [...]

!!!!!--------------------------
!!!!! pour afficher ce qu'on a calcule
!!!!!
!!!! if (draw_gmv) then

!!!!   ! on essaie de comprendre ce qu'il se passe

!!!!   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!!   do i=1,PRcd%nb_vertex
!!!!     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!   enddo

!!!!   do i=1,PRan%nb_vertex
!!!!     v_coor_an(i,:) = PRan%vertex(:,i)
!!!!   enddo

!!!!   rd= 0.5*(PRcd%radius+PRan%radius)


!!!!   v_coor_ptc(1,:) = center(:) + rd*(- t(:) - s(:))     
!!!!   v_coor_ptc(2,:) = center(:) + rd*(+ t(:) - s(:))   
!!!!   v_coor_ptc(3,:) = center(:) + rd*(+ t(:) + s(:))  
!!!!   v_coor_ptc(4,:) = center(:) + rd*(- t(:) + s(:))  


!!!!   call gmv_draw(PRcd%Id,PRan%Id, &
!!!!                 PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!                 PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!                 4,v_coor_ptc(1:4,1:3))

!!!!   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)

!!!! endif
!!
!! end function compute_f2f_common_plane

!! RIP GMV: bit of code inside the function
!!          dedicated to gmv file writing
!!!!subroutine proj_f2f(PRcd,PRan,an_shift, &
!!!!   ! if (draw_gmv) then

!!!!   !   !print*,'Ensemble des points de contact entre ces deux POLYR'
!!!!   !   !do ix =1,nb_ctc
!!!!   !   !   write(*,'(i0,3(1x,D12.5))') ix,pt_ctc(:,ix)
!!!!   !   !enddo
!!!!   !   ! on essaie de comprendre ce qu'il se passe

!!!!   !   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!!   !   do i=1,PRcd%nb_vertex
!!!!   !     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!   !   enddo

!!!!   !   do i=1,PRan%nb_vertex
!!!!   !    v_coor_an(i,:) = PRan%vertex(:,i)
!!!!   !   enddo

!!!!   !   ! pour voir le point qui fait chier 

!!!!   !   DO ix=1,4
!!!!   !     if (ix <= nb_ctc) then
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,ix)
!!!!   !     else
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,1)
!!!!   !     endif
!!!!   !   enddo

!!!!   !   call e_gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!   !                   polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!   !                   PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!   !                   PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!   !                   4,v_coor_ptc(1:4,1:3))

!!!!   !   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!!!   ! endif
!!!!
!!!!   [...]
!!!!
!!!!   ! if (draw_gmv) then
!!!!   !   ! on essaie de comprendre ce qu'il se passe

!!!!   !   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!!   !   do i=1,PRcd%nb_vertex
!!!!   !     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!   !   enddo

!!!!   !   do i=1,PRan%nb_vertex
!!!!   !    v_coor_an(i,:) = PRan%vertex(:,i)
!!!!   !   enddo

!!!!   !   DO ix=1,4
!!!!   !     if (ix <= nb_ctc) then
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,ix)
!!!!   !     else
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,1)
!!!!   !     endif
!!!!   !   enddo

!!!!   !   call e_gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!   !                   polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!   !                   PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!   !                   PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!   !                   4,v_coor_ptc(1:4,1:3))

!!!!   !   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!!!   ! endif
!!!!end subroutine proj_f2f


!! RIP GMV: bit of code inside the function
!!          dedicated to gmv file writing
!!!!subroutine planplan_countour

!!!! if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then 
!!!!   ! on essaie de comprendre ce qu'il se passe pour le couple (idcd,idan) 
!!!!   ! histoire de ne pas gerer des structures de donnees a la noix 
!!!!   ! on passe tout au format stl

!!!!   allocate (v_coor_cd(3*size(PRcd%f2f_set(fi)%G_i),3),&
!!!!   v_coor_an(3*size(PRan%f2f_set(fj)%G_i),3))

!!!!   do i=1,size(PRcd%f2f_set(fi)%G_i)
!!!!     v_coor_cd(3*(i-1)+1,:) = PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+2,:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+3,:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))
!!!!   enddo

!!!!   do i=1,size(PRan%f2f_set(fj)%G_i)
!!!!     v_coor_an(3*(i-1)+1,:) = PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+2,:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+3,:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(i)))
!!!!   enddo

!!!!   call f2f_gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!                     polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!                     size(PRcd%f2f_set(fi)%G_i),v_coor_cd, &
!!!!                     size(PRan%f2f_set(fj)%G_i),v_coor_an )
!!!          
!!!!   deallocate(v_coor_cd,v_coor_an)

!!!! endif


!!!! if (nstep == 1) then
!!!!   allocate (v_coor_cd(3*size(PRcd%f2f_set(fi)%G_i),3),&
!!!!             v_coor_an(3*size(PRan%f2f_set(fj)%G_i),3))

!!!!   do i=1,size(PRcd%f2f_set(fi)%G_i)
!!!!     v_coor_cd(3*(i-1)+1,:) = PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+2,:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))
!!!!     v_coor_cd(3*(i-1)+3,:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))
!!!!   enddo

!!!!   do i=1,size(PRan%f2f_set(fj)%G_i)
!!!!     v_coor_an(3*(i-1)+1,:) = PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+2,:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(i)))
!!!!     v_coor_an(3*(i-1)+3,:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(i)))
!!!!   enddo

!!!!   call f2f_gmv_draw_all(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!                        polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!                        size(PRcd%f2f_set(fi)%G_i),v_coor_cd, &
!!!!                       size(PRan%f2f_set(fj)%G_i),v_coor_an )
!!!          
!!!!   deallocate(v_coor_cd,v_coor_an)

!!!! endif
!!!!
!!!! [...]
!!!!
!!!!--------------------------
!!!! pour afficher ce qu'on a calcule
!!!!
!!!
!!!!    if (draw_gmv) then
!!!
!!!! ! on essaie de comprendre ce qu'il se passe
!!!
!!!!      allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!!
!!!!      do i=1,PRcd%nb_vertex
!!!!        v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!      enddo
!!!
!!!!      do i=1,PRan%nb_vertex
!!!!        v_coor_an(i,:) = PRan%vertex(:,i)
!!!!      enddo
!!!
!!!!      rd= 0.5*(S_POLYR(PRan%id)%radius+S_POLYR(PRcd%id)%radius)
!!!
!!!!      v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!!!      v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!!!      v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!!!      v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!!
!!!
!!!!      call gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!                    PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!                    PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!                    4,v_coor_ptc(1:4,1:3))
!!!
!!!!      deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!!
!!!!    endif
!!!!
!!!! [...]
!!!!
!!!!   !print*,'Ensemble des points de contact entre ces deux POLYR'
!!!!   !do ix =1,nb_ctc
!!!!   !   write(*,'(i0,3(1x,D12.5))') ix,pt_ctc(:,ix)
!!!!   !enddo
!!!!   ! on essaie de comprendre ce qu'il se passe

!!!!   ! if (draw_gmv) then

!!!!   !   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!!   !   do i=1,PRcd%nb_vertex
!!!!   !     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!   !   enddo

!!!!   !   do i=1,PRan%nb_vertex
!!!!   !    v_coor_an(i,:) = PRan%vertex(:,i)
!!!!   !   enddo

!!!!   !   ! pout voir le point qui fait chier 
!
!!!!   !   DO ix=1,4
!!!!   !     if (ix <= nb_ctc) then
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,ix)
!!!!   !     else
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,1)
!!!!   !     endif
!!!!   !   enddo

!!!!   !   call e_gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!   !                   polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!   !                   PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!   !                   PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!   !                   4,v_coor_ptc(1:4,1:3))

!!!!   !   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)

!!!!   ! endif
!!!!
!!!! [...]
!!!!
!!!!   !fd on essaie de gerer ce cas en virant le noeud
!!!!   !fd nb_ctc = 0
!!!!   !fd return
!!!!   ! if (draw_gmv) then

!!!!   !   ! on essaie de comprendre ce qu'il se passe
!!
!!!!   !   allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))

!!!!   !   do i=1,PRcd%nb_vertex
!!!!   !     v_coor_cd(i,:) = PRcd%vertex(:,i)
!!!!   !   enddo

!!!!   !   do i=1,PRan%nb_vertex
!!!!   !    v_coor_an(i,:) = PRan%vertex(:,i)
!!!!   !   enddo

!!!!   !   DO ix=1,4
!!!!   !     if (ix <= nb_ctc) then
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,ix)
!!!!   !     else
!!!!   !       v_coor_ptc(ix,:) = pt_ctc(:,1)
!!!!   !     endif
!!!!   !   enddo

!!!!   !   call e_gmv_draw(polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &
!!!!   !                   polyr2bdyty(2,PRcd%id),polyr2bdyty(2,PRan%id), &
!!!!   !                   PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!!!   !                   PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!!!   !                   4,v_coor_ptc(1:4,1:3))

!!!!   !   deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!!!   ! endif
!!!!end subroutine planplan_countour

!------------------------------------------------------------------------
!-------------- routines de visu --------
!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
!   subroutine gmv_draw(icd,ian, &
!        nb_pt_cd,coor_cd,nb_face_cd,face_cd, &
!        nb_pt_an,coor_an,nb_face_an,face_an, &
!        nb_ptc,coor_ptc)

!     implicit none

!     integer :: icd,ian
!     integer :: nb_pt_cd,nb_face_cd,nb_pt_an,nb_face_an,nb_ptc
!     real(kind=8),dimension(nb_pt_cd,3)   :: coor_cd
!     integer     ,dimension(3,nb_face_cd) :: face_cd
!     real(kind=8),dimension(nb_pt_an,3)   :: coor_an 
!     integer     ,dimension(3,nb_face_an) :: face_an
!     real(kind=8),dimension(nb_ptc,3)     :: coor_ptc   

!     integer :: nfich
!     integer :: i

!     character(len=20) :: nom,step

!     nfich=get_io_unit()

!     nom =' '
!     step=' '
!     write(nom,'("_ctc_",I0,"_",I0)') icd,ian
!     write(step,'("_stp_",I0)') nstep
!     open(nfich,file='visu'//trim(nom)//trim(step)//'.txt')

!     write(nfich,'("gmvinput ascii")')
!     !
!     write(nfich,'("nodes 0")')
!     !
!     write(nfich,'("cells 0")')
!     !
!     write(nfich,'("polygons")')
!     do i=1,nb_face_cd
!        write(nfich,'(2(1x,I5))') 1,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_cd(face_cd(:,i),1)
!        write(nfich,'(8(1x,E12.5))') coor_cd(face_cd(:,i),2)
!        write(nfich,'(8(1x,E12.5))') coor_cd(face_cd(:,i),3)
!     enddo
!     do i=1,nb_face_an
!        write(nfich,'(2(1x,I5))') 2,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_an(face_an(:,i),1)
!        write(nfich,'(8(1x,E12.5))') coor_an(face_an(:,i),2)
!        write(nfich,'(8(1x,E12.5))') coor_an(face_an(:,i),3)
!     enddo
!     write(nfich,'(2(1x,I5))') 3,4 !face a 3 noeuds
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,1)
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,2)
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,3)
!     write(nfich,'("endpoly")')
!     !
! !!$  write(nfich,'("tracers",1x,I1)') nb_ptc
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,1)
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,2)
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,3)
! !!$  write(nfich,'("rien")')
! !!$  write(nfich,'(8(1x,I5))') (i-1,i=1,nb_ptc)
! !!$  write(nfich,'("endtrace")')
!     !
!     write(nfich,'("endgmv")')  
!     !
!     close(nfich)
!   end subroutine gmv_draw
!!!------------------------------------------------------------------------
!   subroutine e_gmv_draw(icd,ian,icdtac,iantac, &
!        nb_pt_cd,coor_cd,nb_face_cd,face_cd, &
!        nb_pt_an,coor_an,nb_face_an,face_an, &
!        nb_ptc,coor_ptc)

!     implicit none

!     integer :: icd,ian,icdtac,iantac
!     integer :: nb_pt_cd,nb_face_cd,nb_pt_an,nb_face_an,nb_ptc
!     real(kind=8),dimension(nb_pt_cd,3)   :: coor_cd
!     integer     ,dimension(3,nb_face_cd) :: face_cd
!     real(kind=8),dimension(nb_pt_an,3)   :: coor_an 
!     integer     ,dimension(3,nb_face_an) :: face_an
!     real(kind=8),dimension(nb_ptc,3)     :: coor_ptc   

!     integer :: nfich
!     integer :: i

!     character(len=20) :: nom,step

!     nfich=get_io_unit()

!     nom =' '
!     step=' '
!     write(nom,'("_ctc_",I0,"@",I0,"_",I0,"@",I0)') icdtac,icd,iantac,ian
!     write(step,'("_stp_",I0)') nstep
!     open(nfich,file='visu'//trim(nom)//trim(step)//'.txt')

!     write(nfich,'("gmvinput ascii")')
!     !
!     write(nfich,'("nodes 0")')
!     !
!     write(nfich,'("cells 0")')
!     !
!     write(nfich,'("polygons")')
!     do i=1,nb_face_cd
!        write(nfich,'(2(1x,I5))') 1,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_cd(face_cd(:,i),1)
!        write(nfich,'(8(1x,E12.5))') coor_cd(face_cd(:,i),2)
!        write(nfich,'(8(1x,E12.5))') coor_cd(face_cd(:,i),3)
!     enddo
!     do i=1,nb_face_an
!        write(nfich,'(2(1x,I5))') 2,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_an(face_an(:,i),1)
!        write(nfich,'(8(1x,E12.5))') coor_an(face_an(:,i),2)
!        write(nfich,'(8(1x,E12.5))') coor_an(face_an(:,i),3)
!     enddo
!     write(nfich,'(2(1x,I5))') 3,4 !face a 3 noeuds
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,1)
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,2)
!     write(nfich,'(8(1x,E12.5))') coor_ptc(1:nb_ptc,3)
!     write(nfich,'("endpoly")')
!     !
! !!$  write(nfich,'("tracers",1x,I1)') nb_ptc
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,1)
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,2)
! !!$  write(nfich,'(8(1x,E12.5))') coor_ptc(:,3)
! !!$  write(nfich,'("rien")')
! !!$  write(nfich,'(8(1x,I5))') (i-1,i=1,nb_ptc)
! !!$  write(nfich,'("endtrace")')
!     !
!     write(nfich,'("endgmv")')  
!     !
!     close(nfich)
!   end subroutine e_gmv_draw
! !!!------------------------------------------------------------------------
!   subroutine f2f_gmv_draw(icd,ian,icdtac,iantac, &
!        nb_face_cd,coor_cd,&
!        nb_face_an,coor_an )

!     implicit none

!     integer :: icd,ian,icdtac,iantac
!     integer :: nb_face_cd,nb_face_an
!     real(kind=8),dimension(3*nb_face_cd,3)   :: coor_cd
!     real(kind=8),dimension(3*nb_face_an,3)   :: coor_an 

!     integer :: nfich
!     integer :: i

!     character(len=20) :: nom,step

!     nfich=get_io_unit()

!     nom =' '
!     step=' '
!     write(nom,'("_ctc_",I0,"@",I0,"_",I0,"@",I0)') icdtac,icd,iantac,ian
!     write(step,'("_stp_",I0)') nstep
!     open(nfich,file='f2f'//trim(nom)//trim(step)//'.txt')

!     write(nfich,'("gmvinput ascii")')
!     !
!     write(nfich,'("nodes 0")')
!     !
!     write(nfich,'("cells 0")')
!     !
!     write(nfich,'("polygons")')
!     do i=1,nb_face_cd
!        write(nfich,'(2(1x,I5))') 1,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_cd(3*(i-1)+1 : 3*(i-1)+3,1)
!        write(nfich,'(8(1x,E12.5))') coor_cd(3*(i-1)+1 : 3*(i-1)+3,2)
!        write(nfich,'(8(1x,E12.5))') coor_cd(3*(i-1)+1 : 3*(i-1)+3,3)
!     enddo
!     do i=1,nb_face_an
!        write(nfich,'(2(1x,I5))') 2,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_an(3*(i-1)+1 : 3*(i-1)+3,1)
!        write(nfich,'(8(1x,E12.5))') coor_an(3*(i-1)+1 : 3*(i-1)+3,2)
!        write(nfich,'(8(1x,E12.5))') coor_an(3*(i-1)+1 : 3*(i-1)+3,3)
!     enddo
!     write(nfich,'("endpoly")')
!     !
!     write(nfich,'("endgmv")')  
!     !
!     close(nfich)
!   end subroutine f2f_gmv_draw
! !!!------------------------------------------------------------------------
!   subroutine f2f_gmv_draw_all(icd,ian,icdtac,iantac, &
!                               nb_face_cd,coor_cd,&
!                               nb_face_an,coor_an )

!     implicit none

!     integer :: icd,ian,icdtac,iantac
!     integer :: nb_face_cd,nb_face_an
!     real(kind=8),dimension(3*nb_face_cd,3)   :: coor_cd
!     real(kind=8),dimension(3*nb_face_an,3)   :: coor_an 

!     integer :: nfich
!     integer :: i

!     logical :: is_first_time = .true.

!     nfich=get_io_unit()

!     if (is_first_time) then
!       is_first_time = .false.
!       open(nfich,file=location('f2f.txt'),status='replace')      
!       write(nfich,'("gmvinput ascii")')
!       !
!       write(nfich,'("nodes 0")')
!       !
!       write(nfich,'("cells 0")')
!       !
!       write(nfich,'("polygons")')
!     else
!       open(nfich,file=location('f2f.txt'),status='old',position='append')      
!       backspace(nfich) !endgmv
!       backspace(nfich) !endpoly
!     endif

!     do i=1,nb_face_cd
!        write(nfich,'(2(1x,I5))') 1,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_cd(3*(i-1)+1 : 3*(i-1)+3,1)
!        write(nfich,'(8(1x,E12.5))') coor_cd(3*(i-1)+1 : 3*(i-1)+3,2)
!        write(nfich,'(8(1x,E12.5))') coor_cd(3*(i-1)+1 : 3*(i-1)+3,3)
!     enddo
!     do i=1,nb_face_an
!        write(nfich,'(2(1x,I5))') 2,3 !face a 3 noeuds
!        write(nfich,'(8(1x,E12.5))') coor_an(3*(i-1)+1 : 3*(i-1)+3,1)
!        write(nfich,'(8(1x,E12.5))') coor_an(3*(i-1)+1 : 3*(i-1)+3,2)
!        write(nfich,'(8(1x,E12.5))') coor_an(3*(i-1)+1 : 3*(i-1)+3,3)
!     enddo
!     write(nfich,'("endpoly")')
!     !
!     write(nfich,'("endgmv")')  
!     !
!     close(nfich)
!   end subroutine f2f_gmv_draw_all





!!$!------------------------------------------------------------------------
!!$!fd  OBSOLETE
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE DETECTION_F2F(id1,id2,Nsep,adist,nb_ctc,PT_CTC,overlap,Nr,t,s,v2v)
!!$
!!$! I
!!$! id1 : id solide 1 (antagoniste)
!!$! id2 : id solide 2 (candidat)
!!$! Nsep: est la derniere normale au plan separateur connue 
!!$!       l'intercentre si on fait une recherche rough a chaque pas
!!$!
!!$! O 
!!$! nb_ctct           : nombre de points de contact
!!$! PT_CTC(1:nb_ctc)  : coordonnees des points de contact
!!$! Nr(1)             : normales aux points de contact si il y a contact (bizarre bizarrre)
!!$! r_cd,r_an         : qui est le vrai candidat et le vrai antagoniste ... comme en entre
!!$! overlap(1:nb_ctc) : les gaps aux points de contact
!!$! Nsep : normale au plan separateur actualisee ou ancienne
!!$
!!$
!!$   IMPLICIT NONE
!!$
!!$   INTEGER,PARAMETER                :: nb_ptc_max=100
!!$   INTEGER                          :: id1,id2,i,k,j,m,inc,inc1,nb_stored,i1,i2
!!$   REAL(kind=8)                     :: dist1,dist2,norm,norm1,norm2,scal
!!$   REAL(kind=8),DIMENSION(3)        :: S,PT,s1,s2,s3,r1,r2,r3,Nr,Nsep,Nsep_o,PC
!!$   REAL(kind=8)                     :: d1,gap
!!$   INTEGER                          :: nb_ptc1,nb_ptc2,nb_ctc 
!!$
!!$   REAL(kind=8),DIMENSION(3,4)             :: PT_CTC
!!$   REAL(kind=8),DIMENSION(4)               :: overlap
!!$  
!!$   REAL(kind=8),DIMENSION(3,nb_ptc_max)    :: PT1,PT2
!!$
!!$   INTEGER,DIMENSION(2*nb_ptc_max)         :: is_ok
!!$   REAL(kind=8),DIMENSION(5,nb_ptc_max)    :: PT1_loc,PT2_loc  !infos liees au contact [gap:normale]
!!$
!!$   TYPE(T_POLYR)                    :: PRan,PRcd
!!$   INTEGER                          :: errare,nb_select
!!$   REAL(kind=8),DIMENSION(3)        :: Ncd
!!$ 
!!$   REAL(kind=8)                     :: norm_max,norm_min,scal1,scal2,adist,scal_min
!!$
!!$   LOGICAL :: bavard=.false. !.false.
!!$
!!$!rp @@@@ new
!!$   INTEGER                          :: nb_vertex_pran_min,i_max
!!$   INTEGER                          :: nb_vertex_prcd_min,j_max
!!$
!!$   REAL(kind=8)                     :: denom,num,ss,tt
!!$   REAL(kind=8)                     :: max_theta
!!$
!!$
!!$   REAL(kind=8),DIMENSION(3)        :: mid_P,p1,p2,mid_normal,max_P1,listsup
!!$   REAL(kind=8),DIMENSION(3)        :: s1moy,s2moy,s3moy,r1moy,r2moy,r3moy
!!$
!!$
!!$   REAL(kind=8),DIMENSION(:,:), ALLOCATABLE   :: Pcd,Pan,rr1,ss1   
!!$!fd pas utile   REAL(kind=8),DIMENSION(:),   ALLOCATABLE   :: theta
!!$   REAL(kind=8)                    :: theta
!!$
!!$   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: cd_skip,an_skip
!!$
!!$!rp @@@@ Special plan moyen
!!$
!!$   INTEGER                        :: kk,imax1,jmin1,jmin,imax,jmax1,jmax
!!$
!!$   REAL(kind=8)                   :: d_an,d_cd,d_min,d_max,G_ini,G_ini_0,angle,angle_min,angle_max,c,si,crit
!!$
!!$   REAL(kind=8),DIMENSION(3,4)    :: normal
!!$
!!$   REAL(kind=8),DIMENSION(3)      :: t,t_temp,n_ini,vec
!!$
!!$   REAL(kind=8),DIMENSION(4)      :: G_max
!!$   integer,DIMENSION(4)           :: all_min
!!$   integer,DIMENSION(4)           :: all_max
!!$
!!$   INTEGER :: irot
!!$
!!$   REAL(kind=8),DIMENSION(2)      :: lvec
!!$
!!$!fd 
!!$
!!$   REAL(kind=8)                   :: tol = 1.d-10,rd
!!$   integer                        :: dump_fich
!!$
!!$                               !123456789012345678901234567890123
!!$   CHARACTER(len=33)  :: IAM = 'mod_PRPRx::detection_common_plane'
!!$
!!$
!!$   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
!!$
!!$   character(len=11) :: nom
!!$
!!$   logical :: is_reverse=.FALSE., draw_gmv=.false.
!!$
!!$   integer :: ii
!!$   real(kind=8) :: ref_size,zero,dir(2),max_norm,dt,c_center(2),c_vec(2)
!!$
!!$
!!$   ! f2f
!!$   real(kind=8) :: xcd_min, xcd_max, ycd_min , ycd_max
!!$   real(kind=8) :: xan_min, xan_max, yan_min , yan_max
!!$   real(kind=8) :: pp(3,3)
!!$   logical :: is_inside 
!!$
!!$  ! f2f
!!$   integer :: fi,fj,iff,my_cd_iff,my_an_iff,ic
!!$   logical :: f2f_found
!!$
!!$   type(T_visavis) :: v2v
!!$
!!$   zero = 1.d-20
!!$   dt = 0.01 ! tol angle
!!$
!!$
!!$!fd pas de surprise si j'initialise (dicton du jour le plus con) 
!!$
!!$   nb_ctc=0
!!$
!!$   Nr = 0.d0
!!$   t  = 0.d0
!!$   s  = 0.d0
!!$
!!$   Nsep_o = Nsep 
!!$
!!$!
!!$   irot=0
!!$
!!$   PRan    = S_POLYR(id1)
!!$   PRcd    = S_POLYR(id2)
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'======================='
!!$     PRINT*,'Detection entre le POLYR cd',id2,' et le POLYR an',id1
!!$     PRINT*,'sep',Nsep
!!$     PRINT*,'vertex de cd:',id2 
!!$     DO i=1,PRcd%nb_vertex
!!$       PRINT*,PRcd%vertex(:,i)
!!$     ENDDO
!!$     PRINT*,'vertex de an:',id1 
!!$     DO i=1,PRan%nb_vertex
!!$       PRINT*,PRan%vertex(:,i)+perio_shift(:)
!!$     ENDDO
!!$   ENDIF
!!$
!!$!fd @@@@@@@@@@@@@@@@@@@@@@@@@@ phase de shadow overlap @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                                begin                                       @@@
!!$!fd @@@
!!$!fd @@@ s-o initialisation de Nr sur la valeur de Nsep                             @@@
!!$!fd @@@   "ligne des centres"|"ancienne normale au plan separateur"                @@@
!!$
!!$   dist1 = -1.D+20
!!$   dist2 =  1.D+20
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 1 vers 2
!!$   !
!!$
!!$   DO i=1,PRan%nb_vertex
!!$     scal= DOT_PRODUCT((PRan%vertex(:,i)+perio_shift(:)),Nsep_o(:))
!!$     IF (scal > dist1) THEN
!!$       dist1=scal
!!$     ENDIF
!!$   ENDDO
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 2 vers 1
!!$   !
!!$
!!$   DO i=1,PRcd%nb_vertex
!!$     scal= DOT_PRODUCT(PRcd%vertex(:,i),Nsep_o(:))
!!$     IF (scal < dist2) THEN
!!$       dist2=scal
!!$     ENDIF
!!$   ENDDO 
!!$
!!$!fd @@@ si le s-o sur Nsep ne passe pas on sort 
!!$
!!$   IF ((dist2 - dist1) > adist) RETURN
!!$
!!$!   print*,'Ca passe le shadow overlap sur l''intercentre'
!!$!   print*,'Critere = ',dist1 - dist2
!!$
!!$   gap=dist2-dist1
!!$   nb_shadow  = nb_shadow + 1
!!$
!!$   scal       = 0.D0
!!$
!!$!fd @@@ ... s-o sur les normales ...
!!$!fd @@@ on en profite pour ne rendre valide que les vertex
!!$!fd @@@ appartenant a des faces licites
!!$!fd @@@ a l'initialisation aucun vertex valide
!!$
!!$   ALLOCATE(cd_skip(PRcd%nb_vertex),an_skip(PRan%nb_vertex))
!!$   cd_skip = 0
!!$   an_skip = 0
!!$
!!$   DO i=1,PRan%nb_faces
!!$
!!$      if (bavard) Then
!!$        print*,'corps an - face ',i,'normale',PRan%normal(:,i)
!!$      endif
!!$
!!$!fd @@@ si la normale a la face n'est pas orientee sur la direction Nsep_o on jarte
!!$!fd @@@ N.B: au premier pas c'est la ligne des centres, apres c'est la derniere direction separatrice
!!$   
!!$      scal1=DOT_PRODUCT(PRan%normal(:,i),Nsep_o(:))
!!$
!!$      IF (scal1 < -0.0001D0) THEN
!!$        if (bavard) print*,'on exclue cette face'
!!$        CYCLE
!!$      ENDIF
!!$
!!$      Ncd(1:3)=PRan%normal(1:3,i)
!!$
!!$      dist1 =  PRan%val_support(i)
!!$      dist2 =  1.D+20
!!$      DO j=1,PRcd%nb_vertex  
!!$
!!$!fd new le 18/04/09
!!$! comme la fonction support de l'an est calculee sans le shift
!!$! et que c'est trop chiant a modifier, on decale le cd 
!!$
!!$        scal=DOT_PRODUCT((PRcd%vertex(:,j) - perio_shift(:)),Ncd(:))
!!$
!!$        IF (scal < dist2) THEN
!!$          dist2=scal
!!$        ENDIF
!!$      ENDDO
!!$ 
!!$!      print*,'Critère = ',dist1 - dist2
!!$
!!$      IF ((dist2 - dist1) > adist) THEN
!!$
!!$        IF (bavard) THEN
!!$          PRINT*,'Ca ne passe pas le shadow overlap PRan face ',i,'avec les vertex du PRcd'
!!$          PRINT*,'dist1= ',dist1,' dist2= ',dist2,' adist = ',adist
!!$        ENDIF
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$!fd @@@ la normale au plan separateur est Ncd
!!$
!!$        Nsep=Ncd
!!$        RETURN
!!$
!!$      ENDIF   
!!$
!!$      IF ( (dist2 - dist1) < gap ) then
!!$        gap=dist2-dist1
!!$        Nsep=Ncd
!!$      ENDIF
!!$
!!$      nb_shadow = nb_shadow + 1
!!$
!!$!fd @@@ on valide les vertex
!!$
!!$      DO j=1,3
!!$
!!$        an_skip(PRan%face(j,i)) = 1
!!$
!!$      ENDDO 
!!$
!!$   ENDDO
!!$
!!$   DO i=1,PRcd%nb_faces
!!$
!!$     if (bavard) Then
!!$       print*,'corps cd - face ',i,'normale',PRcd%normal(:,i)
!!$     endif
!!$
!!$     scal1=DOT_PRODUCT(PRcd%normal(:,i),Nsep_o(:))
!!$
!!$     IF (scal1 > 0.0001D0) THEN
!!$        if (bavard) print*,'on exclue cette face'
!!$        CYCLE
!!$     ENDIF
!!$
!!$     Ncd=PRcd%normal(1:3,i)
!!$      
!!$     dist1 =  1.D+20
!!$     dist2 =  PRcd%val_support(i)
!!$
!!$     DO j=1,PRan%nb_vertex
!!$       scal= DOT_PRODUCT((PRan%vertex(:,j)+perio_shift(:)),Ncd(:))
!!$       IF (scal < dist1) THEN
!!$         dist1=scal
!!$       ENDIF
!!$     ENDDO
!!$
!!$!    print*,'Critère = ',dist2 - dist1
!!$
!!$     IF ( (dist1 - dist2) > adist) THEN
!!$       IF (bavard) THEN
!!$         PRINT*,'Ca ne passe pas le shadow overlap PRcd face ',i,'avec les vertex du PRan'
!!$         PRINT*,'dist1= ',dist2,' dist2= ',dist1,' adist = ',adist
!!$       ENDIF
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$!fd @@@ on garde moins Ncd car on va tester dans l'autre sens ...
!!$
!!$       Nsep=-Ncd
!!$
!!$       DEALLOCATE(cd_skip,an_skip)
!!$!       print*,'... desallocation>'
!!$
!!$       RETURN
!!$     ENDIF   
!!$
!!$     IF ( (dist1 - dist2) < gap) then
!!$        gap=dist1-dist2
!!$        Nsep = -Ncd
!!$     endif
!!$
!!$     nb_shadow = nb_shadow + 1
!!$
!!$!fd @@@ on valide les vertex
!!$
!!$      DO j=1,3
!!$
!!$        cd_skip(PRcd%face(j,i)) = 1
!!$
!!$      ENDDO 
!!$
!!$
!!$   ENDDO
!!$
!!$!fd @@@ on verifie qu'il y a des vertex licites
!!$
!!$   DO i=1,SIZE(cd_skip)
!!$     IF (cd_skip(i) == 0) CYCLE
!!$     IF (cd_skip(i) == 1) EXIT
!!$     PRINT*,'aucun cd_skip'
!!$     STOP
!!$   ENDDO
!!$
!!$   DO i=1,SIZE(an_skip)
!!$     IF (an_skip(i) == 0) CYCLE
!!$     IF (an_skip(i) == 1) EXIT
!!$     PRINT*,'aucun an_skip'
!!$     STOP
!!$   ENDDO
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'1111111'
!!$     PRINT*,'Ca passe le shadow overlap'
!!$     PRINT*,'la distance la plus defavorable= ',gap
!!$     PRINT*,'la normale la plus defavorable =',Nsep
!!$
!!$     DO i=1,SIZE(cd_skip)
!!$       IF (cd_skip(i) == 0) PRINT*,'corps cd - le vertex ',i,' est invisible par la suite'
!!$     ENDDO
!!$
!!$     DO i=1,SIZE(an_skip)
!!$      IF (an_skip(i) == 0) PRINT*,'corps an - le vertex ',i,' est invisible par la suite'
!!$     ENDDO
!!$ 
!!$   ENDIF
!!$
!!$!fd @@@                                 end                                        @@@
!!$!fd @@@@@@@@@@@@@@@@@@@@@@@@@@ phase de shadow overlap @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$
!!$!rp @@@@@@ recherche de l'orientation du plan separateur à la F2F @@@@@@@@@@@@@@@@
!!$
!!$!
!!$   !fd f2f donne mid_P et n_ini
!!$   f2f_found = .FALSE.
!!$   do fi=1,size(PRcd%f2f_set)
!!$     do fj=1,size(PRan%f2f_set)
!!$
!!$
!!$        if (dot_product(PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)),PRan%normal(:,PRan%f2f_set(fj)%G_i(1))) < -1.d0 + f2f_tol) then
!!$
!!$          if (bavard) then
!!$            print*,'cas possible: '
!!$            print*,'faces cd/an',PRcd%f2f_set(fi)%G_i(1),PRan%f2f_set(fj)%G_i(1)
!!$            print*,'cd normale ',PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1))
!!$            print*,'an normale ',PRan%normal(:,PRan%f2f_set(fj)%G_i(1))
!!$            print*,'p scalaire ',dot_product(PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)),PRan%normal(:,PRan%f2f_set(fj)%G_i(1)))
!!$            print*,'tol', f2f_tol
!!$          endif 
!!$
!!$          !fd la normale est la "moyenne" de celles aux faces
!!$          n_ini = 0.5*(PRan%normal(:,PRan%f2f_set(fj)%G_i(1)) - PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)))
!!$          jmin = PRcd%face(1,PRcd%f2f_set(fi)%G_i(1))
!!$          imax=  PRan%face(1,PRan%f2f_set(fj)%G_i(1))
!!$
!!$          !fd pour eviter de garder des faces qui ne sont pas en vis a vis
!!$          !fd inner radius est lie au rayon inscrit des polyedres
!!$
!!$          if (dot_product(n_ini, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+perio_shift(:)) <  &
!!$              -0.1*min(PRcd%inner_radius, PRan%inner_radius)) then
!!$            if (bavard) print*,'gap incoherent on ne garde pas'
!!$            cycle
!!$          endif
!!$
!!$          mid_P = 0.5*( PRcd%vertex(:,jmin) + (PRan%vertex(:,imax)+perio_shift(:)))
!!$
!!$          if (bavard) then
!!$            print*,'le cd set ',fi,' voit le an set ',fj
!!$            print*,'cd set ',PRcd%f2f_set(fi)%G_i(:)
!!$            print*,'an set ',PRan%f2f_set(fj)%G_i(:)
!!$            print*,'normale',n_ini
!!$            print*,'noeud cd ',jmin,' position ',PRcd%vertex(:,jmin)
!!$            print*,'noeud an ',imax,' position ',PRan%vertex(:,imax)+perio_shift(:)
!!$            print*,'gap ',dot_product(n_ini, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+perio_shift(:))
!!$          endif
!!$
!!$
!!$          !fd on rend visible uniquement les noeuds du set
!!$          cd_skip = 0
!!$          do i=1,size(PRcd%f2f_set(fi)%G_i)
!!$            cd_skip(PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))=1            
!!$            cd_skip(PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))=1            
!!$            cd_skip(PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))=1            
!!$          enddo
!!$          if (bavard) print*,'cd_skip ',cd_skip
!!$
!!$          an_skip = 0
!!$          do i=1,size(PRan%f2f_set(fj)%G_i)
!!$            an_skip(PRan%face(1,PRan%f2f_set(fj)%G_i(i)))=1            
!!$            an_skip(PRan%face(2,PRan%f2f_set(fj)%G_i(i)))=1            
!!$            an_skip(PRan%face(3,PRan%f2f_set(fj)%G_i(i)))=1            
!!$          enddo
!!$          if (bavard) print*,'an_skip ',an_skip
!!$
!!$          f2f_found = .TRUE.
!!$        endif
!!$        if (f2f_found) exit
!!$     enddo
!!$     if (f2f_found) exit
!!$   enddo
!!$
!!$   if (.not. f2f_found) return
!!$
!!$!fd @@@                                 end                                    @@@
!!$!rp @@@@@@ recherche de l'orientation du plan separateur à la F2F @@@@@@@@@@@@@@@@
!!$
!!$
!!$!rp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                                begin                                       @@@
!!$
!!$   norm=SQRT(DOT_PRODUCT(n_ini,n_ini))
!!$   mid_normal(:)=n_ini(:)/norm
!!$
!!$   mid_P(:)=((PRan%vertex(:,imax)+perio_shift(:))+PRcd%vertex(:,jmin))*0.5d0
!!$  
!!$!fd @@@ calcul repere local
!!$
!!$   Nr = mid_normal
!!$
!!$   call comp_rep(t,mid_normal,s)
!!$
!!$!--------------------------
!!$! pour afficher ce qu'on a calcule
!!$!
!!$!!   draw_gmv=.true.
!!$
!!$   if (draw_gmv) then
!!$
!!$! on essaie de comprendre ce qu'il se passe
!!$
!!$    allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$    do i=1,PRcd%nb_vertex
!!$      v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$    enddo
!!$
!!$    do i=1,PRan%nb_vertex
!!$      v_coor_an(i,:) = PRan%vertex(:,i)
!!$    enddo
!!$
!!$    rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$    v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$    v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$    v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$    v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$    call gmv_draw(id2,id1, &
!!$                  PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                  PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                  4,v_coor_ptc(1:4,1:3))
!!$
!!$    deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$
!!$   endif
!!$
!!$   draw_gmv = .false.
!!$
!!$   if (bavard) then
!!$     print*,'t ',t
!!$     print*,'n ',Nr
!!$     print*,'s ',s
!!$     print*,'P ',mid_P
!!$     ! pas encore calcule print*,'g ',d_max-d_min
!!$   endif
!!$
!!$!fd @@@                                end                                         @@@
!!$!rp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$!rp @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                              begin                                         @@@
!!$
!!$!rp @@@ Projection des vertex "licites" de l'antagoniste sur le plan moyen 
!!$
!!$! calcul d'une tolerance pour reduire le nombre de points licites a projeter
!!$ 
!!$! pour an calcul de la distance centre inertie - point critique de Cundall
!!$
!!$   norm1=(PRan%vertex(1,imax)-PRan%center(1))**2+ &
!!$        (PRan%vertex(2,imax)-PRan%center(2))**2+ &
!!$        (PRan%vertex(3,imax)-PRan%center(3))**2
!!$
!!$! pour cd calcul de la distance centre inertie - point critique de Cundall
!!$
!!$   norm2=(PRcd%vertex(1,jmin)-PRcd%center(1))**2+ &
!!$        (PRcd%vertex(2,jmin)-PRcd%center(2))**2+ &
!!$        (PRcd%vertex(3,jmin)-PRcd%center(3))**2
!!$
!!$! On trouve la distance minimale
!!$
!!$   ref_size=MIN(norm1,norm2)
!!$
!!$   if (bavard) print*,'ref_size ',ref_size
!!$
!!$   ! Recuperation des vertex à projeter
!!$
!!$   ! coordonnees des points dans le plan separateur
!!$   ALLOCATE(ss1(3,PRan%nb_vertex),stat=errare)
!!$   ss1 = 0.d0
!!$
!!$   inc=0
!!$
!!$   DO i=1,PRan%nb_vertex 
!!$
!!$     if (an_skip(i)== 0) cycle 
!!$
!!$     vec(:)=(PRan%vertex(:,i)+perio_shift(:))-mid_P(:)
!!$     d_an=DOT_PRODUCT(vec(:),mid_normal(:))
!!$
!!$     inc=inc+1
!!$
!!$!rp @@@ Projection des vertex satisfaisant le critere d'alerte sur le plan moyen    
!!$
!!$      s1(1:3)= PRan%vertex(:,i) + perio_shift(:) - (d_an*mid_normal(:))
!!$
!!$!rp @@@ Passage du repere general au repere du plan moyen (t,s)
!!$
!!$      s1moy(1)=s(1)*s1(1)+s(2)*s1(2)+s(3)*s1(3)
!!$      s1moy(2)=t(1)*s1(1)+t(2)*s1(2)+t(3)*s1(3)
!!$      s1moy(3)=mid_normal(1)*s1(1)+mid_normal(2)*s1(2)+mid_normal(3)*s1(3)
!!$
!!$!rp @@@ On conserve ce point
!!$
!!$       ss1(:,inc)=s1moy(:)
!!$
!!$   ENDDO
!!$
!!$ !rp @@@ Nombre de vertex distincts du contour (en oubliant la facétisation en triangle) projetes
!!$
!!$   nb_vertex_pran_min=inc
!!$
!!$   if (bavard) then
!!$     print*,'nbv an projetes ',nb_vertex_pran_min
!!$     do i=1,nb_vertex_pran_min
!!$       print*,ss1(:,i)
!!$     enddo
!!$   endif
!!$
!!$   ALLOCATE(Pan(3,nb_vertex_pran_min+1),stat=errare)
!!$   Pan = 0.d0
!!$
!!$!   print*,'nb_vertex_pran_min',nb_vertex_pran_min
!!$
!!$!rp @@@ Si le nombre de vertex de PRan projetes est superieur a 3  on
!!$!rp @@@      ---CALCUL DE L'ENVELOPPE CONVEXE--- 
!!$
!!$   IF (nb_vertex_pran_min > 2 ) THEN
!!$
!!$!fd barycentre du contour
!!$     c_center = 0.d0
!!$
!!$     theta=0.d0
!!$
!!$!rp @@@ On cherche tout d'abord le sommet d'abcisse la plus grande P1 (Pan(1,:))
!!$
!!$     max_P1(1:2)=ss1(1:2,1)
!!$     i_max=1
!!$
!!$     DO i=2,nb_vertex_pran_min
!!$       IF (max_P1(1)<=ss1(1,i) ) THEN
!!$         max_P1(1:2)=ss1(1:2,i)
!!$         i_max=i
!!$       ENDIF
!!$     ENDDO
!!$
!!$     inc=1
!!$     Pan(:,inc)=ss1(:,i_max) 
!!$
!!$     if (shrink > 0.d0) c_center = c_center + Pan(1:2,inc)
!!$
!!$!fd pour savoir les points deja sur l'enveloppe convexe
!!$!fd on ne met pas le premier car on veut pouvoir fermer le contour
!!$
!!$     is_ok  = 0  
!!$
!!$!rp @@@ On cherche ensuite le sommet suivant P2 (Pan(2,:)) qui forme un angle avec la verticale
!!$!rp @@@ qui soit le plus petit (c'est a dire on maximise le produit scalaire)
!!$
!!$     ! on incline un peu a cause des erreurs d'arrondie
!!$     dir = (/ cos(pi*(0.5-dt)) , sin(pi*(0.5-dt)) /)
!!$
!!$     DO k=1,nb_vertex_pran_min
!!$
!!$       max_theta=-1.D+20
!!$       i_max=0
!!$
!!$       DO i=1,nb_vertex_pran_min
!!$
!!$         ! si deja traite on passe
!!$         IF (is_ok(i) == 1) CYCLE
!!$
!!$         ! evite que ss1 (ensemble des sommets testes) ne soit sur Pk (memes coordonnees)
!!$
!!$         if (inc == 1) then
!!$           ii = 1
!!$         else
!!$           ii = inc -1 
!!$         endif
!!$
!!$         norm=(ss1(2,i)-Pan(2,ii))**2+(ss1(1,i)-Pan(1,ii))**2
!!$
!!$         IF (norm > ref_size*zero) THEN
!!$
!!$           theta=(dir(1)*(ss1(1,i)-Pan(1,ii))) + (dir(2)*(ss1(2,i)-Pan(2,ii)))
!!$
!!$           IF (theta>max_theta) THEN
!!$             max_theta =theta
!!$             i_max=i
!!$           ENDIF
!!$         ENDIF
!!$       ENDDO  
!!$
!!$       if (i_max == 0) then
!!$         print*,'zob'
!!$         stop
!!$       endif
!!$
!!$       inc=inc+1
!!$
!!$       is_ok(i_max) = 1
!!$
!!$       Pan(:,inc)=ss1(:,i_max)
!!$
!!$!fd @@@ Test d'arret contour ferme
!!$
!!$       IF ( (Pan(1,inc)-Pan(1,1))**2 + (Pan(2,inc)-Pan(2,1))**2 < zero*ref_size) EXIT
!!$
!!$       if (shrink > 0.d0) c_center = c_center + Pan(1:2,inc)
!!$
!!$       ! horizontal
!!$       if (abs(Pan(2,inc) - Pan(2,inc-1)) < 1.d-14) then
!!$         if (Pan(1,inc) - Pan(1,inc-1) > 0.d0) then
!!$           theta = -pi*dt
!!$         else
!!$           theta = pi*(1. - dt)
!!$         endif
!!$         dir = (/ cos(theta) , sin(theta) /)
!!$       else 
!!$         !vertical
!!$         if (abs(Pan(1,inc) - Pan(1,inc-1)) < 1.d-14) then
!!$           if (Pan(2,inc) - Pan(2,inc-1) > 0.d0) then
!!$            theta = pi*(0.5 - dt)
!!$           else
!!$            theta = pi*(-0.5  + dt)
!!$           endif
!!$           dir = (/ cos(theta) , sin(theta) /)
!!$         else
!!$           max_norm=(Pan(1,inc) - Pan(1,inc-1))**2+(Pan(2,inc) - Pan(2,inc-1))**2
!!$           max_norm = 1.d0/sqrt(max_norm)
!!$           dir = (/ (Pan(1,inc)-Pan(1,inc-1))*max_norm , (Pan(2,inc) - Pan(2,inc-1))*max_norm /) !- &
!!$!                 (/ cos(dt*pi) , sin(dt*pi) /)
!!$         endif
!!$       endif
!!$
!!$       if (bavard) then
!!$         print *,inc
!!$         print *,dir
!!$         print *,Pan(:,inc)
!!$         print *,Pan(:,inc-1)
!!$       endif
!!$     ENDDO
!!$
!!$     if (shrink > 0.d0) then
!!$
!!$       if (bavard) then
!!$         print*,'nbv an enveloppe convexe sans shrink ',inc
!!$         do i=1,nb_vertex_pran_min+1
!!$          print*,Pan(:,i)
!!$         enddo
!!$       endif
!!$
!!$
!!$       c_center = c_center / real(inc-1,8)
!!$       do i=1, inc
!!$         c_vec = Pan(1:2,i) - c_center  
!!$         Pan(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
!!$       enddo
!!$     endif
!!$
!!$    ELSE
!!$
!!$!rp @@@ Soit le nombre de vertex de PRan projetes est inferieur ou egal a 3 
!!$!fd c'est incomprehensible pourquoi en local ?
!!$          
!!$      inc=nb_vertex_pran_min+1
!!$
!!$      DO m=1,nb_vertex_pran_min
!!$        
!!$        Pan(:,m)=ss1(:,m)
!!$
!!$      ENDDO
!!$
!!$      Pan(:,nb_vertex_pran_min+1)=Pan(:,1)
!!$
!!$    ENDIF
!!$          
!!$    if (bavard) then
!!$      print*,'nbv an enveloppe convexe ',inc
!!$      do i=1,nb_vertex_pran_min+1
!!$        print*,Pan(:,i)
!!$      enddo
!!$    endif
!!$
!!$    DEALLOCATE(ss1)
!!$
!!$!rp Meme exercice pour le polyedre PRcd
!!$
!!$!rp @@@@@@@@@ recherche des vertex de PRcd à projeter @@@@@@@@@
!!$
!!$
!!$    ALLOCATE(rr1(3,PRcd%nb_vertex),stat=errare)
!!$    rr1 = 0.d0
!!$
!!$! Recuperation des vertex à projeter
!!$    inc=0
!!$    DO j=1,PRcd%nb_vertex 
!!$
!!$      if (cd_skip(j) == 0) cycle
!!$
!!$      vec(:)=PRcd%vertex(:,j)-mid_P(:)
!!$      d_cd=DOT_PRODUCT(vec(:),mid_normal(:))
!!$
!!$      inc=inc+1
!!$
!!$!rp @@@ Projection des vertex satisfaisant le critere d'alerte sur le plan moyen            
!!$
!!$      r1(1:3)=PRcd%vertex(:,j)-(d_cd*mid_normal(:))
!!$
!!$!rp @@@ Changement de repères
!!$! Passage du repère général pour les sommets candidats au repère du plan moyen
!!$
!!$      r1moy(1)=s(1)*r1(1)+s(2)*r1(2)+s(3)*r1(3)
!!$      r1moy(2)=t(1)*r1(1)+t(2)*r1(2)+t(3)*r1(3)
!!$      r1moy(3)=mid_normal(1)*r1(1)+mid_normal(2)*r1(2)+mid_normal(3)*r1(3)
!!$
!!$!rp @@@ Création d'un tableau rr permettant de lister l'ensemble des sommets
!!$
!!$      rr1(:,inc)=r1moy(:)
!!$
!!$    ENDDO
!!$
!!$
!!$!rp @@@ Nombre de vertex distincts du contour (en oubliant la facétisation en triangle) projetes
!!$
!!$    nb_vertex_prcd_min=inc
!!$
!!$    if (bavard) then
!!$      print*,'nbv cd projetes ',nb_vertex_prcd_min
!!$      do j=1,nb_vertex_prcd_min
!!$        print*,rr1(:,j)
!!$      enddo
!!$    endif
!!$
!!$    ALLOCATE(Pcd(3,nb_vertex_prcd_min+1),stat=errare)
!!$    Pcd = 0.d0
!!$
!!$!      print*,'nb_vertex_prcd_min',nb_vertex_prcd_min
!!$
!!$!rp @@@ Soit le nombre de vertex de PRcd projetes est superieur a 3 et
!!$!rp @@@ -- CALCUL DE L'ENVELOPPE CONVEXE ---
!!$
!!$    IF (nb_vertex_prcd_min > 2 ) THEN
!!$
!!$!fd barycentre du contour
!!$      c_center = 0.d0
!!$
!!$!fd      ALLOCATE(theta(PRcd%nb_vertex),stat=errare)
!!$      theta = 0.d0
!!$     
!!$!rp @@@ On cherche tout d'abord le sommet d'abcisse la plus grande P1 (Pcd(1,:))
!!$
!!$      max_P1(1:2)=rr1(1:2,1)
!!$      j_max=1
!!$ 
!!$      DO j=2,nb_vertex_prcd_min
!!$        IF (max_P1(1)<=rr1(1,j) ) THEN
!!$            max_P1(1:2)=rr1(1:2,j)
!!$            j_max=j
!!$        ENDIF
!!$      ENDDO
!!$
!!$      inc=1
!!$      Pcd(:,1)=rr1(:,j_max)
!!$
!!$      if (shrink > 0.d0) c_center = c_center + Pcd(1:2,inc)
!!$
!!$      is_ok  = 0
!!$
!!$      dir = (/ cos(pi*(0.5 -dt)) , sin(pi*(0.5-dt)) /)
!!$
!!$      DO k=1,nb_vertex_prcd_min
!!$         
!!$        max_theta=-1.D+20
!!$        j_max=0
!!$
!!$        DO j=1,nb_vertex_prcd_min
!!$
!!$          IF (is_ok(j) == 1) CYCLE
!!$
!!$          if (inc == 1) then
!!$            ii =1
!!$          else
!!$            ii = inc -1 
!!$          endif
!!$
!!$
!!$          norm=(rr1(2,j)-Pcd(2,ii))**2 + (rr1(1,j)-Pcd(1,ii))**2
!!$
!!$          IF (norm > ref_size*zero ) THEN
!!$
!!$            theta= (dir(1)*(rr1(1,j)-Pcd(1,ii)))+ (dir(2)*(rr1(2,j)-Pcd(2,ii)))     
!!$
!!$            IF (theta>max_theta) THEN
!!$              max_theta =theta
!!$
!!$              j_max=j
!!$            ENDIF
!!$
!!$          ENDIF
!!$        ENDDO
!!$
!!$        inc=inc+1
!!$
!!$        is_ok(j_max) = 1
!!$
!!$        Pcd(:,inc)=rr1(:,j_max)
!!$
!!$!rp @@@ Test d'arret
!!$
!!$        IF ( (Pcd(1,inc)-Pcd(1,1))**2 + (Pcd(2,inc)-Pcd(2,1))**2 < zero*ref_size) EXIT
!!$
!!$
!!$       if (shrink > 0.d0) c_center = c_center + Pcd(1:2,inc)
!!$
!!$       ! horizontal
!!$       if (abs(Pcd(2,inc) - Pcd(2,inc-1)) < 1.d-14) then
!!$         if (Pcd(1,inc) - Pcd(1,inc-1) > 0.d0) then
!!$           theta = -pi*dt
!!$         else
!!$           theta = pi*(1. -dt)
!!$         endif
!!$         dir = (/ cos(theta) , sin(theta) /)
!!$       else 
!!$         !vertical
!!$         if (abs(Pcd(1,inc) - Pcd(1,inc-1)) < 1.d-14) then
!!$           if (Pcd(2,inc) - Pcd(2,inc-1) > 0.d0) then
!!$            theta = pi*(0.5 - dt)
!!$           else
!!$            theta = pi*(-0.5 + dt)
!!$           endif
!!$           dir = (/ cos(theta) , sin(theta) /)
!!$         else
!!$           !print*,'tt',Pcd(1,inc)-Pcd(1,inc-1),Pcd(2,inc)-Pcd(2,inc-1)
!!$           !theta = atan(Pcd(2,inc) - Pcd(2,inc-1)/Pcd(1,inc)-Pcd(1,inc-1)) - dt*pi
!!$           max_norm=(Pcd(1,inc) - Pcd(1,inc-1))**2+(Pcd(2,inc) - Pcd(2,inc-1))**2
!!$           max_norm = 1.d0/sqrt(max_norm)
!!$           dir = (/ (Pcd(1,inc)-Pcd(1,inc-1))*max_norm , (Pcd(2,inc) - Pcd(2,inc-1))*max_norm /) !+ &
!!$!                 (/ cos(dt*pi) , sin(dt*pi) /)
!!$         endif
!!$       endif
!!$
!!$       if (bavard) then
!!$         print *,inc
!!$         print *,dir
!!$         print *,Pcd(:,inc)
!!$         print *,Pcd(:,inc-1)
!!$       endif
!!$
!!$
!!$      ENDDO
!!$
!!$     if (shrink > 0.d0) then
!!$       if (bavard) then
!!$        print*,'nbv enveloppe convexe sans shrink',inc
!!$        do j=1,nb_vertex_prcd_min+1
!!$          print*,Pcd(:,j)
!!$        enddo
!!$      endif
!!$
!!$       c_center = c_center / real(inc-1,8)
!!$       do i=1, inc
!!$         c_vec = Pcd(1:2,i) - c_center  
!!$         Pcd(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
!!$       enddo
!!$     endif
!!$
!!$
!!$    ELSE
!!$
!!$!rp @@@ Soit le nombre de vertex de PRcd projetes est inferieur ou egal a 3 
!!$
!!$      inc = nb_vertex_prcd_min + 1 
!!$      DO m=1,nb_vertex_prcd_min
!!$
!!$         Pcd(:,m)=rr1(:,m)
!!$
!!$      ENDDO
!!$
!!$      Pcd(:,nb_vertex_prcd_min+1) = Pcd(:,1)
!!$
!!$
!!$    ENDIF
!!$      
!!$    if (bavard) then
!!$      print*,'nbv enveloppe convexe ',inc
!!$      do j=1,nb_vertex_prcd_min+1
!!$        print*,Pcd(:,j)
!!$      enddo
!!$    endif
!!$
!!$
!!$
!!$    DEALLOCATE(rr1)
!!$
!!$    DEALLOCATE(cd_skip,an_skip)
!!$!        print*,'... desallocation>'
!!$
!!$
!!$!fd @@@                              end                                           @@@
!!$!rp @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES enveloppes convexes @@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                              begin                                         @@@
!!$
!!$
!!$!!-->Optimisation a faire
!!$
!!$
!!$!fd faudrait au moins virer les tests triviaux !! 
!!$!fd ca me desespere de voir une MERDE pareille 
!!$
!!$
!!$    nb_ptc1=0
!!$
!!$!fd le cas 1 | 1 est vire ...
!!$
!!$    IF (nb_vertex_pran_min /= 1 .or. nb_vertex_prcd_min /= 1 ) then
!!$
!!$!fd -->ici on ne fait que le intersections entre segments !! <---
!!$!fd @@@ recherche de l'intersection entre les segments des deux contours
!!$!fd @@@ tres tres nul car n**2 <- ajout test grossier
!!$
!!$      DO  k=1,nb_vertex_pran_min
!!$        xan_min=min(Pan(1,k),Pan(1,k+1)) ; xan_max=max(Pan(1,k),Pan(1,k+1))
!!$        yan_min=min(Pan(2,k),Pan(2,k+1)) ; yan_max=max(Pan(2,k),Pan(2,k+1))
!!$     
!!$        DO m=1,nb_vertex_prcd_min
!!$
!!$!          test grossier boite englobante segments
!!$!
!!$           xcd_min=min(Pcd(1,m),Pcd(1,m+1)) ; xcd_max=max(Pcd(1,m),Pcd(1,m+1))
!!$           if (xcd_max < xan_min .or. xcd_min > xan_max) cycle
!!$
!!$           ycd_min=min(Pcd(2,m),Pcd(2,m+1)) ; ycd_max=max(Pcd(2,m),Pcd(2,m+1))
!!$           if (ycd_max < yan_min .or. ycd_min > yan_max) cycle
!!$
!!$
!!$!rp @@@ Equation parametrique des segments du contour
!!$
!!$           denom=Pan(1,k)*(Pcd(2,m+1)-Pcd(2,m)) + Pan(1,k+1)*(Pcd(2,m)-Pcd(2,m+1)) + &
!!$                 Pcd(1,m+1)*(Pan(2,k+1)-Pan(2,k)) + Pcd(1,m)*(Pan(2,k)-Pan(2,k+1))
!!$
!!$           IF (denom>1.D-07 .OR. denom<-1.D-07) THEN
!!$
!!$!rp @@@ Parametres ss et tt
!!$
!!$             num=Pan(1,k)*(Pcd(2,m+1)-Pcd(2,m)) + Pcd(1,m)*(Pan(2,k)-Pcd(2,m+1)) +  &
!!$                 Pcd(1,m+1)*(Pcd(2,m)-Pan(2,k))
!!$  
!!$             ss=num/denom
!!$
!!$             num=-( Pan(1,k)*(Pcd(2,m)-Pan(2,k+1)) + Pan(1,k+1)*(Pan(2,k)-Pcd(2,m)) + &
!!$                    Pcd(1,m)*(Pan(2,k+1)-Pan(2,k)))
!!$ 
!!$             tt=num/denom
!!$
!!$!rp @@@ les parametres ss et tt sont compris entre 0 et 1
!!$
!!$             IF   ((ss>1.D-12  .AND. ss<=(1.d0 - 1.D-12))  .AND. &
!!$                   (tt>1.D-12  .AND. tt<=(1.d0 - 1.D-12)))   THEN
!!$
!!$               listsup(1)=Pan(1,k) + ss* (Pan(1,k+1)-Pan(1,k))
!!$               listsup(2)=Pan(2,k) + ss* (Pan(2,k+1)-Pan(2,k))
!!$               listsup(3)=Pan(3,1)
!!$
!!$!                    print*, 'listsup', listsup(:)
!!$
!!$!rp @@@ Tri pour les points d'intersection
!!$
!!$!rp @@@ Recuperation de la liste des points resultant de l'intersection entre les faces 
!!$!Passage des sommets de cette liste supplementaire du repere moyen au repere general
!!$!Recuperation de la liste des contacts
!!$               nb_ptc1=nb_ptc1+1              
!!$
!!$               PT1(1,nb_ptc1)=s(1)*listsup(1)+t(1)*listsup(2)+mid_normal(1)*listsup(3)
!!$               PT1(2,nb_ptc1)=s(2)*listsup(1)+t(2)*listsup(2)+mid_normal(2)*listsup(3)
!!$               PT1(3,nb_ptc1)=s(3)*listsup(1)+t(3)*listsup(2)+mid_normal(3)*listsup(3)
!!$
!!$!                     print*, 'lisintersect', PT1(:,nb_ptc1)
!!$              
!!$             ENDIF
!!$           ENDIF
!!$        ENDDO
!!$      ENDDO
!!$    ENDIF
!!$
!!$!rp ---CHOIX DES POINTS DE CONTACTS ---
!!$
!!$    nb_ptc2=0
!!$    PT=0.D0
!!$
!!$!rp @@@ recherche des sommets pcd appartenant a l'enveloppe des pan
!!$ 
!!$    DO m=1,nb_vertex_prcd_min
!!$
!!$      inc=0 
!!$      inc1=0
!!$
!!$      DO  k=1,nb_vertex_pran_min
!!$
!!$!Soit nombre de points de contacts est superieur ou egal a 3
!!$
!!$          IF (nb_vertex_pran_min>=3) THEN
!!$
!!$            scal1 = (Pan(1,k+1)-Pan(1,k))*(Pcd(2,m)-Pan(2,k))-(Pan(2,k+1)-Pan(2,k))*(Pcd(1,m)-Pan(1,k))
!!$
!!$!rp @@@ Soit le produit vectoriel est positif
!!$
!!$            IF (scal1>=-1.D-05 ) THEN
!!$               inc=inc+1
!!$            ENDIF
!!$
!!$            IF (inc==nb_vertex_pran_min) THEN
!!$
!!$!rp @@@ Recuperation de la liste des points cddidats appartenant au contour de l'antagoniste
!!$!Passage des sommets de cette liste candidate du repere moyen au repere general
!!$!Recuperation de la liste des contacts
!!$               nb_ptc2=nb_ptc2+1
!!$
!!$               PT2(1,nb_ptc2)=s(1)*Pcd(1,m)+t(1)*Pcd(2,m)+mid_normal(1)*Pcd(3,m)
!!$               PT2(2,nb_ptc2)=s(2)*Pcd(1,m)+t(2)*Pcd(2,m)+mid_normal(2)*Pcd(3,m)
!!$               PT2(3,nb_ptc2)=s(3)*Pcd(1,m)+t(3)*Pcd(2,m)+mid_normal(3)*Pcd(3,m)
!!$
!!$!               print*, 'Pcandidat',  PT2(:,nb_ptc2)
!!$
!!$            ENDIF
!!$
!!$!rp @@@ Soit le produit vectoriel est negatif
!!$            IF (scal1<=1.D-05 ) THEN
!!$                     inc1=inc1+1
!!$            ENDIF
!!$
!!$            IF (inc1==nb_vertex_pran_min) THEN
!!$
!!$!rp @@@ Recuperation de la liste des points cddidats appartenant au contour de l'antagoniste
!!$!Passage des sommets de cette liste candidate du repere moyen au repere general
!!$!Recuperation de la liste des contacts
!!$               nb_ptc2=nb_ptc2+1
!!$
!!$               PT2(1,nb_ptc2)=s(1)*Pcd(1,m)+t(1)*Pcd(2,m)+mid_normal(1)*Pcd(3,m)
!!$               PT2(2,nb_ptc2)=s(2)*Pcd(1,m)+t(2)*Pcd(2,m)+mid_normal(2)*Pcd(3,m)
!!$               PT2(3,nb_ptc2)=s(3)*Pcd(1,m)+t(3)*Pcd(2,m)+mid_normal(3)*Pcd(3,m)
!!$
!!$!               print*, 'Pcandidat',PT2(:,nb_ptc2) 
!!$
!!$            ENDIF
!!$
!!$!Soit nombre de points de contacts est inferieur a 3
!!$
!!$          ELSE
!!$
!!$!fd c'est quoi ce test moisi !?
!!$!fd            IF (SQRT(Pcd(1,m)**2+Pcd(2,m)**2)>SQRT(Pan(1,k)**2+Pan(2,k)**2)-1.D-12 .AND. &
!!$!fd                SQRT(Pcd(1,m)**2+Pcd(2,m)**2)<SQRT(Pan(1,k)**2+Pan(2,k)**2)+1.D-12 )  THEN
!!$
!!$            lvec(1:2)=Pcd(1:2,m) - Pan(1:2,k)
!!$            if (dot_product(lvec,lvec) < 1.D-12) THEN
!!$
!!$
!!$!                     print*,'nb_vertex_an inf a 3'
!!$
!!$!rp @@@ Recuperation de la liste des points candidats appartenant au contour de l'antagoniste
!!$!Passage des sommets de cette liste candidate du repere moyen au repere general
!!$!Recuperation de la liste des contacts
!!$             nb_ptc2=nb_ptc2+1
!!$
!!$             PT2(1,nb_ptc2)=s(1)*Pcd(1,m)+t(1)*Pcd(2,m)+mid_normal(1)*Pcd(3,m)
!!$             PT2(2,nb_ptc2)=s(2)*Pcd(1,m)+t(2)*Pcd(2,m)+mid_normal(2)*Pcd(3,m)
!!$             PT2(3,nb_ptc2)=s(3)*Pcd(1,m)+t(3)*Pcd(2,m)+mid_normal(3)*Pcd(3,m)
!!$
!!$!               print*, 'Pcandidat', PT2(:,nb_ptc2)
!!$
!!$          ENDIF
!!$
!!$        ENDIF
!!$      ENDDO
!!$    ENDDO
!!$
!!$
!!$!rp @@@ Test dans l'autre sens
!!$!rp @@@ pan appartenant a l'enveloppe des pcd
!!$
!!$   DO  k=1,nb_vertex_pran_min
!!$
!!$     inc=0 
!!$     inc1=0
!!$
!!$     DO m=1,nb_vertex_prcd_min
!!$
!!$!Soit nombre de points de contacts est superieur ou egal a 3
!!$       IF (nb_vertex_prcd_min>=3) THEN
!!$
!!$         scal1 = (Pcd(1,m+1)-Pcd(1,m))*(Pan(2,k)-Pcd(2,m))-(Pcd(2,m+1)-Pcd(2,m))*(Pan(1,k)-Pcd(1,m))
!!$
!!$!rp @@@ Soit le produit vectoriel est positif
!!$         IF (scal1>=-1.D-05 ) THEN
!!$            inc=inc+1
!!$         ENDIF
!!$
!!$         IF (inc==nb_vertex_prcd_min) THEN
!!$
!!$!rp @@@ Recuperation de la liste des points antagonistes appartenant au contour du candidat
!!$!Passage des sommets de cette liste antagoniste du repere moyen au repere general        
!!$!Recuperation de la liste des contacts
!!$            nb_ptc1=nb_ptc1+1
!!$
!!$            PT1(1,nb_ptc1)=s(1)*Pan(1,k)+t(1)*Pan(2,k)+mid_normal(1)*Pan(3,k)
!!$            PT1(2,nb_ptc1)=s(2)*Pan(1,k)+t(2)*Pan(2,k)+mid_normal(2)*Pan(3,k)
!!$            PT1(3,nb_ptc1)=s(3)*Pan(1,k)+t(3)*Pan(2,k)+mid_normal(3)*Pan(3,k)
!!$
!!$!            print*, 'Pantagoniste',PT1(:,nb_ptc1)
!!$
!!$         ENDIF
!!$
!!$
!!$!rp @@@ Soit le produit vectoriel est negatif
!!$         IF (scal1<=1.D-05 ) THEN
!!$           inc1=inc1+1
!!$         ENDIF
!!$
!!$         IF (inc1==nb_vertex_prcd_min) THEN
!!$
!!$!rp @@@ Recuperation de la liste des points antagonistes appartenant au contour du candidat
!!$!Passage des sommets de cette liste antagoniste du repere moyen au repere general        
!!$!Recuperation de la liste des contacts
!!$
!!$            nb_ptc1=nb_ptc1+1
!!$            PT1(1,nb_ptc1)=s(1)*Pan(1,k)+t(1)*Pan(2,k)+mid_normal(1)*Pan(3,k)
!!$            PT1(2,nb_ptc1)=s(2)*Pan(1,k)+t(2)*Pan(2,k)+mid_normal(2)*Pan(3,k)
!!$            PT1(3,nb_ptc1)=s(3)*Pan(1,k)+t(3)*Pan(2,k)+mid_normal(3)*Pan(3,k)
!!$
!!$!             print*, 'Pantagoniste', PT1(:,nb_ptc1)
!!$
!!$         ENDIF
!!$
!!$       ENDIF
!!$     ENDDO
!!$   ENDDO
!!$
!!$   DEALLOCATE(Pan)
!!$   DEALLOCATE(Pcd)
!!$
!!$!fd @@@                              end                                         @@@
!!$!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES eveloppes convexes @@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$!fd @@@ elimination des noeuds detectes plusieurs fois    
!!$  
!!$   is_ok  = 0
!!$
!!$   norm_min=MIN(PRan%min_radius_face,PRcd%min_radius_face) 
!!$   norm_max=MAX(PRan%max_radius_face,PRcd%max_radius_face) 
!!$
!!$   IF (norm_min/norm_max < 0.8) THEN 
!!$     !fd @@@ quand le ratio de taille des faces est petit
!!$     !norm=norm_min*0.5
!!$     norm=norm_min
!!$   ELSE
!!$     !fd @@@ quand le ratio de taille des faces est grand
!!$     !norm=norm_min*0.25
!!$     norm=norm_min
!!$   ENDIF
!!$   norm2=norm*norm
!!$
!!$   nb_stored=0
!!$
!!$   DO i=1,nb_ptc1
!!$     IF (is_ok(i) == 1) CYCLE
!!$     nb_stored=nb_stored+1
!!$     DO j=i+1,nb_ptc1   
!!$       IF (is_ok(j) == 1) CYCLE
!!$       scal=DOT_PRODUCT(PT1(:,i)-PT1(:,j),PT1(:,i)-PT1(:,j))
!!$       IF (scal < 5d-2*norm2) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'dist**2 ','pt1',i,'pt2',j,scal 
!!$           PRINT*,'on retire PT1: ',j
!!$         ENDIF
!!$         is_ok(j)=1
!!$       ENDIF
!!$   ENDDO
!!$
!!$   DO j=1,nb_ptc2   
!!$       IF (is_ok(nb_ptc1+j) == 1) CYCLE
!!$       scal=DOT_PRODUCT(PT1(:,i)-PT2(:,j),PT1(:,i)-PT2(:,j))
!!$       IF (scal < 5d-2*norm2) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'dist**2 ','pt1',i,'pt2',j,scal
!!$           PRINT*,'on retire PT2: ',j
!!$         ENDIF
!!$         is_ok(nb_ptc1+j)=1
!!$       ENDIF
!!$     ENDDO
!!$   ENDDO
!!$
!!$   DO i=1,nb_ptc2
!!$     IF (is_ok(nb_ptc1+i) == 1) CYCLE
!!$     nb_stored=nb_stored+1
!!$     DO j=i+1,nb_ptc2   
!!$       IF (is_ok(nb_ptc1+j) == 1) CYCLE
!!$       scal=DOT_PRODUCT(PT2(:,i)-PT2(:,j),PT2(:,i)-PT2(:,j))
!!$       IF (scal < 5d-2*norm2) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'dist**2 ','pt2',i,'pt2',j,scal
!!$           PRINT*,'on retire PT2: ',j
!!$         ENDIF
!!$         is_ok(nb_ptc1+j)=1
!!$       ENDIF
!!$     ENDDO
!!$   ENDDO
!!$
!!$!fd elimination des noeuds les plus loin des faces en vis a vis
!!$!fd il faut qu'ils soient plus que 0.1 la distance d'alerte
!!$!fd on risque de perdre des noeuds importants sinon
!!$
!!$   k=0
!!$   scal_min=-0.00001*adist
!!$   DO i=1,nb_stored-4
!!$     scal=1.d+20
!!$     i1=0;i2=0    
!!$     DO j=1,nb_ptc1
!!$       IF (is_ok(j) /= 0) CYCLE
!!$       IF (PT1_loc(1,j) < scal .AND. PT1_loc(1,j) < scal_min) THEN
!!$         scal=PT1_loc(1,j) 
!!$         i1=j
!!$       ENDIF
!!$     ENDDO
!!$     DO j=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+j) /= 0) CYCLE 
!!$       IF (PT2_loc(1,j) < scal  .AND. PT2_loc(1,j) < scal_min) THEN
!!$         scal=PT2_loc(1,j) 
!!$         i1=0
!!$         i2=j
!!$       ENDIF
!!$     ENDDO
!!$     IF (i1 /= 0) THEN
!!$       is_ok(i1)=1
!!$       k=k+1
!!$       IF (bavard) PRINT*,'dans PR1 on elimine le noeud',i1
!!$     ENDIF
!!$     IF (i2 /= 0) THEN
!!$      is_ok(nb_ptc1+i2)=1
!!$      k=k+1
!!$       IF (bavard) PRINT*,'dans PR2 on elimine le noeud',i2
!!$     ENDIF
!!$     IF (i1 == 0 .AND. i2 == 0) THEN
!!$       IF (bavard) PRINT*,'aucun point eliminable'
!!$       EXIT
!!$     ENDIF
!!$   ENDDO
!!$
!!$   nb_stored=nb_stored-k
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'il ne reste plus que ',nb_stored,' noeuds'
!!$     PRINT*,'tri par inter-distances'
!!$   ENDIF
!!$
!!$!fd projection sur un plan pour les points non coplanaires ?
!!$
!!$!fd on calcule le centre du nuage.
!!$!fd on vire le plus proche
!!$!fd on recommence ...
!!$
!!$   k=0
!!$   DO i=1,nb_stored-4
!!$
!!$     nb_ctc=0
!!$     PT=0.d0
!!$     DO j=1,nb_ptc1
!!$       IF (is_ok(j) /= 0) CYCLE
!!$       nb_ctc=nb_ctc+1
!!$       PT(:)=PT(:)+PT1(:,j)
!!$     ENDDO
!!$
!!$     DO j=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+j) /= 0) CYCLE
!!$       nb_ctc=nb_ctc+1
!!$       PT(:)=PT(:)+PT2(:,j)
!!$     ENDDO
!!$
!!$     PT_CTC       = 0.D0
!!$     PT=PT/REAL(nb_ctc,8)
!!$
!!$     PC=PT
!!$
!!$     i1=0;i2=0    
!!$     scal_min=1.d20
!!$     DO j=1,nb_ptc1
!!$       IF (is_ok(j) /= 0) CYCLE
!!$       scal=DOT_PRODUCT(PC(:)-PT1(:,j),PC(:)-PT1(:,j))
!!$       IF (scal < scal_min) THEN
!!$         scal_min=scal         
!!$         i1=j
!!$       ENDIF
!!$     ENDDO
!!$     DO j=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+j) /= 0) CYCLE 
!!$       scal=DOT_PRODUCT(PC(:)-PT2(:,j),PC(:)-PT2(:,j))
!!$       IF (scal < scal_min) THEN
!!$         scal_min=scal
!!$         i1=0
!!$         i2=j
!!$       ENDIF
!!$     ENDDO
!!$     IF (i1 /= 0) THEN
!!$       is_ok(i1)=1
!!$       k=k+1
!!$       IF (bavard) PRINT*,'dans PR1 on elimine le noeud',i1
!!$     ENDIF
!!$     IF (i2 /= 0) THEN
!!$      is_ok(nb_ptc1+i2)=1
!!$      k=k+1
!!$       IF (bavard) PRINT*,'dans PR2 on elimine le noeud',i2
!!$     ENDIF
!!$     IF (i1 == 0 .AND. i2 == 0) THEN
!!$       IF (bavard) PRINT*,'aucun point eliminable'
!!$       EXIT
!!$     ENDIF
!!$   ENDDO
!!$
!!$
!!$!fd @@@ Recuperation des points de contacts apres elimination
!!$
!!$   nb_select=0
!!$   DO i=1,nb_ptc1
!!$        !rp @@@ Choix des points seulement en contact
!!$           IF (is_ok(i) /= 0) CYCLE
!!$           nb_select=nb_select+1
!!$!           print*,'nbselect',nb_select
!!$           IF (nb_select > 4) THEN
!!$              PRINT*,'trop de point de contact'
!!$              EXIT
!!$           ENDIF
!!$           PT_CTC(:,nb_select)=PT1(:,i)
!!$!           print*,'PT1contactreel',PT_CTC(:,nb_select)
!!$   ENDDO
!!$
!!$ 
!!$   DO i=1,nb_ptc2
!!$        !rp @@@ Choix des points seulement en contact
!!$           IF (is_ok(nb_ptc1+i) /= 0) CYCLE
!!$           nb_select=nb_select+1 
!!$!           print*,'nbselect',nb_select
!!$           IF (nb_select > 4) THEN
!!$              PRINT*,'trop de point de contact'
!!$              EXIT
!!$           ENDIF
!!$           PT_CTC(:,nb_select)=PT2(:,i)
!!$!           print*,'PT2contactreel',PT_CTC(:,nb_select)
!!$   ENDDO
!!$
!!$   if (nb_select < 3) then
!!$     print*,'Not enough contact nodes between the surfaces'
!!$     nb_ctc = 0
!!$
!!$     ! on essaie de comprendre ce qu'il se passe
!!$
!!$      allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$      do i=1,PRcd%nb_vertex
!!$        v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$      enddo
!!$
!!$      do i=1,PRan%nb_vertex
!!$        v_coor_an(i,:) = PRan%vertex(:,i)
!!$      enddo
!!$
!!$      rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$      v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$      v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$      v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$      v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$      call gmv_draw(id2,id1, &
!!$                    PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                    PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                    4,v_coor_ptc(1:4,1:3))
!!$
!!$      deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$      return
!!$   endif
!!$
!!$   IF (nb_select <= 4) THEN
!!$     nb_ctc=nb_select 
!!$!        print*,'nbselect',nb_select 
!!$   ELSE
!!$     nb_ctc=4
!!$   ENDIF
!!$
!!$   v2v%cd=id2
!!$   v2v%an=id1
!!$   v2v%nb_ctc=nb_ctc
!!$
!!$   !print*,'on pousse dans visavis'
!!$   !print*,'cd/an',v2v%cd,v2v%an
!!$   !print*,'nb_ctc',v2v%nb_ctc
!!$
!!$   DO ic=1,nb_ctc
!!$     ! recherche de la face cd qui contient le noeud projete suivant la direction mid_nornal
!!$     is_inside = .false.
!!$     do iff=1,size(PRcd%f2f_set(fi)%G_i)
!!$       do i=1,3
!!$         pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
!!$       enddo
!!$       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_min,v2v%cd_lcoor(:,ic),bavard)  
!!$       if (is_inside) then
!!$         my_cd_iff = iff
!!$         exit
!!$       endif
!!$     enddo
!!$     if (.not. is_inside) then
!!$       print*,'======================================='
!!$       call LOGMES('Error '//IAM//': unable to find a cd face')
!!$       print*,'contact ',ic,' entre ',id2,id1
!!$       print*,'position ',pt_ctc(:,ic)
!!$       print*,'normale ',mid_normal
!!$       do iff=1,size(PRcd%f2f_set(fi)%G_i)
!!$         print*,'face ', PRcd%f2f_set(fi)%G_i(iff)       
!!$         print*,'n ', PRcd%normal(:,PRcd%f2f_set(fi)%G_i(iff))
!!$ 
!!$         do i=1,3
!!$          print*,'v',i,PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
!!$          pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
!!$         enddo
!!$         is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_min,v2v%cd_lcoor(:,ic),.true.)  
!!$       enddo
!!$
!!$
!!$       ! on essaie de comprendre ce qu'il se passe
!!$
!!$       allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$       do i=1,PRcd%nb_vertex
!!$         v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$       enddo
!!$
!!$       do i=1,PRan%nb_vertex
!!$        v_coor_an(i,:) = PRan%vertex(:,i)
!!$       enddo
!!$
!!$       rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$       v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$       v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$       v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$       v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$       call gmv_draw(id2,id1, &
!!$                  PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                  PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                  4,v_coor_ptc(1:4,1:3))
!!$
!!$       deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$
!!$       nb_ctc = 0
!!$       return
!!$     endif
!!$
!!$     v2v%iff_cd(ic) = PRcd%f2f_set(fi)%G_i(my_cd_iff)
!!$
!!$     !print*,'iff_cd ',v2v%iff_cd(ic)
!!$     !print*,'nodes ',PRcd%face(:,PRcd%f2f_set(fi)%G_i(my_cd_iff))
!!$     !print*,'w ',v2v%cd_lcoor(:,ic)
!!$
!!$     is_inside = .false.
!!$     do iff=1,size(PRan%f2f_set(fj)%G_i)
!!$       do i=1,3
!!$         pp(:,i) = PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(iff))) 
!!$       enddo
!!$       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_max,v2v%an_lcoor(:,ic),bavard)  
!!$       if (is_inside) then
!!$         my_an_iff = iff
!!$         exit
!!$       endif
!!$     enddo
!!$
!!$     if (.not. is_inside) then
!!$       call LOGMES('Error '//IAM//': unable to find an an face')
!!$       print*,'point ',ic
!!$       print*,'position ',pt_ctc(:,ic)
!!$       print*,'normale ',mid_normal
!!$       do iff=1,size(PRan%f2f_set(fj)%G_i)
!!$         print*,'face ', PRan%f2f_set(fj)%G_i(iff)       
!!$         print*,'n ', PRan%normal(:,PRan%f2f_set(fj)%G_i(iff))
!!$         do i=1,3
!!$          pp(:,i)=PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(iff))) 
!!$         enddo
!!$         is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_max,v2v%an_lcoor(:,ic),.true.)  
!!$       enddo       
!!$       ! on essaie de comprendre ce qu'il se passe
!!$
!!$       allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$       do i=1,PRcd%nb_vertex
!!$         v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$       enddo
!!$
!!$       do i=1,PRan%nb_vertex
!!$        v_coor_an(i,:) = PRan%vertex(:,i)
!!$       enddo
!!$
!!$       rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$       v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$       v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$       v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$       v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$       call gmv_draw(id2,id1, &
!!$                  PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                  PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                  4,v_coor_ptc(1:4,1:3))
!!$
!!$       deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$
!!$       nb_ctc = 0
!!$       return
!!$
!!$     endif
!!$
!!$     v2v%iff_an(ic) = PRan%f2f_set(fj)%G_i(my_an_iff)
!!$
!!$     !print*,'iff_an',v2v%iff_an(ic)
!!$     !print*,'nodes ',PRan%face(:,PRan%f2f_set(fj)%G_i(my_an_iff))
!!$     !print*,'w',v2v%an_lcoor(:,ic)
!!$
!!$     ! la distance entre les 2 points
!!$
!!$     overlap(ic)=d_min-d_max
!!$
!!$     if (bavard) then
!!$       print *,'contact: ', ic
!!$       print *,'distance: ',overlap(ic)
!!$       print *,'distances cd/an',d_min,d_max
!!$       print *,'weight cd',v2v%cd_lcoor(:,ic)
!!$       print *,'weight an',v2v%an_lcoor(:,ic)
!!$     endif
!!$   enddo
!!$
!!$ END SUBROUTINE DETECTION_F2F
!!$!!!------------------------------------------------------------------------
!!$ SUBROUTINE DETECTION_F2F_new(id1,id2,Nsep,adist,nb_ctc,PT_CTC,overlap,Nr,t,s,v2v)
!!$
!!$! I
!!$! id1 : id solide 1 (antagoniste)
!!$! id2 : id solide 2 (candidat)
!!$! Nsep: est la derniere normale au plan separateur connue 
!!$!       l'intercentre si on fait une recherche rough a chaque pas
!!$!
!!$! O 
!!$! nb_ctct           : nombre de points de contact
!!$! PT_CTC(1:nb_ctc)  : coordonnees des points de contact
!!$! Nr(1)             : normales aux points de contact si il y a contact (bizarre bizarrre)
!!$! r_cd,r_an         : qui est le vrai candidat et le vrai antagoniste ... comme en entre
!!$! overlap(1:nb_ctc) : les gaps aux points de contact
!!$! Nsep : normale au plan separateur actualisee ou ancienne
!!$
!!$
!!$   IMPLICIT NONE
!!$
!!$   INTEGER                          :: id1,id2,nb_ctc
!!$   REAL(kind=8)                     :: Nsep(3),adist,Nr(3),t(3),s(3)
!!$   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC
!!$   REAL(kind=8),DIMENSION(4)        :: overlap
!!$   type(T_visavis) :: v2v
!!$   !
!!$
!!$   INTEGER,PARAMETER                :: nb_ptc_max=100
!!$   INTEGER                          :: i,k,j,m,inc,inc1
!!$   TYPE(T_POLYR)                    :: PRan,PRcd
!!$
!!$   REAL(kind=8)                     :: dist1,dist2,scal,norm,norm1,norm2
!!$   REAL(kind=8),DIMENSION(3)        :: s1,r1,Nsep_o
!!$   REAL(kind=8)                     :: gap
!!$   INTEGER                          :: nb_ptc
!!$  
!!$   INTEGER,DIMENSION(2*nb_ptc_max)  :: is_ok
!!$
!!$
!!$   INTEGER                          :: errare,nb_select
!!$   REAL(kind=8),DIMENSION(3)        :: Ncd
!!$ 
!!$   REAL(kind=8)                     :: norm_max,norm_min
!!$
!!$   LOGICAL                          :: bavard=.false. !.false.
!!$
!!$   INTEGER                          :: nb_vertex_pran_min, nb_vertex_prcd_min
!!$
!!$   REAL(kind=8),DIMENSION(3)        :: mid_P,p1,p2,mid_normal
!!$   REAL(kind=8),DIMENSION(2)        :: s1moy,r1moy
!!$
!!$   REAL(kind=8),DIMENSION(:,:), ALLOCATABLE   :: Pcd,Pan,rr1,ss1   
!!$   integer,dimension(:),allocatable :: id_Pan,id_Pcd
!!$
!!$   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: cd_skip,an_skip
!!$
!!$   INTEGER                        :: jmin,imax
!!$
!!$   REAL(kind=8)                   :: d_an,d_cd,d_min,d_max
!!$
!!$   REAL(kind=8),DIMENSION(3)      :: n_ini,vec
!!$
!!$   REAL(kind=8),DIMENSION(2)      :: lvec,PT
!!$
!!$!fd 
!!$
!!$   REAL(kind=8)                   :: tol = 1.d-10,rd
!!$   integer                        :: dump_fich
!!$
!!$                               !123456789012345678901234567890123
!!$   CHARACTER(len=33)  :: IAM = 'mod_PRPRx::detection_common_plane'
!!$
!!$
!!$   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
!!$   REAL(kind=8),dimension(:),allocatable :: dist
!!$
!!$   character(len=11) :: nom
!!$
!!$   logical :: is_reverse=.FALSE., draw_gmv=.false.
!!$
!!$   integer :: ii
!!$   real(kind=8) :: ref_size,dir(2),c_center(2),c_vec(2)
!!$
!!$   logical :: is_inside
!!$  ! f2f
!!$   integer :: fi,fj,iff,my_cd_iff,my_an_iff,ic
!!$   logical :: f2f_found
!!$
!!$   real(kind=8) :: pp(3,3),shift_ptc(3)
!!$   real(kind=8),pointer :: points(:,:)
!!$
!!$   integer :: ix
!!$
!!$   !fd par defaut on ne fait pas le so avec les normales des faces
!!$   !fd TODO mettre ca en parametre
!!$
!!$   logical :: skip_so = .true.
!!$
!!$
!!$   real(kind=8) :: tmp,vec1(3),vec2(3),vec3(3)
!!$   integer :: nb_ptc_qh
!!$
!!$   real(kind=8),allocatable :: ptc(:,:),angle(:)
!!$   integer,allocatable :: id_ptc(:)
!!$    
!!$   !fd pas de surprise si j'initialise (dicton du jour le plus con) 
!!$
!!$   nb_ctc=0
!!$
!!$   Nr = 0.d0
!!$   t  = 0.d0
!!$   s  = 0.d0
!!$
!!$   Nsep_o = Nsep 
!!$
!!$   PRan    = S_POLYR(id1)
!!$   PRcd    = S_POLYR(id2)
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'======================='
!!$     PRINT*,'Detection entre le POLYR cd',id2,' et le POLYR an',id1
!!$     PRINT*,'sep',Nsep
!!$     PRINT*,'vertex de cd:',id2 
!!$     DO i=1,PRcd%nb_vertex
!!$       PRINT*,PRcd%vertex(:,i)
!!$     ENDDO
!!$     PRINT*,'vertex de an:',id1 
!!$     DO i=1,PRan%nb_vertex
!!$       PRINT*,PRan%vertex(:,i)+perio_shift(:)
!!$     ENDDO
!!$   ENDIF
!!$
!!$!fd @@@@@@@@@@@@@@@@@@@@@@@@@@ phase de shadow overlap @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                                begin                                       @@@
!!$!fd @@@
!!$!fd @@@ s-o initialisation de Nr sur la valeur de Nsep                             @@@
!!$!fd @@@   "ligne des centres"|"ancienne normale au plan separateur"                @@@
!!$
!!$   dist1 = -1.D+20
!!$   dist2 =  1.D+20
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 1 vers 2
!!$   !
!!$
!!$   DO i=1,PRan%nb_vertex
!!$     scal= DOT_PRODUCT((PRan%vertex(:,i)+perio_shift(:)),Nsep_o(:))
!!$     IF (scal > dist1) THEN
!!$       dist1=scal
!!$     ENDIF
!!$   ENDDO
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 2 vers 1
!!$   !
!!$
!!$   DO i=1,PRcd%nb_vertex
!!$     scal= DOT_PRODUCT(PRcd%vertex(:,i),Nsep_o(:))
!!$     IF (scal < dist2) THEN
!!$       dist2=scal
!!$     ENDIF
!!$   ENDDO 
!!$
!!$!fd @@@ si le s-o sur Nsep ne passe pas on sort 
!!$
!!$   IF (.not. skip_so .and. (dist2 - dist1) > adist) RETURN
!!$
!!$!   print*,'Ca passe le shadow overlap sur l''intercentre'
!!$!   print*,'Critere = ',dist1 - dist2
!!$
!!$   gap=dist2-dist1
!!$   nb_shadow  = nb_shadow + 1
!!$
!!$   scal       = 0.D0
!!$
!!$!fd @@@ ... s-o sur les normales ...
!!$!fd @@@ on en profite pour ne rendre valide que les vertex
!!$!fd @@@ appartenant a des faces licites
!!$!fd @@@ a l'initialisation aucun vertex valide
!!$
!!$   ALLOCATE(cd_skip(PRcd%nb_vertex),an_skip(PRan%nb_vertex))
!!$   cd_skip = 0
!!$   an_skip = 0
!!$
!!$   DO i=1,PRan%nb_faces
!!$
!!$      if (bavard) Then
!!$        print*,'corps an - face ',i,'normale',PRan%normal(:,i)
!!$      endif
!!$
!!$!fd @@@ si la normale a la face n'est pas orientee sur la direction Nsep_o on jarte
!!$!fd @@@ N.B: au premier pas c'est la ligne des centres, apres c'est la derniere direction separatrice
!!$   
!!$      scal=DOT_PRODUCT(PRan%normal(:,i),Nsep_o(:))
!!$
!!$      IF (scal < -0.0001D0) THEN
!!$        if (bavard) print*,'on exclue cette face'
!!$        CYCLE
!!$      ENDIF
!!$
!!$      if (.not. skip_so) then 
!!$
!!$        Ncd(1:3)=PRan%normal(1:3,i)
!!$
!!$        dist1 =  PRan%val_support(i)
!!$        dist2 =  1.D+20
!!$        DO j=1,PRcd%nb_vertex  
!!$
!!$!fd new le 18/04/09
!!$!comme la fonction support de l'an est calculee sans le shift
!!$! et que c'est trop chiant a modifier, on decale le cd 
!!$
!!$          scal=DOT_PRODUCT((PRcd%vertex(:,j) - perio_shift(:)),Ncd(:))
!!$
!!$          IF (scal < dist2) THEN
!!$            dist2=scal
!!$          ENDIF
!!$        ENDDO
!!$ 
!!$!        print*,'Critère = ',dist1 - dist2
!!$
!!$        IF ((dist2 - dist1) > adist) THEN
!!$
!!$          IF (bavard) THEN
!!$            PRINT*,'Ca ne passe pas le shadow overlap PRan face ',i,'avec les vertex du PRcd'
!!$            PRINT*,'dist1= ',dist1,' dist2= ',dist2,' adist = ',adist
!!$          ENDIF
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$!fd @@@ la normale au plan separateur est Ncd
!!$
!!$          Nsep=Ncd
!!$
!!$          DEALLOCATE(cd_skip,an_skip)
!!$
!!$          RETURN
!!$
!!$        ENDIF   
!!$
!!$        IF ( (dist2 - dist1) < gap ) then
!!$          gap=dist2-dist1
!!$          Nsep=Ncd
!!$        ENDIF
!!$
!!$        nb_shadow = nb_shadow + 1
!!$      endif
!!$
!!$
!!$!fd @@@ on valide les vertex
!!$
!!$      DO j=1,3
!!$
!!$        an_skip(PRan%face(j,i)) = 1
!!$
!!$      ENDDO 
!!$
!!$   ENDDO
!!$
!!$   DO i=1,PRcd%nb_faces
!!$
!!$     if (bavard) Then
!!$       print*,'corps cd - face ',i,'normale',PRcd%normal(:,i)
!!$     endif
!!$
!!$     scal=DOT_PRODUCT(PRcd%normal(:,i),Nsep_o(:))
!!$
!!$     IF (scal > 0.0001D0) THEN
!!$        if (bavard) print*,'on exclue cette face'
!!$        CYCLE
!!$     ENDIF
!!$
!!$     if (.not. skip_so) then
!!$
!!$       Ncd=PRcd%normal(1:3,i)
!!$      
!!$       dist1 =  1.D+20
!!$       dist2 =  PRcd%val_support(i)
!!$
!!$       DO j=1,PRan%nb_vertex
!!$         scal= DOT_PRODUCT((PRan%vertex(:,j)+perio_shift(:)),Ncd(:))
!!$         IF (scal < dist1) THEN
!!$           dist1=scal
!!$         ENDIF
!!$       ENDDO
!!$
!!$!    print*,'Critère = ',dist2 - dist1
!!$
!!$       IF ( (dist1 - dist2) > adist) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'Ca ne passe pas le shadow overlap PRcd face ',i,'avec les vertex du PRan'
!!$           PRINT*,'dist1= ',dist2,' dist2= ',dist1,' adist = ',adist
!!$         ENDIF
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$!fd @@@ on garde moins Ncd car on va tester dans l'autre sens ...
!!$
!!$         Nsep=-Ncd
!!$
!!$         DEALLOCATE(cd_skip,an_skip)
!!$!       print*,'... desallocation>'
!!$
!!$         RETURN
!!$       ENDIF   
!!$
!!$       IF ( (dist1 - dist2) < gap) then
!!$          gap=dist1-dist2
!!$          Nsep = -Ncd
!!$       endif
!!$
!!$       nb_shadow = nb_shadow + 1
!!$
!!$     endif
!!$
!!$!fd @@@ on valide les vertex
!!$
!!$     DO j=1,3
!!$
!!$       cd_skip(PRcd%face(j,i)) = 1
!!$
!!$     ENDDO 
!!$
!!$   ENDDO
!!$
!!$!fd @@@ on verifie qu'il y a des vertex licites
!!$
!!$   DO i=1,SIZE(cd_skip)
!!$     IF (cd_skip(i) == 0) CYCLE
!!$     IF (cd_skip(i) == 1) EXIT
!!$     PRINT*,'aucun cd_skip'
!!$     STOP
!!$   ENDDO
!!$
!!$   DO i=1,SIZE(an_skip)
!!$     IF (an_skip(i) == 0) CYCLE
!!$     IF (an_skip(i) == 1) EXIT
!!$     PRINT*,'aucun an_skip'
!!$     STOP
!!$   ENDDO
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'1111111'
!!$     PRINT*,'Ca passe le shadow overlap'
!!$     PRINT*,'la distance la plus defavorable= ',gap
!!$     PRINT*,'la normale la plus defavorable =',Nsep
!!$
!!$     DO i=1,SIZE(cd_skip)
!!$       IF (cd_skip(i) == 0) PRINT*,'corps cd - le vertex ',i,' est invisible par la suite'
!!$     ENDDO
!!$
!!$     DO i=1,SIZE(an_skip)
!!$      IF (an_skip(i) == 0) PRINT*,'corps an - le vertex ',i,' est invisible par la suite'
!!$     ENDDO
!!$ 
!!$   ENDIF
!!$
!!$!fd @@@                                 end                                        @@@
!!$!fd @@@@@@@@@@@@@@@@@@@@@@@@@@ phase de shadow overlap @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$
!!$!rp @@@@@@ recherche de l'orientation du plan separateur à la F2F @@@@@@@@@@@@@@@@
!!$
!!$   if (id2 == idcd .and. id1 == idan) then 
!!$     print*,'nb cd set',size(PRcd%f2f_set)
!!$     do fi=1,size(PRcd%f2f_set)
!!$       print*,'set',fi,PRcd%f2f_set(fi)%G_i(:)
!!$     enddo
!!$     print*,'nb an set',size(PRan%f2f_set)
!!$     do fj=1,size(PRan%f2f_set)
!!$       print*,'set',fj,PRan%f2f_set(fj)%G_i(:)
!!$     enddo
!!$   endif
!!$!
!!$   !fd f2f donne mid_P et n_ini
!!$   f2f_found = .FALSE.
!!$   do fi=1,size(PRcd%f2f_set)
!!$     do fj=1,size(PRan%f2f_set)
!!$
!!$        dist1 = dot_product(PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)),PRan%normal(:,PRan%f2f_set(fj)%G_i(1)))
!!$
!!$        if ( dist1 < -1.d0 + f2f_tol) then
!!$          if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$            print*,'cas possible entre cd set ',fi,' an set ',fj
!!$            print*,'1er face du set cd',PRcd%f2f_set(fi)%G_i(1)
!!$            print*,'cd normale ',PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1))
!!$            print*,'1er face du set an',PRan%f2f_set(fj)%G_i(1)
!!$            print*,'an normale ',PRan%normal(:,PRan%f2f_set(fj)%G_i(1))
!!$
!!$            vec1(:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(1))) - PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(1)))
!!$            vec2(:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(1))) - PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(1)))
!!$            vec3(1) = vec1(2)*vec2(3) - vec1(3)*vec2(2)
!!$            vec3(2) = vec1(3)*vec2(1) - vec1(1)*vec2(3)
!!$            vec3(3) = vec1(1)*vec2(2) - vec1(2)*vec2(1)
!!$            print*,'cd normale ', vec3/sqrt(dot_product(vec3,vec3)) 
!!$                                  
!!$
!!$            vec1(:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(1))) - PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(1)))
!!$            vec2(:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(1))) - PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(1)))
!!$            vec3(1) = vec1(2)*vec2(3) - vec1(3)*vec2(2)
!!$            vec3(2) = vec1(3)*vec2(1) - vec1(1)*vec2(3)
!!$            vec3(3) = vec1(1)*vec2(2) - vec1(2)*vec2(1)
!!$            print*,'an normale ', vec3/sqrt(dot_product(vec3,vec3)) 
!!$                      
!!$            print*,'p scalaire ', dist1
!!$            print*,'tol', f2f_tol
!!$          endif 
!!$
!!$          !fd la normale est la "moyenne" de celles aux faces
!!$          n_ini = 0.5d0*(PRan%normal(:,PRan%f2f_set(fj)%G_i(1)) - PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)))
!!$          jmin = PRcd%face(1,PRcd%f2f_set(fi)%G_i(1))
!!$          imax=  PRan%face(1,PRan%f2f_set(fj)%G_i(1))
!!$
!!$          !fd pour eviter de garder des faces qui verifient le critere sur les normales 
!!$          !fd mais appartiennent a des objets qui ne sont pas en vis a vis
!!$          !fd inner radius est lie au rayon inscrit des polyedres
!!$          !fd TODO virer ca en virant les set qui sont incompatibles avec l'orientation de l'inter - centre
!!$
!!$          dist2 = dot_product(n_ini, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+perio_shift(:)) 
!!$
!!$          if (dist2 < -0.1*min(PRcd%inner_radius, PRan%inner_radius)) then
!!$            if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$              print*,'gap incoherent on ne garde pas'
!!$              print*,'distance normale entre sets ',dist2
!!$              print*,'distance de coupure ', -0.1*min(PRcd%inner_radius, PRan%inner_radius)
!!$            endif
!!$            cycle
!!$          endif
!!$
!!$          !fd si la distance est trop grande on vire aussi 
!!$
!!$          !if (dist2 > 1.5*adist) then
!!$          if (dist2 > adist) then
!!$            if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$              print*,'gap incoherent on ne garde pas'
!!$              print*,'distance normale entre sets ',dist2
!!$              print*,'distance d''alerte ', adist
!!$            endif 
!!$            cycle
!!$          endif
!!$
!!$          mid_P = 0.5*( PRcd%vertex(:,jmin) + (PRan%vertex(:,imax)+perio_shift(:)))
!!$
!!$          if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$            print*,'le cd set ',fi,' voit le an set ',fj
!!$            print*,'cd set ',PRcd%f2f_set(fi)%G_i(:)
!!$            print*,'an set ',PRan%f2f_set(fj)%G_i(:)
!!$            print*,'normale',n_ini
!!$            print*,'noeud cd ',jmin,' position ',PRcd%vertex(:,jmin)
!!$            print*,'noeud an ',imax,' position ',PRan%vertex(:,imax)+perio_shift(:)
!!$            print*,'gap ',dot_product(n_ini, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+perio_shift(:))
!!$            print*,'inner_radius ',PRcd%inner_radius,PRan%inner_radius
!!$          endif
!!$
!!$          ! si on passe ici ca veut dire qu'on a trouve et qu'on va sortir des boucles de test 
!!$
!!$          !fd on rend visible uniquement les noeuds du set
!!$          cd_skip = 0
!!$          do i=1,size(PRcd%f2f_set(fi)%G_i)
!!$            cd_skip(PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))=1            
!!$            cd_skip(PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))=1            
!!$            cd_skip(PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))=1            
!!$          enddo
!!$          if (bavard) print*,'cd_skip ',cd_skip
!!$
!!$          an_skip = 0
!!$          do i=1,size(PRan%f2f_set(fj)%G_i)
!!$            an_skip(PRan%face(1,PRan%f2f_set(fj)%G_i(i)))=1            
!!$            an_skip(PRan%face(2,PRan%f2f_set(fj)%G_i(i)))=1            
!!$            an_skip(PRan%face(3,PRan%f2f_set(fj)%G_i(i)))=1            
!!$          enddo
!!$
!!$          if (bavard) print*,'an_skip ',an_skip
!!$
!!$          f2f_found = .TRUE.
!!$
!!$          ! on essaie de comprendre ce qu'il se passe
!!$          ! histoire de ne pas gerer des structures de donnees a la noix 
!!$          ! on passe tout a la stl
!!$
!!$          if ((id2 == idcd .and. id1 == idan)) then
!!$            allocate (v_coor_cd(3*size(PRcd%f2f_set(fi)%G_i),3),&
!!$                      v_coor_an(3*size(PRan%f2f_set(fj)%G_i),3))
!!$
!!$            do i=1,size(PRcd%f2f_set(fi)%G_i)
!!$              v_coor_cd(3*(i-1)+1,:) = PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))
!!$              v_coor_cd(3*(i-1)+2,:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))
!!$              v_coor_cd(3*(i-1)+3,:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))
!!$            enddo
!!$
!!$            do i=1,size(PRan%f2f_set(fj)%G_i)
!!$              v_coor_an(3*(i-1)+1,:) = PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(i)))
!!$              v_coor_an(3*(i-1)+2,:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(i)))
!!$              v_coor_an(3*(i-1)+3,:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(i)))
!!$            enddo
!!$
!!$            call f2f_gmv_draw(polyr2bdyty(1,id2),polyr2bdyty(1,id1), &
!!$                              polyr2bdyty(2,id2),polyr2bdyty(2,id1), &
!!$                              size(PRcd%f2f_set(fi)%G_i),v_coor_cd, &
!!$                              size(PRan%f2f_set(fj)%G_i),v_coor_an )
!!$             
!!$            deallocate(v_coor_cd,v_coor_an)
!!$
!!$          endif
!!$
!!$
!!$          if (nstep == 1) then
!!$            allocate (v_coor_cd(3*size(PRcd%f2f_set(fi)%G_i),3),&
!!$                      v_coor_an(3*size(PRan%f2f_set(fj)%G_i),3))
!!$
!!$            do i=1,size(PRcd%f2f_set(fi)%G_i)
!!$              v_coor_cd(3*(i-1)+1,:) = PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))
!!$              v_coor_cd(3*(i-1)+2,:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))
!!$              v_coor_cd(3*(i-1)+3,:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))
!!$            enddo
!!$
!!$            do i=1,size(PRan%f2f_set(fj)%G_i)
!!$              v_coor_an(3*(i-1)+1,:) = PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(i)))
!!$              v_coor_an(3*(i-1)+2,:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(i)))
!!$              v_coor_an(3*(i-1)+3,:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(i)))
!!$            enddo
!!$
!!$            call f2f_gmv_draw_all(polyr2bdyty(1,id2),polyr2bdyty(1,id1), &
!!$                              polyr2bdyty(2,id2),polyr2bdyty(2,id1), &
!!$                              size(PRcd%f2f_set(fi)%G_i),v_coor_cd, &
!!$                              size(PRan%f2f_set(fj)%G_i),v_coor_an )
!!$             
!!$            deallocate(v_coor_cd,v_coor_an)
!!$
!!$          endif
!!$
!!$
!!$        endif
!!$        if (f2f_found) exit
!!$     enddo
!!$     if (f2f_found) exit
!!$   enddo
!!$
!!$   if (.not. f2f_found) return
!!$
!!$!fd @@@                                 end                                    @@@
!!$!rp @@@@@@ recherche de l'orientation du plan separateur à la F2F @@@@@@@@@@@@@@@@
!!$
!!$
!!$!rp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                                begin                                       @@@
!!$
!!$   norm=SQRT(DOT_PRODUCT(n_ini,n_ini))
!!$   mid_normal(:)=n_ini(:)/norm
!!$
!!$   mid_P(:)=((PRan%vertex(:,imax)+perio_shift(:))+PRcd%vertex(:,jmin))*0.5d0
!!$  
!!$!fd @@@ calcul repere local
!!$
!!$   Nr = mid_normal
!!$
!!$   call comp_rep(t,mid_normal,s)
!!$
!!$!--------------------------
!!$! pour afficher ce qu'on a calcule
!!$!
!!$!!   draw_gmv=.true.
!!$
!!$   if (draw_gmv) then
!!$
!!$! on essaie de comprendre ce qu'il se passe
!!$
!!$    allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$    do i=1,PRcd%nb_vertex
!!$      v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$    enddo
!!$
!!$    do i=1,PRan%nb_vertex
!!$      v_coor_an(i,:) = PRan%vertex(:,i)
!!$    enddo
!!$
!!$    rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$    v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$    v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$    v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$    v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$
!!$    call gmv_draw(polyr2bdyty(1,id2),polyr2bdyty(1,id1), &
!!$                  PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                  PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                  4,v_coor_ptc(1:4,1:3))
!!$
!!$    deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$
!!$   endif
!!$
!!$   draw_gmv = .false.
!!$
!!$   if (bavard) then
!!$     print*,'t ',t
!!$     print*,'n ',Nr
!!$     print*,'s ',s
!!$     print*,'P ',mid_P
!!$     ! pas encore calcule print*,'g ',d_max-d_min
!!$   endif
!!$
!!$!fd @@@                                end                                         @@@
!!$!rp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$!rp @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                              begin                                         @@@
!!$
!!$!rp @@@ Projection des vertex "licites" de l'antagoniste sur le plan moyen 
!!$
!!$! calcul d'une tolerance pour reduire le nombre de points licites a projeter
!!$ 
!!$! pour an calcul de la distance centre inertie polyr2bdyty(1,id2),polyr2bdyty(1,id1), &- point critique de Cundall
!!$
!!$   norm1=(PRan%vertex(1,imax)-PRan%center(1))**2+ &
!!$        (PRan%vertex(2,imax)-PRan%center(2))**2+ &
!!$        (PRan%vertex(3,imax)-PRan%center(3))**2
!!$
!!$! pour cd calcul de la distance centre inertie - point critique de Cundall
!!$
!!$   norm2=(PRcd%vertex(1,jmin)-PRcd%center(1))**2+ &
!!$        (PRcd%vertex(2,jmin)-PRcd%center(2))**2+ &
!!$        (PRcd%vertex(3,jmin)-PRcd%center(3))**2
!!$
!!$! On trouve la distance minimale
!!$
!!$   ref_size=MIN(norm1,norm2)
!!$
!!$   if (bavard) print*,'ref_size ',ref_size
!!$
!!$   ! Recuperation des vertex à projeter
!!$   ! coordonnees des points dans le plan separateur
!!$   ALLOCATE(ss1(2,PRan%nb_vertex),stat=errare)
!!$   ss1 = 0.d0
!!$
!!$   inc=0
!!$   DO i=1,PRan%nb_vertex 
!!$
!!$     if (an_skip(i)== 0) cycle 
!!$
!!$     inc=inc+1
!!$
!!$     ! Projection des vertex satisfaisant le critere d'alerte sur le plan moyen    
!!$
!!$     vec(:)=(PRan%vertex(:,i)+perio_shift(:))-mid_P(:)
!!$     d_an=DOT_PRODUCT(vec(:),mid_normal(:))
!!$! fd on ecrit tout dans le rep (mid_P,s,t)     s1(1:3)= PRan%vertex(:,i) + perio_shift(:) - (d_an*mid_normal(:))
!!$     s1(:)= vec(:) - (d_an*mid_normal(:))
!!$
!!$     ! Passage du repere general au repere du plan moyen (t,s)
!!$
!!$     ss1(1,inc) = s(1)*s1(1)+s(2)*s1(2)+s(3)*s1(3)
!!$     ss1(2,inc) = t(1)*s1(1)+t(2)*s1(2)+t(3)*s1(3)
!!$
!!$   ENDDO
!!$
!!$   nb_vertex_pran_min=inc
!!$
!!$   !
!!$   if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$     print*,'PR an ',polyr2bdyty(2,id1),' @ ',polyr2bdyty(1,id1)
!!$     print*,'vextex projetes ', nb_vertex_pran_min
!!$   endif
!!$
!!$   ! coordonnees des points dans le plan separateur
!!$   ALLOCATE(Pan(2,nb_vertex_pran_min),stat=errare)
!!$   Pan = 0.d0
!!$   !tableau d'indice pour la suite
!!$   allocate(id_Pan(nb_vertex_pran_min+1),stat=errare)
!!$   id_Pan=0
!!$
!!$   Pan=ss1(:,1:nb_vertex_pran_min)
!!$
!!$   deallocate(ss1)
!!$
!!$   if (bavard) then
!!$     print*,'nbv an projetes ',nb_vertex_pran_min 
!!$     do i=1,nb_vertex_pran_min
!!$       print*,Pan(:,i)
!!$     enddo
!!$   endif
!!$
!!$   call convex_hull(Pan,id_Pan,ref_size)
!!$
!!$   !fd le 1 est forcement dans la liste
!!$   inc=1
!!$   do i=2,nb_vertex_pran_min+1
!!$     if (id_Pan(i) /= 0 .and. id_Pan(i) /= id_Pan(1)) inc=inc+1
!!$   enddo
!!$
!!$   if (bavard) then
!!$     print*,'vertex anta qh (loop)'
!!$     do i=1,nb_vertex_pran_min+1
!!$       if (id_Pan(i) /= 0) print*,Pan(:,id_Pan(i))
!!$     enddo
!!$   endif
!!$
!!$   if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$     print*,'nb vertex candidat dans qh', inc
!!$     print*,'avant'
!!$     do i=1,nb_vertex_pran_min
!!$       print*,Pan(:,i)
!!$     enddo
!!$
!!$     print*,'apres'
!!$     do i=1,nb_vertex_pran_min+1
!!$       if (id_Pan(i) /= 0) print*,Pan(:,id_Pan(i))
!!$     enddo
!!$   endif
!!$
!!$
!!$
!!$!fd on ne shrink pas l'antagoniste
!!$
!!$   if (shrink > 0.d0) then
!!$     c_center = 0.d0
!!$     inc=0
!!$     do i=1,nb_vertex_pran_min+1
!!$       if (id_Pan(i) /= 0 .and. id_Pan(i) /= id_Pan(1)) then
!!$         inc = inc + 1
!!$         c_center = c_center + Pan(1:2,id_Pan(i))
!!$       endif
!!$     enddo
!!$     c_center = c_center / real(inc,8)
!!$     !fd on shrink tous les points et pas juste le convex_hull
!!$     do i=1,nb_vertex_pran_min
!!$       c_vec = Pan(1:2,i) - c_center  
!!$       Pan(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
!!$     enddo
!!$   endif

!!$   if (bavard) then
!!$     print*,'vertex anta enveloppe convexe shrink'
!!$     do i=1,nb_vertex_pran_min+1
!!$       if (id_Pan(i) /= 0) print*,Pan(:,id_Pan(i))
!!$     enddo
!!$   endif
!!$
!!$    ALLOCATE(rr1(2,PRcd%nb_vertex),stat=errare)
!!$    rr1 = 0.d0
!!$
!!$! Recuperation des vertex à projeter
!!$    inc=0
!!$    DO j=1,PRcd%nb_vertex 
!!$
!!$      if (cd_skip(j) == 0) cycle
!!$
!!$      inc=inc+1
!!$
!!$      ! Projection des vertex satisfaisant le critere d'alerte sur le plan moyen            
!!$      vec(:)=PRcd%vertex(:,j)-mid_P(:)
!!$      d_cd=DOT_PRODUCT(vec(:),mid_normal(:))
!!$!fd on ecrit tout dans le rep (mid_P,s,t)  r1(1:3)=PRcd%vertex(:,j)-(d_cd*mid_normal(:))
!!$      r1(:)=vec(:) - (d_cd*mid_normal(:))
!!$
!!$      ! Passage du repere general au repere du plan moyen (t,s)
!!$
!!$      rr1(1,inc)=s(1)*r1(1)+s(2)*r1(2)+s(3)*r1(3)
!!$      rr1(2,inc)=t(1)*r1(1)+t(2)*r1(2)+t(3)*r1(3)
!!$
!!$    ENDDO
!!$
!!$    nb_vertex_prcd_min=inc
!!$
!!$    if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$      print*,'PR cd ',polyr2bdyty(2,id2),' @ ',polyr2bdyty(1,id2)
!!$      print*,'vertex projetes ', nb_vertex_prcd_min
!!$    endif
!!$
!!$    ALLOCATE(Pcd(2,nb_vertex_prcd_min),stat=errare)
!!$    Pcd = 0.d0
!!$
!!$    !tableau d'indice pour la suite
!!$    allocate(id_Pcd(nb_vertex_prcd_min+1),stat=errare)
!!$    id_Pcd=0
!!$
!!$    Pcd=rr1(:,1:nb_vertex_prcd_min)
!!$
!!$    deallocate(rr1)
!!$
!!$    call convex_hull(Pcd,id_Pcd,ref_size)
!!$
!!$   !fd le 1 est forcement dans la liste
!!$    inc=1
!!$    do i=2,nb_vertex_prcd_min+1
!!$      if (id_Pcd(i) /= 0 .and. id_Pcd(i) /= id_Pcd(1)) inc=inc+1
!!$    enddo
!!$    !
!!$    if (bavard .or. (id2 == idcd .and. id1 == idan)) then 
!!$      print*,'nb vertex candidat dans qh', inc
!!$      print*,'avant'
!!$      do i=1,nb_vertex_prcd_min
!!$        print*,Pcd(:,i)
!!$      enddo
!!$
!!$      print*,'apres'
!!$      do i=1,nb_vertex_prcd_min+1
!!$        if (id_Pcd(i) /= 0) print*,Pcd(:,id_Pcd(i))
!!$      enddo
!!$    endif
!!$
!!$  
!!$    if (bavard) then
!!$      print*,'vertex candidat qh (loop)'
!!$      do i=1,nb_vertex_prcd_min+1
!!$        if (id_Pcd(i) /= 0) print*,Pcd(:,id_Pcd(i))
!!$      enddo
!!$    endif
!!$
!!$    if (shrink > 0.d0) then
!!$
!!$      !fd le 1 est forcement dans la liste
!!$      inc=1
!!$      c_center = Pcd(1:2,id_Pcd(1))
!!$
!!$      do i=2,nb_vertex_prcd_min+1
!!$        if (id_Pcd(i) /= 0 .and. id_Pcd(i) /= id_Pcd(1)) then
!!$          inc = inc + 1
!!$          c_center = c_center + Pcd(1:2,id_Pcd(i))
!!$        endif
!!$      enddo
!!$      c_center = c_center / real(inc,8)
!!$
!!$      !fd on shrink tous les points et pas juste le convex_hull
!!$      do i=1,nb_vertex_prcd_min
!!$        c_vec(1:2) = Pcd(1:2,i) - c_center(1:2)  
!!$        Pcd(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
!!$      enddo
!!$
!!$      if (bavard) then
!!$        print*,'vertex candidat enveloppe convexe shrink (loop)'
!!$        do i=1,nb_vertex_prcd_min+1
!!$          if (id_Pcd(i) /= 0) print *,Pcd(:,id_Pcd(i))
!!$        enddo
!!$      endif
!!$
!!$    endif
!!$
!!$    DEALLOCATE(cd_skip,an_skip)
!!$
!!$!fd @@@                              end                                           @@@
!!$! @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES enveloppes convexes @@@@@@@@@@@@@@@@@@@@@
!!$!fd @@@                              begin                                         @@@
!!$
!!$
!!$!?    IF (nb_vertex_pran_min /= 1 .or. nb_vertex_prcd_min /= 1 ) then
!!$
!!$
!!$   norm_min=MIN(PRan%min_radius_face,PRcd%min_radius_face) 
!!$   norm_max=MAX(PRan%max_radius_face,PRcd%max_radius_face) 
!!$
!!$   IF (norm_min/norm_max < 0.8) THEN 
!!$     !fd @@@ quand le ratio de taille des faces est petit
!!$     !norm=norm_min*0.5
!!$     ref_size=norm_min
!!$   ELSE
!!$     !fd @@@ quand le ratio de taille des faces est grand
!!$     !norm=norm_min*0.25
!!$     ref_size=norm_min
!!$   ENDIF
!!$
!!$   call polytopes_intersection(Pan,id_Pan,Pcd,id_Pcd,ref_size,points,nb_ptc,.false.)
!!$  
!!$   if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$     print*,id1,id2,'nombre d''intersection', nb_ptc
!!$     print*,'ref size',ref_size
!!$     print*,'points de l''intersection dans le plan'
!!$     do i=1,nb_ptc
!!$       print*,points(:,i)  
!!$     enddo 
!!$   endif 
!!$
!!$   !fd essaie pour virer les noeuds en trop
!!$   if ( nb_ptc > 4 ) then
!!$     
!!$     allocate(ptc(2,nb_ptc),id_ptc(nb_ptc+1))  
!!$     ptc(1:2,1:nb_ptc) = points(1:2,1:nb_ptc)
!!$     id_ptc = 0
!!$
!!$     !fd on calcule l'enveloppe convexe pour ne garder que ca 
!!$     !fd probablement pas utile 
!!$     !fd par contre ca tourne dans le sens trigo
!!$     call convex_hull(ptc,id_ptc,ref_size)
!!$
!!$     nb_ptc_qh = count(id_ptc /= 0) - 1
!!$
!!$     !fd si l'enveloppe convexe en a encore trop 
!!$
!!$     if (nb_ptc_qh > 4) then
!!$
!!$     !fd on essaie d'eliminer les noeuds le long des axes du contour 
!!$
!!$       !print*,'plus que 4'
!!$       !print*,'indexes qh ',id_ptc
!!$
!!$       do i=2,count(id_ptc /= 0) - 1 
!!$         if (id_ptc(i) == 0) then
!!$           print*,'strange id_ptc'
!!$           print*,id_ptc
!!$           stop
!!$         endif 
!!$         !if (id_ptc(i) < 0) cycle
!!$
!!$         vec1(1:2) = ptc(1:2,id_ptc(i)) - ptc(1:2,abs(id_ptc(i-1)))
!!$         vec2(1:2) = ptc(1:2,id_ptc(i+1)) - ptc(1:2,id_ptc(i))
!!$         tmp = abs(vec1(1)*vec2(2) - vec1(2)*vec2(1))
!!$         !print*, i, tmp , 1d-10 * ref_size
!!$         if ( tmp < 1d-10 * ref_size) id_ptc(i)=-id_ptc(i)
!!$       enddo
!!$
!!$       !print*,'indexes qh ',id_ptc   
!!$
!!$       inc=0
!!$       do i=1,count(id_ptc /= 0) - 1
!!$         if (id_ptc(i) < 0) cycle
!!$         inc = inc+1
!!$         points(1:2,inc) = ptc(1:2,id_ptc(i))
!!$       enddo
!!$       nb_ptc = count(id_ptc > 0) - 1

!!$
!!$       !fd on garde ceux qui ont les angles proches de 90 
!!$       !fd vec1.vec2 / |vec1| |vec2| proche de 0
!!$
!!$       !print*,'plus que 4'
!!$       !print*,'indexes qh ',id_ptc
!!$
!!$       allocate(angle(nb_ptc_qh))  
!!$
!!$       vec1(1:2) = ptc(1:2,id_ptc(1)) - ptc(1:2,id_ptc(nb_ptc_qh))
!!$       vec2(1:2) = ptc(1:2,id_ptc(2)) - ptc(1:2,id_ptc(1))
!!$
!!$       angle(1) = (vec1(1)*vec2(1) + vec1(2)*vec2(2)) / &
!!$          (sqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2)) * sqrt(vec2(1)*vec2(1) + vec2(2)*vec2(2)))
!!$
!!$
!!$       do i=2,nb_ptc_qh
!!$         if (id_ptc(i) == 0) then
!!$           print*,'strange id_ptc'
!!$           print*,id_ptc
!!$           stop
!!$         endif 
!!$         !if (id_ptc(i) < 0) cycle
!!$
!!$         vec1(1:2) = ptc(1:2,id_ptc(i)) - ptc(1:2,abs(id_ptc(i-1)))
!!$         vec2(1:2) = ptc(1:2,id_ptc(i+1)) - ptc(1:2,id_ptc(i))
!!$
!!$         angle(i) = (vec1(1)*vec2(1) + vec1(2)*vec2(2)) / &
!!$            (sqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2)) * sqrt(vec2(1)*vec2(1) + vec2(2)*vec2(2)))
!!$
!!$       enddo
!!$
!!$       !print*,'indexes qh ',id_ptc   
!!$
!!$       do i=1,4
!!$           inc = minloc(array=angle,dim=1)
!!$           angle(inc)=1.d+20
!!$           points(1:2,i) = ptc(1:2,id_ptc(inc))
!!$       enddo
!!$
!!$       nb_ptc = 4
!!$
!!$       deallocate(angle)
!!$
!!$       !print*,nb_ptc
!!$
!!$     else 
!!$
!!$        !print*,'moins que 4'
!!$        !print*,'indexes qh ',id_ptc
!!$
!!$        do i=1,count(id_ptc /= 0) -1
!!$          if (id_ptc(i) == 0) then
!!$            print*,'strange id_ptc'
!!$            print*,id_ptc
!!$            stop
!!$          endif 
!!$          points(1:2,i) = ptc(1:2,id_ptc(i))
!!$        enddo
!!$        nb_ptc = count(id_ptc /= 0) -1
!!$     endif
!!$
!!$     deallocate(ptc,id_ptc)
!!$
!!$   endif
!!$
!!$   if (bavard .or. (id2 == idcd .and. id1 == idan)) then
!!$     print*,id1,id2,'nombre de points retenus', nb_ptc
!!$     print*,'ref size',ref_size
!!$     print*,'points retenus'
!!$     do i=1,nb_ptc
!!$       print*,points(:,i)  
!!$     enddo 
!!$   endif 
!!$
!!$   DEALLOCATE(Pan,id_Pan,Pcd,id_Pcd)
!!$
!!$!fd @@@                              end                                         @@@
!!$!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES eveloppes convexes @@@@@@@@@@@@@@@@@@@@@
!!$
!!$
!!$!fd projection sur un plan pour les points non coplanaires ?
!!$
!!$!fd on calcule le centre du nuage.
!!$!fd on vire le plus proche
!!$!fd on recommence ...
!!$
!!$   ! mettre une valeur en parametre
!!$   ! 4/3 pour un contact face/face
!!$
!!$   if (nb_ptc > 4) then
!!$     is_ok=1
!!$     allocate(dist(nb_ptc))
!!$
!!$     DO i=1,nb_ptc
!!$       PT(:)=PT(:)+points(:,i)
!!$     ENDDO
!!$
!!$     PT=PT/REAL(nb_ptc,8)
!!$
!!$     DO i=1,nb_ptc
!!$       dist(i)=dot_product(PT(:)-points(:,i),PT(:)-points(:,i))
!!$     ENDDO
!!$
!!$     do i=1,4
!!$       inc=maxloc(dist,dim=1)        
!!$       is_ok(inc)=0
!!$       dist(inc)=0.d0
!!$     enddo
!!$   else
!!$     is_ok=0
!!$   endif
!!$
!!$   if (bavard) then
!!$     print*,'mid_P ',mid_P
!!$     print*,'s '    ,s
!!$     print*,'t '    ,t 
!!$  
!!$     print*,'point de l''intersection dans le repere global' 
!!$   endif
!!$
!!$
!!$   !fd on retente un shrink
!!$   inc=0
!!$   c_center = 0.d0
!!$   do i=1,nb_ptc
!!$     if (is_ok(i) /= 0 ) cycle
!!$     inc = inc + 1 
!!$     c_center(1:2) = c_center(1:2) + points(1:2,i)
!!$   enddo
!!$   if (inc /= 0) then
!!$     c_center = c_center / real(inc,8)
!!$
!!$     !fd on shrink tous les points 
!!$     do i=1,nb_ptc
!!$       if (is_ok(i) /= 0 ) cycle
!!$       c_vec(1:2) = points(1:2,i) - c_center(1:2)  
!!$       points(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
!!$     enddo
!!$
!!$   endif
!!$
!!$   nb_select=0
!!$   do i=1,nb_ptc
!!$     if (is_ok(i) /= 0 ) cycle
!!$     nb_select = nb_select + 1 
!!$     PT_CTC(:,nb_select) = mid_P(:) + (points(1,i)*s(:)) + (points(2,i)*t(:))
!!$
!!$     if (bavard) print*,pt_ctc(:,nb_select)
!!$     
!!$   enddo
!!$ 
!!$   if (nb_select < 3) then
!!$     ! fd est ce utile ? print*,'Not enough contact nodes between the surfaces'
!!$     nb_ctc = 0
!!$     return
!!$   endif
!!$
!!$   IF (nb_select <= 4) THEN
!!$     nb_ctc=nb_select 
!!$!        print*,'nbselect',nb_select 
!!$   ELSE
!!$     nb_ctc=4
!!$   ENDIF
!!$
!!$   v2v%cd=id2
!!$   v2v%an=id1
!!$   v2v%nb_ctc=nb_ctc
!!$
!!$   !print*,'on pousse dans visavis'
!!$   !print*,'cd/an',v2v%cd,v2v%an
!!$   !print*,'nb_ctc',v2v%nb_ctc
!!$
!!$   DO ic=1,nb_ctc
!!$     ! recherche de la face cd qui contient le noeud projete suivant la direction mid_nornal
!!$     is_inside = .false.
!!$
!!$     !fd essai pour eviter les erreur d'arrondie
!!$     !shift_ptc(1:3) =  PT_CTC(1:3,ic) - mid_normal(1:3)
!!$
!!$     do iff=1,size(PRcd%f2f_set(fi)%G_i)
!!$       do i=1,3
!!$         pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
!!$       enddo
!!$       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_min,v2v%cd_lcoor(:,ic),bavard)  
!!$       if (is_inside) then
!!$         my_cd_iff = iff
!!$         exit
!!$       endif
!!$     enddo
!!$     if (.not. is_inside) then
!!$       print*,'======================================='
!!$       call LOGMES('Error '//IAM//': unable to find a cd face')
!!$       write(*,'(a,i0,a,i0,a,i0)') 'contact ',ic,' entre ',id2,' et ',id1
!!$       write(*,'(a,3(1x,D12.5))') 'position ',pt_ctc(:,ic)
!!$       write(*,'(a,3(1x,D12.5))') 'normale ',mid_normal
!!$       do iff=1,size(PRcd%f2f_set(fi)%G_i)
!!$          write(*,'(a,3(1x,i0))')'face ', PRcd%f2f_set(fi)%G_i(iff)       
!!$         write(*,'(a,3(1x,D12.5))') 'n ', PRcd%normal(:,PRcd%f2f_set(fi)%G_i(iff))
!!$ 
!!$         do i=1,3
!!$          write(*,'(a,i0,3(1x,D12.5))') 'v',i,PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
!!$          pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
!!$          print*,'vertex',i,pp(:,i)
!!$         enddo
!!$         !!is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_min,v2v%cd_lcoor(:,ic),.true.)  
!!$       enddo
!!$
!!$       !print*,'Ensemble des points de contact entre ces deux POLYR'
!!$       !do ix =1,nb_ctc
!!$       !   write(*,'(i0,3(1x,D12.5))') ix,pt_ctc(:,ix)
!!$       !enddo
!!$       ! on essaie de comprendre ce qu'il se passe
!!$
!!$       allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$       do i=1,PRcd%nb_vertex
!!$         v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$       enddo
!!$
!!$       do i=1,PRan%nb_vertex
!!$        v_coor_an(i,:) = PRan%vertex(:,i)
!!$       enddo
!!$
!!$
!!$       ! pour voir le plan
!!$       !rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$       !
!!$       !v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$       !v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$       !v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$       !v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$       ! pout voir le point qui fait chier 
!!$       !rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$       !v_coor_ptc(1,:) = pt_ctc(:,ic) + rd*0.01*(- t(:) - s(:))     
!!$       !v_coor_ptc(2,:) = pt_ctc(:,ic) + rd*0.01*(+ t(:) - s(:))   
!!$       !v_coor_ptc(3,:) = pt_ctc(:,ic) + rd*0.01*(+ t(:) + s(:))  
!!$       !v_coor_ptc(4,:) = pt_ctc(:,ic) + rd*0.01*(- t(:) + s(:))  
!!$
!!$       ! pout voir le point qui fait chier 
!!$
!!$       DO ix=1,4
!!$         if (ix <= nb_ctc) then
!!$           v_coor_ptc(ix,:) = pt_ctc(:,ix)
!!$         else
!!$           v_coor_ptc(ix,:) = pt_ctc(:,1)
!!$         endif
!!$       enddo
!!$
!!$       call e_gmv_draw(polyr2bdyty(1,id2),polyr2bdyty(1,id1), &
!!$                       polyr2bdyty(2,id2),polyr2bdyty(2,id1), &
!!$                  PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                  PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                  4,v_coor_ptc(1:4,1:3))
!!$
!!$       deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$
!!$       nb_ctc = 0
!!$       return
!!$     endif
!!$
!!$     v2v%iff_cd(ic) = PRcd%f2f_set(fi)%G_i(my_cd_iff)
!!$
!!$     !print*,'iff_cd ',v2v%iff_cd(ic)
!!$     !print*,'nodes ',PRcd%face(:,PRcd%f2f_set(fi)%G_i(my_cd_iff))
!!$     !print*,'w ',v2v%cd_lcoor(:,ic)
!!$
!!$     is_inside = .false.
!!$     do iff=1,size(PRan%f2f_set(fj)%G_i)
!!$       do i=1,3
!!$         pp(:,i) = PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(iff))) 
!!$       enddo
!!$       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_max,v2v%an_lcoor(:,ic),bavard)  
!!$       if (is_inside) then
!!$         my_an_iff = iff
!!$         exit
!!$       endif
!!$     enddo
!!$
!!$     if (.not. is_inside) then
!!$       call LOGMES('Error '//IAM//': unable to find an an face')
!!$       write(*,'(a,i0,a,i0,a,i0)') 'contact ',ic,' entre ',id2,' et ',id1
!!$       write(*,'(a,3(1x,D12.5))') 'position ',pt_ctc(:,ic)
!!$       write(*,'(a,3(1x,D12.5))') 'normale ',mid_normal
!!$       do iff=1,size(PRan%f2f_set(fj)%G_i)
!!$         print*,'face ', PRan%f2f_set(fj)%G_i(iff)       
!!$         print*,'n ', PRan%normal(:,PRan%f2f_set(fj)%G_i(iff))
!!$         do i=1,3
!!$          pp(:,i)=PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(iff))) 
!!$          print*,'vertex',i,pp(:,i)
!!$         enddo
!!$         !!is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_max,v2v%an_lcoor(:,ic),.true.)  
!!$       enddo       
!!$       ! on essaie de comprendre ce qu'il se passe
!!$
!!$       allocate (v_coor_cd(PRcd%nb_vertex,3),v_coor_an(PRan%nb_vertex,3),v_coor_ptc(6,3))
!!$
!!$       do i=1,PRcd%nb_vertex
!!$         v_coor_cd(i,:) = PRcd%vertex(:,i)
!!$       enddo
!!$
!!$       do i=1,PRan%nb_vertex
!!$        v_coor_an(i,:) = PRan%vertex(:,i)
!!$       enddo
!!$
!!$       !rd= 0.5*(S_POLYR(id1)%radius+S_POLYR(id2)%radius)
!!$
!!$       !v_coor_ptc(1,:) = mid_P(:) + rd*(- t(:) - s(:))     
!!$       !v_coor_ptc(2,:) = mid_P(:) + rd*(+ t(:) - s(:))   
!!$       !v_coor_ptc(3,:) = mid_P(:) + rd*(+ t(:) + s(:))  
!!$       !v_coor_ptc(4,:) = mid_P(:) + rd*(- t(:) + s(:))  
!!$
!!$       DO ix=1,4
!!$         if (ix <= nb_ctc) then
!!$           v_coor_ptc(ix,:) = pt_ctc(:,ix)
!!$         else
!!$           v_coor_ptc(ix,:) = pt_ctc(:,1)
!!$         endif
!!$       enddo
!!$
!!$       call e_gmv_draw(polyr2bdyty(1,id2),polyr2bdyty(1,id1), &
!!$                     polyr2bdyty(2,id2),polyr2bdyty(2,id1), &
!!$                  PRcd%nb_vertex,v_coor_cd,PRcd%nb_faces,PRcd%face, &
!!$                  PRan%nb_vertex,v_coor_an,PRan%nb_faces,PRan%face, &
!!$                  4,v_coor_ptc(1:4,1:3))
!!$
!!$       deallocate(v_coor_cd,v_coor_an,v_coor_ptc)
!!$
!!$       nb_ctc = 0
!!$       return
!!$
!!$     endif
!!$
!!$     v2v%iff_an(ic) = PRan%f2f_set(fj)%G_i(my_an_iff)
!!$
!!$     !print*,'iff_an',v2v%iff_an(ic)
!!$     !print*,'nodes ',PRan%face(:,PRan%f2f_set(fj)%G_i(my_an_iff))
!!$     !print*,'w',v2v%an_lcoor(:,ic)
!!$
!!$     ! la distance entre les 2 points
!!$
!!$     overlap(ic)=d_min-d_max
!!$
!!$     if (bavard) then
!!$       print *,'contact: ', ic
!!$       print *,'distance: ',overlap(ic)
!!$       print *,'distances cd/an',d_min,d_max
!!$       print *,'weight cd',v2v%cd_lcoor(:,ic)
!!$       print *,'weight an',v2v%an_lcoor(:,ic)
!!$     endif
!!$   enddo
!!$
!!$ END SUBROUTINE DETECTION_F2F_new
!!$!*********

!!$!------------------------------------------------------------------------
!!$ SUBROUTINE compute_explicit_contact_PRPRx(prefix)
!!$ 
!!$   IMPLICIT NONE  
!!$
!!$   INTEGER                     :: errare 
!!$   INTEGER                     :: icdan,iadj,ibdy,icdbdy,ianbdy,itac
!!$   INTEGER                     :: icdtac,iantac,isee,itacty    
!!$   REAL(kind=8)                :: raycd,rayan,adist,dist,nonuc,gap
!!$   INTEGER                     :: r_icdbdy,r_ianbdy,r_icdtac,r_iantac
!!$   INTEGER                     :: i,id,j,nb_ctc
!!$   INTEGER                     :: size_of_array_this
!!$   REAL(kind=8),DIMENSION(3,4) :: xco,cd_cooref,an_cooref
!!$   REAL(kind=8),DIMENSION(4)   :: ovlap
!!$   REAL(kind=8),DIMENSION(3)   :: sep,t,n,s,cdlev,anlev,Point,Vsep
!!$   REAL(kind=8)                :: norme,den,scal
!!$   REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
!!$   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an
!!$   REAL(kind=8)                :: t1,t2,vls_cst,vln_cst,vlt_cst
!!$   REAL(kind=8)                :: pt_surf
!!$
!!$   CHARACTER(len=5) :: cdcol,ancol
!!$   CHARACTER(len=2) :: prefix
!!$
!!$   LOGICAL :: is_first_time =.TRUE.
!!$   INTEGER :: i_visavis
!!$
!!$   integer :: cd_ent,an_ent
!!$
!!$!fd les trucs de Gilles
!!$   nb_shadow=0
!!$   nb_tot_detect=0
!!$   nb_detection_test=0
!!$   detection_time=0.d0
!!$   nb_ctc_state=0
!!$!fd
!!$
!!$   is_explicit = .TRUE.
!!$
!!$   i_visavis=0
!!$       
!!$   size_of_array_this=SIZE(this)
!!$  
!!$   IF (is_first_time) THEN
!!$
!!$     nb_visavis=0
!!$     IF (nb_rough_PRPRx /= 0 ) THEN
!!$
!!$       ALLOCATE(visavis(size_of_array_this))
!!$       do i=1,size_of_array_this
!!$         allocate(visavis(i)%iff_cd(4),visavis(i)%iff_an(4), &
!!$                  visavis(i)%cd_lcoor(3,4),visavis(i)%an_lcoor(3,4), &
!!$                  visavis(i)%index(4))
!!$       enddo
!!$       !
!!$       ! preparation de la detection 
!!$       !
!!$       DO i=1,nb_rough_PRPRx
!!$         icdbdy    = rough_PRPRx(i)%cd
!!$         ianbdy    = rough_PRPRx(i)%an 
!!$         isee      = rough_PRPRx(i)%isee
!!$         Vsep(1:3) = rough_PRPRx(i)%Vsep(1:3)
!!$
!!$         adist=see(isee)%alert 
!!$         dist=S_POLYR(icdbdy)%radius+S_POLYR(ianbdy)%radius+adist
!!$
!!$         sep=S_POLYR(icdbdy)%center - S_POLYR(ianbdy)%center
!!$         norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)
!!$
!!$         IF (norme <1.D-24) THEN
!!$           PRINT*,'Error in mod_PRPRx, in compute contact polyr'
!!$           PRINT*,' Distance between',icdbdy, ' and ',ianbdy, 'is'
!!$           PRINT*,norme 
!!$           STOP
!!$         ENDIF   
!!$
!!$
!!$!fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
!!$!fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
!!$!fd @@@ et il en manque je pense ...
!!$
!!$         IF (norme < dist*dist) THEN
!!$
!!$!fd @@@ ca n'est pas suffisant non ?
!!$
!!$           IF ((S_POLYR(ianbdy)%maxpos(1)-S_POLYR(icdbdy)%minpos(1)+adist)<0.D0) CYCLE
!!$           IF ((S_POLYR(ianbdy)%maxpos(2)-S_POLYR(icdbdy)%minpos(2)+adist)<0.D0) CYCLE
!!$           IF ((S_POLYR(ianbdy)%maxpos(3)-S_POLYR(icdbdy)%minpos(3)+adist)<0.D0) CYCLE
!!$
!!$!fd @@@ je rajoute ....
!!$
!!$           IF ((S_POLYR(icdbdy)%maxpos(1)-S_POLYR(ianbdy)%minpos(1)+adist)<0.D0) CYCLE
!!$           IF ((S_POLYR(icdbdy)%maxpos(2)-S_POLYR(ianbdy)%minpos(2)+adist)<0.D0) CYCLE
!!$           IF ((S_POLYR(icdbdy)%maxpos(3)-S_POLYR(ianbdy)%minpos(3)+adist)<0.D0) CYCLE
!!$
!!$           sep=sep/SQRT(norme)
!!$
!!$           scal=sep(1)*Vsep(1)+sep(2)*Vsep(2)+sep(3)*Vsep(3)
!!$
!!$           IF (scal==0.D0) rough_PRPRx(i)%Vsep=sep
!!$
!!$           cdcol=get_color_POLYR(icdbdy)
!!$           ancol=get_color_POLYR(ianbdy)
!!$
!!$!fd plus necessaire           if (cdcol(1:2) == prefix .and.  ancol(1:2) == prefix) then
!!$
!!$             i_visavis = i_visavis + 1
!!$
!!$             IF (i_visavis > size_of_array_this) THEN
!!$               PRINT*,'taille de visavis trop petite'
!!$               STOP
!!$             ENDIF
!!$
!!$             CALL INTERSECTION_EXPLICIT(ianbdy,icdbdy,visavis(i_visavis))
!!$
!!$             CALL ini_explicit(visavis(i_visavis))
!!$
!!$             visavis(i_visavis)%isee = isee
!!$
!!$!fd           endif
!!$         ENDIF
!!$       ENDDO
!!$     ENDIF
!!$     is_first_time=.FALSE.
!!$     nb_visavis = i_visavis
!!$   ENDIF
!!$
!!$   nb_PRPRx= 0
!!$   nb_adj  = 0
!!$   icdan   = 0        
!!$
!!$   DO i_visavis=1,nb_visavis
!!$
!!$     isee = visavis(i_visavis)%isee
!!$
!!$     CALL updt_EXPLICIT(visavis(i_visavis),r_icdbdy,r_ianbdy,nb_ctc,xco,n,ovlap,pt_surf,cd_cooref,an_cooref)
!!$
!!$     IF (nb_ctc==0) CYCLE
!!$
!!$     sep=  S_POLYR(r_icdbdy)%center - S_POLYR(r_ianbdy)%center
!!$
!!$     IF ((n(1)*sep(1)+n(2)*sep(2)+n(3)*sep(3))<0.D0) THEN
!!$!fd       if (is_explicit) then
!!$        
!!$         PRINT*,'y a qq chose de foireux dans la definition de la normale qui n''est pas alignee avec l''intercentre '
!!$         PRINT*,'POLYR candidat ',r_icdbdy,' POLYR antagoniste ',r_ianbdy
!!$         PRINT*,'intercentre ',sep
!!$         PRINT*,'normale ',n
!!$         STOP
!!$
!!$!fd       else
!!$!fd burk ... a changer !!
!!$!fd         n=-n
!!$!fd       endif
!!$     ENDIF
!!$
!!$     norme = SQRT(n(1)*n(1)+n(2)*n(2)+n(3)*n(3))
!!$     IF (norme/=0.D0) THEN
!!$       n = n/norme
!!$     ELSE 
!!$       PRINT*,'Le vecteur normale est mal calcule'
!!$       PRINT*,n(1),n(2),n(3),norme
!!$       STOP
!!$     ENDIF
!!$
!!$     !fd cas ou la normale est alignee sur un axe
!!$
!!$     IF(( n(1)*n(2) == 0.D0 ).AND. &
!!$        ( n(2)*n(3) == 0.D0 ).AND. &
!!$        ( n(3)*n(1) == 0.D0 )) THEN
!!$                         
!!$!fd @@@ et allez ... le repere c'est t,n,s !!!
!!$!fd @@@           t(1)= n(3)
!!$!fd @@@           t(2)= n(1)
!!$!fd @@@           t(3)= n(2)
!!$
!!$       IF (n(1) /= 0.d0 .OR. n(2) /= 0.d0) THEN  
!!$         s = (/ 0., 0., 1. /)
!!$         t(1) =  n(2)*s(3) - n(3)*s(2)
!!$         t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$         t(3) =  n(1)*s(2) - n(2)*s(1)
!!$       ELSE
!!$         s = (/ 0., 1., 0. /)
!!$         t(1) =  n(2)*s(3) - n(3)*s(2)
!!$         t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$         t(3) =  n(1)*s(2) - n(2)*s(1)
!!$       ENDIF
!!$     ELSE
!!$       IF (dabs(n(1)) > 0.9 .OR. dabs(n(2)) > 0.9) THEN  
!!$         s = (/ 0., 0., 1. /)
!!$         t(1) =  n(2)*s(3) - n(3)*s(2)
!!$         t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$         t(3) =  n(1)*s(2) - n(2)*s(1)
!!$         norme = SQRT(t(1)*t(1)+t(2)*t(2)+t(3)*t(3))
!!$       ELSE IF (dabs(n(3)) > 0.9) THEN
!!$         s = (/ 0., 1., 0. /)
!!$         t(1) =  n(2)*s(3) - n(3)*s(2)
!!$         t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$         t(3) =  n(1)*s(2) - n(2)*s(1)
!!$       ELSE
!!$         den=1.D0/SQRT( (n(2)*n(3))**2 &
!!$                       +(n(1)*n(3))**2 &
!!$                       +4.*(n(1)*n(2))**2)
!!$                         
!!$          t(1)=    n(2)*n(3)*den
!!$          t(2)=    n(3)*n(1)*den
!!$          t(3)=  -2*n(1)*n(2)*den
!!$       ENDIF
!!$     ENDIF
!!$
!!$!fd @@@ et allez c'est a l'envers
!!$!fd @@@        s(1)     =  t(3)*n(2) - t(2)*n(3)
!!$!fd @@@        s(2)     =  t(1)*n(3) - t(3)*n(1)              
!!$!fd @@@        s(3)     =  t(2)*n(1) - t(1)*n(2)
!!$
!!$!fd @@@ je corrige ...
!!$
!!$     norme = SQRT(t(1)*t(1)+t(2)*t(2)+t(3)*t(3))
!!$     t = t/norme
!!$
!!$     s(1)     =  t(2)*n(3) - t(3)*n(2)
!!$     s(2)     =  t(3)*n(1) - t(1)*n(3)              
!!$     s(3)     =  t(1)*n(2) - t(2)*n(1)
!!$
!!$     norme = SQRT(s(1)*s(1)+s(2)*s(2)+s(3)*s(3))
!!$     s=s/norme
!!$
!!$     icdbdy = r_icdbdy
!!$     ianbdy = r_ianbdy
!!$
!!$     localframe_cd = get_inertia_frameTT_POLYR(icdbdy)
!!$     localframe_an = get_inertia_frameTT_POLYR(ianbdy)
!!$
!!$     cd_Vbegin = get_vlocy_POLYR(icdbdy,iVbeg_)
!!$     an_Vbegin = get_vlocy_POLYR(ianbdy,iVbeg_)
!!$           
!!$     vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ (cd_Vbegin(2)-an_Vbegin(2))*t(2)+(cd_Vbegin(3)-an_Vbegin(3))*t(3)
!!$     vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ (cd_Vbegin(2)-an_Vbegin(2))*n(2)+(cd_Vbegin(3)-an_Vbegin(3))*n(3)
!!$     vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ (cd_Vbegin(2)-an_Vbegin(2))*s(2)+(cd_Vbegin(3)-an_Vbegin(3))*s(3)
!!$
!!$     DO j=1,nb_ctc
!!$
!!$       icdan = icdan+1	  	       	       
!!$       IF (icdan>size_of_array_this) THEN
!!$         PRINT*,'---------------------------------------------'
!!$         PRINT*,'ERROR filling this                           '
!!$         PRINT*,'you rich the allocated size                  '
!!$         PRINT*,'In COMMAND.DAT you should add the keywords : '
!!$         PRINT*,'LOW SIZE ARRAY POLYR                         ' 
!!$         PRINT*,'sizefactor                                   '
!!$         PRINT*,'where sizefactor is an integer specifiyng the'
!!$         PRINT*,'ratio of memory you need (=4 by default)     '
!!$         PRINT*,'Program stop                                 '
!!$         PRINT*,'---------------------------------------------'
!!$         STOP
!!$       ENDIF   
!!$
!!$       nb_adj(icdbdy)          = nb_adj(icdbdy)+1
!!$       iadj                    = nb_adj(icdbdy)
!!$       this(icdan)%iadj        = iadj  
!!$       this(icdan)%icdbdy      = polyr2bdyty(1,icdbdy)
!!$       this(icdan)%icdtac      = icdbdy
!!$       this(icdan)%ianbdy      = polyr2bdyty(1,ianbdy)
!!$       this(icdan)%iantac      = ianbdy
!!$       this(icdan)%isee        = isee    	             
!!$       this(icdan)%tuc         = t
!!$       this(icdan)%nuc         = n
!!$       this(icdan)%suc         = s
!!$
!!$       this(icdan)%coor    = xco(1:3,j)
!!$       this(icdan)%type_ctc    = nb_ctc
!!$
!!$       cd_ent = get_ent_POLYR(this(icdan)%icdtac)
!!$       an_ent = get_ent_POLYR(this(icdan)%iantac) 
!!$         
!!$       entity(cd_ent)%nb = entity(cd_ent)%nb+1
!!$       entity(an_ent)%nb = entity(an_ent)%nb+1
!!$
!!$       cdlev = xco(1:3,j)-PRcoor(1:3,icdbdy)
!!$       anlev = xco(1:3,j)-PRcoor(1:3,ianbdy)
!!$
!!$!fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie de la configuration initiale
!!$
!!$       this(icdan)%coorcd(:)=cd_cooref(:,j)
!!$
!!$       this(icdan)%cooran(:)=an_cooref(:,j)
!!$
!!$!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!!$!             print*,icdan
!!$!             print*, xco(1:3,j)
!!$!             print*,this(icdan)%coorcd(1:3)
!!$!             print*,this(icdan)%cooran(1:3)
!!$!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!!$
!!$
!!$       ! On va calculer le passage rep inertie -> rep général pour l'antagoniste
!!$
!!$        Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
!!$        Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
!!$        Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)
!!$
!!$        Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
!!$        Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
!!$        Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)
!!$
!!$        Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
!!$        Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
!!$        Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)
!!$
!!$        this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) &
!!$                                                        + Rc(1,3)*this(icdan)%tuc(3) 
!!$        this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) &
!!$                                                        + Rc(2,3)*this(icdan)%tuc(3) 
!!$        this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) &
!!$                                                        + Rc(3,3)*this(icdan)%tuc(3) 
!!$
!!$        this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) &
!!$                                                        + Rc(1,3)*this(icdan)%nuc(3) 
!!$        this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) &
!!$                                                        + Rc(2,3)*this(icdan)%nuc(3) 
!!$        this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) &
!!$                                                        + Rc(3,3)*this(icdan)%nuc(3) 
!!$
!!$        this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) &
!!$                                                        + Rc(1,3)*this(icdan)%suc(3) 
!!$        this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) &
!!$                                                        + Rc(2,3)*this(icdan)%suc(3) 
!!$        this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) &
!!$                                                        + Rc(3,3)*this(icdan)%suc(3) 
!!$
!!$
!!$        ! On va calculer le passage rep inertie -> rep général pour le candidat
!!$
!!$        Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
!!$        Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
!!$        Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)
!!$
!!$        Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
!!$        Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
!!$        Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)
!!$
!!$        Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
!!$        Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
!!$        Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)
!!$
!!$
!!$        this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
!!$        this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
!!$        this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 
!!$
!!$        this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
!!$        this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
!!$        this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 
!!$
!!$        this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
!!$        this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
!!$        this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
!!$           
!!$        ! Calcul des vitesses relatives
!!$
!!$        this(icdan)%gapTTbegin = ovlap(j)
!!$
!!$        this(icdan)%vltBEGIN = vlt_cst &
!!$                             + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
!!$                             - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)
!!$
!!$        this(icdan)%vlnBEGIN = vln_cst &     
!!$                             + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
!!$                             - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)
!!$
!!$        this(icdan)%vlsBEGIN= vls_cst &     
!!$                            + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
!!$                            - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)
!!$
!!$
!!$        this(icdan)%rls      = 0.D0
!!$        this(icdan)%rlt      = 0.D0
!!$        this(icdan)%rln      = 0.D0
!!$        this(icdan)%vls      = this(icdan)%vlsBEGIN
!!$        this(icdan)%vlt      = this(icdan)%vltBEGIN
!!$        this(icdan)%vln      = this(icdan)%vlnBEGIN
!!$        this(icdan)%gapTT    = this(icdan)%gapTTbegin
!!$        this(icdan)%status   = 'nknow'
!!$
!!$        this(icdan)%surf     = pt_surf
!!$
!!$!fd        if (is_explicit) then
!!$          this(icdan)%node_rank = j
!!$!fd        else
!!$!fd          this(icdan)%node_rank = 0
!!$!fd        endif
!!$     ENDDO
!!$   ENDDO
!!$
!!$   nb_PRPRx = icdan
!!$
!!$   WRITE(*,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
!!$
!!$
!!$   CALL get_behaviour
!!$
!!$   DO ibdy=1,nb_POLYR
!!$     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
!!$     IF (nb_adj(ibdy) /= 0) THEN
!!$       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
!!$       IF (errare /=0 ) THEN
!!$         PRINT*,'error in allocating adjac(icdbdy)%....., in select_prox_tactors in mod_PRPRx'
!!$         STOP
!!$       END IF
!!$     ENDIF
!!$   ENDDO 
!!$ 
!!$
!!$   DO icdan=1,nb_PRPRx
!!$     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
!!$   END DO 
!!$
!!$
!!$   IF (ALLOCATED(violation)) DEALLOCATE(violation)
!!$   ALLOCATE(violation(nb_PRPRx),stat=errare)
!!$
!!$
!!$
!!$ END SUBROUTINE compute_explicit_contact_PRPRx
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE INTERSECTION_explicit(id1,id2,CT)
!!$
!!$! routine de construction des points porteurs des surfaces
!!$
!!$! I
!!$! id1 : id solide 1 (antagoniste)
!!$! id2 : id solide 2 (candidat)
!!$!
!!$! O
!!$! CT : une structure contenant les infos
!!$!
!!$   IMPLICIT NONE
!!$
!!$
!!$   INTEGER                          :: id1,id2,idir,i
!!$   TYPE(T_visavis)                  :: CT
!!$
!!$   TYPE(T_POLYR),TARGET             :: PR1,PR2
!!$   TYPE(T_POLYR),POINTER            :: PR_cd,PR_an   
!!$
!!$   LOGICAL :: bavard=.FALSE.
!!$
!!$   REAL(kind=8) :: pr1_L1,pr1_L2,pr2_L1,pr2_L2,geotol=3.d-3
!!$   REAL(kind=8),DIMENSION(3)::v
!!$   INTEGER :: pr1_sens,pr2_sens
!!$
!!$   PRINT*,'briques ',id1,id2
!!$
!!$   PR1    = S_POLYR(id1)
!!$   PR2    = S_POLYR(id2)
!!$
!!$!fd test preliminaire
!!$
!!$   IF (NSTEP == 1) THEN
!!$     IF (PR1%nb_vertex /=8 .AND. PR2%nb_vertex /=8) THEN   
!!$       PRINT*,'routine destinee aux maconnerie composee de briques a 8 nouds'
!!$       STOP
!!$     ENDIF
!!$   ENDIF
!!$
!!$!fd @@@ on suppose que les dimensions des briques determinent leur orientations par rapport aux axes x et y
!!$!fd @@@ z est necessairement la verticale !!
!!$
!!$   v = PR1%vertex(:,2)-PR1%vertex(:,1) 
!!$   pr1_L1 = DOT_PRODUCT(v,v)  
!!$   v = PR1%vertex(:,4)-PR1%vertex(:,1) 
!!$   pr1_L2 = DOT_PRODUCT(v,v)  
!!$
!!$!fd @@@ sens x=1, sens y=2
!!$
!!$   IF (pr1_L1 > pr1_L2) THEN
!!$     pr1_sens=1
!!$   ELSE
!!$     pr1_sens=2
!!$   ENDIF 
!!$
!!$   v = PR2%vertex(:,2)-PR2%vertex(:,1) 
!!$   pr2_L1 = DOT_PRODUCT(v,v)  
!!$   v = PR2%vertex(:,4)-PR2%vertex(:,1) 
!!$   pr2_L2 = DOT_PRODUCT(v,v)  
!!$
!!$   PRINT*,'pr1_l1 ',pr1_l1,'pr1_l2 ',pr1_l2
!!$   PRINT*,'pr2_l1 ',pr2_l1,'pr2_l2 ',pr2_l2
!!$
!!$!fd @@@ sens x=1, sens y=2
!!$
!!$   IF (pr2_L1 > pr2_L2) THEN
!!$     pr2_sens=1
!!$   ELSE
!!$     pr2_sens=2
!!$   ENDIF 
!!$  
!!$!fd @@@ on laisse ca pour info mais ca n a aucune importance
!!$
!!$   IF (pr1_sens == 1 .AND. pr2_sens == 1) THEN
!!$
!!$     PRINT*,'x/x'
!!$
!!$   ELSE IF ((pr1_sens == 1 .AND. pr2_sens == 2) .OR. (pr1_sens == 2 .AND. pr2_sens == 1)) THEN
!!$
!!$     PRINT*,'x/y'
!!$
!!$   ELSE IF (pr1_sens == 2 .AND. pr2_sens == 2) THEN
!!$
!!$     PRINT*,'y/y'
!!$
!!$   ELSE
!!$     PRINT*,'bof bof ca ne devrait pas arriver' 
!!$     STOP
!!$   ENDIF
!!$
!!$!fd trois types de rangement :
!!$!fd  idir=3 au dessus de l'autre celui du dessus est le cd    
!!$!fd  idir=2 a cote sens y celui le plus loin en y est le cd
!!$!fd  idir=1 a cote sens x celui le plus loin en x est le cd
!!$!fd
!!$!fd on peut remarquer que dans ces cas la normale doit toujours etre sens + !!
!!$
!!$   IF ( dabs(PR2%minpos(3) - PR1%maxpos(3)) < geotol) THEN 
!!$
!!$     PRINT*,'2 au dessus de 1 sens z'
!!$
!!$     idir = 3
!!$
!!$     CT%cd=id2
!!$     CT%an=id1
!!$
!!$     PR_cd => PR2
!!$     PR_an => PR1
!!$
!!$   ELSE IF ( dabs(PR2%maxpos(3) - PR1%minpos(3)) < geotol) THEN
!!$
!!$     PRINT*,'2 au dessous de 1 sens z: on permute'
!!$
!!$     idir = 3
!!$
!!$     CT%cd=id1
!!$     CT%an=id2
!!$
!!$     PR_cd => PR1
!!$     PR_an => PR2
!!$
!!$   ELSE
!!$
!!$     IF ( dabs(PR2%minpos(2) - PR1%maxpos(2)) < geotol) THEN 
!!$
!!$       PRINT*,'2 au dessus de 1 sens y'
!!$       idir = 2
!!$
!!$       CT%cd=id2
!!$       CT%an=id1
!!$
!!$       PR_cd => PR2
!!$       PR_an => PR1
!!$
!!$     ELSE IF ( dabs(PR2%maxpos(2) - PR1%minpos(2)) < geotol) THEN
!!$
!!$       PRINT*,'2 au dessous de 1 sens y: on permute'
!!$
!!$       idir = 2
!!$
!!$       CT%cd=id1
!!$       CT%an=id2
!!$
!!$       PR_cd => PR1
!!$       PR_an => PR2
!!$
!!$     ELSE
!!$
!!$       IF ( dabs(PR2%minpos(1) - PR1%maxpos(1)) < geotol) THEN 
!!$
!!$         PRINT*,'2 au dessus de 1 sens x'
!!$         idir = 1
!!$
!!$         CT%cd=id2
!!$         CT%an=id1
!!$
!!$         PR_cd => PR2
!!$         PR_an => PR1
!!$
!!$       ELSE IF ( dabs(PR2%maxpos(1) - PR1%minpos(1)) < geotol) THEN
!!$
!!$         PRINT*,'2 au dessous de 1 sens x: on permute'
!!$ 
!!$         idir = 1
!!$
!!$         CT%cd=id1
!!$         CT%an=id2
!!$
!!$         PR_cd => PR1
!!$         PR_an => PR2
!!$
!!$       ELSE
!!$         PRINT*,'moi y en pas compndre' 
!!$         STOP
!!$       ENDIF
!!$     ENDIF
!!$   ENDIF 
!!$
!!$!fd candidat au dessus
!!$
!!$   IF (idir == 3) THEN
!!$
!!$     CT%an_fac=11
!!$     CT%an_pt=5
!!$
!!$     IF (PR_cd%minpos(1) < PR_an%minpos(1)) THEN
!!$
!!$         CT%cd_lcoor(1,1) = 2.d0*((PR_cd%vertex(1,1) - PR_an%vertex(1,5)) / &
!!$                                  (PR_cd%vertex(1,1) - PR_cd%vertex(1,2))) - 1.d0                    
!!$         CT%cd_lcoor(1,4) = 2.d0*((PR_cd%vertex(1,4) - PR_an%vertex(1,8)) / &
!!$                                  (PR_cd%vertex(1,4) - PR_cd%vertex(1,3))) - 1.d0                    
!!$       
!!$         CT%an_lcoor(1,1) = -1.d0
!!$         CT%an_lcoor(1,4) = -1.d0
!!$
!!$     ELSE
!!$
!!$         CT%cd_lcoor(1,1) = -1.d0
!!$         CT%cd_lcoor(1,4) = -1.d0
!!$
!!$         CT%an_lcoor(1,1) = 2.d0*((PR_an%vertex(1,5) - PR_cd%vertex(1,1)) / &
!!$                                  (PR_an%vertex(1,5) - PR_an%vertex(1,6))) - 1.d0                    
!!$         CT%an_lcoor(1,4) = 2.d0*((PR_an%vertex(1,8) - PR_cd%vertex(1,4)) / &
!!$                                  (PR_an%vertex(1,8) - PR_an%vertex(1,7))) - 1.d0                    
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%maxpos(1) > PR_an%maxpos(1)) THEN 
!!$
!!$         CT%cd_lcoor(1,2) = 2.d0*((PR_cd%vertex(1,1) - PR_an%vertex(1,6)) / &
!!$                                  (PR_cd%vertex(1,1) - PR_cd%vertex(1,2))) - 1.d0                    
!!$         CT%cd_lcoor(1,3) = 2.d0*((PR_cd%vertex(1,4) - PR_an%vertex(1,7)) / &
!!$                                  (PR_cd%vertex(1,4) - PR_cd%vertex(1,3))) - 1.d0                   
!!$
!!$         CT%an_lcoor(1,2) = 1.d0
!!$         CT%an_lcoor(1,3) = 1.d0
!!$
!!$     ELSE 
!!$
!!$         CT%cd_lcoor(1,2) = 1.d0
!!$         CT%cd_lcoor(1,3) = 1.d0
!!$
!!$         CT%an_lcoor(1,2) = 2.d0*((PR_an%vertex(1,5) - PR_cd%vertex(1,2)) / &
!!$                                  (PR_an%vertex(1,5) - PR_an%vertex(1,6))) - 1.d0                    
!!$         CT%an_lcoor(1,3) = 2.d0*((PR_an%vertex(1,8) - PR_cd%vertex(1,3)) / &
!!$                                  (PR_an%vertex(1,8) - PR_an%vertex(1,7))) - 1.d0                   
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%minpos(2) < PR_an%minpos(2)) THEN
!!$
!!$         CT%cd_lcoor(2,1) = 2.d0*((PR_cd%vertex(2,1) - PR_an%vertex(2,5)) / &
!!$                                  (PR_cd%vertex(2,1) - PR_cd%vertex(2,4))) - 1.d0                    
!!$         CT%cd_lcoor(2,2) = 2.d0*((PR_cd%vertex(2,2) - PR_an%vertex(2,6)) / &
!!$                                  (PR_cd%vertex(2,2) - PR_cd%vertex(2,3))) - 1.d0                    
!!$       
!!$         CT%an_lcoor(2,1) = -1.d0
!!$         CT%an_lcoor(2,2) = -1.d0
!!$
!!$     ELSE
!!$
!!$         CT%cd_lcoor(2,1) = -1.d0
!!$         CT%cd_lcoor(2,2) = -1.d0
!!$
!!$         CT%an_lcoor(2,1) = 2.d0*((PR_an%vertex(2,5) - PR_cd%vertex(2,1)) / &
!!$                                  (PR_an%vertex(2,5) - PR_an%vertex(2,8))) - 1.d0                    
!!$         CT%an_lcoor(2,2) = 2.d0*((PR_an%vertex(2,6) - PR_cd%vertex(2,2)) / &
!!$                                  (PR_an%vertex(2,6) - PR_an%vertex(2,7))) - 1.d0                    
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%maxpos(2) > PR_an%maxpos(2)) THEN 
!!$
!!$         CT%cd_lcoor(2,3) = 2.d0*((PR_cd%vertex(2,2) - PR_an%vertex(2,7)) / &
!!$                                  (PR_cd%vertex(2,2) - PR_cd%vertex(2,3))) - 1.d0                    
!!$         CT%cd_lcoor(2,4) = 2.d0*((PR_cd%vertex(2,1) - PR_an%vertex(2,8)) / &
!!$                                  (PR_cd%vertex(2,1) - PR_cd%vertex(2,4))) - 1.d0                   
!!$
!!$         CT%an_lcoor(2,3) = 1.d0
!!$         CT%an_lcoor(2,4) = 1.d0
!!$
!!$     ELSE 
!!$
!!$         CT%cd_lcoor(2,3) = 1.d0
!!$         CT%cd_lcoor(2,4) = 1.d0
!!$
!!$         CT%an_lcoor(2,3) = 2.d0*((PR_an%vertex(2,6) - PR_cd%vertex(2,3)) / &
!!$                                  (PR_an%vertex(2,6) - PR_an%vertex(2,7))) - 1.d0                    
!!$         CT%an_lcoor(2,4) = 2.d0*((PR_an%vertex(2,5) - PR_cd%vertex(2,4)) / &
!!$                                  (PR_an%vertex(2,5) - PR_an%vertex(2,8))) - 1.d0                   
!!$
!!$     ENDIF
!!$
!!$     CT%cd_lcoor(3,:) = -1.d0
!!$     CT%an_lcoor(3,:) = +1.d0
!!$
!!$   ELSE IF (idir == 2) THEN
!!$
!!$     CT%an_fac=5
!!$     CT%an_pt=3
!!$
!!$     IF (PR_cd%minpos(1) < PR_an%minpos(1)) THEN
!!$
!!$         CT%cd_lcoor(1,1) = 2.d0*((PR_cd%vertex(1,1) - PR_an%vertex(1,4)) / &
!!$                                  (PR_cd%vertex(1,1) - PR_cd%vertex(1,2))) - 1.d0                    
!!$         CT%cd_lcoor(1,4) = 2.d0*((PR_cd%vertex(1,5) - PR_an%vertex(1,8)) / &
!!$                                  (PR_cd%vertex(1,5) - PR_cd%vertex(1,6))) - 1.d0                    
!!$       
!!$         CT%an_lcoor(1,1) = -1.d0
!!$         CT%an_lcoor(1,4) = -1.d0
!!$
!!$     ELSE
!!$
!!$         CT%cd_lcoor(1,1) = -1.d0
!!$         CT%cd_lcoor(1,4) = -1.d0
!!$
!!$         CT%an_lcoor(1,1) = 2.d0*((PR_an%vertex(1,4) - PR_cd%vertex(1,1)) / &
!!$                                  (PR_an%vertex(1,4) - PR_an%vertex(1,3))) - 1.d0                    
!!$         CT%an_lcoor(1,4) = 2.d0*((PR_an%vertex(1,8) - PR_cd%vertex(1,5)) / &
!!$                                  (PR_an%vertex(1,8) - PR_an%vertex(1,7))) - 1.d0                    
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%maxpos(1) > PR_an%maxpos(1)) THEN 
!!$
!!$         CT%cd_lcoor(1,2) = 2.d0*((PR_cd%vertex(1,1) - PR_an%vertex(1,3)) / &
!!$                                  (PR_cd%vertex(1,1) - PR_cd%vertex(1,2))) - 1.d0                    
!!$         CT%cd_lcoor(1,3) = 2.d0*((PR_cd%vertex(1,5) - PR_an%vertex(1,7)) / &
!!$                                  (PR_cd%vertex(1,5) - PR_cd%vertex(1,6))) - 1.d0                   
!!$
!!$         CT%an_lcoor(1,2) = 1.d0
!!$         CT%an_lcoor(1,3) = 1.d0
!!$
!!$     ELSE 
!!$
!!$         CT%cd_lcoor(1,2) = 1.d0
!!$         CT%cd_lcoor(1,3) = 1.d0
!!$
!!$         CT%an_lcoor(1,2) = 2.d0*((PR_an%vertex(1,4) - PR_cd%vertex(1,2)) / &
!!$                                  (PR_an%vertex(1,4) - PR_an%vertex(1,3))) - 1.d0                    
!!$         CT%an_lcoor(1,3) = 2.d0*((PR_an%vertex(1,8) - PR_cd%vertex(1,6)) / &
!!$                                  (PR_an%vertex(1,8) - PR_an%vertex(1,7))) - 1.d0                   
!!$
!!$     ENDIF
!!$
!!$
!!$     CT%cd_lcoor(2,:) = -1.d0
!!$     CT%an_lcoor(2,:) = +1.d0
!!$
!!$     IF (PR_cd%minpos(3) < PR_an%minpos(3)) THEN
!!$
!!$         CT%cd_lcoor(3,1) = 2.d0*((PR_cd%vertex(3,1) - PR_an%vertex(3,4)) / &
!!$                                  (PR_cd%vertex(3,1) - PR_cd%vertex(3,5))) - 1.d0                    
!!$         CT%cd_lcoor(3,2) = 2.d0*((PR_cd%vertex(3,2) - PR_an%vertex(3,3)) / &
!!$                                  (PR_cd%vertex(3,2) - PR_cd%vertex(3,6))) - 1.d0                    
!!$       
!!$         CT%an_lcoor(3,1) = -1.d0
!!$         CT%an_lcoor(3,2) = -1.d0
!!$
!!$     ELSE
!!$
!!$         CT%cd_lcoor(3,1) = -1.d0
!!$         CT%cd_lcoor(3,2) = -1.d0
!!$
!!$         CT%an_lcoor(3,1) = 2.d0*((PR_an%vertex(3,4) - PR_cd%vertex(3,1)) / &
!!$                                  (PR_an%vertex(3,4) - PR_an%vertex(3,8))) - 1.d0                    
!!$         CT%an_lcoor(3,2) = 2.d0*((PR_an%vertex(3,3) - PR_cd%vertex(3,2)) / &
!!$                                  (PR_an%vertex(3,3) - PR_an%vertex(3,7))) - 1.d0                    
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%maxpos(3) > PR_an%maxpos(3)) THEN 
!!$
!!$         CT%cd_lcoor(3,3) = 2.d0*((PR_cd%vertex(3,2) - PR_an%vertex(3,7)) / &
!!$                                  (PR_cd%vertex(3,2) - PR_cd%vertex(3,6))) - 1.d0                    
!!$         CT%cd_lcoor(3,4) = 2.d0*((PR_cd%vertex(3,1) - PR_an%vertex(3,8)) / &
!!$                                  (PR_cd%vertex(3,1) - PR_cd%vertex(3,5))) - 1.d0                   
!!$
!!$         CT%an_lcoor(3,3) = 1.d0
!!$         CT%an_lcoor(3,4) = 1.d0
!!$
!!$     ELSE 
!!$
!!$         CT%cd_lcoor(3,3) = 1.d0
!!$         CT%cd_lcoor(3,4) = 1.d0
!!$
!!$         CT%an_lcoor(3,3) = 2.d0*((PR_an%vertex(3,3) - PR_cd%vertex(3,6)) / &
!!$                                  (PR_an%vertex(3,3) - PR_an%vertex(3,7))) - 1.d0                    
!!$         CT%an_lcoor(3,4) = 2.d0*((PR_an%vertex(3,4) - PR_cd%vertex(3,5)) / &
!!$                                  (PR_an%vertex(3,4) - PR_an%vertex(3,8))) - 1.d0                   
!!$
!!$     ENDIF
!!$
!!$
!!$   ELSE IF (idir == 1) THEN
!!$
!!$     CT%an_fac=3
!!$     CT%an_pt=2
!!$
!!$     CT%cd_lcoor(1,:) = -1.d0
!!$     CT%an_lcoor(1,:) = +1.d0
!!$
!!$     IF (PR_cd%minpos(2) < PR_an%minpos(2)) THEN
!!$
!!$         CT%cd_lcoor(2,1) = 2.d0*((PR_cd%vertex(2,1) - PR_an%vertex(2,2)) / &
!!$                                  (PR_cd%vertex(2,1) - PR_cd%vertex(2,4))) - 1.d0                    
!!$         CT%cd_lcoor(2,4) = 2.d0*((PR_cd%vertex(2,5) - PR_an%vertex(2,6)) / &
!!$                                  (PR_cd%vertex(2,5) - PR_cd%vertex(2,8))) - 1.d0                    
!!$       
!!$         CT%an_lcoor(2,1) = -1.d0
!!$         CT%an_lcoor(2,4) = -1.d0
!!$
!!$     ELSE
!!$
!!$         CT%cd_lcoor(2,1) = -1.d0
!!$         CT%cd_lcoor(2,4) = -1.d0
!!$
!!$         CT%an_lcoor(2,1) = 2.d0*((PR_an%vertex(2,2) - PR_cd%vertex(2,1)) / &
!!$                                  (PR_an%vertex(2,2) - PR_an%vertex(2,3))) - 1.d0                    
!!$         CT%an_lcoor(2,4) = 2.d0*((PR_an%vertex(2,6) - PR_cd%vertex(2,5)) / &
!!$                                  (PR_an%vertex(2,6) - PR_an%vertex(2,7))) - 1.d0                    
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%maxpos(2) > PR_an%maxpos(2)) THEN 
!!$
!!$         CT%cd_lcoor(2,2) = 2.d0*((PR_cd%vertex(2,1) - PR_an%vertex(2,3)) / &
!!$                                  (PR_cd%vertex(2,1) - PR_cd%vertex(2,4))) - 1.d0                    
!!$         CT%cd_lcoor(2,3) = 2.d0*((PR_cd%vertex(2,5) - PR_an%vertex(2,7)) / &
!!$                                  (PR_cd%vertex(2,5) - PR_cd%vertex(2,8))) - 1.d0                   
!!$
!!$         CT%an_lcoor(2,2) = 1.d0
!!$         CT%an_lcoor(2,3) = 1.d0
!!$
!!$     ELSE 
!!$
!!$         CT%cd_lcoor(2,2) = 1.d0
!!$         CT%cd_lcoor(2,3) = 1.d0
!!$
!!$         CT%an_lcoor(2,2) = 2.d0*((PR_an%vertex(2,2) - PR_cd%vertex(2,4)) / &
!!$                                  (PR_an%vertex(2,2) - PR_an%vertex(2,3))) - 1.d0                    
!!$         CT%an_lcoor(2,3) = 2.d0*((PR_an%vertex(2,6) - PR_cd%vertex(2,8)) / &
!!$                                  (PR_an%vertex(2,6) - PR_an%vertex(2,7))) - 1.d0                   
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%minpos(3) < PR_an%minpos(3)) THEN
!!$
!!$         CT%cd_lcoor(3,1) = 2.d0*((PR_cd%vertex(3,1) - PR_an%vertex(3,2)) / &
!!$                                  (PR_cd%vertex(3,1) - PR_cd%vertex(3,5))) - 1.d0                    
!!$         CT%cd_lcoor(3,2) = 2.d0*((PR_cd%vertex(3,4) - PR_an%vertex(3,3)) / &
!!$                                  (PR_cd%vertex(3,4) - PR_cd%vertex(3,8))) - 1.d0                    
!!$       
!!$         CT%an_lcoor(3,1) = -1.d0
!!$         CT%an_lcoor(3,2) = -1.d0
!!$
!!$     ELSE
!!$
!!$         CT%cd_lcoor(3,1) = -1.d0
!!$         CT%cd_lcoor(3,2) = -1.d0
!!$
!!$         CT%an_lcoor(3,1) = 2.d0*((PR_an%vertex(3,2) - PR_cd%vertex(3,1)) / &
!!$                                  (PR_an%vertex(3,2) - PR_an%vertex(3,8))) - 1.d0                    
!!$         CT%an_lcoor(3,2) = 2.d0*((PR_an%vertex(3,3) - PR_cd%vertex(3,4)) / &
!!$                                  (PR_an%vertex(3,3) - PR_an%vertex(3,7))) - 1.d0                    
!!$
!!$     ENDIF
!!$
!!$     IF (PR_cd%maxpos(3) > PR_an%maxpos(3)) THEN 
!!$
!!$         CT%cd_lcoor(3,3) = 2.d0*((PR_cd%vertex(3,4) - PR_an%vertex(3,7)) / &
!!$                                  (PR_cd%vertex(3,4) - PR_cd%vertex(3,8))) - 1.d0                    
!!$         CT%cd_lcoor(3,4) = 2.d0*((PR_cd%vertex(3,1) - PR_an%vertex(3,6)) / &
!!$                                  (PR_cd%vertex(3,1) - PR_cd%vertex(3,5))) - 1.d0                   
!!$
!!$         CT%an_lcoor(3,3) = 1.d0
!!$         CT%an_lcoor(3,4) = 1.d0
!!$
!!$     ELSE 
!!$
!!$         CT%cd_lcoor(3,3) = 1.d0
!!$         CT%cd_lcoor(3,4) = 1.d0
!!$
!!$         CT%an_lcoor(3,3) = 2.d0*((PR_an%vertex(3,3) - PR_cd%vertex(3,8)) / &
!!$                                  (PR_an%vertex(3,3) - PR_an%vertex(3,7))) - 1.d0                    
!!$         CT%an_lcoor(3,4) = 2.d0*((PR_an%vertex(3,2) - PR_cd%vertex(3,5)) / &
!!$                                  (PR_an%vertex(3,2) - PR_an%vertex(3,6))) - 1.d0                   
!!$
!!$     ENDIF
!!$   ENDIF
!!$
!!$   PRINT*,'intersection explicite'
!!$   PRINT*,'cd ',CT%cd,' an ',CT%an
!!$   DO i=1,4
!!$     PRINT*,'point ',i  
!!$     PRINT*,'cd ',CT%cd_lcoor(:,i)
!!$     PRINT*,'an ',CT%an_lcoor(:,i)
!!$   ENDDO
!!$ END SUBROUTINE INTERSECTION_explicit
!!$
!!$ SUBROUTINE updt_explicit(CT,id2,id1,nb_ctc,pt_ctc,nr,overlap,pt_surf,cd_cooref,an_cooref)
!!$!
!!$! I
!!$! CT
!!$!
!!$! O 
!!$! nb_ctc            : nombre de points de contact
!!$! PT_CTC(1:nb_ctc)  : coordonnees des points de contact
!!$! Nr(1)             : normales aux points de contact  (bizarre bizarrre)
!!$! overlap(1:nb_ctc) : les gaps aux points de contact
!!$! pt_surf: la surface affectee a un point de contact
!!$! id2,id1: le candidat et l'antagoniste
!!$
!!$   IMPLICIT NONE
!!$   INTEGER                          :: id1,id2,nb_ctc,i
!!$   REAL(kind=8),DIMENSION(3)        :: Nr
!!$   REAL(kind=8),DIMENSION(3,4)      :: PT,PT_CTC,an_pt,cd_cooref,an_cooref
!!$   REAL(kind=8),DIMENSION(4)        :: overlap
!!$   TYPE(T_POLYR)                    :: PR1,PR2
!!$   TYPE(T_visavis)                  :: CT
!!$   REAL(kind=8)                     :: a,b,pt_surf
!!$
!!$   nb_ctc=4
!!$
!!$   id1 = CT%an
!!$   id2 = CT%cd
!!$
!!$   PR1 = S_POLYR(id1)
!!$   PR2 = S_POLYR(id2)
!!$
!!$!fd construction de la position des points candidats par interpolation des noeuds 
!!$!fd porteurs des surfaces cd et an 
!!$
!!$!fd actualisation position noeuds porteurs candidats 
!!$
!!$   DO i=1,4
!!$     PT(:,i) = interpolate_H8(CT%cd_lcoor(1,i),CT%cd_lcoor(2,i),CT%cd_lcoor(3,i),PR2%VERTEX)
!!$!     print*,'cd points porteurs conf globale',i, pt(:,i)
!!$   ENDDO
!!$
!!$!fd interpolation sur la face candidate
!!$
!!$   PT_CTC(:,1)=fformQ4(1,1)*PT(:,1)+ &
!!$               fformQ4(2,1)*PT(:,2)+ &
!!$               fformQ4(3,1)*PT(:,3)+ &
!!$               fformQ4(4,1)*PT(:,4)
!!$
!!$   PT_CTC(:,2)=fformQ4(1,2)*PT(:,1)+ &
!!$               fformQ4(2,2)*PT(:,2)+ &
!!$               fformQ4(3,2)*PT(:,3)+ &
!!$               fformQ4(4,2)*PT(:,4)
!!$
!!$   PT_CTC(:,3)=fformQ4(1,3)*PT(:,1)+ &
!!$               fformQ4(2,3)*PT(:,2)+ &
!!$               fformQ4(3,3)*PT(:,3)+ &
!!$               fformQ4(4,3)*PT(:,4)
!!$
!!$   PT_CTC(:,4)=fformQ4(1,4)*PT(:,1)+ &
!!$               fformQ4(2,4)*PT(:,2)+ &
!!$               fformQ4(3,4)*PT(:,3)+ &
!!$               fformQ4(4,4)*PT(:,4)
!!$
!!$!   do i=1,4
!!$!     print*,'cd point de contact conf globale',i,pt_ctc(:,i)
!!$!   enddo
!!$
!!$
!!$!fd actualisation position noeuds porteurs antagonistes
!!$
!!$   DO i=1,4
!!$     PT(:,i) = interpolate_H8(CT%an_lcoor(1,i),CT%an_lcoor(2,i),CT%an_lcoor(3,i),PR1%VERTEX)
!!$!     print*,'an points porteurs conf globale',i, pt(:,i)
!!$   ENDDO
!!$
!!$   AN_PT(:,1) =fformQ4(1,1)*PT(:,1)+ &
!!$               fformQ4(2,1)*PT(:,2)+ &
!!$               fformQ4(3,1)*PT(:,3)+ &
!!$               fformQ4(4,1)*PT(:,4)
!!$
!!$   AN_PT(:,2) =fformQ4(1,2)*PT(:,1)+ &
!!$               fformQ4(2,2)*PT(:,2)+ &
!!$               fformQ4(3,2)*PT(:,3)+ &
!!$               fformQ4(4,2)*PT(:,4)
!!$
!!$   AN_PT(:,3) =fformQ4(1,3)*PT(:,1)+ &
!!$               fformQ4(2,3)*PT(:,2)+ &
!!$               fformQ4(3,3)*PT(:,3)+ &
!!$               fformQ4(4,3)*PT(:,4)
!!$
!!$   AN_PT(:,4) =fformQ4(1,4)*PT(:,1)+ &
!!$               fformQ4(2,4)*PT(:,2)+ &
!!$               fformQ4(3,4)*PT(:,3)+ &
!!$               fformQ4(4,4)*PT(:,4)
!!$
!!$!   do i=1,4
!!$!     print*,'an point de contact conf globale',i,an_pt(:,i)
!!$!   enddo
!!$
!!$
!!$
!!$   Nr(:)=PR1%normal(:,CT%an_fac)
!!$
!!$   overlap(1) = DOT_PRODUCT(PT_CTC(:,1)-an_pt(:,1),Nr(:))
!!$   overlap(2) = DOT_PRODUCT(PT_CTC(:,2)-an_pt(:,2),Nr(:))
!!$   overlap(3) = DOT_PRODUCT(PT_CTC(:,3)-an_pt(:,3),Nr(:))
!!$   overlap(4) = DOT_PRODUCT(PT_CTC(:,4)-an_pt(:,4),Nr(:))
!!$
!!$   a = SQRT(DOT_PRODUCT(PT_CTC(:,2)-PT_CTC(:,1),PT_CTC(:,2)-PT_CTC(:,1)))
!!$   b = SQRT(DOT_PRODUCT(PT_CTC(:,4)-PT_CTC(:,1),PT_CTC(:,4)-PT_CTC(:,1)))
!!$
!!$   pt_surf = a*b*0.25
!!$
!!$   cd_cooref = CT%coorcd
!!$   an_cooref = CT%cooran
!!$
!!$ END SUBROUTINE updt_explicit
!!$!!!--------------------------------------------------------------------------------
!!$  SUBROUTINE ini_explicit(CT)
!!$    !
!!$    ! I
!!$    ! CT
!!$    !
!!$    ! O 
!!$    ! id2,id1: le candidat et l'antagoniste
!!$    
!!$    IMPLICIT NONE
!!$    INTEGER                          :: id1,id2,i
!!$    REAL(kind=8),DIMENSION(3,4)      :: PT,coorcd,cooran
!!$    TYPE(T_POLYR)                    :: PR1,PR2
!!$    TYPE(T_visavis)                  :: CT
!!$
!!$    id1 = CT%an
!!$    id2 = CT%cd
!!$    
!!$    PR1 = S_POLYR(id1)
!!$    PR2 = S_POLYR(id2)
!!$    
!!$    !fd construction de la position des points candidats par interpolation des noeuds 
!!$    !fd porteurs des surfaces cd et an 
!!$    
!!$    !fd position noeuds porteurs candidats 
!!$    
!!$    DO i=1,4
!!$       PT(:,i) = interpolate_H8(CT%cd_lcoor(1,i),CT%cd_lcoor(2,i),CT%cd_lcoor(3,i),PR2%VERTEX_ref)
!!$    ENDDO
!!$    
!!$    !fd interpolation sur la face candidate
!!$    
!!$    CT%coorcd(:,1)=fformQ4(1,1)*PT(:,1)+ &
!!$         fformQ4(2,1)*PT(:,2)+ &
!!$         fformQ4(3,1)*PT(:,3)+ &
!!$         fformQ4(4,1)*PT(:,4)
!!$    
!!$    CT%coorcd(:,2)=fformQ4(1,2)*PT(:,1)+ &
!!$         fformQ4(2,2)*PT(:,2)+ &
!!$         fformQ4(3,2)*PT(:,3)+ &
!!$         fformQ4(4,2)*PT(:,4)
!!$    
!!$    CT%coorcd(:,3)=fformQ4(1,3)*PT(:,1)+ &
!!$         fformQ4(2,3)*PT(:,2)+ &
!!$         fformQ4(3,3)*PT(:,3)+ &
!!$         fformQ4(4,3)*PT(:,4)
!!$    
!!$    CT%coorcd(:,4)=fformQ4(1,4)*PT(:,1)+ &
!!$         fformQ4(2,4)*PT(:,2)+ &
!!$         fformQ4(3,4)*PT(:,3)+ &
!!$         fformQ4(4,4)*PT(:,4)
!!$    
!!$    
!!$    !fd position noeuds porteurs antagonistes
!!$    
!!$    DO i=1,4
!!$       PT(:,i) = interpolate_H8(CT%an_lcoor(1,i),CT%an_lcoor(2,i),CT%an_lcoor(3,i),PR1%VERTEX_ref)
!!$    ENDDO
!!$    
!!$    CT%Cooran(:,1) =fformQ4(1,1)*PT(:,1)+ &
!!$         fformQ4(2,1)*PT(:,2)+ &
!!$         fformQ4(3,1)*PT(:,3)+ &
!!$         fformQ4(4,1)*PT(:,4)
!!$    
!!$    CT%Cooran(:,2) =fformQ4(1,2)*PT(:,1)+ &
!!$         fformQ4(2,2)*PT(:,2)+ &
!!$         fformQ4(3,2)*PT(:,3)+ &
!!$         fformQ4(4,2)*PT(:,4)
!!$    
!!$    CT%Cooran(:,3) =fformQ4(1,3)*PT(:,1)+ &
!!$         fformQ4(2,3)*PT(:,2)+ &
!!$         fformQ4(3,3)*PT(:,3)+ &
!!$         fformQ4(4,3)*PT(:,4)
!!$    
!!$    CT%Cooran(:,4) =fformQ4(1,4)*PT(:,1)+ &
!!$         fformQ4(2,4)*PT(:,2)+ &
!!$         fformQ4(3,4)*PT(:,3)+ &
!!$         fformQ4(4,4)*PT(:,4)
!!$    
!!$    DO i=1,4
!!$       PRINT*,'point contact conf brique',i
!!$       PRINT*,'cd ',ct%coorcd(:,i)
!!$       PRINT*,'an ',ct%cooran(:,i)
!!$    ENDDO
!!$    
!!$  END SUBROUTINE ini_explicit
!!$!!!-------------------------------------------------------------------------  
!!$  FUNCTION interpolate_H8(ksi,eta,zet,vec)
!!$
!!$    IMPLICIT NONE
!!$    REAL(kind=8),DIMENSION(3) :: interpolate_H8
!!$    REAL(kind=8) :: ksi,eta,zet
!!$    REAL(kind=8),DIMENSION(8) :: N
!!$    REAL(kind=8),DIMENSION(3,8) :: vec
!!$    REAL(kind=8) :: UPK,UPE,UPZ,UMK,UMZ,UME
!!$    REAL(kind=8),PARAMETER :: US8=1.d0/8.d0
!!$    INTEGER :: i
!!$    
!!$    !   print*,ksi,eta,zet 
!!$    !   do i=1,8
!!$    !     print*,vec(:,i)
!!$    !   enddo
!!$    
!!$    UPK=1.d0+KSI  ;  UPE=1.d0+ETA  ;  UPZ=1.d0+ZET
!!$    UMK=1.d0-KSI  ;  UME=1.d0-ETA  ;  UMZ=1.d0-ZET
!!$    
!!$    N=RESHAPE((/  US8*UMK *UME *UMZ       , &
!!$         US8*UPK *UME *UMZ       , &
!!$         US8*UPK *UPE *UMZ       , &
!!$         US8*UMK *UPE *UMZ       , &
!!$         US8*UMK *UME *UPZ       , &
!!$         US8*UPK *UME *UPZ       , &
!!$         US8*UPK *UPE *UPZ       , &
!!$         US8*UMK *UPE *UPZ       /),(/SIZE(N)/))
!!$    
!!$    
!!$    interpolate_H8 = 0.d0
!!$    DO i=1,8   
!!$       interpolate_H8(:) = interpolate_H8(:) + (vec(:,i)*N(i))   
!!$    ENDDO
!!$    
!!$  END FUNCTION interpolate_H8

!!$!------------------------------------------------------------------------
!!$ SUBROUTINE compute_contact_PRPRx
!!$ 
!!$   IMPLICIT NONE  
!!$
!!$   INTEGER                     :: errare 
!!$   INTEGER                     :: icdan,iadj,itac
!!$   INTEGER                     :: icdtac,iantac,isee,itacty    
!!$   REAL(kind=8)                :: raycd,rayan,adist,dist,nonuc,gap
!!$   INTEGER                     :: r_icdtac,r_iantac
!!$   INTEGER                     :: i,id,j,nb_ctc
!!$   INTEGER                     :: size_of_array_this
!!$   REAL(kind=8),DIMENSION(3,4) :: xco 
!!$   REAL(kind=8),DIMENSION(4)   :: ovlap
!!$   REAL(kind=8),DIMENSION(3)   :: sep,t,n,s,cdlev,anlev,Point,Vsep
!!$   REAL(kind=8)                :: norme,den,scal
!!$   REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
!!$   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an
!!$   REAL(kind=8)                :: t1,t2,vls_cst,vln_cst,vlt_cst
!!$   REAL(kind=8)                :: pt_surf
!!$
!!$   integer :: cd_ent,an_ent
!!$   character(len=80) :: cout
!!$
!!$   icdan   = 0        
!!$   nb_PRPRx= 0
!!$   nb_adj  = 0
!!$   nb_detection_test=0
!!$   detection_time=0.D0
!!$   nb_shadow=0
!!$   nb_ctc_state=0
!!$
!!$   xco=0.d0
!!$
!!$   IF (nb_rough_PRPRx /= 0 ) THEN
!!$
!!$   size_of_array_this=SIZE(this)
!!$  
!!$   !
!!$   ! preparation de la detection 
!!$   !
!!$     DO i=1,nb_rough_PRPRx
!!$       icdtac    = rough_PRPRx(i)%cd
!!$       iantac    = rough_PRPRx(i)%an 
!!$       isee      = rough_PRPRx(i)%isee
!!$       Vsep(1:3) = rough_PRPRx(i)%Vsep(1:3)
!!$
!!$       adist=see(isee)%alert 
!!$       dist=S_POLYR(icdtac)%radius+S_POLYR(iantac)%radius+adist
!!$
!!$       sep=S_POLYR(icdtac)%center - S_POLYR(iantac)%center
!!$       norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)
!!$
!!$       IF (norme <1.D-24) THEN
!!$          PRINT*,'Error in mod_PRPRx, in compute contact polyr'
!!$          PRINT*,' Distance between',icdtac, ' and ',iantac, 'is'
!!$          PRINT*,norme 
!!$          STOP
!!$       ENDIF   
!!$
!!$
!!$!fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
!!$!fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
!!$!fd @@@ et il en manque je pense ...
!!$
!!$       IF (norme < dist*dist) THEN
!!$
!!$!fd @@@ ca n'est pas suffisant non ?
!!$
!!$         IF ((S_POLYR(iantac)%maxpos(1)-S_POLYR(icdtac)%minpos(1)+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(iantac)%maxpos(2)-S_POLYR(icdtac)%minpos(2)+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(iantac)%maxpos(3)-S_POLYR(icdtac)%minpos(3)+adist)<0.D0) CYCLE
!!$
!!$!fd @@@ je rajoute ....
!!$
!!$         IF ((S_POLYR(icdtac)%maxpos(1)-S_POLYR(iantac)%minpos(1)+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(icdtac)%maxpos(2)-S_POLYR(iantac)%minpos(2)+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(icdtac)%maxpos(3)-S_POLYR(iantac)%minpos(3)+adist)<0.D0) CYCLE
!!$
!!$         sep=sep/SQRT(norme)
!!$
!!$         scal=sep(1)*Vsep(1)+sep(2)*Vsep(2)+sep(3)*Vsep(3)
!!$
!!$         IF (scal==0.D0) THEN
!!$            rough_PRPRx(i)%Vsep=sep
!!$         ENDIF
!!$            
!!$         CALL cpu_time(t1)
!!$         CALL INTERSECTION_POLYR_WORK_new(iantac,icdtac,nb_ctc,xco,n,r_icdtac,r_iantac,ovlap,sep,rough_PRPRx(i)%Vsep,adist,pt_surf)
!!$         CALL cpu_time(t2)
!!$
!!$         nb_tot_detect=nb_tot_detect+1
!!$         nb_detection_test=nb_detection_test+1
!!$         detection_time=detection_time+t2-t1
!!$         
!!$         IF (nb_ctc==0) CYCLE
!!$
!!$!fd @@@ c'est quoi ce merdier pourquoi r_icdtac et iantac ... ?
!!$!fd @@@         sep=  S_POLYR(r_icdtac)%center - S_POLYR(iantac)%center
!!$!fd @@@  en pratique les r_xxx ne servent a rien !!!
!!$
!!$         sep=  S_POLYR(r_icdtac)%center - S_POLYR(r_iantac)%center
!!$
!!$         IF ((n(1)*sep(1)+n(2)*sep(2)+n(3)*sep(3))<0.D0) n=-n
!!$
!!$         norme = SQRT(n(1)*n(1)+n(2)*n(2)+n(3)*n(3))
!!$         IF (norme/=0.D0) THEN
!!$           n = n/norme
!!$         ELSE 
!!$           PRINT*,n(1),n(2),n(3),norme
!!$           CYCLE
!!$         ENDIF
!!$
!!$         !fd cas ou la normale est alignee sur un axe
!!$
!!$         IF(( n(1)*n(2) == 0.D0 ).AND. &
!!$            ( n(2)*n(3) == 0.D0 ).AND. &
!!$            ( n(3)*n(1) == 0.D0 )) THEN
!!$                         
!!$!fd @@@ et allez ... le repere c'est t,n,s !!!
!!$!fd @@@           t(1)= n(3)
!!$!fd @@@           t(2)= n(1)
!!$!fd @@@           t(3)= n(2)
!!$
!!$            IF(n(1) /= 0.d0 .OR. n(2) /= 0.d0) THEN  
!!$              s = (/ 0., 0., 1. /)
!!$              t(1) =  n(2)*s(3) - n(3)*s(2)
!!$              t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$              t(3) =  n(1)*s(2) - n(2)*s(1)
!!$            ELSE
!!$              s = (/ 0., 1., 0. /)
!!$              t(1) =  n(2)*s(3) - n(3)*s(2)
!!$              t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$              t(3) =  n(1)*s(2) - n(2)*s(1)
!!$            ENDIF
!!$
!!$
!!$         ELSE
!!$            IF(dabs(n(1)) > 0.9 .OR. dabs(n(2)) > 0.9) THEN  
!!$              s = (/ 0., 0., 1. /)
!!$              t(1) =  n(2)*s(3) - n(3)*s(2)
!!$              t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$              t(3) =  n(1)*s(2) - n(2)*s(1)
!!$              norme = SQRT(t(1)*t(1)+t(2)*t(2)+t(3)*t(3))
!!$            ELSE IF (dabs(n(3)) > 0.9) THEN
!!$              s = (/ 0., 1., 0. /)
!!$              t(1) =  n(2)*s(3) - n(3)*s(2)
!!$              t(2) =  n(3)*s(1) - n(1)*s(3)              
!!$              t(3) =  n(1)*s(2) - n(2)*s(1)
!!$            ELSE
!!$              den=1.D0/SQRT(  (n(2)*n(3))**2 &
!!$                        +  (n(1)*n(3))**2 &
!!$                        + 4.*(n(1)*n(2))**2)
!!$                         
!!$              t(1)=    n(2)*n(3)*den
!!$              t(2)=    n(3)*n(1)*den
!!$              t(3)=  -2*n(1)*n(2)*den
!!$            ENDIF
!!$         ENDIF
!!$
!!$!fd @@@ et allez c'est a l'envers
!!$!fd @@@        s(1)     =  t(3)*n(2) - t(2)*n(3)
!!$!fd @@@        s(2)     =  t(1)*n(3) - t(3)*n(1)              
!!$!fd @@@        s(3)     =  t(2)*n(1) - t(1)*n(2)
!!$
!!$!fd @@@ je corrige ...
!!$
!!$           norme = SQRT(t(1)*t(1)+t(2)*t(2)+t(3)*t(3))
!!$           t = t/norme
!!$
!!$           s(1)     =  t(2)*n(3) - t(3)*n(2)
!!$           s(2)     =  t(3)*n(1) - t(1)*n(3)              
!!$           s(3)     =  t(1)*n(2) - t(2)*n(1)
!!$
!!$           norme = SQRT(s(1)*s(1)+s(2)*s(2)+s(3)*s(3))
!!$           s=s/norme
!!$
!!$           icdtac                  = r_icdtac
!!$           iantac                  = r_iantac
!!$
!!$
!!$           localframe_cd = get_inertia_frameTT_POLYR(icdtac)
!!$           localframe_an = get_inertia_frameTT_POLYR(iantac)
!!$
!!$           cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
!!$           an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)
!!$           
!!$           vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ (cd_Vbegin(2)-an_Vbegin(2))*t(2)+(cd_Vbegin(3)-an_Vbegin(3))*t(3)
!!$           vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ (cd_Vbegin(2)-an_Vbegin(2))*n(2)+(cd_Vbegin(3)-an_Vbegin(3))*n(3)
!!$           vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ (cd_Vbegin(2)-an_Vbegin(2))*s(2)+(cd_Vbegin(3)-an_Vbegin(3))*s(3)
!!$
!!$           DO j=1,nb_ctc
!!$
!!$             icdan                   = icdan+1	  	       	       
!!$             IF (icdan>size_of_array_this) THEN
!!$                PRINT*,'---------------------------------------------'
!!$                PRINT*,'ERROR filling this                           '
!!$                PRINT*,'you rich the allocated size                  '
!!$                PRINT*,'In COMMAND.DAT you should add the keywords : '
!!$                PRINT*,'LOW SIZE ARRAY POLYR                         ' 
!!$                PRINT*,'sizefactor                                   '
!!$                PRINT*,'where sizefactor is an integer specifiyng the'
!!$                PRINT*,'ratio of memory you need (=4 by default)     '
!!$                PRINT*,'Program stop                                 '
!!$                PRINT*,'---------------------------------------------'
!!$                STOP
!!$             ENDIF   
!!$             nb_adj(icdtac)          = nb_adj(icdtac)+1
!!$             iadj                    = nb_adj(icdtac)
!!$             this(icdan)%iadj        = iadj  
!!$             this(icdan)%icdbdy      = polyr2bdyty(1,icdtac)
!!$             this(icdan)%icdtac      = icdtac
!!$             this(icdan)%ianbdy      = polyr2bdyty(1,iantac)
!!$             this(icdan)%iantac      = iantac
!!$             this(icdan)%isee        = isee    	             
!!$             this(icdan)%tuc         = t
!!$             this(icdan)%nuc         = n
!!$             this(icdan)%suc         = s
!!$
!!$             this(icdan)%coor    = xco(1:3,j)
!!$             this(icdan)%type_ctc    = nb_ctc
!!$
!!$             cd_ent = get_ent_POLYR(this(icdan)%icdtac)
!!$             an_ent = get_ent_POLYR(this(icdan)%iantac) 
!!$         
!!$             entity(cd_ent)%nb = entity(cd_ent)%nb+1
!!$             entity(an_ent)%nb = entity(an_ent)%nb+1
!!$
!!$             cdlev = xco(1:3,j)-PRcoor(1:3,icdtac) 
!!$             anlev = xco(1:3,j)-PRcoor(1:3,iantac)
!!$
!!$!fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
!!$!fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel
!!$
!!$             this(icdan)%coorcd(1)=cdlev(1)*localframe_cd(1,1)+cdlev(2)*localframe_cd(2,1)+cdlev(3)*localframe_cd(3,1)
!!$             this(icdan)%coorcd(2)=cdlev(1)*localframe_cd(1,2)+cdlev(2)*localframe_cd(2,2)+cdlev(3)*localframe_cd(3,2)
!!$             this(icdan)%coorcd(3)=cdlev(1)*localframe_cd(1,3)+cdlev(2)*localframe_cd(2,3)+cdlev(3)*localframe_cd(3,3)
!!$
!!$             this(icdan)%cooran(1)=anlev(1)*localframe_an(1,1)+anlev(2)*localframe_an(2,1)+anlev(3)*localframe_an(3,1)
!!$             this(icdan)%cooran(2)=anlev(1)*localframe_an(1,2)+anlev(2)*localframe_an(2,2)+anlev(3)*localframe_an(3,2)
!!$             this(icdan)%cooran(3)=anlev(1)*localframe_an(1,3)+anlev(2)*localframe_an(2,3)+anlev(3)*localframe_an(3,3)
!!$
!!$!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!!$!             print*,icdan
!!$!             print*, xco(1:3,j)
!!$!             print*,this(icdan)%coorcd(1:3)
!!$!             print*,this(icdan)%cooran(1:3)
!!$!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!!$
!!$
!!$             ! On va calculer le passage rep inertie -> rep général pour l'antagoniste
!!$
!!$             Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
!!$             Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
!!$             Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)
!!$
!!$             Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
!!$             Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
!!$             Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)
!!$
!!$             Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
!!$             Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
!!$             Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)
!!$
!!$             this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 
!!$
!!$             this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 
!!$
!!$             this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
!!$
!!$
!!$             ! On va calculer le passage rep inertie -> rep général pour le candidat
!!$
!!$             Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
!!$             Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
!!$             Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)
!!$
!!$             Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
!!$             Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
!!$             Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)
!!$
!!$             Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
!!$             Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
!!$             Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)
!!$
!!$
!!$             this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 
!!$
!!$             this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 
!!$
!!$             this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
!!$           
!!$             ! Calcul des vitesses relatives
!!$
!!$
!!$             this(icdan)%gapTTbegin = ovlap(j)
!!$
!!$             this(icdan)%vltBEGIN = vlt_cst &
!!$                  + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
!!$                  - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)
!!$
!!$             this(icdan)%vlnBEGIN = vln_cst &     
!!$                  + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
!!$                  - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)
!!$
!!$             this(icdan)%vlsBEGIN= vls_cst &     
!!$                  + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
!!$                  - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)
!!$
!!$
!!$             this(icdan)%rls      = 0.D0
!!$             this(icdan)%rlt      = 0.D0
!!$             this(icdan)%rln      = 0.D0
!!$             this(icdan)%vls      = this(icdan)%vlsBEGIN
!!$             this(icdan)%vlt      = this(icdan)%vltBEGIN
!!$             this(icdan)%vln      = this(icdan)%vlnBEGIN
!!$             this(icdan)%gapTT    = this(icdan)%gapTTbegin
!!$             this(icdan)%status   = 'nknow'
!!$
!!$             this(icdan)%surf     = pt_surf
!!$
!!$             this(icdan)%node_rank = 0
!!$
!!$           ENDDO
!!$         ENDIF  
!!$     ENDDO
!!$     nb_PRPRx=icdan
!!$   ENDIF
!!$
!!$   WRITE(cout,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
!!$   call logmes(cout)
!!$   write(cout,*) 'Total time of detection: ',detection_time
!!$   call logmes(cout)
!!$   write(cout,*) 'Nb detection tests :',REAL(nb_detection_test,8)     
!!$   call logmes(cout)
!!$   
!!$   CALL get_behaviour
!!$
!!$   DO itac=1,nb_POLYR
!!$     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
!!$     IF (nb_adj(itac) /= 0) THEN
!!$       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
!!$       IF (errare /=0 ) THEN
!!$         PRINT*,'error in allocating adjac(icdtac)%....., in select_prox_tactors in mod_PRPRx'
!!$         STOP
!!$       END IF
!!$     ENDIF
!!$   ENDDO 
!!$ 
!!$   DO icdan=1,nb_PRPRx
!!$     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
!!$   END DO 
!!$
!!$   IF (ALLOCATED(violation)) DEALLOCATE(violation)
!!$   ALLOCATE(violation(nb_PRPRx),stat=errare)
!!$
!!$ END SUBROUTINE compute_contact_PRPRx

!!$ SUBROUTINE ANALYSE_ROUGHLY_POLYR(id1,id2,adist,test)
!!$  IMPLICIT NONE
!!$   TYPE(T_POLYR)                    :: PR1,PR2
!!$   INTEGER                          :: id1,id2,i,test
!!$   REAL(kind=8)                     :: scal,dist1,dist2,adist,norme
!!$   REAL(kind=8),DIMENSION(3)        :: sep
!!$
!!$   PR1    = S_POLYR(id1)
!!$   PR2    = S_POLYR(id2)
!!$   test=0
!!$   sep=S_POLYR(id2)%center - S_POLYR(id1)%center
!!$   norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)
!!$   sep=sep/SQRT(norme)
!!$
!!$   dist1 = -10D20
!!$   dist2 =  10D20
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 1 vers 2
!!$   !
!!$   DO i=1,PR1%nb_vertex
!!$     scal=PR1%vertex(1,i)*sep(1)+PR1%vertex(2,i)*sep(2)+PR1%vertex(3,i)*sep(3)
!!$     IF (scal > dist1) THEN
!!$       dist1=scal
!!$     ENDIF
!!$   ENDDO
!!$   !
!!$   ! sommet le plus proche de 2 en projection sens 1 vers 2
!!$   !
!!$   DO i=1,PR2%nb_vertex
!!$     scal=PR2%vertex(1,i)*sep(1)+PR2%vertex(2,i)*sep(2)+PR2%vertex(3,i)*sep(3)
!!$     IF (scal < dist2) THEN
!!$       dist2=scal
!!$     ENDIF
!!$   ENDDO
!!$   
!!$   IF ((dist1-dist2+adist) > 0.D0) test=1
!!$   
!!$ END SUBROUTINE ANALYSE_ROUGHLY_POLYR
!!$!------------------------------------------------------------------------
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE INTERSECTION_POLYR_WORK(id1,id2,nb_ctc,PT_CTC,Nr,r_cd,r_an,overlap,sep,PC,Nsep,adist)
!!$
!!$   IMPLICIT NONE
!!$
!!$
!!$   INTEGER,PARAMETER                :: nb_ptc_max=100
!!$   INTEGER                          :: id1,id2,i,k,j
!!$   INTEGER                          :: status,nb_vert,nb_face1,nb_face2,nb_ar1,nb_ar2
!!$   REAL(kind=8)                     :: dist1,dist2,norm,norm2,som_radius,scal,proj1,proj2,proj3,gap_min
!!$   REAL(kind=8),DIMENSION(3)        :: sep,n,s1,s2,s3,ar1,ar2,PT,n2,S,PC,Nr,Nsep
!!$   REAL(kind=8)                     :: d1,l,div,gap,gap0,factor1,factor2,tmp_data
!!$   INTEGER                          :: nb_ptc1,nb_ptc2,nb_ctc,r_cd,r_an
!!$   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC
!!$   REAL(kind=8),DIMENSION(3,nb_ptc_max)    :: PT1,PT2
!!$   INTEGER,DIMENSION(2*nb_ptc_max)         :: is_ok,is_kept
!!$   INTEGER                          :: max_kept
!!$   REAL(kind=8),DIMENSION(4)        :: overlap
!!$   TYPE(T_POLYR)                    :: PR1,PR2
!!$   INTEGER                          :: nb_face_cd=0,nb_face_an=0,nb_select,crita,critb,test,index
!!$   REAL(kind=8),DIMENSION(3)        :: Ncd,Nan,S_an1,S_an2,S_an3,v1,v2,v3,pv1,pv2,pv3,N_tests
!!$   REAL(kind=8)                     :: data_cd1,data_cd2,data_cd3,data_an1,data_an2,data_an3,scal0,scal1
!!$   REAL(kind=8)                     :: test1,test2,test3     
!!$   REAL(kind=8)                     :: xmin,xmax,ymin,ymax,adist
!!$   
!!$
!!$   INTEGER                          :: nb_stored,nb_kept
!!$
!!$   PR1    = S_POLYR(id1)
!!$   PR2    = S_POLYR(id2)
!!$
!!$   r_cd=id2;r_an=id1 
!!$
!!$   nb_ctc=0
!!$
!!$   Nr=sep
!!$
!!$   dist1 = -10D20
!!$   dist2 =  10D20
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 1 vers 2
!!$   !
!!$
!!$   DO i=1,PR1%nb_vertex
!!$     scal=PR1%vertex(1,i)*Nsep(1)+PR1%vertex(2,i)*Nsep(2)+PR1%vertex(3,i)*Nsep(3)
!!$     IF (scal > dist1) THEN
!!$       dist1=scal
!!$     ENDIF
!!$   ENDDO
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 2 vers 1
!!$   !
!!$
!!$   DO i=1,PR2%nb_vertex
!!$     scal=PR2%vertex(1,i)*Nsep(1)+PR2%vertex(2,i)*Nsep(2)+PR2%vertex(3,i)*Nsep(3)
!!$     IF (scal < dist2) THEN
!!$       dist2=scal
!!$     ENDIF
!!$   ENDDO 
!!$
!!$!fd @@@ et la distance d'alerte c'est pour les chiens ?
!!$
!!$   IF ((dist1 - dist2) < 0.D0) RETURN
!!$
!!$   gap=dist1-dist2
!!$   sep=Nsep
!!$   nb_shadow  = nb_shadow + 1
!!$
!!$   Nr(1:3)    = sep(1:3)
!!$
!!$   scal       = 0.D0
!!$   nb_face_cd = 0
!!$   nb_face_an = 0
!!$   scal0      = -1.D20
!!$
!!$   DO i=1,PR1%nb_faces
!!$      scal1=PR1%normal(1,i)*sep(1)+PR1%normal(2,i)*sep(2)+PR1%normal(3,i)*sep(3)
!!$
!!$      nb_face_cd=nb_face_cd+1
!!$      face_cd(nb_face_cd)=i
!!$
!!$       IF (scal1 < 0.D0) CYCLE
!!$
!!$       Ncd(1:3)=PR1%normal(1:3,i)
!!$       dist1 =  PR1%val_support(i)
!!$       dist2 =  1.D20
!!$       DO j=1,PR2%nb_vertex  
!!$         scal=PR2%vertex(1,j)*Ncd(1)+PR2%vertex(2,j)*Ncd(2)+PR2%vertex(3,j)*Ncd(3)
!!$         IF (scal < dist2) THEN
!!$           dist2=scal
!!$         ENDIF
!!$       ENDDO
!!$ 
!!$       IF ((dist1 - dist2) < 0.D0) THEN
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$
!!$         Nsep=Ncd
!!$         RETURN
!!$       ENDIF   
!!$
!!$       IF((dist1 - dist2)<gap) THEN
!!$         gap=dist1-dist2
!!$         Nr(1:3)=Ncd(1:3)
!!$         scal0=scal1
!!$       ENDIF
!!$       nb_shadow = nb_shadow + 1
!!$   ENDDO
!!$
!!$   scal0=-scal0
!!$   DO i=1,PR2%nb_faces
!!$     scal1=PR2%normal(1,i)*sep(1)+PR2%normal(2,i)*sep(2)+PR2%normal(3,i)*sep(3)     
!!$
!!$     nb_face_an=nb_face_an+1
!!$     face_an(nb_face_an)=i
!!$
!!$     IF (scal1 > 0.D0) CYCLE
!!$     Ncd=PR2%normal(1:3,i)
!!$      
!!$     dist1 =  10.D20
!!$     dist2 =  PR2%val_support(i)
!!$
!!$     DO j=1,PR1%nb_vertex
!!$       scal=PR1%vertex(1,j)*Ncd(1)+PR1%vertex(2,j)*Ncd(2)+PR1%vertex(3,j)*Ncd(3)
!!$       IF (scal < dist1) THEN
!!$         dist1=scal
!!$       ENDIF
!!$     ENDDO
!!$     IF ((dist2 - dist1) < 0.D0) THEN
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$
!!$       Nsep=-Ncd
!!$       RETURN
!!$     ENDIF   
!!$     IF((dist2 - dist1)<gap) THEN
!!$       gap=dist2-dist1
!!$       Nr(1:3)=Ncd(1:3)
!!$     ENDIF   
!!$     nb_shadow = nb_shadow + 1
!!$   ENDDO
!!$
!!$   nb_ptc1= 0
!!$   nb_ptc2= 0
!!$
!!$   DO i=1,nb_face_cd
!!$
!!$      Ncd=PR1%normal(1:3,face_cd(i))
!!$
!!$      s1(1:3)=PR1%vertex(1:3,PR1%face(1,face_cd(i)))
!!$      s2(1:3)=PR1%vertex(1:3,PR1%face(2,face_cd(i)))
!!$      s3(1:3)=PR1%vertex(1:3,PR1%face(3,face_cd(i)))
!!$
!!$!fd @@@ ca ne serait pas 3 fois la meme chose ?
!!$
!!$      data_cd1=s1(1)*Ncd(1)+s1(2)*Ncd(2)+s1(3)*Ncd(3)
!!$      data_cd2=s2(1)*Ncd(1)+s2(2)*Ncd(2)+s2(3)*Ncd(3)
!!$      data_cd3=s3(1)*Ncd(1)+s3(2)*Ncd(2)+s3(3)*Ncd(3)
!!$
!!$      v1=s1-s2
!!$      v2=s1-s3                    
!!$
!!$      N_tests(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$      N_tests(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$      N_tests(3)=v1(1)*v2(2)-v1(2)*v2(1) 
!!$
!!$      DO j=1,nb_face_an
!!$
!!$         S_an1=PR2%vertex(1:3,PR2%face(1,face_an(j)))
!!$         S_an2=PR2%vertex(1:3,PR2%face(2,face_an(j)))
!!$         S_an3=PR2%vertex(1:3,PR2%face(3,face_an(j)))
!!$
!!$         data_an1=S_an1(1)*Ncd(1)+S_an1(2)*Ncd(2)+S_an1(3)*Ncd(3)
!!$         data_an2=S_an2(1)*Ncd(1)+S_an2(2)*Ncd(2)+S_an2(3)*Ncd(3)
!!$         data_an3=S_an3(1)*Ncd(1)+S_an3(2)*Ncd(2)+S_an3(3)*Ncd(3)
!!$
!!$!fd @@@ mettre un seul data_cd ...
!!$
!!$         proj1=data_cd1-data_an1
!!$         proj2=data_cd2-data_an2
!!$         proj3=data_cd3-data_an3
!!$
!!$!fd @@@ le cote 1-2 du an traverse le plan du triangle cd
!!$
!!$         IF (proj1*proj2 < 0.D0) THEN
!!$
!!$             div= data_an1-data_an2
!!$
!!$!fd @@@ ca veut dire que si le cote est parallele a la face cd on ne le traite pas
!!$!fd @@@ ce test est inutile si on a deja ejecte les normales alignees
!!$
!!$             IF (dabs(div)>1.D-12) THEN        
!!$
!!$               l=proj2/div
!!$
!!$!fd @@@ comment ca pourrait etre autre chose ?
!!$ 
!!$               IF (l >= 0.D0 .AND. l <= 1.D0) THEN
!!$
!!$                    S=S_an1*l+(1-l)*S_an2
!!$
!!$                    v1=s1-S
!!$                    v2=s2-S
!!$                    v3=s3-S   
!!$
!!$                    pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$                    pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$                    pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)      
!!$
!!$                    test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$                    pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$                    pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$                    pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$                    test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$                    pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$                    pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$                    pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$
!!$                    test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$                    IF ((test1>=0.D0.AND.test2>=0.D0.AND.test3>=0.D0) .OR. &
!!$                        (test1<=0.D0.AND.test2<=0.D0.AND.test3<=0.D0)) THEN
!!$
!!$                      nb_ptc1=nb_ptc1+1
!!$                      IF (nb_ptc1 > nb_ptc_max) THEN
!!$                        PRINT*,'On a atteint le nombre de point detectable !!'
!!$                        STOP
!!$                      ENDIF
!!$                      PT1(1:3,nb_ptc1)=S(1:3)
!!$                    ENDIF
!!$               ENDIF
!!$             ENDIF 
!!$         ENDIF
!!$
!!$!fd @@@ le cote 1-3 du an traverse le plan du triangle cd
!!$
!!$         IF (proj1*proj3<0.D0) THEN
!!$
!!$             div=data_an1-data_an3
!!$
!!$             IF (dabs(div)>1.D-12) 
!!$               l=proj3/div
!!$               IF (l>=0.D0.AND.l<=1.D0) THEN
!!$                 S=S_an1*l+(1-l)*S_an3
!!$
!!$                 v1=s1-S
!!$                 v2=s2-S
!!$                 v3=s3-S   
!!$
!!$                 pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$                 pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$                 pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)
!!$      
!!$                 test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$                 pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$                 pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$                 pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$                 test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$                 pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$                 pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$                 pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                 
!!$                 test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$                 IF ((test1>=0.D0.AND.test2>=0.D0.AND.test3>=0.D0) .OR. &
!!$                     (test1<=0.D0.AND.test2<=0.D0.AND.test3<=0.D0)) THEN
!!$
!!$                   nb_ptc1=nb_ptc1+1
!!$                   IF (nb_ptc1 > nb_ptc_max) THEN
!!$                     PRINT*,'On a atteint le nombre de point detectable !!'
!!$                     STOP
!!$                   ENDIF
!!$                   PT1(1:3,nb_ptc1)=S(1:3)
!!$                 ENDIF
!!$               ENDIF  
!!$             ENDIF  
!!$         ENDIF
!!$
!!$!fd @@@ le cote 2-3 du an traverse le plan du triangle cd
!!$
!!$         IF (proj2*proj3<0.D0) THEN
!!$
!!$             div=data_an3-data_an2
!!$
!!$             IF (dabs(div)>1.D-12) THEN       
!!$               l=proj2/div
!!$               IF (l>=0.D0.AND.l<=1.D0) THEN
!!$                    S=S_an3*l+(1-l)*S_an2
!!$
!!$                    v1=s1-S
!!$                    v2=s2-S
!!$                    v3=s3-S   
!!$
!!$                    pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$                    pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$                    pv1(3)=v1(1)*v2(2)-v1(2)*v2(1) 
!!$     
!!$                    test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$                    pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$                    pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$                    pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$                    test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$                    pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$                    pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$                    pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$
!!$                    test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$                    IF ((test1>=0.D0.AND.test2>=0.D0.AND.test3>=0.D0) .OR. &
!!$                        (test1<=0.D0.AND.test2<=0.D0.AND.test3<=0.D0)) THEN
!!$                      nb_ptc1=nb_ptc1+1
!!$                      IF (nb_ptc1 > nb_ptc_max) THEN
!!$                         PRINT*,'On a atteint le nombre de point detectable !!'
!!$                         STOP
!!$                      ENDIF
!!$
!!$                      PT1(1:3,nb_ptc1)=S(1:3)
!!$
!!$                    ENDIF
!!$               ENDIF
!!$             ENDIF  
!!$         ENDIF
!!$     ENDDO
!!$     IF (nb_ptc1 > nb_ptc_max) EXIT
!!$   ENDDO
!!$
!!$
!!$   DO i=1,nb_face_an
!!$
!!$      Ncd= PR2%normal(1:3,face_an(i))
!!$
!!$      s1 = PR2%vertex(1:3,PR2%face(1,face_an(i)))
!!$      s2 = PR2%vertex(1:3,PR2%face(2,face_an(i)))
!!$      s3 = PR2%vertex(1:3,PR2%face(3,face_an(i)))
!!$
!!$      data_cd1=s1(1)*Ncd(1)+s1(2)*Ncd(2)+s1(3)*Ncd(3)
!!$      data_cd2=s2(1)*Ncd(1)+s2(2)*Ncd(2)+s2(3)*Ncd(3)
!!$      data_cd3=s3(1)*Ncd(1)+s3(2)*Ncd(2)+s3(3)*Ncd(3)
!!$
!!$      v1=s1-s2
!!$      v2=s1-s3                    
!!$
!!$      N_tests(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$      N_tests(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$      N_tests(3)=v1(1)*v2(2)-v1(2)*v2(1)   
!!$
!!$      DO j=1,nb_face_cd         
!!$
!!$         S_an1=PR1%vertex(1:3,PR1%face(1,face_cd(j)))
!!$         S_an2=PR1%vertex(1:3,PR1%face(2,face_cd(j)))
!!$         S_an3=PR1%vertex(1:3,PR1%face(3,face_cd(j)))
!!$
!!$         data_an1=S_an1(1)*Ncd(1)+S_an1(2)*Ncd(2)+S_an1(3)*Ncd(3)
!!$         data_an2=S_an2(1)*Ncd(1)+S_an2(2)*Ncd(2)+S_an2(3)*Ncd(3)
!!$         data_an3=S_an3(1)*Ncd(1)+S_an3(2)*Ncd(2)+S_an3(3)*Ncd(3)
!!$
!!$         proj1=data_cd1-data_an1
!!$         proj2=data_cd2-data_an2
!!$         proj3=data_cd3-data_an3
!!$
!!$         IF (proj1*proj2<0.D0) THEN
!!$
!!$             div=data_an1-data_an2
!!$
!!$             IF (dabs(div)>1.D-12) THEN        
!!$               l=proj2/div
!!$               IF (l>=0.D0.AND.l<=1.D0) THEN
!!$                    S=S_an1*l+(1-l)*S_an2
!!$
!!$                    v1=s1-S
!!$                    v2=s2-S
!!$                    v3=s3-S   
!!$
!!$                    pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$                    pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$                    pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)      
!!$                    test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$                    pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$                    pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$                    pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$                    test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$                    pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$                    pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$                    pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                    test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$                    IF ((test1>=0.D0.AND.test2>=0.D0.AND.test3>=0.D0) .OR. &
!!$                        (test1<=0.D0.AND.test2<=0.D0.AND.test3<=0.D0)) THEN
!!$                      nb_ptc2=nb_ptc2+1
!!$                      IF (nb_ptc2 > nb_ptc_max) THEN
!!$                         PRINT*,'On a atteint le nombre de point detectable !!'
!!$                         STOP
!!$                      ENDIF
!!$
!!$                       PT2(1:3,nb_ptc2)=S(1:3)
!!$
!!$                    ENDIF
!!$              ENDIF
!!$             ENDIF 
!!$         ENDIF
!!$         IF (proj1*proj3<0.D0) THEN
!!$
!!$             div=data_an1-data_an3
!!$             IF (dabs(div)>1.D-12) THEN       
!!$              l=proj3/div
!!$              IF (l>=0.D0.AND.l<=1.D0) THEN
!!$                    S=S_an1*l+(1-l)*S_an3
!!$
!!$                    v1=s1-S
!!$                    v2=s2-S
!!$                    v3=s3-S   
!!$
!!$                    pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$                    pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$                    pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)      
!!$                    test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$                    pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$                    pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$                    pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$                    test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$                    pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$                    pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$                    pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                    test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$                    IF ((test1>=0.D0.AND.test2>=0.D0.AND.test3>=0.D0) .OR. &
!!$                        (test1<=0.D0.AND.test2<=0.D0.AND.test3<=0.D0)) THEN
!!$                       nb_ptc2=nb_ptc2+1
!!$                       IF (nb_ptc2 > nb_ptc_max) THEN
!!$                          PRINT*,'On a atteint le nombre de point detectable !!'
!!$                          STOP
!!$                       ENDIF
!!$
!!$                       PT2(1:3,nb_ptc2)=S(1:3)
!!$
!!$                    ENDIF
!!$               ENDIF
!!$             ENDIF  
!!$         ENDIF
!!$         IF (proj2*proj3<0.D0) THEN
!!$
!!$             div=data_an3-data_an2
!!$             IF (ABS(div)>1.D-12) THEN       
!!$               l=proj2/div
!!$               IF (l>=0.D0.AND.l<=1.D0) THEN
!!$                    S=S_an3*l+(1-l)*S_an2
!!$
!!$                    v1=s1-S
!!$                    v2=s2-S
!!$                    v3=s3-S   
!!$
!!$                    pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$                    pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$                    pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)      
!!$                    test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$                    pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$                    pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$                    pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$                    test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$                    pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$                    pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$                    pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                    test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$                    IF ((test1>=0.D0.AND.test2>=0.D0.AND.test3>=0.D0) .OR. &
!!$                        (test1<=0.D0.AND.test2<=0.D0.AND.test3<=0.D0)) THEN
!!$                       nb_ptc2=nb_ptc2+1
!!$                       IF (nb_ptc2 > nb_ptc_max) THEN
!!$                          PRINT*,'On a atteint le nombre de point detectable !!'
!!$                          STOP
!!$                       ENDIF
!!$                       PT2(1:3,nb_ptc2)=S(1:3)
!!$                    ENDIF
!!$               ENDIF
!!$             ENDIF  
!!$         ENDIF
!!$       ENDDO
!!$       IF (nb_ptc2 > nb_ptc_max) EXIT
!!$   ENDDO
!!$
!!$!fd @@@ elimination des noeuds detectes plusieurs fois    
!!$  
!!$   is_ok=0
!!$   nb_ctc=0
!!$   norm=MIN(PR1%min_radius_face,PR2%min_radius_face) 
!!$   norm2=norm*norm
!!$   PT=0
!!$
!!$
!!$!fd   print*,'On a trouve'
!!$!fd   DO i=1,nb_ptc1
!!$!fd     print*,'PT1: ',i,PT1(1,i),PT1(2,i),PT1(3,i)
!!$!fd   ENDDO
!!$!fd   DO i=1,nb_ptc2
!!$!fd     print*,'PT2: ',i,PT2(1,i),PT2(2,i),PT2(3,i)
!!$!fd   ENDDO
!!$
!!$!fd   print*,'On en a peut etre trop trouve ...'
!!$
!!$   xmin= 1.d20
!!$   xmax=-1.d20
!!$   ymin= 1.d20
!!$   ymax=-1.d20
!!$
!!$   nb_stored=0
!!$
!!$!fd   print*,'valeur de reference ',norm2
!!$
!!$   DO i=1,nb_ptc1
!!$     IF (is_ok(i) == 1) CYCLE
!!$     nb_stored=nb_stored+1
!!$     DO j=i+1,nb_ptc1   
!!$       IF (is_ok(j) == 1) CYCLE
!!$       scal=(PT1(1,i)-PT1(1,j))*(PT1(1,i)-PT1(1,j))+ &
!!$            (PT1(2,i)-PT1(2,j))*(PT1(2,i)-PT1(2,j))+ &
!!$            (PT1(3,i)-PT1(3,j))*(PT1(3,i)-PT1(3,j))
!!$
!!$!fd       print*,'dist**2 ','pt1',i,'pt1',j,scal
!!$
!!$       IF (scal < 1d-2*norm2) THEN
!!$!fd         print*,'on retire PT1: ',j
!!$         is_ok(j)=1
!!$       ENDIF
!!$     ENDDO
!!$
!!$     DO j=1,nb_ptc2   
!!$       IF (is_ok(nb_ptc1+j) == 1) CYCLE
!!$       scal=(PT1(1,i)-PT2(1,j))*(PT1(1,i)-PT2(1,j))+ &
!!$            (PT1(2,i)-PT2(2,j))*(PT1(2,i)-PT2(2,j))+ &
!!$            (PT1(3,i)-PT2(3,j))*(PT1(3,i)-PT2(3,j))
!!$
!!$!fd       print*,'dist**2 ','pt1',i,'pt2',j,scal
!!$
!!$       IF (scal < 1d-2*norm2) THEN
!!$!fd         print*,'on retire PT2: ',j
!!$         is_ok(nb_ptc1+j)=1
!!$       ENDIF
!!$     ENDDO
!!$     IF ( PT1(1,i) < xmin) xmin = PT1(1,i) 
!!$     IF ( PT1(1,i) > xmax) xmax = PT1(1,i) 
!!$     IF ( PT1(2,i) < ymin) ymin = PT1(2,i) 
!!$     IF ( PT1(2,i) > ymax) ymax = PT1(2,i) 
!!$   ENDDO
!!$
!!$   DO i=1,nb_ptc2
!!$     IF (is_ok(nb_ptc1+i) == 1) CYCLE
!!$     nb_stored=nb_stored+1
!!$     DO j=i+1,nb_ptc2   
!!$       IF (is_ok(nb_ptc1+j) == 1) CYCLE
!!$       scal=(PT2(1,i)-PT2(1,j))*(PT2(1,i)-PT2(1,j))+ &
!!$            (PT2(2,i)-PT2(2,j))*(PT2(2,i)-PT2(2,j))+ &
!!$            (PT2(3,i)-PT2(3,j))*(PT2(3,i)-PT2(3,j))
!!$
!!$!fd       print*,'dist**2 ','pt2',i,'pt2',j,scal
!!$
!!$       IF (scal < 1d-2*norm2) THEN
!!$!fd         print*,'on retire PT2: ',j
!!$         is_ok(nb_ptc1+j)=1
!!$       ENDIF
!!$     ENDDO
!!$     IF ( PT2(1,i) < xmin) xmin = PT2(1,i) 
!!$     IF ( PT2(1,i) > xmax) xmax = PT2(1,i) 
!!$     IF ( PT2(2,i) < ymin) ymin = PT2(2,i) 
!!$     IF ( PT2(2,i) > ymax) ymax = PT2(2,i) 
!!$   ENDDO
!!$
!!$!fd   print*,'On a trouve :',nb_stored
!!$!fd   print*,'On ne cherche plus que parmi'
!!$!fd   DO i=1,nb_ptc1
!!$!fd     if (is_ok(i) /= 0) cycle
!!$!fd     print*,'PT1: ',i,PT1(1,i),PT1(2,i),PT1(3,i)
!!$!fd   ENDDO
!!$!fd   DO i=1,nb_ptc2
!!$!fd     if (is_ok(nb_ptc1+i) /= 0) cycle
!!$!fd     print*,'PT2: ',i,PT2(1,i),PT2(2,i),PT2(3,i)
!!$!fd   ENDDO
!!$
!!$
!!$!fd    print*,'coordonnees extremales'
!!$!fd   print*,xmin,xmax,ymin,ymax
!!$
!!$
!!$   IF (nb_stored > 4) THEN
!!$   
!!$     is_kept=0
!!$     DO i=1,nb_ptc1
!!$       IF (is_ok(i) == 1) CYCLE
!!$       IF (dabs(PT1(1,i) - xmin) < 1d-4*norm) is_kept(i)=is_kept(i)+1  
!!$       IF (dabs(PT1(1,i) - xmax) < 1d-4*norm) is_kept(i)=is_kept(i)+1  
!!$       IF (dabs(PT1(2,i) - ymin) < 1d-4*norm) is_kept(i)=is_kept(i)+1  
!!$       IF (dabs(PT1(2,i) - ymax) < 1d-4*norm) is_kept(i)=is_kept(i)+1  
!!$     ENDDO
!!$
!!$     DO i=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+i) == 1) CYCLE
!!$       IF (dabs(PT2(1,i) - xmin) < 1d-4*norm) is_kept(nb_ptc1+i)=is_kept(nb_ptc1+i)+1  
!!$       IF (dabs(PT2(1,i) - xmax) < 1d-4*norm) is_kept(nb_ptc1+i)=is_kept(nb_ptc1+i)+1  
!!$       IF (dabs(PT2(2,i) - ymin) < 1d-4*norm) is_kept(nb_ptc1+i)=is_kept(nb_ptc1+i)+1  
!!$       IF (dabs(PT2(2,i) - ymax) < 1d-4*norm) is_kept(nb_ptc1+i)=is_kept(nb_ptc1+i)+1  
!!$     ENDDO
!!$
!!$     max_kept=0
!!$     DO i=1,SIZE(is_kept)
!!$       IF (is_kept(i) > max_kept) max_kept=is_kept(i)
!!$     ENDDO
!!$
!!$     nb_kept=0
!!$     DO j=1,max_kept
!!$       DO i=1,SIZE(is_kept)
!!$         IF (is_ok(i) == 1) CYCLE
!!$         IF (is_kept(i) < max_kept - (j-1)) THEN 
!!$           is_ok(i) = 2
!!$           CYCLE
!!$         ENDIF
!!$         is_ok(i) = 0
!!$         IF (nb_kept < 4) nb_kept=nb_kept+1
!!$       ENDDO
!!$       IF (nb_kept == 4) EXIT
!!$     ENDDO
!!$   ENDIF
!!$
!!$!fd   print*,'tri des extremaux, on en a garde: ',nb_kept 
!!$!fd   print*,'On ne cherche plus que parmi'
!!$!fd   DO i=1,nb_ptc1
!!$!fd     if (is_ok(i) /= 0) cycle
!!$!fd     print*,'PT1: ',i,PT1(1,i),PT1(2,i),PT1(3,i)
!!$!fd   ENDDO
!!$!fd   DO i=1,nb_ptc2
!!$!fd     if (is_ok(nb_ptc1+i) /= 0) cycle
!!$!fd     print*,'PT2: ',i,PT2(1,i),PT2(2,i),PT2(3,i)
!!$!fd   ENDDO
!!$
!!$   DO i=1,nb_ptc1
!!$     IF (is_ok(i) /= 0) CYCLE
!!$     nb_ctc=nb_ctc+1
!!$     PT(:)=PT(:)+PT1(:,i)
!!$   ENDDO
!!$
!!$   DO i=1,nb_ptc2
!!$     IF (is_ok(nb_ptc1+i) /= 0) CYCLE
!!$     nb_ctc=nb_ctc+1
!!$     PT(:)=PT(:)+PT2(:,i)
!!$   ENDDO
!!$
!!$   IF (nb_ctc==0) nb_ctc_state=nb_ctc_state+1
!!$
!!$   IF (nb_ctc>0) THEN
!!$
!!$      PT_CTC       = 0.D0
!!$      overlap(1:4) = -ABS(gap)
!!$
!!$      PT=PT/REAL(nb_ctc,8)
!!$      PC=PT
!!$
!!$!fd      print*,'Au final on ne cherche plus que parmi'
!!$!fd      DO i=1,nb_ptc1
!!$!fd        if (is_ok(i) == 1) cycle
!!$!fd        print*,'PT1: ',i,PT1(1,i),PT1(2,i),PT1(3,i)
!!$!fd      ENDDO
!!$!fd      DO i=1,nb_ptc2
!!$!fd        if (is_ok(nb_ptc1+i) == 1) cycle
!!$!fd        print*,'PT2: ',i,PT2(1,i),PT2(2,i),PT2(3,i)
!!$!fd      ENDDO
!!$
!!$!      nb_select=1
!!$!      PT_CTC(1:3,nb_select)=PT(1:3)     
!!$
!!$
!!$      nb_select=0
!!$      DO i=1,nb_ptc1
!!$        IF (nb_select == 4) EXIT
!!$        IF (is_ok(i) /= 0) CYCLE
!!$        scal=(PC(1)-PT1(1,i))*(PC(1)-PT1(1,i))+ &
!!$             (PC(2)-PT1(2,i))*(PC(2)-PT1(2,i))+ &
!!$             (PC(3)-PT1(3,i))*(PC(3)-PT1(3,i))
!!$
!!$        IF(scal<2.D0*norm2) CYCLE
!!$        
!!$        IF (nb_select == 0) THEN
!!$          nb_select=1
!!$          PT_CTC(:,nb_select)=PT1(:,i)
!!$          CYCLE
!!$        ENDIF
!!$
!!$        k=0
!!$        DO j=1,nb_select
!!$          scal=(PT_CTC(1,j)-PT1(1,i))*(PT_CTC(1,j)-PT1(1,i))+ &
!!$               (PT_CTC(2,j)-PT1(2,i))*(PT_CTC(2,j)-PT1(2,i))+ &
!!$               (PT_CTC(3,j)-PT1(3,i))*(PT_CTC(3,j)-PT1(3,i))
!!$          IF(scal>2.D0*norm2) k=k+1
!!$        ENDDO      
!!$
!!$        IF (k==nb_select) THEN
!!$          nb_select=nb_select+1
!!$          PT_CTC(1:3,nb_select)=PT1(1:3,i)
!!$        ENDIF
!!$      ENDDO
!!$
!!$      DO i=1,nb_ptc2
!!$        IF (nb_select == 4) EXIT
!!$        IF (is_ok(nb_ptc1+i) /= 0) CYCLE
!!$
!!$        scal=(PC(1)-PT2(1,i))*(PC(1)-PT2(1,i))+ &
!!$             (PC(2)-PT2(2,i))*(PC(2)-PT2(2,i))+ &
!!$             (PC(3)-PT2(3,i))*(PC(3)-PT2(3,i))
!!$        IF(scal<2.D0*norm2) CYCLE
!!$
!!$        k=0
!!$        DO j=1,nb_select
!!$          scal=(PT_CTC(1,j)-PT2(1,i))*(PT_CTC(1,j)-PT2(1,i))+ &
!!$               (PT_CTC(2,j)-PT2(2,i))*(PT_CTC(2,j)-PT2(2,i))+ &
!!$               (PT_CTC(3,j)-PT2(3,i))*(PT_CTC(3,j)-PT2(3,i))
!!$          IF(scal>2.D0*norm2) k=k+1
!!$        ENDDO      
!!$
!!$        IF (k==nb_select) THEN
!!$          nb_select=nb_select+1
!!$          PT_CTC(1:3,nb_select)=PT2(1:3,i)
!!$        ENDIF
!!$      ENDDO
!!$
!!$!fd      print*,'Pour finir on garde'
!!$!fd      do i=1,nb_select
!!$!fd        print*,PT_CTC(:,i)
!!$!fd      enddo
!!$
!!$      IF (nb_select<3) THEN
!!$         PT_CTC(1:3,1)=PC
!!$         nb_ctc=1
!!$      ELSE IF (nb_select==3) THEN
!!$         nb_ctc=3
!!$      ELSE
!!$         nb_ctc=4
!!$      ENDIF   
!!$   ENDIF
!!$
!!$ END SUBROUTINE INTERSECTION_POLYR_WORK
!!$!------------------------------------------------------------------------
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE INTERSECTION_POLYR_WORK_new(id1,id2,nb_ctc,PT_CTC,Nr,r_cd,r_an,overlap,sep,Nsep,adist,pt_surf)
!!$
!!$
!!$! I
!!$! id1 : id solide 1 (antagoniste)
!!$! id2 : id solide 2 (candidat)
!!$! sep : est l'intercentre
!!$! Nsep: est la derniere normale au plan separateur connue ou l'intercentre
!!$!
!!$! O 
!!$! nb_ctct           : nombre de points de contact
!!$! PT_CTC(1:nb_ctc)  : coordonnees des points de contact
!!$! Nr(1)             : normales aux points de contact  (bizarre bizarrre)
!!$! r_cd,r_an         : qui est le vrai candidat et le vrai antagoniste ... comme en entre
!!$! overlap(1:nb_ctc) : les gaps aux points de contact
!!$! Nsep : normale au plan separateur actualisee ou ancienne
!!$! sep: valeur massacree
!!$! pt_surf: la surface affectee a un point de contact
!!$
!!$   IMPLICIT NONE
!!$
!!$
!!$   INTEGER,PARAMETER                :: nb_ptc_max=100
!!$   INTEGER                          :: id1,id2,i,k,j
!!$   INTEGER                          :: status,nb_vert,nb_face1,nb_face2,nb_ar1,nb_ar2
!!$   REAL(kind=8)                     :: dist1,dist2,norm,norm2,som_radius,scal,proj1,proj2,proj3,gap_min
!!$   REAL(kind=8),DIMENSION(3)        :: sep,n,s1,s2,s3,ar1,ar2,PT,n2,S,PC,Nr,Nsep
!!$   REAL(kind=8)                     :: d1,l,div,gap,gap0,factor1,factor2,tmp_data
!!$   INTEGER                          :: nb_ptc1,nb_ptc2,nb_ctc,r_cd,r_an
!!$
!!$   REAL(kind=8),DIMENSION(3,4)             :: PT_CTC
!!$   REAL(kind=8),DIMENSION(4)               :: overlap
!!$
!!$   REAL(kind=8),DIMENSION(3,nb_ptc_max)    :: PT1,PT2
!!$   INTEGER,DIMENSION(2*nb_ptc_max)         :: is_ok,is_kept
!!$   INTEGER                                 :: max_kept
!!$
!!$   REAL(kind=8),DIMENSION(5,nb_ptc_max)    :: PT1_loc,PT2_loc  !infos liees au contact [gap:normale]
!!$
!!$   TYPE(T_POLYR)                    :: PR1,PR2
!!$   INTEGER                          :: nb_face_cd=0,nb_face_an=0,nb_select,crita,critb,test,index
!!$   REAL(kind=8),DIMENSION(3)        :: Ncd,Nan,S_an1,S_an2,S_an3,v1,v2,v3,pv1,pv2,pv3,N_tests
!!$   REAL(kind=8)                     :: data_cd,data_cd1,data_cd2,data_cd3,data_an1,data_an2,data_an3,scal1
!!$   REAL(kind=8)                     :: test1,test2,test3     
!!$   REAL(kind=8)                     :: scal_min,adist,tmpval,norm_test,norm_max,norm_min
!!$
!!$   
!!$   INTEGER                          :: nb_stored,nb_kept,i1,i2,i3,i4
!!$
!!$   LOGICAL :: bavard=.FALSE. ! .true.
!!$
!!$   REAL(kind=8) :: pt_surf,a,b
!!$
!!$
!!$   LOGICAL :: is_hori
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'======================='
!!$     PRINT*,'Detection entre le POLYR ',id1,' et le POLYR',id2
!!$     PRINT*,'sep',sep
!!$   ENDIF
!!$
!!$   PR1    = S_POLYR(id1)
!!$   PR2    = S_POLYR(id2)
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'vertex de:',id1 
!!$     DO i=1,PR1%nb_vertex
!!$       PRINT*,PR1%vertex(:,i)
!!$     ENDDO
!!$     PRINT*,'vertex de:',id2 
!!$     DO i=1,PR2%nb_vertex
!!$       PRINT*,PR2%vertex(:,i)
!!$     ENDDO
!!$   ENDIF
!!$
!!$
!!$!fd @@@ totalement bidon et dangereux ...
!!$
!!$   r_cd=id2;r_an=id1 
!!$
!!$!fd @@@ phase de shadow overlap:
!!$!fd @@@ s-o initialisation sur la valeur de Nsep= ligne des centres|ancienne normale au plan separateur
!!$
!!$   nb_ctc=0
!!$   Nr=sep
!!$
!!$   dist1 = -10D20
!!$   dist2 =  10D20
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 1 vers 2
!!$   !
!!$
!!$   DO i=1,PR1%nb_vertex
!!$     scal=PR1%vertex(1,i)*Nsep(1)+PR1%vertex(2,i)*Nsep(2)+PR1%vertex(3,i)*Nsep(3)
!!$     IF (scal > dist1) THEN
!!$       dist1=scal
!!$     ENDIF
!!$   ENDDO
!!$
!!$   !
!!$   ! sommet le plus loin de 1 en projection sens 2 vers 1
!!$   !
!!$
!!$   DO i=1,PR2%nb_vertex
!!$     scal=PR2%vertex(1,i)*Nsep(1)+PR2%vertex(2,i)*Nsep(2)+PR2%vertex(3,i)*Nsep(3)
!!$     IF (scal < dist2) THEN
!!$       dist2=scal
!!$     ENDIF
!!$   ENDDO 
!!$
!!$!fd @@@ si le s-o sur Nsep ne passe pas
!!$
!!$   IF ((dist1 - dist2) < -adist) RETURN
!!$
!!$!   print*,'Ca passe le shadow overlap sur l''intercentre'
!!$!   print*,'Critere = ',dist1 - dist2
!!$
!!$   gap=dist1-dist2
!!$   nb_shadow  = nb_shadow + 1
!!$
!!$   sep= Nsep
!!$   Nr = sep
!!$
!!$   scal       = 0.D0
!!$   nb_face_cd = 0
!!$   nb_face_an = 0
!!$
!!$!fd @@@ ... s-o sur les normales ...
!!$
!!$   DO i=1,PR1%nb_faces
!!$
!!$!fd @@@ si la normale a la face n'est pas orientee sur la direction sep=Nsep on jarte
!!$!fd @@@ si on n'a pas mis de step c'est la ligne des centres, sinon c'est la derniere direction separatrice
!!$   
!!$      scal1=PR1%normal(1,i)*sep(1)+PR1%normal(2,i)*sep(2)+PR1%normal(3,i)*sep(3)
!!$
!!$!fd @@@ valeur donnee pour les besoins d'un calcul
!!$
!!$      IF (scal1 < -0.0001D0) CYCLE
!!$
!!$      nb_face_cd=nb_face_cd+1
!!$      face_cd(nb_face_cd)=i
!!$
!!$      Ncd(1:3)=PR1%normal(1:3,i)
!!$      dist1 =  PR1%val_support(i)
!!$      dist2 =  1.D20
!!$      DO j=1,PR2%nb_vertex  
!!$        scal=PR2%vertex(1,j)*Ncd(1)+PR2%vertex(2,j)*Ncd(2)+PR2%vertex(3,j)*Ncd(3)
!!$        IF (scal < dist2) THEN
!!$          dist2=scal
!!$        ENDIF
!!$      ENDDO
!!$ 
!!$!      print*,'Critère = ',dist1 - dist2
!!$
!!$      IF ((dist1 - dist2) < -adist) THEN
!!$
!!$!        print*,'Ca ne passe pas le shadow overlap PR1 face ',i,'avec les vertex du PR2'
!!$!        print*,'dist1= ',dist1,' dist2= ',dist2,' adist = ',adist
!!$
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$!fd @@@ la normale au plan separateur est Ncd
!!$
!!$        Nsep=Ncd
!!$        RETURN
!!$      ENDIF   
!!$
!!$      IF((dist1 - dist2)<gap) THEN
!!$        gap=dist1-dist2
!!$        Nr(1:3)=Ncd(1:3)
!!$      ENDIF
!!$      nb_shadow = nb_shadow + 1
!!$   ENDDO
!!$
!!$   DO i=1,PR2%nb_faces
!!$
!!$     scal1=PR2%normal(1,i)*sep(1)+PR2%normal(2,i)*sep(2)+PR2%normal(3,i)*sep(3)     
!!$
!!$     IF (scal1 > 0.0001D0) CYCLE
!!$
!!$     nb_face_an=nb_face_an+1
!!$     face_an(nb_face_an)=i
!!$
!!$     Ncd=PR2%normal(1:3,i)
!!$      
!!$     dist1 =  10.D20
!!$     dist2 =  PR2%val_support(i)
!!$
!!$     DO j=1,PR1%nb_vertex
!!$       scal=PR1%vertex(1,j)*Ncd(1)+PR1%vertex(2,j)*Ncd(2)+PR1%vertex(3,j)*Ncd(3)
!!$       IF (scal < dist1) THEN
!!$         dist1=scal
!!$       ENDIF
!!$     ENDDO
!!$
!!$!    print*,'Critère = ',dist2 - dist1
!!$
!!$     IF ((dist2 - dist1) < -adist) THEN
!!$
!!$!        print*,'Ca ne passe pas le shadow overlap PR2 face ',i,'avec les vertex du PR1'
!!$!        print*,'dist1= ',dist2,' dist2= ',dist1,' adist = ',adist
!!$
!!$
!!$!fd @@@ on sort de la routine car pas contact 
!!$!fd @@@ on garde moins Ncd car on va tester dans l'autre sens ...
!!$
!!$       Nsep=-Ncd
!!$       RETURN
!!$     ENDIF   
!!$
!!$     IF((dist2 - dist1)<gap) THEN
!!$       gap=dist2-dist1
!!$       Nr(1:3)=Ncd(1:3)
!!$     ENDIF   
!!$     nb_shadow = nb_shadow + 1
!!$   ENDDO
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'Ca passe le shadow overlap'
!!$     PRINT*,'Si contact gap= ',gap,' normale =',Nr
!!$   ENDIF
!!$
!!$!fd @@@ si on en est la c'est qu'on peut supposer qu'il y a contact
!!$
!!$   nb_ptc1= 0
!!$   nb_ptc2= 0
!!$
!!$   DO i=1,nb_face_cd
!!$
!!$      Ncd=PR1%normal(1:3,face_cd(i))
!!$
!!$      s1(1:3)=PR1%vertex(1:3,PR1%face(1,face_cd(i)))
!!$      s2(1:3)=PR1%vertex(1:3,PR1%face(2,face_cd(i)))
!!$      s3(1:3)=PR1%vertex(1:3,PR1%face(3,face_cd(i)))
!!$
!!$!fd @@@ ca ne serait pas 3 fois la meme chose ?
!!$
!!$      data_cd =s1(1)*Ncd(1)+s1(2)*Ncd(2)+s1(3)*Ncd(3)
!!$
!!$      data_cd1=s1(1)*Ncd(1)+s1(2)*Ncd(2)+s1(3)*Ncd(3)
!!$      data_cd2=s2(1)*Ncd(1)+s2(2)*Ncd(2)+s2(3)*Ncd(3)
!!$      data_cd3=s3(1)*Ncd(1)+s3(2)*Ncd(2)+s3(3)*Ncd(3)
!!$
!!$      IF (dabs(data_cd-data_cd1)>1.d-7 .AND. &
!!$          dabs(data_cd-data_cd2)>1.d-7 .AND. &
!!$          dabs(data_cd-data_cd3)>1.d-7) THEN
!!$        PRINT*,'probleme de complaneite des points d''une face pour le cas cd-an'
!!$        STOP
!!$      ENDIF
!!$
!!$      v1=s1-s2
!!$      v2=s1-s3                    
!!$
!!$      N_tests(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$      N_tests(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$      N_tests(3)=v1(1)*v2(2)-v1(2)*v2(1) 
!!$
!!$      norm_test=1d-10*DOT_PRODUCT(N_tests,N_tests)
!!$
!!$!fd@@@ on regarde si un vertex d'en face se "projete" dedans 
!!$
!!$      DO j=1,nb_face_an
!!$
!!$         Nan= PR2%normal(1:3,face_an(j))
!!$
!!$         IF (bavard) THEN
!!$           PRINT*,'<-- PR: ',id2,' Face: ',face_an(j), 'Normale= ',Nan
!!$           PRINT*,'    sur'
!!$           PRINT*,'    PR: ',id1,' Face: ',face_cd(i), 'Normale= ',Ncd,'-->'
!!$         ENDIF
!!$
!!$         tmpval=DOT_PRODUCT(Ncd,Nan)
!!$
!!$         IF (tmpval > -0.01d0) THEN
!!$           IF (bavard) PRINT*,'Les normales sont incompatibles'
!!$           CYCLE
!!$         ENDIF
!!$
!!$         !fd @@@ calcul de vertex internes pour la detection du contact
!!$         !fd @@@ on se des fonctions de forme calculees dans compute box
!!$
!!$         S_an1(:)=fformT3(1,1)*PR2%vertex(:,PR2%face(1,face_an(j)))+ &
!!$                  fformT3(2,1)*PR2%vertex(:,PR2%face(2,face_an(j)))+ &
!!$                  fformT3(3,1)*PR2%vertex(:,PR2%face(3,face_an(j)))
!!$         S_an2(:)=fformT3(1,2)*PR2%vertex(:,PR2%face(1,face_an(j)))+ &
!!$                  fformT3(2,2)*PR2%vertex(:,PR2%face(2,face_an(j)))+ &
!!$                  fformT3(3,2)*PR2%vertex(:,PR2%face(3,face_an(j)))
!!$         S_an3(:)=fformT3(1,3)*PR2%vertex(:,PR2%face(1,face_an(j)))+ &
!!$                  fformT3(2,3)*PR2%vertex(:,PR2%face(2,face_an(j)))+ &
!!$                  fformT3(3,3)*PR2%vertex(:,PR2%face(3,face_an(j)))
!!$
!!$         data_an1=S_an1(1)*Ncd(1)+S_an1(2)*Ncd(2)+S_an1(3)*Ncd(3)
!!$         data_an2=S_an2(1)*Ncd(1)+S_an2(2)*Ncd(2)+S_an2(3)*Ncd(3)
!!$         data_an3=S_an3(1)*Ncd(1)+S_an3(2)*Ncd(2)+S_an3(3)*Ncd(3)
!!$
!!$         proj1=data_cd-data_an1
!!$         proj2=data_cd-data_an2
!!$         proj3=data_cd-data_an3
!!$
!!$!fd @@@ le vertex 1 est suffisamment proche du triangle cd
!!$
!!$         IF (proj1 >= -adist .AND. proj1 < 2.d0*adist) THEN
!!$
!!$!fd @@@ le point ramene sur le plan ...
!!$ 
!!$           S = S_an1 + proj1*Ncd
!!$
!!$           v1=s1-S
!!$           v2=s2-S
!!$           v3=s3-S   
!!$
!!$           pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$           pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$           pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)      
!!$
!!$           test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$           pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$           pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$           pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$           test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$           pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$           pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$           pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$
!!$           test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$           IF ((test1 >= -norm_test .AND. test2 >= -norm_test .AND. test3>= -norm_test) .OR. &
!!$               (test1 <=  norm_test .AND. test2 <=  norm_test .AND. test3<=  norm_test)) THEN
!!$
!!$!fd @@@ ... il est dans le triangle on le garde 
!!$
!!$             nb_ptc1=nb_ptc1+1
!!$             IF (nb_ptc1 > nb_ptc_max) THEN
!!$               PRINT*,'On a atteint le nombre de point detectable !!'
!!$               STOP
!!$             ENDIF
!!$
!!$             IF (bavard) THEN
!!$               PRINT*,'    ','dist',proj1,'alert',adist
!!$               PRINT*,'    ','tests : ',test1,test2,test3
!!$               PRINT*,'    ','vertex ',1,'position: ',S_an1
!!$               PRINT*,'    ','se projete en ',S
!!$               PRINT*,'    ','Ce vertex est dans la face de sommets: '
!!$               PRINT*,'    ','s1: ',s1
!!$               PRINT*,'    ','s2: ',s2
!!$               PRINT*,'    ','s3: ',s3
!!$             ENDIF
!!$!fd @@@ mettre le mi-point
!!$
!!$             PT1(1:3,nb_ptc1)=S(1:3)
!!$             PT1_loc(1,nb_ptc1)  =proj1
!!$             PT1_loc(2:4,nb_ptc1)=Ncd
!!$
!!$!fd @@@ voir le probleme du gap et de la normale au point !!
!!$           ELSE
!!$             IF (bavard) THEN
!!$               PRINT*,'La projection de 1 n''est pas dans la face'
!!$               PRINT*,'tests : ',test1,test2,test3
!!$             ENDIF
!!$           ENDIF
!!$         ELSE
!!$           IF (bavard) PRINT*,proj1,'elles sont trop loin'
!!$         ENDIF
!!$
!!$
!!$!fd @@@ le vertex 2 est suffisamment proche du triangle cd
!!$
!!$         IF (proj2 >= -adist .AND. proj2 < 2.d0*adist) THEN
!!$
!!$!fd @@@ le point ramene sur le plan ...
!!$ 
!!$           S = S_an2 + proj2*Ncd
!!$
!!$           v1=s1-S
!!$           v2=s2-S
!!$           v3=s3-S   
!!$
!!$           pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$           pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$           pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)
!!$      
!!$           test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$           pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$           pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$           pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$           test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$           pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$           pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$           pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                 
!!$           test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$           IF ((test1 >= -norm_test .AND. test2 >= -norm_test .AND. test3>= -norm_test) .OR. &
!!$               (test1 <=  norm_test .AND. test2 <=  norm_test .AND. test3<=  norm_test)) THEN
!!$
!!$             nb_ptc1=nb_ptc1+1
!!$             IF (nb_ptc1 > nb_ptc_max) THEN
!!$               PRINT*,'On a atteint le nombre de point detectable !!'
!!$               STOP
!!$             ENDIF
!!$
!!$             IF (bavard) THEN
!!$               PRINT*,'    ','dist',proj2,'alert',adist
!!$               PRINT*,'    ','tests : ',test1,test2,test3
!!$               PRINT*,'    ','vertex ',2,'position: ',S_an2
!!$               PRINT*,'    ','se projete en ',S
!!$               PRINT*,'    ','Ce vertex est dans la face de sommets: '
!!$               PRINT*,'    ','s1: ',s1
!!$               PRINT*,'    ','s2: ',s2
!!$               PRINT*,'    ','s3: ',s3
!!$             ENDIF
!!$
!!$             PT1(1:3,nb_ptc1)=S(1:3)
!!$             PT1_loc(1,nb_ptc1)  =proj2
!!$             PT1_loc(2:4,nb_ptc1)=Ncd
!!$
!!$           ELSE
!!$             IF (bavard) THEN
!!$               PRINT*,'La projection de 2 n''est pas dans la face'
!!$               PRINT*,'tests : ',test1,test2,test3
!!$             ENDIF
!!$           ENDIF
!!$         ELSE
!!$              IF (bavard) PRINT*,proj2,'elles sont trop loin'
!!$         ENDIF
!!$
!!$!fd @@@ le vertex 3 est suffisamment proche du triangle cd
!!$
!!$         IF (proj3 >= -adist .AND. proj3 < 2.d0*adist) THEN
!!$
!!$!fd @@@ le point ramene sur le plan ...
!!$ 
!!$           S = S_an3 + proj3*Ncd
!!$
!!$           v1=s1-S
!!$           v2=s2-S
!!$           v3=s3-S   
!!$
!!$           pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$           pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$           pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)
!!$      
!!$           test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$           pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$           pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$           pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$           test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$           pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$           pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$           pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                 
!!$           test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$           IF ((test1 >= -norm_test .AND. test2 >= -norm_test .AND. test3>= -norm_test) .OR. &
!!$               (test1 <=  norm_test .AND. test2 <=  norm_test .AND. test3<=  norm_test)) THEN
!!$
!!$             nb_ptc1=nb_ptc1+1
!!$             IF (nb_ptc1 > nb_ptc_max) THEN
!!$               PRINT*,'On a atteint le nombre de point detectable !!'
!!$               STOP
!!$             ENDIF
!!$
!!$             IF (bavard) THEN
!!$               PRINT*,'    ','dist',proj3,'alert',adist
!!$               PRINT*,'    ','tests : ',test1,test2,test3
!!$               PRINT*,'    ','vertex ',3,'position: ',S_an3
!!$               PRINT*,'    ','se projete en ',S
!!$
!!$               PRINT*,'    ','Ce vertex est dans la face de sommets: '
!!$               PRINT*,'    ','s1: ',s1
!!$               PRINT*,'    ','s2: ',s2
!!$               PRINT*,'    ','s3: ',s3
!!$             ENDIF
!!$
!!$             PT1(1:3,nb_ptc1)=S(1:3)
!!$             PT1_loc(1,nb_ptc1)  =proj3
!!$             PT1_loc(2:4,nb_ptc1)=Ncd
!!$
!!$           ELSE
!!$             IF (bavard) THEN
!!$               PRINT*,'La projection de 3 n''est pas dans la face'
!!$               PRINT*,'tests : ',test1,test2,test3
!!$             ENDIF
!!$           ENDIF
!!$         ELSE
!!$           IF (bavard) PRINT*,proj3,'elles sont trop loin'
!!$         ENDIF
!!$      ENDDO
!!$      IF (nb_ptc1 > nb_ptc_max) EXIT
!!$   ENDDO
!!$
!!$!   print*,'***************Changement de role*************************'
!!$
!!$   DO i=1,nb_face_an
!!$
!!$      Ncd= PR2%normal(1:3,face_an(i))
!!$
!!$      s1(1:3) = PR2%vertex(1:3,PR2%face(1,face_an(i)))
!!$      s2(1:3) = PR2%vertex(1:3,PR2%face(2,face_an(i)))
!!$      s3(1:3) = PR2%vertex(1:3,PR2%face(3,face_an(i)))
!!$
!!$!fd @@@ ca ne serait pas 3 fois la meme chose ?
!!$
!!$      data_cd =s1(1)*Ncd(1)+s1(2)*Ncd(2)+s1(3)*Ncd(3)
!!$
!!$      data_cd1=s1(1)*Ncd(1)+s1(2)*Ncd(2)+s1(3)*Ncd(3)
!!$      data_cd2=s2(1)*Ncd(1)+s2(2)*Ncd(2)+s2(3)*Ncd(3)
!!$      data_cd3=s3(1)*Ncd(1)+s3(2)*Ncd(2)+s3(3)*Ncd(3)
!!$
!!$      IF (dabs(data_cd-data_cd1)>1.d-7 .AND. &
!!$          dabs(data_cd-data_cd2)>1.d-7 .AND. &
!!$          dabs(data_cd-data_cd3)>1.d-7) THEN
!!$        PRINT*,'probleme de complaneite des points d''une face pour le cas an-cd'
!!$      ENDIF
!!$
!!$      v1=s1-s2
!!$      v2=s1-s3                    
!!$
!!$      N_tests(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$      N_tests(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$      N_tests(3)=v1(1)*v2(2)-v1(2)*v2(1)   
!!$
!!$      norm_test=1d-10*DOT_PRODUCT(N_tests,N_tests)
!!$
!!$      DO j=1,nb_face_cd         
!!$
!!$         Nan=PR1%normal(1:3,face_cd(j))
!!$ 
!!$         IF (bavard) THEN
!!$           PRINT*,'<-- PR: ',id1,' Face: ',face_cd(j), 'Normale= ',Nan
!!$           PRINT*,'    sur'
!!$           PRINT*,'    PR: ',id2,' Face: ',face_an(i), 'Normale= ',Ncd,'-->'
!!$         ENDIF
!!$
!!$         tmpval=DOT_PRODUCT(Ncd,Nan)
!!$
!!$         IF (tmpval > -0.01d0) THEN
!!$          IF (bavard) PRINT*,'Les normales sont incompatibles'
!!$          CYCLE
!!$         ENDIF
!!$
!!$         !fd @@@ calcul de vertex internes pour la detection du contact
!!$         !fd @@@ on se des fonctions de forme calculees dans compute box
!!$
!!$         S_an1(:)=fformT3(1,1)*PR1%vertex(:,PR1%face(1,face_cd(j)))+ &
!!$                  fformT3(2,1)*PR1%vertex(:,PR1%face(2,face_cd(j)))+ &
!!$                  fformT3(3,1)*PR1%vertex(:,PR1%face(3,face_cd(j)))
!!$         S_an2(:)=fformT3(1,2)*PR1%vertex(:,PR1%face(1,face_cd(j)))+ &
!!$                  fformT3(2,2)*PR1%vertex(:,PR1%face(2,face_cd(j)))+ &
!!$                  fformT3(3,2)*PR1%vertex(:,PR1%face(3,face_cd(j)))
!!$         S_an3(:)=fformT3(1,3)*PR1%vertex(:,PR1%face(1,face_cd(j)))+ &
!!$                  fformT3(2,3)*PR1%vertex(:,PR1%face(2,face_cd(j)))+ &
!!$                  fformT3(3,3)*PR1%vertex(:,PR1%face(3,face_cd(j)))
!!$
!!$         data_an1=S_an1(1)*Ncd(1)+S_an1(2)*Ncd(2)+S_an1(3)*Ncd(3)
!!$         data_an2=S_an2(1)*Ncd(1)+S_an2(2)*Ncd(2)+S_an2(3)*Ncd(3)
!!$         data_an3=S_an3(1)*Ncd(1)+S_an3(2)*Ncd(2)+S_an3(3)*Ncd(3)
!!$
!!$         proj1=data_cd-data_an1
!!$         proj2=data_cd-data_an2
!!$         proj3=data_cd-data_an3
!!$
!!$!fd @@@ le vertex 1 est suffisamment proche du triangle cd
!!$
!!$         IF (proj1 >= -adist .AND. proj1 < 2.d0*adist) THEN
!!$
!!$!fd @@@ le point ramene sur le plan ...
!!$ 
!!$           S = S_an1 + proj1*Ncd
!!$
!!$           v1=s1-S
!!$           v2=s2-S
!!$           v3=s3-S   
!!$
!!$           pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$           pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$           pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)      
!!$
!!$           test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$           pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$           pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$           pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$           test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$           pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$           pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$           pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$
!!$           test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$           IF ((test1 >= -norm_test .AND. test2 >= -norm_test .AND. test3>= -norm_test) .OR. &
!!$               (test1 <=  norm_test .AND. test2 <=  norm_test .AND. test3<=  norm_test)) THEN
!!$
!!$!fd @@@ ... il est dans le triangle on le garde 
!!$
!!$              nb_ptc2=nb_ptc2+1
!!$              IF (nb_ptc2 > nb_ptc_max) THEN
!!$                PRINT*,'On a atteint le nombre de point detectable !!'
!!$                STOP
!!$              ENDIF
!!$
!!$              IF (bavard) THEN
!!$                PRINT*,'    ','dist',proj1,'alert',adist
!!$                PRINT*,'    ','tests : ',test1,test2,test3
!!$                PRINT*,'    ','vertex ',1,'position: ',S_an1
!!$                PRINT*,'    ','se projete en ',S
!!$
!!$                PRINT*,'    ','Ce vertex est dans la face de sommets: '
!!$                PRINT*,'    ','s1: ',s1
!!$                PRINT*,'    ','s2: ',s2
!!$                PRINT*,'    ','s3: ',s3
!!$              ENDIF
!!$!fd @@@ mettre le mi-point
!!$
!!$              PT2(1:3,nb_ptc2)=S(1:3)
!!$              PT2_loc(1,nb_ptc2)  =proj1
!!$              PT2_loc(2:4,nb_ptc2)=Ncd
!!$
!!$!fd @@@ voir le probleme du gap et de la normale au point !!
!!$
!!$           ELSE
!!$             IF (bavard) THEN
!!$               PRINT*,'La projection de 1 n''est pas dans la face'
!!$               PRINT*,'tests : ',test1,test2,test3
!!$             ENDIF
!!$           ENDIF
!!$         ELSE
!!$           IF (bavard) PRINT*,proj1,'elles sont trop loin'
!!$         ENDIF
!!$
!!$
!!$!fd @@@ le vertex 2 est suffisamment proche du triangle cd
!!$
!!$         IF (proj2 >= -adist .AND. proj2 < 2.d0*adist) THEN
!!$
!!$!fd @@@ le point ramene sur le plan ...
!!$ 
!!$           S = S_an2 + proj2*Ncd
!!$
!!$           v1=s1-S
!!$           v2=s2-S
!!$           v3=s3-S   
!!$
!!$           pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$           pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$           pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)
!!$      
!!$           test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$           pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$           pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$           pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$           test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$           pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$           pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$           pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                 
!!$           test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$           IF ((test1 >= -norm_test .AND. test2 >= -norm_test .AND. test3>= -norm_test) .OR. &
!!$               (test1 <=  norm_test .AND. test2 <=  norm_test .AND. test3<=  norm_test)) THEN
!!$
!!$             nb_ptc2=nb_ptc2+1
!!$             IF (nb_ptc2 > nb_ptc_max) THEN
!!$               PRINT*,'On a atteint le nombre de point detectable !!'
!!$               STOP
!!$             ENDIF
!!$  
!!$              IF (bavard) THEN
!!$                PRINT*,'    ','dist',proj2,'alert',adist
!!$                RINT*,'    ','tests : ',test1,test2,test3
!!$                PRINT*,'    ','vertex ',2,'position: ',S_an2
!!$                PRINT*,'    ','se projete en ',S
!!$
!!$                PRINT*,'    ','Ce vertex est dans la face de sommets: '
!!$                PRINT*,'    ','s1: ',s1
!!$                PRINT*,'    ','s2: ',s2
!!$                PRINT*,'    ','s3: ',s3
!!$              ENDIF
!!$
!!$
!!$             PT2(1:3,nb_ptc2)=S(1:3)
!!$             PT2_loc(1,nb_ptc2)  =proj2
!!$             PT2_loc(2:4,nb_ptc2)=Ncd
!!$
!!$           ELSE
!!$             IF (bavard) THEN
!!$               PRINT*,'La projection de 2 n''est pas dans la face'
!!$               PRINT*,'tests: ',test1,test2,test3
!!$             ENDIF
!!$           ENDIF
!!$         ELSE
!!$              IF (bavard) PRINT*,proj2,'elles sont trop loin'
!!$         ENDIF
!!$
!!$!fd @@@ le vertex 3 est suffisamment proche du triangle cd
!!$
!!$         IF (proj3 >= -adist.AND. proj3 < 2.d0*adist) THEN
!!$
!!$!fd @@@ le point ramene sur le plan ...
!!$ 
!!$           S = S_an3 + proj3*Ncd
!!$
!!$           v1=s1-S
!!$           v2=s2-S
!!$           v3=s3-S   
!!$
!!$           pv1(1)=v1(2)*v2(3)-v1(3)*v2(2)
!!$           pv1(2)=v1(3)*v2(1)-v1(1)*v2(3)
!!$           pv1(3)=v1(1)*v2(2)-v1(2)*v2(1)
!!$      
!!$           test1=pv1(1)*N_tests(1)+pv1(2)*N_tests(2)+pv1(3)*N_tests(3)
!!$
!!$           pv2(1)=v2(2)*v3(3)-v2(3)*v3(2)
!!$           pv2(2)=v2(3)*v3(1)-v2(1)*v3(3)
!!$           pv2(3)=v2(1)*v3(2)-v2(2)*v3(1) 
!!$
!!$           test2=pv2(1)*N_tests(1)+pv2(2)*N_tests(2)+pv2(3)*N_tests(3)
!!$
!!$           pv3(1)=v3(2)*v1(3)-v3(3)*v1(2)
!!$           pv3(2)=v3(3)*v1(1)-v3(1)*v1(3)
!!$           pv3(3)=v3(1)*v1(2)-v3(2)*v1(1) 
!!$                 
!!$           test3=pv3(1)*N_tests(1)+pv3(2)*N_tests(2)+pv3(3)*N_tests(3)
!!$
!!$           IF ((test1 >= -norm_test .AND. test2 >= -norm_test .AND. test3>= -norm_test) .OR. &
!!$               (test1 <=  norm_test .AND. test2 <=  norm_test .AND. test3<=  norm_test)) THEN
!!$
!!$             nb_ptc2=nb_ptc2+1
!!$             IF (nb_ptc2 > nb_ptc_max) THEN
!!$               PRINT*,'On a atteint le nombre de point detectable !!'
!!$               STOP
!!$             ENDIF
!!$
!!$             IF (bavard) THEN
!!$               PRINT*,'    ','dist',proj3,'alert',adist
!!$               PRINT*,'    ','tests : ',test1,test2,test3
!!$               PRINT*,'    ','vertex ',3,'position: ',S_an3
!!$               PRINT*,'    ','se projete en ',S
!!$
!!$               PRINT*,'    ','Ce vertex est dans la face de sommets: '
!!$               PRINT*,'    ','s1: ',s1
!!$               PRINT*,'    ','s2: ',s2
!!$               PRINT*,'    ','s3: ',s3
!!$
!!$             ENDIF
!!$             PT2(1:3,nb_ptc2)=S(1:3)
!!$             PT2_loc(1,nb_ptc2)  =proj3
!!$             PT2_loc(2:4,nb_ptc2)=Ncd
!!$
!!$           ELSE
!!$             IF (bavard) THEN
!!$               PRINT*,'La projection de 3 n''est pas dans la face'
!!$               PRINT*,'tests : ',test1,test2,test3
!!$             ENDIF
!!$           ENDIF
!!$         ELSE
!!$           IF (bavard) PRINT*,proj3,'elles sont trop loin'
!!$         ENDIF
!!$       ENDDO
!!$       IF (nb_ptc2 > nb_ptc_max) EXIT
!!$   ENDDO
!!$
!!$!fd @@@ elimination des noeuds detectes plusieurs fois    
!!$  
!!$   is_ok=0
!!$   norm_min=MIN(PR1%min_radius_face,PR2%min_radius_face) 
!!$   norm_max=MAX(PR1%max_radius_face,PR2%max_radius_face) 
!!$
!!$   IF (norm_min/norm_max < 0.8) THEN 
!!$!fd @@@ quand le ratio de taille des faces est petit
!!$!     norm=norm_min*0.5
!!$     norm=norm_min
!!$   ELSE
!!$!fd @@@ quand le ratio de taille des faces est grand
!!$!     norm=norm_min*0.25
!!$     norm=norm_min
!!$   ENDIF      
!!$   norm2=norm*norm
!!$   IF (shrink /= 1.d0) norm2=norm2/(1.d0 - shrink)
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'On a trouve'
!!$     DO i=1,nb_ptc1
!!$       PRINT*,'PR: ',id1,i,PT1(1,i),PT1(2,i),PT1(3,i),PT1_loc(1,i)
!!$     ENDDO
!!$     DO i=1,nb_ptc2
!!$       PRINT*,'PR: ',id2,i,PT2(1,i),PT2(2,i),PT2(3,i),PT2_loc(1,i)
!!$     ENDDO
!!$
!!$     PRINT*,'valeur de reference pour les tris est',norm2
!!$     
!!$     PRINT*,'tri des points proches'
!!$   ENDIF
!!$
!!$   nb_stored=0
!!$
!!$   DO i=1,nb_ptc1
!!$     IF (is_ok(i) == 1) CYCLE
!!$     nb_stored=nb_stored+1
!!$     DO j=i+1,nb_ptc1   
!!$       IF (is_ok(j) == 1) CYCLE
!!$       scal=DOT_PRODUCT(PT1(:,i)-PT1(:,j),PT1(:,i)-PT1(:,j))
!!$       IF (scal < 5d-2*norm2) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'dist**2 ','pt1',i,'pt2',j,scal 
!!$           PRINT*,'on retire PT1: ',j
!!$         ENDIF
!!$         is_ok(j)=1
!!$       ENDIF
!!$     ENDDO
!!$
!!$     DO j=1,nb_ptc2   
!!$       IF (is_ok(nb_ptc1+j) == 1) CYCLE
!!$       scal=DOT_PRODUCT(PT1(:,i)-PT2(:,j),PT1(:,i)-PT2(:,j))
!!$       IF (scal < 5d-2*norm2) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'dist**2 ','pt1',i,'pt2',j,scal
!!$           PRINT*,'on retire PT2: ',j
!!$         ENDIF
!!$         is_ok(nb_ptc1+j)=1
!!$       ENDIF
!!$     ENDDO
!!$   ENDDO
!!$
!!$   DO i=1,nb_ptc2
!!$     IF (is_ok(nb_ptc1+i) == 1) CYCLE
!!$     nb_stored=nb_stored+1
!!$     DO j=i+1,nb_ptc2   
!!$       IF (is_ok(nb_ptc1+j) == 1) CYCLE
!!$       scal=DOT_PRODUCT(PT2(:,i)-PT2(:,j),PT2(:,i)-PT2(:,j))
!!$       IF (scal < 5d-2*norm2) THEN
!!$         IF (bavard) THEN
!!$           PRINT*,'dist**2 ','pt2',i,'pt2',j,scal
!!$           PRINT*,'on retire PT2: ',j
!!$         ENDIF
!!$         is_ok(nb_ptc1+j)=1
!!$       ENDIF
!!$     ENDDO
!!$   ENDDO
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'On a garde :',nb_stored
!!$     PRINT*,'On ne cherche plus que parmi'
!!$     DO i=1,nb_ptc1
!!$       IF (is_ok(i) /= 0) CYCLE
!!$       PRINT*,'PT1: ',i,PT1(1,i),PT1(2,i),PT1(3,i)
!!$     ENDDO
!!$     DO i=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+i) /= 0) CYCLE
!!$       PRINT*,'PT2: ',i,PT2(1,i),PT2(2,i),PT2(3,i)
!!$     ENDDO
!!$     PRINT*,'tri des points par gap'
!!$   ENDIF
!!$
!!$!fd 
!!$!fd essaie d'elimination des noeuds les plus loin des faces en vis a vis
!!$!fd il faut qu'ils soient a plus de 1.d-5*la distance d'alerte
!!$!fd on risque de perdre des noeuds importants sinon
!!$!fd PT1_loc(1,:) est -gap
!!$!fd 
!!$!fd tres dangereux ...
!!$
!!$   k=0
!!$   scal_min=-0.00001*adist
!!$   DO i=1,nb_stored-4
!!$     scal=1.d+20
!!$     i1=0;i2=0    
!!$     DO j=1,nb_ptc1
!!$       IF (is_ok(j) /= 0) CYCLE
!!$       IF (PT1_loc(1,j) < scal .AND. PT1_loc(1,j) < scal_min) THEN
!!$         scal=PT1_loc(1,j) 
!!$         i1=j
!!$       ENDIF
!!$     ENDDO
!!$     DO j=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+j) /= 0) CYCLE 
!!$       IF (PT2_loc(1,j) < scal  .AND. PT2_loc(1,j) < scal_min) THEN
!!$         scal=PT2_loc(1,j) 
!!$         i1=0
!!$         i2=j
!!$       ENDIF
!!$     ENDDO
!!$     IF (i1 /= 0) THEN
!!$       is_ok(i1)=1
!!$       k=k+1
!!$       IF (bavard) PRINT*,'dans PR1 on elimine le noeud',i1
!!$     ENDIF
!!$     IF (i2 /= 0) THEN
!!$      is_ok(nb_ptc1+i2)=1
!!$      k=k+1
!!$       IF (bavard) PRINT*,'dans PR2 on elimine le noeud',i2
!!$     ENDIF
!!$     IF (i1 == 0 .AND. i2 == 0) THEN
!!$       IF (bavard) PRINT*,'aucun point eliminable'
!!$       EXIT
!!$     ENDIF
!!$   ENDDO
!!$
!!$   nb_stored=nb_stored-k
!!$
!!$   IF (bavard) THEN
!!$     PRINT*,'il ne reste plus que ',nb_stored,' noeuds'
!!$     PRINT*,'tri par inter-distances'
!!$   ENDIF
!!$
!!$!fd projection sur un plan pour les points non coplanaires ?
!!$
!!$!fd on calcule le centre du nuage.
!!$!fd on vire le plus proche
!!$!fd on recommence ...
!!$
!!$   k=0
!!$   DO i=1,nb_stored-4
!!$
!!$     nb_ctc=0
!!$     PT=0.d0
!!$     DO j=1,nb_ptc1
!!$       IF (is_ok(j) /= 0) CYCLE
!!$       nb_ctc=nb_ctc+1
!!$       PT(:)=PT(:)+PT1(:,j)
!!$     ENDDO
!!$
!!$     DO j=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+j) /= 0) CYCLE
!!$       nb_ctc=nb_ctc+1
!!$       PT(:)=PT(:)+PT2(:,j)
!!$     ENDDO
!!$
!!$     PT=PT/REAL(nb_ctc,8)
!!$
!!$     i1=0;i2=0    
!!$     scal_min=1.d20
!!$     DO j=1,nb_ptc1
!!$       IF (is_ok(j) /= 0) CYCLE
!!$       scal=DOT_PRODUCT(PT(:)-PT1(:,j),PT(:)-PT1(:,j))
!!$       IF (scal < scal_min) THEN
!!$         scal_min=scal         
!!$         i1=j
!!$       ENDIF
!!$     ENDDO
!!$     DO j=1,nb_ptc2
!!$       IF (is_ok(nb_ptc1+j) /= 0) CYCLE 
!!$       scal=DOT_PRODUCT(PT(:)-PT2(:,j),PT(:)-PT2(:,j))
!!$       IF (scal < scal_min) THEN
!!$         scal_min=scal
!!$         i1=0
!!$         i2=j
!!$       ENDIF
!!$     ENDDO
!!$     IF (i1 /= 0) THEN
!!$       is_ok(i1)=1
!!$       k=k+1
!!$       IF (bavard) PRINT*,'dans PR1 on elimine le noeud',i1
!!$     ENDIF
!!$     IF (i2 /= 0) THEN
!!$       is_ok(nb_ptc1+i2)=1
!!$       k=k+1
!!$       IF (bavard) PRINT*,'dans PR2 on elimine le noeud',i2
!!$     ENDIF
!!$     IF (i1 == 0 .AND. i2 == 0) THEN
!!$       IF (bavard) PRINT*,'aucun point eliminable'
!!$       EXIT
!!$     ENDIF
!!$   ENDDO
!!$
!!$   PC=PT
!!$   PT_CTC       = 0.D0
!!$
!!$   nb_select=0
!!$   DO i=1,nb_ptc1
!!$     IF (is_ok(i) /= 0) CYCLE
!!$     nb_select=nb_select+1
!!$     IF (nb_select > 4) THEN
!!$       PRINT*,'trop de point de contact'
!!$       STOP
!!$     ENDIF
!!$     PT_CTC(:,nb_select)=PT1(:,i)
!!$     overlap(nb_select)=-PT1_loc(1,i)
!!$   ENDDO
!!$
!!$   DO i=1,nb_ptc2
!!$     IF (is_ok(nb_ptc1+i) /= 0) CYCLE
!!$     nb_select=nb_select+1
!!$     IF (nb_select > 4) THEN
!!$       PRINT*,'trop de point de contact'
!!$       STOP
!!$     ENDIF
!!$     PT_CTC(:,nb_select)=PT2(:,i)
!!$     overlap(nb_select)=-PT2_loc(1,i)
!!$   ENDDO
!!$
!!$   pt_surf=0.d0
!!$
!!$!   print*,nb_select
!!$
!!$   IF (nb_select < 4) THEN
!!$      nb_ctc=nb_select 
!!$!      do i=1,nb_ptc1
!!$!        print*,PT1(:,i)
!!$!      enddo
!!$!      do i=1,nb_ptc2
!!$!        print*,PT2(:,i)
!!$!      enddo
!!$   ELSE
!!$     nb_ctc=4
!!$
!!$!     PT=0.d0
!!$!     is_hori =.true.
!!$!     do i=1,nb_ctc
!!$!       PT=PT+PT_CTC(:,i)
!!$!       if (dabs(PT_CTC(3,i) - PT_CTC(3,1)) .gt. 1d-2) is_hori=.false.
!!$!     enddo
!!$
!!$!     if ( is_hori) then
!!$!       PT=PT/REAL(nb_ctc,8)
!!$!       i1=1
!!$!       do i=2,nb_ctc
!!$!         if (((PT_CTC(1,i)-PT_CTC(1,i1))*(PT(2) - PT_CTC(2,i1))-(PT_CTC(2,i)-PT_CTC(2,i1))*(PT(1) - PT_CTC(1,i1))) >= 0.d0) then
!!$!           i2 = i
!!$!           exit
!!$!         endif
!!$!         if (i == nb_ctc) then
!!$!           print*,'fred t es un ane'
!!$!           print*,PT
!!$!           print*,PT_CTC
!!$!           stop
!!$!         endif
!!$!       enddo
!!$!       do i=2,nb_ctc
!!$!         if (i == i1 .or. i == i2) cycle 
!!$!         if (((PT_CTC(1,i)-PT_CTC(1,i2))*(PT(2) - PT_CTC(2,i2))-(PT_CTC(2,i)-PT_CTC(2,i2))*(PT(1) - PT_CTC(1,i2))) >= 0.d0) then
!!$!           i3 = i
!!$!           exit
!!$!         endif
!!$!         if (i == nb_ctc) then
!!$!           print*,'fred t es un ane'
!!$!           print*,PT
!!$!           print*,PT_CTC
!!$!           stop
!!$!         endif
!!$!       enddo
!!$!       do i=2,nb_ctc
!!$!         if (i == i1 .or. i == i2 .or. i == i3) cycle 
!!$!         i4 = i
!!$!       enddo
!!$
!!$!       a = sqrt(dot_product(PT_CTC(:,i2)-PT_CTC(:,i1),PT_CTC(:,i2)-PT_CTC(:,i1)))
!!$!       b = sqrt(dot_product(PT_CTC(:,i4)-PT_CTC(:,i1),PT_CTC(:,i4)-PT_CTC(:,i1)))
!!$
!!$!       pt_surf = a*b*0.25
!!$
!!$!     endif
!!$! calcul de la surface equivalente au point
!!$! ici on suppose qu'on est dans le plan xy
!!$    
!!$
!!$!
!!$   ENDIF   
!!$
!!$
!!$ END SUBROUTINE INTERSECTION_POLYR_WORK_new

!!$!------------------------------------------------------------------------------------------------------
!!$ SUBROUTINE wed_compute_contact_PRPRx
!!$ ! wed means With External Detection
!!$ 
!!$   IMPLICIT NONE  
!!$
!!$   INTEGER                     :: errare 
!!$   INTEGER                     :: icdan,iadj,itac
!!$   INTEGER                     :: icdtac,iantac,isee,itacty    
!!$   REAL(kind=8)                :: raycd,rayan,adist,dist,nonuc,gap
!!$   INTEGER                     :: r_icdtac,r_iantac
!!$   INTEGER                     :: i,id,j,nb_ctc
!!$   INTEGER                     :: size_of_array_this
!!$   REAL(kind=8),DIMENSION(:,:), pointer :: xco 
!!$   REAL(kind=8),DIMENSION(:),   pointer :: ovlap
!!$   REAL(kind=8),DIMENSION(:,:), pointer :: t, n, s
!!$   REAL(kind=8),DIMENSION(3)   :: sep,cdlev,anlev,Point,Vsep
!!$   REAL(kind=8)                :: norme,den,scal
!!$   REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
!!$   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an
!!$   REAL(kind=8)                :: t1,t2,vls_cst,vln_cst,vlt_cst
!!$   REAL(kind=8)                :: pt_surf = 1.d0 ! a modifier !!
!!$
!!$   integer :: cd_ent,an_ent
!!$   character(len=80) :: cout
!!$
!!$   icdan   = 0        
!!$   nb_PRPRx= 0
!!$   nb_adj  = 0
!!$   nb_detection_test=0
!!$   detection_time=0.D0
!!$   nb_shadow=0
!!$   nb_ctc_state=0
!!$
!!$   !xco=0.d0
!!$
!!$   IF (nb_rough_PRPRx /= 0 ) THEN
!!$
!!$   size_of_array_this=SIZE(this)
!!$  
!!$   !
!!$   ! preparation de la detection 
!!$   !
!!$     DO i=1,nb_rough_PRPRx
!!$       icdtac    = rough_PRPRx(i)%cd
!!$       iantac    = rough_PRPRx(i)%an 
!!$       isee      = rough_PRPRx(i)%isee
!!$
!!$       adist=see(isee)%alert 
!!$       dist=S_POLYR(icdtac)%radius+S_POLYR(iantac)%radius+adist
!!$
!!$       perio_shift = 0.d0
!!$       perio_shift(1) = real(rough_PRPRx(i)%xperiodic,8) * xperiode
!!$       perio_shift(2) = real(rough_PRPRx(i)%yperiodic,8) * yperiode
!!$
!!$       sep=S_POLYR(icdtac)%center - (S_POLYR(iantac)%center + perio_shift)
!!$       norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)
!!$
!!$       IF (norme <1.D-24) THEN
!!$          PRINT*,'Error in mod_PRPRx, in compute contact polyr'
!!$          PRINT*,' Distance between',icdtac, ' and ',iantac, 'is'
!!$          PRINT*,norme 
!!$          STOP
!!$       ENDIF   
!!$
!!$
!!$!fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
!!$!fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
!!$!fd @@@ et il en manque je pense ...
!!$
!!$       IF (norme < dist*dist) THEN
!!$
!!$!fd @@@ ca n'est pas suffisant non ?
!!$
!!$         IF (((S_POLYR(iantac)%maxpos(1)+perio_shift(1))-S_POLYR(icdtac)%minpos(1)+adist)<0.D0) CYCLE
!!$         IF (((S_POLYR(iantac)%maxpos(2)+perio_shift(2))-S_POLYR(icdtac)%minpos(2)+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(iantac)%maxpos(3)                 -S_POLYR(icdtac)%minpos(3)+adist)<0.D0) CYCLE
!!$
!!$!fd @@@ je rajoute ....
!!$
!!$         IF ((S_POLYR(icdtac)%maxpos(1)-(S_POLYR(iantac)%minpos(1)+perio_shift(1))+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(icdtac)%maxpos(2)-(S_POLYR(iantac)%minpos(2)+perio_shift(2))+adist)<0.D0) CYCLE
!!$         IF ((S_POLYR(icdtac)%maxpos(3)- S_POLYR(iantac)%minpos(3)                +adist)<0.D0) CYCLE
!!$
!!$         CALL cpu_time(t1)
!!$
!!$         CALL DETECTION_Indian(iantac,icdtac,adist,nb_ctc,xco,ovlap,n)!,t,s)
!!$         if (associated(t)) deallocate(t)
!!$         if (associated(s)) deallocate(s)
!!$         allocate(t(3,nb_ctc))
!!$         allocate(s(3,nb_ctc))
!!$         do j=1,nb_ctc
!!$           call comp_rep(t(:,j),n(:,j),s(:,j))
!!$         end do
!!$
!!$         CALL cpu_time(t2)
!!$
!!$         nb_tot_detect=nb_tot_detect+1
!!$         nb_detection_test=nb_detection_test+1
!!$         detection_time=detection_time+t2-t1
!!$         
!!$         IF (nb_ctc==0) CYCLE
!!$
!!$           localframe_cd = get_inertia_frameTT_POLYR(icdtac)
!!$           localframe_an = get_inertia_frameTT_POLYR(iantac)
!!$
!!$           cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
!!$           an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)
!!$           
!!$           DO j=1,nb_ctc
!!$
!!$           vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1,j)+ (cd_Vbegin(2)-an_Vbegin(2))*t(2,j)+(cd_Vbegin(3)-an_Vbegin(3))*t(3,j)
!!$           vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1,j)+ (cd_Vbegin(2)-an_Vbegin(2))*n(2,j)+(cd_Vbegin(3)-an_Vbegin(3))*n(3,j)
!!$           vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1,j)+ (cd_Vbegin(2)-an_Vbegin(2))*s(2,j)+(cd_Vbegin(3)-an_Vbegin(3))*s(3,j)
!!$
!!$             icdan                   = icdan+1	  	       	       
!!$             IF (icdan>size_of_array_this) THEN
!!$                PRINT*,'---------------------------------------------'
!!$                PRINT*,'ERROR filling this                           '
!!$                PRINT*,'you rich the allocated size                  '
!!$                PRINT*,'In COMMAND.DAT you should add the keywords : '
!!$                PRINT*,'LOW SIZE ARRAY POLYR                         ' 
!!$                PRINT*,'sizefactor                                   '
!!$                PRINT*,'where sizefactor is an integer specifiyng the'
!!$                PRINT*,'ratio of memory you need (=4 by default)     '
!!$                PRINT*,'Program stop                                 '
!!$                PRINT*,'---------------------------------------------'
!!$                STOP
!!$             ENDIF   
!!$             nb_adj(icdtac)          = nb_adj(icdtac)+1
!!$             iadj                    = nb_adj(icdtac)
!!$             this(icdan)%iadj        = iadj  
!!$             this(icdan)%icdbdy      = polyr2bdyty(1,icdtac)
!!$             this(icdan)%icdtac      = icdtac
!!$             this(icdan)%ianbdy      = polyr2bdyty(1,iantac)
!!$             this(icdan)%iantac      = iantac
!!$             this(icdan)%isee        = isee    	             
!!$             this(icdan)%tuc         = t(:,j)
!!$             this(icdan)%nuc         = n(:,j)
!!$             this(icdan)%suc         = s(:,j)
!!$
!!$             this(icdan)%coor        = xco(1:3,j)
!!$             this(icdan)%type_ctc    = nb_ctc
!!$
!!$             cd_ent = get_ent_POLYR(this(icdan)%icdtac)
!!$             an_ent = get_ent_POLYR(this(icdan)%iantac) 
!!$         
!!$             entity(cd_ent)%nb = entity(cd_ent)%nb+1
!!$             entity(an_ent)%nb = entity(an_ent)%nb+1
!!$
!!$!fd le 11/09/08 manque le shift. PRcoor c'est le centre du polyr pas le centre d'inertie
!!$             cdlev = xco(1:3,j)  &
!!$                   - (PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac))
!!$             anlev = xco(1:3,j) &
!!$                   - (PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac))! + perio_shift)
!!$
!!$
!!$!fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
!!$!fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel
!!$
!!$             this(icdan)%coorcd(1)=cdlev(1)*localframe_cd(1,1)+cdlev(2)*localframe_cd(2,1)+cdlev(3)*localframe_cd(3,1)
!!$             this(icdan)%coorcd(2)=cdlev(1)*localframe_cd(1,2)+cdlev(2)*localframe_cd(2,2)+cdlev(3)*localframe_cd(3,2)
!!$             this(icdan)%coorcd(3)=cdlev(1)*localframe_cd(1,3)+cdlev(2)*localframe_cd(2,3)+cdlev(3)*localframe_cd(3,3)
!!$
!!$             this(icdan)%cooran(1)=anlev(1)*localframe_an(1,1)+anlev(2)*localframe_an(2,1)+anlev(3)*localframe_an(3,1)
!!$             this(icdan)%cooran(2)=anlev(1)*localframe_an(1,2)+anlev(2)*localframe_an(2,2)+anlev(3)*localframe_an(3,2)
!!$             this(icdan)%cooran(3)=anlev(1)*localframe_an(1,3)+anlev(2)*localframe_an(2,3)+anlev(3)*localframe_an(3,3)
!!$
!!$!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!!$!             print*,icdan
!!$!             print*, xco(1:3,j)
!!$!             print*,this(icdan)%coorcd(1:3)
!!$!             print*,this(icdan)%cooran(1:3)
!!$!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!!$
!!$
!!$             ! On va calculer le passage rep inertie -> rep général pour l'antagoniste
!!$
!!$             Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
!!$             Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
!!$             Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)
!!$
!!$             Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
!!$             Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
!!$             Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)
!!$
!!$             Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
!!$             Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
!!$             Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)
!!$
!!$             this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 
!!$
!!$             this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 
!!$
!!$             this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
!!$
!!$
!!$             ! On va calculer le passage rep inertie -> rep général pour le candidat
!!$
!!$             Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
!!$             Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
!!$             Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)
!!$
!!$             Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
!!$             Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
!!$             Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)
!!$
!!$             Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
!!$             Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
!!$             Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)
!!$
!!$
!!$             this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
!!$             this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 
!!$
!!$             this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
!!$             this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 
!!$
!!$             this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
!!$             this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
!!$           
!!$             ! Calcul des vitesses relatives
!!$
!!$
!!$             this(icdan)%gapTTbegin      = ovlap(j)
!!$
!!$
!!$             this(icdan)%vltBEGIN = vlt_cst &
!!$                  + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
!!$                  - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)
!!$
!!$             this(icdan)%vlnBEGIN = vln_cst &     
!!$                  + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
!!$                  - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)
!!$
!!$             this(icdan)%vlsBEGIN= vls_cst &     
!!$                  + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
!!$                  - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)
!!$
!!$
!!$             this(icdan)%rls      = 0.D0
!!$             this(icdan)%rlt      = 0.D0
!!$             this(icdan)%rln      = 0.D0
!!$             this(icdan)%vls      = this(icdan)%vlsBEGIN
!!$             this(icdan)%vlt      = this(icdan)%vltBEGIN
!!$             this(icdan)%vln      = this(icdan)%vlnBEGIN
!!$             this(icdan)%gapTT    = this(icdan)%gapTTbegin
!!$             this(icdan)%status   = 'nknow'
!!$
!!$             this(icdan)%surf     = pt_surf
!!$
!!$             this(icdan)%node_rank = 0
!!$
!!$           ENDDO
!!$         ENDIF  
!!$     ENDDO
!!$     nb_PRPRx=icdan
!!$   ENDIF
!!$
!!$   WRITE(cout,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
!!$   call logmes(cout)
!!$   write(cout,*) 'Total time of detection: ',detection_time
!!$   call logmes(cout)
!!$   write(cout,*) 'Nb detection tests :',REAL(nb_detection_test,8)     
!!$   call logmes(cout)
!!$
!!$   CALL get_behaviour
!!$
!!$   DO itac=1,nb_POLYR
!!$     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
!!$     IF (nb_adj(itac) /= 0) THEN
!!$       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
!!$       IF (errare /=0 ) THEN
!!$         PRINT*,'error in allocating adjac(icdtac)%....., in select_prox_tactors in mod_PRPRx'
!!$         STOP
!!$       END IF
!!$     ENDIF
!!$   ENDDO 
!!$ 
!!$   DO icdan=1,nb_PRPRx
!!$     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
!!$   END DO 
!!$
!!$   IF (ALLOCATED(violation)) DEALLOCATE(violation)
!!$   ALLOCATE(violation(nb_PRPRx),stat=errare)
!!$
!!$   if (associated(xco))   deallocate(xco)
!!$   if (associated(ovlap)) deallocate(ovlap)
!!$   if (associated(t)) deallocate(t)
!!$   if (associated(n)) deallocate(n)
!!$   if (associated(s)) deallocate(s)
!!$
!!$ END SUBROUTINE wed_compute_contact_PRPRx
!!$!------------------------------------------------------------------------------------------------------
