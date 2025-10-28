!==========================================================================
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

 !> \brief preparing input data
 !> extracting informations from the contribs in order to use a shared solver
 subroutine prep_nlgs(actif)
   implicit none
   !> Is Stored Delassus Loop algorithm activated
   logical, intent(in) :: actif
   ! 
   integer         :: ik,ibehav,icdan,i
   integer         :: ient !sdl
   real(kind=8)    :: forward,backward,nbc,TshVlt, det

   real(kind=8), dimension(inter_dim) :: vik,rik

   type(T_interaction), pointer :: this, this_jl

   ! behaviours part
   real(kind=8) :: fric,                         & ! friction coefficient
                   tangalrest,normalrest,        & ! restitution coefficients
                   normalcoh,tangalcoh,Wethk,    &
                   forcePERgap,forcePERstrain,   &
                   forcePERstrainrate,prestrain
   ! RGR CRITIC
   real(kind=8) :: ToverH,OTH,vOVERcv, &
                   gap_tol,meff,reff,etan,etat,vloc, &
                   DW, SW

   logical, save :: is_present = .false.
   logical       :: ok
   
   integer(kind=4) :: jl,iadj,jadj,nbadj,jll,icdik,ianik,bandwith,icdent,ianent,istart,n2
   integer(kind=4) :: ikadj,ikstart,jladj,jlstart
   real(kind=8)    :: cd_length,thickness ! mac_czm
   logical, save   :: is_initialized=.false.
   ! bimodal list
   real(kind=8)    :: reac_mean = 0.d0
   integer(kind=4) :: nb_WEAK,nb_STRONG
   !local periodic computation
   real(kind=8) :: Y_x,Y_y,EY_x,EY_y
   real(kind=8), dimension(3) :: E
   !mj randomlist
   integer      :: IAL1
   real(kind=8) :: RA
   !
   real(kind=8) :: un
   real(kind=8) :: pre_gap
   character(len=23) :: IAM
   character(len=80) :: cout
   !      12345678901234567890123
   IAM = 'nlgs_new_int::prep_nlgs'

   new_int_nlgs_solver = .true.
   SDLactif    = actif
   nlgs_loop   = 0

   if( .not. is_initialized ) call init_inter_laws(inter_dim,diagonal_resolution)

   nb_CDAN = get_nb_interactions()

   if (nb_CDAN == 0) return 
  
   if( allocated(ialeat) .and. size(ialeat)<nb_CDAN ) then
     deallocate(ialeat)
   end if
   if( .not. allocated(ialeat) ) then
     allocate(ialeat(nb_CDAN))
   end if
   
   if( allocated(ialeatr) .and. size(ialeatr)<nb_CDAN ) then
     deallocate(ialeatr)
   end if
   if( .not. allocated(ialeatr) ) then
     allocate(ialeatr(nb_CDAN))
   end if
   
   if( allocated(iwksg) .and. size(iwksg)<nb_CDAN ) then
     deallocate(iwksg) 
   end if
   if( .not. allocated(iwksg) ) then
     allocate(iwksg(nb_CDAN))
   end if
   
!!!mj randomlist
   if( allocated(randomlist) .and. size(randomlist)<nb_CDAN ) then
     deallocate(randomlist)
   end if
   if( .not. allocated(randomlist) ) then
     allocate(randomlist(nb_CDAN))
   end if
   
   if( allocated(Xvlton) .and. size(Xvlton)<nb_CDAN ) then
     deallocate(Xvlton)
   end if
   if( .not. allocated(Xvlton) ) then
     allocate(Xvlton(nb_CDAN))
   end if
   Xvlton(1:nb_CDAN) = 0.d0
   
   if( allocated(WRRarray) .and. size(WRRarray)<nb_CDAN ) then
     deallocate(WRRarray)
   end if
   if( .not. allocated(WRRarray) ) then
     allocate(WRRarray(nb_CDAN))
   end if
   WRRarray(1:nb_CDAN) = 1.d0

   !rm : done when filling adjjl
   !do ik = 1, nb_cdan
   !  this => get_interaction(ik)
   !  if( associated(thise%adjjl) ) deallocate(this%adjjl)
   !end do

   do ik = 1, nb_cdan
      ialeat(ik)  = ik
      ialeatr(ik) = ik

      call random_number(ra)
      ial1 = idint(ra*real(nb_cdan,8))+1
      ial1 = min0(ial1,nb_cdan)
      ial1 = max0(1,ial1)
      randomlist(ik)=ial1
   end do

   nb_ENTITY = get_nb_ENTITY()

   call Create_EntityList   

!!!!
 
   reac_mean = 0.d0
  
   do icdan = 1, nb_cdan
     this => get_interaction(icdan)
     this%icdan = icdan
     reac_mean = reac_mean + this%rl(2)

     icdent = this%icdent
     ianent = this%ianent

     if (icdent /= ianent) then       
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = icdan
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(ianent)%list(entity(ianent)%ik) = icdan
     else 
       ! pour ne pas se compter 2 fois 
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = icdan
     endif

     call nullify_reac(this,iReac_)
     call nullify_vlocy(this,iVaux_)
   end do

   if( nb_CDAN /= 0 ) reac_mean = reac_mean/REAL(nb_CDAN,8)
  
   !!!!!!!!!!!!!!!!!
   ! Rnod = [H] Rloc
   !fd  + all the Vaux = 0. for sdl
   !!!!!!!!!!!!!!!!!

   nb_STRONG = 0
   nb_WEAK   = 0
   
   do ik = 1, nb_CDAN
     this => get_interaction(ik)
     call injj(this,this%rl(1:inter_dim),iReac_)
      if( this%rl(2) < reac_mean ) then
        nb_WEAK = nb_WEAK+1
        iwksg(nb_CDAN+1-nb_WEAK) = ik
      else
        this%ws = 1
        nb_STRONG = nb_STRONG+1
        iwksg(nb_STRONG) = ik
      end if
   end do
   
!!!*************************************************************************************
!!! computing local dynamic matrix W (Delassus matrix)
!!!
   if( SDLactif) then
      ! In this case the W matrix is going to be built and stored
      bandwith=0
      istart  = 1  
      do ik = 1, nb_CDAN
        this => get_interaction(ik)
        nbadj = 0

        icdik = this%icdent
        ianik = this%ianent

        if (icdik == ianik) then
          !fd & mr autocontact (avec la langue)       
          nbadj = entity(icdik)%nb-1
        else
          nbadj = entity(icdik)%nb+entity(ianik)%nb-2
        end if

        jl = 0
        
        if (nbadj /= 0) then
           
           if ( allocated(this%adjjl) .and. nbadj > size(this%adjjl)) deallocate(this%adjjl)
           
           if (.not. allocated(this%adjjl)) allocate(this%adjjl(nbadj))
           this%adjjl = 0
           
           do iadj = 1, entity(icdik)%nb
             if(entity(icdik)%list(iadj) == ik) cycle
             jl = jl+1
             this%adjjl(jl) = entity(icdik)%list(iadj)
           end do
           
           do iadj = 1, entity(ianik)%nb
             if(entity(ianik)%list(iadj) == ik) cycle
             
             !!!fd evacuation des contacts partages
             is_present = .FALSE.
             !!! precedemment on a range entity(icdik)%nb-1 contacts on les reparcours 
             !!! a la recherche d'un doublon avec celui qu'on veut poser
             do jadj = 1, entity(icdik)%nb-1
               if (this%adjjl(jadj) /= entity(ianik)%list(iadj)) cycle
               is_present = .TRUE.
               exit
             end do
             if (is_present) cycle
             jl = jl+1
             this%adjjl(jl) = entity(ianik)%list(iadj)
           end do
        end if
        
        this%istart = istart
        istart = inter_dim*inter_dim*jl + istart
        
        this%nbadj = jl
        bandwith = bandwith+jl

      end do
      
      !!!fd paranoiac test
      do ient = 1, nb_ENTITY
        if (entity(ient)%ik /= entity(ient)%nb) then
          call LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
          write(cout,'(A7,I5,A4,I5,A4,I5)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
          call FATERR(IAM,cout)
        end if
      end do
      
      if (allocated(Wab)) deallocate(Wab)
      allocate(Wab(inter_dim*inter_dim*bandwith))
      Wab = 0.d0
      
   end if
!!!
!!! end of computing local dynamic matrix W (Delassus matrix)
!!!*************************************************************************************

   n2 = inter_dim*inter_dim

   do ik = 1, nb_CDAN

     ! --------------------------------------------
     ! Computing the bloc diagonal part of matrix W
     ! --------------------------------------------
     this => get_interaction(ik)

     rik(1)           = 1.d0
     rik(2:inter_dim) = 0.d0

     call nullify_reac(this,iRaux_)
     call injj(this,rik(1:inter_dim),iRaux_)
     call vitrad(this,iVaux_e_invM_t_Raux_)
     call prjj(this,vik(1:inter_dim),iVaux_)

     this%W(1:inter_dim,1) = vik(1:inter_dim)
      
     if (SDLactif) then
       do ikadj = 1, this%nbadj
            
         jl = this%adjjl(ikadj)
         if (jl == 0) then
           icdik = this%icdent
           ianik = this%ianent
           write(*,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'contact :',ik,' icdent ',icdik,' ianent ',ianik
           print*,entity(icdik)%nb
           print*,entity(icdik)%list
           print*,entity(ianik)%nb
           print*,entity(ianik)%list
           write(*,'(A,1x,I0,1x,A)') ' nbabj ',this%nbadj,'liste :'
           print*,this%adjjl(:)

           write(*,'(A,1x,I0)') 'ca chie pour adj ',ikadj
         endif

         this_jl => get_interaction(jl)
         jlstart = this_jl%istart
            
         call prjj(this_jl,vik(1:inter_dim),iVaux_)
            
         ok = .false.
            
         ! shifting index of 1 for storing from 0 to 0+something
         do jladj = 0, this_jl%nbadj-1
           if (this_jl%adjjl(jladj+1) == ik) THEN
             Wab(jlstart + n2*jladj  ) = vik(1) !Wtt
             Wab(jlstart + n2*jladj+1) = vik(2) !Wnt
             if( inter_dim == 3 ) Wab(jlstart + n2*jladj+2) = vik(inter_dim) !Wst
             ok = .true.
           end if
         end do
            
         if (.not. ok) then
           call FATERR(IAM,'unable to find the reverse adjacent !!')
         end if
            
       end do
         
       call nullify_vlocy(this,iVaux_)

     end if
     
     if( inter_dim == 3 ) then
       rik(1:2)       = 0.d0
       rik(inter_dim) = 1.d0

       call nullify_reac(this,iRaux_)
       call injj(this,rik(1:inter_dim),iRaux_)
       call vitrad(this,iVaux_e_invM_t_Raux_)
       call prjj(this,vik(1:inter_dim),iVaux_)

       this%W(1:inter_dim,inter_dim) = vik(1:inter_dim)
        
       if (SDLactif) then
         do ikadj = 1, this%nbadj
              
           jl = this%adjjl(ikadj)

           this_jl => get_interaction(jl)
           jlstart = this_jl%istart
              
           call prjj(this_jl,vik(1:inter_dim),iVaux_)
              
           ok = .FALSE.
 
           do jladj = 0, this_jl%nbadj-1
             if (this_jl%adjjl(jladj+1) == ik) then
               Wab(jlstart + n2*jladj+6) = vik(1) !Wts
               Wab(jlstart + n2*jladj+7) = vik(2) !Wns
               if( inter_dim == 3 ) Wab(jlstart + n2*jladj+8) = vik(inter_dim) !Wss
               ok = .true.
             end if
           end do
              
           if (.not. ok) then
             call FATERR(IAM,'unable to find the reverse adjacent !!')
           end if
              
         end do
           
         call nullify_vlocy(this,iVaux_)

       end if
     end if

     rik(1) = 0.d0
     rik(2) = 1.d0
     if( inter_dim == 3 ) rik(inter_dim) = 0.d0

     call nullify_reac(this,iRaux_)
     call injj(this,rik(1:inter_dim),iRaux_)
     call vitrad(this,iVaux_e_invM_t_Raux_)
     call prjj(this,vik(1:inter_dim),iVaux_)

     this%W(1:inter_dim,2) = vik(1:inter_dim)
      
     if (SDLactif) then
       do ikadj = 1, this%nbadj
            
         jl = this%adjjl(ikadj)

         this_jl => get_interaction(jl)
         jlstart = this_jl%istart
            
         call prjj(this_jl,vik(1:inter_dim),iVaux_)

         ok = .false.

         do jladj = 0, this_jl%nbadj-1
           if (this_jl%adjjl(jladj+1) == ik) then
             Wab(jlstart + n2*jladj+inter_dim  ) = vik(1) !Wtn
             Wab(jlstart + n2*jladj+inter_dim+1) = vik(2) !Wnn
             if( inter_dim == 3 ) Wab(jlstart + n2*jladj+inter_dim+2) = vik(inter_dim) !Wsn
             ok = .true.
           end if
         end do
            
         if (.not. ok) then
           call FATERR(IAM,'unable to find the reverse adjacent !!')
         end if
            
       end do
         
       call nullify_vlocy(this,iVaux_)

     end if
     
     ibehav = this%lawnb

     !!! --------------------------------------
     !!! Warning and coping with critical cases
     !!! --------------------------------------

     if (this%W(2,2) .le. 1.D-18) then
        write(cout,543) ik,this%W(2,2)
543     format(1X,'  Wnn(',I5,') =',D12.5,' < 1.D-18')
        call logmes('Error '//IAM//': '//cout)
        write(cout,'(I8,A2,I7,A1,I7,A1,D12.5)') this%CDAN,': ',this%icdent,',',this%ianent,' ',this%gapTTbegin
        call logmes('Error '//IAM//': '//cout)
     end if

     if (this%W(1,1) .le. 1.D-06*this%W(2,2) .and. &
          !!!                          123456789012345678901234567890
          tact_behav(ibehav)%lawty /= 'ELASTIC_ROD                   ' .and. &
          tact_behav(ibehav)%lawty /= 'VOIGT_ROD                     ' ) then
        
        write(cout,544)ik,this%W(1,1),ik
544     format(1X,'   Wtt(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
        call logmes(cout)
        this%W(1,1)=1.D-06*this%W(2,2)
     end if

     if( inter_dim == 3) then
       if (this%W(inter_dim,inter_dim) .le. 1.D-06*this%W(2,2).and. &
            !!!                          123456789012345678901234567890
            tact_behav(ibehav)%lawty /= 'ELASTIC_ROD                   ' .and. &
            tact_behav(ibehav)%lawty /= 'VOIGT_ROD                     ' ) then
          write(cout,545)ik,this%W(inter_dim,inter_dim),ik
545       format(1X,'   Wss(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
          call logmes(cout)
          this%W(inter_dim,inter_dim)=1.D-06*this%W(2,2)
       end if
     end if
     !!!*************************************
     !!! Preparing auxiliaries
     !!!     
     !!!! default values

     this%fric = get_fric(ibehav,this%statusBEGIN)

     this%WW(1) = this%W(1,1)
     this%WW(2) = this%W(2,2)
     if( inter_dim == 3 ) this%WW(inter_dim) = this%W(inter_dim,inter_dim)

     this%covfree(1:inter_dim) = 0.d0
     this%corl(1:inter_dim)    = 0.d0      

     ! customized values
     if( associated(inter_laws(this%i_law)%prep_func) ) &
       call inter_laws(this%i_law)%prep_func(this,is_initialized)

     !!!--------------------------------------
     !!! Warning non uniqueness cases
     !!!--------------------------------------
       
     call non_uniqueness(this)

     !!!-------------------------------------
     !!! Computing free local vlocy
     !!!-------------------------------------
     call prjj(this,this%vfree(1:inter_dim),iVfree)
       
     !!!-------------------------------------
     !!! Computing free local vlocy
     !!!-------------------------------------
     this%statuscheck='nknow'
     this%status     ='nknow'

   end do

   is_initialized = .true.

 end subroutine prep_nlgs

 !> \brief One step of resolution of NLGS
 subroutine solve_nlgs(i_what)
   implicit none
   !> what to do : 1=prep, 2=iter, 3=check
   integer(kind=4), intent(in) :: i_what
   !
   integer(kind=4)  :: ik,ikk,ibehav,iadj,ikjl,ibdy,istart,iistart,ilaw
   character(len=5) :: sstatusik
   real(kind=8)     :: DET,forward,backward,DFT,DFN,FFN,Cforward,Cbackward
   real(kind=8)     :: fricik, modrl

   real(kind=8), dimension(inter_dim,inter_dim) :: WWik
   real(kind=8), dimension(inter_dim)           :: vlocfreeik, Wrlik, Wrliki, vik, rik, vlik, rlik
   real(kind=8), dimension(inter_dim)           :: vvlocfreeik, vvlik, rrlik, vliki, rliki, rloc, vl, Dvl

   real(kind=8) :: normalcoh,tangalcoh,Wethk
   real(kind=8) :: WRRmin,WRRmax,RWR,bR,alphaik,fnik
    
   !fd czm 
   real(kind=8) :: Hradh_t,Hradh_n
   logical      :: is_cohesive
   real(kind=8) :: k_n,k_t,detJ,Att,Atn,Ant,Ann,vt,vn,ut,un,Ttt,Ttn,Tnt,Tnn
!fd nosldt
   real(kind=8) :: bt
!fd
   real(kind=8) :: snmax

   type(T_interaction), pointer :: this, this_ikjl
   !
   character(len=24) :: IAM
   character(len=80) :: cout
   !      123456789012345678901234
   IAM = 'nlgs_new_int::solve_nlgs'

   RWR    = 0.d0
   bR     = 0.d0
   Enrg   = 0.d0
   WRRmin = 1.D24
   WRRmax = 0.d0

   sum_(1:inter_dim)  = 0.d0
   dsum_(1:inter_dim) = 0.d0

   if (nb_CDAN == 0) return

   if (i_what == i_iter) nlgs_loop = nlgs_loop + 1

   !$OMP PARALLEL DEFAULT(SHARED)                                                            &
   !$OMP PRIVATE(ik,ikk,istart,ikjl,iadj,iistart,                                            &
   !$OMP         rik,rliki,vlik,vlocfreeik,sstatusik,                                        &
   !$OMP         rlik,Wrlik,WWik,                                                            &
   !$OMP         vvlocfreeik,fricik,ibehav,ilaw,rrlik,vvlik,                                 &
   !$OMP         Wrliki,vliki,vik,                                                           &
   !$OMP         rloc,vl,Dvl,DVDV,DVDVRR,DVoR,WRR,                                           &
   !$OMP         Hradh_t,Hradh_n,is_cohesive,k_n,k_t,detJ,Att,Atn,Ant,Ann,vt,vn,ut,un,       &
   !$OMP         Ttt,Ttn,Tnt,Tnn,                                                            &
   !$OMP         modrl,alphaik)

   !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:SumDVDVRR,SumDVDV,SumWRR,SumWRWR,SumDVoR,RWR,bR,   &
   !$OMP                      Nnoact,Nactif,Nstick,Nslide,NOKsta,Nnoctc,WNslide,SNslide,     &
   !$OMP                      Ncompr,Ntract,dynstat,Nvnish,Nhover,Nb_RGR,WNstick,SNstick,    &
   !$OMP                      sum_,dsum_)                                                    &
   !$OMP                      REDUCTION(MAX:MaxDVDV,MAXDVDVRR,WRRmax) REDUCTION(MIN:WRRmin) 

   do ikk = 1, nb_CDAN

     ! Changing at random computational ordering of contact elements
     ik = IALEAT(ikk)
     
     this => get_interaction(ik)

     ! Computations for case i_what = 'i_iter' or case what = 'i_post ' or case what = 'i_check'
     rlik(1:inter_dim)       = 0.d0
     rliki(1:inter_dim)      = 0.d0
     vlik(1:inter_dim)       = 0.d0
     vliki(1:inter_dim)      = 0.d0
     Wrlik(1:inter_dim)      = 0.d0
     Wrliki(1:inter_dim)     = 0.d0
     vlocfreeik(1:inter_dim) = 0.d0

     if (this%forecast == 'noact') then
        
       vlik(1:inter_dim)       = this%vfree(1:inter_dim)
       vlocfreeik(1:inter_dim) = vlik(1:inter_dim)
       !mj sstatusik='noact'
       sstatusik='noctc'

     else if (this%forecast == 'acton') then
        
       if (i_what == i_iter) then
         !rm: pas le meme critere en 2D et 3D...
         !mj if (this(ik)%status == 'noctc') then
         if ( all(this%rl==0.d0) .and. inter_dim==2 .or. &
              this%status=='noctc' .and. inter_dim==3 ) then
           this%ivnish=this%ivnish+1
           if (this%ivnish < this%iskip*(this%iskip+1)/2) then
             cycle
           else
             this%iskip = this%iskip+1
           end if
         else
           this%ivnish = 0
           this%iskip  = 1
         end if
       else
         this%ivnish = 0
         this%iskip  = 1
       end if
       
       !fd a voir ....
       if (i_what == i_check) then
         this%ivnish = 0
         this%iskip  = 1
       end if

       !!! Computing vlocfree ***************************
       rlik(1:inter_dim)  = this%rl(1:inter_dim)
       Wrlik(1:inter_dim) = matmul(this%W(1:inter_dim,1:inter_dim),rlik(1:inter_dim))

       ! Computing          _______________________________ H* p* invM p H Rloc(ik)
       if(.not.SDLactif) then
         ! Computing                           ______________ H* p* invM p H Rloc
         call vitrad(this,iVaux_e_invM_t_Reac_)
         call prjj  (this,vik(1:inter_dim),iVaux_)
         vlik(1:inter_dim) = this%vfree(1:inter_dim) + vik(1:inter_dim)
         ! Computing_________________________________________ H* p* invM p H Rloc - H* p* invM p H Rloc(ik)     
         !                                                  = vlocfree(ik)
         vlocfreeik(1:inter_dim) = vlik(1:inter_dim) - Wrlik(1:inter_dim)
         !
       else
         ! Computing contribution of contacts jl
         vlocfreeik(1:inter_dim) = 0.d0
         istart = this%istart
         
         if (this%nbadj /= 0) then
           do iadj = 1, this%nbadj
             ikjl = this%adjjl(iadj)
             this_ikjl => get_interaction(ikjl)

             iistart = istart + inter_dim*inter_dim*(iadj-1)
             !\todo: rm: is written as a matmul... 
             vlocfreeik(1:inter_dim) = vlocfreeik(1:inter_dim) + matmul (reshape(Wab(iistart:iistart+inter_dim*inter_dim-1), &
                                                                         shape=(/inter_dim,inter_dim/) ), this_ikjl%rl(1:inter_dim))
           end do
         end if

         vlocfreeik(1:inter_dim) = vlocfreeik(1:inter_dim) + this%vfree(1:inter_dim)

         !fd ce signe est normal il vient des 2 constructions differentes
         vlik(1:inter_dim) = vlocfreeik(1:inter_dim) + Wrlik(1:inter_dim)

       end if

       !!! Convert to auxiliary ********************************************************
       WWik(1:inter_dim,1:inter_dim) = this%W(1:inter_dim,1:inter_dim)
       vvlocfreeik(1:inter_dim) = this%covfree(1:inter_dim) + vlocfreeik(1:inter_dim)

       fricik = this%fric
       ilaw   = this%i_law
       ibehav = this%lawnb

       call inter_laws(ilaw)%iter_func(this, rlik, vlik, vvlocfreeik, Wrlik, WWik, rliki, vliki, sstatusik)

       !if( inter_dim == 2 ) rliki(1:inter_dim) = rrlik(1:inter_dim)
       ! vltiki, vlniki, this(ik)%gapTT, are so far not to be used; computation is saved.

       ilaw   = this%i_law
       ibehav = this%lawnb

       if (i_what == i_iter) then
         dsum_(1:inter_dim) = dsum_(1:inter_dim) + dabs(rlik(1:inter_dim) - rliki(1:inter_dim))
          sum_(1:inter_dim) =  sum_(1:inter_dim) + dabs(rlik(1:inter_dim))
       end if

     end if ! end if (this(ik)%forecast == 'noact') else if (this(ik)%forecast == 'acton')
     !!!
     !!! Updating *********************************************
     !!!
     if (i_what /= i_check) then

       if (this%forecast == 'noact') then

         rliki(1:inter_dim)   = 0.d0
         this%rl(1:inter_dim) = rliki(1:inter_dim)

         this%status = sstatusik
         ! No further updating is necessary since noact candidates for contact are, by definition,
         ! assigned not to interfer with other candidates.

       else if (this%forecast == 'acton') then
       
         rliki(1:inter_dim) = RELAX*rliki(1:inter_dim)+RELAX1*rlik(1:inter_dim)
         
         where( dabs(rliki) .lt. 1.D-24 ) rliki = 0.d0

         this%rl(1:inter_dim) = rliki(1:inter_dim)
         this%status = sstatusik

         if( .not. SDLactif ) then
           ! Computing_________________________________________  R - H Rloc(ik) + H Rloc(ik)
           rik(1:inter_dim) = this%rl(1:inter_dim) - rlik(1:inter_dim) 
           ! Injecting difference between impulse reactions after and before going through the single
           ! contact solver
           call injj(this,rik(1:inter_dim),iReac_)
         end if
          
       end if
        
     else
        
       ! Updating is purposedly omitted while check, according to the definition of violations.
       ! Only rltiki, rlniki, (weighting with RELAX is also omitted), rltik, rlnik, sstatusik, 
       ! are used for computing violations.
       ! check is an apart process and does not interfer with iter process values. 
       if (norm_check) then
          RWR = RWR + dot_product(vlik(1:inter_dim),this%rl(1:inter_dim))
          bR  = bR  + dot_product(this%vfree(1:inter_dim)+this%covfree(1:inter_dim),this%rl(1:inter_dim))
       end if
     end if


     !!! end case i_what = 'i_iter' ********************************
     !!! Further computations for case i_what = 'i_post '***********
     if (i_what == i_post) then

       ! Rebuilding relative velocities, gap, status, with last known reaction values 
       Wrliki(1:inter_dim) = matmul(this%W(1:inter_dim,1:inter_dim),rliki(1:inter_dim))
       vliki(1:inter_dim)  = Wrliki(1:inter_dim) + vlocfreeik(1:inter_dim)
       
       where( dabs(vliki) .lt. 1.D-24 ) vliki = 0.d0
       this%vl(1:inter_dim) = vliki(1:inter_dim)

       this%gapTT = this%gapTTbegin + H*vliki(2)

       this%status = sstatusik
       if ( dabs(this%gapTT ) .lt. 1.D-24 ) this%gapTT = 0.d0

     end if

     !!! end case i_what = i_post ******************************************
     !!! Further computations for case i_what = i_check ********************

     if (i_what == i_check) then

       ! Computing discrepancies, violations, mean values
       if( i_checktype == i_QuadN ) then

         Wrliki(2) = this%W(2,2)*rliki(2)
         vliki(2)  = Wrliki(2)+vlocfreeik(2)

         if (dabs(vliki(2)) .lt. 1.D-24) vliki(2)=0.d0
         this%statuscheck=sstatusik

         rloc(2) = 0.5D0*(rlik(2)+rliki(2))
         vl(2)   = 0.5D0*(vlik(2)+vliki(2))      
         Dvl(2)  = vlik(2)-vliki(2)

         DVDV   = Dvl(2)*Dvl(2)
         DVDVRR = DVDV*(rloc(2)*rloc(2))
         DVoR   = rloc(2)*Dvl(2)

         SumDVDV   = SumDVDV+DVDV
         MaxDVDV   = dmax1(MaxDVDV,DVDV)
         SumDVDVRR = SumDVDVRR+DVDVRR
         MaxDVDVRR = dmax1(MaxDVDVRR,DVDVRR)
         SumDVoR   = SumDVoR+DVoR

         WRR    = 0.5D0*(this%W(2,2)*rlik(2)*rlik(2)+Wrliki(2)*rliki(2))
         
         SumWRR = SumWRR+WRR
         SumWRWR= SumWRWR+this%W(2,2)*WRR

       else
         ! Rebuilding relative velocities, gap, status, with last known reaction values 
         Wrliki(1:inter_dim) = matmul(this%W(1:inter_dim,1:inter_dim),rliki(1:inter_dim))
         vliki(1:inter_dim)  = Wrliki(1:inter_dim) + vlocfreeik(1:inter_dim)
         where( dabs(vliki) .lt. 1.D-24) vliki=0.d0

         this%statuscheck=sstatusik

         ! According to above computations
         rloc(1:inter_dim) = 0.5D0*(rlik(1:inter_dim)+rliki(1:inter_dim)) 
         vl(1:inter_dim)   = 0.5D0*(vlik(1:inter_dim)+vliki(1:inter_dim)) 

         Dvl(1:inter_dim) = vlik(1:inter_dim)-vliki(1:inter_dim)
         DVDV   = dot_product(Dvl(1:inter_dim),Dvl(1:inter_dim))
         modrl  = dot_product(rloc(1:inter_dim),rloc(1:inter_dim))
         DVDVRR = DVDV*modrl
         modrl = SQRT(modrl)

         DVor  = dot_product(rloc(1:inter_dim),Dvl(1:inter_dim))

         SumDVDV   = SumDVDV + DVDV
         MaxDVDV   = DMAX1(MaxDVDV,DVDV)
         SumDVDVRR = SumDVDVRR + DVDVRR
         MaxDVDVRR = DMAX1(MaxDVDVRR,DVDVRR)

         SumDVoR = SumDVoR + DVoR

         WRR = 0.5*( dot_product(Wrlik(1:inter_dim),rlik(1:inter_dim))  &
                    +dot_product(Wrliki(1:inter_dim),rliki(1:inter_dim))&
                   )

         SumWRR  = SumWRR + WRR
         SumWRWR = SumWRWR + 0.5D0*( dot_product(Wrlik(1:inter_dim),Wrlik(1:inter_dim))  &
                                    +dot_product(Wrliki(1:inter_dim),Wrliki(1:inter_dim))&
                                   )
         !!!mr .... faudrait pas définir un mot clés afin de ne pas avoir à faire ces opérations
         !!!        tout le temps?!
         !dynstat = dynstat & 
         !        + dabs( ( this(ik)%Wnn*(vltiki-this(ik)%vltBEGIN)-this(ik)%Wtn*(vlniki-this(ik)%vlnBEGIN)) &
         !        * (vltiki+this(ik)%vltBEGIN)/this(ik)%det &
         !        + (-this(ik)%Wnt*(vltiki-this(ik)%vltBEGIN)+this(ik)%Wtt*(vlniki-this(ik)%vlnBEGIN)) &
         !        * (vlniki+this(ik)%vlnBEGIN)/this(ik)%det)
       end if

       ! Arrays for fine violation analysis
       Xvlton(ik)   = DVDV  ! Xvlton(ik)=DVDVRR
       WRRarray(ik) = WRR 

       this%statuscheck = sstatusik

       if( this%forecast == 'noact' ) Nnoact = Nnoact+1

       if( rloc(2) > 0.d0 ) then
          Ncompr=Ncompr+1
       else if( rloc(2) < 0.d0 ) then
          Ntract=Ntract+1
       end if

       if( modrl == 0.d0 ) then
          Nvnish = Nvnish+1
       end if
       Nactif = Ncompr + Ntract
       
       if (this%statuscheck == 'noctc'  .or. &
           this%statuscheck == 'Wnctc'  .or. &
           this%statuscheck == 'Mnctc') then
         Nhover = Nhover + 1 
       end if
       
       if (this%statuscheck == 'stick'  .OR. &
           this%statuscheck == 'Wstck'  .OR. & 
           this%statuscheck == 'Mstck'  .OR. &
           this%statuscheck == 'Cstck') THEN   
         Nstick = Nstick + 1
         if (this%ws == 0) then
            WNstick = WNstick + 1
          ELSE
            SNstick = SNstick + 1
          END  IF
       end if
         
       if(this%statuscheck == 'slifw' .or. this%statuscheck == 'slibw' .or. &
          this%statuscheck == 'Wslfw' .or. this%statuscheck == 'Wslbw' .or. &
          this%statuscheck == 'Mslfw' .or. this%statuscheck == 'Mslbw' .or. &
          this%statuscheck == 'Cslfw' .or. this%statuscheck == 'Cslbw' .or. &
          this%statuscheck == 'slide' .or. this%statuscheck == 'Wslid' .or. &
          this%statuscheck == 'Cslid' ) then
         Nslide=Nslide+1
         if (this%ws == 0) then
           WNslide = WNslide + 1
         else
           SNslide = SNslide + 1
         end if
       end if
         
       if( this%status /= this%statuscheck ) then
         NOKsta = NOKsta + 1
         if (this%ws == 0) then
           NOKweak = NOKweak + 1
         else
           NOKstrg = NOKstrg + 1
         end if
       end if
       
       if (this%statuscheck == '_RGR_') Nb_RGR = Nb_RGR+1
       
       !rm: du coup... mj ? ou le meme que dans nlgs_3D ?
       ! Fine violation analysis
       !mj       if (this(ik)%statuscheck .ne. 'noctc') then
       if (modrl .ne. 0.d0) then !mj
          WRRmin = dmin1(WRRmin,WRRarray(ik))
          WRRmax = dmax1(WRRmax,WRRarray(ik))
          if (QuadWR == 0.d0) then
             Xvlton(ik) = dsqrt(Xvlton(ik))
          ELSE  
             Xvlton(ik) = dsqrt(Xvlton(ik))/(QuadWR*tol)
          end if
          ! The violation has the form (sqrt(DVDV)/sqrt(WRWR))/tol
       end if
         
       call put_violation(this%cdan, this%icdan, Xvlton(ik))
         
     end if

   end do
   !$OMP END DO
   !$OMP END PARALLEL   

   !!! Summarize rough violation analysis ***********************************
   if (i_what == i_check) then
      
      Nactif = nb_CDAN-Nvnish
      ! compute quantities used to check the convergence (QuadDV, MaxmDV, QuadDVR, MaxmDVR and MeanDVoR)
      call compute_convergence_norms_nlgs(Nactif, SumWRWR, SumDVDV, MaxDVDV, &
                                          SumWRR, SumDVDVRR, MaxDVDVRR, SumDVoR, tol, &
                                          QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR)

      ! compute more check quantities
      if (Nactif .ge. 1 .and. SumWRR .gt. 1.D-18 .and. SumWRWR .gt. 1.D-18) then   
         QuadWR   = dsqrt(SumWRWR/real(Nactif,8))
         Dreac    = QuadWR*H  
         
         MeanWRR  = SumWRR/real(Nactif,8)
         rcvltm = - MeanDVoR*tol  
         dynstat  = 0.5D0*dynstat/SumWRR
      else   
         QuadWR   = 0.111D-11 
         Dreac    = 0.111D-11 
         MeanWRR  = 0.111D-11
         rcvltm =   0.000D+00 
         dynstat  = 0.111D-11
      end if
      Enrg = 0.5*RWR + bR
   end if
   
 end subroutine solve_nlgs

 !> \brief Compute convergence norms
 subroutine compute_convergence_norms_nlgs(Nactif_, SumWRWR_, SumDVDV_, MaxDVDV_, &
                                           SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_, &
                                           QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_)
    implicit none
    !> 
    integer(kind=4), intent(in) :: Nactif_
    !> 
    real(kind=8)   , intent(in) :: SumWRWR_
    !> 
    real(kind=8)   , intent(in) :: SumDVDV_
    !> 
    real(kind=8)   , intent(in) :: MaxDVDV_
    !> 
    real(kind=8)   , intent(in) :: SumWRR_
    !> 
    real(kind=8)   , intent(in) :: SumDVDVRR_
    !> 
    real(kind=8)   , intent(in) :: MaxDVDVRR_
    !> 
    real(kind=8)   , intent(in) :: SumDVoR_
    !> 
    real(kind=8)   , intent(in) :: tol_
    !>
    real(kind=8), intent(out) :: QuadDV_
    !>
    real(kind=8), intent(out) :: MaxmDV_
    !>
    real(kind=8), intent(out) :: QuadDVR_
    !>
    real(kind=8), intent(out) :: MaxmDVR_
    !>
    real(kind=8), intent(out) :: MeanDVoR_
    ! locals
    real(kind=8) :: QuadWR_, MeanWRR_

    if (Nactif_ >= 1 .and. SumWRR_ > 1.d-18 .and. SumWRWR_ > 1.d-18) then
       QuadWR_   = dsqrt(SumWRWR_/real(Nactif_,8))
       QuadDV_   = dsqrt(SumDVDV_/real(Nactif_,8)) / (QuadWR_*tol_)
       MaxmDV_   = dsqrt(MaxDVDV_)                 / (QuadWR_*tol_)

       MeanWRR_  = SumWRR_/real(Nactif_,8)
       QuadDVR_  = dsqrt(SumDVDVRR_/real(Nactif_,8)) / (MeanWRR_*tol_)
       MaxmDVR_  = dsqrt(MaxDVDVRR_)                 / (MeanWRR_*tol_)
       MeanDVoR_ = SumDVoR_                          / (SumWRR_*tol_)
    else
       QuadDV_   = 0.111D-11
       MaxmDV_   = 0.111D-11
       
       QuadDVR_  = 0.111D-11
       MaxmDVR_  = 0.111D-11
       MeanDVoR_ = 0.111D-11
    end if

 end subroutine compute_convergence_norms_nlgs

!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 !> \brief 
 subroutine compute_local_free_vlocy(list_INTRF)
   implicit none
   !>
   integer, dimension(:), intent(in), optional :: list_INTRF
   !
   integer(kind=4) :: i,ik
   type(T_interaction), pointer :: this

   if (.not. present(list_INTRF)) then
      if (nb_CDAN == 0) return

      do ik = 1,nb_CDAN
        this => get_interaction(ik)
        call prjj(this,this%vfree(1:inter_dim),iVfree)
      end do
   else
      do i = 1, size(list_INTRF)
        ik=list_INTRF(i)
        this => get_interaction(ik)
        call prjj(this,this%vfree(1:inter_dim),iVfree)
      end do
   end if

 end subroutine compute_local_free_vlocy
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 !> \brief Nullify  reaction of bodies involved in an interaction
 subroutine nullify_reac(this,storage)
   implicit none
   !> interaction
   type(T_interaction), intent(in) :: this
   !> what reaction to nullify
   integer(kind=4)    , intent(in) :: storage
   !
   character(len=27) :: IAM
   character(len=80) :: cout
   !      123456789012345678901234567
   IAM = 'nlgs_new_int::nullify_reac'
  
   select case( this%icdtyp )
   case( i_diskx, i_xksid, i_polyg, i_joncx, i_pt2dx)
     call nullify_reac_rbdy2(this%icdbdy, storage)
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call nullify_reac_rbdy3(this%icdbdy, storage)
   case( i_clxxx, i_diskl, i_pt2dl, i_csxxx )
     call nullify_reac_mecaMAILx(this%icdbdy, storage)
   case default
      write(cout,'(I5,A31)') this%icdtyp ,' is not implemented'
      call faterr(IAM,cout)
   end select
     
   select case( this%iantyp )
   case( i_diskx, i_xksid, i_polyg, i_joncx, i_pt2dx)
     call nullify_reac_rbdy2(this%ianbdy, storage)
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call nullify_reac_rbdy3(this%ianbdy, storage)
   case( i_alpxx, i_diskl, i_pt2dl, i_aspxx )
     call nullify_reac_mecaMAILx(this%ianbdy, storage)
   case default
      write(cout,'(I5,A31)') this%iantyp,' is not implemented'
      call faterr(IAM,cout)
   end select

 end subroutine nullify_reac
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 !> \brief Compute velocity of bodies involved in an interaction using a specific right hand side
 subroutine vitrad(this,storage)
   implicit none
   !> interaction
   type(T_interaction), intent(in) :: this
   !> what right hand side to use and where to store result
   integer(kind=4)    , intent(in) :: storage
   !
   character(len=20) :: IAM
   character(len=80) :: cout
   !      12345678901234567890
   IAM = 'nlgs_new_int::vitrad'
   
   select case( this%icdtyp )
   case( i_diskx, i_xksid, i_polyg, i_joncx, i_pt2dx)
     call comp_vlocy_rbdy2(this%icdbdy, storage)
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call comp_vlocy_rbdy3(this%icdbdy, storage)
   case( i_clxxx, i_diskl, i_pt2dl, i_csxxx )
     call comp_vlocy_mecaMAILx(this%icdbdy, storage)
   case default
      write(cout,'(I5,A31)') this%icdtyp ,' is not implemented'
      call faterr(IAM,cout)
   end select
     
   select case( this%iantyp )
   case( i_diskx, i_xksid, i_polyg, i_joncx, i_pt2dx)
     call comp_vlocy_rbdy2(this%ianbdy, storage)
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call comp_vlocy_rbdy3(this%ianbdy, storage)
   case( i_alpxx, i_diskl, i_pt2dl, i_aspxx )
     call comp_vlocy_mecaMAILx(this%ianbdy, storage)
   case default
      write(cout,'(I5,A31)') this%iantyp,' is not implemented'
      call faterr(IAM,cout)
   end select

 end subroutine vitrad
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 !> \brief Nullify velocity of bodies involved in an interaction
 subroutine nullify_vlocy(this,storage)
   implicit none
   !> interaction
   type(T_interaction), intent(in) :: this
   !> velocity type to nullify
   integer(kind=4)    , intent(in) :: storage
   !
   character(len=27) :: IAM
   character(len=80) :: cout
   !      123456789012345678901234567
   IAM = 'nlgs_newarch::nullify_vlocy'
  
   select case( this%icdtyp )
   case( i_diskx, i_xksid, i_polyg, i_joncx, i_pt2dx)
     call nullify_vlocy_rbdy2(this%icdbdy, storage)
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call nullify_vlocy_rbdy3(this%icdbdy, storage)
   case( i_clxxx, i_diskl, i_pt2dl, i_csxxx )
     call nullify_vlocy_mecaMAILx(this%icdbdy, storage)
   case default
      write(cout,'(I5,A31)') this%icdtyp ,' is not implemented'
      call faterr(IAM,cout)
   end select
     
   select case( this%iantyp )
   case( i_diskx, i_xksid, i_polyg, i_joncx, i_pt2dx)
     call nullify_vlocy_rbdy2(this%ianbdy, storage)
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call nullify_vlocy_rbdy3(this%ianbdy, storage)
   case( i_alpxx, i_diskl, i_pt2dl, i_aspxx )
     call nullify_vlocy_mecaMAILx(this%ianbdy, storage)
   case default
      write(cout,'(I5,A31)') this%iantyp,' is not implemented'
      call faterr(IAM,cout)
   end select

 end subroutine nullify_vlocy

 !------------------------------------------------------------------------
 !> \brief Display an interaction
 subroutine print_info(ik)
   implicit none
   integer(kind=4)   :: ik
   character(len=24) :: IAM
   character(len=80) :: cout
   !      123456789012345678901234
   IAM = 'nlgs_new_int::print_info'
  
   !call display_interaction(ik)
   
 end subroutine print_info
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------  
 !> \brief free entity list
 subroutine Nullify_EntityList_nlgs
   implicit none

   call Free_EntityList

 end subroutine Nullify_EntityList_nlgs
 !------------------------------------------------------------------------  
 !> \brief Get information concerning the loops of NLGS
 subroutine get_nlgs_loop(compteur,err1,err2,err3,contact)
   implicit none
   !> number of iteration done
   integer(kind=4), intent(out) :: compteur
   !> number of contacts
   integer(kind=4), intent(out) :: contact
   !> convergence on 
   real(kind=8), intent(out) :: err1
   !> convergence on 
   real(kind=8), intent(out) :: err2
   !> convergence on 
   real(kind=8), intent(out) :: err3

   compteur = nlgs_loop
   contact  = nb_CDAN
   err1     = MeanDVoR
   err2     = QuadDV
   err3     = QuadDVR

 end subroutine get_nlgs_loop
 !------------------------------------------------------------------------  
 !> \brief ??
 subroutine get_nlgs_network_change(nctc,nweak,nstrong)
   implicit none
   !>
   integer(kind=4), intent(out) :: nctc
   !>
   integer(kind=4), intent(out) :: nweak
   !>
   integer(kind=4), intent(out) :: nstrong

   nctc    = NOKsta
   nweak   = NOKweak
   nstrong = NOKstrg

 end subroutine get_nlgs_network_change
 !------------------------------------------------------------------------  
 !> \brief ??
 subroutine get_nlgs_contact_status(noctc,Wslide,Sslide,Wstick,Sstick)
   implicit none
   !>
   integer(kind=4), intent(out) :: noctc
   !>
   integer(kind=4), intent(out) :: Wslide
   !>
   integer(kind=4), intent(out) :: Sslide
   !>
   integer(kind=4), intent(out) :: Sstick
   !>
   integer(kind=4), intent(out) :: Wstick
   
   noctc  = Nhover !fd obsolete Nnoctc
   Wslide = WNslide
   Sslide = SNslide
   Sstick = SNstick
   Wstick = WNstick

 end subroutine get_nlgs_contact_status
 !------------------------------------------------------------------------  
 !------------------------------------------------------------------------  
 !> \brief ??
 subroutine get_after_iter_check(ddynstat,nnb_CDAN,NNnoact,NNvnish,NNhover,NNcompr,NNtract,NNslide,NNstick,NNOKsta,NNb_RGR)
   implicit none
   !>
   integer(kind=4), intent(out) :: nnb_CDAN
   !>
   integer(kind=4), intent(out) :: NNnoact
   !>
   integer(kind=4), intent(out) :: NNvnish
   !>
   integer(kind=4), intent(out) :: NNhover
   !>
   integer(kind=4), intent(out) :: NNcompr
   !>
   integer(kind=4), intent(out) :: NNtract
   !>
   integer(kind=4), intent(out) :: NNslide
   !>
   integer(kind=4), intent(out) :: NNstick
   !>
   integer(kind=4), intent(out) :: NNOKsta
   !>
   integer(kind=4), intent(out) :: NNb_RGR
   !>
   real(kind=8)   , intent(out) :: ddynstat

   ddynstat=dynstat
   nnb_CDAN=nb_CDAN
   NNnoact=Nnoact
   NNvnish=Nvnish
   NNhover=NNhover
   NNcompr=Ncompr
   NNtract=Ntract
   NNslide=Nslide
   NNstick=Nstick
   NNOKsta=NOKsta
   NNb_RGR=Nb_RGR
  
 end subroutine get_after_iter_check  
 !------------------------------------------------------------------------  
 !------------------------------------------------------------------------  
 !> \brief ??
 subroutine get_somme_rn(ssomme_rn)
   implicit none
   !>
   real(kind=8) :: ssomme_rn
   !
   integer(kind=4) :: ik
   real(kind=8)    :: somme_rn
   type(T_interaction), pointer :: this

   somme_rn = 0.d0
   do ik = 1, nb_CDAN
     this => get_interaction(ik)
     somme_rn = somme_rn+this%rl(2)
   end do
   ssomme_rn = somme_rn/H
  
 end subroutine get_somme_rn  
 !------------------------------------------------------------------------  
 !------------------------------------------------------------------------  
 !> \brief ??
 subroutine update_internal(this)
   implicit none
   !>
   type(T_interaction) :: this
   !
   integer(kind=4)   :: ibehav,ilaw
   character(len=28) :: IAM
   character(len=80) :: cout
          !1234567890123456789012345678
   IAM = 'nlgs_new_int::update_internal'

   ilaw   = this%i_law 

   if( associated(inter_laws(ilaw)%post_func) ) &
     call inter_laws(ilaw)%post_func(this)

 end subroutine update_internal
 !!!---------------------------------------------------------------
 !> \brief ??
 subroutine scale_rloc_nlgs
   implicit none    
   integer(kind=4) :: ik
   type(T_interaction), pointer :: this

   if (nb_CDAN == 0) return

   Scale = dmin1(Scale,1.1d0)
   Scale = dmax1(Scale,0.9d0)      

   ! Rnod = [H] Rloc
   do ik = 1, nb_CDAN 
     this => get_interaction(ik)
     this%rl(1:inter_dim) = this%rl(1:inter_dim)*Scale
     call nullify_reac(this,iReac_)
   end do
   do ik = 1, nb_CDAN
     this => get_interaction(ik)
     call injj(this,this%rl(1:inter_dim),iReac_)
   end do

 end subroutine scale_rloc_nlgs
 !!!---------------------------------------------------------------
 !> \brief ??
 subroutine reverse_nlgs
   implicit none    
   integer(kind=4) :: ik

   if (nb_CDAN == 0) return

   do ik = 1, nb_CDAN
     ialeatr(ik) = ialeat(ik)
   end do
   do ik = 1, nb_CDAN
     ialeat(ik) = nb_CDAN-ialeatr(ik)+1
   end do

 end subroutine reverse_nlgs
 !!!---------------------------------------------------------------
 !> \brief ??
 subroutine bimodal_list_nlgs
   implicit none
   integer(kind=4) :: ik

   if (nb_CDAN == 0) return

   do ik = 1, nb_CDAN
     ialeat(ik) = iwksg(ik)
   end do

 end subroutine bimodal_list_nlgs
 !!!---------------------------------------------------------------
 !> \brief ??
 subroutine RnodHRloc_nlgs(list_INTRF, storage_reac)
   implicit none
   !> (optional) list of contact to work on
   integer(kind=4), dimension(:), intent(in), optional :: list_INTRF
   !> (optional) type of reaction to work with
   integer(kind=4), intent(in), optional :: storage_reac
   ! locals
   integer(kind=4) :: i,ik
   integer(kind=4) :: storage
   type(T_interaction), pointer :: this

   ! the reaction torque will be stored in Reac...
   storage = iReac_

   ! ... unless the user choose another location
   if (present(storage_reac)) storage = storage_reac

   if (.not. present(list_INTRF)) then
      if (nb_CDAN == 0) return
   
      do ik = 1, nb_CDAN  
        this => get_interaction(ik)
        call nullify_reac(this, storage)
      end do
      do ik = 1, nb_CDAN
        this => get_interaction(ik)
        call injj(this, this%rl(1:inter_dim), storage)
      end do
   else
      do i = 1, size(list_INTRF)
        ik = list_INTRF(i)
        this => get_interaction(ik)
        call nullify_reac(this, storage)
      end do
      do i = 1, size(list_INTRF)
        ik = list_INTRF(i)
        this => get_interaction(ik)
        call injj(this, this%rl(1:inter_dim), storage)
      end do
   end if

 end subroutine RnodHRloc_nlgs
 !!!---------------------------------------------------------------
 !!!---------------------------------------------------------------
 !> \brief Set parameters value to use during NLGS resolution
 subroutine set_nlgs_parameter(normtype,tolerence,relaxation)
   implicit none
   !> Type of norm to use for convergence check
   character(len=5), intent(in)  :: normtype
   !> Convergence criterion
   real(kind=8), intent(in) :: tolerence
   !> Relaxation parameter
   real(kind=8), intent(in) :: relaxation

   tol    = tolerence
   RELAX  = relaxation
   RELAX1 = 1.d0-RELAX

   select case(normtype)
   case('QuaN ')
      i_checktype = i_QuadN
   case('Quad ')
      i_checktype = i_Quad
   case('Maxm ')
      i_checktype = i_Maxm
   case('QM/16')
      i_checktype = i_QMs16
   case DEFAULT
      call logmes(normtype)
      call faterr('nlgs_new_int::set_nlgs_parameter','unknown norm type')
   end select

 end subroutine set_nlgs_parameter
 !!!---------------------------------------------------------------
 !> \brief ??
 subroutine prep_check_nlgs(iconv)
   implicit none
   !> 
   integer(kind=4), intent(out) :: iconv
   !
   integer(kind=4) :: ik
    
   iconv = 0
   conv_contact = .true.
   if (nb_CDAN == 0) return

   iconv = 1
   conv_contact = .false.
   
   call RnodHRloc_nlgs

   Dcrac  = 1.d0 !distance caracteristique

   SumDVDV  = 0.d0
   MaxDVDV  = 0.d0
   SumDVDVRR= 0.d0
   MaxDVDVRR= 0.d0
   SumWRWR  = 0.d0
   SumWRR   = 0.d0
   SumDVoR  = 0.d0
   dynstat  = 0.d0
   
   Dreac  = 0.d0 ! "reacteristic" distance 
   Nnoact = 0    ! number of contacts being forecasted inactive 
   Nactif = 0    ! number of active contacts (contacts where the normal reaction is not vanishing)
   
   Nvnish = 0    ! number of candidates with vanishing reactions
   Nnoctc = 0    ! number of no contacts, obsolete!
   Nhover = 0    ! number of hovering contacts (separated contacts active or not)
   
   Nslide = 0    ! number of sliding contacts
   Nstick = 0    ! number of sticking contacts
   Ncompr = 0    ! number of compressing contacts
   Ntract = 0    ! number of tensile contacts
   NOKsta = 0    ! number of questionable status
   Nb_RGR = 0    ! number of contacts where the "Radjai Gap Rescue" is active
   
   ! mj & fd -> comments ?
   WNslide = 0
   WNstick = 0
   SNstick = 0
   SNslide = 0
   NOKweak = 0
   NOKstrg = 0

   ! nbCDAN-Nnoact-Nnoctc = Nactif = Ncomp+Ntract = Nslide+Nstick+Nb_RGR

 end subroutine prep_check_nlgs
 !!!-------------------------------------------------------------------------------------
 subroutine comp_check_nlgs(iconv)
   implicit none
   !>
   integer(kind=4), intent(out) :: iconv
   ! locals
   logical :: converged

   iconv = 1
   
   !am: check convergence using stored quantities 
   call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, converged)

   conv_contact = converged

   if (converged) iconv = 0

   if (i_checktype == i_Maxm .and. converged) then
     QuadDV  = MaxmDV
     QuadDVR = MaxmDVR
   end if

   Scale = 1.d0+rcvltm

 end subroutine comp_check_nlgs

 !> \brief Check convergence using given quantities against stored tolerance
 subroutine check_convergence_nlgs(QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_, converged)
   implicit none
   !> velocity quad norm value
   real(kind=8), intent(in) :: QuadDV_
   !> velocity max norm value
   real(kind=8), intent(in) :: MaxmDV_
   !> dv quad norm value
   real(kind=8), intent(in) :: QuadDVR_
   !> dv max norm value
   real(kind=8), intent(in) :: MaxmDVR_
   !> mean dv something norm value
   real(kind=8), intent(in) :: MeanDVoR_
   !> is converged
   logical, intent(out) :: converged

   converged = .false.

   select case(i_checktype)
   case(i_Quad, i_QuadN)
     if ( dabs(MeanDVoR_) .lt. 1.d0 .and. &
          dabs(QuadDV_)   .lt. 1.d0 .and. &
          dabs(QuadDVR_)  .lt. 1.d0) then
       converged = .true.
     end if
   case(i_Maxm)
     if ( dabs(MeanDVoR_) .lt. 1.d0 .and. &
          dabs(MaxmDV_)   .lt. 1.d0 .and. &
          dabs(MaxmDVR_)  .lt. 1.d0) then
       converged = .true.
     end if
   case(i_QMs16)
     if ( dabs(MeanDVoR_) .lt. 1.d0 .and. &
          dabs(QuadDV_)   .lt. 1.d0 .and. &
          dabs(QuadDVR_)  .lt. 1.d0 .and. &
          dabs(MaxmDV_)   .lt. 16.666d0 .and. &
          dabs(MaxmDVR_)  .lt. 16.666d0) then
       converged = .true.
     end if
   end select

 end subroutine check_convergence_nlgs

 !!!------------------------------------------------------------------
 !> \brief Display convergence values
 subroutine display_check_nlgs
   implicit none
   character(len=103) :: cout 

   if (nb_CDAN == 0) return
       
   call logmes(' ')
   select case(i_checktype)
   case(i_Quad,i_QuadN)
      write(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Quad ','     Maxm'
      call logmes(cout)
      write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
      call logmes(cout)
      write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
      call logmes(cout)
   case(i_Maxm)
      write(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Maxm ','     Maxm'
      call logmes(cout)
      write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
      call logmes(cout)
      write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
      call logmes(cout)
   case(i_QMs16)
      write(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  QM/16','1/16 Maxm'
      call logmes(cout)
      write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV *0.06D0 
      call logmes(cout)
      write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR*0.06D0 
      call logmes(cout)
   end select
   
   write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','MeanDVoR/SumWRR  =',MeanDVoR,'Free run length  =',Dreac 
   call logmes(cout)
   write(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','dynamic/static   =',dynstat
   call logmes(cout)
   write(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Nvnish =',Nvnish,'  nbCDAN =',nb_CDAN, 'Nhover =',Nhover
   call logmes(cout)
   write(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ncompr =',Ncompr,'  Nnoact =', Nnoact, 'Nslide =',Nslide
   call logmes(cout)
   write(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ntract =',Ntract,'  NOKsta =',NOKsta , 'Nstick =',Nstick 
   call logmes(cout)
   write(cout,'(1X,A3,23X,A10,I10)')                      ' @ ',                   '  Nb_RGR =',Nb_RGR 
   call logmes(cout)
   call logmes('  ')

 end subroutine display_check_nlgs
 !!!---------------------------------------------------------------------
 !> \brief Display sum of RlocN (whatever that is)
 subroutine display_rlocn_sum_nlgs
   implicit none
   integer(kind=4) :: ik
   type(T_interaction), pointer :: this
    
   somme_rn = 0.d0
   do ik = 1, nb_CDAN
     this => get_interaction(ik)
     somme_rn = somme_rn+this%rl(2)
   end do
   write(6,'(A11,D14.7)') 'RlocN SUM: ', somme_rn/H

 end subroutine display_rlocn_sum_nlgs
 !!!---------------------------------------------------------------------
 !> \brief Update internal value of contact
 subroutine update_tact_behav_nlgs
   implicit none
   integer(kind=4) :: ik
   type(T_interaction), pointer :: this

   if (nb_CDAN == 0) return
   
   do ik = 1, nb_CDAN
     this => get_interaction(ik)
     call update_internal(this)
   end do

 end subroutine update_tact_behav_nlgs
 !!!---------------------------------------------------------------------
 !> \brief Write convergence value to a file
 subroutine write_norm_check_nlgs(istat)
   implicit none
   integer(kind=4), intent(in) :: istat

   select case(istat)
   case(1)
      norm_check = .true.
      norm_fich  = get_io_unit() 
      open(unit=norm_fich,file=trim(location('NORM_NLGS.DAT')),status='REPLACE')
   case(2)
     if (norm_check) then
       select case(i_checktype)
       case(i_Quad,i_QuadN)
         write(norm_fich,'(3(2X,D14.7),3(1X,I8))') abs(meanDVoR),QuadDVR,QuadDV,NOKsta,NOKweak,NOKstrg
       case(i_Maxm)
         write(norm_fich,'(3(2X,D14.7),3(1X,I8))') abs(meanDVoR),MaxmDVR,MaxmDV,NOKsta,NOKweak,NOKstrg
       end select
     end if
   case(3)
     if (norm_check) close(norm_fich)
   end select

 end subroutine write_norm_check_nlgs
 !!!------------------------------------------------------------------------
 !> \brief Scrambles the ordering of candidates for contact
 subroutine scramble_nlgs
   implicit none
   integer(kind=4) :: ik
   integer(kind=4) :: IALEATik,IAL1,IAL2
   real(kind=8)    :: RA

   do ik = 1, nb_CDAN/2
      
     call random_number(RA)
     IAL1 = idint(RA*real(nb_CDAN,8))+1
     IAL1 = min0(IAL1,nb_CDAN)
     IAL1 = max0(1,IAL1)
     
     call random_number(RA)
     IAL2 = idint(RA*real(nb_CDAN,8))+1
     IAL2 = min0(IAL2,nb_CDAN)
     IAL2 = max0(1,IAL2)
     
     IALEATik     = IALEAT(IAL1)
     IALEAT(IAL1) = IALEAT(IAL2)
     IALEAT(IAL2) = IALEATik
      
   end do
   
 end subroutine scramble_nlgs
 !!!mj---------------------------------------------------------------------- 
 !> \brief Scrambles the ordering of candidates for contact
 subroutine quick_scramble_nlgs
   implicit none
   integer(kind=4) :: ik,IALEATik

   do ik = 1, nb_CDAN
     IALEATik   = IALEAT(ik)
     IALEAT(ik) = IALEAT(randomlist(ik))
     IALEAT(randomlist(ik)) = IALEATik
   end do
   
 end subroutine quick_scramble_nlgs
 !!!mr------------------------------------------------------------------------
 !!!------------------------------------------------------------------------
 !> \brief ???????
 subroutine update_friction_coefficient(fric)
   implicit none
   real(kind=8), intent(inout) :: fric
   !
   integer(kind=4) :: icdan
   real(kind=8)    :: thickness

   do icdan = 1, nb_CDAN
     !CALL read_friction_map(fric,thickness)
   end do

 end subroutine update_friction_coefficient
 !!!------------------------------------------------------------------------
 !> \brief Get all, or a part of, summations used to compute the quantities required to check the convergence
 !> all outpout variables are optional in order to allow the user to define which variables he want to collect
 subroutine get_error(SumDVDVRR_, Nactif_, MeanWRR_, &
                      tol_, SumDVDV_, QuadWR_, SumDVoR_, &
                      SumWRR_, SumWRWR_, MaxDVDV_, MaxDVDVRR_) 
   implicit none
   !>
   integer(kind=4), intent(out), optional :: Nactif_
   !>
   real(kind=8), intent(out), optional :: SumDVDVRR_
   !>
   real(kind=8), intent(out), optional :: MeanWRR_
   !>
   real(kind=8), intent(out), optional :: tol_
   !>
   real(kind=8), intent(out), optional :: SumDVDV_
   !>
   real(kind=8), intent(out), optional :: QuadWR_
   !>
   real(kind=8), intent(out), optional :: SumDVoR_
   !>
   real(kind=8), intent(out), optional :: SumWRR_
   !>
   real(kind=8), intent(out), optional :: SumWRWR_
   !>
   real(kind=8), intent(out), optional :: MaxDVDV_
   !>
   real(kind=8), intent(out), optional :: MaxDVDVRR_
                                          
   ! only the asked values are returned 
   ! \todo : initialize output values ?
   if (present(SumDVDVRR_)) SumDVDVRR_ = SumDVDVRR
   if (present(Nactif_))    Nactif_    = Nactif
   if (present(MeanWRR_))   MeanWRR_   = MeanWRR
   if (present(tol_))       tol_       = tol
   if (present(SumDVDV_))   SumDVDV_   = SumDVDV
   if (present(QuadWR_))    QuadWR_    = QuadWR
   if (present(SumDVoR_))   SumDVoR_   = SumDVoR
   if (present(SumWRR_))    SumWRR_    = SumWRR
   if (present(SumWRWR_))   SumWRWR_   = SumWRWR
   if (present(MaxDVDV_))   MaxDVDV_   = MaxDVDV
   if (present(MaxDVDVRR_)) MaxDVDVRR_ = MaxDVDVRR
 
 end subroutine get_error
 !!!------------------------------------------------------------------------
 !> \brief ??
 subroutine get_conv(my_sum_t,my_sum_n,my_dsum_t,my_dsum_n)
   implicit none
   !>
   real(kind=8), intent(out) :: my_sum_t
   !>
   real(kind=8), intent(out) :: my_sum_n
   !>
   real(kind=8), intent(out) :: my_dsum_t
   !>
   real(kind=8), intent(out) :: my_dsum_n

   my_sum_t  = sum_(1)
   my_sum_n  = sum_(2)
   my_dsum_t = dsum_(1)
   my_dsum_n = dsum_(2)
   !my_sum_s  = sum_(3)
   !my_dsum_s = dsum_(3)

 end subroutine

 !> \brief Activate diagonal resolution
 subroutine active_diagonal_resolution
   implicit none

   diagonal_resolution = .true.

 end subroutine active_diagonal_resolution
   
 !> \brief Get all interactions reaction and velocity in on flat array
 function get_all_this()
   implicit none
   !> 
   real(kind=8), dimension(:,:), pointer :: get_all_this
   !
   type(T_interaction), pointer :: this
   integer(kind=4) :: i_cdan, n

   get_all_this => null()

   if( nb_cdan <= 0 ) return

   allocate( get_all_this(10,nb_cdan) )

   do i_cdan = 1, nb_cdan

     this => get_interaction(i_cdan)

     get_all_this(         1:  inter_dim,i_cdan) = this%coor
     get_all_this(  inter_dim+1:2*inter_dim,i_cdan) = this%uc(1:inter_dim,1)
     get_all_this(2*inter_dim+1:3*inter_dim,i_cdan) = this%uc(1:inter_dim,2)
     n = 3
     if ( inter_dim == 3 ) then
       get_all_this(3*inter_dim+1:4*inter_dim,i_cdan) = this%uc(1:inter_dim,inter_dim)
       n = 4
     end if
     get_all_this(   n *inter_dim+1:(n+1)*inter_dim,i_cdan) = this%rl(1:inter_dim)
     get_all_this((n+1)*inter_dim+1:(n+2)*inter_dim,i_cdan) = this%vl(1:inter_dim)
     
   end do

 end function

