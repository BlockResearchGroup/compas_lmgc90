!===========================================================================
!
! Copyright 2000-2022 CNRS-UM.
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

! Defines methods to work on interaction no matter the value of dimension


 !> \brief Empty function for compatibility with new_arch
 function new_interaction(other_int)
   implicit none
   type(T_interaction), optional, intent(in) :: other_int
   type(T_interaction), pointer              :: new_interaction

   new_interaction => null()
 end function

 !> \brief Get the number of interactions
 function get_nb_interactions()
   implicit none
   !> number of interactions in this array
   integer(kind=4) :: get_nb_interactions

   get_nb_interactions = nb_interactions

 end function

 !> \brief Get the number of rough interactions
 function get_nb_rough_interactions()
   implicit none
   !> number of rough interactions in all_t2t array
   integer(kind=4) :: get_nb_rough_interactions

   get_nb_rough_interactions = nb_tact2tacts

 end function

 !> \brief Get the number of recup interactions
 function get_nb_recup_interactions()
   implicit none
   !> number of recup interactions
   integer(kind=4) :: get_nb_recup_interactions

   get_nb_recup_interactions = nb_recup

 end function

 !> \brief Get the number of verlet interactions
 function get_nb_verlet_interactions()
   implicit none
   !> number of interactions in verlet array
   integer(kind=4) :: get_nb_verlet_interactions

   get_nb_verlet_interactions = nb_verlets

 end function

 !> \brief Get one interaction from this
 function get_interaction(i_inter)
   implicit none
   !> id of desired interaction
   integer(kind=4), intent(in) :: i_inter
   !> pointer on desired interaction
   type(T_interaction), pointer :: get_interaction

   if( is_one_current ) then
     get_interaction => this1(i_inter)%inter
   else
     get_interaction => this2(i_inter)%inter
   end if

 end function

 !> \brief Get one interaction from verlet
 function get_verlet_interaction(i_verlet)
   implicit none
   !> id of desired interaction
   integer(kind=4), intent(in) :: i_verlet
   !> pointer on desired interaction
   type(T_interaction), pointer :: get_verlet_interaction

   if( is_one_current ) then
     get_verlet_interaction => this2(i_verlet)%inter
   else
     get_verlet_interaction => this1(i_verlet)%inter
   end if

 end function

 !> \brief allocate number of t2t structures
 subroutine set_nb_tact2tacts(nb_t2t)
   implicit none
   !> number of tact2tact data structures needed
   integer(kind=4), intent(in) :: nb_t2t
   !
   integer(kind=4) :: i_t2t

   nb_tact2tacts = nb_t2t

   if( allocated(all_t2t) ) then
     if( size(all_t2t) < nb_t2t ) then
       do i_t2t = 1, size(all_t2t)
         call erase_face2faces_in_t2t(all_t2t(i_t2t))
       end do
       deallocate(all_t2t)
     end if
   end if

   if( .not. allocated(all_t2t) ) allocate(all_t2t(nb_t2t))

 end subroutine

 !> \brief allocate number of f2f structures in a t2t
 subroutine set_nb_face2faces(t2t, nb_f2f)
   implicit none
   !> tact2tact in which to set the number of f2f
   type(T_tact2tact) :: t2t
   !> number of face2face data structures needed
   integer(kind=4), intent(in) :: nb_f2f
   !
   integer(kind=4) :: i_f2f

   t2t%nb_f2f = nb_f2f

   if( allocated(t2t%f2f) ) then
     if( size(t2t%f2f) < nb_f2f ) then
       call erase_face2faces_in_t2t(t2t)
     end if
   end if

   if( .not. allocated(t2t%f2f) ) allocate(t2t%f2f(nb_f2f))

 end subroutine

 !> \brief Add a rough interaction in 'this' array
 subroutine add_t2t(cdan, icdan, icdtac, iantac, isee, i_param, xperiodic, yperiodic)
   implicit none
   !> [in] type of interaction
   integer(kind=4), intent(in) :: cdan
   !> [in] index of interaction
   integer(kind=4), intent(in) :: icdan
   !> [in] index of candidat contactor
   integer(kind=4), intent(in) :: icdtac
   !> [in] index of antagonist contactor
   integer(kind=4), intent(in) :: iantac
   !> [in] id of the contact law
   integer(kind=4), intent(in) :: isee
   !> [in] some integer parameters
   integer(kind=4), intent(in) :: i_param
   !> [in] is the contact periodic along x-axis (1:yes, 0:no)
   integer(kind=4), intent(in) :: xperiodic
   !> [in] is the contact periodic along y-axis (1:yes, 0:no)
   integer(kind=4), intent(in) :: yperiodic
   !
   character(len=80) :: cout
   character(len=34) :: IAM
   !      1234567890123456789012345678901234
   IAM = 'interaction::add_rough_interaction'

   all_t2t(icdan)%cdan = cdan

   all_t2t(icdan)%icdtac    = icdtac
   all_t2t(icdan)%iantac    = iantac
   all_t2t(icdan)%isee      = isee
   all_t2t(icdan)%i_param   = i_param
   all_t2t(icdan)%xperiodic = xperiodic
   all_t2t(icdan)%yperiodic = yperiodic

   call erase_face2faces_in_t2t(all_t2t(icdan))

 end subroutine

 !> \brief Initialize interactions of a tact2tact structure
 subroutine initialize_interactions(f2f, nb_ctc)
   implicit none
   !> f2f to initialize
   type(T_face2face) :: f2f
   !> number of contacts for this t2t
   integer(kind=4)  , intent(in)    :: nb_ctc
   !
   integer(kind=4) :: i_ctc

   if( allocated(f2f%ctc) ) then
     call erase_interactions_in_f2f(f2f)
   end if

   allocate(f2f%ctc(nb_ctc))
   do i_ctc = 1, nb_ctc
     allocate(f2f%ctc(i_ctc)%inter)
     call initialize_interaction(f2f%ctc(i_ctc)%inter)
   end do

 end subroutine

 !> \brief Allocate field of an interaction
 subroutine initialize_interaction(inter)
   use overall, only: max_internal_tact
   implicit none
   !> Interaction to initialize
   type(T_interaction), pointer :: inter
   !
   integer(kind=4) :: i_dim

   inter%status      = 'nknow'
   inter%statusBEGIN = 'nknow'
   inter%statuscheck = 'nknow'

   !allocate(inter%coor(inter_dim)) 
   !allocate(inter%uc(inter_dim,inter_dim))
   !if( inter_dim == 2 ) then
   !  allocate(inter%Gcd(inter_dim,1))
   !  allocate(inter%Gan(inter_dim,1))
   !else
   !  allocate(inter%Gcd(inter_dim,inter_dim))
   !  allocate(inter%Gan(inter_dim,inter_dim))
   !end if
   !allocate(inter%vlBEGIN(inter_dim))
   !allocate(inter%vl(inter_dim))
   !allocate(inter%rl(inter_dim))
   !allocate(inter%vfree(inter_dim))
   !allocate(inter%covfree(inter_dim))
   !allocate(inter%corl(inter_dim))
   !allocate(inter%W(inter_dim,inter_dim))
   !allocate(inter%WW(inter_dim))
   !allocate(inter%invW(inter_dim))

   inter%coor(1:inter_dim)           = 0.D0
   inter%uc(1:inter_dim,1:inter_dim) = 0.D0
   inter%Gcd(1:inter_dim,:)          = 0.D0
   inter%Gan(1:inter_dim,:)          = 0.D0
   inter%vlBEGIN(1:inter_dim)        = 0.D0
   inter%vl(1:inter_dim)             = 0.D0
   inter%rl(1:inter_dim)             = 0.D0
   inter%vfree(1:inter_dim)          = 0.D0
   inter%covfree(1:inter_dim)        = 0.D0
   inter%corl(1:inter_dim)           = 0.D0
   inter%W(1:inter_dim,1:inter_dim)  = 0.D0
   inter%WW(1:inter_dim)             = 0.D0
   inter%invW(1:inter_dim)           = 0.D0

   do i_dim = 1, inter_dim
     inter%W(i_dim,i_dim) = 1.D0
   end do
   inter%det      = 1.D0
   inter%forecast = 'acton'
   inter%fric     = 0.D0
   inter%forward  = 0.D0
   inter%backward = 0.D0
   inter%ivnish   = 0
   inter%iskip    = 1
   inter%istart   = 0
   inter%nbadj    = 0

   !nothing to do yet
   !allocate(inter%adjjl(s1))

   inter%nb_internal = 0
   inter%internal(1:max_internal_tact) = 0.D0

 end subroutine

 !> \brief resize this array if needs be
 subroutine reallocate_this(this,nb)
   implicit none
   !> this array to reallocate
   type(T_ptr_interaction), dimension(:), pointer :: this
   !> needed size of this
   integer(kind=4), intent(in) :: nb

   if( associated(this) ) then
     if( size(this) < nb ) then
       call erase_interactions_in_this(this)
     end if
   end if 
     
   if( .not. associated(this) ) allocate(this(nb))

 end subroutine

 !> \brief Take all interaction of t2t to put them in a this array
 subroutine t2t_to_this()
   implicit none
   integer(kind=4) :: i_t2t, i_f2f, i_ctc, i_inter
   type(T_ptr_interaction), dimension(:), pointer :: this

   nb_interactions = 0
   do i_t2t = 1, nb_tact2tacts
     do i_f2f = 1, all_t2t(i_t2t)%nb_f2f
       nb_interactions = nb_interactions + all_t2t(i_t2t)%f2f(i_f2f)%nb_ctc
     end do
   end do

   if( is_one_current ) then
     call reallocate_this(this1, nb_interactions)
     this => this1
   else
     call reallocate_this(this2, nb_interactions)
     this => this2
   end if

   i_inter = 0
   do i_t2t = 1, nb_tact2tacts
     do i_f2f = 1, all_t2t(i_t2t)%nb_f2f
       do i_ctc = 1, all_t2t(i_t2t)%f2f(i_f2f)%nb_ctc
         i_inter = i_inter + 1
         if( associated(this(i_inter)%inter) ) deallocate(this(i_inter)%inter)
         this(i_inter)%inter => all_t2t(i_t2t)%f2f(i_f2f)%ctc(i_ctc)%inter
         all_t2t(i_t2t)%f2f(i_f2f)%ctc(i_ctc)%inter => null()
       end do
     end do
   end do

 end subroutine

 !> \brief Stock local reaction from this to verlet
 subroutine stock_rloc()
   implicit none
   !
   integer(kind=4) :: i_inter
   type(T_interaction), pointer :: inter

   if( allocated(verlet) ) then
     if( size(verlet) < nb_interactions ) deallocate(verlet)
   end if

   if( .not. allocated(verlet) ) allocate(verlet(nb_interactions))

   nb_verlets = nb_interactions

   verlet(1:nb_interactions) = 0
   do i_inter = 1, nb_interactions
     inter => get_interaction(i_inter)
     verlet( vrlt_map(inter%icdtac) + inter%iadj ) = i_inter
   end do

   is_one_current = .not. is_one_current
     
 end subroutine

 !> \brief Recup local reaction from verlet to this
 subroutine recup_rloc()
   use overall, only: Hrate, logmes
   implicit none
   character(len=80) :: cout
   integer(kind=4)   :: i_inter, i_adj
   type(T_interaction), pointer :: inter, v_inter

   inter   => null()
   v_inter => null()

   nb_recup = 0

   if( nb_verlets == 0 ) return

   do i_inter = 1, nb_interactions
     inter => get_interaction(i_inter)
     do i_adj = vrlt_map(inter%icdtac)+1, vrlt_map(inter%icdtac+1)
       v_inter => get_verlet_interaction(verlet(i_adj))
       if( v_inter%cdan   == inter%cdan   .and. &
           v_inter%icdbdy == inter%icdbdy .and. &
           v_inter%icdtac == inter%icdtac .and. &
           v_inter%ianbdy == inter%ianbdy .and. &
           v_inter%iantac == inter%iantac       ) then

         inter%rl(:) = v_inter%rl(:) * Hrate
         inter%statusBEGIN = v_inter%status
         inter%internal(:) = v_inter%internal(:)

         nb_recup = nb_recup + 1
         exit
       end if
     end do

   end do

   write(cout,'(1X,I0,1x,A)') nb_recup ,'contacts recup'
   call logmes(cout)

 end subroutine

 !> \brief Erase all interactions in a this
 subroutine erase_interactions_in_this(this)
   implicit none
   type(T_ptr_interaction), dimension(:), pointer :: this
   !
   integer(kind=4) :: i_inter

   if( associated(this) ) then
     do i_inter = 1, size(this)
       if( associated(this(i_inter)%inter) ) then
         call erase_interaction(this(i_inter)%inter)
         deallocate(this(i_inter)%inter)
       end if
     end do
     deallocate(this)
     nullify(this)
   end if

 end subroutine

 !> \brief Erase all face2faces in a t2t
 subroutine erase_face2faces_in_t2t(t2t)
   implicit none
   type(T_tact2tact), intent(inout) :: t2t
   !
   integer(kind=4) :: i_f2f

   if( allocated(t2t%f2f) ) then
     t2t%nb_f2f = 0
     do i_f2f = 1, size(t2t%f2f)
       call erase_interactions_in_f2f(t2t%f2f(i_f2f))
     end do
     deallocate(t2t%f2f)
   end if

 end subroutine

 !> \brief Erase all interactions in a f2f
 subroutine erase_interactions_in_f2f(f2f)
   implicit none
   type(T_face2face), intent(inout) :: f2f
   !
   integer(kind=4) :: i_ctc

   if( allocated(f2f%ctc) ) then
     f2f%nb_ctc = 0
     do i_ctc = 1, size(f2f%ctc)
       if( associated(f2f%ctc(i_ctc)%inter) ) then
         call erase_interaction(f2f%ctc(i_ctc)%inter)
         deallocate(f2f%ctc(i_ctc)%inter)
         nullify(f2f%ctc(i_ctc)%inter)
       end if
     end do
     deallocate(f2f%ctc)
   end if

 end subroutine

 subroutine erase_interaction(inter)
   implicit none
   type(T_interaction) :: inter

   !if( allocated(inter%vlbegin) ) deallocate(inter%vlbegin)
   !if( allocated(inter%vl     ) ) deallocate(inter%vl     )
   !if( allocated(inter%rl     ) ) deallocate(inter%rl     )
   !if( allocated(inter%vfree  ) ) deallocate(inter%vfree  )
   !if( allocated(inter%covfree) ) deallocate(inter%covfree)
   !if( allocated(inter%corl   ) ) deallocate(inter%corl   )
   !if( allocated(inter%coor   ) ) deallocate(inter%coor   )
   !if( allocated(inter%Gcd    ) ) deallocate(inter%Gcd    )
   !if( allocated(inter%Gan    ) ) deallocate(inter%Gan    )
   !if( allocated(inter%W      ) ) deallocate(inter%W      )
   !if( allocated(inter%WW     ) ) deallocate(inter%WW     )
   !if( allocated(inter%invW   ) ) deallocate(inter%invW   )
   if( allocated(inter%adjjl  ) ) deallocate(inter%adjjl  )

 end subroutine

 subroutine clean_memory_interaction()
   implicit none
   integer(kind=4) :: i

   nb_tact2tacts   = 0
   nb_interactions = 0
   nb_verlets      = 0

   if( allocated(all_t2t) ) then
     do i = 1, size(all_t2t)
       call erase_face2faces_in_t2t(all_t2t(i))
     end do
     deallocate(all_t2t)
   end if

   if( allocated(vrlt_map) ) deallocate(vrlt_map)
   if( allocated(verlet  ) ) deallocate(verlet  )

   call erase_interactions_in_this(this1)
   call erase_interactions_in_this(this2)

 end subroutine

 !> \brief Get data allowing to display interactions
 function get_ptr_all()
   implicit none
   !> pointer on section of array holding data of verlet
   real(kind=8), dimension(:,:), pointer :: get_ptr_all
   !
   integer(kind=4) :: icdan
   type(T_interaction), pointer :: inter

   get_ptr_all => null()
   if( nb_verlets == 0 ) return

   if( associated(all_this) ) then
     if( nb_verlets > size(all_this,2) ) then
       deallocate(all_this)
       nullify(all_this)
     end if
   end if
   if( .not. associated(all_this) ) allocate(all_this((2+inter_dim)*inter_dim+1,nb_verlets))
    
   do icdan = 1, nb_verlets

     inter => get_verlet_interaction(icdan)

     all_this(            1:  inter_dim, icdan) = inter%coor
     all_this(  inter_dim+1:2*inter_dim, icdan) = inter%uc(:,1)
     all_this(2*inter_dim+1:3*inter_dim, icdan) = inter%uc(:,2)
     if( inter_dim > 2 ) all_this(3*inter_dim+1:4*inter_dim, icdan) = inter%uc(:,inter_dim)
     all_this((inter_dim+1)*inter_dim+1:(inter_dim+2)*inter_dim, icdan) = inter%rl / H
     all_this((inter_dim+2)*inter_dim+1                        , icdan) = inter%gapTT

   end do

   get_ptr_all => all_this(:,1:nb_verlets)

 end function

 !> \brief Reallocate this if needs be before reading
 subroutine reset_this_for_read(nb_read)
   implicit none
   !> number of interactions read during first reading
   integer(kind=4), intent(in) :: nb_read

   if( is_one_current ) then
     call reallocate_this(this1,nb_read)
   else
     call reallocate_this(this2,nb_read)
   end if

   nb_interactions = nb_read

 end subroutine

