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

! Defines a type of interaction with dimensions as a parameter (inter_dim, space_dim)

  !------------------------------------------------------------------------  
  !> Interaction type
  type, public :: T_interaction ! <space_dim> <inter_dim>

    !== identification part ==!

    !> type id of contact
    integer(kind=4) :: cdan = 0
    !> index of the interaction
    integer(kind=4) :: icdan = 0

    !> entity index of candidate
    integer(kind=4) :: icdent = 0
    !> entity index of antagonist
    integer(kind=4) :: ianent = 0

    !> body type of candidate
    integer(kind=4) :: icdbtyp = 0
    !> body type of antagonist
    integer(kind=4) :: ianbtyp = 0

    !> body index of candidate
    integer(kind=4) :: icdbdy = 0
    !> body index of antagonist
    integer(kind=4) :: ianbdy = 0

    !> contactor type of candidate
    integer(kind=4) :: icdctyp = 0
    !> contactor type of antagonist
    integer(kind=4) :: ianctyp = 0

    !> contactor index of candidate
    integer(kind=4) :: icdtac = 0
    !> contactor index of antagonist
    integer(kind=4) :: iantac = 0

    !> contactor index of candidate in the body list
    integer(kind=4) :: icdbtac = 0
    !> contactor index of antagonist in the body list
    integer(kind=4) :: ianbtac = 0

    !> a contactor sub-id of candidate
    integer(kind=4) :: icdsci  = 0
    !> a contactor sub-id of antagonist
    integer(kind=4) :: iansci  = 0

    !== end identification part ==!


    !> index of adjacent
    integer(kind=4) :: iadj = 0
    !> index of see type
    integer(kind=4) :: isee = 0
    !> index of contact law
    integer(kind=4) :: lawnb = 0
    !> contact law type
    integer(kind=4) :: i_law = 0

    !== geometric detection part ==!

    !> contact locus
    real(kind=8), dimension(space_dim) :: coor
    
    !!> local frame (t,n,s)
    !real(kind=8), dimension(space_dim,space_dim) :: uc
    real(kind=8), dimension(space_dim) :: nuc, tuc, suc

    !> gap at predicted time (1-theta)*h
    real(kind=8) :: gapTTbegin
    !> gap at predicted time (1-theta)*h + h
    real(kind=8) :: gapTT

    !> area (length) of contact
    real(kind=8) :: area

    !== end geometric detection part ==!


    integer(kind=4) :: xperiodic, yperiodic, zperiodic

    !== RIGIDS/RIGIDS ==!
    !> moment/rotation computation
    real(kind=8) :: Gcdt3, Gcdn3, Gant3, Gann3
    real(kind=8), dimension(inter_dim) :: Gcds, Gans, Gcdt, Gant, Gcdn, Gann
    !===================!

    !== xSx ==!
    real(kind=8), dimension(3) :: weight
    !== PRx ==!
    integer(kind=4) :: id_f_cd = 0
    integer(kind=4) :: id_f_an = 0
    integer(kind=4) :: type_ctc= 0
    !== SPx ==!
    real(kind=8) :: QSij, QCij
    !== DKx ==!
    !> candidat point coordinates
    real(kind=8), dimension(space_dim) :: icdcoor
    !> antagonist point coordinates
    real(kind=8), dimension(space_dim) :: iancoor
    !== PLx ==!
    !> contact complementaire et flag nouveau contact
    integer(kind=4) :: icocdan, dct
    !== xLp ==!
    !> normalized projection of candidat on antagonist
    real(kind=8) :: cpcd
    !== PTL ==!
    !> initial distance
    real(kind=8) :: nonuc0
    !===========!


    !== solver part ==!

    !!> local relative velocity at beginnig
    !real(kind=8), dimension(inter_dim) :: vlBEGIN
    !!> local relative velocity
    !real(kind=8), dimension(inter_dim) :: vl
    !!> local reaction
    !real(kind=8), dimension(inter_dim) :: rl
    real(kind=8) :: vltBEGIN, vlnBegin, vlsBegin, vlt, vln, vls, rlt, rln, rls

    !> status at end of time step
    integer(kind=4) :: status
    !> status at beginning of time step... what status ?
    integer(kind=4) :: statusBEGIN

    !> number of internal variables
    integer(kind=4) :: nb_internal = 0
    !> internal variables
    real(kind=8), dimension(max_internal_tact) :: internal = 0.d0

    !== here for historic reasons   ==!
    !>\todo to move/remove at some point

    !> group of contact icdan (for mpi)
    integer(kind=4) :: group

    !> effective mass for md method 
    real(kind=8) :: meff
    !> effective radius for md method 
    real(kind=8) :: reff

  end type T_interaction


  type, public :: T_this_adjac
     
     ! adjac(icdtac)%icdan(iadj):
     ! serial number in this for adjacent contactor iadj to candidate contactor icdtac.
     ! For the definition of adjacent see below in type T_verlt.
     ! When performing stock_rloc, verlt type is filled in according to adjac order, i.e.
     ! verlt(icdtac)%icdan(iadj)=adjac(icdtac)%icdan(iadj)

     integer, dimension(:), pointer :: icdan => null()
     
  end type T_this_adjac


  type, public :: T_verlet

     !> \todo: there is to many data in this type
     !>        since the candidat contactor number is used to index
     !>        the verlet array, just storing the contact number and
     !>        antagonist contactor number should be enough. Every
     !>        other information about involved bodiesnumber should
     !>        be get from tact2bdyty array.

     ! Let be some candidate contactor CLxxx icdtac supported by some candidate 
     ! body icdbdy and some antagonist contactor CLxxx iantac supported by some
     ! antagonist body ianbdy. The contactors may be close enough, within some 
     ! alert distance so that the the antagonist contactor is said 'adjacent' to
     ! the candidate contactor (the antagonist body is said as well adjacent to 
     ! the candidate body).

     ! A list of candidate antagonist pairs contactor-contactor
     ! adjacent to a given contactor is useful for quick access to
     ! data. Such a list is a generalisation of Verlet lists.

     !> size of below arrays
     integer :: adjsz = 0

     !> serial number in this for adjacent contactor iadj
     !> to candidate contactor icdtac.
     integer, dimension(:), pointer :: icdan  => null()

     !> serial number of candidate body
     integer :: cdbdy

     !> rank of candidate contactor attach to cdbdy
     integer:: cdtac

     !> type of model of cdbdy
     integer:: cdmodel

     !> serial number of candidate sub-contactor id
     integer, dimension(:), pointer :: cdsci => null()

     !> serial number of antagonist body for adjacent contact iadj.
     integer, dimension(:), pointer :: anbdy  => null()

     !> rank of antagonist contactor attach to anbdy for adjacent contactor iadj.
     integer, dimension(:), pointer :: antac  => null()

     !> type of model of anbdy for adjacent contactor iadj.
     integer, dimension(:), pointer :: anmodel=> null()

     !> serial number of antagonist sub-contactor id
     integer, dimension(:), pointer :: ansci => null()

     !> verlt(icdtac)%rlt(iadj): first tangential components of reaction impulse;
     real(kind=8), dimension(:), pointer :: rlt => null()

     !> verlt(icdtac)%rln(iadj): normal component of reaction impulse;
     real(kind=8), dimension(:), pointer :: rln => null()

     !> verlt(icdtac)%rls(iadj): second tangential components of reaction impulse;
     real(kind=8), dimension(:), pointer :: rls => null()

     !> verlt(icdtac)%vlt(iadj): first tangential components of local velocy;
     real(kind=8), dimension(:), pointer :: vlt => null()

     !> verlt(icdtac)%vln(iadj): normal component of local velocy;
     real(kind=8), dimension(:), pointer :: vln => null()

     !> verlt(icdtac)%vls(iadj): second tangential components of local velocy;
     real(kind=8), dimension(:), pointer :: vls => null()

     real(kind=8), dimension(:), pointer :: gapTT => null()

     !> verlt(icdtac)%status(iadj): status of contact labelled iadj;
     integer     , dimension(:), pointer :: status => null()

     !> verlt(icdtac)%tuc(iadj): first tangential vector;
     real(kind=8), dimension(:,:), pointer :: tuc => null()

     !> verlt(icdtac)%nuc(iadj): normal vector;
     real(kind=8), dimension(:,:), pointer :: nuc => null()

     !> verlt(icdtac)%suc(iadj): second tangential vector;
     real(kind=8), dimension(:,:), pointer :: suc => null()

     !> verlt(icdtac)%internal(iadj): internal parameters
     real(kind=8), dimension(:,:), pointer :: internal => null()

     !> components of antagonist contact point;
     real(kind=8), dimension(:,:), pointer :: coor => null()


     !  === Specific to mod_PRPRx.f90 ===

     real(kind=8), dimension(:,:), pointer :: icdcoor => null() ! verlt(icdtac)%icdcoor
     real(kind=8), dimension(:,:), pointer :: iancoor => null() ! verlt(icdtac)%iancoor

     integer, dimension(:), pointer :: id_f_cd => null()
     integer, dimension(:), pointer :: id_f_an => null()

     !  === End specific ===

  end type T_verlet


  !> contact predigree
  type, public :: T_con
    !> module name
    character( len = 9 ) :: module_name
    !> must be something like i_dkdkx, i_plplx ...
    integer :: id_cdan
    !> candidate contactor id (i_diskx, i_polyr, i_clxxx, etc)
    integer :: id_cdtac
    !> antagonist contactor id (i_diskx, i_polyr, i_aspxx, etc)
    integer :: id_antac

    !> number of candidate
    integer :: nb_cd     
    !> number of antagoniste
    integer :: nb_an
    !> current number of contacts after rough detection (in rough)
    integer :: nb_rough
    !> current number of contacts after fine detection (in this)
    integer :: nb_this
    !> current number of contact stored in verlet (in verlt)
    integer :: nb_verlet
    !> 
    
  end type T_con
  
