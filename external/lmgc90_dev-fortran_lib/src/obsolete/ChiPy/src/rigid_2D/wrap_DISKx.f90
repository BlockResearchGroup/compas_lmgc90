!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE wrap_DISKx

  !!****h* LMGC90.CHIPY/DISKx
  !! NAME
  !!  module wrap_DISKx
  !! PURPOSE
  !!  Wrap the public API
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/DISKx
  !!****

  USE diskx,ONLY:&
      read_bodies_DISKx, &
      get_nb_DISKx, &
      get_DISKx2RBDY2, & ! <- am: debut des fonctions supplementaires
      get_radius_DISKx, &
      get_mean_radius_DISKx, &
      get_max_radius_DISKx, &
      get_min_radius_DISKx, &
      get_color_DISKx

  PUBLIC 

CONTAINS

!!!---------------------------------------------------------------------

    SUBROUTINE LoadTactors
      IMPLICIT NONE

       !!****e* DISKx/LoadTactors
       !! NAME
       !!   wrap_diskx.readbodies
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for DISKx contactors
       !! USES
       !!  LMGC90.CORE/DISKx/read_bodies_DISKx
       !!****

       CALL read_bodies_DISKx

    END SUBROUTINE

!!!-am----- début des définitions de fonctions supplémentaires...------
    ! procèdure qui récupère le nombre de DISKx
    function GetNbDISKx()
      IMPLICIT NONE
	
      integer :: GetNbDISKx
		
       !!****e* DISKx/GetNbDISKx
       !! NAME
       !!   wrap_diskx.getnbdiskx
       !! PURPOSE
       !!  return the number of diskx
       !! USES
       !!  LMGC90.CORE/DISKx/get_nb_DISKx
       !!****

      GetNbDISKx=get_nb_DISKx()

    END function GetNbDISKx
    
    ! procèdure qui récupère la table de corresondance entre les contacteurs
 	! disques et les corps associé, dans RBDY2
    subroutine GetDISKx2RBDY2(diskx2rbdy2_, nb_disk_)
       implicit none
       integer, intent(in) :: nb_disk_ ! nombre de disques attentu par celui qui appelle cette 
                                       ! fonction, i.e. la taille de diskx2rbdy2_	
       integer, dimension(nb_disk_), intent(out) :: diskx2rbdy2_ ! vecteur pour récupérer la table de
														  ! correpondance
       !f2py optional, depend(nb_disk_) :: nb_disk_=len(diskx2rbdy2_)
	
       !!****e* DISKx/GetDISKx2RBDY2
       !! NAME
       !!   wrap_diskx.getdiskx2rbdy2
       !! PURPOSE
       !!  return the table diskx2rbdy2 giving, for a DISKx itact
       !!  the number of the coreponding body in RBDY2: diskx2rbdy2(itact)
       !! USES
       !!  LMGC90.CORE/DISKx/get_DISKx2RBDY2
       !!****

       call get_DISKx2RBDY2(diskx2rbdy2_, nb_disk_)
    
    end subroutine GetDISKx2RBDY2

    ! fonction qui récupère le rayon du DISKx, d'indice itact
    function GetRadiusDISKx(itact)
      implicit none
      integer, intent(in) :: itact ! indice du DISKx dont on veut récupérer le rayon
      real(kind=8) :: GetRadiusDISKx ! rayon du DISKx, d'indice itact
   	
      !!****e* DISKx/GetRadiusDISKx
      !! NAME
      !!   wrap_diskx.getradiusdiskx
      !! PURPOSE
      !!  return the radius of diskx
      !! USES
      !!  LMGC90.CORE/DISKx/get_radius_DISKx
      !!****

      GetRadiusDISKx = get_radius_DISKx(itact)
   		
    end function GetRadiusDISKx
  
    ! fonction qui renvoie le rayon moyen des DISKx
    function GetMeanRadiusDISKx()
       implicit none
       real(kind=8) :: GetMeanRadiusDISKx ! rayon moyen des DISKx
 	   
       !!****e* DISKx/GetMeanRadiusDISKx
       !! NAME
       !!   wrap_diskx.getmeanradiusdiskx
       !! PURPOSE
       !!  return the mean radius of the diskx
       !! USES
       !!  LMGC90.CORE/DISKx/get_mean_radius_DISKx
       !!****
   		
       GetMeanRadiusDISKx = get_mean_radius_DISKx()

    end function GetMeanRadiusDISKx

    ! fonction qui renvoie le rayon du plus grand DISKx
    function GetMaxRadiusDISKx()
       implicit none
       real(kind=8) :: GetMaxRadiusDISKx ! rayon du plus grand DISKx
        
       !!****e* DISKx/GetMaxRadiusDISKx
       !! NAME
       !!   wrap_diskx.getmaxradiusdiskx
       !! PURPOSE
       !!  return the max radius of the diskx
       !! USES
       !!  LMGC90.CORE/DISKx/get_max_radius_DISKx
       !!****
                
       GetMaxRadiusDISKx = get_max_radius_DISKx()

    end function GetMaxRadiusDISKx

    ! fonction qui renvoie le rayon du plus petit DISKx
    function GetMinRadiusDISKx()
       implicit none
       real(kind=8) :: GetMinRadiusDISKx ! rayon du plus petit DISKx

       !!****e* DISKx/GetMinRadiusDISKx
       !! NAME
       !!   wrap_diskx.getminradiusdiskx
       !! PURPOSE
       !!  return the min radius of the diskx
       !! USES
       !!  LMGC90.CORE/DISKx/get_min_radius_DISKx
       !!****

       GetMinRadiusDISKx = get_min_radius_DISKx()

    end function GetMinRadiusDISKx

    ! fonction qui renvoie la couleur d'un DISKx
    function GetColor(itact)

       implicit none

       ! variable d'entrée:
       integer :: itact ! numéro du DISKx considéré

       ! valeur de retour:
       character(len=5) :: GetColor ! couleur du DISKx considéré

       GetColor = get_color_DISKx(itact)

    end function GetColor

!!!-am------ fin des définitions de fonctions supplémentaires...-------

END MODULE wrap_DISKx
