! module permettant d'utiliser le couplage gaz-grains en python
module wrap_GGCxx

   use gas_grains_coupling, only: new_gas_grains_coupling, &
                                  set_fluid_model, &
                                  set_distribution_type, & 
                                  set_weight_type, &
                                  set_fluid_viscosity, &
                                  set_average_pressure, &
                                  get_max_height, &
                                  get_min_height, &
                                  delete_gas_grains_coupling, &
                                  Grains_In_Mesh, &
                                  Compute_Average_Node_Fields, &
                                  Compute_Elementary_Biot, &
                                  Compute_Hydrodynamic_Force, &
                                  set_gas_grains_coupling_fields, &
                                  Add_Biot_To_External_Flux_By_Element, &
                                  Set_Hydrodynamic_Force_By_Grain, &
                                  Update_Fields, &
                                  Init_Fixed_Point_Algorithm, &
                                  Increment_Fixed_Point_Algorithm, &
                                  Check_Fixed_Point_Algorithm_Convergence, &
                                  Update_Fixed_Point_Algorithm, &
                                  compute_mean_porosity_error, &
                                  compute_mean_pressure
  
contains

   ! fonction qui permet de fixer le type d'equation a resoudre,
   ! en fonction du modele pour le fluide
   subroutine SetFluidModel(model)

      implicit none

      ! variable d'entree :
      character(len=7) :: model ! modele a utiliser pour le fluide :
         ! * 'gaz_lin' : gaz parfait, avec le modele linearise
         ! * 'gaz_std' : gaz parfait, avec le modele standard (i.e. non lineaire)
         ! * 'incomp_' : fluide incompressible
   
      call set_fluid_model(model)

   end subroutine SetFluidModel
   
   ! constructeur : 
   ! * recupere les donnees pour le couplage :
   !   - le maillage de thermique
   !   - les numeros des disques dans RBDY2 
   subroutine NewGasGrainsCoupling
         
      implicit none
            
      call new_gas_grains_coupling

   end subroutine NewGasGrainsCoupling
         
   ! fonction qui gere la distribution uniforme de rayon dans 
   ! un intervalle [R_min, R_max] donne
   subroutine SetDistributionType(distribution)

      implicit none
 
      ! variable d'entree :
      character(len=5) :: distribution ! chaine indiquant quel type de 
         ! distribution on souhaite utiliser :
         !    * 'mono_' : on est dans le cas monodisperse
         !    * 'unifo' : on considere une distribution uniforme
         !    * 'gener' : cas general ou on ne connait pas la distribution
         !                utilisee a priori
     
      call set_distribution_type(distribution)

   end subroutine SetDistributionType

   ! fonction qui permet de changer la strategie de calcul de la 
   ! vitesse moyennee des grains
   ! N.B.: utiliser les masses et la strategie par defaut
   !       (adaptee au cas general : polydisperse, avec plusieurs
   !       materiaux)
   subroutine SetWeightType(weight)
 
      implicit none

      ! variable d'entree :
      character(len=5) :: weight ! chaine indiquant quel type de poids
         ! on souhaite utiliser :
         !    * 'vol__' : on pondere avec le volume des grains
         !    * 'none_' : on ne pondere pas (i.e. on applique un
         !                poids de 1 a chaque grain)

      call set_weight_type(weight)

   end subroutine SetWeightType
 
   ! fonction qui recupere la viscosite du fluide
   subroutine SetFluidViscosity(eta)
            
      implicit none
         
      ! variable d'entree :
      real(kind=8) :: eta ! viscosite du fluide
         
      ! on stocke la viscosite dans le module
      call set_fluid_viscosity(eta)
         
   end subroutine SetFluidViscosity

   ! fonction qui recupere la pression moyenne du fluide
   subroutine SetAveragePressure(P0)
            
      implicit none
         
      ! variable d'entree :
      real(kind=8) :: P0 ! pression moyenne du fluide
         
      ! on stocke la pression moyennedu fluide dans le module
      call set_average_pressure(P0)
          
   end subroutine SetAveragePressure

   ! fonction qui renvoie l'altitude du grain centre d'inertie du
   ! grain le plus haut de l'echantillon
   ! N.B.: cette fonction se sert des coordonnes des grains
   !       stockees dans le module
   function GetMaxHeight()

      implicit none

      ! valeur de retour :
      real(kind=8) :: GetMaxHeight

      GetMaxHeight = get_max_height()

   end function GetMaxHeight
         
   ! fonction qui renvoie l'altitude du grain centre d'inertie du
   ! grain le plus bas de l'echantillon
   ! N.B.: cette fonction se sert des coordonnes des grains
   !       stockees dans le module
   function GetMinHeight()

      implicit none

      ! valeur de retour :
      real(kind=8) :: GetMinHeight

      GetMinHeight = get_min_height()

   end function GetMinHeight

   ! destructeur: libere l'espace memoire occuppe par le couplage
   ! gaz-grains
   subroutine DeleteGasGrainsCoupling
         
      implicit none

      call delete_gas_grains_coupling

   end subroutine DeleteGasGrainsCoupling

   ! fonction qui cherche dans quel element se trouve chaque grain
   ! a partir des coordonnees des grains (a la fin du pas de temps)
   ! apres les avoir recuperes
   subroutine GrainsInMesh
         
      implicit none
         
      call Grains_In_Mesh

   end subroutine GrainsInMesh

   ! fonction qui calcule la porosite et le champ de vitesse
   ! barycentrque des grains, apres avoir recupere les vitesses
   ! des grains a la fin du pas de temps
   subroutine ComputeAverageNodeFields
            
      implicit none
         
      call Compute_Average_Node_Fields

   end subroutine ComputeAverageNodeFields
         
   ! fonction qui calcule les vecteurs elementaires corespondant
   ! a la contribution du terme de Biot (cas lineaire), a la fin du pas
   subroutine ComputeElementaryBiot
 
      implicit none

      call Compute_Elementary_Biot

   end subroutine ComputeElementaryBiot
 
   ! fonction qui calcule la force hydrodynamique sur les grains,
   ! a la fin du pas de temps, a partir du champ de pression
   ! (temperature) a la fin du pas de temps et du champ de porosite
   ! a la fin du pas de temps
   subroutine ComputeHydrodynamicForce
         
      implicit none
         
      call Compute_Hydrodynamic_Force
         
   end subroutine ComputeHydrodynamicForce
         
   ! fonction qui depose le champ de proosite aux points de Gauss
   ! (dans le "field" 'SPHV') et de coefficient de diffusion aux points de
   ! Gauss (dans le "filed" 'COCO'), apres l'avoir calcule
   subroutine SetGasGrainsGouplingFields
         
      implicit none
         
      call set_gas_grains_coupling_fields
            
   end subroutine SetGasGrainsGouplingFields

   ! fonction qui ajoute la contribution du terme de Biot aux flux
   ! externes par element, a la fin du pas
   subroutine AddBiotToExternalFluxByElement

      implicit none

      call Add_Biot_To_External_Flux_By_Element

   end subroutine AddBiotToExternalFluxByElement
         
   ! fonction qui depose la force hydrodynamique, moyennee sur le
   ! pas de temps, sur les grains a la fin du pas de temps ; apres
   ! l'avoir calculee
   subroutine SetHydrodynamicForceByGrain
 
      implicit none
 
      call Set_Hydrodynamic_Force_By_Grain
 
   end subroutine SetHydrodynamicForceByGrain
 
   ! fonction qui met a jour les donnees du module pour le prochain
   ! pas de temps
   subroutine UpdateFields
         
       implicit none
         
       call Update_Fields

   end subroutine UpdateFields

   ! fonction qui initialise le point fixe sur les vitesses des
   ! grains a la fin du pas de temps, pour ammorcer un nouveau pas
   ! de temps
   subroutine InitFixedPointAlgorithm
 
      implicit none

      call Init_Fixed_Point_Algorithm
            
    end subroutine InitFixedPointAlgorithm
 
   ! fonction qui prepare la nouvelle iteration du point fixe : 
   !   * force la vitesse a la fin du pas a etre la derniere 
   !     approximation calculee par le point fixe
   !   * calcule les positions corespondantes
   subroutine IncrementFixedPointAlgorithm
 
      implicit none
 
      call Increment_Fixed_Point_Algorithm
   
   end subroutine IncrementFixedPointAlgorithm
 
   ! fonction qui recupere la nouvelle approximation des vitesses 
   ! a la fin du pas de temps et renvoie vrai si l'algorithme 
   ! de point fixe a converge 
   function CheckFixedPointAlgorithmConvergence(norm_type, tol)
 
      implicit none

      ! variables d'entree :
      character(len=5), intent(in) :: norm_type ! type de norme utilisee pour
         ! estimer la convergence
      real(kind=8), intent(in) :: tol ! tolerance pour setimer la convergence

      ! valeur de retour :
      logical :: CheckFixedPointAlgorithmConvergence ! vaut "vrai"
      ! ssi on a atteint un nouvel etat d'equilibre

      CheckFixedPointAlgorithmConvergence = &
         Check_Fixed_Point_Algorithm_Convergence(norm_type, tol)
 
   end function CheckFixedPointAlgorithmConvergence

   ! fonction qui met a jour l'algorithme de point fixe :
   subroutine UpdateFixedPointAlgorithm
 
      implicit none
 
      call Update_Fixed_Point_Algorithm

   end subroutine UpdateFixedPointAlgorithm

   ! fonction qui calcule l'erreur sur la porosite moyenne, i.e. l'erreur
   ! entre la porosite moyenne calculee a partir des porosites aux noeuds
   ! et la porosite moyenne theorique calculee directement a partir des 
   ! volumes des grains et du volume du domaine, a la fin du pas de temps
   function ComputeMeanPorosityError()

      implicit none

      ! valeur de retour :
      real(kind=8) :: ComputeMeanPorosityError ! erreur relative sur la
         ! porosite moyenne

      ComputeMeanPorosityError = compute_mean_porosity_error()

   end function ComputeMeanPorosityError

   ! fonction qui calcule la pression moyenne et l'ecart type sur la pression
   ! , a la fin du pas de temps
   subroutine ComputeMeanPressure(P_mean, P_stdev)

      implicit none

      ! variables de sorties :
      real(kind=8), intent(out) :: P_mean ! pression moyenne
      real(kind=8), intent(out) :: P_stdev ! ecart-type sur la pression

      call compute_mean_pressure(P_mean, P_stdev)

   end subroutine ComputeMeanPressure

end module wrap_GGCxx
