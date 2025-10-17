      ! module permettant d'utiliser le couplage gaz-grains en python
      module wrap_GGCxx

         use gas_grains_coupling, only: &
              new_gas_grains_coupling, &
              set_uniform_distribution, &
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
              Update_Fixed_Point_Algorithm
  
      contains

         ! constructeur : 
         ! * récupère les données pour le couplage :
         !   - le maillage de thermique
         !   - les numéros des disques dans RBDY2 
         subroutine NewGasGrainsCoupling
         
            implicit none
            
            call new_gas_grains_coupling

         end subroutine NewGasGrainsCoupling
         
         ! fonction qui gere la distribution uniforme de rayon dans 
         ! un intervalle [R_min, R_max] donne
         subroutine SetUniformDistribution(R_min, R_max)
         
            implicit none

            ! variables d'entree:
            real(kind=8) :: R_min, R_max ! intervalle [R_min, R_max] ou 
               ! sont tires les rayons

            call set_uniform_distribution(R_min, R_max)

         end subroutine SetUniformDistribution
 
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
 
         ! fonction qui récupère la viscosité du fluide
         subroutine SetFluidViscosity(eta)
            
            implicit none
         
            ! variable d'entrée :
            real(kind=8) :: eta ! viscosité du fluide
         
            ! on stocke la viscosité dans le module
            call set_fluid_viscosity(eta)
         
         end subroutine SetFluidViscosity

         ! fonction qui récupère la pression moyenne du fluide
         subroutine SetAveragePressure(P0)
            
            implicit none
         
            ! variable d'entrée :
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

         ! fonction qui cherche dans quel élément se trouve chaque grain
         ! à partir des coordonnées des grains (à la fin du pas de temps)
         ! après les avoir récupérés
         subroutine GrainsInMesh
         
            implicit none
         
            call Grains_In_Mesh

         end subroutine GrainsInMesh

         ! fonction qui calcule la porosité et le champ de vitesse
         ! barycentrque des grains, après avoir récupéré les vitesses
         ! des grains à la fin du pas de temps
         subroutine ComputeAverageNodeFields
            
            implicit none
            
            call Compute_Average_Node_Fields

         end subroutine ComputeAverageNodeFields
         
		 ! fonction qui calcule les vecteurs élémentaires corespondant
		 ! à la contribution du terme de Biot (cas linéaire), à la fin du pas
		 subroutine ComputeElementaryBiot
		 
		    implicit none
		
		    call Compute_Elementary_Biot
		
		 end subroutine ComputeElementaryBiot
		 
         ! fonction qui calcule la force hydrodynamique sur les grains,
         ! à la fin du pas de temps, à partir du champ de pression
         ! (température) à la fin du pas de temps et du champ de porosité
         ! à la fin du pas de temps
         subroutine ComputeHydrodynamicForce
         
            implicit none
         
            call Compute_Hydrodynamic_Force
         
         end subroutine ComputeHydrodynamicForce
         
         ! fonction qui dépose le champ de proosité aux points de Gauss
         ! (dans le "field" 'SPHV') et de coefficient de diffusion aux points de
         ! Gauss (dans le "filed" 'COCO'), après l'avoir calculé
         subroutine SetGasGrainsGouplingFields
         
            implicit none
         
            call set_gas_grains_coupling_fields
            
         end subroutine SetGasGrainsGouplingFields

         ! fonction qui ajoute la contribution du terme de Biot aux flux
		 ! externes par élément, à la fin du pas
		 subroutine AddBiotToExternalFluxByElement

            implicit none
			
			call Add_Biot_To_External_Flux_By_Element
			
	     end subroutine AddBiotToExternalFluxByElement
         
		 ! fonction qui dépose la force hydrodynamique, moyennée sur le
		 ! pas de temps, sur les grains à la fin du pas de temps ; après
		 ! l'avoir calculée
		 subroutine SetHydrodynamicForceByGrain
		 
		    implicit none
		 
		    call Set_Hydrodynamic_Force_By_Grain
		 
		 end subroutine SetHydrodynamicForceByGrain
		 
         ! fonction qui met à jour les données du module pour le prochain
         ! pas de temps
         subroutine UpdateFields
         
             implicit none
         
            call Update_Fields

         end subroutine UpdateFields

		 ! fonction qui initialise le point fixe sur les vitesses des
		 ! grains à la fin du pas de temps, pour ammorcer un nouveau pas
		 ! de temps
		 subroutine InitFixedPointAlgorithm
		 
		    implicit none
			
			call Init_Fixed_Point_Algorithm
            
		 end subroutine InitFixedPointAlgorithm
		 
		 ! fonction qui prépare la nouvelle itération du point fixe : 
		 !   * force la vitesse à la fin du pas a être la dernière 
		 !     approximation calculee par le point fixe
		 !   * calcule les positions corespondantes
		 subroutine IncrementFixedPointAlgorithm
		 
		    implicit none
		 
		    call Increment_Fixed_Point_Algorithm
		   
		 end subroutine IncrementFixedPointAlgorithm
		 
		 ! fonction qui récupère la nouvelle approximation des vitesses 
		 ! à la fin du pas de temps et renvoie vrai si l'algorithme 
		 ! de point fixe a convergé 
		 function CheckFixedPointAlgorithmConvergence(norm_type, tol)
		 
		    implicit none
			
			! variables d'entrée :
			character(len=5), intent(in) :: norm_type ! type de norme utilisee pour
			   ! estimer la convergence
			real(kind=8), intent(in) :: tol ! tolérance pour setimer la convergence
			
			! valeur de retour :
			logical :: CheckFixedPointAlgorithmConvergence ! vaut "vrai"
			   ! ssi on a atteint un nouvel état d'équilibre
			
			CheckFixedPointAlgorithmConvergence = &
			   Check_Fixed_Point_Algorithm_Convergence(norm_type, tol)
		 
		 end function CheckFixedPointAlgorithmConvergence
		 
		 ! fonction qui met à jour l'algorithme de point fixe :
		 subroutine UpdateFixedPointAlgorithm
		 
		    implicit none
			
			call Update_Fixed_Point_Algorithm
		 
		 end subroutine UpdateFixedPointAlgorithm
		
      end module wrap_GGCxx
