import os,sys
sys.path.append('/home0/martin0/WORK/CODING/v2/compil_ifort/LMGC90v2_dev/ChiPy')
from ChiPy import *


####
# pas indispensable
#wrap_overall.setworkingdirectory('/home/dubois/tmp_EXAMPLES/test_v2/LMGC90v2_Examples/silo_DISKx/')

# on indique qu'on travaille en deformation planes
wrap_overall.dime(2, 1)

### definition des parametres du calcul ### 

# choix du pas de temps
print 'INIT TIME STEPPING'
wrap_overall.settimestep(5.e-5)

# initialisation de l'integrateur en temps :
# en thermique, on utilise Cranck-Nicholson et pas la theta-methode
# utilisee en meca
wrap_overall.initcranknickolsonintegrator(0.5)

# initialisation de l'integrateur en temps pour la meca :
wrap_overall.initthetaintegrator(0.5)

### lecture du modele ###

# lecture des corps
print 'READ BODIES'
# lecture du maillage de thermique : pour le calcul de la pression
wrap_mailx.readbodies()
# lecture des grains
wrap_rbdy2.readbodies()

# lecture du modele
print 'READ MODELS'
wrap_models.readmodels()

# lecture des parametres materiaux
print 'READ BEHAVIOURS'
wrap_bulk_behav.readbehaviours()

# lecture des interactions de contact (entre les grains)
wrap_tact_behav.readbehaviours()

### ecriture paranoiaque du modele ###

# ecriture des corps
print 'WRITE BODIES'
wrap_overall.writebodies()
wrap_mailx.writebodies()
wrap_rbdy2.writebodies()

# ecriture du modele
print 'WRITE MODELS'
wrap_models.writemodels()

# ecriture des parametres materiaux
print 'WRITE BEHAVIOURS'
wrap_bulk_behav.writebehaviours()
wrap_tact_behav.writebehaviours()

### initialisation du modeles ###

# on dimensionne et on initie la construction des mapping
print 'INIT MODELS'
wrap_models.initmodels()

# chargement des choses et construction des mapping
print 'LOAD MODELS'
wrap_thermailx.readmodels()
print 'LOAD BEHAVIOURS'
wrap_thermailx.readbehaviours()
# chargement du modele rigide
wrap_rbdy2.loadbehaviours()

# lecture des contacteurs, pour les grains
wrap_diskx.loadtactors()
wrap_joncx.loadtactors()

### lecture de la condition initiale et des conditions aux limites ###

# lecture de la condition initiale
print 'READ INI DOF'
wrap_overall.readinidof()
wrap_thermailx.readinidof()
wrap_rbdy2.readinidof()

# lecture des contacts initiaux
print 'READ INI Vloc Rloc'
wrap_overall.readinivlocrloc()
wrap_dkdkx.readinivlocrloc()
wrap_dkjcx.readinivlocrloc()

# lecture des conditions aux limites
print 'READ DRIVEN DOF'
wrap_thermailx.readdrivendof()
wrap_rbdy2.readdrivendof()

### ecriture paranoiaque de la condition initiale ###
print 'WRITE LAST DOF'
wrap_overall.writelastdof()
wrap_thermailx.writelastdof()
wrap_rbdy2.writelastdof()

### post2D ##

# on demande d'afficher la temperature aux points de Gauss
print 'DISPLAY THERMAL GPV'
wrap_post2d.withthermalgpv()

# affichage des grains :
# on donne le rayon de reference pour l'affichage des contacteurs
wrap_post2d.referenceradius(1e-6)
# on demande d'afficher les contacteurs
wrap_post2d.withtactor()
wrap_post2d.withcontactpoint()

# on initialise la creation des sorties GMV
print 'INIT GMV'
wrap_post2d.initgmv()

print 'WRITE OUTPUT GMV'
wrap_overall.writeoutgmv(1)
wrap_post2d.writeoutgmv(1)

### preparation de l'algo de detection par les boites ###
print 'COMPUTE BOX'
wrap_dkdkx.computebox()
wrap_dkjcx.computebox()
wrap_afterall.computebox()

# on calcule les masses des grains
print 'COMPUTE MASS'
wrap_rbdy2.computemass()

# initialisation du couplage gaz-grains
print 'INIT GAS GRAINS COUPLING'
# on donne la viscosite du fluide
wrap_ggcxx.setfluidviscosity(1.8e-5)
# on donne la pression moyenne du fluide
wrap_ggcxx.setaveragepressure(1.0e5)
# on alloue l'espace memoire pour le module
# et on initialise les champs
wrap_ggcxx.newgasgrainscoupling()

# on ajoute la contribution du terme de Biot
# calcule a l'instant initial (lors de 
# l'initialisation du module), aux flux
# externes, par element
print 'ADD BIOT THERM TO EXTERNAL FLUX BY ELEMENT'
wrap_ggcxx.addbiottoexternalfluxbyelement()

# calcul des flux, grad et valeurs aux pg, initiaux
# N.B.: on fait cette etape ici, afin de palcer
#       la contribution du terme de Biot initilal
#       dans les flux externes du debut du pas
print 'UPDATE THERM BULK'
wrap_thermailx.updatethermbulk()

# on fixe les parametres de la boucle de point fixe
# la tolerance :
tol = 1.e-3
# le nombre maximum d'iterations :
nb_iter_max = 100

# boucle en temps
#for k in xrange(1, 4601, 1):
for k in xrange(1, 2, 1):
   #
   print 'increment : ',k
   #
   print 'INCREMENT STEP (GLOBAL)'
   wrap_overall.incrementstep()
   
   print 'DISPLAY TIMES'
   wrap_overall.displaytimes()

   ### initialisation de la boucle de point fixe sur la vitesse des 
   ### grains a la fin du pas de temps
   print 'INIT FIXED POINT ALGORITHM'
   wrap_ggcxx.initfixedpointalgorithm()

   l = 1
   
   ### debut de la boucle de point fixe
   while True:
      print 'increment (fixed point algo) : ', l

      ### on stocke l'approximation des vitesses a la fin du pas courante
      ### dans V et on clacul les nouvelles positions ad hoc dans X
      print 'INCREMENT FIXED POINT ALGORITHM'
      wrap_ggcxx.incrementfixedpointalgorithm()

      ### calcul des champs moyennes aux noeuds et des termes de couplage
      ### pour le calcul de la nouvelle pression

      # recherche de quel element contient quel noeud
      # et calcul des valeurs des fonctions de formes
      print 'GRAINS IN MESH'
      wrap_ggcxx.grainsinmesh()

      # ATTENTION: on desactive le calcul des chaps moyennes
      #            pour que le calcul thermique ne soit pas influence
      #            par le clacul granulaire, avant que le couplage soit
      #            en place!!!

      # calcul des champs moyennes aux noeuds : 
      # porosite et vitesse barycentrique des grains
      print 'COMPUTE AVERAGE NODE FIELDS'
      wrap_ggcxx.computeaveragenodefields()

      # on depose les champs utilises par le couplage
      #  * la porosite dans SPHV
      #  * le coefficient de diffusion dans COCO
      print 'SET GAS GRAINS COUPLING FIELDS'
      wrap_ggcxx.setgasgrainsgouplingfields()

      # calcul du terme source corepsondant au terme de Biot
      print 'COMPUTE BIOT TERM'   
      wrap_ggcxx.computeelementarybiot()

      # on initialise le calcul de la pression pour une nouvelle iteration 
      # du point fixe
      print 'INCREMENT STEP (PRESSION)'
      wrap_thermailx.incrementstep()
   
      # on ajoute la contribution du terme de Biot
      # aux flux externes, par element
      print 'ADD BIOT THERM TO EXTERNAL FLUX BY ELEMENT'
      wrap_ggcxx.addbiottoexternalfluxbyelement()

      ### calcul de la nouvelle pression au travers d'un calcul de thermique
      ### (dans lequel on a incorpore les termes de couplage)

      # calcul des flux nodaux imposes
      print 'COMPUTE EXTERNAL FLUX'
      wrap_thermailx.computeexternalflux()
   
      # calcul de la capacite (ie masse)
      print 'COMPUTE CAPACITY'
      wrap_thermailx.computecapacity()
  
      # calcul de la conductivite
      print 'COMPUTE CONDUCTIVITY'
      wrap_thermailx.computeconductivity()
   
      # calcul du second membre (=0 ici)
      print 'COMPUTE INTERNAL FLUX'
      wrap_thermailx.computeinternalflux()
   
      # assemblage du second membre
      print 'ASSEMB THERM RHS'
      wrap_thermailx.assembthermrhs()
   
      # assemblage de la matrice des iterations
      print 'ASSEMB THERM KT'
      wrap_thermailx.assembthermkt()

      # resolution, calcul  de la temperature
      print 'COMPUTE THERM DOF'
      wrap_thermailx.computethermdof()
   
      # affichage
      #print 'DISPLAY OUT DOF'
      #wrap_thermailx.displayoutdof()

      ### calcul de la force hydrodynamiue appliquee aux grains, a la fin
      ### du pas

      # calcul des forces hydrodynamiques appliquees
      # sur les grains (a la fin du pas)
      print 'COMPUTE HYDRODYNAMIC FORCE'
      wrap_ggcxx.computehydrodynamicforce()

      ### calcul du nouvel etat d'equilibre a la fin du pas de temps
   
      # on initialise le calcul de la cinematique des grains pour une nouvelle iteration 
      # du point fixe
      print 'INCREMENT STEP (GRAINS)'
      wrap_rbdy2.incrementstep()
      
      # on calcule les forces exterieures 
      print 'COMPUTE Fext'
      wrap_rbdy2.computefext()
      
      # on leur ajoute la contibution des forces hydrodynamiques
      print 'SET HYDRODYNAMIC FORCE BY GRAIN'
      wrap_ggcxx.sethydrodynamicforcebygrain()
   
      # on clacule les forces interieures (ici =0)
      print 'COMPUTE Fint'
      wrap_rbdy2.computefint()
      
      # on calcule la vitesse libre
      print 'COMPUTE Free Vlocy'
      wrap_rbdy2.computefreevelocity()
      #
      # on fait la detetcion de contacts
      print 'SELECT PROX TACTORS'
      wrap_overall.selectproxtactors(1)
      wrap_dkdkx.selectproxtactors()
      wrap_dkjcx.selectproxtactors()
      #
      # on resoud le contact
      wrap_dkdkx.recuprloc()
      wrap_dkjcx.recuprloc()
      wrap_nlgs.exsolver('Stored_Delassus_Loops         ','Quad',0.1666e-3,1.,50,1000)
      wrap_dkdkx.stockrloc()
      wrap_dkjcx.stockrloc()
      #
      # on calcule la nouvelle cinematique des grains
      print 'COMPUTE DOF'
      wrap_rbdy2.computedof()
      #
      ### on test si on l'etat d'equilibre a ete atteint
      if wrap_ggcxx.checkfixedpointalgorithmconvergence('Vlocy', tol) or l >= nb_iter_max:

         # si on a converge, ou on a atteint le nombre maximum d'iterations,
		 # on quitte la boucle de point fixe
         break
      
      # si on n'a pas converge, on met a jour l'approximation de la 
      # vitesse des grains a la fin du pas de temps
      wrap_ggcxx.updatefixedpointalgorithm()
      
      # on incremente l'itere
      l = l + 1  

   # on ecrit le nombre d'iterations effectuees par l'algorithme de
   # point fixe dans un fichier
   
   # on ouvre le fichier pour stocker le nombre d'iterations effectuees par le point fixe
   # de couplage
   if k == 1:
      f_couplage=open('OUTBOX/COUPLAGE_ITERATIONS.DAT', 'w') 	
   else:
      f_couplage=open('OUTBOX/COUPLAGE_ITERATIONS.DAT', 'a')	

   # on ecrit le nombre d'iterations effectuees par le point fixe de couplage dans le fichier
   # 'OUTBOX/COUPLAGE_ITERATIONS.DAT'
   f_couplage.write("%14.7lf %7d\n" % (wrap_overall.gettime(), l))

   # on ferme le fichier 'OUTBOX/COUPLAGE_ITERATIONS.DAT'
   f_couplage.close()	    

   # si on a pas converge
   if l == nb_iter_max:
         
      # on affiche un message d'erreur
      print 'Error: Fixed point algorithm did not convegerd!'
            
      # on quitte le programme
      sys.exit(-1)
          
   # si on a converge, on affiche un message
   print 'Fixed point algorithm converged in :', l, ' itertions' 		    

   # mise a jour de la cinematique des grains
   print 'UPDATE DOF'
   wrap_overall.updatedof()
   wrap_rbdy2.updatedof()
   #

   ### calcul des champ moyennes dans la configuration a la fin du pas 
   ### de temps
   	 
   # recherche de quel element contient quel noeud
   # et calcul des valeurs des fonctions de formes
   print 'GRAINS IN MESH'
   wrap_ggcxx.grainsinmesh()

   # calcul des champs moyennes aux noeuds : 
   # porosite et vitesse barycentrique des grains
   print 'COMPUTE AVERAGE NODE FIELDS'
   wrap_ggcxx.computeaveragenodefields()
              
   ### mise a jour des champs pour les termes de couplage
   
   # mise a jour des champs moyennes aux noeuds
   
   print 'UPDATE FIELDS' 
   wrap_ggcxx.updatefields()

   ### mise a jour des champs pour le calcul de la nouvelle pression

   # mise a jour des valeurs aux noeuds
   print 'UPDATE THERM DOF'
   wrap_thermailx.updatethermdof()
   
   # calcul des flux, grad et valeurs aux pg
   print 'UPDATE THERM BULK'
   wrap_thermailx.updatethermbulk()

   # ecriture des temperatures aux noeuds, calculees precedemment
   #print 'WRITE LAST DOF'
   #wrap_overall.writelastdof()
   #wrap_thermailx.writelastdof()
   #wrap_rbdy2.writelastdof()
   
   # ecriture des temperatures aux noeuds et deplacements/vitesses des
   # grains, tous les 50 pas
   print 'WRITE OUT DOF'
   wrap_overall.writeoutdof(100)
   wrap_thermailx.writeoutdof()
   wrap_rbdy2.writeoutdof()
   
   # ecriture des valeurs aux points de gauss calculees precedemment
   #print 'WRITE LAST GPV'
   #wrap_overall.writelastgpv()
   #wrap_mailx.writelastgpv()
   
   # ecriture des valeurs aux points de gauss, tous les 50 pas
   print 'WRITE OUT GPV'
   wrap_overall.writeoutgpv(100)
   wrap_mailx.writeoutgpv()
   
   # ecriture des vitesses locales/reperes locaux de contact
   #print 'WRITE LAST Vloc Rloc'
   #wrap_overall.writelastvlocrloc()
   #wrap_dkdkx.writelastvlocrloc()
   #wrap_dkjcx.writelastvlocrloc()
   
   # ecriture des vitesses locales/reperes locaux de contact, tous les
   # 50 pas
   print 'WRITE OUT Vloc Rloc'
   wrap_overall.writeoutvlocrloc(100)
   wrap_dkdkx.writeoutvlocrloc()
   wrap_dkjcx.writeoutvlocrloc()
   
   ### post2D ###
   
   # sortie gmv tous les 50 pas
   wrap_overall.writeoutgmv(50)
   wrap_post2d.writeoutgmv(50)
   
   ### gestion des writeout ###
   wrap_overall.cleanwriteoutflags()

# liberation memoire pour le couplage gaz-grains
wrap_ggcxx.deletegasgrainscoupling()
