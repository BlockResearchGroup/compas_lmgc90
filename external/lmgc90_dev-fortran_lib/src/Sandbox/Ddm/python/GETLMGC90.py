
# --------------------------------------------------------------------
# ICETA Damien      L.M.G.C. SYSTEMES MULTICONTACTS  le 09 / 11 / 2008
# transposition en python de la routine ecrite en matlab par 
# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 22 / 01 / 2003
# avec pour l'instant une seule lecture de fichier (1 passe par fichier) 
# --------------------------------------------------------------------

# ===========================> INTRO <================================

# Lecture de 1 passe dans un fichier AVS 


from numpy import * 
#import scipy as Sci
#import scipy.linalg
from dam_class import *
import os,sys
sys.path.append('/home0/iceta0/LMGC90/LMGC90v2_dev/Sandbox/Ddm/ChiPy_src')
from ChiPy import *







#print 'test'


















def GETLMGC90():


	info = wrap_avsxx.dgetinfo()
	

	# ligne d entete
	#ligne decoupe est la liste des termes de notre ligne
	num_nodes = int(info[0]); # number of nodes
	num_cells = int(info[1]); # number of cells
	num_ndata = int(info[2]); # length of the data associated with nodes
	num_cdata = int(info[3]); # length of the data associated with cells
	num_mdata = int(info[4]); # length of the data associated with the model

	
	#print 'num_nodes' , num_nodes
	#print 'num_cells' , num_cells
	#print 'num_ndata' , num_ndata
	#print 'num_cdata' , num_cdata
	#print 'num_mdata' , num_mdata







	# ---------------> lecture des coordonees des noeuds <----------------
	
	# creation d'une matrice coor
	# dont les numeros de ligne sont les numers de corps et les trois colones
	# correpondent au trois coordonnees dans l'espace, la troisiemeetant nulle en 2D)

	for i in r_[1:num_nodes+1]:
		
		COOR=wrap_avsxx.dgetcoor(i)
		
		if (i==int(COOR[0])):
			x,y,r = float(COOR[1]),float(COOR[2]),float(COOR[3])	
		else:
			print 'probleme d indice ligne 55 de get lmgc90'
			raw_input()		
			
		#print 'testi',i		
		if (i==1):
			a = array([x,y,r])
			#print a
			xcoor = vstack([a])
			#print 'coucou1'
		else:
			a = array([x,y,r])
			xcoor = vstack([xcoor,a])
			#print 'coucou2'
		
	##print xcoor  # ici array avec les coordonees
	xcoor = mat(xcoor.copy()) # ecriture avec la classe matrice de numpy
	##print xcoor[:,1]
	# on a ici la matrice des coordonees
	
	
	
	
	
	
	
	
	
	
	
	# ----------------> lecture du maillage <--------------
	# on recherche le graphe de conectivite des noeuds
	# Boucle sur les maillages des elements
	nb_zone1 = 1;  #moi ce qui m interesse ici ce n est travailler
	# qu avec des segments donc une seule zone
	

	for i in r_[1:num_cells+1]:
		
		DMAIL=wrap_avsxx.dgetmaillage(i)
		
		cell_id = int(DMAIL[0])			# numero de l element
		mat_id  = int(1)			# identifiant du materiau
		cell_type = 'line'			# type d element
		candidat = int(DMAIL[1])			# numero de l element
		antagoniste = int(DMAIL[2])			# numero de l element
		
		if (cell_id <> i):
			print 'probleme d indice ligne 100 de get lmgc90'
			raw_input()
		
		

		#if (str(cell_type) <> 'ligne')
		#	#print 'erreur seul les segment sont implementes comme type d elements'
		
		if (i==1):
			a = array([candidat,antagoniste])
			matrice_contactes = vstack([a])
		else:
			a = array([candidat,antagoniste])
			matrice_contactes = vstack([matrice_contactes,a])
		
	matrice_contactes = mat(matrice_contactes.copy()) # ecriture avec la classe matrice de numpy
	
	#print 'matrice_contactes',matrice_contactes
	#raw_input()
	#ecriture sous forme d un objet representatif d un maillage
	#from dam_class import *
	ListMesh1 = MAIL_ELMT_FINI()      # instanciation de l objet ob
	ListMesh1.TYPE = 'SEG2'	
	ListMesh1.MAIL = matrice_contactes	
	##print ListMesh1.MAIL
	
	# ----------------> lecture du champ par points <--------------
	#   chpo1 : champ par point (a 1 seule sous-zone par limitation d'AVS)
	#     chpo1{1}{i}.COMP : nom de la composante numero i
	#     chpo1{1}{i}.UNIT : nom de l'unite de la composante numero i
	#     chpo1{1}{i}.XVAL(nbno,nbval) : valeurs
	
	
	nombre_de_composantes = num_ndata
	
	# A la main pour l instant c'est pas beau:
	liste_nom_composantes = ['RAYO','MASS','VX','VY','VIX','VIY','VDX','VDY','FDX','FDY']
		
	# important il va falloir choisir parmis ces methodes d ecriture
	champ1 = []
	champ2 = []
	champ1bis = {}
	champ2bis = {}
	
	#chpo1 = UN_CHAMP(nom_composante,'unitee',vecteur)      # instanciation de l objet ob		
	
	for i in r_[0:num_ndata]:
		
		# sur le format de la classe UN_CHAMP on va creer autant d objet champ qu il n y a de composantes
		# et leur atribuer le nom de composante


		
		#lignedecoupe = line.split(',')  #decoupage d une chaine separee par des virgules
		#mais ici ca met des espaces dans les noms
		#donc
		#line = line.replace(',',' ') # je remplace les virgules par des espaces
		#lignedecoupe = line.split()   # je decoupe avec le separateur espace
		
		nom_composante = str(liste_nom_composantes[i])
	
		#print 'nom_composante',nom_composante
		vecteur = []
		chpo1 = UN_CHAMP(nom_composante,'unitee',vecteur)      # instanciation de l objet ob		
		#chpo1.affiche() #ne pas enlever cetteligne meme si aucune explication pour la garder
		champ1.append(chpo1)
		
		
		# test de la methode utilisant les dictionaires (fonctionne tout aussi bien)
		chpo1 = {}
		chpo2 = {}      # dictionnaire vide
		chpo2['COMP'] = nom_composante
		chpo2['UNIT'] = 'unitee'
		chpo2['XVAL'] = vecteur
		##print chpo2     # {'I':'je', 'she':'vous', 'you':'vous'}
		##print chpo2['I'] # 'je'
		#del chpo2['I']
		##print chpo2     # {'she':'vous', 'you':'vous'}
		#chpo2['we'] = 'nous'
  		chpo2.keys()           # ['we', 'she', 'you']
  		chpo2.values()         # ['nous', 'elle', 'vous']
  		chpo2.items()          # [('we','nous'), ('she','elle'), ('you','      vous')]
  		#chpo2.has_key('I')     # False
  		#chpo2.has_key('we')    # True
		
		champ2.append(chpo2)
		# ici pour bien utiliser les dictionnaires plutot que de creer une liste faudrait
		# remetre une couche de dico par dessus avec le nom de la variable chorespondante (plus facile pour relire apres)
		champ1bis[nom_composante] = chpo1
		champ2bis[nom_composante] = chpo2
		
	#les resultats pour ces methodes sont:

	### fd & di champ1[1].affiche()

	#print 'test23'
	##print champ1[1].XVAL[1]
	##print champ2[1]
	##print champ2[1]['COMP']
	##print champ1bis
	##print champ2bis
	#champ1bis['RAYO,'].affiche()
	##print champ2bis['RAYO,']['COMP']
	# en conclusion il parait difficile de melanger les methodes cf champ1bis
	# choix tres arbitraire de garder la structure avec les objet classe UN_CHAMP
	
	
	
	
		
	for i in r_[1:num_nodes+1]: #parcour tous les noeuds

		DDONNEESG=wrap_avsxx.dgetdonneesgrain(i)

		#print 'DDONNEESG',DDONNEESG







		node_id = int(DDONNEESG[0])			# numero du noeud
		
		if (node_id<>i):
			print 'erreur d indice dans getlmgc90','node_id',node_id,'i',i
		
		for j in r_[0:nombre_de_composantes]: #parcour toutes les composantes
			valeur  = float(DDONNEESG[j])
			champ1[j-1].XVAL = hstack([champ1[j-1].XVAL,valeur])
			##print 'fin'
	#print champ1[3].XVAL[1]
	#print champ1[3].XVAL
	#print champ1[3]
	
	#verification que le nombre de noeuds chorrespond bien
	#print shape(champ1[1].XVAL)
	##print num_nodes
	##print r_[0:10]
	##print shape(r_[0:10])
	
	
	
	
	
	
	
	
	
	# ----------------> lecture du champ par element <--------------
	#   chpo1 : champ par point (a 1 seule sous-zone par limitation d'AVS)
	#     chpo1{1}{i}.COMP : nom de la composante numero i
	#     chpo1{1}{i}.UNIT : nom de l'unite de la composante numero i
	#     chpo1{1}{i}.XVAL(nbno,nbval) : valeurs
	
	
	nombre_de_composantes = num_cdata
	
	# A la main pour l instant c'est pas beau:
	liste_nom_composantes2 = ['FN','VN','SDM','GAP']
	

	# important il va falloir choisir parmis ces methodes d ecriture
	champelment1 = []
	champelment2 = []
	champelment1bis = {}
	champelment2bis = {}
	
	for i in r_[0:nombre_de_composantes]:
		
		# sur le format de la classe UN_CHAMP on va creer autant d objet champ qu il n y a de composantes
		# et leur atribuer le nom de composante

		
		nom_composante = str(liste_nom_composantes2[i])
		#print 'nom_composante',nom_composante
		vecteur = []
		chpoelment1 = UN_CHAMP(nom_composante,'unitee',vecteur)      # instanciation de l objet ob
		### fd & di chpoelment1.affiche() 
		champelment1.append(chpoelment1)
		
		# test de la methode utilisant les dictionaires (fonctionne tout aussi bien)
		chpoelment1 = {}
		chpoelment2 = {}      # dictionnaire vide
		chpoelment2['COMP'] = nom_composante
		chpoelment2['UNIT'] = 'unitee'
		chpoelment2['XVAL'] = vecteur
		##print chpo2     # {'I':'je', 'she':'vous', 'you':'vous'}
		##print chpo2['I'] # 'je'
		#del chpo2['I']
		##print chpo2     # {'she':'vous', 'you':'vous'}
		#chpo2['we'] = 'nous'
  		chpoelment2.keys()           # ['we', 'she', 'you']
  		chpoelment2.values()         # ['nous', 'elle', 'vous']
  		chpoelment2.items()          # [('we','nous'), ('she','elle'), ('you','      vous')]
  		#chpo2.has_key('I')     # False
  		#chpo2.has_key('we')    # True
		
		champelment2.append(chpoelment2)
		# ici pour bien utiliser les dictionnaires plutot que de creer une liste faudrait
		# remetre une couche de dico par dessus avec le nom de la variable chorespondante (plus facile pour relire apres)
		champelment1bis[nom_composante] = chpoelment1
		champelment2bis[nom_composante] = chpoelment2
		
	#les resultats pour ces methodes sont:
	### fd & di champelment1[0].affiche()
	#print 'test23'
	##print champ1[1].XVAL[1]
	##print champ2[1]
	##print champ2[1]['COMP']
	##print champ1bis
	##print champ2bis
	#champ1bis['RAYO,'].affiche()
	##print champ2bis['RAYO,']['COMP']
	# en conclusion il parait difficile de melanger les methodes cf champ1bis
	# choix tres arbitraire de garder la structure avec les objet classe UN_CHAMP
	
	# Restera a enlever les virgules des noms de composantes
	# chaine[:-1]
	





	for i in r_[1:num_cells+1]: 

		DDONECONTA = wrap_avsxx.dgetdonneescontact(i)
		
		#print 'numero sdm',str(DDONECONTA[3])

		cell_id = int(DDONECONTA[0])			
		
		if (i <> cell_id):
			print 'erreur d indice dans getlmgc90 ligne 312'
			raw_input()
		
		
		for j in r_[0:nombre_de_composantes]: #parcour toutes les composantes
			valeur  = float(DDONECONTA[j])
			champelment1[j-1].XVAL = hstack([champelment1[j-1].XVAL,valeur])
			##print 'fin'
	#print champelment1[3].XVAL[1]
	#print champelment1[3].XVAL
	#print champelment1[3]
	
	#verification que le nombre de noeuds chorrespond bien
	#print shape(champelment1[1].XVAL)
	##print num_nodes
	##print r_[0:10]
	##print shape(r_[0:10])
	
	
	
	#   chml : champ constant par element
	#     chml{i}.COMP : nom de la composante numero i
	#     chml{i}.UNIT : nom de l'unite de la composante numero i
	#     chml{i}.XVAL(nbel,nbval) : valeurs
	#   cara : champ constant de caracteristiques (modele selon AVS)
	#     cara{i}.COMP : nom de la composante numero i
	#     cara{i}.UNIT : nom de l'unite de la composante numero i
	#     cara{i}.XVAL(nbel,nbval) : valeurs
	
	print 'fin de recuperation des donnees lmgc90'
	return  xcoor,matrice_contactes,ListMesh1,champ1,champelment1
