import os
from numpy import * 
#import scipy as Sci
#import scipy.linalg
from dam_class import *


def Difference_Listes(liste1,liste2):
	#Creer la liste des element de liste1 n appartenant pas a liste2
	
	# a optimiser
	# attention a l ordre (ici on trouve les element de 1 pas dans 2 et on ne fait pas l inverse)
	liste_neutrinos = []
	
	#on enleve les valeurs doubles qu'il peut y avoir dans liste2
	liste2=unique(liste2)  
	
	
	liste_neutrinos = setdiff1d(liste1, liste2) 
	
	
	
	#a = set(liste1)
	#b = set(liste2)
	#c=a.difference(b)
	#liste_neutrinos = mat(c)
			
	##par defaut
	#neutrinos='vrai'
	## On va tester chacun des elements de la liste1 et voir si il appartient a la deuxieme
	#for i in liste1: # on parcour la liste1
	#	for j in liste2: # on parcour la liste2
	#		if (i==j):
	#			# alors ce n est pas un neutrinos
	#			neutrinos='faux' # on l a trouve dans liste2
	#			break # on sort de la boucle de test a la premiere valeur commune
	#			
	#	# on vient de tester toutes les valeurs
	#	if (neutrinos=='vrai'):	#si on ne l a pas trouve
	#		liste_neutrinos = hstack([liste_neutrinos,i])
	#	else:
	#		neutrinos='vrai'
		
	return liste_neutrinos

def Intersection_Listes(liste1,liste2):
	Liste_1pasdans2 = Difference_Listes(liste1,liste2)
	Liste_1pasdansListe_1pasdans2=Difference_Listes(liste1,Liste_1pasdans2)
	return Liste_1pasdansListe_1pasdans2



def Findoccurences(val,list_val):
	# trouve pour chacun des termes de val le premier indice ou il apparait dans list_val
	#function [ind] = findoccur(val,list_val)


	#optimiser avec
	#bincount(x,weights=None)
	#Returns the number of occurrences of each value in x. 






	dim1 = shape(val)[0]				#taille de la liste des entree
	list_dim1 = shape(list_val)[0]		#taille de la liste dans laquelles on doit trouver les occurences

	if (list_dim1 == 0):
		ind = zeros([1,dim1]);			#Si la liste des occurences possibles est vide alors chaque occurence est nulle
	else:
		ind = []
		#if iscell(list_val):		# dans matlab on fesait un test pour savoir 
									# si l on avait bien une matrice icie je sais pas encore comment le faire
		for i in val:
			#on test chacun des element de val
			compteur=-1
			for j in list_val:
				compteur = compteur + 1
				if (i==j):
					# Creation de la liste
					ind.append(compteur)
					break
	return ind	
			
			#for i = 1:dim1
			#	ind1 = 0;
			#	for j = 1:list_dim1
			#		if strcmp(val(i),list_val(j))
			#			ind1 = j;
			#			break
			#	ind(i) = ind1;
					
			#pour la suite cf la remarque sur iscell
			#else
			#for i = 1:dim1
			#ind1 = find(list_val == val(i));
			#if (length(ind1) == 1)
			#ind(i) = ind1;
			#elseif (length(ind1) == 0)
			#ind(i) = 0;
			#else
			#error('multiple occurencies')
			
def Findmultiplicite(val,list_val):
	# Trouve le nombre de fois ou l on a chacun des termes de val val dans list_val
	#function [ind] = findoccur(val,list_val)

	dim1 = shape(val)[0]				#taille de la liste des entree
	list_dim1 = shape(list_val)[0]		#taille de la liste dans laquelles on doit trouver les occurences

	if (list_dim1 == 0):
		ind = zeros([1,dim1]);			#Si la liste des occurences possibles est vide alors chaque occurence est nulle
	else:
		ind = []
		#if iscell(list_val):		# dans matlab on fesait un test pour savoir 
									# si l on avait bien une matrice icie je sais pas encore comment le faire
		for i in val:
			#on test chacun des element de val
			compteur = -1 
			for j in list_val:
				if (i==j):
					compteur = compteur + 1
					i
			# Creation de la liste
			ind.append(compteur)
	
	return ind	
