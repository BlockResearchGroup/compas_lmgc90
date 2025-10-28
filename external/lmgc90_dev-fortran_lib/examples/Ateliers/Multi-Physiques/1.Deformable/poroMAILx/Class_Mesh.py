###################################################################
# Importation de module complementaire
from __future__ import print_function
import math,os,sys,copy
# Make all numpy available via shorter 'num' prefix
import numpy as num
import scipy as sci
import string
# Make all matlib functions accessible at the top level via matlib.func()
import numpy.matlib as matlib
import numpy.linalg as linalg
from numpy.linalg import lstsq
from numpy.linalg import pinv
from numpy.linalg import solve
from numpy.linalg import eig
# Make some matlib functions accessible directly at the top level via, e.g. rand(3,3)
from numpy.matlib import rand,zeros,ones,empty,eye
#~ import pylab
import time

Keywords=['$NOD\r\n','$ELM\r\n','$ENDNOD\r\n']
KeywordsAlt=['$NOD\n','$ELM\n','$ENDNOD\n']
Keywords2=['$Nodes\r\n','$Elements\r\n','$EndNodes\r\n']
KeywordsAlt2=['$Nodes\n','$Elements\n','$EndNodes\n']


def delete_node(node, prec = 1.0e-06):
	"Suppression des noeuds en double a une precision donne prec"
	nb_node_init = node.shape[0]
	
	new_node = [node[0]]
	
	for i in range(node.shape[0]-1):
		
		if node.shape[1] >= 1 : 
			dx = (num.array(new_node)[:,0] - node[i,0])
			dist = dx
		if node.shape[1] >= 2 : 
			dy = (num.array(new_node)[:,1] - node[i,1])
			dist = num.sqrt(dx**2 + dy**2)
		if node.shape[1] >= 3 : 
			dz = (num.array(new_node)[:,2] - node[i,2])
			dist = num.sqrt(dx**2 + dy**2 + dz**2)
		
		if num.prod((dist>=prec)) == 1 :
			new_node.append(node[i,:])
	
	new_node = num.array(new_node)
	
	nb_node_final = new_node.shape[0]
	
	print('--------------------------------------------------------------------')
	print('	Le systeme a trouve ',nb_node_init-nb_node_final,' noeud(s) surabondant')
	print('		pour une precision ',prec)
	print('	Nombre de noeuds initial : ',nb_node_init)
	print('	Nombre de noeuds final : ',nb_node_final)
	print('--------------------------------------------------------------------')
	
	return new_node

def ordonne_node(node):
	"Ordonne la liste des noeuds"
	
	Ordonne_node  = num.zeros(node .shape,float)
	Ordonne_node [0,:] = node [0,:]
	
	for i in range(node.shape[0]-1):
		                
		if node.shape[1] >= 1 : 
			dx = (node[1:,0] - Ordonne_node [i,0])
			dist = dx
		if node.shape[1] >= 2 : 
			dy = (node[1:,1] - Ordonne_node [i,1])
			dist = num.sqrt(dx**2 + dy**2)
		if node.shape[1] >= 3 : 
			dz = (node[1:,2] - Ordonne_node [i,2])
			dist = num.sqrt(dx**2 + dy**2 + dz**2)
		                
		ind = (dist == num.min(dist)).nonzero()[0][0] + 1
		Ordonne_node [i+1,:] = node [ind,:]
		A = node.tolist()
		A.remove(A[ind])
		node  = num.array(A)
		
	return Ordonne_node

class gmsh():
	
	"Classe de fonction pour travailler les fichier gmsh"
		
	def __init__(self, path = 'Not defined', file_in = 'Not defined', file_out = 'Not defined'):
		"Initialisation de la classe"
		print('--------------------------------------------------------------------')
		print("	Lecture du fichier de maillage initial : ",file_in)
		print('--------------------------------------------------------------------')
		
		if path == 'Not defined':
			path = os.getcwd()
		self.path = path
		self.file_in  = path + os.sep + file_in
		self.file_out = path + os.sep + file_out
		self.InitVariable()
	
	def __str__(self):
		"fonction d'impression des caracteristiques"
		
		stri  = '--------------------------------------------------------------------\n'
		stri += '\tMaillage au format gmsh issu du fichier : '+self.file_in+'\n'
		stri += '\tRepertoire de travail : '+self.path+'\n'
		stri += '--------------------------------------------------------------------\n'
		stri += 'Description du maillage initial\n'
		stri += '--------------------------------------------------------------------\n'
		stri += 'Nombre de noeuds : '+str(self.node.shape[0])+'\n'
		stri += '\n'
		stri += '----- Element de type lineique -----\n'
		stri += 'Nombre d''elements Seg2: '+str(self.Seg2.shape[0])+'\n'
		stri += 'Nombre d''elements Seg3: '+str(self.Seg3.shape[0])+'\n'
		stri += '\n'
		stri += '----- Element de type surfacique -----\n'
		stri += 'Nombre d''elements Tri3: '+str(self.Tri3.shape[0])+'\n'
		stri += 'Nombre d''elements Tri6: '+str(self.Tri6.shape[0])+'\n'
		stri += 'Nombre d''elements Qua4: '+str(self.Qua4.shape[0])+'\n'
		stri += 'Nombre d''elements Qua8: '+str(self.Qua8.shape[0])+'\n'
		stri += 'Nombre d''elements Qua9: '+str(self.Qua9.shape[0])+'\n'
		stri += '\n'
		stri += '----- Element de type volumique -----\n'
		stri += 'Nombre d''elements Tet4: '+str(self.Tet4.shape[0])+'\n'
		stri += 'Nombre d''elements Tet6: '+str(self.Tet6.shape[0])+'\n'
		stri += 'Nombre d''elements Tet10: '+str(self.Tet10.shape[0])+'\n'
		stri += 'Nombre d''elements Cub8: '+str(self.Cub8.shape[0])+'\n'
		stri += 'Nombre d''elements Cub20: '+str(self.Cub20.shape[0])+'\n'
		stri += '\n'
		stri += '--------------------------------------------------------------------\n'
		stri += 'Description du maillage en memoire ou final\n'
		stri += '--------------------------------------------------------------------\n'
		stri += 'Nombre de noeuds : '+str(self.new_node.shape[0])+'\n'
		stri += '\n'
		stri += '----- Element de type ponctuel -----\n'
		stri += 'Nombre d''elements Nod1: '+str(self.new_Nod1.shape[0])+'\n'
		stri += '----- Element de type lineique -----\n'
		stri += 'Nombre d''elements Seg2: '+str(self.new_Seg2.shape[0])+'\n'
		stri += 'Nombre d''elements Seg3: '+str(self.new_Seg3.shape[0])+'\n'
		stri += '\n'
		stri += '----- Element de type surfacique -----\n'
		stri += 'Nombre d''elements Tri3: '+str(self.new_Tri3.shape[0])+'\n'
		stri += 'Nombre d''elements Tri6: '+str(self.new_Tri6.shape[0])+'\n'
		stri += 'Nombre d''elements Qua4: '+str(self.new_Qua4.shape[0])+'\n'
		stri += 'Nombre d''elements Qua8: '+str(self.new_Qua8.shape[0])+'\n'
		stri += 'Nombre d''elements Qua9: '+str(self.new_Qua9.shape[0])+'\n'
		stri += '\n'
		stri += '----- Element de type volumique -----\n'
		stri += 'Nombre d''elements Tet4: '+str(self.new_Tet4.shape[0])+'\n'
		stri += 'Nombre d''elements Tet6: '+str(self.new_Tet6.shape[0])+'\n'
		stri += 'Nombre d''elements Tet10: '+str(self.new_Tet10.shape[0])+'\n'
		stri += 'Nombre d''elements Cub8: '+str(self.new_Cub8.shape[0])+'\n'
		stri += 'Nombre d''elements Cub20: '+str(self.new_Cub20.shape[0])+'\n'
		stri += '\n'
		stri += '--------------------------------------------------------------------\n'
		
		return stri
	
	def InitVariable(self):
		"Initialisation des variables"
		
		self.new_node = num.zeros((0,4),float)
		
		self.new_Nod1 = num.zeros((0,3),int)
		self.new_Seg2 = num.zeros((0,4),int)
		self.new_Tri3 = num.zeros((0,5),int)
		self.new_Tri6 = num.zeros((0,8),int)
		self.new_Qua4 = num.zeros((0,6),int)
		self.new_Qua9 = num.zeros((0,11),int)
		self.new_Seg3 = num.zeros((0,5),int)
		self.new_Qua8 = num.zeros((0,10),int)
		self.new_Cub20 = num.zeros((0,22),int)
		self.new_Cub8 = num.zeros((0,10),int)
		self.new_Tet4 = num.zeros((0,6),int)
		self.new_Tet10 = num.zeros((0,12),int)
		self.new_Tet6 = num.zeros((0,8),int)
		
		self.physical = 0

	def ReadNodes(self,scale_x = 1., scale_y = 1., scale_z = 1.):
		"Lecture des noeuds de gmsh"
		
		print('--------------------------------------------------------------------')
		print("	Reading nodes : Please wait ...")
		print('--------------------------------------------------------------------')
		
		fichierGmsh=open(self.file_in,'r')
		Gmsh = fichierGmsh.readlines()
		fichierGmsh.close
		if '$MeshFormat' in Gmsh[0]:
			self.node = self.ReadNodesV2(Gmsh)
			self.new_node = self.node
		else : 
			self.node = self.ReadNodesV1(Gmsh)
			self.new_node = self.node
		
		self.node[:,1] = self.node[:,1]*scale_x
		self.node[:,2] = self.node[:,2]*scale_y
		self.node[:,3] = self.node[:,3]*scale_z
				
		self.DX = num.max(self.node[:,1])
		self.DY = num.max(self.node[:,2]) 
		self.DZ = num.max(self.node[:,3])
		
		self.new_Nod1 = num.zeros((self.node.shape[0],3),int)
		
		self.new_Nod1[:,0] = self.node[:,0]
		self.new_Nod1[:,1] = num.ones(self.node.shape[0])*(self.physical + 1)
		self.new_Nod1[:,2] = self.node[:,1]
		

	def ReadNodesV1(self,Gmsh):
		try:
			IndNodeD=Gmsh.index(Keywords[0])
		except ValueError:
			IndNodeD=Gmsh.index(KeywordsAlt[0])
		try:
			IndNodeF=Gmsh.index(Keywords[2])
		except ValueError:
			IndNodeF=Gmsh.index(KeywordsAlt[2])     
			
		NbrNode = IndNodeF-IndNodeD-2
		Nodes = num.zeros((NbrNode,4),float)
		for i in range(NbrNode):
			line=string.split(Gmsh[IndNodeD+2+i])
			Nodes[i,:]=[int(line[0]),float(line[1]),float(line[2]),float(line[3])] 
		return Nodes

	def ReadNodesV2(self,Gmsh):
		try:
			IndNodeD=Gmsh.index(Keywords2[0])
		except ValueError:
			IndNodeD=Gmsh.index(KeywordsAlt2[0])
		try:
			IndNodeF=Gmsh.index(Keywords2[2])
		except ValueError:
			IndNodeF=Gmsh.index(KeywordsAlt2[2])
			
		NbrNode = IndNodeF-IndNodeD-2
		Nodes = num.zeros((NbrNode,4),float)
		for i in range(NbrNode):
			line=string.split(Gmsh[IndNodeD+2+i])
			Nodes[i,:]=[int(line[0]),float(line[1]),float(line[2]),float(line[3])]  
		return Nodes
		
	def ReadElement(self):
		"Lecture des elements de gmsh"
		
		print('--------------------------------------------------------------------')
		print("	Reading elements : Please wait ...")
		print('--------------------------------------------------------------------')
		
		fichierGmsh=open(self.file_in,'r')
		Gmsh = fichierGmsh.readlines()
		fichierGmsh.close
		if '$MeshFormat' in Gmsh[0]:    
			Connec3DT6,Connec3DC20,Connec3DC8,Connec3DH10,Connec3DH4,Connec2DQ9,Connec2DQ8,Connec2DQ4,Connec2DT6,Connec2DT3,Connec1DL3,Connec1DL2 = self.ReadElementV2(Gmsh)
			self.Seg2 = Connec1DL2
			self.Seg3 = Connec1DL3
			self.Tri3 = Connec2DT3
			self.Tri6 = Connec2DT6
			self.Qua4 = Connec2DQ4
			self.Qua8 = Connec2DQ8
			self.Qua9 = Connec2DQ8
			self.Tet4 = Connec3DH4
			self.Tet10 = Connec3DH10
			self.Cub8 = Connec3DC8
			self.Cub20 = Connec3DC20
			self.Tet6 = Connec3DT6
			self.new_Seg2 = self.Seg2
			self.new_Seg3 = self.Seg3
			self.new_Tri3 = self.Tri3
			self.new_Tri6 = self.Tri6
			self.new_Tet4 = self.Tet4
			self.new_Tet10 = self.Tet10
			self.new_Cub8 = self.Cub8
			self.new_Qua4 = self.Qua4
			self.new_Qua9 = self.Qua9
			self.new_Qua8 = self.Qua8
			self.new_Cub20 = self.Cub20
			self.new_Tet6 = self.Tet6
			
			
		else :
			Connec3DC20,Connec3DC8,Connec3DH10,Connec3DH4,Connec2DQ9,Connec2DQ8,Connec2DQ4,Connec2DT6,Connec2DT3,Connec1DL3,Connec1DL2 = self.ReadElementV1(Gmsh)
			self.Seg2 = Connec1DL2
			self.Seg3 = Connec1DL3
			self.Tri3 = Connec2DT3
			self.Tri6 = Connec2DT6
			self.Qua4 = Connec2DQ4
			self.Qua8 = Connec2DQ8
			self.Qua9 = Connec2DQ8
			self.Tet4 = Connec3DH4
			self.Tet10 = Connec3DH10
			self.Cub8 = Connec3DC8
			self.Cub20 = Connec3DC20
			self.new_Seg2 = self.Seg2
			self.new_Seg3 = self.Seg3
			self.new_Tri3 = self.Tri3
			self.new_Tri6 = self.Tri6
			self.new_Tet4 = self.Tet4
			self.new_Tet10 = self.Tet10
			self.new_Cub8 = self.Cub8
			self.new_Qua4 = self.Qua4
			self.new_Qua9 = self.Qua9
			self.new_Qua8 = self.Qua8
			self.new_Cub20 = self.Cub20
		
		S2= 0
		S3= 0
		T3= 0
		T6= 0
		Q4= 0
		Q8= 0
		Q9= 0
		H4= 0
		H10= 0
		C8= 0
		C20 = 0
		P6 = 0
		
		nb_physical_group = 0
		self.Physical_group={}
		
		if self.Seg2.shape[0]!=0 : 
			S2 = num.max(self.Seg2[:,1])
			Gr = self.get_value_in_array(Vec = self.Seg2[:,1])
			self.Physical_group['Seg2'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Seg3.shape[0]!=0 : 
			S3 = num.max(self.Seg3[:,1])
			Gr = self.get_value_in_array(Vec = self.Seg3[:,1])
			self.Physical_group['Seg3'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Tri3.shape[0]!=0 : 
			T3 = num.max(self.Tri3[:,1])
			Gr = self.get_value_in_array(Vec = self.Tri3[:,1])
			self.Physical_group['Tri3'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Tri6.shape[0]!=0 : 
			T6 = num.max(self.Tri6[:,1])
			Gr = self.get_value_in_array(Vec = self.Tri6[:,1])
			self.Physical_group['Tri6'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Qua4.shape[0]!=0 : 
			Q4 = num.max(self.Qua4[:,1])
			Gr = self.get_value_in_array(Vec = self.Qua4[:,1])
			self.Physical_group['Qua4'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Qua8.shape[0]!=0 : 
			Q8 = num.max(self.Qua8[:,1])
			Gr = self.get_value_in_array(Vec = self.Qua8[:,1])
			self.Physical_group['Qua8'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Qua9.shape[0]!=0 : 
			Q9 = num.max(self.Qua9[:,1])
			Gr = self.get_value_in_array(Vec = self.Qua9[:,1])
			self.Physical_group['Qua9'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Tet4.shape[0]!=0 : 
			H4 = num.max(self.Tet4[:,1])
			Gr = self.get_value_in_array(Vec = self.Tet4[:,1])
			self.Physical_group['Tet4'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Tet10.shape[0]!=0 : 
			H10 = num.max(self.Tet10[:,1])
			Gr = self.get_value_in_array(Vec = self.Tet10[:,1])
			self.Physical_group['Tet10'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Cub8.shape[0]!=0 : 
			C8  = num.max(self.Cub8[:,1])
			Gr = self.get_value_in_array(Vec = self.Cub8[:,1])
			self.Physical_group['Cub8'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Cub20.shape[0]!=0 : 
			C20 = num.max(self.Cub20[:,1])
			Gr = self.get_value_in_array(Vec = self.Cub20[:,1])
			self.Physical_group['Cub20'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		if self.Tet6.shape[0]!=0 : 
			P6 = num.max(self.Tet6[:,1])
			Gr = self.get_value_in_array(Vec = self.Tet6[:,1])
			self.Physical_group['Tet6'] = Gr
			nb_physical_group = nb_physical_group + Gr.shape[0]
		
		self.physical_init = num.max(num.array([S2,S3,T3,T6,Q4,Q8,Q9,H4,H10,C8,C20,P6])) + 100
		self.physical = 0
		
		self.node_on_physical_group = {}
		self.get_node_on_physical_group(connec = self.Seg2 , mail = "Seg2")
		self.get_node_on_physical_group(connec = self.Seg3 , mail = "Seg3")
		self.get_node_on_physical_group(connec = self.Tri3 , mail = "Tri3")
		self.get_node_on_physical_group(connec = self.Tri6 , mail = "Tri6")
		self.get_node_on_physical_group(connec = self.Qua4 , mail = "Qua4")
		self.get_node_on_physical_group(connec = self.Qua8 , mail = "Qua8")
		self.get_node_on_physical_group(connec = self.Qua9 , mail = "Qua9")
		self.get_node_on_physical_group(connec = self.Tet4 , mail = "Tet4")
		self.get_node_on_physical_group(connec = self.Cub8 , mail = "Cub8")
		self.get_node_on_physical_group(connec = self.Cub20, mail = "Cub20")
		self.get_node_on_physical_group(connec = self.Tet6 , mail = "Tet6")

	def get_node_on_physical_group(self,connec = "not defined", mail = "not defined"):
		"Recuperation des noeuds d'un groupe physique"
		
		if connec.shape[0]!=0 :
			node_on_group = []
			Gr = self.Physical_group[mail]
			for i in range(Gr.shape[0]):
				elem = (connec[:,1]==Gr[i]).nonzero()[0]
				node = connec[elem,2:].reshape((connec[elem,2:].shape[0]*connec[elem,2:].shape[1]))
				self.node_on_physical_group[Gr[i]]= num.sort(self.get_value_in_array(Vec = node))

	def get_value_in_array(self,Vec):
		"Renvoie une liste des valeurs contenu dans un vecteur"
		
		value = num.array([],int)
		
		for i in range(Vec.shape[0]):
			if value.__contains__(Vec[i]):
				pass
			else :
				value = num.concatenate((value,num.array([Vec[i]])),axis = 0)
				
		return value

	def ReadElementV1(self,Gmsh):
		
		try:
			IndElem=Gmsh.index(Keywords[1])
		except ValueError:
			IndElem=Gmsh.index(KeywordsAlt[1])

		NbrElem = int(Gmsh[IndElem+1])
		TypeElem = num.zeros((NbrElem,5),int)
	
		for i in range(NbrElem):
			line=string.split(Gmsh[IndElem+2+i])
			TypeElem[i,:]=[int(line[0]),int(line[1]),int(line[2]),int(line[3]),int(line[4])]

		# Recherche des elements 1D - L2 noeuds
		Ind=(TypeElem[:,1]==1).nonzero()
		IndL2D1_i=Ind[0]
		Connec1DL2 = num.zeros((len(IndL2D1_i),4),int)
		j=0
		for i in IndL2D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec1DL2[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6])]
		
		# Recherche des elements 1D - L3 noeuds
		Ind=(TypeElem[:,1]==8).nonzero()
		IndL3D1_i=Ind[0]
		Connec1DL3 = num.zeros((len(IndL3D1_i),5),int)
		j=0
		for i in IndL3D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec1DL3[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),int(line[7])]

		# Recherche des elements 2D - T3 noeuds
		Ind=(TypeElem[:,1]==2).nonzero()
		IndT3D1_i=Ind[0]
		Connec2DT3 = num.zeros((len(IndT3D1_i),5),int)
		j=0
		for i in IndT3D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec2DT3[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),int(line[7])]
		
		# Recherche des elements 2D - T6 noeuds
		Ind=(TypeElem[:,1]==9).nonzero()
		IndT6D2_i=Ind[0]
		Connec2DT6 = num.zeros((len(IndT6D2_i),8),int)
		j=0
		for i in IndT6D2_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec2DT6[j-1,:]=[int(line[0]),int(line[2]),
							   int(line[5]),int(line[6]),int(line[7]),
							   int(line[8]),int(line[9]),int(line[10])]

		# Recherche des elements 2D - Q4 noeuds
		Ind=(TypeElem[:,1]==3).nonzero()
		IndQ4D1_i=Ind[0]
		Connec2DQ4 = num.zeros((len(IndQ4D1_i),6),int)
		j=0
		for i in IndQ4D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec2DQ4[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),int(line[7]),int(line[8])]
		
		# Recherche des elements 2D - Q8 noeuds
		Ind=(TypeElem[:,1]==16).nonzero()
		IndQ8D2_i=Ind[0]
		
		Connec2DQ8 = num.zeros((len(IndQ8D2_i),10),int)
		j=0
		for i in IndQ8D2_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec2DQ8[j-1,:]=[int(line[0]),int(line[2]),
							   int(line[5]),int(line[6]),int(line[7]),int(line[8]),
							   int(line[9]),int(line[10]),int(line[11]),int(line[12])]
		
		# Recherche des elements 2D - Q9 noeuds
		Ind=(TypeElem[:,1]==10).nonzero()
		IndQ9D2_i=Ind[0]
		Connec2DQ9 = num.zeros((len(IndQ9D2_i),11),int)
		j=0
		for i in IndQ9D2_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec2DQ9[j-1,:]=[int(line[0]),int(line[2]),
							   int(line[5]),int(line[6]),int(line[7]),int(line[8]),
							   int(line[9]),int(line[10]),int(line[11]),int(line[12]),int(line[13])]
		
		# Recherche des elements 3D - H4 noeuds
		IInd=(TypeElem[:,1]==4).nonzero()
		IndH4D3_i=Ind[0]
		Connec3DH4 = num.zeros((len(IndH4D3_i),6),int)
		j=0
		for i in IndH4D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec3DH4[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),int(line[7]),int(line[8])]
		
		# Recherche des elements 3D - H10 noeuds
		Ind=(TypeElem[:,1]==11).nonzero()
		IndH10D3_i=Ind[0]
		Connec3DH10 = num.zeros((len(IndH10D3_i),12),int)
		j=0
		for i in IndH10D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec3DH10[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),
								int(line[7]),int(line[8]),int(line[9]),int(line[10]),
								int(line[11]),int(line[12]),int(line[13]),int(line[14])]
		
		# Recherche des elements 3D - C8 noeuds
		Ind=(TypeElem[:,1]==5).nonzero()
		IndC8D3_i=Ind[0]
		Connec3DC8 = num.zeros((len(IndC8D3_i),10),int)
		j=0
		for i in IndC8D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec3DC8[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),
								int(line[7]),int(line[8]),int(line[9]),int(line[10]),
								int(line[11]),int(line[12])]
		# Recherche des elements 3D - C20 noeuds
		Ind=(TypeElem[:,1]==17).nonzero()
		IndC20D3_i=Ind[0]
		Connec3DC20 = num.zeros((len(IndC20D3_i),22),int)
		j=0
		for i in IndC20D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			Connec3DC20[j-1,:]=[int(line[0]),int(line[2]),int(line[5]),int(line[6]),
								int(line[7]),int(line[8]),int(line[9]),int(line[10]),
								int(line[11]),int(line[12]),int(line[13]),int(line[14]),
								int(line[15]),int(line[16]),
								int(line[17]),int(line[18]),int(line[19]),int(line[20]),
								int(line[21]),int(line[22]),int(line[23]),int(line[24])]
		
		return Connec3DC20,Connec3DC8,Connec3DH10,Connec3DH4,Connec2DQ9,Connec2DQ8,Connec2DQ4,Connec2DT6,Connec2DT3,Connec1DL3,Connec1DL2

	def ReadElementV2(self,Gmsh):
		
		try:
			IndElem=Gmsh.index(Keywords2[1])
		except ValueError:
			IndElem=Gmsh.index(KeywordsAlt2[1])

		NbrElem = int(Gmsh[IndElem+1])
		TypeElem = num.zeros((NbrElem,5),int)
		for i in range(NbrElem):
			line=string.split(Gmsh[IndElem+2+i])
			TypeElem[i,:]=[int(line[0]),int(line[1]),int(line[2]),int(line[3]),int(line[4])]
		
		
		# Recherche des elements 1D - L2 noeuds
		Ind=(TypeElem[:,1]==1).nonzero()
		IndL2D1_i=Ind[0]
		Connec1DL2 = num.zeros((len(IndL2D1_i),4),int)
		j=0
		for i in IndL2D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			
			Connec1DL2[j-1,:]=[elmNum,RegPhys,Node1,Node2]
		
		# Recherche des elements 1D - L3 noeuds
		Ind=(TypeElem[:,1]==8).nonzero()
		IndL3D1_i=Ind[0]
		Connec1DL3 = num.zeros((len(IndL3D1_i),5),int)
		j=0
		for i in IndL3D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			
			Connec1DL3[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3]

		# Recherche des elements 2D - T3 noeuds
		Ind=(TypeElem[:,1]==2).nonzero()
		IndT3D1_i=Ind[0]
		Connec2DT3 = num.zeros((len(IndT3D1_i),5),int)
		j=0
		for i in IndT3D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			
			Connec2DT3[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3]
		
		# Recherche des elements 2D - T6 noeuds
		Ind=(TypeElem[:,1]==9).nonzero()
		IndT6D2_i=Ind[0]
		Connec2DT6 = num.zeros((len(IndT6D2_i),8),int)
		j=0
		for i in IndT6D2_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			
			
			Connec2DT6[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3,Node4,Node5,Node6]

		# Recherche des elements 2D - Q4 noeuds
		Ind=(TypeElem[:,1]==3).nonzero()
		IndQ4D1_i=Ind[0]
		Connec2DQ4 = num.zeros((len(IndQ4D1_i),6),int)
		j=0
		for i in IndQ4D1_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			
			Connec2DQ4[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3,Node4]
		
		# Recherche des elements 2D - Q8 noeuds
		Ind=(TypeElem[:,1]==16).nonzero()
		IndQ8D2_i=Ind[0]
		Connec2DQ8 = num.zeros((len(IndQ8D2_i),10),int)
		j=0
		for i in IndQ8D2_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			Node7 = int(line[9+NbTag])
			Node8 = int(line[10+NbTag])
			
			Connec2DQ8[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3,Node4,Node5,Node6,Node7,Node8]
		
		# Recherche des elements 2D - Q9 noeuds
		Ind=(TypeElem[:,1]==10).nonzero()
		IndQ9D2_i=Ind[0]
		Connec2DQ9 = num.zeros((len(IndQ9D2_i),11),int)
		j=0
		for i in IndQ9D2_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			Node7 = int(line[9+NbTag])
			Node8 = int(line[10+NbTag])
			Node9 = int(line[11+NbTag])
			
			Connec2DQ9[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3,Node4,Node5,Node6,Node7,Node8,Node9]
			
		# Recherche des elements 3D - H4 noeuds
		Ind=(TypeElem[:,1]==4).nonzero()
		IndH4D3_i=Ind[0]
		Connec3DH4 = num.zeros((len(IndH4D3_i),6),int)
		j=0
		for i in IndH4D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			
			Connec3DH4[j-1,:]=[elmNum,RegPhys,Node1,Node2,Node3,Node4]
		
		# Recherche des elements 3D - H10 noeuds
		Ind=(TypeElem[:,1]==11).nonzero()
		IndH10D3_i=Ind[0]
		Connec3DH10 = num.zeros((len(IndH10D3_i),12),int)
		j=0
		for i in IndH10D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			Node7 = int(line[9+NbTag])
			Node8 = int(line[10+NbTag])
			Node9 = int(line[11+NbTag])
			Node10 = int(line[12+NbTag])
			
			Connec3DH10[j-1,:]=[elmNum,RegPhys,Node1,Node2,
								Node3,Node4,Node5,Node6,
								Node7,Node8,Node9,Node10]

		
		# Recherche des elements 3D - C8 noeuds
		Ind=(TypeElem[:,1]==5).nonzero()
		IndC8D3_i=Ind[0]
		Connec3DC8 = num.zeros((len(IndC8D3_i),10),int)
		j=0
		for i in IndC8D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			Node7 = int(line[9+NbTag])
			Node8 = int(line[10+NbTag])
			Connec3DC8[j-1,:]=[elmNum,RegPhys,Node1,Node2,
								Node3,Node4,Node5,Node6,
								Node7,Node8]
		
		# Recherche des elements 3D - C20 noeuds
		Ind=(TypeElem[:,1]==17).nonzero()
		IndC20D3_i=Ind[0]
		Connec3DC20 = num.zeros((len(IndC20D3_i),22),int)
		j=0
		for i in IndC20D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			Node7 = int(line[9+NbTag])
			Node8 = int(line[10+NbTag])
			Node9 = int(line[11+NbTag])
			Node10 = int(line[12+NbTag])
			Node11 = int(line[13+NbTag])
			Node12 = int(line[14+NbTag])
			Node13 = int(line[15+NbTag])
			Node14 = int(line[16+NbTag])
			Node15 = int(line[17+NbTag])
			Node16 = int(line[18+NbTag])
			Node17 = int(line[19+NbTag])
			Node18 = int(line[20+NbTag])
			Node19 = int(line[21+NbTag])
			Node20 = int(line[22+NbTag])
			Connec3DC20[j-1,:]=[elmNum,RegPhys,Node1,Node2,
								Node3,Node4,Node5,Node6,
								Node7,Node8,Node9,Node10,
								Node11,Node12,Node13,Node14,
								Node15,Node16,Node17,Node18,
								Node19,Node20]
								
		# Recherche des elements 3D - P6 noeuds
		Ind=(TypeElem[:,1]==6).nonzero()
		IndT6D3_i=Ind[0]
		Connec3DT6 = num.zeros((len(IndT6D3_i),8),int)
		j=0
		for i in IndT6D3_i:
			j=j+1
			line=string.split(Gmsh[IndElem+2+i])
			elmNum = int(line[0])
			NbTag = int(line[2])
			RegPhys = int(line[3])
			Node1 = int(line[3+NbTag])
			Node2 = int(line[4+NbTag])
			Node3 = int(line[5+NbTag])
			Node4 = int(line[6+NbTag])
			Node5 = int(line[7+NbTag])
			Node6 = int(line[8+NbTag])
			Connec3DT6[j-1,:]=[elmNum,RegPhys,Node1,Node2,
								Node3,Node4,Node5,Node6]

		return Connec3DT6,Connec3DC20,Connec3DC8,Connec3DH10,Connec3DH4,Connec2DQ9,Connec2DQ8,Connec2DQ4,Connec2DT6,Connec2DT3,Connec1DL3,Connec1DL2
	
	
	def write_connec(self,connec,type_element):
		"ecriture de la connectivite dans le fichier du maillage etendue"
		#self.entity = 0
		for i in range(connec.shape[0]):
			self.elem += 1
			self.entity += 1
			#$Elements
			#number-of-elements
			#elm-number elm-type number-of-tags < tag > ... node-number-list
			#number-of-tags
			#gives the number of integer tags that follow for the n-th element. By default, the first tag is the number of the physical entity to which the element belongs; the second is the number of the elementary geometrical entity to which the element belongs; the third is the number of a mesh partition to which the element belongs. All tags must be postive integers, or zero. A zero tag is equivalent to no tag
			self.file.write('%i %i %i %i %i %i'%(self.elem,type_element,3,connec[i,1],self.entity,0))
			#self.file.write('%i %i %i'%(self.elem,type_element,0))
			for j in range(connec.shape[1]-2):
				self.file.write(' %i'%(connec[i,j+2]))
			self.file.write('\n')
		

	def write_vtk_nodal_value(self,result = "not defined", vec_title = "not defined",sca_title = "not_defined",\
	                          n = 0, path = "not defined", mesh = "Tri3", name = "sol_", dim = 3):
		"Creation des fichiers resultats pour paraview"
		import Vtk
	
		file = open(path + os.sep + name +str(n)+'.vtk' ,'w')
		export = Vtk.Formatter(file)
		export.header('Affichage des caracteristiques du maillage')
		if mesh == "Nod1" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Nod1[:,2:] - 1, 1*num.ones((self.new_Nod1.shape[0]),int))
		if mesh == "Seg2" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Seg2[:,2:] - 1, 3*num.ones((self.new_Seg2.shape[0]),int))
		if mesh == "Tri3" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Tri3[:,2:] - 1, 5*num.ones((self.new_Tri3.shape[0]),int))
		if mesh == "Qua4" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Qua4[:,2:] - 1, 9*num.ones((self.new_Qua4.shape[0]),int))
		if mesh == "Tri6" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Tri6[:,2:] - 1, 22*num.ones((self.new_Tri6.shape[0]),int))
		if mesh == "Qua8" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Qua8[:,2:] - 1, 23*num.ones((self.new_Qua8.shape[0]),int))
		if mesh == "Cub8" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Cub8[:,2:] - 1, 12*num.ones((self.new_Cub8.shape[0]),int))
		if mesh == "Tet4" :
			export.unstructured_grid('FLOAT',self.node[:,1:], self.new_Tet4[:,2:] - 1, 10*num.ones((self.new_Tet4.shape[0]),int))
		if mesh == "Cub20" :
			Permute = num.array(([1,2, 3, 4, 5, 6, 7, 8, 9, 12, 14, 10, 17, 19, 20, 18, 11, 13, 15, 16]),int) - 1
			Cub20 = self.new_Cub20[:,2:]
			Cub20 = Cub20[:,Permute]
			export.unstructured_grid('FLOAT',self.node[:,1:], Cub20 - 1, 25*num.ones((Cub20.shape[0]),int))

		export.point_data(self.node.shape[0])
		################################################################
		# Nombre de champs a ecrire
		export.field_data(name = 'field_data', number = len(vec_title) + len(sca_title))
		
		vec = 0
		
		# Ecriture d'un champs de vecteur 2D
		if dim == 2:
			for i in range( len(vec_title)):
				Z = num.zeros((result.shape[0],1),float)
				U = num.concatenate((result[: ,  i*dim:(i+1)*dim],Z),axis = 1)
				export.vectors('FLOAT',U,self.node.shape[0],vec_title[i])
				vec = (i+1)*dim
		# Ecriture d'un champs de vecteur 3D
		if dim == 3:
			for i in range( len(vec_title)):
				U = result[:, i*dim:(i+1)*dim]
				export.vectors('FLOAT',U,self.node.shape[0],vec_title[i])
				vec = (i+1)*dim
		
		#Ecriture des autres champs scalaire
		for j in range(len(sca_title)):
			export.scalar('FLOAT',result[:,vec+j],self.node.shape[0],sca_title[j])

		file.close()
		

