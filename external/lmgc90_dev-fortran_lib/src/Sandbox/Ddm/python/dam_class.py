# On veut construire ici l equivalent de la structure mail sous matlab:
#   mail : maillage
#     mail{i} : maillage elementaire de la sous-zone i
#     mail{i}.TYPE : type d'elements
#     mail{i}.MAIL(nbeli,nbnni) : connectivite
	
# que l'on appel avec s.chiffre etc...

from numpy import * 
#import scipy as Sci
#import scipy.linalg

class C(object):
	x = 23             # x et y : attributs de classe
	y = x + 5
	def affiche(self): # m hode affiche()
		self.z = 42    # attribut d instance
		print C.y,     # dans une m hode, on qualifie un attribut
		print self.z # mais pas un attribut d instance
#ob = C()      # instanciation de l objet ob
#ob.affiche() # 28 42 (l appel, ob affecte self)

class Vecteur2D(object):
	def __init__(self, x, y):
		self.x = x
		self.y = y
	def __add__(self, autre): # addition vectorielle
		return Vecteur2D(self.x + autre.x, self.y + autre.y)
	def __str__(self):       # affichage d un Vecteur2D
		return "Vecteur(%g, %g)" % (self.x, self.y)
#v1 = Vecteur2D(1.2, 2.3)
#v2 = Vecteur2D(3.4, 4.5)
#print v1 + v2 # affiche : Vecteur(4.6, 6.8)


# tentative de creeation de classe MAIL_ELMT_FINI
class MAIL_ELMT_FINI(object):
	zone = 1
	nbeli = 2
	nbnni = 2
	def TYPE(self):
		self.TYPE = 'SEG2' 							#: type d'elements
		print self.TYPE
	def MAIL(self):
		self.MAIL = arange(12) 			#: connectivite
		print self.MAIL				
	
#ob = MAIL_ELMT_FINI()      # instanciation de l objet ob
#ob.TYPE()	
#ob.MAIL()	
#print ob

# tentative de creeation de classe CHAMPS
#   chpo1 : champ par point (a 1 seule sous-zone par limitation d'AVS)
#     chpo1{1}{i}.COMP : nom de la composante numero i
#     chpo1{1}{i}.UNIT : nom de l'unite de la composante numero i
#     chpo1{1}{i}.XVAL(nbno,nbval) : valeurs
class UN_CHAMP(object):
	def __init__(self,nom_composante,unitee,vecteur):
		self.COMP = nom_composante
		self.UNIT = unitee
		self.XVAL = vecteur 
		#zone = 1
	def COMP(self):
		#self.COMP = self.nom_composante 							#: type d'elements
		print self.COMP
	def UNIT(self):
		#self.UNIT = self.unitee 			#: connectivite
		print self.UNIT			
	def XVAL(self):
		#self.XVAL = self.vecteur 			#: connectivite
		print self.XVAL
	def affiche(self):
		#self.COMP = self.nom_composante
		#self.UNIT = self.unitee
		#self.XVAL = self.vecteur
		print self.COMP
		print self.UNIT
		print self.XVAL
		

#vecteur = r_[1:10]
#ob = UN_CHAMP('nom_composante','unitee',1)      # instanciation de l objet ob
#ob.COMP
#ob.UNIT
#ob.XVAL		
#print ob


class UNE_MAP(object):
	def __init__(self,numer2,mapcomPrim1,listComPrim1):
		self.NUIP = numer2
		self.MAIP = mapcomPrim1
		self.NAIP = listComPrim1
		self.NUMP = numer2
		self.NAMP = mapcomPrim1
		self.MAPP = listComPrim1 
	def NUIP(self):					# node numbers of the first mapping
		print self.NUIP
	def NAIP(self):					# dof names of the first mapping
		print self.MAIP		
	def MAIP(self):					# dof number in the first mapping
		print self.NAIP
	def NUMP(self):					# node numbers of the first mapping
		print self.NUMP
	def NAMP(self):					# dof names of the first mapping
		print self.MAPP		
	def MAPP(self):					# dof number in the first mapping
		print self.NAMP
	def BE(self):
		print self.BE		
	def affiche(self):
		print self.NUIP
		print self.MAIP
		print self.NAIP
