import os
from numpy import * 
#import scipy as Sci
#import scipy.linalg
from dam_class import *
from Fonction_de_calcul import *

def ListCompChml(chml1):
	#function [ListComp1] = ListCompChml(chml1)
	## DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 28 / 12 / 2002
	# Extrait du champ par element constant chml1 la liste des noms des composantes ListComp1(ncomp1)
	nbcomp1 = shape(chml1)[0]
	ListComp1 = []
	##print" 'nbcomp1',nbcomp1
	for i in r_[1:nbcomp1+1]:
		ListComp1 = hstack([ListComp1,chml1[i-1].COMP]) 
	##print" ListComp1 ,'i',i, 'test699'
	return ListComp1

def ExtrChml2(chml1,ListComp1,liste_nouveaux_nom):
	#function [chml2] = ExtrChml2(chml1,ListComp1,varargin)
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 28 / 12 / 2002
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 23 / 09 / 2002
	#   Possibilite de changer le nom de la composante en arg. optionnel

	# Extrait le champ par element constant chml2 a partir du
	# champ par element constant chml1 constitue des seules
	# composantes presentes dans ListComp1(ncomp1)
	# Si ListComp2(ncomp1) est fourni, les noms des ncomp1 composantes
	# sont changes en consequence.

	# par default varargin sera 'default'
	#if (varargin[1] == 'default' )
	ListComp2 = ListComp1
	#else
	#	ListComp2 = varargin[1]

	nbcomp2 = shape(ListComp1)[0]
	##print" nbcomp2
	
	if (nbcomp2 <> shape(ListComp2)[0]):
	  ##print" ListComp1
	  ##print" ListComp2
	  print 'bad number of new component names'
	  

	
	# Utilisation de ListCompChml du modul Decomposition_de_domaine
	# Extrait du champ par element constant chml1 la liste des noms des composantes ListComp1(ncomp1)
	List1 = ListCompChml(chml1);
	
	##print" ListComp1 , List1 , 'affichage des liste'
	
	List_composante_numerotee = Findoccurences(ListComp1,List1)
	###print" List_composante_numerotee,'List_composante_numerotee'
	#raw_input()
	#moi je n ai qu une seul zone
	chml2 = []
	##print" shape(ListComp1)[0] ,'test24'
	##print" r_[0:shape(ListComp1)[0]]
	for i in r_[0:shape(ListComp1)[0]]:
		nom_composante_ancienne=ListComp1[i]
		##print" 'List_composante_numerotee[i]',List_composante_numerotee[i]
		indice_composante_chml1=List_composante_numerotee[i]
		champ = chml1[indice_composante_chml1]
		champ.COMP = liste_nouveaux_nom[i]
		###print" champ.COMP, 'champ.COMP'
		###print" champ.XVAL, 'champ.XVAL'
		#champ.affiche()
		chml2.append(champ)
	return chml2
	
	
	#if length(find(ListZone1)) ~= nbcomp2
	#  ListComp1
	#  List1
	#  error('Not all the components where found')
	#end
	#
	#reset(chml2
	###print" chml2,'chml2'
	
	#for zo2 in = 1:length(ListZone1)
	#	zo1 = ListZone1(zo2);
	#	chml2{zo2} = chml1{zo1};
	#	chml2{zo2}.COMP = ListComp2{zo2};
	
	

def ExtrChpo(chml1,ListComp1,liste_nouveaux_nom):

	# comme je n ai qu une zone l extration est identique a celle d un champ
	chml2 = ExtrChml2(chml1,ListComp1,liste_nouveaux_nom)
	
	return chml2
	


def MaxiChml(chml1,liste_nom_composantes='tout'):
	#function [max1] = MaxiChml(chml1,varargin)
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 29 / 12 / 2002
	#
	# max1 = Maxichml(chml1)
	# max1 = Maxichml(chml1,ListComp1)
	# Trouve la valeur maximale d'un champ par element constant chml1
	# pour les composantes dont les noms sont dans le deuxieme
	# argument s il existe (sinon, partout).
	
	##print" 'test100000000'
	### fd&dichml1[0].affiche()
	##print" 'test100000000'
	ListComp1 = ListCompChml(chml1)
	##print" ListComp1
	
	if (liste_nom_composantes=='tout'):
		###print" chml1,'test678'
		#chml1[0].affiche()
		ListComp1 = ListCompChml(chml1)
		##print" ListComp1 , 'test666'
	else:
		ListComp1 = liste_nom_composantes
	#else
	#  narg
	#  error('Bad number of arguments')
	#end


	# Utilisation de ListCompChml du modul Decomposition_de_domaine
	# Extrait du champ par element constant chml1 la liste des noms des composantes ListComp1(ncomp1)
	List1 = ListCompChml(chml1);
	##print" List1 ,'test366'
	##print" ListComp1
	List_composante_numerotee = Findoccurences(ListComp1,List1)
	
	#moi je n ai qu une seul zone
	max = []
	##print" r_[0:shape(ListComp1)[0]],'testr'
	for i in r_[0:shape(ListComp1)[0]]:
		nom_composante_ancienne=ListComp1[i]
		indice_composante_chml1=List_composante_numerotee[i]
		##print" i ,'test345'
		
		max1= chml1[indice_composante_chml1].XVAL.max()
		##print" max1,'testmax'
		max.append(max1)
	return max

	#max1 = []
	#nbcomp1 = shape(chml1)[0]
	#for i1 in r_[1:nbcomp1]:
	#  chmle1 = chml1[i1]
	#  l1 = Findoccurences([chmle1.COMP,ListComp1])
	#  if l1
	#	max1 = max([max1 chmle1.XVAL.max()])
	#end
	#clear chmle1;
	#end



def ElemChml(chml1,mail1,val1):
	#function [mail2,listelem1] = ElemChml(chml1,mail1,val1)
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 25 / 12 / 2002

	# Extrait le maillage du champ par element constant (chml1,mail1)
	# sur lequel il vaut val1
	# Retourne le maillage mail2 et la correspondance d'elements dans
	# mail1: l'element i de mail2 est l'element listelem1(i) de mail1
	# (numerotations globales des elements)

	nbcomp1 = shape(chml1)[0]
	if (nbcomp1 <> 1):
	  #print" nbcomp1 , 'Erreure More than 1 component in the field'
	  raw_input('Erreure More than 1 component in the field')

	xval1 = chml1[0].XVAL
	#[nbelt,nbval] = size(xval1);
	nbelt,nbval = shape(xval1)[0],1 #on sort le nombre d elements et le nombre de valeurs
	# ici nbval en dur c pas beau
	
	if (nbval <> 1): #si plus d une valeur alors un truc cloche
		##print" 'nbval', nbval
		raw_input('More than 1 value for the component')
		

	# List of global numbers of elements
	listelem1 = where(xval1==val1)[0]
	# verification ok
	###print" xval1 # semble ok
	###print" listelem1,'listelem1' # semble ok
	#raw_input()
	
	#peut etre utiliser le traspose
	
	# Element extraction
	mail2 = ElemMesh(mail1,listelem1);
	#verification ok
	##print" 'mail2',mail2.MAIL
	return mail2,listelem1


def ElemMesh(mail1,listelem1):
	#function [mail2] = ElemMesh(mail1,listelem1)
	#DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 25 / 12 / 2002
	#Extrait le maillage mail2 du mailllage mail1 qui contient les
	#elements listelem1 de mail1
	#(numerotations globales des elements)

	listelem1 = sort(listelem1)
	nbelt1  = shape(listelem1)[0]
	#nbzone1 = shape(mail1)[0]



	
	if (mail1.TYPE == 'SEG2'):
		#clear mail2; 
		zo2 = 0
		i1 = 1 				
		nbelc1 = 0
		#for zo1 = 1:nbzone1
		#maile1 = mail1{zo1};
		maile1 = mail1
		topo1  = maile1.MAIL;
		
		nbel1,nbnn1 = shape(topo1)[0],shape(topo1)[1]
		nbelc1 = nbelc1 + nbel1
		ielem2 = 0 
		topo2 = zeros([nbelt1,nbnn1])
		
		while (i1-1 <= nbelt1-1) and (listelem1[i1-1] <= nbelc1-1):
			#   The element is in this zone
			ielem1 = (nbel1-1) - (nbelc1-1) + listelem1[i1-1]
			##print" ielem1 ,'ielem1'
			topo2[ielem2,0] = topo1[ielem1,0]
			topo2[ielem2,1] = topo1[ielem1,1]
			ielem2 = ielem2 + 1
			i1 = i1 + 1

		###print" topo2 ,'topo2'
		#raw_input()
		#if ielem2
		#zo2 = zo2 + 1;
		#mail2{zo2} = struct('TYPE',maile1.TYPE,'MAIL',topo2);


		#ecriture sous forme d un objet representatif d un maillage
		#from dam_class import *
		mail2 = MAIL_ELMT_FINI()      # instanciation de l objet ob
		mail2.TYPE = 'SEG2'	
		mail2.MAIL = topo2
		
	elif (mail1.TYPE == 'POIN1'):
		#clear mail2; 
		zo2 = 0
		i1 = 1 				
		nbelc1 = 0
		#for zo1 = 1:nbzone1
		#maile1 = mail1{zo1};
		maile1 = mail1
		topo1  = maile1.MAIL;
		##print" listelem1,'listelem1'
		
		nbel1 = shape(topo1)[0]
		nbelc1 = nbelc1 + nbel1
		ielem2 = 0 
		topo2 = zeros([nbelt1])
		
		while (i1-1 <= nbelt1-1) and (listelem1[i1-1] <= nbelc1-1):
			#   The element is in this zone
			ielem1 = (nbel1-1) - (nbelc1-1) + listelem1[i1-1]
			topo2[ielem2] = topo1[ielem1]
			ielem2 = ielem2 + 1
			i1 = i1 + 1


		#if ielem2
		#zo2 = zo2 + 1;
		#mail2{zo2} = struct('TYPE',maile1.TYPE,'MAIL',topo2);


		#ecriture sous forme d un objet representatif d un maillage
		#from dam_class import *
		mail2 = MAIL_ELMT_FINI()      # instanciation de l objet ob
		mail2.TYPE = 'SEG2'	
		mail2.MAIL = topo2
		
	else:
		print ('type de maillage non implemente dans ElemMesh')
		
	
	return mail2
	
	# ou avec un dictionnaire
	#mail2 = {}      # dictionnaire vide
	#mail2['TYPE'] = 'SEG2'
	#mail2['MAIL'] = topo2	


	#clear topo2;

	# disp(['Nb elements ' int2str(ielem2) ' Nb zones ' int2str(zo2)])


def DD2(chml1,mail1,idim,restriction_nsdm='inf'):
	#function [ListMeshSdm,ListContSdm,varargout] = DD2(chml1,mail1,idim,varargin)
	# Construit les sous-domaines a partir d'un champ les reperants
	#
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 26 / 12 / 2002
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 16 / 02 / 2003
	#   Cas des SEG2
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 15 / 12 / 2004
	#   Ajout du 3D
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 13 / 05 / 2005
	#   Limitation possible du nombre de sous domaines
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 25 / 07 / 2006
	#   Possibilite d'avoir la correspondance d'elements
	#
	# Entrees
	#   chml1		Champ constant par element reperant le sous-domaine
	#   mail1		Maillage associe
	#   idim		Dimension de l'espace
	# Entree optionnelle
	#   nsdm1		Restriction sur le nb de sous domaines
	# Sorties
	#   ListMeshSdm{sdm2}	Liste des maillages des sous domaines
	#   ListContSdm{sdm2}	Liste des maillages des contours des sous domaines
	# Sortie optionnelle
	#   ListElemSdm{sdm2}   Liste des correspondances d'elements

	# Avec le champ par element (chml1,mail1) reperant l'appartenance
	# des elements a un sous domaine (composante de nom SDM),
	# on decoupe le maillage mail1, pour obtenir
	# la liste des maillages des sous domaines ListMeshSdm(nsdm1)
	# et en prime la liste des maillages de leur contour ListContSdm(nsdm1)
	#
	# L'element i de ListMeshSdm{sdm2} est l'element ListElemSdm{sdm2}(i)
	# de mail1.

	#nout = nargout - 2; #???

	chmls1 = ExtrChml2(chml1,['SDM'],['SDM'])
	
	###print" 'affichechmls0'
	#chmls1[0].affiche()
	###print" chml1
	###print" chmls1
	#chml1[0].affiche()
	
	nsdm1 = int(MaxiChml(chmls1,['SDM'])[0])
	###print" nsdm1, 'testnsdm1'  

	if (restriction_nsdm == 'inf'):
	  print 'Number of subdomains ' , nsdm1
	else:
	  print 'Total number of subdomains ', nsdm1
	  nsdm1 = restriction_nsdm 
	  print 'Modified number of subdomains ', nsdm1
	#  nin1
	#  error('Wrong number of optional arguments')
	#end
	
	
	#clear ListElemSdm;
	ListMeshSdm = []
	ListElemSdm = []
	ListContSdm = []
	for sdm1 in r_[1:nsdm1+1]:
		msdm1,listelem1 = ElemChml(chmls1,mail1,sdm1);
		###print" msdm1.MAIL
		#csdm1 = ContourMesh3(msdm1,idim)
		#pour des seg2 les contour c est l ensemble des noeuds
		maillage = msdm1.MAIL.copy()
		
		nombre_elements = shape(maillage)[0]
		maillage.resize(1,2*nombre_elements)
		#maillage.sort()
		#maillage.argsort(kind="mergesort")
		#maillage=SuppreDoublons(maillage)
		#maillage = asarray(maillage[0])
				
		
		maillage = unique(maillage)	
		#maillage=mat(maillage)
		ListMeshSdm.append(msdm1)
		ListContSdm.append(maillage)
		ListElemSdm.append(listelem1)
		#clear msdm1 csdm1;
		#end
	return ListMeshSdm,ListContSdm,ListElemSdm
#
#	#clear chmls1;
#
#	switch nout
#	  case 0,
#	  case 1,
#		varargout(1) = {ListElemSdm};
#	  otherwise,
#		nout
#		error('Bad number of output variables')
#	end


def DDInt2(ListContSdm,nombre_mini_noeuds=0):
	#function [lnod1,lind1,ListMeshInt] = DDInt2(ListContSdm,varargin)
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 26 / 12 / 2002
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 06 / 11 / 2005
	#  Treatment of weak interfaces and optional arguments

	# Avec la liste des maillages des contours des sous domaines
	# ListContSdm(nsdm1), on trouve :
	# - la liste des maillages des interfaces    ListMeshInt(nint1)
	# - le graphe de connectivite de la decomposition (lnod1,lind1)
	#   (les aretes sont les interfaces, les sommets les sous domaines)

	# Inputs
	#  ListContSdm{nsdm1}	list of meshes of boundary of subdomains
	# Optional inputs
	#  WeakIntOption	keyword for weak interfaces detection
	#      'None':    no detection of weak interfaces
	#      'Skip':    do not consider weak interfaces (beware that
	#                 this may change the reference problem)
	#      'Micro':   let weak interface be micro only
	#      'Macro':   let weak interface be macro only
	#      'Rbms':    macro part on weak interfaces is rbm per subdomain
	#      'Corners': consider weak interfaces as an augmentation of
	#                 corner nodes (see FETI-DP)
	#  nbming1		minimum number of nodes to consider an interface
	#                       as strong
	# Outputs
	#  (lnod1,lind1)	decomposition graph connectivity
	#                       (edges are interfaces, vertices are subdomains)
	#  ListMeshInt{nint1}	list of meshes of interfaces

	#nsdm1 = shape(ListContSdm[0])[0]
	###print" ListContSdm
	#raw_input()
	nsdm1 = len(ListContSdm)
	ListMeshInt = []

	#nin1 = nargin-1;
	if (nombre_mini_noeuds==0):
		WeakIntOption = 'None'
	else:
	  WeakIntOption = 'yes'
	  nbming1       = nombre_mini_noeuds
	  #print" 'Treatment of weak interfaces: ', WeakIntOption
	  #print" 'Minimum number of nodes: ', nbming1


	# Avec les maillages des contours, on cherche les interfaces
	# et leurs maillages dans la liste ListMeshInt(nint1),
	# on place de plus la topologie de la decomposition dans
	# TopoDec(nint1,2)
	nint1 = 0
	nint0 = 0
	
	###print" ListContSdm[1], 'ListContSdm'
	#raw_input()
	TopoDec = []
	ListMeshInt=[]

	for sdm1 in r_[1:nsdm1]:
		for sdm2 in r_[sdm1+1:nsdm1+1]:
			#mint1 = IntersectMesh(ListContSdm[sdm1],ListContSdm[sdm2]);
			mint1 = intersect1d(ListContSdm[sdm1-1], ListContSdm[sdm2-1]) 
			
			if (shape(mint1)[0]<>0):
				##print" 'A new interface'
				#nmail1 = ChangeMesh2(mint1,'POI1');
				nmail1 = MAIL_ELMT_FINI()      # instanciation de l objet ob
				nmail1.TYPE = 'POI1'	
				nmail1.MAIL = mint1
				nnod1 = shape(nmail1.MAIL)[0]
				###print" nnod1
				#raw_input()
				if (WeakIntOption == 'None') and (nnod1 <= nbming1):
					##print" 'Get rid of this interface'
					nint0 = nint0 + 1
				else:
					##print" 'Classical treatment'
					nint1 = nint1 + 1
					
					if (nint1 ==1):
						TopoDec = [sdm1,sdm2]
					else:
						TopoDec = vstack([TopoDec,[sdm1,sdm2]])
					ListMeshInt.append(mat(mint1))
	
	
	TopoDec = array(mat(TopoDec))
	##print" 'ListContSdm' ,ListContSdm	
	##print" ListMeshInt
	#taille=shape(TopoDec)[0]*0.5
	
	##print" 'TopoDec',TopoDec
	#TopoDec = TopoDec.resize(taille,2)
	
	##print" 'TopoDec',TopoDec
	# Graphe de connectivite entre sous domaines
	#clear MeshDec;
	#MeshDec{1} = struct('TYPE','SEG2','MAIL',TopoDec);
	MeshDec = MAIL_ELMT_FINI()      # instanciation de l objet ob
	MeshDec.TYPE = 'SEG2'	
	MeshDec.MAIL = TopoDec
	
	
	# ListMeshInt est la liste des maillages des interfaces
	
	lnod1,lind1 = MeshToGraph(MeshDec);
	#clear TopoDec MeshDec;

	#disp(['Number of detected interfaces ' int2str(nint1)])
	#if strcmp(WeakIntOption,'Skip')
	#  disp(['Number of skipped weak interfaces ' int2str(nint0)])
	#end
	return lnod1,lind1,ListMeshInt
	

def MeshToGraph(mail1):
	#function [lnod1,lind1] = MeshToGraph(mail1)
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 30 / 12 / 2002

	# Transform a mesh into a connectivity graph.
	# Some information is lost during the conversion (type of elements...)
	# The edges of the graph are the elements, the vertices are the nodes:
	# the element iel (local numbering of elements) is linked to the nodes
	# lnod1(lind1(iel):lind1(iel+1)-1)

	lnod1 = []; ind1 = 0;
	lind1 = [1];

	# Loop on elements
	iel = 0;
	nbzone1 = 1; #length(mail1); #moi j ai une seule zone
	#for zo1 = 1:nbzone1
	#topo1 = mail1{zo1}.MAIL;
	topo1 = mail1.MAIL
	
	###print" 'mail1',mail1.MAIL
	
	
	###print" 'shapetopo1',shape(topo1)[0],shape(topo1)[1],topo1[0,:]
	
	nbel1,nbno1 = shape(topo1)[0],shape(topo1)[1]
	for el1 in r_[1:nbel1+1]:
		iel = iel + 1
		#lnod1(ind1+1:ind1+nbno1) = topo1(el1,:)
		lnod1 = hstack([lnod1,topo1[el1-1,:]])
		ind1 = ind1 + nbno1
		#lind1(iel+1) = ind1 + 1
		lind1 = hstack([lind1 , (ind1 + 1)])
	return  lnod1,lind1
	#end
	#end



def ElemChpo(chpo1,nmail1,val1):
	#function [nmail2,listelem1] = ElemChpo(chpo1,nmail1,val1)
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 12 / 08 / 2003

	# Extrait le maillage du champ par point (chpo1,nmail1)
	# sur lequel il vaut val1
	# Retourne le maillage nmail2 et la correspondance d'elements dans
	# nmail1: l'element i de nmail2 est l'element listelem1(i) de nmail1
	# (numerotations globales des elements)

	# List of global numbers of elements
	listelem1 = zeros([1,0])
	#decal1 = 0 # for global numbering of elements over subzones
	#nbzo1 = length(chpo1);
	#for zo1 = 1:nbzo1
	#chpoe1 = chpo1{zo1};
	chpoe1 = chpo1
	nbcomp1 = shape(chpoe1)[0]
	if (nbcomp1 <> 1):
		##print" zo1
		##print" nbcomp1
		#print" 'More than 1 component in the field'
		raw_input()
	  	#end
	xval1 = chpoe1[nbcomp1-1].XVAL
	#if (shape(xval1)[1] <> 1):
	#	##print" shape(xval1)
	#	##print" 'error Complex field component not usable'
	#	raw_input
	#end
	
	criterion = (xval1 == val1)
	listelem1 = where(criterion)[0]
	
	#listelem1 = [listelem1 (find(xval1(:,1)' == val1) + decal1)];
	#decal1 = decal1 + size(xval1,1);
	#end

	# Element extraction
	nmail2 = ElemMesh(nmail1,listelem1)
	
	return nmail2,listelem1
	

def DDExt(ListContSdm,nfext1):
	#ListMeshExt1 = DDExt(ListContSdm,nfext1);
	#function [ListMeshExt] = DDExt(ListContSdm,mail1)
	#% DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 29 / 12 / 2002

	#% Avec la liste des maillages des contours des sous domaines
	#% ListContSdm(nsdm1), et le maillage d'un bord exterieur mail1,
	#% on decoupe mail1
	#% en la liste des maillages des exterieurs locaux aux sous domaines
	#% ListMeshExt(nsdm1).

	#nsdm1 = length(ListContSdm);

	# Avec les maillages des contours, on cherche l'interface exterieure
	ListMeshExt = []
	nsdm1 = len(ListContSdm)
	for sdm1 in r_[1:nsdm1+1]:
		mext1 = intersect1d(ListContSdm[sdm1-1],unique(nfext1))
		ListMeshExt.append(mat(mext1))
	#  clear mext1;
	#end
	return ListMeshExt


def ChangeMesh(mail1,type):
	
	if (type <> 'POI1'):
		print type , 'non implemente dans ChangeMesh'
	else:
		nmail1 = MAIL_ELMT_FINI()      # instanciation de l objet ob
		nmail1.TYPE = 'POI1'
		maillage = mat(mail1.MAIL).T
		#taille d une colone de la matrice
		nblin=shape(maillage)[1]
		#nbre de colonnes
		nbcol=shape(maillage)[0]
		#liste_neuds_maillage = hstack((maillage[0,:],maillage[1,:]))
		liste_neuds_maillage = maillage.reshape(nbcol*nblin) 
		# passage d'un format matrice a un format liste
		liste_neuds_maillage = array(liste_neuds_maillage)[0]
		#suppresion des doublons 
		liste_neuds_maillage=unique(liste_neuds_maillage)
		nmail1.MAIL = liste_neuds_maillage
		return nmail1


def ReduChpo(chpo1,nchpo1,nmail1):
	
	Place_des_noeuds_de_sdm1_dans_champtotal = digitize(nmail1,nchpo1)
	###print" 	nchpo1, 'nchpo1'
	###print" 	nmail1.MAIL, 'nmail1.MAILbis'
	##print" 	Place_des_noeuds_de_sdm1_dans_champtotal, 'Place_des_noeuds_de_sdm1_dans_champtotalbis'
	chpoE = []
	##print" len(chpo1) , 'en(chpo1)'
	for i in r_[1:len(chpo1)+1]:
		###print" (Place_des_noeuds_de_sdm1_dans_champtotal).astype(int) , 'Place_des_noeuds_de_sdm1_dans_champtotal'
		###print" (chpo1[0].XVAL[Place_des_noeuds_de_sdm1_dans_champtotal.astype(int) ])
		##print" i,'i',chpo1[i-1],shape(chpo1[i-1].XVAL)
		xval1  = chpo1[i-1].XVAL[Place_des_noeuds_de_sdm1_dans_champtotal.astype(int)-1]
		comp   = chpo1[i-1].COMP
		unite  = chpo1[i-1].UNIT
		champx = UN_CHAMP(comp,unite,1)      # instanciation de l objet ob
		champx.XVAL = xval1
		chpoE.append(champx)
	
	return chpoE,Place_des_noeuds_de_sdm1_dans_champtotal

def ReduChml(chpo1,nchpo1,nmail1):
	
	
	
	#juste ##print" 'nmail1.MAIL',nmail1.MAIL
	###print" 'nchpo1.MAIL',nchpo1.MAIL
	###print" 'nmail1.MAIL',nmail1.MAIL
	
	
	
	#Place_des_segments_de_sdm1_dans_champtotal = digitize(nchpo1.MAIL,nmail1.MAIL)
	###print" Place_des_segments_de_sdm1_dans_champtotal
	
	
	#raw_input()
	# juste pour tester imperativement trouver mieux
	liste = []
	for k in r_[0:len(nchpo1.MAIL)]:
		for m in r_[0:len(nmail1.MAIL)]:
			###print" nchpo1.MAIL[k,0]
			###print" nmail1.MAIL[k,0]
			if (nchpo1.MAIL[k,0]==nmail1.MAIL[m,0]) and (nchpo1.MAIL[k,1]==nmail1.MAIL[m,1]):
				liste.append(m)
				###print" m
				#raw_input()
				break
	
	
	
	
	Place_des_segments_de_sdm1_dans_champtotal=mat(liste)
	
	
	
	
	##print" nchpo1.MAIL,'nchpo1'		# semble ok
	##print" nmail1.MAIL,'nmail1.MAIL' # semble ok
	
	##print" liste
	##print" Place_des_segments_de_sdm1_dans_champtotal,'Place_des_segments_de_sdm1_dans_champtotal'
	#raw_input()
	#raw_input()
	###print" 	nchpo1, 'nchpo1'
	###print" 	nmail1.MAIL, 'nmail1.MAILbis'
	###print" 	Place_des_noeuds_de_sdm1_dans_champtotal, 'Place_des_noeuds_de_sdm1_dans_champtotalbis'
	chpoE = []
	###print" len(chpo1) , 'en(chpo1)'
	###print" shape(chpo1) ,'test'
	for i in r_[1:len(chpo1)+1]:
		###print" (Place_des_noeuds_de_sdm1_dans_champtotal).astype(int) , 'Place_des_noeuds_de_sdm1_dans_champtotal'
		###print" (chpo1[0].XVAL[Place_des_noeuds_de_sdm1_dans_champtotal.astype(int) ])
		##print" i,'i',chpo1[i-1],shape(chpo1[i-1].XVAL)
		xval1  = chpo1[i-1].XVAL[Place_des_segments_de_sdm1_dans_champtotal.astype(int)]
		comp   = chpo1[i-1].COMP
		unite  = chpo1[i-1].UNIT
		champx = UN_CHAMP(comp,unite,1)      # instanciation de l objet ob
		champx.XVAL = xval1
		chpoE.append(champx)
	
	return chpoE,Place_des_segments_de_sdm1_dans_champtotal
	
def mM_Prepro_Sdm_Gran3(xcoort1,chpoE,nmail1):
	#(nmail1,chpoE,numer1,nmailg1,km,numerg1,modlg1,idimg,xcoort1,mode1)
	#function [ME,MGE,MapSdm1] = mM_Prepro_Sdm_Gran3( ...
	#                                  nmail1,chpoE,numer1, ...
	#                                  nmailg1,km,numerg1, ...
	#                                  modlg1,idimg,xcoort1,mode1);
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 02 / 01 / 2003
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 24 / 08 / 2006
	#   On retourne aussi BsE
	# ICETA Damien      L.M.G.C. SYSTEMES MULTICONTACTS  le 27 / 05 / 2008
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 27 / 05 / 2008
	#   Cas du granulaire


	# Micro-macro approach : prepocessing for subdomains

	idim = shape(xcoort1)[1]
	
	if (idim == 2):
		idof = 3
	else:
		idof= 6
	
	
	
	
	
	###print" idim
	#raw_input()
	##print" 'Mass'
	
	# Partie classique
	
	#% modele
	#  [modl1,intg1] = ModlIntg13(nmail1,'ELASTIQUE','MASSE',mode1,idim);
	
	# masse
	chpo2,nmail2 = ExtrChpo(chpoE,['MASS'],['MASS']),nmail1
	#clear mass1;
	nbzone1 = 1 #length(nmail2); #simplification zone unique
	#for zo1 = 1:nbzone1
	#topo1 = nmail2{zo1}.MAIL;
	topo1 = nmail2.MAIL
	#m1 = chpo2{zo1}{1}.XVAL;
	m1 = chpo2[0].XVAL
	#nbel1 = size(topo1,1);
	#nbel1 = shape(topo1)[0]
	#xval1 = zeros(idim,idim,nbel1);
	
	
	#recuperation du rayon et calcul de l'inertie
	chpo3,nmail3 = ExtrChpo(chpoE,['RAYO'],['RAYO']),nmail1
	#clear mass1;
	nbzone1 = 1 #length(nmail2); #simplification zone unique
	#for zo1 = 1:nbzone1
	#topo1 = nmail2{zo1}.MAIL;
	#topo3 = nmail3.MAIL
	#m1 = chpo2{zo1}{1}.XVAL;
	r1 = chpo3[0].XVAL
	#nbel1 = size(topo1,1);
	#nbel1 = shape(topo1)[0]
	#xval1 = zeros(idim,idim,nbel1);
	
	# probleme cf correction provisoire
	#
	#xval1 = zeros((idim,idim,nbel1))
	#
	#for el1 in r_[0:nbel1]:
	#	#xval1(:,:,el1) = m1(el1,1) * eye(idim);
	#	###print" m1[el1]* eye(idim)
	#	#raw_input()
	#	xval1[:,:,el1] = m1[el1] * eye(idim)
	##end
	##mass1{zo1} = struct('XVAL',xval1)
	#
	# correction provisoire
	
	##print" 'm1',m1
	
	nbel1 = shape(m1)[0]
	
	##print" 'shape(m1)[0]',shape(m1)[0],'shape(m1)[1]',shape(m1)[1]
	xval1 = zeros([nbel1*idof,nbel1*idof])
	
	compteur =-1
	for el1 in r_[0:nbel1]:
		for id in r_[0:idof]:
			
			if (id<>2):
				compteur = compteur +1
				##print" 'compteur' , compteur, el1, id
				xval1[compteur,compteur] = m1[el1]
			else:
				compteur = compteur + 1
				##print" 'compteur' , compteur, el1, id
				xval1[compteur,compteur] = m1[el1]*r1[el1]*r1[el1]	
			
			
			
			
	#fin de correction provisoire
	##print" 'xval1' ,xval1
	##print" 'shape(xval1)',shape(xval1),nbel1	
	
	mass1 = UN_CHAMP('MASS','unite',1)      # instanciation de l objet ob
	mass1.XVAL = xval1
	
	
	###print" xval1
	#raw_input()
	
	
	#clear topo1 m1;
	#end
	#%  [modl1,intg1] = ModlIntg13(mail1,'ELASTIQUE','RIGIDITE',mode1,idim);
	#%  matr1 = ChmlToCham(chmlE,mail1,intg1);
	#%  [rigi1,bsigma1] = Rigi9(modl1,matr1,mail1,intg1,xcoort1,mode1, ...
	#%                          'GenDualOp');
	#%  clear matr1;
	
	#	% Partie macro locale
	#% ...................
	#
	#% Pseudo-rigidite macro locale
	#  rigig1 = ManuRigi(nmailg1,km*eye(idimg,idimg));


	#  disp('  Assembling masses')

	#% On passe en matriciel
	#% .....................


	#% Assemblages de la rigidite classique
	#  [listDdlDual1,listDdlPrim1] = ListDdlModl2(modl1);
	#  [mapddlDual1,mapddlPrim1] = MapddlRigi2(modl1,nmail1, ...
	#										  numer1,listDdlDual1, ...
	#										  numer1,listDdlPrim1);
	#  ME = RigiToMatrix(mass1,modl1,nmail1, ...
	#					numer1,mapddlDual1,listDdlDual1, ...
	#					numer1,mapddlPrim1,listDdlPrim1);
	#
	#% Assemblage de la pseudo-rigidite macro
	#  [listDdlDualg1,listDdlPrimg1] = ListDdlModl2(modlg1);
	#  [mapddlDualg1,mapddlPrimg1] = MapddlRigi2(modlg1,nmailg1, ...
	#											numerg1,listDdlDualg1, ...
	#											numerg1,listDdlPrimg1);
	#  MGE = RigiToMatrix(rigig1,modlg1,nmailg1, ...
	#					 numerg1,mapddlDualg1,listDdlDualg1, ...
	#					 numerg1,mapddlPrimg1,listDdlPrimg1);
	#
	#%% Assemblage de Bsigma
	#%  [listComDual1,listComPrim1] = ListCompModl3(modl1);
	#%  [mapcomDual1,mapddlDual0] = MapcomB2(modl1,mail1,intg1, ...
	#%                                       numer2,listComDual1, ...
	#%                                       numer1,listDdlDual1, ...
	#%                                       'DUAL');
	#%  BsE = BToMatrix3(bsigma1,modl1,mail1,intg1, ...
	#%                   numer1,mapddlDual1,listDdlDual1, ...
	#%                   numer2,mapcomDual1,listComDual1, ...
	#%                   'DUAL');

	#  clear intg1 modl1;


	
	#MapSdm1 = struct( ...
	#'NUMP',numer1,'MAPP',mapddlPrim1,'NAMP',{listDdlPrim1}, ...
	#'NUMD',numer1,'MAPD',mapddlDual1,'NAMD',{listDdlDual1});
	
	
	#tentative non esthetique
	numer1 = nmail1.MAIL
	nameddlPrim1 = (['UX','UY','RZ'])

	listDdlPrim1x=[]
	listDdlPrim1y=[]
	listDdlPrim1z=[]
	for i in numer1:         #!!!!! 2D creer une matrice avec les deux coordonees	
		listDdlPrim1x = hstack([listDdlPrim1x,3*i-2])
		listDdlPrim1y = hstack([listDdlPrim1y,3*i-1])
		listDdlPrim1z = hstack([listDdlPrim1z,3*i])
		
		
		
	listDdlPrim1 = vstack([ listDdlPrim1x , listDdlPrim1y , listDdlPrim1z]).T
	##print" 'listDdlPrim1',listDdlPrim1

	MapSdm1 = UNE_MAP(mat(numer1), nameddlPrim1 ,listDdlPrim1)
	return mass1,MapSdm1


def mM_Prepro_Sdm_Gran3_opti(xcoort1,chpoE,nmail1):
	#(nmail1,chpoE,numer1,nmailg1,km,numerg1,modlg1,idimg,xcoort1,mode1)
	#function [ME,MGE,MapSdm1] = mM_Prepro_Sdm_Gran3( ...
	#                                  nmail1,chpoE,numer1, ...
	#                                  nmailg1,km,numerg1, ...
	#                                  modlg1,idimg,xcoort1,mode1);
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 02 / 01 / 2003
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 24 / 08 / 2006
	#   On retourne aussi BsE
	# ICETA Damien      L.M.G.C. SYSTEMES MULTICONTACTS  le 27 / 05 / 2008
	# DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 27 / 05 / 2008
	#   Cas du granulaire


	# Micro-macro approach : prepocessing for subdomains

	idim = shape(xcoort1)[1]
	
	if (idim == 2):
		idof = 3
	else:
		idof= 6
	
	
	
	
	
	###print" idim
	#raw_input()
	##print" 'Mass'
	
	# Partie classique
	
	#% modele
	#  [modl1,intg1] = ModlIntg13(nmail1,'ELASTIQUE','MASSE',mode1,idim);
	
	# masse
	chpo2,nmail2 = ExtrChpo(chpoE,['MASS'],['MASS']),nmail1
	#clear mass1;
	nbzone1 = 1 #length(nmail2); #simplification zone unique
	#for zo1 = 1:nbzone1
	#topo1 = nmail2{zo1}.MAIL;
	topo1 = nmail2.MAIL
	#m1 = chpo2{zo1}{1}.XVAL;
	m1 = chpo2[0].XVAL
	#nbel1 = size(topo1,1);
	#nbel1 = shape(topo1)[0]
	#xval1 = zeros(idim,idim,nbel1);
	
	
	#recuperation du rayon et calcul de l'inertie
	chpo3,nmail3 = ExtrChpo(chpoE,['RAYO'],['RAYO']),nmail1
	#clear mass1;
	nbzone1 = 1 #length(nmail2); #simplification zone unique
	#for zo1 = 1:nbzone1
	#topo1 = nmail2{zo1}.MAIL;
	#topo3 = nmail3.MAIL
	#m1 = chpo2{zo1}{1}.XVAL;
	r1 = chpo3[0].XVAL
	#nbel1 = size(topo1,1);
	#nbel1 = shape(topo1)[0]
	#xval1 = zeros(idim,idim,nbel1);
	
	# probleme cf correction provisoire
	#
	#xval1 = zeros((idim,idim,nbel1))
	#
	#for el1 in r_[0:nbel1]:
	#	#xval1(:,:,el1) = m1(el1,1) * eye(idim);
	#	###print" m1[el1]* eye(idim)
	#	#raw_input()
	#	xval1[:,:,el1] = m1[el1] * eye(idim)
	##end
	##mass1{zo1} = struct('XVAL',xval1)
	#
	# correction provisoire
	
	##print" 'm1',m1
	
	nbel1 = shape(m1)[0]
	
	##print" 'shape(m1)[0]',shape(m1)[0],'shape(m1)[1]',shape(m1)[1]
	#xval1 = zeros([nbel1*idof,nbel1*idof])
	Liste_MSE =[]
	
	compteur =-1
	for el1 in r_[0:nbel1]:
		for id in r_[0:idof]:
			
			if (id<>2):
				#compteur = compteur +1
				##print" 'compteur' , compteur, el1, id
				#xval1[compteur] = m1[el1]
				Liste_MSE =hstack([Liste_MSE,m1[el1]])
			else:
				#compteur = compteur + 1
				##print" 'compteur' , compteur, el1, id
				#xval1[compteur] = m1[el1]*r1[el1]*r1[el1]
				Liste_MSE =hstack([Liste_MSE,m1[el1]*r1[el1]*r1[el1]])	
			
			
			
			
	#fin de correction provisoire
	##print" 'xval1' ,xval1
	##print" 'shape(xval1)',shape(xval1),nbel1	
	
	#mass1 = UN_CHAMP('MASS','unite',1)      # instanciation de l objet ob
	#mass1.XVAL = xval1
	
	
	###print" xval1
	#raw_input()
	
	
	#clear topo1 m1;
	#end
	#%  [modl1,intg1] = ModlIntg13(mail1,'ELASTIQUE','RIGIDITE',mode1,idim);
	#%  matr1 = ChmlToCham(chmlE,mail1,intg1);
	#%  [rigi1,bsigma1] = Rigi9(modl1,matr1,mail1,intg1,xcoort1,mode1, ...
	#%                          'GenDualOp');
	#%  clear matr1;
	
	#	% Partie macro locale
	#% ...................
	#
	#% Pseudo-rigidite macro locale
	#  rigig1 = ManuRigi(nmailg1,km*eye(idimg,idimg));


	#  disp('  Assembling masses')

	#% On passe en matriciel
	#% .....................


	#% Assemblages de la rigidite classique
	#  [listDdlDual1,listDdlPrim1] = ListDdlModl2(modl1);
	#  [mapddlDual1,mapddlPrim1] = MapddlRigi2(modl1,nmail1, ...
	#										  numer1,listDdlDual1, ...
	#										  numer1,listDdlPrim1);
	#  ME = RigiToMatrix(mass1,modl1,nmail1, ...
	#					numer1,mapddlDual1,listDdlDual1, ...
	#					numer1,mapddlPrim1,listDdlPrim1);
	#
	#% Assemblage de la pseudo-rigidite macro
	#  [listDdlDualg1,listDdlPrimg1] = ListDdlModl2(modlg1);
	#  [mapddlDualg1,mapddlPrimg1] = MapddlRigi2(modlg1,nmailg1, ...
	#											numerg1,listDdlDualg1, ...
	#											numerg1,listDdlPrimg1);
	#  MGE = RigiToMatrix(rigig1,modlg1,nmailg1, ...
	#					 numerg1,mapddlDualg1,listDdlDualg1, ...
	#					 numerg1,mapddlPrimg1,listDdlPrimg1);
	#
	#%% Assemblage de Bsigma
	#%  [listComDual1,listComPrim1] = ListCompModl3(modl1);
	#%  [mapcomDual1,mapddlDual0] = MapcomB2(modl1,mail1,intg1, ...
	#%                                       numer2,listComDual1, ...
	#%                                       numer1,listDdlDual1, ...
	#%                                       'DUAL');
	#%  BsE = BToMatrix3(bsigma1,modl1,mail1,intg1, ...
	#%                   numer1,mapddlDual1,listDdlDual1, ...
	#%                   numer2,mapcomDual1,listComDual1, ...
	#%                   'DUAL');

	#  clear intg1 modl1;


	
	#MapSdm1 = struct( ...
	#'NUMP',numer1,'MAPP',mapddlPrim1,'NAMP',{listDdlPrim1}, ...
	#'NUMD',numer1,'MAPD',mapddlDual1,'NAMD',{listDdlDual1});
	
	
	#tentative non esthetique
	numer1 = nmail1.MAIL
	nameddlPrim1 = (['UX','UY','RZ'])

	listDdlPrim1x=[]
	listDdlPrim1y=[]
	listDdlPrim1z=[]
	for i in numer1:         #!!!!! 2D creer une matrice avec les deux coordonees	
		listDdlPrim1x = hstack([listDdlPrim1x,3*i-2])
		listDdlPrim1y = hstack([listDdlPrim1y,3*i-1])
		listDdlPrim1z = hstack([listDdlPrim1z,3*i])
		
		
		
	listDdlPrim1 = vstack([ listDdlPrim1x , listDdlPrim1y , listDdlPrim1z]).T
	##print" 'listDdlPrim1',listDdlPrim1

	MapSdm1 = UNE_MAP(mat(numer1), nameddlPrim1 ,listDdlPrim1)
	return Liste_MSE,MapSdm1
	
def Mm_Prepro_MapSdm(MapSdm1,ListMapInt2):
	#function [bE,mapLocInt1] = mM_Prepro_MapSdm2(MapSdm1,ListMapInt2);
	#% DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 03 / 01 / 2003
	#% DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 10 / 02 / 2003
	#%   passage de donnees locales au sous domaine
	#%
	#% Micro-macro approach: local interface assembling facilities

	nint2 = len(ListMapInt2)

	#% Matrice booleenne bE d'assemblage des quantites d'interfaces locales
	#% vers le sous domaine... on suppose dual et primal identiques !
	#nbddlsdm1 = max(max(MapSdm1.MAPP))
	nbddlsdm1 = concatenate((MapSdm1.MAPP)).max()

	#nbddlsdm1 = array(nbddlsdm1)[0] # passage matrice array
	##print" nbddlsdm1,'nbddlsdm1'
	#bE = zeros([1,nbddlsdm1])
	# clear nbddlsdm1;
	for i1 in r_[0:nint2]:
		MapInt1 = ListMapInt2[i1]
		
		bEiPrim1 = AssembleOperator( MapInt1.NUMP,MapInt1.MAPP,MapInt1.NAMP,MapSdm1.NUMP,MapSdm1.MAPP,MapSdm1.NAMP)
		##print" 'shape(bEiPrim1)',shape(bEiPrim1)
		
		if (i1==0):
			#bE = sparse.lil_matrix(bE)
			bE = (bEiPrim1)
		else:
			#bE = sparse.lil_matrix(hstack(bE,bEiPrim1))
			
			##print" 'bE',shape(bE)
			##print" 'bEiPrim1',shape(bEiPrim1)
			
			bE = (vstack([bE,bEiPrim1]))
	#%MapInt1.NUMP
	#%MapInt1.MAPP
	#%MapSdm1.NUMP
	#%MapSdm1.MAPP
	#%bEiPrim1
	#%keyboard

	#	clear MapInt1 bEiPrim1;
	#  end

	#% Mapping des quantites d'interfaces locales vers l'interface locale
	#mapLocInt1 = zeros([0,len(MapSdm1.NAMP)]);
	mapLocInt1 = zeros([0,len(MapSdm1.MAPP[0])]);
	shift1 = 0
	for i1 in r_[0:nint2]:
		MapInt1 = ListMapInt2[i1]
		#%   On suppose tous les MapInt1.NAMP identiques a MapSdm1.NAMP !
		mapLocInt1 = vstack([mapLocInt1 , MapInt1.MAPP + shift1])
		#shift1 = shift1 + max(max(MapInt1.MAPP))
		shift1 = shift1 + concatenate(MapInt1.MAPP).max()

		#clear MapInt1;
	#end
	#% Si on a une quantite definie sur les interfaces locales
	#% Vc(idim,nbnoi)
	#% Si on a un vecteur sur l'ensemble des ddl d'interface V(nbnoi*idim,1)
	#% Si on a un vecteur sur l'ensemble des ddl du sdm U(nbddl,1)
	#% Pour passer de U a V : V = bE * U
	#% Pour passer de V a Vc : Vc = V(mapLocInt1)'
	#% Pour passer de Vc a V : clear V; V(mapLocInt1) = Vc'; V = V';


	#% Matrice booleenne signee CE d'assemblage des quantites macro
	#% d'interfaces locales vers les quantites macro de l'interface
	#% globale... on suppose dual et primal identiques !
	#% Attention a la convention de signe.
	#%%  CE = sparse(nint2*idimg,nint1*idimg);
	#%%  for i1 = 1:nint2
	#%%    sign1 = lsign2(i1);
	#%%    CE((i1-1)*idimg+1:i1*idimg,(int1-1)*idimg+1:int1*idimg) = ...
	#%%      sign1*eye(idimg);
	#%%  end
	
	return bE,mapLocInt1

def AssembleOperator(num1,map1,nam1,num2,map2,nam2):
	#function [bE] = AssembleOperator(num1,map1,nam1,num2,map2,nam2);
	#% DUREISSEIX David  L.M.G.C. SYSTEMES MULTICONTACTS  le 29 / 12 / 2002

	#% Use two mappings of dof, to build the mapping matrix from
	#% one dof ordering to the other

	#% Input
	#%   num1(nbnode1)         node numbers of the first mapping
	#%   nam1(nbname1)         dof names of the first mapping
	#%   map1(nbnode1,nbname1) dof number in the first mapping
	#%   num2(nbnode2)         node numbers of the second mapping
	#%   nam2(nbname2)         dof names of the second mapping
	#%   map2(nbnode2,nbname2) dof number in the second mapping

	#% Output
	#%   bE(nbddl1,nbddl2)     boolean mapping operator

	#% The dof number of node occurence ino1, i.e. of node number
	#% num1(ino1), and name occurence ina1, i.e. of name nam1(ina1),
	#% is dof1 = map1(ino1,ina1)
	#% if dof1 = 0, the dof is not present in the assembled vector.
	#% Same thing for the second mapping.
	#% If V1 is the vector of the first mapping, and V2 of the second one,
	#% V1 = bE * V2
	####print" 'num1',num1,nam1,num2,nam2
	#raw_input()
	
	
	
		

	
	#taillenam1=shape(nam1)[0]

	#nam1new=array(nam1)[0]
	#
	#for i in r_[1:taillenam1]: #on ajoute tous les ddls dans un seul vecteur
	#	###print" i
	#	#nam1new=hstack([nam1new,array(nam1)[i]])
	#	nam1new=concatenate((nam1new,array(nam1)[i]), axis=None)
	
	#nam1=array(nam1[0])+array(nam1[1])

	# pb de liste et array a optimiser
	#taillenam1=shape(nam1[0])[1]
	####print" 'taillenam1',taillenam1
	#raw_input()
	
	#for i in r_[1:taillenam1]
	
	#nam1new = 

	
	#nam1 = nam1new

	

	#nam2=array(nam2)[0] # passage matrice array
	
	####print" 'num1',num1,nam1,num2,nam2
	#raw_input()
	
	#nam1=nam1.astype(int)
	
	#format matrice -> liste
	num1 = mat(num1)
	num1 = num1.reshape(shape(num1)[1])
	num1 = array(num1)[0]
	#format matrice -> liste
	num2 = mat(num2)
	num2 = num2.reshape(shape(num2)[1])
	num2 = array(num2)[0]
	
	
	
	
	
	
	#print 'map1', map1
	#print 'nam1',nam1
	#print 'num2',num2
	#print 'nam2',nam2
	
	####print" 'la'
	#raw_input()
	
	
	nbddl1 = len(num1)*len(nam1) -1 #concatenate(map1).max()
	nbddl2 = len(num2)*len(nam2) -1 #concatenate(map2).max()
	
	
	#bE = sparse(nbddl1,nbddl2); #optimisation sparse
	####print" nbddl1,'nbddl1',nbddl2

	bE = zeros([nbddl1+1,nbddl2+1])
	#% Find num1 in num2, and nam1 in nam2
	in1 = digitize(num1,num2) -1 #donne la place des num1 dans num2
	####print" in1,'in1'
	
	id1 = []
	#id1 = digitize(nam1,nam2)
	for i in r_[0:len(nam1)]:   # pas beau ces boucles mais pas mieu pour l instant
		for j in r_[0:len(nam2)]:
			####print" i,j
			####print" 'nam1[i]',nam1[i]
			###print" 'nam2[j]',nam2[j]
			if (nam1[i]==nam2[j]):
				id1 = hstack([id1,j])
	
	
	for ino1 in r_[0:len(num1)]:
		for ina1 in r_[0:len(nam1)]:
			
			##print" ino1,'ino1'
			##print" ina1,'ina1'
			##print" map1
			ddl1 = ino1*len(nam1)+ina1
			##print" 'ddl1',ddl1
			ino2 = in1[ino1]
			ina2 = id1[ina1]
			#%if (num1(ino1) ~= num2(ino2) | ~all(nam1{ina1} == nam2{ina2}))
			#%  error('PBPB')
			#%end
			#if (ino2 <>0 ) and (ina2 <>0) and (ddl1<>0):	
			##print" ino2,'ino2'
			##print" ina2,'ina2'				
				
			ddl2 = ino2*len(nam1)+ina2   # map2[ino2,ina2]
			#if (ddl2<>0):
			##print" 'test555',ddl1,ddl2
			bE[ddl1,ddl2] = 1
			#end
			#end 
		#end
	#end

	return bE

