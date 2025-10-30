from numpy  import arange

from ..config.lmgc90dicts import *
from ..utilities.error import *
from .lecture import *

from ..avatar import *
from ..avatar.contactor import *
from ..models import *

from ..shared import model as T_model


## @class mesh
# a mesh holds
#  -nodes
#  -elements (=bulks)
#  -groups
#  -models
#  -materials       
#  -contactors
# @todo : update doxygen and docstrings
class mesh(avatar):
    """ class allowing to define meshed avatars from a mesh file
        Methods :
          - __init__
          - lecture
          - imposeDrivenDof
          - imposeInitValue
          - rotateBody
          - moveBody
          - addNodes
          - addGroups
          - addElements
          
    """
    ## constructor de mesh(part)
    #@todo format ne sert a rien puisque le type est devine a partir de l'extension du fichier !
    def __init__(self,number=1,meshFile=None,
                 meshType='normal',dimension=2):
        
        """ 
        __init__(self,number=1,meshFile=None,
                 meshType='normal',dimension=2)
        create an avatar ot type MAILx
        """
        self.meshFile  = meshFile
        self.meshType  = meshType 
        # models of the mesh
        self.dofModel        = []
        # nodes involved with a given model
        self.model2nodes     = {}
        # 
        self.drvDofModel     = []
        #
        self.modelDrvDof2nodes    = {}
        
        # appel du constructeur de la super classe
        self.avatar = avatar(number=number,dimension = dimension)
  
        if meshFile != None:
            self.read(dimension)

    ## reading of mesh file
    def read(self,dim):
          """read(self,dim)
          read a mesh from a file of gmsh or sysweld format
          if mesh file has 'msh' extension, file is of type gmsh
                           'txt' extension, file is of type sysweld
          dimension of mesh is given in input because gmsh does not make any difference between 2D and 3D
                                  
          """
          
          ensNoeud,ensElem    =   lecture(self.meshFile,dim)
               
          self.avatar.addNodes(ensNoeud)
          self.avatar.addElements(ensElem)
          self.avatar.bulks.setAdjacBulk2Nodes(self.avatar.nodes)

          self.avatar.defineGroups()

          print('\t Mesh body numbered\t:',self.number,' read ')

                    
    def __str__(self):
        impr='Body :\t%s\tof type :\t%s\n' %(self.number,self.atype)
        impr +='\tNumber of elements :\t%d\n\tNumber of nodes :\t%d\n' % (len(self.elements),len(self.nodes))
        return impr

    
    def vtkObject(self,factor=200,centrumX=200,centrumY=200):
         # fait en haut from vtk import vtkPoints,vtkCellArray,vtkPolyData,vtkPolyDataMapper,vtkActor2D
        
         try:
             import vtk
         except ImportError:
             print('Unable to import vtk module')
             return None

         pts    = vtk.vtkPoints()
         lines  = vtk.vtkCellArray()
         datas  = vtk.vtkPolyData()
         mapper = vtk.vtkPolyDataMapper2D()
    
         if self.dimension == '2':
             actor = vtk.vtkActor2D()
             if 'Line' not in self.groups.liste():
                 msg="No outside boundary defined"
                 showError(msg)
                 return actor
             ext=self.groups['Line']

             ListeNodes=list(map(int,ext.nodes))
             ListeNodes.sort()
             ListeNodes=list(map(str,ListeNodes))

             for n in ListeNodes:
                pts.InsertPoint(int(n),float(self.nodes[n].x)*factor+centrumX,float(self.nodes[n].y)*factor+centrumY,0.)
                
             for e in ext.elements:
                 lines.InsertNextCell(len(self.elements[e].connectivity))
                 for j in self.elements[e].connectivity:
                     lines.InsertCellPoint(j)
                    
             datas.SetPoints(pts)
             datas.SetLines(lines)
             mapper.SetInput(datas)
             actor.SetMapper(mapper)

             return actor
            
#
# ---- Ajout de fonction pour le calcul de grandeur volumique en preprocessing
#
    def initGaussPoints(self):
       if self.etype == 'T3xxx':
           pointsGauss=[{'coordonnees':(1./3.,1./3.),'poids':1.},
                        {'coordonnees':(2./3.,1./6.),'poids':1./3.},
                        {'coordonnees':(1./6.,2./3.),'poids':1./3.},
                        {'coordonnees':(1./6.,1./6.),'poids':1./3.}]
       GP=gaussPoints()
       numero = 0
       for pg in pointsGauss:
           GP.addGP(numero,gausspoint(pg['coordonnees'][0],pg['coordonnees'][1],poids=pg['poids']))
           numero=numero+1
       self.gaussPoints=GP
       
       for gp in self.gaussPoints.liste():

           coorG=matrixmultiply(self.shape(self.gaussPoints[gp].xi,
                                           self.gaussPoints[gp].eta,
                                           self.gaussPoints[gp].zeta),self.coordNode)
           self.gaussPoints[gp].x = coorG[0]
           self.gaussPoints[gp].y = coorG[1]
           # AJOUTER SUIVANT LA DIMENSION DE L ELEMENT
           self.gaussPoints[gp].z = 0.
           
       for gp in self.gaussPoints.liste():
           J           = matrixmultiply(self.DN(self.gaussPoints[gp].xi,
                                           self.gaussPoints[gp].eta,
                                           self.gaussPoints[gp].zeta),self.coordNode)
           detJ        = J[0,0]*J[1,1]-J[0,1]*J[1,0]
           self.gaussPoints[gp].J        = J
           self.gaussPoints[gp].detJ     = detJ
           

    def shape(self,x,y,z):
        if self.etype == 'T3xxx':
            N=array([x,y,1.-x-y],'f')
        return N
        

    def DN(self,x,y,z):
        if self.etype == 'T3xxx':
            Dn  = array([[1.,0.,-1.],
                     [0.,1.,-1.]],'f')
        return Dn


    def computeVolumicFlux(self,fluxSource,t):
        vector=array([0.,0.,0.],'f')
        for pg in self.gaussPoints.liste():
            func   = fluxSource.fluxVolumique(self.gaussPoints[pg].x,self.gaussPoints[pg].y,self.gaussPoints[pg].z,t)
            vector = vector + self.gaussPoints[pg].poids*1./2.*self.gaussPoints[pg].detJ*self.shape(
            self.gaussPoints[pg].xi,self.gaussPoints[pg].eta,self.gaussPoints[pg].zeta)*func
        return vector


    def imposeVolumicThermalFlux(self,group='all',fluxType=None,temps=arange(0.,10.,1.)):
        listElements = self.find(etype='elements',group=group)
        for el in listElements:
            self.elements[el].initGaussPoints()
        for el in listElements:
            for t in temps:
                self.elements[el].computeVolumicFlux(fluxType,t)

