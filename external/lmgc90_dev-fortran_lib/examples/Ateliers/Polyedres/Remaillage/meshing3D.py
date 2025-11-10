from gmshpy import *
import numpy

class MeshGenerator(object):
   def __init__(self, nameMesh, dimension, lc):
      self.dim = dimension
      self.order = 1
      self.nameMesh = nameMesh
      self.model = GModel()
      self.model.setFactory("Gmsh")
      self.lc=lc   
      self.tag=0
      self.g=numpy.zeros(3)
      
      self.nodes=[]
      self.edges=[]
      self.faces=[]
           
      GmshSetOption('Print', 'GeoOnlyPhysicals', 0.)
      #GmshSetOption('Geometry', 'autoCoherence', 1.)
      #GmshSetOption('Mesh',  'CharacteristicLengthFactor', lc)
      GmshSetOption('Mesh',  'Algorithm', 6.)
      GmshSetOption('Mesh',  'CharacteristicLengthMin', lc)
      GmshSetOption('Mesh',  'CharacteristicLengthMax', lc)
      GmshSetOption('Mesh',  'Optimize', 1.0) 



   def addNode(self,coor):
     """ xx.addNode(coor);

      Use gmshpy module to add a node to a GModel

      Parameters:

      - coor: array of coordinates

     """
     v = self.model.addVertex(coor[0], coor[1], coor[2], self.lc)
     self.nodes.append(v)
     self.g += numpy.array(coor)

   def computeBarycenter(self):

     self.g = self.g/len(self.nodes)   
       
     
   def addEdge(self,b,e):
     """ xx.addEdge(b,e);

      Use gmshpy  module to add an Edge to a GModel

      Parameters:

      - b,e: index of the nodes

     """
     ee = self.model.addLine(self.nodes[b],self.nodes[e])
     self.edges.append(ee)
     ee.addPhysicalEntity(1000)

   def addFace(self,le):
     """ xx.addFace(le);

      Use gmshpy  module to add a Face to a GModel

      Parameters:

      - le: index of edges

     """
     contour=[]    
     for ie in le:
        if ie > 0 : 
          contour.append(self.edges[ie])
        else:
          #contour.append(self.edges[-ie].reverse())
          contour.append(self.edges[-ie])          
          
     #if len(contour) < 4:
     #  f = self.model.addPlanarFace([contour])
     #else:  
     #  # marche pas -> 
     #  f = self.model.addRuledFaces(GEdgeVector([contour]))
     #  #f = self.model.addPlanarFace([contour])

     if  len(contour) < 5:
       f = self.model.addRuledFaces([contour])
       #print f
       for i in f:
         i.addPhysicalEntity(self.tag)
     else:
       f = self.model.addPlanarFace([contour])
       f.addPhysicalEntity(self.tag)

     #numericalTagsOfFace = f.getPhysicalEntities()

     #self.tag +=1

     self.faces.append(f)
     
   def addVolume(self):

     v = self.model.addVolume([self.faces])
            
   def dump(self):
     #self.model.healGeometry()       
     self.model.writeGEO(self.nameMesh+".geo")

     self.model = GModel()
     self.model.load(self.nameMesh+".geo")

     self.model.mesh(self.dim)
     ##self.model.removeDuplicateMeshVertices(1e-3) 
     self.model.save(self.nameMesh+".msh")

     
       




     
