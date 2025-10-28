from __future__ import print_function
from gmshpy import *
from pylmgc90.pre import *
from pylmgc90.chipy import *

import numpy as np

class ParticlesToMesh(object):
   """ 
   """ 
   def __init__(self, nameMesh, dimension, mesh_group, nb_fields):
      self.dim = dimension
      self.order = 1
      self.nameMesh = nameMesh
      self.model = GModel()
      self.model.load(nameMesh+'.geo')
      self.model.load(nameMesh+'.msh')

      # on recupere depuis gmsh le maillage que lmgc90 utilise
      all = readMesh(name=nameMesh+'.msh', dim=dimension)
      meshes=all.separateMeshes(dim=dimension, entity_type="geometricalEntity", keep_all_elements=False)

      # map eleid gmsh vers eleid lmgc90   
      self.gmsh2lmgc_eleid={}

      # map de eleid lmgc90 vers l'eleid gmsh 
      self.lmgc2gmsh_eleid={}

      # on ne conserve qu'un maillage dans le fichier gmsh
      mesh = meshes[mesh_group]

      # peche aux noeuds
      self.nb_nodes = len(mesh.nodes)
      coor=np.zeros((self.nb_nodes,dimension),dtype=float)
      for idn,my_node in enumerate(mesh.nodes):
        coor[idn,:]=my_node.coor 
    
      # peche aux connectivites
      self.nb_elements = len(mesh.bulks)
      elem=self.nb_elements*[0]
      for ide,my_bulk in enumerate(mesh.bulks):
         elem[ide]=my_bulk.connectivity 

         # remplissage map (ce sont des dicos)
         if my_bulk.originalnumber != None:
           self.gmsh2lmgc_eleid[my_bulk.originalnumber]=ide+1
           self.lmgc2gmsh_eleid[ide+1] = my_bulk.originalnumber

      self.mesh=[coor,elem]

      ### pour le calcul
      # le premier field est le nombre de particules par maille
      self.nb_fields = nb_fields      
      self.element_fields=np.zeros((1+nb_fields,self.nb_elements),dtype=float)

      # le premier field est le volume d un noeud
      self.node_fields=np.zeros((1+nb_fields,self.nb_nodes),dtype=float)

      for i in range(self.nb_elements) :  
         gmsh_ele = self.model.getMeshElementByTag(self.lmgc2gmsh_eleid[i+1])
         v_ele = gmsh_ele.getVolume()
         N = gmsh_ele.getNumVertices()

         lmgc_ele=self.mesh[1][i]

         # on teste que ca ne deconne pas trop
         if N != len(lmgc_ele):
           print('Non conforming number of nodes')
           sys.exit()

         SF = 1./N

         # calcul du volume aux noeuds 
         for j in range(N) :
            self.node_fields[0,lmgc_ele[j]-1] += SF * v_ele

      print('surface totale : ',sum(self.node_fields[0,:]))
            
      self.nb_particles = 0
      self.particles = {}

      ### pour la visu

      # rien

   def cleanMesh(self):
      #for k in xrange(self.nb_fields+1):
      #  self.element_fields[k,:] = 0.

      self.element_fields[:,:] = 0.

      # on conserve le volume aux noeuds
      #for k in xrange(1,self.nb_fields+1):
      #  self.node_fields[k,:] = 0.

      self.node_fields[1:,:] = 0.

      self.nb_particles=0
      self.particles={}

   def pushOneParticleToMesh(self,coor,values):

      # on cherche ele et proj de la particule
      gmsh_eleid,proj = self._getlocalprojection(coor[0],coor[1],0.)
      if gmsh_eleid == 0: return

      # on recupere le rang de l ele lmgc90 (de 1 a n !)
      lmgc_eleid = self.gmsh2lmgc_eleid[gmsh_eleid]

      # on incremente le nb de particules de la maille
      self.element_fields[0,lmgc_eleid-1] += 1. 

      connectivity = self.mesh[1][lmgc_eleid-1]

      # on parcourt le field 1 ... nb_fields+1
      for k in range(self.nb_fields):
        self.element_fields[k+1,lmgc_eleid - 1] += values[k] 
        for j in range(len(connectivity)):
          self.node_fields[k+1,connectivity[j]-1] += proj[j]*values[k]

      # tableau qui va de 1 a n
      self.nb_particles+=1
      self.particles[self.nb_particles]=(values[0],lmgc_eleid,proj)     

   def ComputeNodalFieldsOnMesh(self,p0,d,mu,Cp,Cd,Vs):
 
      # fd fait de la merde car il pre-suppose ce qui est dans les fields 
      # node_fields : nb_particule, area, vx, vy

      for i in range(self.nb_nodes):
        if self.node_fields[1,i] != 0.:
          # vx,vy,...  
          self.node_fields[2:,i] = self.node_fields[2:,i] / min(self.node_fields[0,i],self.node_fields[1,i])
          # puis calcul compacite
          self.node_fields[1,i] = min(1.,self.node_fields[1,i] / self.node_fields[0,i])        

      # la physique 
      for i in range(self.nb_nodes):
        phi = 1. - self.node_fields[1,i]
        # Cp = 2*phi+1/3
        Cp[i] = min(0.75,(2.*phi + 1.)/3.)
        # Cd = p0 K(phi)/mu 
        Cd[i] = p0*d*d*(Cp[i]**3)/(180.*((1.-Cp[i])**2)*mu) 
        # p0*vs
        Vs[2*i]   = p0*self.node_fields[2,i]
        Vs[2*i+1] = p0*self.node_fields[3,i]

   def ComputeParticleForce(self,gradP):
      #
      F=np.zeros(self.dim*self.nb_particles)

      # le tableau particles va de 1 a n
      for i in range(1,self.nb_particles+1):       
        area = self.particles[i][0]
        lmgc_eleid = self.particles[i][1]
        proj = self.particles[i][2]
        connectivity = self.mesh[1][lmgc_eleid-1] 
        # interpolation
        gx = 0. ; gy = 0. ; cs = 0.
        for k in range(len(connectivity)):
          cs += proj[k]*self.node_fields[1,connectivity[k]-1]
          gx += proj[k]*gradP[connectivity[k]-1,0]
          gy += proj[k]*gradP[connectivity[k]-1,1]
        # 
        F[2*(i-1)]    = - area*gx/cs
        F[2*(i-1)+1]  = - area*gy/cs

      return F

   def pushParticlesToMesh(self,coors):

       ele,lcoor= self._getlocalcoordinates(coors)

       for i in range(len(ele)):
          if ele[i] < 0 : continue
          lmgc_eleid = self.gmsh2lmgc_eleid[ele[i]]
          self.element_fields[0,lmgc_eleid-1] += 1. 

   def _getlocalprojection (self, x, y, z) :
      p = SPoint3(x,y,z)
      element = self.model.getMeshElementByCoord (p, self.dim)
      if (element == None) : return 0, None
      u,v,w=element.xyz2uvw(x,y,z)
      #
      N = element.getNumVertices()
      SF = fullVectorDouble (N)
      element.getShapeFunctions(u,v,w,SF.getDataPtr())
      #
      proj=np.zeros((N))
      for i in range(N):
        proj[i]=SF(i)

      return element.getNum(),proj

   def _getlocalcoordinates ( self, coor) :
      element,lcoor = self.model.getMeshElementsByCoords(coor)
      return [e.getNum() if e else -1 for e in element],lcoor 

