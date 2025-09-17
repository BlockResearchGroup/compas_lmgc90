from __future__ import print_function

def remesh(coor,connectivity,name,lc):
  '''
    coor : coordinates of nodes
    connectivity: flat vector with arbitrary conectivity of faces
    name : file name of the mesh
    lc : mesh size
  '''  
  
  nbn = coor.shape[0]/3
  
  print('nombre de noeuds ',nbn)

  # lets scan the connectivity vector
  idx=0

  nbf = connectivity[idx]
  idx+=1
  
  print('nombre de faces ',nbf)

  le=[] # liste des edges
  ie=0  # index du dernier edge pose dans la liste des edges

  lf={} # liste des faces

  for f in range(nbf):
    # nombre de sommets de la face  
    nbs = connectivity[idx]
    idx +=1
  
    lf[f]=[] # a list to store the edges of a face
  
    ls = [] # a list to store the vertex of a face (ordered) 
    for i in range(nbs):
      s=connectivity[idx]
      idx+=1
      
      ls.append(s)
      
    # building a common edge list
    for i in range(nbs):
      nd = ls[i]
      nf = ls[(i+1)%nbs]
      e = (min(nd,nf),max(nd,nf))

      signe=1 
             
      if e in le :
        idx_e = signe*le.index(e)
      else:
        le.append(e)  
        idx_e = signe*ie
        ie+=1

      lf[f].append(idx_e)

  print('liste des arretes ',le)
  print('liste des faces ',lf)

  
  from meshing3D import *    

  gene = MeshGenerator(name,3,lc)

  for i in range(nbn):
     gene.addNode(coor[3*i:3*(i+1)])

  gene.computeBarycenter()
     
  for i in range(ie):
     gene.addEdge(le[i][0]-1,le[i][1]-1)

  for i in range(nbf):
     gene.addFace(lf[i])

  #gene.addVolume()
     
  gene.dump()   
    

### lets read some existing objects

import os,sys
from numpy import *
import pickle

nbr = 8

for i in range(1,nbr+1):
  print('-------------')
  name='bloc'+str(i)
  print(name)

  f = open('bloc'+str(i)+'.dict', 'r')
  bloc = pickle.load(f)
  f.close()  

  #fd on doit prendre qqch < thickness pour que la detection non convexe fonctionne
  remesh(bloc['coor'],bloc['connectivity'],name,0.08)



