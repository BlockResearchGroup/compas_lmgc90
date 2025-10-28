
try:
  import gmshpy
except ImportError:
  print('ERROR : Python binding of GMSH not available !')
  print('ERROR : You cannot use gmshutils module until gmshpy is installed')
  raise

import numpy as np

class Track(object):
   """
      Class to generate an octree object from a list of node coordinates
      and connectivity and track a point inside id.
   """

   def __init__(self, vertices, connec):
     """
     A numpy array with node coordinates and the connectivities
     in a...
     """
     assert( isinstance(vertices,np.ndarray) )

     #attributes of the class
     self._space_dim = vertices.shape[1]

     self._elements = []
     self._nodes    = {}
     self._octree   = None

     if self._space_dim == 2:
       self._nodes = { i+1 : gmshpy.MVertex(x, y, 0, None, i) for i, (x, y) in enumerate(vertices) }
     else:
       self._nodes = { i+1 : gmshpy.MVertex(x, y, z, None, i) for i, (x, y, z) in enumerate(vertices) }


     idx = 1
     el_num = 0
     for i in range(connec[0]) :
       el_num += 1
       if self._space_dim == 2 :
         if connec[idx] == 1:
           self._elements.append( gmshpy.MPoint(self._nodes[connec[idx+1]],el_num, 0) )
         elif connec[idx] == 2:
           self._elements.append( gmshpy.MLine(self._nodes[connec[idx+1]],self._nodes[connec[idx+2]],el_num, 0) )
           #self._elements = gmshpy.MLine3(connec[idx+0],connec[idx+1],
           #                               connec[idx+2],el_num))
         elif connec[idx] == 3:
           self._elements.append( gmshpy.MTriangle(self._nodes[connec[idx+1]],self._nodes[connec[idx+2]],
                                                   self._nodes[connec[idx+3]],el_num) 
                                )
         elif connec[idx] == 6:
           self._elements.append( gmshpy.MTriangle6(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                    self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                    self._nodes[connec[idx+5]], self._nodes[connec[idx+6]],
                                                    el_num, 0)
                                )
         elif connec[idx] == 4:
           self._elements.append( gmshpy.MQuadrangle(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                     self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                     el_num, 0)
                                )
         elif connec[idx] == 8:
           self._elements.append( gmshpy.MQuadrangle8(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                      self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                      self._nodes[connec[idx+5]], self._nodes[connec[idx+6]],
                                                      self._nodes[connec[idx+7]], self._nodes[connec[idx+8]],
                                                      el_num, 0)
                                )
         else:
           print('unknown element type !!!')
       elif self._space_dim == 3 :
         if connec[idx] == 4:
           self._elements.append( gmshpy.MTetrahedron(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                      self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                      el_num, 0)
                                )
         elif connec[idx] == 10:
           self._elements.append( gmshpy.MTetrahedron10(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                        self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                        self._nodes[connec[idx+5]], self._nodes[connec[idx+6]],
                                                        self._nodes[connec[idx+7]], self._nodes[connec[idx+8]],
                                                        self._nodes[connec[idx+9]], self._nodes[connec[idx+10]],
                                                        el_num, 0)
                                )
         elif connec[idx] == 6:
           self._elements.append( gmshpy.MPrism(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                self._nodes[connec[idx+5]], self._nodes[connec[idx+6]],
                                                el_num, 0)
                                )
         elif connec[idx] == 15:
           self._elements.append( gmshpy.MPrism15(self._nodes[connec[idx+ 1]], self._nodes[connec[idx+ 2]],
                                                  self._nodes[connec[idx+ 3]], self._nodes[connec[idx+ 4]],
                                                  self._nodes[connec[idx+ 5]], self._nodes[connec[idx+ 6]],
                                                  self._nodes[connec[idx+ 7]], self._nodes[connec[idx+ 8]],
                                                  self._nodes[connec[idx+ 9]], self._nodes[connec[idx+10]],
                                                  self._nodes[connec[idx+11]], self._nodes[connec[idx+12]],
                                                  self._nodes[connec[idx+13]], self._nodes[connec[idx+14]],
                                                  connec[idx+15], el_num, 0)
                                )
         elif connec[idx] == 8:
           self._elements.append( gmshpy.MHexahedron(self._nodes[connec[idx+1]], self._nodes[connec[idx+2]],
                                                     self._nodes[connec[idx+3]], self._nodes[connec[idx+4]],
                                                     self._nodes[connec[idx+5]], self._nodes[connec[idx+6]],
                                                     self._nodes[connec[idx+7]], self._nodes[connec[idx+8]],
                                                     el_num, 0)
                                )
         elif connec[idx] == 20:
           self._elements.append( gmshpy.MHexahedron20(self._nodes[connec[idx+ 1]], self._nodes[connec[idx+ 2]],
                                                       self._nodes[connec[idx+ 3]], self._nodes[connec[idx+ 4]],
                                                       self._nodes[connec[idx+ 5]], self._nodes[connec[idx+ 6]],
                                                       self._nodes[connec[idx+ 7]], self._nodes[connec[idx+ 8]],
                                                       self._nodes[connec[idx+ 9]], self._nodes[connec[idx+10]],
                                                       self._nodes[connec[idx+11]], self._nodes[connec[idx+12]],
                                                       self._nodes[connec[idx+13]], self._nodes[connec[idx+14]],
                                                       self._nodes[connec[idx+15]], self._nodes[connec[idx+16]],
                                                       self._nodes[connec[idx+17]], self._nodes[connec[idx+18]],
                                                       self._nodes[connec[idx+19]], self._nodes[connec[idx+20]],
                                                       el_num, 0)
                                )
         else:
           print('unknown element type !!!')
       else:
         print('unknown space_dim!!!')

       idx += connec[idx]+1
     
     self._octree = gmshpy.MElementOctree(self._elements)


   def updateCoordinates(self,vertices):
     """
     Update vertices coordinates
     """

     assert( isinstance(vertices,np.ndarray) )
     assert( self._space_dim  == vertices.shape[1] )
     assert( len(self._nodes) == vertices.shape[0] )

     if self._space_dim == 2:
       for i, (x, y) in enumerate(vertices):
         self._nodes[i+1].setXYZ( x, y, 0. )
     else:
       for i, (x, y, z) in enumerate(vertices):
         self._nodes[i+1].setXYZ( x, y, z )

     self._octree = gmshpy.MElementOctree(self._elements)


   def getLocalCoordinates(self, points) :
      """
      Input : points coordinates
      Return : element including the points and reference coordinates
      """

      assert(isinstance(points,np.ndarray))

      nums = np.empty(points.shape[0],dtype=int)
      coors= np.empty(points.shape)

      for i, p in enumerate(points):
        if self._space_dim == 2:
          element = self._octree.find(p[0],p[1], 0.)
          if element:
            nums[i] = element.getNum()
            coors[i,:] = element.xyz2uvw(p[0], p[1], 0.)[:self._space_dim]
          else:
            nums[i]    = -99
            coors[i,:] = 0.
        else:
          element = self._octree.find(p[0],p[1],p[2])
          if element:
            nums[i] = element.getNum()
            coors[i,:] = element.xyz2uvw(p[0], p[1], p[2])
          else:
            nums[i] = -99
            coors[i,:] = 0.

      return nums, coors

################
##if __name__ == "__main__" :
##
##  vertices = [(10, (0., 0.)), (15, (1., 0.)), (20, (1., 1.)), (12, (0., 1.))]
##  triangles = [(100, (10, 15, 20)), (200, (20, 12, 10))]
##
##
##  X, Y, Z = 2./3, 1./3, 0.
##
##  t = track(vertices,triangles)
##  u, v, w = t.getLocalCoordinates(X, Y, Z)
##  print(u, v, w)

