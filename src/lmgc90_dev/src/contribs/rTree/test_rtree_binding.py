#!${PYTHON_EXECUTABLE}
import sys
sys.path.append("${CMAKE_BINARY_DIR}")

from pylmgc90 import pyRtree
import numpy as np

def test2d():
  t = pyRtree.rTree(2)
  
  bb = np.zeros([2,2],dtype=np.double)
  bb[1,:] = 1.
  t.insert(bb[0,:], bb[1,:], 0)
  
  bb[:,0] += 3.
  t.insert(bb[0,:], bb[1,:], 1)
  
  bb[:,1] += 3.
  t.insert(bb[0,:], bb[1,:], 3)
  
  bb[:,0] -= 3.
  t.insert(bb[0,:], bb[1,:], 2)
  
  lb = np.array([[0.5,0.5],[3.5,3.5]])
  sb = np.array([[1.5,1.5],[2.5,2.5]])

  if t.search(lb[0,:],lb[1,:]) != [0,1,3,2] :
    print('error in first test 2d')
    sys.exit(1)
  if len(t.search(sb[0,:],sb[1,:])) > 0 :
    print('error in second test 2d')
    sys.exit(1)

  del t

def test3d():

  t = pyRtree.rTree(3)
  bb = np.zeros([2,3],dtype=np.double)
  bb[1,:] = 1.
  t.insert(bb[0,:], bb[1,:], 0)
  
  bb[:,0] += 3.
  t.insert(bb[0,:], bb[1,:], 1)
  
  bb[:,1] += 3.
  t.insert(bb[0,:], bb[1,:], 3)
  
  bb[:,0] -= 3.
  t.insert(bb[0,:], bb[1,:], 2)
  
  lb = np.array([[0.5,0.5,0.],[3.5,3.5,1.]])
  sb = np.array([[1.5,1.5,0.],[2.5,2.5,1.]])
  
  if t.search(lb[0,:],lb[1,:]) != [0,1,3,2] :
    print('error in first test 3d')
    sys.exit(1)
  if len(t.search(sb[0,:],sb[1,:])) > 0 :
    print('error in second test 3d')
    sys.exit(1)


if __name__ == "__main__" :

  test2d()

  test3d()

