
cimport numpy as np
import numpy as np

import cython as ct

cimport ptree

np.import_array()

cdef class rTree:

  cdef ptree.RT2 * rtree2_ptr
  cdef ptree.RT3 * rtree3_ptr

  cdef int nb_boxes
  cdef int sdim

  @ct.locals(sdim=int)
  def __cinit__(self,sdim):

    self.nb_boxes = 0

    assert(sdim==2 or sdim==3)
    self.sdim = sdim

    if sdim==2 :
      self.rtree2_ptr = new ptree.RT2()
      self.rtree3_ptr = NULL
    else :
      self.rtree2_ptr = NULL
      self.rtree3_ptr = new ptree.RT3()

  def __dealloc__(self):
    del self.rtree2_ptr
    del self.rtree3_ptr

  @ct.locals(box_llr=np.ndarray,box_urf=np.ndarray,id=int)
  def insert(self, box_llr, box_urf, id):
    self.nb_boxes += 1
    if self.sdim==2:
      self.rtree2_ptr.Insert(<double*>box_llr.data,<double*>box_urf.data,id)
    else:
      self.rtree3_ptr.Insert(<double*>box_llr.data,<double*>box_urf.data,id)

  def reset(self):
    self.nb_boxes = 0
    if self.sdim==2:
      self.rtree2_ptr.RemoveAll()
    else:
      self.rtree3_ptr.RemoveAll()

  @ct.locals(box_llr=np.ndarray,box_urf=np.ndarray)
  def search(self, box_llr, box_urf):
    list_id = np.zeros([self.nb_boxes+1], dtype=np.int32)
    cdef int [:] list_view = list_id
    if self.sdim == 2:
      self.rtree2_ptr.Search(<double*>box_llr.data,<double*>box_urf.data, ptree.fill_list, <void*>&list_view[0])
    else:
      self.rtree3_ptr.Search(<double*>box_llr.data,<double*>box_urf.data, ptree.fill_list, <void*>&list_view[0])
  
    l = list()
    for i in xrange(list_id[0]):
      l.append(list_id[i+1])
  
    return l

