
cdef extern from "ptree.h":

  cdef cppclass RT2:
    RT2() except +
    void RemoveAll()
    void Insert(double * bmin, double * bmax, int i)
    void Search(double * bmin, double * bmax, bint(int,void*),void*)

  cdef cppclass RT3:
    RT3() except +
    void RemoveAll()
    void Insert(double * bmin, double * bmax, int i)
    void Search(double * bmin, double * bmax, bint(int,void*),void*)

  bint fill_list(int id, void* ilist)

