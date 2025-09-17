
#ifndef ptree_H
#define ptree_H

#include "RTree.h"

typedef RTree<int,double,2> RT2;
typedef RTree<int,double,3> RT3;

bool fill_list(int id, void* ilist);

#endif // ptree_H
