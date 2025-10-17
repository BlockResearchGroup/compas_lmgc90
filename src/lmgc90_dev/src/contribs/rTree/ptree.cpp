
#include "ptree.h"

bool fill_list(int id, void* list)
{
  int* ilist = (int*)list;
  ilist[0]++;
  ilist[ilist[0]] = id;
  return true; // keep going
}

