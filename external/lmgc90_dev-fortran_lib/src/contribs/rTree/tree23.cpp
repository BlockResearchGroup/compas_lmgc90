
#include "tree23.h"


int ilist_index;

RT2 tree2;
RT3 tree3;

bool fill_list(int id, void* list)
{
  ilist_index++;
  int* ilist = (int*)list;
  ilist[ilist_index] = id;
  return true; // keep going
}

void reset_tree2(void)
{
  tree2.RemoveAll();
}

void reset_tree3(void)
{
  tree3.RemoveAll();
}

void add_to_tree2(double * bmin, double * bmax, int tacid)
{
  tree2.Insert(bmin, bmax, tacid);
}
void add_to_tree3(double * bmin, double * bmax, int tacid)
{
  tree3.Insert(bmin, bmax, tacid);
}

void search_in_tree2(double * bmin, double * bmax, int* ilist)
{
  ilist_index = 0;
  ilist[0] = tree2.Search(bmin, bmax, fill_list, ilist);
}

void search_in_tree3(double * bmin, double * bmax, int* ilist)
{
  ilist_index = 0;
  ilist[0] = tree3.Search(bmin, bmax, fill_list, ilist);
}

