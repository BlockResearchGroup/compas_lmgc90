
//#include "exception.h"

#include "ann.h"

extern "C" void annSetNbKdTrees(int nb)
{
  if( all_kd != NULL )
  {
    annFinalize();
  }

  nb_kd  = nb;
  all_kd = new ANNkd_tree*[nb];

};
     
extern "C" void annAddKdTree(int i_tree, double ** nodes, int nb_nodes, int space_dim)
{

  if( i_tree < 0 || i_tree > nb_kd )
  {
     std::cout << "[ANN] wrong index of KD tree to add: "<< i_tree << std::endl ;
     return ;
  }

  all_kd[i_tree] = new ANNkd_tree(nodes, nb_nodes, space_dim);
  
};

extern "C" void annSearchNearestKd(int i_tree, double * test, int nb_near, int * nearests, double * dists)
{

  if( i_tree < 0 || i_tree > nb_kd )
  {
     std::cout << "[ANN] wrong index of KD tree to search nearest: " << i_tree << std::endl ;
     return ;
  }

  all_kd[i_tree]->annkSearch(test, nb_near, nearests, dists);
};

extern "C" void annRadiiSearchKd(int i_tree, double * tests, int nb_tests, double radius, int ** in, double ** dists, int * max_in)
{
  int nb_in;

  if( i_tree < 0 || i_tree > nb_kd )
  {
     std::cout << "[ANN] wrong index of KD tree to radius search: " << i_tree << std::endl ;
     return ;
  }

  int dim = all_kd[i_tree]->theDim();

  *max_in = 0;

  for( unsigned int i = 0; i<nb_tests; i++ )
  {
    nb_in = all_kd[i_tree]->annkFRSearch(&(tests[i*dim]), radius, 0, NULL, NULL, 0.);
    if( nb_in > *(max_in) )
      (*max_in) = nb_in;
  }

  // exit if no point within radius
  if( *max_in == 0 ) return;

  (*in)    = new int   [*max_in*nb_tests];
  (*dists) = new double[*max_in*nb_tests];

  for( unsigned int i=0; i< *max_in*nb_tests; i++ )
  {
    (*in)[i]    = -1 ;
    (*dists)[i] =  0.;
  }

  // return value is the same than precedent call, so it is discarded
  for( unsigned int i = 0; i<nb_tests; i++ )
  {
    all_kd[i_tree]->annkFRSearch(&(tests[i*dim]), radius, *max_in, &((*in)[*max_in*i]), &((*dists)[*max_in*i]), 0.);
  }

}

extern "C" void annSetNbBdTrees(int nb)
{
  if( all_bd != NULL )
  {
    annFinalize();
  }

  nb_bd  = nb;
  all_bd = new ANNbd_tree*[nb];

};
     
extern "C" void annAddBdTree(int i_tree, double ** nodes, int nb_nodes, int space_dim)
{

  if( i_tree < 0 || i_tree > nb_bd )
  {
     std::cout << "[ANN] wrong index of KD tree to add: "<< i_tree << std::endl ;
     return ;
  }

  all_bd[i_tree] = new ANNbd_tree(nodes, nb_nodes, space_dim);
  
};

extern "C" void annSearchNearestBd(int i_tree, double * test, int nb_near, int * nearests, double * dists)
{

  if( i_tree < 0 || i_tree > nb_bd )
  {
     std::cout << "[ANN] wrong index of KD tree to search nearest: " << i_tree << std::endl ;
     return ;
  }

  all_bd[i_tree]->annkSearch(test, nb_near, nearests, dists);
};

extern "C" void annRadiusSearchBd(int i_tree, double * test, double radius, int ** in, double ** dists, int * nb_in)
{

  if( i_tree < 0 || i_tree > nb_bd )
  {
     std::cout << "[ANN] wrong index of KD tree to radius search: " << i_tree << std::endl ;
     return ;
  }

  (*nb_in) = all_bd[i_tree]->annkFRSearch(test, radius, 0, NULL, NULL, 0.);

  // exit if no point within radius
  if( *nb_in == 0 ) return;

  (*in)    = new int   [*nb_in];
  (*dists) = new double[*nb_in];

  // return value is the same than precedent call, so it is discarded
  all_bd[i_tree]->annkFRSearch(test, radius, *nb_in, *in, *dists, 0.);

}

extern "C" void annFinalize(void)
{
   if( nb_kd > 0 )
   {
     for(int i=0; i<nb_kd; i++)
       delete all_kd[i];
     delete all_kd;
     nb_kd  = 0;
     all_kd = NULL;
   }
   
   if( nb_bd > 0 )
   {
     for(int i=0; i<nb_bd; i++)
       delete all_bd[i];
     delete all_bd;
     nb_bd  = 0;
     all_bd = NULL;
   }
   
   annClose();

};
