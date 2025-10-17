#include <stdio.h>

void set_simulation_parameters(double* dt, int* nb_dt, double* theta, int* f1, double* eps, int* gs1, int* gs2, int* f2, int* f3);
void init_simulation();
void run_simulation();

int main()
{
 int nb_dt = 500; // number of time step to simulate
 int f1 = 1;      // detection frequency ?
 int f2 = 10;     // gmv frequency
 int f3 = 1;      // vlocrloc frequency ?
 int gs2 = 1000;  // nb de pas dans la resolution du contact 
 int gs1 = 50;    // nb de pas de la deuxieme boucle dans la resolution du contact 

 double dt = 1e-2;   // time step
 double theta = 0.5; // theta
 double eps = 0.1666e-3;  // tolerance (what of ?)

 set_simulation_parameters(&dt, &nb_dt, &theta, &f1, &eps, &gs1, &gs2, &f2, &f3);
 init_simulation();
 run_simulation();
}
