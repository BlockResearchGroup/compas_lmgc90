#include <vector>

#include "lmgcplugin.hh"

#include "rigid_2D/wrap_RBDY2.h"
#include "rigid_2D/wrap_DISKx.h"
#include "rigid_2D/wrap_POLYG.h"
#include "rigid_2D/wrap_JONCx.h"

#include "contact_2D/wrap_DKDKx.h"
#include "contact_2D/wrap_DKPLx.h"
#include "contact_2D/wrap_DKJCx.h"
#include "contact_2D/wrap_PLPLx.h"
#include "contact_2D/wrap_PLJCx.h"

#include "kernel/wrap_nlgs.h"

#include "shared/wrap_overall.h"
#include "shared/wrap_afterall.h"
#include "shared/wrap_bulk_behav.h"
#include "shared/wrap_tact_behav.h"

#include "post/wrap_postpro.h"
#include "post/wrap_post2D.h"

/* Constructor with argument 
----------------------------*/
LMGCPlugIn::LMGCPlugIn(const string &insertion_file_,const string
    	&simulation_file_,const double &f_density,int &error):
	SolidInteractions(insertion_file_,simulation_file_,f_density,error)
{
  PEL_LABEL( "LMGCPlugIn::LMGCPlugIn" ) ;

  int particles_count;

  int nb_dt;
  double dt, theta; 
  double solid_density;
  double * new_mass;

  bool set_corrector;

  fluid_density = f_density; // storing the fluid density

  // --- lmgc90 simulation parameters definition --- 
  theta = 0.5 ;		//
  nb_dt = 1; 		// always in our case
  dt    = 2e-4 ;	// therefore must be equal to fluid time step

  // set parameters of the simulation
  //set_simulation_parameters(&dt, &nb_dt, &theta, &freq1, &tol, &gs1, &gs2, &freq2, &freq3, &set_corrector);

  // read the DATBOX and initialize the simulation,
  init_simulation(dt, theta);

  // generation of an array holding the id connection between lmgc90 and peligriff
  int nb_rbdy2 = RBDY2_GetNbRBDY2();
  nb_particles = nb_rbdy2 - JONCx_GetNbJONCx();
  particles_count = 0;

  griff2lmgc = new int[nb_particles];
  
  //  -- first the DISKx
  nb_diskx = DISKx_GetNbDISKx();
  int * diskx_in_rbdy2 = new int [nb_diskx];
  DISKx_GetDISKx2RBDY2(&diskx_in_rbdy2[0], nb_diskx);
  for(int i=0; i<nb_diskx; i++)
  {
    griff2lmgc[particles_count] = diskx_in_rbdy2[i];
    particles_count++;
  }

//  -- and the POLYG
  nb_polyg = POLYG_GetNbPOLYG();
  int * polyg_in_rbdy2 = new int (nb_polyg);
  POLYG_GetPOLYG2RBDY2(&polyg_in_rbdy2[0], nb_polyg);
  for(int i=0; i<nb_polyg; i++)
  {
    griff2lmgc[particles_count] = polyg_in_rbdy2[i];
    particles_count++;
  }

  //  --  others ?

  // multiply every particles mass and inertia by 1.0-fluid_density/solid_density
  for(int i=0; i<nb_particles; i++)
  {
    RBDY2_GetDensity(griff2lmgc[i], &solid_density) ;
    RBDY2_GetMass_Ptr(griff2lmgc[i], &new_mass) ;
    new_mass[0] *= ( 1.0 - fluid_density/solid_density ) ;
  }

  delete diskx_in_rbdy2;
  delete polyg_in_rbdy2;
}    
 
    
/* Destructor 
-------------*/
LMGCPlugIn::~LMGCPlugIn()
{  
  PEL_LABEL( "LMGCPlugIn::~LMGCPlugIn" ) ;

  postpro_ClosePostproFiles();
  //postpro_3D_ClosePostproFiles();

  delete griff2lmgc;

}


    
/* Simulation 
-------------*/
void LMGCPlugIn::Simulation(const bool &predictor,
	const double &contact_force_coef,const bool &explicit_added_mass)
{
  PEL_LABEL( "LMGCPlugIn::Simulation" ) ;

  int freq_detec    = 1    ;	// detection frequency
  int freq_vlocrloc = 40   ;	// VlocRloc save frequency
  int freq_gmv      = 40   ;	// gmv frequency
  int gs_it1        = 50   ;	// gauss seidel first loop number of iterations
  int gs_it2        = 1000 ;	// gauss seidel second loop number of iterations

  double tol        = 0.1666e-3; 	//
  //run one time step of LMGC90 simulation without updating positions and velocities
  run_simulation(freq_detec, freq_gmv, freq_vlocrloc, tol, gs_it1, gs_it2);

}



    
/* Write solids features to be sent as a stream to the fluid part 
-----------------------------------------------------------------*/
void LMGCPlugIn::WriteParticlesInFluid( istringstream &is )
{
  PEL_LABEL( "LMGCPlugIn::WriteParticlesInFluid(istringstream)" ) ;

  double rho, I3, radius ;
  double vel[3], pos[2];
  double * mass;
  ostringstream particles_features;

  particles_features.precision(10);
  particles_features << nb_particles << endl;

  // -------- DISKx -------- //
  for(int i=1; i<=nb_diskx; i++)
  {
    int lmgc_index = griff2lmgc[i-1];
    RBDY2_GetVelocity(lmgc_index, vel);
    RBDY2_GetDensity(lmgc_index, &rho);
    RBDY2_GetMass_Ptr(lmgc_index, &mass);
    I3 = mass[2];
    RBDY2_GetCoor(lmgc_index, pos);
    radius = DISKx_GetRadius(i);

    particles_features << i-1 << " " << 1 << endl;				//particle id and number of nodes
    particles_features << "P\t"; 						//particle type
    particles_features << vel[0] << "  " << vel[1] << "\t" << vel[2] << "\t";	//v(1) v(2) and omega, translational and rotational velocity
    // mass and ineria are divided by the coeff 1 - fluid_density/solid_density
    // to get back the real mass/inertia of particle
    // since the mass/inertia stored in lmgc90 is now modified
    particles_features << rho << "\t" << mass[0]/(1.0-fluid_density/rho) << "\t";
    particles_features << I3/(1.0-fluid_density/rho)<< "\t";			//density, mass and inertia
    particles_features << pos[0] << " " << pos[1] << endl;			//gravity center position
    particles_features << radius << " " << 1 << endl;				//radius of the disc, et le 1 je ne sais toujours pas
    particles_features << pos[0] << " " << pos[1] << endl;			//gravity center position again
  }

  // -------- POLYG -------- //
  for(int i=1; i<=nb_polyg; i++)
  {
    int lmgc_index = griff2lmgc[nb_diskx + i-1];
    RBDY2_GetVelocity(lmgc_index, vel);
    RBDY2_GetDensity(lmgc_index, &rho);
    RBDY2_GetMass_Ptr(lmgc_index, &mass);
    I3 = mass[2];
    RBDY2_GetCoor(lmgc_index, pos);

    int nb_nodes = POLYG_GetNbPointOutline(i);
    double * vertexes_pos = new double[2*nb_nodes];
    POLYG_GetOutline(i, &vertexes_pos[0], nb_nodes);
    radius = POLYG_GetRadius(i);

    particles_features << nb_diskx+ i-1 << " " << nb_nodes << endl;	//particle id and number of nodes
    particles_features << "P\t"; 						//
    particles_features << vel[0] << "  " << vel[1] << "\t" << vel[2] << "\t";	//v(1) v(2) and omega, translational and rotational velocity
    particles_features << rho << "\t" << mass[0]/(1.0-fluid_density/rho) << "\t";
    particles_features << I3/(1.0-fluid_density/rho) << "\t";			//density, mass and inertia
    particles_features << pos[0] << " " << pos[1] << endl;			//gravity center position
    particles_features << radius << " " << nb_nodes << endl;			//radius of the particle and number of nodes
    for(int j=1; j<nb_nodes; j++) // on commence a 1 car le premier element rendu par lmgc est le centre
       particles_features << vertexes_pos[2*j] << " " << vertexes_pos[2*j+1] << endl;
    delete vertexes_pos;
  }

  // -------- Other -------- //

  is.clear();
  is.str(particles_features.rdbuf()->str());

}


    
/* Update solids velocities 
----------------------------*/
void LMGCPlugIn::UpdateParticlesVelocities( 
	const vector<vector<double> > &velocities,
        const bool &b_update_velocity_nm1 = true )
{
  PEL_LABEL( "LMGCPlugIn::UpdateParticlesVelocities(array)" ) ;

  double vel[3];

  // update of the LMGC velocity by the result of the coupling step
  for(int i=0; i<nb_particles; i++)
  {
    vel[0] = velocities[i][0];
    vel[1] = velocities[i][1];
    vel[2] = velocities[i][5];
    RBDY2_SetVelocity(griff2lmgc[i],vel);
  }

  // update of position and velocity in LMGC90
  lmgc_update();

}



    
/* Save results 
---------------*/
void LMGCPlugIn::SaveResults(const string &filename)
{
  PEL_LABEL( "LMGCPlugIn::SaveResults" ) ;

  istringstream is;
  std::string fn = "Res/particles_features";
  fn += filename + ".dat" ;
  const char * file( fn.c_str() );
  ofstream os(file,ios::out);
  this->WriteParticlesInFluid(is);  
  os << is;
  os.close();
}  




/* Read the solid parameters file and check that the fluid time
   step and the solid time step match as well as total gravity
---------------------------------------------------------------*/
int LMGCPlugIn::verify_fluid_solid_time_step_and_gravity(
    	const double &fluid_timestep,const geomVector *split_fluid_gravity)
{
  PEL_LABEL( "LMGCPlugIn::verify_fluid_solid_time_step_and_gravity" ) ;
  double solid_timestep,total_gravity_magnitude;
  geomVector solid_gravity(split_fluid_gravity->getVecSize()); 
  int n_solid_timesteps,error=0;; 
  double grav[3];
    
  solid_timestep = overall_GetTimeStep();
  n_solid_timesteps = 1; //overall_GetNbTimeStep();
  bulk_behav_GetGravity(grav);

  solid_gravity(0) = grav[0];
  solid_gravity(1) = grav[1];

  // Time step verification
  if (abs(fluid_timestep-n_solid_timesteps*solid_timestep)>1e-10)
  {
     cout << "\nFluid time step = " << fluid_timestep << endl <<
    	"Solid time step = " << n_solid_timesteps*solid_timestep << endl;     
     string error_message="Warning : fluid and solid time steps do not match\n";
     error_message+="          Check the input files and adjust the time steps";
     cout << error_message << endl;
     error=1;
  }   
  
  // Gravity magnitude : 0 or 9.81
  // In solid file, gravity is prescribed in the opposite way
  total_gravity_magnitude=((*split_fluid_gravity)-solid_gravity).calcNorm();
  geomVector oneNull(split_fluid_gravity->getVecSize());
  if ((total_gravity_magnitude>1e-8)&&(abs(total_gravity_magnitude-9.81)>1e-8))
  {
    cout << "\nFluid gravity = " << *split_fluid_gravity << endl <<
    	"Solid gravity = " << oneNull-solid_gravity << endl;
     string error_message="Warning : total gravity is neither 0 nor 9.81\n";
     error_message+="          Check the input files and adjust the gravity";
     cout << error_message << endl;
     error=1;
  }  
  
  if (error == 0)
     cout << "\nThe gravity and timestep in fluid and GRAINS are matching\n" 
     	<< endl;

  return error;

}



/* Get the Grains case files 
----------------------------*/
void LMGCPlugIn::get_solid_case_files(list<string> &case_files)
{
  PEL_LABEL( "LMGCPlugIn:: get_solid_case_files" ) ;

  cout << "!!! Warning:  get_solid_case_files is not implemented yet !!!" 
  	<< endl;

}



/* Initialize corrector step 
----------------------------*/
void LMGCPlugIn::InitializeCorrectorStep(
	const vector<vector<double> > &velocities)
{
  PEL_LABEL( "LMGCPlugIn::InitializeCorrectorStep" ) ;
}




/* Write particles velocity and gravity center only in a stream for 
   the coupling with the fluid flow
-------------------------------------------------------------------*/
void LMGCPlugIn::WritePVGCInFluid(istringstream &is)
{
  PEL_LABEL( "LMGCPlugIn::WritePVGCInFluid" ) ;
  
  double vel[3], pos[2];

  ostringstream particles_velpos;

  particles_velpos.precision(10);
  particles_velpos<< nb_particles << endl;

  for(int i=0; i<nb_particles; i++)
  {
    //recuperation des valeurs a mettre dans le stream
    RBDY2_GetVelocity(griff2lmgc[i], vel);
    RBDY2_GetCoor(griff2lmgc[i], pos);

    //remplissage du stream
    particles_velpos << vel[0] << " " << vel[1] << " " << vel[2] << " ";
    particles_velpos << pos[0] << " " << pos[1] << endl;

  }

  is.clear();
  is.str(particles_velpos.rdbuf()->str());

}



/* Read the solid parameters file and check that the coupling scheme
    is set similarly in the fluid and solid data files  
--------------------------------------------------------------------*/
int LMGCPlugIn::verify_coupling_scheme(const string &fluid_couplingscheme)
{
  PEL_LABEL( "LMGCPlugIn::verify_coupling_scheme" ) ;

  return (0);
  
}




/* Activate explicit added mass 
-------------------------------*/
void LMGCPlugIn::ActivateExplicitAddedMass()
{
  PEL_LABEL( "LMGCPlugIn::ActivateExplicitAddedMass" ) ;

}



// fonctions qui remplacent le mod_lmgcPlugin
// en reprenant les fonctions disponibles dans ChiPy_BindC

void init_simulation(double dt, double theta)
{
  //overall_DIME(3,0);

  overall_SetTimeStep(dt);
  overall_InitThetaIntegrator(theta);

  RBDY2_ReadBodies();
  //RBDY3_ReadBodies();

  bulk_behav_ReadBehaviours();
  tact_behav_ReadBehaviours();

  DISKx_LoadTactors();
  JONCx_LoadTactors();
  //SPHER_LoadTactors();
  //PLANx_LoadTactors();
  
  RBDY2_LoadBehaviours();
  //RBDY3_LoadBehaviours();

  overall_ReadIniDof();
  RBDY2_ReadIniDof();
  //RBDY3_ReadIniDof();

  overall_ReadIniVlocRloc();
  DKJCx_ReadIniVlocRloc();
  DKDKx_ReadIniVlocRloc();
  //SPSPx_ReadIniVlocRloc();
  //SPPLx_ReadIniVlocRloc();

  RBDY2_ReadDrivenDof();
  //RBDY3_ReadDrivenDof();

  postpro_PostproBeforeComputation();

  DKJCx_Init();
  DKDKx_Init();
  //SPSPx_Init();
  //SPPLx_Init();
  afterall_ComputeBox();

  RBDY2_ComputeMass();
  //RBDY3_ComputeMass();

}


void run_simulation(int freq_detec, int freq_gmv, int freq_vlocrloc, double eps, int gs_it1, int gs_it2)
{
  double relax = 1.0;
  overall_IncrementStep();
  RBDY2_IncrementStep();
  //RBDY3_IncrementStep();
  //POLYR_IncrementStep();

  RBDY2_ComputeFext();
  //RBDY3_ComputeFext();

  RBDY2_ComputeFint();
  //RBDY3_ComputeFext();

  RBDY2_ComputeFreeVelocity();
  //RBDY3_ComputeFreeVelocity();

  overall_SelectProxTactors(freq_detec);
  DKJCx_SelectProxTactors();
  DKDKx_SelectProxTactors();
  //SPSPx_SelectProxTactors();
  //SPPLx_SelectProxTactors();
  /* si tu utilise du contact type PRPRx,
   * vaut mieux changer un peu la commande :
  PRPRx_WcpSelectProxTactors();
  */


  DKJCx_RecupRloc();
  DKDKx_RecupRloc();

  nlgs_ExSolver("Stored_Delassus_Loops         ", "Quad ", eps, relax, gs_it1, gs_it2);
  
  DKJCx_StockRloc();
  DKDKx_StockRloc();

  //SPSPx_RecupRloc();
  //SPPLx_RecupRloc();
  //
  //nlgs_3D_ExSolver();
  //
  //SPSPx_StockRloc();
  //SPPLx_StockRloc();

  RBDY2_ComputeDof();
  //RBDY3_ComputeDof();
}


void lmgc_update()
{
  RBDY2_CompCoor();
  //RBDY3_CompCoor();

  overall_UpdateDof();
  RBDY2_UpdateDof();
  //RBDY3_UpdateDof();

  overall_CleanWriteOutFlags();

}
