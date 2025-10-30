#ifndef _lmgcplugin__
#define _lmgcplugin__

#include <c++headers.hh>
#include <solidinteractions.hh>
using namespace std;


/** @brief The Class LMGCPlugIn.

The LMGC Plug In.
*/

class LMGCPlugIn : public SolidInteractions 
{  
  private :

    /** Backup of the fluid density for the WriteParticlesInFluid method
    */
    double fluid_density;

    /** array containing the id connections between griff and lmgc and its size
    */
    int nb_particles ; 
    int * griff2lmgc ;

    /* several size of arrays of lmgc
    */
    int nb_diskx, nb_dispx, nb_polyg;

    /** @name Constructors & Destructor */
    //@{
    /** @brief Copy constructor */
    LMGCPlugIn(const LMGCPlugIn &PI);
    //@}    

  public :
    /** @name Constructors & Destructor */
    //@{
    /** @brief Constructor witht argument 
    @param insertion_file_ insertion file name 
    @param simulation_file_ simulation file name 
    @param fluid_density fluid density 
    @param error =0 if the construction is successful */
    LMGCPlugIn(const string &insertion_file_,const string
    	&simulation_file_,const double &fluid_density,int &error);
    
    /** @brief Destructor */
    ~LMGCPlugIn();
    //@}
    
    /** @brief Simulation 
    @param predictor if yes, predictor phase, otherwise corrector phase 
    @param contact_force_coef contact forces coefficient 
    @param explicit_added_mass whether to treat added mass (and torque) term
    	explicitly */
    void Simulation(const bool &predictor=true,
    	const double &contact_force_coef=1.,
	const bool &explicit_added_mass=false);
    
    /** @brief Write solids features to be sent as a stream to the fluid part 
    @param is the stream */
    void WriteParticlesInFluid( istringstream &is );

    /** @brief Write particles velocity and gravity center only in a stream for 
    the coupling with the fluid flow
    @param is input stream */
    void WritePVGCInFluid(istringstream &is);
    
    /** @brief Update solids velocities @param velocities array containing velocities for all particles */
    void UpdateParticlesVelocities( const vector<vector<double> > &velocities, const bool &b_update_velocity_nm1 );
    
    /** @brief Save results 
    @param filename file name */
    void SaveResults(const string &filename);            

    /** @brief Read the solid parameters file and check that the fluid time
    step and the solid time step match as well as total gravity
    @param fluid_timestep fluid time step 
    @param split_fluid_gravity fluid gravity acceleration on the fluid side */
    int verify_fluid_solid_time_step_and_gravity(
    	const double &fluid_timestep,const geomVector *split_fluid_gravity);

    /** @brief Read the solid parameters file and check that the coupling scheme
    is set similarly in the fluid and solid data files
    @param fluid_couplingscheme fluid coupling scheme */
    int verify_coupling_scheme(const string &fluid_couplingscheme);

    /** @brief Get the Grains case files 
    @param case_files list of files required to write the case */
    void get_solid_case_files(list<string> &case_files);

    /** @brief Initialize corrector step 
    @param velocities array containing velocities for all particles */
    void InitializeCorrectorStep( const vector<vector<double> > &velocities );

    /** @brief Activate explicit added mass */
    void ActivateExplicitAddedMass();
};


// Autres fonctions utilisées par la classe
void init_simulation(double dt, double theta);
void run_simulation(int freq_detec, int freq_gmv, int freq_vlocrloc, double eps, int gs_it1, int gs_it2);
void lmgc_update(void);

#endif 
