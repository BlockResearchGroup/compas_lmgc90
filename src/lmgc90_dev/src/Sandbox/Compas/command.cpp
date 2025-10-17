
#include "LMGC90/shared/wrap_bulk_behav.h"
#include "LMGC90/shared/wrap_overall.h"
#include "LMGC90/shared/wrap_parameters.h"
#include "LMGC90/shared/wrap_tact_behav.h"
#include "LMGC90/shared/wrap_utilities.h"

#include "LMGC90/rigid_3D/wrap_RBDY3.h"
#include "LMGC90/rigid_3D/wrap_POLYR.h"

#include "LMGC90/contact_3D/wrap_inter_handler_3D.h"
#include "LMGC90/contact_3D/wrap_PRPRx.h"

#include "LMGC90/kernel/wrap_nlgs_3D.h"

#include "LMGC90/post/wrap_postpro_3D.h"

int main(void) {

  const int i_prprx = 18;//parameters_getInteractionId("PRPRx");

  // todo...
  //chipy.checkDirectories

  // set some data
  int dim  = 3;
  int mhyp = 0;

  int nb_steps = 100;
  double dt    = 1e-3;

  double theta = 0.5;

  double tol = 1.666e-4;
  double relax = 0.1;

  char norm[] = "QM/16";

  int gs_it1 = 50;
  int gs_it2 = 500;

  char solver_type[] = "Stored_Delassus_Loops         ";

  int record  = 0;
  int restart = 0;

  int freq_detec = 1;
  int reset = 0;

  int freq_write = 1;

  overall_Initialize();
  parameters_checkAll();

  nlgs_3D_DiagonalResolution();
  PRPRx_UseCpCundallDetection(200);
  PRPRx_LowSizeArrayPolyr(10);
  
  
  //chipy.SetDimension
  overall_DIME(dim,mhyp);
  
  TimeEvolution_SetTimeStep(dt);
  Integrator_InitTheta(theta);
  
  //chipy.ReadDatbox(deformable=False);
  bulk_behav_ReadBehaviours();
  tact_behav_ReadBehaviours();
  
  RBDY3_ReadBodies();
  RBDY3_LoadBehaviours();
  
  TimeEvolution_ReadIniDof();
  RBDY3_ReadIniDof();
  RBDY3_ReadDrivenDof();
  POLYR_LoadTactors();
  overall_InitEntityList();
  
  TimeEvolution_ReadIniVlocRloc(record);
  PRPRx_ReadIniVlocRloc(record);
  
  // no write
  //io_hdf5_initOutFile( 'lmgc90.h5' );
  // no display
  
  postpro_3D_PostproBeforeComputation( restart );
  
  // chipy.ComputeMass();
  RBDY3_ComputeMass();
  
  for( unsigned int k=0; k<nb_steps; k++ ) {
  
    overall_CleanWriteOutFlags();
    TimeEvolution_IncrementStep();
    RBDY3_IncrementStep();
    RBDY3_ComputeFext();
    RBDY3_ComputeBulk();
    RBDY3_ComputeFreeVelocity();
  
    overall_SelectProxTactors(freq_detec);
    PRPRx_SelectProxTactors(reset);
  
    inter_handler_3D_recupRloc(i_prprx);
    nlgs_3D_ExSolver(solver_type,norm,tol,relax,gs_it1,gs_it2);
    nlgs_3D_UpdateTactBehav();
  
    inter_handler_3D_stockRloc(i_prprx);
  
    RBDY3_ComputeDof();
    TimeEvolution_UpdateStep();
    RBDY3_UpdateDof();
    // no hdf5...
    //if TimeEvolution_GetStep()%freq_write == 0
    //  io_hdf5_write( );
    TimeEvolution_WriteOutDof(freq_write);
    RBDY3_WriteOutDof();
    TimeEvolution_WriteOutVlocRloc(freq_write);
    PRPRx_WriteOutVlocRloc();

    // python only...
    //chipy.WriteDisplayFiles(freq_display);
    postpro_3D_PostproDuringComputation();
  
  }
  
  postpro_3D_ClosePostproFiles();
  
  bulk_behav_CleanMemory();
  tact_behav_CleanMemory();
  // inter_handler ?
  PRPRx_CleanMemory();
  POLYR_CleanMemory();
  RBDY3_CleanMemory();
  postpro_3D_CleanMemory();
  nlgs_3D_IsInitialized(0);
  // no hdf5...
  //io_hdf5_cleanMemory();
  overall_Finalize();
  utilities_Finalize();

}
