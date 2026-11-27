
def run(theta=0.5,dt=1e-3):

   import shutil
   from pathlib import Path

   from pylmgc90 import chipy

   dim = 3
   src_dir = Path('./OUTBOX')

   # count number of output files
   dof_out = [p for p in src_dir.iterdir() if p.stem == 'DOF.OUT']
   nb_record = len(dof_out)

   chipy.Initialize()

   display = Path('./DISPLAY')
   display.mkdir(exist_ok=True)

   chipy.SetDimension(dim)

   chipy.TimeEvolution_SetTimeStep(dt)
   chipy.Integrator_InitTheta(theta)

   chipy.ReadOutbox(deformable=False, record=1)

   chipy.PRPRx_UseStoDetection(True,-1.,1e-3)
   chipy.PRPRx_ForceF2fDetection()
   chipy.PRPRx_LowSizeArrayPolyr(100)   
   chipy.Integrator_SetContactDetectionConfiguration(1.-theta,0.)   
   chipy.RBDY3_ComputeContactDetectionConfiguration()
   chipy.SelectProxTactors()
   
   chipy.OpenDisplayFiles(write_f2f=3)

   chipy.ComputeMass()

   for k in range(2,nb_record+1,1):

     chipy.ReadIni(k)
     chipy.ComputeFext()
     chipy.ComputeRnod()
     # chipy.SelectProxTactors()     
     # chipy.RecupRloc()
     chipy.WriteDisplayFiles(1)

   chipy.CloseDisplayFiles()
   chipy.Finalize()


if __name__ == "__main__":

    run()
