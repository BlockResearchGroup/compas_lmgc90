
import shutil
from pathlib import Path

from pylmgc90 import pre

src_dir = Path('./OUTBOX')
des_dir = Path('./DATBOX')

dim = 3
mats, mods, bodies, tacts, sees, inters = pre.readDatbox(dim, src_dir, step=1)
pre.writeDatbox(dim, mats, mods, bodies, tacts, sees, datbox_dir=des_dir)

txt_files = [p for p in src_dir.iterdir() if p.suffix == '.txt']
for txt in txt_files:
  #txt.copy(des_dir) # python 3.14 only...
  shutil.copy2(txt, des_dir)



from pylmgc90 import chipy

# count number of output files
dof_out = [p for p in src_dir.iterdir() if p.stem == 'DOF.OUT']
nb_record = len(dof_out)

chipy.Initialize()
chipy.checkDirectories()

chipy.Initialize()

chipy.SetDimension(dim)

chipy.ReadDatbox(deformable=False)

chipy.OpenDisplayFiles()

chipy.ComputeMass()

for k in range(1,nb_record+1,1):

  chipy.ReadIni(k)
  #chipy.ComputeFext()
  chipy.ComputeRnod()
  chipy.WriteDisplayFiles(1)

chipy.CloseDisplayFiles()
chipy.Finalize()

