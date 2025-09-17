
import sys
import subprocess

def runScript(cmd, out_file=None):
  p = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
  out, err = p.communicate()
  p.wait()
  if out_file is None:
    print(out)
  else:
    with open(out_file,'a') as f:
        f.write(out.decode())
  if p.returncode != 0 :
    sys.exit(p.returncode)

list_script = [ 'tact_behav', 'bulk_behav', 'models', 'order', ]
if sys.version_info.major > 2:
    list_script.extend( ['bodies_2d', 'bodies_3d', ] )

if '--with-hdf5' in sys.argv and sys.version_info.major > 2:
    ofile = 'log.txt'
    f = open(ofile,'w')
    f.close()
    runScript(sys.executable+' bodies_2d.py --with-hdf5 --novisu', ofile)
    runScript(sys.executable+' bodies_3d.py --with-hdf5 --novisu', ofile)
    runScript(sys.executable+' last.py --with-hdf5 --novisu', ofile)


for s in list_script:
    runScript(sys.executable+' '+s+'.py --novisu')

