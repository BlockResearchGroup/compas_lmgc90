
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

list_opt = [ '2d', '3d', ]
for s in list_opt:
    runScript(sys.executable+' bodies.py --novisu '+s)

