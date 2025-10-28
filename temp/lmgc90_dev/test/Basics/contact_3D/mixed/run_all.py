
import sys
import subprocess

def runScript(cmd):
  p = subprocess.Popen( cmd, stdout=subprocess.PIPE, shell=True)
  out, err = p.communicate()
  p.wait()
  print(out)
  if p.returncode != 0 :
    sys.exit(p.returncode)

runScript(sys.executable+' detection_3D.py')
runScript(sys.executable+' post_mortem.py')

