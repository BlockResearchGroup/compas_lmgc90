#!/usr/bin/python

import sys
import os
import subprocess
import shutil

try:
   nb_sdm=int(sys.argv[1])
except:
   print "Usage : " + sys.argv[0] + " create_working_directory <int>"
   sys.exit(-1)

subprocess.check_call(['rm -rf DDM_WD_*'],shell=True)

for i in range(nb_sdm):
   working_directory="DDM_WD_" + "%(#)05d" % {"#": i+1}
   os.mkdir(working_directory)
   shutil.copytree("DATBOX", working_directory + "/DATBOX")
   os.mkdir(working_directory + "/OUTBOX")
   os.mkdir(working_directory + "/DISPLAY")
   os.mkdir(working_directory + "/POSTPRO")
