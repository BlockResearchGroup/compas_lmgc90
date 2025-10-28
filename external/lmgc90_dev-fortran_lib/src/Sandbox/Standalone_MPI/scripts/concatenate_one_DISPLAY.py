#!/usr/bin/python

          ###################################################################
          #                                                                 #
          #        POSTRAITEMENT DES SORTIES PARALLELES DE LA NSCDD         #
          #                                                                 #
          #    Script permettant de concatener les fichiers a_ibdy3         #
          #    sortis par sous-domaines                                     # 
          #                                                                 #
          ###################################################################



import os
import sys

if (len(sys.argv) != 3):
    print "Ce script doit etre lance commme suit :"
    print "concatenate_one_DISPLAY.py X X"
    print "Avec X X la dimension (2 ou 3) et le numero de display souhaite, respectivement"
    sys.exit()	

d = int(sys.argv[1])
i = int(sys.argv[2])

nb_SDM=0

# Calcul du nombre de sous-domaines pour lesquels des sorties
# .OUT sont effectuees en parallele.
for root, dirs, files in os.walk(".", topdown=False):
    for myfile in dirs:
        if not myfile.startswith("DDM_WD_"):
           continue
        T=myfile.split("_")
        try:
           n=int(T[2])
        except Exception:
           continue
        nb_SDM=max(nb_SDM,n)


if (d == 2):
   
   # cas 2D :
   
   # Concatenation des a_ibdyx par sous-domaines en un seul a_rbdyx dans le dossier DISPLAY.
   my_inputs=[]
   for j in range(nb_SDM):
       my_inputs.append("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/DISPLAY/a_ibdy2_"+"%(#)07d" % {"#": i}+".vtu")
   
   my_output=open("DISPLAY/a_ibdy2_"+"%(#)07d" % {"#": i}+".vtu","w")
   for my_file in my_inputs:
       my_input=open(my_file, "r")
   
       for line in my_input:
           my_output.write(line)
       my_input.close()
   
   my_output.close()

if (d==3):

   # cas 3D :
   
   # Concatenation des a_ibdyx par sous-domaines en un seul a_rbdyx dans le dossier DISPLAY.
   my_inputs=[]
   for j in range(nb_SDM):
       my_inputs.append("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/DISPLAY/a_ibdy3_"+"%(#)07d" % {"#": i}+".vtu")
   
   my_output=open("DISPLAY/a_ibdy3_"+"%(#)07d" % {"#": i}+".vtu","w")
   for my_file in my_inputs:
       my_input=open(my_file, "r")
   
       for line in my_input:
           my_output.write(line)
       my_input.close()
   
   my_output.close()
   
