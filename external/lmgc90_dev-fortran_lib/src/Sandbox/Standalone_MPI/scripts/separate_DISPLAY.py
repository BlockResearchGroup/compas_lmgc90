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

# cas 2D :

nb_display=0
# Calcul du nombre de a_ibdyx ecrits en parallele.
for root, dirs, files in os.walk("DDM_WD_00001/DISPLAY", topdown=False):
    for myfile in files:
        if not myfile.startswith("a_ibdy2_"):
           continue
        # decoupage 'a', 'ibdy2', 'xxxxxxx.vtu'
        T1=myfile.split("_")
        # decoupage 'xxxxxxx', 'vtu'
        T2=T1[2].split(".")
        try:
           n=int(T2[0])
        except Exception:
           continue
        nb_display=max(nb_display,n)

opening_tags= ("<?xml version=\"1.0\"?>\n",
               "<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"BigEndian\">\n",
               "  <UnstructuredGrid>\n")

closing_tags= ("</UnstructuredGrid>\n",
               "</VTKFile>\n")

# Concatenation des a_ibdyx par sous-domaines en un seul a_rbdyx dans le dossier DISPLAY.
for i in range(nb_display):
    for j in range(nb_SDM):
        my_file="DDM_WD_"+"%(#)05d" % {"#": j+1}+"/DISPLAY/a_ibdy2_"+"%(#)07d" % {"#": i+1}+".vtu"

        my_output=open("DISPLAY/a_ibdy2_"+"%(#)05d" % {"#": j+1}+"_"+"%(#)07d" % {"#": i+1}+".vtu","w")
        
        my_input=open(my_file, "r")
 
        if j != 0:
            for line in opening_tags:
               my_output.write(line)
        for line in my_input:
            my_output.write(line)
        if j != nb_SDM - 1:
            for line in closing_tags:
               my_output.write(line)
        my_input.close()

        my_output.close()

# cas 3D :

nb_display=0
# Calcul du nombre de a_ibdyx ecrits en parallele.
for root, dirs, files in os.walk("DDM_WD_00001/DISPLAY", topdown=False):
    for myfile in files:
        if not myfile.startswith("a_ibdy3_"):
           continue
        # decoupage 'a', 'ibdy3, 'xxxxxxx.vtu'
        T1=myfile.split("_")
        # decoupage 'xxxxxxx', 'vtu'
        T2=T1[2].split(".")
        try:
           n=int(T2[0])
        except Exception:
           continue
        nb_display=max(nb_display,n)

opening_tags= ("<?xml version=\"1.0\"?>\n",
               "<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"BigEndian\">\n",
               "  <UnstructuredGrid>\n")

closing_tags= ("</UnstructuredGrid>\n",
               "</VTKFile>\n")

# Concatenation des a_ibdyx par sous-domaines en un seul a_rbdyx dans le dossier DISPLAY.
for i in range(nb_display):
    for j in range(nb_SDM):
        my_file="DDM_WD_"+"%(#)05d" % {"#": j+1}+"/DISPLAY/a_ibdy3_"+"%(#)07d" % {"#": i+1}+".vtu"

        my_output=open("DISPLAY/a_ibdy3_"+"%(#)05d" % {"#": j+1}+"_"+"%(#)07d" % {"#": i+1}+".vtu","w")
        
        my_input=open(my_file, "r")
 
        if j != 0:
            for line in opening_tags:
               my_output.write(line)
        for line in my_input:
            my_output.write(line)
        if j != nb_SDM - 1:
            for line in closing_tags:
               my_output.write(line)
        my_input.close()

        my_output.close()

