#!/usr/bin/python
          ###################################################################
          #                                                                 #
          #        POSTRAITEMENT DES SORTIES PARALLELES DE LA NSCDD         #
          #                                                                 #
          #    Script permettant de concatener les fichiers .OUT et .LAST   #
          #    sortis par sous-domaines (dossier OUTBOX) en fichiers .OUT   #
          #  et .LAST pour l'ensemble de l'echantillon (dossier OUTBOX_CAT) #
          #                                                                 #
          ###################################################################


import os
import sys

nb_DOF=0
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

# Calcul du nombre de pas de temps pour lesquels des sorties
# .OUT sont effectuees en parallele.
for root, dirs, files in os.walk("DDM_WD_00001/OUTBOX", topdown=False):
    for myfile in files:
        if not myfile.startswith("DOF.OUT."):
           continue
        T=myfile.split(".")
        try:
           n=int(T[2])
        except Exception:
           continue
        nb_DOF=max(nb_DOF,n)

# Concatenation des DOF.OUT par sous-domaines en un seul DOF.OUT
# (pour chaque pas de sortie) dans le dossier OUTBOX_CAT.
for i in range(nb_DOF):
    my_inputs=[]
    for j in range(nb_SDM):
        my_inputs.append("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/OUTBOX/DOF.OUT."+str(i+1))

    my_output=open("OUTBOX/DOF.OUT."+str(i+1), "w")
    for my_file in my_inputs:
        my_input=open(my_file, "r")

        for line in my_input:
            my_output.write(line)
        my_input.close()

    my_output.close()

# Concatenation des DOF.LAST par sous-domaines en un seul DOF.LAST
# dans le dossier OUTBOX.
my_inputs=[]
for j in range(nb_SDM):
    my_inputs.append("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/OUTBOX/DOF.LAST")

my_output=open("OUTBOX/DOF.LAST","w")
for my_file in my_inputs:
    my_input=open(my_file, "r")

    for line in my_input:
        my_output.write(line)
    my_input.close()

my_output.close()

# Concatenation des Vloc_Rloc.OUT par sous-domaines en un seul Vloc_Rloc.OUT
# (pour chaque pas de sortie) dans le dossier OUTBOX.
for i in range(nb_DOF):
    my_inputs=[]
    for j in range(nb_SDM):
        my_inputs.append("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/OUTBOX/Vloc_Rloc.OUT."+str(i+1))

    my_output=open("OUTBOX/Vloc_Rloc.OUT."+str(i+1),"w")
    for my_file in my_inputs:
        my_input=open(my_file, "r")

        for line in my_input:
            my_output.write(line)
        my_input.close()

    my_output.close()

# Concatenation des Vloc_Rloc.LAST par sous-domaines en un seul Vloc_Rloc.LAST
# dans le dossier OUTBOX.
my_inputs=[]
for j in range(nb_SDM):
    my_inputs.append("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/OUTBOX/Vloc_Rloc.LAST")

my_output=open("OUTBOX/Vloc_Rloc.LAST","w")
for my_file in my_inputs:
    my_input=open(my_file, "r")

    for line in my_input:
        my_output.write(line)
    my_input.close()

my_output.close()

