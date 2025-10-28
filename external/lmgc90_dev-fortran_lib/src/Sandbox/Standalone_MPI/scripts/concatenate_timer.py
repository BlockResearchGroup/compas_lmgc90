#!/usr/bin/python
import os
import sys
import numpy
import subprocess

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

# Concatenation des TIMER.OUT par sous-domaines en un seul TIMER.OUT
# dans le dossier OUTBOX.

# declaration des listes utilisees pour stocker
#    * le nom de chaque compteur
names = []
#    * le temps mesure par chaque compteur
total_times = []

chaine_timer = []

# on indique que le prochain fichier a etre ouvert est le premier
first_file = True
# pour chaque fichier
for j in range(nb_SDM):

    chaine_timer = "cp DDM_WD_"+"%(#)05d" % {"#": j+1}+"/OUTBOX/TIMER.OUT OUTBOX/TIMER_SDM_"+"%(#)05d" % {"#": j+1}

    subprocess.check_call(chaine_timer,shell=True)

    # on ajoute une liste de temps pour le processus courant
    total_times.append([])
    # on ouvre le fichier en lecture
    my_input=open("DDM_WD_"+"%(#)05d" % {"#": j+1}+"/OUTBOX/TIMER.OUT", "r")
    # on zappe les deux premeires lignes du fichier
    my_input.readline()
    my_input.readline()
    # on lit la ligne suivante
    l = my_input.readline()
    # on la separe en colonnes
    t = l.split()
    # tant que la ligne courante contient plusieurs colonnes
    while len(t) > 1:
       # si le fichier est le premier ouvert
       if j == 0:
          # on ajoute le nom du compteur courant a la liste des noms
          names.append(l[3:23])
       # on ajoute le temps mesure par le timer courant, a liste des temps du processus courant 
       time = l[25:39]
       time = time.replace('D', 'E')
       total_times[j].append(float(time)) 
       # on lit la ligne suivante
       l = my_input.readline()
       # on la separe en colonnes
       t = l.split()
    # ici, on est arrive a la ligne separant les compteurs du bloc recaptitualtif

    # on zappe la ligne suivante
    my_input.readline()
    # on lit la ligne suivante, donnant la duree totale de la simulation
    l = my_input.readline()
    # si le fichier est le premier ouvert
    if j == 0:
       # on insere le nom du timer mesurant temps total au debut de la liste des noms
       names.insert(0, "global")
    
    # on insere le temps total au debut de la liste des temps du processus courant 
    time = l[25:39]
    time = time.replace('D', 'E')
    total_times[j].insert(0, float(time))

# on convertit le tableau des temps en tableau NumPy
total_times = numpy.array(total_times)

# on recupere le temps max pour chaque timer
max_total_times = numpy.amax(total_times, axis=0)

# on recupere le numero de processus qui realise le max, pour chaque timer
argmax_total_times = numpy.argmax(total_times, axis=0)

# on calcule la somme des temps pour chaque timer
sum_total_times = numpy.sum(total_times, axis=0)

# on ecrit le fichier de resultat

my_output=open("OUTBOX/TIMER.OUT","w")

my_output.write("MAXIMA :\n")
my_output.write("=======\n")
my_output.write(25*" " + "Elapsed time :" + 3*" " + "Process       :\n")
my_output.write("\n")

for j in range(1, len(names)):
   my_output.write(" - " + "%20s" % names[j] + "  " + "%14.7le" % max_total_times[j] + " s" + 8*" " + "%6d" % (argmax_total_times[j] + 1)  + " \n") 

my_output.write(24*" " + 17*"-" + "\n")
my_output.write("  Total elapsed time     %14.7le" % max_total_times[0] + 10*" " + "%6d" % (argmax_total_times[0] + 1)  + " \n")

my_output.write("\nSUMS   :\n")
my_output.write("=======\n")
my_output.write(25*" " + "Elapsed time :" + 3*" " + "Elapsed ratio :\n")

i_global = 100./sum_total_times[0]
for j in range(1, len(names)):
   my_output.write(" - " + "%20s" % names[j] + "  " + "%14.7le" % sum_total_times[j] + " s" + 8*" " + "%6.2lf" % (sum_total_times[j]*i_global) + " %\n") 

my_output.write(24*" " + 17*"-" + "\n")
my_output.write("  Accounted time         %14.7le" % sum(sum_total_times[1:]) + " s" + 8*" " + "%6.2lf" % (sum(sum_total_times[1:])*i_global)  + " %\n")
my_output.write("  Total elapsed time     %14.7le" % sum_total_times[0] + "\n")

my_output.close()

