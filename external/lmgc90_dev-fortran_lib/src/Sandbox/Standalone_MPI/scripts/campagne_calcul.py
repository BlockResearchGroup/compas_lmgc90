#!/usr/bin/python

import os, sys
import subprocess
import time


if (len(sys.argv) != 4):
    print "Ce script doit etre lance commme suit :"
    print "campagne_calcul.py X X X"
    print "Avec X X X : nb_decompositions, shemas_comm (1=CENTRA, 2=DECENT) et SDL_ELG (1=SDL, 2=ELG)"
    sys.exit()

#amd18
home_directory = '/home0/visseq0/LMGC90/build_LMGC90_mpif90_opt/bin/'

# on recupere le nombre de tirage a effectuer
nb_decompositions = int(sys.argv[1])
shemas_comm       = int(sys.argv[2])
SDL_ELG           = int(sys.argv[3])

if (nb_decompositions > 5):
    print "Le script est actuellement prevu pour 5 decompositions au max :"
    print "Faire les modifications necessaires avant de le re-executer !!"
    sys.exit()

decoupe=[]
nb_shemas = 0

##########################################################
#                 Decoupes considerees
##########################################################
decoupe.append("1_1_1")
decoupe.append("1_1_2")
decoupe.append("1_1_3")
decoupe.append("2_1_2")
decoupe.append("2_2_2")
#"2_1_3","2_2_2","2_2_3","3_2_3","3_3_3","3_3_4")

##########################################################
# Determination du schema de communication inter-processus
##########################################################
if (shemas_comm == 1):
   scheme = 'C'
elif (shemas_comm == 2):
   scheme = 'D'
else:
   print "shemas_comm doit etre compris entre 1 et 2 !!"
   print "           1 : centralise -> 'C',"
   print "           2 : decentralise -> 'D'."
   sys.exit()

############################################################
# Determination du "type" de solver (dans tous les cas nlgs)
############################################################
if (SDL_ELG == 1):
   type_solv = 'SDL'
elif (SDL_ELG == 2):
   type_solv = 'ELG'
else:
   print "SDL_ELG doit etre compris entre 1 et 2 !!"
   print "           1 : SDL,"
   print "           2 : ELG."
   sys.exit()

nb_sdm = 0

############################################################
# Debut de la boucle de calculs sur les decoupes considerees
############################################################
for k in range(nb_decompositions):

    print ' =============================== '
    print '  Decomposition ',decoupe[k]
    print ''

    T = decoupe[k].split("_")
    nb_sdm = int(T[0]) * int(T[1]) * int(T[2])

    print '  Copie du DATBOX et du standelone.in'
    subprocess.check_call(['cp -r ../'+decoupe[k]+'/DATBOX .'],shell=True)
    subprocess.check_call(['cp ../'+decoupe[k]+'/standalone.in .'],shell=True)

    print '  Lancement du calcul'
    subprocess.check_call(['mkdir-lmgc'],shell=True)
    subprocess.check_call(['create_working_directory.py '+str(nb_sdm)],shell=True)
    subprocess.check_call(['nohup mpirun.openmpi -n '+str(nb_sdm)+' '+home_directory+scheme+'_'+type_solv],shell=True)

    print '  Sauvegarde des donnees'
    subprocess.check_call(['concatenate_timer.py'])
    subprocess.check_call(['mv OUTBOX/TIMER.OUT TIMER_'+decoupe[k]+'_'+scheme+'_'+type_solv+'.OUT'],shell=True)
    subprocess.check_call(['mv nohup.out nohup_'+decoupe[k]+'_'+scheme+'_'+type_solv+'.out'],shell=True)
    subprocess.check_call(['mv POSTPRO POSTPRO_'+decoupe[k]+'_'+scheme+'_'+type_solv],shell=True)


    print '  Nettoyage du repertoire de calcul'
    subprocess.check_call(['rm -rf DATBOX'],shell=True)
    subprocess.check_call(['rm standalone.in'],shell=True)
    subprocess.check_call(['rm -rf DISPLAY'],shell=True)
    subprocess.check_call(['rm -rf OUTBOX'],shell=True)
    subprocess.check_call(['rm -rf DDM_WD_*'],shell=True)

print '=========='
print 'Fin script'
print '=========='
