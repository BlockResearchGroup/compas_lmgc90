# -*- coding: iso-8859-1 -*-

import os
import sys

def check():
	return

########################################
# Fonction d'installation de ChiPyGUI
def install(INSTALL_REP, force = False):
	print "#######################################################"
	print ""
	print "Installation de ChiPyGUI"
	chiPedMess = "Ok"
	os.chdir(os.pardir)
	SETUPROOT = os.getcwd()
	try:
		old = os.getcwd()
		os.chdir(INSTALL_REP + "ChiPyGUI")
	except:
		INSTALLING = True
	else:
		os.chdir(old)
		if(force == False):
			print "ChiPyGUI semble etre deja installe"
			print "Voulez-vous le reinstaller ? (y/n)"
			choix = raw_input()
		else:
			choix = "y"
	
		if(choix.lower() == "y" or choix.lower() == "yes\n"):
			INSTALLING = True
			os.system("rm \"" + INSTALL_REP + "ChiPyGUI\" -r")
		else:
			INSTALLING = False
	
	if(INSTALLING):
		print "cp -Rf ChiPyGUI \"" + INSTALL_REP + "\""
		exitCode = os.system("cp -Rf ChiPyGUI \"" + INSTALL_REP + "\"")
		if(exitCode != 0):
			chiPedMess = "Error"
			print "Installation : " + chiPedMess
			print ""
			return

		## Creation du fichier export
		CHIPYEDITORINSTALL = INSTALL_REP + "ChiPyGUI" + os.sep
		fic = open(CHIPYEDITORINSTALL + "ChiPyGUI_env.sh", 'w')
	
		fic.write("export CHIPYGUI_INSTALL=\"" + CHIPYEDITORINSTALL + "\"\n")
		fic.close()
	
		# Ecriture dans le fichier .bashrc
		os.chdir(SETUPROOT)
		os.system("cp ~/.bashrc monbash")
	
		found = False
		fic = open("monbash", "r")
		liste = fic.read()
		fic.close()
		liste = liste.split("\n")
		if(liste[-1] == ""):
			liste = liste[:-1]
		for i in range(len(liste)):
			if(liste[i].find("ChiPyGUI_env.sh") > 0):
				liste[i] = ". " + CHIPYEDITORINSTALL + "ChiPyGUI_env.sh"
				found = True
	
		if(found == False):
			liste.append(". " + CHIPYEDITORINSTALL + "ChiPyGUI_env.sh")
	
		fic = open("monbash", "w")
		for i in liste[:-1]:
			fic.write(i + "\n")
		fic.write(liste[-1])
		fic.close()
	
		os.system("cp monbash ~/.bashrc")
		os.system("rm monbash")
	else:
		chiPedMess = "Skipped"
	
	print "Installation : " + chiPedMess
	print ""
	return chiPedMess

if(__name__ == "__main__"):
	if(len(sys.argv)>1):
		ls_opt = sys.argv[1:]
	else:
		ls_opt = []

	INSTALL_REP = ""

	for opt in ls_opt:
		if(opt.startswith("-w=")):
			INSTALL_REP = opt.split("=")[1].strip()

	if(INSTALL_REP == ""):
		print "Chemin d'installation de ChiPyGUI :"
		INSTALL_REP = raw_input()
		if(INSTALL_REP.endswith("\n")):
			INSTALL_REP = INSTALL_REP[:-1]

	if("--force" in ls_opt):
		install(INSTALL_REP, force = True)
	else:
		install(INSTALL_REP)
