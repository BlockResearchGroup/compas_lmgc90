# -*- coding: iso-8859-1 -*-

import sys
import os

filePath = __file__
if(__name__ != "__main__"):
	leNom = __name__
	leRep = ""

	if(__name__.find(".") > 0):
		leNom = __name__.split(".")[-1].strip()
		leRep = __name__.split(".")[0].strip()

	EDITORPATH = filePath.split(__name__ + ".py")[0]
	if(leRep != ""):
		EDITORPATH = EDITORPATH.split(leRep)[0].strip()
else:
	sys.exit(0)

if(EDITORPATH.endswith(os.sep) == False):
	EDITORPATH = EDITORPATH + os.sep

try:
	fic = open(EDITORPATH + "Data" + os.sep + "options.dat", "r")
except:
	language = "fr"
else:
	line = fic.readline()
	while(line != ""):
		if(line.startswith("LANGUAGE")):
			language = line.split(":")[1].strip()
		line = fic.readline()
	fic.close()

listeLangue = {"Francais":"fr", "English":"en"}
reverseListeLangue = {"fr":"Francais", "en":"English"}

def getLanguages():
	return listeLangue

def getCurrentLanguage():
	return reverseListeLangue[language]

##
WRAP_OVERALL_MODULE = "wrap_overall"
WRAP_AFTERALL_MODULE = "wrap_afterall"
##

if(language == "fr"):
	# MODELE
	MOD_FIRST_LINE = "#--ChiPy-Editor--#"

	# TITRES
	LG_TITLE_MAIN = "ChiPyGUI"
	
	## MENUS
	LG_MENU_FILE = "Fichier"
	LG_MENU_NEWFILE = "Nouveau"
	LG_MENU_OPENFILE = "Ouvrir"
	LG_MENU_SAVE = "Enregistrer"
	LG_MENU_SAVEAS = "Enregistrer sous"
	LG_MENU_CLOSE = "Fermer"
	LG_MENU_QUIT = "Quitter"
	LG_MENU_MACRO = "Macro"
	LG_MENU_LOAD = "Charger"
	LG_MENU_LOADREP = "Charger un repertoire"
	LG_MENU_OPTION = "Options"
	LG_MENU_PREFERENCE = "Préférences"
	LG_MENU_VIERGE = "Fichier vierge"
	LG_MENU_TEMPLATE = "A partir d'un modele"
	LG_MENU_EDITION = "Edition"
	LG_MENU_COPY = "Copier"
	LG_MENU_PASTE = "Coller"
	LG_MENU_AIDE = "Aide"
	LG_MENU_ABOUT = "A propos"

	## FILETYPE
	FILETYPE_MACRO = "Fichier macro"
	FILETYPE_PYTHON = "Fichier python"
	FILETYPE_TEMPLATE = "Fichier modele"

	## LISTBOX ENTRIES
	LISTBOX_COMMAND_COM = "Commentaire"
	LISTBOX_COMMAND_IMPORT = "Import"
	LISTBOX_COMMAND_FROM = "From ... Import ..."
	LISTBOX_COMMAND_PYTHON = "Ligne Python"
	LISTBOX_COMMAND_PRINT = "Fonction python \"print\""
	LISTBOX_COMMAND_WRAP = "Fonction ChiPy"
	LISTBOX_COMMAND_FOR = "Boucle FOR"
	LISTBOX_COMMAND_ENDTAB = "Fin de boucle"
	LISTBOX_COMMAND_ENDLINE = "Retour a la ligne"
	LISTBOX_COMMAND_NONE = "----------------"

	## BOUTONS
	BUT_ADD_COMMAND = "Ajouter commande"
	BUT_DEL_COMMAND = "Supprimer commande"
	BUT_OK = "OK"
	BUT_CANCEL = "Annuler"
	BUT_RIGHT_VALIDATE = "Valider"
	
	## LABELS
	LAB_LEFTPANEL_MODULE = "Modules :"
	LAB_LEFTPANEL_FONCTION = "Fonctions :"
	LAB_LEFTPANEL_MACRO = "Macros :"
	LAB_MIDDLEPANEL_EDITOR = "Fichier : %s"
	LAB_TOP_COMMENTAIRE = "Commentaire :"
	LAB_TOP_PRINT = "Fonction python \"print\":"
	LAB_TOP_BOUCLEFOR = "Boucle FOR :"
	LAB_TOP_MODULE = "Module :"
	LAB_TOP_IMPORT = "Import :"
	LAB_TOP_FROM = "From :"
	LAB_TOP_FONCTION = "Fonction :"
	LAB_TOP_VARIABLE = "Variable :"
	LAB_TOP_STEP = "Step :"
	LAB_TOP_MINRANGE = "Debut :"
	LAB_TOP_MAXRANGE = "Fin :"
	LAB_TOP_MODULE = "Module :"
	LAB_TOP_FONCTION = "Fonction :"
	LAB_TOP_ARGUMENTS = "Arguments :"
	LAB_TOP_OPTIONAL = "Optionnel"
	LAB_TOP_REQUIRED = "Requis"
	LAB_TOP_PYTHON = "Ligne Python :"
	LAB_RIGHT_PARAM = "Paramètres de la commande"
	LAB_RIGHT_FROMTITLE = "From ... Import ..."
	LAB_RIGHT_IMPORTTITLE = "Import ..."
	LAB_RIGHT_ENDLINE = "Ligne vierge"
	LAB_RIGHT_ENDTAB = "Fin de boucle"
	LAB_RIGHT_REQUIRED = "Paramètres requis"
	LAB_RIGHT_OPTIONAL = "Paramètres optionnels"
	LAB_RIGHT_VALUE = "Valeur :"
	LAB_RIGHT_ARGUMENT = "Paramètre :"
	LAB_RIGHT_DOC = "Voir la documentation"
	LAB_RIGHT_NOREQARG = "Aucun paramètre requis"
	LAB_RIGHT_NOOPTARG = "Aucun paramètre optionnel"
	LAB_RIGHT_MODULE = "Module :"
	LAB_RIGHT_FONCTION = "Fonction :"
	LAB_RIGHT_PRINT = "Fonction python 'print'"
	LAB_RIGHT_IMPORTMODULE = "Module :"
	LAB_RIGHT_FROMMODULE = "Fonctions :"
	LAB_RIGHT_PYTHON = "Ligne Python :"
	LAB_OPTION_BACKGROUND = "Couleur de fond"
	LAB_OPTION_FONT = "Couleur font"
	LAB_OPTION_OPTION = "Option"
	
	## CHECKBUTTON
	
	CHK_LAB_INCREMENT = "Incremente les prochaines lignes"
	
	## DIALOG
	DLG_BUT_SAVE = "Enregistrer"
	DLG_BUT_DONTSAVE = "Ne pas enregistrer"
	DLG_BUT_CANCEL = "Annuler"
	
	DLG_TITLE_WARNING = "Attention"
	DLG_TITLE_FILETOOPEN = "Ouvrir"
	DLG_TITLE_TEMPLATETOOPEN = "Choix d'un modèle"
	DLG_TITLE_ERROR = "Impossible de lire le fichier"
	DLG_TITLE_SAVEERROR = "Impossible d'enregistrer le fichier"
	DLG_TITLE_FILESAVEAS = "Enregistrer sous..."
	DLG_TITLE_REPTOOPEN = "Ouvrir un repertoire..."
	
	DLG_LAB_ERROROPENING = "Impossible d'ouvrir le fichier %s"
	DLG_LAB_FILEOPENED = "Voulez-vous enregistrer les modifications\ndans le fichier %s avant de le fermer ?"
	DLG_LAB_LINEERROR = "Une erreur a été detectée dans les fonctions suivantes :\n\n%s\nQue voulez-vous faire ?"
	DLG_ABOUT_TEXT = "ChiPyGUI v1.0\n\nChiPyGui est un editeur de fichier de\ncommande pour LMGC90."
	DLG_LAB_MUSTBEABSOLUTE = "Le chemin du fichier doit etre absolu (\"/../../../x.py\")"
	
	## ERREUR
	ERR_MODULE_INEXISTANT = "Erreur, module %s inexistant..."
	ERR_FONCTION_INEXISTANT = "Erreur, fonction %s inexistante..."
	ERR_FICHIER_NON_TROUVE = "Erreur, impossible d'ouvrir le fichier %s"
	ERR_FORMAT_INCONNU = "Erreur, format du fichier %s inconnu"
	ERR_SAVE_LINE = "Ligne %d : %s"
	ERR_PMW = "Impossible d'importer PMW. Veuillez l'installer"
	ERR_LIB = "Impossible d'importer %s"
	
	## TEXTBOX STRING
	TXT_ENDFILE = "FIN DU FICHIER"
	TXT_ENDTAB = "FIN BOUCLE"

	## OPTIONS
	OPT_CMD_VAL = "Commande valide"
	OPT_CMD_PARREQ = "Paramètre requis manquant"
	OPT_CMD_PAROPT = "Paramètre optionnel manquant"
	OPT_CMD_INF = "Information"
	OPT_CMD_NOEXIST = "Fonction inexistante"
	OPT_CMD_WARNING = "Warning"
	OPT_CMD_WARN = "Modules importants"
	OPT_CMD_IGNORE = "Ligne python"
	OPT_LANGUAGE = "Langage :"
	#OPT_PATH = "Chemin d'installation ChiPyGUI :"
	OPT_MODE_FUNCMOD = "Fonction puis module"
	OPT_MODE_MODFUNC = "Module puis fonction"
	OPT_MODE = "Selection des commandes :"
	OPT_EXPLORATEUR = "Navigateur web :"
	OPT_DOC = "Chemin de la documentation :"

	## COMBO
	COMBO_NO_COLOR = "Aucune"
	COMBO_NONE = "-- Aucun --"
	COMBO_REQUIRED = "-- Argument Requis --"
	COMBO_OPTIONAL = "-- Argument Optionel --"
	COMBO_SELECT = "-- Selectionnez un argument --"

	## NOTEBOOK
	NTB_PAGE_EDITION = "Edition"
	NTB_PAGE_COLOR = "Coloration"

	## WARNING
	WARN_PLEASE_USE = "Veuillez utiliser %s"
	WARN_PARAM_ERROR = "ATTENTION !! LIGNE : %s.%s(%s)"
	WARN_PLEASE_SET = "VEUILLEZ VERIFIER LA VARIABLE D'ENVIRONNEMENT : CHIPYGUI_INSTALL"
	WARN_NO_CATALOG = "ATTENTION !! Impossible de charger le catalogue de fonctions ChiPy\nUtilisez l'option --generate-catalog pour generer le catalogue\na partir de la librairie ChiPy"
	WARN_CONTINUE = "Voulez-vous utiliser ChiPyGUI sans la librairie ChiPy (o/n) ?"
	WARN_YES = ["oui", "o"]
	
	## STATUS MESSAGE
	STATUS_OTHER_MODULES = "Autres Modules :"
	STATUS_OTHER_FUNCTIONS = "Autres fonctions :"

	## INFO
	INFO_LOADING_CATALOG = "Chargement des fonctions ChiPy à partir du catalogue"
	INFO_GENERATE_CATALOG = "Generation du catalogue des fonctions ChiPy"

	## AIDE
	HELP_OPEN_FILE = "Ouvre le fichier"
	HELP_GENERATE_CATALOG = "Genere le catalogue des fonctions ChiPy"

if(language == "en"):
	# MODELE
	MOD_FIRST_LINE = "#--ChiPy-Editor--#"

	# TITRES
	LG_TITLE_MAIN = "ChiPyGUI"
	
	## MENUS
	LG_MENU_FILE = "File"
	LG_MENU_NEWFILE = "New"
	LG_MENU_OPENFILE = "Open"
	LG_MENU_SAVE = "Save"
	LG_MENU_SAVEAS = "Save as"
	LG_MENU_CLOSE = "Close"
	LG_MENU_QUIT = "Quit"
	LG_MENU_MACRO = "Macro"
	LG_MENU_LOAD = "Load"
	LG_MENU_LOADREP = "Load from directory"
	LG_MENU_OPTION = "Options"
	LG_MENU_PREFERENCE = "Preferences"
	LG_MENU_VIERGE = "Empty file"
	LG_MENU_TEMPLATE = "From template"
	LG_MENU_EDITION = "Edition"
	LG_MENU_COPY = "Copy"
	LG_MENU_PASTE = "Paste"
	LG_MENU_AIDE = "Help"
	LG_MENU_ABOUT = "About"

	## FILETYPE
	FILETYPE_MACRO = "Macro file"
	FILETYPE_PYTHON = "Python file"
	FILETYPE_TEMPLATE = "Template file"

	## LISTBOX ENTRIES
	LISTBOX_COMMAND_COM = "Commentaire"
	LISTBOX_COMMAND_IMPORT = "Import"
	LISTBOX_COMMAND_FROM = "From ... Import ..."
	LISTBOX_COMMAND_PYTHON = "Python Line"
	LISTBOX_COMMAND_PRINT = "Python Function \"print\""
	LISTBOX_COMMAND_WRAP = "ChiPy Function"
	LISTBOX_COMMAND_FOR = "FOR Loop"
	LISTBOX_COMMAND_ENDTAB = "End of loop"
	LISTBOX_COMMAND_ENDLINE = "New line"
	LISTBOX_COMMAND_NONE = "----------------"

	## BOUTONS
	BUT_ADD_COMMAND = "Add command"
	BUT_DEL_COMMAND = "Delete command"
	BUT_OK = "OK"
	BUT_CANCEL = "Cancel"
	BUT_RIGHT_VALIDATE = "Validate"
	
	## LABELS
	LAB_LEFTPANEL_MODULE = "Modules :"
	LAB_LEFTPANEL_FONCTION = "Functions :"
	LAB_LEFTPANEL_MACRO = "Macros :"
	LAB_MIDDLEPANEL_EDITOR = "File : %s"
	LAB_TOP_COMMENTAIRE = "Commentaire :"
	LAB_TOP_PRINT = "Python Function \"print\":"
	LAB_TOP_BOUCLEFOR = "Boucle FOR :"
	LAB_TOP_MODULE = "Module :"
	LAB_TOP_IMPORT = "Import :"
	LAB_TOP_FROM = "From :"
	LAB_TOP_FONCTION = "Function :"
	LAB_TOP_VARIABLE = "Variable :"
	LAB_TOP_STEP = "Step :"
	LAB_TOP_MINRANGE = "From :"
	LAB_TOP_MAXRANGE = "To :"
	LAB_TOP_MODULE = "Module :"
	LAB_TOP_FONCTION = "Function :"
	LAB_TOP_ARGUMENTS = "Arguments :"
	LAB_TOP_OPTIONAL = "Optional"
	LAB_TOP_REQUIRED = "Required"
	LAB_TOP_PYTHON = "Python Line :"
	LAB_RIGHT_PARAM = "Command Arguments :"
	LAB_RIGHT_ENDLINE = "New line"
	LAB_RIGHT_ENDTAB = "End of loop"
	LAB_RIGHT_REQUIRED = "Required arguments"
	LAB_RIGHT_OPTIONAL = "Optional arguments"
	LAB_RIGHT_ARGUMENT = "Argument :"
	LAB_RIGHT_DOC = "Documentation"
	LAB_RIGHT_VALUE = "Value :"
	LAB_RIGHT_NOREQARG = "No required arguments"
	LAB_RIGHT_NOOPTARG = "No optional arguments"
	LAB_RIGHT_MODULE = "Module :"
	LAB_RIGHT_FONCTION = "Function :"
	LAB_RIGHT_PRINT = "Python function 'print'"
	LAB_RIGHT_IMPORTMODULE = "Module :"
	LAB_RIGHT_FROMMODULE = "Fynctions :"
	LAB_RIGHT_PYTHON = "Python Line :"
	LAB_OPTION_BACKGROUND = "Background color"
	LAB_OPTION_FONT = "Font color"
	LAB_OPTION_OPTION = "Option"
	
	## CHECKBUTTON
	
	CHK_LAB_INCREMENT = "Increment next line"
	
	## DIALOG
	DLG_BUT_SAVE = "Save"
	DLG_BUT_DONTSAVE = "Don't save"
	DLG_BUT_CANCEL = "Cancel"
	
	DLG_TITLE_WARNING = "Warning"
	DLG_TITLE_FILETOOPEN = "Open"
	DLG_TITLE_TEMPLATETOOPEN = "From template"
	DLG_TITLE_ERROR = "Can't read file"
	DLG_TITLE_SAVEERROR = "Can't save file"
	DLG_TITLE_FILESAVEAS = "Save as..."
	DLG_TITLE_REPTOOPEN = "Open directory..."
	
	DLG_LAB_ERROROPENING = "Can't open file %s"
	DLG_LAB_FILEOPENED = "Save modifications in file %s before closing it ?"
	DLG_LAB_LINEERROR = "Errors have been detected in lines :\n\n%s\nContinue ?"

	DLG_ABOUT_TEXT = "ChiPyGUI v1.0\n\nChiPyGui is a file command editor for LMGC90."
	DLG_LAB_MUSTBEABSOLUTE = "The file path must be absolute (\"/../../../x.py\")"
	
	## ERREUR
	ERR_MODULE_INEXISTANT = "Error, %s module not found..."
	ERR_FONCTION_INEXISTANT = "Error, %s function not found..."
	ERR_FICHIER_NON_TROUVE = "Error, can't open file %s"
	ERR_FORMAT_INCONNU = "Error, file format unknown (%s)"
	ERR_SAVE_LINE = "Line %d : %s"
	ERR_PMW = "Can't import PMW. Please install it"
	ERR_LIB = "Can't import %s"
	
	## TEXTBOX STRING
	TXT_ENDFILE = "END OF FILE"
	TXT_ENDTAB = "END LOOP"

	## OPTIONS
	OPT_CMD_VAL = "Valid command"
	OPT_CMD_PARREQ = "Required argument missing"
	OPT_CMD_PAROPT = "Optional argument missing"
	OPT_CMD_INF = "Information"
	OPT_CMD_NOEXIST = "Function not found"
	OPT_CMD_WARNING = "Warning"
	OPT_CMD_WARN = "Importants modules"
	OPT_CMD_IGNORE = "Python line"
	OPT_LANGUAGE = "Language :"
	#OPT_PATH = "Installation path for ChiPyGUI :"
	OPT_MODE_FUNCMOD = "Function then module"
	OPT_MODE_MODFUNC = "Module then function"
	OPT_MODE = "Commands selection :"
	OPT_EXPLORATEUR = "Web navigator :"
	OPT_DOC = "Documentation path :"

	## COMBO
	COMBO_NO_COLOR = "None"
	COMBO_NONE = "-- None --"
	COMBO_REQUIRED = "-- Required Arguments --"
	COMBO_OPTIONAL = "-- Optional Arguments --"
	COMBO_SELECT = "-- Select --"

	## NOTEBOOK
	NTB_PAGE_EDITION = "Edition"
	NTB_PAGE_COLOR = "Coloration"

	## WARNING
	WARN_PLEASE_USE = "Please use %s"
	WARN_PARAM_ERROR = "WARNING !! LINE : %s.%s(%s)"
	WARN_PLEASE_SET = "PLEASE CHECK ENVIRONMENT VARIABLE : CHIPYGUI_INSTALL"
	WARN_NO_CATALOG = "WARNING !! Can't load ChiPy's catalog\nUse option --generate-catalog"
	WARN_CONTINUE = "Do you want to use ChiPyGUI without ChiPy library (y/n) ?"
	WARN_YES = ["yes", "y"]
	
	## STATUS MESSAGE
	STATUS_OTHER_MODULES = "Other Modules :"
	STATUS_OTHER_FUNCTIONS = "Other functions :"

	## INFO
	INFO_LOADING_CATALOG = "Loading ChiPy functions from catalog"
	INFO_GENERATE_CATALOG = "Generating ChiPy functions from dynamic library"

	## AIDE
	HELP_OPEN_FILE = "Open file"
	HELP_GENERATE_CATALOG = "Generate ChiPy's functions catalog"
